# =============================================================================
# claim_auditor_team.R
# Statistical Peer Review Assistant
# =============================================================================
# This file defines:
#   1. RAG knowledge-base loading and retrieval
#   2. Expertise prompt loading
#   3. Multi-agent specialist team
#   4. Claim-level audit
#   5. Whole-paper audit
# =============================================================================

library(ellmer)
library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(glue)
library(tibble)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

# -----------------------------------------------------------------------------
# Model settings
# -----------------------------------------------------------------------------

MODEL_NAME <- Sys.getenv(
  "ANTHROPIC_MODEL",
  unset = "claude-haiku-4-5-20251001"
)

# -----------------------------------------------------------------------------
# Knowledge base workspace
# -----------------------------------------------------------------------------

KB_WORKSPACE <- new.env(parent = emptyenv())

# -----------------------------------------------------------------------------
# Project helpers
# -----------------------------------------------------------------------------

find_project_root <- function() {
  current <- normalizePath(getwd(), mustWork = TRUE)
  
  candidates <- unique(c(
    current,
    dirname(current),
    dirname(dirname(current)),
    dirname(dirname(dirname(current)))
  ))
  
  for (candidate in candidates) {
    if (
      file.exists(file.path(candidate, "claim_auditor_team.R")) ||
      dir.exists(file.path(candidate, "expertise")) ||
      dir.exists(file.path(candidate, "data"))
    ) {
      return(candidate)
    }
  }
  
  current
}

find_expertise_dir <- function() {
  current <- normalizePath(getwd(), mustWork = TRUE)
  
  candidates <- unique(c(
    file.path(current, "expertise"),
    file.path(dirname(current), "expertise"),
    file.path(dirname(dirname(current)), "expertise"),
    file.path(find_project_root(), "expertise")
  ))
  
  candidates <- normalizePath(candidates, mustWork = FALSE)
  
  for (candidate in candidates) {
    if (
      dir.exists(candidate) &&
      file.exists(file.path(candidate, "claim_extractor.md"))
    ) {
      return(candidate)
    }
  }
  
  stop(
    "Could not find an expertise/ folder containing claim_extractor.md.\n",
    "Make sure your working directory is the main stat_claim_auditor folder."
  )
}

expertise <- function(filename, fallback = NULL) {
  expertise_dir <- find_expertise_dir()
  path <- file.path(expertise_dir, filename)
  
  if (!file.exists(path)) {
    if (!is.null(fallback)) {
      return(fallback)
    }
    
    stop(
      "Expertise file not found: ", filename, "\n",
      "Expected location: ", path
    )
  }
  
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

# -----------------------------------------------------------------------------
# Fallback prompts
# -----------------------------------------------------------------------------

fallback_claim_extractor <- "
# Claim Extractor Agent

Extract statistical claims from scientific/manuscript text.

Return concise bullets or numbered claims.

Do not audit yet.
Do not give a verdict.
Do not invent information.
"

fallback_statistical_misconception <- "
# Statistical Misconception Agent

Identify statistical reasoning and reporting problems.

Focus on:
- p-value misuse
- confidence intervals and effect sizes
- relative percent change vs absolute difference
- odds ratio vs risk ratio
- hazard ratio interpretation
- multiple comparisons
- outcome switching
- small sample overinterpretation
- non-independence / pseudoreplication
- overfitting and missing validation
- missing uncertainty
- Simpson's paradox
- regression to the mean
- inconsistent or impossible table values

Rules:
- Do not invent numbers.
- If calculating from reported numbers, label as approximate.
- For continuous outcomes, do not call the issue relative risk unless the outcome is actually a risk/probability.
- Be concise.
"

fallback_causal_inference <- "
# Causal Inference Agent

Evaluate causal language and causal assumptions.

Focus on:
- observational vs randomized/experimental design
- confounding
- reverse causality
- selection bias
- collider bias
- confounding by indication
- target trial framing
- causal overclaiming

Rules:
- If the design is randomized or experimental, do not frame the main issue as confounding unless there is evidence of failed randomization, noncompliance, attrition, imbalance, or unclear assignment.
- For randomized/experimental studies, focus on randomization, allocation, blinding, independence, protocol deviations, and generalizability.
- Do not decide whether a biomedical claim is true.
- Be concise.
"

fallback_skeptical_reviewer <- "
# Skeptical Reviewer Agent

Identify overconfidence, missing information, unsupported assumptions, and what cannot be assessed.

Focus on:
- missing sample sizes or group sizes
- missing primary outcome hierarchy
- table/figure inconsistencies
- unsupported causal or mechanistic claims
- hidden multiplicity
- data availability and reproducibility
- whether the critique should be softened because only limited text was provided

Do not invent facts.
Be concise.
"

fallback_guardrail_coach <- "
# Reviewer Language and Guardrail Agent

Translate statistical concerns into safe, professional reviewer-style language.

Return:
- cautious reviewer framing
- questions for authors
- suggested manuscript revision language
- limitation/disclaimer

Rules:
- Do not provide medical advice.
- Do not say accept or reject.
- Do not invent confidence intervals, p-values, sample sizes, or effect sizes.
- If an example revision needs numbers not reported, use placeholders.
"

fallback_manuscript_type_classifier <- "
# Manuscript Type Classifier Agent

Classify manuscript or excerpt into one of:
- Randomized trial
- Experimental behavioral study
- Observational study
- Prediction model / machine learning study
- Diagnostic accuracy study
- Systematic review / meta-analysis
- Case report / case series
- General statistical manuscript
- Unclear

Return:
- Manuscript type
- Evidence from text
- Confidence: low / moderate / high
"

fallback_reporting_guideline <- "
# Reporting Guideline Agent

Review reporting and transparency issues.

Focus on:
- incomplete methods reporting
- missing sample size/group size details
- missing primary/secondary outcome distinction
- unclear randomization or allocation
- missing assumptions/diagnostics
- missing confidence intervals/effect sizes
- unclear tables/figures
- missing data handling
- data/code availability

Do not invent guideline requirements.
Be concise.
"

fallback_prediction_model_review <- "
# Prediction Model Review Agent

Review prediction model and machine-learning claims.

Focus on:
- intended use
- target population
- outcome/predictor timing
- data leakage
- overfitting
- internal/external validation
- calibration
- discrimination
- resampling
- sample size/events
- clinical usefulness

If not applicable, say briefly that this agent is not applicable.
"

# -----------------------------------------------------------------------------
# Knowledge base loading
# -----------------------------------------------------------------------------

load_knowledge_base <- function(
    path = "data/RAG_baseline.csv"
) {
  if (!file.exists(path)) {
    alt_path <- file.path(find_project_root(), path)
    
    if (file.exists(alt_path)) {
      path <- alt_path
    } else {
      stop(
        "Knowledge base file not found: ", path, "\n",
        "Make sure the RAG CSV is inside your data/ folder."
      )
    }
  }
  
  kb <- readr::read_csv(path, show_col_types = FALSE)
  names(kb) <- stringr::str_trim(names(kb))
  
  required_cols <- c(
    "id",
    "category",
    "paper_type",
    "topic",
    "source",
    "source_url",
    "full_reference",
    "misconception",
    "principle",
    "warning_sign",
    "expert_question",
    "example",
    "issue_or_guideline",
    "reviewer_question"
  )
  
  missing_cols <- setdiff(required_cols, names(kb))
  
  if (length(missing_cols) > 0) {
    stop(
      "Knowledge base is missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  kb <- kb |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ ifelse(is.na(.x), "", as.character(.x))
      )
    )
  
  assign("kb", kb, envir = KB_WORKSPACE)
  invisible(TRUE)
}

get_knowledge_base <- function() {
  if (!exists("kb", envir = KB_WORKSPACE)) {
    load_knowledge_base()
  }
  
  get("kb", envir = KB_WORKSPACE)
}

# -----------------------------------------------------------------------------
# RAG retrieval
# -----------------------------------------------------------------------------

clean_words <- function(text) {
  text |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9%]+", " ") |>
    stringr::str_split("\\s+") |>
    unlist() |>
    unique()
}

retrieve_context <- function(
    claim,
    top_n = 8,
    paper_type = NULL,
    review_mode = NULL
) {
  kb <- get_knowledge_base()
  
  query_text <- paste(
    claim,
    paper_type %||% "",
    review_mode %||% "",
    collapse = " "
  )
  
  claim_lower <- stringr::str_to_lower(query_text)
  claim_words <- clean_words(query_text)
  claim_words <- claim_words[nchar(claim_words) >= 3]
  
  scored <- kb |>
    dplyr::rowwise() |>
    dplyr::mutate(
      combined_text = paste(
        category,
        paper_type,
        topic,
        source,
        source_url,
        full_reference,
        misconception,
        issue_or_guideline,
        principle,
        warning_sign,
        expert_question,
        reviewer_question,
        example,
        sep = " "
      ),
      row_words = list(clean_words(combined_text)),
      keyword_score = sum(claim_words %in% row_words),
      phrase_bonus = dplyr::case_when(
        stringr::str_detect(claim_lower, "p-value|p value|p<|p <|significant|statistically significant|null hypothesis") &
          stringr::str_detect(stringr::str_to_lower(combined_text), "p-value|statistical significance|asa|abandon|beyond p") ~ 8,
        
        stringr::str_detect(claim_lower, "forking|p-hacking|p hacking|researcher degrees|multiple comparison|multiplicity|subgroup|outcome switching") &
          stringr::str_detect(stringr::str_to_lower(combined_text), "forking|degrees of freedom|multiple comparisons|false-positive|subgroup|harking") ~ 8,
        
        stringr::str_detect(claim_lower, "prediction|machine learning|model|auc|accuracy|calibration|validation|train|test|cross-validation|overfit|overfitting") &
          stringr::str_detect(stringr::str_to_lower(combined_text), "prediction|machine learning|auc|calibration|validation|overfitting|resampling|tripod") ~ 9,
        
        stringr::str_detect(claim_lower, "causal|cause|causes|caused|prevent|prevents|effect of|confounding|confounder|dag|directed acyclic|collider|target trial") &
          stringr::str_detect(stringr::str_to_lower(combined_text), "causal|confounding|confounder|dag|directed acyclic|target trial|collider|hern") ~ 9,
        
        stringr::str_detect(claim_lower, "randomized|randomised|experiment|experimental|allocation|assignment|trial|consort") &
          stringr::str_detect(stringr::str_to_lower(combined_text), "randomized|randomised|experiment|trial|consort|statistical methods in psychology") ~ 8,
        
        stringr::str_detect(claim_lower, "psychology|behavioral|behavioural|anova|effect size|confidence interval|cohen|assumption") &
          stringr::str_detect(stringr::str_to_lower(combined_text), "psychology|effect sizes|confidence intervals|assumptions|statistical methods") ~ 9,
        
        stringr::str_detect(claim_lower, "odds ratio|\\bor\\b") &
          stringr::str_detect(stringr::str_to_lower(combined_text), "odds ratio|risk ratio|odds versus risk") ~ 7,
        
        stringr::str_detect(claim_lower, "relative|absolute|percent|percentage|%|increase|decrease|baseline") &
          stringr::str_detect(stringr::str_to_lower(combined_text), "relative|absolute|risk|baseline|effect size") ~ 5,
        
        stringr::str_detect(claim_lower, "diagnostic|sensitivity|specificity|predictive value|ppv|npv|likelihood ratio|test accuracy") &
          stringr::str_detect(stringr::str_to_lower(combined_text), "diagnostic|sensitivity|specificity|predictive value|likelihood ratio") ~ 9,
        
        stringr::str_detect(claim_lower, "missing|imputation|complete case|na|drop|dropped") &
          stringr::str_detect(stringr::str_to_lower(combined_text), "missing|imputation|complete-case") ~ 8,
        
        stringr::str_detect(claim_lower, "generalize|generalise|extrapolate|external validity|transport|single center|single centre") &
          stringr::str_detect(stringr::str_to_lower(combined_text), "generalisation|extrapolation|external validity|generalize") ~ 8,
        
        stringr::str_detect(claim_lower, "simpson|strata|stratified|aggregate|subgroup reversal") &
          stringr::str_detect(stringr::str_to_lower(combined_text), "simpson") ~ 10,
        
        stringr::str_detect(claim_lower, "regression to the mean|extreme baseline|baseline high|baseline low") &
          stringr::str_detect(stringr::str_to_lower(combined_text), "regression to the mean") ~ 10,
        
        stringr::str_detect(claim_lower, "systematic review|meta-analysis|meta analysis|prisma") &
          stringr::str_detect(stringr::str_to_lower(combined_text), "prisma|systematic review|meta-analysis") ~ 8,
        
        stringr::str_detect(claim_lower, "table|figure|mean|sd|standard deviation|cell size|inconsistent|impossible|df|degrees of freedom") &
          stringr::str_detect(stringr::str_to_lower(combined_text), "reporting|data organization|statistical methods|common statistical mistakes|transparency") ~ 7,
        
        TRUE ~ 0
      ),
      score = keyword_score + phrase_bonus
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::desc(score), id) |>
    dplyr::select(-combined_text, -row_words)
  
  scored |>
    dplyr::slice_head(n = top_n)
}

format_context_for_agent <- function(context_rows) {
  if (is.null(context_rows) || nrow(context_rows) == 0) {
    return("No relevant knowledge base context was retrieved.")
  }
  
  safe_get <- function(df, row_i, col_name) {
    if (!col_name %in% names(df)) {
      return("")
    }
    
    value <- df[[col_name]][row_i]
    
    if (length(value) == 0 || is.na(value) || is.null(value)) {
      return("")
    }
    
    as.character(value)
  }
  
  context_text <- purrr::map_chr(seq_len(nrow(context_rows)), function(i) {
    paste0(
      "Knowledge Base Entry ", safe_get(context_rows, i, "id"), "\n",
      "Category: ", safe_get(context_rows, i, "category"), "\n",
      "Paper type: ", safe_get(context_rows, i, "paper_type"), "\n",
      "Topic: ", safe_get(context_rows, i, "topic"), "\n",
      "Source: ", safe_get(context_rows, i, "source"), "\n",
      "URL: ", safe_get(context_rows, i, "source_url"), "\n",
      "Full reference: ", safe_get(context_rows, i, "full_reference"), "\n",
      "Issue or guideline: ", safe_get(context_rows, i, "issue_or_guideline"), "\n",
      "Misconception: ", safe_get(context_rows, i, "misconception"), "\n",
      "Principle: ", safe_get(context_rows, i, "principle"), "\n",
      "Warning sign: ", safe_get(context_rows, i, "warning_sign"), "\n",
      "Expert question: ", safe_get(context_rows, i, "expert_question"), "\n",
      "Reviewer question: ", safe_get(context_rows, i, "reviewer_question"), "\n",
      "Example: ", safe_get(context_rows, i, "example"), "\n"
    )
  })
  
  paste(context_text, collapse = "\n---\n")
}

# -----------------------------------------------------------------------------
# Agent creation
# -----------------------------------------------------------------------------

create_agent <- function(system_prompt) {
  ellmer::chat_anthropic(
    model = MODEL_NAME,
    system_prompt = system_prompt
  )
}

claim_extractor_agent <- create_agent(
  paste(
    expertise("claim_extractor.md", fallback_claim_extractor),
    "",
    "You are part of a statistical peer-review assistant.",
    "Your role is extraction only. Do not audit or judge the claim.",
    sep = "\n"
  )
)

statistical_misconception_agent <- create_agent(
  paste(
    expertise("statistical_misconception.md", fallback_statistical_misconception),
    "",
    "Use retrieved knowledge base entries when relevant.",
    "Do not invent citations, facts, confidence intervals, p-values, sample sizes, or effect sizes.",
    "If calculating an approximate value, label it clearly as approximate.",
    sep = "\n"
  )
)

causal_inference_agent <- create_agent(
  paste(
    expertise("causal_inference.md", fallback_causal_inference),
    "",
    "Focus on design-appropriate causal critique.",
    "If randomized/experimental, do not overuse confounding language.",
    sep = "\n"
  )
)

skeptical_reviewer_agent <- create_agent(
  paste(
    expertise("skeptical_reviewer.md", fallback_skeptical_reviewer),
    "",
    "Your job is to reduce overconfidence and identify what cannot be assessed.",
    "Do not add unsupported facts.",
    sep = "\n"
  )
)

guardrail_coach_agent <- create_agent(
  paste(
    expertise("guardrail_coach.md", fallback_guardrail_coach),
    "",
    "Write professional, cautious, author-facing and reviewer-facing language.",
    "Do not provide medical advice or journal accept/reject decisions.",
    sep = "\n"
  )
)

manuscript_type_classifier_agent <- create_agent(
  paste(
    expertise("manuscript_type_classifier.md", fallback_manuscript_type_classifier),
    "",
    "Classify manuscript type only. Do not audit.",
    sep = "\n"
  )
)

reporting_guideline_agent <- create_agent(
  paste(
    expertise("reporting_guideline.md", fallback_reporting_guideline),
    "",
    "Use retrieved knowledge base context. Do not invent guideline requirements.",
    sep = "\n"
  )
)

prediction_model_review_agent <- create_agent(
  paste(
    expertise("prediction_model_review.md", fallback_prediction_model_review),
    "",
    "Review prediction and ML claims only when relevant.",
    sep = "\n"
  )
)

final_synthesizer_agent <- create_agent(
  paste(
    "You are the final synthesizer for a statistical peer-review assistant.",
    "You write concise, professional, reviewer-style summaries.",
    "Use only provided agent outputs and retrieved knowledge base context.",
    "Do not invent citations, confidence intervals, p-values, sample sizes, or effect sizes.",
    "Do not make journal accept/reject decisions.",
    sep = "\n"
  )
)

orchestrator <- final_synthesizer_agent

# -----------------------------------------------------------------------------
# Generic chat wrappers
# -----------------------------------------------------------------------------

agent_chat <- function(agent, prompt) {
  response <- agent$chat(prompt, echo = FALSE)
  paste(as.character(response), collapse = "\n")
}

consult_claim_extractor <- function(prompt) {
  agent_chat(claim_extractor_agent, prompt)
}

consult_statistical_misconception <- function(prompt) {
  agent_chat(statistical_misconception_agent, prompt)
}

consult_causal_inference <- function(prompt) {
  agent_chat(causal_inference_agent, prompt)
}

consult_skeptical_reviewer <- function(prompt) {
  agent_chat(skeptical_reviewer_agent, prompt)
}

consult_guardrail_coach <- function(prompt) {
  agent_chat(guardrail_coach_agent, prompt)
}

consult_manuscript_type_classifier <- function(prompt) {
  agent_chat(manuscript_type_classifier_agent, prompt)
}

consult_reporting_guideline <- function(prompt) {
  agent_chat(reporting_guideline_agent, prompt)
}

consult_prediction_model_review <- function(prompt) {
  agent_chat(prediction_model_review_agent, prompt)
}

# -----------------------------------------------------------------------------
# Text utilities for full-paper review
# -----------------------------------------------------------------------------

safe_truncate <- function(text, max_chars = 6000) {
  text <- paste(as.character(text), collapse = "\n")
  if (nchar(text) <= max_chars) {
    return(text)
  }
  
  paste0(substr(text, 1, max_chars), "\n\n[TRUNCATED]")
}

split_paper_into_sections <- function(text) {
  text <- paste(as.character(text), collapse = "\n")
  text <- stringr::str_replace_all(text, "\r\n|\r", "\n")
  text <- stringr::str_replace_all(text, "[ \t]+", " ")
  text <- stringr::str_replace_all(text, "\n{3,}", "\n\n")
  
  lower <- stringr::str_to_lower(text)
  
  find_pos <- function(patterns) {
    positions <- purrr::map_dbl(patterns, function(pat) {
      loc <- stringr::str_locate(
        lower,
        stringr::regex(pat, ignore_case = TRUE)
      )[, 1]
      
      if (is.na(loc)) {
        return(Inf)
      }
      
      as.numeric(loc)
    })
    
    pos <- suppressWarnings(min(positions, na.rm = TRUE))
    
    if (is.infinite(pos) || is.na(pos)) {
      return(NA_real_)
    }
    
    pos
  }
  
  markers <- tibble::tibble(
    section = c(
      "Abstract",
      "Introduction",
      "Methods",
      "Results",
      "Discussion",
      "Tables and Figures",
      "References"
    ),
    pos = as.numeric(c(
      find_pos(c("\\babstract\\b")),
      find_pos(c("\\bintroduction\\b")),
      find_pos(c("\\bresearch methods and procedures\\b", "\\bmethods\\b", "\\bmethod\\b")),
      find_pos(c("\\bresults\\b")),
      find_pos(c("\\bdiscussion\\b")),
      find_pos(c("\\btable 1\\b", "\\bfigure 1\\b", "\\btable 2\\b")),
      find_pos(c("\\breferences\\b"))
    ))
  ) |>
    dplyr::filter(!is.na(pos), is.finite(pos)) |>
    dplyr::arrange(pos) |>
    dplyr::distinct(section, .keep_all = TRUE)
  
  if (nrow(markers) == 0) {
    return(list(
      Full_Text = text
    ))
  }
  
  sections <- list()
  
  for (i in seq_len(nrow(markers))) {
    start <- markers$pos[i]
    end <- if (i < nrow(markers)) markers$pos[i + 1] - 1 else nchar(text)
    
    section_text <- substr(text, start, end)
    section_text <- stringr::str_trim(section_text)
    
    if (nchar(section_text) > 50) {
      sections[[markers$section[i]]] <- section_text
    }
  }
  
  sections
}

make_paper_map_text <- function(sections, max_chars_per_section = 4500) {
  section_names <- names(sections)
  
  parts <- purrr::map_chr(section_names, function(sec) {
    paste0(
      "\n\n## ", sec, "\n",
      safe_truncate(sections[[sec]], max_chars_per_section)
    )
  })
  
  paste(parts, collapse = "\n")
}

summarize_paper_sections <- function(
    paper_text,
    paper_type = "Auto-detect / unclear",
    max_chars_per_section = 4500
) {
  sections <- split_paper_into_sections(paper_text)
  section_text <- make_paper_map_text(sections, max_chars_per_section)
  
  prompt <- glue::glue("
You are creating a section-by-section statistical map of a full uploaded paper.

Paper type selected by user:
{paper_type}

Read the section excerpts below and produce a concise paper map.

Return exactly this structure:

# Paper Map

1. Study type and design
- Identify the design using only the text.

2. Sample and groups
- Report sample size, group sizes, recruitment source, and any inconsistencies or unclear details.

3. Exposure/intervention/predictor
- Identify main exposure or experimental condition.

4. Outcomes
- List primary-looking and secondary-looking outcomes.

5. Statistical methods
- List statistical tests/models and any covariates/subgroups.

6. Main results
- Summarize main numerical findings from text/tables.

7. Tables and figures to check
- Identify tables/figures and what each contains.

8. Claims that need review
- List 4-8 claims or conclusions that need statistical review.

9. Paper-level red flags to check
- Mention internal consistency, multiplicity/outcome hierarchy, group sizes/df, broad generalization, reproducibility, and data availability if relevant.

Rules:
- Do not invent values.
- If a detail is absent, say 'not clear from provided text'.
- Do not audit deeply yet; map the paper first.

SECTION EXCERPTS:
{section_text}
")
  
  agent_chat(final_synthesizer_agent, prompt)
}

# -----------------------------------------------------------------------------
# Claim-level audit
# -----------------------------------------------------------------------------

audit_peer_review <- function(
    claim,
    source_text = "",
    paper_type = "Auto-detect / unclear",
    review_mode = "Statistical peer-review comments",
    top_n = 8,
    show_retrieved_context = FALSE
) {
  load_knowledge_base()
  
  source_excerpt <- safe_truncate(source_text, 5000)
  
  # Claim/paragraph review does not create a paper map.
  # These defaults prevent accidental whole-paper prompt variables from crashing.
  paper_map <- ""
  benchmark_context <- ""
  
  retrieval_query <- paste(
    paper_type,
    review_mode,
    claim,
    source_excerpt,
    "claim paragraph abstract statistical review p values effect sizes confidence intervals causal language multiplicity subgroup missing denominator reporting",
    collapse = " "
  )
  
  retrieved_context <- retrieve_context(
    retrieval_query,
    top_n = top_n,
    paper_type = paper_type,
    review_mode = review_mode
  )
  
  retrieved_context_text <- format_context_for_agent(retrieved_context)
  
  if (isTRUE(show_retrieved_context)) {
    cat("\n================ RETRIEVED RAG CONTEXT ================\n")
    cat(retrieved_context_text)
    cat("\n=======================================================\n\n")
  }
  
  extraction <- consult_claim_extractor(glue::glue("
Extract the claim structure for statistical peer review.

Paper type:
{paper_type}

Review mode:
{review_mode}

Claim:
{claim}

Source context:
{source_excerpt}

Return concise bullets only:
- Population/sample
- Exposure/intervention/predictor
- Outcome
- Comparison group
- Effect measure or performance metric
- Time frame
- Study design
- Denominator/sample size
- Missing information needed for peer review
"))
  
  stat_review <- consult_statistical_misconception(glue::glue("
Audit this selected claim for statistical reasoning and reporting problems.

Paper type:
{paper_type}

Review mode:
{review_mode}

Claim:
{claim}

Claim extraction:
{extraction}

Retrieved knowledge base:
{retrieved_context_text}

Rules:
- Prioritize the top 2-4 statistical issues only.
- If the outcome is continuous, say absolute mean difference or relative percent change, not risk.
- Do not invent confidence intervals, p-values, sample sizes, effect sizes, or diagnostics.
- If suggesting a calculation, say authors should report/calculate it.
- If only a claim/excerpt is provided, state that the review is limited to provided text.
"))
  
  causal_review <- consult_causal_inference(glue::glue("
Evaluate causal inference concerns for the selected claim.

Paper type:
{paper_type}

Review mode:
{review_mode}

Claim:
{claim}

Claim extraction:
{extraction}

Statistical review:
{stat_review}

Retrieved knowledge base:
{retrieved_context_text}

Rules:
- If this is randomized/experimental, do not frame the main issue as confounding unless there is evidence of failed randomization, imbalance, noncompliance, attrition, or unclear assignment.
- Focus on design-appropriate causal wording.
- Do not invent facts.
"))
  
  reporting_review <- consult_reporting_guideline(glue::glue("
Evaluate reporting/transparency concerns for this selected claim.

Paper type:
{paper_type}

Review mode:
{review_mode}

Claim:
{claim}

Source excerpt:
{source_excerpt}

Retrieved knowledge base:
{retrieved_context_text}

Return concise bullets:
- missing reporting elements
- reviewer questions
- suggested author clarification

Do not invent guideline requirements.
"))
  
  prediction_review <- consult_prediction_model_review(glue::glue("
Evaluate prediction-model or machine-learning concerns if relevant.

Paper type:
{paper_type}

Review mode:
{review_mode}

Claim:
{claim}

Claim extraction:
{extraction}

Retrieved knowledge base:
{retrieved_context_text}

If not applicable, say briefly that this agent is not applicable.
"))
  
  skeptic_review <- consult_skeptical_reviewer(glue::glue("
Critique the previous agent outputs as a skeptical statistical reviewer.

Claim:
{claim}

Claim extraction:
{extraction}

Statistical review:
{stat_review}

Causal review:
{causal_review}

Reporting review:
{reporting_review}

Prediction/model review:
{prediction_review}

Focus on:
- overconfidence risks
- what cannot be assessed from provided text
- whether wording should be softened
- invented-number risk
"))
  
  guardrail_review <- consult_guardrail_coach(glue::glue("
Prepare safe peer-review style language.

Claim:
{claim}

Previous agent outputs:
{stat_review}

{causal_review}

{reporting_review}

{skeptic_review}

Rules:
- Do not invent numerical values.
- Use placeholders where values are missing.
- Keep language professional and constructive.
"))
  
  final_prompt <- glue::glue("
You are the final synthesizer for a statistical peer-review assistant.

Selected claim:
{claim}

Paper type:
{paper_type}

Review mode:
{review_mode}

Source excerpt:
{source_excerpt}

Retrieved knowledge base:
{retrieved_context_text}

Claim extraction:
{extraction}

Statistical review:
{stat_review}

Causal review:
{causal_review}

Reporting review:
{reporting_review}

Prediction/model review:
{prediction_review}

Skeptical review:
{skeptic_review}

Reviewer language guidance:
{guardrail_review}

Abstract/excerpt review guardrail:
- If the provided text appears to be an abstract, summary, or short excerpt, do not treat missing full-paper details as confirmed flaws.
- Abstracts often omit CONSORT flow diagrams, baseline tables, ICC/design effect, full adverse-event tables, statistical analysis plan details, subgroup interaction tests, and full secondary endpoint results.
- For good randomized-trial abstracts that report design, sample size, primary endpoint, effect estimate, confidence interval, and p-value, default to “Supported” or “Questionable,” not “Misleading,” unless the abstract itself makes an unsupported overclaim.
- Missing details from an abstract should be phrased as “verify in the full paper,” not “not reported” or “prevents assessment,” unless the abstract makes a strong claim that depends on the missing detail.
- Do not invent or assume that reported confidence intervals are unadjusted, incorrectly clustered, or invalid unless the text says so.
- Do not say “baseline balance is unverified” as a major concern for a randomized-trial abstract; say “baseline balance should be verified in the full paper.”
- Do not use observational-confounding language for randomized trials unless there is evidence of failed randomization, imbalance, nonadherence, or differential attrition.
- For abstract-only reviews, focus on whether the abstract’s wording is appropriately cautious, not whether every full-paper reporting element is present.

Final answer must use this exact structure:

# Claim Review Summary

1. Verdict
Choose exactly one:
- Supported
- Questionable
- Misleading
- Incorrect
- Cannot assess from provided text
- For abstract-only good RCT claims, use “Supported” when the primary conclusion is backed by a randomized design and reports an effect estimate with uncertainty.
- Use “Questionable” only when the abstract wording overstates secondary outcomes, generalizability, subgroup findings, safety, mechanism, or clinical importance.
- Use “Misleading” only when the abstract makes a clear unsupported statistical or causal overclaim, not merely because full-paper details are absent.

2. Main problem
Explain the single most important statistical issue in 2-3 plain-language sentences.
Focus on the primary issue only.
Do not list broad generic problems that are not necessary for this claim.

3. Why this matters
List 2-3 short bullets directly tied to the main issue.
Do not add unrelated statistical concerns.

4. Safer wording
Rewrite the claim more carefully in one sentence.
Use placeholders only when needed.
Do not invent numbers.

5. What to check
List only the most important 3 items the reader should check.

6. Technical note
Optional. Include only if needed.
Keep under 60 words.

Verdict rules:
- Do not combine verdict labels.
- Use only one of the five allowed labels: Supported, Questionable, Misleading, Incorrect, or Cannot assess from provided text.
- Use “Supported” only when the claim is appropriately cautious and the provided text gives enough evidence to support the wording.
- Use “Questionable” when the claim may be reasonable, but important statistical details are missing.
- Use “Misleading” when the claim may contain some truth but is worded in a way that exaggerates, omits key context, or leads readers to an unsupported conclusion.
- Use “Incorrect” when the claim directly misdefines or misinterprets a statistical concept.
- Use “Cannot assess from provided text” only when the text is too incomplete to identify or evaluate the statistical claim.
- Missing details alone do not mean “Cannot assess.”
- If the claim contains a recognizable statistical reasoning error, choose “Misleading” or “Incorrect,” not “Cannot assess.”

Issue-specific explanation rules:
- If the issue is causal overclaim, focus on association vs causation, temporal order, confounding, reverse causality, and study design.
- If the issue is p-value misunderstanding, focus on what a p-value does and does not mean. Use “Incorrect” if the claim directly misdefines the p-value.
- If the issue is non-significant result overinterpretation, focus only on failure to reject the null, confidence interval width, statistical power/sample size, and equivalence or noninferiority logic. Do not mention confounding, bias, DAGs, or observational design unless the original claim explicitly involves observational causal inference.
- If the issue is relative risk without absolute risk, focus on baseline risk, absolute risk difference, denominator, and confidence interval.
- If the issue is odds ratio/risk ratio confusion, focus on baseline risk, outcome frequency, and why odds ratios can exaggerate risk ratios when outcomes are common.
- If the issue is missing denominator or comparison group, focus on denominator, event rate, comparison group, time frame, and outcome definition.
- If the issue is small sample/no control group, focus on uncertainty, lack of comparison group, placebo/natural recovery, and overclaiming.
- If the issue is multiple testing/selective reporting, focus on number of tests, pre-specification, multiplicity adjustment, and selective reporting.
- If the issue is prediction model overclaim, focus on validation, calibration, discrimination, threshold choice, external validation, and clinical utility.
- If the issue is subgroup/generalization overclaim, focus on interaction testing, subgroup sample size, uncertainty, and whether the claim applies beyond the studied population.

Style rules:
- Keep the full answer under 350 words.
- Use plain language first.
- Do not write a long reviewer report for simple claims.
- Do not invent confidence intervals, p-values, sample sizes, effect sizes, or diagnostic results.
- Do not cite too many sources. Mention at most 2 relevant sources in the technical note if needed.
- Do not use highly technical terms unless necessary; if used, define them briefly.
")
  
  final_text <- agent_chat(final_synthesizer_agent, final_prompt)
  
  list(
    final_text = final_text,
    retrieved_context = retrieved_context,
    agent_trace = list(
      extraction = extraction,
      stat_review = stat_review,
      causal_review = causal_review,
      reporting_review = reporting_review,
      prediction_review = prediction_review,
      skeptic_review = skeptic_review,
      guardrail_review = guardrail_review
    )
  )
}

# -----------------------------------------------------------------------------
# Whole-paper audit
# -----------------------------------------------------------------------------

audit_full_paper <- function(
    paper_text,
    paper_type = "Auto-detect / unclear",
    review_mode = "Whole-paper statistical peer review",
    top_n = 10,
    show_retrieved_context = FALSE,
    benchmark_context = ""
) {
  load_knowledge_base()
  
  paper_map <- summarize_paper_sections(
    paper_text = paper_text,
    paper_type = paper_type,
    max_chars_per_section = 4500
  )
  
  retrieval_query <- paste(
    paper_type,
    review_mode,
    paper_map,
    benchmark_context,
    "whole paper statistical review internal consistency tables figures group sizes df p values means sd outcome switching randomization multiplicity reproducibility generalizability",
    collapse = " "
  )
  
  retrieved_context <- retrieve_context(
    retrieval_query,
    top_n = top_n,
    paper_type = paper_type,
    review_mode = review_mode
  )
  
  retrieved_context_text <- format_context_for_agent(retrieved_context)
  
  if (isTRUE(show_retrieved_context)) {
    cat("\n================ RETRIEVED RAG CONTEXT ================\n")
    cat(retrieved_context_text)
    cat("\n=======================================================\n\n")
  }
  
  sections <- split_paper_into_sections(paper_text)
  section_text <- make_paper_map_text(sections, max_chars_per_section = 4000)
  
  stat_review <- consult_statistical_misconception(glue::glue("
You are reviewing a full uploaded paper, not just one claim.

Paper type:
{paper_type}

Review mode:
{review_mode}

Paper map:
{paper_map}

Retrieved knowledge base:
{retrieved_context_text}

Section excerpts:
{safe_truncate(section_text, 18000)}

Provide a whole-paper statistical critique.

Focus on:
- internal consistency of n, group sizes, means, SDs, df, F statistics, p-values, tables, and figures
- missing confidence intervals/effect sizes
- primary vs secondary outcome hierarchy
- multiplicity and outcome switching
- subgroup/covariate claims
- appropriateness of tests/models
- independence/clustering/session design
- broad conclusions beyond the data
- reproducibility/data availability

Rules:
- Do not calculate or report new CIs, Cohen’s d, eta-squared, or approximate p-values.
- Instead, recommend that authors report them.
- Do not label an issue “confirmed” unless the paper text directly shows it.
- If only the extracted text is available, phrase missing items as “not clearly reported in the extracted text.”
- For well-designed randomized trials, separate primary-outcome validity from secondary-outcome interpretation.
- Do not treat subgroup or secondary-outcome uncertainty as a major flaw unless it directly undermines the primary conclusion.
- Label routine requests as reviewer checks, not confirmed flaws.
"))
  
  causal_review <- consult_causal_inference(glue::glue("
You are reviewing causal and design claims in a full paper.

Paper type:
{paper_type}

Paper map:
{paper_map}

Statistical review:
{stat_review}

Retrieved knowledge base:
{retrieved_context_text}

Focus on:
- whether the design supports causal language
- randomization or experimental assignment
- internal validity
- independence/session/group effects
- generalizability
- whether mechanistic claims exceed the evidence

Rules:
- If experimental/randomized, do not overuse observational-confounding language.
- Be design-specific.
- Do not invent facts.
"))
  
  reporting_review <- consult_reporting_guideline(glue::glue("
You are reviewing reporting and transparency in a full paper.

Paper type:
{paper_type}

Paper map:
{paper_map}

Retrieved knowledge base:
{retrieved_context_text}

Section excerpts:
{safe_truncate(section_text, 16000)}

Focus on:
- sample/group size reporting
- randomization/assignment reporting
- primary outcome clarity
- table/figure clarity
- assumptions/diagnostics
- missing data
- data/code availability
- reproducibility
- limitations

Be concise and author-facing.
"))
  
  skeptic_review <- consult_skeptical_reviewer(glue::glue("
Critique the whole-paper review as a skeptical reviewer.

Paper map:
{paper_map}

Statistical review:
{stat_review}

Causal/design review:
{causal_review}

Reporting review:
{reporting_review}

Focus on:
- which concerns are directly supported by the paper text
- which are only checks/questions
- whether the review overstates the problem
- what should be verified against tables/figures
- whether any external-known critique should be separated from paper-internal critique

Do not invent facts.
"))
  
  final_prompt <- glue::glue("
You are the final synthesizer for a whole-paper statistical peer-review assistant.

This is a full uploaded paper review, not a single-claim audit.

Paper type:
{paper_type}

Review mode:
{review_mode}

Paper map:
{paper_map}

Retrieved knowledge base:
{retrieved_context_text}

Statistical review:
{stat_review}

Causal/design review:
{causal_review}

Reporting review:
{reporting_review}

Skeptical review:
{skeptic_review}

Benchmark / external validation context:
{benchmark_context}

False-positive guardrail:
- If the paper appears to be a randomized, controlled, blinded, peer-reviewed clinical trial with a clear primary outcome, sample size, effect estimates, and confidence intervals, do not assign High concern unless there is a clearly confirmed statistical flaw that directly threatens the primary conclusion.
- For well-reported randomized trials with a statistically supported primary outcome, default to Low concern or Low-to-Moderate concern.
- Do not label secondary outcome, subgroup, or exploratory-analysis concerns as “Major confirmed issue” unless they directly invalidate the primary conclusion or the authors make a strong unsupported claim from them.
- For well-designed RCTs with a statistically supported primary outcome, secondary endpoint concerns should usually be labeled “Moderate interpretation concern” or “Standard reviewer check.”
- Do not say “critical issues limit confidence” unless the issue directly threatens the primary conclusion.
- Do not treat missing protocol, supplement, or data-sharing details as confirmed flaws if they may appear outside the extracted paper text.
- Do not state that a paper is “suitable for practice guidance,” “ready for clinical use,” or “clinically actionable.”
- Instead, state what statistical conclusion appears supported and what findings require caution.
- Distinguish a limitation from a flaw. A limitation does not automatically mean the paper has a serious statistical problem.

Severity-label rules:
Use “Major confirmed issue” only when:
- the primary analysis is invalid,
- the main conclusion is unsupported,
- there is a direct internal inconsistency,
- key denominators/outcomes are impossible or contradictory,
- or the paper makes a strong claim that the reported data clearly do not support.

Use “Moderate interpretation concern” for:
- secondary outcomes with uncertain interpretation,
- subgroup findings without clear interaction evidence,
- non-primary endpoints that require cautious wording,
- or claims where the evidence is suggestive but not definitive.

Use “Standard reviewer check” for:
- routine requests for confidence intervals, assumptions, sensitivity analyses, subgroup details, or protocol/supplement verification.

Use “Reporting gap from extracted text” when:
- the extracted paper text does not show a detail, but it may appear in the protocol, supplement, appendix, or registry.

Use “Not assessable without supplement/protocol” when:
- the issue cannot be judged from the extracted manuscript text alone.

Final answer must use this exact structure:

# Whole-Paper Review Summary

1. Overall concern level
Choose one: Low concern, Low-to-Moderate concern, Moderate concern, High concern, or Cannot assess from extracted text.

2. Paper type and design
Briefly identify the study design and why it matters for review.

3. Big-picture statistical concerns
List the top concerns. For each concern, label it as one of:
- Major confirmed issue
- Moderate interpretation concern
- Standard reviewer check
- Reporting gap from extracted text
- Not assessable without supplement/protocol

Do not label a concern as “Major confirmed issue” unless it satisfies the severity-label rules above.

4. Internal consistency checks
List table/result checks the reviewer should perform, including n/group sizes, df, means/SDs, p-values, figures, and repeated datasets if relevant.

5. Reporting and transparency concerns
List missing or unclear reporting elements. Use cautious wording such as “not shown in the extracted text” or “should be verified in the supplement/protocol.”

6. Claims needing more cautious wording
List 2-4 claims/conclusions that may overstate the evidence.

7. Suggested reviewer comment
Write a polished reviewer-style comment. Do not mention acceptance, rejection, practice guidance, clinical readiness, or clinical actionability. Use cautious wording such as “the manuscript should clarify...” and “the provided text does not clearly show...”

8. Overall assessment for users
Briefly state what statistical conclusion appears supported and what findings require cautious interpretation. Do not say the paper is suitable for practice guidance or clinical use.

9. Relevant knowledge base sources
Mention only sources from retrieved context.

10. Limitation
State this is an AI-assisted whole-paper statistical screen based on text extracted from the uploaded paper and not a substitute for expert statistical/domain review.

Rules:
- Do not invent confidence intervals, p-values, sample sizes, effect sizes, or diagnostics.
- Do not calculate new statistics.
- If a concern needs external information, label it as external/benchmark context, not as something proven from the paper.
- If a concern is a check rather than confirmed, say it should be checked or verified.
- For well-reported RCTs, do not overstate routine reviewer checks as serious flaws.
- Be professional and concise.
- Entire response must be under 1000 words.
")

  final_text <- agent_chat(final_synthesizer_agent, final_prompt)
  
  list(
    final_text = final_text,
    paper_map = paper_map,
    retrieved_context = retrieved_context,
    agent_trace = list(
      paper_map = paper_map,
      stat_review = stat_review,
      causal_review = causal_review,
      reporting_review = reporting_review,
      skeptic_review = skeptic_review
    )
  )
}

# -----------------------------------------------------------------------------
# Backward-compatible functions
# -----------------------------------------------------------------------------

audit_claim <- function(
    claim,
    paper_type = "Auto-detect / unclear",
    review_mode = "Extract and audit statistical claims",
    show_retrieved_context = TRUE
) {
  result <- audit_peer_review(
    claim = claim,
    source_text = claim,
    paper_type = paper_type,
    review_mode = review_mode,
    top_n = 8,
    show_retrieved_context = show_retrieved_context
  )
  
  cat(result$final_text)
  invisible(result)
}

demo_audit <- function(example = c("vaccine", "odds", "causal", "prediction", "pvalue", "experiment")) {
  example <- match.arg(example)
  
  claim <- switch(
    example,
    vaccine = "A news article says a vaccine increases the risk of myocarditis by 50%, so the vaccine is dangerous.",
    odds = "Patients taking Drug A had an odds ratio of 2.0 for recovery, so patients were twice as likely to recover.",
    causal = "People who drink coffee have lower cancer rates, so coffee prevents cancer.",
    prediction = "The machine learning model achieved an AUC of 0.91, proving it is ready for clinical deployment.",
    pvalue = "Only statistically significant outcomes were discussed, and the authors conclude that the intervention works because p < 0.05.",
    experiment = "Participants eating from self-refilling bowls ate significantly more soup than those eating from normal bowls, representing a 73% increase in intake."
  )
  
  audit_claim(claim)
}

