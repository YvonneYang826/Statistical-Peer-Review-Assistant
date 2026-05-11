# =============================================================================
# app.R
# Statistical Peer Review Assistant
# =============================================================================
# Clean workflow:
#
#   Workflow A: Whole Paper Review
#   Upload PDF/TXT -> store full extracted text -> run audit_full_paper()
#
#   Workflow B: Claim / Paragraph Review
#   Paste text in chat -> store pasted text -> run audit_peer_review()
#
#   The app no longer extracts optional claims from uploaded full papers.
#
#   Evaluation / Robustness tab now includes:
#   - Test Set 1: Obvious Wrong Statistical Claims
#   - Test Set 2: Known Problem / Stress-Test Papers
#   - Test Set 3: Good Statistical Analysis / False-Positive Controls
#   - Test Set 4: Abstract-Only Review / Abstract False-Positive Check
#   - Simple 0-2 score and detailed 10-point score
# =============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(stringr)
library(markdown)
library(htmltools)
library(shinychat)
library(ellmer)
library(readr)
library(DT)

source("claim_auditor_team.R")

# -----------------------------------------------------------------------------
# Paths
# -----------------------------------------------------------------------------

resource_path <- "data/RAG Resource with URL.csv"
eval_path <- "data/evaluation_cases.csv"

if (!file.exists(resource_path)) {
  warning(
    "Resource file not found at data/RAG Resource with URL.csv. ",
    "Make sure the corrected CSV is inside your data/ folder."
  )
}

# -----------------------------------------------------------------------------
# Evaluation Test Set 1 Results
# -----------------------------------------------------------------------------

test_set1_results <- data.frame(
  Claim_ID = paste0("Claim ", 1:10),
  
  Claim_Type = c(
    "Correlation as causation",
    "Relative risk without absolute risk",
    "Odds ratio interpreted as risk ratio",
    "P-value means probability the null is true",
    "Non-significant means no effect",
    "Small sample overinterpretation",
    "Multiple testing / cherry-picking",
    "Missing denominator",
    "Simpson's paradox / aggregation issue",
    "Prediction model overclaim"
  ),
  
  Test_Claim = c(
    "People who drink more coffee have lower rates of depression, so coffee prevents depression.",
    "This medication doubles the risk of a rare side effect, so it is extremely dangerous.",
    "The odds ratio for recovery was 2.0, so patients were twice as likely to recover.",
    "Because p = 0.03, there is only a 3% chance that the null hypothesis is true.",
    "The treatment had no effect because the result was not statistically significant.",
    "We tested 8 patients and 6 improved, proving that the treatment works.",
    "We tested 30 outcomes and found 2 statistically significant results, so the intervention is effective.",
    "There were 10 serious adverse events in the vaccine group, so the vaccine is unsafe.",
    "Treatment A has a higher overall recovery rate than Treatment B, so it is better for all patients.",
    "Our model has 92% accuracy, so it is ready for clinical use."
  ),
  
  Expected_Flag = c(
    "Association is not causation; need temporality, confounding control, and reverse-causality assessment.",
    "Relative risk alone is insufficient; need baseline risk, absolute risk increase, denominator, and confidence interval.",
    "Odds ratio is not the same as risk ratio, especially when the outcome is common; need baseline recovery rate.",
    "A p-value is not the probability that the null hypothesis is true.",
    "Failure to reject the null does not prove no effect; need confidence interval, power, and equivalence/noninferiority logic.",
    "Small uncontrolled sample cannot prove efficacy; need control group, uncertainty, and replication.",
    "Multiple outcomes inflate false positives; need pre-specification and multiplicity adjustment.",
    "Raw adverse-event count is uninterpretable without denominator, time frame, comparison group, and causality assessment.",
    "Overall rate can hide subgroup reversals; need subgroup-specific rates and case-mix assessment.",
    "Accuracy alone is insufficient for clinical use; need validation, calibration, sensitivity/specificity, threshold, and utility."
  ),
  
  App_Verdict = c(
    "Misleading",
    "Misleading",
    "Incorrect",
    "Incorrect",
    "Incorrect",
    "Misleading",
    "Misleading",
    "Cannot assess from provided text",
    "Misleading",
    "Misleading"
  ),
  
  Simple_Score_0_2 = c(
    2, 2, 2, 2, 2, 2, 2, 1.5, 2, 2
  ),
  
  Detailed_Score_10 = c(
    9.0, 9.0, 9.5, 9.3, 9.0, 9.0, 9.5, 8.5, 9.0, 8.8
  ),
  
  Notes = c(
    "Correctly identified causal overclaim. Output is accurate but slightly long.",
    "Strong explanation of relative vs absolute risk. Safer wording could be simpler.",
    "Very strong. Correctly explained OR vs RR and baseline-rate dependence.",
    "Correct statistical explanation. Verdict 'Incorrect' is appropriate for direct statistical misdefinition.",
    "Correct issue and verdict. Still slightly over-includes broad study-design concerns.",
    "Correctly flags small sample, no control group, and overclaiming. Minor guardrail issue: approximate CI.",
    "Strong result. Correctly identifies multiplicity, expected false positives, and need for pre-specification.",
    "Correctly identifies missing denominator and comparison group. Verdict is conservative; 'Misleading' could also be justified.",
    "Correctly flags aggregate-to-all-patients overclaim and Simpson's paradox concern.",
    "Correctly flags accuracy-only overclaim. Slightly assumes training-set optimism, but main critique is correct."
  ),
  
  stringsAsFactors = FALSE
)


# -----------------------------------------------------------------------------
# Evaluation Test Set 2: Known Problem / Stress-Test Papers
# -----------------------------------------------------------------------------

test_set2_results <- data.frame(
  Case_ID = c(
    "TS2-01",
    "TS2-02",
    "TS2-03",
    "TS2-04"
  ),
  
  Paper = c(
    "Wansink Bottomless Bowls",
    "Reinhart & Rogoff: Growth in a Time of Debt",
    "LaCour & Green canvassing experiment",
    "MMR vaccination and autism epidemiology article"
  ),
  
  Test_Type = c(
    "Known-problem paper",
    "Known-problem paper",
    "Known-problem paper",
    "Known-problem paper"
  ),
  
  Expected_Behavior = c(
    "Flag reporting gaps, missing effect sizes/CIs, unclear outcome hierarchy, unsupported mechanism, and reproducibility concerns.",
    "Flag causal overclaiming from descriptive/correlational macroeconomic data, missing uncertainty, threshold sensitivity, and reproducibility/internal-consistency concerns.",
    "Flag multiplicity, attrition/missing data, clustering/model-specification gaps, unexplained effect trajectories, and reproducibility concerns.",
    "Flag observational-design limitations, uncontrolled multiplicity, unclear pre-specification, post-hoc explanation of a marginal finding, and undocumented confounding adjustment."
  ),
  
  App_Behavior = c(
    "Moderate concern. Correctly supported the direct experimental effect but flagged missing effect sizes/CIs, unclear randomization/group sizes, multiple outcomes without hierarchy, and unsupported mechanism.",
    "High concern. Correctly flagged causal claims from descriptive panel data, no CIs/p-values, possible post-hoc thresholds, reverse causality, and arithmetic/internal-consistency checks.",
    "High concern. Correctly flagged multiplicity, clustering uncertainty, attrition, unexplained effect amplification/decay, missing model details, and limited reproducibility details.",
    "Moderate concern. Correctly identified retrospective observational design, 14 tests without visible pre-specification/multiplicity adjustment, post-hoc dismissal of a marginal 0–5 month finding, and unclear confounding adjustment."
  ),
  
  Simple_Score_0_2 = c(
    1.5,
    2.0,
    1.5,
    1.75
  ),
  
  Detailed_Score_10 = c(
    8.0,
    9.0,
    8.0,
    8.5
  ),
  
  Notes = c(
    "Good paper-internal screen. It cannot fully detect cross-paper Wansink/Food and Brand Lab irregularities without external benchmark context.",
    "Strong stress-test result. Correctly identified the main statistical and causal weaknesses.",
    "Useful stress-test result. Correctly flags many paper-internal issues, but external data-fabrication concerns require external benchmark context.",
    "Good observational-study stress test. The app correctly avoided claiming the study proves harm, but flagged limitations in causal inference, multiplicity, and post-hoc artifact explanation."
  ),
  
  stringsAsFactors = FALSE
)


# -----------------------------------------------------------------------------
# Evaluation Test Set 3: Good Statistical Analysis / False-Positive Controls
# -----------------------------------------------------------------------------

test_set3_results <- data.frame(
  Case_ID = c(
    "TS3-01",
    "TS3-02",
    "TS3-03",
    "TS3-04"
  ),
  
  Paper = c(
    "Randomized Trial of Early Detection and Treatment of Postpartum Hemorrhage",
    "Phase 3 Trial of Sotatercept for Pulmonary Arterial Hypertension",
    "Cardiovascular and Renal Outcomes with Empagliflozin in Heart Failure",
    "Polatuzumab Vedotin in Previously Untreated Diffuse Large B-Cell Lymphoma"
  ),
  
  Test_Type = c(
    "Good-paper false-positive control",
    "Good-paper false-positive control",
    "Good-paper false-positive control",
    "Good-paper false-positive control"
  ),
  
  Expected_Behavior = c(
    "Recognize strong cluster-RCT design and supported primary outcome; raise only standard cluster, secondary outcome, and generalizability checks.",
    "Recognize strong phase 3 RCT design and supported primary endpoint; treat secondary endpoint/unblinding/generalizability points as cautious checks.",
    "Recognize large double-blind RCT and supported primary composite endpoint; separate primary result from subgroup, composite-outcome, and safety-reporting checks.",
    "Recognize phase 3 RCT and supported primary PFS endpoint; flag OS, subgroup, and safety interpretation as cautions rather than major flaws."
  ),
  
  App_Behavior = c(
    "Low-to-Moderate concern. Correctly supported the primary outcome and limited concerns to multiplicity, ICC/design effect, source verification, composite components, and generalizability.",
    "Low-to-Moderate concern. Correctly supported the primary 6-minute walk distance endpoint and treated secondary outcomes/unblinding as cautious interpretation checks.",
    "Low-to-Moderate concern. Correctly supported the composite primary endpoint and flagged component interpretation, subgroup heterogeneity, safety counts, and interim-analysis transparency.",
    "Low-to-Moderate concern. Correctly supported PFS improvement but raised appropriate caution about lack of OS benefit, exploratory subgroups, and adverse event differences."
  ),
  
  Simple_Score_0_2 = c(
    2.0,
    2.0,
    1.75,
    1.75
  ),
  
  Detailed_Score_10 = c(
    8.8,
    8.5,
    8.3,
    8.3
  ),
  
  Notes = c(
    "Successful false-positive control. Good separation of supported primary outcome from secondary checks.",
    "Successful false-positive control. Good separation of primary and secondary endpoints.",
    "Mostly successful. Some subgroup/generalizability wording could be softened.",
    "Mostly successful. Avoid saying the primary endpoint is 'borderline' when it met pre-specified criteria."
  ),
  
  stringsAsFactors = FALSE
)

# -----------------------------------------------------------------------------
# Evaluation Test Set 4: Abstract-Only Review / Abstract False-Positive Check
# -----------------------------------------------------------------------------

test_set4_results <- data.frame(
  Case_ID = c(
    "TS4-01",
    "TS4-02",
    "TS4-03",
    "TS4-04",
    "TS4-05",
    "TS4-06",
    "TS4-07"
  ),
  
  Paper = c(
    "Wansink Bottomless Bowls abstract",
    "Reinhart & Rogoff abstract",
    "LaCour & Green abstract",
    "Postpartum hemorrhage RCT abstract",
    "Sotatercept PAH trial abstract",
    "Empagliflozin heart failure trial abstract",
    "Polatuzumab DLBCL trial abstract"
  ),
  
  Test_Type = c(
    "Problematic-paper abstract",
    "Problematic-paper abstract",
    "Problematic-paper abstract",
    "Good-paper abstract false-positive control",
    "Good-paper abstract false-positive control",
    "Good-paper abstract false-positive control",
    "Good-paper abstract false-positive control"
  ),
  
  Expected_Behavior = c(
    "Flag missing uncertainty, unsupported secondary null findings, BMI moderation reporting gaps, and cautious interpretation from abstract-only evidence.",
    "Flag causal overclaiming, threshold uncertainty, missing statistical inference, confounding, and reverse causality.",
    "Flag missing quantitative effect sizes/CIs, missing interaction testing, clustering, persistence/spillover uncertainty, and replication-detail limitations.",
    "Recognize strong cluster-RCT abstract and support the primary result; ask for component breakdown and missing-data checks without treating abstract omissions as fatal flaws.",
    "Recognize supported primary endpoint and treat secondary endpoint/multiplicity and safety details as full-paper verification checks.",
    "Recognize strong RCT abstract with HR/CI/p-value; flag composite outcome decomposition and subgroup consistency as interpretation checks.",
    "Recognize supported PFS result while flagging OS/PFS discordance and safety interpretation as cautions rather than major flaws."
  ),
  
  App_Behavior = c(
    "Questionable. Correctly flagged missing CIs/effect sizes, unsupported secondary null outcomes, and missing BMI interaction test.",
    "Misleading. Correctly flagged observational causal language, unverified thresholds, missing CIs/regression results, confounding, and reverse causality.",
    "Misleading. Correctly flagged qualitative effect language, missing effect sizes/CIs, missing interaction testing, clustering, and multiplicity concerns.",
    "Supported. Correctly supported the composite primary outcome and framed component breakdown, baseline comparability, and missing data as checks.",
    "Supported. Correctly supported the primary endpoint and treated hierarchical secondary testing and safety details as full-paper verification checks.",
    "Questionable. Mostly appropriate: flagged composite outcome decomposition, diabetes subgroup interaction, and absolute benefit context; slightly cautious but no longer severe.",
    "Questionable. Mostly appropriate: flagged PFS/OS discordance and uncertainty; slightly strong but useful for oncology interpretation."
  ),
  
  Simple_Score_0_2 = c(
    1.75,
    2.00,
    1.75,
    2.00,
    2.00,
    1.75,
    1.75
  ),
  
  Detailed_Score_10 = c(
    8.5,
    9.0,
    8.3,
    9.0,
    9.0,
    8.2,
    8.2
  ),
  
  Notes = c(
    "Good abstract-level sensitivity. It appropriately avoids calling the primary intake result unsupported, but flags unreported secondary and moderation statistics.",
    "Strong problematic-abstract detection. Correctly identifies causal and threshold-reporting issues.",
    "Good problematic-abstract detection. Slightly harsh, but appropriate because the abstract uses strong qualitative claims without effect estimates.",
    "Successful abstract false-positive control after revision. The verdict is Supported and remaining items are framed as full-paper checks.",
    "Successful abstract false-positive control after revision. The primary endpoint is supported; secondary multiplicity details are framed as verification items.",
    "Mostly successful. Could be Supported with caution rather than Questionable, but the critique is useful and not overly severe.",
    "Mostly successful. Correctly identifies PFS/OS caution, but should avoid overstating OS as always required for benefit."
  ),
  
  stringsAsFactors = FALSE
)

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

load_resource_for_display <- function() {
  if (!file.exists(resource_path)) {
    return(data.frame(
      message = "Resource file not found. Expected: data/RAG Resource with URL.csv"
    ))
  }
  
  resource_df <- readr::read_csv(resource_path, show_col_types = FALSE)
  names(resource_df) <- stringr::str_trim(names(resource_df))
  
  if (!all(c("id", "Title", "url") %in% names(resource_df))) {
    return(data.frame(
      message = "Resource file must contain columns: id, Title, url"
    ))
  }
  
  resource_df |>
    dplyr::mutate(
      id = as.character(id),
      Title = as.character(Title),
      url = as.character(url),
      url = stringr::str_trim(url),
      url_full = dplyr::if_else(
        stringr::str_detect(url, "^https?://"),
        url,
        paste0("https://", url)
      ),
      Link = paste0(
        "<a href='",
        htmltools::htmlEscape(url_full),
        "' target='_blank'>Open source</a>"
      )
    ) |>
    dplyr::select(id, Title, url_full, Link)
}

load_eval_cases <- function(path = eval_path) {
  if (!file.exists(path)) {
    return(data.frame(
      case_id = character(),
      input_text_or_claim = character(),
      paper_type = character(),
      review_mode = character(),
      known_critique_source = character(),
      known_issue = character(),
      expected_flag = character(),
      app_flag = character(),
      score = numeric(),
      notes = character()
    ))
  }
  
  readr::read_csv(path, show_col_types = FALSE)
}

extract_text_from_upload <- function(file_info) {
  if (is.null(file_info)) {
    stop("No file uploaded.")
  }
  
  ext <- tolower(tools::file_ext(file_info$name))
  
  if (ext == "txt") {
    text <- readr::read_file(file_info$datapath)
    
    if (nchar(text) < 20) {
      stop("The TXT file appears to contain too little text.")
    }
    
    return(text)
  }
  
  if (ext == "pdf") {
    if (!requireNamespace("pdftools", quietly = TRUE)) {
      stop("Package 'pdftools' is required for PDF upload. Install it with install.packages('pdftools').")
    }
    
    pages <- tryCatch(
      {
        suppressWarnings(pdftools::pdf_text(file_info$datapath))
      },
      error = function(e) {
        stop(
          "PDF text extraction failed. This PDF may be encrypted, corrupted, scanned/image-based, or malformed. ",
          "Try downloading the PDF again, opening it in Preview/Adobe and exporting/saving a new copy, or converting it to TXT first. ",
          "Original error: ",
          e$message
        )
      }
    )
    
    text <- paste(pages, collapse = "\n\n")
    text <- stringr::str_replace_all(text, "\r\n|\r", "\n")
    text <- stringr::str_replace_all(text, "[ \t]+", " ")
    text <- stringr::str_replace_all(text, "\n{3,}", "\n\n")
    text <- stringr::str_trim(text)
    
    if (nchar(text) < 100) {
      stop(
        "The PDF text could not be extracted well. It may be scanned/image-based or malformed. ",
        "Try converting the PDF to TXT or uploading a cleaner PDF."
      )
    }
    
    return(text)
  }
  
  stop("Unsupported file type. Please upload a PDF or TXT file.")
}

split_review_output <- function(text) {
  text <- as.character(text)
  text <- paste(text, collapse = "\n")
  
  quick_pattern <- "#+\\s*Quick Review Summary"
  full_pattern <- "#+\\s*Full Reviewer Details"
  whole_pattern <- "#+\\s*Whole-Paper Review Summary"
  
  if (stringr::str_detect(text, stringr::regex(whole_pattern, ignore_case = TRUE))) {
    return(list(
      quick = text,
      full = ""
    ))
  }
  
  has_quick <- stringr::str_detect(
    text,
    stringr::regex(quick_pattern, ignore_case = TRUE)
  )
  
  has_full <- stringr::str_detect(
    text,
    stringr::regex(full_pattern, ignore_case = TRUE)
  )
  
  if (has_quick && has_full) {
    parts <- stringr::str_split(
      text,
      stringr::regex(full_pattern, ignore_case = TRUE),
      n = 2
    )[[1]]
    
    quick <- parts[1]
    full <- paste0("# Full Reviewer Details\n", parts[2])
    
    return(list(
      quick = quick,
      full = full
    ))
  }
  
  list(
    quick = text,
    full = ""
  )
}

render_markdown_fragment <- function(text) {
  HTML(markdown::markdownToHTML(
    text = text,
    fragment.only = TRUE
  ))
}

is_command_like <- function(text) {
  text <- stringr::str_to_lower(stringr::str_trim(text))
  
  stringr::str_detect(
    text,
    "^(run|start|analyze|analyse|review|do|begin|go|full paper|whole paper|paper analysis|full analysis)"
  ) &&
    nchar(text) < 120
}

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------

ui <- page_navbar(
  title = "Statistical Peer Review Assistant",
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1E5AA8",
    secondary = "#5DADE2",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  
  header = tags$head(
    tags$style(HTML("
      body {
        background-color: #F5F9FF;
        color: #1F2D3D;
      }

      .app-title {
        background: linear-gradient(135deg, #0B3D91, #1E88E5);
        color: white;
        padding: 22px 28px;
        border-radius: 18px;
        margin-top: 18px;
        margin-bottom: 20px;
        box-shadow: 0 8px 24px rgba(30, 90, 168, 0.20);
      }

      .app-title h1 {
        font-weight: 750;
        margin-bottom: 8px;
      }

      .app-title p {
        font-size: 1.02rem;
        margin-bottom: 0;
        opacity: 0.95;
      }

      .blue-card {
        background-color: white;
        border: 1px solid #D8E8FF;
        border-radius: 18px;
        box-shadow: 0 6px 18px rgba(30, 90, 168, 0.08);
        padding: 20px;
        margin-bottom: 18px;
        overflow: hidden;
      }

      .review-settings-card {
        padding-bottom: 34px;
        margin-bottom: 22px;
        overflow: visible;
      }

      .review-settings-inputs {
        margin-bottom: 52px;
      }

      .review-settings-note {
        margin-top: 8px;
      }

      .section-label {
        color: #0B3D91;
        font-weight: 700;
        font-size: 1.05rem;
        margin-bottom: 12px;
      }

      .btn-primary {
        background-color: #1E5AA8 !important;
        border-color: #1E5AA8 !important;
        border-radius: 12px;
        font-weight: 650;
        padding: 10px 18px;
      }

      .btn-primary:hover {
        background-color: #0B3D91 !important;
        border-color: #0B3D91 !important;
      }

      .btn-outline-primary {
        border-radius: 12px;
        font-weight: 650;
      }

      textarea.form-control {
        border-radius: 14px;
        border: 1px solid #BFD7F5;
      }

      textarea.form-control:focus,
      input.form-control:focus,
      select.form-select:focus {
        border-color: #1E88E5;
        box-shadow: 0 0 0 0.2rem rgba(30, 136, 229, 0.20);
      }

      .small-note {
        color: #5F6C7B;
        font-size: 0.92rem;
      }

      .warning-box {
        background-color: #EAF4FF;
        border-left: 5px solid #1E88E5;
        padding: 13px 15px;
        border-radius: 10px;
        color: #1F2D3D;
        margin-bottom: 16px;
      }

      .success-box {
        background-color: #F0F7FF;
        border-left: 5px solid #0B3D91;
        padding: 13px 15px;
        border-radius: 10px;
        color: #1F2D3D;
        margin-bottom: 16px;
      }

      .audit-output {
        background-color: #FFFFFF;
        border-radius: 14px;
        padding: 16px;
        border: 1px solid #D8E8FF;
        line-height: 1.55;
      }

      .audit-output h1,
      .audit-output h2,
      .audit-output h3 {
        color: #0B3D91;
      }

      .upload-box {
        background-color: #F0F7FF;
        border: 1px solid #C7DFFF;
        border-radius: 14px;
        padding: 14px 16px;
        margin-bottom: 10px;
      }

      .upload-box .form-label {
        color: #0B3D91;
        font-weight: 650;
      }

      .left-chat-card {
        min-height: auto;
      }

      .right-workspace {
        max-height: none;
        overflow-y: visible;
        padding-right: 0;
      }

      .process-log {
        background-color: #F8FBFF;
        border: 1px solid #D8E8FF;
        border-radius: 14px;
        padding: 14px 16px;
        min-height: 210px;
        max-height: 340px;
        overflow-y: auto;
        line-height: 1.55;
      }

      .process-log h1,
      .process-log h2,
      .process-log h3 {
        color: #0B3D91;
        font-size: 1rem;
        margin-top: 0.7rem;
      }

      .process-log p {
        margin-bottom: 0.6rem;
      }

      .paper-map-box {
        background-color: #F8FBFF;
        border: 1px solid #D8E8FF;
        border-radius: 14px;
        padding: 14px 16px;
        line-height: 1.55;
      }

      .score-card {
        background-color: #F8FBFF;
        border: 1px solid #D8E8FF;
        border-radius: 14px;
        padding: 14px 16px;
        margin-bottom: 14px;
      }

      details {
        background-color: #FFFFFF;
        border-radius: 12px;
      }

      summary {
        font-weight: 700;
        color: #0B3D91;
        cursor: pointer;
      }
    "))
  ),
  
  nav_panel(
    "Review Assistant",
    
    div(
      class = "app-title",
      h1("Statistical Peer Review Assistant"),
      p("Choose one workflow: upload a full paper for whole-paper review, or paste a claim/paragraph for focused review.")
    ),
    
    layout_columns(
      col_widths = c(5, 7),
      
      div(
        class = "blue-card left-chat-card",
        
        div(class = "section-label", "Manuscript Input"),
        
        div(
          class = "small-note",
          "Use PDF/TXT upload for whole-paper review. Use the chat box for a single claim, paragraph, abstract, table text, or news statement."
        ),
        
        br(),
        
        div(
          class = "upload-box",
          
          uiOutput("paper_upload_ui"),
          
          actionButton(
            inputId = "load_uploaded_file",
            label = "Load uploaded paper",
            class = "btn-outline-primary"
          )
        ),
        
        br(),
        
        shinychat::chat_ui(
          id = "source_chat",
          messages = list(
            list(
              role = "assistant",
              content = paste(
                "Hi! Upload a full paper for whole-paper review,",
                "or paste a single claim/paragraph here for focused review."
              )
            )
          ),
          placeholder = "Paste a claim, paragraph, abstract, or table text...",
          height = "460px",
          width = "100%"
        ),
        
        br(),
        
        actionButton(
          inputId = "new_review",
          label = "Start new review",
          class = "btn-outline-primary"
        )
      ),
      
      div(
        class = "right-workspace",
        
        div(
          class = "blue-card review-settings-card",
          
          div(class = "section-label", "Review Settings"),
          
          div(
            class = "review-settings-inputs",
            layout_columns(
              col_widths = c(6, 6),
              
              div(
                class = "mb-3",
                tags$label("Paper type", class = "form-label"),
                div(
                  class = "form-control",
                  style = "background-color: #F8FBFF;",
                  "Auto-detect / unclear"
                )
              ),
              
              selectInput(
                inputId = "review_mode",
                label = "Review mode",
                choices = c(
                  "Whole-paper statistical peer review",
                  "Claim / paragraph statistical review"
                ),
                selected = "Whole-paper statistical peer review"
              )
            )
          ),
          
          div(
            class = "small-note review-settings-note",
            "The app will show the correct review action after you upload a paper or paste text."
          )
        ),
        
        conditionalPanel(
          condition = "output.hasUploadedPaper",
          div(
            class = "blue-card",
            
            div(class = "section-label", "Whole Paper Review"),
            
            div(
              class = "success-box",
              strong("Paper loaded. "),
              "Click the button below to review the full manuscript structure, methods, results, tables, and conclusions."
            ),
            
            actionButton(
              inputId = "run_whole_paper_review",
              label = "Run whole-paper review",
              class = "btn-primary"
            )
          )
        ),
        
        conditionalPanel(
          condition = "output.hasPastedText",
          div(
            class = "blue-card",
            
            div(class = "section-label", "Claim / Paragraph Review"),
            
            div(
              class = "success-box",
              strong("Text loaded. "),
              "Click the button below to review the pasted claim, paragraph, abstract, or table text."
            ),
            
            textAreaInput(
              inputId = "claim_or_paragraph_to_review",
              label = "Text to review",
              value = "",
              rows = 6,
              placeholder = "Paste or edit the claim/paragraph here."
            ),
            
            actionButton(
              inputId = "run_claim_review",
              label = "Run claim/paragraph review",
              class = "btn-primary"
            )
          )
        ),
        
        conditionalPanel(
          condition = "!output.hasUploadedPaper && !output.hasPastedText",
          div(
            class = "blue-card",
            div(class = "section-label", "Next Step"),
            div(
              class = "warning-box",
              "Upload a full paper on the left, or paste a claim/paragraph into the chat box."
            )
          )
        ),
        
        div(
          class = "blue-card",
          
          div(class = "section-label", "Agent Process"),
          
          div(
            class = "process-log",
            uiOutput("agent_process_display")
          )
        ),
        
        conditionalPanel(
          condition = "output.hasPaperMap",
          div(
            class = "blue-card",
            tags$details(
              tags$summary("Show paper map"),
              br(),
              div(
                class = "paper-map-box",
                uiOutput("paper_map_display")
              )
            )
          )
        ),
        
        div(
          class = "blue-card",
          
          div(class = "section-label", "Review Result"),
          
          uiOutput("audit_status_message"),
          
          conditionalPanel(
            condition = "output.hasAuditResult",
            
            div(
              class = "audit-output",
              uiOutput("quick_review_result")
            ),
            
            conditionalPanel(
              condition = "output.hasFullDetails",
              br(),
              tags$details(
                tags$summary("Show full reviewer details"),
                br(),
                div(
                  class = "audit-output",
                  uiOutput("full_review_result")
                )
              )
            )
          ),
          
          conditionalPanel(
            condition = "!output.hasAuditResult",
            div(
              class = "small-note",
              "Your review result will appear here."
            )
          )
        ),
        
        conditionalPanel(
          condition = "output.hasAuditResult",
          div(
            class = "blue-card",
            tags$details(
              tags$summary("Show retrieved RAG context"),
              br(),
              tableOutput("kb_table")
            )
          )
        )
      )
    )
  ),
  
  nav_panel(
    "Knowledge Base",
    
    div(
      class = "blue-card",
      h3("RAG Resource List"),
      
      p("This table uses the corrected resource URL CSV you provided: data/RAG Resource with URL.csv."),
      
      p("These are the sources used to document the knowledge base."),
      
      DTOutput("resource_display")
    )
  ),
  
  nav_panel(
    "Evaluation / Robustness",

    div(
      class = "blue-card",
      h3("Evaluation and Robustness Check"),

      p("This tab documents benchmark tests used to evaluate whether the app detects known statistical problems while avoiding false-positive over-criticism of well-reported statistical papers."),

      div(
        class = "score-card",
        h4("Scoring system"),

        p("The simple benchmark score is used for concise reporting:"),

        tags$ul(
          tags$li(strong("0 = "), "missed the expected issue or produced a misleading false positive"),
          tags$li(strong("1 = "), "partially identified the issue or partially over-criticized the paper"),
          tags$li(strong("2 = "), "correctly identified the issue or appropriately avoided false-positive severity")
        ),

        p("A more detailed 10-point score is recorded for internal review. The 10-point score considers verdict/severity accuracy, issue detection, explanation quality, reviewer usefulness, and user-friendliness.")
      ),

      h4("Overall Evaluation Summary"),

      tableOutput("overall_eval_summary"),

      br(),

      h4("Test Set 1: Obvious Wrong Statistical Claims"),

      p("This claim-level test set uses short claims with known statistical flaws. Each claim was pasted into the Claim / Paragraph Review workflow and compared with the expected issue."),

      tableOutput("test_set1_summary"),

      br(),

      DTOutput("test_set1_display"),

      br(),

      tags$details(
        tags$summary("Show Test Set 1 interpretation"),
        br(),
        div(
          class = "audit-output",
          p("Test Set 1 evaluates whether the app can detect obvious claim-level reasoning errors, such as p-value misinterpretation, odds ratio/risk ratio confusion, missing denominator, multiple testing, and prediction-model overclaiming."),
          p("The app performed well overall. The main remaining weakness is output style: some answers are longer than necessary or include secondary issues that are not central to the simple claim.")
        )
      ),

      br(),

      h4("Test Set 2: Known Problem / Stress-Test Papers"),

      p("This whole-paper test set uses papers known to contain or be associated with statistical, reporting, reproducibility, or interpretive concerns. The goal is to check whether the app flags serious issues when the paper should receive a critical review."),

      tableOutput("test_set2_summary"),

      br(),

      DTOutput("test_set2_display"),

      br(),

      tags$details(
        tags$summary("Show Test Set 2 interpretation"),
        br(),
        div(
          class = "audit-output",
          p("Test Set 2 checks sensitivity. The app should identify major concerns in problematic papers rather than giving overly reassuring reviews."),
          p("The app performed best on Reinhart & Rogoff, where it correctly flagged causal overclaiming, missing statistical uncertainty, threshold sensitivity, and reverse causality. For Wansink and LaCour & Green, the app produced useful paper-internal critiques, but external benchmark context is still needed for cross-paper irregularities or data-fabrication concerns.")
        )
      ),

      br(),

      h4("Test Set 3: Good Statistical Analysis / False-Positive Controls"),

      p("This whole-paper test set uses well-reported statistical papers, mostly randomized trials with clear primary endpoints. The goal is to check whether the app avoids falsely labeling strong statistical work as seriously flawed."),

      tableOutput("test_set3_summary"),

      br(),

      DTOutput("test_set3_display"),

      br(),

      tags$details(
        tags$summary("Show Test Set 3 interpretation"),
        br(),
        div(
          class = "audit-output",
          p("Test Set 3 checks specificity. The app should recognize strong primary analyses and limit its comments to standard reviewer checks, secondary-outcome cautions, subgroup interpretation, and transparency questions."),
          p("After the severity-calibration revision, the app performed well on good-paper controls. It generally gave Low-to-Moderate concern, supported the primary endpoint, and avoided treating routine reviewer checks as major confirmed flaws."),
          p("Remaining minor issue: the app can still sound slightly too critical for some RCTs, especially when discussing subgroup heterogeneity, baseline imbalances, or primary endpoints that are statistically significant but close to the threshold.")
        )
      ),

      br(),
      
      h4("Test Set 4: Abstract-Only Review"),
      
      p("This test set evaluates how the app behaves when only article abstracts are pasted into the Claim / Paragraph Review workflow. The goal is to check whether the app can flag problematic abstracts while avoiding false positives for well-written RCT abstracts."),
      
      tableOutput("test_set4_summary"),
      
      br(),
      
      DTOutput("test_set4_display"),
      
      br(),
      
      tags$details(
        tags$summary("Show Test Set 4 interpretation"),
        br(),
        div(
          class = "audit-output",
          p("Test Set 4 was repeated after adding the abstract-review guardrail. The revised app performs much better: good RCT abstracts are no longer labeled as misleading solely because the abstract omits full-paper details such as CONSORT diagrams, baseline tables, SAP details, ICC/design effect, or full adverse-event tables."),
          p("The app now appropriately supports the postpartum hemorrhage and sotatercept abstracts while still asking useful verification questions. It also continues to flag problematic abstracts such as Reinhart & Rogoff and LaCour & Green."),
          p("Remaining limitation: for some good RCT abstracts, such as empagliflozin and polatuzumab, the app still uses Questionable when Supported with caution may be more appropriate. This is a minor calibration issue rather than a major failure.")
        )
      )
    )
  ),
  
  nav_panel(
    "About / Methods",
    
    div(
      class = "blue-card",
      
      h3("About This App"),
      
      p("This app is an AI-assisted statistical peer-review tool for manuscripts and statistical claims."),
      
      h4("Two separate workflows"),
      tags$ol(
        tags$li(strong("Whole Paper Review: "), "upload a PDF/TXT paper, then run a full-paper statistical screen."),
        tags$li(strong("Claim / Paragraph Review: "), "paste one claim, paragraph, abstract, or table text for focused review.")
      ),
      
      h4("Whole-paper review checks"),
      tags$ul(
        tags$li("study design and sample structure"),
        tags$li("group sizes, tables, figures, and result consistency"),
        tags$li("primary vs secondary outcome clarity"),
        tags$li("multiplicity and outcome switching concerns"),
        tags$li("causal/mechanistic language"),
        tags$li("reporting, transparency, and reproducibility")
      ),
      
      h4("Claim/paragraph review checks"),
      tags$ul(
        tags$li("p-value and inference interpretation"),
        tags$li("relative vs absolute effect communication"),
        tags$li("odds ratio / risk ratio / hazard ratio interpretation"),
        tags$li("confounding or causal overclaiming"),
        tags$li("missing denominators, uncertainty, or comparison groups")
      ),
      
      h4("Guardrails"),
      tags$ul(
        tags$li("The app does not determine whether a scientific claim is ultimately true or false."),
        tags$li("The app does not replace expert statistical or domain review."),
        tags$li("The app should not invent confidence intervals, p-values, sample sizes, effect sizes, biomedical rates, causal facts, or citations."),
        tags$li("Outputs are first-pass peer-review support, not journal accept/reject decisions."),
        tags$li("When only partial text is provided, the review is limited to that text.")
      ),
      
      h4("Robustness plan"),
      p("The app should be evaluated on benchmark papers or claims with known expert critiques. For each case, compare whether the app flags the expected issue, partially flags it, or misses it."),
      
      h4("Current evaluation"),
      p("The evaluation now includes four benchmark sets: obvious wrong statistical claims, known-problem full papers, good-paper full-text false-positive controls, and abstract-only review checks. Together, these tests evaluate both sensitivity to statistical problems and specificity against over-criticizing well-reported papers.")
    )
  )
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------

server <- function(input, output, session) {
  
  source_text_store <- reactiveVal("")
  upload_reset_token <- reactiveVal(0)
  input_kind <- reactiveVal("none")
  audit_result <- reactiveVal(NULL)
  audit_running <- reactiveVal(FALSE)
  followup_client <- reactiveVal(NULL)
  
  agent_process_log <- reactiveVal(c(
    "### Ready\nAgent progress will appear here after you start a review."
  ))
  
  add_process_log <- function(message) {
    current <- agent_process_log()
    agent_process_log(c(current, message))
  }
  
  reset_process_log <- function() {
    agent_process_log(c(
      "### Ready\nAgent progress will appear here after you start a review."
    ))
  }
  
  reset_all <- function() {
    source_text_store("")
    input_kind("none")
    audit_result(NULL)
    audit_running(FALSE)
    followup_client(NULL)
    reset_process_log()
    upload_reset_token(upload_reset_token() + 1)
    
    updateTextAreaInput(session, "claim_or_paragraph_to_review", value = "")
    updateSelectInput(session, "review_mode", selected = "Whole-paper statistical peer review")
  }
  
  # ---------------------------------------------------------------------------
  # Upload UI reset
  # ---------------------------------------------------------------------------
  
  output$paper_upload_ui <- renderUI({
    upload_reset_token()

    fileInput(
      inputId = "paper_upload",
      label = "Upload full paper PDF or TXT",
      accept = c(".pdf", ".txt"),
      buttonLabel = "Browse...",
      placeholder = "No file selected"
    )
  })
  
  # ---------------------------------------------------------------------------
  # Reset
  # ---------------------------------------------------------------------------
  
  observeEvent(input$new_review, {
    reset_all()
    
    shinychat::chat_clear("source_chat", session = session)
    shinychat::chat_append(
      "source_chat",
      paste(
        "New review started.",
        "Upload a full paper for whole-paper review,",
        "or paste a single claim/paragraph for focused review."
      ),
      session = session
    )
  })
  
  # ---------------------------------------------------------------------------
  # Upload full paper
  # ---------------------------------------------------------------------------
  
  observeEvent(input$load_uploaded_file, {
    req(input$paper_upload)
    
    text <- tryCatch(
      extract_text_from_upload(input$paper_upload),
      error = function(e) {
        showNotification(e$message, type = "error", duration = 10)
        NULL
      }
    )
    
    if (is.null(text)) return(NULL)
    
    source_text_store(text)
    input_kind("uploaded_paper")
    audit_result(NULL)
    followup_client(NULL)
    reset_process_log()
    
    updateSelectInput(session, "review_mode", selected = "Whole-paper statistical peer review")
    updateTextAreaInput(session, "claim_or_paragraph_to_review", value = "")
    
    shinychat::chat_append(
      "source_chat",
      paste0(
        "Uploaded paper loaded. I extracted text from the full file.\n\n",
        "Next step: click **Run whole-paper review** on the right. ",
        "This will analyze the manuscript structure, methods, results, tables, and conclusions."
      ),
      session = session
    )
  })
  
  # ---------------------------------------------------------------------------
  # Chat input: pasted claim/paragraph before review; follow-up after review
  # ---------------------------------------------------------------------------
  
  observeEvent(input$source_chat_user_input, {
    user_message <- trimws(input$source_chat_user_input)
    
    if (nchar(user_message) < 3) {
      return(NULL)
    }
    
    # After review: follow-up Q&A
    if (!is.null(audit_result())) {
      req(followup_client())
      
      stream <- followup_client()$stream_async(user_message)
      
      shinychat::chat_append(
        "source_chat",
        stream,
        session = session
      )
      
      return(NULL)
    }
    
    # If a paper has already been uploaded and user types a command, do not treat as new manuscript text
    if (input_kind() == "uploaded_paper" && is_command_like(user_message)) {
      shinychat::chat_append(
        "source_chat",
        "I already have the uploaded paper. Please click **Run whole-paper review** on the right to start the full-paper analysis.",
        session = session
      )
      return(NULL)
    }
    
    # Otherwise: pasted text workflow
    source_text_store(user_message)
    input_kind("pasted_text")
    audit_result(NULL)
    followup_client(NULL)
    reset_process_log()
    
    updateSelectInput(session, "review_mode", selected = "Claim / paragraph statistical review")
    updateTextAreaInput(session, "claim_or_paragraph_to_review", value = user_message)
    
    shinychat::chat_append(
      "source_chat",
      paste0(
        "Text loaded for claim/paragraph review.\n\n",
        "Next step: click **Run claim/paragraph review** on the right."
      ),
      session = session
    )
  })
  
  # ---------------------------------------------------------------------------
  # Run whole-paper review
  # ---------------------------------------------------------------------------
  
  observeEvent(input$run_whole_paper_review, {
    req(source_text_store())
    
    if (input_kind() != "uploaded_paper") {
      showNotification("Please upload a PDF/TXT paper first.", type = "error")
      return(NULL)
    }
    
    if (nchar(trimws(source_text_store())) < 50) {
      showNotification("The uploaded paper text is too short to review.", type = "error")
      return(NULL)
    }
    
    audit_running(TRUE)
    audit_result(NULL)
    followup_client(NULL)
    
    agent_process_log(c(
      "### Starting whole-paper review\nThe app is preparing to map and review the full manuscript."
    ))
    
    shinychat::chat_append(
      "source_chat",
      "I’ll run a whole-paper statistical review now. This may take longer because I need to map the paper sections and review the big-picture design, results, tables, and conclusions.",
      session = session
    )
    
    result <- withProgress(
      message = "Running whole-paper statistical review...",
      value = 0,
      {
        incProgress(0.12, detail = "Building paper map...")
        add_process_log(
          "### Step 1 — Paper Mapper\nBuilding a section-by-section map of the full paper."
        )
        Sys.sleep(0.2)
        
        incProgress(0.15, detail = "Retrieving RAG context...")
        add_process_log(
          "### Step 2 — RAG Retriever\nRetrieving relevant statistical-review sources for the whole-paper critique."
        )
        Sys.sleep(0.2)
        
        incProgress(0.18, detail = "Consulting statistical reviewer...")
        add_process_log(
          "### Step 3 — Statistical Reviewer\nChecking design, outcomes, tables, group sizes, effect reporting, multiplicity, and internal consistency."
        )
        Sys.sleep(0.2)
        
        incProgress(0.16, detail = "Consulting causal/design reviewer...")
        add_process_log(
          "### Step 4 — Causal / Design Reviewer\nChecking whether causal or mechanistic claims are supported by the study design."
        )
        Sys.sleep(0.2)
        
        incProgress(0.16, detail = "Consulting reporting reviewer...")
        add_process_log(
          "### Step 5 — Reporting Reviewer\nChecking transparency, assumptions, reproducibility, and missing reporting elements."
        )
        Sys.sleep(0.2)
        
        incProgress(0.13, detail = "Consulting skeptical reviewer...")
        add_process_log(
          "### Step 6 — Skeptical Reviewer\nSeparating confirmed issues from checks that need verification."
        )
        Sys.sleep(0.2)
        
        incProgress(0.10, detail = "Synthesizing whole-paper review...")
        add_process_log(
          "### Step 7 — Final Synthesizer\nCombining the agents' findings into a whole-paper review summary."
        )
        
        out <- tryCatch(
          audit_full_paper(
            paper_text = source_text_store(),
            paper_type = "Auto-detect / unclear",
            review_mode = input$review_mode,
            top_n = 10,
            show_retrieved_context = FALSE
          ),
          error = function(e) {
            full_msg <- conditionMessage(e)
            
            parent_msg <- tryCatch(
              {
                parent <- rlang::cnd_parent(e)
                if (!is.null(parent)) {
                  conditionMessage(parent)
                } else {
                  ""
                }
              },
              error = function(err) ""
            )
            
            if (nchar(parent_msg) > 0) {
              full_msg <- paste0(full_msg, "\nParent error: ", parent_msg)
            }
            
            add_process_log(
              paste0("### Error\n❌ Error during whole-paper review:\n\n", full_msg)
            )
            
            list(
              final_text = paste0(
                "An error occurred while running the whole-paper review:\n\n",
                e$message,
                "\n\nPlease check that:\n",
                "- your RAG dataset is inside the data/ folder\n",
                "- claim_auditor_team.R uses the correct knowledge base path\n",
                "- ANTHROPIC_API_KEY is set\n",
                "- your working directory is the main stat_claim_auditor folder"
              ),
              paper_map = NULL,
              retrieved_context = NULL,
              agent_trace = NULL
            )
          }
        )
        
        out
      }
    )
    
    audit_result(result)
    audit_running(FALSE)
    
    add_process_log("### Complete\n✅ Whole-paper review complete. The result is shown in the Review Result panel.")
    
    shinychat::chat_append(
      "source_chat",
      "Whole-paper review complete. I placed the paper map and review result on the right. You can now ask follow-up questions here.",
      session = session
    )
    
    kb_text <- if (!is.null(result$retrieved_context)) {
      format_context_for_agent(result$retrieved_context)
    } else {
      "No knowledge base context available."
    }
    
    paper_map_text <- if (!is.null(result$paper_map)) {
      result$paper_map
    } else {
      "No paper map available."
    }
    
    followup_client(
      chat_anthropic(
        model = MODEL_NAME,
        system_prompt = glue::glue("
You are a friendly statistical explanation assistant.

The user will ask follow-up questions about a whole-paper statistical review.

Review type:
Whole-paper review

Paper type:
Auto-detect / unclear

Review mode:
{input$review_mode}

Review result:
{result$final_text}

Paper map:
{paper_map_text}

Knowledge base context:
{kb_text}

Rules:
- Answer in plain English.
- Assume the user may have scientific training but may not be a statistician.
- Do not add new biomedical facts, rates, or medical advice.
- Do not invent citations or numerical results.
- If the question requires journal-specific, medical, or domain decisions, recommend asking a qualified statistician, clinician, domain expert, or journal editor.
- Keep answers under 300 words.
")
      )
    )
  })
  
  # ---------------------------------------------------------------------------
  # Run claim/paragraph review
  # ---------------------------------------------------------------------------
  
  observeEvent(input$run_claim_review, {
    text_to_review <- trimws(input$claim_or_paragraph_to_review)
    
    if (input_kind() != "pasted_text" && nchar(text_to_review) < 10) {
      showNotification("Please paste a claim or paragraph into the chat first.", type = "error")
      return(NULL)
    }
    
    if (nchar(text_to_review) < 10) {
      showNotification("Please provide more text for claim/paragraph review.", type = "error")
      return(NULL)
    }
    
    source_text_store(text_to_review)
    input_kind("pasted_text")
    
    audit_running(TRUE)
    audit_result(NULL)
    followup_client(NULL)
    
    agent_process_log(c(
      "### Starting claim/paragraph review\nThe app is preparing a focused statistical review."
    ))
    
    shinychat::chat_append(
      "source_chat",
      paste0(
        "I’ll run claim/paragraph statistical review on this text now:\n\n> ",
        substr(text_to_review, 1, 500),
        ifelse(nchar(text_to_review) > 500, "...", "")
      ),
      session = session
    )
    
    result <- withProgress(
      message = "Running claim/paragraph statistical review...",
      value = 0,
      {
        incProgress(0.15, detail = "Retrieving RAG context...")
        add_process_log(
          "### Step 1 — RAG Retriever\nRetrieving relevant sources from the statistical peer-review knowledge base."
        )
        Sys.sleep(0.2)
        
        incProgress(0.15, detail = "Consulting claim extractor...")
        add_process_log(
          "### Step 2 — Claim Extractor\nIdentifying population, comparison, outcome, effect measure, and missing information."
        )
        Sys.sleep(0.2)
        
        incProgress(0.20, detail = "Consulting statistical reviewer...")
        add_process_log(
          "### Step 3 — Statistical Reviewer\nChecking p-values, effect measures, uncertainty, multiplicity, and common statistical mistakes."
        )
        Sys.sleep(0.2)
        
        incProgress(0.15, detail = "Consulting causal/design reviewer...")
        add_process_log(
          "### Step 4 — Causal / Design Reviewer\nChecking causal language and whether the design supports the claim."
        )
        Sys.sleep(0.2)
        
        incProgress(0.15, detail = "Consulting reporting reviewer...")
        add_process_log(
          "### Step 5 — Reporting Reviewer\nChecking reporting gaps and transparency concerns."
        )
        Sys.sleep(0.2)
        
        incProgress(0.10, detail = "Consulting skeptical reviewer...")
        add_process_log(
          "### Step 6 — Skeptical Reviewer\nReducing overconfidence and identifying what cannot be assessed."
        )
        Sys.sleep(0.2)
        
        incProgress(0.10, detail = "Synthesizing final response...")
        add_process_log(
          "### Step 7 — Final Synthesizer\nCombining the agents' findings into a concise peer-review summary."
        )
        
        out <- tryCatch(
          audit_peer_review(
            claim = text_to_review,
            source_text = text_to_review,
            paper_type = "Auto-detect / unclear",
            review_mode = input$review_mode,
            top_n = 8,
            show_retrieved_context = FALSE
          ),
          error = function(e) {
            add_process_log(
              paste0("### Error\n❌ Error during claim/paragraph review: ", e$message)
            )
            
            list(
              final_text = paste0(
                "An error occurred while running the claim/paragraph review:\n\n",
                e$message,
                "\n\nPlease check that:\n",
                "- your RAG dataset is inside the data/ folder\n",
                "- claim_auditor_team.R uses the correct knowledge base path\n",
                "- ANTHROPIC_API_KEY is set\n",
                "- your working directory is the main stat_claim_auditor folder"
              ),
              retrieved_context = NULL,
              agent_trace = NULL
            )
          }
        )
        
        out
      }
    )
    
    audit_result(result)
    audit_running(FALSE)
    
    add_process_log("### Complete\n✅ Claim/paragraph review complete. The result is shown in the Review Result panel.")
    
    shinychat::chat_append(
      "source_chat",
      "Claim/paragraph review complete. I placed the result on the right. You can now ask follow-up questions here.",
      session = session
    )
    
    kb_text <- if (!is.null(result$retrieved_context)) {
      format_context_for_agent(result$retrieved_context)
    } else {
      "No knowledge base context available."
    }
    
    followup_client(
      chat_anthropic(
        model = MODEL_NAME,
        system_prompt = glue::glue("
You are a friendly statistical explanation assistant.

The user will ask follow-up questions about a claim/paragraph statistical review.

Review type:
Claim/paragraph review

Paper type:
Auto-detect / unclear

Review mode:
{input$review_mode}

Review result:
{result$final_text}

Knowledge base context:
{kb_text}

Rules:
- Answer in plain English.
- Assume the user may have scientific training but may not be a statistician.
- Do not add new biomedical facts, rates, or medical advice.
- Do not invent citations or numerical results.
- If the question requires journal-specific, medical, or domain decisions, recommend asking a qualified statistician, clinician, domain expert, or journal editor.
- Keep answers under 300 words.
")
      )
    )
  })
  
  # ---------------------------------------------------------------------------
  # Reactive status flags
  # ---------------------------------------------------------------------------
  
  output$hasUploadedPaper <- reactive({
    input_kind() == "uploaded_paper" && nchar(source_text_store()) > 20
  })
  outputOptions(output, "hasUploadedPaper", suspendWhenHidden = FALSE)
  
  output$hasPastedText <- reactive({
    input_kind() == "pasted_text" && nchar(source_text_store()) > 3
  })
  outputOptions(output, "hasPastedText", suspendWhenHidden = FALSE)
  
  output$hasAuditResult <- reactive({
    !is.null(audit_result())
  })
  outputOptions(output, "hasAuditResult", suspendWhenHidden = FALSE)
  
  output$hasPaperMap <- reactive({
    result <- audit_result()
    !is.null(result) && !is.null(result$paper_map)
  })
  outputOptions(output, "hasPaperMap", suspendWhenHidden = FALSE)
  
  output$hasFullDetails <- reactive({
    result <- audit_result()
    if (is.null(result)) return(FALSE)
    
    parts <- split_review_output(result$final_text)
    nchar(parts$full) > 0
  })
  outputOptions(output, "hasFullDetails", suspendWhenHidden = FALSE)
  
  # ---------------------------------------------------------------------------
  # Status UI
  # ---------------------------------------------------------------------------
  
  output$audit_status_message <- renderUI({
    if (isTRUE(audit_running())) {
      div(
        class = "warning-box",
        strong("Running review... "),
        "The app is consulting multiple specialist agents. Watch the Agent Process panel for updates."
      )
    } else {
      NULL
    }
  })
  
  output$agent_process_display <- renderUI({
    log_text <- paste(agent_process_log(), collapse = "\n\n---\n\n")
    render_markdown_fragment(log_text)
  })
  
  # ---------------------------------------------------------------------------
  # Paper map
  # ---------------------------------------------------------------------------
  
  output$paper_map_display <- renderUI({
    req(audit_result())
    req(audit_result()$paper_map)
    
    render_markdown_fragment(audit_result()$paper_map)
  })
  
  # ---------------------------------------------------------------------------
  # Review result
  # ---------------------------------------------------------------------------
  
  output$quick_review_result <- renderUI({
    req(audit_result())
    
    parts <- split_review_output(audit_result()$final_text)
    
    render_markdown_fragment(parts$quick)
  })
  
  output$full_review_result <- renderUI({
    req(audit_result())
    
    parts <- split_review_output(audit_result()$final_text)
    
    if (nchar(parts$full) == 0) {
      return(HTML("<em>No separate full details section was detected.</em>"))
    }
    
    render_markdown_fragment(parts$full)
  })
  
  # ---------------------------------------------------------------------------
  # Retrieved KB table
  # ---------------------------------------------------------------------------
  
  output$kb_table <- renderTable({
    req(audit_result())
    
    kb <- audit_result()$retrieved_context
    
    if (is.null(kb)) {
      return(data.frame(Message = "No retrieved knowledge base context available."))
    }
    
    available_cols <- intersect(
      c(
        "id",
        "category",
        "paper_type",
        "topic",
        "source",
        "source_url",
        "principle",
        "reviewer_question"
      ),
      names(kb)
    )
    
    kb |>
      dplyr::select(dplyr::all_of(available_cols))
  })
  
  # ---------------------------------------------------------------------------
  # Knowledge Base tab
  # ---------------------------------------------------------------------------
  
  output$resource_display <- renderDT({
    resource_df <- load_resource_for_display()
    
    DT::datatable(
      resource_df,
      escape = FALSE,
      options = list(
        pageLength = 12,
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # ---------------------------------------------------------------------------
  # Evaluation tab
  # ---------------------------------------------------------------------------

  output$overall_eval_summary <- renderTable({
    all_results <- dplyr::bind_rows(
      test_set1_results |>
        dplyr::transmute(
          Test_Set = "Test Set 1: Obvious wrong claims",
          Simple_Score_0_2,
          Detailed_Score_10
        ),
      test_set2_results |>
        dplyr::transmute(
          Test_Set = "Test Set 2: Known problem papers",
          Simple_Score_0_2,
          Detailed_Score_10
        ),
      test_set3_results |>
        dplyr::transmute(
          Test_Set = "Test Set 3: Good-paper full-text controls",
          Simple_Score_0_2,
          Detailed_Score_10
        ),
      test_set4_results |>
        dplyr::transmute(
          Test_Set = "Test Set 4: Abstract-only review",
          Simple_Score_0_2,
          Detailed_Score_10
        )
    )
    
    all_results |>
      dplyr::group_by(Test_Set) |>
      dplyr::summarise(
        Cases = dplyr::n(),
        Average_Simple_Score = round(mean(Simple_Score_0_2), 2),
        Average_Detailed_Score = round(mean(Detailed_Score_10), 2),
        Full_or_Near_Full_Score = paste0(
          sum(Simple_Score_0_2 >= 1.75),
          " / ",
          dplyr::n()
        ),
        .groups = "drop"
      )
  })

  output$test_set1_summary <- renderTable({
    data.frame(
      Metric = c(
        "Number of test claims",
        "Average simple score",
        "Average detailed score",
        "Claims fully correct on simple score",
        "Main remaining weakness"
      ),
      Result = c(
        nrow(test_set1_results),
        round(mean(test_set1_results$Simple_Score_0_2), 2),
        round(mean(test_set1_results$Detailed_Score_10), 2),
        paste0(sum(test_set1_results$Simple_Score_0_2 >= 2), " out of ", nrow(test_set1_results)),
        "Some outputs are still too long or include secondary issues that are not central to the simple claim."
      )
    )
  })

  output$test_set1_display <- renderDT({
    DT::datatable(
      test_set1_results,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      rownames = FALSE
    )
  })

  output$test_set2_summary <- renderTable({
    data.frame(
      Metric = c(
        "Number of known-problem papers",
        "Average simple score",
        "Average detailed score",
        "Cases with strong issue detection",
        "Main remaining weakness"
      ),
      Result = c(
        nrow(test_set2_results),
        round(mean(test_set2_results$Simple_Score_0_2), 2),
        round(mean(test_set2_results$Detailed_Score_10), 2),
        paste0(sum(test_set2_results$Simple_Score_0_2 >= 1.75), " out of ", nrow(test_set2_results)),
        "External benchmark context is still needed for cross-paper irregularities, retractions, or data-fabrication concerns."
      )
    )
  })

  output$test_set2_display <- renderDT({
    DT::datatable(
      test_set2_results,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      rownames = FALSE
    )
  })

  output$test_set3_summary <- renderTable({
    data.frame(
      Metric = c(
        "Number of good-paper controls",
        "Average simple score",
        "Average detailed score",
        "Successful false-positive controls",
        "Main remaining weakness"
      ),
      Result = c(
        nrow(test_set3_results),
        round(mean(test_set3_results$Simple_Score_0_2), 2),
        round(mean(test_set3_results$Detailed_Score_10), 2),
        paste0(sum(test_set3_results$Simple_Score_0_2 >= 1.75), " out of ", nrow(test_set3_results)),
        "The app occasionally uses slightly harsh wording for subgroup heterogeneity or statistically significant endpoints near the threshold."
      )
    )
  })

  output$test_set3_display <- renderDT({
    DT::datatable(
      test_set3_results,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      rownames = FALSE
    )
  })
  
  output$test_set4_summary <- renderTable({
    data.frame(
      Metric = c(
        "Number of abstract-only cases",
        "Average simple score",
        "Average detailed score",
        "Cases with strong performance",
        "Main finding"
      ),
      Result = c(
        nrow(test_set4_results),
        round(mean(test_set4_results$Simple_Score_0_2), 2),
        round(mean(test_set4_results$Detailed_Score_10), 2),
        paste0(sum(test_set4_results$Simple_Score_0_2 >= 1.75), " out of ", nrow(test_set4_results)),
        "After revision, the app handles abstract-only review much better and avoids most severe false positives for good RCT abstracts."
      )
    )
  })
  
  output$test_set4_display <- renderDT({
    DT::datatable(
      test_set4_results,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      rownames = FALSE
    )
  })

}

shinyApp(ui, server)