# Statistical Peer Review Assistant

R/Shiny statistical review checks

Statistical Peer Review Assistant is a module-based R/Shiny app for first-pass statistical screening of academic and scientific manuscripts. It combines PDF/TXT manuscript review, claim-level review, retrieval-augmented statistical guidance, specialist LLM agents, and answer-safety guardrails.

Live demo: [Add live app link here]

Walkthrough video: [Add demo video link here]

This is a portfolio and course project proof of concept. The goal is to demonstrate a structured educational and reviewer-support LLM workflow: uploaded manuscripts or pasted claims are reviewed using current text context, retrieved statistical-review principles, multi-agent critique, and cautious reviewer-style synthesis.

The app is not designed to replace a statistician, subject-matter expert, journal reviewer, or medical expert. It is intended to help identify issues that a human reviewer may want to check.

## What the app does

A user can:

- open the app locally in R/Shiny,
- upload a full manuscript as a PDF or TXT file,
- run a whole-paper statistical peer review,
- paste a single claim, abstract, table excerpt, or paragraph,
- run a focused claim/paragraph statistical review,
- view an agent process trace,
- review retrieved statistical-review resources,
- inspect evaluation and robustness test results,
- use the output as a first-pass guide for human review.

The current app separates two workflows intentionally:

- Whole-paper statistical peer review
- Claim / paragraph statistical review

Earlier versions tried to extract optional claims from uploaded whole papers, but testing showed that whole-paper review and claim-level review serve different purposes. The final design keeps those workflows separate.

## Quick start

From the project root in R or RStudio:

```r
install.packages(c(
  "shiny", "ellmer", "shinychat", "bslib", "dplyr", "stringr",
  "markdown", "htmltools", "readr", "DT", "purrr", "glue",
  "tibble", "pdftools"
))
```

Then run:

```r
shiny::runApp()
```

The expected project structure is:

```text
stat_claim_auditor/
├── app.R
├── claim_auditor_team.R
├── Statistical_Peer_Review_Assistant_Tutorial.qmd
├── README.md
├── data/
│   └── RAG_baseline.csv
└── expertise/
    ├── claim_extractor.md
    ├── statistical_misconception.md
    ├── causal_inference.md
    ├── skeptical_reviewer.md
    ├── guardrail_coach.md
    ├── reporting_guideline.md
    ├── manuscript_type_classifier.md
    └── prediction_model_review.md
```

## API key configuration

The app uses Anthropic Claude through `ellmer::chat_anthropic()`.

Do not commit API keys. Locally, set your API key in R:

```r
Sys.setenv(ANTHROPIC_API_KEY = "your_api_key_here")
```

For a persistent setup, add this to `~/.Renviron`:

```text
ANTHROPIC_API_KEY=your_api_key_here
ANTHROPIC_MODEL=claude-haiku-4-5-20251001
```

Then restart R and check:

```r
Sys.getenv("ANTHROPIC_API_KEY")
Sys.getenv("ANTHROPIC_MODEL")
```

In the project, the model name is controlled by:

```r
MODEL_NAME <- Sys.getenv(
  "ANTHROPIC_MODEL",
  unset = "claude-haiku-4-5-20251001"
)
```

Do not place the API key directly in `app.R`.

## Project data design

The app uses one curated RAG baseline file:

```text
data/RAG_baseline.csv
```

This file is the backend knowledge base used by `claim_auditor_team.R` for retrieval. It contains statistical peer-review resources and fields such as:

- id
- category
- paper_type
- topic
- source
- source_url
- full_reference
- misconception
- principle
- warning_sign
- expert_question
- example
- issue_or_guideline
- reviewer_question

The RAG baseline includes sources on statistical reporting, causal inference, p-values, multiplicity, prediction modeling, diagnostic reasoning, missing data, generalizability, and common misleading statistical concepts.

The purpose of the RAG file is not to make the LLM memorize specific papers. It gives the agents reliable statistical-review principles and reviewer questions that can be retrieved when relevant to a claim or manuscript.

## How the app works

The app follows a controlled workflow rather than sending user text directly to a generic chatbot.

For whole-paper review:

```text
User uploads PDF or TXT
  -> App extracts manuscript text
  -> Paper Mapper splits and summarizes sections
  -> RAG Retriever finds relevant statistical-review resources
  -> Statistical Reviewer checks statistical reasoning and reporting
  -> Causal / Design Reviewer checks causal language and design support
  -> Reporting Reviewer checks guideline-related reporting elements
  -> Skeptical Reviewer reduces overconfidence
  -> Final Synthesizer creates a structured reviewer-style summary
```

For claim/paragraph review:

```text
User pastes a claim, abstract, table excerpt, or paragraph
  -> App stores the focused text
  -> RAG Retriever finds relevant statistical-review resources
  -> Claim Extractor identifies the claim structure
  -> Statistical Misconception Agent checks statistical interpretation
  -> Causal Inference Agent checks causal overclaiming
  -> Skeptical Reviewer checks what cannot be verified
  -> Guardrail Coach suggests cautious wording
  -> Final Synthesizer creates the final claim review
```

The app shows a visible agent process trace so users can see what the review system is doing.

## Whole-paper review

Whole-paper review begins with a Paper Mapper step. The purpose of this step is not to critique the paper yet. Instead, it creates a structured overview of the uploaded manuscript so later agents can understand the study design, sample, outcomes, statistical methods, and main claims.

The app searches for common manuscript sections such as:

- Abstract
- Introduction
- Methods
- Results
- Discussion
- Tables / Figures
- References

If section headings cannot be detected reliably, the app keeps the document as a single full-text block and continues the review.

The final whole-paper output uses this structure:

```text
# Whole-Paper Review Summary

1. Overall concern level
2. Paper type and design
3. Big-picture statistical concerns
4. Internal consistency checks
5. Reporting and transparency concerns
6. Claims needing more cautious wording
7. Suggested reviewer comment
8. Overall assessment for users
9. Relevant knowledge base sources
10. Limitation
```

The app also includes a false-positive guardrail for well-reported randomized trials. If a paper appears to be a randomized, controlled, blinded clinical trial with a clear primary outcome, sample size, effect estimates, and confidence intervals, the app should not assign high concern unless there is a clearly confirmed statistical flaw that directly threatens the primary conclusion.

## Claim and paragraph review

The claim-level workflow is designed for shorter inputs such as:

- a single statistical claim,
- an abstract,
- a table excerpt,
- a paragraph from a Results or Discussion section,
- a sentence from a manuscript conclusion.

The claim-level final output uses this structure:

```text
# Claim Review Summary

1. Verdict
2. Main problem
3. Why this matters
4. Safer wording
5. What to check
6. Technical note
```

Allowed verdicts are:

- Supported
- Questionable
- Misleading
- Incorrect
- Cannot assess from provided text

The app includes an abstract-specific guardrail. If the provided text appears to be an abstract or short excerpt, missing full-paper details should be treated as items to verify in the full paper, not as confirmed flaws.

## Multi-agent design

The app uses a small specialist team rather than one large all-purpose prompt. Each specialist returns a focused intermediate output, and the final synthesizer combines those outputs into the user-facing review.

Current agents include:

- Claim Extractor
- Statistical Misconception Agent
- Causal Inference Agent
- Skeptical Reviewer
- Guardrail Coach
- Manuscript Type Classifier
- Reporting Guideline Agent
- Prediction Model Review Agent
- Final Synthesizer

Most specialist prompts are stored as external markdown files inside the `expertise/` folder. This makes the prompts easier to edit without rewriting the R code. The final synthesizer is defined directly in `claim_auditor_team.R` because it functions as the orchestration layer.

## RAG role

RAG is part of the review layer, not a replacement for statistical judgment.

RAG helped because it gave the app an evidence layer instead of relying only on model memory. The app can retrieve relevant statistical-review principles based on the current claim, manuscript map, paper type, and review mode.

RAG also made the system more complex. Retrieval had to account for:

- p-value and statistical-significance language,
- causal claims and confounding,
- prediction model and machine-learning claims,
- multiplicity and subgroup claims,
- diagnostic testing language,
- missing data and generalizability concerns,
- reporting-guideline expectations.

The final design treats RAG as supporting evidence for the LLM, not as a replacement for the reviewer logic.

## Evaluation and robustness testing

The project includes an Evaluation / Robustness tab with app-based test sets. The goal was not only to test whether the app can identify problems, but also whether it avoids over-criticizing strong statistical work.

The main scoring system was:

- 0 = missed the expected issue or produced a misleading false positive
- 1 = partially identified the issue or partially over-criticized the paper
- 2 = correctly identified the issue or appropriately avoided false-positive severity

The scoring system was developed manually for this project. It is not a validated peer-review metric. It was used to make testing more consistent across repeated app outputs.

## Test sets

The final evaluation included five test sets.

### Test Set 1: Obvious wrong statistical claims

This test set checked basic claim-level statistical reasoning. Examples included:

- correlation as causation,
- relative risk without absolute risk,
- odds ratio interpreted as risk ratio,
- p-value interpreted as the probability that the null hypothesis is true,
- non-significant result interpreted as no effect,
- small uncontrolled sample overinterpreted,
- multiple testing and cherry-picking,
- missing denominator,
- Simpson’s paradox,
- prediction model overclaim.

The app performed well overall. The main remaining weakness was output style: some answers were longer than necessary or included secondary issues that were not central to the simple claim.

### Test Set 2: Known-problem or stress-test papers

This test set checked sensitivity. The goal was to see whether the app could flag serious problems when a paper should receive a critical review.

Examples included:

- Wansink / Bottomless Bowls,
- Reinhart & Rogoff, Growth in a Time of Debt,
- LaCour & Green canvassing experiment,
- MMR vaccination and autism stress-test case.

The app performed best when the issue was visible from the paper text itself. For cross-paper irregularities, retractions, or suspected data fabrication, external benchmark context is still needed.

### Test Set 3: Good full-paper false-positive controls

This test set checked specificity. The goal was to see whether the app could avoid falsely labeling strong statistical work as seriously flawed.

Examples included:

- postpartum hemorrhage randomized trial,
- sotatercept pulmonary arterial hypertension trial,
- empagliflozin heart failure trial,
- polatuzumab vedotin DLBCL trial.

The whole-paper false-positive guardrail worked. The app generally gave Low-to-Moderate concern, supported the primary endpoint, and avoided treating routine reviewer checks as major confirmed flaws.

### Test Set 4: Abstract-only review

This test set checked whether the claim/paragraph workflow handles abstracts differently from full papers.

This was important because abstracts omit many full-paper details. The app initially produced false positives by treating omitted details as flaws. After adding the abstract-review guardrail, performance improved.

The main lesson was that missing full-paper details should be phrased as “verify in the full paper,” not as confirmed flaws.

### Test Set 5: Baseline existing-LLM comparison

The baseline comparison tested whether the custom app adds value beyond asking a general-purpose LLM to act as a statistical peer reviewer.

The baseline LLM did not use:

- the Shiny interface,
- the RAG retrieval table,
- specialist agents,
- the paper mapper,
- the evaluation-tuned guardrails.

The comparison showed that carefully prompted general LLMs can perform well on many statistical review tasks. However, the app still adds value through reproducibility, fixed workflows, RAG grounding, visible agent roles, separated review modes, and documented robustness testing.

The app should not be presented as simply “better than ChatGPT.” A more accurate framing is:

```text
The app turns LLM-based statistical peer review into a structured, documented, reproducible workflow with RAG grounding, agent specialization, and built-in robustness testing.
```

## Repository organization

```text
app.R
claim_auditor_team.R
README.md
Statistical_Peer_Review_Assistant_Tutorial.qmd

data/
  RAG_baseline.csv

expertise/
  claim_extractor.md
  statistical_misconception.md
  causal_inference.md
  skeptical_reviewer.md
  guardrail_coach.md
  reporting_guideline.md
  manuscript_type_classifier.md
  prediction_model_review.md
```

## Source and deployment policy

The architecture is designed for permission-cleared review and educational materials. A local development workspace may use test articles, public papers, instructor-created examples, or user-provided manuscripts.

A public repository should not redistribute:

- API keys,
- private manuscripts,
- copyrighted full-text PDFs without permission,
- private review materials,
- runtime logs containing manuscript text,
- user-uploaded documents.

A fresh public clone should still demonstrate:

- the Shiny review interface,
- the claim-level review workflow,
- the whole-paper review workflow,
- the curated RAG baseline structure,
- the specialist prompt files,
- the evaluation and robustness documentation,
- the Quarto tutorial.

For deployment of the Shiny app, make sure the server has:

- `app.R`
- `claim_auditor_team.R`
- `data/RAG_baseline.csv`
- the `expertise/` prompt files
- required R packages
- `ANTHROPIC_API_KEY` configured securely as an environment variable

Do not place the API key directly in the app code.

## Limitations

This is not a production peer-review system. Current limitations include:

- It cannot verify raw data.
- It cannot confirm fraud, fabrication, or hidden data errors.
- It depends on PDF text extraction quality.
- It does not replace expert statistical, clinical, or scientific review.
- It may over-focus on statistical reporting.
- The RAG retrieval is keyword-based rather than embedding-based.
- The knowledge base needs maintenance.
- LLM outputs may vary across repeated runs.
- External benchmark context is needed for known retractions, replication failures, or cross-paper irregularities.

## Potential future work

Future work could include:

- embedding-based RAG,
- structured JSON agent outputs,
- citation validation,
- PDF table extraction,
- programmatic table consistency checks,
- benchmark context fields,
- batch evaluation mode,
- deployment with persistent privacy-safe logs,
- more paper-type-specific review modes,
- user-adjustable severity settings,
- a short video demonstration.

## Project framing

Statistical Peer Review Assistant is not just a Shiny chatbot. It is a structured statistical review app where uploaded manuscripts and pasted claims are evaluated through current text context, RAG evidence, specialist agents, skepticism checks, and cautious reviewer-style synthesis.

The app is best understood as a first-pass statistical reasoning assistant. It helps reviewers, students, and researchers identify issues to check, but final interpretation should always remain with qualified human experts.

## How GenAI was used

Generative AI was used throughout the project in three ways:

1. Design support: to brainstorm the app workflow, agent roles, and evaluation structure.
2. Coding support: to write and revise Shiny code, debug errors, simplify the UI, reset file uploads, and improve prompt guardrails.
3. Evaluation support: to interpret test outputs and decide whether failures represented sensitivity problems, specificity problems, or prompt-calibration issues.

Final decisions were based on manual testing, inspection of outputs, and iterative comparison against expected statistical-review behavior.
