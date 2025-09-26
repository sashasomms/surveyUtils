# Research Workflow Orchestration Utilities ==================================
#'
#' High-level workflow functions that orchestrate multiple utils to complete
#' common research tasks. These functions coordinate between API calls,
#' data processing, codebook generation, and survey scoring.
#'
#' @author Sasha L Sommerfeldt
#' @date 2025

# Setup and Dependencies ------------------------------------------------------

library(dplyr)
library(readr)
library(stringr)

# Source all required utility scripts
source("../utils/external/surveymonkey_utils.R")
source("../utils/data/data_codebook_utils.R")
source("../utils/data/data_wrangling_utils.R") 
source("../utils/data/survey_scoring_utils.R")

# Complete Workflow Functions ------------------------------------------------

#' Complete SurveyMonkey Download and Codebook Generation
#'
#' Downloads survey response data and generates an enhanced codebook template
#' using the maximal codebook library for automatic scale matching.
#'
#' @param token SurveyMonkey OAuth token
#' @param survey_name Name of survey to process (as it appears in SurveyMonkey)
#' @param output_path Path for saving outputs (codebook and raw data)
#' @param use_maximal_codebook Whether to use maximal codebook for enhanced matching (default: TRUE)
#' @param fetch_new_metadata Whether to fetch fresh metadata from API (default: TRUE)
#' @param save_raw_csv Whether to save raw response data to CSV (default: TRUE)
#' @return List containing survey data and codebook
#' 
#' @examples
#' \dontrun{
#' token <- load_sm_token()
#' results <- download_and_generate_codebook(
#'   token, 
#'   "My Survey",
#'   output_path = "config/",
#'   use_maximal_codebook = TRUE
#' )
#' }
#'
download_and_generate_codebook <- function(token, 
                                           survey_name,
                                           output_path = "config/",
                                           use_maximal_codebook = TRUE,
                                           fetch_new_metadata = TRUE,
                                           save_raw_csv = TRUE) {
  
  message("\n=== STEP 1: DOWNLOADING SURVEY DATA ===")
  df_raw <- process_survey_responses(
    token, 
    survey_name = survey_name,
    save_csv = save_raw_csv,
    output_path = ifelse(save_raw_csv, "data/raw/", NULL)
  )
  
  message("\n=== STEP 2: GENERATING CODEBOOK ===")
  if (use_maximal_codebook) {
    message("Using maximal codebook for enhanced template generation...")
    codebook <- generate_enhanced_codebook_template(
      token,
      survey_name = survey_name,
      output_path = output_path,
      fetch_new_metadata = fetch_new_metadata,
      include_summary_scores = FALSE
    )
  } else {
    message("Generating basic template without maximal codebook...")
    codebook <- generate_basic_codebook_template(
      token,
      survey_name = survey_name,
      fetch_new_metadata = fetch_new_metadata,
      output_path = output_path,
      include_summary_scores = FALSE
    )
  }
  
  message("\n=== DOWNLOAD & GENERATION COMPLETE ===")
  message("Survey responses: ", nrow(df_raw), " participants")
  message("Codebook variables: ", nrow(codebook))
  
  return(list(
    raw_data = df_raw,
    codebook = codebook
  ))
}

#' Complete Survey Data Processing Pipeline
#'
#' Orchestrates the entire processing workflow from raw SurveyMonkey data to
#' analysis-ready dataset with scored instruments. Includes:
#' 1. Column name standardization
#' 2. Short name application from codebook
#' 3. Double header processing
#' 4. Multi-select summary creation
#' 5. Date filtering
#' 6. Survey duration calculation
#' 7. Attention check scoring
#' 8. Response text to numeric mapping
#' 9. Numeric type conversion
#' 10. Straightlining detection
#' 11. Reverse coding
#' 12. Survey scoring (totals and subscales)
#' 13. Codebook updating with computed scores
#'
#' @param raw_data Raw survey data frame from SurveyMonkey
#' @param codebook Data codebook data frame
#' @param survey_name Survey name for output file naming
#' @param params Parameters list (from parameters template)
#' @return Fully processed and scored data frame
#' 
#' @examples
#' \dontrun{
#' # Using parameters from template
#' source("config/parameters_mystudy.R")
#' 
#' processed_data <- complete_processing_pipeline(
#'   raw_data = df_raw,
#'   codebook = codebook,
#'   survey_name = params$survey_name,
#'   params = params
#' )
#' }
#'
complete_processing_pipeline <- function(raw_data, 
                                         codebook, 
                                         survey_name,
                                         params) {
  
  message("\n=== STARTING COMPLETE PROCESSING PIPELINE ===")
  message("Processing ", nrow(raw_data), " survey responses\n")
  
  # Call the master processing function with all parameters
  processed_data <- process_survey_data(
    data = raw_data,
    codebook = codebook,
    survey_name = survey_name,
    score_surveys = params$score_surveys,
    output_path = params$processed_dir,
    start_date_col = params$start_date_col,
    end_date_col = params$end_date_col,
    date_format = params$date_format,
    min_date = params$min_date,
    max_date = params$max_date,
    export_attention_failures = params$export_attention_failures,
    attention_dir = params$attention_dir,
    remove_attention_fails = params$remove_attention_fails,
    straightline_threshold = params$straightline_threshold,
    remove_straightliners = params$remove_straightliners,
    update_codebook_with_scored = params$update_codebook_with_scored
  )
  
  message("\n=== PROCESSING PIPELINE COMPLETE ===")
  message("Final dataset: ", nrow(processed_data), " participants, ", 
          ncol(processed_data), " variables")
  
  return(processed_data)
}

#' Complete End-to-End Survey Processing Workflow
#'
#' Master orchestration function that combines download, codebook generation,
#' and complete data processing in a single call. This is the main entry point
#' for processing a new survey from start to finish.
#'
#' @param token SurveyMonkey OAuth token
#' @param params Parameters list from parameters template
#' @return List containing raw data, processed data, and codebook
#' 
#' @examples
#' \dontrun{
#' # Complete workflow with parameters
#' source("config/parameters_mystudy.R")
#' token <- load_sm_token(params$creds_path)
#' 
#' results <- complete_survey_workflow(token, params)
#' 
#' # Access results
#' df_processed <- results$processed_data
#' codebook <- results$codebook
#' }
#'
complete_survey_workflow <- function(token, params) {
  
  message("\n╔════════════════════════════════════════════╗")
  message("║   COMPLETE SURVEY PROCESSING WORKFLOW     ║")
  message("║   Survey: ", params$survey_name)
  message("╚════════════════════════════════════════════╝\n")
  
  # Step 1 & 2: Download and generate codebook
  step1_results <- download_and_generate_codebook(
    token = token,
    survey_name = params$survey_name,
    output_path = params$config_dir,
    use_maximal_codebook = TRUE,
    fetch_new_metadata = params$fetch_new_metadata,
    save_raw_csv = params$save_raw_csv
  )
  
  message("\n⚠️  MANUAL STEP REQUIRED:")
  message("Review and edit the generated codebook if needed:")
  message("  ", file.path(params$config_dir, 
                          paste0(make_clean_names(params$survey_name), 
                                 "_generated_enhanced_codebook.csv")))
  message("\nPress Enter to continue with processing, or Ctrl+C to stop and edit codebook...")
  readline()
  
  # Step 3-13: Complete processing pipeline
  processed_data <- complete_processing_pipeline(
    raw_data = step1_results$raw_data,
    codebook = step1_results$codebook,
    survey_name = params$survey_name,
    params = params
  )
  
  message("\n╔════════════════════════════════════════════╗")
  message("║         WORKFLOW COMPLETE ✓                ║")
  message("╚════════════════════════════════════════════╝")
  
  return(list(
    raw_data = step1_results$raw_data,
    processed_data = processed_data,
    codebook = step1_results$codebook
  ))
}

# Project Setup Functions -----------------------------------------------------

#' Setup New Research Project Structure
#'
#' Creates a complete project structure with all necessary directories
#' and configuration files for a new research study.
#'
#' @param project_name Name of the new project
#' @param base_dir Base directory for the project (default: current directory)
#' @param create_readme Whether to create a README file (default: TRUE)
#' @param copy_templates Whether to copy parameter templates (default: TRUE)
#' @return Path to the created project directory
#' 
#' @examples
#' \dontrun{
#' project_path <- setup_new_research_project("Mindfulness Study 2025")
#' }
#'
setup_new_research_project <- function(project_name, 
                                       base_dir = ".", 
                                       create_readme = TRUE,
                                       copy_templates = TRUE) {
  
  # Create clean project directory name
  clean_name <- project_name %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]", "_") %>%
    str_replace_all("_{2,}", "_") %>%
    str_remove("^_|_$")
  
  project_dir <- file.path(base_dir, clean_name)
  
  message("\n=== CREATING NEW PROJECT STRUCTURE ===")
  message("Project: ", project_name)
  message("Directory: ", project_dir, "\n")
  
  # Create directory structure
  dirs_to_create <- c(
    project_dir,
    file.path(project_dir, "code"),
    file.path(project_dir, "config"),
    file.path(project_dir, "data"),
    file.path(project_dir, "data/raw"),
    file.path(project_dir, "data/processed"),
    file.path(project_dir, "data/attention_checks"),
    file.path(project_dir, "results"),
    file.path(project_dir, "results/visualizations"),
    file.path(project_dir, "results/tables"),
    file.path(project_dir, "notebooks")
  )
  
  for (dir in dirs_to_create) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    message("✓ Created: ", dir)
  }
  
  # Copy parameter template if requested
  if (copy_templates) {
    template_source <- "../utils/templates/parameters_template.R"
    template_dest <- file.path(project_dir, "config/parameters_template.R")
    
    if (file.exists(template_source)) {
      file.copy(template_source, template_dest)
      message("\n✓ Copied parameter template to: ", template_dest)
    } else {
      message("\n⚠️  Parameter template not found at: ", template_source)
    }
  }
  
  # Create README if requested
  if (create_readme) {
    readme_content <- paste0(
      "# ", project_name, "\n\n",
      "Created: ", Sys.Date(), "\n\n",
      "## Project Structure\n\n",
      "- `code/` - Analysis scripts\n",
      "- `config/` - Configuration files and codebooks\n", 
      "- `data/raw/` - Raw data from SurveyMonkey\n",
      "- `data/processed/` - Cleaned and processed data\n",
      "- `data/attention_checks/` - Attention check failure reports\n",
      "- `results/visualizations/` - Plots and figures\n",
      "- `results/tables/` - Summary tables\n",
      "- `notebooks/` - Exploratory analysis\n\n",
      "## Getting Started\n\n",
      "1. Customize parameters: `config/parameters_template.R` → `config/parameters_", clean_name, ".R`\n",
      "2. Add SurveyMonkey credentials (outside repo): `../survey_monkey_secrets.csv`\n",
      "3. Run processing workflow:\n\n",
      "```r\n",
      "source(\"config/parameters_", clean_name, ".R\")\n",
      "source(params$utils_path)\n",
      "token <- load_sm_token(params$creds_path)\n",
      "\n",
      "# Complete workflow\n",
      "results <- complete_survey_workflow(token, params)\n",
      "```\n"
    )
    
    readme_path <- file.path(project_dir, "README.md")
    writeLines(readme_content, readme_path)
    message("✓ Created README: ", readme_path)
  }
  
  message("\n=== PROJECT SETUP COMPLETE ===")
  message("\nNext steps:")
  message("1. Customize: ", file.path(project_dir, "config/parameters_template.R"))
  message("2. Create credentials file outside repo with SurveyMonkey token")
  message("3. Run workflow with complete_survey_workflow()")
  
  return(project_dir)
}

# Utility Functions -----------------------------------------------------------

#' Print Pipeline Summary
#'
#' Displays a summary of the processing pipeline steps
#'
#' @export
print_pipeline_steps <- function() {
  message("\n╔════════════════════════════════════════════════════════════╗")
  message("║        SURVEY PROCESSING PIPELINE STEPS                   ║")
  message("╠════════════════════════════════════════════════════════════╣")
  message("║  1. Column name standardization (snake_case, strip HTML)  ║")
  message("║  2. Short name application from codebook                  ║")
  message("║  3. Double header processing (question + response text)   ║")
  message("║  4. Multi-select summary creation                         ║")
  message("║  5. Date filtering (remove test/QA responses)             ║")
  message("║  6. Survey duration calculation                           ║")
  message("║  7. Attention check scoring                               ║")
  message("║  8. Response text to numeric mapping                      ║")
  message("║  9. Numeric type conversion                               ║")
  message("║ 10. Straightlining detection                              ║")
  message("║ 11. Reverse coding                                        ║")
  message("║ 12. Survey scoring (totals and subscales)                 ║")
  message("║ 13. Codebook updating with computed scores                ║")
  message("╚════════════════════════════════════════════════════════════╝\n")
}