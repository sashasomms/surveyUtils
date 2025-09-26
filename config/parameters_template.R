# Survey Processing Parameters Template =====================================
#' 
#' Template configuration file for surveyUtils data processing pipeline.
#' Copy this file and customize for each study/survey.
#'
#' Usage:
#'   1. Copy to your project: cp parameters_template.R config/parameters_mystudy.R
#'   2. Customize all parameters below
#'   3. Add config/ to .gitignore to avoid committing credentials
#'   4. Source in your processing script: source("config/parameters_mystudy.R")
#'
#' @author Sasha Sommerfeldt
#' @date 2025-09-26

# =============================================================================
# SURVEY IDENTIFICATION
# =============================================================================

# Survey name as it appears in SurveyMonkey (used for API lookup)
# This should exactly match the title or nickname of the survey in your SurveyMonkey account
survey_name <- "My Study Name"

# =============================================================================
# AUTHENTICATION
# =============================================================================

# Path to CSV file containing your SurveyMonkey OAuth token
# CSV format: single column named "access_token" with token in first data row
# SECURITY: Store this file OUTSIDE your git repository
# SECURITY: Never commit tokens to version control
# Example token file location: "../credentials/survey_monkey_secrets.csv"
creds_path <- "../survey_monkey_secrets.csv"

# =============================================================================
# DIRECTORY STRUCTURE
# =============================================================================
# All paths are relative to your project root
# Directories will be created automatically if they don't exist

# UTILS LOCATION
# Path to surveyUtils functions (workflow_utils.R sources all other utils)
utils_path <- "../utils/core/workflow_utils.R"

# DATA DIRECTORIES
# Raw data from SurveyMonkey API (before processing)
raw_dir <- "data/raw"

# Configuration files (codebooks, parameters)
config_dir <- "config"

# Processed/cleaned data ready for analysis
processed_dir <- "data/processed"

# Attention check failure exports (for payment exclusion)
attention_dir <- "data/attention_checks"

# Visualization outputs
plots_dir <- "results/visualizations"

# SHARED RESOURCES
# Path to maximal codebook library (institutional knowledge base)
# This should point to your organization's shared maximal codebook
# Don't have one yet? Just generate the basic codebook with your first survey, then save it as a maximal to get started.
maximal_codebook_path <- "../utils/data_codebook/maximal_codebook.csv"

# =============================================================================
# DATE COLUMN CONFIGURATION
# =============================================================================

# Column names for date/time variables in your data
# These are standardized names after API extraction
start_date_col <- "start_date"  # Survey start timestamp
end_date_col <- "end_date"      # Survey completion timestamp

# Date format from SurveyMonkey API
# Options: "ISO8601" (YYYY-MM-DDTHH:MM:SS), "mdy_HMS" (MM/DD/YYYY HH:MM:SS)
date_format <- "ISO8601"

# =============================================================================
# DATA FILTERING OPTIONS
# =============================================================================

# DATE RANGE FILTERING
# Remove test/QA responses collected before official launch
# Format: "YYYY-MM-DD" or NULL to disable
min_date <- "2025-01-15"  # Remove responses before this date
max_date <- NULL          # Remove responses after this date (usually NULL)

# ATTENTION CHECKS
# Minimum proportion of attention checks participant must pass (0-1)
# Example: 0.8 means must pass 80% of attention checks
attention_threshold <- 0.8

# Whether to remove participants who fail attention checks
remove_attention_fails <- FALSE  # FALSE = flag only, TRUE = remove from dataset

# STRAIGHTLINING DETECTION
# Proportion of identical responses within a scale to flag as straightlining
# Example: 1.0 means 100% identical responses (most conservative)
# Example: 0.8 means 80% or more identical responses
straightline_threshold <- 1.0

# Whether to remove straightliners from dataset
remove_straightliners <- FALSE  # FALSE = flag only, TRUE = remove from dataset

# =============================================================================
# PROCESSING OPTIONS
# =============================================================================

# DATA EXPORT
# Save raw data from API to CSV
save_raw_csv <- TRUE

# Export list of attention check failures (e.g., to reference for payment/bonus exclusion)
export_attention_failures <- TRUE

# CODEBOOK GENERATION
# Fetch new metadata from API (TRUE) or use existing cached metadata (FALSE)
fetch_new_metadata <- FALSE

# Include placeholder rows for summary scores in codebook template
include_summary_scores_in_codebook <- FALSE

# SURVEY SCORING
# Automatically score all surveys found in codebook
score_surveys <- TRUE

# Add computed score variables (totals/subscales) back to codebook
update_codebook_with_scored <- TRUE

# =============================================================================
# ADVANCED OPTIONS (Optional Customization)
# =============================================================================

# COLUMN NAME CLEANING
# Suffixes to remove from column names after double header processing
remove_suffixes <- c("_response", "_open_ended")

# Prefixes to remove from column names (e.g., redundant question stems)
remove_prefixes <- c("in_the_past_week_")


# =============================================================================
# PACKAGE PARAMETERS LIST
# =============================================================================
# Combine all parameters into a single list for easy passing to functions

params <- list(
  # Survey identification
  survey_name = survey_name,
  creds_path = creds_path,
  
  # Directory structure
  utils_path = utils_path,
  raw_dir = raw_dir,
  config_dir = config_dir,
  processed_dir = processed_dir,
  attention_dir = attention_dir,
  plots_dir = plots_dir,
  maximal_codebook_path = maximal_codebook_path,
  
  # Date configuration
  start_date_col = start_date_col,
  end_date_col = end_date_col,
  date_format = date_format,
  min_date = min_date,
  max_date = max_date,
  
  # Filtering options
  attention_threshold = attention_threshold,
  remove_attention_fails = remove_attention_fails,
  straightline_threshold = straightline_threshold,
  remove_straightliners = remove_straightliners,
  
  # Processing options
  save_raw_csv = save_raw_csv,
  export_attention_failures = export_attention_failures,
  fetch_new_metadata = fetch_new_metadata,
  include_summary_scores_in_codebook = include_summary_scores_in_codebook,
  score_surveys = score_surveys,
  update_codebook_with_scored = update_codebook_with_scored,
  
  # Advanced options
  remove_suffixes = remove_suffixes,
  remove_prefixes = remove_prefixes,
  matrix_stem_lookup = matrix_stem_lookup
)

# =============================================================================
# PARAMETER VALIDATION
# =============================================================================

#' Validate Survey Parameters
#' 
#' Checks that all required parameters are present and properly formatted
#' Provides helpful error messages for common configuration mistakes
#' 
#' @param params List of survey parameters to validate
#' @return TRUE if valid, stops execution with error message if invalid
#'
validate_survey_params <- function(params) {
  
  # Check for missing required parameters
  required_params <- c("survey_name", "creds_path", "raw_dir", "config_dir", 
                       "processed_dir", "date_format")
  missing_params <- setdiff(required_params, names(params))
  
  if (length(missing_params) > 0) {
    stop("Missing required parameters: ", paste(missing_params, collapse = ", "))
  }
  
  # Validate credentials file exists
  if (!file.exists(params$creds_path)) {
    stop("Credentials file not found: ", params$creds_path, 
         "\nCreate a CSV with 'access_token' column containing your SurveyMonkey OAuth token.",
         "\nStore this file OUTSIDE your git repository for security.")
  }
  
  # Validate date format
  valid_formats <- c("ISO8601", "mdy_HMS")
  if (!params$date_format %in% valid_formats) {
    stop("Invalid date_format: ", params$date_format, 
         "\nMust be one of: ", paste(valid_formats, collapse = ", "))
  }
  
  # Validate threshold parameters
  if (params$attention_threshold < 0 || params$attention_threshold > 1) {
    stop("attention_threshold must be between 0 and 1, got: ", params$attention_threshold)
  }
  
  if (params$straightline_threshold < 0 || params$straightline_threshold > 1) {
    stop("straightline_threshold must be between 0 and 1, got: ", params$straightline_threshold)
  }
  
  # Validate date format if min_date specified
  if (!is.null(params$min_date)) {
    tryCatch({
      as.Date(params$min_date)
    }, error = function(e) {
      stop("Invalid min_date format: ", params$min_date, 
           "\nMust be in 'YYYY-MM-DD' format (e.g., '2025-01-15')")
    })
  }
  
  message("✓ Survey parameters validation passed")
  return(TRUE)
}

# Validate parameters when this file is sourced
validate_survey_params(params)

# =============================================================================
# USAGE NOTES
# =============================================================================

# After customizing this file, source it in your processing script to make all parameters available:
#
# source("config/parameters_mystudy.R")
# source(params$utils_path)  # Uses path from parameters
# 
# token <- load_sm_token(params$creds_path)
# df_raw <- process_survey_responses(token, 
#                                    survey_name = params$survey_name,
#                                    save_csv = params$save_raw_csv,
#                                    output_path = params$raw_dir)
