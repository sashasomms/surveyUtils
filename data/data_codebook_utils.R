# Data Codebook Management Utilities =========================================
#'
#' Functions for generating and managing data codebooks from survey structures.
#' Handles basic template generation from SurveyMonkey API and enhanced generation
#' using institutional knowledge from a maximal codebook library.
#'
#' @author Sasha L Sommerfeldt  
#' @date 2025

# Setup and Dependencies ------------------------------------------------------

library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(janitor)

# Codebook Schema Definition --------------------------------------------------

# Define the canonical column order and structure for codebooks
CODEBOOK_CORE_COLS <- c(
  "col_num", "question_text", "variable_name", "short_variable_name",
  "cat", "scale_abbrev", "scale_full", "subscale",
  "coding_direction", "min", "max",
  "response_choices", "response_coding", "response_format", 
  "required", "correct_response"
)

CODEBOOK_METADATA_COLS <- c(
  "question_format", "family", "subtype",
  "matrix_row_id", "matrix_row_text",
  "question_id", "page_number", "variable_name_R"
)

CODEBOOK_ALL_COLS <- c(CODEBOOK_CORE_COLS, CODEBOOK_METADATA_COLS)

# Column defaults for ensuring all columns exist
CODEBOOK_COLUMN_DEFAULTS <- list(
  col_num = NA_integer_,
  question_text = NA_character_,
  variable_name = NA_character_,
  short_variable_name = NA_character_,
  cat = NA_character_,
  scale_abbrev = NA_character_,
  scale_full = NA_character_,
  subscale = NA_character_,
  coding_direction = NA_real_,
  min = NA_real_,
  max = NA_real_,
  response_choices = NA_character_,
  response_coding = NA_character_,
  response_format = NA_character_,
  required = NA,
  correct_response = NA_character_,
  question_format = NA_character_,
  family = NA_character_,
  subtype = NA_character_,
  matrix_row_id = NA_character_,
  matrix_row_text = NA_character_,
  question_id = NA_character_,
  page_number = NA_integer_,
  variable_name_R = NA_character_
)

# Helper Functions for Match Processing --------------------------------------

#' Initialize Match Columns in Questions DataFrame
#'
#' Adds empty match-related columns to a questions data frame
#'
#' @param questions_df Questions data frame
#' @return questions_df with match columns initialized
#'
initialize_match_columns <- function(questions_df) {
  questions_df %>% mutate(
    codebook_match = FALSE,
    match_confidence = NA_real_,
    matched_short_variable_name = NA_character_,
    matched_scale_abbrev = NA_character_,
    matched_scale_full = NA_character_,
    matched_subscale = NA_character_,
    matched_cat = NA_character_,
    matched_response_format = NA_character_,
    matched_correct_response = NA_character_,
    matched_coding_direction = NA_real_,
    match_method = NA_character_,
    metadata_mismatch_flags = ""
  )
}

#' Apply Codebook Match to Question Row
#'
#' Helper function to apply matched codebook information to a question row
#'
#' @param questions_df Questions data frame
#' @param row_index Index of row to update
#' @param codebook_match Single row from maximal codebook with match info
#' @param match_method Method used for matching ("exact_variable_name", "fuzzy_text", etc.)
#' @param match_confidence Confidence score (0-1)
#' @return Updated questions_df
#'
apply_codebook_match <- function(questions_df, row_index, codebook_match, 
                                 match_method, match_confidence = 1.0) {
  questions_df$codebook_match[row_index] <- TRUE
  questions_df$match_confidence[row_index] <- match_confidence
  questions_df$matched_short_variable_name[row_index] <- codebook_match$short_variable_name
  questions_df$matched_scale_abbrev[row_index] <- codebook_match$scale_abbrev
  questions_df$matched_scale_full[row_index] <- codebook_match$scale_full
  questions_df$matched_subscale[row_index] <- codebook_match$subscale
  questions_df$matched_cat[row_index] <- codebook_match$cat
  questions_df$matched_correct_response[row_index] <- codebook_match$correct_response
  questions_df$matched_response_format[row_index] <- codebook_match$response_format
  questions_df$matched_coding_direction[row_index] <- codebook_match$coding_direction
  questions_df$match_method[row_index] <- match_method
  
  return(questions_df)
}

#' Check for Metadata Mismatches Between Question and Codebook Match
#'
#' @param current_row Single row from questions_df
#' @param codebook_match Single row from maximal codebook
#' @return String of comma-separated mismatch flags, or empty string
#'
check_metadata_mismatches <- function(current_row, codebook_match) {
  mismatch_flags <- c()
  
  if (!is.na(codebook_match$response_choices) && !is.na(current_row$response_choices) && 
      codebook_match$response_choices != current_row$response_choices) {
    mismatch_flags <- c(mismatch_flags, "response_choices")
  }
  if (!is.na(codebook_match$response_coding) && !is.na(current_row$response_coding) && 
      codebook_match$response_coding != current_row$response_coding) {
    mismatch_flags <- c(mismatch_flags, "response_coding")
  }
  if (!is.na(codebook_match$min) && !is.na(current_row$min) && 
      codebook_match$min != current_row$min) {
    mismatch_flags <- c(mismatch_flags, "min")
  }
  if (!is.na(codebook_match$max) && !is.na(current_row$max) && 
      codebook_match$max != current_row$max) {
    mismatch_flags <- c(mismatch_flags, "max")
  }
  if (!is.na(codebook_match$family) && !is.na(current_row$family) && 
      codebook_match$family != current_row$family) {
    mismatch_flags <- c(mismatch_flags, "family")
  }
  
  return(paste(mismatch_flags, collapse = ", "))
}

#' Ensure Required Columns Exist in DataFrame
#'
#' Adds missing columns with appropriate NA types
#'
#' @param df Data frame
#' @param column_specs Named list of column_name = default_value pairs
#' @return df with all specified columns present
#'
ensure_columns_exist <- function(df, column_specs) {
  for (col_name in names(column_specs)) {
    if (!col_name %in% names(df)) {
      df[[col_name]] <- column_specs[[col_name]]
    }
  }
  return(df)
}

#' Standardize Codebook Column Order and Structure
#'
#' Ensures codebook has all required columns in the correct order
#'
#' @param df Codebook data frame (may have extra or missing columns)
#' @return Standardized codebook with canonical column order
#'
standardize_codebook_structure <- function(df) {
  # Add any missing columns with appropriate defaults
  df <- ensure_columns_exist(df, CODEBOOK_COLUMN_DEFAULTS)
  
  # Select columns in canonical order (keeps extras at end)
  df %>% select(all_of(CODEBOOK_ALL_COLS), everything())
}

#' Create Summary Score Row Template
#'
#' @param scale_name Scale abbreviation
#' @param base_template Optional base template to copy structure from
#' @return Single-row data frame for summary score
#'
create_summary_score_row <- function(scale_name, base_template = NULL) {
  # Get column structure from base template if provided
  if (!is.null(base_template)) {
    row_template <- base_template[1, ]
    row_template[1, ] <- NA  # Clear all values
  } else {
    # Use canonical columns
    row_template <- as.data.frame(
      setNames(replicate(length(CODEBOOK_ALL_COLS), NA, simplify = FALSE),
               CODEBOOK_ALL_COLS)
    )
  }
  
  # Fill in summary score specifics
  row_template$variable_name <- paste0(make_clean_names(scale_name), "_total")
  row_template$short_variable_name <- paste0(make_clean_names(scale_name), "_total")
  row_template$variable_name_R <- paste0(make_clean_names(scale_name), "_total")
  row_template$cat <- "summary_scores"
  row_template$scale_abbrev <- scale_name
  row_template$scale_full <- paste(scale_name, "Total Score")
  row_template$question_text <- paste(scale_name, "Total Score")
  row_template$question_format <- "computed"
  
  return(row_template)
}

# Basic Codebook Template Generation (Uses SurveyMonkey API) -----------------

#' Generate Basic Codebook Template from SurveyMonkey Metadata
#'
#' Creates a basic codebook CSV template using SurveyMonkey metadata CSV from
#' surveymonkey_utils. Variable names are cleaned to snake_case. Scale information 
#' must be filled manually.
#'
#' @param token SurveyMonkey OAuth token
#' @param survey_name Survey name for processing
#' @param fetch_new_metadata Whether to fetch fresh metadata or use existing file
#' @param output_path Path where to save the codebook CSV
#' @param include_summary_scores Whether to include placeholders for summary scores
#' @return Data frame containing the basic codebook template
#' 
#' @examples
#' \dontrun{
#' token <- load_sm_token()
#' codebook <- generate_basic_codebook_template(
#'   token = token,
#'   survey_name = "My Survey",
#'   output_path = "config/"
#' )
#' }
#'
generate_basic_codebook_template <- function(token,
                                             survey_name,
                                             fetch_new_metadata = TRUE,
                                             output_path = "config/",
                                             include_summary_scores = TRUE) {
  
  # Metadata filename and path (so can check if it exists)
  clean_survey_name <- survey_name %>% make_clean_names()
  metadata_filename <- paste0(clean_survey_name, "_metadata_", Sys.Date(), ".csv")
  metadata_filepath <- file.path(output_path, metadata_filename)
  message("metadata_filepath is ", metadata_filepath)
  
  if (!file.exists(metadata_filepath) || fetch_new_metadata) {
    message("Fetching survey metadata using process_survey_structure() from surveymonkey_utils.R")
    survey_metadata <- process_survey_structure(token, survey_name = survey_name, output_path = output_path)
  } else if (file.exists(metadata_filepath) && !fetch_new_metadata) {
    message("Using existing survey metadata: ", metadata_filepath)
    survey_metadata <- read_csv(metadata_filepath, show_col_types = FALSE)
  }
  
  message("Generating basic codebook template from ", nrow(survey_metadata), " questions/items")
  
  # Create standardized codebook format from metadata
  codebook <- survey_metadata %>%
    mutate(
      # Use question text for question_text documentation (combining matrix row text if needed)
      question_text = question_text %>% str_remove_all("<[^>]*>"),
      
      # Create clean snake_case variable names from question text (not question IDs)
      variable_name = case_when(
        # Multi-select: "Question text - Choice text"
        question_format == "multi_select" & !is.na(choice_text) ~ 
          make_clean_names(paste(question_text, "-", choice_text)),
        
        # Multi-select "Other" text field: "Question text - Other (please specify)"  
        question_format == "multi_select" & is_other_text_field ~ 
          make_clean_names(paste(question_text, "- Other (please specify)")),
        
        # Matrix and single-select: use question text directly
        TRUE ~ make_clean_names(question_text)
      ),
      # Create short variable name (will be enhanced by maximal codebook matching)
      short_variable_name = "",  # Start blank, will be filled by maximal codebook matches
      variable_name_R = variable_name,  # Copy for potential _R suffix if reverse-coded
      
      # Set empty defaults instead of placeholder text
      cat = "",  # Empty instead of "survey_items"
      scale_abbrev = "",  # Short scale name (e.g., "PHQ9")
      scale_full = "",   # Full scale name (e.g., "Patient Health Questionnaire-9")
      subscale = "",
      correct_response = "",  # Correct answer (for attention checks)
      response_format = "",   # Format for response conversion ("numeric", "", etc.)
      
      # Set coding direction based on question format
      coding_direction = case_when(
        question_format %in% c("single_select", "matrix") ~ 1,
        question_format == "multi_select" ~ NA_real_,  # Binary coding
        TRUE ~ 1
      ),
      
      # Use response coding info from metadata
      min = case_when(
        question_format == "multi_select" ~ 0,
        !is.na(response_coding) ~ 1,  # Default min for coded responses
        TRUE ~ NA_real_
      ),
      max = case_when(
        question_format == "multi_select" ~ 1,
        !is.na(response_coding) ~ num_response_choices,
        TRUE ~ NA_real_
      ),
      
      # Add column number for sorting
      col_num = row_number()
    ) %>%
    standardize_codebook_structure() %>%
    arrange(page_number, col_num)
  
  # Add summary score placeholders if requested
  if (include_summary_scores) {
    unique_scales <- codebook %>%
      filter(scale_abbrev != "", !is.na(scale_abbrev)) %>%
      pull(scale_abbrev) %>%
      unique()
    
    if (length(unique_scales) > 0) {
      summary_rows <- map_dfr(unique_scales, ~create_summary_score_row(.x, codebook))
      codebook <- bind_rows(codebook, summary_rows)
    }
  }
  
  # Save to CSV if path provided
  if (!is.null(output_path)) {
    codebook_filename <- paste0(clean_survey_name, "_generated_codebook.csv")
    codebook_filepath <- file.path(output_path, codebook_filename)
    dir.create(dirname(codebook_filepath), recursive = TRUE, showWarnings = FALSE)
    write_csv(codebook, codebook_filepath)
    message("Basic codebook template saved to: ", codebook_filepath)
    message("\nNext steps (if not using enhanced codebook):")
    message("1. Fill in 'scale_abbrev' and 'scale_full' columns with survey instrument names")
    message("2. Set 'coding_direction' (-1 for reverse, 1 for forward)")
    message("3. Add 'cat' categories (attention_check, demo, etc.)")
    message("4. Verify min/max values and response_coding are correct")
    message("5. Add any missing summary score variables")
  }
  
  return(codebook)
}

# Maximal Codebook Library System (Institutional Knowledge) ------------------

#' Load Maximal Codebook Library
#'
#' Loads the maximal codebook library containing institutional knowledge
#' of known survey instruments with their coding patterns.
#'
#' @param library_path Path to maximal codebook library CSV
#' @return Data frame containing maximal codebook library
#' 
#' @examples
#' maximal_lib <- load_maximal_codebook("config/maximal_codebook.csv")
#'
load_maximal_codebook <- function(library_path = "config/maximal_codebook.csv") {
  
  if (!file.exists(library_path)) {
    message("Maximal codebook library not found at: ", library_path)
    message("Creating empty library. Add scales using update_maximal_codebook()")
    
    # Create empty library with canonical structure
    empty_library <- as.data.frame(
      setNames(replicate(length(CODEBOOK_ALL_COLS), 
                         character(0), simplify = FALSE),
               CODEBOOK_ALL_COLS)
    )
    
    # Create directory if needed
    dir.create(dirname(library_path), recursive = TRUE, showWarnings = FALSE)
    write_csv(empty_library, library_path)
    return(empty_library)
  }
  
  library_data <- read_csv(library_path, show_col_types = FALSE)
  message("Loaded maximal codebook with ", nrow(library_data), " items from ", 
          length(unique(library_data$scale_abbrev)), " scales")
  return(library_data)
}

#' Match Questions to Maximal Codebook
#'
#' Uses exact matching on variable_name first, then fuzzy text matching for remaining
#' questions to identify items in the maximal codebook library.
#'
#' @param questions_df Data frame of questions (from basic codebook template)
#' @param maximal_codebook Data frame from load_maximal_codebook()
#' @param similarity_threshold Minimum similarity for fuzzy matching (0-1)
#' @return Data frame with matched codebook information added
#' 
#' @examples
#' matched <- match_questions_to_codebook(basic_template, maximal_codebook)
#'
match_questions_to_codebook <- function(questions_df, maximal_codebook, similarity_threshold = 0.8) {
  
  if (nrow(maximal_codebook) == 0) {
    message("Maximal codebook is empty - no matches possible")
    return(initialize_match_columns(questions_df))
  }
  
  message("Matching ", nrow(questions_df), " questions against maximal codebook...")
  
  # Initialize all match columns
  questions_df <- initialize_match_columns(questions_df)
  
  # STEP 1: Try exact matches on variable_name first
  exact_matches <- 0
  for (i in 1:nrow(questions_df)) {
    if (is.na(questions_df$variable_name[i])) next
    
    exact_match_idx <- which(maximal_codebook$variable_name == questions_df$variable_name[i])
    
    if (length(exact_match_idx) > 0) {
      codebook_match <- maximal_codebook[exact_match_idx[1], ]
      questions_df <- apply_codebook_match(
        questions_df, i, codebook_match,
        match_method = "exact_variable_name",
        match_confidence = 1.0
      )
      exact_matches <- exact_matches + 1
    }
  }
  
  message("Found ", exact_matches, " exact variable_name matches")
  
  # STEP 2: Fuzzy text matching for remaining unmatched questions
  unmatched_indices <- which(!questions_df$codebook_match)
  fuzzy_matches <- 0
  
  if (length(unmatched_indices) > 0) {
    message("Attempting fuzzy matching for ", length(unmatched_indices), " remaining questions...")
    
    for (i in unmatched_indices) {
      current_question_text <- str_to_lower(questions_df$question_text[i] %||% "")
      
      # Calculate Jaccard similarity with all maximal codebook items
      similarities <- sapply(maximal_codebook$question_text, function(lib_text) {
        q_words <- str_split(str_remove_all(current_question_text, "[[:punct:]]"), "\\s+")[[1]]
        l_words <- str_split(str_remove_all(str_to_lower(lib_text), "[[:punct:]]"), "\\s+")[[1]]
        
        intersection <- length(intersect(q_words, l_words))
        union <- length(union(q_words, l_words))
        
        ifelse(union > 0, intersection / union, 0)
      })
      
      if (length(similarities) > 0) {
        best_match_idx <- which.max(similarities)
        best_similarity <- similarities[best_match_idx]
        
        if (length(best_similarity) > 0 && best_similarity >= similarity_threshold) {
          codebook_match <- maximal_codebook[best_match_idx, ]
          questions_df <- apply_codebook_match(
            questions_df, i, codebook_match,
            match_method = "fuzzy_text",
            match_confidence = best_similarity
          )
          
          # Check for metadata mismatches and flag them
          questions_df$metadata_mismatch_flags[i] <- check_metadata_mismatches(
            questions_df[i, ], codebook_match
          )
          
          fuzzy_matches <- fuzzy_matches + 1
        }
      }
    }
    
    message("Found ", fuzzy_matches, " fuzzy text matches")
  }
  
  total_matches <- sum(questions_df$codebook_match)
  message("Total matches: ", total_matches, "/", nrow(questions_df), " (", 
          round(100 * total_matches / nrow(questions_df), 1), "%)")
  message("  - Exact matches: ", exact_matches)
  message("  - Fuzzy matches: ", fuzzy_matches)
  
  return(questions_df)
}

#' Apply Maximal Codebook Matches to Template
#'
#' Updates a basic codebook template with information from maximal codebook matches.
#' API-derived structure is preserved, but scale metadata is filled in automatically.
#'
#' @param template_df Basic codebook template 
#' @param matched_questions Output from match_questions_to_codebook()
#' @param confidence_threshold Minimum confidence difference for auto-resolving multiple matches
#' @return Enhanced codebook template with matched information applied
#' 
#' @examples
#' enhanced <- apply_codebook_matches(basic_template, matched_questions)
#'
apply_codebook_matches <- function(template_df, matched_questions, confidence_threshold = 0.1) {
  
  enhanced_template <- template_df
  
  # Ensure metadata cols exist
  enhanced_template <- ensure_columns_exist(enhanced_template, list(
    codebook_match_confidence = NA_real_,
    codebook_match_method = NA_character_,
    codebook_multiple_matches = FALSE
  ))
  
  for (i in seq_len(nrow(template_df))) {
    # Match by question text instead of question_id
    if ("question_text" %in% names(matched_questions) && "question_text" %in% names(template_df)) {
      match_rows <- which(matched_questions$question_text == template_df$question_text[i])
    } else {
      match_rows <- i
    }
    
    if (length(match_rows) > 0 && all(match_rows <= nrow(matched_questions))) {
      match_info <- matched_questions[match_rows, , drop = FALSE]
      valid_matches <- match_info %>% filter(!is.na(codebook_match) & codebook_match)
      
      if (nrow(valid_matches) == 1) {
        # Single match -> apply
        m <- valid_matches[1, ]
        enhanced_template <- .apply_match(enhanced_template, i, m)
        
      } else if (nrow(valid_matches) > 1) {
        # Multiple matches: try confidence-based resolution
        valid_matches <- valid_matches %>% arrange(desc(match_confidence))
        best <- valid_matches[1, ]
        second_best <- valid_matches[2, ]
        
        if (!is.na(best$match_confidence) && 
            !is.na(second_best$match_confidence) &&
            (best$match_confidence - second_best$match_confidence) >= confidence_threshold) {
          # Clear winner by confidence
          enhanced_template <- .apply_match(enhanced_template, i, best)
          message("Auto-resolved multiple matches for question_text = ", 
                  template_df$question_text[i], 
                  " with confidence = ", round(best$match_confidence, 3))
        } else {
          # Ambiguous -> flag for human intervention
          warning("Multiple ambiguous codebook matches for question_text = ", 
                  template_df$question_text[i], " (row ", i, "). Manual resolution required.")
          enhanced_template$codebook_multiple_matches[i] <- TRUE
        }
      }
    }
  }
  
  # Summarize results
  applied_matches <- sum(!is.na(enhanced_template$codebook_match_confidence), na.rm = TRUE)
  multi_matches   <- sum(enhanced_template$codebook_multiple_matches, na.rm = TRUE)
  high_confidence <- sum(enhanced_template$codebook_match_confidence >= 0.9, na.rm = TRUE)
  
  message("Applied codebook matches to ", applied_matches, " items")
  message("High confidence matches (>=0.9): ", high_confidence)
  if (multi_matches > 0) {
    message("Found ", multi_matches, " items with multiple ambiguous matches (manual intervention needed)")
  }
  
  return(enhanced_template)
}

# Helper: apply a single match row to template
.apply_match <- function(df, i, m) {
  if (!is.na(m$matched_short_variable_name)) {
    df$short_variable_name[i] <- m$matched_short_variable_name
  }
  if (!is.na(m$matched_scale_abbrev)) {
    df$scale_abbrev[i] <- m$matched_scale_abbrev
  }
  if (!is.na(m$matched_scale_full)) {
    df$scale_full[i] <- m$matched_scale_full
  }
  if (!is.na(m$matched_subscale)) {
    df$subscale[i] <- m$matched_subscale
  }
  if (!is.na(m$matched_cat)) {
    df$cat[i] <- m$matched_cat
  }
  if (!is.na(m$matched_correct_response)) {
    df$correct_response[i] <- m$matched_correct_response
  }
  if (!is.na(m$matched_response_format)) { 
    df$response_format[i] <- m$matched_response_format
  }
  if (!is.na(m$matched_coding_direction)) {
    df$coding_direction[i] <- m$matched_coding_direction
  }
  df$codebook_match_confidence[i] <- m$match_confidence
  df$codebook_match_method[i] <- m$match_method
  return(df)
}

# Enhanced Codebook Template Generation (API + Maximal Codebook) --------------

#' Generate Enhanced Codebook Template with Maximal Codebook Matching
#'
#' Creates an enhanced codebook template that combines SurveyMonkey metadata
#' with institutional knowledge from the maximal codebook library for automatic
#' scale identification and coding direction assignment.
#'
#' @param token SurveyMonkey OAuth token
#' @param survey_name Survey name for filename generation
#' @param output_path Where to save the enhanced template
#' @param maximal_codebook_path Path to maximal codebook library
#' @param similarity_threshold Minimum similarity for fuzzy matching
#' @param fetch_new_metadata Whether to fetch fresh metadata or use existing file
#' @param include_summary_scores Whether to include placeholders for summary scores
#' @return Enhanced codebook template with automatic scale matching
#' 
#' @examples
#' \dontrun{
#' token <- load_sm_token()
#' enhanced <- generate_enhanced_codebook_template(
#'   token = token,
#'   survey_name = "Depression Study",
#'   output_path = "config/data_codebook.csv"
#' )
#' }
#'
generate_enhanced_codebook_template <- function(token, 
                                                survey_name,
                                                output_path = NULL,
                                                maximal_codebook_path = "config/maximal_codebook.csv",
                                                similarity_threshold = 0.8,
                                                fetch_new_metadata = TRUE,
                                                include_summary_scores = FALSE) {
  
  message("=== ENHANCED CODEBOOK GENERATION ===")
  message("output_path is ", output_path)
  
  # Step 1: Generate basic template from SurveyMonkey metadata  
  message("Step 1: Generating basic template from metadata...")
  basic_template <- generate_basic_codebook_template(
    token,
    survey_name = survey_name,
    fetch_new_metadata = fetch_new_metadata,
    output_path = output_path,
    include_summary_scores = include_summary_scores
  )
  
  # Step 2: Load maximal codebook and apply matches
  message("Step 2: Loading maximal codebook and matching questions...")
  maximal_codebook <- load_maximal_codebook(maximal_codebook_path)
  
  if (nrow(maximal_codebook) > 0) {
    enhanced_template <- match_questions_to_codebook(basic_template, maximal_codebook, similarity_threshold)
    
    # Transfer matched fields to the actual codebook columns
    enhanced_template <- enhanced_template %>%
      mutate(
        short_variable_name = if_else(!is.na(matched_short_variable_name), 
                                      matched_short_variable_name, 
                                      short_variable_name),
        scale_abbrev = if_else(!is.na(matched_scale_abbrev), 
                               matched_scale_abbrev, 
                               scale_abbrev),
        scale_full = if_else(!is.na(matched_scale_full), 
                             matched_scale_full, 
                             scale_full),
        subscale = if_else(!is.na(matched_subscale), 
                           matched_subscale, 
                           subscale),
        cat = if_else(!is.na(matched_cat), 
                      matched_cat, 
                      cat),
        correct_response = if_else(!is.na(matched_correct_response), 
                                   matched_correct_response, 
                                   correct_response),
        response_format = if_else(!is.na(matched_response_format), 
                                  matched_response_format, 
                                  response_format),
        coding_direction = if_else(!is.na(matched_coding_direction), 
                                   matched_coding_direction, 
                                   coding_direction),
        # Add tracking columns for reporting
        codebook_match_confidence = match_confidence,
        codebook_match_method = match_method
      )
  } else {
    message("Maximal codebook is empty - using basic template")
    enhanced_template <- basic_template %>%
      mutate(
        codebook_match_confidence = NA_real_,
        codebook_match_method = NA_character_
      )
  }
  
  if (include_summary_scores) {
    # Step 3: Add summary scores for matched scales
    message("Step 3: Adding summary score variables...")
    matched_scales <- enhanced_template %>%
      filter(!is.na(scale_abbrev), scale_abbrev != "") %>%
      pull(scale_abbrev) %>%
      unique()
    
    if (length(matched_scales) > 0) {
      summary_rows <- map_dfr(matched_scales, ~create_summary_score_row(.x, enhanced_template))
      
      # Add match metadata to summary rows
      summary_rows <- summary_rows %>%
        mutate(
          codebook_match_confidence = 1.0,
          codebook_match_method = "summary_score"
        )
      
      enhanced_template <- bind_rows(enhanced_template, summary_rows)
      message("Added summary scores for: ", paste(matched_scales, collapse = ", "))
    }
  }
  
  # Step 4: Save enhanced template
  if (!is.null(output_path)) {
    clean_survey_name <- survey_name %>% make_clean_names()
    codebook_filename <- paste0(clean_survey_name, "_generated_enhanced_codebook.csv")
    codebook_filepath <- file.path(output_path, codebook_filename)
    dir.create(dirname(codebook_filepath), recursive = TRUE, showWarnings = FALSE)
    write_csv(enhanced_template, codebook_filepath)
    
    # Generate summary report
    total_items <- nrow(enhanced_template)
    matched_items <- sum(!is.na(enhanced_template$codebook_match_confidence), na.rm = TRUE)
    items_with_scale <- sum(!is.na(enhanced_template$scale_abbrev) & 
                              enhanced_template$scale_abbrev != "", na.rm = TRUE)
    items_needing_scale <- sum(is.na(enhanced_template$scale_abbrev) | 
                                 enhanced_template$scale_abbrev == "", na.rm = TRUE)
    
    message("\n=== ENHANCED CODEBOOK COMPLETE ===")
    message("Enhanced template saved to: ", codebook_filepath)
    message("Total items: ", total_items)
    message("Items matched to maximal codebook: ", matched_items, " (", 
            round(100 * matched_items / total_items, 1), "%)")
    message("Items with scale_abbrev filled: ", items_with_scale)
    message("Items that may need scale_abbrev (only relevant for scales): ", items_needing_scale)
    message("\nNext steps:")
    message("1. Review auto-matched scales and coding directions")
    message("2. Fill in scale_abbrev for remaining ", items_needing_scale, " items")
    message("3. Use update_maximal_codebook() after manual completion")
  }
  
  return(enhanced_template)
}

# Maximal Codebook Maintenance ------------------------------------------------

#' Update Maximal Codebook from Completed Study
#'
#' Adds new scale information to the maximal codebook library from a completed
#' study codebook, building institutional knowledge for future projects.
#'
#' @param study_codebook Completed study codebook with scale information
#' @param maximal_codebook_path Path to maximal codebook library CSV
#' @param study_name Name of current study (for tracking)
#' @return Updated maximal codebook data frame
#' 
#' @examples
#' updated <- update_maximal_codebook(completed_codebook, 
#'                                   study_name = "Mindfulness Study 2025")
#'
update_maximal_codebook <- function(study_codebook, 
                                    maximal_codebook_path = "config/maximal_codebook.csv", 
                                    study_name = NULL) {
  
  # Load current maximal codebook
  current_codebook <- load_maximal_codebook(maximal_codebook_path)
  
  # Filter to completed scale items only
  scale_items <- study_codebook %>%
    filter(!is.na(scale_abbrev), scale_abbrev != "") %>%
    filter(!is.na(variable_name_R), variable_name_R != "") %>%
    filter(!is.na(question_text), question_text != "")
  
  if (nrow(scale_items) == 0) {
    message("No completed scale items found in study codebook")
    return(current_codebook)
  }
  
  message("Processing ", nrow(scale_items), " scale items for maximal codebook update...")
  
  # Prepare new entries - use standardized structure
  new_entries <- scale_items %>%
    standardize_codebook_structure()
  
  # Add new entries that don't already exist (based on question text)
  updated_codebook <- bind_rows(current_codebook, new_entries) %>%
    distinct(question_text, .keep_all = TRUE)
  
  # Save updated maximal codebook
  write_csv(updated_codebook, maximal_codebook_path)
  
  new_items <- nrow(updated_codebook) - nrow(current_codebook)
  message("Maximal codebook updated! Added ", new_items, " new items. Total size: ", nrow(updated_codebook))
  
  return(updated_codebook)
}

# Utility Functions -----------------------------------------------------------

#' Get Available Surveys from Codebook
#' 
#' Extracts unique survey scale names from a completed codebook
#' 
#' @param codebook Codebook data frame
#' @return Character vector of unique survey scale names
#' 
#' @examples
#' available_surveys <- get_available_surveys_from_codebook(codebook)
#'
get_available_surveys_from_codebook <- function(codebook) {
  if (!"scale_abbrev" %in% names(codebook)) {
    stop("Codebook must have 'scale_abbrev' column")
  }
  
  surveys <- codebook %>% 
    filter(!is.na(scale_abbrev), scale_abbrev != "", !is.na(variable_name_R)) %>%
    pull(scale_abbrev) %>% 
    unique() %>% 
    sort()
  
  return(surveys)
}

# Workflow Integration Functions ----------------------------------------------

#' Setup New Survey Project with Codebook Generation
#'
#' Complete workflow that downloads survey metadata from SurveyMonkey and 
#' generates either basic or enhanced codebook template based on availability
#' of maximal codebook library.
#'
#' @param token SurveyMonkey OAuth token
#' @param survey_name Name of survey in SurveyMonkey
#' @param codebook_output_path Where to save codebook template
#' @param use_maximal_codebook Whether to use maximal codebook for enhancement
#' @param force_regenerate Whether to overwrite existing codebook
#' @return List containing metadata paths and codebook template
#' 
#' @examples
#' \dontrun{
#' token <- load_sm_token()
#' results <- setup_new_survey_project(token, "My New Survey",
#'                                    "config/data_codebook.csv")
#' }
#'
setup_new_survey_project <- function(token, survey_name, 
                                     codebook_output_path = "config/data_codebook.csv",
                                     use_maximal_codebook = TRUE, 
                                     force_regenerate = FALSE) {
  
  message("Setting up new survey project for: ", survey_name)
  
  # Load SurveyMonkey utils if needed
  if (!exists("process_survey_structure", mode = "function")) {
    source("code/utils/external/surveymonkey_utils.R")
  }
  
  # Generate codebook template if needed
  if (!file.exists(codebook_output_path) || force_regenerate) {
    if (use_maximal_codebook) {
      message("Generating enhanced codebook template with maximal codebook...")
      codebook <- generate_enhanced_codebook_template(
        token = token,
        survey_name = survey_name, 
        output_path = codebook_output_path
      )
    } else {
      message("Generating basic codebook template...")
      codebook <- generate_basic_codebook_template(
        token = token,
        survey_name = survey_name,
        output_path = dirname(codebook_output_path)
      )
      
      # Save to specified path
      write_csv(codebook, codebook_output_path)
    }
  } else {
    message("Using existing codebook: ", codebook_output_path)
    codebook <- read_csv(codebook_output_path, show_col_types = FALSE)
  }
  
  message("\n=== PROJECT SETUP COMPLETE ===")
  message("Codebook: ", nrow(codebook), " variables")
  message("Codebook saved: ", codebook_output_path)
  
  message("\nNext steps:")
  message("1. Edit the codebook: ", codebook_output_path)
  message("2. Fill in 'scale_abbrev' and 'scale_full' names for survey instruments")  
  message("3. Set 'coding_direction' (-1 for reverse, 1 for forward)")
  message("4. Generate response data using process_survey_responses() from surveymonkey_utils")
  message("5. Run data processing pipeline to clean and score the data")
  if (use_maximal_codebook) {
    message("6. Use update_maximal_codebook() to add new scales to the maximal codebook")
  }
  
  return(list(
    codebook_path = codebook_output_path,
    codebook = codebook
  ))
}

#' Add Computed Score Variables to Codebook
#' 
#' Creates codebook entries for total and subscale scores after they've been computed.
#' Calculates theoretical min/max based on item ranges, not actual data.
#' This function should be added to data_codebook_utils.R
#' 
#' @param codebook Data codebook data frame
#' @param survey_list Character vector of surveys that were scored
#' @param suffix Suffix used for total score columns (default: "_total")
#' @param output_path Optional path to save updated codebook
#' @return Updated codebook with score variable entries added
#' 
#' @examples
#' updated_codebook <- add_computed_scores_to_codebook(codebook, c("phq", "gad"))
#'
add_computed_scores_to_codebook <- function(codebook, survey_list, 
                                            suffix = "_total", 
                                            output_path = NULL) {
  
  message("Adding computed score variables to codebook...")
  
  score_rows <- list()
  
  for (survey_name in survey_list) {
    # Get all items for this survey
    survey_items <- codebook %>%
      filter(scale_abbrev == survey_name) %>%
      filter(!is.na(short_variable_name), short_variable_name != "")
    
    if (nrow(survey_items) == 0) {
      warning("No items found for survey: ", survey_name)
      next
    }
    
    # Get scale info
    scale_info <- survey_items %>%
      select(scale_abbrev, scale_full) %>%
      distinct() %>%
      slice(1)
    
    # Calculate total score min/max
    total_min <- sum(survey_items$min, na.rm = TRUE)
    total_max <- sum(survey_items$max, na.rm = TRUE)
    
    # Create total score row
    total_var_name <- paste0(survey_name, suffix)
    
    # Check if this total already exists
    if (!total_var_name %in% codebook$variable_name) {
      score_rows[[length(score_rows) + 1]] <- tibble(
        col_num = max(codebook$col_num, na.rm = TRUE) + length(score_rows) + 1,
        question_text = paste(scale_info$scale_full, "- Total Score"),
        variable_name = total_var_name,
        short_variable_name = total_var_name,
        variable_name_R = total_var_name,
        cat = "computed_scores",
        scale_abbrev = survey_name,
        scale_full = scale_info$scale_full,
        subscale = NA_character_,
        coding_direction = NA_real_,
        min = total_min,
        max = total_max,
        response_choices = NA_character_,
        response_coding = NA_character_,
        response_format = "numeric",
        required = NA,
        correct_response = NA_character_,
        question_format = "computed",
        family = NA_character_,
        subtype = NA_character_,
        matrix_row_id = NA_character_,
        matrix_row_text = NA_character_,
        question_id = paste0("computed_", total_var_name),
        page_number = NA_integer_
      )
    }
    
    # Check if survey has subscales
    if ("subscale" %in% names(survey_items)) {
      subscales <- survey_items %>%
        filter(!is.na(subscale), subscale != "") %>%
        pull(subscale) %>%
        unique()
      
      # Create subscale score rows
      for (subscale_name in subscales) {
        subscale_items <- survey_items %>%
          filter(subscale == subscale_name)
        
        subscale_min <- sum(subscale_items$min, na.rm = TRUE)
        subscale_max <- sum(subscale_items$max, na.rm = TRUE)
        
        subscale_var_name <- paste0(survey_name, "_", subscale_name)
        
        # Check if this subscale already exists
        if (!subscale_var_name %in% codebook$variable_name) {
          score_rows[[length(score_rows) + 1]] <- tibble(
            col_num = max(codebook$col_num, na.rm = TRUE) + length(score_rows) + 1,
            question_text = paste(scale_info$scale_full, "-", subscale_name, "Subscale"),
            variable_name = subscale_var_name,
            short_variable_name = subscale_var_name,
            variable_name_R = subscale_var_name,
            cat = "computed_scores",
            scale_abbrev = survey_name,
            scale_full = scale_info$scale_full,
            subscale = subscale_name,
            coding_direction = NA_real_,
            min = subscale_min,
            max = subscale_max,
            response_choices = NA_character_,
            response_coding = NA_character_,
            response_format = "numeric",
            required = NA,
            correct_response = NA_character_,
            question_format = "computed",
            family = NA_character_,
            subtype = NA_character_,
            matrix_row_id = NA_character_,
            matrix_row_text = NA_character_,
            question_id = paste0("computed_", subscale_var_name),
            page_number = NA_integer_
          )
        }
      }
    }
  }
  
  # Combine all score rows
  if (length(score_rows) == 0) {
    message("No new score variables to add (all already exist in codebook)")
    return(codebook)
  }
  
  score_rows_df <- bind_rows(score_rows)
  
  # Ensure consistent column types before binding with codebook
  score_rows_df <- score_rows_df %>%
    mutate(
      matrix_row_id = as.character(matrix_row_id),
      question_id = as.character(question_id)
    )
  
  # Also ensure codebook has consistent types
  codebook <- codebook %>%
    mutate(
      matrix_row_id = as.character(matrix_row_id),
      question_id = as.character(question_id)
    )
  
  
  # Add to codebook
  updated_codebook <- bind_rows(codebook, score_rows_df)
  
  # Count what was added
  n_total <- sum(str_detect(score_rows_df$variable_name, paste0(suffix, "$")))
  n_subscale <- nrow(score_rows_df) - n_total
  
  message("Added ", nrow(score_rows_df), " computed score variables to codebook")
  message("  Total scores: ", n_total)
  message("  Subscale scores: ", n_subscale)
  
  # Save if path provided
  if (!is.null(output_path)) {
    write_csv(updated_codebook, output_path)
    message("Updated codebook saved to: ", output_path)
  }
  
  return(updated_codebook)
}

# Utility Functions -----------------------------------------------------------

# Null coalescing operator
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x