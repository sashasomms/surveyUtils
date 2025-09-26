# Data Wrangling Utilities for Survey Data Processing ========================
#' 
#' This script contains reusable functions for processing survey data,
#' particularly data exported from SurveyMonkey with double headers
#' and various question format types.
#'
#' Dependencies: dplyr, tidyr, stringr, janitor
#' 
#' @author Sasha Sommerfeldt
#' @date 2025
#' 

# Setup and Dependencies ------------------------------------------------------

library(dplyr)
library(tidyr)
library(stringr)
library(janitor)


# Attention Checks ------------------------------------------------------------
#' Get Attention Check Questions from Codebook
#'
#' @param codebook Data frame containing question metadata
#' @return Data frame with attention check variable names and correct responses
#'
get_attention_checks <- function(codebook) {
  # Make sure to include short_variable_name if it exists in codebook
  cols_to_select <- c("variable_name", "correct_response")
  if ("short_variable_name" %in% names(codebook)) {
    cols_to_select <- c("variable_name", "short_variable_name", "correct_response")
  }
  
  attention_checks <- codebook %>%
    filter(cat == "attention_check", !is.na(correct_response), correct_response != "") %>%
    select(all_of(cols_to_select))
  
  if (nrow(attention_checks) == 0) {
    message("No attention check questions found in codebook")
  } else {
    message("Found ", nrow(attention_checks), " attention check questions")
  }
  
  return(attention_checks)
}

#' Score Individual Attention Checks
#'
#' @param responses_df Data frame of survey responses
#' @param attention_checks Data frame from get_attention_checks()
#' @return Data frame with attention check scores added
#'
score_individual_attention_checks <- function(responses_df, attention_checks) {
  
  if (nrow(attention_checks) == 0) {
    message("No attention checks found, returning same df")
    return(responses_df)
  }
  
  for (i in 1:nrow(attention_checks)) {
    var_name <- attention_checks$variable_name[i]
    correct_responses <- attention_checks$correct_response[i]
    
    # Get short_variable_name if it exists in the data frame
    short_var_name <- if ("short_variable_name" %in% names(attention_checks)) {
      attention_checks$short_variable_name[i]
    } else {
      NA
    }
    
    # Determine which column name to use (prefer short_variable_name if available)
    col_name <- NULL
    if (!is.na(short_var_name) && short_var_name != "" && short_var_name %in% names(responses_df)) {
      col_name <- short_var_name
    } else if (var_name %in% names(responses_df)) {
      col_name <- var_name
    }
    
    # Check if column exists in data
    if (is.null(col_name)) {
      available_names <- if (!is.na(short_var_name)) {
        paste0("'", var_name, "' or '", short_var_name, "'")
      } else {
        paste0("'", var_name, "'")
      }
      warning("Attention check column ", available_names, " not found in data. Available columns: ", 
              paste(head(names(responses_df), 10), collapse = ", "))
      next
    }
    
    # Handle multiple correct responses (semicolon separated)
    correct_options <- str_split(correct_responses, ";")[[1]] %>% str_trim()
    
    # Score: 1 if correct, 0 if incorrect
    responses_df[[paste0(col_name, '_attn_correct')]] <- ifelse(
      responses_df[[col_name]] %in% correct_options, 1, 0
    )
    
    # Set NA responses to 0
    responses_df[is.na(responses_df[[paste0(col_name, '_attn_correct')]]), paste0(col_name, '_attn_correct')] <- 0
    
    message("Scored: ", col_name, " (", sum(responses_df[[paste0(col_name, '_attn_correct')]]), 
            "/", nrow(responses_df), " correct)")
  }
  
  return(responses_df)
}

#' Calculate Overall Attention Score
#'
#' @param responses_df Data frame with individual attention check scores
#' @param attention_checks Data frame from get_attention_checks()
#' @return Data frame with attn_score column added
#'
calculate_attention_score <- function(responses_df, attention_checks) {
  
  if (nrow(attention_checks) == 0) {
    return(responses_df)
  }
  
  # Simply find all attention check columns that were created
  attn_cols <- responses_df %>% select(ends_with('_attn_correct')) %>% names()
  
  # Calculate overall attention score
  if (length(attn_cols) > 0) {
    responses_df$attn_score <- rowSums(responses_df[attn_cols], na.rm = TRUE) / length(attn_cols)
  } else {
    warning("No attention check columns found for scoring")
    responses_df$attn_score <- NA
  }
  
  return(responses_df)
}

#' Export Attention Check Failures
#'
#' @param responses_df Data frame with attention scores
#' @param threshold Minimum proportion to pass
#' @param output_dir Directory to save failures file
#' @param filename Custom filename (optional)
#' @return NULL (side effect: writes CSV file)
#'
export_attention_failures <- function(responses_df, threshold, output_dir) {
  
  attn_fails <- responses_df %>% filter(attn_score < threshold)
  n_fails <- nrow(attn_fails)
  
  if (n_fails == 0) {
    message("No attention check failures to export")
    return(invisible(NULL))
  }
  
  # Try to find ID columns for payment systems
  id_cols <- names(attn_fails)[str_detect(names(attn_fails), "_id$|prolific|prodege|cloud_research")]
  other_cols <- c("age", "attn_score", "survey_duration", "start_date", "end_date")
  export_cols <- c(id_cols, other_cols[other_cols %in% names(attn_fails)])
  
  clean_survey_name <- survey_name %>% make_clean_names()
  out_filename <- paste0(clean_survey_name, "_attention_fails.csv")
  out_filepath <- file.path(output_dir, out_filename)
  dir.create(dirname(out_filepath), recursive = TRUE, showWarnings = FALSE)
  attn_fails %>% select(all_of(export_cols)) %>% write_csv(out_filepath)
  message("Attention check failures exported to: ", out_filepath)
}

#' Remove Attention Check Failures
#'
#' @param responses_df Data frame with attention scores
#' @param threshold Minimum proportion to pass
#' @return Data frame with failures removed
#'
remove_attention_failures <- function(responses_df, threshold) {
  
  n_original <- nrow(responses_df)
  cleaned_df <- responses_df %>% filter(attn_score >= threshold)
  n_retained <- nrow(cleaned_df)
  n_removed <- n_original - n_retained
  
  message("Removed ", n_removed, " participants (", round(n_removed/n_original*100, 1), "%)")
  message("Participants retained: ", n_retained, "/", n_original)
  
  return(cleaned_df)
}

#' Score Attention Checks Using Codebook (Main Function)
#'
#' @param responses_df Data frame of survey responses
#' @param codebook Data frame containing question metadata
#' @param threshold Minimum proportion of attention checks to pass (default 0.8)
#' @param export_attention_failures Export failure list for payment exclusion (default FALSE)
#' @param output_dir Directory to save attention check failures file
#' @param remove_attention_fails Remove failed participants from dataset (default FALSE)
#' @param filename Custom filename for failures export
#' @return Data frame with attention checks scored
#'
score_attention_checks <- function(responses_df, 
                                   codebook, 
                                   threshold = 0.8,
                                   export_attention_failures = FALSE,
                                   output_dir = NULL,
                                   remove_attention_fails = FALSE) {
  
  # Get attention check questions
  attention_checks <- get_attention_checks(codebook)
  
  # Score individual checks
  scored_df <- score_individual_attention_checks(responses_df, attention_checks)
  
  # Calculate overall score
  scored_df <- calculate_attention_score(scored_df, attention_checks)
  
  if (nrow(attention_checks) > 0) {
    n_fails <- sum(scored_df$attn_score < threshold)
    message("Attention check failures: ", n_fails, " (", round(n_fails/nrow(scored_df)*100, 1), "%)")
  }
  
  # Export failures if requested
  if (export_attention_failures && !is.null(output_dir)) {
    export_attention_failures(scored_df, threshold, output_dir)
  }
  
  # Remove failures if requested
  if (remove_attention_fails) {
    scored_df <- remove_attention_failures(scored_df, threshold)
  }
  
  return(scored_df)
}

# Column Naming ----------------------------------------------------------------

#' Process Double Headers from SurveyMonkey Export
#' 
#' Processes SurveyMonkey data with double header format where the first row
#' contains question text and the second row contains response choices. Combines
#' these into meaningful snake_case column names. For multi-select questions,
#' removes number suffixes from column names and appends response choice text.
#' For other question types, preserves number suffixes (e.g., child_1, child_2).
#' 
#' @param data A data frame with double headers where:
#'   - Row 1 contains question text (becomes column names)
#'   - Row 2 contains response choices (appended to multi-select column names)
#'   - Row 3+ contains actual survey response data
#' @param codebook Data frame containing survey metadata with columns:
#'   - variable_name: Full variable names from survey
#'   - short_variable_name: Abbreviated variable names  
#'   - question_format: Question type ("multi_select", "single_select", "matrix", etc.)
#' @param remove_suffixes Character vector of suffixes to remove from final column names
#' @param remove_prefixes Character vector of prefixes to remove from final column names
#' 
#' @return A cleaned data frame with:
#'   - Meaningful snake_case column names
#'   - Multi-select columns named as "base_name_response_choice" 
#'   - Non-multi-select numbered columns preserved (e.g., "child_age_1")
#'   - Double header rows removed
#'   - HTML tags stripped from all content
#' 
#' @details 
#' The function works by:
#' 1. Identifying multi-select questions using the codebook
#' 2. For multi-selects: strips trailing numbers and appends response choice text
#' 3. For other questions: preserves original numbering
#' 4. Applies snake_case formatting to all column names
#' 5. Removes specified prefixes and suffixes
#' 
#' Multi-select example: 
#' - Input columns: demo_race_ethnicity_17, demo_race_ethnicity_18
#' - Output columns: demo_race_ethnicity_black_or_african_american, demo_race_ethnicity_hispanic_or_latinx
#' 
#' Non-multi-select example:
#' - Input columns: child_age_1, child_age_2  
#' - Output columns: child_age_1, child_age_2 (preserved)
#' 
#' @examples
#' \dontrun{
#' # Basic usage
#' clean_data <- process_double_headers(raw_survey_data, codebook)
#' 
#' # With custom suffix removal
#' clean_data <- process_double_headers(
#'   raw_survey_data, 
#'   codebook,
#'   remove_suffixes = c("_response", "_open_ended", "_other")
#' )
#' }
#' 
#' @seealso 
#' \code{\link{apply_short_variable_names}} for renaming columns before double header processing
#' 
#' @export
process_double_headers <- function(data, 
                                   codebook,
                                   remove_suffixes = c("_response", "_open_ended"),
                                   remove_prefixes = c("in_the_past_week_")) {
  
  # Convert to tibble for consistent handling
  df <- as_tibble(data)
  
  # Process double header columns
  df_dblhead_cols <- df %>%
    slice(1) %>%
    select_if(negate(is.na)) %>%
    mutate(across(everything(), as.character)) %>%
    pivot_longer(everything()) %>%
    rowwise() %>%
    mutate(
      # For multi-select, determine base name but keep original for grouping
      base_name = {
        if (str_detect(name, "_\\d+$")) {
          base <- str_replace(name, "_\\d+$", "")
          
          # Check if this base name matches any row in codebook
          codebook_match <- codebook %>%
            filter((short_variable_name == base & !is.na(short_variable_name)) | 
                     (variable_name == base & !is.na(variable_name))) %>%
            slice(1)
          
          if (nrow(codebook_match) > 0 && !is.na(codebook_match$question_format) && 
              codebook_match$question_format == "multi_select") {
            base
          } else {
            name
          }
        } else {
          name
        }
      }
    ) %>%
    ungroup() %>%
    mutate(
      # Create final column name with response choice
      final_name = if_else(
        value != "" & !is.na(value),
        paste0(base_name, "_", make_clean_names(value)),
        base_name
      )
    ) %>%
    select(name, final_name)
  
  # Create named vector for renaming (new_name = old_name)
  rename_vector <- setNames(df_dblhead_cols$name, df_dblhead_cols$final_name)
  
  # Apply the combined headers 
  df <- df %>%
    rename(any_of(rename_vector)) %>%
    slice(-1) %>%
    clean_names()
  
  # Remove specified suffixes and prefixes...
  
  return(df)
}


#' Apply Short Variable Names from Codebook
#'
#' Renames columns to use short_variable_name from codebook where available.
#' For multi-select questions, strips trailing numbers before matching.
#' Should be run BEFORE processing double headers so multi-select response 
#' choices get appended to the short names.
#'
#' @param data Data frame with cleaned column names 
#' @param codebook Data frame containing variable_name and short_variable_name columns
#' @return Data frame with columns renamed to short_variable_name where available
#'
apply_short_names <- function(data, codebook) {
  
  # Filter codebook to items with short variable names
  codebook_with_short <- codebook %>%
    filter(!is.na(short_variable_name), 
           short_variable_name != "", 
           short_variable_name != variable_name)
  
  if (nrow(codebook_with_short) == 0) {
    message("No short variable names found in codebook for renaming")
    return(data)
  }
  
  # Process each column name
  current_names <- names(data)
  rename_vector <- character(0)
  
  for (col_name in current_names) {
    # Strip trailing numbers to try matching
    base_name <- str_replace(col_name, "_\\d+$", "")
    
    # Look for match in codebook
    codebook_match <- codebook_with_short %>%
      filter(variable_name == base_name)
    
    if (nrow(codebook_match) > 0) {
      # Found a match - use short name and preserve any number suffix
      suffix <- str_extract(col_name, "_\\d+$")
      if (!is.na(suffix)) {
        new_name <- paste0(codebook_match$short_variable_name[1], suffix)
      } else {
        new_name <- codebook_match$short_variable_name[1]
      }
      rename_vector[new_name] <- col_name
    }
  }
  
  if (length(rename_vector) == 0) {
    message("No columns matched for renaming")
    return(data)
  }
  
  # Apply the mapping using rename()
  data_renamed <- data %>% rename(any_of(rename_vector))
  
  message("Renamed ", length(rename_vector), " columns using short_variable_name from codebook")
  return(data_renamed)
  
  message("Renamed ", length(name_mapping), " columns using short_variable_name from codebook")
  return(data_renamed)
}


#' Create Summary Columns for Multi-Select Questions
#' 
#' Creates concatenated summary columns for multi-select questions using codebook
#' to identify which questions are multi-selects. The summary shows all selected
#' response choices in a single column.
#' 
#' @param data A data frame with processed multi-select columns 
#' @param codebook Data frame containing question metadata with short_variable_name and question_format
#' @return Data frame with summary columns added for multi-select questions
#' 
#' @details
#' For each multi-select question, creates a summary column named "{base_name}_summary"
#' that concatenates all selected response choices with commas.
#' 
#' Example: If someone selected "Black or African American" and "Hispanic or Latinx"
#' for race/ethnicity, the summary column would contain: 
#' "Black or African American, Hispanic or Latinx"
#' 
#' @examples
#' \dontrun{
#' data_with_summaries <- create_multiselect_summaries(processed_data, codebook)
#' }
#'
create_multiselect_summary_cols <- function(data, codebook) {
  
  # Find multi-select questions in codebook
  multiselect_questions <- codebook %>%
    filter(question_format == "multi_select") %>%
    select(short_variable_name) %>%
    filter(!is.na(short_variable_name), short_variable_name != "") %>%
    distinct() %>%
    pull(short_variable_name)
  
  if (length(multiselect_questions) == 0) {
    message("No multi-select questions found in codebook")
    return(data)
  }
  
  for (base_name in multiselect_questions) {
    # Find all columns that start with this base name
    related_cols <- names(data)[str_starts(names(data), paste0(base_name, "_"))]
    
    if (length(related_cols) > 0) {
      # Create summary column name
      summary_col_name <- paste0(base_name, "_summary")
      
      # Create summary by concatenating non-NA, non-empty responses
      data[[summary_col_name]] <- apply(data[related_cols], 1, function(row) {
        # Get non-NA, non-empty values
        selected <- row[!is.na(row) & row != ""]
        
        if (length(selected) > 0) {
          # Extract the choice part from column names and clean up
          choice_names <- names(selected) %>%
            str_replace(paste0("^", base_name, "_"), "") %>%
            str_replace_all("_", " ") %>%
            str_to_title()
          paste(choice_names, collapse = ", ")
        } else {
          ""
        }
      })
      
      message("Created summary column: ", summary_col_name, " (", length(related_cols), " source columns)")
    }
  }
  
  return(data)
}

# Data Cleaning and Processing Functions ====================================

#' Parse Date Columns with Auto-Detection
#'
#' Helper function to parse date columns using flexible format detection.
#'
#' @param data Data frame containing date columns
#' @param date_cols Character vector of column names containing dates
#' @param date_format Optional specific date format (e.g., "ISO8601", "mdy_HMS")
#' @return Data frame with parsed date columns (same names, POSIXct format)
#'
parse_date_columns <- function(data, date_cols, date_format = NULL) {
  
  for (col in date_cols) {
    if (!col %in% names(data)) {
      warning("Column ", col, " not found in data")
      next
    }
    
    # Check if column has any non-NA values
    sample_date <- data[[col]][!is.na(data[[col]])][1]
    if (is.na(sample_date)) {
      warning("No valid dates found in column: ", col)
      next
    }
    
    # Parse based on specified format or auto-detect
    if (!is.null(date_format)) {
      # Use specified format
      if (date_format == "ISO8601") {
        data[[col]] <- as.POSIXct(data[[col]], format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
      } else if (date_format == "mdy_HMS") {
        data[[col]] <- mdy_hms(data[[col]])
      } else {
        # Custom format provided
        data[[col]] <- as.POSIXct(data[[col]], format = date_format)
      }
      message("Using specified format '", date_format, "' for column: ", col)
    } else {
      # Auto-detect with flexible parser
      data[[col]] <- parse_date_time(data[[col]], 
                                     orders = c("ymd HMS", "mdy HMS", "dmy HMS", "ymd HM", "mdy HM"))
      message("Auto-detecting date format for column: ", col)
    }
    
    # Check parsing success
    parsed_count <- sum(!is.na(data[[col]]))
    total_count <- nrow(data)
    
    if (parsed_count < total_count * 0.9) {
      warning("Only ", parsed_count, " out of ", total_count, 
              " dates parsed successfully in column: ", col)
    } else {
      message("Successfully parsed ", parsed_count, " dates in column: ", col)
    }
  }
  
  return(data)
}

#' Filter Survey Data by Date Range
#'
#' Filters survey responses based on start date, typically to remove QA/testing data
#' collected before official launch.
#'
#' @param data Survey response data frame
#' @param start_date_col Name of column containing start dates
#' @param min_date Minimum date to include (as Date or character in "YYYY-MM-DD" format)
#' @param max_date Maximum date to include (optional)
#' @param date_format Format of dates in the data (default: "ISO8601" for SurveyMonkey)
#' @return Filtered data frame with date filtering applied
#'
filter_by_date_range <- function(data, 
                                 start_date_col = "start_date",
                                 end_date_col = "end_date",
                                 min_date = NULL,
                                 max_date = NULL,
                                 date_format = "ISO8601") {
  
  n_start <- nrow(data)
  message("Starting with ", n_start, " responses")
  
  if (is.null(min_date) && is.null(max_date)) {
    message("No date filters specified, returning original data")
    return(data)
  }
  
  # Parse the date column
  data <- parse_date_columns(data, c(start_date_col, end_date_col), date_format)
  
  # Apply date filters
  if (!is.null(min_date)) {
    min_date <- as.POSIXct(min_date)
    n_before_min <- sum(data[[start_date_col]] < min_date, na.rm = TRUE)
    message("Removing ", n_before_min, " responses before ", min_date)
    data <- data %>% filter(.data[[start_date_col]] >= min_date)
  }
  
  if (!is.null(max_date)) {
    max_date <- as.POSIXct(max_date)
    n_after_max <- sum(data[[start_date_col]] > max_date, na.rm = TRUE)
    message("Removing ", n_after_max, " responses after ", max_date)
    data <- data %>% filter(.data[[start_date_col]] <= max_date)
  }
  
  n_final <- nrow(data)
  message("Retained ", n_final, " responses after date filtering")
  
  return(data)
}

#' Calculate Survey Duration
#'
#' Calculates the time taken to complete the survey based on start and end timestamps.
#'
#' @param data Survey response data frame
#' @param start_date_col Name of column containing start timestamps
#' @param end_date_col Name of column containing end timestamps  
#' @param units Time units for duration ("mins", "hours", "secs")
#' @param date_format Format of timestamps in the data
#' @return Data frame with survey_duration column added
#'
calculate_survey_duration <- function(data,
                                      start_date_col = "start_date",
                                      end_date_col = "end_date", 
                                      units = "mins",
                                      date_format = "mdy_HMS") {
  
  message("Calculating survey duration in ", units)
  
  # Parse timestamps based on format
  if (date_format == "mdy_HMS") {
    data <- data %>%
      mutate(
        start_parsed = mdy_hms(.data[[start_date_col]]),
        end_parsed = mdy_hms(.data[[end_date_col]])
      )
  } else {
    data <- data %>%
      mutate(
        start_parsed = as.POSIXct(.data[[start_date_col]]),
        end_parsed = as.POSIXct(.data[[end_date_col]])
      )
  }
  
  # Calculate duration
  data <- data %>%
    mutate(survey_duration = as.numeric(difftime(end_parsed, start_parsed, units = units))) %>%
    select(-start_parsed, -end_parsed)
  
  # Summary statistics
  duration_stats <- data %>%
    summarise(
      mean_duration = mean(survey_duration, na.rm = TRUE),
      median_duration = median(survey_duration, na.rm = TRUE),
      min_duration = min(survey_duration, na.rm = TRUE),
      max_duration = max(survey_duration, na.rm = TRUE),
      .groups = "drop"
    )
  
  message("Duration summary (", units, "): ",
          "Mean = ", round(duration_stats$mean_duration, 1),
          ", Median = ", round(duration_stats$median_duration, 1),
          ", Range = ", round(duration_stats$min_duration, 1), 
          " to ", round(duration_stats$max_duration, 1))
  
  return(data)
}

#' Convert Response Text to Numeric Codes Using Codebook
#'
#' Maps text responses (e.g., "Strongly Disagree") to their corresponding numeric 
#' codes (e.g., "5") using the response_choices and response_coding columns from 
#' the codebook. This should be run BEFORE converting data types to numeric.
#'
#' @param data Survey response data frame
#' @param codebook Data frame containing variable metadata with response_choices and response_coding columns
#' @param response_choices_col Column name in codebook containing response text options (default "response_choices")
#' @param response_coding_col Column name in codebook containing corresponding numeric codes (default "response_coding")
#' @return Data frame with text responses converted to numeric codes (still as character)
#'
#' @details
#' The function works by:
#' 1. Parsing the response_choices and response_coding columns (assumed to be delimited strings)
#' 2. Creating mapping dictionaries for each survey item
#' 3. Replacing text responses with their numeric codes
#' 4. Preserving original values that don't have mappings (e.g., open-ended responses)
#' 
#' Expected codebook format:
#' - response_choices: "Strongly Disagree;Disagree;Neither;Agree;Strongly Agree"
#' - response_coding: "1;2;3;4;5"
#' 
#' @examples
#' \dontrun{
#' # Convert text responses to codes before numeric conversion
#' data_coded <- convert_response_text_to_codes(data, codebook)
#' }
#'
convert_response_text_to_codes <- function(data, 
                                           codebook,
                                           response_choices_col = "response_choices",
                                           response_coding_col = "response_coding") {
  
  message("Converting text responses to numeric codes using codebook...")
  
  # Filter codebook to rows marked for numeric conversion with response mappings
  codebook_to_numeric <- codebook %>%
    filter(
      response_format == "numeric",
      !is.na(.data[[response_choices_col]]) & .data[[response_choices_col]] != "",
      !is.na(.data[[response_coding_col]]) & .data[[response_coding_col]] != ""
    )
  
  if (nrow(codebook_to_numeric) == 0) {
    message("No variables found with response_format = 'numeric' and complete response mappings")
    return(data)
  }
  
  # Track conversion statistics
  columns_processed <- 0
  
  # Process each variable with response mappings
  for (i in 1:nrow(codebook_to_numeric)) {
    var_info <- codebook_to_numeric[i, ]

    # Determine which column name to use (prefer short_variable_name if available)
    col_name <- NULL
    if ("short_variable_name" %in% names(var_info) && 
        !is.na(var_info$short_variable_name) && 
        var_info$short_variable_name != "" && 
        var_info$short_variable_name %in% names(data)) {
      col_name <- var_info$short_variable_name
    } else if ("variable_name" %in% names(var_info) && 
               !is.na(var_info$variable_name) && 
               var_info$variable_name != "" && 
               var_info$variable_name %in% names(data)) {
      col_name <- var_info$variable_name
    }
    
    # Skip if column not found in data
    if (is.null(col_name)) {
      message("Not able to complete conversion for ", var_info$question_text)
      next
    }
    
    # Parse response choices and codes
    response_choices <- str_split(var_info[[response_choices_col]], ";")[[1]] %>% 
      str_trim()
    response_codes <- str_split(var_info[[response_coding_col]], ";")[[1]] %>% 
      str_trim()
    
    # Validate that choices and codes have same length
    if (length(response_choices) != length(response_codes)) {
      warning("Mismatch in response choices and codes for variable: ", col_name,
              " (", length(response_choices), " choices vs ", length(response_codes), " codes)")
      next
    }
    
    # Create mapping dictionary
    response_mapping <- setNames(response_codes, response_choices)
    
    # Apply mapping to the column
    original_values <- data[[col_name]] %>% str_trim()
    data[[col_name]] <- ifelse(
      original_values %in% names(response_mapping),
      response_mapping[original_values],
      original_values  # Keep original value if no mapping found
    )
    
    columns_processed <- columns_processed + 1
    
  }
  
  message("Response text to code conversion complete!")
  message("Processed ", columns_processed, " columns")
  
  return(data)
}

#' Convert Columns to Numeric Data Type with Validation
#'
#' Converts specified columns to numeric and validates that response patterns
#' remain unchanged after conversion.
#' Should be used after convert_response_text_to_codes() has already mapped
#' text responses to numeric codes.
#'
#' @param data Data frame containing columns to convert
#' @param cols_to_convert Character vector of column names to convert to numeric
#' @return Data frame with specified columns converted to numeric data type
#'
convert_columns_to_numeric <- function(data, cols_to_convert) {
  
  # Filter to columns that actually exist in data
  existing_cols <- cols_to_convert[cols_to_convert %in% names(data)]
  missing_cols <- cols_to_convert[!cols_to_convert %in% names(data)]
  
  if (length(missing_cols) > 0) {
    message("Columns not found in data: ", paste(missing_cols, collapse = ", "))
  }
  
  if (length(existing_cols) == 0) {
    message("No columns found to convert")
    return(data)
  }
  
  message("Checking ", length(existing_cols), " columns for conversion to numeric data type")
  
  # Store frequency tables before conversion for validation
  before_convert <- map(data[existing_cols], ~ table(., useNA = "ifany"))
  
  # Convert to numeric (temp version) - suppress warnings for validation
  temp_data <- data %>%
    mutate(across(all_of(existing_cols), ~ suppressWarnings(as.numeric(.))))
  
  # Store frequency tables after conversion for validation
  after_convert <- map(temp_data[existing_cols], ~ table(., useNA = "ifany"))
  
  # Validate each column conversion
  safe_to_convert_cols <- character(0)
  for (col in existing_cols) {
    before_table <- before_convert[[col]]
    after_table <- after_convert[[col]]
    
    # Check if same number of unique values
    before_unique <- length(before_table)
    after_unique <- length(after_table)
    
    # Replace this faulty validation:
    frequencies_match <- identical(sort(as.numeric(before_table)), sort(as.numeric(after_table)))
    
    # With this correct validation:
    original_nas <- sum(is.na(data[[col]]))
    converted_nas <- sum(is.na(temp_data[[col]]))
    no_new_nas <- converted_nas == original_nas
    
    if (frequencies_match && before_unique == after_unique && no_new_nas) {
      safe_to_convert_cols <- c(safe_to_convert_cols, col)
    } else {
      warning("Conversion issue for column: ", col)
      message("Conversion issue for column: ", col)
      message("    Before conversion - unique values: ", before_unique)
      message("    After conversion - unique values: ", after_unique)
      message("    Before frequencies: ", paste(names(before_table), "=", before_table, collapse = ", "))
      message("    After frequencies: ", paste(names(after_table), "=", after_table, collapse = ", "))
      
      # Check for specific issues
      original_nas <- sum(is.na(data[[col]]))
      converted_nas <- sum(is.na(temp_data[[col]]))
      if (converted_nas > original_nas) {
        new_nas <- converted_nas - original_nas
        message("    WARNING: ", new_nas, " new NAs introduced - check for non-numeric values")
      }
    }
  }
  
  # For cols that passed validation, do actual conversion to numeric
  if (length(safe_to_convert_cols) > 0) {
    message("Final conversion: ", length(safe_to_convert_cols), " of requested ", length(existing_cols), " columns passed validation and were converted to numeric.")
    data <- data %>%
      mutate(across(all_of(safe_to_convert_cols), as.numeric))
  } else {
    message("No columns passed validation for numeric conversion")
  }
  
  return(data)
}

#' Convert Columns to Numeric Using Codebook
#'
#' Converts survey response columns to numeric based on codebook metadata.
#' Identifies numeric columns by presence of min/max values in codebook.
#'
#' @param data Survey response data frame
#' @param codebook Data frame containing variable metadata
#' @param min_col Column in codebook indicating minimum numeric value
#' @param max_col Column in codebook indicating maximum numeric value
#' @return Data frame with numeric conversion applied
#'
convert_to_numeric_from_codebook <- function(data, 
                                             codebook,
                                             response_choices_col = "response_choices",
                                             response_coding_col = "response_coding") {
  
  # Identify columns that should be numeric (have min/max values)
  numeric_vars <- codebook %>%
    filter(response_format == "numeric",
           !is.na(.data[[response_choices_col]]) & .data[[response_choices_col]] != "",
           !is.na(.data[[response_coding_col]]) & .data[[response_coding_col]] != "") %>%
    filter(short_variable_name %in% names(data) | variable_name %in% names(data))
  
  if (nrow(numeric_vars) == 0) {
    message("No numeric variables found in codebook that match data columns")
    return(data)
  }
  
  # Get column names to convert (prefer short_variable_name if available)
  cols_to_convert <- numeric_vars %>%
    mutate(
      col_name = case_when(
        short_variable_name %in% names(data) ~ short_variable_name,
        variable_name %in% names(data) ~ variable_name,
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(col_name)) %>%
    pull(col_name)
  
  # Use the base conversion function
  data <- convert_columns_to_numeric(data, cols_to_convert)
  
  return(data)
}

#' Reverse Code Specified Columns
#'
#' Reverse codes specified columns using provided min/max values.
#' Creates new columns with "_R" suffix to preserve originals.
#'
#' @param data Data frame containing columns to reverse code
#' @param reverse_specs Named list where names are column names and values are 
#'   lists with 'min' and 'max' elements, e.g. list(col1 = list(min = 1, max = 5))
#' @return Data frame with reverse-coded columns added
#'
reverse_code_columns <- function(data, reverse_specs) {
  
  # Filter to columns that actually exist in data
  existing_cols <- names(reverse_specs)[names(reverse_specs) %in% names(data)]
  missing_cols <- names(reverse_specs)[!names(reverse_specs) %in% names(data)]
  
  if (length(missing_cols) > 0) {
    message("Columns not found in data: ", paste(missing_cols, collapse = ", "))
  }
  
  if (length(existing_cols) == 0) {
    message("No columns found to reverse code")
    return(data)
  }
  
  message("Reverse coding ", length(existing_cols), " columns")
  
  for (col_name in existing_cols) {
    spec <- reverse_specs[[col_name]]
    min_val <- spec$min
    max_val <- spec$max
    new_col_name <- paste0(col_name, "_R")
    
    # Reverse code: new_value = (min + max) - old_value
    data[[new_col_name]] <- (min_val + max_val) - data[[col_name]]
    
    # Validate reverse coding worked
    original_range <- range(data[[col_name]], na.rm = TRUE)
    reversed_range <- range(data[[new_col_name]], na.rm = TRUE)
    
    message("Reverse coded ", col_name, " -> ", new_col_name,
            " (", min_val, "-", max_val, " scale)")
  }
  
  return(data)
}

#' Reverse Code Variables Using Codebook
#'
#' Reverse codes survey items based on coding_direction in codebook.
#' Creates new columns with "_R" suffix to preserve originals.
#'
#' @param data Survey response data frame
#' @param codebook Data frame containing variable metadata
#' @param coding_direction_col Column indicating coding direction (-1 = reverse)
#' @param min_col Column indicating minimum scale value
#' @param max_col Column indicating maximum scale value
#' @return Data frame with reverse-coded columns added
#'
reverse_code_from_codebook <- function(data, 
                                       codebook,
                                       coding_direction_col = "coding_direction",
                                       min_col = "min",
                                       max_col = "max") {
  
  # Find variables that need reverse coding
  reverse_vars <- codebook %>%
    filter(.data[[coding_direction_col]] == -1) %>%
    filter(!is.na(.data[[min_col]]) & !is.na(.data[[max_col]])) %>%
    filter(short_variable_name %in% names(data) | variable_name %in% names(data))
  
  if (nrow(reverse_vars) == 0) {
    message("No variables found for reverse coding")
    return(data)
  }
  
  # Create reverse specs list
  reverse_specs <- list()
  
  for (i in 1:nrow(reverse_vars)) {
    var_info <- reverse_vars[i, ]
    
    # Get column name (prefer short_variable_name)
    col_name <- if (var_info$short_variable_name %in% names(data)) {
      var_info$short_variable_name
    } else if (var_info$variable_name %in% names(data)) {
      var_info$variable_name
    } else {
      next
    }
    
    reverse_specs[[col_name]] <- list(
      min = var_info[[min_col]],
      max = var_info[[max_col]]
    )
  }
  
  # Use the base reverse coding function
  data <- reverse_code_columns(data, reverse_specs)
  
  return(data)
}

#' Detect Straightlining in Survey Responses
#'
#' Identifies participants who respond with the same value across multiple
#' survey items, which may indicate inattentive responding or satisficing behavior.
#'
#' @param data Survey response data frame
#' @param codebook Data frame containing variable metadata
#' @param scale_abbrev_col Column in codebook indicating scale membership
#' @param min_items_per_scale Minimum number of items required to check straightlining within a scale
#' @param straightline_threshold Proportion of identical responses required to flag (default 0.8)
#' @param check_across_scales Whether to check for straightlining across different scales (default FALSE)
#' @param exclude_reverse_coded Whether to exclude reverse-coded items from straightlining checks (default TRUE)
#' @return Data frame with straightlining flags added
#'
detect_straightlining <- function(data,
                                  codebook,
                                  scale_abbrev_col = "scale_abbrev",
                                  min_items_per_scale = 3,
                                  straightline_threshold = 0.8,
                                  check_across_scales = FALSE,
                                  exclude_reverse_coded = TRUE,
                                  remove_straightliners = FALSE) {
  
  message("Detecting straightlining patterns in survey responses...")
  
  # Initialize straightlining columns
  data$straightline_flags <- ""
  data$straightline_count <- 0
  data$max_consecutive_same <- 0
  
  # Get scales from codebook
  scales_to_check <- codebook %>%
    filter(!is.na(.data[[scale_abbrev_col]]), .data[[scale_abbrev_col]] != "") %>%
    group_by(.data[[scale_abbrev_col]]) %>%
    summarise(
      item_count = n(),
      scale_items = list(coalesce(short_variable_name, variable_name)),
      .groups = "drop"
    ) %>%
    filter(item_count >= min_items_per_scale)
  
  if (nrow(scales_to_check) == 0) {
    message("No scales found with sufficient items for straightlining detection")
    return(data)
  }
  
  straightline_summary <- list()
  
  # Check each scale separately
  for (i in 1:nrow(scales_to_check)) {
    scale_info <- scales_to_check[i, ]
    scale_name <- scale_info[[scale_abbrev_col]]
    scale_items <- scale_info$scale_items[[1]]
    
    # Filter to items that exist in data
    available_items <- scale_items[scale_items %in% names(data)]
    
    if (length(available_items) < min_items_per_scale) {
      message("Scale ", scale_name, " has insufficient items in data (", 
              length(available_items), " < ", min_items_per_scale, ")")
      next
    }
    
    # Optionally exclude reverse-coded items
    if (exclude_reverse_coded) {
      reverse_items <- codebook %>%
        filter(.data[[scale_abbrev_col]] == scale_name, coding_direction == -1) %>%
        pull(coalesce(short_variable_name, variable_name))
      
      available_items <- setdiff(available_items, reverse_items)
      
      if (length(available_items) < min_items_per_scale) {
        message("Scale ", scale_name, " has insufficient forward-coded items")
        next
      }
    }
    
    # Calculate straightlining for this scale
    scale_straightlining <- calculate_scale_straightlining(
      data[available_items], 
      scale_name, 
      straightline_threshold
    )
    
    # Add flags to main data
    data$straightline_flags <- ifelse(
      scale_straightlining$is_straightlining,
      paste(data$straightline_flags, scale_name, sep = ifelse(data$straightline_flags == "", "", "; ")),
      data$straightline_flags
    )
    
    data$straightline_count <- data$straightline_count + as.numeric(scale_straightlining$is_straightlining)
    
    # Track consecutive responses
    data$max_consecutive_same <- pmax(data$max_consecutive_same, scale_straightlining$max_consecutive)
    
    straightline_summary[[scale_name]] <- list(
      scale = scale_name,
      items_checked = length(available_items),
      straightlining_cases = sum(scale_straightlining$is_straightlining),
      straightlining_rate = mean(scale_straightlining$is_straightlining)
    )
    
    message("  ", scale_name, ": ", sum(scale_straightlining$is_straightlining), 
            " straightlining cases (", round(mean(scale_straightlining$is_straightlining) * 100, 1), "%)")
  }
  
  # Optional: Check for straightlining across all scales
  if (check_across_scales) {
    all_scale_items <- unlist(lapply(scales_to_check$scale_items, function(x) x[x %in% names(data)]))
    if (length(all_scale_items) >= min_items_per_scale) {
      global_straightlining <- calculate_scale_straightlining(
        data[all_scale_items], 
        "GLOBAL", 
        straightline_threshold
      )
      
      data$global_straightlining <- global_straightlining$is_straightlining
      message("  Global straightlining: ", sum(global_straightlining$is_straightlining), " cases")
    }
  }
  
  # Summary
  total_flagged <- sum(data$straightline_count > 0)
  message("Straightlining detection complete:")
  message("  Total participants flagged: ", total_flagged, " (", 
          round(total_flagged / nrow(data) * 100, 1), "%)")
  message("  Mean flags per participant: ", round(mean(data$straightline_count), 2))
  
  if (remove_straightliners) {
    data <- filter_straightlining(data, max_flags = 2)
  }
  
  return(data)
}

#' Calculate Straightlining for a Set of Items
#'
#' Helper function that calculates straightlining metrics for a given set of survey items
#'
calculate_scale_straightlining <- function(item_data, scale_name, threshold) {
  
  # Calculate proportion of identical responses for each participant
  straightlining_props <- apply(item_data, 1, function(row) {
    valid_responses <- row[!is.na(row)]
    if (length(valid_responses) <= 1) return(0)
    
    # Find the most common response
    response_table <- table(valid_responses)
    max_count <- max(response_table)
    
    # Proportion of responses that are the most common
    max_count / length(valid_responses)
  })
  
  # Calculate maximum consecutive identical responses
  max_consecutive <- apply(item_data, 1, function(row) {
    valid_responses <- row[!is.na(row)]
    if (length(valid_responses) <= 1) return(0)
    
    # Count maximum consecutive identical responses
    rle_result <- rle(valid_responses)
    max(rle_result$lengths)
  })
  
  # Determine straightlining cases
  is_straightlining <- straightlining_props >= threshold
  
  return(data.frame(
    is_straightlining = is_straightlining,
    straightlining_proportion = straightlining_props,
    max_consecutive = max_consecutive
  ))
}

#' Filter Out Straightlining Participants
#'
#' Removes participants who show excessive straightlining behavior
#'
#' @param data Data frame with straightlining flags
#' @param max_flags Maximum number of scale flags allowed (default 2)
#' @param exclude_global Whether to exclude global straightliners (default TRUE)
#' @return Filtered data frame
#'
filter_straightlining <- function(data, max_flags = 2, exclude_global = TRUE) {
  
  n_start <- nrow(data)
  
  # Filter based on number of flags
  filtered_data <- data %>%
    filter(straightline_count <= max_flags)
  
  # Optionally filter global straightliners
  if (exclude_global && "global_straightlining" %in% names(data)) {
    filtered_data <- filtered_data %>%
      filter(!global_straightlining)
  }
  
  n_removed <- n_start - nrow(filtered_data)
  message("Removed ", n_removed, " participants for excessive straightlining (", 
          round(n_removed/n_start*100, 1), "%)")
  
  return(filtered_data)
}


#' Filter by Survey Duration
#'
#' Removes responses that are too fast or too slow based on statistical thresholds.
#'
#' @param data Data frame with survey_duration column
#' @param method Method for determining threshold ("mean_sd", "median_mad", "percentile")
#' @param lower_threshold Lower threshold (for percentile method, 0-1)
#' @param upper_threshold Upper threshold (for percentile method, 0-1) 
#' @param sd_multiplier For mean_sd method, number of SDs below mean for cutoff
#' @return Filtered data frame
#'
filter_by_duration <- function(data,
                               method = "mean_sd",
                               lower_threshold = 0.05,
                               upper_threshold = 0.95,
                               sd_multiplier = 1) {
  
  n_start <- nrow(data)
  
  if (!"survey_duration" %in% names(data)) {
    stop("survey_duration column not found. Run calculate_survey_duration() first.")
  }
  
  if (method == "mean_sd") {
    mean_duration <- mean(data$survey_duration, na.rm = TRUE)
    sd_duration <- sd(data$survey_duration, na.rm = TRUE)
    cutoff <- mean_duration - (sd_multiplier * sd_duration)
    
    data_filtered <- data %>% filter(survey_duration >= cutoff)
    
    message("Removed responses < ", round(cutoff, 1), " minutes (mean - ", sd_multiplier, "SD)")
    
  } else if (method == "percentile") {
    lower_cutoff <- quantile(data$survey_duration, lower_threshold, na.rm = TRUE)
    upper_cutoff <- quantile(data$survey_duration, upper_threshold, na.rm = TRUE)
    
    data_filtered <- data %>% 
      filter(survey_duration >= lower_cutoff & survey_duration <= upper_cutoff)
    
    message("Removed responses outside ", lower_threshold*100, "th-", upper_threshold*100, "th percentiles")
  }
  
  n_removed <- n_start - nrow(data_filtered)
  message("Removed ", n_removed, " responses based on duration (", 
          round(n_removed/n_start*100, 1), "%)")
  message("Retained ", nrow(data_filtered), " responses")
  
  return(data_filtered)
}




# Complete Processing Pipeline -----------------------------------------------

#' Complete Survey Data Processing Pipeline
#' 
#' Applies all data processing steps in the correct order using
#' parameters loaded from a configuration, then scores surveys using data dictionary.
#' 
#' @param data Raw survey data with double headers
#' @param config_params List containing all configuration parameters
#' @param data_dict_path Path to data dictionary CSV file (optional)
#' @param score_surveys Logical, whether to score surveys after processing (default TRUE)
#' @return Fully processed data frame with survey scores
#' 
#' @examples
#' # Load parameters from separate file
#' source("survey_parameters.R")
#' processed_data <- process_survey_data(raw_data, survey_params, 
#'                                      data_dict_path = "config/data_dictionary.csv")
#'
process_survey_data <- function(data, codebook, survey_name, 
                                score_surveys = TRUE, 
                                output_path = "data/processed",
                                start_date_col = "start_date",
                                end_date_col = "end_date",
                                date_format = "ISO8601",
                                min_date = NULL,
                                max_date = NULL,
                                export_attention_failures = TRUE,
                                attention_dir = "data/attention",
                                remove_attention_fails = FALSE,
                                straightline_threshold = 0.8,
                                remove_straightliners = TRUE,
                                update_codebook_with_scored = TRUE) {
  
  message("Starting survey data processing pipeline...")
  
  # Step 1: Convert to snake case
  message("\nStep 1: Converting column names to snakecase...")
  data <- data %>% 
    rename_with(~ str_remove_all(., "<[^>]*>")) %>%  # strip HTML from column names
    janitor::clean_names()
  
  message("\nStep 2: Renaming with shortened names from codebook...")
  data <- apply_short_names(data, codebook)
  
  message("\nStep 3: Processing double headers...")
  data <- process_double_headers(data, codebook)
  
  message("\nStep 4: Processing multi-selects to create a summary col...")
  data <- create_multiselect_summary_cols(data, codebook)
  
  message("\nStep 5: Filtering by date range...")
  data <- filter_by_date_range(data, 
                                start_date_col = start_date_col,
                                min_date = min_date,
                                max_date = NULL,
                                date_format = date_format)
  
  message("\nStep 6: Computing survey duration...")
  data <- calculate_survey_duration(data, 
                                    start_date_col = start_date_col,
                                    end_date_col = end_date_col,
                                    date_format = date_format)
    
  message("\nStep 7: Scoring attention checks...")
  data <- score_attention_checks(data, codebook, 
                                 export_attention_failures = export_attention_failures,
                                 output_dir = attention_dir,
                                 remove_attention_fails = remove_attention_fails)
    
  message("\nStep 8: Mapping text responses to numeric codings...")
  data <- convert_response_text_to_codes(data, codebook,
                                 response_choices_col = "response_choices",
                                 response_coding_col = "response_coding")
  
  message("\nStep 9: Converting to Numeric...")
  data <- convert_to_numeric_from_codebook(data, codebook)

    
  message("\nStep 10: Checking for straightlining...")
  data <- detect_straightlining(data, codebook, 
                                straightline_threshold = straightline_threshold,
                                exclude_reverse_coded = FALSE,
                                remove_straightliners = remove_straightliners)

  
  message("\nStep 11: Reverse coding...")
  data <- reverse_code_from_codebook(data, codebook)
      
  # Step 12: Score surveys if requested and data dictionary provided
  if (score_surveys && !is.null(codebook)) {
    message("\nStep 12: Scoring surveys...")
    
    
    # Get available surveys
    available_surveys <- get_available_surveys(codebook)
    message("Available surveys: ", paste(available_surveys, collapse = ", "))
    
    # Score all available surveys
    data <- score_surveys(data, codebook, available_surveys[!available_surveys %in% "phs"], verbose = TRUE, update_codebook_with_scored = update_codebook_with_scored)
  }
  
  message("Survey data processing complete!")
  clean_survey_name <- survey_name %>% make_clean_names()
  out_filename <- paste0(clean_survey_name, "_processed_", Sys.Date(), ".csv")
  out_filepath <- file.path(output_path, out_filename)
  dir.create(dirname(out_filepath), recursive = TRUE, showWarnings = FALSE)
  write_csv(data, out_filepath)
  
  message("\n=== SURVEY PROCESSING COMPLETE ===")
  message("Processed data saved to to: ", out_filepath)
  return(data)
}