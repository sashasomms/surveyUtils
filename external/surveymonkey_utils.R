# SurveyMonkey API Utilities =================================================
#'
#' Pure API functions for interacting with SurveyMonkey to download survey
#' response data and metadata. These functions handle only API communication
#' and data retrieval - no business logic or template generation.
#'
#' @author Sasha L Sommerfeldt
#' @date 2025

# Setup and Dependencies ------------------------------------------------------

library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(stringr)
library(janitor)

# Authentication and Security -------------------------------------------------

#' Load SurveyMonkey OAuth Token Securely
#'
#' Loads OAuth token from a secure CSV file outside the project directory
#'
#' @param creds_path Path to CSV file containing access_token column
#' @return Character string containing the OAuth token
#' 
#' @examples
#' token <- load_sm_token("../survey_monkey_secrets.csv")
#'
load_sm_token <- function(creds_path = "../survey_monkey_secrets.csv") {
  if (!file.exists(creds_path)) {
    stop("Credentials file not found: ", creds_path,
         "\nCreate a CSV with 'access_token' column containing your SurveyMonkey OAuth token.")
  }
  
  df_creds <- read_csv(creds_path, show_col_types = FALSE)
  
  if (!"access_token" %in% names(df_creds)) {
    stop("Credentials file must contain 'access_token' column")
  }
  
  token <- df_creds %>% pull(access_token)
  message("SurveyMonkey OAuth token loaded successfully")
  return(token)
}

# Caching Utilities -----------------------------------------------------------
# One of the API calls (in fetch_surveys_sm) gets a list of all of our surveys with their associated names, nicknames, IDs.
# This is pretty static (our overall list of surveys isnt changing that often), 
# so to reduce API calls we cache it every 24 hours instead of repeatedly making API calls 
# to retrieve it whenever we need to find the id for an individual survey.

#' Get Cached Survey List
#' 
#' Retrieves cached survey list if it exists and is within the cache time limit
#' 
#' @param cache_hours Maximum age of cache in hours
#' @return Data frame of surveys if valid cache exists, NULL otherwise
#' 
get_cached_surveys <- function(cache_hours = 24) {
  cache_file <- file.path(tempdir(), "sm_surveys_cache.rds")
  
  if (!file.exists(cache_file)) {
    return(NULL)
  }
  
  # Check if cache is still valid
  cache_age_hours <- as.numeric(difftime(Sys.time(), file.mtime(cache_file), units = "hours"))
  
  if (cache_age_hours > cache_hours) {
    message("Survey cache expired (", round(cache_age_hours, 1), " hours old)")
    return(NULL)
  }
  
  tryCatch({
    cached_data <- readRDS(cache_file)
    message("Survey cache found (", round(cache_age_hours, 1), " hours old)")
    return(cached_data)
  }, error = function(e) {
    message("Error reading survey cache: ", e$message)
    return(NULL)
  })
}

#' Cache Survey List
#' 
#' Saves survey list to temporary cache file
#' 
#' @param surveys Data frame of surveys to cache
#' 
cache_surveys <- function(surveys) {
  cache_file <- file.path(tempdir(), "sm_surveys_cache.rds")
  
  tryCatch({
    saveRDS(surveys, cache_file)
    message("Survey list cached successfully")
  }, error = function(e) {
    warning("Failed to cache survey list: ", e$message)
  })
}

#' Clear Survey Cache
#' 
#' Removes cached survey list file
#' 
#' @export
clear_survey_cache <- function() {
  cache_file <- file.path(tempdir(), "sm_surveys_cache.rds")
  
  if (file.exists(cache_file)) {
    file.remove(cache_file)
    message("Survey cache cleared")
  } else {
    message("No survey cache found")
  }
}

# Survey Discovery Functions --------------------------------------------------

#' Fetch Info on all Surveys from SurveyMonkey API
#'
#' This function retrieves a list of surveys from the SurveyMonkey API using
#' OAuth token authentication. Includes optional caching to reduce API calls.
#'
#' @param sm_oauth_token A character string containing the OAuth bearer token
#'   for SurveyMonkey API authentication. This token should have the necessary
#'   permissions to read survey data. 
#'   To generate an OAuth token, create an app with your account that is connected to the surveys at https://developer.surveymonkey.com/apps/
#'   Keep the OAuth token secure by reading it in from a separate file rather than including in visible code. 
#'   Avoid uploading token info and other secrets to places like GitHub.
#' @param use_cache Logical. If TRUE, will use cached survey list if available and less than cache_hours old
#' @param cache_hours Number of hours to cache survey list (default: 24)
#' @param force_refresh Logical. If TRUE, ignores cache and makes fresh API call
#'
#' @return A data frame containing information on all of the surveys. Including columns for
#'   id, title, nickname, href.
#'
#' @examples
#' \dontrun{
#' # Use cached version if available
#' surveys <- fetch_surveys_sm(token)
#' 
#' # Force fresh download
#' surveys <- fetch_surveys_sm(token, force_refresh = TRUE)
#' }
#'
#' @seealso
#' \url{https://developer.surveymonkey.com/api/v3/#surveys} for API documentation
#'
#' @export
fetch_surveys_sm <- function(sm_oauth_token, use_cache = TRUE, cache_hours = 24, force_refresh = FALSE) {
  
  # Check cache first if enabled
  if (use_cache && !force_refresh) {
    cached_surveys <- get_cached_surveys(cache_hours)
    if (!is.null(cached_surveys)) {
      message("Using cached survey list (", nrow(cached_surveys), " surveys)")
      return(cached_surveys)
    }
  }
  
  # Make fresh API call
  message("Fetching survey list from API...")
  
  # Construct the API endpoint URL
  url <- "https://api.surveymonkey.com/v3/surveys/"
  
  # Make the API request
  response <- GET(url, add_headers(Authorization = paste("Bearer", sm_oauth_token)))
  
  # Check for API errors
  if (status_code(response) != 200) {
    stop("API request failed with status: ", status_code(response),
         "\nResponse: ", content(response, "text"))
  }
  
  # Parse the JSON response
  data <- fromJSON(content(response, "text", encoding = "UTF-8"))
  
  # Convert to data frame
  df <- as.data.frame(data$data)
  message("Found ", nrow(df), " surveys")
  
  # Cache the results if caching is enabled
  if (use_cache) {
    cache_surveys(df)
  }
  
  return(df)
}

#' Find Survey ID by Name
#' 
#' Searches for a survey by title or nickname and returns its ID
#' 
#' @param surveys Data frame of surveys from fetch_surveys_sm()
#' @param survey_name Survey title or nickname to search for
#' @return Character string containing survey ID
#' 
#' @examples
#' \dontrun{
#' surveys <- fetch_surveys_sm(token)
#' id <- find_survey_id_sm("My Survey", surveys = surveys) # provide surveys
#' # OR
#' id <- find_survey_id_sm("My Survey", token = token) # provide token and will call fetch_surveys_sm
#' }
#'
find_survey_id_sm <- function(survey_name, token = NULL, surveys = NULL) {
  if (!is.null(surveys)) {
    survey_id <- surveys %>% 
      filter(title == survey_name | nickname == survey_name) %>% 
      pull(id)
  } else if (!is.null(token)) {
    survey_id <- fetch_surveys_sm(token) %>% 
      filter(title == survey_name | nickname == survey_name) %>% 
      pull(id)
  }
  
  if (length(survey_id) == 0) {
    stop("Survey not found: ", survey_name,
         "\nAvailable surveys: ", paste(surveys$title, collapse = ", "))
  }
  
  if (length(survey_id) > 1) {
    warning("Multiple surveys found with name: ", survey_name,
            "\nUsing first match: ", survey_id[1])
    survey_id <- survey_id[1]
  }
  
  return(survey_id)
}

# Survey Structure Analysis ---------------------------------------------------

#' Fetch Detailed Survey Structure from SurveyMonkey API
#'
#' Retrieves detailed information about survey questions, including question types,
#' answer choices, and matrix structures needed for codebook generation.
#'
#' @param sm_oauth_token OAuth token for SurveyMonkey API
#' @param survey_id Survey ID to analyze
#' @return List containing detailed survey structure information
#' 
#' @examples
#' \dontrun{
#' structure <- fetch_survey_structure_sm(token, "123456789")
#' }
#'
fetch_survey_structure_sm <- function(sm_oauth_token, survey_id) {
  # Construct the API endpoint URL for survey details
  url <- paste0("https://api.surveymonkey.com/v3/surveys/", survey_id, "/details")
  
  # Make the API request
  response <- GET(url, add_headers(Authorization = paste("Bearer", sm_oauth_token)))
  
  # Check for API errors
  if (status_code(response) != 200) {
    stop("API request failed with status: ", status_code(response),
         "\nResponse: ", content(response, "text"))
  }
  
  # Parse the JSON response with simplifyVector = FALSE to preserve structure
  data <- fromJSON(content(response, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  
  message("Survey structure retrieved for: ", data$title)
  return(data)
}

# Response Data Download ------------------------------------------------------

#' Download Survey Response Data from SurveyMonkey API
#'
#' This function retrieves response data for a specific survey from the SurveyMonkey API using
#' OAuth token authentication. Handles pagination automatically to get all responses.
#'
#' @param sm_oauth_token A character string containing the OAuth bearer token
#' @param surveys Optional data frame of surveys for name lookup
#' @param survey_id A character string or numeric value specifying the survey ID
#' @param survey_name A character string specifying the survey name (requires surveys)
#' @param per_page Number of responses per API request (max 100)
#' @return A data frame containing the individual responses data for the specified survey
#'
#' @examples
#' \dontrun{
#' # Download by survey ID
#' responses <- download_responses_sm(token, survey_id = "123456789")
#' 
#' # Download by survey name
#' surveys <- fetch_surveys_sm(token)
#' responses <- download_responses_sm(token, surveys, survey_name = "My Survey")
#' }
#'
#' @seealso
#' \url{https://developer.surveymonkey.com/api/v3/#survey-responses-bulk} for API documentation
#' @seealso \code{\link{fetch_surveys_sm}} to get survey IDs
#'
download_responses_sm <- function(sm_oauth_token, surveys = NULL, survey_id = NULL, 
                                  survey_name = NULL, per_page = 100) {
  
  # Resolve survey ID
  if (is.null(survey_id) && !is.null(survey_name) && !is.null(surveys)) {
    survey_id <- find_survey_id_sm(survey_name, surveys = surveys)
  } else if (is.null(survey_id) && !is.null(survey_name) && is.null(surveys) && !is.null(sm_oauth_token)) {
    survey_id <- find_survey_id_sm(survey_name, token = sm_oauth_token)
  } else if (is.null(survey_id)) {
    stop("Must provide either survey_id, or survey_name + surveys, or survey_name + token")
  }
  
  message("Downloading responses for survey: ", survey_id)
  
  # Construct the API endpoint URL
  url <- paste0("https://api.surveymonkey.com/v3/surveys/", survey_id, "/responses/bulk")
  
  # Make initial API request
  response <- GET(url, 
                  query = list(per_page = per_page), 
                  config = add_headers(Authorization = paste("Bearer", sm_oauth_token)))
  
  # Check for API errors
  if (status_code(response) != 200) {
    stop("API request failed with status: ", status_code(response),
         "\nResponse: ", content(response, "text"))
  }
  
  # Parse the JSON response
  data <- fromJSON(content(response, "text", encoding = "UTF-8"), simplifyVector = FALSE)
  
  # Start with first page of data - keep as list for now
  all_responses <- data$data
  
  # Handle pagination if there are more pages
  if (!is.null(data$links$`next`)) {
    n_pages <- ceiling(data$total / per_page)
    message("Found ", data$total, " responses across ", n_pages, " pages")
    
    for (page in 2:n_pages) {
      message("Downloading page ", page, " of ", n_pages)
      
      response <- GET(url, 
                      query = list(per_page = per_page, page = page), 
                      config = add_headers(Authorization = paste("Bearer", sm_oauth_token)))
      
      if (status_code(response) != 200) {
        warning("Failed to download page ", page, ": status ", status_code(response))
        next
      }
      
      # Parse and append data
      data <- fromJSON(content(response, "text", encoding = "UTF-8"), simplifyVector=FALSE)
      all_responses <- c(all_responses, data$data)
    }
  }
  
  message("Downloaded ", length(all_responses), " responses successfully")
  return(all_responses)
}

# Data Processing Functions ---------------------------------------------------


#' Extract Survey Metadata (Enhanced with 'Other' Option Support)
#' 
#' Creates comprehensive metadata with mapping for standard choices AND "Other" text fields.
#' Detects "Other" options and creates additional metadata rows for custom text columns.
#' 
extract_survey_metadata <- function(survey_structure) {
  metadata_list <- list()
  
  if (!is.null(survey_structure$pages)) {
    for (page_idx in seq_along(survey_structure$pages)) {
      page <- survey_structure$pages[[page_idx]]
      
      if (!is.null(page$questions)) {
        for (q_idx in seq_along(page$questions)) {
          question <- page$questions[[q_idx]]
          
          # Basic question info
          question_id <- as.character(question$id)
          question_text <- question$headings[[1]]$heading %||% paste("Question", question_id)
          question_text <- str_replace_all(question_text, "\n", " ") %>% str_trim()
          
          family <- question$family %||% ""
          subtype <- question$subtype %||% ""
          
          # Determine question format
          question_format <- determine_question_format(family, subtype, question)
          
          # Extract response choices and detect "Other" options
          choices_info <- extract_choice_info(question)
          other_choice_ids <- detect_other_options(question)
          
          # Create metadata row(s)
          if (question_format == "matrix") {
            # Matrix questions create multiple rows (one per row item)
            if (!is.null(question$answers$rows)) {
              for (row in question$answers$rows) {
                row_info <- list(
                  question_id = question_id,
                  question_format = question_format,
                  question_text = row$text,  # Use matrix row text
                  matrix_row_id = row$id,
                  matrix_row_text = row$text,
                  response_choices = choices_info$choice_text,
                  response_coding = choices_info$choice_coding,
                  num_response_choices = choices_info$num_choices,
                  page_number = page_idx,
                  required = !is.null(question$required) && !is.null(question$required$text),
                  family = family,
                  subtype = subtype,
                  # Column mapping fields for double headers
                  choice_id = NA,          
                  choice_text = NA,        
                  lookup_key = paste0(question_id, "_", row$id),
                  is_other_option = FALSE,
                  is_other_text_field = FALSE
                )
                metadata_list <- append(metadata_list, list(row_info))
              }
            }
            
          } else if (question_format == "multi_select") {
            # Multi-select: create one row per choice option
            if (!is.null(question$answers$choices)) {
              for (choice in question$answers$choices) {
                is_other <- as.character(choice$id) %in% other_choice_ids
                
                choice_info <- list(
                  question_id = question_id,
                  question_format = question_format,
                  question_text = question_text,  
                  matrix_row_id = NA,
                  matrix_row_text = NA,
                  response_choices = choices_info$choice_text,
                  response_coding = choices_info$choice_coding,
                  num_response_choices = choices_info$num_choices,
                  page_number = page_idx,
                  required = !is.null(question$required) && !is.null(question$required$text),
                  family = family,
                  subtype = subtype,
                  # Column mapping fields for double headers
                  choice_id = choice$id,                 
                  choice_text = choice$text,             
                  lookup_key = paste0(question_id, "_", choice$id),
                  is_other_option = is_other,
                  is_other_text_field = FALSE
                )
                metadata_list <- append(metadata_list, list(choice_info))
              }
              
              # Add metadata row for "Other" text field if any "Other" options exist
              if (length(other_choice_ids) > 0) {
                other_text_info <- list(
                  question_id = question_id,
                  question_format = question_format,
                  question_text = paste0(question_text, " - Other (please specify)"),  
                  matrix_row_id = NA,
                  matrix_row_text = NA,
                  response_choices = NA,
                  response_coding = NA,
                  num_response_choices = NA,
                  page_number = page_idx,
                  required = !is.null(question$required) && !is.null(question$required$text),
                  family = family,
                  subtype = subtype,
                  # Column mapping for the "Other" text field
                  choice_id = NA,                 
                  choice_text = "Other (please specify)",             
                  lookup_key = paste0(question_id, "_other_text"),
                  is_other_option = FALSE,
                  is_other_text_field = TRUE
                )
                metadata_list <- append(metadata_list, list(other_text_info))
              }
            }
            
          } else {
            # Regular questions (single-select, text, etc.)
            has_other <- length(other_choice_ids) > 0
            
            metadata_row <- list(
              question_id = question_id,
              question_format = question_format,
              question_text = question_text,
              matrix_row_id = NA,
              matrix_row_text = NA,
              response_choices = choices_info$choice_text,
              response_coding = choices_info$choice_coding,
              num_response_choices = choices_info$num_choices,
              page_number = page_idx,
              required = !is.null(question$required) && !is.null(question$required$text),
              family = family,
              subtype = subtype,
              # Column mapping fields for double headers
              choice_id = NA,               
              choice_text = NA,             
              lookup_key = question_id,
              is_other_option = FALSE,
              is_other_text_field = FALSE
            )
            metadata_list <- append(metadata_list, list(metadata_row))
          }
        }
      }
    }
  }
  
  # Convert to data frame
  if (length(metadata_list) > 0) {
    metadata_df <- bind_rows(metadata_list)
    
    # Count "Other" fields for reporting
    other_count <- sum(metadata_df$is_other_option | metadata_df$is_other_text_field, na.rm = TRUE)
    
    message("Extracted metadata for ", nrow(metadata_df), " questions/items")
    if (other_count > 0) {
      message("Found ", other_count, " 'Other' options and text fields")
    }
    
    return(metadata_df)
  } else {
    message("No question metadata found")
    return(data.frame())
  }
}

#' Determine Question Format
#' 
#' Categorizes questions into standard formats for codebook generation
#' 
determine_question_format <- function(family, subtype, question) {
  # Single-select: radio buttons, dropdowns
  if (family == "single_choice" || (family == "multiple_choice" && subtype == "menu")) {
    return("single_select")
  }
  
  # Multi-select: checkboxes
  if (family == "multiple_choice" && subtype %in% c("vertical", "vertical_two_col")) {
    return("multi_select")
  }
  
  # Matrix questions
  if (family == "matrix") {
    return("matrix")
  }
  
  # Text questions
  if (family == "open_ended") {
    if (subtype %in% c("single", "essay")) {
      return("text")
    } else if (subtype == "numerical") {
      return("numeric")
    }
  }
  
  # Ranking questions
  if (family == "ranking") {
    return("ranking")
  }
  
  # Default fallback
  return(paste0(family, "_", subtype))
}

#' Extract Choice Information
#' 
#' Extracts response choice text and coding from question structure
#' 
extract_choice_info <- function(question) {
  choice_texts <- c()
  choice_codings <- c()
  
  if (!is.null(question$answers$choices)) {
    for (i in seq_along(question$answers$choices)) {
      choice <- question$answers$choices[[i]]
      choice_texts <- c(choice_texts, choice$text)
      
      # SurveyMonkey typically uses 1-based indexing for coding
      # But we'll use the actual choice ID if available
      if (!is.null(choice$weight)) {
        choice_codings <- c(choice_codings, choice$weight)
      } else {
        choice_codings <- c(choice_codings, i)
      }
    }
  }
  
  # Handle text questions (no choices)
  if (length(choice_texts) == 0) {
    return(list(
      choice_text = NA,
      choice_coding = NA,
      num_choices = 0
    ))
  }
  
  return(list(
    choice_text = paste(choice_texts, collapse = "; "),
    choice_coding = paste(choice_codings, collapse = "; "),
    num_choices = length(choice_texts)
  ))
}

#' Enhanced Flatten Survey Response Data with 'Other' Support
#' 
#' Fixed version that properly handles "Other" response options where users
#' select "Other" and provide custom text.
#' 
flatten_survey_responses <- function(raw_responses, survey_structure) {
  if (length(raw_responses) == 0) {
    message("No responses to flatten")
    return(data.frame())
  }
  
  message("Flattening ", length(raw_responses), " responses...")
  
  # Get question types for processing logic
  question_types <- get_question_types(survey_structure)
  
  # Initialize list to store flattened rows
  flattened_rows <- list()
  
  # Process each response
  for (i in seq_along(raw_responses)) {
    response <- raw_responses[[i]]
    
    if (is.null(response$id)) {
      warning("Response ", i, " missing ID field. Skipping.")
      next
    }
    
    # Start with response metadata
    row_data <- list(
      response_id = response$id,
      date_created = response$date_created,
      date_modified = response$date_modified,
      response_status = response$response_status,
      custom_value = response$custom_value %||% "",
      first_name = response$first_name %||% "",
      last_name = response$last_name %||% "",
      email_address = response$email_address %||% "",
      ip_address = response$ip_address %||% ""
    )
    
    # Add custom variables if they exist
    if (!is.null(response$custom_variables)) {
      for (var_name in names(response$custom_variables)) {
        row_data[[var_name]] <- response$custom_variables[[var_name]]
      }
    }
    
    # Process each page of questions
    if (!is.null(response$pages)) {
      for (page in response$pages) {
        if (!is.null(page$questions)) {
          for (question in page$questions) {
            question_id <- as.character(question$id)
            question_type <- question_types[[question_id]]
            
            # Process answers based on question type
            if (!is.null(question$answers)) {
              
              # Matrix questions: use technical names q123_456 (question_id + row_id)
              if (question_type$family == "matrix") {
                for (answer in question$answers) {
                  if (!is.null(answer$row_id)) {
                    col_name <- paste0("q", question_id, "_", answer$row_id)
                    
                    # Get choice text or answer text
                    if (!is.null(answer$choice_id)) {
                      choice_text <- question_type$choices[[as.character(answer$choice_id)]]
                      # For "Other" responses, append the custom text
                      if (!is.null(answer$text) && answer$text != "") {
                        final_text <- paste0(choice_text %||% "Other", ": ", answer$text)
                      } else {
                        final_text <- choice_text %||% answer$text %||% "Selected"
                      }
                      row_data[[col_name]] <- final_text
                    } else {
                      row_data[[col_name]] <- answer$text %||% ""
                    }
                  }
                }
                
              } else if (question_type$is_single_select) {
                # Single-select: use technical name q123
                col_name <- paste0("q", question_id)
                answer_text <- ""
                
                for (answer in question$answers) {
                  if (!is.null(answer$choice_id)) {
                    choice_text <- question_type$choices[[as.character(answer$choice_id)]]
                    # Handle "Other" responses with custom text
                    if (!is.null(answer$text) && answer$text != "") {
                      answer_text <- paste0(choice_text %||% "Other", ": ", answer$text)
                    } else {
                      answer_text <- choice_text %||% answer$text %||% "Selected"
                    }
                    break
                  } else if (!is.null(answer$text)) {
                    answer_text <- answer$text
                    break
                  }
                }
                row_data[[col_name]] <- answer_text
                
              } else {
                # Multi-select: create technical columns q123_456 for ALL choices
                # Get all possible choices for this question
                all_choices <- question_type$choices
                
                # Create columns for all choices
                for (choice_id in names(all_choices)) {
                  col_name <- paste0("q", question_id, "_", choice_id)
                  row_data[[col_name]] <- ""  # Default to empty
                }
                
                # Also create "Other" text column if question has other option
                other_col_name <- paste0("q", question_id, "_other_text")
                
                # Fill in the selected choices
                for (answer in question$answers) {
                  if (!is.null(answer$choice_id)) {
                    col_name <- paste0("q", question_id, "_", answer$choice_id)
                    
                    # Check if this is an "Other" response with custom text
                    if (!is.null(answer$text) && answer$text != "") {
                      # This is an "Other" response - mark the choice and store the text
                      row_data[[col_name]] <- "1"  # Mark the "Other" choice as selected
                      row_data[[other_col_name]] <- answer$text  # Store the custom text
                    } else {
                      # Regular choice selection
                      row_data[[col_name]] <- answer$text %||% "1"
                    }
                  } else if (!is.null(answer$text)) {
                    # Pure open text for multi-select (not "Other" option)
                    col_name <- paste0("q", question_id)
                    row_data[[col_name]] <- answer$text %||% ""
                  }
                }
              }
            } else {
              # Question with no answers - create empty technical column
              if (question_type$family == "matrix") {
                # Skip - matrix questions without answers don't get columns
              } else {
                col_name <- paste0("q", question_id)
                row_data[[col_name]] <- ""
              }
            }
          }
        }
      }
    }
    
    flattened_rows[[i]] <- row_data
  }
  
  # Convert to data frame
  df <- bind_rows(flattened_rows)
  # Reorder columns to match survey order with custom variables after metadata  
  df <- reorder_survey_columns(df, survey_structure)
  
  message("Flattened data has ", nrow(df), " rows and ", ncol(df), " columns (using technical column names)")
  
  return(df)
}


#' Get Question Types and Metadata
#' 
#' Extracts question types and answer choices from survey structure
#' 
get_question_types <- function(survey_structure) {
  question_types <- list()
  
  if (!is.null(survey_structure$pages)) {
    for (page in survey_structure$pages) {
      if (!is.null(page$questions)) {
        for (question in page$questions) {
          question_id <- as.character(question$id)
          
          # Determine if single-select based on question family/subtype
          family <- question$family %||% ""
          subtype <- question$subtype %||% ""
          
          # Single-select: radio buttons, dropdowns, matrix
          # Multi-select: checkboxes
          is_single_select <- family == "single_choice" || (family == "matrix") ||
            (family == "multiple_choice" && subtype %in% c("menu"))
          
          # Extract choices
          choices <- list()
          if (!is.null(question$answers$choices)) {
            for (choice in question$answers$choices) {
              choices[[as.character(choice$id)]] <- choice$text
            }
          }
          
          question_types[[question_id]] <- list(
            is_single_select = is_single_select,
            family = family,
            subtype = subtype,
            choices = choices
          )
        }
      }
    }
  }
  
  return(question_types)
}

#' Reorder Columns to Match Survey Order
#' 
#' Rearranges columns so questions appear in survey order with related columns grouped
#' 
reorder_survey_columns <- function(df, survey_structure) {
  # Get metadata columns (keep at front)
  metadata_cols <- c("response_id", "date_created", "date_modified", 
                     "response_status", "custom_value", "first_name", 
                     "last_name", "email_address", "ip_address")
  
  # Get survey question order
  question_order <- c()
  if (!is.null(survey_structure$pages)) {
    for (page in survey_structure$pages) {
      if (!is.null(page$questions)) {
        for (question in page$questions) {
          question_order <- c(question_order, as.character(question$id))
        }
      }
    }
  }
  
  # Group columns by type
  question_columns <- list()
  custom_var_columns <- c()
  other_columns <- c()
  
  for (col_name in names(df)) {
    if (col_name %in% metadata_cols) {
      next  # Handle metadata separately
    } else if (str_starts(col_name, "q")) {
      # Extract question ID
      question_id <- str_extract(col_name, "(?<=q)\\d+")
      if (!is.null(question_id) && !is.na(question_id)) {
        if (is.null(question_columns[[question_id]])) {
          question_columns[[question_id]] <- c()
        }
        question_columns[[question_id]] <- c(question_columns[[question_id]], col_name)
      } else {
        other_columns <- c(other_columns, col_name)
      }
    } else {
      # Custom variables - anything not in standard metadata or questions
      # Common custom variables like participant_id, family_id, or any other non-standard columns
      custom_var_columns <- c(custom_var_columns, col_name)
    }
  }
  
  # Build final column order
  final_order <- c()
  
  # Add metadata columns that exist
  final_order <- c(final_order, intersect(metadata_cols, names(df)))
  
  # Add custom variables (participant_id, family_id, etc.) after metadata
  final_order <- c(final_order, sort(custom_var_columns))
  
  # Add question columns in survey order
  for (q_id in question_order) {
    if (!is.null(question_columns[[q_id]])) {
      # Sort question columns within each question (for consistent ordering of multi-select)
      final_order <- c(final_order, sort(question_columns[[q_id]]))
    }
  }
  
  # Add any remaining columns
  final_order <- c(final_order, other_columns)
  
  # Reorder dataframe
  final_order <- intersect(final_order, names(df))  # Only include columns that exist
  return(df[, final_order, drop = FALSE])
}


#' Detect "Other" Options in Survey Question
#' 
#' Identifies which response choices are "Other" options based on common patterns.
#' SurveyMonkey typically marks these with specific indicators.
#' 
detect_other_options <- function(question) {
  other_choice_ids <- c()
  
  if (!is.null(question$answers$choices)) {
    for (choice in question$answers$choices) {
      choice_text <- str_to_lower(choice$text %||% "")
      
      # Check for common "Other" patterns
      is_other <- str_detect(choice_text, "other") |
        str_detect(choice_text, "please specify") |
        str_detect(choice_text, "none of the above") |
        # SurveyMonkey API sometimes has a specific "other" field indicator
        (!is.null(choice$is_other) && choice$is_other == TRUE) |
        # Check for visible flag in choice structure
        (!is.null(choice$visible) && !is.null(choice$is_other))
      
      if (is_other) {
        other_choice_ids <- c(other_choice_ids, as.character(choice$id))
      }
    }
  }
  
  return(other_choice_ids)
}


#' Enhanced Create Double Headers 
#' 
#' Updated to properly handle "Other" text fields using the enhanced metadata
#' 
create_double_headers <- function(flattened_data, survey_structure) {
  if (nrow(flattened_data) == 0) {
    return(flattened_data)
  }
  
  # Get the enhanced metadata with "Other" option mapping
  metadata <- extract_survey_metadata(survey_structure)
  
  # Create mapping from technical column names to question text and choice text
  column_mapping <- metadata %>%
    mutate(
      # Technical column name that flatten_survey_responses creates
      technical_col_name = case_when(
        is_other_text_field ~ lookup_key,                               # Other text: q123_other_text
        !is.na(choice_id) ~ paste0("q", question_id, "_", choice_id),   # Multi-select: q123_456
        !is.na(matrix_row_id) ~ paste0("q", question_id, "_", matrix_row_id), # Matrix: q123_789
        TRUE ~ paste0("q", question_id)                                 # Single-select: q123
      )
    ) %>%
    select(technical_col_name, question_text, choice_text, is_other_text_field)
  
  # Process column names and create choice header row
  col_names <- names(flattened_data)
  new_column_names <- character(length(col_names))
  choice_header_row <- character(length(col_names))
  
  for (i in seq_along(col_names)) {
    col_name <- col_names[i]
    
    # Check if this is a metadata column
    if (col_name %in% c("response_id", "date_created", "date_modified", 
                        "response_status", "custom_value", "first_name", 
                        "last_name", "email_address", "ip_address")) {
      # Use friendly names for metadata columns
      friendly_names <- list(
        "response_id" = "Respondent ID",
        "date_created" = "Start Date", 
        "date_modified" = "End Date",
        "response_status" = "Response Status",
        "custom_value" = "Custom Value",
        "first_name" = "First Name",
        "last_name" = "Last Name", 
        "email_address" = "Email Address",
        "ip_address" = "IP Address"
      )
      new_column_names[i] <- friendly_names[[col_name]] %||% col_name
      choice_header_row[i] <- ""
      
    } else {
      # Look up in column mapping using technical name
      mapping_row <- column_mapping %>% filter(technical_col_name == col_name)
      
      if (nrow(mapping_row) > 0) {
        # Found mapping - use question text as column name, choice text as second row
        new_column_names[i] <- mapping_row$question_text[1]
        choice_header_row[i] <- mapping_row$choice_text[1] %||% ""
      } else {
        # No mapping found - use as-is with title case (for custom variables, etc.)
        display_name <- str_replace_all(col_name, "_", " ") %>% str_to_title()
        new_column_names[i] <- display_name
        choice_header_row[i] <- ""
      }
    }
  }
  
  # Set the new column names
  names(flattened_data) <- new_column_names
  
  # Create the choice header row
  choice_row_df <- data.frame(matrix(choice_header_row, nrow = 1), stringsAsFactors = FALSE)
  names(choice_row_df) <- new_column_names
  
  # Combine: choice header row first, then actual data
  result <- bind_rows(choice_row_df, flattened_data)
  
  message("Created double header format with 'Other' option support")
  return(result)
}

# Simple Workflow Functions --------------------------------------------------

#' Process Survey Structure into CSV Format
#'
#' Takes survey meta data and converts it to CSV-format for export.
#' Separate from API downloads for flexibility.
#'
#' @param token SurveyMonkey OAuth token
#' @param survey_name Survey name to process
#' @param save_csv Whether to save structure data to CSV
#' @param output_path Path to save response data (default: config/)
#' @return Processed survey metadata dataframe
#' 
#' @examples
#' \dontrun{
#' token <- load_sm_token()
#' metadata <- process_survey_structure(token, survey_name = "My Survey")
#' }
#'
process_survey_structure <- function(token, 
                                     survey_name, 
                                     save_csv = TRUE,
                                     output_path = "config/") {
  # 1. Get survey ID
  survey_id <- find_survey_id_sm(survey_name, token = token)
  
  # 2. Extract structure data from API
  message("Processing structure data...")
  survey_structure <- fetch_survey_structure_sm(token, survey_id = survey_id)
  
  # 3. Parse the extracted structure data
  processed_data <- extract_survey_metadata(survey_structure)
  
  # 4. Save metadata if requested
  metadata_filepath <- NULL
  if (save_csv && nrow(processed_data) > 0) {
    # Create directory if needed
    dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
    
    # Create filename
    if (is.null(survey_name)) {
      survey_name <- survey_structure$title %||% "survey_data"
    }
    clean_survey_name <- survey_name %>% make_clean_names()
    metadata_filename <- paste0(clean_survey_name, "_metadata.csv")
    metadata_filepath <- file.path(output_path, metadata_filename)
    
    write_csv(processed_data, metadata_filepath)
    message("Metadata saved to: ", metadata_filepath)
  }
  
  return(processed_data)
}

#' Process Survey Responses into CSV Format
#'
#' Takes raw response data and converts it to CSV-ready format with headers.
#' Separate from API downloads for flexibility.
#'
#' @param token SurveyMonkey OAuth token
#' @param survey_name Survey name to process
#' @param save_csv Whether to save response data to CSV
#' @param output_path Path to save response data (default: data/responses/)
#' @param include_double_headers Whether to include question text headers
#' @return Processed survey response dataframe
#' 
#' @examples
#' \dontrun{
#' token <- load_sm_token()
#' responses <- process_survey_responses(token, survey_name = "My Survey")
#' }
#'
process_survey_responses <- function(token, 
                                     survey_name,
                                     save_csv = TRUE,
                                     output_path = "data/responses/",
                                     include_double_headers = TRUE) {
  
  # 1. Get survey ID
  survey_id <- find_survey_id_sm(survey_name, token = token)
  
  # 2. Download responses data via API
  raw_responses <- download_responses_sm(token, survey_id = survey_id, per_page = 100)
  
  # 3. Fetch survey structure from API
  survey_structure <- fetch_survey_structure_sm(token, survey_id = survey_id)
  
  # 4. Flatten the responses data
  message("Processing response data...")
  flattened_data <- flatten_survey_responses(raw_responses, survey_structure)
  
  # 5. Add double headers if requested
  if (include_double_headers && nrow(flattened_data) > 0) {
    processed_data <- create_double_headers(flattened_data, survey_structure)
  } else {
    processed_data <- flattened_data
  }
  
  # 6. Save responses if requested
  responses_filepath <- NULL
  if (save_csv && nrow(processed_data) > 0) {
    # Create directory if needed
    dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
    
    # Create filename
    if (is.null(survey_name)) {
      survey_name <- survey_structure$title %||% "survey_data"
    }
    clean_survey_name <- survey_name %>% make_clean_names()
    responses_filename <- paste0(clean_survey_name, "_responses_", Sys.Date(), ".csv")
    responses_filepath <- file.path(output_path, responses_filename)
    
    write_csv(processed_data, responses_filepath)
    message("Response data saved to: ", responses_filepath)
  }
  
  return(processed_data)
}

# Utility Functions -----------------------------------------------------------

# Null coalescing operator
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x