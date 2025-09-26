# Survey Scoring Utilities ===================================================
#'
#' Functions for scoring surveys based on data codebook configuration.
#' Handles forward/reverse coding, missing data, validation, and subscales.
#'
#' @author Sasha L Sommerfeldt
#' @date 2025

# Setup and Dependencies ------------------------------------------------------

library(tidyverse)

# Core Scoring Functions ------------------------------------------------------

#' Score Individual Survey Scale
#' 
#' Adapted from lmsupport() - scores a single survey scale with forward/reverse items
#' 
#' @param df Data frame containing survey items
#' @param forward Character vector of forward-coded item column names
#' @param reverse Character vector of reverse-coded item column names (optional)
#' @param range Numeric vector of length 2 specifying min/max valid values
#' @param prorate Logical, whether to prorate for missing items (default TRUE)
#' @param max_miss Numeric, maximum proportion of missing items allowed (default 0.20)
#' @return Numeric vector of survey scores
#' 
#' @examples
#' # Score PHQ-9 with some reverse items
#' phq_score <- var_score(data, 
#'                       forward = c("phq_001", "phq_002"), 
#'                       reverse = c("phq_003"),
#'                       range = c(0, 3))
#'
var_score <- function(df, forward, reverse = NULL, range = NULL, prorate = TRUE, max_miss = 0.20) {
  
  # Input validation
  all_items <- c(forward, reverse)
  missing_cols <- setdiff(all_items, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing columns in data: ", paste(missing_cols, collapse = ", "))
  }
  
  # Select relevant items
  d <- df[, all_items, drop = FALSE]
  
  # Convert to numeric if needed
  d <- d %>% mutate(across(everything(), as.numeric))
  
  # Check for out of range values
  if (!is.null(range)) {
    if (min(d, na.rm = TRUE) < range[1] || max(d, na.rm = TRUE) > range[2]) {
      warning(paste("Item score(s) out of range. Expected:", range[1], "-", range[2], 
                    "Found:", min(d, na.rm = TRUE), "-", max(d, na.rm = TRUE)))
    }
  }
  
  # Validate range parameter for reverse scoring
  if (!is.null(reverse) && (is.null(range) || length(range) != 2)) {
    stop('Must specify item range (range = c(min, max)) to reverse score items')
  }
  
  # Reverse score relevant items
  if (!is.null(reverse)) {
    for (v in reverse) {
      d[, v] <- (range[1] + range[2]) - d[, v]
    }   
  }
  
  # Calculate scores
  if (prorate) {
    total <- rowMeans(d, na.rm = TRUE) * ncol(d)
  } else {
    total <- rowSums(d, na.rm = TRUE)
  }
  
  # Handle excessive missing data
  miss_prop <- rowSums(is.na(d)) / ncol(d)
  total[miss_prop > max_miss] <- NA
  
  return(total)
}

# Data Dictionary Processing --------------------------------------------------

#' Load and Validate Data Dictionary
#' 
#' Loads data codebook from CSV and validates required columns
#' 
#' @param codebook_path Path to data codebook CSV file
#' @param required_cols Character vector of required column names
#' @return Data frame containing validated data codebook
#' 
#' @examples
#' codebook <- load_codebook("config/codebook.csv")
#'

load_codebook <- function(codebook_path, 
                          required_cols = c("short_variable_name", "scale_abbrev", "coding_direction")) {
  
  if (!file.exists(codebook_path)) {
    stop("Data codebook file not found: ", codebook_path)
  }
  
  # Load codebook
  codebook <- read_csv(codebook_path, show_col_types = FALSE)
  
  # Validate required columns
  missing_cols <- setdiff(required_cols, names(codebook))
  if (length(missing_cols) > 0) {
    stop("Data codebook missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Clean and validate coding_direction values
  if ("coding_direction" %in% names(codebook)) {
    valid_directions <- c(-1, 1)
    invalid_directions <- setdiff(unique(codebook$coding_direction), valid_directions)
    if (length(invalid_directions) > 0) {
      stop("Invalid coding_direction values found: ", paste(invalid_directions, collapse = ", "),
           ". Must be -1 (reverse) or 1 (forward)")
    }
  }
  
  message("Data codebook loaded successfully: ", nrow(codebook), " variables")
  return(codebook)
}

#' Get Survey Configuration from Data Dictionary
#' 
#' Extracts forward/reverse items and score range for a specific survey or subscale
#' Automatically detects whether to use short_variable_name or variable_name based on data
#' 
#' @param codebook Data codebook data frame
#' @param survey_name Name of survey scale to extract
#' @param subscale_name Optional name of subscale to extract
#' @param data Optional data frame to calculate actual range from
#' @return List containing forward items, reverse items, and range
#' 
#' @examples
#' config <- get_survey_config(codebook, "phq", data = my_data)
#' config_subscale <- get_survey_config(codebook, "phq", subscale_name = "somatic", data = my_data)
#'
get_survey_config <- function(codebook, survey_name, subscale_name = NULL, data = NULL) {
  
  # Filter to survey items
  survey_items <- codebook %>% 
    filter(scale_abbrev == survey_name) %>%
    filter(!is.na(short_variable_name))  # Exclude summary scores
  
  # Filter to subscale if specified
  if (!is.null(subscale_name)) {
    if (!"subscale" %in% names(codebook)) {
      stop("Data codebook must have 'subscale' column to filter by subscale")
    }
    survey_items <- survey_items %>% filter(subscale == subscale_name)
  }
  
  if (nrow(survey_items) == 0) {
    stop("No items found for survey: ", survey_name, 
         if(!is.null(subscale_name)) paste0(", subscale: ", subscale_name) else "")
  }
  
  # Determine which variable name column to use
  use_short_names <- TRUE
  if (!is.null(data)) {
    # Check if short_variable_name columns exist in data
    short_matches <- sum(survey_items$short_variable_name %in% names(data))
    
    # Check if variable_name columns exist in data (if available)
    if ("variable_name" %in% names(survey_items)) {
      long_matches <- sum(survey_items$variable_name %in% names(data))
      
      # Use variable_name if it has more matches than short_variable_name
      if (long_matches > short_matches) {
        use_short_names <- FALSE
      }
    }
  }
  
  # Select appropriate variable name column
  var_name_col <- if (use_short_names) "short_variable_name" else "variable_name"
  
  # Get forward and reverse items
  forward_items <- survey_items %>% 
    filter(coding_direction == 1) %>% 
    pull(!!sym(var_name_col))
  
  reverse_items <- survey_items %>% 
    filter(coding_direction == -1) %>% 
    pull(!!sym(var_name_col))
  
  # Set reverse to NULL if empty
  if (length(reverse_items) == 0) {
    reverse_items <- NULL
  }
  
  # Determine range
  if (!is.null(data)) {
    # Calculate range from actual data
    all_items <- c(forward_items, reverse_items)
    existing_items <- intersect(all_items, names(data))
    
    if (length(existing_items) > 0) {
      item_range <- data %>% 
        select(all_of(existing_items)) %>% 
        summarise(min = min(., na.rm = TRUE), max = max(., na.rm = TRUE)) %>%
        as.numeric()
    } else {
      warning("No survey items found in data for: ", survey_name, 
              " (tried ", var_name_col, ")")
      item_range <- NULL
    }
  } else {
    # Use theoretical range from data codebook if available
    if (all(c("min", "max") %in% names(survey_items))) {
      item_range <- c(min(survey_items$min, na.rm = TRUE), 
                      max(survey_items$max, na.rm = TRUE))
    } else {
      item_range <- NULL
    }
  }
  
  return(list(
    survey_name = survey_name,
    subscale_name = subscale_name,
    forward = forward_items,
    reverse = reverse_items,
    range = item_range,
    n_items = length(c(forward_items, reverse_items)),
    var_name_col = var_name_col  # Track which column was used
  ))
}

#' Check if Survey Has Subscales
#' 
#' Determines if a survey has non-empty subscale definitions
#' 
#' @param codebook Data codebook data frame
#' @param survey_name Name of survey to check
#' @return Logical indicating if survey has subscales
#'
has_subscales <- function(codebook, survey_name) {
  
  if (!"subscale" %in% names(codebook)) {
    return(FALSE)
  }
  
  survey_items <- codebook %>% 
    filter(scale_abbrev == survey_name) %>%
    filter(!is.na(short_variable_name))
  
  # Check if any items have non-NA subscale values
  any(!is.na(survey_items$subscale) & survey_items$subscale != "")
}

#' Get Subscale Names for Survey
#' 
#' Extracts unique subscale names for a given survey
#' 
#' @param codebook Data codebook data frame
#' @param survey_name Name of survey
#' @return Character vector of unique subscale names
#'
get_subscales <- function(codebook, survey_name) {
  
  if (!"subscale" %in% names(codebook)) {
    return(character(0))
  }
  
  subscales <- codebook %>% 
    filter(scale_abbrev == survey_name) %>%
    filter(!is.na(short_variable_name)) %>%
    filter(!is.na(subscale) & subscale != "") %>%
    pull(subscale) %>%
    unique() %>%
    sort()
  
  return(subscales)
}

# Main Scoring Functions ------------------------------------------------------

#' Score Single Survey Scale
#' 
#' Scores one survey using data codebook configuration
#' 
#' @param data Data frame containing survey items
#' @param codebook Data codebook data frame  
#' @param survey_name Name of survey to score
#' @param prorate Whether to prorate for missing items
#' @param max_miss Maximum proportion missing allowed
#' @param verbose Whether to print processing messages
#' @return Numeric vector of survey scores
#' 
#' @examples
#' phq_scores <- score_survey(data, codebook, "phq")
#'
score_survey <- function(data, codebook, survey_name, 
                         prorate = TRUE, max_miss = 0.20, verbose = TRUE) {
  
  if (verbose) message("Scoring survey: ", survey_name)
  
  # Get survey configuration
  config <- get_survey_config(codebook, survey_name, data = data)
  
  if (verbose) {
    message("  Items: ", config$n_items, 
            " (", length(config$forward), " forward, ", 
            length(config$reverse %||% character(0)), " reverse)")
    if (!is.null(config$range)) {
      message("  Range: ", config$range[1], " - ", config$range[2])
    }
  }
  
  # Score the survey
  scores <- var_score(data, 
                      forward = config$forward,
                      reverse = config$reverse, 
                      range = config$range,
                      prorate = prorate,
                      max_miss = max_miss)
  
  return(scores)
}

#' Score Survey Subscale
#' 
#' Scores a specific subscale of a survey
#' 
#' @param data Data frame containing survey items
#' @param codebook Data codebook data frame
#' @param survey_name Name of survey
#' @param subscale_name Name of subscale to score
#' @param prorate Whether to prorate for missing items
#' @param max_miss Maximum proportion missing allowed
#' @param verbose Whether to print processing messages
#' @return Numeric vector of subscale scores
#'
score_subscale <- function(data, codebook, survey_name, subscale_name,
                           prorate = TRUE, max_miss = 0.20, verbose = TRUE) {
  
  if (verbose) message("  Scoring subscale: ", subscale_name)
  
  # Get subscale configuration
  config <- get_survey_config(codebook, survey_name, subscale_name = subscale_name, data = data)
  
  if (verbose) {
    message("    Items: ", config$n_items,
            " (", length(config$forward), " forward, ",
            length(config$reverse %||% character(0)), " reverse)")
  }
  
  # Score the subscale
  scores <- var_score(data,
                      forward = config$forward,
                      reverse = config$reverse,
                      range = config$range,
                      prorate = prorate,
                      max_miss = max_miss)
  
  return(scores)
}

#' Score Multiple Survey Scales
#' 
#' Scores multiple surveys (including subscales) and adds them as columns to the data
#' 
#' @param data Data frame containing survey items
#' @param codebook Data codebook data frame
#' @param survey_list Character vector of survey names to score
#' @param prorate Whether to prorate for missing items  
#' @param max_miss Maximum proportion missing allowed
#' @param suffix Suffix to add to score column names (default: "_total")
#' @param score_subscales Whether to score subscales (default: TRUE)
#' @param verbose Whether to print processing messages
#' @return Data frame with added score columns
#' 
#' @examples
#' # Score specific surveys with subscales
#' # Creates columns like: phq_total, phq_somatic, phq_cognitive
#' scored_data <- score_surveys(data, codebook, c("phq", "gad"))
#' 
#' # Score all surveys in codebook
#' all_surveys <- unique(codebook$scale_abbrev)
#' scored_data <- score_surveys(data, codebook, all_surveys)
#'
score_surveys <- function(data, codebook, survey_list, 
                          prorate = TRUE, max_miss = 0.20, 
                          suffix = "_total",
                          score_subscales = TRUE, verbose = TRUE,
                          update_codebook_with_scored = TRUE) {
  
  if (verbose) message("Scoring ", length(survey_list), " surveys...")
  
  scored_data <- data
  
  for (survey_name in survey_list) {
    tryCatch({
      # Score the total survey
      scores <- score_survey(scored_data, codebook, survey_name, 
                             prorate = prorate, max_miss = max_miss, verbose = verbose)
      
      # Add total score to data frame
      score_col_name <- paste0(survey_name, suffix)
      scored_data[[score_col_name]] <- scores
      
      if (verbose) {
        n_scored <- sum(!is.na(scores))
        message("  Added column: ", score_col_name, " (", n_scored, " valid scores)")
      }
      
      # Score subscales if they exist and scoring is enabled
      if (score_subscales && has_subscales(codebook, survey_name)) {
        subscales <- get_subscales(codebook, survey_name)
        
        if (length(subscales) > 0) {
          if (verbose) message("  Found ", length(subscales), " subscales")
          
          for (subscale_name in subscales) {
            tryCatch({
              subscale_scores <- score_subscale(scored_data, codebook, survey_name, subscale_name,
                                                prorate = prorate, max_miss = max_miss, verbose = verbose)
              
              # Add subscale score to data frame - subscale name is the identifier
              subscale_col_name <- paste0(survey_name, "_", subscale_name)
              scored_data[[subscale_col_name]] <- subscale_scores
              
              if (verbose) {
                n_subscale_scored <- sum(!is.na(subscale_scores))
                message("    Added column: ", subscale_col_name, " (", n_subscale_scored, " valid scores)")
              }
              
            }, error = function(e) {
              warning("Failed to score subscale '", subscale_name, "' for survey '", survey_name, "': ", e$message)
            })
          }
        }
      }
      
    }, error = function(e) {
      warning("Failed to score survey '", survey_name, "': ", e$message)
    })
  }
  
  if (verbose) message("Survey scoring complete!")
  
  if (update_codebook_with_scored) {
    message("Adding new scores to codebook..")
    # Update codebook with computed score variables
    updated_codebook <- add_computed_scores_to_codebook(
      codebook, 
      survey_list,
      output_path = "config/test_codebook.csv"
    )
  }

  return(scored_data)
}

# Utility Functions -----------------------------------------------------------

#' Get Available Surveys from Data Dictionary
#' 
#' @param codebook Data codebook data frame
#' @return Character vector of unique survey names
#' 
#' @examples
#' available_surveys <- get_available_surveys(codebook)
#'

get_available_surveys <- function(codebook) {
  if (!"scale_abbrev" %in% names(codebook)) {
    stop("Data codebook must have 'scale_abbrev' column")
  }
  
  surveys <- codebook %>% 
    filter(!is.na(scale_abbrev)) %>%
    pull(scale_abbrev) %>% 
    unique() %>% 
    sort()
  
  return(surveys)
}

#' Generate Survey Scoring Report
#' 
#' Creates a summary report of survey scoring results (totals and subscales)
#' 
#' @param data Data frame with scored surveys
#' @param survey_list Character vector of survey names
#' @param suffix Suffix used for total score columns
#' @return Data frame with scoring summary statistics
#' 
#' @examples
#' report <- generate_scoring_report(scored_data, c("phq", "gad"))
#'
generate_scoring_report <- function(data, survey_list, suffix = "_total") {
  
  # Get all score columns that start with survey names from the list
  survey_pattern <- paste0("^(", paste(survey_list, collapse = "|"), ")(_|", suffix, ")")
  score_cols <- names(data)[str_detect(names(data), survey_pattern)]
  
  if (length(score_cols) == 0) {
    stop("No score columns found in data")
  }
  
  report <- data %>%
    select(all_of(score_cols)) %>%
    summarise(across(everything(), list(
      n_valid = ~sum(!is.na(.)),
      n_missing = ~sum(is.na(.)), 
      mean = ~mean(., na.rm = TRUE),
      sd = ~sd(., na.rm = TRUE),
      min = ~min(., na.rm = TRUE),
      max = ~max(., na.rm = TRUE)
    ))) %>%
    pivot_longer(everything(), 
                 names_to = c("score_name", "statistic"), 
                 names_pattern = "(.+)_(n_valid|n_missing|mean|sd|min|max)$") %>%
    pivot_wider(names_from = statistic, values_from = value) %>%
    mutate(
      score_type = if_else(str_detect(score_name, paste0(suffix, "$")), "total", "subscale"),
      survey = str_extract(score_name, paste0("^(", paste(survey_list, collapse = "|"), ")"))
    ) %>%
    arrange(survey, score_type, score_name)
  
  return(report)
}