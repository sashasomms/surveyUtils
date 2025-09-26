#' Upload Cleaned Survey Data to Box
#'
#' This function uploads a cleaned survey dataset to a specified Box folder.
#'
#' @param data A data frame containing the cleaned survey data
#' @param file_name Character string for the filename (include .csv, .xlsx, etc.)
#' @param box_folder_id Numeric or character Box folder ID where file should be uploaded.
#'   Use box_ls() to find folder IDs, or 0 for root folder.
#' @param overwrite Logical indicating whether to overwrite existing files with same name
#'
#' @return Information about the uploaded file
#'
#' @examples
#' \dontrun{
#' # Upload to root folder
#' upload_survey_to_box(clean_data, "survey_responses_2024.csv", 0)
#' 
#' # Upload to specific folder
#' upload_survey_to_box(clean_data, "survey_responses_2024.csv", "123456789")
#' }
#'
#' @export
upload_survey_to_box <- function(data, file_name, box_folder_id = 0, overwrite = TRUE) {
  # Save data locally first (temporary file)
  temp_file <- file.path(tempdir(), file_name)
  
  # Determine file type and save accordingly
  file_ext <- tools::file_ext(file_name)
  if (file_ext == "csv") {
    write.csv(data, temp_file, row.names = FALSE)
  } else if (file_ext %in% c("xlsx", "xls")) {
    library(openxlsx)
    write.xlsx(data, temp_file)
  } else {
    stop("Unsupported file format. Use .csv or .xlsx")
  }
  
  # Upload to Box
  result <- box_ul(
    file = temp_file,
    dir_id = box_folder_id,
    overwrite = overwrite
  )
  
  # Clean up temp file
  unlink(temp_file)
  
  return(result)
}
