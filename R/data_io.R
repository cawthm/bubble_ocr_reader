#' Data I/O Module
#'
#' Functions for loading and saving data files.

#' Load bubble map template
#'
#' Reads the CSV file containing template bubble coordinates.
#'
#' @param path Path to the bubble map CSV file
#' @return A data.table with columns: target, x, y, z
#' @export
load_bubble_map <- function(path = "data/bubble_map.csv") {
  if (!file.exists(path)) {
    stop("Bubble map file not found: ", path)
  }

  bubble_map <- readr::read_csv(path, show_col_types = FALSE)

  # Validate required columns
  required_cols <- c("target", "x", "y")
  missing_cols <- setdiff(required_cols, names(bubble_map))

  if (length(missing_cols) > 0) {
    stop("Bubble map missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Add z column if missing (default to 1)
  if (!"z" %in% names(bubble_map)) {
    bubble_map$z <- 1
  }

  # Ensure correct types
  bubble_map$x <- as.integer(bubble_map$x)
  bubble_map$y <- as.integer(bubble_map$y)
  bubble_map$z <- as.numeric(bubble_map$z)

  data.table::as.data.table(bubble_map)
}

#' Load student roster
#'
#' Reads a CSV file containing student information.
#'
#' @param path Path to the roster CSV file
#' @return A data.table with student information
#' @export
load_roster <- function(path) {
  if (!file.exists(path)) {
    stop("Roster file not found: ", path)
  }

  roster <- readr::read_csv(path, show_col_types = FALSE)

  # Standardize column names
  names(roster) <- tolower(names(roster))

  # Handle common column name variations
  if ("id" %in% names(roster) && !"student_id" %in% names(roster)) {
    roster <- dplyr::rename(roster, student_id = id)
  }

  if (!"student_id" %in% names(roster)) {
    stop("Roster must contain a student_id (or id) column")
  }

  # Ensure student_id is character
  roster$student_id <- as.character(roster$student_id)

  data.table::as.data.table(roster)
}

#' Save graded results to CSV
#'
#' @param results A data.table with graded results
#' @param path Output file path
#' @param overwrite If TRUE, overwrite existing file (default: FALSE)
#' @export
save_results <- function(results, path, overwrite = FALSE) {
  if (file.exists(path) && !overwrite) {
    stop("File already exists: ", path, ". Use overwrite=TRUE to replace.")
  }

  # Ensure directory exists
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)

  readr::write_csv(results, path)
  message("Results saved to: ", path)

  invisible(path)
}

#' Save summary statistics to file
#'
#' @param stats Statistics from calculate_statistics()
#' @param path Output file path
#' @param format Output format: "txt" or "csv" (default: "txt")
#' @export
save_statistics <- function(stats, path, format = "txt") {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)

  if (format == "txt") {
    report <- format_summary_report(stats)
    writeLines(report, path)
  } else if (format == "csv") {
    # Save aggregate stats as CSV
    agg_df <- tibble::as_tibble(stats$aggregate)
    readr::write_csv(agg_df, path)
  } else {
    stop("Unknown format: ", format, ". Use 'txt' or 'csv'.")
  }

  message("Statistics saved to: ", path)
  invisible(path)
}

#' Generate output file path with timestamp
#'
#' @param prefix File name prefix (e.g., "results")
#' @param quiz_name Quiz name to include in filename
#' @param extension File extension (default: "csv")
#' @param output_dir Output directory (default: "output")
#' @param timestamp_format Format for timestamp (default: "%Y%m%d_%H%M%S")
#' @return A file path string
#' @export
generate_output_path <- function(prefix, quiz_name, extension = "csv",
                                  output_dir = "output",
                                  timestamp_format = "%Y%m%d_%H%M%S") {
  timestamp <- format(Sys.time(), timestamp_format)
  filename <- sprintf("%s_%s_%s.%s", prefix, quiz_name, timestamp, extension)
  file.path(output_dir, filename)
}

#' List available scan files in a directory
#'
#' @param scan_dir Directory to search for PDF files
#' @param pattern File pattern to match (default: PDF files)
#' @return Character vector of file paths
#' @export
list_scan_files <- function(scan_dir, pattern = "\\.pdf$") {
  if (!dir.exists(scan_dir)) {
    stop("Scan directory not found: ", scan_dir)
  }

  files <- list.files(scan_dir, pattern = pattern, full.names = TRUE,
                      ignore.case = TRUE)

  if (length(files) == 0) {
    warning("No matching files found in: ", scan_dir)
  }

  files
}

#' Validate a file before processing
#'
#' Checks that a file exists and has the expected extension.
#'
#' @param path File path to validate
#' @param expected_ext Expected file extension(s) (e.g., c("pdf", "PDF"))
#' @return TRUE if valid, throws error otherwise
#' @export
validate_file <- function(path, expected_ext = c("pdf", "PDF")) {
  if (!file.exists(path)) {
    stop("File not found: ", path)
  }

  ext <- tools::file_ext(path)
  if (!ext %in% expected_ext) {
    warning("Unexpected file extension: ", ext, ". Expected: ",
            paste(expected_ext, collapse = ", "))
  }

  TRUE
}
