#!/usr/bin/env Rscript

#' Bubble Test OCR Reader - Command Line Interface
#'
#' Usage:
#'   Rscript scripts/grade.R --scan-dir <path> --quiz <name> --key <path>
#'
#' Examples:
#'   Rscript scripts/grade.R --scan-dir input/quiz_1 --quiz quiz_1 --key keys/quiz_1.csv
#'   Rscript scripts/grade.R --scan-dir input/final --quiz final --key a,b,c,d,e

# Parse command line arguments
suppressPackageStartupMessages({
  library(argparse)
})

parser <- ArgumentParser(
  description = "Process scanned bubble test answer sheets and grade them."
)

parser$add_argument(
  "--scan-dir", "-s",
  required = TRUE,
  help = "Directory containing PDF scan files"
)

parser$add_argument(
  "--quiz", "-q",
  required = TRUE,
  help = "Name of the quiz (used in output filenames)"
)

parser$add_argument(
  "--key", "-k",
  required = TRUE,
  help = "Answer key: path to CSV file OR comma-separated answers (e.g., a,b,c,d)"
)

parser$add_argument(
  "--config", "-c",
  default = "config/config.yaml",
  help = "Path to configuration file (default: config/config.yaml)"
)

parser$add_argument(
  "--output-dir", "-o",
  default = "output",
  help = "Directory for output files (default: output)"
)

parser$add_argument(
  "--bubble-map", "-b",
  default = "data/bubble_map.csv",
  help = "Path to bubble map CSV (default: data/bubble_map.csv)"
)

parser$add_argument(
  "--roster", "-r",
  default = NULL,
  help = "Path to student roster CSV (optional)"
)

parser$add_argument(
  "--verbose", "-v",
  action = "store_true",
  default = TRUE,
  help = "Print progress messages (default: TRUE)"
)

parser$add_argument(
  "--quiet",
  action = "store_true",
  default = FALSE,
  help = "Suppress progress messages"
)

args <- parser$parse_args()

# Load required packages
suppressPackageStartupMessages({
  library(magick)
  library(tidyverse)
  library(data.table)
  library(yaml)
})

# Source all modules
source("R/config.R")
source("R/data_io.R")
source("R/image_processing.R")
source("R/bubble_detection.R")
source("R/template_matching.R")
source("R/response_extraction.R")
source("R/grading.R")
source("R/pipeline.R")

# Main execution
main <- function() {
  verbose <- args$verbose && !args$quiet

  if (verbose) {
    cat("=== Bubble Test OCR Reader ===\n\n")
  }

  # Load configuration
  config <- tryCatch(
    load_config(args$config),
    warning = function(w) {
      if (verbose) message("Using default configuration")
      get_default_config()
    }
  )

  config$processing$verbose <- verbose

  # Load bubble map
  if (verbose) cat("Loading bubble map...\n")
  bubble_map <- load_bubble_map(args$bubble_map)

  # Parse answer key
  if (verbose) cat("Loading answer key...\n")
  if (file.exists(args$key)) {
    answer_key <- load_answer_key(args$key)
  } else {
    # Assume comma-separated answers
    answers <- strsplit(args$key, ",")[[1]]
    answer_key <- create_answer_key(answers)
  }

  if (verbose) {
    cat(sprintf("  %d questions in key\n", nrow(answer_key)))
  }

  # Find scan files
  if (verbose) cat("Scanning for PDF files...\n")
  scan_files <- list_scan_files(args$scan_dir)

  if (length(scan_files) == 0) {
    stop("No PDF files found in: ", args$scan_dir)
  }

  if (verbose) {
    cat(sprintf("  Found %d scan files\n\n", length(scan_files)))
  }

  # Process scans
  if (verbose) cat("Processing scans...\n")
  raw_results <- process_batch(
    scan_files,
    bubble_map,
    config,
    quiz_name = args$quiz,
    verbose = verbose
  )

  if (nrow(raw_results) == 0) {
    stop("No results extracted from scans")
  }

  # Grade responses
  if (verbose) cat("\nGrading responses...\n")
  graded <- grade_responses(raw_results, answer_key)

  # Add roster info if available
  if (!is.null(args$roster) && file.exists(args$roster)) {
    if (verbose) cat("Merging with roster...\n")
    roster <- load_roster(args$roster)
    graded <- dplyr::left_join(graded, roster, by = "student_id")
  }

  # Calculate statistics
  stats <- calculate_statistics(graded)

  # Save outputs
  dir.create(args$output_dir, showWarnings = FALSE, recursive = TRUE)

  results_path <- generate_output_path(
    "results", args$quiz, "csv", args$output_dir
  )
  summary_path <- generate_output_path(
    "summary", args$quiz, "txt", args$output_dir
  )

  save_results(graded, results_path, overwrite = TRUE)
  save_statistics(stats, summary_path)

  # Print summary
  if (verbose) {
    cat("\n")
    cat(format_summary_report(stats))
    cat("\n\n")
    cat("Output files:\n")
    cat(sprintf("  Results: %s\n", results_path))
    cat(sprintf("  Summary: %s\n", summary_path))
  }
}

# Run main function
tryCatch(
  main(),
  error = function(e) {
    cat("Error:", e$message, "\n", file = stderr())
    quit(status = 1)
  }
)
