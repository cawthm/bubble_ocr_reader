#' Pipeline Module
#'
#' Main orchestration functions for processing scans end-to-end.

#' Process a single scan
#'
#' Complete pipeline for processing one scanned answer sheet.
#'
#' @param path Path to the PDF scan file
#' @param bubble_map A data.table with template bubble positions
#' @param config A list of configuration parameters
#' @return A list with student_id, test_version, responses, and file path
#' @export
process_single_scan <- function(path, bubble_map, config = list()) {
  # Set defaults from config
  image_config <- config$image %||% list()
  detection_config <- config$detection %||% list()

  peak_threshold <- detection_config$peak_threshold %||% 10
  dedup_radius <- detection_config$dedup_radius %||% 10
  confidence_min <- detection_config$confidence_min %||% 40
  marker_z_threshold <- detection_config$marker_z_threshold %||% 400

  # Step 1: Read and preprocess the image
  image <- read_pdf_as_image(path)
  processed_image <- preprocess_image(image, image_config)

  # Step 2: Convert to matrix and find peaks
  matrix1 <- image_to_matrix(processed_image)
  peaks1 <- find_peaks(matrix1)

  # Step 3: Find bubbles and deduplicate
  bubbles1 <- find_bubbles(peaks1, threshold = peak_threshold)
  bubbles1 <- deduplicate_bubbles(bubbles1, radius = dedup_radius)

  # Step 4: Calculate perspective correction
  corrections <- calculate_perspective_correction(
    bubbles1,
    bubble_map,
    z_threshold = marker_z_threshold
  )

  # Step 5: Apply correction and re-process
  corrected_image <- apply_perspective_correction(processed_image, corrections)
  matrix2 <- image_to_matrix(corrected_image)
  peaks2 <- find_peaks(matrix2)

  # Step 6: Find bubbles in corrected image
  bubbles2 <- find_bubbles(peaks2, threshold = peak_threshold)
  bubbles2 <- deduplicate_bubbles(bubbles2, radius = dedup_radius)

  # Step 7: Match to template and extract responses
  matches <- find_nearest_template_match(bubbles2, bubble_map)
  prepared <- prepare_matches_for_extraction(matches)

  # Step 8: Extract all data
  extracted <- extract_all(prepared, confidence_min = confidence_min)

  # Return structured result
  list(
    student_id = extracted$student_id,
    test_version = extracted$test_version,
    responses = extracted$responses,
    file = path
  )
}

#' Process a batch of scans
#'
#' Process multiple scan files and aggregate results.
#'
#' @param paths Character vector of paths to PDF scan files
#' @param bubble_map A data.table with template bubble positions
#' @param config A list of configuration parameters
#' @param quiz_name Name of the quiz (for output)
#' @param verbose If TRUE, print progress messages (default: TRUE)
#' @return A data.table with all student responses
#' @export
process_batch <- function(paths, bubble_map, config = list(), quiz_name = "quiz",
                          verbose = TRUE) {
  results <- list()

  for (i in seq_along(paths)) {
    if (verbose) {
      message(sprintf("[%d/%d] Processing: %s", i, length(paths), basename(paths[i])))
    }

    tryCatch(
      {
        result <- process_single_scan(paths[i], bubble_map, config)

        # Flatten responses into a row per question
        if (nrow(result$responses) > 0) {
          student_data <- tibble::tibble(
            student_id = result$student_id,
            test_version = result$test_version,
            quiz = quiz_name,
            result$responses,
            file = result$file
          )
          results[[i]] <- student_data
        }
      },
      error = function(e) {
        warning(sprintf("Error processing %s: %s", paths[i], e$message))
      }
    )
  }

  # Combine all results
  all_results <- dplyr::bind_rows(results)

  if (verbose) {
    message(sprintf(
      "Processed %d scans, %d students",
      length(paths),
      length(unique(all_results$student_id))
    ))
  }

  data.table::as.data.table(all_results)
}

#' Run a complete grading session
#'
#' High-level entry point that processes scans, grades them, and generates output.
#'
#' @param scan_dir Directory containing PDF scan files
#' @param quiz_name Name of the quiz
#' @param answer_key Either a path to key file or vector of answers
#' @param bubble_map_path Path to bubble map CSV
#' @param roster_path Optional path to student roster CSV
#' @param output_dir Directory for output files
#' @param config Optional configuration list
#' @return A list with graded results and statistics
#' @export
run_grading_session <- function(scan_dir, quiz_name, answer_key,
                                bubble_map_path = "data/bubble_map.csv",
                                roster_path = NULL,
                                output_dir = "output",
                                config = list()) {
  # Load bubble map
  bubble_map <- load_bubble_map(bubble_map_path)

  # Load or create answer key
  if (is.character(answer_key) && length(answer_key) == 1 && file.exists(answer_key)) {
    key <- load_answer_key(answer_key)
  } else if (is.character(answer_key)) {
    key <- create_answer_key(answer_key)
  } else {
    stop("Invalid answer_key format")
  }

  # Find scan files
  scan_files <- list.files(scan_dir, pattern = "\\.pdf$", full.names = TRUE)

  if (length(scan_files) == 0) {
    stop("No PDF files found in: ", scan_dir)
  }

  message(sprintf("Found %d scan files in %s", length(scan_files), scan_dir))

  # Process scans
  raw_results <- process_batch(scan_files, bubble_map, config, quiz_name)

  # Grade responses
  graded <- grade_responses(raw_results, key)

  # Add roster info if available
  if (!is.null(roster_path) && file.exists(roster_path)) {
    roster <- load_roster(roster_path)
    graded <- dplyr::left_join(graded, roster, by = "student_id")
  }

  # Calculate statistics
  stats <- calculate_statistics(graded)

  # Save outputs
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

  results_file <- file.path(output_dir, sprintf("results_%s_%s.csv", quiz_name, timestamp))
  summary_file <- file.path(output_dir, sprintf("summary_%s_%s.txt", quiz_name, timestamp))

  readr::write_csv(graded, results_file)
  writeLines(format_summary_report(stats), summary_file)

  message(sprintf("Results saved to: %s", results_file))
  message(sprintf("Summary saved to: %s", summary_file))

  list(
    graded = graded,
    statistics = stats,
    results_file = results_file,
    summary_file = summary_file
  )
}

#' Null-coalescing operator
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
