#' Grading Module
#'
#' Functions for loading answer keys, grading responses, and calculating statistics.

#' Load an answer key from a file or name
#'
#' Supports two formats:
#' 1. Simple key (no versions): columns resp_no, key
#' 2. Versioned key: columns resp_no, plus version columns (e.g., wxxy, wxxz, zyzx)
#'
#' @param key_source Either a file path to a CSV, or a quiz name to look up in config
#' @param config Optional config list with answer_keys mapping
#' @return A data.table with columns: resp_no, key (if simple) or version columns
#' @export
load_answer_key <- function(key_source, config = NULL) {

  # Determine file path
  if (file.exists(key_source)) {
    key_path <- key_source
  } else if (!is.null(config) && key_source %in% names(config$answer_keys)) {
    key_path <- config$answer_keys[[key_source]]
  } else {
    stop("Answer key not found: ", key_source)
  }

  # Read the key file
  key <- readr::read_csv(key_path, show_col_types = FALSE)

  # Standardize column names
  if ("question_number" %in% names(key)) {
    key <- dplyr::rename(key, resp_no = question_number)
  }
  if ("correct_answer" %in% names(key)) {
    key <- dplyr::rename(key, key = correct_answer)
  }

  # Ensure resp_no exists
  if (!"resp_no" %in% names(key)) {
    key$resp_no <- seq_len(nrow(key))
  }

  # Detect if this is a versioned key (has columns matching version pattern)
  version_cols <- detect_version_columns(names(key))

  if (length(version_cols) > 0) {
    # Versioned key: normalize all version columns to lowercase
    for (col in version_cols) {
      key[[col]] <- tolower(stringr::str_trim(key[[col]]))
    }
    attr(key, "versioned") <- TRUE
    attr(key, "versions") <- version_cols
  } else if ("key" %in% names(key)) {
    # Simple key: normalize the key column
    key$key <- tolower(stringr::str_trim(key$key))
    attr(key, "versioned") <- FALSE
  }

  data.table::as.data.table(key)
}

#' Detect version columns in a key file
#'
#' Version columns are 4-character strings using only w, x, y, z
#'
#' @param col_names Vector of column names
#' @return Vector of column names that match version pattern
#' @noRd
detect_version_columns <- function(col_names) {
  version_pattern <- "^[wxyz]{4}$"
  col_names[grepl(version_pattern, col_names, ignore.case = TRUE)]
}

#' Get the correct answers for a specific version
#'
#' @param key A data.table from load_answer_key()
#' @param version The test version string (e.g., "wxxy")
#' @return A data.table with columns: resp_no, key
#' @export
get_version_key <- function(key, version) {
  version <- tolower(version)

  # Check if key is versioned
  is_versioned <- isTRUE(attr(key, "versioned"))

  if (!is_versioned) {
    # Simple key - return as-is (ignoring version)
    if (!"key" %in% names(key)) {
      stop("Simple key must have a 'key' column")
    }
    return(key[, .(resp_no, key)])
  }

  # Versioned key - look up the version column
  versions <- attr(key, "versions")

  if (!version %in% tolower(versions)) {
    available <- paste(versions, collapse = ", ")
    stop("Version '", version, "' not found in key. Available versions: ", available)
  }

  # Find the matching column (case-insensitive)
  version_col <- versions[tolower(versions) == version][1]

  # Use data.frame then convert to avoid 'key' column name conflict with data.table
  result <- data.frame(
    resp_no = key$resp_no,
    key = key[[version_col]],
    stringsAsFactors = FALSE
  ) |> data.table::as.data.table()

  result
}

#' Create an answer key from a vector
#'
#' Convenience function to create a key from a vector of answers.
#'
#' @param answers A character vector of correct answers (a, b, c, d, e)
#' @param chapter Optional chapter number(s)
#' @return A data.table with columns: resp_no, correct_ans
#' @export
create_answer_key <- function(answers, chapter = NA) {
  cleaned <- tolower(stringr::str_trim(answers))
  data.frame(
    resp_no = seq_along(answers),
    key = cleaned,
    chapter = chapter,
    stringsAsFactors = FALSE
  ) |> data.table::as.data.table()
}

#' Grade student responses against an answer key
#'
#' @param responses A data.table with columns: resp_no, ans (from extract_responses)
#'   May also include test_version column for versioned keys
#' @param key A data.table from load_answer_key() - either simple or versioned
#' @param version Optional test version string (e.g., "wxxy"). If NULL and responses
#'   contains a test_version column, that will be used.
#' @return A data.table with columns: resp_no, ans, key, correct
#' @export
grade_responses <- function(responses, key, version = NULL) {
  # Check if key is versioned
  is_versioned <- isTRUE(attr(key, "versioned"))

  if (is_versioned) {
    # Need to grade by version
    if (is.null(version) && "test_version" %in% names(responses)) {
      # Grade each student by their version
      return(grade_responses_by_version(responses, key))
    } else if (!is.null(version)) {
      # Single version specified
      version_key <- get_version_key(key, version)
      return(grade_responses_simple(responses, version_key))
    } else {
      stop("Versioned key requires either 'version' parameter or 'test_version' column in responses")
    }
  } else {
    # Simple key
    return(grade_responses_simple(responses, key))
  }
}

#' Grade responses against a simple (non-versioned) key
#' @noRd
grade_responses_simple <- function(responses, key) {
  # Ensure key has the right columns
  if (!"key" %in% names(key)) {
    stop("Key must have a 'key' column")
  }

  # Join responses with key
  graded <- dplyr::left_join(
    responses,
    key[, c("resp_no", "key")],
    by = "resp_no"
  ) |>
    dplyr::mutate(
      ans = tolower(stringr::str_trim(ans)),
      key = tolower(stringr::str_trim(key)),
      correct = (ans == key)
    ) |>
    # Filter out responses not in the key (spurious detections)
    dplyr::filter(!is.na(key)) |>
    data.table::as.data.table()

  graded
}

#' Grade responses when each student may have a different version
#' @noRd
grade_responses_by_version <- function(responses, key) {
  # Get unique student/version combinations
  if (!"test_version" %in% names(responses)) {
    stop("Responses must have 'test_version' column for versioned grading")
  }

  # Split by version and grade each group
  versions <- unique(responses$test_version)
  results <- list()

  for (v in versions) {
    v_responses <- responses[test_version == v]

    tryCatch({
      v_key <- get_version_key(key, v)
      v_graded <- grade_responses_simple(v_responses, v_key)
      results[[v]] <- v_graded
    }, error = function(e) {
      warning("Could not grade version '", v, "': ", e$message)
      # Return responses with NA for correctness
      v_graded <- v_responses
      v_graded$key <- NA_character_
      v_graded$correct <- NA
      results[[v]] <<- v_graded
    })
  }

  dplyr::bind_rows(results) |> data.table::as.data.table()
}

#' Calculate score from graded responses
#'
#' @param graded A data.table from grade_responses() with a 'correct' column
#' @return A list with: n_correct, n_total, score (as decimal), percentage
#' @export
calculate_score <- function(graded) {
  n_correct <- sum(graded$correct, na.rm = TRUE)
  n_total <- nrow(graded)

  list(
    n_correct = n_correct,
    n_total = n_total,
    score = n_correct / n_total,
    percentage = round(100 * n_correct / n_total, 1)
  )
}

#' Calculate statistics for multiple graded results
#'
#' @param all_graded A data.table with graded results from multiple students,
#'   must include student_id and correct columns
#' @return A list with per_student and per_question statistics
#' @export
calculate_statistics <- function(all_graded) {
  # Per-student statistics
  per_student <- all_graded |>
    dplyr::group_by(student_id) |>
    dplyr::summarise(
      n_correct = sum(correct, na.rm = TRUE),
      n_total = dplyr::n(),
      score = n_correct / n_total,
      percentage = round(100 * score, 1),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(percentage))

  # Per-question statistics
  per_question <- all_graded |>
    dplyr::group_by(resp_no) |>
    dplyr::summarise(
      n_correct = sum(correct, na.rm = TRUE),
      n_total = dplyr::n(),
      pct_correct = round(100 * n_correct / n_total, 1),
      .groups = "drop"
    ) |>
    dplyr::arrange(resp_no)

  # Aggregate statistics
  scores <- per_student$percentage

  aggregate <- list(
    n_students = nrow(per_student),
    mean = round(mean(scores), 1),
    median = round(stats::median(scores), 1),
    sd = round(stats::sd(scores), 1),
    min = min(scores),
    max = max(scores)
  )

  list(
    per_student = per_student,
    per_question = per_question,
    aggregate = aggregate
  )
}

#' Generate a summary report
#'
#' @param stats Output from calculate_statistics()
#' @return A character string with formatted summary
#' @export
format_summary_report <- function(stats) {
  lines <- c(
    "===== GRADING SUMMARY =====",
    "",
    sprintf("Students graded: %d", stats$aggregate$n_students),
    "",
    "Score Distribution:",
    sprintf("  Mean:   %.1f%%", stats$aggregate$mean),
    sprintf("  Median: %.1f%%", stats$aggregate$median),
    sprintf("  Std Dev: %.1f%%", stats$aggregate$sd),
    sprintf("  Range:  %.1f%% - %.1f%%", stats$aggregate$min, stats$aggregate$max),
    "",
    "Most Missed Questions:",
    ""
  )

  # Add most missed questions
  most_missed <- stats$per_question |>
    dplyr::arrange(pct_correct) |>
    utils::head(5)

  for (i in seq_len(nrow(most_missed))) {
    lines <- c(lines, sprintf(
      "  Q%d: %.1f%% correct",
      most_missed$resp_no[i],
      most_missed$pct_correct[i]
    ))
  }

  paste(lines, collapse = "\n")
}
