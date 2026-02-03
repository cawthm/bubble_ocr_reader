#' Response Extraction Module
#'
#' Functions for extracting student ID, test version, and responses
#' from matched bubble data.

#' Extract student ID from matched bubbles
#'
#' Parses ID bubble matches to extract the 10-digit student ID.
#'
#' @param matches A data.table with matched bubble data including mark_type,
#'   ans, resp_no, and metric columns
#' @return A 10-character string representing the student ID, or NA if incomplete
#' @export
extract_student_id <- function(matches) {
  id_matches <- matches |>
    dplyr::filter(mark_type == "ID") |>
    dplyr::group_by(resp_no) |>
    dplyr::slice_max(metric, n = 1) |>
    dplyr::ungroup() |>
    dplyr::arrange(as.numeric(resp_no)) |>
    dplyr::select(resp_no, ans, metric)

  if (nrow(id_matches) == 0) {
    return(NA_character_)
  }

  # Concatenate digits
  student_id <- paste0(id_matches$ans, collapse = "")

  # Validate length

  if (nchar(student_id) < 9) {
    warning("Student ID appears incomplete: ", student_id)
  }

  student_id
}

#' Extract test version from matched bubbles
#'
#' Parses TV (test version) bubble matches to extract the 4-character version code.
#' The test version grid has 4 columns, each with options w/x/y/z.
#'
#' @param matches A data.table with matched bubble data including mark_type,
#'   ans, resp_no, and metric columns
#' @return A 4-character string (e.g., "wxyz"), or NA if incomplete
#' @export
extract_test_version <- function(matches) {
 tv_matches <- matches |>
    dplyr::filter(mark_type == "TV") |>
    dplyr::group_by(resp_no) |>
    dplyr::slice_max(metric, n = 1) |>
    dplyr::ungroup() |>
    dplyr::arrange(as.numeric(resp_no))

  if (nrow(tv_matches) == 0) {
    return(NA_character_)
  }

  # Concatenate the answers for positions 1-4
  version <- paste0(tv_matches$ans, collapse = "")

  if (nchar(version) != 4) {
    warning("Test version appears incomplete: ", version, " (expected 4 characters)")
  }

  tolower(version)
}

#' Extract question responses from matched bubbles
#'
#' Parses QU (question) bubble matches to extract student responses.
#'
#' @param matches A data.table with matched bubble data including mark_type,
#'   ans, resp_no, and metric columns
#' @param n_questions Expected number of questions (for validation)
#' @param confidence_min Minimum confidence score to accept a response (default: 40)
#' @return A data.table with columns: resp_no, ans, metric
#' @export
extract_responses <- function(matches, n_questions = NULL, confidence_min = 40) {
  responses <- matches |>
    dplyr::filter(mark_type == "QU") |>
    dplyr::group_by(resp_no) |>
    dplyr::slice_max(metric, n = 1) |>
    dplyr::filter(metric >= confidence_min) |>
    dplyr::ungroup() |>
    dplyr::select(resp_no, ans, metric) |>
    dplyr::arrange(as.numeric(resp_no)) |>
    data.table::as.data.table()

  if (!is.null(n_questions) && nrow(responses) != n_questions) {
    warning(
      "Expected ", n_questions, " responses but found ", nrow(responses),
      ". Some questions may be unanswered or have low confidence."
    )
  }

  responses
}

#' Extract all information from matched bubbles
#'
#' Combines extraction of student ID, test version, and responses.
#'
#' @param matches A data.table with matched bubble data
#' @param n_questions Expected number of questions (optional)
#' @param confidence_min Minimum confidence score for responses (default: 40)
#' @return A list with components: student_id, test_version, responses (data.table)
#' @export
extract_all <- function(matches, n_questions = NULL, confidence_min = 40) {
  list(
    student_id = extract_student_id(matches),
    test_version = extract_test_version(matches),
    responses = extract_responses(matches, n_questions, confidence_min)
  )
}

#' Parse and prepare matches for extraction
#'
#' Takes raw matched data and adds parsed columns for mark type, answer, and position.
#'
#' @param raw_matches A data.table from find_nearest_template_match()
#' @return A data.table with additional parsed columns
#' @export
prepare_matches_for_extraction <- function(raw_matches) {
  raw_matches |>
    tidyr::separate(
      neighbor_name,
      into = c("mark_type", "ans", "resp_no"),
      remove = FALSE
    ) |>
    dplyr::filter(!is.na(resp_no)) |>
    dplyr::filter(mark_type %in% c("ID", "TV", "QU")) |>
    dplyr::arrange(resp_no) |>
    dplyr::mutate(
      metric = z / (distance + 1),
      resp_no = as.integer(resp_no)
    ) |>
    dplyr::group_by(mark_type, resp_no) |>
    data.table::as.data.table()
}

#' Convert Excel-style cell address
#'
#' Utility function for debugging - converts x,y coordinates to Excel-style addresses.
#'
#' @param x Column number (1-indexed)
#' @param y Row number
#' @return A string like "A1", "AA100", etc.
#' @export
excel_address <- function(x, y) {
  column_address <- ""

  while (x > 0) {
    index <- (x - 1) %% 26 + 1
    column_address <- paste0(LETTERS[index], column_address)
    x <- (x - index) / 26
  }

  paste0(column_address, y)
}
