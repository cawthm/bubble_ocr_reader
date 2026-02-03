# Tests for response_extraction.R

test_that("extract_student_id returns concatenated digits", {
  matches <- data.table::data.table(
    mark_type = rep("ID", 10),
    resp_no = as.character(1:10),
    ans = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"),
    metric = rep(100, 10)
  )

  student_id <- extract_student_id(matches)

  expect_equal(student_id, "0123456789")
})

test_that("extract_student_id selects highest metric", {
  matches <- data.table::data.table(
    mark_type = rep("ID", 4),
    resp_no = c("1", "1", "2", "2"),
    ans = c("5", "1", "2", "8"),
    metric = c(50, 100, 100, 50) # Higher metric for "1" at position 1
  )

  student_id <- extract_student_id(matches)

  expect_equal(student_id, "12")
})

test_that("extract_student_id returns NA for empty matches", {
  matches <- data.table::data.table(
    mark_type = character(0),
    resp_no = character(0),
    ans = character(0),
    metric = numeric(0)
  )

  student_id <- extract_student_id(matches)

  expect_true(is.na(student_id))
})

test_that("extract_test_version returns highest metric answer", {
  matches <- data.table::data.table(
    mark_type = c("TV", "TV", "TV"),
    resp_no = c("1", "2", "3"),
    ans = c("x", "y", "z"),
    metric = c(50, 100, 75)
  )

  version <- extract_test_version(matches)

  expect_equal(version, "y")
})

test_that("extract_test_version returns NA for no matches", {
  matches <- data.table::data.table(
    mark_type = "ID",
    resp_no = "1",
    ans = "5",
    metric = 100
  )

  version <- extract_test_version(matches)

  expect_true(is.na(version))
})

test_that("extract_responses filters by confidence", {
  matches <- data.table::data.table(
    mark_type = rep("QU", 5),
    resp_no = as.character(1:5),
    ans = c("a", "b", "c", "d", "e"),
    metric = c(100, 50, 30, 80, 45)
  )

  responses <- extract_responses(matches, confidence_min = 40)

  # Should exclude Q3 (metric=30)
  expect_equal(nrow(responses), 4)
  expect_false("3" %in% as.character(responses$resp_no))
})

test_that("extract_responses selects highest metric per question", {
  matches <- data.table::data.table(
    mark_type = rep("QU", 6),
    resp_no = c("1", "1", "2", "2", "3", "3"),
    ans = c("a", "b", "c", "d", "e", "a"),
    metric = c(100, 50, 50, 100, 80, 90)
  )

  responses <- extract_responses(matches, confidence_min = 40)

  expect_equal(nrow(responses), 3)
  expect_equal(responses[resp_no == 1]$ans, "a") # Higher metric
  expect_equal(responses[resp_no == 2]$ans, "d")
  expect_equal(responses[resp_no == 3]$ans, "a")
})

test_that("extract_responses warns for question count mismatch", {
  matches <- data.table::data.table(
    mark_type = rep("QU", 3),
    resp_no = as.character(1:3),
    ans = c("a", "b", "c"),
    metric = c(100, 100, 100)
  )

  expect_warning(
    extract_responses(matches, n_questions = 5),
    "Expected 5 responses"
  )
})

test_that("extract_all combines all extractions", {
  matches <- data.table::data.table(
    mark_type = c(rep("ID", 3), "TV", rep("QU", 2)),
    resp_no = c("1", "2", "3", "1", "1", "2"),
    ans = c("1", "2", "3", "x", "a", "b"),
    metric = rep(100, 6)
  )

  result <- extract_all(matches)

  expect_true("student_id" %in% names(result))
  expect_true("test_version" %in% names(result))
  expect_true("responses" %in% names(result))

  expect_equal(result$student_id, "123")
  expect_equal(result$test_version, "x")
  expect_equal(nrow(result$responses), 2)
})

test_that("excel_address converts coordinates correctly", {
  expect_equal(excel_address(1, 1), "A1")
  expect_equal(excel_address(26, 1), "Z1")
  expect_equal(excel_address(27, 1), "AA1")
  expect_equal(excel_address(1, 100), "A100")
})
