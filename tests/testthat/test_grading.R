# Tests for grading.R

test_that("create_answer_key creates valid key", {
  answers <- c("a", "b", "c", "d", "e")
  result <- create_answer_key(answers)

  expect_equal(nrow(result), 5)
  expect_true("resp_no" %in% names(result))
  expect_true("key" %in% names(result))
  expect_equal(result$resp_no, 1:5)
  expect_equal(result$key, c("a", "b", "c", "d", "e"))
})

test_that("create_answer_key normalizes case", {
  answers <- c("A", "B", "C")
  result <- create_answer_key(answers)

  expect_equal(result$key, c("a", "b", "c"))
})

test_that("grade_responses calculates correct answers", {
  responses <- data.table::data.table(
    resp_no = 1:5,
    ans = c("a", "b", "c", "d", "e"),
    metric = rep(100, 5)
  )

  answer_key <- create_answer_key(c("a", "b", "c", "d", "e"))

  graded <- grade_responses(responses, answer_key)

  expect_equal(nrow(graded), 5)
  expect_true("correct" %in% names(graded))
  expect_true(all(graded$correct)) # All should be correct
})

test_that("grade_responses handles incorrect answers", {
  responses <- data.table::data.table(
    resp_no = 1:5,
    ans = c("a", "a", "a", "a", "a"), # All 'a'
    metric = rep(100, 5)
  )

  answer_key <- create_answer_key(c("a", "b", "c", "d", "e"))

  graded <- grade_responses(responses, answer_key)

  expect_equal(sum(graded$correct), 1) # Only first should be correct
})

test_that("grade_responses is case-insensitive", {
  responses <- data.table::data.table(
    resp_no = 1:3,
    ans = c("A", "B", "C"),
    metric = rep(100, 3)
  )

  answer_key <- create_answer_key(c("a", "b", "c"))

  graded <- grade_responses(responses, answer_key)

  expect_true(all(graded$correct))
})

test_that("calculate_score returns correct statistics", {
  # Use data.frame then convert to avoid key column conflict
  graded <- data.frame(
    resp_no = 1:10,
    ans = letters[1:10],
    key = letters[1:10],
    correct = c(rep(TRUE, 7), rep(FALSE, 3)),
    stringsAsFactors = FALSE
  ) |> data.table::as.data.table()

  score <- calculate_score(graded)

  expect_equal(score$n_correct, 7)
  expect_equal(score$n_total, 10)
  expect_equal(score$score, 0.7)
  expect_equal(score$percentage, 70)
})

test_that("calculate_statistics computes per-student stats", {
  # Use data.frame then convert to avoid key column conflict
  all_graded <- data.frame(
    student_id = c(rep("student1", 5), rep("student2", 5)),
    resp_no = rep(1:5, 2),
    ans = rep(c("a", "b", "c", "d", "e"), 2),
    key = rep(c("a", "b", "c", "d", "e"), 2),
    correct = c(rep(TRUE, 5), c(TRUE, TRUE, TRUE, FALSE, FALSE)),
    stringsAsFactors = FALSE
  ) |> data.table::as.data.table()

  stats <- calculate_statistics(all_graded)

  expect_true("per_student" %in% names(stats))
  expect_true("per_question" %in% names(stats))
  expect_true("aggregate" %in% names(stats))

  expect_equal(nrow(stats$per_student), 2)
  expect_equal(stats$aggregate$n_students, 2)
  expect_equal(stats$aggregate$mean, 80) # (100 + 60) / 2
})

test_that("calculate_statistics computes per-question stats", {
  # Use data.frame then convert to avoid key column conflict
  all_graded <- data.frame(
    student_id = c(rep("student1", 3), rep("student2", 3)),
    resp_no = rep(1:3, 2),
    ans = c("a", "b", "c", "a", "b", "x"),
    key = c("a", "b", "c", "a", "b", "c"),
    correct = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
    stringsAsFactors = FALSE
  ) |> data.table::as.data.table()

  stats <- calculate_statistics(all_graded)

  expect_equal(nrow(stats$per_question), 3)
  expect_equal(stats$per_question$pct_correct[1], 100) # Q1: both correct
  expect_equal(stats$per_question$pct_correct[3], 50) # Q3: one correct
})

test_that("format_summary_report produces readable output", {
  stats <- list(
    aggregate = list(
      n_students = 10,
      mean = 75.5,
      median = 77.0,
      sd = 12.3,
      min = 50,
      max = 95
    ),
    per_question = data.frame(
      resp_no = 1:5,
      pct_correct = c(90, 80, 70, 60, 50)
    )
  )

  report <- format_summary_report(stats)

  expect_type(report, "character")
  expect_true(grepl("Students graded: 10", report))
  expect_true(grepl("Mean:", report))
  expect_true(grepl("Most Missed Questions", report))
})
