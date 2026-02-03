# Tests for data_io.R

test_that("load_bubble_map reads valid CSV", {
  # Skip if running from tests directory
  skip_if_not(file.exists("data/bubble_map.csv"), "bubble_map.csv not found")

  bubble_map <- load_bubble_map("data/bubble_map.csv")

  expect_s3_class(bubble_map, "data.table")
  expect_true("target" %in% names(bubble_map))
  expect_true("x" %in% names(bubble_map))
  expect_true("y" %in% names(bubble_map))
  expect_true("z" %in% names(bubble_map))
  expect_gt(nrow(bubble_map), 0)
})

test_that("load_bubble_map errors for missing file", {
  expect_error(
    load_bubble_map("nonexistent.csv"),
    "Bubble map file not found"
  )
})

test_that("generate_output_path creates correct format", {
  path <- generate_output_path(
    prefix = "results",
    quiz_name = "quiz_1",
    extension = "csv",
    output_dir = "output",
    timestamp_format = "%Y%m%d"
  )

  expect_true(grepl("^output/results_quiz_1_", path))
  expect_true(grepl("\\.csv$", path))
})

test_that("list_scan_files returns vector", {
  # Create a temp directory for testing
  temp_dir <- tempdir()

  # Should return empty vector with warning for no matches
  expect_warning(
    files <- list_scan_files(temp_dir, pattern = "\\.nonexistent$"),
    "No matching files found"
  )

  expect_type(files, "character")
  expect_equal(length(files), 0)
})

test_that("list_scan_files errors for missing directory", {
  expect_error(
    list_scan_files("nonexistent_directory"),
    "Scan directory not found"
  )
})

test_that("validate_file checks existence", {
  expect_error(
    validate_file("nonexistent_file.pdf"),
    "File not found"
  )
})

test_that("validate_file warns for wrong extension", {
  # Create a temp file with wrong extension
  temp_file <- tempfile(fileext = ".txt")
  writeLines("test", temp_file)

  expect_warning(
    validate_file(temp_file, expected_ext = c("pdf")),
    "Unexpected file extension"
  )

  unlink(temp_file)
})

test_that("validate_file passes for valid file", {
  # Create a temp file with correct extension
  temp_file <- tempfile(fileext = ".pdf")
  writeLines("test", temp_file)

  expect_true(validate_file(temp_file, expected_ext = c("pdf")))

  unlink(temp_file)
})
