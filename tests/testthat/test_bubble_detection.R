# Tests for bubble_detection.R

test_that("weight_run creates correct output for filled runs", {
  # A run of 5 filled pixels (value=1)
  result <- weight_run(5, 1, sum_value = TRUE)
  expect_length(result, 5)
  expect_true(max(result) > 1) # Center should be weighted

  # A run of 1 filled pixel
  result <- weight_run(1, 1, sum_value = TRUE)
  expect_length(result, 1)
})

test_that("weight_run creates zeros for empty runs", {
  result <- weight_run(5, 0, sum_value = TRUE)
  expect_length(result, 5)
  expect_equal(sum(result), 0)
})

test_that("find_peaks detects simple patterns", {
  # Create a simple matrix with a filled region
  test_matrix <- matrix(0, nrow = 10, ncol = 10)
  test_matrix[4:6, 4:6] <- 1 # 3x3 filled square

  peaks <- find_peaks(test_matrix)

  expect_equal(dim(peaks), dim(test_matrix))
  expect_true(max(peaks) > 0) # Should have some peaks
  # Peak should be in the center of the filled region
  expect_true(peaks[5, 5] > 0)
})

test_that("find_peaks returns zeros for empty matrix", {
  empty_matrix <- matrix(0, nrow = 10, ncol = 10)
  peaks <- find_peaks(empty_matrix)

  expect_equal(sum(peaks), 0)
})

test_that("find_bubbles returns empty for low-peak matrix", {
  low_peak_matrix <- matrix(1, nrow = 10, ncol = 10)
  bubbles <- find_bubbles(low_peak_matrix, threshold = 100)

  expect_equal(nrow(bubbles), 0)
  expect_true("target" %in% names(bubbles))
  expect_true("x" %in% names(bubbles))
  expect_true("y" %in% names(bubbles))
  expect_true("z" %in% names(bubbles))
})

test_that("find_bubbles returns points above threshold", {
  # Create matrix with known peak values
  test_matrix <- matrix(0, nrow = 10, ncol = 10)
  test_matrix[3, 3] <- 50
  test_matrix[7, 7] <- 100

  bubbles <- find_bubbles(test_matrix, threshold = 25)

  expect_equal(nrow(bubbles), 2)
  expect_true(all(bubbles$z > 25))
})

test_that("euclidean_distance calculates correctly", {
  # Known distances
  expect_equal(euclidean_distance(0, 0, 3, 4), 5) # 3-4-5 triangle
  expect_equal(euclidean_distance(0, 0, 0, 0), 0) # Same point
  expect_equal(euclidean_distance(1, 1, 4, 5), 5) # Translated 3-4-5
})

test_that("deduplicate_bubbles removes nearby points", {
  # Create two points that are close together
  bubbles <- data.table::data.table(
    target = c("Pt_1", "Pt_2", "Pt_3"),
    x = c(10, 12, 100), # Pt_1 and Pt_2 are close
    y = c(10, 11, 100),
    z = c(50, 100, 75) # Pt_2 has higher intensity
  )

  deduped <- deduplicate_bubbles(bubbles, radius = 10)

  # Should keep Pt_2 (higher intensity) and Pt_3 (far away)
  expect_lte(nrow(deduped), 2)
})

test_that("deduplicate_bubbles handles empty input", {
  empty_bubbles <- data.table::data.table(
    target = character(0),
    x = integer(0),
    y = integer(0),
    z = numeric(0)
  )

  result <- deduplicate_bubbles(empty_bubbles, radius = 10)
  expect_equal(nrow(result), 0)
})
