# Tests for config.R

test_that("get_default_config returns complete config", {
  config <- get_default_config()

  expect_type(config, "list")
  expect_true("image" %in% names(config))
  expect_true("detection" %in% names(config))
  expect_true("paths" %in% names(config))

  # Check image defaults

  expect_equal(config$image$target_width, 1260)
  expect_equal(config$image$target_height, 1530)
  expect_equal(config$image$median_radius, 14)
  expect_equal(config$image$threshold_percent, 40)

  # Check detection defaults
  expect_equal(config$detection$peak_threshold, 10)
  expect_equal(config$detection$dedup_radius, 10)
  expect_equal(config$detection$confidence_min, 40)
})

test_that("validate_config catches invalid values", {
  # Missing required section
  expect_error(validate_config(list()), "Missing required config section")

  # Invalid image width
  bad_config <- get_default_config()
  bad_config$image$target_width <- -100
  expect_error(validate_config(bad_config), "must be a positive number")

  # Invalid threshold
  bad_config <- get_default_config()
  bad_config$image$threshold_percent <- 150
  expect_error(validate_config(bad_config), "must be between 0 and 100")
})

test_that("validate_config accepts valid config", {
  config <- get_default_config()
  expect_true(validate_config(config))
})

test_that("merge_with_defaults fills in missing values", {
  partial_config <- list(
    image = list(target_width = 800)
  )

  merged <- merge_with_defaults(partial_config)

  # User value preserved
  expect_equal(merged$image$target_width, 800)

  # Default values filled in
  expect_equal(merged$image$target_height, 1530)
  expect_equal(merged$detection$peak_threshold, 10)
})

test_that("load_config returns defaults for missing file", {
  # Should return defaults with warning
  expect_warning(
    config <- load_config("nonexistent_file.yaml"),
    "Config file not found"
  )

  expect_type(config, "list")
  expect_equal(config$image$target_width, 1260)
})
