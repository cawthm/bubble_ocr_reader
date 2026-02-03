#' Configuration Module
#'
#' Functions for loading and validating configuration files.

#' Load configuration from a YAML file
#'
#' @param path Path to the YAML configuration file
#' @return A list containing configuration values
#' @export
load_config <- function(path = "config/config.yaml") {
  if (!file.exists(path)) {
    warning("Config file not found: ", path, ". Using defaults.")
    return(get_default_config())
  }

  config <- yaml::read_yaml(path)
  validate_config(config)

  config
}

#' Get default configuration values
#'
#' Returns sensible defaults for all configuration parameters.
#'
#' @return A list of default configuration values
#' @export
get_default_config <- function() {
  list(
    image = list(
      target_width = 1260,
      target_height = 1530,
      colorspace = "LinearGray",
      colors = 2,
      median_radius = 14,
      threshold_type = "white",
      threshold_percent = 40
    ),
    detection = list(
      peak_threshold = 10,
      dedup_radius = 10,
      confidence_min = 40,
      marker_z_threshold = 400
    ),
    template = list(
      markers = c("MK_NA_1", "MK_NA_2", "MK_NA_3", "MK_NA_4")
    ),
    paths = list(
      bubble_map = "data/bubble_map.csv",
      roster = "data/roster.csv",
      scans_dir = "input/",
      output_dir = "output/",
      keys_dir = "keys/"
    ),
    answer_keys = list(),
    output = list(
      results_prefix = "results",
      summary_prefix = "summary",
      audit_prefix = "audit",
      timestamp_format = "%Y%m%d_%H%M%S"
    ),
    processing = list(
      parallel = FALSE,
      n_workers = 4,
      verbose = TRUE,
      save_intermediate = FALSE
    )
  )
}

#' Validate configuration values
#'
#' Checks that required fields are present and have valid values.
#'
#' @param config A configuration list to validate
#' @return TRUE if valid, throws error otherwise
#' @export
validate_config <- function(config) {
  # Check required sections exist
  required_sections <- c("image", "detection", "paths")

  for (section in required_sections) {
    if (!section %in% names(config)) {
      stop("Missing required config section: ", section)
    }
  }

  # Validate image parameters
  if (!is.null(config$image$target_width)) {
    if (!is.numeric(config$image$target_width) || config$image$target_width <= 0) {
      stop("image.target_width must be a positive number")
    }
  }

  if (!is.null(config$image$target_height)) {
    if (!is.numeric(config$image$target_height) || config$image$target_height <= 0) {
      stop("image.target_height must be a positive number")
    }
  }

  if (!is.null(config$image$threshold_percent)) {
    if (!is.numeric(config$image$threshold_percent) ||
        config$image$threshold_percent < 0 ||
        config$image$threshold_percent > 100) {
      stop("image.threshold_percent must be between 0 and 100")
    }
  }

  # Validate detection parameters
  if (!is.null(config$detection$peak_threshold)) {
    if (!is.numeric(config$detection$peak_threshold) || config$detection$peak_threshold < 0) {
      stop("detection.peak_threshold must be non-negative")
    }
  }

  TRUE
}

#' Merge user config with defaults
#'
#' Fills in missing values from defaults.
#'
#' @param user_config User-provided configuration list
#' @return A complete configuration list
#' @export
merge_with_defaults <- function(user_config) {
  defaults <- get_default_config()

  # Recursive merge function
  merge_lists <- function(base, override) {
    if (!is.list(base) || !is.list(override)) {
      return(override)
    }

    for (name in names(override)) {
      if (name %in% names(base) && is.list(base[[name]]) && is.list(override[[name]])) {
        base[[name]] <- merge_lists(base[[name]], override[[name]])
      } else {
        base[[name]] <- override[[name]]
      }
    }

    base
  }

  merge_lists(defaults, user_config)
}

#' Print configuration summary
#'
#' @param config A configuration list
#' @export
print_config <- function(config) {
  cat("=== Bubble OCR Configuration ===\n\n")

  cat("Image Processing:\n")
  cat(sprintf("  Size: %dx%d\n", config$image$target_width, config$image$target_height))
  cat(sprintf("  Median radius: %d\n", config$image$median_radius))
  cat(sprintf("  Threshold: %d%%\n", config$image$threshold_percent))

  cat("\nDetection:\n")
  cat(sprintf("  Peak threshold: %d\n", config$detection$peak_threshold))
  cat(sprintf("  Dedup radius: %d\n", config$detection$dedup_radius))
  cat(sprintf("  Min confidence: %d\n", config$detection$confidence_min))

  cat("\nPaths:\n")
  cat(sprintf("  Bubble map: %s\n", config$paths$bubble_map))
  cat(sprintf("  Scans dir: %s\n", config$paths$scans_dir))
  cat(sprintf("  Output dir: %s\n", config$paths$output_dir))

  invisible(config)
}
