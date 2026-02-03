#' Image Processing Module
#'
#' Functions for reading, preprocessing, and converting images for bubble detection.

#' Read a PDF file as an image
#'
#' @param path Path to the PDF file
#' @return A magick image object
#' @export
read_pdf_as_image <- function(path) {
  if (!file.exists(path)) {
    stop("File not found: ", path)
  }

  if (!grepl("\\.pdf$", path, ignore.case = TRUE)) {
    warning("File may not be a PDF: ", path)
  }

  magick::image_read(path)
}

#' Preprocess an image for bubble detection
#'
#' Applies scaling, grayscale conversion, median filtering, and thresholding.
#'
#' @param image A magick image object
#' @param config A list containing image processing parameters:
#'   - target_width: Target width in pixels (default: 1260)
#'   - target_height: Target height in pixels (default: 1530)
#'   - colorspace: Color space for quantization (default: "LinearGray")
#'   - colors: Number of colors after quantization (default: 2)
#'   - median_radius: Radius for median filter (default: 14)
#'   - threshold_type: Type of threshold (default: "white")
#'   - threshold_percent: Threshold percentage (default: 40)
#' @return A preprocessed magick image object
#' @export
preprocess_image <- function(image, config = list()) {
  # Set defaults
  target_width <- config$target_width %||% 1260
  target_height <- config$target_height %||% 1530
  colorspace <- config$colorspace %||% "LinearGray"
  colors <- config$colors %||% 2
  median_radius <- config$median_radius %||% 14
  threshold_type <- config$threshold_type %||% "white"
  threshold_percent <- config$threshold_percent %||% 40

  # Build geometry string
  geometry <- paste0(target_width, "x", target_height)
  threshold_str <- paste0(threshold_percent, "%")

  # Apply processing pipeline
  image |>
    magick::image_scale(geometry) |>
    magick::image_quantize(colorspace = colorspace, max = colors) |>
    magick::image_median(radius = median_radius) |>
    magick::image_threshold(
      type = threshold_type,
      threshold = threshold_str,
      channel = NULL
    )
}

#' Convert a magick image to a binary matrix
#'
#' Extracts pixel values from a magick image and converts to a binary matrix
#' where 1 = black (filled) and 0 = white (empty).
#'
#' @param image A preprocessed magick image object
#' @return A numeric matrix with values 0 (white) or 1 (black)
#' @export
image_to_matrix <- function(image) {
  # Extract the first channel as a matrix
  # image[[1]] gives the raw data, [1,,] gets first channel
  raw_matrix <- image[[1]][1, , ]

  # Convert hex strings to integers (0-255)
  int_matrix <- apply(raw_matrix, c(1, 2), function(x) {
    strtoi(paste0("0x", x))
  })

  # Normalize and invert: 255 (white) -> 0, 0 (black) -> 1
  # Transpose to get correct orientation
  binary_matrix <- 1 - t(int_matrix) / 255

  binary_matrix
}

#' Read and process a PDF file in one step
#'
#' Convenience function that combines reading, preprocessing, and conversion.
#'
#' @param path Path to the PDF file
#' @param config Optional configuration list for preprocessing
#' @return A binary matrix representation of the image
#' @export
read_and_process <- function(path, config = list()) {
  image <- read_pdf_as_image(path)
  processed <- preprocess_image(image, config)
  image_to_matrix(processed)
}

#' Null-coalescing operator
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
