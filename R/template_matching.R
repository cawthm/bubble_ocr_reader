#' Template Matching Module
#'
#' Functions for matching detected bubbles to template positions
#' and calculating perspective corrections.

#' Find nearest template match for each bubble
#'
#' Matches detected bubbles to their nearest template positions.
#'
#' @param bubbles A data.table with detected bubble positions (target, x, y, z)
#' @param template A data.table with template positions (target, x, y, z)
#' @return A data.table with bubble positions and their nearest template matches
#' @export
find_nearest_template_match <- function(bubbles, template) {
  if (nrow(bubbles) == 0) {
    return(data.table::data.table())
  }

  bubbles <- bubbles[, 1:4]
  template <- template[, 1:4]

  neighbors <- tibble::tibble(
    neighbor_name = character(),
    x_neigh = numeric(),
    y_neigh = numeric(),
    z_neigh = numeric(),
    distance = numeric()
  )

  for (i in 1:nrow(bubbles)) {
    distances <- purrr::map2_dbl(
      .x = template$x,
      .y = template$y,
      .f = euclidean_distance,
      x2 = bubbles$x[i],
      y2 = bubbles$y[i]
    )

    idx <- order(distances)[1]

    new <- tibble::tibble(
      neighbor_name = template[idx, ]$target,
      x_neigh = template[idx, ]$x,
      y_neigh = template[idx, ]$y,
      z_neigh = template[idx, ]$z,
      distance = sort(distances)[1]
    )

    neighbors <- dplyr::bind_rows(neighbors, new)
  }

  dplyr::bind_cols(bubbles, neighbors)
}

#' Calculate perspective correction parameters
#'
#' Uses the 4 calibration markers to calculate perspective transformation
#' parameters for correcting scan distortion.
#'
#' @param bubbles A data.table with detected bubble positions
#' @param template A data.table with template positions including markers
#' @param marker_names Names of the 4 calibration markers
#'   (default: MK_NA_1, MK_NA_2, MK_NA_3, MK_NA_4)
#' @param z_threshold Minimum intensity for marker detection (default: 400)
#' @return A numeric vector of correction parameters for image_distort(),
#'   or NULL if markers could not be found
#' @export
calculate_perspective_correction <- function(
    bubbles,
    template,
    marker_names = c("MK_NA_1", "MK_NA_2", "MK_NA_3", "MK_NA_4"),
    z_threshold = 400) {

  # Match bubbles to template
  matches <- find_nearest_template_match(bubbles[, 1:4], template)

  # Parse the match names and filter for markers
  marks <- matches |>
    dplyr::arrange(distance) |>
    tidyr::separate(
      neighbor_name,
      into = c("mark_type", "ans", "resp_no"),
      remove = FALSE
    ) |>
    dplyr::mutate(ans = dplyr::na_if(ans, "NA")) |>
    dplyr::filter(mark_type == "MK") |>
    dplyr::arrange(distance) |>
    dplyr::filter(z >= z_threshold) |>
    dplyr::mutate(metric = z / distance) |>
    dplyr::ungroup() |>
    dplyr::group_by(neighbor_name) |>
    dplyr::slice_max(order_by = metric, n = 1) |>
    dplyr::slice_head(n = 1) |>
    dplyr::select(
      point = neighbor_name, x, y, z,
      x_target = x_neigh, y_target = y_neigh, distance
    ) |>
    dplyr::mutate(from_to = paste(x, y, x_target, y_target, sep = ","))

  # Filter to just the corner markers
  marks <- marks |>
    dplyr::filter(point %in% marker_names)

  if (nrow(marks) < 4) {
    warning("Could not find all 4 calibration markers. Found: ", nrow(marks))
    return(NULL)
  }

  # Build correction vector: x1,y1,x1',y1', x2,y2,x2',y2', ...
  correction_str <- paste0(marks$from_to, collapse = ",")
  corrections <- as.integer(strsplit(correction_str, ",")[[1]])

  corrections
}

#' Apply perspective correction to an image
#'
#' Uses the calculated correction parameters to straighten a scanned image.
#'
#' @param image A magick image object
#' @param corrections A numeric vector of correction parameters from
#'   calculate_perspective_correction()
#' @return A corrected magick image object
#' @export
apply_perspective_correction <- function(image, corrections) {
  if (is.null(corrections)) {
    warning("No corrections provided, returning original image")
    return(image)
  }

  if (length(corrections) != 16) {
    stop("Corrections must be a vector of 16 integers (4 point pairs)")
  }

  magick::image_distort(image, distortion = "perspective", corrections)
}

#' Import euclidean_distance from bubble_detection
#' @noRd
euclidean_distance <- function(x1, y1, x2, y2) {
  sqrt((x2 - x1)^2 + (y2 - y1)^2)
}
