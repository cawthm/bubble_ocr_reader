#' Bubble Detection Module
#'
#' Functions for detecting filled bubbles in binary image matrices.

#' Weight run helper for peak detection
#'
#' Given a run length and value, creates a weighted vector that concentrates
#' the value in the center of the run. Used for RLE-based peak detection.
#'
#' @param length The length of the run
#' @param value The value of the run (0 or 1)
#' @param sum_value If TRUE, multiply the center value by the run length
#' @return A numeric vector of the same length as the run
#' @noRd
weight_run <- function(length, value, sum_value = FALSE) {
  left_side <- length %/% 2 - 1
  middle <- rep(value * (sum_value * length + !sum_value), min(length, 2))
  right_side <- length %/% 2 - 1 + length %% 2

  c(rep(0, max(left_side, 0)), middle, rep(0, right_side))
}

#' Find peaks in a binary matrix
#'
#' Uses run-length encoding to find peak concentrations of filled pixels.
#' Peaks are detected by finding runs of 1s and weighting them by their length.
#'
#' @param binary_matrix A binary matrix (0s and 1s)
#' @return A matrix of peak intensities (higher = stronger peak)
#' @export
find_peaks <- function(binary_matrix) {
  # Process columns (vertical runs)
  col_rle <- rle(c(binary_matrix))
  col_peaks <- matrix(
    unlist(purrr::map2(col_rle$lengths, col_rle$values, weight_run, sum_value = TRUE)),
    ncol = ncol(binary_matrix)
  )

  # Process rows (horizontal runs)
  row_rle <- rle(c(t(binary_matrix)))
  row_peaks <- t(matrix(
    unlist(purrr::map2(row_rle$lengths, row_rle$values, weight_run, sum_value = TRUE)),
    ncol = ncol(t(binary_matrix))
  ))

  # Multiply to find intersections (peaks in both directions)
  col_peaks * row_peaks
}

#' Find potential bubbles from a peak matrix
#'
#' Identifies points in the peak matrix that exceed a threshold,
#' indicating potential filled bubbles.
#'
#' @param peak_matrix A matrix of peak intensities from find_peaks()
#' @param threshold Minimum peak intensity to consider (default: 10)
#' @return A data.table with columns: target, x, y, z (intensity)
#' @export
find_bubbles <- function(peak_matrix, threshold = 10) {
  # Find indices above threshold
  above_threshold <- which(peak_matrix > threshold)

  if (length(above_threshold) == 0) {
    return(data.table::data.table(
      target = character(0),
      x = integer(0),
      y = integer(0),
      z = numeric(0)
    ))
  }

  # Convert linear indices to x,y coordinates
  n_rows <- nrow(peak_matrix)

  data.table::data.table(
    target = paste0("Pt_", seq_along(above_threshold)),
    x = above_threshold %/% n_rows,
    y = above_threshold %% n_rows,
    z = peak_matrix[above_threshold]
  )
}

#' Calculate Euclidean distance between two points
#'
#' @param x1 X coordinate of first point
#' @param y1 Y coordinate of first point
#' @param x2 X coordinate of second point
#' @param y2 Y coordinate of second point
#' @return The Euclidean distance
#' @export
euclidean_distance <- function(x1, y1, x2, y2) {
  sqrt((x2 - x1)^2 + (y2 - y1)^2)
}

#' Deduplicate nearby bubbles
#'
#' Removes duplicate detections of the same bubble by keeping only the
#' highest-intensity point within a given radius.
#'
#' @param bubbles A data.table from find_bubbles() with columns: target, x, y, z
#' @param radius Maximum distance to consider points as duplicates (default: 10)
#' @return A deduplicated data.table
#' @export
deduplicate_bubbles <- function(bubbles, radius = 10) {
  if (nrow(bubbles) == 0) {
    return(bubbles)
  }

  # Find nearest neighbors (excluding self)
  closest <- find_nearest_neighbors(bubbles, bubbles, exclude_self = TRUE)

  # Split into those that need deduplication and those that don't
  exclude_list <- closest[distance > radius, ]
  dedupe_list <- closest[distance <= radius, ]
  holding_list <- exclude_list

  # Iteratively deduplicate

  while (nrow(exclude_list) < nrow(closest)) {
    holding_list <- holding_list[is.na(x), ]

    while (nrow(dedupe_list) > 0) {
      # Get batch of nearby points
      batch <- dedupe_list[
        neighbor_name %in% c(dedupe_list$target[1], dedupe_list$neighbor_name[1]),
      ]

      # Keep the one with highest intensity
      holding_list <- rbind(holding_list, batch[z == max(z), ][1, ]) |>
        unique()

      dedupe_list <- dedupe_list[!target %in% batch$target, ]
    }

    closest <- rbind(exclude_list, holding_list)
    closest <- find_nearest_neighbors(
      closest[, 1:4],
      closest[, 1:4],
      exclude_self = TRUE
    ) |>
      dplyr::arrange(distance) |>
      data.table::as.data.table()

    dedupe_list <- closest[distance <= radius, ]
    exclude_list <- closest[distance > radius, ]
  }

  exclude_list[, .(target, x, y, z)]
}

#' Find nearest neighbors between two sets of points
#'
#' For each point in the target set, finds the nearest point in the scan set.
#'
#' @param target A data.table with columns: target, x, y, z
#' @param scan A data.table with columns: target, x, y, z
#' @param exclude_self If TRUE, excludes self-matches (use when target == scan)
#' @return A data.table with original columns plus neighbor info
#' @noRd
find_nearest_neighbors <- function(target, scan, exclude_self = FALSE) {
  target <- target[, 1:4]
  scan <- scan[, 1:4]

  # Handle empty target case
  if (nrow(target) == 0) {
    return(data.table::data.table(
      target = character(),
      x = integer(),
      y = integer(),
      z = numeric(),
      neighbor_name = character(),
      x_neigh = numeric(),
      y_neigh = numeric(),
      z_neigh = numeric(),
      distance = numeric()
    ))
  }

  neighbors <- tibble::tibble(
    neighbor_name = character(),
    x_neigh = numeric(),
    y_neigh = numeric(),
    z_neigh = numeric(),
    distance = numeric()
  )

  for (i in 1:nrow(target)) {
    distances <- purrr::map2_dbl(
      .x = scan$x,
      .y = scan$y,
      .f = euclidean_distance,
      x2 = target$x[i],
      y2 = target$y[i]
    )

    # If excluding self, get second nearest; otherwise get nearest
    idx <- order(distances)[1 + exclude_self]

    new <- tibble::tibble(
      neighbor_name = scan[idx, ]$target,
      x_neigh = scan[idx, ]$x,
      y_neigh = scan[idx, ]$y,
      z_neigh = scan[idx, ]$z,
      distance = sort(distances)[1 + exclude_self]
    )

    neighbors <- dplyr::bind_rows(neighbors, new)
  }

  dplyr::bind_cols(target, neighbors)
}
