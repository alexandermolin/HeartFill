#' Star outline coordinates
#'
#' A regular star with one point facing up, fitted to a unit box centred on 0.
#'
#' @param points Number of star points.
#' @param inner Inner radius as a fraction of the outer radius
#'   (0.382 gives the classic five-pointed star).
#' @return A list with numeric vectors `x` and `y` in `[-0.5, 0.5]`.
#' @export
star_shape <- function(points = 5, inner = 0.382) {
  n <- 2 * points
  t <- pi / 2 + seq(0, 2 * pi, length.out = n + 1)[-(n + 1)]
  r <- rep(c(1, inner), points)
  x <- r * cos(t)
  y <- r * sin(t)
  # Centre the bounding box and scale the larger side to 1.
  x <- x - mean(range(x))
  y <- y - mean(range(y))
  s <- max(diff(range(x)), diff(range(y)))
  list(x = x / s, y = y / s)
}

#' Stars pattern geometry for ggpattern / gridpattern
#'
#' Draws a staggered grid of five-pointed stars clipped to the boundary
#' polygon. After `library(heartfill)` use `pattern = "stars"` in any
#' `ggpattern` geom. Honours the same aesthetics as
#' [create_pattern_hearts()].
#'
#' @inheritParams create_pattern_hearts
#' @return A grob.
#' @export
create_pattern_stars <- function(params, boundary_df, aspect_ratio,
                                 legend = FALSE) {
  tile_shape_pattern(params, boundary_df, legend, star_shape(), "stars")
}
