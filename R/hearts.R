#' Heart outline coordinates
#'
#' Classic parametric heart curve, normalised to fit a unit box centred on 0.
#'
#' @param n Number of vertices.
#' @return A list with numeric vectors `x` and `y` in `[-0.5, 0.5]`.
#' @export
heart_shape <- function(n = 60) {
  t <- seq(0, 2 * pi, length.out = n + 1)[-(n + 1)]
  x <- 16 * sin(t)^3
  y <- 13 * cos(t) - 5 * cos(2 * t) - 2 * cos(3 * t) - cos(4 * t)
  x <- x / 32
  y <- (y - mean(range(y))) / diff(range(y))
  list(x = x, y = y)
}

#' Hearts pattern geometry for ggpattern / gridpattern
#'
#' Draws a staggered grid of hearts clipped to the boundary polygon. Not
#' usually called directly: after `library(HeartFill)` use
#' `pattern = "hearts"` in any `ggpattern` geom.
#'
#' Honoured aesthetics: `pattern_fill` (heart colour), `pattern_colour`
#' (heart outline), `pattern_linewidth`, `pattern_alpha`,
#' `pattern_spacing` (grid step along the shape's longer side, fraction of
#' panel; the grid follows the shape's proportions with an even gap), `pattern_density`
#' (heart size relative to spacing), `pattern_angle` (heart rotation),
#' `pattern_xoffset`, `pattern_yoffset`.
#'
#' @param params One row of pattern parameters (supplied by ggpattern).
#' @param boundary_df Polygon data frame with `x`, `y`, `id` in npc.
#' @param aspect_ratio Viewport aspect ratio (unused; physical units are used).
#' @param legend Logical, whether drawing a legend key.
#' @return A grob.
#' @export
create_pattern_hearts <- function(params, boundary_df, aspect_ratio,
                                  legend = FALSE) {
  tile_shape_pattern(params, boundary_df, legend, heart_shape(), "hearts")
}
