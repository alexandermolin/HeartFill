#' Flower outline coordinates
#'
#' A rounded-petal flower (polar rose) with one petal facing up and a
#' circular hole in the centre, fitted to a unit box centred on 0. The hole
#' is a second sub-path (`id == 2`), so the bar fill shows through it.
#'
#' @param petals Number of petals.
#' @param depth How deeply the petals are separated, 0 (circle) to 1.
#' @param centre Radius of the centre hole relative to the outer radius;
#'   0 for no hole.
#' @param n Number of vertices on the petal outline.
#' @return A list with numeric vectors `x`, `y` in `[-0.5, 0.5]` and
#'   sub-path `id`.
#' @export
flower_shape <- function(petals = 5, depth = 0.6, centre = 0.2, n = 200) {
  t <- seq(0, 2 * pi, length.out = n + 1)[-(n + 1)]
  r <- 1 - depth + depth * abs(cos(petals / 2 * (t - pi / 2)))
  x <- r * cos(t)
  y <- r * sin(t)
  id <- rep(1L, n)
  if (centre > 0) {
    tc <- seq(0, 2 * pi, length.out = 41)[-41]
    x <- c(x, centre * cos(tc))
    y <- c(y, centre * sin(tc))
    id <- c(id, rep(2L, 40))
  }
  # Centre on the petal outline's bounding box and scale the larger side to 1.
  outer <- id == 1L
  mx <- mean(range(x[outer]))
  my <- mean(range(y[outer]))
  s <- max(diff(range(x[outer])), diff(range(y[outer])))
  list(x = (x - mx) / s, y = (y - my) / s, id = id)
}

#' Flowers pattern geometry for ggpattern / gridpattern
#'
#' Draws a staggered grid of five-petal flowers clipped to the boundary
#' polygon. After `library(HeartFill)` use `pattern = "flowers"` in any
#' `ggpattern` geom. Honours the same aesthetics as
#' [create_pattern_hearts()].
#'
#' @inheritParams create_pattern_hearts
#' @return A grob.
#' @export
create_pattern_flowers <- function(params, boundary_df, aspect_ratio,
                                   legend = FALSE) {
  tile_shape_pattern(params, boundary_df, legend, flower_shape(), "flowers")
}
