#' Butterfly outline coordinates
#'
#' A stylised butterfly built from separate sub-paths (body, four wings, two
#' antennae) that do not overlap, fitted to a unit box centred on 0. The
#' thin gaps between parts let the bar fill show through.
#'
#' @param antenna_width Thickness of the antennae, in shape units.
#' @return A list with numeric vectors `x`, `y` in `[-0.5, 0.5]` and
#'   sub-path `id`.
#' @export
butterfly_shape <- function(antenna_width = 0.03) {
  ell <- function(cx, cy, a, b, rot, n = 60) {
    t <- seq(0, 2 * pi, length.out = n + 1)[-(n + 1)]
    r <- rot * pi / 180
    x <- a * cos(t)
    y <- b * sin(t)
    list(x = cx + x * cos(r) - y * sin(r), y = cy + x * sin(r) + y * cos(r))
  }
  # A thin curved band: the centre line offset by half the width each side.
  antenna <- function(side, n = 30) {
    t <- seq(0, 1, length.out = n)
    x <- side * (0.02 + 0.16 * t)
    y <- 0.31 + 0.28 * t - 0.08 * t^2
    nx <- -c(diff(y), diff(y)[n - 1])
    ny <- c(diff(x), diff(x)[n - 1])
    l <- sqrt(nx^2 + ny^2) * 2 / antenna_width
    list(x = c(x + nx / l, rev(x - nx / l)), y = c(y + ny / l, rev(y - ny / l)))
  }
  parts <- list(
    ell(0, -0.02, 0.30, 0.045, 90),                                  # body
    ell(0.30, 0.17, 0.25, 0.17, 35), ell(-0.30, 0.17, 0.25, 0.17, -35),  # upper wings
    ell(0.22, -0.20, 0.17, 0.12, -40), ell(-0.22, -0.20, 0.17, 0.12, 40), # lower wings
    antenna(1), antenna(-1)
  )
  x <- unlist(lapply(parts, `[[`, "x"))
  y <- unlist(lapply(parts, `[[`, "y"))
  id <- rep(seq_along(parts), lengths(lapply(parts, `[[`, "x")))
  # Centre the bounding box and scale the larger side to 1.
  x <- x - mean(range(x))
  y <- y - mean(range(y))
  s <- max(diff(range(x)), diff(range(y)))
  list(x = x / s, y = y / s, id = id)
}

#' Butterflies pattern geometry for ggpattern / gridpattern
#'
#' Draws a staggered grid of butterflies clipped to the boundary polygon.
#' After `library(HeartFill)` use `pattern = "butterflies"` in any
#' `ggpattern` geom. Honours the same aesthetics as
#' [create_pattern_hearts()].
#'
#' @inheritParams create_pattern_hearts
#' @return A grob.
#' @export
create_pattern_butterflies <- function(params, boundary_df, aspect_ratio,
                                       legend = FALSE) {
  tile_shape_pattern(params, boundary_df, legend, butterfly_shape(), "butterflies")
}
