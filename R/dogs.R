#' Dog face outline coordinates
#'
#' A dog head with two floppy ears hanging at the sides, fitted to a unit
#' box centred on 0. The ears are separate sub-paths with a thin gap to the
#' head; the eyes and muzzle are holes, so the bar fill shows through, with a
#' filled nose inside the muzzle.
#'
#' @param face Logical, draw the eyes, muzzle and nose.
#' @return A list with numeric vectors `x`, `y` in `[-0.5, 0.5]` and
#'   sub-path `id`.
#' @export
dog_shape <- function(face = TRUE) {
  ell <- function(cx, cy, a, b, rot = 0, m = 60) {
    t <- seq(0, 2 * pi, length.out = m + 1)[-(m + 1)]
    r <- rot * pi / 180
    x <- a * cos(t)
    y <- b * sin(t)
    list(x = cx + x * cos(r) - y * sin(r), y = cy + x * sin(r) + y * cos(r))
  }
  parts <- list(
    ell(0, 0, 0.30, 0.36),                  # head
    ell(-0.43, 0.06, 0.10, 0.28, -18),      # left ear
    ell(0.43, 0.06, 0.10, 0.28, 18)         # right ear
  )
  if (face) {
    parts <- c(parts, list(
      ell(-0.12, 0.10, 0.045, 0.055),       # eyes
      ell(0.12, 0.10, 0.045, 0.055),
      ell(0, -0.14, 0.17, 0.13),            # muzzle (hole)
      ell(0, -0.09, 0.075, 0.05)            # nose (filled again inside muzzle)
    ))
  }
  x <- unlist(lapply(parts, `[[`, "x"))
  y <- unlist(lapply(parts, `[[`, "y"))
  id <- rep(seq_along(parts), lengths(lapply(parts, `[[`, "x")))
  # Centre on the head and ears' bounding box and scale the larger side to 1.
  outer <- id <= 3L
  mx <- mean(range(x[outer]))
  my <- mean(range(y[outer]))
  s <- max(diff(range(x[outer])), diff(range(y[outer])))
  list(x = (x - mx) / s, y = (y - my) / s, id = id)
}

#' Dogs pattern geometry for ggpattern / gridpattern
#'
#' Draws a staggered grid of dog faces clipped to the boundary polygon.
#' After `library(HeartFill)` use `pattern = "dogs"` in any `ggpattern`
#' geom. Honours the same aesthetics as [create_pattern_hearts()].
#'
#' @inheritParams create_pattern_hearts
#' @return A grob.
#' @export
create_pattern_dogs <- function(params, boundary_df, aspect_ratio,
                                legend = FALSE) {
  tile_shape_pattern(params, boundary_df, legend, dog_shape(), "dogs")
}
