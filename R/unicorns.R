#' Unicorn head outline coordinates
#'
#' A unicorn head and neck in profile (facing right) with a pointed ear,
#' fitted to a unit box centred on 0. The horn and the scalloped mane are
#' separate sub-paths with a thin gap to the head; the eye and nostril are
#' holes, so the bar fill shows through.
#'
#' @param mane Logical, draw the mane along the back of the neck.
#' @param face Logical, cut out the eye and nostril.
#' @param m Number of vertices per curved segment.
#' @return A list with numeric vectors `x`, `y` in `[-0.5, 0.5]` and
#'   sub-path `id`.
#' @export
unicorn_shape <- function(mane = TRUE, face = TRUE, m = 24) {
  ell <- function(cx, cy, a, b, rot = 0, n = 40) {
    t <- seq(0, 2 * pi, length.out = n + 1)[-(n + 1)]
    r <- rot * pi / 180
    x <- a * cos(t)
    y <- b * sin(t)
    list(x = cx + x * cos(r) - y * sin(r), y = cy + x * sin(r) + y * cos(r))
  }
  # Quadratic Bezier from p0 to p2 pulled towards p1 (last point dropped so
  # consecutive segments join without duplicates).
  bez <- function(p0, p1, p2, t = seq(0, 1, length.out = m + 1)[-(m + 1)]) {
    list(x = (1 - t)^2 * p0[1] + 2 * (1 - t) * t * p1[1] + t^2 * p2[1],
         y = (1 - t)^2 * p0[2] + 2 * (1 - t) * t * p1[2] + t^2 * p2[2])
  }
  join <- function(...) {
    s <- list(...)
    list(x = unlist(lapply(s, `[[`, "x")), y = unlist(lapply(s, `[[`, "y")))
  }
  # Back of the neck, shared by the head outline and the mane.
  poll <- c(-0.22, 0.62)
  back_ctrl <- c(-0.62, 0.30)
  withers <- c(-0.58, -0.90)
  head <- join(
    bez(c(0.02, -0.90), c(0.00, -0.45), c(0.30, -0.25)),  # throat
    bez(c(0.30, -0.25), c(0.55, -0.12), c(0.72, -0.16)),  # jaw and chin
    bez(c(0.72, -0.16), c(0.92, -0.10), c(0.80, 0.12)),   # muzzle
    bez(c(0.80, 0.12), c(0.45, 0.30), c(0.12, 0.56)),     # bridge of nose
    bez(c(0.12, 0.56), c(0.06, 0.80), c(-0.06, 0.98)),    # ear, front
    bez(c(-0.06, 0.98), c(-0.12, 0.78), poll),            # ear, back
    bez(poll, back_ctrl, withers)                         # back of neck
  )
  horn <- list(x = c(0.14, 0.30, 0.64), y = c(0.645, 0.48, 1.14))
  parts <- list(head, horn)

  if (mane) {
    # Offset the back-of-neck curve outwards: a gap, then scalloped tufts.
    t <- seq(0.04, 0.80, length.out = 4 * m)
    p <- bez(poll, back_ctrl, withers, t)
    dx <- 2 * (1 - t) * (back_ctrl[1] - poll[1]) + 2 * t * (withers[1] - back_ctrl[1])
    dy <- 2 * (1 - t) * (back_ctrl[2] - poll[2]) + 2 * t * (withers[2] - back_ctrl[2])
    len <- sqrt(dx^2 + dy^2)
    nx <- dy / len
    ny <- -dx / len
    s <- (t - min(t)) / diff(range(t))
    inner <- 0.05
    outer <- inner + 0.08 + 0.10 * abs(sin(4 * pi * s))
    parts <- c(parts, list(list(
      x = c(p$x + nx * inner, rev(p$x + nx * outer)),
      y = c(p$y + ny * inner, rev(p$y + ny * outer))
    )))
  }
  if (face) {
    parts <- c(parts, list(
      ell(0.20, 0.32, 0.06, 0.045, -20),   # eye
      ell(0.70, 0.01, 0.035, 0.05, 30)     # nostril
    ))
  }
  x <- unlist(lapply(parts, `[[`, "x"))
  y <- unlist(lapply(parts, `[[`, "y"))
  id <- rep(seq_along(parts), lengths(lapply(parts, `[[`, "x")))
  # Centre the bounding box and scale the larger side to 1.
  x <- x - mean(range(x))
  y <- y - mean(range(y))
  s <- max(diff(range(x)), diff(range(y)))
  list(x = x / s, y = y / s, id = id)
}

#' Unicorns pattern geometry for ggpattern / gridpattern
#'
#' Draws a staggered grid of unicorn heads clipped to the boundary polygon.
#' After `library(heartfill)` use `pattern = "unicorns"` in any `ggpattern`
#' geom. Honours the same aesthetics as [create_pattern_hearts()].
#'
#' @inheritParams create_pattern_hearts
#' @return A grob.
#' @export
create_pattern_unicorns <- function(params, boundary_df, aspect_ratio,
                                    legend = FALSE) {
  tile_shape_pattern(params, boundary_df, legend, unicorn_shape(), "unicorns")
}
