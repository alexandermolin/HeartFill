#' Sugar beet outline coordinates
#'
#' A sugar beet: a conical root tapering to a thin tail, topped by a fan of
#' leaves, fitted to a unit box centred on 0. The leaves are separate
#' sub-paths with a thin gap to the root; the leaf midribs and the growth
#' rings on the root are holes, so the bar fill shows through.
#'
#' @param leaves Number of leaves (1 to 5).
#' @param details Logical, cut out the leaf midribs and root rings.
#' @param m Number of vertices per curved segment.
#' @return A list with numeric vectors `x`, `y` in `[-0.5, 0.5]` and
#'   sub-path `id`.
#' @export
sugarbeet_shape <- function(leaves = 3, details = TRUE, m = 30) {
  leaves <- max(1L, min(5L, as.integer(leaves)))
  # Quadratic Bezier from p0 to p2 pulled towards p1.
  bez <- function(p0, p1, p2, t = seq(0, 1, length.out = m)) {
    list(x = (1 - t)^2 * p0[1] + 2 * (1 - t) * t * p1[1] + t^2 * p2[1],
         y = (1 - t)^2 * p0[2] + 2 * (1 - t) * t * p1[2] + t^2 * p2[2])
  }
  rot <- function(p, deg, ox, oy) {
    r <- deg * pi / 180
    list(x = ox + p$x * cos(r) - p$y * sin(r),
         y = oy + p$x * sin(r) + p$y * cos(r))
  }
  # Root: domed crown, shoulders curving down to a thin tail.
  crown_t <- seq(0, pi, length.out = m)
  shoulder <- bez(c(0.38, 0.10), c(0.47, 0.02), c(0.44, -0.26))
  taper <- bez(c(0.44, -0.26), c(0.38, -0.66), c(0.012, -0.80))
  right <- list(x = c(shoulder$x, taper$x[-1]), y = c(shoulder$y, taper$y[-1]))
  k <- length(right$x)
  root <- list(
    x = c(0.38 * cos(crown_t), -right$x[-1], rev(right$x[-c(1, k)])),
    y = c(0.10 + 0.09 * sin(crown_t), right$y[-1], rev(right$y[-c(1, k)]))
  )
  parts <- list(root)

  # Leaves: pointed ovals along the x axis, rotated into a fan above the
  # crown. Bases sit on an arc so neighbouring leaves do not overlap.
  leaf <- function(len, wid) {
    t <- seq(0, 1, length.out = m)
    w <- wid * sin(pi * t^0.75)
    list(x = c(len * t, rev(len * t)), y = c(w, rev(-w)))
  }
  angles <- if (leaves == 1) 90 else seq(90 + 17 * (leaves - 1), 90 - 17 * (leaves - 1), length.out = leaves)
  for (a in angles) {
    len <- 0.62 - 0.004 * abs(a - 90)
    r <- a * pi / 180
    parts <- c(parts, list(rot(leaf(len, 0.12), a,
                               0.15 * cos(r), 0.08 + 0.15 * sin(r))))
  }

  if (details) {
    # Thin sliver: a crescent between two nearby arcs.
    sliver <- function(p0, p1, p2, bulge) {
      a <- bez(p0, p1, p2)
      b <- bez(p0, c(p1[1], p1[2] + bulge), p2)
      list(x = c(a$x, rev(b$x)), y = c(a$y, rev(b$y)))
    }
    for (a in angles) {
      len <- 0.62 - 0.004 * abs(a - 90)
      r <- a * pi / 180
      rib <- sliver(c(0.12 * len, 0), c(0.5 * len, 0.02), c(0.85 * len, 0), 0.05)
      parts <- c(parts, list(rot(rib, a, 0.15 * cos(r), 0.08 + 0.15 * sin(r))))
    }
    parts <- c(parts, list(
      sliver(c(-0.30, -0.10), c(0, -0.19), c(0.30, -0.10), 0.05),
      sliver(c(-0.24, -0.36), c(0, -0.44), c(0.24, -0.36), 0.045)
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

#' Sugar beets pattern geometry for ggpattern / gridpattern
#'
#' Draws a staggered grid of sugar beets clipped to the boundary polygon.
#' After `library(HeartFill)` use `pattern = "sugarbeets"` in any
#' `ggpattern` geom. Honours the same aesthetics as
#' [create_pattern_hearts()].
#'
#' @inheritParams create_pattern_hearts
#' @return A grob.
#' @export
create_pattern_sugarbeets <- function(params, boundary_df, aspect_ratio,
                                      legend = FALSE) {
  tile_shape_pattern(params, boundary_df, legend, sugarbeet_shape(), "sugarbeets")
}
