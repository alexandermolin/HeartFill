#' Cat face outline coordinates
#'
#' A cat head with two pointed ears, fitted to a unit box centred on 0. The
#' eyes and nose are holes (extra sub-paths), so the bar fill shows through.
#'
#' @param eyes Logical, cut out the eyes and nose.
#' @param n Number of vertices on the head outline.
#' @return A list with numeric vectors `x`, `y` in `[-0.5, 0.5]` and
#'   sub-path `id`.
#' @export
cat_shape <- function(eyes = TRUE, n = 120) {
  ell <- function(cx, cy, a, b, from = 0, to = 360, m = 40) {
    t <- seq(from, to, length.out = m) * pi / 180
    list(x = cx + a * cos(t), y = cy + b * sin(t))
  }
  # Head ellipse walked anticlockwise; the arcs under each ear are replaced
  # by straight edges to the ear tip, giving one simple outline.
  a <- 0.5
  b <- 0.42
  seg <- function(from, to) ell(0, 0, a, b, from, to, max(2, round(n * (to - from) / 360)))
  pt <- function(deg) c(a * cos(deg * pi / 180), b * sin(deg * pi / 180))
  s1 <- seg(-210, 25)            # left cheek, chin, right cheek
  s2 <- seg(85, 95)              # forehead between the ears
  tip_r <- c(0.40, 0.66)
  tip_l <- c(-0.40, 0.66)
  hx <- c(s1$x, tip_r[1], s2$x, tip_l[1])
  hy <- c(s1$y, tip_r[2], s2$y, tip_l[2])
  parts <- list(list(x = hx, y = hy))

  if (eyes) {
    parts <- c(parts, list(
      ell(-0.19, 0.04, 0.065, 0.11),
      ell(0.19, 0.04, 0.065, 0.11),
      list(x = c(-0.07, 0.07, 0), y = c(-0.10, -0.10, -0.19))  # nose
    ))
  }
  x <- unlist(lapply(parts, `[[`, "x"))
  y <- unlist(lapply(parts, `[[`, "y"))
  id <- rep(seq_along(parts), lengths(lapply(parts, `[[`, "x")))
  # Centre on the head outline's bounding box and scale the larger side to 1.
  outer <- id == 1L
  mx <- mean(range(x[outer]))
  my <- mean(range(y[outer]))
  s <- max(diff(range(x[outer])), diff(range(y[outer])))
  list(x = (x - mx) / s, y = (y - my) / s, id = id)
}

#' Cats pattern geometry for ggpattern / gridpattern
#'
#' Draws a staggered grid of cat faces clipped to the boundary polygon.
#' After `library(heartfill)` use `pattern = "cats"` in any `ggpattern`
#' geom. Honours the same aesthetics as [create_pattern_hearts()].
#'
#' @inheritParams create_pattern_hearts
#' @return A grob.
#' @export
create_pattern_cats <- function(params, boundary_df, aspect_ratio,
                                legend = FALSE) {
  tile_shape_pattern(params, boundary_df, legend, cat_shape(), "cats")
}
