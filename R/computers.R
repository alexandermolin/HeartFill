#' Computer outline coordinates
#'
#' A desktop computer: a monitor on a stand with a keyboard in front, fitted
#' to a unit box centred on 0. The stand and keyboard are separate sub-paths
#' with a thin gap to the monitor. The screen is a hole, so the bar fill
#' shows through, with a few filled lines of "text" on it; the keyboard keys
#' are holes too.
#'
#' @param screen Logical, cut out the screen (with text lines).
#' @param keyboard Logical, draw the keyboard.
#' @return A list with numeric vectors `x`, `y` in `[-0.5, 0.5]` and
#'   sub-path `id`.
#' @export
computer_shape <- function(screen = TRUE, keyboard = TRUE) {
  # Rectangle with rounded corners (radius r), traced anticlockwise.
  rrect <- function(x0, y0, x1, y1, r = 0, m = 8) {
    if (r <= 0) return(list(x = c(x0, x1, x1, x0), y = c(y0, y0, y1, y1)))
    arc <- function(cx, cy, from) {
      t <- (from + seq(0, 90, length.out = m)) * pi / 180
      list(x = cx + r * cos(t), y = cy + r * sin(t))
    }
    a <- list(arc(x1 - r, y0 + r, -90), arc(x1 - r, y1 - r, 0),
              arc(x0 + r, y1 - r, 90), arc(x0 + r, y0 + r, 180))
    list(x = unlist(lapply(a, `[[`, "x")), y = unlist(lapply(a, `[[`, "y")))
  }
  parts <- list(
    rrect(-0.50, -0.05, 0.50, 0.60, 0.05),                    # monitor
    list(x = c(-0.07, 0.07, 0.10, 0.20, 0.20, -0.20, -0.20, -0.10),
         y = c(-0.09, -0.09, -0.20, -0.21, -0.25, -0.25, -0.21, -0.20))  # stand
  )
  if (screen) {
    parts <- c(parts, list(
      rrect(-0.42, 0.03, 0.42, 0.52, 0.02),                   # screen (hole)
      rrect(-0.36, 0.41, 0.06, 0.45),                         # text lines
      rrect(-0.36, 0.31, 0.24, 0.35),
      rrect(-0.30, 0.21, 0.12, 0.25),
      rrect(-0.36, 0.11, -0.12, 0.15)
    ))
  }
  if (keyboard) {
    parts <- c(parts, list(rrect(-0.46, -0.44, 0.46, -0.29, 0.02)))
    # Two rows of keys, cut out as holes.
    for (yb in c(-0.335, -0.39)) {
      for (x in seq(-0.41, 0.35, length.out = 9)) {
        parts <- c(parts, list(rrect(x, yb, x + 0.06, yb + 0.035)))
      }
    }
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

#' Computers pattern geometry for ggpattern / gridpattern
#'
#' Draws a staggered grid of desktop computers clipped to the boundary
#' polygon. After `library(HeartFill)` use `pattern = "computers"` in any
#' `ggpattern` geom. Honours the same aesthetics as
#' [create_pattern_hearts()].
#'
#' @inheritParams create_pattern_hearts
#' @return A grob.
#' @export
create_pattern_computers <- function(params, boundary_df, aspect_ratio,
                                     legend = FALSE) {
  tile_shape_pattern(params, boundary_df, legend, computer_shape(), "computers")
}
