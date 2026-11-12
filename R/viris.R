#' Viri mascot image, recoloured
#'
#' Viri is a hand-drawn icosahedral virus capsid with a face, arms and legs,
#' coloured with a yellow marker. This returns the drawing as an RGBA array
#' with the marker ink swapped for `colour`. The black outlines and the white
#' paper are kept: each pixel keeps its brightness, and its saturation decides
#' how much of `colour` it receives, so the marker's hatching survives.
#'
#' @param colour Any R colour for the marker ink, or `NULL`/`NA` to keep the
#'   original yellow.
#' @param alpha Opacity multiplier in `[0, 1]`.
#' @return A numeric array `[height, width, 4]` with values in `[0, 1]`,
#'   usable with [grid::rasterGrob()].
#' @export
viri_image <- function(colour = NULL, alpha = 1) {
  if (is.null(viri_cache$base)) {
    viri_cache$base <- png::readPNG(
      system.file("extdata", "viri.png", package = "heartfill", mustWork = TRUE)
    )
  }
  img <- viri_cache$base
  if (!is.null(colour) && !is.na(colour)) {
    key <- paste(grDevices::col2rgb(colour), collapse = ",")
    if (is.null(viri_cache[[key]])) viri_cache[[key]] <- recolour_ink(img, colour)
    img <- viri_cache[[key]]
  }
  if (alpha < 1) img[, , 4] <- img[, , 4] * max(0, alpha)
  img
}

#' Recolour the marker ink of a drawing
#'
#' Swaps the coloured ink of an image array for `colour`, as [viri_image()]
#' does for the packaged mascot. Each pixel keeps its brightness and its
#' saturation decides how much of `colour` it receives, so black outlines and
#' white paper are kept. Use it on the full-resolution drawing:
#' `png::writePNG(recolour_ink(png::readPNG("viri_yellow.png"), "#0072B2"), "viri_blue.png")`.
#'
#' With a very dark `colour` (e.g. black) the inked areas merge with the
#' outlines drawn on top of them.
#'
#' @param img Numeric array `[height, width, 3 or 4]` with values in `[0, 1]`,
#'   as returned by [png::readPNG()].
#' @param colour Any R colour for the ink.
#' @param ink_saturation Saturation of fully inked pixels in `img`.
#' @param paper_value Brightness of the blank paper in `img`.
#' @return An array like `img`.
#' @export
recolour_ink <- function(img, colour, ink_saturation = 0.8, paper_value = 0.95) {
  target <- grDevices::col2rgb(colour)[, 1] / 255
  mx <- pmax(img[, , 1], img[, , 2], img[, , 3])
  mn <- pmin(img[, , 1], img[, , 2], img[, , 3])
  ink <- pmin(1, ifelse(mx > 0, (mx - mn) / mx, 0) / ink_saturation)
  val <- pmin(1, mx / paper_value)
  for (k in 1:3) img[, , k] <- val * (1 - ink + ink * target[k])
  img
}

# Session cache: the decoded PNG plus one recoloured copy per colour.
viri_cache <- new.env(parent = emptyenv())

#' Viris pattern geometry for ggpattern / gridpattern
#'
#' Draws a staggered grid of Viri mascots (see [viri_image()]) clipped to the
#' boundary polygon. After `library(heartfill)` use `pattern = "viris"` in
#' any `ggpattern` geom.
#'
#' Unlike the shape patterns this one tiles an image. `pattern_fill` sets the
#' colour of the marker ink (`NA` keeps the original yellow); the outlines
#' stay black and `pattern_colour` is ignored. `pattern_spacing`,
#' `pattern_density`, `pattern_angle`, `pattern_alpha`, `pattern_xoffset` and
#' `pattern_yoffset` work as in [create_pattern_hearts()].
#'
#' @inheritParams create_pattern_hearts
#' @return A grob.
#' @export
create_pattern_viris <- function(params, boundary_df, aspect_ratio,
                                 legend = FALSE) {
  to_x <- function(v) grid::convertX(grid::unit(v, "npc"), "bigpts", valueOnly = TRUE)
  to_y <- function(v) grid::convertY(grid::unit(v, "npc"), "bigpts", valueOnly = TRUE)
  vp_w <- to_x(1)
  vp_h <- to_y(1)
  bx <- to_x(boundary_df$x)
  by <- to_y(boundary_df$y)

  img <- viri_image(params$pattern_fill, alpha_or_1(params$pattern_alpha))
  # Image box with its longer side scaled to 1, like the shape functions.
  iw <- ncol(img) / max(dim(img)[1:2])
  ih <- nrow(img) / max(dim(img)[1:2])

  if (legend) {
    # Legend keys are tiny: show one centred mascot instead of a grid.
    size <- 0.8 * min(diff(range(bx)) / iw, diff(range(by)) / ih)
    key <- grid::rasterGrob(img, mean(range(bx)), mean(range(by)),
                            width = size * iw, height = size * ih,
                            default.units = "bigpts")
    return(clip_to_boundary(key, bx, by, boundary_df$id, "viris",
                            params$pattern_res))
  }

  spacing <- params$pattern_spacing * min(vp_w, vp_h)
  size <- spacing * params$pattern_density
  if (!is.finite(size) || size <= 0) return(grid::nullGrob())

  # Staggered lattice in the pattern's own frame (u across, v up), rotated by
  # `pattern_angle` about the panel-anchored origin so bars line up.
  a <- (params$pattern_angle %||% 0) * pi / 180
  x0 <- params$pattern_xoffset * vp_w
  y0 <- params$pattern_yoffset * vp_h
  u <- (bx - x0) * cos(a) + (by - y0) * sin(a)
  v <- -(bx - x0) * sin(a) + (by - y0) * cos(a)
  gap <- spacing - size
  du <- size * iw + gap
  dv <- (size * ih + gap) * sqrt(3) / 2
  g <- expand.grid(col = seq(floor(min(u) / du) - 1, ceiling(max(u) / du) + 1),
                   row = seq(floor(min(v) / dv) - 1, ceiling(max(v) / dv) + 1))
  gu <- g$col * du + ifelse(g$row %% 2 == 0, 0, du / 2)
  gv <- g$row * dv
  keep <- gu > min(u) - size & gu < max(u) + size &
    gv > min(v) - size & gv < max(v) + size
  if (!any(keep)) return(grid::nullGrob())

  tiles <- grid::rasterGrob(
    img, gu[keep], gv[keep], width = size * iw, height = size * ih,
    default.units = "bigpts",
    vp = grid::viewport(x = x0, y = y0, width = vp_w, height = vp_h,
                        just = c("left", "bottom"), default.units = "bigpts",
                        angle = params$pattern_angle %||% 0)
  )
  clip_to_boundary(tiles, bx, by, boundary_df$id, "viris", params$pattern_res)
}
