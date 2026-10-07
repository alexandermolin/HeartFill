#' Text fill pattern
#'
#' Registers a pattern that fills the area with repeated `text`, written in
#' staggered rows at `angle` degrees, and returns its name for use as
#' `pattern = ` in any `ggpattern` geom:
#'
#' ```r
#' geom_col_pattern(pattern = text_pattern("hello", 45), ...)
#' ```
#'
#' The text colour is `pattern_fill` (with `pattern_alpha`). Row spacing is
#' `pattern_spacing` and the text height is `pattern_density` times that
#' spacing, as for the shape patterns. The pattern's own `angle` is used, so
#' `pattern_angle` is ignored. Registration lasts for the R session; call
#' `text_pattern()` again before redrawing a saved plot in a new session.
#'
#' @param text A single string to repeat.
#' @param angle Angle of the text and its rows, in degrees anticlockwise.
#' @param fontface Font face: `"plain"`, `"bold"`, `"italic"` or
#'   `"bold.italic"`.
#' @param family Font family (`""` for the device default).
#' @param name Pattern name to register under; by default built from `text`
#'   and `angle`, e.g. `"text:hello@45"`.
#' @return The pattern name (a string).
#' @export
text_pattern <- function(text, angle = 0, fontface = "plain", family = "",
                         name = NULL) {
  stopifnot(is.character(text), length(text) == 1, nzchar(text),
            is.numeric(angle), length(angle) == 1, is.finite(angle))
  name <- name %||% sprintf("text:%s@%g", text, angle)
  fun <- function(params, boundary_df, aspect_ratio, legend = FALSE) {
    tile_text_pattern(params, boundary_df, legend, text, angle, fontface, family)
  }
  current <- getOption("ggpattern_geometry_funcs", list())
  current[[name]] <- fun
  options(ggpattern_geometry_funcs = current)
  name
}

# Tile `text` in staggered rows running at `angle`, clipped to the boundary.
tile_text_pattern <- function(params, boundary_df, legend, text, angle,
                              fontface, family) {
  to_x <- function(v) grid::convertX(grid::unit(v, "npc"), "bigpts", valueOnly = TRUE)
  to_y <- function(v) grid::convertY(grid::unit(v, "npc"), "bigpts", valueOnly = TRUE)
  vp_w <- to_x(1)
  vp_h <- to_y(1)
  bx <- to_x(boundary_df$x)
  by <- to_y(boundary_df$y)

  spacing <- params$pattern_spacing * min(vp_w, vp_h)
  fs <- spacing * params$pattern_density
  if (!is.finite(fs) || fs <= 0) return(grid::nullGrob())
  col <- grDevices::adjustcolor(params$pattern_fill,
                                alpha.f = alpha_or_1(params$pattern_alpha))
  font_gp <- function(size) grid::gpar(fontsize = size, fontface = fontface,
                                       fontfamily = family, col = col)
  # Text width per unit of font size, measured at the drawing size.
  w1 <- grid::convertWidth(grid::grobWidth(grid::textGrob(text, gp = font_gp(fs))),
                           "bigpts", valueOnly = TRUE) / fs
  a <- angle * pi / 180

  if (legend) {
    # One string, as large as fits the key once rotated.
    ext_w <- w1 * abs(cos(a)) + abs(sin(a))
    ext_h <- w1 * abs(sin(a)) + abs(cos(a))
    fs <- 0.8 * min(diff(range(bx)) / ext_w, diff(range(by)) / ext_h)
    cx <- mean(range(bx))
    cy <- mean(range(by))
  } else {
    # Lattice in the text's own frame: u along the text, v across rows.
    du <- w1 * fs + 0.6 * fs
    dv <- spacing
    x0 <- params$pattern_xoffset * vp_w
    y0 <- params$pattern_yoffset * vp_h
    u <- (bx - x0) * cos(a) + (by - y0) * sin(a)
    v <- -(bx - x0) * sin(a) + (by - y0) * cos(a)
    g <- expand.grid(col = seq(floor(min(u) / du) - 1, ceiling(max(u) / du) + 1),
                     row = seq(floor(min(v) / dv) - 1, ceiling(max(v) / dv) + 1))
    gu <- g$col * du + ifelse(g$row %% 2 == 0, 0, du / 2)
    gv <- g$row * dv
    cx <- x0 + gu * cos(a) - gv * sin(a)
    cy <- y0 + gu * sin(a) + gv * cos(a)
  }
  texts <- grid::textGrob(rep(text, length(cx)), cx, cy, rot = angle,
                          default.units = "bigpts", gp = font_gp(fs))
  clip_to_boundary(texts, bx, by, boundary_df$id, "text", params$pattern_res)
}
