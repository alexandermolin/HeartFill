# Shared engine: tile any unit-box polygon over a boundary and clip to it.
# `shape` is a list(x, y) with coordinates in [-0.5, 0.5], plus an optional
# `id` splitting it into sub-paths; overlaps are cut out (even-odd rule).
tile_shape_pattern <- function(params, boundary_df, legend, shape, name) {
  # Work in physical units so shapes are never squashed by panel aspect.
  to_x <- function(v) grid::convertX(grid::unit(v, "npc"), "bigpts", valueOnly = TRUE)
  to_y <- function(v) grid::convertY(grid::unit(v, "npc"), "bigpts", valueOnly = TRUE)
  vp_w <- to_x(1)
  vp_h <- to_y(1)
  bx <- to_x(boundary_df$x)
  by <- to_y(boundary_df$y)

  spacing <- params$pattern_spacing * min(vp_w, vp_h)
  size <- spacing * params$pattern_density

  # Staggered (hex-like) grid anchored to the panel so bars line up. The
  # lattice follows the shape's proportions while keeping the same gap
  # between shapes in both directions; for a square shape the steps are
  # `spacing` across and `spacing * sqrt(3) / 2` between rows.
  x0 <- params$pattern_xoffset * vp_w
  y0 <- params$pattern_yoffset * vp_h
  gap <- spacing - size
  dx <- size * diff(range(shape$x)) + gap
  dy <- (size * diff(range(shape$y)) + gap) * sqrt(3) / 2
  rows <- seq(floor((min(by) - y0) / dy) - 1, ceiling((max(by) - y0) / dy) + 1)
  cols <- seq(floor((min(bx) - x0) / dx) - 1, ceiling((max(bx) - x0) / dx) + 1)
  g <- expand.grid(col = cols, row = rows)
  cx <- x0 + g$col * dx + ifelse(g$row %% 2 == 0, 0, dx / 2)
  cy <- y0 + g$row * dy

  # Drop centres that cannot touch the boundary box.
  keep <- cx > min(bx) - size & cx < max(bx) + size &
    cy > min(by) - size & cy < max(by) + size
  cx <- cx[keep]
  cy <- cy[keep]

  # Legend keys are tiny: show one centred shape instead of a grid.
  if (legend) {
    cx <- mean(range(bx))
    cy <- mean(range(by))
    size <- 0.8 * min(diff(range(bx)), diff(range(by)))
  }
  if (length(cx) == 0) return(grid::nullGrob())

  a <- params$pattern_angle * pi / 180
  sx <- (shape$x * cos(a) - shape$y * sin(a)) * size
  sy <- (shape$x * sin(a) + shape$y * cos(a)) * size
  nv <- length(sx)

  fill <- grDevices::adjustcolor(params$pattern_fill, alpha.f = alpha_or_1(params$pattern_alpha))
  col <- params$pattern_colour
  col <- if (is.null(col) || is.na(col)) NA else
    grDevices::adjustcolor(col, alpha.f = alpha_or_1(params$pattern_alpha))

  sub <- shape$id %||% rep(1L, nv)
  shapes <- grid::pathGrob(
    x = rep(cx, each = nv) + rep(sx, length(cx)),
    y = rep(cy, each = nv) + rep(sy, length(cx)),
    id = rep(sub, length(cx)),
    pathId = rep(seq_along(cx), each = nv),
    rule = "evenodd",
    default.units = "bigpts",
    gp = grid::gpar(fill = fill, col = col,
                    lwd = params$pattern_linewidth %||% 0.5)
  )
  clip_to_boundary(shapes, bx, by, boundary_df$id, name, params$pattern_res)
}

# Mask a grob (in bigpts) to the boundary polygon.
clip_to_boundary <- function(grob, bx, by, id, name, res) {
  masker <- grid::polygonGrob(bx, by, id = id, default.units = "bigpts",
                              gp = grid::gpar(fill = "white", col = NA))
  gridpattern::alphaMaskGrob(grob, masker, name = name, res = res)
}

alpha_or_1 <- function(a) if (is.null(a) || is.na(a)) 1 else a

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a
