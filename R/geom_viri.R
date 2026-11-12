#' Viri mascots as points
#'
#' A scatterplot geom that draws the Viri mascot (see [viri_image()]) at each
#' point instead of a dot. Works like [ggplot2::geom_point()].
#'
#' @section Aesthetics:
#' Requires `x` and `y`. Understands:
#' * `colour`: colour of the marker ink; the outlines stay black. `NA` (the
#'   default) keeps the original yellow.
#' * `size`: length of the mascot's longer side in mm (default 8). When
#'   mapping `size`, set a suitable range, e.g.
#'   `scale_size(range = c(5, 14))`, as the ggplot2 default range is meant
#'   for dots and gives very small mascots.
#' * `alpha`: transparency.
#' * `angle`: rotation in degrees anticlockwise.
#'
#' @inheritParams ggplot2::geom_point
#' @return A ggplot2 layer.
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(iris, aes(Sepal.Length, Sepal.Width, colour = Species)) +
#'   geom_viri(size = 7)
#' }
#' @export
geom_viri <- function(mapping = NULL, data = NULL, stat = "identity",
                      position = "identity", ..., na.rm = FALSE,
                      show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(
    geom = GeomViri, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' @rdname geom_viri
#' @format NULL
#' @usage NULL
#' @export
GeomViri <- ggplot2::ggproto(
  "GeomViri", ggplot2::Geom,
  required_aes = c("x", "y"),
  non_missing_aes = "size",
  default_aes = ggplot2::aes(colour = NA, size = 8, alpha = NA, angle = 0),

  draw_key = function(data, params, size) {
    grid::rasterGrob(viri_image(data$colour, alpha_or_1(data$alpha)),
                     0.5, 0.5, height = grid::unit(0.9, "npc"))
  },

  draw_panel = function(data, panel_params, coord) {
    if (nrow(data) == 0) return(grid::nullGrob())
    pts <- coord$transform(data, panel_params)
    # One recoloured image per colour/alpha; an unrotated group is a single
    # vectorised raster, rotated points each need their own viewport.
    key <- paste(pts$colour, pts$alpha, pts$angle)
    grobs <- lapply(split(seq_len(nrow(pts)), factor(key, unique(key))), function(i) {
      img <- viri_image(pts$colour[i[1]], alpha_or_1(pts$alpha[i[1]]))
      # Size is the longer side; give the matching width and let the height
      # follow.
      w <- pts$size[i] * ncol(img) / max(dim(img)[1:2])
      h <- pts$size[i] * nrow(img) / max(dim(img)[1:2])
      angle <- pts$angle[i[1]]
      if (angle %% 360 == 0) {
        return(list(grid::rasterGrob(
          img, grid::unit(pts$x[i], "npc"), grid::unit(pts$y[i], "npc"),
          width = grid::unit(w, "mm"), height = grid::unit(h, "mm")
        )))
      }
      lapply(seq_along(i), function(k) grid::rasterGrob(
        img, vp = grid::viewport(pts$x[i[k]], pts$y[i[k]],
                                 width = grid::unit(w[k], "mm"),
                                 height = grid::unit(h[k], "mm"), angle = angle)
      ))
    })
    do.call(grid::grobTree, unlist(grobs, recursive = FALSE, use.names = FALSE))
  }
)
