#' DNA error bars
#'
#' Error bars drawn as a DNA double helix running from `ymin` to `ymax`: two
#' intertwined strands joined by base-pair rungs. Each bar starts and ends at
#' the helix's full width with a rung, which acts as the cap, so the ends
#' mark `ymin` and `ymax` exactly. Works like [ggplot2::geom_errorbar()],
#' including with `position_dodge()` and `coord_flip()`.
#'
#' The helix is built in physical units when the plot is drawn, so it keeps
#' its proportions whatever the bar length or plot size. The number of
#' half-turns is rounded to a whole number per bar, so the actual pitch varies
#' slightly around `pitch`.
#'
#' @section Aesthetics:
#' Requires `x`, `ymin` and `ymax`. Understands `colour` (first strand and end
#' caps), `colour2` (second strand; by default a lighter tint of `colour`),
#' `linewidth`, `alpha` and `width`.
#'
#' @inheritParams ggplot2::geom_errorbar
#' @param width Helix width in x-axis units (as the cap width of
#'   `geom_errorbar()`).
#' @param pitch Length of one full helix turn, as a multiple of the helix
#'   width. Real B-DNA is about 1.7.
#' @param rungs Number of base-pair rungs per half-turn.
#' @param bases Colour the rungs as base pairs? `TRUE` uses a built-in A/T/G/C
#'   palette, `FALSE` draws them in the strand colour; or give four colours
#'   for A, T, G and C.
#' @return A ggplot2 layer.
#' @examples
#' \dontrun{
#' library(ggplot2)
#' df <- data.frame(group = c("A", "B", "C"), value = c(5, 9, 3), se = c(1, 2, 0.8))
#' ggplot(df, aes(group, value)) +
#'   geom_col(fill = "grey85") +
#'   geom_errorbar_dna(aes(ymin = value - se, ymax = value + se), width = 0.15)
#' }
#' @export
geom_errorbar_dna <- function(mapping = NULL, data = NULL, stat = "identity",
                              position = "identity", ..., width = 0.2,
                              pitch = 1.7, rungs = 4, bases = TRUE,
                              na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE) {
  ggplot2::layer(
    geom = GeomErrorbarDna, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(width = width, pitch = pitch, rungs = rungs, bases = bases,
                  na.rm = na.rm, ...)
  )
}

#' @rdname geom_errorbar_dna
#' @format NULL
#' @usage NULL
#' @export
GeomErrorbarDna <- ggplot2::ggproto(
  "GeomErrorbarDna", ggplot2::Geom,
  required_aes = c("x", "ymin", "ymax"),
  optional_aes = "colour2",
  default_aes = ggplot2::aes(colour = "grey20", linewidth = 0.6, alpha = NA),
  extra_params = c("na.rm", "width"),
  draw_key = ggplot2::draw_key_vpath,

  setup_data = function(data, params) {
    w <- data$width %||% params$width %||% (ggplot2::resolution(data$x, FALSE) * 0.2)
    data$xmin <- data$x - w / 2
    data$xmax <- data$x + w / 2
    data$width <- NULL
    data
  },

  draw_panel = function(data, panel_params, coord, pitch = 1.7, rungs = 4,
                        bases = TRUE) {
    if (nrow(data) == 0) return(grid::nullGrob())
    # Three reference points per bar, transformed separately so the helix
    # follows the bar under any coord (e.g. coord_flip()).
    tr <- function(x, y) {
      coord$transform(data.frame(x = x, y = y), panel_params)[c("x", "y")]
    }
    col1 <- ggplot2::alpha(data$colour, data$alpha)
    col2 <- if (is.null(data$colour2)) lighten(data$colour) else data$colour2
    grid::gTree(
      p0 = tr(data$x, data$ymin), p1 = tr(data$x, data$ymax),
      edge = tr(data$xmax, data$ymin),
      col1 = col1, col2 = ggplot2::alpha(col2, data$alpha),
      lwd = data$linewidth * ggplot2::.pt,
      pitch = pitch, rungs = max(1L, as.integer(rungs)),
      bases = base_palette(bases),
      cl = "dna_errorbar"
    )
  }
)

# Build the helices once the panel's physical size is known.
#' @importFrom grid makeContent
#' @export
makeContent.dna_errorbar <- function(x) {
  mm_x <- function(v) grid::convertX(grid::unit(v, "npc"), "mm", valueOnly = TRUE)
  mm_y <- function(v) grid::convertY(grid::unit(v, "npc"), "mm", valueOnly = TRUE)
  x0 <- mm_x(x$p0$x); y0 <- mm_y(x$p0$y)
  x1 <- mm_x(x$p1$x); y1 <- mm_y(x$p1$y)
  hw <- sqrt((mm_x(x$edge$x) - x0)^2 + (mm_y(x$edge$y) - y0)^2)
  len <- sqrt((x1 - x0)^2 + (y1 - y0)^2)

  back <- front <- rung <- list()
  for (i in which(is.finite(len) & len > 0 & is.finite(hw) & hw > 0)) {
    ax <- c(x1[i] - x0[i], y1[i] - y0[i]) / len[i]   # along the bar
    px <- c(ax[2], -ax[1])                           # across it
    n <- max(1L, round(len[i] / (x$pitch * 2 * hw[i] / 2)))   # half-turns
    at <- function(t, side) {
      off <- side * hw[i] * cos(pi * n * t)
      list(x = x0[i] + ax[1] * len[i] * t + px[1] * off,
           y = y0[i] + ax[2] * len[i] * t + px[2] * off)
    }
    # Strands cross once per half-turn; which one is in front alternates.
    for (k in seq_len(n) - 1L) {
      t <- seq(k / n, (k + 1) / n, length.out = 25)
      s1 <- c(at(t, 1), col = x$col1[i], lwd = x$lwd[i])
      s2 <- c(at(t, -1), col = x$col2[i], lwd = x$lwd[i])
      if (k %% 2 == 0) {
        front <- c(front, list(s1)); back <- c(back, list(s2))
      } else {
        front <- c(front, list(s2)); back <- c(back, list(s1))
      }
    }
    # Rungs, skipping those too close to a crossing to be visible.
    j <- 0:(n * x$rungs)
    j <- j[abs(cos(pi * j / x$rungs)) > 0.2]
    a <- at(j / (n * x$rungs), 1)
    b <- at(j / (n * x$rungs), -1)
    mid <- list(x = (a$x + b$x) / 2, y = (a$y + b$y) / 2)
    cap <- j == 0 | j == n * x$rungs
    if (is.null(x$bases)) {
      ca <- cb <- rep(x$col1[i], length(j))
    } else {
      # Deterministic pseudo-sequence; partner halves are complementary
      # (A-T, G-C).
      base <- (j * 7 + i * 3) %% 4 + 1
      ca <- ifelse(cap, x$col1[i], x$bases[base])
      cb <- ifelse(cap, x$col1[i], x$bases[c(2, 1, 4, 3)[base]])
    }
    rung <- c(rung, list(data.frame(
      x0 = c(a$x, mid$x), y0 = c(a$y, mid$y),
      x1 = c(mid$x, b$x), y1 = c(mid$y, b$y),
      col = c(ca, cb), lwd = ifelse(c(cap, cap), 1, 0.8) * x$lwd[i]
    )))
  }
  if (length(front) == 0) return(grid::setChildren(x, grid::gList()))

  strands <- function(parts) {
    grid::polylineGrob(
      unlist(lapply(parts, `[[`, "x")), unlist(lapply(parts, `[[`, "y")),
      id.lengths = lengths(lapply(parts, `[[`, "x")), default.units = "mm",
      gp = grid::gpar(col = vapply(parts, `[[`, "", "col"),
                      lwd = vapply(parts, `[[`, 0, "lwd"), lineend = "round")
    )
  }
  rung <- do.call(rbind, rung)
  rungs <- grid::segmentsGrob(rung$x0, rung$y0, rung$x1, rung$y1,
                              default.units = "mm",
                              gp = grid::gpar(col = rung$col, lwd = rung$lwd,
                                              lineend = "butt"))
  grid::setChildren(x, grid::gList(strands(back), rungs, strands(front)))
}

# Colours for A, T, G, C rungs, or NULL to use the strand colour.
base_palette <- function(bases) {
  if (isTRUE(bases)) return(c("#1baf7a", "#e34948", "#eda100", "#2a78d6"))
  if (isFALSE(bases) || is.null(bases)) return(NULL)
  stopifnot("`bases` must be TRUE, FALSE or four colours (A, T, G, C)" =
              is.character(bases) && length(bases) == 4)
  unname(bases)
}

# Tint colours towards white, keeping their alpha.
lighten <- function(col, by = 0.55) {
  m <- grDevices::col2rgb(col, alpha = TRUE) / 255
  grDevices::rgb(m[1, ] + (1 - m[1, ]) * by, m[2, ] + (1 - m[2, ]) * by,
                 m[3, ] + (1 - m[3, ]) * by, m[4, ])
}
