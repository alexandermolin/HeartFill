# Recolour a Viri drawing in each colour of the Okabe-Ito palette.
# Usage: Rscript viri_okabe_ito.R [input.png] [output_dir]
# Without an input the packaged (480 px) drawing is used.
library(heartfill)

args <- commandArgs(TRUE)
input <- if (length(args) >= 1) args[1] else
  system.file("extdata", "viri.png", package = "heartfill", mustWork = TRUE)
outdir <- if (length(args) >= 2) args[2] else "viri_okabe_ito"

okabe_ito <- c(orange = "#E69F00", skyblue = "#56B4E9", bluishgreen = "#009E73",
               yellow = "#F0E442", blue = "#0072B2", vermillion = "#D55E00",
               reddishpurple = "#CC79A7", black = "#000000")

img <- png::readPNG(input)
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
for (nm in names(okabe_ito)) {
  out <- file.path(outdir, sprintf("viri_%s.png", nm))
  png::writePNG(recolour_ink(img, okabe_ito[[nm]]), out)
  message("wrote ", out)
}
