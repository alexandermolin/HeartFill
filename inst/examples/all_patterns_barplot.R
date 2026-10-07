# One bar per heartfill pattern, each in its own colour.
library(ggplot2)
library(ggpattern)
library(heartfill)

set.seed(sample.int(1e6, 1))
patterns <- c("hearts", "stars", "flowers", "butterflies", "cats", "dogs",
              "unicorns", "sugarbeets", "computers")
df <- data.frame(
  pattern = factor(patterns, levels = patterns),
  value = round(runif(9, 5, 20), 1),
  # Wide shapes (butterflies, dogs) and tall ones (unicorns, sugar beets)
  # need more room per tile.
  spacing = c(0.06, 0.06, 0.06, 0.07, 0.06, 0.09, 0.07, 0.08, 0.07),
  density = c(0.75, 0.75, 0.75, 0.8, 0.75, 0.85, 0.8, 0.85, 0.8)
)

# Validated categorical palette (slots 1-8), fixed order; the palette has
# only 8 hues, so the 9th bar uses neutral ink (shape and axis label carry
# identity).
cols <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300",
          "#4a3aa7", "#e34948", "#52514e")

p <- ggplot(df, aes(pattern, value)) +
  geom_col_pattern(
    aes(pattern = pattern, pattern_fill = pattern,
        pattern_spacing = spacing, pattern_density = density),
    fill = "#fcfcfb",
    colour = "#52514e",
    linewidth = 0.4,
    pattern_colour = NA,
    pattern_angle = 0,
    width = 0.75
  ) +
  scale_pattern_manual(values = setNames(levels(df$pattern), levels(df$pattern)),
                       name = "Pattern") +
  scale_pattern_fill_manual(values = cols, name = "Pattern") +
  scale_pattern_spacing_identity() +
  scale_pattern_density_identity() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(title = "heartfill: every pattern", x = NULL, y = "Value") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(1.2, "cm"))

out <- if (length(commandArgs(TRUE))) commandArgs(TRUE)[1] else "all_patterns_barplot.png"
ggsave(out, p, width = 11, height = 5.5, dpi = 150, bg = "white")
message("wrote ", out)
