# Random 5-bar barplot filled with dogs, one dog colour per bar.
library(ggplot2)
library(ggpattern)
library(HeartFill)

set.seed(sample.int(1e6, 1))
df <- data.frame(
  group = factor(LETTERS[1:5]),
  value = round(runif(5, 3, 20), 1)
)

# Validated categorical palette (slots 1-5), fixed order.
dog_cols <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4")

p <- ggplot(df, aes(group, value)) +
  geom_col_pattern(
    aes(pattern_fill = group),
    pattern = "dogs",
    fill = "#fcfcfb",
    colour = "#52514e",
    linewidth = 0.4,
    pattern_colour = NA,
    pattern_spacing = 0.09,
    pattern_density = 0.85,
    pattern_angle = 0,
    width = 0.7
  ) +
  scale_pattern_fill_manual(values = dog_cols, name = "Group") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Random values, filled with dogs", x = NULL, y = "Value") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(1.2, "cm"))

out <- if (length(commandArgs(TRUE))) commandArgs(TRUE)[1] else "dog_barplot.png"
ggsave(out, p, width = 7, height = 5, dpi = 150, bg = "white")
message("wrote ", out)
