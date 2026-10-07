# Random 4-bar barplot, each bar filled with its own text at its own angle.
library(ggplot2)
library(ggpattern)
library(heartfill)

set.seed(sample.int(1e6, 1))
df <- data.frame(
  group = factor(c("A", "B", "C", "D")),
  value = round(runif(4, 5, 20), 1)
)

# text_pattern() registers each text/angle pair and returns its pattern name.
fills <- c(A = text_pattern("hello", 0),
           B = text_pattern("heartfill", 30),
           C = text_pattern("R", 90, fontface = "bold"),
           D = text_pattern("ggplot2", -45, fontface = "italic"))

# Validated categorical palette (slots 1-4), fixed order.
cols <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100")

p <- ggplot(df, aes(group, value)) +
  geom_col_pattern(
    aes(pattern = group, pattern_fill = group),
    fill = "#fcfcfb",
    colour = "#52514e",
    linewidth = 0.4,
    pattern_spacing = 0.04,
    pattern_density = 0.7,
    width = 0.7
  ) +
  scale_pattern_manual(values = fills, name = "Group") +
  scale_pattern_fill_manual(values = cols, name = "Group") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Random values, filled with text", x = NULL, y = "Value") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key.size = unit(1.2, "cm"))

out <- if (length(commandArgs(TRUE))) commandArgs(TRUE)[1] else "text_barplot.png"
ggsave(out, p, width = 7, height = 5, dpi = 150, bg = "white")
message("wrote ", out)
