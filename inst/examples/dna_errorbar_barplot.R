# Random 5-bar barplot with DNA double-helix error bars.
library(ggplot2)
library(HeartFill)

set.seed(sample.int(1e6, 1))
df <- data.frame(
  group = factor(LETTERS[1:5]),
  value = round(runif(5, 6, 18), 1),
  se = round(runif(5, 1, 4), 1)
)

p <- ggplot(df, aes(group, value)) +
  geom_col(fill = "#dcdbd7", width = 0.7) +
  geom_errorbar_dna(aes(ymin = value - se, ymax = value + se),
                    colour = "#52514e", width = 0.14, linewidth = 0.7) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Random values with DNA error bars", x = NULL, y = "Value") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

out <- if (length(commandArgs(TRUE))) commandArgs(TRUE)[1] else "dna_errorbar_barplot.png"
ggsave(out, p, width = 7, height = 5, dpi = 150, bg = "white")
message("wrote ", out)
