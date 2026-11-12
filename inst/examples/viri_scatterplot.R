# Random scatterplot with Viri mascots as points, one ink colour per group.
library(ggplot2)
library(heartfill)

set.seed(sample.int(1e6, 1))
n <- 14
df <- data.frame(
  group = factor(rep(c("A", "B", "C"), each = n)),
  x = c(rnorm(n, 3, 1), rnorm(n, 6, 1.1), rnorm(n, 8.5, 0.9))
)
df$y <- 2 + 0.9 * df$x + rnorm(nrow(df), 0, 1.3)

# Validated categorical palette (slots 1-3), fixed order.
viri_cols <- c("#2a78d6", "#eb6834", "#1baf7a")

p <- ggplot(df, aes(x, y, colour = group)) +
  geom_viri(size = 9) +
  scale_colour_manual(values = viri_cols, name = "Group") +
  labs(title = "Random values, drawn with Viris", x = "x", y = "y") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        legend.key.size = unit(1.2, "cm"))

out <- if (length(commandArgs(TRUE))) commandArgs(TRUE)[1] else "viri_scatterplot.png"
ggsave(out, p, width = 7, height = 5, dpi = 150, bg = "white")
message("wrote ", out)
