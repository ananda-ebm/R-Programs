library(tidyverse)
library(patchwork)

# suitably use setwd()
df <- openxlsx::read.xlsx("Table_02_MG_ADL_score_change_from_baseline.xlsx")

# Preserve top-to-bottom order in the plot
df$Treatment <- factor(df$Treatment, levels = rev(df$Treatment))

# Label column combining MD and CrI, formatted with fixed decimals
df$MD_label  <- sprintf("%.2f", df$MD)
df$CrI_label <- sprintf("(%.2f, %.2f)", df$lower, df$upper)

# ---- Shared theme elements for row alignment ----
n <- nrow(df)

# ---- Axis limits & arrow logic ----
# Any CI that extends past these limits is truncated and gets an arrowhead
# on the truncated end(s) instead of running off the plot.
xlim_low  <- -7
xlim_high <- 1.2

df <- df %>%
  mutate(
    left_clip  = lower < xlim_low,
    right_clip = upper > xlim_high,
    seg_x      = ifelse(left_clip, xlim_low, lower),
    seg_xend   = ifelse(right_clip, xlim_high, upper)
  )

seg_none  <- filter(df, !left_clip & !right_clip)
seg_left  <- filter(df,  left_clip & !right_clip)
seg_right <- filter(df, !left_clip &  right_clip)
seg_both  <- filter(df,  left_clip &  right_clip)

arrow_spec <- function(ends) {
  arrow(angle = 30, length = unit(0.09, "inches"), ends = ends, type = "closed")
}

# ---- Forest plot (left panel)

p_forest <- ggplot(df, aes(x = MD, y = Treatment)) +
  # CI segments split by truncation status, each with the correct arrow ends
  geom_segment(
    data = seg_none,
    aes(x = seg_x, xend = seg_xend, y = Treatment, yend = Treatment),
    color = "grey50", linewidth = 1.1, lineend = "round"
  ) +
  geom_segment(
    data = seg_left,
    aes(x = seg_x, xend = seg_xend, y = Treatment, yend = Treatment),
    color = "grey50", linewidth = 1.1, lineend = "round", arrow = arrow_spec("first")
  ) +
  geom_segment(
    data = seg_right,
    aes(x = seg_x, xend = seg_xend, y = Treatment, yend = Treatment),
    color = "grey50", linewidth = 1.1, lineend = "round", arrow = arrow_spec("last")
  ) +
  geom_segment(
    data = seg_both,
    aes(x = seg_x, xend = seg_xend, y = Treatment, yend = Treatment),
    color = "grey50", linewidth = 1.1, lineend = "round", arrow = arrow_spec("both")
  ) +
  geom_point(size = 3.2, shape = 15, colour = "#3B5B92") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "black",
             linewidth = 1) +
  annotate("text", x = 0, y = -0.15, label = "\u2190 Favours treatment",
           hjust = 1.05, vjust = 3, size = 4.3, color = "black") +
  annotate("text", x = 0, y = -0.15, label = "Favours placebo \u2192",
           hjust = -0.05, vjust = 3, size = 4.3, color = "black") +
  scale_x_continuous(
    breaks = seq(xlim_low, xlim_high, 1),
    limits = c(xlim_low, xlim_high + 0.5), # tweak
    expand = c(0, 0)
  ) +
  # Lock the y-axis order explicitly. Without this, splitting the CI lines
  # into several geom_segment() layers (one per clip scenario) can cause
  # ggplot to reorder the discrete y-axis based on which layer first
  # introduces each treatment, since each layer only holds a subset of rows.
  scale_y_discrete(limits = levels(df$Treatment), drop = FALSE) +
  labs(x = "Time-course Adjusted Results at Week 24", y = NULL) +
  coord_cartesian(ylim = c(0.5, n + 0.5), clip = "off") +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.line.x = element_line(),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 16), vjust = -3),
    plot.margin = margin(t = 5, r = 4, b = 20, l = 5),
    plot.background  = element_blank(),
    panel.background = element_blank(),
    legend.background = element_blank(),
    legend.box.background = element_blank()
  )

# ---- Table panel (right side): Treatment | Mean Difference | CrI (95%) ----
# Build a long-format data frame so all three "columns" share the same y-axis
# and therefore stay row-aligned with the forest plot via patchwork.

tbl <- data.frame(
  Treatment = rep(df$Treatment, 3),
  col = factor(rep(c("Treatment", "Mean Difference", "CrI (95%)"), each = n),
               levels = c("Treatment", "Mean Difference", "CrI (95%)")),
  label = c(as.character(df$Treatment), df$MD_label, df$CrI_label)
)

p_table <- ggplot(tbl, aes(x = col, y = Treatment, label = label)) +
  geom_text(size = 4.0, color = "black", hjust = 0.5) +
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits = levels(df$Treatment), drop = FALSE) +
  coord_cartesian(ylim = c(0.5, n + 0.5), clip = "off") +
  theme_void(base_size = 11) +
  theme(
    axis.text.x.top = element_text(size = 10.5, face = "bold", color = "black",
                                   margin = margin(b = 4)),
    plot.margin = margin(t = 5, r = 0, b = 10, l = 15)
  )

# ---- Combine: forest plot (left) + table (right), rows aligned ----
final_plot <- p_forest + p_table +
  plot_layout(widths = c(1, 0.75)) + # tweak
  plot_annotation(
    theme = theme(
      plot.background = element_rect(
        fill = "transparent",
        colour = NA
      ),
      panel.background = element_rect(
        fill = "transparent",
        colour = NA
      )
    )
  )

final_plot

# suitably use setwd()
ggsave("FP_02_MG_ADL_score_change_from_baseline.png",
       final_plot,
       width = 12,
       height = 5.5,
       dpi = 1000,
       bg = "transparent")