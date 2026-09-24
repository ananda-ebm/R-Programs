library(tidyverse)
library(patchwork)

df <- data.frame(comparison = rep("VAY300QM vs. Telitacicept 160 mg", 2),
                 timepoint = c("24 week", "48 week"),
                 RR = c(1.06, 1.23),
                 lower = c(0.84, 0.92),
                 upper = c(1.38, 1.79))

df$Treatment <- df$comparison
df$timepoint <- factor(df$timepoint, levels = c("24 week", "48 week"))
df$RowID <- paste(df$Treatment, df$timepoint, sep = " | ")
df$RowID <- factor(df$RowID, levels = rev(unique(df$RowID)))
df$RR_label  <- sprintf("%.2f", df$RR)
df$CrI_label <- sprintf("(%.2f, %.2f)", df$lower, df$upper)

timepoint_colors <- c("24 week" = "#1F77B4", "48 week" = "#D62728")

n <- nrow(df)

xlim_low  <- 0.5  # tweak
xlim_high <- 2.0  # tweak

# ---- Forest plot (middle panel) ----

p_forest <- ggplot(df, aes(x = RR, y = RowID, colour = timepoint)) +
  geom_segment(
    aes(x = lower, xend = upper, y = RowID, yend = RowID, colour = timepoint),
    linewidth = 1.1, lineend = "round"
  ) +
  geom_point(size = 3.2, shape = 15) +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "black",
             linewidth = 1) +
  annotate("text", x = 1, y = -0.15, label = "\u2190 Favours placebo",
           hjust = 1.05, vjust = 2.5, size = 5.0, color = "black", fontface = "bold") +
  annotate("text", x = 1, y = -0.15, label = "Favours treatment \u2192",
           hjust = -0.05, vjust = 2.5, size = 5.0, color = "black", fontface = "bold") +
  scale_x_continuous(
    breaks = seq(xlim_low, xlim_high, 0.25), # tweak
    limits = c(xlim_low - 0.05, xlim_high + 0.05), # tweak
    expand = c(0, 0)
  ) +
  scale_colour_manual(values = timepoint_colors, name = NULL,
                      guide = guide_legend(nrow = 1)) +
  scale_y_discrete(limits = levels(df$RowID), drop = FALSE) +
  labs(x = NULL, y = NULL) +
  coord_cartesian(ylim = c(0.5, n + 0.5), clip = "off") +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey85", linewidth = 0.4),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.line.x = element_line(),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 16), vjust = -3),
    plot.margin = margin(t = 5, r = 4, b = 20, l = 10),
    plot.background  = element_blank(),
    panel.background = element_blank(),
    legend.text = element_text(size = 14),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.margin = margin(t = 25, r = 0, b = 0, l = 0)
  )

# ---- Table panels: Treatment (left) | forest plot (middle) | Risk Ratio, CrI (95%) (right) ----

tbl_left <- data.frame(
  RowID = df$RowID,
  col = factor("Treatment", levels = "Treatment"),
  label = as.character(df$Treatment)
)

p_table_left <- ggplot(tbl_left, aes(x = col, y = RowID, label = label)) +
  geom_text(size = 5.5, color = "black", hjust = 0) +
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits = levels(df$RowID), drop = FALSE) +
  coord_cartesian(ylim = c(0.5, n + 0.5), clip = "off") +
  theme_void(base_size = 11) +
  theme(
    axis.text.x.top = element_blank(),
    plot.margin = margin(t = 5, r = 130, b = 10, l = 0)
  )

tbl_right <- data.frame(
  RowID = rep(df$RowID, 2),
  timepoint = rep(df$timepoint, 2),
  col = factor(rep(c("Risk Ratio", "CrI (95%)"), each = n),
               levels = c("Risk Ratio", "CrI (95%)")),
  label = c(df$RR_label, df$CrI_label)
)

tbl_right$text_color <- timepoint_colors[as.character(tbl_right$timepoint)]

p_table_right <- ggplot(tbl_right, aes(x = col, y = RowID, label = label)) +
  geom_text(aes(colour = text_color), size = 5.5, hjust = 0.5, fontface = "bold") +
  scale_colour_identity() +
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits = levels(df$RowID), drop = FALSE) +
  coord_cartesian(ylim = c(0.5, n + 0.5), clip = "off") +
  theme_void(base_size = 11) +
  theme(
    axis.text.x.top = element_text(size = 12.5, face = "bold", color = "black",
                                   margin = margin(b = 4), vjust = -20),
    plot.margin = margin(t = 5, r = 0, b = 10, l = 0)
  )

# ---- Combine: Treatment table (left) + forest plot (middle) + RR/CrI table (right) ----

final_plot <- p_table_left + p_forest + p_table_right +
  plot_layout(widths = c(0.4, 1, 0.5), guides = "collect") + # tweak
  plot_annotation(
    theme = theme(
      legend.position = "top",
      legend.direction = "horizontal",
      legend.text = element_text(size = 14),
      legend.margin = margin(t = 25, r = 0, b = 0, l = 0),
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

ggsave("FP07.png",
       final_plot,
       width = 14,
       height = 6,
       dpi = 2000,
       bg = "transparent")
