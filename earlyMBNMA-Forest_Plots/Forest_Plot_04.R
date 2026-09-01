library(tidyverse)
library(patchwork)

# suitably use setwd()
df <- openxlsx::read.xlsx("Table_04_MG_ADL_score_improvement_from_baseline.xlsx")

# Preserve top-to-bottom order in the plot
df$Treatment <- factor(df$Treatment, levels = rev(df$Treatment))

# Label column combining MD and CrI, formatted with fixed decimals
df$MD_label  <- sprintf("%.2f", df$MD)
df$CrI_label <- sprintf("(%.2f, %.2f)", df$lower, df$upper)

# ---- Shared theme elements for row alignment ----
n <- nrow(df)

# ---- Forest plot (left panel)

p_forest <- ggplot(df, aes(x = MD, y = Treatment)) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0, linewidth = 1.1, color = "grey50") +
  geom_point(size = 3.2, shape = 15, color = "#3B5B92") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "black",
             linewidth = 1) +
  annotate("text", x = 0, y = -0.15, label = "\u2190 Favours treatment",
           hjust = 1.05, vjust = 3, size = 3.3, color = "black") +
  annotate("text", x = 0, y = -0.15, label = "Favours placebo \u2192",
           hjust = -0.05, vjust = 3, size = 3.3, color = "black") +
  scale_x_continuous(
    breaks = c(-3.5, -2.5, -1.5, -0.5, 0),
    limits = c(-4, 0.5), # tweak
    expand = c(0, 0)
  ) +
  labs(x = "Mean Difference", y = NULL) +
  coord_cartesian(ylim = c(0.5, n + 0.5), clip = "off") +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    # panel.grid.major.y = element_line(color = "grey92", linewidth = 0.5),
    # panel.grid.major.x = element_line(color = "grey92", linewidth = 0.5),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 9.5, color = "black"),
    axis.line.x = element_line(),
    axis.title.x = element_text(size = 11, face = "bold", margin = margin(t = 16)),
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
  geom_text(size = 3.6, color = "black", hjust = 0.5) +
  scale_x_discrete(position = "top") +
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
ggsave("FP_04_MG_ADL_score_improvement_from_baseline.png", 
       final_plot,
       width = 12, 
       height = 5.5, 
       dpi = 1000, 
       bg = "transparent")
