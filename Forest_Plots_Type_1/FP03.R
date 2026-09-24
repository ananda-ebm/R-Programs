RR_table <- data.frame(Comparison = c("Telitacicept 160 mg vs. PBO", 
                                      "VAY300QM vs. PBO", 
                                      "VAY300QM vs. Telitacicept 160 mg"),
                       RR = c(1.47, 1.08, 0.78),
                       Lower = c(0.82, 0.90, 0.56),
                       Upper = c(1.78, 1.23, 1.36))

df1 <- data.frame(comparison = RR_table$Comparison,
                  RR = RR_table$RR,
                  lower = RR_table$Lower,
                  upper = RR_table$Upper)

df2 <- df1 %>%
  mutate(
    Significant = dplyr::if_else(lower < 1 & upper < 1, "Yes", "No"),
    Label = sprintf("%.2f (%.2f, %.2f)", RR, lower, upper)
  ) %>%
  mutate(comparison = factor(comparison, levels = unique(comparison)))
# Preserve the order of treatments as they appear in the data

forest_plot <- df2 %>% ggplot(
  aes(x = RR,
      y = comparison,
      xmin = lower,
      xmax = upper,
      colour = Significant)) +
  geom_errorbar(orientation = "y", width = 0.25, linewidth = 0.9, colour = "#2166AC") +
  geom_point(size = 4, shape = 18, colour = "#2166AC") +
  geom_vline(xintercept = 1, linetype = "dashed", 
             colour = "grey40", linewidth = 0.7) +
  geom_text(aes(x = upper, label = Label),
            hjust = -0.08, size = 4.5, colour = "grey20") +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.35))) +
  labs(x = NULL, y = NULL) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(colour = "grey40", size = 10),
        plot.caption = element_text(colour = "grey50", size = 9),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 11))

forest_plot

ggsave("FP03.png",
       forest_plot,
       width = 11.5,
       height = 5.5,
       dpi = 2000,
       bg = "white")