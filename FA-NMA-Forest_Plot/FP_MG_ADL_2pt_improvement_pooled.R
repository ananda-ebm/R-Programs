df <- openxlsx::read.xlsx("data.xlsx")

View(df)

for (i in 1:nrow(df)) {
  df$se[i] <- (df$CI_upper[i] - df$CI_lower[i]) / (2 * 1.96)
  
  if(df$time[i] <= 4){
    df$follow.up[i] <- "LEQ 4"
  }
  else if(df$time[i] <= 8){
    df$follow.up[i] <- "5-8"
  }
  else{
    df$follow.up[i] <- "9-26"
  }
}

library(meta)

result.df <- data.frame(
  follow.up = numeric(3),
  n.study = integer(3),
  pooled.ES = numeric(3),
  CI.lower = numeric(3),
  CI.upper = numeric(3)
)

for (i in 1:3) {
  
  dat <- df[((i - 1) * 5 + 1):(i * 5), ]
  
  result.df$follow.up[i] <- dat$follow.up[5]
  
  result.df$n.study[i] <- nrow(dat)
  
  dat$TE <- qlogis(dat$ES)
  dat$seTE <- dat$se / (dat$ES * (1 - dat$ES))
  
  m <- metagen(
    TE = TE,
    seTE = seTE,
    data = dat,
    sm = "PLOGIT",
    method.tau = "REML"
  )
  
  result.df$pooled.ES[i] <- plogis(m$TE.random)
  result.df$CI.lower[i] <- plogis(m$lower.random)
  result.df$CI.upper[i] <- plogis(m$upper.random)
}

result.df

library(tidyverse)
library(patchwork)

# Recode raw follow-up codes into readable labels
result.df$follow.up <- recode(
  result.df$follow.up,
  "LEQ 4" = "≤4 weeks",
  "5-8"   = "5–8 weeks",
  "9-26"  = "9–26 weeks"
)

# Preserve top-to-bottom order in the plot
result.df$follow.up <- factor(
  result.df$follow.up,
  levels = rev(c("≤4 weeks", "5–8 weeks", "9–26 weeks"))
)

# Label columns, formatted with fixed decimals
result.df$ES_label  <- sprintf("%.2f", result.df$pooled.ES)
result.df$CI_label  <- sprintf("(%.2f, %.2f)", result.df$CI.lower, result.df$CI.upper)

n <- nrow(result.df)

# Forest plot (left panel)

p_forest <- ggplot(result.df, aes(x = pooled.ES, y = follow.up)) +
  geom_errorbar(aes(xmin = CI.lower, xmax = CI.upper),
                height = 0,
                linewidth = 1.1, color = "grey50",
                orientation = "y") +
  geom_point(size = 3.2, shape = 15, color = "#3B5B92") +
  scale_x_continuous(
    breaks = seq(0, 1, 0.2),
    limits = c(0.15, 0.75), # tweak
    expand = c(0, 0)
  ) +
  labs(x = "Proportion with \u22652-point MG-ADL improvement in Placebo", y = NULL) +
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
    axis.title.x = element_text(size = 11, face = "bold", margin = margin(t = 10)),
    plot.margin = margin(t = 5, r = 4, b = 20, l = 5),
    plot.background  = element_blank(),
    panel.background = element_blank(),
    legend.background = element_blank(),
    legend.box.background = element_blank()
  )

# Table panel (right side): Follow-up | Studies | Pooled proportion | 95% CI

tbl <- data.frame(
  follow.up = rep(result.df$follow.up, 4),
  col = factor(
    rep(c("Follow-up", "Studies", "Pooled proportion", "95% CI"), each = n),
    levels = c("Follow-up", "Studies", "Pooled proportion", "95% CI")
  ),
  label = c(
    as.character(result.df$follow.up),
    as.character(result.df$n.study),
    result.df$ES_label,
    result.df$CI_label
  )
)

p_table <- ggplot(tbl, aes(x = col, y = follow.up, label = label)) +
  geom_text(size = 5, color = "black", hjust = 0.5) +   # increased from 3.6
  scale_x_discrete(position = "top") +
  coord_cartesian(ylim = c(0.5, n + 0.5), clip = "off") +
  theme_void(base_size = 11) +
  theme(
    axis.text.x.top = element_text(size = 13, face = "bold", color = "black",   # increased from 10.5
                                   margin = margin(b = 4)),
    plot.margin = margin(t = 5, r = 0, b = 10, l = 0)
  )

# Combine: forest plot (left) + table (right), rows aligned

final_plot <- p_forest + p_table +
  plot_layout(widths = c(1, 1.1)) + # tweak
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = "transparent", colour = NA),
      panel.background = element_rect(fill = "transparent", colour = NA)
    )
  )

final_plot

ggsave("FP_MG_ADL_pooled_followup.png",
       final_plot,
       width = 12,
       height = 4.5,
       dpi = 1000,
       bg = "transparent")