# Packages
library(tidyverse)
library(scales)
library(ggrepel)

# Data -------------------------------------------------------------------------
# .rds files should be read with readRDS(), not load()
load("data.rds")

plot_df <- data %>%
  transmute(
    date = as.Date(paste0(year, "-01-01")),
    `Charge d'intérêts nominale (% du PIB)` = charge_interets / PIB,
    `Charge d'intérêts réelle (% du PIB)` = (charge_interets - taxe_inflationniste) / PIB
  ) %>%
  pivot_longer(-date, names_to = "variable", values_to = "value") %>%
  arrange(date) %>%
  mutate(
    variable = factor(
      variable,
      levels = c("Charge d'intérêts nominale (% du PIB)", "Charge d'intérêts réelle (% du PIB)")
    )
  )

last_points <- plot_df %>%
  group_by(variable) %>%
  slice_max(date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(label = label_percent(accuracy = 0.1)(value))

# Plot -------------------------------------------------------------------------
ggplot(plot_df, aes(x = date, y = value, color = variable)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = 0.9) +
  scale_x_date(
    date_labels = "%Y",
    breaks = seq(as.Date("1980-01-01"), as.Date("2100-01-01"), by = "2 years"),
    expand = expansion(mult = c(0.01, 0.08)) # make room on the right for labels
  ) +
  scale_color_manual(
    NULL,
    values = c(
      "Charge d'intérêts nominale (% du PIB)" = "#3B82F6", # blue
      "Charge d'intérêts réelle (% du PIB)"   = "#10B981"  # green
    )
  ) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    breaks = pretty_breaks(8)
  ) +
  geom_label(
    data = last_points,
    aes(label = label),
    nudge_x = 200, direction = "y",
    label.size = 0, fill = alpha("white", 0.8),
    show.legend = FALSE, box.padding = 0.5, point.padding = 0.5
  ) +
  labs(
    title = "Charge d’intérêt de la dette publique : nominale vs. réelle",
    x = NULL, y = "% du PIB", color = NULL,
    caption = "Source : Insee, calculs de l’auteur"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = c(0.5, 0.2),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    plot.title = element_text(face = "bold")
  )



ggsave("figure2.png", width = 1.25*6, height = 1.25*3.375, bg = "white")
ggsave("figure2.pdf", width = 1.25*6, height = 1.25*3.375)

