
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
load("data.rds")

# Helper for French number formatting (Md€)
lab_md_eur <- label_number(accuracy = 1, big.mark = " ", decimal.mark = ",", suffix = " Md€")

plot_df <- data %>%
  transmute(
    date = as.Date(paste0(year, "-01-01")),
    `Charge d'intérêts nominale (Md€)` = charge_interets,
    `Charge d'intérêts réelle (Md€)`   = charge_interets - taxe_inflationniste
  ) %>%
  pivot_longer(-date, names_to = "variable", values_to = "value") %>%
  mutate(
    variable = factor(
      variable,
      levels = c("Charge d'intérêts nominale (Md€)", "Charge d'intérêts réelle (Md€)")
    )
  )

# Keep last points for end labels
last_pts <- plot_df %>%
  group_by(variable) %>%
  filter(date == max(date, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(label = paste0(number(value, accuracy = 1, big.mark = " ", decimal.mark = ","), " Md€"))

ggplot(plot_df, aes(date, value, color = variable)) +
  # zero line
  geom_hline(yintercept = 0, linetype = "dashed") +
  # main lines
  geom_line(linewidth = 1.05) +
  # end dots
  geom_point(data = last_pts, size = 2.2) +
  # end labels slightly to the right
  geom_label(
    data = last_pts,
    aes(label = label),
    hjust = -0.15, vjust = 0.5, size = 3.6, fontface = "bold",
    show.legend = FALSE
  ) +
  # scales
  scale_x_date(breaks = as.Date(paste0(seq(1978, 2100, 2), "-01-01")),
               date_labels = "%Y",
               expand = expansion(mult = c(0.01, 0.15))) +
  scale_y_continuous(labels = lab_md_eur, breaks = pretty_breaks(n = 10)) +
  scale_color_manual(
    NULL,
    values = c(
      "Charge d'intérêts nominale (Md€)" = "#3B82F6", # blue
      "Charge d'intérêts réelle (Md€)"   = "#10B981"  # green
    )
  ) +
  # titles (edit as needed)
  labs(
    title = "Charge d’intérêt de la dette publique : nominale vs réelle",
    subtitle = "La charge d'intérêt « réelle » déduit l’effet d’érosion par l’inflation (taxe inflationniste).",
    x = NULL, y = NULL,
    caption = NULL
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.position = c(0.3, 0.2),
    plot.margin = margin(10, 30, 10, 10) # room for end labels
  )



ggsave("figure2b.png", width = 1.25*6, height = 1.25*3.375, bg = "white")
ggsave("figure2b.pdf", width = 1.25*6, height = 1.25*3.375)


