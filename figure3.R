
library(tidyverse)

load("data.rds")
library(tidyverse)
library(ggrepel)  # for non-overlapping end labels

# ——— Prep ———
plt <- data %>%
  transmute(
    date = as.Date(paste0(year, "-01-01")),
    `Solde public (% du PIB)` = deficit_PIB/100,
    `Solde public corrigé de la « taxe inflationniste » (% du PIB)` = deficit_PIB/100 + taxe_inflationniste/PIB
  ) %>%
  pivot_longer(-date, names_to = "variable", values_to = "value") %>%
  mutate(variable = factor(
    variable,
    levels = c("Solde public (% du PIB)",
               "Solde public corrigé de la « taxe inflationniste » (% du PIB)")
  ))

# End-of-series data for labels
last_pts <- plt %>%
  group_by(variable) %>%
  filter(date == max(date)) %>%
  ungroup()

# ——— Plot ———
ggplot(plt, aes(x = date, y = value, color = variable)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = -0.03, linetype = "dotted") +
  geom_line(linewidth = 1) +
  # end labels
  geom_point(data = last_pts, size = 2) +
  ggrepel::geom_label_repel(
    data = last_pts,
    aes(label = scales::label_percent(accuracy = 0.1, decimal.mark = ",")(value)),
    nudge_x = 200,  # pushes labels slightly to the right
    direction = "y",
    min.segment.length = 0,
    segment.alpha = 0.5,
    box.padding = 0.25,
    label.size = 0
  ) +
  scale_x_date(
    limits = c(as.Date("1980-01-01"), NA),     # start at 1980
    date_breaks = "2 years",
    date_labels = "%Y",
    expand = expansion(mult = c(0, .05))       # small right margin for labels
  ) +
  scale_y_continuous(
    breaks = seq(-0.10, 0.10, by = 0.01),
    labels = scales::label_percent(accuracy = 1, decimal.mark = ",")
  ) +
  scale_color_manual(
    NULL,
    values = c(
      "Solde public (% du PIB)" = "#1f77b4",
      "Solde public corrigé de la « taxe inflationniste » (% du PIB)" = "#d62728"
    )
  ) +
  labs(
    x = NULL, y = NULL,
    caption = "Sources : Insee, calculs de l’auteur",
    subtitle = "Ligne pointillée : seuil de -3 % (Maastricht)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    legend.justification = "left",
    plot.subtitle = element_text(margin = margin(b = 6)),
    plot.caption = element_text(hjust = 0),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


ggsave("figure3.png", width = 1.25*6, height = 1.25*3.375, bg = "white")
ggsave("figure3.pdf", width = 1.25*6, height = 1.25*3.375)


library(tidyverse)
library(ggrepel)

# --- Prep ---
plt <- data %>%
  transmute(
    date = as.Date(paste0(year, "-01-01")),
    `Déficit public (% du PIB)` = -deficit_PIB/100,
    `Déficit public corrigé de la « taxe inflationniste » (% du PIB)` = -deficit_PIB/100 - taxe_inflationniste/PIB
  ) %>%
  pivot_longer(-date, names_to = "variable", values_to = "value") %>%
  mutate(
    variable = factor(
      variable,
      levels = c(
        "Déficit public (% du PIB)",
        "Déficit public corrigé de la « taxe inflationniste » (% du PIB)"
      )
    )
  )

last_pts <- plt %>%
  group_by(variable) %>%
  filter(date == max(date)) %>%
  ungroup()

# --- Plot ---
ggplot(plt, aes(date, value, color = variable)) +
  geom_hline(yintercept = 0, linetype = "dashed") +          # équilibre
  geom_hline(yintercept = 0.03, linetype = "dotted") +       # seuil Maastricht +3%
  geom_line(linewidth = 1) +
  geom_point(data = last_pts, size = 2) +
  ggrepel::geom_label_repel(
    data = last_pts,
    aes(label = scales::label_percent(accuracy = 0.1, decimal.mark = ",")(value)),
    nudge_x = 200,
    direction = "y",
    min.segment.length = 0,
    segment.alpha = 0.5,
    box.padding = 0.25,
    label.size = 0
  ) +
  scale_x_date(
    limits = c(as.Date("1980-01-01"), NA),
    date_breaks = "2 years",
    date_labels = "%Y",
    expand = expansion(mult = c(0, .05))
  ) +
  scale_y_continuous(
    breaks = seq(-0.1, 0.15, by = 0.01),
    labels = scales::label_percent(accuracy = 1, decimal.mark = ",")
  ) +
  scale_color_manual(
    NULL,
    values = c(
      "Déficit public (% du PIB)" = "#1f77b4",
      "Déficit public corrigé de la « taxe inflationniste » (% du PIB)" = "#d62728"
    )
  ) +
  labs(
    x = NULL, y = NULL,
    subtitle = "Pointillés : seuil de +3 % (Maastricht) ; tirets : équilibre",
    caption = "Sources : Insee, calculs de l’auteur"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    legend.justification = "left",
    plot.subtitle = element_text(margin = margin(b = 6)),
    plot.caption = element_text(hjust = 0),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


ggsave("figure3b.png", width = 1.25*6, height = 1.25*3.375, bg = "white")
ggsave("figure3b.pdf", width = 1.25*6, height = 1.25*3.375)
