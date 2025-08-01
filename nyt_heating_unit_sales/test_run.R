library(tidyverse)
library(ggtext)
library(scales)

sales <- read_csv("data/sales.csv")

chart_cols <- c("#6da393", "#999999")

long_sales <- sales |>
  pivot_longer(cols = 2:4, names_to = "group", values_to = "sales") |>
  mutate(
    chart_grouping = ifelse(group == "heat_pump", "heat_pump", "gas_oil")
  ) |>
  group_by(year, chart_grouping) |>
  summarise(total = sum(sales))

most_recent <- long_sales |>
  filter(year == 2024) |>
  mutate(
    short_amount = label_comma(.1, scale = 1e-6, suffix = " million")(total),
    cat_label = case_when(
      chart_grouping == "gas_oil" ~ "<strong>Gas and oil furnaces</strong>",
      chart_grouping == "heat_pump" ~ "<strong>Heat pumps</strong>",
      TRUE ~ "ERROR"
    ),
    full_label = paste(cat_label, short_amount, sep = "<br>")
  )


long_sales |>
  ggplot(aes(year, total, color = chart_grouping, fill = chart_grouping)) +
  geom_line(linewidth = 1) +
  geom_point(data = most_recent, size = 3.5, shape = 21, color = "white") +
  geom_textbox(
    data = most_recent,
    aes(label = full_label),
    family = "Libre Franklin",
    fill = NA,
    halign = 0,
    box.size = 0,
    hjust = 0
  ) +
  scale_color_manual(values = rev(chart_cols)) +
  scale_fill_manual(values = rev(chart_cols)) +
  scale_y_continuous(
    breaks = c(1:4) * 1e6,
    limits = c(500000, NA),
    labels = c("1", "2", "3", "4 million")
  ) +
  scale_x_continuous(
    breaks = c(seq(from = 2005, to = 2020, by = 5), 2024),
    expand = expansion(add = c(0, 0))
  ) +
  coord_cartesian(clip = "off", xlim = c(NA, 2024)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_line(),
    axis.ticks.length = unit(2, "mm"),
    axis.line.x = element_line(),
    axis.text.y = element_markdown(
      hjust = 0,
      margin = margin(r = -30),
      fill = "white",
      padding = unit(1, "mm")
    ),
    axis.text.x = element_text(
      hjust = c(0, rep(.5, 4)),
      margin = margin(t = 7)
    ),
    legend.position = "none",
    plot.margin = margin(r = 120, t = 10, l = 10, b = 10),
    text = element_text(family = "Libre Franklin"),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = 16),
    plot.caption = element_text(hjust = 0),
    plot.caption.position = "plot"
  ) +
  labs(
    y = "",
    x = "",
    title = "Heating units sold in the U.S.",
    caption = "Source: AHRI"
  )
