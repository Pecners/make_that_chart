library(tidyverse)
library(readxl)
library(lubridate)
library(glue)
library(scales)
library(ggtext)
library(glue)

# source: https://data.bls.gov/timeseries/ces0000000001?output_view=net_1mth
jobs <- read_xlsx("data/july_jobs.xlsx", skip = 11)

long_jobs <- jobs |>
  pivot_longer(cols = -1, names_to = "month", values_to = "jobs") |>
  mutate(ym = ym(glue("{Year}-{month}"))) |>
  filter(ym >= ymd("2024-07-01") & !is.na(jobs)) |>
  mutate(xind = row_number())


ylabs <- tibble(
  y = c(1:3) * 100,
) |>
  mutate(lab = label_comma(scale = 1e3, prefix = "+")(y))

down <- tibble(
  x = c(11:12),
  y = c(144, 147)
)

update_geom_defaults(geom = "text", list(family = "Libre Franklin"))
update_geom_defaults(geom = "label", list(family = "Libre Franklin"))


long_jobs |>
  ggplot(aes(
    x = xind,
    y = jobs,
    fill = ifelse(xind == 13, "#FE8918", "#BDBDBD")
  )) +

  geom_col(
    data = down,
    inherit.aes = FALSE,
    aes(x, y),
    color = "#BDBDBD",
    fill = NA
  ) +

  annotate(
    geom = "segment",
    x = 13,
    xend = 13,
    y = 50,
    yend = 240,
    linewidth = .4
  ) +

  geom_col() +

  geom_text(
    data = ylabs,
    inherit.aes = FALSE,
    aes(
      x = -.25,
      y = c(100, 200, 300),
      label = c("+100,000", "+200,000", "+300,000")
    ),
    size = 3,
    vjust = -0.5,
    hjust = 0,
    color = "grey40"
  ) +

  annotate(
    geom = "text",
    label = "+73,000 jobs\nin July",
    x = 13.5,
    y = 275,
    hjust = 1,
    lineheight = 1
  ) +

  annotate(
    geom = "label",
    label = "Growth in May and\nJune was lower than\ninitially expected.",
    label.size = 0,
    lineheight = 1,
    hjust = 0,
    x = 7.5,
    y = 200
  ) +

  annotate(
    geom = "curve",
    x = 11,
    xend = 11.5,
    y = 180,
    yend = 160,
    curvature = -.45,
    linewidth = .4
  ) +

  annotate(
    geom = "label",
    label = "REVISED\nDOWN",
    fill = alpha("white", .75),
    lineheight = 1,
    x = 11.5,
    y = 90,
    label.size = 0,
    label.padding = unit(0, "mm"),
    fontface = "italic",
    color = "grey40"
  ) +

  scale_fill_identity() +

  scale_y_continuous(
    expand = expansion(add = c(0, NA)),
    breaks = c(1:3) * 100
  ) +

  scale_x_continuous(
    breaks = seq(from = 1, to = 13, by = 2),
    expand = expansion(add = c(0, NA)),
    labels = c(
      "July '24",
      "Sept.",
      "Nov.",
      "Jan. '25",
      "March",
      "May",
      "July"
    )
  ) +

  theme(
    panel.grid.major.y = element_line(
      linetype = 3,
      color = "grey70",
      size = .25
    ),
    axis.line.x = element_line(linetype = 1, color = "black", linewidth = .25),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(color = "grey40", margin = margin(b = 10)),
    text = element_text(family = "Libre Franklin"),
    plot.caption = element_textbox_simple(hjust = 0, color = "grey30")
  ) +

  labs(
    x = "",
    y = "",
    title = "Monthly change in jobs",
    caption = glue(
      "Source: Bureau of Labor Statistics",
      "<span style='color: {'grey75'};'> \u2219 </span>",
      "Note: Data is seasonally adjusted.",
      "<span style='color: {'grey75'};'> \u2219 </span>",
      "By Christine Zhang"
    )
  )
