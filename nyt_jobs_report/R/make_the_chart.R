library(readxl)
library(tidyverse)
library(glue)
library(lubridate)
library(scales)
library(ggtext)

jobs <- read_xlsx("data/july_jobs.xlsx", skip = 11)

long_jobs <- jobs |>
  pivot_longer(cols = -1, names_to = "month", values_to = "jobs") |>
  mutate(ym = ym(glue("{Year}-{month}"))) |>
  filter(
    ym >= ymd("2024-07-01") &
      !is.na(jobs)
  ) |>
  mutate(xind = row_number())

y_labels <- tibble(
  y = c(1:3) * 100
) |>
  mutate(lab = label_comma(scale = 1e3, prefix = "+")(y))

first_values <- tibble(
  x = 11:12,
  y = c(144, 147)
)

x_labels <- c(
  "July '24",
  "Sept.",
  "Nov.",
  "Jan. '25",
  "March",
  "May",
  "July"
)

this_grey <- "#BDBDBD"
this_orange <- "#FE8918"

update_geom_defaults("text", list(family = "Libre Franklin"))
update_geom_defaults("label", list(family = "Libre Franklin"))


long_jobs |>
  ggplot(aes(xind, jobs)) +

  geom_text(
    data = y_labels,
    aes(label = lab, x = -.5, y = y + 10),
    hjust = 0,
    color = "grey40",
    size = 4
  ) +

  annotate(geom = "segment", x = 13, xend = 13, y = 50, yend = 250) +

  geom_col(
    data = first_values,
    aes(x = x, y = y),
    fill = NA,
    color = this_grey
  ) +

  geom_col(aes(
    fill = ifelse(xind == 13, this_orange, this_grey)
  )) +

  annotate(
    geom = "label",
    label = "REVISED\nDOWN",
    fontface = "italic",
    x = 11.5,
    y = 90,
    fill = alpha("white", .75),
    label.size = 0,
    color = "grey40",
    lineheight = 1
  ) +

  annotate(
    geom = "text",
    x = 13.5,
    y = 275,
    label = "+73,000 jobs\nin July",
    hjust = 1,
    lineheight = 1
  ) +

  annotate(
    geom = "label",
    label.size = 0,
    x = 7.5,
    y = 200,
    label = "Growth in May and\nJune was lower than\ninitially estimated.",
    lineheight = 1,
    hjust = 0
  ) +

  annotate(
    geom = "curve",
    x = 11,
    xend = 11.5,
    y = 180,
    yend = 160,
    curvature = -.4
  ) +

  scale_y_continuous(expand = c(0, 0)) +

  scale_x_continuous(
    breaks = seq(from = 1, to = 13, by = 2),
    labels = x_labels,
    expand = expansion(add = c(0, .25))
  ) +

  scale_fill_identity() +

  theme(
    panel.background = element_blank(),
    panel.grid.major.y = element_line(
      color = "grey70",
      linetype = 3,
      size = .25
    ),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line.x = element_line(linewidth = .25),
    plot.caption = element_textbox_simple(color = "grey30"),
    text = element_text(family = "Libre Franklin")
  ) +

  labs(
    x = "",
    y = "",
    title = "Monthly change in jobs",
    caption = glue(
      "Source: Bureau of Labor Statistics",
      "<span style='color:{'grey70'};'> \u2219 </span>",
      "Note: Data is seasonally adjusted.",
      "<span style='color:{'grey70'};'> \u2219 </span>",
      "By Christine Zhang"
    )
  )
