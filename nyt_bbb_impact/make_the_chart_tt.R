
library(tidyverse)
library(scales)
source("make_data.R"))

chart_cols <- c("#d35400", "#58a497")

d |> 
  mutate(fill_col = ifelse(est_change < 0,
         chart_cols[1], chart_cols[2])) |> 
  ggplot(
    aes(x = household_group,
        y = est_change, 
        fill = fill_col,
      color = fill_col)
  ) +
  geom_col() +
  geom_text(aes(label = label_percent(.1)(est_change),
                vjust = ifelse(
                  est_change < 0, 1.5, -.5
                )), fontface = "bold") +
  geom_text(aes(label = household_group,
                y = ifelse(est_change > 0, -.005, .005)),
              color = "grey50", lineheight = 1) +
  geom_hline(yintercept = 0) +
  scale_fill_identity() +
  scale_color_identity() +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(text = element_text(family = "Libre Franklin"),
        plot.title = element_text(face = "bold", size = 12,
                                  color = "grey30"),
      plot.caption = element_text(lineheight = 1.2, size = 8,
                                  hjust = 0, color = "grey30",
                                margin = margin(t = 20))) +
  labs(title = "How the Bill Would Affect Households at Different Income Ranks",
       subtitle = "Estimated annual average change in resources between 2026-34",
      caption = "Note: Estimated annual average effect of the House version of the One Big Beautiful Bill Act on after-tax income. Groups are based on income adjusted for household size. Source: Congressional Budget Office" |> 
        str_wrap(110))
