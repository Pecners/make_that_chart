library(tidyverse)
library(scales)
source("make_data.R")

chart_cols <- c("#d35400", "#58a497")

d |> 
  ggplot(aes(household_group, est_change,
             fill = ifelse(est_change > 0, chart_cols[2], chart_cols[1]),
             color = ifelse(est_change > 0, chart_cols[2], chart_cols[1]))) +
  geom_col() +
  geom_hline(yintercept = 0) +
  geom_text(aes(label = label_percent(.1)(est_change),
                vjust = ifelse(est_change < 0, 1.5, -.5)),
              family = "Libre Franklin") +
  geom_text(aes(label = household_group,
                y = ifelse(est_change > 0, -.003, .003)),
            color = "grey50", lineheight = 1,
            family = "Libre Franklin", size = 3) + 
  scale_fill_identity() +
  scale_color_identity() +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(text = element_text(family = "Libre Franklin"),
        title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "plain", color = "grey30"),
        plot.caption = element_text(face = "plain", hjust = 0, size = 9,
                                    lineheight = 1.1, color = "grey30",
                                    margin = margin(t = 10, b = 5))) +
  labs(title = "How the Bill Would Affect Households at Different Income Ranks",
       subtitle = "Estimated annual average change in resources between 2026-34",
       caption = glue::glue(
        "Note: Estimated annual average effect of the House",
        "version of the One Big Beautiful Bill Act on after-tax",
        "income. Groups are based on income adjusted for household size.  |  ",
        "Source: Congressional Budget Office"
      ) |> str_wrap(110)
    )

