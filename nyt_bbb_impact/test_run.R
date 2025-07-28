library(tidyverse)
library(scales)

d <- read_rds("tutorials/nytimes_big_bill/data/houshold_est_change.rda")

f <- d$household_group

dd <- d |> 
  mutate(household_group = factor(household_group,
                                  levels = f))

these_colors <- c("#D35400", "#58A497")

dd |> 
  ggplot(aes(household_group, est_change,
             fill = ifelse(est_change > 0, these_colors[2], these_colors[1]),
             color = ifelse(est_change > 0, these_colors[2], these_colors[1]))) +
  geom_col(width = .8) +
  geom_text(aes(label = label_percent(.1)(est_change),
                vjust = ifelse(est_change < 0 , 1.5, -.5)),
            fontface = "bold", family = "Libre Franklin") +
  geom_text(aes(label = household_group,
                y = ifelse(est_change < 0, .003, -.003)),
            lineheight = .9, color = "grey50", size = 3.5,
            family = "Libre Franklin") +
  geom_hline(yintercept = 0) +
  scale_fill_identity() +
  scale_color_identity() +
  theme_void() +
  theme(text = element_text(family = "Libre Franklin"),
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(color = "grey30", size = 14,
                                     margin = margin(t = "10"))) +
  labs(
    title = "How the Bill Would Affect Households at Different Income Ranks",
    subtitle = "Estimated annual average change in resources between 2026-34"
  )
