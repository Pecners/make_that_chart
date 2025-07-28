
d <- tibble::tribble(
  ~household_group, ~est_change,
  "Bottom\n10%", -.039,
  "10th-\n20th", -.012,
  "20th-\n30th", -.004,
  "30th-\n40th", .001,
  "40th-\n50th", .005,
  "50th-\n60th", .008,
  "60th-\n70th", .011,
  "70th-\n80th", .013,
  "80th-\n90th", .015,
  "Top\n10%", .023
) |> 
  mutate(household_group = factor(
    household_group,
    levels = c(
      "Bottom\n10%", 
      "10th-\n20th", 
      "20th-\n30th", 
      "30th-\n40th", 
      "40th-\n50th", 
      "50th-\n60th", 
      "60th-\n70th", 
      "70th-\n80th", 
      "80th-\n90th", 
      "Top\n10%"
    )
  ))
