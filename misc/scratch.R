library(tidyverse)
library(hms)

wind_data <- vroom::vroom("/Users/kevin/Documents/wind/data/june_data.csv")


wind_data |> plot_smooth_scatter()

wind_data |> mutate_east_north() |> view()

wind_data |> summarise_10_median(wspd_vec_mean)

wind_data |> mutate_first_diff() |> summarise_10_median(wspd_vec_mean_lag1)
