#' Title
#'
#'
#' @import dplyr
#' @import lubridate
#'
#' @export
#'
mutate_east_north <- function(wind_data) {
    wind_data |>
        mutate(minute_of_hour = minute(time),
               hour = hour(time),
               minute_of_day = 60*hour + minute_of_hour) |>
        mutate(wdir_rad = wdir_vec_mean * (pi / 180),
               northerly = wspd_vec_mean * sin(wdir_rad),
               easterly = wspd_vec_mean * cos(wdir_rad))
}


#' Title
#'
#'
#' @import dplyr
#' @import lubridate
#'
#' @export
#'
summarise_10_median <- function(wind_data, ...) {
    vars <- rlang::ensyms(...)

    wind_data |>
        mutate_east_north() |>
        group_by(time = as_hms(time)) |>
        summarise(across(all_of(as.character(vars)), ~ median(.x, na.rm = TRUE), .names = "median_{.col}")) |>
        mutate(time = as_datetime("2025-04-06") + time) |>
        group_by(time_grp_10 = floor_date(time, "10 mins")) |>
        summarise(across(starts_with("median_"), mean, .names = "mean_{.col}")) |>
        mutate(time_grp_10 = hms::as_hms(time_grp_10)) |>
        rename(time = time_grp_10)
}




