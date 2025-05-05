#' Title
#'
#'
#' @import dplyr
#' @import lubridate
#'
#' @export
#'
mutate_east_north <- function(wind_data) {
    #degree start at zero from north
    #increases in clockwise direction
    wind_data |>
        mutate(wdir_rad = wdir_vec_mean * (pi / 180),
               northerly = wspd_vec_mean * cos(wdir_rad),
               easterly = wspd_vec_mean * sin(wdir_rad))
}


#' Title
#'
#'
#' @import dplyr
#' @import lubridate
#'
#' @export
#'
mutate_first_diff <- function(wind_data) {
    wind_data |>
        mutate(wspd_vec_mean_lag1 = c(diff(wspd_vec_mean),NA))
}


#' Title
#'
#'
#' @import dplyr
#' @import lubridate
#'
#' @export
#'
mutate_daynight <- function(wind_data) {
    wind_data |>
        mutate_time() |>
        mutate(daynight = case_when(hour %in% c(2:12) ~ "N",
                                    hour %in% c(14:24) ~ "D",
                                    .default = "T"))
}

#' Title
#'
#'
#' @import dplyr
#' @import lubridate
#'
#' @export
#'
mutate_vecmean <- function(wind_data) {
    wind_data |>
        mutate(wdir_rad = atan2(easterly, northerly),
               wspd_vec_mean = northerly / cos(wdir_rad))
}


#' Title
#'
#'
#' @import dplyr
#' @import lubridate
#'
#' @export
#'
mutate_time <- function(wind_data) {
    wind_data |>
        mutate(minute_of_hour = minute(time),
               hour = hour(time),
               minute_of_day = 60*hour + minute_of_hour)
}


#' Title
#'
#'
#' @import dplyr
#' @import lubridate
#'
#' @export
#'
mutate_addtime <- function(wind_data) {
    tix <- seq(ymd_hms('1993-07-21 00:00:00'), length = nrow(wind_data),by='min')
    wind_data$time <- tix
    wind_data
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
    #expects easterly and northerly already in wind_data

    wind_data |>
        group_by(time = as_hms(time)) |>
        summarise(across(all_of(as.character(vars)), ~ median(.x, na.rm = TRUE), .names = "median_{.col}")) |>
        mutate(time = as_datetime("2025-04-06") + time) |>
        group_by(time_grp_10 = floor_date(time, "10 mins")) |>
        summarise(across(starts_with("median_"), mean, .names = "mean_{.col}")) |>
        mutate(time_grp_10 = hms::as_hms(time_grp_10)) |>
        rename(time = time_grp_10)
}


#' Title
#'
#'
#' @import dplyr
#' @import lubridate
#'
#' @export
#'
summarise_10_five_quantiles <- function(wind_data, ...) {
    vars <- rlang::ensyms(...)

    wind_data |>
        group_by(time = as_hms(time)) |>
        summarise(across(all_of(as.character(vars)),
                   list(
                       q10 = ~quantile(.x, 0.10, na.rm = TRUE),
                       q25 = ~quantile(.x, 0.25, na.rm = TRUE),
                       q50 = ~quantile(.x, 0.50, na.rm = TRUE),
                       q75 = ~quantile(.x, 0.75, na.rm = TRUE),
                       q90 = ~quantile(.x, 0.90, na.rm = TRUE)
                   ))) |>
        mutate(time = as_datetime("2025-04-06") + time) |>
        group_by(time_grp_10 = floor_date(time, "10 mins")) |>
        summarise(across(ends_with(c("q10", "q25", "q50", "q75", "q90")), mean, .names = "mean_{.col}")) |>
        mutate(time_grp_10 = hms::as_hms(time_grp_10)) |>
        rename(time = time_grp_10)
}




