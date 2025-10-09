#' Title
#'
#' @description
#' Given a tibble with wind vector direction in degrees and the average signed
#' wind vector speed, return a tibble with the northerly and easterly wind components.
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
#' @param wind_data
#'
#' @returns
#' @export
#'
#' @examples
mutate_fill_missing <- function(wind_data) {
    wind_data |>
        group_by(year = year(time)) |>
        tidyr::complete(time = seq(min(time), max(time), by = "1 min")) |>
        ungroup() |>
        select(-year)
}


#' Title
#'
#' @param wind_data
#'
#' @returns
#' @export
#'
#' @examples
gap_summary <- function(wind_data, gap_size = 1) {

    wind_data %>%
        arrange(time) %>%
        mutate(row_index = row_number()) %>%
        mutate(
            gap_start_index = lag(row_index),
            previous_time = lag(time),
            gap_duration = time - lag(time)
        ) %>%
        # Filter for gaps longer than 1 minute
        filter(gap_duration > minutes(1)) %>%
        # Select and rename columns for a clean report, including the new index
        select(
            gap_start_index,
            gap_start_time = previous_time,
            gap_end_time = time,
            duration = gap_duration
        )
}

ortho_decomp <- function(cur_x, cur_y, lag_x, lag_y) {
    #performs operations by row
    Vcur <- c(cur_x, cur_y)
    Vprv <- c(lag_x, lag_y)
    DD <- Vcur - Vprv

    nn <- sqrt(sum(Vprv^2))

    ss <- sum(DD * Vprv) / nn^2
    pp <- ss * Vprv
    hh <- DD - pp

    pp_axis <- ss * nn

    crossprod <- Vprv[1] * Vcur[2] - Vprv[2] * Vcur[1]
    hh_dir <- ifelse(crossprod < 0, 1, -1)
    hh_axis <- sqrt(sum(hh^2)) * hh_dir

    #pp_axis is parallel to Vprv
    #hh_axis is orthogonal to Vprv
    c(pp_axis = pp_axis, hh_axis = hh_axis)
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
               wspd_vec_mean = northerly / cos(wdir_rad)) |>
        mutate(wdir_rad = if_else(wdir_rad < 0, wdir_rad + 2*pi, wdir_rad))
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




