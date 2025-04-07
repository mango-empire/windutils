#' Title
#'
#'
#' @import dplyr
#' @import ggplot2
#'
#' @export
#'
plot_wspd_median <- function(wind_data) {

    wind_data |>
        summarise_10_median(wspd_vec_mean) |>
        ggplot(aes(time, mean_median_wspd_vec_mean)) +
            geom_point(shape = 1) +
            ggtitle("Wind Speed (m/s)") +
            ylab("median") +
            xlab("hour (UTC)") +
            theme_minimal()
}

#' Title
#'
#' @import dplyr
#' @import ggplot2
#'
#' @export
#'
plot_wspd_max <- function(wind_data) {

    wind_data |>
        mutate_east_north() |>
        group_by(minute_of_day) |>
        summarise(max_wspd = max(wspd_vec_mean, na.rm = TRUE)) |>
        mutate(hour = minute_of_day / 60) |>
        ggplot(aes(hour, max_wspd)) +
            geom_point() +
            theme_minimal() +
            xlim(0, 24)

}

#' Title
#'
#'
#' @import dplyr
#' @import ggplot2
#'
#' @export
#'
plot_cycle_median <- function(wind_data) {

    wind_data |>
        mutate_east_north() |>
        summarise_10_median(easterly, northerly) |>
        ggplot(aes(mean_median_northerly, mean_median_easterly)) +
            geom_path() +
            theme_minimal()
}
