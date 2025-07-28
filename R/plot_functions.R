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
        summarise_10_median(wspd_vec_mean, easterly, northerly) |>
        mutate(mean_mag_comp_median = sqrt(mean_median_easterly^2 + mean_median_northerly^2)) |>
        select(time, mean_median_wspd_vec_mean, mean_mag_comp_median) |>
        pivot_longer(cols = starts_with("mean"),
                     names_to = 'type',
                     values_to = 'wnd_speed') |>
        ggplot(aes(time, wnd_speed, group = type, shape = type)) +
            geom_point() +
            scale_shape_manual(values = c("mean_median_wspd_vec_mean" = 16, "mean_mag_comp_median" = 3)) +
            ggtitle("Wind Speed (m/s)") +
            ylab("median") +
            xlab("hour (UTC)") +
            theme_minimal()
}


#' Title
#'
#'
#' @import dplyr
#' @import ggplot2
#'
#' @export
#'
plot_wspd_median_compare <- function(wind_data, synthetic_data) {

    ww_real <- wind_data |>
        summarise_10_median(wspd_vec_mean, easterly, northerly) |>
        mutate(mean_mag_comp_median = sqrt(mean_median_easterly^2 + mean_median_northerly^2)) |>
        select(time, mean_median_wspd_vec_mean, mean_mag_comp_median) |>
        pivot_longer(cols = starts_with("mean"),
                     names_to = 'type',
                     values_to = 'wnd_speed') |>
        mutate(source = 'real')


    ww_synth <- synthetic_data |>
        summarise_10_median(wspd_vec_mean, easterly, northerly) |>
        mutate(mean_mag_comp_median = sqrt(mean_median_easterly^2 + mean_median_northerly^2)) |>
        select(time, mean_median_wspd_vec_mean, mean_mag_comp_median) |>
        pivot_longer(cols = starts_with("mean"),
                     names_to = 'type',
                     values_to = 'wnd_speed') |>
        mutate(source = 'synthetic')

    ww_combine <- rbind(ww_synth, ww_real)

    ww_combine |>
        ggplot(aes(time, wnd_speed, group = type, shape = type, color = source)) +
        geom_point() +
        geom_vline(xintercept = as_hms("12:00:00"), linetype = "longdash") +
        geom_vline(xintercept = as_hms("2:00:00"), linetype = "longdash") +
        scale_shape_manual(values = c("mean_median_wspd_vec_mean" = 16, "mean_mag_comp_median" = 3),
                           labels = c("mean_median_wspd_vec_mean" = "Vector",
                                      "mean_mag_comp_median" = "Component")) +
        ggtitle("Synthetic vs Real") +
        ylab("Median Wind Speed (m/s)") +
        ggsci::scale_color_npg(labels = c(
            "real" = "Real Data",
            "synthetic" = "Synthetic Data"
        )) +
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

    if(!all(c("northerly", "easterly") %in% names(wind_data))) {
        wind_data <- wind_data |> mutate_east_north()
    }

    wind_data |>
        group_by(minute_of_day) |>
        summarise(max_wspd = max(wspd_vec_mean, na.rm = TRUE)) |>
        mutate(hour = minute_of_day / 60) |>
        ggplot(aes(hour, max_wspd)) +
            geom_point() +
            geom_vline(xintercept = 12, linetype = "longdash", color = 'red') +
            geom_vline(xintercept = 2, linetype = "longdash", color = 'red') +
            xlim(0, 24) +
            ylim(12, 26) +
            theme_minimal() +
            xlab("hour") +
            ylab("Maximum Wind Speed (m/s)")

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

    if(!all(c("northerly", "easterly") %in% names(wind_data))) {
        wind_data <- wind_data |> mutate_east_north()
    }

    wind_data |>
        summarise_10_median(easterly, northerly) |>
        ggplot(aes(mean_median_easterly, mean_median_northerly)) +
            geom_path() +
            theme_minimal()
}


#' Title
#'
#'
#' @import dplyr
#' @import ggplot2
#'
#' @export
#'
plot_smooth_scatter <- function(wind_data) {

    if(!all(c("northerly", "easterly") %in% names(wind_data))) {
        wind_data <- wind_data |> mutate_east_north()
    }

    night_data <- wind_data |> mutate_daynight() |> filter(daynight == "N")
    day_data <-   wind_data |> mutate_daynight() |> filter(daynight == "D")

    par(mfrow=c(1,2),mar=c(4,4,3,0.5))
    smoothScatter(night_data$easterly, night_data$northerly, xlim=c(-25,21.5),ylim=c(-20.4,21.5),
                  xlab ="Easterly (m/s)",ylab="Northerly (m/s)",main="Nighttime wind vectors")
    abline(h=0,col=gray(0.6),lty=2)
    abline(v=0,col=gray(0.6),lty=2)
    smoothScatter(day_data$easterly, day_data$northerly,xlim=c(-25,21.5),ylim=c(-20.4,21.5),
                  xlab ="Easterly (m/s)",ylab="Northerly (m/s)",main="Daytime wind vectors")
    abline(h=0,col=gray(0.6),lty=2)
    abline(v=0,col=gray(0.6),lty=2)
}



#' Title
#'
#'
#' @import dplyr
#' @import ggplot2
#'
#' @export
#'
plot_five_quantile <- function(wind_data) {

    if(!all(c("time") %in% names(wind_data))) {
        wind_data <- wind_data |> mutate_addtime() |> mutate_time()
    }

    tmpdf <- wind_data |>
        mutate_first_diff() |>
        summarise_10_five_quantiles(wspd_vec_mean_lag1)

    colnames(tmpdf) <- c('time', 'q10', 'q25', 'q50', 'q75', 'q90')

    tmpdf |>
        pivot_longer(starts_with("q")) |>
        ggplot(aes(time, value, group = name)) +
            geom_path() +
            ylim(-1.5, 1.5) +
            xlab("time") +
            ylab("quantiles of change in windspeed (m/s)") +
            geom_vline(xintercept = as_hms("12:00:00"), linetype = "longdash") +
            geom_vline(xintercept = as_hms("2:00:00"), linetype = "longdash")
}


plot_wspd_hist <- function(wind_data) {
    wspd
}



#' Title
#'
#'
#' @import dplyr
#' @import ggplot2
#'
#' @export
#'
plot_five_quantile_compare <- function(wind_data, synthetic_data) {

    if(!all(c("time") %in% names(wind_data))) {
        wind_data <- wind_data |> mutate_addtime() |> mutate_time()
    }
    if(!all(c("time") %in% names(synthetic_data))) {
        synthetic_data <- synthetic_data |> mutate_addtime() |> mutate_time()
    }


    ddf1 <- wind_data |>
        mutate_first_diff() |>
        summarise_10_five_quantiles(wspd_vec_mean_lag1) |>
        mutate(source = 'real')

    ddf2 <- synthetic_data |>
        mutate_first_diff() |>
        summarise_10_five_quantiles(wspd_vec_mean_lag1) |>
        mutate(source = 'synthetic')

    ddf2 <- ddf2[-144,]

    colnames(ddf1) <- c('time', 'q10', 'q25', 'q50', 'q75', 'q90', 'source')
    colnames(ddf2) <- c('time', 'q10', 'q25', 'q50', 'q75', 'q90', 'source')

    full_df <- rbind(ddf1, ddf2)

    full_df |>
        pivot_longer(starts_with("q")) |>
        ggplot(aes(time, value, group = interaction(name,source), color = source)) +
        geom_path() +
        ggsci::scale_color_npg() +
        ylim(-1.5, 1.5) +
        xlab("time") +
        ylab("quantiles of change in windspeed (m/s)") +
        geom_vline(xintercept = as_hms("12:00:00"), linetype = "longdash") +
        geom_vline(xintercept = as_hms("2:00:00"), linetype = "longdash") +
        theme_minimal()
}

