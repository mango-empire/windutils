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

    E0.5 <- wind_data |> summarise_10_median(easterly) |> _$mean_median_easterly
    N0.5 <- wind_data |> summarise_10_median(northerly) |> _$mean_median_northerly

    par(mar=c(4,4,0.5,0.5))
    plot(E0.5,N0.5,type="n",asp=1, xlab="Easterly (m/s)",ylab="Northerly (m/s)",cex.axis=0.72,cex.lab=
             0.72)
    lines(E0.5[1:13],N0.5[1:13],col=3)
    lines(E0.5[13:73],N0.5[13:73],col=gray(0.6))
    lines(E0.5[73:85],N0.5[73:85],col=2)
    lines(E0.5[c(85:144,1)],N0.5[c(85:144,1)])
    points(E0.5[0:23*6+1],N0.5[0:23*6+1],col=c(3,3,rep(gray(0.6),10),2,2,rep(1,10)),
           pch=c(4,rep(1,23)))
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


#' Title
#'
#' @param wind_data
#' @param wspd_lim
#'
#' @returns
#' @export
#'
#' @examples
plot_orthogonal_decomp <- function(wind_data, skip_n = 1440, wspd_lim = c(1,5)) {
    #requires daynight and east_north

    if(!all(c("northerly", "easterly") %in% names(wind_data))) {
        wind_data <- wind_data |> mutate_east_north()
    }

    if(!all(c("daynight") %in% names(wind_data))) {
        wind_data <- wind_data |> mutate_daynight()
    }

    decomp <- wind_data |>
        filter(wspd_vec_mean <= wspd_lim[2], wspd_vec_mean > wspd_lim[1]) |>
        mutate_fill_missing() |>
        mutate(lag_northerly = lag(northerly, 1),
               lag_easterly = lag(easterly, 1)) |>
        rowwise() |>
        mutate(ortho_decomp = list(ortho_decomp(easterly, northerly, lag_easterly, lag_northerly))) |>
        unnest_wider(ortho_decomp) |>
        select(time, hh_axis, pp_axis, wspd_vec_mean, daynight) |>
        filter(row_number() %% skip_n != 0) |> #skip every skip_nth row
        drop_na()

    bw <- c(0.4,0.4)
    xl <- c(-3,3)
    xs <- c(-10.5,9.8)
    hlev <- -6:0/2
    gs <- c(101,101)

    ltx <- paste0("(", wspd_lim[1],",", wspd_lim[2],"]")
    mlab1 <- paste0("nighttime, wind speed ", ltx, " m/s")
    mlab2 <- paste0("daytime, wind speed ", ltx, " m/s")

    par(mfrow=c(2,2),mar=c(4,4,3.5,0.5))
    ttp <- decomp |> filter(daynight == "N")
    x31 = ttp$hh_axis
    y31 =  ttp$pp_axis
    smoothScatter(x31,y31,xlab="change orthogonal to current direction",xlim=xs,ylim=xs,
                  ylab="change along current direction",asp=1,nbin=201,cex.axis=0.85,cex.lab=0.85)
    hh31 = bkde2D(cbind(x31,y31),bandwidth=bw,gridsize=gs)
    contour(hh31$x1,hh31$x2,log10(hh31$fhat),levels=hlev,xlim=xl,ylim=xl,asp=1,
            xlab="change orthogonal to current direction",
            ylab="change along current direction",cex.axis=0.85,cex.lab=0.85)
    abline(h=0,col=gray(0.5),lty=2)
    abline(v=0,col=gray(0.5),lty=2)
    mtext(mlab1,side=3,line=1.5,at=-5.2,cex=0.85)

    ttp <- decomp |> filter(daynight == "D")
    x31 = ttp$hh_axis
    y31 =  ttp$pp_axis
    smoothScatter(x31,y31,xlab="change orthogonal to current direction",xlim=xs,ylim=xs,
                  ylab="change along current direction",asp=1,nbin=201,cex.axis=0.85,cex.lab=0.85)
    hh31 = bkde2D(cbind(x31,y31),bandwidth=bw,gridsize=gs)
    contour(hh31$x1,hh31$x2,log10(hh31$fhat),levels=hlev,xlim=xl,ylim=xl,asp=1,
            xlab="change orthogonal to current direction",
            ylab="change along current direction",cex.axis=0.85,cex.lab=0.85)
    abline(h=0,col=gray(0.5),lty=2)
    abline(v=0,col=gray(0.5),lty=2)
    mtext(mlab2,side=3,line=1.5,at=-5.2,cex=0.85)

}
