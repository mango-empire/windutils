preprocess_wind <- function(wind_data) {
    df_out <- wind_data |>
        filter(month(time) == 6) |>
        mutate(minute_of_hour = minute(time),
               hour = hour(time),
               minute_of_day = 60*hour + minute_of_hour) |>
        mutate(wdir_rad = wdir_vec_mean * (pi / 180),
               northerly = wspd_vec_mean * sin(wdir_rad),
               easterly = wspd_vec_mean * cos(wdir_rad))
    df_out
}
