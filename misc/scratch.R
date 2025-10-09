library(tidyverse)
library(hms)
library(KernSmooth)

wind_data <- vroom::vroom("/Users/kevin/Documents/wind/data/june_data.csv")
wind_data <- wind_data |>
    filter(year(time) %in% c(1998:2020)) |>
    #mutate_fill_missing() |>
    filter(day(time) %in% c(1:21))


#wind_data <- vroom::vroom("/Users/kevin/Documents/wind/data/gen_vqvae_v2.csv", col_names = FALSE)
colnames(wind_data) <- c("easterly", "northerly")
wind_data <- mutate_addtime(wind_data)

wind_data <- wind_data |> filter(year(time) %in% c(1998:2020))
wind_data <- wind_data |> mutate_vecmean() |>
    mutate(wdir_vec_mean = (wdir_rad * 180) / (pi))


wind_data <- vroom::vroom("/Users/kevin/Documents/wind/data/training_imputed.csv") |>
    select(time, atmos_pressure, temp_mean, rh_mean, wspd_vec_mean, wdir_vec_mean)


wind_data |> plot_smooth_scatter()

wind_data |> mutate_east_north() |> view()

wind_data |> summarise_10_median(wspd_vec_mean)

wind_data |> mutate_first_diff() |> summarise_10_median(wspd_vec_mean_lag1)

ortho_decomp <- function(cur_x, cur_y, lag_x, lag_y) {
    Vcur <- cbind(cur_x, cur_y)
    Vprv <- cbind(lag_x, lag_y)
    DD <- Vcur - Vprv

    nn <- sqrt(apply(Vprv^2, 1, sum))

    ss <- apply(DD * Vprv, 1, sum) / nn^2
    pp <- ss * Vprv
    hh <- DD - pp

    pp_axis <- ss * nn

    crossprod <- Vprv[,1] * Vcur[,2] - Vprv[,2] * Vcur[,1]
    hh_dir <- ifelse(crossprod < 0, 1, -1)
    hh_axis <- sqrt(apply(hh^2, 1, sum)) * hh_dir

    cbind(pp_axis, hh_axis)
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


dir_dif <- function(x) {
    # Find indices of values > 180 and adjust them
    idx_pos <- which(x > 180)
    x[idx_pos] <- x[idx_pos] - 360

    # Find indices of values < -180 and adjust them
    idx_neg <- which(x < -180)
    x[idx_neg] <- x[idx_neg] + 360

    x
}

DD_norm <- function(cur_x, cur_y, lag_x, lag_y) {
    Vcur <- c(cur_x, cur_y)
    Vprv <- c(lag_x, lag_y)
    DD <- Vcur - Vprv
    sqrt(sum(DD^2))
}

DD_head <- function(cur_x, cur_y, lag_x, lag_y) {
    Vcur <- c(cur_x, cur_y)
    Vprv <- c(lag_x, lag_y)
    DD <- Vcur - Vprv
    atan2(DD[1],DD[2]) * 180 / pi
}

tmp <- wind_data |>
    mutate_daynight() |>
    mutate_east_north() |>
    mutate(lag_northerly = lag(northerly, 1),
           lag_easterly = lag(easterly, 1),
           lag_wdir = lag(wdir_vec_mean, 1)) |>
    mutate(DH31 = dir_dif(wdir_vec_mean - lag_wdir)) |>
    rowwise() |>
    mutate(DD_norm = DD_norm(easterly, northerly, lag_easterly, lag_northerly),
           DD_head = DD_head(easterly, northerly, lag_easterly, lag_northerly)) |>
    mutate(DH = dir_dif(lag_wdir - DD_head)) |>
    mutate(hh_axis = sin(DH31 * pi / 180) * wspd_vec_mean,
           pp_axis = cos(DH * pi / 180) * DD_norm) |>
    filter(wspd_vec_mean <= 5, daynight == "")



tmp2 <- wind_data |>
    mutate_daynight() |>
    mutate_east_north() |>
    filter(wspd_vec_mean <= 5, wspd_vec_mean > 1, daynight == "D") |>
    mutate_fill_missing() |>
    mutate(lag_northerly = lag(northerly, 1),
           lag_easterly = lag(easterly, 1)) |>
    rowwise() |>
    mutate(ortho_decomp = list(ortho_decomp_row(easterly, northerly, lag_easterly, lag_northerly))) |>
    unnest_wider(ortho_decomp) |>
    select(time, hh_axis, pp_axis, daynight) |>
    drop_na()

















wind_data[2,]$wspd_vec_mean * sin(2.5 * pi / 180)


ch <- ortho_decomp(tmp$easterly, tmp$northerly, tmp$lag_easterly, tmp$lag_northerly)


ch$check[1:10,]

tmp[1:10,]

cur_x <- tmp$easterly
cur_y <- tmp$northerly
lag_x <- tmp$lag_easterly
lag_y <- tmp$lag_northerly

smoothScatter(tmp2$hh_axis, tmp2$pp_axis, xlab = "orthogonal", ylab = "along", main = "Night Time", asp=1,nbin=201)

bkest <- KernSmooth::bkde2D(cbind(tmp2$hh_axis, tmp2$pp_axis), bandwidth=c(0.2,0.2),gridsize=c(201,201))
contour(bkest$x2,bkest$x1,log10(t(bkest$fhat)),levels=-5:0/2,xlim=c(-2,2),ylim=c(-2,2),asp=1)



smoothScatter(pp_axis, hh_axis)


wd2 <- wind_data[1:15,]

tmp2 <- wd2 |>
    mutate_east_north() |>
    select(-atmos_pressure, -temp_mean, -rh_mean, -wdir_rad) |>
    mutate(lag_northerly = lag(northerly, 1),
           lag_easterly = lag(easterly, 1))














#-----

#load training data
Wind31.tr <- read_table("/Users/kevin/Documents/wind/data/MS-Wind-Training.txt", col_names = FALSE)
colnames(Wind31.tr) <- c("time", "wspd_vec_mean", "wdir_vec_mean", "easterly", "northerly")
#Wind31.tr$time <- as.character(Wind31.tr$time)
Wind31.tr <- as.data.frame(Wind31.tr)
Wind31.tr <- Wind31.tr[,c("time", "wspd_vec_mean", "wdir_vec_mean",
                          "wspd_vec_mean", "wspd_vec_mean", "wdir_vec_mean", "easterly", "northerly")]

Wind31 <- read_csv("Data/june_data.csv")


y31 <- year(Wind31.tr$time)

# Year for whole dataset

y31w = year(Wind31$time)

# No serious evidence of trend in average wind speed

summary(lm(tapply(Wind31$wspd_vec_mean,y31w,mean) ~ I(0:30)))

# Extract day of June

dd31 = day(Wind31.tr$time)

# Extract hour

h31 = hour(Wind31.tr$time)

# Extract minute of hour

m31 = minute(Wind31.tr$time)

# Minute of day

mc31 = 60*h31+m31

# Minute of year starting June 1

my31 = mc31+1440*(dd31-1)

# Minutes from 2:00 through 11:59

night31 = (mc31 >= 120 & mc31 < 720)

# Not near nighttime.

day31 = (mc31 >= 840)





# min1.y is first minute of each year
I31 <- 1:nrow(Wind31.tr)
min1.y = NULL
for (yy in 1998:2020) min1.y = c(min1.y,min(I31[y31 == yy]))

# Find indices where next minute is available.

Icon = NULL
for(yy in 1998:2020) {
    myy = my31[y31 == yy]
    ttt = diff(myy)
    nyy = rep(FALSE,length(myy))
    nyy[c(ttt,0) == 1] = TRUE
    Icon = c(Icon,nyy)
}

# Find indices where next two minutes are available.

Icon2 = NULL
for(yy in 1998:2020) {
    myy = my31[y31 == yy]
    ttt = diff(myy)
    tt2 = diff(myy,lag=2)
    nyy = rep(FALSE,length(myy))
    nyy[c(ttt,0) == 1 & c(tt2,0,0) == 2] = TRUE
    Icon2 = c(Icon2,nyy)
}



# Converts an angle to be between +/- 180.

dir.dif = function(x) {
    x[x>180] = x[x>180] - 360
    x[x< -180] = x[x< -180] + 360
    x
}




wsr <- c(1,5)
bw=c(0.4,0.4)
xl=c(-3,3)
xs=c(-10.5,9.8)
hlev=-6:0/2
gs=c(101,101)
mlab1 = ""
mlab2 = ""
par(mfrow=c(2,2),mar=c(4,4,3.5,0.5))
n31 = I31[Icon & night31 & Wind31.tr[,5] > wsr[1] & Wind31.tr[,5] <= wsr[2]]
dh31 = dir.dif(Wind31.tr[n31+1,6]-Wind31.tr[n31,6])
norm.dwv31 = sqrt((Wind31.tr[n31+1,7]-Wind31.tr[n31,7])^2+(Wind31.tr[n31+1,8]-Wind31.tr[n31,8])^2)
head31n =  atan2(Wind31.tr[n31+1,7]-Wind31.tr[n31,7],
                 Wind31.tr[n31+1,8]-Wind31.tr[n31,8])*180/pi
dhead31n = dir.dif(Wind31.tr[n31,6]-head31n)
x31 = sin(dh31*pi/180)*Wind31.tr[n31+1,5]
y31 =  cos(dhead31n*pi/180)*norm.dwv31
smoothScatter(x31,y31,xlab="change orthogonal to current direction",xlim=xs,ylim=xs,
              ylab="change along current direction",asp=1,nbin=201,cex.axis=0.85,cex.lab=0.85)
hh31 = bkde2D(cbind(x31,y31),bandwidth=bw,gridsize=gs)
contour(hh31$x1,hh31$x2,log10(hh31$fhat),levels=hlev,xlim=xl,ylim=xl,asp=1,
        xlab="change orthogonal to current direction",
        ylab="change along current direction",cex.axis=0.85,cex.lab=0.85)
abline(h=0,col=gray(0.5),lty=2)
abline(v=0,col=gray(0.5),lty=2)
mtext(mlab1,side=3,line=1.5,at=-5.2,cex=0.85)
n31 = I31[Icon & day31 & Wind31.tr[,5] > wsr[1] & Wind31.tr[,5] <= wsr[2]]
dh31 = dir.dif(Wind31.tr[n31+1,6]-Wind31.tr[n31,6])
norm.dwv31 = sqrt((Wind31.tr[n31+1,7]-Wind31.tr[n31,7])^2+(Wind31.tr[n31+1,8]-Wind31.tr[n31,8])^2)
head31n =  atan2(Wind31.tr[n31+1,7]-Wind31.tr[n31,7],
                 Wind31.tr[n31+1,8]-Wind31.tr[n31,8])*180/pi
dhead31n = dir.dif(Wind31.tr[n31,6]-head31n)
x31 = sin(dh31*pi/180)*Wind31.tr[n31+1,5]
y31 =  cos(dhead31n*pi/180)*norm.dwv31
smoothScatter(x31,y31,xlab="change orthogonal to current direction",xlim=xs,ylim=xs,
              ylab="change along current direction",asp=1,nbin=201,cex.axis=0.85,cex.lab=0.85)
hh31 = bkde2D(cbind(x31,y31),bandwidth=bw,gridsize=gs)
contour(hh31$x1,hh31$x2,log10(hh31$fhat),levels=hlev,xlim=xl,ylim=xl,asp=1,
        xlab="change orthogonal to current direction",
        ylab="change along current direction",cex.axis=0.85,cex.lab=0.85)
abline(h=0,col=gray(0.5),lty=2)
abline(v=0,col=gray(0.5),lty=2)
mtext(mlab2,side=3,line=1.5,at=-5.2,cex=0.85)












#---


tbb <- wind_data |>
    mutate_daynight() |>
    mutate_east_north() |>
    filter(wspd_vec_mean <= 5, wspd_vec_mean > 1) |>
    mutate_fill_missing() |>
    mutate(lag_northerly = lag(northerly, 1),
           lag_easterly = lag(easterly, 1)) |>
    rowwise() |>
    mutate(ortho_decomp = list(ortho_decomp_row(easterly, northerly, lag_easterly, lag_northerly))) |>
    unnest_wider(ortho_decomp) |>
    select(time, hh_axis, pp_axis, wspd_vec_mean, daynight) |>
    drop_na()

mlab1="nighttime, wind speed (1,5] m/s"
mlab2="daytime, wind speed (1,5] m/s"
par(mfrow=c(2,2),mar=c(4,4,3.5,0.5))
ttp <- tbb |> filter(daynight == "N")
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

ttp <- tbb |> filter(daynight == "D")
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




wind_data |>
    mutate_east_north() |>
    mutate_daynight() |>
    plot_orthogonal_decomp()



