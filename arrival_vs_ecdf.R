library(lubridate); library(dplyr)

## Import temperature data ----
# USGS stations
usgs_data <- readRDS("data and imports/usgs_wq.rds") %>%
  mutate(date.local = ymd(datetime, tz= 'America/New_York'),
         year = year(date.local)) %>%
  filter(month(date.local) %in% 3:7)

# Visual inspection
library(ggplot2)
ggplot() + geom_point(data = usgs_data, aes(x = date.local, y = mwt,
                                            color = site_name)) +
  facet_wrap(~year, scales = 'free_x', ncol = 1)

# BWT from Storm King AR unit
rec_data <- read.csv(file.path('p:/obrien/biotelemetry/hudson sb/detections',
                               'receiver logs/receiver_events_2016-2017.csv'),
                     stringsAsFactors = F)

rec_data <- rec_data %>%
  filter(Description == 'Average temperature') %>%
  mutate(date.utc = ymd_hms(Date.and.Time..UTC.),
         date.local = with_tz(date.utc, tz = 'America/New_York'),
         Data = as.numeric(Data),
         date.floor = floor_date(date.local, 'day'),
         year = year(date.local)) %>%
  filter(month(date.local) %in% 3:7,
         year == 2017) %>%
  group_by(date.floor, year) %>%
  summarize(avg.temp = mean(Data))

# Visual inspection
ggplot() +
  geom_line(data = rec_data, aes(x = date.floor, y = avg.temp), lwd = 1) +
  geom_line(data = usgs_data, aes(x = date.local, y = mwt,
                                            color = site_name), lwd = 1) +
  facet_wrap(~year, scales = 'free_x', ncol = 1) +
  labs(x = NULL, y = 'Temperature (C)') +
  theme_bw() +
  theme(legend.position = 'none')

ggplot() +
  geom_line(data = filter(usgs_data, grepl('pough', site_name)),
            aes(x = date.local, y = mdisch * 0.0283168), lwd = 1) +
  facet_wrap(~year, scales = 'free_x', ncol = 1) +
  labs(x = NULL, y = 'Discharge (cm^3 s^-1)') +
  theme_bw()

##
##
## EVERYTHIUNG BELOW THIS IS SAMPLE CODE. WILL NOT RUN WITH THESE DATA----
##
##

## Create ECDF plot of detection returns, per array ----
## Import detections ----
dets <- readRDS('data and imports/hud_detects.RDS') %>%
  filter(grepl('Above|Saug|Between|Newb|Below', array),
         date.local >= '2017-01-01',
         month(date.local) %in% 3:7) %>%
  mutate(year = year(date.local),
         date.floor = floor_date(date.local, 'day'))

# allhud <- dets %>%
#   group_by(transmitter, year) %>%
#   summarize(min = min(date.local))

river_sub <- function(area){
  dets %>%
    filter(array == area) %>%
    group_by(transmitter, year) %>%
    summarize(min = min(date.local))
}

det_ecdfplot <- function(data, year, ylab = NULL, ...){
  data <- split(data, data$year)
  data <- lapply(data, function(x){ecdf(x$min)})

  plot(x = as_datetime(knots(data[[year]])),
       y = data[[year]](knots(data[[year]])),
       ylim = c(0, 1),
       xlim = c(ymd_hms(paste0(year, '0401 00:00:00')),
                ymd_hms(paste0(year, '0615 00:00:00'))),
       xlab = 'Date',
       ylab = ifelse(is.null(ylab),
                     eval(expression(paste(
                       'Fraction detected in', year))),
                     ylab),
       pch = 16, ...)
  LLines <- function(..., log, axes, frame.plot, panel.first, panel.last) {
    lines(...)
  }
  LLines(x = as_datetime(knots(data[[year]])),
        y = data[[year]](knots(data[[year]])),
        type = 's', ...)
}


## Create temperature v ECDF plot ----
temp_ecdfplot <- function(year, subsets){
  temp_data <- usgs_data[usgs_data$year == year,]

  par(mar = c(4, 4, 1, 4) + 0.1)
  plot(x = temp_data[grepl('pough', temp_data$site_name),]$date.local,
       y = temp_data[grepl('pough', temp_data$site_name),]$mwt,
       lty = 1,
       xlim = c(ymd_hms(paste0(year, '0401 00:00:00')),
                ymd_hms(paste0(year, '0615 00:00:00'))),
       ylim = c(0, 25),
       xaxt = 'n',
       xlab = 'Date',
       ylab = 'Temperature (C)',
       type = 'l',
       lwd = 2)
  lines(x = temp_data[grepl('alb', temp_data$site_name),]$date.local,
        y = temp_data[grepl('alb', temp_data$site_name),]$mwt,
        lwd = 2, lty = 2)
  axis.POSIXct(1, at = seq(ymd_hms(paste0(year, '0401 00:00:00')),
                           ymd_hms(paste0(year, '0615 00:00:00')),
                           by = 'month'), format = '%m-%Y')

  river_sub <- function(area){
    dets %>%
      filter(array == area) %>%
      group_by(transmitter, year) %>%
      summarize(min = min(date.local))
  }

  colors4plot <- function(x){
    switch(x,
           'Below' = hcl(h = 1, l = 65, c = 100),
           'West Point-Newburgh' = hcl(h = 15, l = 65, c = 100),
           'Between' = hcl(h = 87, l = 65, c = 100),
           'Saugerties-Coxsackie' = hcl(h = 195, l = 65, c = 100),
           'Above' = hcl(h = 231, l = 65, c = 100))
  }


  for(i in seq_along(subsets)){
    par(new = T)
    det_ecdfplot(data = river_sub(subsets[i]), year = year, ylab = '', axes = F,
                 col = colors4plot(subsets[i]))

  }
  axis(4, las = 0.5, col.axis = 'blue')
  mtext(side = 4, line = 2, text = eval(expression(paste(
    'Fraction detected in', year))), col = 'blue')

}

temp_ecdfplot(year = '2018', subsets = unique(dets$array))


## ECDF v temperature? ----
allhud <- dets %>%
  group_by(transmitter, year) %>%
  summarize(min = min(date.floor))

allhud <- split(allhud, allhud$year)
allhud <- lapply(allhud, function(x){ecdf(x$min)})

j <- data.frame(date.floor = (as_datetime(knots(data[['2018']]))),
                pct = data[['2018']](knots(data[['2018']])))
k <- left_join(j, usgs_data)
