library(lubridate); library(dplyr)

## Import detections ----
dets <- readRDS('data and imports/hud_detects.RDS') %>%
  filter(grepl('Above|Saug|Between|Newb|Below', array))


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
  facet_wrap(~year, scales = 'free_x')

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
  group_by(date.floor) %>%
  summarize(avg.temp = mean(Data))

# Visual inspection
ggplot() + geom_point(data = rec_data2, aes(x = date.floor, y = avg.temp))


##
##
## EVERYTHIUNG BELOW THIS IS SAMPLE CODE. WILL NOT RUN WITH THESE DATA----
##
##

## Create ECDF plot of detection returns, per array ----
species_all <- species %>%
  group_by(array, Common.Name, transmitter) %>%
  distinct(array, transmitter, .keep_all = T) %>%
  summarize(min = min(date.utc))

det_ecdfplot <- function(spec.plot, array, ylab = NULL, ...){

  data <- filter(species_all, grepl(spec.plot, Common.Name, ignore.case = T))
  data <- split(data, data$array)
  data <- lapply(data, function(x){ecdf(x$min)})

  plot(x = as_datetime(knots(data[[array]])),
       y = data[[array]](knots(data[[array]])),
       ylim = c(0, 1),
       xlim = c(ymd_hms('20161111 19:00:00'),
                ymd_hms('20170328 21:00:00')),
       xlab = 'Date',
       ylab = ifelse(is.null(ylab),
                     eval(expression(paste(
                       'Fraction detected in', array, 'array'))),
                     ylab),
       pch = 16, ...)

  # This is just to stop lines() from throwing warnings when we specify
  # graphical parameters it doesn't like.
  LLines <- function(..., log, axes, frame.plot, panel.first, panel.last) {
    lines(...)
  }
  LLines(x = as_datetime(knots(data[[array]])),
         y = data[[array]](knots(data[[array]])),
         type = 's', ...)
}

det_ecdfplot(spec.plot = 'sturg', array = 'MD WEA')

## Create temperature v ECDF plot ----
temp_ecdfplot <- function(array, spec.data){
  temp.data <- rec.data[rec.data$array == array,]

  par(mar = c(4, 4, 1, 4) + 0.1)
  plot(x = temp.data$Date.Time,
       y = temp.data$avg.temp,
       xlim = c(ymd_hms('20161111 19:00:00'),
                ymd_hms('20170328 21:00:00')),
       ylim = c(4.5, 16.5),
       xaxt = 'n',
       xlab = 'Date',
       ylab = 'Temperature (C)',
       type = 'l',
       lwd = 2)
  axis.POSIXct(1, at = seq(ymd('2016-11-11'),
                           ymd('2017-03-28'),
                           by = 'month'), format = '%m-%Y')

  par(new = T)
  det_ecdfplot(spec.plot = spec.data, array = array, ylab = '', axes = F,
               col = 'blue')
  axis(4, las = 0.5, col.axis = 'blue')
  mtext(side = 4, line = 2, text = eval(expression(paste(
    'Fraction detected in', array, 'array'))), col = 'blue')
}

temp_ecdfplot(array = 'MD WEA', spec.data = 'sturg')


## Same thing, but for season/array combinations ----
# Edit detection data
sp_season <- species %>%
  mutate(year = ifelse(month(date.local) %in% 10:12 |
                         (month(date.local) == 9 & day(date.local) %in% 16:30),
                       year(date.local) - 1999, year(date.local) - 2000),
         season = ifelse(month(date.local) %in% 4:8 |
                           (month(date.local) == 3 & day(date.local) %in% 16:31) |
                           (month(date.local) == 9 & day(date.local) %in% 1:15),
                         'SprSum', 'AutWin'),
         season = paste0(season, year)) %>%
  group_by(array, Common.Name, transmitter, season) %>%
  distinct(array, transmitter, .keep_all = T) %>%
  summarize(min = min(date.local))

# det_ecdfplot <- function(data, spec.plot, array,
#                          season.plot = NULL, ylab = NULL, ...){
#
#   data <- filter(data,
#                  grepl(spec.plot, Common.Name, ignore.case = T),
#                  season == season.plot)
#   if(is.null(season.plot)){
#     data <- split(data, data$array)
#   } else{
#     data <- split(data, list(data$array, data$season))
#   }
#   data <- lapply(data, function(x){ecdf(x$min)})
#
#   date_lims <- if(grepl('Aut', season.plot)){
#     ymd_hms(c(paste0('20', as.numeric(substr(season.plot, 7, 8)) - 1,
#                      '1101 00:00:00'),
#               paste0('20', substr(season.plot, 7, 8), '0331 23:59:59')),
#             tz = 'America/New_York')
#   } else {
#     ymd_hms(c(paste0('20', substr(season.plot, 7, 8), '0401 00:00:00'),
#               paste0('20', substr(season.plot, 7, 8), '0930 23:59:59')),
#             tz = 'America/New_York')
#   }
#
#   subset <- grep(array, names(data))
#
#   plot(x = as_datetime(knots(data[[subset]])),
#        y = data[[subset]](knots(data[[subset]])),
#        ylim = c(0, 1),
#        xlim = date_lims,
#        xlab = 'Date',
#        ylab = ifelse(is.null(ylab),
#                      eval(expression(paste(
#                        'Fraction detected in', array, 'array,', season.plot))),
#                      ylab),
#        pch = 16, ...)
#
#   # This is just to stop lines() from throwing warnings when we specify
#   # graphical parameters it doesn't like.
#   LLines <- function(..., log, axes, frame.plot, panel.first, panel.last) {
#     lines(...)
#   }
#   LLines(x = as_datetime(knots(data[[subset]])),
#          y = data[[subset]](knots(data[[subset]])),
#          type = 's', ...)
# }
# det_ecdfplot(data = sp_season, 'str', 'MD WEA', season.plot = 'SprSum17')
#
# temp_ecdfplot <- function(spec.data, array, spec.plot, season.plot){
#   temp.data <- rec.data[rec.data$array == array &
#                           rec.data$season == season.plot,]
#
#   date_lims <- if(grepl('Aut', season.plot)){
#     ymd_hms(c(paste0('20', as.numeric(substr(season.plot, 7, 8)) - 1,
#                      '1001 00:00:00'),
#               paste0('20', substr(season.plot, 7, 8), '0331 23:59:59')),
#             tz = 'America/New_York')
#   } else {
#     ymd_hms(c(paste0('20', substr(season.plot, 7, 8), '0401 00:00:00'),
#               paste0('20', substr(season.plot, 7, 8), '0930 23:59:59')),
#             tz = 'America/New_York')
#   }
#
#   par(mar = c(4, 4, 1, 4) + 0.1)
#   plot(x = temp.data$Date.Time,
#        y = temp.data$avg.temp,
#        xlim = date_lims,
#        # ylim = c(4.5, 16.5),
#        xaxt = 'n',
#        xlab = 'Date',
#        ylab = 'Temperature (C)',
#        type = 'l',
#        lwd = 2)
#   axis.POSIXct(1, at = seq(date_lims[1], date_lims[2], by = 'month'),
#                format = '%m-%Y')
#
#   par(new = T)
#   det_ecdfplot(data = spec.data, spec.plot = spec.plot, array = array,
#                season.plot = season.plot, ylab = '', axes = F, col = 'blue')
#   axis(4, las = 0.5, col.axis = 'blue')
#   mtext(side = 4, line = 2, text = eval(expression(paste(
#     'Fraction detected in', array, 'array,', season.plot))), col = 'blue')
# }
# temp_ecdfplot(spec.data = sp_season, array = 'MD WEA',
#               spec.plot = 'str', season.plot = 'AutWin17')
# temp_ecdfplot(spec.data = sp_season, array = 'MD WEA',
#               spec.plot = 'str', season.plot = 'SprSum17')
# temp_ecdfplot(spec.data = sp_season, array = 'MD WEA',
#               spec.plot = 'str', season.plot = 'AutWin18')


d_ecdfplot <- function(data, spec.plot, array,
                       season.plot = NULL, ylab = NULL, ...){

  data <- filter(data,
                 grepl(spec.plot, Common.Name, ignore.case = T),
                 season == season.plot)
  if(is.null(season.plot)){
    data <- split(data, data$array)
  } else{
    data <- split(data, list(data$array, data$season))
  }
  data <- lapply(data, function(x){ecdf(x$min)})

  subset <- grep(array, names(data))

  plot(x = as_datetime(knots(data[[subset]])),
       y = data[[subset]](knots(data[[subset]])),
       ylim = c(0, 1),
       xlab = '',
       ylab = ifelse(is.null(ylab),
                     eval(expression(paste(
                       'Fraction detected in', array, 'array,', season.plot))),
                     ylab),
       pch = 16, ...)

  # This is just to stop lines() from throwing warnings when we specify
  # graphical parameters it doesn't like.
  LLines <- function(..., log, axes, frame.plot, panel.first, panel.last) {
    lines(...)
  }
  LLines(x = as_datetime(knots(data[[subset]])),
         y = data[[subset]](knots(data[[subset]])),
         type = 's', ...)
}

t_ecdfplot <- function(spec.data, array, spec.plot){
  temp.data <- rec.data[rec.data$array == array,]

  date_lims <- ymd(c('20161101', '20180101'), tz = 'America/New_York')

  par(mar = c(3.5, 3.5, 1, 3.5))
  plot(x = temp.data$Date.Time,
       y = temp.data$avg.temp,
       xlim = c(date_lims[1], date_lims[2]),
       xaxt = 'n',
       xlab = 'Date',
       ylab = 'Temperature (C)',
       type = 'l',
       lwd = 2,
       mgp = c(1.6, 0.6, 0))
  abline(v = as.POSIXct('2017-03-16'), col = 'red')
  abline(v = as.POSIXct('2017-09-16'), col = 'red')
  axis.POSIXct(1, at = seq(date_lims[1], date_lims[2], by = 'month'),
               format = '%m-%Y', mgp = c(1.6, 0.6, 0))

  par(new = T)
  d_ecdfplot(data = spec.data, spec.plot = spec.plot, array = array,
             season.plot = 'AutWin17', ylab = '', axes = F, col = 'deepskyblue4',
             xlim = c(date_lims[1], date_lims[2]))
  par(new = T)
  d_ecdfplot(data = spec.data, spec.plot = spec.plot, array = array,
             season.plot = 'SprSum17', ylab = '', axes = F, col = 'orangered',
             xlim = c(date_lims[1], date_lims[2]))
  par(new = T)
  d_ecdfplot(data = spec.data, spec.plot = spec.plot, array = array,
             season.plot = 'AutWin18', ylab = '', axes = F, col = 'deepskyblue4',
             xlim = c(date_lims[1], date_lims[2]))
  axis(4, col.axis = 'blue')
  mtext(side = 4, line = 2, mgp = c(1.6, 0.6, 0), text = eval(expression(paste(
    'Fraction detected in', array, 'array'))), col = 'blue')
}

t_ecdfplot(spec.data = sp_season, array = 'Inner', spec.plot = 'c stur')
t_ecdfplot(spec.data = sp_season, array = 'MD WEA', spec.plot = 'c stur')
t_ecdfplot(spec.data = sp_season, array = 'Outer', spec.plot = 'c stur')

