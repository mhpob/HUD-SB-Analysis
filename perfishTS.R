# Data import ----
library(ggplot2); library(imputeTS); library(lubridate); library(dplyr)

all <- readRDS('hud_detects.RDS')
# 2017 spawning season (encompassed by Mar through June)
hud17 <- all %>%
  filter(date.local >= '2017-03-15',
         date.local <= '2017-07-01',
         array %in% c('Above', 'Saugerties-Coxsackie', 'Between',
                      'West Point-Newburgh', 'Below'),
         # 11507 and 11480 should be dropped since they probably died (last array
         # isn't below WPT/NBGH)
         !grepl('11(480|507)', transmitter)) %>%
  mutate(year = '2017',
         doy = yday(date.floor))

# 2018 spawning season
hud18 <- all %>%
  filter(date.local >= '2018-03-15',
         date.local <= '2018-07-01',
         array %in% c('Above', 'Saugerties-Coxsackie', 'Between',
                      'West Point-Newburgh', 'Below'),
         # Remove those that are probably dead (1 detection or all detections at
         # one site)
         !grepl('11(424|479|494|500|505)', transmitter)) %>%
  mutate(year = '2018',
         doy = yday(date.floor))

hud <- rbind(hud17, hud18)

ggplot() + geom_point(data = distinct(hud, transmitter, doy, lat, year),
                      aes(x = doy, y = lat, color = year)) +
  facet_wrap(~ transmitter)

# Data munging ----
# Find mean daily position. Start with mean lat, as Hudson is a linear
#    N/S system.

agg.pos <- hud %>%
  group_by(transmitter, sex, region, doy, year) %>%
  summarize(lat.avg = mean(as.numeric(lat)),
            lat.max = max(as.numeric(lat))) %>%
  arrange(transmitter, doy, year)

# No padding ----
# When the fish isn't heard on a day between first and last days detected,
# insert a day with NA location. Fill this with imputed value.
agg.pos.spl <- split(agg.pos, agg.pos$transmitter)
agg.pos.spl <- lapply(agg.pos.spl, function(x) split(x, x$year))
agg.pos.spl <- lapply(agg.pos.spl, function(y){
  lapply(y, function(x){
    hold <- data.frame(doy = seq(range(x$doy)[1],
                                 range(x$doy)[2],
                                 by = 1),
                       transmitter = x$transmitter[1],
                       year = x$year[1])

    suppressWarnings(suppressMessages(
      hold <- x %>%
        full_join(hold) %>%
        arrange(doy)
    ))
    hold$avg.imp  <-  na.ma(hold$lat.avg, k = 2)
    hold$max.imp = na.ma(hold$lat.max, k = 2)
    hold
  })
})

agg.pos.imp <- lapply(agg.pos.spl, function(x) do.call(rbind, x))
agg.pos.imp <- do.call(rbind, agg.pos.imp)

# Mean daily latitude
ggplot() +
  annotate('rect', xmin = yday('2018-04-01'),
           xmax = yday('2018-07-01'),
           ymin = 42.07, ymax = 42.36, fill = 'pink') +
  annotate('rect', xmin = yday('2018-04-01'),
           xmax = yday('2018-07-01'),
           ymin = 41.32, ymax = 41.52, fill = 'lightblue') +
  geom_point(data = agg.pos.imp,
             aes(x = doy, y = lat.max, alpha = year), col = 'red') +
  facet_wrap(~ transmitter)

# Mean daily imputed latitude
# # Using a dummy DOY as work-around for month labels
agg.pos.imp$dummydoy <- (agg.pos.imp$doy - 1) + as.Date('2017-01-01')
ggplot() +
  annotate('rect', xmin = as.Date('2017-04-01'),
           xmax = as.Date('2017-07-01'),
           ymin = 42.07, ymax = 42.36, fill = 'pink') +
  annotate('rect', xmin = as.Date('2017-04-01'),
           xmax = as.Date('2017-07-01'),
           ymin = 41.32, ymax = 41.52, fill = 'lightblue') +
  geom_line(data = agg.pos.imp,
             aes(x = dummydoy, y = avg.imp, lty = year), col = 'black', lwd = 1.1) +
  geom_line(data = agg.pos.imp,
             aes(x = dummydoy, y = lat.avg, lty = year), col = 'red', lwd = 1.1) +
  labs(x = NULL, y = NULL, lty = NULL) +
  facet_wrap(~ transmitter) +
  theme_bw()


# Padded data ----
agg.pad.spl <- split(agg.pos, agg.pos$transmitter)
hud.date.seq <- range(hud$date.floor)
hud.date.seq <- seq(hud.date.seq[1] - days(2), hud.date.seq[2] + days(2),
                    by = 'day')
agg.pad.spl <- lapply(agg.pad.spl, function(x){
  hold <- data.frame(date.floor = hud.date.seq,
                     transmitter = x$transmitter[1])
  suppressWarnings(suppressMessages(
    hold <- x %>%
      full_join(hold) %>%
      arrange(date.floor) %>%
      mutate(avg.pad = lat.avg,
             max.pad = lat.max)
  ))

  hold[hold$date.floor %in%
         c(seq(min(hud.date.seq), min(x$date.floor) - days(2), by = 'day'),
           seq(max(x$date.floor) + days(2), max(hud.date.seq), by = 'day')),
       ]$avg.pad <- 40.85
  hold[hold$date.floor %in%
         c(seq(min(hud.date.seq), min(x$date.floor) - days(2), by = 'day'),
           seq(max(x$date.floor) + days(2), max(hud.date.seq), by = 'day')),
       ]$max.pad <- 40.85

  hold$avg.imp <- na.ma(hold$avg.pad, k = 2)
  hold$max.imp <- na.ma(hold$max.pad, k = 2)
  hold
})

agg.pad.imp <- do.call(rbind, agg.pad.spl)

# Mean daily imputed padded latitude
# ggplot() +
#   annotate('rect', xmin = as.POSIXct('2017-04-01'),
#            xmax = as.POSIXct('2017-06-24'),
#            ymin = 42.07, ymax = 42.36, fill = 'pink') +
#   annotate('rect', xmin = as.POSIXct('2017-04-01'),
#            xmax = as.POSIXct('2017-06-24'),
#            ymin = 41.32, ymax = 41.52, fill = 'lightblue') +
#   geom_point(data = agg.pad.imp, aes(x = date.floor, y = avg.imp), col = 'black') +
#   geom_point(data = agg.pad.imp, aes(x = date.floor, y = lat.avg), col = 'red') +
#   facet_wrap(~ transmitter)


# Remove some things to clear up memory when being sourced ----
rm(agg.pad.spl, agg.pos, agg.pos.spl, all, hud, hud.date.seq)
