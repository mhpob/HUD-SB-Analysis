# Data import ----
library(ggplot2); library(imputeTS); library(lubridate); library(dplyr)

all <- readRDS('hud_detects.RDS')
hud <- all %>%
  filter(date.local >= '2017-03-15',
         array %in% c('Above', 'Saugerties-Coxsackie', 'Between',
                      'West Point-Newburgh', 'Below'))

# Need to find a better selection rule; but 11507 and 11480 should be dropped
# since they probably died (last array isn't below WPT/NBGH)

hud <- hud %>%
  filter(!grepl('11480|11507', Transmitter))

# Data munging ----
# Find mean daily position. Start with mean lat, as Hudson is a linear
#    N/S system.

agg.pos <- hud %>%
  group_by(Transmitter, date.floor) %>%
  summarize(lat.avg = mean(as.numeric(lat)),
            lat.max = max(as.numeric(lat))) %>%
  arrange(date.floor)

# No padding ----
# When the fish isn't heard on a day between first and last days detected,
# insert a day with NA location. Fill this with imputed value.
agg.pos.spl <- split(agg.pos, agg.pos$Transmitter)
agg.pos.spl <- lapply(agg.pos.spl, function(x){
  hold <- data.frame(date.floor = seq(range(x$date.floor)[1],
                                      range(x$date.floor)[2],
                                      by = 'day'),
                     Transmitter = x$Transmitter[1])

  suppressWarnings(suppressMessages(
    x %>%
      full_join(hold) %>%
      arrange(date.floor) %>%
      mutate(avg.imp = na.ma(lat.avg, k = 2),
             max.imp = na.ma(lat.max, k = 2))
  ))
})

agg.pos.imp <- do.call(rbind, agg.pos.spl)

# Mean daily latitude
# ggplot() +
#   annotate('rect', xmin = as.POSIXct('2017-04-01'),
#            xmax = as.POSIXct('2017-06-24'),
#            ymin = 42.07, ymax = 42.36, fill = 'pink') +
#   annotate('rect', xmin = as.POSIXct('2017-04-01'),
#            xmax = as.POSIXct('2017-06-24'),
#            ymin = 41.32, ymax = 41.52, fill = 'lightblue') +
#   geom_point(data = agg.pos.imp, aes(x = date.floor, y = lat.max), col = 'red') +
#   facet_wrap(~Transmitter)

# Mean daily imputed latitude
# ggplot() +
#   annotate('rect', xmin = as.POSIXct('2017-04-01'),
#            xmax = as.POSIXct('2017-06-24'),
#            ymin = 42.07, ymax = 42.36, fill = 'pink') +
#   annotate('rect', xmin = as.POSIXct('2017-04-01'),
#            xmax = as.POSIXct('2017-06-24'),
#            ymin = 41.32, ymax = 41.52, fill = 'lightblue') +
#   geom_point(data = agg.pos.imp, aes(x = date.floor, y = avg.imp), col = 'black') +
#   geom_point(data = agg.pos.imp, aes(x = date.floor, y = lat.avg), col = 'red') +
#   facet_wrap(~Transmitter)


# Padded data ----
agg.pad.spl <- split(agg.pos, agg.pos$Transmitter)
hud.date.seq <- range(hud$date.floor)
hud.date.seq <- seq(hud.date.seq[1] - days(2), hud.date.seq[2] + days(2),
                    by = 'day')
agg.pad.spl <- lapply(agg.pad.spl, function(x){
  hold <- data.frame(date.floor = hud.date.seq,
                     Transmitter = x$Transmitter[1])
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
#   facet_wrap(~Transmitter)
