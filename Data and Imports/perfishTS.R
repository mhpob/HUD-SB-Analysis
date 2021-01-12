# Data import ----
library(ggplot2); library(imputeTS); library(lubridate); library(dplyr)

all <- readRDS('data and imports/hud_detects.RDS')
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


rkm <- read.csv('data and imports/rkms.csv')

hud <- left_join(hud, rkm, by = c('station', 'lat', 'long'))



# ggplot() + geom_point(data = distinct(hud, transmitter, doy, rkm, year),
#                       aes(x = doy, y = rkm, color = year)) +
#   facet_wrap(~ transmitter)

# Data munging ----
# Find mean daily position. Start with mean lat, as Hudson is a linear
#    N/S system.

agg.pos <- hud %>%
  group_by(transmitter, sex, region, doy, year) %>%
  summarize(rkm.avg = mean(as.numeric(rkm)),
            rkm.max = max(as.numeric(rkm))) %>%
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
    hold$avg.imp  <-  na_ma(hold$rkm.avg, k = 2)
    hold$max.imp = na_ma(hold$rkm.max, k = 2)
    hold
  })
})

agg.pos.imp <- lapply(agg.pos.spl, function(x) do.call(rbind, x))
agg.pos.imp <- do.call(rbind, agg.pos.imp)


# Mean/SD of number of non-imputed values
agg.pos.imp %>%
  filter(!is.na(rkm.avg)) %>%
  group_by(transmitter, year) %>%
  summarize(count = n()) %>%
  group_by(year) %>%
  summarize(mean(count),
            sd(count))


# Mean/SD of number of imputed values
agg.pos.imp %>%
  filter(is.na(rkm.avg)) %>%
  group_by(transmitter, year) %>%
  summarize(count = n()) %>%
  group_by(year) %>%
  summarize(mean(count),
            sd(count))

# Mean daily latitude
# ggplot() +
#   annotate('rect', xmin = yday('2018-04-01'),
#            xmax = yday('2018-07-01'),
#            ymin = 42.07, ymax = 42.36, fill = 'pink') +
#   annotate('rect', xmin = yday('2018-04-01'),
#            xmax = yday('2018-07-01'),
#            ymin = 41.32, ymax = 41.52, fill = 'lightblue') +
#   geom_point(data = agg.pos.imp,
#              aes(x = doy, y = rkm.max, alpha = year), col = 'red') +
#   facet_wrap(~ transmitter)
#
# # Mean daily imputed latitude
# # # Using a dummy DOY as work-around for month labels
agg.pos.imp$dummydoy <- (agg.pos.imp$doy - 1) + as.Date('2017-01-01')
# ggplot() +
#   annotate('rect', xmin = as.Date('2017-04-01'),
#            xmax = as.Date('2017-07-01'),
#            ymin = 42.07, ymax = 42.36, fill = 'pink') +
#   annotate('rect', xmin = as.Date('2017-04-01'),
#            xmax = as.Date('2017-07-01'),
#            ymin = 41.32, ymax = 41.52, fill = 'lightblue') +
#   geom_line(data = agg.pos.imp,
#              aes(x = dummydoy, y = avg.imp, lty = year), col = 'black', lwd = 1.1) +
#   geom_line(data = agg.pos.imp,
#              aes(x = dummydoy, y = rkm.avg, lty = year), col = 'red', lwd = 1.1) +
#   labs(x = NULL, y = NULL, lty = NULL) +
#   facet_wrap(~ transmitter) +
#   theme_bw()


# Padded data ----
agg.pad.spl <- split(agg.pos, agg.pos$transmitter)
agg.pad.spl <- lapply(agg.pad.spl, function(x) split(x, x$year))
hud.doy.seq <- range(hud$doy)
hud.doy.seq <- seq(hud.doy.seq[1] - 2, hud.doy.seq[2] + 2, by = 1)
agg.pad.spl <- lapply(agg.pad.spl, function(y){
  lapply(y, function(x){
    hold <- data.frame(doy = hud.doy.seq,
                       transmitter = x$transmitter[1],
                       year = x$year[1])
    suppressWarnings(suppressMessages(
      hold <- x %>%
        full_join(hold) %>%
        arrange(doy) %>%
        mutate(avg.pad = rkm.avg,
               max.pad = rkm.max)
    ))

    hold[hold$doy %in%
           c(seq(min(hud.doy.seq), min(x$doy) - 2, by = 1),
             seq(max(x$doy) + 2, max(hud.doy.seq), by = 1)),
         ]$avg.pad <- 35
    hold[hold$doy %in%
           c(seq(min(hud.doy.seq), min(x$doy) - 2, by = 1),
             seq(max(x$doy) + 2, max(hud.doy.seq), by = 1)),
         ]$max.pad <- 35

    hold$avg.imp <- na_ma(hold$avg.pad, k = 2)
    hold$max.imp <- na_ma(hold$max.pad, k = 2)
    hold
  })
})

agg.pad.imp <- lapply(agg.pad.spl, function(x) do.call(rbind, x))
agg.pad.imp <- do.call(rbind, agg.pad.imp)

# Mean daily imputed padded rkmitude
# ggplot() +
#   annotate('rect', xmin = yday('2017-04-01'),
#            xmax = yday('2017-06-24'),
#            ymin = 42.07, ymax = 42.36, fill = 'pink') +
#   annotate('rect', xmin = yday('2017-04-01'),
#            xmax = yday('2017-06-24'),
#            ymin = 41.32, ymax = 41.52, fill = 'lightblue') +
#   geom_point(data = agg.pad.imp, aes(x = doy, y = avg.imp, group = year),
#              col = 'black') +
#   geom_point(data = agg.pad.imp, aes(x = doy, y = rkm.avg, group = year),
#              col = 'red') +
#   facet_wrap(~ transmitter)


# Remove some things to clear up memory when being sourced ----
rm(agg.pad.spl, agg.pos, agg.pos.spl, all, hud, hud.doy.seq)
