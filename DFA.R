library(lubridate); library(dplyr)

all <- readRDS('hud_detects.RDS')
hud <- all %>%
  filter(date.local >= '2017-03-15',
         array %in% c('Above', 'Saugerties-Coxsackie', 'Between',
                      'West Point-Newburgh', 'Below'))

# Need to find a better selection rule; but 11507 and 11480 should be dropped.
test <- hud %>%
  group_by(Transmitter) %>%
  arrange(date.local) %>%
  summarize(last(date.local),
            last(array))

hud <- hud %>%
  filter(!grepl('11480|11507', Transmitter))

# group_by(hud, Region) %>% summarize(n_distinct(Transmitter))
# library(ggplot2)
# ggplot() + geom_point(data = hud, aes(x = date.local, y = Transmitter,
#                                       color = array))


# 1) Find mean daily position. Start with mean lat/long, after proof of concept,
#    move to RKM (need to use gdistance?)
#    Need to bound the time series (if no first/last obs, set equal to Verrazno,
#    (40.605, -74.045))?? PROBABLY NOT

avg.pos <- hud %>%
  mutate(floor.date = floor_date(date.local, 'day')) %>%
  group_by(Transmitter, floor.date) %>%
  summarize(lat.avg = mean(as.numeric(lat)),
            long.avg = mean(as.numeric(long))) %>%
  arrange(floor.date)


# 2) Fill zeros. Check out methods in imputeTS. Others more appropriate?
avg.pos.spl <- split(avg.pos, avg.pos$Transmitter)

avg.pos.spl <- lapply(avg.pos.spl, function(x){
  hold <- data.frame(floor.date = seq(range(x$floor.date)[1],
                                      range(x$floor.date)[2],
                                      by = 'day'),
                     Transmitter = x$Transmitter[1])
  hold$floor.date <- ymd(hold$floor.date, tz = 'America/New_York')
  suppressWarnings(suppressMessages(
    x %>%
      full_join(hold)
  ))
})

avg.pos <- do.call(rbind, avg.pos.spl)
rm(avg.pos.spl)

library(imputeTS)
avg.pos <- avg.pos %>%
  group_by(Transmitter) %>%
  arrange(floor.date) %>%
  mutate(lat.imp = na.ma(lat.avg, k = 2),
         long.imp = na.ma(long.avg, k = 2))

ggplot(data = avg.pos) +
  geom_point(aes(x = floor.date, y = lat.imp)) +
  geom_point(aes(x = floor.date, y = lat.avg), col = 'red') +
  facet_wrap(~Transmitter)

# 3) DFA

