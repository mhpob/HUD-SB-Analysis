library(dplyr)
hud_detects <- readRDS('data and imports/hud_detects.rds')

recats <- read.csv('manuscript/recategorized_rkm.csv')
recats <- recats %>%
  mutate(pred18 = factor(pred18, levels = c('Upper', 'Lower'), ordered = T))

hud_detects <- hud_detects %>%
  left_join(recats, by = 'transmitter') %>%
  filter(date.floor < lubridate::ymd('2019-01-01', tz = 'America/New_York')) %>%
  mutate(cluster17 = ifelse(is.na(cluster17), 'Other', cluster17),
         cluster17 = factor(cluster17, levels = c('Other', 'Lower', 'Upper'), ordered = T),
         year = lubridate::year(date.local),
         doy = lubridate::yday(date.local),
         array = case_when(grepl('Ab|Be|Saug|Newb', array) ~ 'Hudson',
                           grepl('N[JY]', array) ~ 'NYB',
                           array == 'Ches' ~ 'CH',
                           array == 'LI Sound' ~ 'LIS',
                           T ~ gsub(' .*', '', array)),
         array = factor(array,
                        levels = c('CH', 'VA', 'MD', 'DE', 'Hudson', 'NYB',
                                   'LIS', 'MA', 'ME'), ordered = T))

reduced_pts <- distinct(hud_detects, cluster17, date.floor, array, .keep_all = T)

library(ggplot2)
kneebone <-
  ggplot() +
  geom_point(data = reduced_pts,
             aes(x = array, y = date.floor, color = cluster17),
             position = position_dodge(width = 0.5)) +
  # position_dodge() only works horizontally; have to plot x on y and coord_flip
  coord_flip() +
  # then scale the y axis since it's flipped
  scale_y_datetime(limits = c(lubridate::ymd('2016-01-01', tz = 'America/New_York'),
                              lubridate::ymd('2018-12-31', tz = 'America/New_York')),
                   expand = c(0,7*24*3600)) +
  scale_color_manual(values = c('gray', '#7DC6D8','#FF7762'),
                     guide = guide_legend(reverse = T)) +
  labs(x = NULL, y = NULL, color = "2017 Spawning region") +
  theme_bw() +
  theme(legend.position = c(0.02, 0.4),
        legend.justification = c(0, 1),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.text.y = element_text(size = 8),
        axis.title.x=element_text(size = 10),
        axis.text.x=element_text(size = 8),
        plot.margin = margin(0, 7, 0, 0))





library(ragg)
agg_tiff("p:/obrien/biotelemetry/hudson sb/hud-sb-analysis/manuscript/revision/figures/Figure7.tif",
         width = 7.5, height = 3.5, units = 'in', res = 600, compression = 'lzw')
fig7

dev.off()
