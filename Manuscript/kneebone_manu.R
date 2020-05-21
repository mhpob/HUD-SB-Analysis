library(dplyr)
hud_detects <- readRDS('data and imports/hud_detects.rds')

recats <- read.csv('manuscript/recategorized.csv')
recats <- recats %>%
  mutate(
         pred18 = factor(pred18, levels = c('Upper', 'Lower'), ordered = T))

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

reduced_pts <- distinct(hud_detects, transmitter, doy, array, .keep_all = T)

library(ggplot2)
ggplot() + geom_point(data = reduced_pts,
                                aes(x = array, y = doy, color = cluster17),
                                position = position_dodge(width = 0.3)) +
  # position_dodge() only works horizontally; have to plot x on y and coord_flip
  coord_flip() +
  # then scale the y axis since it's flipped
  scale_y_continuous(expand = c(0.005, 0)) +
  scale_color_grey(start = 0.8, end = 0.2,
                   guide = guide_legend(reverse = T)) +
  facet_wrap(~ year) +
  labs(x = NULL, y = 'Day of Year', color = "2017 Spawning Region") +
  theme_bw() +
  theme(legend.position = c(0.02, 0.3),
        legend.justification = c(0, 1),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.background = element_rect(fill = NA),
        strip.text = element_text(size = 12),
        # axis.text.y.left = element_text(angle = 35),
        axis.text.y = element_text(size = 10),
        axis.title.x=element_text(size = 12),
        axis.text.x=element_text(size = 12),
        strip.background.x = element_blank(),
        strip.text.x = element_text(size = 16),
        plot.margin = unit(c(0.2, 0.2, 0.1, 0.05), "cm"),
        panel.spacing.x = unit(0.01, "lines"))
