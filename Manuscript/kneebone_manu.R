library(dplyr)
hud_detects <- readRDS('data and imports/hud_detects.rds')

hud_detects <- hud_detects %>%
  left_join(readRDS('data and imports/recat_spawning_region.rds')) %>%
  filter(date.floor < lubridate::ymd('2019-01-01', tz = 'America/New_York')) %>%
  mutate(year = lubridate::year(date.local),
         doy = lubridate::yday(date.local),
         array = ifelse(grepl('Ab|Be|Saug|Newb', array), 'Hudson', array),
         array = factor(array,
                        levels = c('Ches', 'VA Coast', 'MD Coast', 'DE Coast',
                                   'DE', 'NJ Coast', 'NY Coast', 'Hudson',
                                   'LI Sound', 'MA', 'ME'), ordered = T))

reduced_pts <- distinct(hud_detects, transmitter, doy, array, .keep_all = T) %>%
  mutate(recat_region = factor(recat_region,
                               levels = c('Other',
                                          'West Point-Newburgh',
                                          'Saugerties-Coxsackie'),
                               ordered = T))

library(ggplot2)
ggplot() + geom_point(data = filter(reduced_pts,
                                              !is.na(recat_region)),
                                aes(x = array, y = doy, color = recat_region),
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
  theme(legend.position = c(0.02, 0.5),
        legend.justification = c(0, 1),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.background = element_rect(fill = NA),
        strip.text = element_text(size = 12),
        axis.text.y.left = element_text(angle = 35),
        axis.text.y = element_text(size = 10),
        axis.title.x=element_text(size = 12),
        axis.text.x=element_text(size = 12),
        strip.background.x = element_blank(),
        strip.text.x = element_text(size = 16),
        plot.margin = unit(c(0.2, 0.2, 0.1, 0.05), "cm"),
        panel.spacing.x = unit(0.01, "lines"))