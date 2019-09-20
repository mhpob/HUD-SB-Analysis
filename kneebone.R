library(dplyr)
hud_detects <- readRDS('data and imports/hud_detects.rds')

hud_detects <- hud_detects %>%
  filter(date.floor < as.Date('2019-01-01')) %>%
  mutate(year = lubridate::year(date.local),
         doy = lubridate::yday(date.local),
         array = ifelse(grepl('Ab|Be|Saug|Newb', array), 'Hudson', array),
         array = factor(array,
                        levels = c('Ches', 'VA Coast', 'MD Coast', 'DE Coast',
                                   'DE', 'NJ Coast', 'NY Coast', 'Hudson',
                                   'LI Sound', 'MA', 'ME'), ordered = T))

reduced_pts <- distinct(hud_detects, transmitter, doy, array, .keep_all = T)

library(ggplot2)
region <- ggplot() + geom_point(data = reduced_pts,
                      aes(x = array, y = doy, color = region),
                      position = position_dodge(width = 0.3)) +
  # position_dodge() only works horizontally; have to plot x on y and coord_flip
  coord_flip() +
  # then scale the y axis since it's flipped
  scale_y_continuous(expand = c(0.005, 0)) +
  scale_color_grey(start = 0.8, end = 0.2) +
  facet_wrap(~ year) +
  labs(x = NULL, y = 'Day of Year', color = "Tagging Region") +
  theme_bw() +
  theme(legend.position = c(0.02, 0.4),
        legend.justification = c(0, 1),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.background = element_rect(fill = NA),
        strip.text = element_text(size = 12),
        axis.text.y.left = element_text(angle = 35),
        axis.text.y = element_text(size = 10),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(0.2, 0.2, 0.1, 0.05), "cm"),
        panel.spacing.x = unit(0.01, "lines"))


sex <- ggplot() + geom_point(data = reduced_pts,
                      aes(x = array, y = doy, color = sex),
                      position = position_dodge(width = 0.3)) +
  # position_dodge() only works horizontally; have to plot x on y and coord_flip
  coord_flip() +
  scale_y_continuous(expand = c(0.005, 0)) +
  scale_color_grey(start = 0.8, end = 0.2) +
  facet_wrap(~ year) +
  labs(x = NULL, y = 'Day of Year', color = "Sex") +
  # scale_x_continuous(expand = c(0, 0)) +
  theme_bw() +
  theme(legend.position = c(0.02, 0.4),
        legend.justification = c(0, 1),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text.y.left = element_text(angle = 35),
        axis.text.y = element_text(size = 10),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(0, 0.2, 0.1, 0.05), "cm"),
        panel.spacing.x = unit(0.01, "lines"))



reduced_pts_len <- filter(hud_detects, variable == 'TL') %>%
  distinct(transmitter, doy, array, .keep_all = T) %>%
  mutate(size.bin = case_when(value < 80 ~ '< 80',
                              value >= 80 & value < 90 ~ '80 - 90',
                              T ~ '> 90'),
         size.bin = factor(size.bin, levels = c('< 80', '80 - 90', '> 90'),
                           ordered = T))
length <- ggplot() + geom_point(data = reduced_pts_len,
                      aes(x = array, y = doy, color = size.bin),
                      position = position_dodge(width = 0.5)) +
  # position_dodge() only works horizontally; have to plot x on y and coord_flip
  coord_flip() +
    scale_y_continuous(expand = c(0.005, 0)) +
  scale_color_grey(start = 0.8, end = 0.2,
                   guide = guide_legend(reverse = T)) +
  facet_wrap(~ year) +
  labs(x = NULL, y = 'Day of Year', color = "Total Length (cm)") +
  theme_bw() +
  theme(legend.position = c(0.02, 0.5),
        legend.justification = c(0, 1),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text.y.left = element_text(angle = 35),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        plot.margin = unit(c(0, 0.2, 0.1, 0.05), "cm"),
        panel.spacing.x = unit(0.01, "lines"))

library(cowplot)

plot_grid(region, sex, length, ncol = 1)
