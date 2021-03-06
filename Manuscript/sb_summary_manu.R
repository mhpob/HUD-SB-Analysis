# Code based off of sb_summery.R and modified for manuscript

library(ggplot2); library(data.table)

detects <- setDT(readRDS('data and imports/hud_detects.RDS'))
detects <- detects[, region := ifelse(grepl('West', region), 'Lower', 'Upper')]

data_lab <- c(
  'TL' = 'Total length (cm)',
  'FL' = 'Fork length (cm)',
  'Weight' = 'Weight (kg)'
)

plot_data <- unique(detects, by = c('transmitter', 'variable'))
plot_data <- plot_data[variable %in% c('TL', 'Weight')]

fig3 <- ggplot() +
  geom_boxplot(data = plot_data,
               aes(x = region, y = value, fill = sex)) +
  geom_hline(data = data.frame(variable = rep('TL', 2),
                               y_int =  c(71.1, 101.6)),
             aes(yintercept = y_int), linetype = 'dashed') +
  facet_wrap(~ variable, scales = 'free',
             labeller = labeller(variable = data_lab)) +
  scale_fill_manual(values = c('white', 'gray')) +
  labs(x = 'Region of tagging', y = 'Value', fill = 'Sex') +
  theme_bw() +
  theme(legend.position = c(0.63, 0.83),
        strip.background = element_blank(),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10))


ggsave("manuscript/figures/submitted/Figure3.tif", fig3,
       width = 5.2, height = 3.75, units = 'in', dpi = 600,
       device = 'tiff', compression = 'lzw')


# t-test ----
tl <- unique(detects, by = c('transmitter', 'variable'))[variable == 'TL']

t.test(tl[region == 'Lower']$value, tl[region == 'Upper']$value, var.equal = F)
t.test(tl[sex == 'Male']$value, tl[sex == 'Female']$value, var.equal = F)


wt <-unique(detects, by = c('transmitter', 'variable'))[variable == 'Weight']

t.test(wt[region == 'Lower']$value, wt[region == 'Upper']$value, var.equal = F)
t.test(wt[sex == 'Male']$value, wt[sex == 'Female']$value, var.equal = F)
