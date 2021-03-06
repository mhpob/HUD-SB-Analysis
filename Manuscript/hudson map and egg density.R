library(data.table)

eggs <- fread('p:/obrien/biotelemetry/hudson sb/sb_egg_densities.csv')

eggs <- eggs[, fdensity := egg_density/sum(.SD[,egg_density]), by = year]


lat_assign <- function(arg) {
  switch(arg,
         'Battery' = 40.787,
         'Yonkers' = 40.935,
         'TappanZee' = 41.079,
         'Croton-Haverstraw' = 41.193,
         'IndianPoint' = 41.269,
         'WestPoint' = 41.385,
         'Cornwall' = 41.488,
         'Poughkeepsie' = 41.630,
         'HydePark' = 41.772,
         'Kingston' = 41.934,
         'Saugerties' = 42.088,
         'Catskill' = 42.289,
         'Albany' = 42.549)
}


eggs <- eggs[, latitude := lat_assign(region), by = 1:nrow(eggs)]



library(ggplot2)

egg_density <- ggplot(data = eggs,
                      aes(x = fdensity, y = latitude, group = region)) +
  annotate('rect', xmin = 0, xmax = 0.75,
           ymin = 42.02, ymax = 42.33, fill = '#FF7762', alpha = 0.7) +
  annotate('rect', xmin = 0, xmax = 0.75,
           ymin = 41.35, ymax = 41.52, fill = '#7DC6D8', alpha = 0.7) +
  geom_boxplot(outlier.shape = 'circle open') +
  geom_point(shape = 'circle open') +
  labs(x = 'Fractional egg density', y = NULL) +
  scale_y_continuous(limits = c(40.59, 42.75), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 0.75), expand = c(0,0)) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.x= element_text(size = 12),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())




library(sf)

midatl <- st_read(file.path('p:/obrien/biotelemetry/past sb/past-analysis',
                            'manuscript/plos one/atlcoast.gpkg'))
hud_base <- st_crop(midatl, xmin = -74.1, xmax = -73.6, ymin = 40.59, ymax = 42.77)
plot_points <- data.table(readRDS('data and imports/hud_detects.RDS'))[
  grepl('West|Below|Betw|Sau|Above', array) & date.local < '2019-01-01']
plot_points <- unique(plot_points, by = 'station')

hudson <-
  ggplot() +
  annotate('rect', xmin = -74, xmax = -73.75,
           ymin = 42.02, ymax = 42.33, fill = '#FF7762') +
  annotate('rect', xmin = -74.05, xmax = -73.9,
           ymin = 41.35, ymax = 41.52, fill = '#7DC6D8') +
  geom_sf(data = hud_base) +
  coord_sf(xlim = c(-74.1, -73.6), ylim = c(40.59, 42.77), expand = F, clip = 'off') +
  annotate('rect', xmin = -73.95, xmax = -73.75,
           ymin = 42.02, ymax = 42.33, color = 'black', fill = NA) +
  annotate('rect', xmin = -74.05, xmax = -73.9,
           ymin = 41.35, ymax = 41.52, color = 'black', fill = NA) +
  geom_point(data = plot_points,
             aes(x = long, y = lat), size = 3.5, pch = 1) +
  annotate('text', x = -74.1, y = 40.73, label = 'NYC',
           size = 12 / .pt, hjust = 'right', fontface = 'bold') +
  annotate('text', x = -74.1, y = 41.07, label = 'TZB',
           size = 12 / .pt, hjust = 'right', fontface = 'bold') +
  annotate('text', x = -74.1, y = 41.72, label = 'USGS',
           size = 12 / .pt, hjust = 'right', fontface = 'bold') +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        panel.grid = element_blank())


rkm <- data.table(rkm = seq(0, 246, 2),
                  latitude = scales::rescale(seq(0, 246, 2), c(40.7, 42.76)))

rkm_axis <- ggplot(data = rkm, aes(y = latitude)) +
  scale_y_continuous(labels =  rkm[seq(1, 124, 20)][, rkm],
                     breaks = rkm[seq(1, 124, 20)][, latitude]) +
  coord_fixed(ratio = 50, ylim = c(40.59, 42.77), expand = F) +
  labs(y = NULL, x = NULL)+
  theme(panel.background = element_blank(),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_line())

library(patchwork)
fig1 <- rkm_axis + hudson + egg_density + plot_annotation(title = 'R-Km  Latitude')

ggsave("p:/obrien/biotelemetry/hudson sb/hud-sb-analysis/manuscript/revision/figures/Figure1.tif", fig1,
       width = 5.2, height = 5.2, units = 'in', dpi = 600,
       device = 'tiff', compression = 'lzw')
