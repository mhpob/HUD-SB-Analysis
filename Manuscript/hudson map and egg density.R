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
           ymin = 42.07, ymax = 42.36, fill = 'pink') +
  annotate('rect', xmin = 0, xmax = 0.75,
           ymin = 41.32, ymax = 41.52, fill = 'lightblue') +
  geom_boxplot(outlier.shape = 'circle open') +
  geom_point(shape = 'circle open') +
  labs(x = 'Fractional egg density', y = NULL) +
  scale_y_continuous(limits = c(40.59, 42.75), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 0.75), expand = c(0,0)) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.title.x= element_text(size = 15),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(), panel.ontop = T)




library(sf)

midatl <- st_read(file.path('p:/obrien/biotelemetry/past sb/past-analysis',
                            'manuscript/plos one/atlcoast.gpkg'))
hud_base <- st_crop(midatl, xmin = -74.1, xmax = -73.6, ymin = 40.59, ymax = 42.77)
plot_points <- fread('p:/obrien/biotelemetry/hudson sb/hudsonpoints.csv')

hudson <-
  ggplot() +
  annotate('rect', xmin = -74, xmax = -73.75,
           ymin = 42.07, ymax = 42.36, fill = 'pink') +
  annotate('rect', xmin = -74.05, xmax = -73.9,
           ymin = 41.32, ymax = 41.52, fill = 'lightblue') +
  geom_sf(data = hud_base) +
  coord_sf(xlim = c(-74.1, -73.6), ylim = c(40.59, 42.77), expand = F, clip = 'off') +
  annotate('rect', xmin = -73.95, xmax = -73.75,
           ymin = 42.07, ymax = 42.36, color = 'black', fill = NA) +
  annotate('rect', xmin = -74.05, xmax = -73.9,
           ymin = 41.32, ymax = 41.52, color = 'black', fill = NA) +
  geom_point(data = plot_points[Type == 'Receiver'],
             aes(x = Long, y = Lat), size = 3.5, pch = 1) +
  annotate('text', x = -74.1, y = 40.73, label = 'NYC',
  size = 18 / .pt, hjust = 'right', fontface = 'bold') +
  annotate('text', x = -74.1, y = 40.85, label = 'GWB',
           size = 18 / .pt, hjust = 'right', fontface = 'bold') +
  annotate('text', x = -74.1, y = 41.72, label = 'USGS',
           size = 18 / .pt, hjust = 'right', fontface = 'bold') +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        panel.grid = element_blank())


rkm <- data.table(rkm = seq(0, 246, 2),
                  latitude = scales::rescale(seq(0, 246, 2), c(40.7, 42.76)))

rkm_axis <- ggplot(data = rkm, aes(y = latitude)) +
  scale_y_continuous(labels =  rkm[seq(1, 124, 40)][, rkm],
                     breaks = rkm[seq(1, 124, 40)][, latitude]) +
  coord_fixed(ratio = 50, ylim = c(40.59, 42.77), expand = F) +
  labs(y = NULL, x = NULL)+
  theme(panel.background = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.y = element_line())

library(patchwork)
rkm_axis + hudson + egg_density + plot_annotation(title = 'R-Km  Latitude')
