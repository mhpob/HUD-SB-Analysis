library(ggplot2); library(sf)

midatl <- st_read(file.path('p:/obrien/biotelemetry/past sb/past-analysis',
                            'manuscript/plos one/atlcoast.gpkg'))

plot_points <- read.csv('p:/obrien/biotelemetry/hudson sb/hudsonpoints.csv')

hud_s <- ggplot() + geom_sf(data = midatl, fill = 'darkgray') +
  coord_sf(xlim = c(-74.1, -73.8336), ylim = c(40.5980, 41.13725), expand = F) +
  scale_x_continuous(breaks = c(-74.0, -73.9)) +
  geom_point(data = plot_points, aes(x = Long, y = Lat,
                                     shape = Type), size = 3.5, stroke = 1.5) +
  scale_shape(solid = F) +
  theme_bw() +
  theme(axis.title = element_blank(), legend.position = 'none',
        panel.grid.major = element_line(color = 'white'))
hud_sm <- ggplot() +
  annotate('rect', xmin = -74.05, xmax = -73.9,
           ymin = 41.32, ymax = 41.52, fill = 'lightblue') +
  geom_sf(data = midatl, fill = 'darkgray') +
  coord_sf(xlim = c(-74.1, -73.8336), ylim = c(41.13725, 41.67650), expand = F) +
  scale_x_continuous(breaks = c(-74.0, -73.9)) +
  geom_point(data = plot_points, aes(x = Long, y = Lat,
                                     shape = Type), size = 3.5, stroke = 1.5) +
  scale_shape(solid = F) +
  theme_bw() +
  theme(axis.title = element_blank(), legend.position = 'none',
        panel.grid.major = element_line(color = 'white'))
hud_nm <- ggplot() +
  annotate('rect', xmin = -74, xmax = -73.75,
           ymin = 42.07, ymax = 42.36, fill = 'pink') +
  geom_sf(data = midatl, fill = 'darkgray') +
  coord_sf(xlim = c(-74.0664, -73.8), ylim = c(41.67650, 42.21575), expand = F) +
  scale_x_continuous(breaks = c(-74.0, -73.9)) +
  geom_point(data = plot_points, aes(x = Long, y = Lat,
                                     shape = Type), size = 3.5, stroke = 1.5) +
  scale_shape(solid = F) +
  theme_bw() +
  theme(axis.title = element_blank(), legend.position = 'none',
        panel.grid.major = element_line(color = 'white'))
hud_n <- ggplot() +
  annotate('rect', xmin = -74, xmax = -73.75,
           ymin = 42.07, ymax = 42.36, fill = 'pink') +
  geom_sf(data = midatl, fill = 'darkgray') +
  coord_sf(xlim = c(-73.8914, -73.625), ylim = c(42.21575, 42.75500), expand = F) +
  scale_x_continuous(breaks = c(-73.8, -73.7)) +
  geom_point(data = plot_points, aes(x = Long, y = Lat,
                                     shape = Type), size = 3.5, stroke = 1.5) +
  scale_shape(solid = F) +
  theme_bw() +
  theme(axis.title = element_blank(), legend.position = 'none',
        panel.grid.major = element_line(color = 'white'))

library(gridExtra)
grid.arrange(hud_n, hud_nm, hud_sm, hud_s, nrow = 1)
