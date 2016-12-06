library(ggplot2); library(rgdal)

midatl <- readOGR('c:/users/secor/desktop/gis products/chesapeake/midatlantic',
                  'matl_states_land')

plot_points <- read.csv('p:/obrien/biotelemetry/hudson sb/points_2016.csv')

midatl <- fortify(midatl)

hud_s <- ggplot() + geom_polygon(data = midatl, aes(long, lat, group = group),
                                 fill = 'darkgray') +
  coord_map(xlim = c(-74.1, -73.8336), ylim = c(40.5980, 41.13725)) +
  geom_point(data = plot_points, aes(x = Long, y = Lat,
                                     shape = Type), size = 3.5) +
  theme_bw() +
  theme(axis.title = element_blank(), legend.position = 'none')
hud_sm <- ggplot() +
  annotate('rect', xmin = -74.05, xmax = -73.9,
           ymin = 41.32, ymax = 41.52, fill = 'blue') +
  geom_polygon(data = midatl, aes(long, lat, group = group),
               fill = 'darkgray') +
  coord_map(xlim = c(-74.1, -73.8336), ylim = c(41.13725, 41.67650)) +
  geom_point(data = plot_points, aes(x = Long, y = Lat,
                                     shape = Type), size = 3.5) +
  theme_bw() +
  theme(axis.title = element_blank(), legend.position = 'none')
hud_nm <- ggplot() +
  annotate('rect', xmin = -74, xmax = -73.75,
           ymin = 42.07, ymax = 42.36, fill = 'pink') +
  geom_polygon(data = midatl, aes(long, lat, group = group),
               fill = 'darkgray') +
  coord_map(xlim = c(-74.0664, -73.8), ylim = c(41.67650, 42.21575)) +
  geom_point(data = plot_points, aes(x = Long, y = Lat,
                                     shape = Type), size = 3.5) +
  theme_bw() +
  theme(axis.title = element_blank(), legend.position = 'none')
hud_n <- ggplot() +
  annotate('rect', xmin = -74, xmax = -73.75,
           ymin = 42.07, ymax = 42.36, fill = 'pink') +
  geom_polygon(data = midatl, aes(long, lat, group = group),
               fill = 'darkgray') +
  coord_map(xlim = c(-73.8914, -73.625), ylim = c(42.21575, 42.75500)) +
  geom_point(data = plot_points, aes(x = Long, y = Lat,
                                     shape = Type), size = 3.5) +
  theme_bw() +
  theme(axis.title = element_blank(), legend.position = 'none')

library(gridExtra)
grid.arrange(hud_s, hud_sm, hud_nm, hud_n,
             nrow = 1, left = 'Latitude', bottom = 'Longitude')
