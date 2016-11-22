library(ggplot2); library(rgdal)

midatl <- readOGR('c:/users/secor/desktop/gis products/chesapeake/midatlantic',
                  'matl_states_land')

plot_points <- read.csv('p:/obrien/biotelemetry/hudson sb/plotting points.csv')

midatl <- fortify(midatl)

hud_s <- ggplot() + geom_polygon(data = midatl, aes(long, lat, group = group),
                                 fill = 'darkgray') +
  coord_map(xlim = c(-74.1, -73.8336), ylim = c(40.5980, 41.1545)) +
  geom_point(data = plot_points, aes(x = Long, y = Lat,
                                     color = Owner, shape = Owner)) +
  theme_bw() +
  theme(axis.title = element_blank(), legend.position = 'none')
hud_sm <- ggplot() +
  annotate('rect', xmin = -74.05, xmax = -73.9,
           ymin = 41.32, ymax = 41.52, fill = 'blue') +
  geom_polygon(data = midatl, aes(long, lat, group = group),
               fill = 'darkgray') +
  coord_map(xlim = c(-74.1, -73.8336), ylim = c(41.1545, 41.711)) +
  geom_point(data = plot_points, aes(x = Long, y = Lat,
                                     color = Owner, shape = Owner)) +
  theme_bw() +
  theme(axis.title = element_blank(), legend.position = 'none')
hud_nm <- ggplot() +
  annotate('rect', xmin = -74, xmax = -73.75,
           ymin = 42.07, ymax = 42.36, fill = 'pink') +
  geom_polygon(data = midatl, aes(long, lat, group = group),
               fill = 'darkgray') +
  coord_map(xlim = c(-74.0176, -73.7512), ylim = c(41.711, 42.2675)) +
  geom_point(data = plot_points, aes(x = Long, y = Lat,
                                     color = Owner, shape = Owner)) +
  theme_bw() +
  theme(axis.title = element_blank(), legend.position = 'none')
hud_n <- ggplot() +
  annotate('rect', xmin = -74, xmax = -73.75,
           ymin = 42.07, ymax = 42.36, fill = 'pink') +
  geom_polygon(data = midatl, aes(long, lat, group = group),
               fill = 'darkgray') +
  coord_map(xlim = c(-73.8338, -73.5674), ylim = c(42.2675, 42.8240)) +
  geom_point(data = plot_points, aes(x = Long, y = Lat,
                                     color = Owner, shape = Owner)) +
  theme_bw() +
  theme(axis.title = element_blank(), legend.position = 'none')

library(gridExtra)
grid.arrange(hud_s, hud_sm, hud_nm, hud_n,
             nrow = 1, left = 'Latitude', bottom = 'Longitude')
