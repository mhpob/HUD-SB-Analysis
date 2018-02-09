
library(dtwclust)

source('perfishTS.R')

k <- reshape2::dcast(agg.pad.imp, Transmitter ~ date.floor, value.var = 'avg.imp')
row.names(k) <- k$Transmitter
k <- as.matrix(k[, -1])

k <- tslist(k)


l <- tsclust(k, k = 2, distance = 'sbd', centroid = 'shape')
plot(l)
ord <- l$order

plot.data <- agg.pos.imp %>%
  ungroup() %>%
  mutate(Transmitter = factor(Transmitter,
                              levels = levels(factor(Transmitter))[ord]))


ggplot() + geom_raster(data = plot.data, aes(x = date.floor, y = Transmitter,
                                           fill = avg.imp)) +
  scale_fill_gradient(low = 'blue', high = 'orange') +
  scale_x_datetime(limits = c(ymd_hms('2017-04-02 00:00:00'),
                              ymd_hms('2017-06-26 00:00:00'))) +
  theme_bw() +
  theme(legend.position = c(0.9, 0.2), axis.title = element_blank(),
        axis.text.y = element_blank()) +
  labs(fill = 'River km')








# kml and kmlShape (which is different from kShape...) ----
k <- reshape2::dcast(agg.pad.imp, Transmitter ~ date.floor, value.var = 'avg.imp')
row.names(k) <- k$Transmitter
k <- as.matrix(k[, -1])

library(kmlShape)
p <- cldsWide(k, id = rownames(k))

kmlclus <- kmlShape(p, 2, timeScale = 3, toPlot = 'none')
plot(kmlclus, col = 'clusters')



library(kml)

p <- cld(k, idAll = row.names(k))
kml(p, parAlgo = parALGO(startingCond = 'maxDist'), toPlot = 'both')
plot(p)

