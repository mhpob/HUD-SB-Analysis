library(dtwclust)

source('perfishTS.R')

# agg.pad.imp <- agg.pad.imp %>%
#   group_by(Transmitter) %>%
#   mutate(avg.scale = scale(avg.imp, center = 40.85, scale = sd(avg.imp)))
# test <- split(agg.pad.imp, agg.pad.imp$Transmitter)

# Put time series into rows ----
r_series <- reshape2::dcast(agg.pad.imp, Transmitter ~ date.floor, value.var = 'avg.imp')
row.names(r_series) <- r_series$Transmitter
r_series <- as.matrix(r_series[, -1])


clus.eval <- function(data, n, dist, cent, window){
  clus <- tsclust(series = data, k = n, distance = dist, centroid = cent,
                  window.size = window, trace = T,
                  control = partitional_control(pam.precompute = FALSE,
                                                iter.max = 500))

  print(clus)

  plot(clus) + ylim(40.8, 42.75) +
    annotate('rect', xmin = 0, xmax = 86,
             ymin = 42.07, ymax = 42.36, fill = 'pink', alpha = 0.4) +
    annotate('rect', xmin = 0, xmax = 86,
             ymin = 41.32, ymax = 41.52, fill = 'lightblue', alpha = 0.4)

  clus
}

c2 <- clus.eval(r_series, 2, 'dtw_basic', 'median', '7')
c3 <- clus.eval(r_series, 3, 'dtw_basic', 'median', '7')
c4 <- clus.eval(r_series, 4, 'dtw_basic', 'median', '7')
c5 <- clus.eval(r_series, 5, 'dtw_basic', 'median', '7')

sapply(list(K2 = c2, K3 = c3, K4 = c4, K5 = c5), cvi,
       type = 'internal')

cents <- data.frame(cent = rep(c('Centroid 1', 'Centroid 2'), each = 86),
                    value = c(c2@centroids[[1]], c2@centroids[[2]]),
                    date = rep(unique(agg.pad.imp$date.floor), times = 2))

TS <- data.frame(TS = do.call(c, c2@datalist),
                 trans = rep(names(c2@datalist), each = 86),
                 date = rep(unique(agg.pad.imp$date.floor), times = 66),
                 cent = paste0('Centroid ', rep(c2@cluster, each = 86)))

ggplot(cents) +
  facet_wrap(~cent) + ylim(40.8, 42.75) +
  annotate('rect', xmin = ymd_hms('2017-04-02 00:00:00'),
           xmax = ymd_hms('2017-06-24 00:00:00'),
           ymin = 42.07, ymax = 42.36, fill = 'pink') +
  annotate('rect', xmin = ymd_hms('2017-04-02 00:00:00'),
           xmax = ymd_hms('2017-06-24 00:00:00'),
           ymin = 41.32, ymax = 41.52, fill = 'lightblue') +
  geom_line(data = TS, aes(x = date, y = TS, group = trans), color = 'gray') +
  geom_line(aes(x = date, y = value), lwd = 1.5) +
  labs(x = NULL, y = 'Latitude') +
  theme_bw()

# 4 Clusters
cents <- data.frame(cent = rep(c('Centroid 1', 'Centroid 2', 'Centroid 3', 'Centroid 4'), each = 86),
                    value = c(c4@centroids[[1]], c4@centroids[[2]], c4@centroids[[3]], c4@centroids[[4]]),
                    date = rep(unique(agg.pad.imp$date.floor), times = 4))

TS <- data.frame(TS = do.call(c, c4@datalist),
                 trans = rep(names(c4@datalist), each = 86),
                 date = rep(unique(agg.pad.imp$date.floor), times = 66),
                 cent = paste0('Centroid ', rep(c4@cluster, each = 86)))

# slotNames()
# [1] "iter"      "converged" "clusinfo"  "cldist"    "call"      "family"    "control"   "datalist"
# [9] "type"      "distance"  "centroid"  "preproc"   "k"         "cluster"   "centroids" "distmat"
# [17] "proctime"  "dots"      "args"      "seed"

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




frechet_func <- function(x, y){distFrechet()}



# kml and kmlShape (which is different from kShape...) ----
k <- reshape2::dcast(agg.pad.imp, Transmitter ~ date.floor, value.var = 'avg.imp')
row.names(k) <- k$Transmitter
k <- as.matrix(k[, -1])

library(kmlShape)

pr_DB$set_entry(FUN = distFrechet, names = c('Frechet'),
                loop = T, type = 'metric', distance = T)


p <- cldsWide(k, id = rownames(k))

kmlclus <- kmlShape(p, 2, timeScale = 0.03, toPlot = 'none')
plot(kmlclus, col = 'clusters')



library(kml)

p <- cld(k, idAll = row.names(k))
kml(p, parAlgo = parALGO(startingCond = 'maxDist'), toPlot = 'both')
plot(p)

