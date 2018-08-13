library(dtwclust)

source('perfishTS.R')

# agg.pad.imp <- agg.pad.imp %>%
#   group_by(transmitter) %>%
#   mutate(avg.scale = scale(avg.imp, center = 40.85, scale = sd(avg.imp)))
# test <- split(agg.pad.imp, agg.pad.imp$transmitter)

# Put time series into rows ----
r_series <- reshape2::dcast(agg.pad.imp, transmitter + year ~ doy,
                            value.var = 'avg.imp')

# Separate years
r_series17 <- r_series[r_series$year == 2017,]
row_ts <- function(data, year){
  hold <- data[data$year == year,]
  row.names(hold) <- hold$transmitter
  as.matrix(hold[, !names(hold) %in% c('transmitter', 'year')])
}

r_series17 <- row_ts(r_series, 2017)
r_series18 <- row_ts(r_series, 2018)

# Cluster multiple times and pick the most frequent ----
source('TS_select.R')

selections <- function(n_clust){
  temp <- lapply(list(r_series17, r_series18),
                 TS_select, reps = 1000, n_clusters = n_clust, dist = 'dtw_basic',
                 cent = 'median', window = '7')
  temp <- setNames(temp, c('r_series17', 'r_series18'))
  temp
}
winner <- function(ts_obj, year){
  yr <- ifelse(year == 2017, 'r_series17', 'r_series18')
  ts_obj[[yr]][['results']][[which.max(ts_obj[[yr]][['key']]$n)]]
}
clusterplot <- function(win_data){
  plot(win_data, plot = F) +
    ylim(40.8, 42.75) +
    annotate('rect', xmin = 0, xmax = 86,
             ymin = 42.07, ymax = 42.36, fill = 'pink', alpha = 0.4) +
    annotate('rect', xmin = 0, xmax = 86,
             ymin = 41.32, ymax = 41.52, fill = 'lightblue', alpha = 0.4)
}

c2 <- selections(2)
c2_17 <- winner(c2, 2017)
c2_18 <- winner(c2, 2018)

clusterplot(c4_17)

c3 <- selections(3)
c3_17 <- winner(c3, 2017)
c3_18 <- winner(c3, 2018)

c4 <- selections(4)
c4_17 <- winner(c4, 2017)
c4_18 <- winner(c4, 2018)

c5 <- selections(5)
c5_17 <- winner(c5, 2017)
c5_18 <- winner(c5, 2018)

# 2017 CVIs: 2 votes for 2 and 4 clusters, 1 for 5 and 3
sapply(list(K2 = c2_17, K3 = c3_17, K4 = c4_17, K5 = c5_17), cvi,
       type = 'internal')
# 2018 CVIs: 4 for 5, 1 for 3 and 4
sapply(list(K2 = c2_18, K3 = c3_18, K4 = c4_18, K5 = c5_18), cvi,
       type = 'internal')


# 2 clusters ----
cents <- data.frame(cent = rep(c('Centroid 1', 'Centroid 2'), each = 86),
                    value = c(c2[[1]][[3]]@centroids[[1]],
                              c2[[1]][[3]]@centroids[[2]]),
                    date = rep(unique(agg.pad.imp$doy), times = 2))

TS <- data.frame(TS = do.call(c, c2[[1]][[3]]@datalist),
                 trans = rep(names(c2[[1]][[3]]@datalist), each = 86),
                 date = rep(unique(agg.pad.imp$doy), times = 40),
                 cent = paste0('Centroid ', rep(c2[[1]][[3]]@cluster,
                                                each = 86))) %>%
  left_join(distinct(agg.pad.imp, transmitter, sex),
            by = c('trans' = 'transmitter'))

ggplot(cents) +
  facet_wrap(~cent) + ylim(40.8, 42.75) +
  annotate('rect', xmin = min(agg.pad.imp$doy),
           xmax = max(agg.pad.imp$doy),
           ymin = 42.07, ymax = 42.36, fill = 'pink') +
  annotate('rect', xmin = min(agg.pad.imp$doy),
           xmax = max(agg.pad.imp$doy),
           ymin = 41.32, ymax = 41.52, fill = 'lightblue') +
  geom_line(data = TS, aes(x = date, y = TS, group = trans), color = 'gray') +
  geom_line(aes(x = date, y = value), lwd = 1.5) +
  labs(x = NULL, y = 'Latitude') +
  theme_bw()

# 4 Clusters
cents <- data.frame(cent = rep(paste('Centroid', 1:4, sep = ' '), each = 86),
                    value = do.call(c, c4@centroids),
                    date = rep(unique(agg.pad.imp$date.floor), times = 4))

TS <- data.frame(TS = do.call(c, c4@datalist),
                 trans = rep(names(c4@datalist), each = 86),
                 date = rep(unique(agg.pad.imp$date.floor), times = 66),
                 cent = paste0('Centroid ', rep(c4@cluster, each = 86))) %>%
  left_join(distinct(detects, transmitter, Sex), by = c('trans' = 'transmitter'))



#
temp <- data.frame(transmitter = names(c2@datalist), cluster = c2@cluster) %>%
  left_join(distinct(detects, transmitter, region)) %>%
  mutate(cluster = case_when(cluster == 1 ~ 'West Point-Newburgh',
                             T ~ 'Saugerties-Coxsackie'),
         correct = case_when(cluster == region ~ T,
                             T ~ F))

# Correct/Incorrect classification
temp %>% group_by(region, correct) %>% summarize(n())


# slotNames()
# [1] "iter"      "converged" "clusinfo"  "cldist"    "call"      "family"    "control"   "datalist"
# [9] "type"      "distance"  "centroid"  "preproc"   "k"         "cluster"   "centroids" "distmat"
# [17] "proctime"  "dots"      "args"      "seed"

plot.data <- agg.pos.imp %>%
  ungroup() %>%
  mutate(transmitter = factor(transmitter,
                              levels = levels(factor(transmitter))[ord]))


ggplot() + geom_raster(data = plot.data, aes(x = date.floor, y = transmitter,
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
k <- reshape2::dcast(agg.pad.imp, transmitter ~ date.floor, value.var = 'avg.imp')
row.names(k) <- k$transmitter
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

