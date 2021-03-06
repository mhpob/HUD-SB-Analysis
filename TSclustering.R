library(dtwclust)

source('data and imports/perfishTS.R')

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
                 TS_select, reps = 10000, n_clusters = n_clust, dist = 'dtw_basic',
                 cent = 'median', window = '7')
  temp <- setNames(temp, c('r_series17', 'r_series18'))
  temp
}
winner <- function(ts_obj, year, rank){
  yr <- ifelse(year == 2017, 'r_series17', 'r_series18')
  index <- which(ts_obj[[yr]][['key']]$n == sort(ts_obj[[yr]][['key']]$n, decreasing = T)[rank])
  ts_obj[[yr]][['results']][[index]]
}
clusterplot <- function(win_data){
  plot(win_data, plot = F) +
    ylim(40.8, 42.75) +
    annotate('rect', xmin = 0, xmax = 86,
             ymin = 42.07, ymax = 42.36, fill = 'pink', alpha = 0.4) +
    annotate('rect', xmin = 0, xmax = 86,
             ymin = 41.32, ymax = 41.52, fill = 'lightblue', alpha = 0.4)
}

# c2 <- selections(2) #saved as .rda
# saveRDS(c2, 'p:/obrien/biotelemetry/hudson sb/manuscript/c2_manuscript.rda')
c2 <- readRDS('p:/obrien/biotelemetry/hudson sb/manuscript/c2_manuscript.rda')
c2_17 <- winner(c2, 2017, 2)
c2_18 <- winner(c2, 2018, 4)

clusterplot(c2_18)

# c3 <- selections(3) #saved as .rda
c3 <- readRDS('data and imports/cluster data/c3.rda')
c3_17 <- winner(c3, 2017)
c3_18 <- winner(c3, 2018)

# c4 <- selections(4) #saved as .rda
c4 <- readRDS('data and imports/cluster data/c4.rda')
c4_17 <- winner(c4, 2017)
c4_18 <- winner(c4, 2018)

# c5 <- selections(5)  #saved as .rda
c5 <- readRDS('data and imports/cluster data/c5.rda')
c5_17 <- winner(c5, 2017)
c5_18 <- winner(c5, 2018)


# 2017 CVIs: 2 votes for 2 and 4 clusters, 1 for 5 and 3
cvi_c2_17 <- as.data.frame(
  sapply(list(K2 = c2_17, K3 = c3_17, K4 = c4_17, K5 = c5_17), cvi,
         type = 'internal'),
)
cvi_c2_17$cvi_choice <- NA

for(i in seq_len(nrow(cvi_c2_17))){
  cvi_c2_17$cvi_choice[i] <- switch(rownames(cvi_c2_17)[i],
                         Sil = colnames(cvi_c2_17)[which.max(cvi_c2_17[i,])],
                         SF = colnames(cvi_c2_17)[which.max(cvi_c2_17[i,])],
                         CH = colnames(cvi_c2_17)[which.max(cvi_c2_17[i,])],
                         D = colnames(cvi_c2_17)[which.max(cvi_c2_17[i,])],
                         DB = colnames(cvi_c2_17)[which.min(cvi_c2_17[i,])],
                         DBstar = colnames(cvi_c2_17)[which.min(cvi_c2_17[i,])],
                         COP = colnames(cvi_c2_17)[which.min(cvi_c2_17[i,])])
}

# 2018 CVIs: 4 for 5, 1 for 3 and 4
cvi_c2_18 <- as.data.frame(
  sapply(list(K2 = c2_18, K3 = c3_18, K4 = c4_18, K5 = c5_18), cvi,
         type = 'internal')
)
cvi_c2_18$cvi_choice <- NA

for(i in seq_len(nrow(cvi_c2_18))){
  cvi_c2_18$cvi_choice[i] <- switch(rownames(cvi_c2_18)[i],
                            Sil = colnames(cvi_c2_18)[which.max(cvi_c2_18[i,])],
                            SF = colnames(cvi_c2_18)[which.max(cvi_c2_18[i,])],
                            CH = colnames(cvi_c2_18)[which.max(cvi_c2_18[i,])],
                            D = colnames(cvi_c2_18)[which.max(cvi_c2_18[i,])],
                            DB = colnames(cvi_c2_18)[which.min(cvi_c2_18[i,])],
                            DBstar = colnames(cvi_c2_18)[which.min(cvi_c2_18[i,])],
                            COP = colnames(cvi_c2_18)[which.min(cvi_c2_18[i,])])
}

# Clean plotting ----
cleanplot <- function(dat, highlight = NULL, highlight_only = F){
  ncentroids <- as.numeric(substr(dat, 2, 2))
  nseries <- ifelse(substr(dat, 4, 6) == '17', 66, 40)

  cents <- data.frame(cent = rep(paste('Centroid', 1:ncentroids, sep = ' '),
                                 each = 88),
                      value = do.call(c, get(dat)@centroids),
                      date = rep(unique(agg.pad.imp$doy), times = ncentroids)) %>%
    mutate(date = as.Date('2017-01-01') + (date - 1))
  TS <- data.frame(TS = do.call(c, get(dat)@datalist),
                   trans = rep(names(get(dat)@datalist), each = 88),
                   date = rep(unique(agg.pad.imp$doy), times = nseries),
                   cent = paste0('Centroid ',
                                 rep(get(dat)@cluster, each = 88))) %>%
    mutate(date = as.Date('2017-01-01') + (date - 1)) %>%
    left_join(filter(distinct(ungroup(agg.pad.imp), transmitter, sex),
                     !is.na(sex)),
              by = c('trans' = 'transmitter'))

  mindate <- as.Date('2017-01-01') + (min(agg.pad.imp$doy) - 1)
  maxdate <- as.Date('2017-01-01') + (max(agg.pad.imp$doy) - 1)

  if(!is.null(highlight)){
    TS_highlight <- TS[TS$trans %in% highlight,]

    if(highlight_only == T){
      ggplot(cents) +
        facet_wrap(~cent) + ylim(40.8, 42.75) +
        annotate('rect',
                 xmin = mindate,
                 xmax = maxdate,
                 ymin = 42.07, ymax = 42.36, fill = 'pink') +
        annotate('rect',
                 xmin = mindate,
                 xmax = maxdate,
                 ymin = 41.32, ymax = 41.52, fill = 'lightblue') +
        geom_line(data = TS_highlight, aes(x = date, y = TS, group = trans),
                  color = 'red') +
        geom_line(aes(x = date, y = value), lwd = 1.5) +
        scale_x_date(limits = c(as.Date('2017-04-01'), as.Date('2017-07-01'))) +
        labs(x = NULL, y = 'Latitude') +
        theme_bw()
    } else{
      ggplot(cents) +
        facet_wrap(~cent) + ylim(40.8, 42.75) +
        annotate('rect',
                 xmin = mindate,
                 xmax = maxdate,
                 ymin = 42.07, ymax = 42.36, fill = 'pink') +
        annotate('rect',
                 xmin = mindate,
                 xmax = maxdate,
                 ymin = 41.32, ymax = 41.52, fill = 'lightblue') +
        geom_line(data = TS, aes(x = date, y = TS, group = trans), color = 'gray') +
        geom_line(aes(x = date, y = value), lwd = 1.5) +
        geom_line(data = TS_highlight, aes(x = date, y = TS, group = trans),
                  color = 'red') +
        scale_x_date(limits = c(as.Date('2017-04-01'), as.Date('2017-07-01'))) +
        labs(x = NULL, y = 'Latitude') +
        theme_bw()
    }

  } else{
    ggplot(cents) +
      facet_wrap(~cent) + ylim(40.8, 42.75) +
      annotate('rect',
               xmin = mindate,
               xmax = maxdate,
               ymin = 42.07, ymax = 42.36, fill = 'pink') +
      annotate('rect',
               xmin = mindate,
               xmax = maxdate,
               ymin = 41.32, ymax = 41.52, fill = 'lightblue') +
      geom_line(data = TS, aes(x = date, y = TS, group = trans), color = 'gray') +
      geom_line(aes(x = date, y = value), lwd = 1.5) +
      scale_x_date(limits = c(as.Date('2017-04-01'), as.Date('2017-07-01'))) +
      labs(x = NULL, y = 'Latitude') +
      theme_bw()
  }
}

# Second-most-frquent
# c2_18 <- c2[['r_series18']][['results']][[4]]

base_plot <- cleanplot('c2_18')

p2017 <- cleanplot('c2_17') +
  labs(x = NULL, y = NULL) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 16),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        plot.margin = unit(c(0.2, 0.2, 0.1, 0.05), "cm"))

p2018 <- cleanplot('c2_18') +
  labs(y = NULL) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 16),
        plot.margin = unit(c(0, 0.2, 0.1, 0.05), "cm"))

library(cowplot)
plot_grid(p2017, p2018, ncol = 1)

# Export recategorized fish ----
c2 <- readRDS('data and imports/cluster data/c2.rda')
c2_17 <- winner(c2, 2017)

recat <- data.frame(transmitter = names(c2_17@datalist),
                    recat_region = ifelse(c2_17@cluster == 1,
                                          'Saugerties-Coxsackie',
                                          'West Point-Newburgh'))

# Two fish skip-spawned and are recategorized by 2018 behavior
recat <- rbind(recat,
  data.frame(transmitter = c('A69-1303-11428', 'A69-1303-11504'),
             recat_region = c('West Point-Newburgh',
                              'Saugerties-Coxsackie')))

# 4 fish were strays when tagged
recat <- rbind(recat,
  data.frame(transmitter = c('A69-1303-11496', 'A69-1303-11481',
                             'A69-1303-11470', 'A69-1303-11471'),
             recat_region = 'Other'))
saveRDS(recat, 'data and imports/recat_spawning_region.RDS')
