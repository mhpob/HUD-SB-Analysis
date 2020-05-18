library(dtwclust)

source('data and imports/perfishTS.R')


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


#create cluster
clust17 <- tsclust(series = r_series17, k = 2, distance = 'dtw_basic',
        centroid = 'median',
        window.size = '7')
plot(clust17)

# predict using cluster
pred18 <- predict(clust17, r_series18)


all_clust <- left_join(tibble(transmitter = names(clust17@datalist),
                               cluster17 = as.numeric(clust17@cluster)),
                        tibble(transmitter = names(pred18), pred18 = pred18)) %>%
  left_join(agg.pos.imp %>%
              ungroup() %>%
              filter(!is.na(region)) %>%
              distinct(transmitter, region))

# write.csv(all_clust, 'manuscript/recategorized.csv', row.names = F)

xtabs(~ cluster17 + pred18, data = all_clust)
chisq.test(xtabs(~ cluster17 + pred18, data = all_clust))
chisq.test(xtabs(~ region + cluster17, data = all_clust))
chisq.test(xtabs(~ region + pred18, data = all_clust))


cleanplot <- function(dat, highlight = NULL, highlight_only = F){
  ncentroids <- 2

  if(grepl('17', dat)){
    nseries <- 66
    assignments <- get('clust17')@cluster
  } else{
    nseries <- 40
    assignments <- predict(clust17, r_series18)
  }

  cents <- data.frame(cent = rep(paste('Centroid', 1:ncentroids, sep = ' '),
                                 each = 88),
                      value = do.call(c, get('clust17')@centroids),
                      date = rep(unique(agg.pad.imp$doy), times = ncentroids)) %>%
    mutate(date = as.Date('2017-01-01') + (date - 1))
  TS <- data.frame(TS = do.call(c, list(t(get(dat)))),
                   trans = rep(row.names(get(dat)), each = 88),
                   date = rep(unique(agg.pad.imp$doy), times = nseries),
                   cent = paste0('Centroid ',
                                 rep(assignments, each = 88))) %>%
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
        facet_wrap(~cent) + ylim(40.95, 42.75) +
        annotate('rect',
                 xmin = mindate,
                 xmax = maxdate,
                 ymin = 42.07, ymax = 42.36, fill = 'pink') +
        annotate('rect',
                 xmin = mindate,
                 xmax = maxdate,
                 ymin = 41.32, ymax = 41.52, fill = 'lightblue') +
        geom_line(data = TS_highlight, aes(x = date, y = TS, group = trans),
                  color = 'red', lwd = 1.5) +
        geom_line(aes(x = date, y = value), lwd = 1.5) +
        scale_x_date(limits = c(as.Date('2017-04-01'), as.Date('2017-07-01'))) +
        labs(x = NULL, y = 'Latitude') +
        theme_bw()
    } else{
      ggplot(cents) +
        facet_wrap(~cent) + ylim(40.95, 42.75) +
        annotate('rect',
                 xmin = mindate,
                 xmax = maxdate,
                 ymin = 42.07, ymax = 42.36, fill = 'pink') +
        annotate('rect',
                 xmin = mindate,
                 xmax = maxdate,
                 ymin = 41.32, ymax = 41.52, fill = 'lightblue') +
        geom_line(data = TS, aes(x = date, y = TS, group = trans), color = 'gray35') +
        geom_line(aes(x = date, y = value), size = 2.5) +
        geom_line(data = TS_highlight, aes(x = date, y = TS, group = trans),
                  color = 'red', size = 1.5) +
        scale_x_date(breaks = 'month',
                     date_labels = '%b',
                     limits = c(as.Date('2017-04-01'), as.Date('2017-06-30')),
                     expand = c(0, 0)) +
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
      geom_line(data = TS, aes(x = date, y = TS, group = trans)) +
      geom_line(aes(x = date, y = value), size = 1.5) +
      scale_x_date(limits = c(as.Date('2017-04-01'), as.Date('2017-07-01'))) +
      labs(x = NULL, y = 'Latitude') +
      theme_bw()
  }
}


p2017 <- cleanplot('r_series17',
                   highlight = all_clust[all_clust$cluster17 != all_clust$pred18,]$transmitter) +
  labs(x = NULL, y = NULL) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0.2, 0.2, 0.1, 0.05), "cm"),
        strip.background = element_blank(),
        panel.spacing.x = unit(1, 'lines'))
# p2017

p2018 <- cleanplot('r_series18',
                   highlight = all_clust[all_clust$cluster17 != all_clust$pred18,]$transmitter) +
  labs(y = NULL) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 16),
        plot.margin = unit(c(0, 0.2, 0.1, 0.05), "cm"),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing.x = unit(1, 'lines'))

# p2018
library(patchwork)

p2017 / p2018

