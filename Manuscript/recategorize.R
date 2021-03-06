library(dtwclust)

source('data and imports/perfishTS.R')


# Put time series into rows ----
r_series <- reshape2::dcast(agg.pad.imp, transmitter + year ~ doy,
                            value.var = 'avg.imp')

# Separate years
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


all_clust <- full_join(tibble(transmitter = names(clust17@datalist),
                               cluster17 = as.numeric(clust17@cluster)),
                        tibble(transmitter = names(pred18), pred18 = pred18)) %>%
  full_join(agg.pos.imp %>%
              ungroup() %>%
              filter(!is.na(region)) %>%
              distinct(transmitter, region, sex)) %>%
  mutate(cluster17 = ifelse(cluster17 == 1, 'Upper', 'Lower'),
         pred18 = ifelse(pred18 == 1, 'Upper', 'Lower'),
         region = ifelse(grepl('West', region), 'Lower', 'Upper'))

# write.csv(all_clust, 'manuscript/recategorized_rkm.csv', row.names = F)

# Test for categorization differences
xtabs(~ cluster17 + region, data = all_clust)
xtabs(~ pred18 + region, data = all_clust)
xtabs(~ cluster17 + pred18, data = all_clust)
chisq.test(xtabs(~ cluster17 + pred18, data = all_clust), correct = F)
chisq.test(xtabs(~ cluster17 + region, data = all_clust), correct = F)
chisq.test(xtabs(~ pred18 + region, data = all_clust), correct = F)

# Test for sex differences
chisq.test(xtabs(~ sex + cluster17, data = all_clust), correct = F)

#Run summary ----
library(data.table)
entry_exit <- setDT(all_clust)[setDT(agg.pos.imp), on = 'transmitter']
entry_exit <- entry_exit[,.(entry = min(dummydoy),
                                    exit = max(dummydoy)),
                                 by = c('year', 'transmitter', 'cluster17')]
entry_exit[, .(min.entry = min(entry),
               med.entry = median(entry),
               max.entry = max(entry),
               min.exit = min(exit),
               med.exit = median(exit),
               max.exit = max(exit)),
           by = c('year', 'cluster17')]



#Plotting ----
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


  base_plot <- ggplot(cents) +
    facet_wrap(~cent) +
    scale_y_continuous(limits = c(0, 255), expand = c(0, 0)) +
    annotate('rect',
             xmin = mindate,
             xmax = maxdate,
             ymin = 160, ymax = 200, fill = '#FF7762', alpha = 0.7) +
    annotate('rect',
             xmin = mindate,
             xmax = maxdate,
             ymin = 80, ymax = 100, fill = '#7DC6D8', alpha = 0.7) +
    scale_x_date(breaks = 'month',
                 date_labels = '%b',
                 limits = c(as.Date('2017-04-01'), as.Date('2017-06-30')),
                 expand = c(0, 0))  +
    labs(x = NULL, y = 'River kilometer') +
    theme_bw()


  if(!is.null(highlight)){
    TS_highlight <- TS[TS$trans %in% highlight,]

    if(highlight_only == T){
      base_plot +
        geom_line(data = TS_highlight, aes(x = date, y = TS, group = trans),
                  color = 'red', lwd = 1.2) +
        geom_line(aes(x = date, y = value), lwd = 1.2)

    } else{
      base_plot +
        geom_line(data = TS, aes(x = date, y = TS, group = trans),
                  size = 0.25, color = 'gray35') +
        geom_line(aes(x = date, y = value), size = 1.2) +
        geom_line(data = TS_highlight, aes(x = date, y = TS, group = trans),
                  color = 'red', size = 0.75)
    }

  } else{
    base_plot +
      geom_line(data = TS, aes(x = date, y = TS, group = trans),
                size = 0.2, color = 'gray35') +
      geom_line(aes(x = date, y = value), size = 1.2)
  }
}


p2017 <-
  cleanplot('r_series17') +
  scale_y_continuous(limits = c(0, 255), expand = c(0, 0), breaks = seq(50, 250, 50)) +
  geom_text(data = data.frame(x = as.Date('2017-04-10'), y = 240,
                              lab = '2017', cent = 'Centroid 1'),
            aes(x = x, y = y, label = lab), size = 10/.pt) +
  labs(x = NULL, y = NULL) +
  theme(axis.text.y = element_text(size = 8, angle = 90, hjust = 0.5),
        strip.text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0.2, 0.2, 0.1, 0.05), "cm"),
        strip.background = element_blank(),
        panel.spacing.x = unit(1, 'lines'))
# p2017

p2018 <-
  cleanplot('r_series18',
            highlight = all_clust[all_clust$cluster17 != all_clust$pred18,]$transmitter) +
  geom_text(data = data.frame(x = as.Date('2017-04-10'), y = 240,
                              lab = '2018', cent = 'Centroid 1'),
            aes(x = x, y = y, label = lab), size = 10/.pt) +
  labs(y = 'River kilometer') +
  theme(axis.text.y = element_text(size = 8, angle = 90, hjust = 0.5),
        axis.text.x = element_text(size = 8),
        axis.title = element_text(size = 12, hjust = 1.75),
        strip.text = element_text(size = 12),
        plot.margin = unit(c(0, 0.2, 0.1, 0.05), "cm"),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing.x = unit(1, 'lines'))

# p2018
library(patchwork)

fig4 <- p2017 / p2018 &
  theme(plot.margin = margin(0, 0, 0, 0))


ggsave("p:/obrien/biotelemetry/hudson sb/hud-sb-analysis/manuscript/revision/figures/Figure4.tif", fig4,
       width = 5.2, height = 4, units = 'in', dpi = 600,
       device = 'tiff', compression = 'lzw')
