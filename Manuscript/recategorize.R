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

clust17 <- tibble(transmitter = names(clust17@datalist),
                cluster17 = as.numeric(clust17@cluster))
pred18 <- tibble(transmitter = names(pred18), pred18 = pred18)

all_clust <- inner_join(clust17, pred18)

xtabs(~ cluster17 + pred18, data = all_clust)



