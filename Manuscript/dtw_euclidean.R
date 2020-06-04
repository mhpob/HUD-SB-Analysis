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


## DTW ----
warp <- dtw(r_series17[grepl('486', rownames(r_series17)),],
            r_series17[grepl('499', rownames(r_series17)),],
            keep.internals = T, window.size = 7)

## Have to hack dtw::dtwPlotTwoWay()
dtw_plot <- function (d, xts = NULL, yts = NULL, offset = 1, ts.type = "l",
                      pch = 21, match.indices = NULL,
                      match.col = 'gray',  match.lty = 1,
                      ylab = '', xlab = '', ...) {
  if (is.null(xts) || is.null(yts)) {
    xts <- d$query
    yts <- d$reference
  }
  if (is.null(xts) || is.null(yts))
    stop("Original timeseries are required")
  ytso <- yts + offset
  maxlen <- max(length(xts), length(ytso))
  length(xts) <- maxlen
  length(ytso) <- maxlen
  def.par <- par(no.readonly = TRUE)
  if (offset != 0) {
    par(mar = c(3, 4, 2, 3) + 0.1)
  }
  matplot(cbind(xts, ytso), type = ts.type, pch = pch, xlab = xlab,
          ylab = ylab, ylim = c(min(xts), max(ytso) + 0.1), axes = FALSE, ...)
  box()
  axis(1, cex.axis = 1.25, at = seq(1, 88, 14),
       labels = format(
         seq.Date(as.Date('2019-04-02'), as.Date('2019-04-01') + 88, by = '14 day'),
         '%d-%b'))
  axis(2, at = round(c(min(xts), mean(c(min(xts), max(xts))), max(xts)), 1), cex.axis = 1.25)
  if (offset != 0) {
    axis(4, at = round(c(min(xts), mean(c(min(xts), max(xts))), max(xts)), 1) + offset,
         labels = round(c(min(xts), mean(c(min(xts), max(xts))), max(xts)), 1), cex.axis = 1.25)
  }
  if (is.null(match.indices)) {
    ml <- length(d$index1)
    idx <- 1:ml
  }
  else if (length(match.indices) == 1) {
    idx <- seq(from = 1, to = length(d$index1), length.out = match.indices)
  }
  else {
    idx <- match.indices
  }
  segments(d$index1[idx], xts[d$index1[idx]], d$index2[idx],
           ytso[d$index2[idx]], col = match.col, lty = match.lty)
  par(def.par)
}



dtw_plot(warp, match.indices = 30, col = c('blue', 'red'), lty = 1, lwd = 3)
title(main = 'Dynamic Time Warping', adj = 0.01, line = 1)
title(ylab = 'Latitude (°N)', cex.lab = 2, line = 2.5)



## Euclidean ----
# Hack of the hack
euc_plot <- function (d, xts = NULL, yts = NULL, offset = 1, ts.type = "l",
                      pch = 21, match.indices = NULL,
                      match.col = 'gray',  match.lty = 1,
                      ylab = '', xlab = '', ...) {
  if (is.null(xts) || is.null(yts)) {
    xts <- d$query
    yts <- d$reference
  }
  if (is.null(xts) || is.null(yts))
    stop("Original timeseries are required")
  ytso <- yts + offset
  maxlen <- max(length(xts), length(ytso))
  length(xts) <- maxlen
  length(ytso) <- maxlen
  def.par <- par(no.readonly = TRUE)
  if (offset != 0) {
    par(mar = c(3, 4, 2, 3) + 0.1)
  }
  matplot(cbind(xts, ytso), type = ts.type, pch = pch, xlab = xlab,
          ylab = ylab, ylim = c(min(xts), max(ytso) + 0.1), axes = FALSE, ...)
  box()
  axis(1, cex.axis = 1.25, at = seq(1, 88, 14),
       labels = format(
         seq.Date(as.Date('2019-04-02'), as.Date('2019-04-01') + 88, by = '14 day'),
         '%d-%b'))
  axis(2, at = round(c(min(xts), mean(c(min(xts), max(xts))), max(xts)), 1), cex.axis = 1.25)
  if (offset != 0) {
    axis(4, at = round(c(min(xts), mean(c(min(xts), max(xts))), max(xts)), 1) + offset,
         labels = round(c(min(xts), mean(c(min(xts), max(xts))), max(xts)), 1), cex.axis = 1.25)
  }
  if (is.null(match.indices)) {
    ml <- length(xts)
    idx <- 1:ml
  }
  else if (length(match.indices) == 1) {
    idx <- seq(from = 1, to = length(xts), length.out = match.indices)
  }
  else {
    idx <- match.indices
  }
  segments(idx, xts[idx], idx, ytso[idx], col = match.col, lty = match.lty)
  par(def.par)
}

euc_plot(warp, match.indices = 30, col = c('blue', 'red'), lty = 1, lwd = 3)
title(main = 'Euclidean', adj = 0.01, line = 1)
title(ylab = 'Latitude (°N)', cex.lab = 2, line = 2.5)

