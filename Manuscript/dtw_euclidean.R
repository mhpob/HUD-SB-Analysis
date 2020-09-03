## Import/munge data ----
library(dtw)
source('data and imports/perfishTS.R')

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



## Create function for figure ----
#   Got this by hacking dtw::dtwPlotTwoWay(). Needed to do so in order to get
#     the margins right and adjust to plot Euclidean distance.

dtw_euc_plot <- function (d, xts = NULL, yts = NULL, offset = 1, ts.type = "l",
                          pch = 21, match.indices = NULL,
                          match.col = 'gray',  match.lty = 1,
                          ylab = '', xlab = '', ...) {

  xts <- d$query
  yts <- d$reference

  ytso <- yts + offset

  maxlen <- max(length(xts), length(ytso))
  length(xts) <- maxlen
  length(ytso) <- maxlen

  def.par <- par(no.readonly = TRUE)

  plot_body <- function(){
    matlines(cbind(xts, ytso), type = ts.type, pch = pch, xlab = xlab,
            ylab = '', ylim = c(min(xts), max(ytso) + 10), ...)
    box()
    axis(2, at = round(c(min(xts), mean(c(min(xts), max(xts))), ceiling(max(xts)))),
         cex.axis = 1.25, tcl = -0.25, mgp = c(3 ,0.5, 0))
    axis(4, at = round(c(min(xts), mean(c(min(xts), max(xts))), ceiling(max(xts)))) + offset,
         labels = round(c(min(xts), mean(c(min(xts), max(xts))), ceiling(max(xts)))),
         cex.axis = 1.25, tcl = -0.25, mgp = c(3 ,0.5, 0))
  }

  plot_segments <- function(type){
    if(type == 'dtw'){
      indices <- d$index1
    }else{
      indices <- xts
    }

    if (is.null(match.indices)) {
      ml <- length(indices)
      idx <- 1:ml
    }else if (length(match.indices) == 1) {
      idx <- seq(from = 1, to = length(indices), length.out = match.indices)
    }else {
      idx <- match.indices
    }

    if(type == 'dtw'){
      segments(d$index1[idx], xts[d$index1[idx]],
               d$index2[idx], ytso[d$index2[idx]],
               col = match.col, lty = match.lty)
    }else{
      segments(idx, xts[idx], idx, ytso[idx],
               col = match.col, lty = match.lty)
    }
  }


  par(mar = c(0, 4, 0, 3), oma = c(2, 0, 1.5, 0), mfcol = c(2, 1))


  matplot(cbind(xts, ytso), pch = pch, xlab = xlab, type = 'n',
           ylab = '', ylim = c(min(xts), max(ytso) + 15), axes = FALSE, ...)

  plot_segments(type = 'Euclid')

  plot_body()

  title(main = 'Euclidean', adj = 0.01, line = -1)
  axis(1, cex.axis = 1.25, at = seq(1, 88, 14), labels = F, tcl = 0.25)

  segments(c(28, 1), c(xts[28], ytso[1]),
           c(28, 1), c(ytso[28], xts[1]), col = 'red')
  points(x = c(28, 1), y = c(xts[28], ytso[1]), col = 'red')
  segments(c(34, 9), c(xts[34], ytso[9]),
           c(34, 9), c(ytso[34], xts[9]), col = 'red')
  points(x = c(34, 9), y = c(xts[34], ytso[9]), col = 'red')
  segments(c(75, 54), c(xts[75], ytso[54]),
           c(75, 54), c(ytso[75], xts[54]), col = 'red')
  points(x = c(75, 54), y = c(xts[75], ytso[54]), col = 'red')
  segments(c(86, 58), c(xts[86], ytso[58]),
           c(86, 58), c(ytso[86], xts[58]), col = 'red')
  points(x = c(86, 58), y = c(xts[86], ytso[58]), col = 'red')


  text(-1, 210, expression('A'[1]))
  text(31, 34, expression('A'[2]))
  text(7, 363, expression('B'[1]))
  text(36, 168, expression('B'[2]))
  text(55, 340, expression('C'[1]))
  text(75, 112, expression('C'[2]))
  text(59, 230, expression('D'[1]))
  text(84, 35, expression('D'[2]))


  matplot(cbind(xts, ytso), pch = pch, xlab = xlab, type = 'n',
          ylab = '', ylim = c(min(xts), max(ytso) + 15), axes = FALSE, ...)

  plot_segments(type = 'dtw')


  plot_body()

  title(main = 'Dynamic time warping', adj = 0.01, line = -1)
  axis(1, cex.axis = 1.25, at = seq(1, 88, 14),
       labels = format(
         seq.Date(as.Date('2019-04-02'), as.Date('2019-04-01') + 88, by = '14 day'),
         '%d-%b'))


  # These are segments connecting points of entry, arrival on spawning ground,
  # departure from spawning ground, and departure from river. Not done functionally,
  # here.
  segments(28, xts[28],
           1, ytso[1], col = 'red')
  points(x = c(28, 1), y = c(xts[28], ytso[1]), col = 'red')
  segments(34, xts[34],
           9, ytso[9], col = 'red')
  points(x = c(34, 9), y = c(xts[34], ytso[9]), col = 'red')
  segments(75, xts[75],
           54, ytso[54], col = 'red')
  points(x = c(75, 54), y = c(xts[75], ytso[54]), col = 'red')
  segments(86, xts[86],
           58, ytso[58], col = 'red')
  points(x = c(86, 58), y = c(xts[86], ytso[58]), col = 'red')

  text(-1, 210, expression('A'[1]))
  text(31, 34, expression('A'[2]))
  text(7, 363, expression('B'[1]))
  text(36, 168, expression('B'[2]))
  text(55, 340, expression('C'[1]))
  text(76, 150, expression('C'[2]))
  text(57, 190, expression('D'[1]))
  text(84, 35, expression('D'[2]))


  mtext('River kilometer', side = 2, line = -2, outer = T, cex = 1.75)


  par(def.par)
}



## Plot! ----
tiff("p:/obrien/biotelemetry/hudson sb/hud-sb-analysis/manuscript/revision/figures/Figure2.tif",
     width = 5.2, height = 3.75, units = 'in', compression = 'lzw', res = 600,
     pointsize = 6)
dtw_euc_plot(warp, col = c('burlywood', 'burlywood4'), offset = 175, lty = 1, lwd = 3)

dev.off()
