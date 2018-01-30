# Data import ----
library(lubridate); library(dplyr)

all <- readRDS('hud_detects.RDS')
hud <- all %>%
  filter(date.local >= '2017-03-15',
         array %in% c('Above', 'Saugerties-Coxsackie', 'Between',
                      'West Point-Newburgh', 'Below'))

# Need to find a better selection rule; but 11507 and 11480 should be dropped
# since they probably died (last array isn't below WPT/NBGH)
hud %>%
  group_by(Transmitter) %>%
  arrange(date.local) %>%
  summarize(last(date.local),
            last(array))

hud <- hud %>%
  filter(!grepl('11480|11507', Transmitter))

# Number of fish from each spawning region left in the analysis
# group_by(hud, Region) %>% summarize(n_distinct(Transmitter))
# library(ggplot2)
# ggplot() + geom_point(data = hud, aes(x = date.local, y = Transmitter,
#                                       color = array))

# Data munging ----
# 1) Find mean daily position. Start with mean lat, as Hudson is a linear
#    N/S system.

avg.pos <- hud %>%
  group_by(Transmitter, date.floor) %>%
  summarize(lat.avg = mean(as.numeric(lat))) %>%
  arrange(date.floor)

# 2) Fill NAs in time series.
#    Check out methods in imputeTS. Using moving average; others more appropriate?

# On a per-fish basis...
avg.pos.spl <- split(avg.pos, avg.pos$Transmitter)

# When the fish isn't heard on a day between first and last days detected,
# insert a day with NA location. Fill this with imputed value.
library(imputeTS)
avg.pos.spl <- lapply(avg.pos.spl, function(x){
  hold <- data.frame(date.floor = seq(range(x$date.floor)[1],
                                      range(x$date.floor)[2],
                                      by = 'day'),
                     Transmitter = x$Transmitter[1])
  suppressWarnings(suppressMessages(
  x %>%
    full_join(hold) %>%
    arrange(date.floor) %>%
    mutate(lat.imp = na.ma(lat.avg, k = 2))
  ))
})

imp.pos <- do.call(rbind, avg.pos.spl)

# ggplot(data = imp.pos) +
#   geom_point(aes(x = floor.date, y = lat.imp)) +
#   geom_point(aes(x = floor.date, y = lat.avg), col = 'red') +
#   facet_wrap(~Transmitter)

# First crack at DFA using MARSS ----
# https://cran.r-project.org/web/packages/MARSS/vignettes/UserGuide.pdf

library(MARSS)
library(reshape2)
k <- dcast(avg.pos, Transmitter ~ floor.date, value.var = 'lat.imp')
row.names(k) <- k$Transmitter
k <- as.matrix(k[, -1])


dfa <- MARSS(k, form = 'dfa',
             model = list(m = 2, R = 'diagonal and unequal'))


H.inv <- varimax(coef(dfa, type = 'matrix')$Z)$rotmat
Z.rot <- coef(dfa, type = 'matrix')$Z %*% H.inv
trends.rot <- solve(H.inv) %*% dfa$states


spp = rownames(k)
minZ = 0.05
ylims = c(-1.1*max(abs(Z.rot)), 1.1*max(abs(Z.rot)))
par(mfrow=c(ceiling(dim(trends.rot)[1]/2),2), mar=c(3,4,1.5,0.5), oma=c(0.4,1,1,1))
for(i in 1:2) {
  plot(c(1:N.ts)[abs(Z.rot[,i])>minZ], as.vector(Z.rot[abs(Z.rot[,i])>minZ,i]),
       type="h", lwd=2, xlab="", ylab="", xaxt="n", ylim=ylims, xlim=c(0,N.ts+1))
  for(j in 1:N.ts) {
    if(Z.rot[j,i] > minZ) {text(j, -0.05, spp[j], srt=90, adj=1, cex=0.9)}
    if(Z.rot[j,i] < -minZ) {text(j, 0.05, spp[j], srt=90, adj=0, cex=0.9)}
    abline(h=0, lwd=1, col="gray")
  } # end j loop
  mtext(paste("Factor loadings on trend",i,sep=" "),side=3,line=.5)
} # end i loop

plot(cbind(trends.rot[1,], trends.rot[2,]) ~
          seq(ymd('2017-04-04'), ymd('2017-06-24'), by = 'day'), xaxt = 'n', xlab = '')
axis.Date(1, at = seq(ymd('2017-04-04'), ymd('2017-06-24'), by = 'month'),
          format = '%b')


# Second crack (padded time series) ----
#  The first crack yields weird behavior on the ends of the trends/modeled ts,
#  Might be due to NAs on the ends of the ts being modeled as missing data of a
#  trend that tends toward mean latitude (~ 41.5 - 42.0).

#  For this attempt, fill the beginning/ending NAs with GWB's latitude (40.85).
#  This will assume that the fish are making excursions from a resident pool
#  below GWB.
avg.pos.spl <- split(avg.pos, avg.pos$Transmitter)

hud.date.seq <- range(hud$date.floor)
hud.date.seq <- seq(hud.date.seq[1] - days(2), hud.date.seq[2] + days(2),
                    by = 'day')

avg.pos.spl <- lapply(avg.pos.spl, function(x){
  hold <- data.frame(date.floor = hud.date.seq,
                     Transmitter = x$Transmitter[1])
  suppressWarnings(suppressMessages(
  hold <- x %>%
    full_join(hold) %>%
    arrange(date.floor) %>%
    mutate(lat.pad = lat.avg)
  ))
  hold[hold$date.floor %in%
          c(seq(min(hud.date.seq), min(x$date.floor) - days(2), by = 'day'),
            seq(max(x$date.floor) + days(2), max(hud.date.seq), by = 'day')),
        ]$lat.pad <- 40.85

  hold$lat.imp  <-  na.ma(hold$lat.pad, k = 2)
  hold
})

pad.pos <- do.call(rbind, avg.pos.spl)

ggplot(data = pad.pos) +
  geom_point(aes(x = date.floor, y = lat.imp)) +
  geom_point(aes(x = date.floor, y = lat.avg), col = 'red') +
  facet_wrap(~Transmitter)


# Future work ----
#  1) Use different "pool" locations.
#     Use model fits to find best "pool", e.g. GWB, Lincoln tunnel, NY Harbor,
#     Verrazano?
#  2) Don't pad, but normalize ts according to latitude of these pools. Might be
#     an issue since

