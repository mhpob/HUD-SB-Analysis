

# Replicate n number of times. Save results in a list.
# To do: switch to parSapply
library(parallel)
cl <- makeCluster(detectCores() - 1)
clusterEvalQ(cl, library(dtwclust))
clusterExport(cl, 'r_series18')
j <- parSapply(cl, 1:1000, function(i){
  tsclust(series = r_series18, k = 3, distance = 'dtw_basic',
          centroid = 'median',
          window.size = 7, trace = F,
          control = partitional_control(pam.precompute = FALSE,
                                        iter.max = 500))
})
stopCluster(cl)

# Remove clusters with only one time series
p <- sapply(j, function(x){1 %in% x@clusinfo$size})
j <- j[!p]

# Remove clusters that are equal
q <- t(combn(1:length(j), 2))
g <- apply(q, 1, function(x) setequal(j[[x[1]]]@clusinfo, j[[x[2]]]@clusinfo))
# g <- outer(q[,1], q[,2],
#            FUN = function(x, y){setequal(j[[x]]$clusinfo, j[[y]]$clusinfo)})
q <- cbind(q, g)
q <- q[q[, 3] == 1,]
qq <- filter(data.frame(q), !V1 %in% V2) %>% group_by(V1) %>% summarise(n())
q <- j[!q[,1] %in% q[, 2]]

# Evaluate CVIs for each run, combine list into one data set.
k <- q %>%
  lapply(cvi, type = c('Sil', 'D', 'COP', 'DBstar', 'CH', 'SF')) %>%
  lapply(t) %>%
  do.call(rbind, .)

# Convert CVIs to 1 if == min/max (depending on index) and 0 otherwise.
# Sil, SF, CH, and D are to be maximized
kmax <- k %>%
  .[, c('Sil', 'SF', 'CH', 'D')] %>%
  apply(., 2,
        function(x) ifelse(x == max(x), 1, 0))

# DBstar, and COP are to be minimized
kmin <- k %>%
  .[, c('DBstar', 'COP')] %>%
  apply(., 2,
        function(x) ifelse(x == min(x), 1, 0))

# Sum across rows (i.e., for each run) to find the run with the greatest number
# of "winning" indices
k <- cbind(kmax, kmin) %>%
  data.frame %>%
  mutate(wins = rowSums(.))

# Select the run with the greatest number of CVI "wins".
l <- j[which(k$wins == max(k$wins))]

# inspect
l
plot(l)