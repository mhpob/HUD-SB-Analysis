

# Replicate n number of times. Save results in a list.
# To do: switch to parSapply
j <- replicate(n = 100,
               tsclust(series = r_series18, k = 3, distance = 'dtw_basic',
                       centroid = 'median',
                       window.size = 7, trace = F,
                       control = partitional_control(pam.precompute = FALSE,
                                                     iter.max = 500)),
               simplify = F)

# Evaluate CVIs for each run, combine list into one data set.
k <- j %>%
  lapply(cvi) %>%
  lapply(t) %>%
  do.call(rbind, .)

# Convert CVIs to 1 if == min/max (depending on index) and 0 otherwise.
# Sil, SF, CH, and D are to be maximized
kmax <- k %>%
  .[, c('Sil', 'SF', 'CH', 'D')] %>%
  apply(., 2,
        function(x) ifelse(x == max(x), 1, 0))

# DB, DBstar, and COP are to be minimized
kmin <- k %>%
  .[, c('DB', 'DBstar', 'COP')] %>%
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