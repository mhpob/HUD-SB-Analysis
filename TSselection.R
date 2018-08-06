

# Replicate n number of times. Save results in a list.
# To do: switch to parSapply
library(parallel)
cl <- makeCluster(detectCores() - 1)
clusterEvalQ(cl, library(dtwclust))
clusterEvalQ(cl, library(dplyr))
clusterExport(cl, 'r_series18')
all_results <- parSapply(cl, 1:25, function(i){
  tsclust(series = r_series18, k = 3, distance = 'dtw_basic',
          centroid = 'median',
          window.size = 7, trace = F,
          control = partitional_control(pam.precompute = FALSE,
                                        iter.max = 500))
})


# Remove clusters with only one time series
trim_results <- sapply(all_results, function(x){1 %in% x@clusinfo$size})
trim_results <- all_results[!trim_results]

# Remove clusters that are equal
combos <- t(combn(1:length(trim_results), 2))

clusterExport(cl, 'trim_results')
clusterExport(cl, 'combos')
combos_equal <- parApply(cl, combos, 1, function(x){
  dplyr::setequal(trim_results[[x[1]]]@clusinfo, trim_results[[x[2]]]@clusinfo)
})
stopCluster(cl)
# q is switching to combos
combos <- cbind(combos, combos_equal)

# Note: The below has a side-effect of dropping unique solutions
# Okay with this, as a unique solution out of 1 << reps should have little support
combos <- combos[combos[, 3] == 1,]
combos <- combos[!combos[, 1] %in% combos[, 2],]

run_freq <- group_by(data.frame(combos), V1) %>% summarise(n())
trim_results <- trim_results[unique(combos[,1])]

# inspect
best_fit
plot(best_fit)


## Orignially thought to run CVIs for each  output ----
## However, this leads to some strange behavior (over-emphasis on fewer trends)
## Found it to be better to just pick those that emerge most-often. Keeping this
## code here because I worked hard on it, and something of the sort could be useful
## later. If used, this should go as the step after counting how many fits were
## repeated.

# Evaluate CVIs for each run, combine list into one data set.
# cvis <- trim_results %>%
#   lapply(cvi, type = c('Sil', 'D', 'COP', 'DBstar', 'CH', 'SF')) %>%
#   lapply(t) %>%
#   do.call(rbind, .)

# Convert CVIs to 1 if == min/max (depending on index) and 0 otherwise.
# Sil, SF, CH, and D are to be maximized
# cvimax <- cvis %>%
#   .[, c('Sil', 'SF', 'CH', 'D')] %>%
#   apply(., 2,
#         function(x) ifelse(x == max(x), 1, 0))

# DBstar, and COP are to be minimized
# cvimin <- cvis %>%
#   .[, c('DBstar', 'COP')] %>%
#   apply(., 2,
#         function(x) ifelse(x == min(x), 1, 0))

# Sum across rows (i.e., for each run) to find the run with the greatest number
# of "winning" indices
# cvis <- cbind(cvimax, cvimin) %>%
#   data.frame %>%
#   mutate(wins = rowSums(.)) %>%
#   cbind(cvis)

# Select the run with the greatest number of CVI "wins".
# best_fit <- trim_results[which(cvis$wins == max(cvis$wins))]