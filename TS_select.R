TS_select <- function(ts_data, reps, n_clusters, dist, cent, window){
  library(parallel); library(dplyr)

  # Replicate n number of times. Save results in a list.
  cat('Replicating... \n')
  cl <- makeCluster(detectCores() - 1)
  clusterEvalQ(cl, library(dtwclust))
  clusterExport(cl, c('ts_data', 'n_clusters', 'dist', 'cent', 'window'),
                envir = environment())

  all_results <- parSapply(cl, 1:reps, function(i){
    tsclust(series = ts_data, k = n_clusters, distance = dist,
            centroid = cent,
            window.size = window, trace = F,
            control = partitional_control(pam.precompute = FALSE,
                                          iter.max = 500))
  })

  stopCluster(cl)

  # Remove clusters with only one time series
  cat('Trimming... \n')
  trim_results <- sapply(all_results, function(x){1 %in% x@clusinfo$size})
  trim_results <- all_results[!trim_results]

  # Remove clusters that are equal
  combos <- t(combn(1:length(trim_results), 2))

  combos_equal <- apply(combos, 1, function(x){
    # Need to use dplyr::setequal. Won't work with base::setequal
    dplyr::setequal(trim_results[[x[1]]]@clusinfo, trim_results[[x[2]]]@clusinfo)
  })

  combos <- cbind(combos, combos_equal)

  # Note: The below has a side-effect of dropping unique solutions
  # Okay with this, as a unique solution out of 1 << reps should have little support
  combos <- combos[combos[, 3] == 1,]
  combos <- combos[!combos[, 1] %in% combos[, 2],]

  cat('Summarizing... \n')
  run_freq <- combos %>%
    data.frame %>%
    group_by(V1) %>%
    summarize(n()) %>%
    rename(index = V1, n = `n()`) %>%
    mutate(index = row.names(.))
  trim_results <- trim_results[unique(combos[,1])]

  # join
  list(results = trim_results, key = run_freq)
}

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