library(dplyr)

all <- readRDS('data and imports/hud_detects.RDS') %>%
  distinct(transmitter, region) %>%
  left_join(readRDS('data and imports/recat_spawning_region.rds')) %>%
  transmute(transmitter = transmitter,
            region = region,
            # 11428 and 11504 were categorized based on 2018 clusters. Remove
            # this and things categorized as "other"
            recat17 = ifelse(grepl('11(428|504)', transmitter) |
                               recat_region == 'Other', NA,
                          as.character(recat_region)))


winner <- function(ts_obj, year){
  yr <- ifelse(year == 2017, 'r_series17', 'r_series18')
  ts_obj[[yr]][['results']][[which.max(ts_obj[[yr]][['key']]$n)]]
}

c2 <- readRDS('data and imports/cluster data/c2.rda')
c2_18 <- winner(c2, 2018)

recat18 <- data.frame(transmitter = names(c2_18@datalist), recat18 = c2_18@cluster)
recat18$recat18 <- ifelse(recat18$recat18 == 1, 'Saugerties-Coxsackie',
                          'West Point-Newburgh')
all18 <- left_join(all, recat18)

# 2017 tagged v clustered confusion matrix
table(all18[, c(3,2)])

#2018 tagged v clustered confusion matrix
table(all18[, c(4,2)])

# 2017 v 2018 clusters confusion matrix
table(all18[, c(4,3)])