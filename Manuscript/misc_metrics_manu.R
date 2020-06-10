# Adapted from "misc final report metrics.R"

library(lubridate); library(dplyr)
detects <- readRDS('data and imports/hud_detects.RDS') %>%
  mutate(year = lubridate::year(date.local),
         month = lubridate::month(date.local),
         region = ifelse(grepl('Saug', region), 'Upper', 'Lower'))

recats <- read.csv('manuscript/recategorized.csv')

# Arrive/depart Hudson R ----
detects %>%
  distinct(transmitter, date.local, .keep_all = T) %>%
  filter(month %in% 3:7,
         grepl('Above|Saug|Between|Newb|Below', array)) %>%
  left_join(recats[, 1:3]) %>%
  group_by(year, transmitter, cluster17) %>%
  summarize(min = min(date.local),
            max = max(date.local)) %>%
  group_by(year, cluster17) %>%
  summarize(mean(max),
            median(max))



# Average transit rates ----
ny2mass <- detects %>%
  filter(month(date.local) %in% seq(2, 9, 1),
         array == 'MA') %>%
  distinct(transmitter, year) %>%
  # Subset detects by transmitter x year combinations that contain Mass array
  left_join(detects) %>%
  mutate(array = ifelse(grepl('West|Saug|Above|Below|Between', array), 'Hudson',
                        array)) %>%
  filter(array %in% c('Hudson', 'MA'),
         month(date.local) %in% seq(2, 9, 1)) %>%
  arrange(transmitter, date.local) %>%
  mutate(next_array = c(.$array[seq(2, nrow(.), 1)], NA)) %>%
  filter(next_array == 'MA') %>%
  distinct(transmitter, year, array, .keep_all = T)

trans_yr_nest <- split(ny2mass, ny2mass$transmitter) %>%
  lapply(., function(trans_nest) split(trans_nest, trans_nest$year))

transit_nest <- lapply(trans_yr_nest, function(trans_nest){
  lapply(trans_nest, function(yr_nest){
    yr_nest$date.local[2] - yr_nest$date.local[1]
  })
})

transit <- bind_rows(transit_nest, .id = 'transmitter')


transit2mass <- transit %>%
  left_join(detects %>%
              distinct(transmitter, region)) %>%
  left_join(recats[, 1:3])

transit2mass %>%
  tidyr::pivot_longer(cols = starts_with('2'),
                      names_to = 'year',
                      values_to = 'time') %>%
  filter(!is.na(cluster17)) %>%
  group_by(year, cluster17) %>%
  summarize(min = min(as.numeric(time), na.rm = T) / 60 / 60 / 24,
            max = max(as.numeric(time), na.rm = T) / 60 / 60 / 24,
            mean = mean(as.numeric(time), na.rm = T) / 60 / 60 / 24)
