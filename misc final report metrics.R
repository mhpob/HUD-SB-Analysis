library(lubridate); library(dplyr)
detects <- readRDS('data and imports/hud_detects.RDS') %>%
  mutate(year = lubridate::year(date.local),
         month = lubridate::month(date.local))

# Range of Hud River arrival and departure ----
detects %>%
  filter(month %in% 3:7,
         grepl('Above|Saug|Between|Newb|Below', array),
         (year == 2017 & !grepl('11(480|507)', transmitter)) |
           (year == 2018 & !grepl('11(424|479|494|500|505)', transmitter))) %>%
  group_by(year, transmitter) %>%
  summarize(min = min(date.local),
            max = max(date.local)) %>%
  group_by(year) %>%
  summarize(min_min = min(min),
            min_max = max(min),
            max_min = min(max),
            max_max = max(max))


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

transit <- unlist(transit_nest)

range(transit, na.rm = T)
median(transit, na.rm = T)

mass2ches <- detects %>%
  filter(month(date.local) %in% c(1, seq(9, 12, 1)),
         grepl('VA|Ches', array)) %>%
  mutate(year = ifelse(month(date.local) == 1, year - 1, year)) %>%
  distinct(transmitter, year) %>%
  # Subset secor.sb by transmitter x year combinations that contain VA/Ches arrays
  left_join(mutate(detects, year = ifelse(month(date.local) == 1, year - 1, year))) %>%
  mutate(array = ifelse(grepl('VA|Ches', array), 'south',
                        array)) %>%
  filter(array %in% c('MA', 'south'),
         month(date.local) %in% c(1, seq(9, 12, 1))) %>%
  arrange(transmitter, date.local) %>%
  mutate(next_array = c(.$array[seq(2, nrow(.), 1)], NA)) %>%
  filter(next_array == 'south') %>%
  distinct(transmitter, array, .keep_all = T)

trans_yr_nest <- split(mass2ches, mass2ches$transmitter) %>%
  lapply(., function(trans_nest) split(trans_nest, trans_nest$year))

transit_nest <- lapply(trans_yr_nest, function(trans_nest){
  lapply(trans_nest, function(yr_nest){
    yr_nest$date.local[2] - yr_nest$date.local[1]
  })
})

transit <- unlist(transit_nest)

range(transit, na.rm = T)
median(transit, na.rm = T)


ches2ny <- detects %>%
  mutate(array = ifelse(grepl('West|Saug|Above|Below|Between', array), 'Hudson',
                        array)) %>%
  filter(month(date.local) %in% seq(1, 5, 1),
         array == 'Hudson') %>%
  distinct(transmitter, year) %>%
  # Subset detects by transmitter x year combinations that contain Mass array
  left_join(detects) %>%
  mutate(array = ifelse(grepl('West|Saug|Above|Below|Between', array), 'Hudson',
                        array)) %>%
  filter(array %in% c('Hudson', 'VA Coast', 'Ches'),
         month(date.local) %in% seq(1, 5 , 1)) %>%
  arrange(transmitter, date.local) %>%
  mutate(next_array = c(.$array[seq(2, nrow(.), 1)], NA)) %>%
  filter(next_array == 'Hudson') %>%
  distinct(transmitter, year, array, .keep_all = T)

trans_yr_nest <- split(ches2ny, ches2ny$transmitter) %>%
  lapply(., function(trans_nest) split(trans_nest, trans_nest$year))

transit_nest <- lapply(trans_yr_nest, function(trans_nest){
  lapply(trans_nest, function(yr_nest){
    yr_nest$date.local[2] - yr_nest$date.local[1]
  })
})

transit <- unlist(transit_nest)

range(transit, na.rm = T)
median(transit, na.rm = T)
