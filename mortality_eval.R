library(TelemetryR); library(ggplot2); library(lubridate); library(dplyr)
detects <- readRDS('data and imports/hud_detects.RDS')
# Overall loss
loss_overall <- trans_loss(detects, 'date.local', 'transmitter')

k <- filter(loss_overall, date <= '2018-01-01') %>%
  mutate(index = as.numeric(row.names(.)))

plot(k$index, k$remaining)

m_nls <- nls(remaining ~ a * exp(-b * index),
             start = list(a = 100, b = 0.02), data = k)
m_nls
m_lm <- lm(data = k, log(remaining) ~ index)


lines(k$index, fitted(m_lm))
lines(k$index, fitted(m_nls), col = 'red')



k <- filter(loss_overall, date >= '2016-05-20', date <= '2016-07-01') %>%
  mutate(index = as.numeric(row.names(.)))

plot(k$index, k$remaining)

m_nls <- nls(remaining ~ a * exp(-b * index),
             start = list(a = 100, b = 0.02), data = k)
m_nls
m_lm <- lm(data = k, log(remaining) ~ index)


lines(k$index, fitted(m_lm))
lines(k$index, fitted(m_nls), col = 'red')


k <- filter(loss_overall, date >= '2016-09-15', date <= '2016-12-31') %>%
  mutate(index = as.numeric(row.names(.)))

plot(k$index, k$remaining)

m_nls <- nls(remaining ~ a * exp(-b * index),
             start = list(a = 100, b = 0.02), data = k)

m_lm <- lm(data = k, log(remaining) ~ index)


lines(k$index, fitted(m_lm))
lines(k$index, fitted(m_nls), col = 'red')



k <- filter(loss_overall, date >= '2017-04-15', date <= '2017-07-01') %>%
  mutate(index = as.numeric(row.names(.)))

plot(k$index, k$remaining)

m_nls <- nls(remaining ~ a * exp(-b * index),
             start = list(a = 100, b = .2), data = k)
m_nls
m_lm <- lm(data = k, log(remaining) ~ index)


lines(k$index, fitted(m_lm))
lines(k$index, fitted(m_nls), col = 'red')


k <- filter(loss_overall, date >= '2017-08-15', date <= '2017-11-01') %>%
  mutate(index = as.numeric(row.names(.)))

plot(k$index, k$remaining)

m_nls <- nls(remaining ~ a * exp(-b * index),
             start = list(a = 100, b = .2), data = k)
m_nls
m_lm <- lm(data = k, remaining ~ index)


lines(k$index, fitted(m_lm))
lines(k$index, fitted(m_nls), col = 'red')

# ----
c2 <- readRDS('data and imports/cluster data/c2.rda')
temp <- data.frame(transmitter = names(c2[[1]]$results[[1]]@datalist),
                   cluster = c2[[1]]$results[[1]]@cluster)%>%
  left_join(distinct(detects, transmitter, region)) %>%
  mutate(cluster = case_when(cluster == 1 ~ 'Up-River',
                             T ~ 'Salt Front'),
         correct = case_when(cluster == region ~ T,
                             T ~ F))

clustered <- detects %>%
  right_join(temp)

j <- split(clustered, clustered$transmitter)
j <- lapply(j, track, dates = 'date.local', ids = c('array', 'cluster'))
p <- bind_rows(j, .id = 'transmitter')
