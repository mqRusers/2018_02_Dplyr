library('dplyr')

data("PlantGrowth")
str(PlantGrowth)

summary<- PlantGrowth %>% group_by(group) %>% summarise(mean = mean(weight), median = median(weight))

plot(mean~group, summary)

tapply(PlantGrowth$weight, PlantGrowth$group, mean)

summary<- PlantGrowth %>% 
    group_by(group) %>% 
    filter(group != "trt2") %>%
    summarise(mean = mean(weight), median = median(weight))

blah <- data.frame(weight = c(0,0,0,0), group = c(rep("zero", 4)))

summary<- PlantGrowth %>% 
  bind_rows(blah) %>%
  group_by(group) %>% 
  filter(weight > 0.1) %>%
  summarise(mean = mean(weight), median = median(weight))

summary<- PlantGrowth %>% 
  bind_rows(blah) %>%
  mutate(group = as.factor(group)) %>%
  group_by(group) %>% 
  filter(weight > 0.1) %>%
  summarise(mean = mean(weight), median = median(weight))


summary<- PlantGrowth %>% 
  mutate(weight = log10(weight)) %>%
  group_by(group) %>% 
  filter(weight > 0.1) %>%
  summarise(mean = mean(weight), median = median(weight))

install.packages("tidyr")
require(tidyr)
install.packages("readr")
require(readr)

stocks <- data.frame(
    time = as.Date("2009-01-01") + 0:9,
    x = rnorm(10, 0, 1),
    y = rnorm(10, 0, 2), 
    z = rnorm(10, 0, 4)
)

# just like melt
stocksm <- stocks %>% gather(stock, price)
stocksm <- stocks %>% 
  gather(stock, price, -time) >%>
  filter(price > -1)
  
stocksm %>% spread(stock, price)

arrange(stocks, time)
arrange(stocks, desc(time))




