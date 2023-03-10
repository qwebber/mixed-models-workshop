
library(lme4)
library(nlme)
library(ggplot2)
library(broom.mixed)
library(tidyverse)

## load dragon data 
load("dragons.RData")

## convert to tibble and check out the columns
dragons <- as_tibble(dragons)

## remove X column and view head of dataset
dragons <-   
  dragons %>% 
  select(-X) 

## check relationships between all pairwise combinations of variables
dragons %>% GGally::ggpairs()


dragons %>% 
  select(mountainRange, site, testScore) %>% 
  filter(!is.na(testScore)) %>% 
  group_by(mountainRange, site) %>% 
  summarise(average_test = mean(testScore)) %>% 
  ggplot(aes(x = mountainRange, 
             y = average_test, 
             fill = mountainRange)) +
  geom_col() +
  scale_fill_viridis_d() +
  theme(axis.text.x = element_text(hjust = 1, angle = 45)) +
  facet_wrap(~ site)

dragons %>% 
  ggplot(aes(x = bodyLength, 
             y = testScore, 
             color = mountainRange)) + 
  geom_point() 

summary(lmer(bodyLength ~ testScore + (1|mountainRange), data = dragons))

as_tibble(dragons)

dragons %>% GGally::ggpairs() 

ggplot(dragons) +
  geom_boxplot(aes(mountainRange, bodyLength)) +
  facet_wrap(~site)
