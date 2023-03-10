
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


summary(lmer(bodyLength ~ testScore + (1|mountainRange), data = dragons))

as_tibble(dragons)

dragons %>% GGally::ggpairs() 

ggplot(dragons) +
  geom_boxplot(aes(mountainRange, bodyLength)) +
  facet_wrap(~site)
