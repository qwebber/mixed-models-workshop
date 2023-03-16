
library(lme4)
library(nlme)
library(ggplot2)
library(tidyverse)

## load dragon data 
load("dragons.RData")

## convert to tibble and check out the columns
dragons <- as_tibble(dragons)

## remove X column and 
dragons <-   
  dragons %>% 
  select(-X) 

## view head of dataset 
head(dragons)

## plot differences in mean test scores across mountain ranges and sites 
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

## plot relationship between test score and body length by mountain range
dragons %>% 
  ggplot(aes(x = bodyLength, 
             y = testScore, 
             color = mountainRange)) + 
  geom_point() 

## check relationships between all pairwise combinations of variables
dragons %>% GGally::ggpairs()

## run linear model
model1 <- lm(testScore ~ bodyLength + mountainRange, data = dragons)
summary(model1)

## run mixed model
model2 <- lmer(testScore ~ bodyLength + (1|mountainRange), data = dragons)
summary(model2)
