
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

## where's my p-value?

## Extract full model summary (including p-values for each fixed effect):
jtools::summ(model2)

## another option is to use the nlme R package (function is lme) -- this is the package used by Zuur et al in their books
model3 <- lme(testScore ~ bodyLength, random = ~1|mountainRange, data = dragons)
summary(model3)


## generate fake dataset for nested random effects
df <- tibble(A = runif(150), 
             B = runif(150),
             ID = rep(c("A", "B", "C", "D", "E", 
                        "F", "G", "H", "I", "J",
                        "H", "I", "K", "L", "M"), 
                      times = 10),
             population = rep(c("1", "2", "3", "4", "5", 
                                "6", "7", "8", "9", "10"), 
                              each = 15))

head(df)

## mixed model syntax
mod1 <- lmer(A ~ B + (1 | population/ID), data = df)


## generate fake dataset for crossed random effects
df <- tibble(A = runif(180), 
             B = runif(180),
             ID = rep(c("A", "B", "C", "D", "E", 
                        "F", "G", "H", "I", "J",
                        "H", "I", "K", "L", "M"), 
                      times = 12),
             season = rep(c("early winter", "mid winter", "late winter", 
                            "early spring", "mid spring", "late spring",
                            "early summer", "mid summer", "late summer", 
                            "early fall", "mid fall", "late fall"), 
                          each = 15))

head(df)

## mixed model syntax
mod1 <- lmer(A ~ B + (1 | season) + (1 | ID), data = df)


## mixed model with mountain range and site as nested random effects
model4 <- lmer(testScore ~ bodyLength + (1|mountainRange/site), data = dragons)
summary(model4)


ggplot(dragons, aes(x = bodyLength, y = testScore, colour = site)) +
  facet_wrap(~mountainRange, nrow=2) +   # a panel for each mountain range
  geom_point(alpha = 0.5) +
  theme_classic() +
  geom_line(data = cbind(dragons, pred = predict(model3)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
  theme(legend.position = "none",
        panel.spacing = unit(2, "lines"))  # adding space between panels
