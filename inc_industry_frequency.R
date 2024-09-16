# Which industries are the most and least numerous? 

## Load libraries

library(dplyr)
library(ggplot2)
library(scales)

## Load data

setwd('D:/wd/')
inc <- read.csv("inc_corrected.csv",header = TRUE)

## Count industry

industry_freq <- inc %>%  select(industry) %>%
  count(industry, name = "number") %>%
  group_by(industry) %>%
  mutate(
    frequency = (number/nrow(inc))
        ) %>%
  arrange(frequency)

industry_freq

industry_freq$industry <- factor(industry_freq$industry,
                                 levels = industry_freq$industry)
## Draw chart

ggplot(count_ind,aes(x = industry_freq$industry, y = industry_freq$frequency)) +
  geom_col(width = 0.8) +
  geom_text(aes(label = industry_freq$number, hjust = - 0.35), size = 3) +
  coord_flip() +
  labs(title = "INC - number of companies per industry",
       x = "Industry",
       y = "Frequency") +
  scale_y_continuous(labels = percent_format()) +
  theme_minimal()

