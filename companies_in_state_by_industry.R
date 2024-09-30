# A script checking how many companies from a given area operate 
# in particular states


# Load data
inc <-  read.csv("inc_corrected.csv")

#Load libraries
library(tidyverse)


#Prepare data
inc_geogr <- inc %>% select(state,industry,name,revenue)


# Analysis

inc_overall_count <- inc_geogr %>%
  group_by(state) %>%
  summarise(
    total = n(), ## total = all companies in state
    distinct_industries_in_state = n_distinct(industry)
  ) %>%
  arrange(desc(total))


inc_industry_count <- inc_geogr %>%
  group_by(state,industry) %>%
  summarise(
    number = n() # number of companies in particular industries in state
  ) %>%
  arrange(
    state,
    desc(number)
    )

inc_table <- left_join(inc_industry_count,inc_overall_count, by = "state")

inc_table <- inc_table %>%
  mutate(
    industry_percentage = round(((number / total) * 100),2)
  )


inc_table <- inc_table %>%
  relocate(distinct_industries_in_state, .after = industry_percentage)



# Conclusions

## * Most companies on the INC 2019 list are headquartered in California.

## * There are companies in each of the 27 industries on the INC list in 
##   California
