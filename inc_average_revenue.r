library(tidyverse)
library(moments)
setwd('D:/wd')
inc <- read.csv(file = 'inc_corrected.csv', header = TRUE, sep = ",")

# Average company revenue – how does it look in different industries?

avgrev <- inc %>% select(industry, revenue) %>%
  group_by(industry) %>%
  summarize(
   mean_rev = mean(revenue),
    median_rev = median(revenue),
    count = n(),
    skewness = skewness(revenue,na.rm = TRUE)
  )

print.data.frame(avgrev)

# Skewness analysis shows that for all industries except media,
# the distribution of the revenue variable is right-skewed.
# In this situation, the best determinant of the average revenue value will be
# the median.

avgrev %>% 
  select(industry,median_rev) %>%
  arrange(desc(median_rev))


# Considering the median, the highest average revenue was recorded for the industry:
## *Logistics & Transportation (27.6),
## *Energy (19.2),
## *Computer Hardware (18.7).


top5avgrev <- avgrev %>% 
  select (industry,median_rev) %>%
  slice_max(median_rev, n = 5) %>%
  arrange(desc(median_rev))

top5avgrev$industry <- factor(top5avgrev$industry, 
                                levels = top5avgrev$industry)

chart <- ggplot(top5avgrev, aes(x = industry, y = median_rev)) +
  geom_bar(stat = "identity", fill = "darkgray") +
  labs(title = "INC 2019: median of revenue - top5",
       x=NULL,
       y="median of revenue") +
  theme_minimal()


