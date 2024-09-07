library(tidyverse)
library(moments)
setwd('D:/wd')

inc <- read.csv(file = 'inc_corrected.csv', header = TRUE, sep = ",")

skim(inc)

str(inc)

# Średni przychód firm – jak prezentuje się w różnych branżach?

przychod <- inc %>% select(industry, revenue) %>%
  group_by(industry) %>%
  summarize(
    sredni_przychod = mean(revenue),
    mediana_przychodow = median(revenue),
    liczba = n(),
    skosnosc = skewness(revenue,na.rm = TRUE)
  )

print.data.frame(przychod)

# Analiza skośności wskazuje, że dla wszystkich branży za wyjątkiem mediów
# rozkład wartości zmiennej revenue jest prawostronnie skośny.
# W tej sytuacji najlepszym wyznacznikiem średniej wartości revenue będzie
# mediana.

przychod %>% 
  select(industry,mediana_przychodow) %>%
  arrange(desc(mediana_przychodow))


#Biorąc pod uwagę medianę, najwyższy średni przychod odnotowano dla branży:
# *Logistics & Transportation (27.6),
# *Energy (19.2), 
# *Computer Hardware (18.7).


top5przychod <- przychod %>% 
  select (industry,mediana_przychodow) %>%
  slice_max(mediana_przychodow, n = 5) %>%
  arrange(desc(mediana_przychodow))

top5przychod$industry <- factor(top5przychod$industry, 
                                levels = top5przychod$industry)

wykres <- ggplot(top5przychod, aes(x = industry, y = mediana_przychodow)) +
  geom_bar(stat = "identity", fill = "darkgray") +
  labs(title = "INC 2019: median of revenue - top5",
       x=NULL,
       y="median of revenue") +
  theme_minimal()