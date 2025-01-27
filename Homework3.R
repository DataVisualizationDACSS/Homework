linkBoston="https://github.com/DACSS-Visual/SpatialData/raw/refs/heads/main/data/BostonContrib.xlsx"
bostonCont=rio::import(linkBoston)

library(sf)
linkZips='https://raw.githubusercontent.com/DACSS-Visual/SpatialData/refs/heads/main/data/zip_codes.json'
bostonZips=sf::read_sf(linkZips)

plot(bostonZips[2])

library(ggplot2)
library(tidyverse)

base = ggplot(data = bostonZips) +
  geom_sf(fill = 'black')

base

##########################################################################################################

bostonstats = bostonCont %>%
  filter(`Tender Type Description` == 'Credit Card' | `Tender Type Description` == 'Cash') %>%
  select(Zip, Amount, `Tender Type Description`)

bostonAgg = bostonstats %>%
  group_by(Zip, `Tender Type Description`) %>%
  summarise_at(vars(Amount),
               list(counts = length,
                    avgtransaction = mean))

bostonAgg = bostonAgg %>%
  group_by(Zip) %>%
  mutate(percentage = counts / sum(counts) * 100)

df = merge(bostonZips, bostonAgg,
           by.x = 'ZIP5',
           by.y = 'Zip')

##########################################################################################################

a = ggplot() + theme_void() +
  geom_sf(data = df,
          aes(fill = avgtransaction)) +
  scale_fill_viridis_c(direction = -1,
                       na.value = 'red') +
  facet_grid(cols = vars(`Tender Type Description`))

map = ggplot() + geom_sf(fill = 'white') + theme_void() +
  geom_sf(data = df,
          aes(fill = avgtransaction), color = NA)+
  scale_fill_viridis_c(direction = -1) +
  facet_grid(cols = vars(`Tender Type Description`)) +
  labs(fill = 'Average Amount Spent \n Per Transaction ($USD)',
       title = 'The Versatility of the Modern Credit Card',
       caption = "Credit cards are used in a wide variety of purchases, while cash is consistently used for smaller transaction types.") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = -0.5))

map
#########################################################################################################

saveRDS(map, file = "plot3.rds")