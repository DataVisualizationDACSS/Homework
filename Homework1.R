rm(list = ls())

location='https://github.com/DACSS-Visual/tabular_univar_cat/raw/main/data/'
file='eduwa.rda'
link=paste0(location,file)

load(file=url(link))

table(eduwa$LocaleType)
cityEduwa=eduwa[eduwa$LocaleType=='City',]
table(cityEduwa$LocaleSub)
cityEduwa$LocaleSub=droplevels(cityEduwa$LocaleSub)

# you start from here
table1 = table(cityEduwa$LocaleSub)
table2 = prop.table(table1)*100

library(tidyverse)

df = as.data.frame(table2)
names(df) = c("LocaleSub", "Pct")
df

plot1 = ggplot(data = df,
               aes(x = reorder(LocaleSub, Pct), y = Pct)) +
  theme_classic() +
  geom_bar(fill = "lightblue", stat = "identity") +
  labs(title = "Types of Cities",
       x = "City Size",
       y = "Percentage",
       subtitle = "What types of cities are represented in the data?",
       caption = "Source: US Department of Education") +
  geom_text(vjust = 0,
            size = 3,
            aes(y = Pct,
                label = paste0(round(Pct, 2), '%'))) +
  theme(plot.subtitle = element_text(hjust = 0),
        plot.title = element_text(hjust = 0.5))

plot1

saveRDS(plot1, file = "plot1.rds")

