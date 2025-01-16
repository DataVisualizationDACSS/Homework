linkMass="https://github.com/DACSS-Visual/tabular_bivar_catcat/raw/refs/heads/main/data/MSP%20DFS%20Arrests%2019-20Q1.xlsx"

library(rio)
library(tidyverse)
arrests=rio::import(linkMass,which = 1)
head(arrests)

copy = arrests %>%
  mutate(offense = case_when(`Arrest Offense by UCR Code` == '09A' ~ "Murder",
                             `Arrest Offense by UCR Code` == '100' ~ "Kidnapping",
                             `Arrest Offense by UCR Code` == '120' ~ 'Robbery',
                             `Arrest Offense by UCR Code` == '200' ~ 'Arson',
                             `Arrest Offense by UCR Code` == '210' ~ 'Blackmail',
                             `Arrest Offense by UCR Code` == '220' ~ 'Burglary',
                             `Arrest Offense by UCR Code` == '240' ~ 'Motor Theft',
                             `Arrest Offense by UCR Code` == '250' ~ 'Counterfeiting',
                             `Arrest Offense by UCR Code` == '270' ~ 'Embezzlement',
                             `Arrest Offense by UCR Code` == '280' ~ 'Stolen Property Offenses',
                             `Arrest Offense by UCR Code` == '290' ~ 'Vandalism',
                             `Arrest Offense by UCR Code` == '370' ~ 'Pornography',
                             `Arrest Offense by UCR Code` == '510' ~ 'Bribery',
                             `Arrest Offense by UCR Code` == '520' ~ 'Weapon Law Violations',
                             `Arrest Offense by UCR Code` == '720' ~ 'Animal Cruelty',
                             `Arrest Offense by UCR Code` == '11A' ~ 'Rape',
                             `Arrest Offense by UCR Code` == '11B' ~ 'Sodomy',
                             `Arrest Offense by UCR Code` == '11C' ~ 'Sexual Assault with an Object',
                             `Arrest Offense by UCR Code` == '11D' ~ 'Fondling',
                             `Arrest Offense by UCR Code` == '13A' ~ 'Aggravated Assault',
                             `Arrest Offense by UCR Code` == '13B' ~ 'Simple Assault',
                             `Arrest Offense by UCR Code` == '13C' ~ 'Intimidation',
                             `Arrest Offense by UCR Code` == '23A-H' ~ 'Larceny',
                             `Arrest Offense by UCR Code` == '26A-G' ~ 'Fraud Offenses',
                             `Arrest Offense by UCR Code` == '35A-B' ~ 'Drug/Narcotic Offenses',
                             `Arrest Offense by UCR Code` == '36A' ~ 'Incest',
                             `Arrest Offense by UCR Code` == '36B' ~ 'Statutory Rape',
                             `Arrest Offense by UCR Code` == '39A-D' ~ 'Gambling Offenses',
                             `Arrest Offense by UCR Code` == '40A-C' ~ 'Prostitution Offenses',
                             `Arrest Offense by UCR Code` == '64A-B' ~ 'Human Trafficking Offenses',
                             `Arrest Offense by UCR Code` == '90A' ~ 'Bad Checks',
                             `Arrest Offense by UCR Code` == '90B' ~ 'Curfew/Loitering/Vagrancy Violations',
                             `Arrest Offense by UCR Code` == '90C' ~ 'Disorderly Conduct',
                             `Arrest Offense by UCR Code` == '90D' ~ 'DUI',
                             `Arrest Offense by UCR Code` == '90E' ~ 'Drunkeness',
                             `Arrest Offense by UCR Code` == '90F' ~ 'Family Offenses, Nonviolent',
                             `Arrest Offense by UCR Code` == '90G' ~ 'Liquor Law Violations',
                             `Arrest Offense by UCR Code` == '90H' ~ 'Peeping Tom',
                             `Arrest Offense by UCR Code` == '90J' ~ 'Trespassing',
                             `Arrest Offense by UCR Code` == '90Z' ~ 'All Other Offenses',
                             `Arrest Offense by UCR Code` == 'MV' ~ 'MV'),
         race = case_when(Race == 'I' ~ 'Native',
                          Race == 'O' ~ 'Asian',
                          Race == 'B' ~ 'Black',
                          Race == 'H' ~ 'Hispanic',
                          Race == 'J' ~ 'Middle Eastern',
                          Race == 'W' ~ 'White',
                          Race == 'U' ~ 'Unknown',
                          Race == 'N' ~ 'Not Applicable'))
main = copy %>%
  mutate(offense = replace_na(offense, 'All Other Offenses'))


CrimeDay = table(main$offense, main$race)

df = as.data.frame(CrimeDay)
names(df) = c("crime", "race", "counts")

mgCol = 100*prop.table(CrimeDay, margin = 2)

df$pct = round(as.data.frame(mgCol)[,3],1)

# Starting Here

base_points = ggplot(df, aes(x = race, y = reorder(crime, pct)))
tablePlot = base_points + geom_point(aes(size = pct))
tablePlot = tablePlot + geom_text(aes(label = pct),
                                  nudge_x = 0.3,
                                  size = 3)
tablePlot

base2 = ggplot(df, aes(x = crime, y = pct))
bars2 = base2 + geom_bar(stat = "identity") + theme_minimal()
bars2 = bars2 + facet_grid(~ race)
bars2

barsFacet = bars2 + facet_grid(~ race)
barsFacet + coord_flip()


baseRE = ggplot(df, 
                aes(x = reorder(crime, pct),
                    y = pct)) + theme_minimal()
barsRE = baseRE + geom_bar(stat = "identity")
barsREFacet = barsRE + facet_grid(~ race)
barsREFacet = barsREFacet + coord_flip()

barsREFacet


baseHeat = ggplot(df, aes(x = race,
                          y = reorder(crime, pct),
                          fill = pct*100)) + theme_classic()
heatDefault = baseHeat + geom_tile()
heatDefault


heatDefault = heatDefault + labs(y = "Crime", x = "Race", title = "Specific Type of Crime Frequency by Race", subtitle = "")
heatDefault = heatDefault + theme(axis.text.x = element_text(angle = 60,
                                                             vjust = 0.6),
                                  legend.title = element_blank(),
                                  legend.position = 'top',
                                  legend.direction = 'horizontal',
                                  legend.key.width = unit(1, "cm"),
                                  legend.key.height = unit(1, "cm"))
heatDefault

heatWithRed = heatDefault + scale_fill_gradientn(colors = rev(colorspace::heat_hcl(4)))
heatWithRed

saveRDS(heatWithRed, file = "plot2.rds")
