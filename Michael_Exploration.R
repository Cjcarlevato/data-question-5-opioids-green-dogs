library(tidyverse)
library(magrittr)

opioids<- read_csv('data/opioids.csv')
overdoses<- read_csv('data/overdoses.csv')
#prescribers<- read_csv('data/prescriber-info.csv')
#prescribers_2016<- read_csv('data/opioid_prescribers_2016.csv')
#prescribers_2016<- read_csv('data/opioid_prescribers_2016.csv')
prescribers<- read_csv('data/opioid_prescribers_2015.csv')
#opioid_prescribers_2016 <- read_csv('data/opioid_prescribers_2016.csv')
zips <- read_csv('data/zips.csv') %>% select(Zip = zip, County = COUNTYNAME, FP = county) %>% mutate(FP = as.numeric(FP))
not_states <- list('AA','GU','ZZ','AE')
#remove non-state entries

prescribers <- prescribers[!(prescribers$State %in% not_states), ]

prescribers <- inner_join(prescribers, zips, by = 'Zip')

prescriptions_by_state <- prescribers %>% group_by(State) %>% summarize(Total_Claims = sum(Total_Claim_Count)) %>% select(Abbrev = State, Total_Claims) %>% 
  inner_join(overdoses[, c('Population', 'Abbrev')]) %>% mutate(Claims_Per_100000 = Total_Claims / Population * 100000)

ggplot(prescriptions_by_state, aes(x=reorder(Abbrev, Claims_Per_100000), y = Claims_Per_100000)) + geom_col()


TN_Counties <- prescribers %>% group_by(State, FP) %>% summarize(Total_Claims = sum(Total_Claim_Count)) %>% filter(State == 'TN', FP %/% 1000 == 47)

library(ggmap)
library(rgeos)
library(maptools)

sh <- readShapePoly('data/cb_2017_us_county_500k/cb_2017_us_county_500k.shp')
TN_sh <- sh[sh$STATEFP == 47,]
plot(TN_sh)

popest <- read_csv('data/popest.csv') %>% select(FP = GEO.id2, County = 'GEO.display-label', Pop = respop72016)
TN_Counties <- inner_join(TN_Counties, popest) 
TN_Counties <- TN_Counties %>% mutate(Claims_Per_100000 = 100000 * Total_Claims / Pop)
#TN_Counties <- TN_Counties %>% filter(FP %/% 1000 == 47)  #Remove any county not in TN
#TN_Counties <- TN_Counties %>% mutate(COUNTYFP = FP - 47000)

TN_sh@data <- merge(TN_sh@data, TN_Counties, by.x = 'GEOID', by.y= 'FP')

p <- colorRampPalette(c("white", "red"))(128)
palette(p)

claims <- TN_sh@data$Claims_Per_100000
cols <- (claims - min(claims))/diff(range(claims))*127+1 #scale to the palette
plot(TN_sh, col=cols)

demo <- read_csv('data/UnemploymentReport.csv') %>% select(FP = FIPS, Unemployment = `2015`, Med_Income = Median_Household_Income_2016)
TN_Counties <- TN_Counties %>% inner_join(demo, by = 'FP')

library(scales)
prescribers %>% 
  group_by(Generic_Name) %>% 
  summarize(sum(Total_Claim_Count)) %>% 
  arrange(desc(`sum(Total_Claim_Count)`)) %>% 
  top_n(10, `sum(Total_Claim_Count)`) %>% 
  ggplot(aes(x = reorder(Generic_Name, `sum(Total_Claim_Count)`), y = `sum(Total_Claim_Count)`)) + geom_col() + coord_flip() +
  xlab('Generic Name') +
  ylab('Total Number of Prescriptions') +
  ggtitle('Most Frequently Prescribed Opioids - 2016') +
  theme_bw() + scale_y_continuous(labels = comma)

prescribers %>% filter(State == 'TN') %>% summarize(sum(Total_Claim_Count)/66)
popest %>% filter(FP %/% 1000 == 47) %>% summarize(sum(Pop))

head(opioids)
head(overdoses)
head(prescribers)
oxycontin <- prescribers %>% 
  group_by(State,Gender) %>% 
  summarize(sum(OXYCONTIN))


ggplot(data = oxycontin, 
       aes(reorder(State, `sum(OXYCONTIN)`), `sum(OXYCONTIN)`)) +
  geom_bar(stat = 'identity', aes(fill = Gender)) +
  xlab('State') +
  ylab('Total Number of Prescriptions') +
  ggtitle('        Oxycontin Prescriptions') +
  theme_bw()

ggplot(oxycontin, aes(x = reorder(State, `sum(OXYCONTIN)`), y = `sum(OXYCONTIN)`)) + geom_col(aes(fill = Gender))

overdoses <- overdoses %>% mutate(Deaths_Per_100000 = Deaths/Population * 100000) %>% 
  arrange(Deaths_Per_100000)

ggplot(overdoses, aes(x = reorder(Abbrev, Deaths_Per_100000), y = Deaths_Per_100000)) + geom_col() +
  xlab('State') + 
  ylab('Number of Deaths per 100,000 Residents') +
  ggtitle('Prescription Drug Overdoses') +
  theme_bw()

opioid_vec
opioid_vec = c(opioids[['Drug Name']], opioids[['Generic Name']])

opioid_vec = gsub("-", ".", opioid_vec)
opioid_vec = gsub("/", ".", opioid_vec)

intersect(names(prescribers), opioid_vec)

names(prescribers)

opioid_prescribers <- prescribers %>% select(NPI, Gender, State, Credentials, Specialty, Opioid.Prescriber, intersect(names(prescribers), opioid_vec))

opioid_prescribers <- opioid_prescribers %>% mutate(Total_Opioids = ACETAMINOPHEN.CODEINE + FENTANYL + HYDROCODONE.ACETAMINOPHEN + OXYCODONE.ACETAMINOPHEN + OXYCONTIN)

op_by_state <- opioid_prescribers %>% group_by(State) %>% summarize(sum(Total_Opioids)) %>% 
  arrange(`sum(Total_Opioids)`) %>% mutate(Abbrev = State) %>% 
  inner_join(overdoses, by = 'Abbrev') %>% mutate(Prescriptions_Per_100000 = `sum(Total_Opioids)` / Population *100000) %>% 
  arrange(Prescriptions_Per_100000)

ggplot(data = op_by_state, 
       aes(reorder(Abbrev, Prescriptions_Per_100000), Prescriptions_Per_100000)) + geom_col()

opioid_prescribers = opioid_prescribers %>% arrange(desc(Total_Opioids)) %>% 
  mutate(Cumulative_Count = cumsum(Total_Opioids)) %>% mutate(Cumulative_Percentage = Cumulative_Count / sum(Total_Opioids))

opioid_prescribers %>% filter(Opioid.Prescriber == 1)

op_docs <- opioid_prescribers %>% filter(Opioid.Prescriber == 1)

op_docs <- op_docs %>% mutate(Position = cumsum(Opioid.Prescriber)) %>% mutate(Percentile = 1 - Position/sum(Opioid.Prescriber))

ggplot(op_docs, aes(x = Percentile, y = Total_Opioids)) + geom_line()

# About 25% of Opioid Prescribers are prescribing 80% of the opioids (for the 5 that we have identified as opioids)
ggplot(op_docs, aes(x = Percentile, y = 1 - Cumulative_Percentage)) + geom_line() + 
  xlab('Prescriber Percentile') +
  ylab('Cumulative Percentage of Prescriptions') +
  theme_bw() +
  geom_hline(yintercept =0.2, linetype="dashed", color = "red")

TN_Counties %>% arrange(Claims_Per_100000)
  
