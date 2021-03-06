library(tidyverse)
library(magrittr)

opioids<- read_csv('data/opioids.csv')
overdoses<- read_csv('data/overdoses.csv')
prescribers<- read_csv('data/prescriber-info.csv')
not_states <- list('AA','GU','ZZ','AE')
#remove non-state entries

prescribers <- prescribers[!(prescribers$State %in% not_states), ]
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

  