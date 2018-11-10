library(tidyverse)
library(magrittr)
library(stringr)

opioids<- read_csv('data/opioids.csv')
overdoses<- read_csv('data/overdoses.csv')
prescribers<- read_csv('data/prescriber-info.csv')
overdoses_1999_2014 <- read.csv('data/opioid_overdoses_1999_2014.csv')
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


#Datasets Questions:
#1. Explore the dataset. Consider plotting the number of types of opioids by state and number of opioid prescribers by state. Understand the most prevalent types of opioids prescribed. Explore the states with the highest prevalence of fatal opioid overdoses. How does this compare to total overdoses? Is there a correlation between type of opioids prescribed and overdose deaths? What patterns emerge? Use different types of plots to visualize your data (histogram, bee swarm, box plot, etc...).

#2. Test the assumption of the Pareto effect on opioid prescribing using the dataset (i.e., 20% of prescribers provide 80% of the opioids).

#3. Build a model to predict the number of fatal opioid overdoses. Consider building the model for a specific state, but that’s not necessary. Be creative—consider crossing the raw features that are available and creating new features (i.e., total type of opioids). Socioeconomics are often a driving factor for health outcomes. Can you find another dataset that provides information at the state level and can be used in your model? Can you perform tests to explain the variability in your outcome (i.e, effect estimation)?
  
ggplot (overdoses, aes(x= Abbrev, y = Deaths)) + geom_col()

overdoses <- overdoses %>% 
  mutate(Deaths_Per_100000 = Deaths/Population * 100000) %>%
  arrange(Deaths_Per_100000)


ggplot (overdoses, aes(x= Abbrev, y = Deaths_Per_100000)) + geom_col() 



prescribers <- prescribers[!(prescribers$State %in% not_states), ]
head(opioids)
head(overdoses)
head(prescribers)
oxycontin <- prescribers %>% 
  group_by(State,Gender) %>% 
  summarize(sum(OXYCONTIN))


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

overdoses_1999_2014 <- overdoses_1999_2014 %>%
  mutate(Deaths = as.character(Deaths), Year = as.character(Year), Population = as.character(Population), as.character(Crude.Rate), as.character(Prescriptions.Dispensed.by.US.Retailers.in.that.year..millions.)) %>%
  mutate(Deaths = as.numeric(Deaths), Year = as.numeric(Year), Population = as.numeric(Population), as.numeric(Crude.Rate), as.numeric(Prescriptions.Dispensed.by.US.Retailers.in.that.year..millions.)) 



tn_overdoses <- overdoses_1999_2014 %>% 
  filter(State == "Tennessee") 
  

ggplot (data = tn_overdoses, aes(x=Year, y= Deaths)) + geom_col() +ggtitle('Opioid Overdose Deaths TN: 1999 - 2014')


overdoses_2014 <- overdoses_1999_2014 %>% 
  filter(Year == 2014) %>%
  arrange(desc(Deaths)) %>%
  slice(1:10)


ggplot(data=overdoses_2014, aes(x= State, y= Deaths)) +geom_col()


