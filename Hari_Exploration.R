# ### Questions:
# 1. Explore the dataset. Consider plotting the number of types of opioids by state and number of 
#opioid prescribers by state. Understand the *most prevalent* types of opioids prescribed. Explore
#the states with the highest prevalence of fatal opioid overdoses. How does this compare to total
#overdoses?
# Is there a correlation between type of opioids prescribed and overdose deaths? What patterns emerge? 
#   Use different types of plots to visualize your data (histogram, bee swarm, box plot, etc...).  

# 2. Test the assumption of the Pareto effect on opioid prescribing using the dataset
#(i.e., 20% of prescribers provide 80% of the opioids).  
# 
# 3. Build a model to predict the number of fatal opioid overdoses. Consider building the
# model for a specific state, but that’s not necessary. Be creative—consider crossing the raw 
# features that are available and creating new features (i.e., total type of opioids). 
# Socioeconomics are often a driving factor for health outcomes. Can you find another dataset
# that provides information at the state level and can be used in your model? Can you perform tests 
# to explain the variability in your outcome (i.e, effect estimation)?
#   
#   #### Modeling dataset:
#   
#   https://www.kaggle.com/apryor6/detecting-frequent-opioid-prescription

library(tidyverse)
library(plotly)
library(tidyr)
library(ggmap)



opioids<- read_csv('./opioids.csv')
overdoses<- read_csv('./overdoses.csv')
prescribers<- read_csv('./prescriber-info.csv')
opioid_prescribers_2013 <- read_csv('./opioid_prescribers_2013.csv')
opioid_prescribers_2014 <- read_csv('./opioid_prescribers_2014.csv')
opioid_prescribers_2015 <- read_csv('./opioid_prescribers_2015.csv')
opioid_prescribers_2016 <- read_csv('./opioid_prescribers_2016.csv')


# renamed columns in opioids 
opioids <- opioids %>% 
  select('Drug Name', 'Generic Name') %>% 
  dplyr::rename(Drug_Name = "Drug Name", Generic_Name = "Generic Name")

overdoses <- overdoses %>% mutate(Deaths_Per_100000 = Deaths/Population * 100000) %>% 
  arrange(Deaths_Per_100000)

ggplot(overdoses, aes(x = reorder(Abbrev, Deaths_Per_100000), y = Deaths_Per_100000)) + geom_col() +
  xlab('State') + 
  ylab('Number of Deaths per 100,000 Residents') +
  ggtitle('        Prescription Drug Overdoses') +
  theme_bw()



# converting prescribers to long format


#Didnot work
# keys <- c('Drug')
# tdata <-  data.table(opioids, key = keys)
# tbounce <- data.table(prescribers.long, key = keys)
# tbounce[tdata, Bounced :=1L]
prescribers.long <- gather(prescribers,'Drug', 'Value', ABILIFY:ZOLPIDEM.TARTRATE)
head(prescribers.long)
prescribers.long <- prescribers.long %>%
dplyr::rename(Drug_Name = 'Drug')
head(prescribers.long)

total_drugs <-  
  prescribers.long %>%
  select(State, Opioid.Prescriber) %>%
  filter(Opioid.Prescriber == 1) %>% 
  group_by(State, Opioid.Prescriber) %>% 
  summarise(sum = sum(Opioid.Prescriber, na.rm = TRUE)) %>% 
  arrange(desc(sum))
total_drugs

ggplot(total_drugs, aes(x = reorder(State, sum), y = sum)) + geom_col() +
  xlab('State') + 
  ylab('Drugs Prescriptions per State') +
  ggtitle('            Total Number of Drugs Prescriptions per State')

# prescribersjoin <- prescribers.long %>% 
#   left_join(opioids, by= 'Drug') %>% 
#   filter(Drug) %>% 
#   slice(n()) %>% 
#   ungroup
str(prescribers.long)
str(opioids)
#prescribers1 <- intersect(prescribers.long, opioids$Drug == CODEINE)
# test <-  intersect(prescribers.long$Drug, opioids$Drug)
# test

## merge dataframes opioids with prescribers
opioids$Drug_Name <-  gsub("-", ".",opioids$Drug_Name)
opioids$Drug_Name <-  gsub(" ", ".",opioids$Drug_Name)
opioids$Generic_Name <-  gsub("/", ".",opioids$Generic_Name)
opioids$Generic_Name <-  gsub(" ", ".",opioids$Generic_Name)

list.opioids <- dput(as.character(opioids$Drug_Name))
list.opioids2 <- dput(as.character(opioids$Generic_Name))
opioids_in_prescribers <- prescribers %>% select(NPI,Gender,State,Credentials,Specialty, Opioid.Prescriber,one_of(list.opioids),one_of(list.opioids2))
colnames(opioids_in_prescribers)

opioids_in_prescribers.long <- gather(opioids_in_prescribers,'Drug', 'Value', ACETAMINOPHEN.CODEINE:TRAMADOL.HCL)
head(opioids_in_prescribers.long)







