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
library(magrittr)
library(reshape)
library(tidyr)
library(choroplethr)
library(maps)
library(mapdata)
library(ggmap)
library(fiftystater)


opioids<- read_csv('./opioids.csv')
overdoses<- read_csv('./overdoses.csv')
prescribers<- read_csv('./prescriber-info.csv')
not_states <- list('AA','GU','ZZ','AE')
#remove non-state entries

prescribers <- prescribers[!(prescribers$State %in% not_states), ]
head(opioids)
head(overdoses)
head(prescribers1)
oxycontin <- prescribers %>% 
  group_by(State,Gender) %>% 
  summarize(sum(OXYCONTIN))

class(opioids)


ggplot(data = oxycontin, 
       aes(reorder(State, `sum(OXYCONTIN)`), `sum(OXYCONTIN)`)) +
  geom_bar(stat = 'identity', aes(fill = Gender)) +
  xlab('State') +
  ylab('Total Number of Prescriptions') +
  ggtitle('        Oxycontin Prescriptions') +
  theme_bw()

overdoses <- overdoses %>% mutate(Deaths_Per_100000 = Deaths/Population * 100000) %>% 
  arrange(Deaths_Per_100000)

ggplot(overdoses, aes(x = reorder(Abbrev, Deaths_Per_100000), y = Deaths_Per_100000)) + geom_col() +
  xlab('State') + 
  ylab('Number of Deaths per 100,000 Residents') +
  ggtitle('        Prescription Drug Overdoses') +
  theme_bw()



# Maps
overdoses2 <- overdoses %>% 
mutate(overdoses, states = tolower(State))
  merge(,by.x="state_lower",by.y="region") %>%
  arrange(desc(DeathsPerPop)) 
ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE)



long <- gather(prescribers,'Drug', 'Value', ABILIFY:ZOLPIDEM.TARTRATE)
head(long)
#prescribers1 <- select(prescribers, contains('COD'),contains('HCL'))
#ld2 <-  melt(prescribers, id.vars = c("State", "Gender"),variable.name = "item")


