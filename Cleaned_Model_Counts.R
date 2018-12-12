library(tidyverse)
library(magrittr)
library(scales)

op_list = c('CODEINE', 'BUPRENORPHINE', 'BUTORPHANOL',
            'FENTANYL', 'HYDROCODONE', 'HYDROMORPHONE', 'OXYCODONE', 'LEVORPHANOL',
            'MEPERIDINE', 'METHADONE', 'MORPHINE', 'NALBUPHINE', 'OPIUM', 'PENTAZOCINE',
            'TAPENTADOL', 'TRAMADOL')

state_abbrev <- read_csv('data/overdoses.csv') %>% select(State, Abbrev)

population <- read_csv('data/State/Demographics/Population.csv')
pct_white <- read_csv('data/State/Demographics/Pct_White.csv') %>% select(Year, State, Pct_White)

unemployment <- read_csv('data/unemployment.csv') %>% 
  group_by(year, State) %>% 
  summarise(Unemployment = mean(value)) %>% 
  ungroup() %>% rename(Year = year)

Rural <- read_csv('data/State/Rural/Rural.csv') %>% 
  select(State, Pct_Rural)

Election <- read_csv('data/State/Election.csv')

Income <- read_csv('data/State/Incomes.csv')

od_2016 <- read_csv('data/KFF/raw_data_2016.csv') %>% 
  select(State, Opioid_Death_Rate = "Opioid Overdose Death Rate (Age-Adjusted)") 

opioid_prescribers_2016 <- read_csv('data/opioid_prescribers_2016.csv')

drugs_by_state_2016 <- opioid_prescribers_2016 %>% group_by(State, Generic_Name) %>% 
  summarize(Total_Prescriptions = sum(Total_Claim_Count)) %>% ungroup() %>% 
  rename(Abbrev = State) %>% 
  inner_join(state_abbrev) %>% inner_join(population %>% filter(Year == 2016)) %>%  
  mutate(Prescription_Rate = Total_Prescriptions/ totpop * 100000)

drugs_by_state_wide_2016 <- drugs_by_state_2016 %>% 
  select(-Total_Prescriptions) %>% spread(Generic_Name, Prescription_Rate) %>% 
  inner_join(od_2016) %>% mutate_all(funs(replace(.,is.na(.),0)))

drugs_by_class_2016 <- op_list %>% 
  map(~ drugs_by_state_wide_2016 %>% 
        select(matches(.x)) %>%
        reduce(`+`)) %>%
  set_names(op_list) %>%       
  bind_cols(drugs_by_state_wide_2016 %>% select(Year, Abbrev, State, Opioid_Death_Rate), .)

drugs_by_class_2016 <- drugs_by_class_2016 %>% 
  inner_join(unemployment %>% select(Year, State, Unemployment))

drugs_by_class_2016 <- drugs_by_class_2016 %>% 
  inner_join(Income) %>% 
  inner_join(pct_white)

state_2016 <- read_csv('data/Prescribing_Rate/State_2016.csv')
vet <- read_csv('data/State/Vet/Vet_Pct_2016.csv')
lfr <- read_csv('data/State/Labor_Force/LFP_2016.csv')

drugs_by_class_2016 <- drugs_by_class_2016 %>% inner_join(state_2016) %>% 
  inner_join(Rural) %>% inner_join(vet) %>% inner_join(lfr) %>%  inner_join(Election)



od_2015 <- read_csv('data/KFF/raw_data_2015.csv') %>% 
  select(State, Opioid_Death_Rate = "Opioid Overdose Death Rate (Age-Adjusted)") 

opioid_prescribers_2015 <- read_csv('data/opioid_prescribers_2015.csv')

drugs_by_state_2015 <- opioid_prescribers_2015 %>% group_by(State, Generic_Name) %>% 
  summarize(Total_Prescriptions = sum(Total_Claim_Count)) %>% ungroup() %>% 
  rename(Abbrev = State) %>% 
  inner_join(state_abbrev) %>% inner_join(population %>% filter(Year == 2015)) %>%  
  mutate(Prescription_Rate = Total_Prescriptions/ totpop * 100000)

drugs_by_state_wide_2015 <- drugs_by_state_2015 %>% 
  select(-Total_Prescriptions) %>% spread(Generic_Name, Prescription_Rate) %>% 
  inner_join(od_2015) %>% mutate_all(funs(replace(.,is.na(.),0)))

drugs_by_class_2015 <- op_list %>% 
  map(~ drugs_by_state_wide_2015 %>% 
        select(matches(.x)) %>%
        reduce(`+`)) %>%
  set_names(op_list) %>%       
  bind_cols(drugs_by_state_wide_2015 %>% select(Year, Abbrev, State, Opioid_Death_Rate), .)

drugs_by_class_2015 <- drugs_by_class_2015 %>% 
  inner_join(unemployment %>% select(Year, State, Unemployment))

drugs_by_class_2015 <- drugs_by_class_2015 %>% 
  inner_join(Income) %>% 
  inner_join(pct_white)

state_2015 <- read_csv('data/Prescribing_Rate/State_2015.csv')
vet <- read_csv('data/State/Vet/Vet_Pct_2015.csv')
lfr <- read_csv('data/State/Labor_Force/LFP_2015.csv')

drugs_by_class_2015 <- drugs_by_class_2015 %>% inner_join(state_2015) %>% 
  inner_join(Rural) %>% inner_join(vet) %>% inner_join(lfr) %>% inner_join(Election)



od_2014 <- read_csv('data/KFF/raw_data_2014.csv') %>% 
  select(State, Opioid_Death_Rate = "Opioid Overdose Death Rate (Age-Adjusted)") 

opioid_prescribers_2014 <- read_csv('data/opioid_prescribers_2014.csv')

drugs_by_state_2014 <- opioid_prescribers_2014 %>% group_by(State, Generic_Name) %>% 
  summarize(Total_Prescriptions = sum(Total_Claim_Count)) %>% ungroup() %>% 
  rename(Abbrev = State) %>% 
  inner_join(state_abbrev) %>% inner_join(population %>% filter(Year == 2014)) %>%  
  mutate(Prescription_Rate = Total_Prescriptions/ totpop * 100000)

drugs_by_state_wide_2014 <- drugs_by_state_2014 %>% 
  select(-Total_Prescriptions) %>% spread(Generic_Name, Prescription_Rate) %>% 
  inner_join(od_2014) %>% mutate_all(funs(replace(.,is.na(.),0)))

drugs_by_class_2014 <- op_list %>% 
  map(~ drugs_by_state_wide_2014 %>% 
        select(matches(.x)) %>%
        reduce(`+`)) %>%
  set_names(op_list) %>%       
  bind_cols(drugs_by_state_wide_2014 %>% select(Year, Abbrev, State, Opioid_Death_Rate), .)

drugs_by_class_2014 <- drugs_by_class_2014 %>% 
  inner_join(unemployment %>% select(Year, State, Unemployment))

drugs_by_class_2014 <- drugs_by_class_2014 %>% 
  inner_join(Income) %>% 
  inner_join(pct_white)

state_2014 <- read_csv('data/Prescribing_Rate/State_2014.csv')
vet <- read_csv('data/State/Vet/Vet_Pct_2014.csv')
lfr <- read_csv('data/State/Labor_Force/LFP_2014.csv')

drugs_by_class_2014 <- drugs_by_class_2014 %>% inner_join(state_2014) %>% 
  inner_join(Rural) %>% inner_join(vet) %>% inner_join(lfr)%>% inner_join(Election)


od_2013 <- read_csv('data/KFF/raw_data_2013.csv') %>% 
  select(State, Opioid_Death_Rate = "Opioid Overdose Death Rate (Age-Adjusted)") 

opioid_prescribers_2013 <- read_csv('data/opioid_prescribers_2013.csv')

drugs_by_state_2013 <- opioid_prescribers_2013 %>% group_by(State, Generic_Name) %>% 
  summarize(Total_Prescriptions = sum(Total_Claim_Count)) %>% ungroup() %>% 
  rename(Abbrev = State) %>% 
  inner_join(state_abbrev) %>% inner_join(population %>% filter(Year == 2013)) %>%  
  mutate(Prescription_Rate = Total_Prescriptions/ totpop * 100000)

drugs_by_state_wide_2013 <- drugs_by_state_2013 %>% 
  select(-Total_Prescriptions) %>% spread(Generic_Name, Prescription_Rate) %>% 
  inner_join(od_2013) %>% mutate_all(funs(replace(.,is.na(.),0)))

drugs_by_class_2013 <- op_list %>% 
  map(~ drugs_by_state_wide_2013 %>% 
        select(matches(.x)) %>%
        reduce(`+`)) %>%
  set_names(op_list) %>%       
  bind_cols(drugs_by_state_wide_2013 %>% select(Year, Abbrev, State, Opioid_Death_Rate), .)

drugs_by_class_2013 <- drugs_by_class_2013 %>% 
  inner_join(unemployment %>% select(Year, State, Unemployment))

drugs_by_class_2013 <- drugs_by_class_2013 %>% 
  inner_join(Income) %>% 
  inner_join(pct_white)

state_2013 <- read_csv('data/Prescribing_Rate/State_2013.csv')
vet <- read_csv('data/State/Vet/Vet_Pct_2013.csv')
lfr <- read_csv('data/State/Labor_Force/LFP_2013.csv')

drugs_by_class_2013 <- drugs_by_class_2013 %>% inner_join(state_2013) %>% 
  inner_join(Rural) %>% inner_join(vet) %>% inner_join(lfr)%>% inner_join(Election)


drugs_by_class <- rbind(drugs_by_class_2016, drugs_by_class_2015, drugs_by_class_2014, drugs_by_class_2013)
drugs_by_class <- drugs_by_class %>% mutate(Opioid_Death_Rate = as.numeric(Opioid_Death_Rate))

drugs_by_class <- drugs_by_class %>% drop_na

correlations <- cor(drugs_by_class %>% select(-c(State, Abbrev)))

library(rsample)
library(randomForest)
library(miscTools)

opioid_splits <- initial_split(drugs_by_class %>% select(-c(Abbrev, State, Year)), prop = 0.80)
train <- training(opioid_splits)
test <- testing(opioid_splits)

lm <- lm(Opioid_Death_Rate~., data = train)
summary(lm)

rfMod <- randomForest(Opioid_Death_Rate~., data=train, mtry = 10)
print(rfMod)

plot(rfMod)

varImpPlot(rfMod, sort=TRUE, n.var=10, main = "variable importance", color = 'darkviolet')

test$predictions <- predict(rfMod, test)

r2 <- rSquared(test$Opioid_Death_Rate, test$Opioid_Death_Rate - test$predictions)
print(r2)

mae <- mean(abs(test$Opioid_Death_Rate - test$predictions))
print(mae)

mse <- mean((test$Opioid_Death_Rate - test$predictions)^2)
print(mse)

ggplot(aes(Opioid_Death_Rate, predictions), data = test) +
  geom_point() +
  geom_smooth() +
  geom_line(aes(Opioid_Death_Rate, Opioid_Death_Rate), color = 'red')
