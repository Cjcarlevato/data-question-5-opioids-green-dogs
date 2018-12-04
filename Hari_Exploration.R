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
library(dplyr)
require(scales)
library(ggpubr)
library(directlabels)
library(qcc)
library(magrittr)


opioids<- read_csv('./opioids.csv')
overdoses<- read_csv('./overdoses.csv')
prescribers<- read_csv('./prescriber-info.csv')
opioid_prescribers_2013 <- read_csv('./opioid_prescribers_2013.csv')
opioid_prescribers_2014 <- read_csv('./opioid_prescribers_2014.csv')
opioid_prescribers_2015 <- read_csv('./opioid_prescribers_2015.csv')
opioid_prescribers_2016 <- read_csv('./opioid_prescribers_2016.csv')

overdoses_prescribers<- merge(overdoses, prescribers, by= 'State')
overdoses_prescribers.long <- gather(overdoses_prescribers,'Drug_Name', 'Value', ABILIFY:ZOLPIDEM.TARTRATE)

overdoses_deaths <- overdoses_prescribers.long %>%
  group_by(State, Deaths, Population) %>% 
  summarise(sum = sum(Deaths, na.rm = T)) %>% 
   mutate(Deaths_Per_100000 = Deaths/Population * 100000) %>% 
  arrange(Deaths_Per_100000)

ggplot(overdoses_deaths, aes(x = reorder(State, Deaths_Per_100000), y = Deaths_Per_100000)) +
  geom_col(width = .9, aes(fill=State),show.legend=FALSE) +
  xlab('State') + 
  ylab('Number of Deaths per 100,000 Residents') +
  ggtitle('        Overdose Death Rate') +
  theme_bw()



overdoses_deaths$hover <- with(overdoses_deaths, paste(State, '<br>', "Deaths:", Deaths, '<br>',"Population:", Population, '<br>', 'Deaths_per_100000:', Deaths_Per_100000))
l <- list(color = toRGB("white"), width = 2)
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)
p <- plot_geo(overdoses_deaths, locationmode = 'USA-states') %>%
  add_trace(
    z = ~Deaths_Per_100000, text = ~hover, locations = ~State,
    color = ~Population, colors = 'Purples'
  ) %>%
  colorbar(title = "    Deaths \nPer 100000") %>%
  layout(
    title = 'Deaths Per 100000',
    geo = g)

p


# Total Drug prescriptions with a value of 1 in opioid.prescriber columns
opioid_prescriptions <-  
  overdoses_prescribers.long %>%
  filter(Opioid.Prescriber == 1) %>% 
  group_by(State, Opioid.Prescriber) %>% 
  summarise(sum = sum(Opioid.Prescriber, na.rm = TRUE)) %>% 
  arrange(desc(sum))
opioid_prescriptions

ggplot(opioid_prescriptions, aes(x = reorder(State, sum), y = sum)) + 
  geom_col(width = .9, aes(fill=State),show.legend=FALSE) +
  xlab('State') + 
  ylab('opioid Prescriptions per State') +
  ggtitle('            Total Number of Opioid prescriptions > 10 times a year per State')


opioids$Drug_Name <-  gsub("-", ".",opioids$Drug_Name)
opioids$Drug_Name <-  gsub(" ", ".",opioids$Drug_Name)
opioids$Generic_Name <-  gsub("/", ".",opioids$Generic_Name)
opioids$Generic_Name <-  gsub(" ", ".",opioids$Generic_Name)

list.opioids <- dput(as.character(opioids$Drug_Name))
list.opioids2 <- dput(as.character(opioids$Generic_Name))
top_11_opioids <- overdoses_prescribers %>% 
            select(NPI,Gender,State,Credentials,Specialty, Population, Deaths,Opioid.Prescriber,one_of(list.opioids),one_of(list.opioids2))
top_11_opioids



# only opipoid(11)

top_11_opioids.long <- gather(top_11_opioids,'Drug_Name', 'Value', ACETAMINOPHEN.CODEINE:TRAMADOL.HCL)
top_11_opioids <-  
  top_11_opioids.long %>%
  group_by(State,Drug_Name, Population) %>% 
  summarise(sum = sum(Value, na.rm = TRUE)) %>% 
  mutate(opioids_per_100000 = sum/Population*100000) %>% 
  arrange(desc(sum))

opioids_only_drugs_OA <-top_11_opioids  %>% filter(Drug_Name == 'OXYCODONE.ACETAMINOPHEN') %>% group_by(State)
opioids_only_drugs_HA <-top_11_opioids  %>% filter(Drug_Name  == 'HYDROCODONE.ACETAMINOPHEN') %>% group_by(State)
opioids_only_drugs_T <-top_11_opioids  %>% filter(Drug_Name  == 'TRAMADOL.HCL') %>% group_by(State)
opioids_only_drugs_OHCL <-top_11_opioids  %>% filter(Drug_Name  == 'OXYCODONE.HCL') %>% group_by(State)
opioids_only_drugs_OA$hover <- with(opioids_only_drugs_OA, paste(State, '<br>', "opioids_per_100000:", opioids_per_100000))
opioids_only_drugs_HA$hover <- with(opioids_only_drugs_HA, paste(State, '<br>', "opioids_per_100000:", opioids_per_100000))
opioids_only_drugs_T$hover <- with(opioids_only_drugs_T, paste(State, '<br>', "opioids_per_100000:", opioids_per_100000))
opioids_only_drugs_OHCL$hover <- with(opioids_only_drugs_OHCL, paste(State, '<br>', "opioids_per_100000:", opioids_per_100000))


g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)
p1 <- plot_geo(opioids_only_drugs_HA, locationmode = 'USA-states') %>%
  add_trace(z = ~opioids_per_100000, text = ~hover, locations = ~State,
    color = ~opioids_per_100000, colors = 'Purples') %>%
  colorbar(title = "HYDROCODONE.\nACETAMINOPHEN", x = -.12, y = .75, xanchor = 'left', yanchor = 'middle') %>%
  layout(geo =g)
  
    
p1


p2 <- plot_geo(opioids_only_drugs_T,locationmode = 'USA-states') %>%
  add_trace(
    z = ~opioids_per_100000, text = ~hover, locations = ~State,
    color = ~opioids_per_100000, colors = 'Purples'
  ) %>%
  colorbar(title = "TRAMADOL.HCL", x = .95, y = .75, xanchor = 'left', yanchor = 'middle') %>%
  layout(annotations = list(x = 0.9 , y = 1.2, text = "HYD", showarrow = F),
    
    geo = g,title = 'Top Opioids prescribed in 2014 per 100000')

p2

p3 <- plot_geo(opioids_only_drugs_OHCL, locationmode = 'USA-states') %>%
  add_trace(
    z = ~opioids_per_100000, text = ~hover, locations = ~State,
    color = ~opioids_per_100000, colors = 'Purples'
  ) %>%
  colorbar(title = "OXYCODONE.HCL",x = -.12, y = .27, xanchor = 'left', yanchor = 'middle') %>%
  layout(geo = g)


p4 <- plot_geo(opioids_only_drugs_OA, locationmode = 'USA-states') %>%
  add_trace(
    z = ~opioids_per_100000, text = ~hover, locations = ~State,
    color = ~opioids_per_100000, colors = 'Purples'
  ) %>%
  colorbar(title = "OXYCODONE.\nACETAMINOPHEN",x = .95, y = .25, xanchor = 'left', yanchor = 'middle')  %>%
  layout(
    geo = g)

p4
p <- subplot(p1,p2,p3,p4, nrows = 2) 
p


#Mering population data with opioids_in_prescriber

#overdoses_prescribers<- merge(overdoses, prescribers, by= 'State')

# Merge Overdoses with 11 opioids in 2014 data and with precribers

#overdoses_merge_opioids <- merge(overdoses, opioids_11_prescribers)
#head(overdoses_merge_opioids)

#overdoses_merge_11_opioids.long <- gather(overdoses_merge_opioids,'Drug', 'Value', ACETAMINOPHEN.CODEINE:TRAMADOL.HCL)
#head(overdoses_merge_11_opioids.long)

overdoses_11_opioids_2014 <-  
  top_11_opioids.long %>%
  group_by(State,Drug_Name,Population, Deaths, Gender, Specialty) %>% 
  summarise(Total_prescriptions = sum(Value)) %>% 
  mutate(prescriptions_per_cap = Total_prescriptions/Population*1000000) %>% 
  mutate(Deaths_Per_100000 = Deaths/Population * 100000)


# overdoses_prescribers.long <- gather(overdoses_prescribers, 'Drug', 'Value', ABILIFY:ZOLPIDEM.TARTRATE)
# overdoses_prescribers.long


# write.csv(as.data.frame(overdoses_prescribers.long), file = '/Users/hari/NSSdatascience/R/overdoses_prescribers.long.csv')
# overdoses_prescribers.long <- read_csv('./overdoses_prescribers.long.csv')
overdoses_total_opioids_2014 <-  
  overdoses_prescribers.long %>%
  group_by(State,Drug_Name,Population, Deaths,Gender, Specialty) %>% 
  summarise(Total_prescriptions = sum(Value)) %>% 
  mutate(prescriptions_per_cap = Total_prescriptions/Population*1000000) %>% 
  mutate(Deaths_Per_100000 = Deaths/Population * 100000)


g1 <- ggscatter(overdoses_total_opioids_2014, x = "prescriptions_per_cap", y = "Deaths_Per_100000", 
                add = "reg.line", conf.int = TRUE, 
                color = "red", fill='red', size = 2,
                cor.coef = TRUE, cor.method = "pearson",
                add.params = list(color = "blue", fill = "lightgray"),
                xlab = "prescriptions_per_cap", ylab = "Deaths_Per_100000", title = 'Correlation between deaths_per_10000 vs total opiods')
g1

g2 <- ggscatter(overdoses_11_opioids_2014, x = "prescriptions_per_cap", y = "Deaths_Per_100000",
          add = "reg.line", conf.int = TRUE, 
          color = "red", fill='red', size = 2,
          cor.coef = TRUE, cor.method = "pearson",
          add.params = list(color = "blue", fill = "lightgray"),
          xlab = "prescriptions_per_cap", ylab = "Deaths_Per_100000", title = 'Correlation between deaths_per_10000 vs \ntotal number of 11 opiods prescriptions')

g2
ggarrange(g1, g2, ncol = 2, nrow = 2)


# Prescribers and Gender analysis

# gender <- overdoses_11_opioids_2014 %>% 
#   group_by(Drug,Gender,Specialty) %>% 
#   summarize(sum(prescriptions_per_cap)) %>% 
#   count(Specialty, Gender) 
# 
# 
# gender
# 
# gender2 <- overdoses_11_opioids_2014 %>% 
#   group_by(Drug, 'Specialty') %>% 
#   count(Specialty)
# gender2

# gender3 <- count(overdoses_11_opioids_2014, Specialty, Gender)
# gender3 <- gender3%>% arrange(desc(n)) %>%  filter(n > 100)
# gender3


gender <-  overdoses_prescribers.long %>% group_by(Gender,Population, Specialty) %>% 
  summarise(Total_prescriptions = sum(Value)) %>% 
  mutate(Prescriptions_per_100000 = Total_prescriptions/Population*100000) %>% 
  arrange(desc(Prescriptions_per_100000))
 

gender <- gender %>% group_by(Specialty,Gender) %>% 
  summarise(sum = sum(Prescriptions_per_100000,na.rm = T))
gender


g1 <- ggplot(data = gender, 
       aes(reorder(Specialty, sum), sum)) +
  geom_bar(stat = 'identity', aes(fill = Gender)) + coord_flip()+
  xlab('Specialty') +
  ylab('Number of Prescribers') +
  scale_y_continuous(labels = comma)+
  ggtitle('        Prescribers Specialty in State of TN') +

  theme_bw()
g1


# 2013 to 2016 data that includes additional opioids

opioid_prescribers_2013$Drug_Name <-  gsub("-", ".",opioid_prescribers_2013$Drug_Name)
opioid_prescribers_2013$Generic_Name <-  gsub("/", ".",opioid_prescribers_2013$Generic_Name)
opioid_prescribers_2014$Drug_Name <-  gsub("-", ".",opioid_prescribers_2014$Drug_Name)
opioid_prescribers_2014$Generic_Name <-  gsub("/", ".",opioid_prescribers_2014$Generic_Name)
opioid_prescribers_2015$Drug_Name <-  gsub("-", ".",opioid_prescribers_2015$Drug_Name)
opioid_prescribers_2015$Generic_Name <-  gsub("/", ".",opioid_prescribers_2015$Generic_Name)
opioid_prescribers_2016$Drug_Name <-  gsub("-", ".",opioid_prescribers_2016$Drug_Name)
opioid_prescribers_2016$Generic_Name <-  gsub("/", ".",opioid_prescribers_2016$Generic_Name)

overdoses_2013<- merge(opioid_prescribers_2013, overdoses, by = 'State')
overdoses_2014<- merge(opioid_prescribers_2014, overdoses, by = 'State')
overdoses_2015<- merge(opioid_prescribers_2015, overdoses, by = 'State')
overdoses_2016<- merge(opioid_prescribers_2016, overdoses, by = 'State')

opioids_2013 <- overdoses_2013 %>%
  group_by(Drug_Name,State,Population) %>%  summarize(Total_prescriptions = sum(Total_Claim_Count)) %>% arrange(desc(Total_prescriptions)) %>% 
  mutate(Prescriptions_per_100000 = Total_prescriptions/Population*100000) %>% group_by(Drug_Name) %>% summarise(Prescriptions_per_100000=sum(Prescriptions_per_100000)) %>% 
  arrange(desc(Prescriptions_per_100000)) %>% top_n(10)


opioids_2014 <- overdoses_2014 %>% 
  group_by(Drug_Name,State,Population) %>%  summarize(Total_prescriptions = sum(Total_Claim_Count)) %>% arrange(desc(Total_prescriptions)) %>% 
  mutate(Prescriptions_per_100000 = Total_prescriptions/Population*100000) %>% group_by(Drug_Name) %>% summarise(Prescriptions_per_100000=sum(Prescriptions_per_100000)) %>% 
  arrange(desc(Prescriptions_per_100000)) %>% top_n(10)

opioids_2015 <- overdoses_2015 %>% 
  group_by(Drug_Name,State,Population) %>%  summarize(Total_prescriptions = sum(Total_Claim_Count)) %>% arrange(desc(Total_prescriptions)) %>% 
  mutate(Prescriptions_per_100000 = Total_prescriptions/Population*100000) %>% group_by(Drug_Name) %>% summarise(Prescriptions_per_100000=sum(Prescriptions_per_100000)) %>% 
  arrange(desc(Prescriptions_per_100000)) %>% top_n(10)


opioids_2016 <- overdoses_2016 %>%
  group_by(Drug_Name,State,Population) %>%  summarize(Total_prescriptions = sum(Total_Claim_Count)) %>% arrange(desc(Total_prescriptions)) %>% 
  mutate(Prescriptions_per_100000 = Total_prescriptions/Population*100000) %>% group_by(Drug_Name) %>% summarise(Prescriptions_per_100000=sum(Prescriptions_per_100000)) %>% 
  arrange(desc(Prescriptions_per_100000)) %>% top_n(10)


opioids_2013_plot <- ggplot(data = opioids_2013, 
                       aes(reorder(Drug_Name, Prescriptions_per_100000), Prescriptions_per_100000)) +
  geom_bar(stat = 'identity', aes(fill = Prescriptions_per_100000),show.legend = FALSE) + 
  coord_flip()+
  xlab('Opioid') +
  ylab('Prescriptions per 100000') +
  ggtitle('Top Opioid prescriptions in 2013') +
  scale_y_continuous(labels = comma)+
  theme_bw()
opioids_2013_plot


opioids_2014_plot <- ggplot(data = opioids_2014, 
                       aes(reorder(Drug_Name, Prescriptions_per_100000), Prescriptions_per_100000)) +
  geom_bar(stat = 'identity', aes(fill = Prescriptions_per_100000),show.legend = FALSE) + coord_flip()+
  xlab('Opioid') +
  ylab('Prescriptions per 100000') +
  ggtitle('Top Opioid prescriptions in 2014') +
  scale_y_continuous(labels = comma)+
  theme_bw()
opioids_2014_plot

# 2015 complete data set

opioids_2015_plot <- ggplot(data = opioids_2015, 
                       aes(reorder(Drug_Name, Prescriptions_per_100000), Prescriptions_per_100000)) +
  geom_bar(stat = 'identity', aes(fill = Prescriptions_per_100000),show.legend = FALSE) + coord_flip()+
  #geom_text(aes(label=round(Prescriptions_per_100000)), vjust=1.5, size=3.5)+
  xlab('Opioid') +
  ylab('Prescriptions per 100000') +
  ggtitle('Top Opioid prescriptions in 2015') +
  scale_y_continuous(labels = comma)+
  theme_bw()
opioids_2015_plot

# 2016



opioids_2016_plot <- ggplot(data = opioids_2016, 
                       aes(reorder(Drug_Name, Prescriptions_per_100000), Prescriptions_per_100000)) +
  geom_bar(stat = 'identity', aes(fill = Prescriptions_per_100000),show.legend = FALSE) + coord_flip()+
  xlab('Opioid') +
  ylab('Prescriptions per 100000') +
  ggtitle('Top Opioid prescriptions in 2016') +
  scale_y_continuous(labels = comma)+
  theme_bw()
opioids_2016_plot

ggarrange(opioids_2013_plot, opioids_2014_plot, opioids_2015_plot, opioids_2016_plot, ncol = 2, nrow = 2)


# # Pareto Exploration
# # using qcc library 


pareto_prescriptions <-  
  overdoses_prescribers.long %>%
  filter(State=='TN',Opioid.Prescriber == 1) %>% 
  group_by(Specialty) %>% 
  summarise(Total_prescriptions = sum(Value, na.rm = TRUE)) %>% 
    arrange(desc(Total_prescriptions))
pareto_prescriptions

# 
# pareto_prescriptions <- pareto_prescriptions %>% select(Specialty,Prescriptions_per_100000) %>%  arrange(desc(Prescriptions_per_100000))
# pareto_prescriptions

Pareto_20_80 = pareto_prescriptions$Total_prescriptions
names(Pareto_20_80)= pareto_prescriptions$Specialty
par(mar=c(10, 3, 3, 1))
pareto.chart(Pareto_20_80,cumperc = seq(0,100,by =5),col=rainbow(length(pareto_prescriptions$Specialty)),cex.names=.90, main="Pareto Effect in TN",ylab = 'Count')
options(scipen=999)

# Pareto using ggplot2


# using ggplot2
# overdoses_2016
# pareto_prescriptions_0 <-
#   overdoses_2016 %>%
#   group_by(npi) %>%
#   summarise(Total_prescriptions = sum(Total_Claim_Count, na.rm = TRUE)) %>%
#   arrange(desc(Total_prescriptions)) %>%
#   mutate(relative_freq = Total_prescriptions/sum(Total_prescriptions),cumulative_freq = cumsum(relative_freq))
# pareto_prescriptions_0
# 
# # 
# the_order <- pareto_prescriptions_0$npi
# p0 <-   ggplot(data=pareto_prescriptions_0,(aes(x = npi, weight = relative_freq))) +
#   geom_bar(width = 0.5, aes(fill=npi),show.legend=FALSE) +
#   scale_x_discrete(limits = the_order) +
#   scale_y_continuous(label = scales::percent) +
#   geom_point(aes(x = npi, y = cumulative_freq), color = 'red') +
#   geom_line(aes(x = npi, y = cumulative_freq, group = 1), color = 'red') +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))+
#   geom_vline(aes(xintercept = 8, linetype = "dotted"), colour= 'blue', data = pareto_prescriptions_0, show.legend = FALSE) +
#   geom_hline(aes(yintercept = .8, linetype = "1F"), colour= 'blue', data = pareto_prescriptions_0,show.legend = FALSE) +
#   labs(x = "", y = "Relative frequency",
#        title = "A Pareto diagram for 20% of Prescribers \nprescribring 80% opioids in 2016") +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# p0



pareto_prescriptions_1 <-  
  overdoses_prescribers.long %>%
  filter(Opioid.Prescriber == 1) %>% 
  group_by(Specialty) %>% 
  summarise(Total_prescriptions = sum(Value, na.rm = TRUE)) %>% 
  arrange(desc(Total_prescriptions)) %>% 
  mutate(relative_freq = Total_prescriptions / sum(Total_prescriptions),cumulative_freq = cumsum(relative_freq))
pareto_prescriptions_1 
the_order <- pareto_prescriptions_1$Specialty
p <-   ggplot(data=pareto_prescriptions_1,(aes(x = Specialty, weight = relative_freq))) +
  geom_bar(width = 0.5, aes(fill=Specialty),show.legend=FALSE) +
  scale_x_discrete(limits = the_order) +
  scale_y_continuous(label = scales::percent) +
  geom_point(aes(x = Specialty, y = cumulative_freq), color = 'red') +
  geom_line(aes(x = Specialty, y = cumulative_freq, group = 1), color = 'red') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))+
  geom_vline(aes(xintercept = 3.5, linetype = "dotted"), colour= 'blue', data = pareto_prescriptions_1, show.legend = FALSE) +                                                                                                     
  geom_hline(aes(yintercept = .8, linetype = "1F"), colour= 'blue', data = pareto_prescriptions_1,show.legend = FALSE) + 
  labs(x = "", y = "Relative frequency", 
       title = "A Pareto diagram for Prescribers by Specialty \nprescribring 80% opioids in 2014") +
  theme(plot.title = element_text(hjust = 0.5))
pareto_prescriptions_1 
p

pareto_prescriptions_1 %>% distinct()

pareto_prescriptions_2 <-  
  overdoses_prescribers.long %>%
  filter(State=='TN',Opioid.Prescriber == 1) %>% 
  group_by(Specialty) %>% 
  summarise(Total_prescriptions = sum(Value, na.rm = TRUE)) %>% 
  arrange(desc(Total_prescriptions)) %>% 
  mutate(relative_freq = Total_prescriptions / sum(Total_prescriptions),cumulative_freq = cumsum(relative_freq))
pareto_prescriptions_2 
the_order <- pareto_prescriptions_2$Specialty
p1 <-   ggplot(data=pareto_prescriptions_2,(aes(x = Specialty, weight = relative_freq))) +
  geom_bar(width = 0.5, aes(fill=Specialty),show.legend=FALSE) +
  scale_x_discrete(limits = the_order) +
  scale_y_continuous(label = scales::percent) +
  geom_point(aes(x = Specialty, y = cumulative_freq), color = 'red') +
  geom_line(aes(x = Specialty, y = cumulative_freq, group = 1), color = 'red') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))+
  geom_vline(aes(xintercept = 3, linetype = "dotted"), colour= 'blue', data = pareto_prescriptions_2, show.legend = FALSE) +                                                                                                     
  geom_hline(aes(yintercept = .8, linetype = "1F"), colour= 'blue', data = pareto_prescriptions_2,show.legend = FALSE) + 
  labs(x = "", y = "Relative frequency", 
       title = "A Pareto diagram for Prescribers by  Specialty\nprescribring 80%  all drugs in TN 2014") +
  theme(plot.title = element_text(hjust = 0.5))


p1


pareto_prescriptions_3 <-  
  opioids_11_prescribers.long %>%
  filter(State=='TN',Opioid.Prescriber == 1) %>% 
  group_by(Specialty) %>% 
  summarise(Total_prescriptions = sum(Value, na.rm = TRUE)) %>% 
  arrange(desc(Total_prescriptions)) %>% 
  mutate(relative_freq = Total_prescriptions / sum(Total_prescriptions),cumulative_freq = cumsum(relative_freq))
pareto_prescriptions_3 
the_order <- pareto_prescriptions_3$Specialty
p2 <-   ggplot(data=pareto_prescriptions_3,(aes(x = Specialty, weight = relative_freq))) +
  geom_bar(width = 0.5, aes(fill=Specialty),show.legend=FALSE) +
  scale_x_discrete(limits = the_order) +
  scale_y_continuous(label = scales::percent) +
  geom_point(aes(x = Specialty, y = cumulative_freq), color = 'red') +
  geom_line(aes(x = Specialty, y = cumulative_freq, group = 1), color = 'red') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))+
  geom_vline(aes(xintercept = 6, linetype = "dotted"), colour= 'blue', data = pareto_prescriptions_3, show.legend = FALSE) +                                                                                                     
  geom_hline(aes(yintercept = .8, linetype = "1F"), colour= 'blue', data = pareto_prescriptions_3,show.legend = FALSE) + 
  labs(x = "", y = "Relative frequency", 
       title = "A Pareto diagram for Prescribers by specialty \nprescribring  80% of 11 opioids in TN") +
  theme(plot.title = element_text(hjust = 0.5))
pareto_prescriptions_3

p2


pareto_prescriptions_4 <-  
  overdoses_2016 %>%
  filter(State=='TN') %>% 
  group_by(Specialty) %>% 
  summarise(Total_prescriptions = sum(Total_Claim_Count, na.rm = TRUE)) %>% 
  arrange(desc(Total_prescriptions)) %>% 
  mutate(relative_freq = Total_prescriptions / sum(Total_prescriptions),cumulative_freq = cumsum(relative_freq))

the_order <- pareto_prescriptions_4$Specialty
pareto_prescriptions_4

p3 <-   ggplot(data=pareto_prescriptions_4,(aes(x = reorder(Specialty, relative_freq), weight = relative_freq))) +
  geom_bar(width = 0.5, aes(fill=Specialty),show.legend=FALSE) +
  scale_x_discrete(limits = the_order) +
  scale_y_continuous(label = scales::percent) +
  geom_point(aes(x = reorder(Specialty, cumulative_freq),y = cumulative_freq), color = 'red') +
  geom_line(aes(x = reorder(Specialty, cumulative_freq), y = cumulative_freq, group = 1), color = 'red') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))+
  geom_vline(aes(xintercept = 6, linetype = "dotted"), colour= 'blue', data = pareto_prescriptions_4, show.legend = FALSE) +                                                                                                     
  geom_hline(aes(yintercept = .8, linetype = "1F"), colour= 'blue', data = pareto_prescriptions_4,show.legend = FALSE) + 
  labs(x = "", y = "Relative frequency", 
       title = "A Pareto diagram for Prescribers  by Specialty \nprescribring  80% of 11 opioids in TN 2016") +
  theme(plot.title = element_text(hjust = 0.5))


p3


  # geom_path(aes(y=cumulative_sum, group=1)) 

  
  #labs(title = "Pareto Plot", subtitle = "XXXX", x = 'Prescriber', y ='Prescriptions_per_100000')

pareto_prescriptions2 <-  
  overdoses_2016 %>%
  filter(State =='TN') %>% 
  group_by(Specialty, Population) %>% 
  summarise(Total_prescriptions = sum(Total_Claim_Count, na.rm = TRUE)) %>% 
  mutate(Prescriptions_per_100000 = Total_prescriptions/Population*100000) %>% 
  arrange(desc(Total_prescriptions))

pareto_prescriptions2

pareto_prescriptions2 <- pareto_prescriptions2 %>% select(Specialty,Prescriptions_per_100000) %>%  arrange(Prescriptions_per_100000)
pareto_prescriptions2

Pareto_number_data = pareto_prescriptions2$Prescriptions_per_100000
names(Pareto_number_data)= pareto_prescriptions2$Specialty
pareto.chart(Pareto_number_data, cumperc = seq(0,100,by =5),col=rainbow(length(pareto_prescriptions2))) 
