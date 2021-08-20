library(tidyverse)
library(ggplot2)
library(here)
library(lubridate)
library(parallel)
library(patchwork)
library(extrafont)
library(skimr)
library(beepr)
library(ggbump)

here()
usethis::edit_r_environ()
set.seed(1)

options(scipen=999)
source("SlicedTheme.R")
modelData<-as_tibble(read.csv("Season 2/sliced-s01e10/train.csv",header=T,stringsAsFactors=F,sep=","))


age_inDays<-function(age){
  
  if(grepl("month",age)){
    
    as.numeric(stringr::str_match(age,"\\d"))*30
    
  } else if(grepl("year",age)){
    as.numeric(stringr::str_match(age,"\\d"))*365
  }  else if(grepl("day",age)){
    as.numeric(stringr::str_match(age,"\\d"))
  }  else if(grepl("week",age)){
    as.numeric(stringr::str_match(age,"\\d"))*7
  } else {
    
    NA
  } 
  
  
  
}

age_inDaysDF<-function(v){
  unlist(lapply(v,age_inDays))
}

modelData$age_upon_outcome<-age_inDaysDF(modelData$age_upon_outcome)

modelData$moring<-am(ymd_hms(modelData$datetime))
modelData$dayofWeek<-wday(ymd_hms(modelData$datetime))
modelData$month<-month(ymd_hms(modelData$datetime))
modelData$year<-year(ymd_hms(modelData$datetime))

modelData<-modelData%>%
  mutate(breed = tolower(breed),
         animal_type = tolower(animal_type)
  )

modelData$astrix<-grepl("\\*",modelData$name)
modelData$noName<-is.na(modelData$name)
modelData$mix<-grepl("mix",modelData$breed)

animalExtractor<-function(breed){
  trimws(str_remove(str_remove(breed,"mix"),"\\s"))
}
animalExtractordf<-function(v){
  unlist(lapply(v, animalExtractor))
}

modelData$animal_type2<-modelData$animal_type
modelData$animal_type2[!modelData$animal_type2%in%c("dog","cat")]<-
  animalExtractordf(modelData$breed[!modelData$animal_type2%in%c("dog","cat")])

modelData$tabby<-grepl("tabby",modelData$breed)

breedz<-na.omit(unique((modelData%>%separate_rows(breed,sep="/"))$breed))

modelData[,breedz]<-lapply(breedz,function(brs){
  
  as.numeric(grepl(brs,modelData$breed))
  
})%>%do.call(cbind,.)

library(reshape)
## outcome by day of week
modelData%>%
  group_by(dayofWeek)%>%
  summarise(
    adoptionRate = mean(outcome_type == "adoption"),
    transferRate = mean(outcome_type == "transfer"),
    deathRate = mean(outcome_type == "no outcome")
    # n = n()
  )%>% pivot_longer(!dayofWeek, names_to = "type", values_to = "rate")%>%
  ggplot(aes(x = dayofWeek, y = rate, color = type))+
  geom_line()+
  geom_point()+
  scale_color_sliced()+
  scale_x_continuous(breaks = 1:7,labels = c("Sun","Mon", "Tues", "Wed", "Thurs", "Fri", "Sat"))+
  labs(
    title = "Waiting for the Weekend",
    subtitle = "Rate of Outcomes for Pets at Shelters by Day Of Week",
    x = "Day",
    y = "Rate of Outcome",
    caption = "Hump day, more like bumped-off day..."
  )

## animal type breed by time series
modelData%>%
  mutate(datetime=week(ymd_hms(datetime)))%>%
  group_by(datetime)%>%
  summarise(
    dogRate = mean(animal_type == "dog"),
    catRate = mean(animal_type == "cat")
  )%>% pivot_longer(!datetime, names_to = "type", values_to = "rate")%>%
  ggplot(aes(x = datetime, y = rate, color = type))+
  geom_line()+
  geom_point()+
  scale_color_sliced()+
  labs(
    title = "Dog Days of Winter",
    subtitle = "Week of Year for adoptions of dogs and cats",
    x = "week",
    y = "Rate of Animal Type",
    caption = "I would have thought more dogs in the summer, interesting."
  )

## age of outcome, old dogs vs old cats
modelData%>%
  mutate(age_upon_outcome=(round((age_upon_outcome)/7,0)))%>%
  group_by(age_upon_outcome)%>%
  summarise(
    adoptionRate = mean(outcome_type == "adoption"),
    transferRate = mean(outcome_type == "transfer"),
    deathRate = mean(outcome_type == "no outcome")
    # n = n()
  )%>% pivot_longer(!age_upon_outcome, names_to = "type", values_to = "rate")%>%
  ggplot(aes(x = age_upon_outcome, y = rate, color = type))+
  geom_line()+
  geom_point()+
  scale_color_sliced()+
  labs(
    title = "Waiting for the Weekend",
    subtitle = "Rate of Outcomes for Pets at Shelters by Day Of Week",
    x = "DWeeks old",
    y = "Rate of Outcome",
    caption = "Hump day, more like bumped-off day..."
  )


## can we figure out linespans?
modelData%>%
  mutate(animal_type2 = fct_lump(as.factor(animal_type2), n = 10))%>%
  group_by(animal_type2)%>%
  summarise(age = max(age_upon_outcome, na.rm = T)/365,
            adoptionRate = mean(outcome_type == "adoption", na.rm = T))%>%
  mutate(animal_type2 = fct_reorder(animal_type2, age))%>%
  ggplot(aes(x = animal_type2, y = age, fill = adoptionRate))+
  geom_bar(stat = "identity")+
  scale_fill_sliced("Divergent", discrete = F)+
labs(
  title = "Eight The Hard Way",
  subtitle = "Max Ages of Animals in Shelters",
  x = "Animal",
  y = "Max Age"
)
  
## common names
modelData%>%
  mutate(name = fct_lump(as.factor(name), n = 12))%>%
  group_by(name, animal_type)%>%
  summarise(
    n = n(),
    adoptionRate = mean(outcome_type == "adoption", na.rm = T))%>%
  mutate(name = fct_reorder(name, n))%>%
  filter(name != "Other")%>%
  ggplot(aes(x = name, y = n, fill = adoptionRate))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_sliced("Divergent", discrete = F)+
  labs(
    title = "Name it yourself!",
    subtitle = "Common names at Shelters",
    x = "name",
    y = "Number In System",
    caption = "Bella, how tragic.."
  )+
  facet_wrap(animal_type~.)

## color on animal
modelData%>%
  mutate(color = fct_lump(as.factor(color), n = 12))%>%
  group_by(color)%>%
  summarise(
    n = n(),
    adoptionRate = mean(outcome_type == "adoption", na.rm = T))%>%
  mutate(color = fct_reorder(color, n))%>%
  filter(color != "Other")%>%
  ggplot(aes(x = color, y = n, fill = adoptionRate))+
  geom_bar(stat = "identity")+
  scale_fill_sliced("Divergent", discrete = F)+
  labs(
    title = "Pet The Rainbow!",
    subtitle = "Common Colors at Shelters",
    x = "Color",
    y = "Number In System",
    caption = "sadly no one wants brown animals"
  )

# wordcloud
library(wordcloud2) 

# have a look to the example dataset
demoFreq
# Basic plot
modelData%>%
  mutate(animal_type2 = fct_lump(animal_type2,300))%>%
  group_by(animal_type2)%>%
  summarise(freq = round((n())^.25,0))%>%
  rename(word = animal_type2)%>%
wordcloud2(data=., size=1,minSize =0, color='random-light', backgroundColor="black", shape = 'star')
wcName<-modelData%>%
  mutate(
    name = str_remove(name, "[[:punct:]]"),
    name = fct_lump(name,5300))%>%
  group_by(name)%>%
  summarise(freq = round((n()),0))%>%
  rename(word = name)%>%
  filter(!word  %in% c("Other",NA))%>%
  wordcloud2(data=., size=1,minSize =0, color='random-light', backgroundColor="black", shape = 'circle')

htmlwidgets::saveWidget(wcName,"wcName.html",selfcontained = F)
webshot::webshot("wcName.html","wcName.png",vwidth = 1600, vheight = 1600, delay =60)
# gif
library(gganimate)

myanim<-modelData%>%
  mutate(datetime=as.integer(round(month(ymd_hms(datetime)),0)))%>%
  filter( animal_type%in%c("dog","cat"))%>%
  group_by(datetime,animal_type,outcome_type)%>%
  summarise(
   n = n()
  )%>% 
  ungroup()%>%
  pivot_wider(id_cols = c(datetime,outcome_type),names_from = c(animal_type),values_from = c(n) )%>%
  janitor::clean_names()%>%
  ggplot(aes(x = dog, y = cat, color = outcome_type, label = outcome_type))+
  geom_line()+
  geom_text(size = 10)+
  scale_color_sliced()+
  theme(legend.position = "none") +
labs(
    title = "Dog Days of Winter",
    subtitle = "{month.name[frame_time]} Totals for Outcomes of Dogs and Cats",
    x = "# of Dogs",
    y = "# of Cats",
    caption = "I would have thought more dogs in the summer would be adopted, not cats..Who wants a cat in the summer?"
  )+
  transition_time(datetime) +
  ease_aes()+
  enter_fade() + 
  exit_shrink()+
  shadow_trail( size = 4,alpha =.5)

animate(myanim, height = 800, width =1600)
