library(tidyverse)
library(tidymodels)
library(recipes)
library(themis)
library(vip)
library(ggplot2)
library(furrr)
library(here)
library(lubridate)
library(mgcv)
library(parallel)
library(workflowsets)
library(tidyposterior)
library(discrim)
library(hablar)
library(doParallel)
library(patchwork)
library(textrecipes)
library(extrafont)
library(fastshap)
library(skimr)
library(beepr)
options(tidymodels.dark = TRUE)
here()
usethis::edit_r_environ()
set.seed(1)

options(scipen=999)
source("SlicedTheme.R")
modelData<-as_tibble(read.csv("Season 2/sliced-s01e02/train.csv",header=T,stringsAsFactors=F,sep=","))

#### GGAlly corr plot

#### Facet Patchwork
p1<-modelData%>%
  ggplot(aes(x=damaged,fill=flight_impact))+
  geom_histogram(stat="count")+
  theme(legend.position = "top")
p2<-modelData%>%
  ggplot(aes(x=damaged,fill=species_quantity))+
  geom_histogram()+
  theme(legend.position = "top")

p1+p2
## not seeing a ton based on these ratios

# lets remove some extra variables like name, I bet Tony is hamming it up..
modelData<-modelData%>%
  dplyr::select(
    -id, 
    -operator,
    -aircraft,
    -airport,
    -species_name
  )

# note on Kaggle says there is some nested data in engine type and precipitation, lets fix that
enginetypes<-na.omit(unique((modelData%>%separate_rows(engine_type,sep="/"))$engine_type))
# make one hots for each engine type
modelData[,enginetypes]<-lapply(enginetypes,function(et){
  as.numeric(grepl(et,modelData$engine_type))
})%>%do.call(cbind,.) 
modelData$C<-as.numeric(modelData$C+modelData$c>0)
# precipitation
precipitations<-unique(trimws(na.omit((modelData%>%separate_rows(precipitation,sep=","))$precipitation)))
# make one hots for each engine type
modelData[,precipitations]<-lapply(precipitations,function(pr){
  as.numeric(grepl(pr,modelData$precipitation))
})%>%do.call(cbind,.) 

modelData$engine3_position[modelData$engine3_position=="CHANGE CODE"]<-NA

skim(modelData[,1:20])
# observations
# day is most likely useless unless a holiday..
# Jan 27 is most likely accident day..
modelData%>%
  mutate(dayMonth = days(incident_day)+months(incident_month))%>%
  group_by(dayMonth)%>%
  summarise(
    damaged = sum(damaged,na.rm=T)/n()
  )%>%
  arrange(desc(damaged))
modelData<-modelData%>%
  # aircraft model.mass,engine_make, position, is a factor
  mutate_at(
    vars(aircraft_model,aircraft_mass,engine_make,
         engine1_position,engine2_position,engine4_position),
    as.factor)%>%
  mutate_if(is.character,as.factor)%>%
  # remove engine type and precip
  dplyr::select(
    -precipitation,
    -engine_type
  )
modelData$damaged<-as.factor(modelData$damaged)

# lets explore the data we are modeling
# we found that departure is the worst of the parts of flight, lets see
modelData%>%
  group_by(flight_phase)%>%
  summarise(damaged = sum(damaged==1,na.rm=T)/n())%>%
  na.omit()%>%
  ggplot(aes(x=flight_phase,y = damaged,fill = flight_phase))+
           geom_bar(width =1 , stat = "identity", color = "white")+
           scale_y_continuous(breaks= 0:(length(unique(modelData$flight_phase))-1))+
coord_polar()+
  labs(title = "Radar shows.. Descent and In Flight are no good!",
       fill = "Phase",
       y = "% flights damaged")+
  scale_fill_sliced(palette = "Sequential") ## can we tswift?
  
# so I thought it was departure but that is safe!!! we can check the final SHAPs if we have time

# engine type F looks like it is significant, I wonder how
modelData%>%separate_rows(engine_type,sep="/")%>%
  mutate(engine_type = ifelse(engine_type=="c","C",engine_type))%>%
  group_by(engine_type)%>%
  summarise(damaged = sum(damaged,na.rm=T)/n())%>%
  na.omit()%>%
  ggplot(aes(x=engine_type,y = damaged,fill = engine_type))+
  geom_bar(width =1 , stat = "identity", color = "white")+
  scale_y_continuous(breaks= 0:(length(unique(modelData$flight_phase))-1))+
  labs(title = "B-have! Turbojets are nooooo good ",
       fill = "Engine Type",
       y = "% flights damaged",
       x = "Engine Type")+
  tayloRswift::scale_fill_taylor("lover") ## can we t

# how about the cross of engine and phase? might be why the linear model did well..
modelData%>%separate_rows(engine_type,sep="/")%>%
  mutate(engine_type = ifelse(engine_type=="c","C",engine_type))%>%
  group_by(engine_type,flight_phase)%>%
  summarise(damaged = sum(damaged,na.rm=T)/n())%>%
  na.omit()%>%
  ggplot(aes(x=engine_type,y = flight_phase,color = damaged,size = damaged))+
  geom_point()+
  labs(title = "Lol at parked..",
       fill = "% flights damaged",
       y = "Phase",
       x = "Engine Type")+
  tayloRswift::scale_fill_taylor("lover") ## can we 

# lets look at years
modelData%>%
  # group_by(engine_type,flight_phase)%>%
  # summarise(damaged = sum(damaged,na.rm=T)/n())%>%
  # na.omit()%>%
  ggplot(aes(x=incident_year,y = engines,color = damaged,size = damaged))+
  geom_point(alpha=.1)+
  geom_smooth()+
  labs(title = "This is an ugly scatter plot",
       subtitle = "cool to see less engines on average, go green!",
       color = "% flights damaged",
       y = "engines",
       x = "year")+
  tayloRswift::scale_fill_taylor("lover") ## can we t


modelData%>%
  group_by(incident_year)%>%
  summarise(damaged = sum(damaged,na.rm=T)/n())%>%
  # na.omit()%>%
  ggplot(aes(x=incident_year,y =  damaged))+
  geom_line()+
  # geom_smooth()+
  labs(title = "Improvement in Flight Safety",
       subtitle = "Wow, look at the decrease by year!!",
       y = "Damage Rate on Flights with accidents",
       x = "year")+
  scale_fill_sliced(palette = "Qualitative") ## can we 


modelDataTS<-modelData%>%
  mutate(Date = as_date(days(incident_day)+months(incident_month)+years(incident_year),origin=ymd("0000-1-1")))%>%
  group_by(Date)%>%
  summarise(
    damaged = sum(damaged==1,na.rm=T)/n()
  )%>%
  ungroup()%>%
  complete(Date=seq.Date(min(Date), max(Date), by="day"),fill=list(damaged=0))

modelDataTS<-ts(modelDataTS$damaged,start = c(1990,02,02),frequency = 365)

plot(decompose(modelDataTS))

modelData%>%
  mutate(species_id = fct_lump(species_id,9))%>%
  group_by(species_id)%>%
  summarise(damaged = sum(damaged==1,na.rm=T)/n(),
            samples = n(),
            states = length(unique(state)))%>%
  na.omit()%>%
  ggplot(aes(x=samples ,y =  states, color = species_id, size = damaged))+
  geom_point()+
  # geom_smooth()+
  labs(title = "The more common the bird the more samples by state",
       subtitle = "More states is generally more damage",
       y = "Damage Rate on Flights with accidents",
       x = "Samples")+
  scale_color_sliced(palette = "Qualitative") ## can we 
# lots of very rare events
