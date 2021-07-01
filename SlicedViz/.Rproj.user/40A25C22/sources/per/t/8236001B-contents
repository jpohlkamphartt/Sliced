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
options(tidymodels.dark = TRUE)
here()
usethis::edit_r_environ()
set.seed(1)

options(scipen=999)
source("SlicedTheme.R")

modelData<-as_tibble(read.csv("Season 2/sliced-s01e01/train.csv",header=T,stringsAsFactors=F,sep=","))

## plan
# cleanup - exploration
skimr::skim(modelData)

# remove: game_id, names
modelData<-modelData%>%
  dplyr::select(
    -game_id, -names
  )
# fix shitespace issues on character strings
modelData$category<-apply( modelData%>%dplyr::select(contains("category")) , 1 , paste , collapse = "," )
# make comma seperated mechanic/category, designer column into 1-hots
designers<-unique(trimws((modelData%>%separate_rows(designer,sep=","))$designer))
designers<-designers[designers!="NA"]
modelData[,designers]<-lapply(designers,function(d){
  grepl(d,modelData$designer)
})%>%do.call(cbind,.)
#mechanic
mechanics<-unique(trimws((modelData%>%separate_rows(mechanic,sep=","))$mechanic))
mechanics<-mechanics[mechanics!="NA"]
modelData[,mechanics]<-lapply(mechanics,function(d){
  grepl(d,modelData$mechanic)
})%>%do.call(cbind,.)
# turn categoryX into 1-hots
categories<-unique(trimws((modelData%>%separate_rows(category,sep=","))$category))
categories<-categories[categories!="NA"]
modelData[,categories]<-lapply(categories,function(d){
  grepl(d,modelData$category)
})%>%do.call(cbind,.)
modelData<-modelData%>%
  # dplyr::select(
  #   -contains("category"),
  #   -designer,
  #   -mechanic
  # )%>%
  # fix years that make no sense
  mutate(year = ifelse(year<1600,NA,year))

# feature eng
modelData<-modelData%>%
  mutate(
    range_players = max_players-min_players,
    range_time = max_time-min_time,
    adult = age>=18
  )%>%
  mutate_if(is.logical,as.numeric)


# plots
### top designers?
p1<-ggplot(modelData%>%,aes(y=geek_rating,x= fill=fct_lump(category1,7)))+
  geom_histogram(position = "dodge")+
  scale_fill_manual(values = Sliced_Qualitative_Palette)


modelDataPlot<-as_tibble(read.csv("Season 2/sliced-s01e01/train.csv",header=T,stringsAsFactors=F,sep=","))

modelDataPlot%>%separate_rows(designer,sep=",")%>%
  mutate(designer =trimws(designer),
         fct_lump(designer,10))%>%
ggplot(aes(y=geek_rating,x=num_votes,color=designer))+
  geom_point()+
  scale_fill_manual(values = Sliced_Qualitative_Palette)

modelData%>%separate_rows(designer,sep=",")%>%
  mutate(designer =trimws(designer),
         fct_lump(designer,10))%>%
  group_by(designer)%>%
  summarise(geek_rating=mean(geek_rating,na.rm=T),
            war = sum(`War Games`,na.rm=T)/n()
  ggplot(aes(y=geek_rating,x=num_votes,color=designer))+
  geom_point()+
  scale_fill_manual(values = Sliced_Qualitative_Palette)


p2<-ggplot(modelData%>%filter(year>1960),aes(x=year,y=geek_rating,color=fct_lump(category1,7)))+
  geom_point(alpha=.2)+
  scale_fill_manual(values = Sliced_Qualitative_Palette)
p1+p2

### top categories?

### old games still good?

### what is up with wargames?

### cardz though why?