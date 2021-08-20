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
modelData<-as_tibble(read.csv("Season 2/sliced-s01e08/train.csv",header=T,stringsAsFactors=F,sep=","))

modelData$artists

artists<-unique(
  (modelData%>%separate_rows(artists,sep = '\', \''))$artists%>%
    stringr::str_remove_all(., "[[:punct:]]")
  )
modelData[,artists]<-lapply(artists,function(mc){
  grepl(mc,stringr::str_remove_all(modelData$artists, "[[:punct:]]"))
})%>%do.call(cbind,.)
# top artists
modelData%>%separate_rows(artists,sep = '\', \'')%>%
  mutate(artists = stringr::str_remove_all(artists, "[[:punct:]]"),
         elite = popularity>quantile(popularity,.98))%>%
  group_by(artists)%>%
  summarise(popularity = mean(popularity),
            eliteNum = sum(elite),
            n = n()
  )%>%
  ungroup()%>%
  filter(eliteNum ==1)%>%
  mutate(artists = fct_reorder(artists, popularity))%>%
  filter(popularity > quantile(popularity, .8))%>%
  ggplot(aes(x = artists, y = popularity, fill = eliteNum))+
  geom_bar(stat = "identity")+
  scale_fill_sliced("Divergent",discrete = F)+
  theme(axis.text.x = element_text(angle = 35, size = 7))
  



# 1 hit wonders

# popularity by language

# rise of danceability

# can we find when dubstep happened? temp = 140

# most popular keys by time

# cluster similar songs

