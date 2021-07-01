#### Libraries ####
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
library(ggmap)
here()
usethis::edit_r_environ()
set.seed(1)  # Number 1 Stunnnnna - Big Tymers

options(scipen=999)
source("SlicedTheme.R")
modelData<-as_tibble(read.csv("Season 2/sliced-s01e05/train.csv",header=T,stringsAsFactors=F,sep=","))
holdData<-as_tibble(read.csv("Season 2/sliced-s01e05/test.csv",header=T,stringsAsFactors=F,sep=","))

modelData$latitude

modelData<-modelData%>%
  dplyr::select(
    -id,
    -host_name,
  )%>%
  mutate(
    last_review= ymd(last_review),
    room_type = as.factor(room_type),
    host_id = as.factor(host_id),
    neighbourhood_group = as.factor(neighbourhood_group),
    neighbourhood = as.factor(neighbourhood),
    reviews_per_month = min(reviews_per_month, 30),
    lastMonth = month(last_review),
    lastYear = year(last_review)
  )%>%
  dplyr::select(-last_review)

nycmap<-get_map("New York City", zoom = 11,  maptype = "toner-background", darken = .7)

ggmap(nycmap)+
        geom_point(
          data=modelData, 
          aes(x = longitude, y = latitude,
          color  = price),alpha = .3,size = 2)+
  scale_color_sliced("Divergent", discrete = F)+
  labs(title = "Costs on The Island",
       subtitle = "AirBNB Pricing in NYC",
       x = "Longitude",
       y = "Latitute",
       color = "Price"
  )
jpeg(filename = "NYC AirBNB Density.jpeg",quality = 1,width = 6,height = 10,units = "in",res = 500)
ggmap(nycmap)+
  stat_density_2d(
    data=modelData, 
    aes(x = longitude, y = latitude,
        fill = ..level..), geom = "polygon", alpha = .3,adjust=1)+
  scale_fill_sliced("Sequential", discrete = F)+
  scale_x_continuous(breaks = c(-74,-73.95,-73.9),limits = c(-74.02,-73.9) )+
  ylim(40.65,40.88)+
  labs(title = "Downtown The Place To Be!",
       subtitle = "AirBNB Listing Density in NYC",
       x = "Longitude",
       y = "Latitute",
       fill = "Listing Density",
       caption = "  Interesting low density artifact around Central Park, 
       guess you really shouldn't be in the park after dark.."
  )+
  theme(
    plot.caption = element_text(hjust = 0)
  )

dev.off()
modelData%>%
  ggplot(aes(x = reviews_per_month, y = price, color = neighbourhood_group))+
  geom_point(alpha = .1)+
  geom_smooth()+
  scale_y_continuous(trans = "log10")+
  scale_y_continuous(trans = "log10")+
  scale_color_sliced()+
  labs(title = "What is going on with that one place that has more reviews than days???")
  
corMat<-cor(modelData%>%select_if(is.numeric))%>%as.data.frame()%>%
  rownames_to_column()%>%arrange(desc(price))

modelData%>%
  sample_frac(.2)%>% ### TOO MUCH DATA FOR THIS COMPUTER TO PLOT
  mutate(highPrice = price>median(price))%>%
  GGally::ggscatmat(columns = which(colnames(modelData)%in%corMat$rowname),
                    color = "highPrice", alpha = .1, corMethod = "spearman")+
  scale_color_sliced()

# density plot for price and region
# which neighborhood are most expensive?
modelData%>%
  ggplot(aes(x = price, fill = neighbourhood_group))+
  geom_density(alpha = .4)+
  scale_x_continuous(trans = "log10")+
  scale_fill_sliced("Sequential")+# custom palette designed for yall
  labs(
    title = "Manhattan or MADhattan?",
    subtitle = "Density of Price of AirBNBs in NYC",
    x = "Price (Log10 Scaled)",
    y = "Density of Prices",
    fill = "Neighborhood"
  )

modelData%>%
  mutate(neighbourhood= fct_lump(neighbourhood,10))%>%
  group_by(neighbourhood)%>%
  summarise(price = median(price))%>%
  ggplot(aes(y = price, x = neighbourhood, fill = neighbourhood))+
  geom_bar(stat = "identity")+
  scale_fill_sliced()+# custom palette designed for yall
  labs(
    title = "Midtown Shuffle",
    subtitle = "Pricing of AirBNBs by Region in NYC",
    y = "Median Price",
    x = "Neighborhood",
    caption = "Bushwick or Bush League, amitire???"
  )+
  theme(legend.position = "", axis.text.x = element_text(size = 9, angle =35))

# how old is the oldest review? do old review correlate with anything?
modelData%>%
  sample_frac(.6)%>%
  mutate(neighbourhood= fct_lump(neighbourhood,10))%>%
  ggplot(aes(y =price, x = last_review, color = neighbourhood))+
  geom_point(alpha = .1)+
  scale_color_sliced()+
  scale_x_continuous(breaks = 10)+
  geom_rug()+
  labs(title = "This is a ball pit")

# sharing a room better make it cheap! are there more shared rooms anywhere?
modelData%>%
  mutate(neighbourhood= fct_lump(neighbourhood,10))%>%
  group_by(neighbourhood_group)%>%
  mutate(total = n())%>%
  ungroup()%>%
  group_by(neighbourhood_group,room_type)%>%
  summarise(prop = n()/total)%>%
  ggplot(aes(y =room_type, x = neighbourhood_group, fill = prop))+
  geom_tile()+
  scale_fill_sliced("Divergent", discrete = F)

 
# 

