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
modelData<-as_tibble(read.csv("Season 2/sliced-s01e06/train.csv",header=T,stringsAsFactors=F,sep=","))


currenttop<-(modelData%>%filter(Year == 2021, Month == 4,Rank<10))$Game

modelData%>%
  filter(Game %in%currenttop)%>%
  mutate(date = ym(paste(Year,Month,sep   = "-")))%>%
  ggplot(aes(x = date, y = Rank, color = as.factor(Game)))+
  geom_point(size =5)+
  geom_bump(size = 2, smooth = 8,alpha = .95)+
  scale_color_sliced(palette = "Sequential")+
  scale_y_reverse()+
  labs(
    title = "Chattin' And Chartin'",
    subtitle = "Temporal Look at Rankings for Current top 10 Games",
    y = "Ranking",
    x = "Time",
    color = "Game",
    caption = "These games cover a variety of Genres, showing that uniqueness is paramount to success"
  )

skim(modelData)

# look at the relationships between:
# hours watched vs streamed
modelData%>%
  ggplot(aes(x = Hours_Streamed/Hours_watched, y = log(Rank)))+#, color = Rank))+
  geom_point(aes(color = (Streamers)),alpha = .5)+
  # geom_smooth(method = "lm")+
  scale_alpha_manual(values = c(.1,1))+
  scale_color_sliced(palette = "Divergent",discrete = F)+
  scale_y_reverse()+
  labs(
    title = "Streaming into the Void..",
    subtitle = "Impact of Streaming on Global Ranking",
    x = "Streaming Ratio (Streaming/Watching Hrs)",
    y = "Logged Rank",
    color = "Streamers",
    alpha = "top 20% of Streamers",
    caption = "We see that the top games have both high streaming and watching but for less popular games the amount of streamers relative to watchers grows."
  )
    
    
# peak viewers/channels
modelData%>%
  filter(Game %in%currenttop)%>%
  mutate(date = ym(paste(Year,Month,sep   = "-")))%>%
  group_by(Month)%>%
  summarise(vperc=Peak_viewers/Peak_channels,
            Streamers = mean(Streamers,na.rm=T))%>%
  ggplot(aes(x = Month, y = vperc, fill = Streamers))+
  geom_bar(stat = "identity")+
  scale_fill_sliced(palette = "Divergent",discrete = F)+
  scale_x_discrete(limits = c("Jan", "Feb", "Mar", "Apr", "May","Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))+
  labs(
    y = "Peak Viewers Per Channel",
    title = "Schooooools Out For Gaming",
    subtitle = "Impact of Month on Gaming Viewership and Streaming",
    caption = "Notice that at the start of the school year gaming goes down, but comes back in the spring.
    Addiction is a bitch."
  )
    

modelData%>%
  group_by(Game)%>%
  summarise(vperc=Peak_viewers/Peak_channels,
              Rank = mean(Rank,na.rm=T))%>%
  ungroup()%>%
  mutate(vpercQuant = ecdf(vperc)(vperc))%>%
  arrange(desc(vperc))%>%
  head(20)%>%
  mutate(Game = fct_reorder(Game, Rank))%>%
  ggplot(aes(x = Game, y = Rank, fill = vpercQuant))+#, color = Rank))+
  geom_bar(stat = "identity")+
  scale_fill_sliced(palette = "Divergent",discrete = F)+
  labs(
    title = "The Dedicated Few",
    subtitle = "Top 20 Games with Most Viewers Per Channel",
    x = "Game",
    y = "Global Rank",
    fill = "Percentile of Viewers/Channel",
    caption = "Interesting to see many of these games have a long history in gaming and have rich stories.
    People are dedicated to the lore!"
  )+
  theme(axis.text.x = element_text(angle = 35, size = 7))
# streamers by date
modelData%>%
  mutate(date = ym(paste(Year,Month,sep   = "-")), topRank = Rank <21)%>%
  group_by(date,topRank)%>%
  summarise(Streamers = sum(Streamers,na.rm=T))%>%
  ggplot(aes(x = date, y = Streamers, color = topRank))+
  geom_point(size =2)+
  geom_line()+
  scale_color_sliced()+
  labs(
    y = "Total Streamers",
    title = "The Rise of Twitch",
    subtitle = "Increase in Streaming on Twitch by Date",
    color = "Top 20",
    caption = "It is interesting to see in late 2017 the top games begin to pull away from the rest.
    Is this when people began to see the monitization opportunity and followed the herd?"
  )

# total hours by date
modelData%>%
  mutate(date = ym(paste(Year,Month,sep   = "-")), topRank = Rank <21)%>%
  group_by(date,topRank)%>%
  summarise(Streamers = sum(Hours_watched,na.rm=T))%>%
  ggplot(aes(x = date, y = Streamers, color = topRank))+
  geom_point(size =2)+
  geom_line()+
  scale_color_sliced()+
  labs(
    y = "Total Hours Watched",
    title = "Many Lifetimes Spent Watching",
    subtitle = "Increase in Watching on Twitch by Date",
    color = "Top 20",
    caption = "Damn that is a lot of game watching, I guess I like to watch reviews of boardgames so I can't judge."
  )

modelData%>%
  group_by(Game)%>%
  summarise(maxRank = min(Rank),
            wks = n(),
            avrgRank = mean(Rank))%>%
  filter(maxRank<6, avrgRank<quantile(avrgRank,.95),wks<quantile(wks,.9))%>%
  arrange(desc(avrgRank))%>%
  head(8)%>%
  dplyr::select(Game)%>%
  left_join(modelData)%>%
  mutate(date = ym(paste(Year,Month,sep   = "-")))%>%
  ggplot(aes(x = date, y = Rank, color = as.factor(Game)))+
  geom_point(size =5)+
  geom_bump(size = 2, smooth = 8,alpha = .95)+
  scale_color_sliced(palette = "Sequential")+
  scale_y_reverse()+
  labs(
    title = "Short Lived But Once Loved",
    subtitle = "Games that were highly ranked but dropped off and do not have high week counts",
    y = "Ranking",
    x = "Time",
    color = "Game",
    caption = "Can see Among Us and Fall Guys were popular once..Sorry Nick/Meg"
  )

# Rank vs streamers/channels
# game titles? text mining?
library(gganimate)
myanim<-modelData%>%
  filter(Game %in%(modelData%>%filter(Year == 2021, Month == 4,Rank<11))$Game)%>%
  mutate(date = ym(paste(Year,Month,sep   = "-")))%>%
  ggplot(aes(x = date, y = log(Hours_watched), color = as.factor(Game)))+
  geom_point(size = 5)+
  scale_color_sliced()+
  Sliced_theme()+
  labs(
    title = "Watch The Games Bounce As They Move To The Top!",
    subtitle = "Top Games Evolution Over Time",
    y = "Log Rank",
    x = "Date",
    color = "Game"
  )+
  transition_time(date) +
  ease_aes()+
  enter_fade() + 
  exit_shrink()+
  shadow_trail( size = 1.5,alpha =.5)

animate(myanim, height = 800, width =800)


modelData%>%  filter(Game %in%currenttop)%>%
  mutate(date = ym(paste(Year,Month,sep   = "-")))%>%
  ggplot(aes(x = log(Hours_Streamed), y = log(Hours_watched)))+
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white")+
  scale_fill_sliced(palette = "Divergent",discrete = F)+
  Sliced_theme()+
  labs(
    y = "Log(Hours Watched)",
    x = "Log(Hours Streamed)",
    title = "Hypno-Blob! Vote for the weird viz",
    subtitle = 'Density of Activity for Month {frame_time}',
    caption = "Notice that at the start of the school year gaming goes down, but comes back in the spring.
    Addiction is a bitch."
  )+
  transition_time(Month) +
  ease_aes()
