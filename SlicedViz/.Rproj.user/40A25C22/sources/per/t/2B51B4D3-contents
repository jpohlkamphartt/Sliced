library(tidyverse)
library(ggplot2)
library(here)
library(lubridate)
library(parallel)
library(patchwork)
library(extrafont)
library(skimr)
library(beepr)
here()
usethis::edit_r_environ()
set.seed(1)

options(scipen=999)
source("SlicedTheme.R")

modelData<-as_tibble(read.csv("Season1/s00e01/Copy of sliced-s00e01-data.csv",header=T,stringsAsFactors=F,sep=","))

modelData<-modelData%>%
  dplyr::select(
    -X,
    -id,
    -dec,
    -dec_o,
    # -idg,
    # -position,
    # -positin1,
    # -pid,
    -field,
    -from,
    -career,
    -partner
  )

skim(modelData)

modelData$match<-as.factor(modelData$match)

# correlation plot
corMat<-cor(
  modelData%>%
    mutate(match=as.numeric(match))%>%
    select_if(is.numeric),use = "pairwise.complete.obs")%>%
  as.data.frame()%>%
  rownames_to_column()%>%
  arrange(desc(match))

modelData%>%
  GGally::ggscatmat(columns = which(colnames(modelData)%in%corMat$rowname[2:10]),color="match", alpha=.1, corMethod = "spearman")+
  scale_color_sliced(palette = "Qualitative")

# looks like the ..._o and ... scores are the most important!

# density plots for factors: mn_sat, tuition, race
varz2graph<-c("match","mn_sat", "tuition", "race","race_o")
graphlist<-lapply(1:length(varz2graph),function(i){
  modelData%>%
    mutate_at(vars(varz2graph),as.factor)%>%
    mutate_at(vars(varz2graph[i]),fct_lump,n=10)%>%
    ggplot(aes_string(
      x=varz2graph[i]
    ))+
    geom_histogram(stat="count")+
    theme(legend.position = "top",axis.text.x = element_text(size = 10,angle = 45))
})

graphlist[[1]]+graphlist[[2]]+graphlist[[3]]+graphlist[[4]]

# histogram with damage fill
graphlist_filled<-lapply(2:length(varz2graph),function(i){
  modelData%>%
    mutate_at(vars(varz2graph),as.factor)%>%
    mutate_at(vars(varz2graph[i]),fct_lump,n=10)%>%
    ggplot(aes_string(
      x=varz2graph[i],fill = varz2graph[1]
    ))+
    geom_histogram(stat="count")+
    theme(legend.position = "top",axis.text.x = element_text(size = 10,angle = 45))
})

graphlist_filled[[1]]+graphlist_filled[[2]]+graphlist_filled[[3]]+graphlist_filled[[4]]

# proportion plot with damage
graphlist_prop<-lapply(2:length(varz2graph),function(i){
  modelData%>%
    mutate_at(vars(varz2graph),as.factor)%>%
    mutate_at(vars(varz2graph[i]),fct_lump,n=10)%>%
    group_by_at(vars(varz2graph[i]))%>%
    summarise(prop = sum(match==1)/n(),
              n = n())%>%
    ggplot(aes_string(
      x=varz2graph[i],y = "prop",fill = "n"
    ))+
    geom_bar(stat = "identity")+
    scale_fill_sliced("Divergent",discrete = F)+
    theme(legend.position = "top",axis.text.x = element_text(size = 10,angle = 45))
})

graphlist_prop[[1]]+graphlist_prop[[2]]+graphlist_prop[[3]]+graphlist_prop[[4]]
# density plots of initial indicators
modelData$ageDiff<-abs(modelData$age-modelData$age_o)

varz2graph<-c("match","int_corr","attr_o","fun_o","like_o","attr","fun","like","ageDiff","prob")
tranz<-c("identity",'identity',"identity",'identity',"identity",'identity',"identity",'identity')#,'log10','log10','log10')
graphlist<-lapply(2:length(varz2graph),function(i){
   modelData%>%
    ggplot(aes_string(
      x=varz2graph[i]
    ))+
    geom_density()+
    theme(legend.position = "top")
  # +
  #   scale_x_continuous(trans=tranz[i])
})

graphlist[[1]]+graphlist[[2]]+graphlist[[3]]+graphlist[[4]]+
  graphlist[[5]]+graphlist[[6]]+graphlist[[7]]+graphlist[[8]]+graphlist[[9]]

# density with damage fill
graphlist_filled<-lapply(2:length(varz2graph),function(i){
  modelData%>%
    ggplot(aes_string(
      x=varz2graph[i],fill = varz2graph[1]
    ))+
    geom_density(alpha = .5)+
    theme(legend.position = "top")#+
    # scale_x_continuous(trans=tranz[i])
})

graphlist_filled[[1]]+graphlist_filled[[2]]+graphlist_filled[[3]]+graphlist_filled[[4]]+
  graphlist_filled[[5]]+graphlist_filled[[6]]+graphlist_filled[[7]]+graphlist_filled[[8]]+
  graphlist_filled[[9]]

# does similar age matter?
graphlist_filled[[8]]+
  labs(title = "Effect on Relative Ages of Partners",
       subtitle = "Bigger Differences Creates Less Matches",
       y = "Density",
       x = "Age Difference",
       fill = "Did They Match")+
  theme(legend.position = "right")+
  scale_fill_sliced(palette = "Qualitative")

# going out create more matches? 
varz2graph<-c("match","goal", "date", "go_out","dining","clubbing","imprelig")
graphlist_prop<-lapply(2:length(varz2graph),function(i){
  modelData%>%
    mutate_at(vars(varz2graph),as.factor)%>%
    mutate_at(vars(varz2graph[i]),fct_lump,n=10)%>%
    group_by_at(vars(varz2graph[i]))%>%
    summarise(prop = sum(match==1)/n(),
              n = n())%>%
    ggplot(aes_string(
      x=varz2graph[i],y = "prop",fill = "n"
    ))+
    geom_bar(stat = "identity")+
    scale_fill_sliced("Divergent",discrete = F)+
    theme(legend.position = "",axis.text.x = element_text(size = 10,angle = 45))
})

graphlist_prop[[1]]+graphlist_prop[[2]]+graphlist_prop[[3]]+graphlist_prop[[4]]+
  graphlist_prop[[5]]+graphlist_prop[[6]]

# looks like their is a homebody effect!
graphlist_prop[[3]]+graphlist_prop[[4]]+
  graphlist_prop[[5]]+theme(legend.position ="right")+
  plot_annotation(
    title = 'Homebodies want bodies',
    subtitle = 'The effect of low social activity on matching',
    caption = 'Notice how there is a spike in low outing people matching'
  )
# does race matter? samerace/imprace/match
modelData%>%
  group_by(imprace,samerace)%>%
  summarise(match = sum(match ==1, na.rm = T)/n())%>%
  na.omit()%>%
  ggplot(aes(x=as.factor(imprace),y=as.factor(samerace),fill= match))+
  geom_tile()+
  scale_fill_sliced("Divergent",discrete = F)

modelData%>%
  group_by(imprace,samerace)%>%
  summarise(match = sum(match ==1, na.rm = T)/n(),
            n = n())%>%
  na.omit()%>%
  ggplot(aes(x=as.factor(imprace),y=as.factor(samerace),size = n,color= match))+
  geom_point()+
  labs(title = "Importance of Race in Starting Relationships",
       subtitle = "Even if you don't say race is important, it has an impact",
       color = "% of matches",
       y = "Same Race",
       x = "Importance Of Race To Dater")+
  scale_color_sliced("Divergent", discrete = F)+
  theme_dark()

# interests

# all the crazy other factors

# which round in the night?
varz2graph<-c("match","wave","round","order","position","positin1","pid")
tranz<-c("identity",'identity',"identity",'identity',"identity",'identity')
graphlist<-lapply(2:length(varz2graph),function(i){
  modelData%>%
    ggplot(aes_string(
      x=varz2graph[i]
    ))+
    geom_density()+
    theme(legend.position = "top")
  # +
  #   scale_x_continuous(trans=tranz[i])
})

graphlist[[1]]+graphlist[[2]]+graphlist[[3]]+graphlist[[4]]+
  graphlist[[5]]+graphlist[[6]]

# density with damage fill
graphlist_filled<-lapply(2:length(varz2graph),function(i){
  modelData%>%
    ggplot(aes_string(
      x=varz2graph[i],fill = varz2graph[1]
    ))+
    geom_density(alpha = .5)+
    theme(legend.position = "top")#+
  # scale_x_continuous(trans=tranz[i])
})

graphlist_filled[[1]]+graphlist_filled[[2]]+graphlist_filled[[3]]+graphlist_filled[[4]]+
  graphlist_filled[[5]]+graphlist_filled[[6]]

graphlist_filled[[3]]+
  labs(title = "Burn out on the dating track",
       subtitle = "The longer into the night the lower the matching odds",
       y = "Density",
       x = "Round into Speed Dating",
       fill = "Did They Match")+
  theme(legend.position = "right")+
  scale_fill_sliced(palette = "Qualitative")

library(tidygeocoder)

tic()
modelData<-modelData%>%
  mutate(country = "USA")%>%
geocode(postalcode = zipcode,country = country, method = 'osm')
toc()


library(ggbump)
modelData_cumsum<-modelData%>%
  group_by(wave, idg)%>%
  summarise(matches = cumsum(match))%>%
  summarise(matches = max(matches))%>%
  ungroup()%>%
  group_by(idg)%>%
  mutate(totalMatches = cumsum(matches))%>%
  ungroup()%>%
  group_by(wave)%>%
  mutate(rank = order(totalMatches,decreasing = T))%>%ungroup()
 
hotMatches<-modelData_cumsum%>%
  filter(wave == max(wave),
         rank %in% 1:5
         )%>%
  dplyr::select(idg)%>%unlist()
  
modelData_cumsum%>%
filter(idg%in%hotMatches)%>%
ggplot(aes(x = wave, y = rank, color = as.factor(idg)))+
  geom_point(size = 5)+
  geom_bump(size = 2, smooth = 8) +
  scale_color_sliced(palette = "Sequential")+
  labs(title = "Race For The Matches!",
       subtitle = "The Progression of Ranking in Cumulative Matches",
       y = "Ranking in Total Matches",
       x = "Wave of Speed Dating",
       color = "ID of Participant")+
  scale_y_reverse()
