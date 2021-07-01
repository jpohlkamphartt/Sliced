#### Data Viz Defaults ####
### time series
# create ts
modelDataTS<-modelData%>%
  #make date column
  mutate(Date = as_date(days(incident_day)+months(incident_month)+years(incident_year),origin=ymd("0000-1-1")))%>%
  # get data summarized by date
  group_by(Date)%>%
  summarise(
    damaged = sum(damaged==1,na.rm=T)/n()
  )%>%
  ungroup()%>%
  # fill in all dates
  complete(
    Date=seq.Date(min(Date), max(Date), by="day"),
    # this is the default for the filled data
    fill=list(damaged=0)
    )

#daily very noisy
ggplot(modelDataTS,aes(x=Date,y=damaged))+
  geom_line(alpha=.2)+
  geom_smooth()

#monthly less noisy
modelData%>%
  #make date column
  mutate(Date = as_date(months(incident_month)+years(incident_year),origin=ymd("0000-1-1")))%>%
  # get data summarized by date
  group_by(Date)%>%
  summarise(
    damaged = sum(damaged==1,na.rm=T)/n()
  )%>%
  ungroup()%>%
  # fill in all dates
  complete(
    Date=seq.Date(min(Date), max(Date), by="month"),
    # this is the default for the filled data
    fill=list(damaged=0)
  )%>%
ggplot(aes(x=Date,y=damaged))+
  geom_line(alpha=.2)+
  geom_smooth()

modelDataTS<-ts(
  #response data
  modelDataTS$damaged,
  # start date
  start = c(1990,02,02),
  # frequency
  frequency = 365)

# decomp plot
plot(decompose(modelDataTS))
acf_ts<-acf(modelDataTS,lag.max =200)
acf_ts$lag[abs(acf_ts$acf)>.02]
pacf_ts<-pacf(modelDataTS,lag.max =200)
plot(pacf_ts)
pacf_ts$lag[abs(pacf_ts$acf)>.02]

# simple time series
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
  scale_fill_sliced(palette = "Qualitative")

### radial coordinates
# radial
ggplot(DF, aes(x = variable, y = value, fill = variable)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_errorbar(aes(ymin = value - se(DF$value), 
                    ymax = value + se(DF$value), 
                    color = variable), 
                width = .2) + 
  scale_y_continuous(breaks = 0:nlevels(DF$variable)) +
  theme_gray() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank()) +
  coord_polar()

# pie
holdPredz%>%group_by(.pred_class)%>%
  summarise(prop = 100*round(n()/nrow(holdPredz),3),
  )%>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)%>%
  ggplot(aes(x = "", y = prop, fill = .pred_class)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  scale_fill_manual(values = Sliced_Qualitative_Palette) +
  theme_void()+
  ggtitle("Proportion of Predicted Matches")

### correlations 
# fancy ggally
corMat<-cor(modelData%>%mutate(damaged=as.numeric(damaged))%>%select_if(is.numeric),use = "pairwise.complete.obs")%>%as.data.frame()%>%rownames_to_column()%>%arrange(desc(damaged))

modelData%>%
  GGally::ggscatmat(columns = which(colnames(modelData)%in%corMat$rowname[2:10]),color="damaged", alpha=.1, corMethod = "spearman")+
  scale_color_sliced(palette = "Qualitative")

# simple heatmap
library(reshape2)
ggplot(melt(corMat), aes(x=rowname, y=variable, fill=value)) + 
  geom_tile()+
  scale_fill_sliced(palette = "Divergent",discrete = F)

### bar graphs
# histogram facet
varz2graph<-c("damaged","species_quantity","operator_id","aircraft_mass","faa_region")

graphlist<-lapply(1:length(varz2graph),function(i){
  modelData%>%
    mutate_at(vars(varz2graph[i]),fct_lump,n=10)%>%
    ggplot(aes_string(
      x=varz2graph[i]
    ))+
    geom_histogram(stat="count")+
    theme(legend.position = "top")
})

graphlist[[1]]+graphlist[[2]]+graphlist[[3]]+graphlist[[4]]

# histogram with damage fill
graphlist_filled<-lapply(2:length(varz2graph),function(i){
  modelData%>%
    mutate_at(vars(varz2graph[i]),fct_lump,n=10)%>%
    ggplot(aes_string(
      x=varz2graph[i],fill = varz2graph[1]
    ))+
    geom_histogram(stat="count")+
    theme(legend.position = "top")
})

graphlist_filled[[1]]+graphlist_filled[[2]]+graphlist_filled[[3]]+graphlist_filled[[4]]

# proportion plot with damage
graphlist_prop<-lapply(2:length(varz2graph),function(i){
  modelData%>%
    mutate_at(vars(varz2graph[i]),fct_lump,n=10)%>%
    group_by_at(vars(varz2graph[i]))%>%
    summarise(prop = sum(damaged==1)/n(),
              n = n())%>%
    ggplot(aes_string(
      x=varz2graph[i],y = "prop",fill = "n"
    ))+
    geom_bar(stat = "identity")+
    theme(legend.position = "top")+
    scale_fill_sliced("Divergent",discrete = F)
})

graphlist_prop[[1]]+graphlist_prop[[2]]+graphlist_prop[[3]]+graphlist_prop[[4]]

# simple histogram
modelData%>%
  # drop_na(species_quantity)%>%
  ggplot(aes(
    x=damaged,
    # fill=species_quantity
  ))+
  geom_histogram(stat="count")+
  theme(legend.position = "top")+
  # scale_y_continuous(trans='log10')+
  scale_fill_sliced()

# density plot
# density facet
varz2graph<-c("damaged","incident_year","height","speed","distance")
tranz<-c("identity",'identity','log10','log10','log10')
graphlist<-lapply(2:length(varz2graph),function(i){
  modelData%>%
    ggplot(aes_string(
      x=varz2graph[i]
    ))+
    geom_density()+
    theme(legend.position = "top")+
    scale_x_continuous(trans=tranz[i])
})

graphlist[[1]]+graphlist[[2]]+graphlist[[3]]+graphlist[[4]]

# density with damage fill
graphlist_filled<-lapply(2:length(varz2graph),function(i){
  modelData%>%
    ggplot(aes_string(
      x=varz2graph[i],fill = varz2graph[1]
    ))+
    geom_density(alpha = .5)+
    theme(legend.position = "top")+
    scale_x_continuous(trans=tranz[i])
})

graphlist_filled[[1]]+graphlist_filled[[2]]+graphlist_filled[[3]]+graphlist_filled[[4]]

### 2D graphs
# simple scatter
modelData%>%
  ggplot(aes(color=as.factor(damaged),x=height,y=speed))+
  geom_point()+
  geom_smooth()+
  theme(legend.position = "top")+
  scale_color_sliced(discrete = T)+
  scale_x_continuous(trans = "log10")+
  scale_y_continuous(trans = "log10")+
  labs(title = "Faster = Higher = Damage?",
       color = "Damaged",
       y = "Log10 Speed",
       x = "Log10 Height")+
  geom_rug()
  
# proportional bar chart
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
  scale_fill_sliced()

# dot plot of variables
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
  scale_color_sliced("Divergent", discrete = F) 

# real scatter plot
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
  scale_color_sliced(palette = "Qualitative")

# nice bar chart
modelData%>%
  mutate(state = fct_lump(state,15))%>%
  group_by(state)%>%
  summarise(damaged = sum(damaged==1,na.rm=T)/n(),
            samples = n(),
            numUN = length(unique(species_id)))%>%
  na.omit()%>%
  ggplot(aes(x=state ,size =  numUN, fill = samples, y = damaged))+
  geom_bar(stat="identity")+
  # geom_smooth()+
  labs(title = "",
       subtitle = "",
       y = "",
       x = "")+
  scale_fill_sliced(palette = "Divergent",discrete = F)

# hex plot
modelData%>%
  ggplot(aes(x=height,y=distance))+
  geom_hex()+
  # geom_smooth()+
  labs(title = "",
       subtitle = "",
       y = "",
       x = "")+
  scale_fill_sliced(palette = "Divergent",discrete = F)+
  scale_fill_continuous(trans = "log10")

# contour map
modelData%>%
  #filter if almost all data is zero
  filter(height>0, distance>0)%>%
  drop_na(height,distance)%>%
  ggplot(aes(x=height,y=distance))+
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white")

#### US MAP
# Add State by zipcode
modelData%>%bind_rows(holdData)%>%
  dplyr::select(-state)%>%
  left_join(zipcodeR::zip_code_db %>%
              mutate(zipcode = as.numeric(zipcode))%>%
              dplyr::select(
                zipcode,state
              ),
            by = c("postal_code"= "zipcode")
  )%>%
  group_by(state)%>%
  summarise(sales = sum(sales))%>%
  usmap::plot_usmap("state", data = ., values = "sales") +
  scale_fill_sliced("Divergent",discrete = F)

# Already Have State
modelData%>%bind_rows(holdData)%>%
  filter(segment!="corporate")%>%
  group_by(state)%>%
  summarise(discount = mean(discount))%>%
  usmap::plot_usmap("states", data = ., values = "discount") +
  scale_fill_sliced("Divergent",discrete = F)+
  labs(
    title = "Discounts are bigger in TEXAS",
    subtitle = "State level variability in discount rates",
    caption = "Interesting that the green belt and midwest do not get discounts",
    fill = "Average Discount"
  )

### bump
## regions by rainfall and temperature
model_cum<-modelData%>%
  mutate(
    monthYear=my(paste(month(date),year(date))))%>%
  complete(monthYear = seq.Date(min(monthYear),max(monthYear),by="month"),
           fill=list(rainfall=0))%>%
  group_by(monthYear,location)%>%
  summarise(rainfall = cumsum(rainfall))%>%
  summarise(rainfall = max(rainfall))%>%
  ungroup()%>%
  group_by(location)%>%
  mutate(totalRain = cumsum(rainfall))%>%
  ungroup()%>%
  group_by(monthYear)%>%
  mutate(Rank = order(totalRain, decreasing = T))%>%
  ungroup()

nosunCities<-model_cum%>%
  filter(monthYear== max(monthYear),
         Rank %in% 1:5)%>%
  dplyr::select(location)%>%unlist()

model_cum%>%
  filter(monthYear>ymd("2015-01-1"))%>%
  filter(location %in% nosunCities)%>%
  ggplot(aes(x = monthYear, y = Rank, color = as.factor(location)))+
  geom_point(size =5)+
  geom_bump(size = 2, smooth = 8,alpha = .95)+
  scale_color_sliced(palette = "Sequential")+
  scale_y_reverse()+
  labs(
    title = "Race to Rain!",
    subtitle = "Temporal Rankings of Current Top 5 Rained On Locations",
    y = "Ranking",
    x = "Time",
    color = "Location",
    caption = "Sale has just stayed wet for a couple years, 
    like that one gym towel.."
  )

