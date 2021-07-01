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
modelData<-as_tibble(read.csv("Season 2/sliced-s01e03/train.csv",header=T,stringsAsFactors=F,sep=","))
holdData<-as_tibble(read.csv("Season 2/sliced-s01e03/test.csv",header=T,stringsAsFactors=F,sep=","))


#### Lets look at the correlation Structure
corMat<-cor(modelData%>%select_if(is.numeric))%>%as.data.frame()%>%
  rownames_to_column()%>%arrange(desc(profit))

modelData%>%
  mutate(highProf = as.numeric(profit>median(profit)))%>%
  GGally::ggscatmat(columns = which(colnames(modelData)%in%corMat$rowname),
                    color = "highProf", alpha = .1, corMethod="spearman")+
  scale_color_sliced()
# looks like low discounts and high sales are important

#### Then we will look at the distributions of the inputs with the outcome
varzGraph<-c("profit","sales","quantity","discount")
tranz<-c("pseudo_log","log10","log10","identity")
gList<-lapply(1:4,function(i){
  
 modelData%>%
    ggplot(aes_string(x=varzGraph[i]))+
    geom_density()+
    scale_x_continuous(trans = tranz[i])
  
})

gList[[1]]+gList[[2]]+gList[[3]]+gList[[4]]

gList<-lapply(1:4,function(i){
  
  modelData%>%
    mutate_if(is.character, as.factor)%>%
    ggplot(aes_string(x=varzGraph[i],fill="segment"))+
    geom_density(alpha = .5)+
    scale_x_continuous(trans = tranz[i])+
    theme(legend.position = "")  
})

gList[[1]]+gList[[2]]+gList[[3]]+gList[[4]]+theme(legend.position ="right")+
  plot_annotation(
    title = 'We are all suckers!',
    subtitle = 'The effect of corporate deals on sales',
    caption = 'Notice how there are lower discounts for home office'
  )
# looks like more sales and discounts for corportate

#### Fun Questions
### Are certain categories more common in certain regions: Can we find Buffalo selling lots of tables?
modelData%>%
  group_by(region)%>%
  mutate(allsales = sum(sales))%>%
  ungroup()%>%
  group_by(category,region)%>%
  summarise(salesProp = sum(sales)/max(allsales))%>%
  ggplot(aes(y = category, x= region,fill = salesProp))+
  geom_tile()+
  scale_fill_sliced("Divergent",discrete = F)+
  labs(
  title = "Look at all those East Coast Nerds!",
  subtitle = "The impact of region on proportion of sales",
  caption = "Needs by region is an interesting thing",
  x = "Region of USA",
  y = "Category of Sales",
  fill = "Proportion of Total Sales"
  )
# lets try subcategories 
modelData %>%
  group_by(region)%>%
  mutate(allsales = sum(sales))%>%
  ungroup()%>%
  group_by(sub_category,region)%>%
  summarise(salesProp = sum(sales)/max(allsales))%>%
  ggplot(aes(x = sub_category, y= region,fill = salesProp))+
  geom_tile()+
  scale_fill_sliced("Divergent",discrete = F)+
  theme(legend.position = "bottom",axis.text.x = element_text(size = 10,angle = 45))+
  labs(
    title = "Apparently The East Doesn't Love Tables? Buffalo where you at?",
    subtitle = "The impact of region on proportion of sales of sub-categories",
    caption = "South hates copiers and bookcases.. is this a library thing?",
    y = "Region of USA",
    y = "Sub-Category of Sales",
    fill = "Proportion of Total Sales"
  )

##viz by location
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

### Discount rate by state, who is cheap? correlation with regions
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


### I want to look at the impact of discounts on sales/qunatity - is that a smart strategy
modelData%>%bind_rows(holdData)%>%
  ggplot(aes(x= discount, y=sales,color = quantity))+
  geom_jitter(alpha = .2)+
  geom_smooth()+
  geom_rug()+
  scale_color_sliced("Divergent",discrete = F)+
  scale_y_continuous(trans = "log10")+
  labs(
    title = "Sale! Sale! Sale!",
    subtitle = "The Impact of Discounts on Sales",
    caption = "Looking Left to right we can see the life-cycle of a product new to old, 
    where unwanted products in low stock are heavily discounted",
    fill = "Quantity Sold",
    y = "Number Of Sales",
    x= "Discount Rate"
  )


### Shipping types by segment and region. who gets the fancy shipping? 
modelData %>%
  group_by(region)%>%
  mutate(allsales = sum(sales))%>%
  ungroup()%>%
  group_by(,ship_mode,region)%>%
  summarise(salesProp = sum(sales)/max(allsales))%>%
  ggplot(aes(x = ship_mode, y= region,fill = salesProp))+
  geom_tile()+
  scale_fill_sliced("Divergent",discrete = F)+
  theme(legend.position = "bottom",axis.text.x = element_text(size = 10,angle = 45))+
  labs(
    title = "Central USA Ain't Fancy!",
    subtitle = "Regional Variation in Shipping Methods",
    caption = "The South has the lowest rates of standard shipping, is this heat related?",
    y = "Region of USA",
    x = "Mail Type",
    fill = "Proportion of Total Sales"
  )
