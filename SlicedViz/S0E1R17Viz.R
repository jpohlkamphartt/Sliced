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
modelData<-as_tibble(read.csv("Season 2/sliced-s01e07/train.csv",header=T,stringsAsFactors=F,sep=","))

skim(modelData)

# age vs credit limit
modelData%>%
  group_by(customer_age)%>%
  summarise(credit_limit = median(credit_limit),
            gender = mean(gender =="F"))%>%
  filter(customer_age<65)%>%
  ggplot(aes(x = customer_age, y = credit_limit, fill = gender))+
  geom_bar(width = 1, stat = "identity", color = "white")+
  scale_fill_sliced("Divergent",discrete = F)+
  geom_smooth( se = F)+
  scale_color_sliced()+
  labs(
    title = "Midlife Crisis, more like Midlife Credit!",
    subtitle = "Age Curve for Credit Limit",
    x = "Age",
    y = "Credit Limit",
    color = "% Female"
  )

# months inactive vs attrition
modelData%>%
  group_by(months_inactive_12_mon)%>%
  summarise(attrition = sum(attrition_flag)/n(),
            n = n())%>%
  ggplot(aes(x = as.factor(months_inactive_12_mon), y = attrition, fill = n))+
  geom_bar(stat="identity")+
  scale_fill_sliced("Divergent",discrete = F)
  
  

modelData$cluster<-kmeans(modelData%>%
         dplyr::select(customer_age,
                       credit_limit,
                       income_category,
                       education_level,
                       gender
                       )%>%
         mutate(gender = gender =="F")%>%
         mutate_if(is.character, as.factor)%>%
         mutate_if(is.factor, as.numeric)
         ,
       centers = 3)$cluster
# total transactions
modelData%>%
  ggplot(aes(x = total_trans_amt, y = total_trans_ct, color = as.factor(cluster)))+
  geom_point(alpha = .5)+
  scale_color_sliced()+
  geom_smooth()+
  labs(title = "Big Money + Low Count = Outie!",
       subtitle = "Varying Transaction Behaviours For Attrition Detection",
       caption = "I guess if you are rich enough you never close the account..
       wish I was a baller...",
       x = "Total Transaction Value",
       y = "Total Transactions"
  )
  

# balance vs limit
modelData%>%
  mutate(total_revolving_bal_group = cut(total_revolving_bal, seq(min(total_revolving_bal), max(total_revolving_bal) + 10, 10), right = FALSE,labels = FALSE))%>%
  group_by(total_revolving_bal_group,gender)%>%
  summarise(credit_limit = median(credit_limit),
            total_revolving_bal = mean(total_revolving_bal))%>%
  filter(total_revolving_bal>5, total_revolving_bal<2500)%>%
  ggplot(aes(x = total_revolving_bal, y = credit_limit, color = as.factor(gender)))+
  geom_point(alpha = .15)+
  geom_smooth()+
  scale_color_sliced()

modelData%>%
  group_by(customer_age, income_category, education_level)%>%
  summarise(credit_limit = median(credit_limit))%>%
  filter(customer_age<65)%>%
  ggplot(aes(x = customer_age, y = credit_limit))+
  geom_bar(width = 1, stat = "identity", color = "white")+
  scale_fill_sliced("Divergent",discrete = F)+
  geom_smooth( se = F)+
  scale_color_sliced()+
  labs(
    title = "Midlife Crisis, more like Midlife Credit!",
    subtitle = "Age Curve for Credit Limit",
    x = "Age",
    y = "Credit Limit",
    color = "% Female"
  )+
  facet_grid(income_category ~ education_level)

# balance vs gender
modelData%>%
  mutate(income_category = factor(income_category,
                                  levels = c("Unknown","Less than $40K","$40K - $60K","$60K - $80K", "$80K - $120K","$120K +")))%>%
  group_by(income_category,gender)%>%
  summarise(total_revolving_bal = median(total_revolving_bal))%>%
  ggplot(aes(y = total_revolving_bal, x = income_category, fill = as.factor(gender)))+
  geom_bar(stat="identity", position = "dodge")+
  scale_fill_sliced()

# education vs gender vs credit limit, bias?
modelData%>%
  group_by(education_level,gender)%>%
  summarise(total_revolving_bal = median(total_revolving_bal))%>%
  ggplot(aes(y = total_revolving_bal, x = education_level, fill = as.factor(gender)))+
  geom_bar(stat="identity", position = "dodge")+
  scale_fill_sliced()

modelData%>%
  bind_rows(as_tibble(read.csv("Season 2/sliced-s01e07/test.csv",header=T,stringsAsFactors=F,sep=",")))%>%
  mutate(income_category = factor(income_category,
                                  levels = c("Unknown","Less than $40K","$40K - $60K","$60K - $80K", "$80K - $120K","$120K +")),
         education_level = factor(education_level,
                                  levels = c("Unknown" ,"Uneducated","High School","College","Graduate","Post-Graduate", "Doctorate") ))%>%
  group_by(education_level,income_category)%>%
  summarise(gender = mean(gender =="F"))%>%
  ggplot(aes(y = income_category, x = education_level, fill = gender))+
  geom_tile()+
  scale_fill_sliced(palette = "Divergent",discrete = F)

# utilization?