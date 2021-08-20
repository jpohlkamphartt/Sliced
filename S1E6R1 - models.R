#### Libraries ####
# library(lightgbm)
library(tidyverse)
library(tidymodels)
library(recipes)
library(themis)
library(vip)
library(ggplot2)
# library(furrr)
library(here)
library(lubridate)
# library(mgcv)
library(parallel)
# library(workflowsets)
# library(tidyposterior)
library(discrim)
library(hablar)
library(doParallel)
library(patchwork)
library(textrecipes)
# library(extrafont)
# library(fastshap)
library(skimr)
library(beepr)
# library(tayloRswift)
library(treesnip)
library(stacks)
# library(tictoc)
#### ####
options(tidymodels.dark = TRUE)
here()
usethis::edit_r_environ()
set.seed(1) # I am number one - Nelly

all_cores <- parallel::detectCores()
registerDoParallel(cores=all_cores-2)
options(scipen=999)
source("SlicedTheme.R")
modelData<-as_tibble(read.csv("Season 2/sliced-s01e06/train.csv",header=T,stringsAsFactors=F,sep=","))
holdData<-as_tibble(read.csv("Season 2/sliced-s01e06/test.csv",header=T,stringsAsFactors=F,sep=","))

# there are 15 games that are not in the holdout, we should be able to use lag data for an ARMA model by game

skim(modelData)

fortTS<-modelData%>%
  filter(Game =="World of Warcraft")%>%
  mutate(date = ym(paste(Year,Month,sep   = "-")))%>%
  dplyr::select(date, Hours_watched)

ggplot(fortTS,aes(x=date,y = Hours_watched))+
  geom_point()+
  geom_line()+
  scale_y_reverse()

modelDataTS<-ts(fortTS$Hours_watched,start = c(2019,02,01), frequency = 12)
plot(decompose(modelDataTS))
acf_ts<-acf(modelDataTS,lag.max =15)
pacf_ts<-pacf(modelDataTS,lag.max =15)

# lag 1 is useful, might be worth using 1,2,6,12
# for new games, we will have to have a default Hours_watched on lag = 500?

modelData<-modelData%>%
  mutate(date = ym(paste(Year,Month,sep   = "-")))%>%
  arrange(date)%>%
  group_by(Game)%>%
  mutate(
    Hours_watched_L1 = coalesce(lag(Hours_watched,1),-1000),
    Hours_watched_L2 = coalesce(lag(Hours_watched,2),-1000),
    Hours_watched_L3 = coalesce(lag(Hours_watched,3),-1000),
    Hours_watched_L4 = coalesce(lag(Hours_watched,4),-1000),
    Hours_watched_L6 = coalesce(lag(Hours_watched,6),-1000),
    Hours_watched_L12 = coalesce(lag(Hours_watched,12),-1000)
  )%>%ungroup()%>%
  mutate(
    Streamers = ifelse(Streamers==0,1,Streamers),
    Hours_Streamed_Per = Hours_Streamed/Streamers,
    viewPchan = Peak_viewers/Peak_channels 
  )
# could do well to transform all numerics to a more normal looking shape


modelSplit<-initial_split(modelData)
modelTrain<-training(modelSplit)
modelTest<-testing(modelSplit)
modelFoldz<-vfold_cv(modelTrain,v=6,repeats=1)

rec_gb<-recipe(
  Hours_watched ~ ., data = modelTrain)%>%
  step_rm(date,Game,Rank)%>%
  step_ns(Month,deg_free = 4)%>%
  step_ns(Year,deg_free = 3)%>%
  # step_log(Hours_Streamed,Peak_viewers,Peak_channels,Streamers,Avg_viewer_ratio,Hours_Streamed_Per )%>%
  # step_novel(all_nominal_predictors()) %>%
  # step_other(all_nominal_predictors(), threshold = .1) %>%
  # step_unknown(all_nominal_predictors()) %>%
  step_zv(all_predictors())
  # step_dummy(all_nominal_predictors(),one_hot=T)%>%
  

ctrl_grid<-control_stack_grid() ## we stacking

catSpec<-boost_tree(trees = tune(), tree_depth = tune(),
                    learn_rate = tune(), mtry = tune())%>%
  set_engine("xgboost")%>%
  set_mode("regression")

catGrid<-grid_max_entropy(
  trees(range = c(500L, 1500L)),
  tree_depth(range = c(3L, 12L)),
  learn_rate(range = c(-2, -.05)),
  finalize(mtry(range = c(2L, unknown())), modelTrain),
  size = 20,
  variogram_range = .75
)

catWflow<-workflow()%>%
  add_recipe(rec_gb)%>%
  add_model(catSpec)

catRes<-tune_grid(
  catWflow,
  resamples = modelFoldz,
  grid = catGrid,
  metrics = metric_set(rmse),
  control = ctrl_grid
)

catV1<-filter_parameters(catRes,parameters = select_best(catRes, "rmse"))

catWflow%>%finalize_workflow(parameters = select_best(catRes, "rmse"))%>%fit(modelTrain)%>%pull_workflow_fit() %>%vip()

collect_metrics(catRes)%>%arrange(mean,std_err)
autoplot(catRes)

rec_lm<-recipe(  Hours_watched ~ ., data = modelTrain)%>%
  step_mutate(Hours_watched_L1= ifelse(Hours_watched_L1==-1000, NA,Hours_watched_L1),
              Hours_watched_L2=  ifelse(Hours_watched_L2==-1000, NA,Hours_watched_L2),
              Hours_watched_L6= ifelse(Hours_watched_L6==-1000, NA,Hours_watched_L6),
              Hours_watched_L12= ifelse(Hours_watched_L12==-1000, NA,Hours_watched_L12))%>%
  step_rm(date)%>%
  step_ns(Month,deg_free = 4)%>%
  step_ns(Year,deg_free = 3)%>%
  step_novel(all_nominal_predictors()) %>%
  step_other(all_nominal_predictors(), threshold = .3) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_impute_bag(all_numeric_predictors(),impute_with = imp_vars(Game))%>%
  step_BoxCox(Hours_Streamed,Peak_viewers,Peak_channels,Streamers,Avg_viewer_ratio,Hours_Streamed_Per )%>%
  step_zv(all_predictors())%>%
  step_dummy(all_nominal_predictors(),one_hot=T)

glmSpec<-linear_reg(mixture = tune(), penalty = tune())%>%
  set_engine("glmnet")

glmGrid<-grid_max_entropy(
  mixture(),
  penalty(),
  # threshold(range = c(.01,.5)),
  size = 10,
  variogram_range = .7
)

glmWflow<-workflow()%>%
  add_recipe(rec_lm)%>%
  add_model(glmSpec)

glmRes<-tune_grid(
  glmWflow,
  resamples = modelFoldz,
  grid = glmGrid,
  metrics = metric_set(rmse),
  control = ctrl_grid
)
beepr::beep("fanfare")

glmWflow%>%finalize_workflow(parameters = select_best(glmRes, "rmse"))%>%fit(modelTrain)%>%pull_workflow_fit() %>%vip()

collect_metrics(glmRes)%>%filter(.metric =="rmse")%>%arrange(mean,std_err)
autoplot(glmRes)

glmV1<-filter_parameters(glmRes,parameters = select_best(glmRes, "rmse"))

rec_knn<-recipe(  Hours_watched ~ ., data = modelTrain)%>%
  step_mutate(Hours_watched_L1= ifelse(Hours_watched_L1==-1000, NA,Hours_watched_L1),
              Hours_watched_L2=  ifelse(Hours_watched_L2==-1000, NA,Hours_watched_L2),
              Hours_watched_L6= ifelse(Hours_watched_L6==-1000, NA,Hours_watched_L6),
              Hours_watched_L12= ifelse(Hours_watched_L12==-1000, NA,Hours_watched_L12))%>%
  step_rm(date)%>%
  step_ns(Month,deg_free = 4)%>%
  step_ns(Year,deg_free = 3)%>%
  step_impute_bag(all_numeric_predictors(),impute_with = imp_vars(Game))%>%
  step_BoxCox(Hours_Streamed,Peak_viewers,Peak_channels,Streamers,Avg_viewer_ratio,Hours_Streamed_Per )%>%
  step_novel(all_nominal_predictors())%>%
  step_other(all_nominal_predictors(), threshold = tune())%>%
  step_unknown(all_nominal_predictors())%>%
  step_zv(all_numeric_predictors())%>%
  step_dummy(all_nominal_predictors())

knnSpec<-nearest_neighbor(neighbors = tune(), weight_func = tune())%>%
  set_engine("kknn")%>%
  set_mode("regression")

knnGrid<-grid_max_entropy(
  neighbors(range= c(4L,8L)),
  weight_func(c("optimal","rectangular")),
  threshold(range = c(.01,.5)),
  size = 10,
  variogram_range = .7
)

knnWflow<-workflow()%>%
  add_recipe(rec_knn)%>%
  add_model(knnSpec)

knnRes<-tune_grid(
  knnWflow,
  resamples = modelFoldz,
  grid = knnGrid,
  metrics = metric_set(rmse),
  control = ctrl_grid
)

beepr::beep("fanfare")

collect_metrics(knnRes)%>%filter(.metric =="rmse")%>%arrange(mean,std_err)
autoplot(knnRes)

knnV1<-filter_parameters(knnRes,parameters = select_best(knnRes, "rmse"))

model_st<-
  stacks()%>%
  add_candidates(catRes)%>%
  # add_candidates(glmRes)%>%
  # add_candidates(knnRes)%>%
  blend_predictions(
    metric = metric_set(rmse),
    penalty = 10^(-6:-1), mixture = (0:5)/5)%>%
  fit_members()
autoplot(model_st, type = "weights")
autoplot(model_st)
autoplot(model_st, type = "members")

#### hold out
holdData<-as_tibble(read.csv("Season 2/sliced-s01e06/test.csv",header=T,stringsAsFactors=F,sep=","))

holdData<-holdData%>%
  mutate(hold = T)%>%
  bind_rows(modelData%>%mutate(hold = F))%>%
  mutate(date = ym(paste(Year,Month,sep   = "-")))%>%
  arrange(date)%>%
  group_by(Game)%>%
  mutate(
    Hours_watched_L1 = coalesce(lag(Hours_watched,1),-1000),
    Hours_watched_L2 = coalesce(lag(Hours_watched,2),-1000),
    Hours_watched_L6 = coalesce(lag(Hours_watched,6),-1000),
    Hours_watched_L12 = coalesce(lag(Hours_watched,12),-1000)
  )%>%ungroup()%>%
  mutate(
    Streamers = ifelse(Streamers==0,1,Streamers),
    Hours_Streamed_Per = Hours_Streamed/Streamers,
    viewPchan = Peak_viewers/Peak_channels 
  )%>%
  filter(hold ==T)

holdData<-distinct(holdData)

predz<-predict(model_st,
               new_data = holdData,
               type = "numeric"
)


holdData$Hours_watched<-predz$.pred

holdData<-holdData%>%
  arrange(desc(Hours_watched))%>%
  mutate(Rank = row_number())

hist(holdData$Rank)

write_csv(holdData%>%dplyr::select(Game,Rank), "S1E6sub2.csv")

