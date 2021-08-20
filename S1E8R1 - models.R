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
set.seed(1987) # Mid Thirties Never Felt So Young

all_cores <- parallel::detectCores()
registerDoParallel(cores=all_cores-2)
options(scipen=999)
source("SlicedTheme.R")
modelData<-as_tibble(read.csv("Season 2/sliced-s01e08/train.csv",header=T,stringsAsFactors=F,sep=","))
artistsData<-as_tibble(read.csv("Season 2/sliced-s01e08/artists.csv",header=T,stringsAsFactors=F,sep=","))

artistDataFunc<-function(i){
  artistID<-unique(
    (modelData[i,]%>%separate_rows(id_artists,sep = '\', \''))$id_artists%>%
      stringr::str_remove_all(., "[[:punct:]]")
  )
  artistsData%>%filter(id%in%artistID)%>%
    summarise(followers = sum(followers),
           genres = paste(
             genres,
             collapse = ","),
           popularity_avrg = mean(popularity)
    )
}

artistdataAttatch<-lapply(1:nrow(modelData),artistDataFunc)%>%do.call(bind_rows, .)
artistdataAttatch<-artistdataAttatch%>%rename(popularity_artist = popularity)
modelData<-bind_cols(modelData,artistdataAttatch)

holdData<-as_tibble(read.csv("Season 2/sliced-s01e08/test.csv",header=T,stringsAsFactors=F,sep=","))



skim(modelData)

# create remix flag > token names?
# minor/major key?
# temporal?

head(modelData$name,100)

modelData$remix<- grepl("remix",tolower(modelData$name))|grepl("rmx",tolower(modelData$name))
modelData$radio<- grepl("radio",tolower(modelData$name))
modelData$live<- grepl("live",tolower(modelData$name))
library("textcat")
modelData$language<-textcat(modelData$name)
modelData$language<-as.factor(coalesce(modelData$language,"Unknown"))

modelSplit<-initial_split(modelData)
modelTrain<-training(modelSplit)
modelTest<-testing(modelSplit)
modelFoldz<-vfold_cv(modelTrain,v=6,repeats=1)

rec_gb<-recipe(
  popularity  ~ ., 
  data = modelTrain)%>%
  step_rm(id, name, artists, id_artists,release_day)%>%
  step_mutate(key = as.factor(key),
              release_month = coalesce(release_month, 0)
              )%>%
  step_ns(release_year, deg_free   = 3)%>%
  step_ns(release_month, deg_free  = 4)%>%
  step_zv(all_predictors())%>%
  step_dummy(all_nominal_predictors())

ctrl_grid<-control_stack_grid() ## we stacking

catSpec<-boost_tree(trees = tune(), tree_depth = tune(),
                    learn_rate = tune(), mtry = tune())%>%
  set_engine("xgboost")%>%
  set_mode("regression")

catGrid<-grid_max_entropy(
  trees(range = c(200L, 1200L)),
  tree_depth(range = c(2L, 12L)),
  learn_rate(range = c(-3, -.05)),
  finalize(mtry(range = c(2L, unknown())), modelTrain),
  size = 50,
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

catWflow%>%finalize_workflow(parameters = select_best(catRes, "rmse"))%>%fit(modelTrain)%>%pull_workflow_fit() %>%vip()

collect_metrics(catRes)%>%arrange(mean,std_err)
autoplot(catRes)
