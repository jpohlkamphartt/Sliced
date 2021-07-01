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

all_cores <- parallel::detectCores()
registerDoParallel(cores=all_cores-2)
options(scipen=999)
source("SlicedTheme.R")

modelData<-as_tibble(read.csv("Season 2/sliced-s01e01/train.csv",header=T,stringsAsFactors=F,sep=","))

## plan
# cleanup - exploration
skimr::skim(modelData)
# plot response
p1<-ggplot(modelData,aes(x=geek_rating,fill=fct_lump(category1,7)))+
  geom_histogram(position = "dodge")+
  scale_fill_manual(values = Sliced_Qualitative_Palette)
p2<-ggplot(modelData%>%filter(year>1960),aes(x=year,y=geek_rating,color=fct_lump(category1,7)))+
  geom_point(alpha=.2)+
  scale_fill_manual(values = Sliced_Qualitative_Palette)
p1+p2
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
  dplyr::select(
    -contains("category"),
    -designer,
    -mechanic
    )%>%
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
    
# preprocess setup
modelSplit<-initial_split(modelData)
modelTrain<-training(modelSplit)
modelTest<-testing(modelSplit)
modelFolds<-vfold_cv(modelTrain, v=5, repeats = 1)
Rec<-recipe(geek_rating~.,data = modelTrain)%>%
  step_zv(all_numeric_predictors())%>%
  step_impute_median(all_numeric_predictors())%>%
  step_nzv(all_numeric_predictors(),freq_cut=499/1)%>%
  step_corr(all_numeric_predictors(),threshold = .95)%>%
  step_YeoJohnson(contains(c("players","time","num_votes","age")))

class(Rec)<-c("base",class(Rec))

preprocesses<-list(
  base = Rec
)
# model setup
## parametric = glm
glmSpec<-linear_reg(mode = "regression",mixture = tune(),penalty = tune()) %>%
  set_engine("glmnet")
  
## non-parametric = xgb
xgbSpec_g<-boost_tree(trees = tune(), min_n = tune(), tree_depth = tune())%>%
  set_engine("xgboost", objective = "reg:tweedie")%>%
  set_mode("regression")

xgbSpec_t<-boost_tree(trees = tune(), min_n = tune(), tree_depth = tune())%>%
  set_engine("xgboost")%>%
  set_mode("regression")

models<-list(
  glm = glmSpec,
  xgbT =xgbSpec_t,
  xgbG =xgbSpec_g
)
# test models
tictoc::tic()
wfloz<-workflow_set(preprocesses,models,cross=T)%>%
  workflow_map(
    resamples = modelFolds,
    grid = 5,
    metrics = metric_set(rmse),
    seed =1,
    verbose = T
  )
tictoc::toc()    

rank_results(wfloz)
autoplot(wfloz)

modelV1bst<-select_best(pull_workflow_set_result(wfloz,"base_xgbG"),metric = "rmse")
modelV1mod<-finalize_workflow(pull_workflow(wfloz,"base_xgbG"),modelV1bst)
modelV1<-fit(modelV1mod,modelTrain)
finalFit<-modelV1
saveRDS(modelV1,"firstmodel.rds")

# tune final model
finalGrid<-grid_latin_hypercube(
  trees(),
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size= sample_prop(),
  finalize(mtry(),modelTrain),
  learn_rate(),
  size = 100
)

xgbSpec<-boost_tree(
  trees=tune(),
  tree_depth=tune(),
  min_n=tune(),
  loss_reduction=tune(),
  sample_size=tune(),
  mtry=tune(),
  learn_rate=tune()
)%>%
  set_engine("xgboost")%>%
  set_mode("regression")

finalWflow<-workflow()%>%
  add_recipe(Rec)%>%
  add_model(xgbSpec)
tictoc::tic()

finalRes<-tune_grid(
  finalWflow,
  resamples = modelFolds,
  grid = finalGrid,
  metrics = metric_set(rmse)
)
tictoc::toc()

collect_metrics(finalRes)%>%relocate(mean)%>% arrange(mean)

finalModBest<-select_best(finalRes,metric = "rmse")
finalMod<-finalize_workflow(finalWflow,finalModBest)
finalFit<-fit(finalMod,modelTrain)

finalFit%>%pull_workflow_fit()%>%vip(geom = "point",num_features = 20)

shap<-predict(xgb.Booster.complete(pull_workflow_fit(finalFit)$fit),
              newdata=modelTrain%>%dplyr::select(
                (xgb.Booster.complete(pull_workflow_fit(finalFit)$fit))$feature_names)%>%as.matrix(),
              predcontrib=T)

shap<-as_tibble(shap)
attr(shap,which="baseline")<-shap[["BIAS"]]
shap[["BIAS"]]<-NULL
class(shap)<-c(class(shap),"explain")

autoplot(shap,num_features =20)
autoplot(shap,type="dependence",feature = "num_votes",
         X=modelTrain%>%dplyr::select(
           (xgb.Booster.complete(pull_workflow_fit(finalFit)$fit))$feature_names),
         smooth=T,color_by="Economic")

### holdout code
holdData<-as_tibble(read.csv("Season 2/sliced-s01e01/test.csv",header=T,stringsAsFactors=F,sep=","))

## plan
# cleanup - exploration
skimr::skim(holdData)

# remove: game_id, names
holdData<-holdData%>%
  dplyr::select(
     -names
  )
# fix shitespace issues on character strings
holdData$category<-apply( holdData%>%dplyr::select(contains("category")) , 1 , paste , collapse = "," )
# make comma seperated mechanic/category, designer column into 1-hots
holdData[,designers]<-lapply(designers,function(d){
  grepl(d,holdData$designer)
})%>%do.call(cbind,.)
#mechanic
holdData[,mechanics]<-lapply(mechanics,function(d){
  grepl(d,holdData$mechanic)
})%>%do.call(cbind,.)
# turn categoryX into 1-hots
holdData[,categories]<-lapply(categories,function(d){
  grepl(d,holdData$category)
})%>%do.call(cbind,.)
holdData<-holdData%>%
  dplyr::select(
    -contains("category"),
    -designer,
    -mechanic
  )%>%
  # fix years that make no sense
  mutate(year = ifelse(year<1600,NA,year))

# feature eng
holdData<-holdData%>%
  mutate(
    range_players = max_players-min_players,
    range_time = max_time-min_time,
    adult = age>=18
  )%>%
  mutate_if(is.logical,as.numeric)

holdData$geek_rating<-predict(finalFit,new_data = holdData)$.pred

holdData%>%
  ggplot(aes(x=geek_rating))+
  geom_histogram()

write_csv(holdData%>%dplyr::select(game_id,geek_rating),"submission_jph.csv")
