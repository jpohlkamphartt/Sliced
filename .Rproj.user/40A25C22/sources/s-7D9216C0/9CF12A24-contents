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
options(tidymodels.dark = TRUE)
here()
usethis::edit_r_environ()
set.seed(1)

all_cores <- parallel::detectCores()
registerDoParallel(cores=all_cores-2)

options(scipen=999)
commaRemove<-function(v){as.numeric(gsub(",","",v))}
naFixer<-function(v){ifelse(v=="",NA, v)}
source("SlicedTheme.R")
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

commaRemove<-function(v){as.numeric(gsub(",","",v))}
naFixer<-function(v){ifelse(v=="",NA, v)}

modelData<-as_tibble(read.csv("Season1/s00e03/Copy of sliced_data.csv",header=T,stringsAsFactors=F,sep=","))

# Plan
# data cleanup
# model if volitile
# feature engineering: temporal, game title similarity, ?
# model direction
# viz

skimr::skim(modelData)


modelData<-modelData%>%
  dplyr::select(# remove duplicate variables
    -month,
    -month_num,
    -year
    )%>%
  mutate(# fix dates/character columns
    avg_peak_perc = as.numeric(stringr::str_replace_all(avg_peak_perc,"%",""))/100,
    date = ymd(yearmonth))%>%
  dplyr::select(-yearmonth)

corMat<-cor(modelData%>%select_if(is.numeric), use = "pairwise.complete.obs")%>%
  as.data.frame()%>%rownames_to_column()

modelData%>%
  GGally::ggscatmat(columns = which(colnames(modelData)%in%corMat$rowname),color = "volatile",alpha = .2, corMethod = "spearman")+
  tayloRswift::scale_color_taylor("lover") # lover = best album cover

table(modelData$volatile!=0) # not too imbalanced

modelData<-modelData%>%group_by(gamename)%>%
  mutate(gain_1 = lag(gain,1),
         # gain_2 = lag(gain,2),
         # gain_3 = lag(gain,3),
         gain_4 = lag(gain,4),
         # gain_5 = lag(gain,5),
         gain_6 = lag(gain,6),
         # gain_7 = lag(gain,7),
         # gain_8 = lag(gain,8),
         # gain_9 = lag(gain,9),
         # gain_10 = lag(gain,10),
         # gain_11 = lag(gain,11),
         gain_12 = lag(gain,12),
         avg_peak_perc_1 = lag(avg_peak_perc,1),
         # avg_peak_perc_2 = lag(avg_peak_perc,2),
         # avg_peak_perc_3 = lag(avg_peak_perc,3),
         avg_peak_perc_4 = lag(avg_peak_perc,4),
         # avg_peak_perc_5 = lag(avg_peak_perc,5),
         avg_peak_perc_6 = lag(avg_peak_perc,6),
         # avg_peak_perc_7 = lag(avg_peak_perc,7),
         # avg_peak_perc_8 = lag(avg_peak_perc,8),
         # avg_peak_perc_9 = lag(avg_peak_perc,9),
         # avg_peak_perc_10 = lag(avg_peak_perc,10),
         # avg_peak_perc_11 = lag(avg_peak_perc,11),
         avg_peak_perc_12 = lag(avg_peak_perc,12)
  )%>%ungroup()%>%
  arrange(desc(avg))

modelData_vmod<-modelData%>%
  mutate(plusBool=as.factor(volatile!=0),month = month(date),year = year(date))%>%
  dplyr::select(-date,-gamename,-volatile)

modelData_vmod_split<-initial_split(modelData_vmod)
modelData_vmod_train<-training(modelData_vmod_split)
modelData_vmod_test<-testing(modelData_vmod_split)

modelData_vmod_folds<-vfold_cv(modelData_vmod_train,v=5,repeats = 1)

skimr::skim(modelData_vmod)

table(modelData_vmod$gain==0)

# try a couple different data transformations
# normalize numerics
bsRec<-recipe(volBool~.,data=modelData_vmod_train)%>%
  step_normalize(all_numeric_predictors(),-month,-year)%>% 
  step_impute_median(all_numeric())#%>%
  # step_smote(volBool)
class(bsRec)<-c("base",class(bsRec))

# box cox the skewed numerics
coxRec<-recipe(volBool~.,data=modelData_vmod_train)%>%
  step_BoxCox(all_numeric_predictors(),-month,-year)%>% 
  step_impute_median(all_numeric())%>%
  step_downsample()
#%>%
# step_smote(volBool)
class(coxRec)<-c("cox",class(coxRec))

preproceses<-list(
  # base = bsRec,
  cox = coxRec
)

# try a few models
# penalized regression
glm_spec<-logistic_reg(penalty = tune(),mixture = tune())%>%set_engine("glmnet")
# MARS
mars_spec<-discrim_flexible(prod_degree = tune())%>%set_engine("earth")
# xgBOOOOOOOOst
xg_spec<-boost_tree(trees = tune(), min_n = tune(), tree_depth = tune())%>%set_engine("xgboost")%>%set_mode("classification")

modelz<-list(
 glm= glm_spec,
 mars= mars_spec,
 xg= xg_spec
)

wflow<-workflow_set(preproceses,modelz,cross = T)
wflow<-wflow%>%
  workflow_map(
    resamples = modelData_vmod_folds,
    grid = 5,
    metrics = metric_set(accuracy, mn_log_loss, roc_auc),
    seed = 1, # cause I am only aiming for the podium
    verbose = T
  )
    
rank_results(wflow)%>%
  relocate(rank,mean)

autoplot(wflow)

finalModel_vmod_best<-select_best(pull_workflow_set_result(wflow,"cox_xg"),metric = "accuracy")
finalModel_vmod_mod<-finalize_workflow(pull_workflow(wflow,"cox_xg"),finalModel_vmod_best)
finalModel_vmod_fit<-fit(finalModel_vmod_mod,modelData_vmod_train)

#### model for once determined to be volatile to select direction ####
modelData_vdir<-modelData%>%
  filter(volatile!=0)%>%
  mutate(plusBool=as.factor(volatile>0))%>%
  dplyr::select(-date,-gamename,-volatile)

modelData_vdir_split<-initial_split(modelData_vdir)
modelData_vdir_train<-training(modelData_vdir_split)
modelData_vdir_test<-testing(modelData_vdir_split)

modelData_vdir_folds<-vfold_cv(modelData_vdir_train,repeats = 2)

skimr::skim(modelData_vdir)

table(modelData_vdir$plusBool)

# try a couple different data transformations
# normalize numerics
bsRec<-recipe(plusBool~.,data=modelData_vdir_train)%>%
  step_normalize(all_numeric_predictors())%>% 
  step_impute_median(all_numeric())#%>%
# step_smote(plusBool)
class(bsRec)<-c("base",class(bsRec))

# box cox the skewed numerics
coxRec<-recipe(plusBool~.,data=modelData_vdir_train)%>%
  step_BoxCox(all_numeric_predictors())%>% 
  step_impute_median(all_numeric())#%>%
# step_smote(plusBool)
class(coxRec)<-c("cox",class(coxRec))

preproceses<-list(
  # base = bsRec,
  cox = coxRec
)

# try a few models
# penalized regression
glm_spec<-logistic_reg(penalty = tune(),mixture = tune())%>%set_engine("glmnet")
# MARS
mars_spec<-discrim_flexible(prod_degree = tune())%>%set_engine("earth")
# xgBOOOOOOOOst
xg_spec<-boost_tree(trees = tune(), min_n = tune(), tree_depth = tune())%>%set_engine("xgboost")%>%set_mode("classification")

modelz<-list(
  glm= glm_spec,
  mars= mars_spec,
  xg= xg_spec
)

wflow<-workflow_set(preproceses,modelz,cross = T)
wflow<-wflow%>%
  workflow_map(
    resamples = modelData_vdir_folds,
    grid = 5,
    metrics = metric_set(accuracy, mn_log_loss, roc_auc),
    seed = 1, # cause I am only aiming for the podium
    verbose = T
  )

rank_results(wflow)%>%
  relocate(rank,mean)

autoplot(wflow)

finalModel_vdir_best<-select_best(pull_workflow_set_result(wflow,"cox_xg"),metric = "accuracy")
finalModel_vdir_mod<-finalize_workflow(pull_workflow(wflow,"cox_xg"),finalModel_vdir_best)
finalModel_vdir_fit<-fit(finalModel_vdir_mod,modelData_vdir_train)

## holdout code
holdData<-as_tibble(read.csv("Season1/s00e03/Copy of sliced_holdout_data.csv",header=T,stringsAsFactors=F,sep=","))

holdData<-holdData%>%
  dplyr::select(# remove duplicate variables
    -month,
    -month_num,
    -year
  )%>%
  mutate(# fix dates/character columns
    avg_peak_perc = as.numeric(stringr::str_replace_all(avg_peak_perc,"%",""))/100,
    date = ymd(yearmonth))%>%
  dplyr::select(-yearmonth)

holdData<-holdData%>%group_by(gamename)%>%
  mutate(gain_1 = lag(gain,1),
         # gain_2 = lag(gain,2),
         # gain_3 = lag(gain,3),
         gain_4 = lag(gain,4),
         # gain_5 = lag(gain,5),
         gain_6 = lag(gain,6),
         # gain_7 = lag(gain,7),
         # gain_8 = lag(gain,8),
         # gain_9 = lag(gain,9),
         # gain_10 = lag(gain,10),
         # gain_11 = lag(gain,11),
         gain_12 = lag(gain,12),
         avg_peak_perc_1 = lag(avg_peak_perc,1),
         # avg_peak_perc_2 = lag(avg_peak_perc,2),
         # avg_peak_perc_3 = lag(avg_peak_perc,3),
         avg_peak_perc_4 = lag(avg_peak_perc,4),
         # avg_peak_perc_5 = lag(avg_peak_perc,5),
         avg_peak_perc_6 = lag(avg_peak_perc,6),
         # avg_peak_perc_7 = lag(avg_peak_perc,7),
         # avg_peak_perc_8 = lag(avg_peak_perc,8),
         # avg_peak_perc_9 = lag(avg_peak_perc,9),
         # avg_peak_perc_10 = lag(avg_peak_perc,10),
         # avg_peak_perc_11 = lag(avg_peak_perc,11),
         avg_peak_perc_12 = lag(avg_peak_perc,12)
  )%>%ungroup()%>%
  arrange(desc(avg))%>%
  mutate(month = month(date),year = year(date))

holdData$isVol<-predict(finalModel_vmod_fit,new_data = holdData)
holdData$volDir<-predict(finalModel_vdir_fit,new_data = holdData)
holdData$predVol<-as.logical(holdData$isVol$.pred_class)*ifelse(as.logical(holdData$volDir$.pred_class),1,-1)
