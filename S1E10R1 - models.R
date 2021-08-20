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
modelData<-as_tibble(read.csv("Season 2/sliced-s01e10/train.csv",header=T,stringsAsFactors=F,sep=","))
holdData<-as_tibble(read.csv("Season 2/sliced-s01e10/test.csv",header=T,stringsAsFactors=F,sep=","))

skim(modelData)

age_inDays<-function(age){
 
  if(grepl("month",age)){
   
    as.numeric(stringr::str_match(age,"\\d"))*30

  } else if(grepl("year",age)){
    as.numeric(stringr::str_match(age,"\\d"))*365
  }  else if(grepl("day",age)){
    as.numeric(stringr::str_match(age,"\\d"))
  }  else if(grepl("week",age)){
    as.numeric(stringr::str_match(age,"\\d"))*7
  } else {
    
    NA
  } 
    
    
  
}

age_inDaysDF<-function(v){
  unlist(lapply(v,age_inDays))
}

modelData$age_upon_outcome<-age_inDaysDF(modelData$age_upon_outcome)

modelData$moring<-am(ymd_hms(modelData$datetime))
modelData$dayofWeek<-wday(ymd_hms(modelData$datetime))
modelData$month<-month(ymd_hms(modelData$datetime))
modelData$year<-year(ymd_hms(modelData$datetime))

modelData<-modelData%>%
  mutate(breed = tolower(breed),
         animal_type = tolower(animal_type)
  )

modelData$astrix<-grepl("\\*",modelData$name)
modelData$noName<-is.na(modelData$name)
modelData$mix<-grepl("mix",modelData$breed)

animalExtractor<-function(breed){
  trimws(str_remove(str_remove(breed,"mix"),"\\s"))
}
animalExtractordf<-function(v){
 unlist(lapply(v, animalExtractor))
}

modelData$animal_type2<-modelData$animal_type
modelData$animal_type2[!modelData$animal_type2%in%c("dog","cat")]<-
  animalExtractordf(modelData$breed[!modelData$animal_type2%in%c("dog","cat")])

modelData$tabby<-grepl("tabby",modelData$breed)

breedz<-na.omit(unique((modelData%>%separate_rows(breed,sep="/"))$breed))

modelData[,breedz]<-lapply(breedz,function(brs){
  
  as.numeric(grepl(brs,modelData$breed))
  
})%>%do.call(cbind,.)

modelSplit<-initial_split(modelData,strata = animal_type)
modelTrain<-training(modelSplit)
modelTest<-testing(modelSplit)
modelFoldz<-vfold_cv(modelTrain, v = 5, repeats = 1)

modelData%>%dplyr::select(-breedz)%>%skim()

recglm<-recipe(outcome_type~., data = modelTrain)%>%
  step_rm(name, id,date_of_birth, datetime,animal_type,breed,breedz)%>%
  step_mutate(dayofWeek = as.factor(dayofWeek))%>%
  step_mutate(moring = as.numeric(moring),
              astrix = as.numeric(astrix),
              noName = as.numeric(noName),
              mix    = as.numeric(mix   ))%>%
  step_ns(month, deg_free = 4)%>%
  step_ns(year, deg_free = 3)%>%
  step_impute_bag(age_upon_outcome, impute_with = imp_vars(animal_type2))%>%
  step_novel(all_nominal_predictors())%>%
  step_other(all_nominal_predictors(),threshold = .05)%>%
  step_unknown(all_nominal_predictors())%>%
  step_zv(all_predictors())%>%
  step_nzv(all_predictors(),freq_cut = 99/1)%>%
  step_dummy(all_nominal_predictors())%>%
  step_downsample(outcome_type)

recgb<-recipe(outcome_type~., data = modelTrain)%>%
  step_rm(name, id,date_of_birth, datetime,animal_type, breedz,color,breed,spay_neuter)%>%
  step_mutate(dayofWeek = as.numeric(dayofWeek%in%c(1,7)))%>%
  step_mutate(moring = as.numeric(moring),
              astrix = as.numeric(astrix),
              noName = as.numeric(noName),
              mix    = as.numeric(mix   ))%>%
  # step_ns(month, deg_free = 4)%>%
  # step_ns(year, deg_free = 3)%>%
  step_impute_bag(age_upon_outcome, impute_with = imp_vars(animal_type2))%>%
  step_novel(all_nominal_predictors())%>%
  step_other(all_nominal_predictors(),threshold = .01)%>%
  step_unknown(all_nominal_predictors())%>%
  step_zv(all_predictors())%>%
  step_dummy(all_nominal_predictors())%>%
  step_downsample(outcome_type)
  
ctrl_grid<-control_stack_grid()

glmSpec<-multinom_reg(mixture = tune(), penalty = tune())%>%
  set_engine("glmnet")%>%
  set_mode("classification")

glmGrid<- grid_max_entropy(
  mixture(),
  penalty(),
  size = 6,
  variogram_range = .75
)
glmWflow<-workflow()%>%
  add_recipe(recglm)%>%
  add_model(glmSpec)
glmRes<-tune_grid(
  glmWflow,
  resamples = modelFoldz,
  grid = glmGrid,
  metrics = metric_set(mn_log_loss),
  control = ctrl_grid
)

beepr::beep("fanfare")

glmWflow%>%finalize_workflow(parameters = select_best(glmRes, "mn_log_loss"))%>%
  fit(modelTrain)%>%vip()

gbSpec<-boost_tree(trees = tune(),tree_depth = tune(), learn_rate = tune(), mtry = tune())%>%
  set_engine("xgboost", objective = "multi:softprob")%>%
  set_mode("classification")

gbGrid<-grid_max_entropy(
  trees(range = c(250,1200)),
  tree_depth(range = c(2,12)),
  learn_rate(range = c(-3,-.05)),
  finalize(mtry(range = c(2, unknown())),modelTrain),
  size = 20,
  variogram_range = .75
)

gbWflow<-workflow()%>%
  add_recipe(recgb)%>%
  add_model(gbSpec)

gbRes<-tune_grid(
  gbWflow,
  resamples = modelFoldz,
  grid = gbGrid,
  metrics = metric_set(mn_log_loss),
  control = ctrl_grid
)

autoplot(gbRes)


gbFit<-gbWflow%>%finalize_workflow(parameters = select_best(gbRes, "mn_log_loss"))%>%
  fit(modelTrain)

predz<-predict(gbFit, new_data = holdData, type = "prob")

model_st<-
  stacks()%>%
  add_candidates(glmRes)%>%
  add_candidates(gbRes)%>%
  blend_predictions(
    metric = metric_set(mn_log_loss),
    penalty = 10^(-6:-1), mixture = (0:10)/10)%>%
  fit_members()
autoplot(model_st, type = "weights")

#
holdData<-as_tibble(read.csv("Season 2/sliced-s01e10/test.csv",header=T,stringsAsFactors=F,sep=","))

age_inDays<-function(age){
  
  if(grepl("month",age)){
    
    as.numeric(stringr::str_match(age,"\\d"))*30
    
  } else if(grepl("year",age)){
    as.numeric(stringr::str_match(age,"\\d"))*365
  }  else if(grepl("day",age)){
    as.numeric(stringr::str_match(age,"\\d"))
  }  else if(grepl("week",age)){
    as.numeric(stringr::str_match(age,"\\d"))*7
  } else {
    
    NA
  } 
  
  
  
}

age_inDaysDF<-function(v){
  unlist(lapply(v,age_inDays))
}

holdData$age_upon_outcome<-age_inDaysDF(holdData$age_upon_outcome)

holdData$moring<-am(ymd_hms(holdData$datetime))
holdData$dayofWeek<-wday(ymd_hms(holdData$datetime))
holdData$month<-month(ymd_hms(holdData$datetime))
holdData$year<-year(ymd_hms(holdData$datetime))

holdData<-holdData%>%
  mutate(breed = tolower(breed),
         animal_type = tolower(animal_type)
  )

holdData$astrix<-grepl("\\*",holdData$name)
holdData$noName<-is.na(holdData$name)
holdData$mix<-grepl("mix",holdData$breed)

animalExtractor<-function(breed){
  trimws(str_remove(str_remove(breed,"mix"),"\\s"))
}
animalExtractordf<-function(v){
  unlist(lapply(v, animalExtractor))
}

holdData$animal_type2<-holdData$animal_type
holdData$animal_type2[!holdData$animal_type2%in%c("dog","cat")]<-
  animalExtractordf(holdData$breed[!holdData$animal_type2%in%c("dog","cat")])

holdData$tabby<-grepl("tabby",holdData$breed)

# breedz<-na.omit(unique((holdData%>%separate_rows(breed,sep="/"))$breed))

holdData[,breedz]<-lapply(breedz,function(brs){
  
  as.numeric(grepl(brs,holdData$breed))
  
})%>%do.call(cbind,.)

holdData$animal_type2[holdData$animal_type2=="leghorn"]<-"dog"

holdData$adoption<-predz$.pred_adoption
holdData$`no outcome`<-predz$`.pred_no outcome`
holdData$transfer<-predz$.pred_transfer
  
write_csv(holdData%>%dplyr::select(id, adoption, `no outcome`, transfer),"E10sub1.csv")
  