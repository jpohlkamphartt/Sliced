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
library(skimr)
library(beepr)
library(tayloRswift)
library(treesnip)
library(stacks)
options(tidymodels.dark = TRUE)
here()
usethis::edit_r_environ()
set.seed(1)

all_cores <- parallel::detectCores()
registerDoParallel(cores=all_cores-2)
options(scipen=999)
source("SlicedTheme.R")
modelData<-as_tibble(read.csv("Season 2/sliced-s01e02/train.csv",header=T,stringsAsFactors=F,sep=","))

# lets remove some extra variables like name, I bet Tony is hamming it up..
modelData<-modelData%>%
  dplyr::select(
    -id, 
    -operator,
    -aircraft,
    -airport,
    -species_name
  )

# note on Kaggle says there is some nested data in engine type and precipitation, lets fix that
enginetypes<-na.omit(unique((modelData%>%separate_rows(engine_type,sep="/"))$engine_type))
# make one hots for each engine type
modelData[,enginetypes]<-lapply(enginetypes,function(et){
  as.numeric(grepl(et,modelData$engine_type))
})%>%do.call(cbind,.) 
modelData$C<-as.numeric(modelData$C+modelData$c>0)
# precipitation
precipitations<-unique(trimws(na.omit((modelData%>%separate_rows(precipitation,sep=","))$precipitation)))
# make one hots for each engine type
modelData[,precipitations]<-lapply(precipitations,function(pr){
  as.numeric(grepl(pr,modelData$precipitation))
})%>%do.call(cbind,.) 

modelData$engine3_position[modelData$engine3_position=="CHANGE CODE"]<-NA

skim(modelData)
# observations
# day is most likely useless unless a holiday..
# Jan 27 is most likely accident day..
modelData%>%
  mutate(dayMonth = days(incident_day)+months(incident_month))%>%
  group_by(dayMonth)%>%
  summarise(
    damaged = sum(damaged,na.rm=T)/n()
  )%>%
  arrange(desc(damaged))
modelData<-modelData%>%
  # aircraft model.mass,engine_make, position, is a factor
  mutate_at(
    vars(aircraft_model,aircraft_mass,engine_make,
         engine1_position,engine2_position,engine4_position),
    as.factor)%>%
  mutate_if(is.character,as.factor)%>%
  # remove engine type and precip
  dplyr::select(
    -precipitation,
    -engine_type
  )%>%
  mutate(
    aircraft_mass= factor(aircraft_mass,levels = c("1", "2"," 3", "4", "5"),ordered = T),
    species_quantity=factor(species_quantity,levels = c("1","2-10", "11-100", "Over 100"),ordered = T)
  )
  

modelData$damaged<-as.factor(modelData$damaged)


modelFormula_Cat<-as.formula(damaged~aircraft_mass+engines+
                               flight_phase+visibility+height+speed+distance+species_quantity+flight_impact+        
                               D+A+C+`F`+B+c +FOG+RAIN+SNOW+
                               species_id+faa_region+ 
                               incident_year +incident_month)

modelFormula_glm<-as.formula(damaged~aircraft_mass+engines+engine1_position+engine2_position+state+   
                           flight_phase+visibility+height+speed+distance+species_id+species_quantity+flight_impact+        
                           D+A+C+`F`+B+c +FOG+RAIN+SNOW+NONE+
                          aircraft_type+engine_make+state+operator_id+incident_year+operator_id+incident_month
                           )

modelFormula_knn<-as.formula(damaged~aircraft_mass+engines+   
                               flight_phase+visibility+height+speed+species_quantity+flight_impact+
                               aircraft_type+operator_id+faa_region
)

# helicopters????
# no barrel roll?

# lots of NAs, imputing is gonna suck

# lets build our preprocessing, I bet Julia is doing somethign really cool for this, 
# step_magic
modelSplit<-initial_split(modelData)
modelTrain<-training(modelSplit)
modelTest<-testing(modelSplit)
modelFoldz<-vfold_cv(modelTrain,v=5,repeats=1) # no need to be wasting time on repeats..

Rec_Cat<-
  recipe( modelFormula_Cat ,data=modelTrain)%>%
  step_ordinalscore(aircraft_mass,species_quantity)%>%
  step_novel(all_nominal_predictors()) %>%
  step_other(all_nominal_predictors(), threshold = 0.01) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_impute_knn(all_numeric_predictors())%>%
  step_zv(all_predictors())%>%
  step_dummy(all_nominal_predictors(),one_hot=T)%>%
  step_corr(all_numeric_predictors(),threshold = .9)%>%
  step_smote(damaged)

Rec_glm<-
  recipe(modelFormula_glm ,data=modelTrain)%>%
  step_ordinalscore(aircraft_mass,species_quantity)%>%
  step_num2factor(incident_month,levels = as.character(sort(unique(modelData$incident_month))))%>%
  step_novel(all_nominal_predictors()) %>%
  step_other(all_nominal_predictors(), threshold = 0.01) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_impute_knn(all_numeric_predictors())%>%
  step_zv(all_predictors())%>%
  step_dummy(all_nominal_predictors(),one_hot=T)%>%
  step_corr(all_numeric_predictors(),threshold = .9)%>%
  step_smote(damaged)

Rec_knn<-
  recipe(modelFormula_knn ,data=modelTrain)%>%
  step_ordinalscore(aircraft_mass,species_quantity)%>%
  step_novel(all_nominal_predictors()) %>%
  step_other(all_nominal_predictors(), threshold = 0.01) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_impute_knn(all_numeric_predictors())%>%
  step_zv(all_predictors())%>%
  step_dummy(all_nominal_predictors(),one_hot=T)%>%
  step_corr(all_numeric_predictors(),threshold = .9)%>%
  step_smote(damaged)

# lets pick some models, we will try a parametric and non-parametric model
ctrl_grid <- control_stack_grid()

glmSpec<-logistic_reg(mixture = tune(),penalty = tune())%>%
  set_engine("glmnet")%>%
  set_mode("classification")

glmGrid<-grid_latin_hypercube(
  mixture(),
  penalty(),
  size = 5
)

glmWflow<-workflow()%>%
  add_recipe(Rec_glm)%>%
  add_model(glmSpec)

glmRes<-tune_grid(
  glmWflow,
  resamples = modelFoldz,
  grid = glmGrid,
  metrics = metric_set(mn_log_loss,roc_auc),
  control = ctrl_grid
)


knnSpec <- nearest_neighbor(
  neighbors = tune(), weight_func = tune()
) %>% set_engine("kknn") %>% set_mode("classification")

knnGrid<-grid_latin_hypercube(
  neighbors(),
  weight_func(),
  size = 5
)

knnWflow<-workflow()%>%
  add_recipe(Rec_knn)%>%
  add_model(knnSpec)

knnRes<-tune_grid(
  knnWflow,
  resamples = modelFoldz,
  grid = knnGrid,
  metrics = metric_set(mn_log_loss,roc_auc),
  control = ctrl_grid
)
catSpec<-boost_tree( trees = tune(), min_n = tune(), tree_depth = tune(),
                     loss_reduction = tune(),sample_size = tune(),learn_rate = tune(),mtry = tune())%>%
  set_engine("catboost")%>%
  set_mode("classification")

catGrid<-grid_latin_hypercube(
  trees(),
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  learn_rate(),
  finalize(mtry(), modelTrain),
  size = 2
)

catWflow<-workflow()%>%
  add_recipe(Rec_Cat)%>%
  add_model(catSpec)

catRes<-tune_grid(
  catWflow,
  resamples = modelFoldz,
  grid = catGrid,
  metrics = metric_set(mn_log_loss,roc_auc),
  control = ctrl_grid
)
model_st <- 
  stacks() %>%
  add_candidates(glmRes) %>%
  add_candidates(knnRes) %>%
  add_candidates(catRes) %>%
  # determine how to combine their predictions
  blend_predictions(metric =metric_set(mn_log_loss),penalty = 10^(-6:-1),mixture = (0:5)/5) %>%
  # fit the candidates with nonzero stacking coefficients
  fit_members()
model_st


autoplot(model_st)
autoplot(model_st, type = "members")
autoplot(model_st, type = "weights")

collect_parameters(model_st,"glmRes")
glmSpec_max<-logistic_reg(mixture = 0.0783,penalty = 0.0418)%>%
  set_engine("glmnet")%>%
  set_mode("classification")
glmWflow_max<-workflow()%>%
  add_recipe(Rec_glm)%>%
  add_model(glmSpec_max)

glmWflow_max%>%fit(modelTrain)%>%
  pull_workflow_fit() %>%
  tidy()%>%View()

# holdout data
holdData<-as_tibble(read.csv("Season 2/sliced-s01e02/test.csv",header=T,stringsAsFactors=F,sep=","))

holdData<-holdData%>%
  dplyr::select(
    -operator,
    -aircraft,
    -airport,
    -species_name
  )

# note on Kaggle says there is some nested data in engine type and precipitation, lets fix that
# make one hots for each engine type
holdData[,enginetypes]<-lapply(enginetypes,function(et){
  as.numeric(grepl(et,holdData$engine_type))
})%>%do.call(cbind,.) 
holdData$C<-as.numeric(holdData$C+holdData$c>0)
# precipitation
# make one hots for each engine type
holdData[,precipitations]<-lapply(precipitations,function(pr){
  as.numeric(grepl(pr,holdData$precipitation))
})%>%do.call(cbind,.) 

holdData$engine3_position[holdData$engine3_position=="CHANGE CODE"]<-NA

#
holdData<-holdData%>%
  # aircraft model.mass,engine_make, position, is a factor
  mutate_at(
    vars(aircraft_model,aircraft_mass,engine_make,#distance ,height,
         engine1_position,engine2_position,engine4_position),
    as.factor)%>%
  mutate_if(is.character,as.factor)%>%
  # remove engine type and precip
  dplyr::select(
    -precipitation,
    -engine_type
  )%>%
  mutate(
    aircraft_mass= factor(aircraft_mass,levels = c("1", "2"," 3", "4", "5"),ordered = T),
    species_quantity=factor(species_quantity,levels = c("1","2-10", "11-100", "Over 100"),ordered = T)
  )


holdData$damaged<-predict(model_st,
                new_data = holdData,
                type = "prob")$.pred_1
write_csv(holdData%>%dplyr::select(id,damaged),"submission5JPH.csv")
