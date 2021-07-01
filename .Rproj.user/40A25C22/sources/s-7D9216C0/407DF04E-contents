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
options(tidymodels.dark = TRUE)
here()
usethis::edit_r_environ()
set.seed(1)

all_cores <- parallel::detectCores()
registerDoParallel(cores=all_cores-2)
options(scipen=999)
source("SlicedTheme.R")
modelData<-as_tibble(read.csv("Season 2/sliced-s01e02/train.csv",header=T,stringsAsFactors=F,sep=","))

p1<-modelData%>%
  ggplot(aes(color=as.factor(damaged),x=height,y=speed))+
  geom_point()+
  theme(legend.position = "top")+
  scale_color_sliced(discrete = T)
p2<-modelData%>%
  ggplot(aes(x=damaged,fill=species_quantity))+
  geom_histogram()+
  theme(legend.position = "top")

p1+p2
## not seeing a ton based on these ratios

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
)
modelData$damaged<-as.factor(modelData$damaged)


modelFormula<-as.formula(damaged~aircraft_mass  +  engines   +     engine1_position+engine2_position + faa_region   +   
                           flight_phase  +   visibility   +   height      +    speed     +      distance    +   species_id    +  species_quantity+ flight_impact+        
                           D          +    A     +        C        +      F    +         B    +        c       +     FOG      +     RAIN   +       
                           SNOW )


# helicopters????
# no barrel roll?

# lots of NAs, imputing is gonna suck

# lets build our preprocessing, I bet Julia is doing somethign really cool for this, 
# step_magic
modelSplit<-initial_split(modelData)
modelTrain<-training(modelSplit)
modelTest<-testing(modelSplit)
modelFoldz<-vfold_cv(modelTrain,v=5,repeats=1) # no need to be wasting time on repeats..

Rec1<-recipe( modelFormula ,data=modelTrain)%>%
  # we should make an other column for some of the ones with lots of possibilities
  step_impute_knn(all_nominal_predictors())%>%
  step_novel(all_nominal_predictors())%>%
  step_other(species_id,
             faa_region , threshold = .025)%>%
  step_unknown(all_nominal_predictors(),new_level = "UNKWN")%>%
  # lots of new levels
  # do the dummy? we have a lot of factors so 1-hot first
  step_dummy(all_nominal_predictors())%>%
  # zero variance? we have to remove the random columns
  step_zv(all_numeric_predictors())%>%
  # impute missing data? - after we make dummies , need this to be fast so not using knn
  step_impute_knn(all_numeric_predictors())%>%
  # remove some really out there events - removing things that happen less than 1/500 times
  step_nzv(all_numeric_predictors(),freq_cut = 499/1)%>%
  # remove correlations
  step_corr(all_numeric_predictors(),threshold = .95)%>%
  step_smote(damaged)
class(Rec1)<-c("Rec1",class(Rec1))

Rec2<-recipe(damaged~.,data=modelTrain)%>%
  # we should make an other column for some of the ones with lots of possibilities
  step_impute_mode(all_nominal_predictors())%>%
  step_novel(all_nominal_predictors())%>%
  step_other(airport_id,operator_id,species_id,
             engine_make,engine_model,aircraft_make,
             aircraft_model,state, threshold = .025)%>%
  step_unknown(all_nominal_predictors(),new_level = "UNKWN")%>%
  # lots of new levels
  # do the dummy? we have a lot of factors so 1-hot first
  step_dummy(all_nominal_predictors())%>%
  # zero variance? we have to remove the random columns
  step_zv(all_numeric_predictors())%>%
  # impute missing data? - after we make dummies , need this to be fast so not using knn
  step_impute_knn(all_numeric_predictors())%>%
  # remove some really out there events - removing things that happen less than 1/500 times
  step_nzv(all_numeric_predictors(),freq_cut = 499/1)%>%
  # remove correlations
  step_corr(all_numeric_predictors(),threshold = .95)%>%
  step_rose(damaged)
class(Rec2)<-c("Rec2",class(Rec2))



precz<-list(
  b1=Rec1#,
  # b2=Rec2#,
  # b3=Rec3,
  # b4=Rec4,
  )
# lets pick some models, we will try a parametric and non-parametric model
glmSpec<-logistic_reg(mixture = tune(),penalty = tune())%>%
  set_engine("glmnet")%>%
  set_mode("classification")
xgSpec<-boost_tree(trees = 1000, min_n = tune(), tree_depth = tune(),learn_rate = tune())%>%
  set_engine("xgboost")%>%
  set_mode("classification")
catSpec<-boost_tree(trees = 1000, min_n = tune(), tree_depth = tune(),learn_rate = tune())%>%
  set_engine("catboost")%>%
  set_mode("classification")
modelz<-list(
  glm=glmSpec,
  # xg=xgSpec,
  cat = catSpec)

wflowz<-workflow_set(precz,modelz,cross=T)%>%
  workflow_map(
    resamples = modelFoldz,
    grid = 50,
    metrics = metric_set(mn_log_loss,roc_auc),
    seed=1, # because I am like Nelly.. #1
    verbose = T)

rank_results(wflowz)
autoplot(wflowz)

modelV1best<-select_best(pull_workflow_set_result(wflowz,"base_glm"),metric = "mn_log_loss")
modelV1mod<-finalize_workflow(pull_workflow(wflowz,"base_glm"),modelV1best)
modelV1<-fit(modelV1mod,modelTrain)
modelV1%>%pull_workflow_fit()%>%vip(geom = "point",num_features = 20)
# take off is dangerous, makes sense
# wild that glm was better but I am guessing xgb will fit nicer with more runs

saveRDS(modelV1,"firstmodel.rds")

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
  )

Rec%>%prep()%>%bake(new_data=holdData)

holdData$damaged<-predict(modelV1, new_data = holdData)$.pred_class

write_csv(holdData%>%dplyr::select(id,damaged),"submission1JPH.csv")

### assume XGB
finalGrid<-grid_latin_hypercube(
  trees(),
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  learn_rate(),
  size = 25
)

xgbSpec2<-boost_tree(
  trees = tune(), min_n = tune(), tree_depth = tune(),
  loss_reduction = tune(),sample_size = tune(),learn_rate = tune()
  )%>%
  set_engine("xgboost")%>%
  set_mode("classification")

finalWflow<-workflow()%>%
  add_recipe(Rec)%>%
  add_model(xgbSpec2)

finalRes<-tune_grid(
  finalWflow,
  resamples = modelFoldz,
  grid = finalGrid,
  metrics = metric_set(mn_log_loss,roc_auc)
)
# check to see this actually runs before committing

finalModBest<-select_best(finalRes,metric = "mn_log_loss")
finalMod<-finalize_workflow(finalWflow,finalModBest)
finalFit<-fit(finalMod,modelTrain)
finalFit%>%pull_workflow_fit()%>%vip(geom = "point",num_features = 20)

holdData$damaged<-predict(finalFit, new_data = holdData)$.pred_class

write_csv(holdData%>%dplyr::select(id,damaged),"submission2JPH.csv")

# I write my own shap because the package is kinda broken..
shap<-predict(xgb.Booster.complete(pull_workflow_fit(finalFit)$fit),
              newdata = modelTrain%>%dplyr::select(
                (xgb.Booster.complete(pull_workflow_fit(finalFit)$fit))$feature_names
                )%>%as.matrix(),
              predcontrib=T)

shap<-as_tibble(shap)
attr(shap,which="baseline")<-shap[["BIAS"]]
shap[["BIAS"]]<-NULL
class(shap)<-c(class(shap),"explain")# this makes autoplot work from fastshap

autoplot(shap, num_features = 20)

# I am dying to make these predictions... error in my formats I think in the holdout


# happy pride everyone!!!!