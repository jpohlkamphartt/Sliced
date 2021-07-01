# Season 0 Ep 1 Round 2
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
modelData<-as_tibble(read.csv("Season1/s00e01/Copy of sliced-s00e01-data.csv",header=T,stringsAsFactors=F,sep=","))

modelData<-modelData%>%
  dplyr::select(
    -X,
    -iid,
    -id,
    -dec,
    -dec_o,
    -idg,
    -position,
    -positin1,
    -pid,
    -field,
    -from,
    -career,
    -partner
  )
    
skim(modelData)

modelData$match<-as.factor(modelData$match)

wideFormula<-formula(
  match ~  gender + condtn + wave + round + order  + int_corr + samerace+
    age_o + race_o + pf_o_att + pf_o_sin + pf_o_int + pf_o_fun + pf_o_amb + pf_o_sha + attr_o+
    sinc_o + intel_o + fun_o + amb_o + shar_o + like_o + prob_o + met_o + age +
    field_cd + undergra + mn_sat + tuition + race + imprace + imprelig + zipcode + income+
    goal + date + go_out + career_c + sports + tvsports + exercise + dining + museums+
    art + hiking + gaming + clubbing + reading + tv + theater + movies + concerts+
    music + shopping + yoga + exphappy + attr1_1 + sinc1_1 + intel1_1 + fun1_1+
    # expnum + 
    amb1_1 + shar1_1 + 
    # attr4_1 + sinc4_1 + intel4_1 + fun4_1 + amb4_1 + shar4_1 + 
    attr2_1+ sinc2_1 + intel2_1 + fun2_1 + amb2_1 + shar2_1 + attr3_1 +
    sinc3_1 + fun3_1 + intel3_1+ amb3_1 + 
    # attr5_1 + sinc5_1 + intel5_1 + fun5_1 + amb5_1 +
    attr + sinc + intel+fun + amb + shar + like + prob + met + match_es
  # + attr1_s + sinc1_s+intel1_s + fun1_s + amb1_s + shar1_s + attr3_s + sinc3_s + intel3_s + fun3_s + amb3_s
)

mediumFormula<-formula(
  match ~  gender + 
    # condtn + wave + round + order +  
    int_corr + samerace+ age_o + 
    # race_o + 
    pf_o_att + pf_o_sin + pf_o_int + pf_o_fun + pf_o_amb + pf_o_sha + attr_o+
    sinc_o + intel_o + fun_o + amb_o + shar_o + like_o + prob_o + met_o + age +
    # field_cd + undergra + mn_sat + tuition + race + imprace + imprelig + zipcode + income+
    goal + date + go_out + 
    # career_c + sports + tvsports + exercise + dining + museums+
    # art + hiking + gaming + clubbing + reading + tv + theater + movies + concerts+
    # music + shopping + yoga + exphappy + 
    attr1_1 + sinc1_1 + intel1_1 + fun1_1+
    # expnum + 
    amb1_1 + shar1_1 + 
    # attr4_1 + sinc4_1 + intel4_1 + fun4_1 + amb4_1 + shar4_1 + 
    attr2_1+ sinc2_1 + intel2_1 + fun2_1 + amb2_1 + shar2_1 + attr3_1 +
    sinc3_1 + fun3_1 + intel3_1+ amb3_1 + 
    # attr5_1 + sinc5_1 + intel5_1 + fun5_1 + amb5_1 +
    attr + sinc + intel+fun + amb + shar + like + prob + met + match_es
  # + attr1_s + sinc1_s+intel1_s + fun1_s + amb1_s + shar1_s + attr3_s + sinc3_s + intel3_s + fun3_s + amb3_s
)

narrowFormula<-formula(
  match ~  gender + 
    # condtn + wave + round + order +   
    int_corr + samerace+ 
    age_o +
    # race_o + 
    pf_o_att + pf_o_sin + pf_o_int + pf_o_fun + pf_o_amb + pf_o_sha + attr_o+
    sinc_o + intel_o + fun_o + amb_o + shar_o + like_o + prob_o + met_o +
    age +
    # field_cd + undergra + mn_sat + tuition + race + imprace + imprelig + zipcode + income+
    # goal + date + go_out + 
    # career_c + sports + tvsports + exercise + dining + museums+
    # art + hiking + gaming + clubbing + reading + tv + theater + movies + concerts+
    # music + shopping + yoga + exphappy + 
    attr1_1 + sinc1_1 + intel1_1 + fun1_1+
    # expnum + 
    amb1_1 + shar1_1 + 
    # attr4_1 + sinc4_1 + intel4_1 + fun4_1 + amb4_1 + shar4_1 + 
    attr2_1+ sinc2_1 + intel2_1 + fun2_1 + amb2_1 + shar2_1 + attr3_1 +
    sinc3_1 + fun3_1 + intel3_1+ amb3_1 + 
    # attr5_1 + sinc5_1 + intel5_1 + fun5_1 + amb5_1 +
    attr + sinc + intel+fun + amb + shar + like + prob + met + match_es
  # + attr1_s + sinc1_s+intel1_s + fun1_s + amb1_s + shar1_s + attr3_s + sinc3_s + intel3_s + fun3_s + amb3_s
)

modelSplit<-initial_split(modelData)
modelTrain<-training(modelSplit)
modelTest<-testing(modelSplit)
modelFolds<-vfold_cv(modelTrain, v=5, repeats = 1)

Rec_Cat<-
  recipe(mediumFormula ,data=modelTrain)%>%
  step_novel(all_nominal_predictors()) %>%
  step_other(all_nominal_predictors(), threshold = 0.01) %>%
  step_unknown(all_nominal_predictors()) %>%
  # step_BoxCox(all_numeric_predictors())%>% 
  # step_YeoJohnson(all_numeric_predictors())%>% 
  step_impute_knn(all_numeric_predictors())%>%
  step_zv(all_predictors())%>%
  step_dummy(all_nominal_predictors(),one_hot=T)%>%
  step_corr(all_numeric_predictors(),threshold = .9)%>%
  step_smote(match)

Rec_glm<-
  recipe(wideFormula ,data=modelTrain)%>%
  step_novel(all_nominal_predictors()) %>%
  step_other(all_nominal_predictors(), threshold = 0.01) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_impute_knn(all_numeric_predictors())%>%
  step_zv(all_predictors())%>%
  step_dummy(all_nominal_predictors(),one_hot=T)%>%
  step_corr(all_numeric_predictors(),threshold = .9)%>%
  step_smote(match)

Rec_knn<-
  recipe(narrowFormula,data=modelTrain)%>%
  step_novel(all_nominal_predictors()) %>%
  step_other(all_nominal_predictors(), threshold = 0.01) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_impute_knn(all_numeric_predictors())%>%
  step_zv(all_predictors())%>%
  step_dummy(all_nominal_predictors(),one_hot=T)%>%
  step_corr(all_numeric_predictors(),threshold = .9)%>%
  step_smote(match)

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
  resamples = modelFolds,
  grid = glmGrid,
  metrics = metric_set(mn_log_loss,roc_auc,accuracy),
  control = ctrl_grid
)

knnSpec <- nearest_neighbor(
  neighbors = tune(), weight_func = tune()
) %>% set_engine("kknn") %>%
  set_mode("classification")

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
  resamples = modelFolds,
  grid = knnGrid,
  metrics = metric_set(mn_log_loss,roc_auc),
  control = ctrl_grid
)

catSpec<-boost_tree( trees = tune(), min_n = tune(), tree_depth = tune(),
                     sample_size = tune(),learn_rate = tune(),mtry = tune())%>%
  set_engine("catboost")%>%
  set_mode("classification")

catGrid<-grid_latin_hypercube(
  trees(),
  tree_depth(),
  min_n(),
  sample_size = sample_prop(),
  learn_rate(),
  finalize(mtry(), modelTrain),
  size = 8
)

catWflow<-workflow()%>%
  add_recipe(Rec_Cat)%>%
  add_model(catSpec)

catRes<-tune_grid(
  catWflow,
  resamples = modelFolds,
  grid = catGrid,
  metrics = metric_set(mn_log_loss,roc_auc),
  # metrics = metric_set(rmse,rsq,mae),
  control = ctrl_grid
)

model_st <- 
  stacks() %>%
  add_candidates(glmRes) %>%
  add_candidates(knnRes) %>%
  add_candidates(catRes) %>%
  # determine how to combine their predictions
  blend_predictions(
    metric =metric_set(mn_log_loss),# roc_auc rmse rsq mae 
    penalty = 10^(-6:-1),mixture = (0:5)/5) %>%
  # fit the candidates with nonzero stacking coefficients
  fit_members()
model_st
beepr::beep("fanfare")

autoplot(model_st)
autoplot(model_st, type = "members")
autoplot(model_st, type = "weights")
  
# holdout data
holdData<-as_tibble(read.csv("Season1/s00e01/Copy of sliced-s00e01-holdout.csv",header=T,stringsAsFactors=F,sep=","))

holdData$match<-predict(model_st,
                          new_data = holdData,
                          type = "class")$.pred_class

ggplot(holdData,aes(x=match))+
  geom_histogram(stat="count")

write_csv(holdData%>%dplyr::select(id,match),"S0E1R2submission1JPH.csv")
