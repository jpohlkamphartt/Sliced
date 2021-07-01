#### Process
1. Determine Response Type
2. Clean up inputs
3. Throw all inputs into a kitchen sink xgboost/black box
4. Explore inputs
4a. Check for transformation
4b. Check for correlations/redundancy
5. feature engineer
6. set up models to test
7. build model
7a. while models running:
  - make new visualizations
  - trash talk
  - prepare performance code
  - prepare holdout code
8. run perf code, select best model
9. run hold out code
9a. check hold out predictions
10. clean up final results and rerun models if needed
11. build something off the wall

#### Sliced Template
### Packages

### Response Exploration
## Response Type: 
Continuous: Is it normally distributed? Can it be normally approximated or should we use another distribution?
Categorical: Is this an ensemble model problem where we can use layered binary responses? If not is this ordinal? Or simple multiclass? What is the class balance like?
Count: Does this include counts of 0 or only positive? Are the counts symetric or long tailed? Which way is the tail?
Time-to-event: Is this a survival problem? Is there censorship?
## Distribution of Response:
## Applicable Models:
xgboost
glm/gam
random forest
MARS
BART
  

### Predictors Exploration
## Predictors Types:
Continuous: Is this normally distributed? Uni-modal? Long-tailed?
Categorical: Is there any structure/heiracy to these? Are these ordered? Look at class imbalance, are there classes that are deterministic to the response but not impactful to the holdout?
Count:
Temporal: Seasonality? Autocorrelations? Relationship to other predictors?
Text: Keywords that might be useful indicators for response? Worth running sentiment analysis on? Phrases or words more important?
Spatial: are the locations correlated? what is the density of events by location? can we do any dimensionality reduction on the spatial data?
## Distribution Of Predictors
## Interaction Of Predictors
- Create correlation plots
- Are there variable that are redundant?
- Is there key interactions that should be explicit features? 
- Time dependent Predictors?

### Feature Engineering
- variable transformations to normality
- group-wise scaling or proportions (how much of a group vs normalizing within group)
- temporal variables: seasonal (week/month/year) lags, moving averages (uniform vs decaying weights), time of last event
- sentiment variables from text
- heirchal categorical variables (eg. "ales" from beer types)
- dimension reduction? clustering or PCA
  
### Model Design
- Parametric or non-parametric?
- Can we ensemble?
- What is the response distribution/task?
- do we need dummy variables?
- make full list of candidate models to test: simple vs complex features, parametric vs non-parametric, expensive vs cheap models
- mixed effects?
- basis functions?
- regularization?


### Model Tuning
- what is our performance metric? is there a secondary metric to control for?
- what grid size can we test fast?
-0
what are the hyper-parameters?

### Model Performance
- perf vs baseline (start a kitchen sink XGBOOST/glm at begining of show)
- baysian model credibility for checking multiple models of varying complexity
- graphics: ROC, class accuracy matrix, residual analysis

### Hold Out Prediction
- how do we get the predictions from the particular model type
- explore predictions, any patterns or insights