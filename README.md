# obama-clinton
Lasso model predicting the winning spread of Obama over Clinton measured as percentage of the total vote. 

This data project can help modern politics to:
  1. More effectively allocate online and on the ground resource
  2. Forecast performance
  3. Identify key battlegrounds

The raw data include information on:
  1. Primary performance over counties
  2. County demographics, timestamp

Applied methods:
 1. PCA
 2. Lasso
 3. Post-lasso
 4. Stepwise regression
 5. K-fold cross validation (10-fold)
 6. Causal Modeling via double selection

The final model selected is Lasso using R^2 and AIC as selection criterias. 
Causal model was built to investigate the impact of changing hispanic/black demographics.
