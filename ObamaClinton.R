########################################################
### Case: 2008 Democratic Primaries - Clinton vs. Obama
########################################################
source("DataAnalyticsFunctions.R")
# read data into R
election_data <- read.csv("ElectionDataAlone.csv")

# Next use the function summary to inspect the data
summary(election_data)

##############################################
# Cleaning up the data
# Write a function that replaces NAs with the mean of the non-missing data 
# in the column. This function can be called for different data sets to 
# impute the data.
impute_data <- function(vec, mn) {
  ifelse(is.na(vec), mn, vec)
}
# Find the means for all the numeric columns. 
# The function sapply automatically runs the mean function 
# (specified as second argument) on the columns 10 through 41. 
# The means are then saved in the vector named train_data_mean. 
# We use the argument na.rm=TRUE to ask the function to ignore NA entries.
data_mean <- sapply(election_data[,10:41],mean, na.rm=TRUE)

# Run this command to look at means for all the columns we found by running the sapply function
(data_mean)

# Impute the missing data. Loop through all the rows and 
# for each column call the function impute_train_data.
for(i in 10:41) {
  election_data[,i]<-impute_data(election_data[,i],data_mean[i-9])
}
# Run the summary function again. Now you see that no demographic/county columns have NA entries.
summary(election_data)

# Create two separate data sets from the data in electionData.
election_data$ElectionDate <- as.Date(election_data$ElectionDate, format="%m/%d/%Y")
election_data_train <- election_data[election_data$ElectionDate < as.Date("2/19/2008", format="%m/%d/%Y"), ]
election_data_test <- election_data[election_data$ElectionDate >= as.Date("2/19/2008", format="%m/%d/%Y"), ]

# If you want to write these data sets back out into spreadsheets, 
# use the following "write" commands in R.
# write.csv(electionDataTrain, "electionDataTrain.csv")
# write.csv(electionDataTest, "electionDataTest.csv")

##########################################################
### End of Data Cleaning up
##########################################################
#
# Create some possible variables that might be of interest.
# (things we might like to predict in a regression using the demographic information). 
# These variables directly become a part of our data set election_data_train. You can use the command names(election_data_train) to see that these are added as columns in our data set.
election_data_train$Obama_margin <- election_data_train$Obama - election_data_train$Clinton
election_data_train$Obama_margin_percent <- 100*election_data_train$Obama_margin/election_data_train$TotalVote
election_data_train$Obama_wins <- ifelse(election_data_train$Obama_margin >0, 1,0)
names(election_data_train)
###
### Based on the data, to account for the size of possible delegates on each county
### we will work with election_data_train$Obama_margin_percent to be the target out models.
###


#load all the libraries that we may need
library(ggplot2)
library(tidyverse)
library(dplyr)
library(MASS)
library(usmap)
library(glmnet)
library(MLmetrics)
library(scales)
library(plfm)
#prepare data set for different questions

model_train <- election_data_train[,-4:-5]
model_train <- model_train[, -5: -7]
model_train <- model_train[, -39]
model_train <- model_train[, -37]
model_train #the train data set for modeling

model_test <- election_data_test[,-4:-5]
model_test <- model_test[, -5: -7]
model_test #the test data set for modeling


###
### Provide visualizations
###
data_for_plot <- election_data_train
data_for_plot$Clinton_margin <- data_for_plot$Clinton - data_for_plot$Obama
data_for_plot$Clinton_margin_percent <- 100*data_for_plot$Clinton_margin/data_for_plot$TotalVote
data_for_plot$Clinton_wins <- ifelse(data_for_plot$Clinton_margin >0, 1,0)
data_for_plot$state = data_for_plot$State
data_for_plot$Obama_wins = as.factor(data_for_plot$Obama_wins)
data_for_plot$Clinton_wins = as.factor(data_for_plot$Clinton_wins)

data_for_plot$voter_pattern=0
data_for_plot$voter_pattern=ifelse(data_for_plot$Obama_wins==1, 1, 0)

#plot 1
#US Map by voter pattern (state wise)
plot_usmap(data = data_for_plot, values = "voter_pattern") + 
  scale_fill_continuous(low="light blue", high="dark blue",name = "Voter Pattern") + 
  theme(legend.position = "right")


#plot 2
#Voter pattern across regions differentiated by Election Type 
data_for_plot$voter_pattern=ifelse(data_for_plot$Obama_wins==1, "Obama", "Clinton")
ggplot(data_for_plot, aes(x=Region, y=TotalVote, fill=voter_pattern))+
  geom_bar(position="fill", stat="identity")+
  facet_wrap(~ElectionType)

#plot 3
#Ethnicity distribution
data_subset=data_for_plot %>% 
  group_by(Region) %>% 
  summarize(Sum_White=sum(White)/sum(White, Black, Asian, AmericanIndian, Hispanic, Hawaiian), 
            Sum_Black=sum(Black)/sum(White, Black, Asian, AmericanIndian, Hispanic, Hawaiian),
            Sum_Asian=sum(Asian)/sum(White, Black, Asian, AmericanIndian, Hispanic, Hawaiian),
            Sum_AmericanIndian=sum(AmericanIndian)/sum(White, Black, Asian, AmericanIndian, Hispanic, Hawaiian),
            Sum_Hispanic=sum(Hispanic)/sum(White, Black, Asian, AmericanIndian, Hispanic, Hawaiian),
            Sum_Hawaiian=sum(Hawaiian)/sum(White, Black, Asian, AmericanIndian, Hispanic, Hawaiian))

class(data_for_plot$voter_pattern)

data_subset$voter_pattern <- data_for_plot$voter_pattern
str(data_subset)
data_long <- data_subset %>%
  gather("Ethnicity", "Value", -Region)

ggplot(data_long, aes(x=Region, y=Value, fill=Ethnicity))+
  geom_bar(position="fill", stat="identity")

#plot 4 
# age distribution across regions 
age_data=data_for_plot[,c(3,11:13)]
class(age_data)

age_dist = age_data %>%
  gather("Age_Group", "Value", -Region)

ggplot(age_dist, aes(x=Region, y=Value, fill=Age_Group))+
  geom_bar(position="fill", stat="identity")+facet_wrap(~data_for_plot$voter_pattern)

#plot 5 
#median income by region coloured by voter pattern 
ggplot(data_for_plot, aes(x=Region))+geom_boxplot(aes(y=MedianIncome, color=voter_pattern))+scale_y_continuous(labels = dollar)+
  labs(title="Plot 1: Median income distribution by Region and \n Voter Pattern in 2005", x="Region", y="Median Income")

#plot 6 
#poverty spread across regions colored by voter pattern 
ggplot(data_for_plot, aes(x=Region))+geom_boxplot(aes(y=Poverty, color=voter_pattern))+
  labs(title="Plot 2: Persons in Poverty by Region and \n Voter Pattern in 2004", x="Region", y="Percentage of People in Poverty")

#plot 7 = more rich voted for obama
#income above 75k 
ggplot(data_for_plot, aes(x=Region))+geom_boxplot(aes(y=IncomeAbove75K, color=voter_pattern))+
  labs(title="Plot 3: Households with Income above \n 75K by Region and Voter Pattern in 1999", x="Region", y="Percentage of Households with Income above 75k")


###
### Prediction. No additional script beyond the cleaning up the data
### provided above. (FIPS variable behaves as an identifier for each observation
### so, might not want to include it in a linear regression.)
###

model_train

#null model
null_model <- lm(Obama_margin_percent ~ 1, data = model_train)
summary(null_model)
AIC(null_model)

#full model
full_model <- lm(Obama_margin_percent ~ ., data = model_train)
summary(full_model)
AIC(full_model)

#stepwise regression model
step(full_model, trace = 1, direction="both")
step_model <- lm(Obama_margin_percent ~ State + Age65andAbove + White + 
                   Asian + AmericanIndian + Hawaiian + HighSchool + Bachelors + 
                   Poverty + IncomeAbove75K + UnemployRate + SpeakingNonEnglish + 
                   SocialSecurity + RetiredWorkers + Disabilities + DisabilitiesRate + 
                   Homeowner + PopDensity, data = model_train)
summary(step_model)
AIC(step_model)

#lasso
model_train_matrix <- model.matrix(Obama_margin_percent ~ ., data=model_train)
model_train_y <- model_train$Obama_margin_percent

#lasso selection
lasso_model1 <- glmnet(model_train_matrix, model_train_y, alpha = 1, family = 'gaussian')
plot(lasso_model1, xvar = 'lambda', label = T)
lasso_model2 <- cv.glmnet(model_train_matrix, model_train_y, alpha = 1, family = 'gaussian', type.measure = 'mse')
lasso_lam_1se <- lasso_model2$lambda.1se
lasso_coef_1se <- coef(lasso_model1, lasso_lam_1se)
summary(lasso_coef_1se)
plot(lasso_model2)

lasso_model_best <- glmnet(model_train_matrix, model_train_y, alpha = 1, family = 'gaussian', lambda = lasso_lam_1se)

## get the support
lasso_supp <- support(lasso_model_best$beta)
length(lasso_supp)
colnames(model_train_matrix[,lasso_supp])

inthemodel_train <- unique(c(lasso_supp))# unique grabs union
lasso_train_data <- cBind(model_train_matrix[,inthemodel_train]) 
lasso_train_data <- as.data.frame(as.matrix(lasso_train_data))# make it a data.frame
dim(lasso_train_data) ## p about half n


#fit the final model
final_fit <- lm(model_train_y ~., data=lasso_train_data)
summary(final_fit)
AIC(final_fit)

#10 folds cv
n_folds <- 10
folds_i <- sample(rep(1:n_folds, length.out = nrow(lasso_train_data)))
cv_tmp <- matrix(NA, nrow = n_folds, ncol = 1)

cv_data_train <- lasso_train_data
cv_data_train$cv_y <- model_train_y
cv_data_train

for (k in 1:n_folds) {
  test_i <- which(folds_i == k)
  train_xy <- cv_data_train[-test_i, ]
  test_xy <- cv_data_train[test_i, ]
  fitted_models <- lm(cv_y~., data=train_xy)
  xt <- test_xy[,-68]  
  yt <- test_xy$cv_y
  xt <- as.data.frame(xt)
  pred <- predict.lm(fitted_models,newdata = xt,type="response")
  cv_tmp[k,] <- MSE(pred, yt)
}
#MSE of 10 folds cv
cv_tmp

#prediction
model_test
model_test_matrix <- model.matrix( ~ ., data=model_test)
colnames(model_test_matrix[,lasso_supp])
inthemodel_test <- unique(c(lasso_supp))# unique grabs union
lasso_test_data <- cBind(model_test_matrix[,inthemodel_test]) 
lasso_test_data <- as.data.frame(as.matrix(lasso_test_data))# make it a data.frame
dim(lasso_test_data) ## p about half n
lasso_pred <- predict.lm(final_fit,newdata = lasso_test_data,type="response")
lasso_pred
model_test_with_prediction <- model_test
model_test_with_prediction$lasso_pred <- lasso_pred

###
### Unsupervised learning to explore the data using a subset of the demographic variables. 
###

drop <- c("FIPS","County","State","Region","ElectionDate","ElectionType","TotalVote",
          "Clinton","Obama","Obama_margin_percent","Obama_margin","Obama_wins")
data_pca_train <- election_data_train[,!(names(election_data_train) %in% drop)]
pca <- prcomp(data_pca_train, scale=TRUE)

plot(pca,main="PCA: Variance Explained by Factors")
mtext(side=1, "Factors",  line=1, font=2)

pcScore <- predict(pca)
pcScore

plot(pcScore[,1:2], pch=21,  main="")
plot(pcScore[,3:4], pch=21,  main="")

loadings <- pca$rotation[,1:4]

#PC1 Loadings
v1 <- loadings[order(abs(loadings[,1]), decreasing=TRUE)[1:35],1]
loadingfit1 <- lapply(1:35, function(k) ( t(v1[1:k])%*%v1[1:k] - 3/4 )^2)
v1 [1:which.min(loadingfit)]
#PC2 Loadings
v2 <- loadings[order(abs(loadings[,1]), decreasing=TRUE)[1:35],2]
loadingfit <- lapply(1:35, function(k) ( t(v2[1:k])%*%v2[1:k] - 3/4 )^2)
v2 [1:which.min(loadingfit)]
#PC3 Loadings
v3 <- loadings[order(abs(loadings[,1]), decreasing=TRUE)[1:35],3]
loadingfit <- lapply(1:35, function(k) ( t(v3[1:k])%*%v3[1:k] - 3/4 )^2)
v3 [1:which.min(loadingfit)]
#PC4 Loadings
v4 <- loadings[order(abs(loadings[,1]), decreasing=TRUE)[1:35],4]
loadingfit <- lapply(1:35, function(k) ( t(v4[1:k])%*%v4[1:k] - 3/4 )^2)
v4 [1:which.min(loadingfit)]

###
###  Invetigate impact of changing hispanic demographic
###
#### Model with 1771 controls to measure the impact of 10% larger Hispanic demographic
Q4_data_train <- election_data_train
Q4_y <- Q4_data_train$Obama_margin_percent
Q4_x <- model.matrix(Obama_margin_percent ~ .-Hispanic-Obama_wins-Obama_margin-FIPS-ElectionDate-TotalVote-Clinton-Obama, data = Q4_data_train )
Q4_d <- Q4_data_train$Hispanic

## Step 1.Select controls that are good to predict the outcome
Q4_model1 <- glmnet(Q4_x, Q4_y, alpha = 1, family = 'gaussian')
Q4_model2 <- cv.glmnet(Q4_x, Q4_y, alpha = 1, family = 'gaussian', type.measure = 'mse')
Q4_lam_1se <- Q4_model2$lambda.1se
# Call Lasso 
Q4_lasso_1se <- glmnet(Q4_x, Q4_y,alpha = 1, family = 'gaussian',lambda = Q4_lam_1se)
# Get the support
Q4_supp1 <- support(Q4_lasso_1se$beta)
# Step 1 selected
length(Q4_supp1)
### controls
colnames(Q4_x[,Q4_supp1])

###
### Step 2.Select controls that are good to predict the treatment
Q4_model3 <- glmnet(Q4_x, Q4_d, alpha = 1, family = 'gaussian')
Q4_model4 <- cv.glmnet(Q4_x, Q4_d, alpha = 1, family = 'gaussian', type.measure = 'mse')
Q4_lam_d_1se <- Q4_model4$lambda.1se
# Call Lasso 
Q4_lasso_d_1se <- glmnet(Q4_x, Q4_d, alpha = 1, family = 'gaussian',lambda = Q4_lam_d_1se)
# Get the support
Q4_supp2 <-support(Q4_lasso_d_1se$beta)
### Step 2 selected
length(Q4_supp2)
### controls
colnames(Q4_x[,Q4_supp2])

###
### Step 3.Combine all selected and refit linear regression
Q4_inthemodel <- unique(c(Q4_supp1, Q4_supp2)) # unique grabs union
Q4_selectdata <- cBind(Q4_d, Q4_x[,Q4_inthemodel]) 
Q4_selectdata <- as.data.frame(as.matrix(Q4_selectdata)) # make it a data.frame
dim(Q4_selectdata) ## p about half n

Q4_causal_lm <- lm(Q4_y ~ ., data = Q4_selectdata)
summary(Q4_causal_lm)$coef["Q4_d",]
## Not significant for casual inference

####
#### Feel free to compare/contrast your results with the following simple regression model
#### 
HispanicSimple <- glm(Obama_margin_percent ~ Hispanic, data = Q4_data_train )
summary(HispanicSimple)


####
### Investigate impact of changing black demographic
####
#### Model with 1771 controls to measure the impact of 5% larger Black demographic
Q4_data_train
Q4_y2 <- Q4_data_train$Obama_margin_percent
Q4_x2 <- model.matrix( Obama_margin_percent ~ .-Black-Obama_wins-Obama_margin-FIPS-ElectionDate-TotalVote-Clinton-Obama, data = Q4_data_train )
Q4_d2 <- Q4_data_train$Black
## Step 1.Select controls that are good to predict the outcome
Q4_model5 <- glmnet(Q4_x2, Q4_y2, alpha = 1, family = 'gaussian')
Q4_model6 <- cv.glmnet(Q4_x2, Q4_y2, alpha = 1, family = 'gaussian', type.measure = 'mse')
Q4_lam_y2 <- Q4_model6$lambda.1se
# Call Lasso 
Q4_lam_1se2 <- glmnet(Q4_x2, Q4_y2,alpha = 1, family = 'gaussian',lambda = Q4_lam_y2)
# Get the support
Q4_supp3 <- support(Q4_lam_1se2$beta)
# Step 1 selected
length(Q4_supp3)
### controls
colnames(Q4_x2[,Q4_supp3])

###
### Step 2.Select controls that are good to predict the treatment
Q4_model7 <- glmnet(Q4_x2, Q4_d2, alpha = 1, family = 'gaussian')
Q4_model8 <- cv.glmnet(Q4_x2, Q4_d2, alpha = 1, family = 'gaussian', type.measure = 'mse')
Q4_lam_d_1se2 <- Q4_model8$lambda.1se
# Call Lasso 
Q4_lasso_d_1se2 <- glmnet(Q4_x2,Q4_d2,alpha = 1, family = 'gaussian',lambda = Q4_lam_d_1se2)
# Get the support
Q4_supp4<-support(Q4_lasso_d_1se2$beta)
### Step 2 selected
length(Q4_supp4)
### controls
colnames(Q4_x2[,Q4_supp4])

###
### Step 3.Combine all selected and refit linear regression
Q4_inthemodel2 <- unique(c(Q4_supp3,Q4_supp4)) # unique grabs union
Q4_selectdata2 <- cBind(Q4_d2,Q4_x2[,Q4_inthemodel2]) 
Q4_selectdata2 <- as.data.frame(as.matrix(Q4_selectdata2)) # make it a data.frame
dim(Q4_selectdata2) ## p about half n

causal_lm <- lm(Q4_y2 ~ ., data=Q4_selectdata2)
summary(causal_lm)$coef["Q4_d2",]
## Not significant for casual inference

####
#### Feel free to compare/contrast your results with the following simple regression model
#### 
BlackSimple <- glm(Obama_margin_percent ~ Black, data = Q4_data_train )
summary(BlackSimple)
####

####
#### More Visualization for Obama
#### 
####
#votes across regions
Q5_data_train <- election_data_train
Q5_data_train$state = Q5_data_train$State
Q5_data_train$Obama_wins = as.factor(Q5_data_train$Obama_wins)
Q5_data_train$Clinton_wins = as.factor(Q5_data_train$Clinton_wins)

Q5_data_train$voter_pattern=0
Q5_data_train$voter_pattern=ifelse(Q5_data_train$Obama_wins==1, 1, 0)

Q5_data_train$voter_pattern=ifelse(Q5_data_train$Obama_wins==1, "Obama", "Clinton")

ggplot(Q5_data_train, aes(x=Region, y=TotalVote, fill=voter_pattern))+
  geom_bar(position="fill", stat="identity") + ggtitle("Split of Votes per Region")

#creating a susbet to plot ethnicity across regions
Q5_data_subset <- Q5_data_train %>% 
  group_by(Region) %>% 
  summarize(Percent_White=sum(White)/sum(White, Black, Asian, AmericanIndian, Hispanic, Hawaiian), 
            Percent_Black=sum(Black)/sum(White, Black, Asian, AmericanIndian, Hispanic, Hawaiian),
            Percent_Asian=sum(Asian)/sum(White, Black, Asian, AmericanIndian, Hispanic, Hawaiian),
            Percent_AmericanIndian=sum(AmericanIndian)/sum(White, Black, Asian, AmericanIndian, Hispanic, Hawaiian),
            Percent_Hispanic=sum(Hispanic)/sum(White, Black, Asian, AmericanIndian, Hispanic, Hawaiian),
            Percent_Hawaiian=sum(Hawaiian)/sum(White, Black, Asian, AmericanIndian, Hispanic, Hawaiian))

class(Q5_data_train$voter_pattern)
summary(Q5_data_subset)

Q5_data_subset$voter_pattern <- Q5_data_train$voter_pattern
str(Q5_data_subset)
Q5_data_long <- Q5_data_subset %>%
  gather("Ethnicity", "Value", -Region)

#graphing ethnicity across regions
ggplot(Q5_data_long, aes(x=Region, y=Value, fill=Ethnicity))+ 
  geom_bar(position="fill", stat="identity") + 
  ggtitle("Ethnicity Makeups Across Regions")
