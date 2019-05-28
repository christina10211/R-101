
setwd('/Users/christina/Documents/Applied_Analytics/Frameworks/all')

# ensure analysisData.csv and scoringData.csv are in your working directory
install.packages('tidyverse')
library(dplyr)
library(tidyverse)


#data exploring
-----------------------------------------------------------------------------------
data = read.csv('analysisData.csv')
summary(data)
table(data$neighbourhood)
table(data$neighbourhood_cleansed)
table(data$neighbourhood_group_cleansed)
summary(data$bedrooms)
summary(data$bathrooms)
summary(data$neighbourhood_cleansed)
summary(data$neighbourhood_group_cleansed)
summary(data$bed_type)
summary(data$accommodates)
table(data$bed_type)
table(data$accommodates)

#Remove columns that are not useful by going over the data table under Kaggle data section
data <- select(data, -listing_url,-scrape_id,-last_scraped,-name,-summary,-space,-description,-experiences_offered,-neighborhood_overview,-notes,-transit,-access,-interaction,-house_rules,-thumbnail_url,-medium_url,-xl_picture_url,-host_id,-host_url,-host_name,-host_since,-host_about,-jurisdiction_names,-picture_url,-host_acceptance_rate,-host_thumbnail_url,-host_picture_url,-amenities,-first_review,-last_review,-requires_license,-license,-country,-country_code,-has_availability,-calendar_last_scraped)

#Linear Regression
-----------------------------------------------------------------------------------
  #Trial model
  model = lm(price~minimum_nights+review_scores_rating,data)

# read in scoring data and apply model to generate predictions
scoringData = read.csv('scoringData.csv')
pred = predict(model,newdata=scoringData)

# construct submision from predictions
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'sample_submission.csv',row.names = F)

#linear regression (mannual feature selection)
model1 = lm(price~accommodates+review_scores_rating, data)
summary(model1)
pred = predict(model1)

rmse1 = sqrt(mean((pred-data$price)^2))
rmse1

#Adding mroe variables
model2 = lm(price~accommodates+review_scores_rating+bathrooms+bedrooms, data)
summary(model2)
pred = predict(model2)

rmse2 = sqrt(mean((pred-data$price)^2))
rmse2

model3 = lm(price~accommodates+review_scores_rating, data)
summary(model3)
pred = predict(model3)

rmse3 = sqrt(mean((pred-data$price)^2))
rmse3

#Create dummy variables of neighbourhood_group_cleansed
-----------------------------------------------------------------------------------
  install.packages('dummies')
library(dummies)
data=cbind(data,dummy(data$neighbourhood_group_cleansed,sep = "_"))
str(data)
data$Staten = data$'data_Staten Island'
data$'data_Staten Island'

#Adding dummy columns to the model
model5 = lm(price~normal_accommodates+normal_review_scores+normal_bedrooms+normal_bathrooms+data_Bronx+data_Brooklyn+data_Manhattan+data_Queens+normal_number_of_reviews,data)
summary(model5)
pred = predict(model5)
rmse5 = sqrt(mean((pred-data$price)^2))
rmse5

#add dummy variables to test data
scoringData = read.csv('scoringData.csv')
scoringData=cbind(scoringData,dummy(scoringData$neighbourhood_group_cleansed,sep = "_"))

str(scoringData)

library(data.table)
setnames(scoringData, old=c("scoringData_Bronx","scoringData_Brooklyn","scoringData_Manhattan","scoringData_Queens"), new=c("data_Bronx", "data_Brooklyn","data_Manhattan","data_Queens"))

str(scoringData)
pred = predict(model4,newdata=scoringData)
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'submission4.csv',row.names = F)

#Normalize Data
-----------------------------------------------------------------------------------
  data = mutate(data, normal_review_scores = scale(data$review_scores_rating))
data = cbind(data, normal_accommodates = scale(data$accommodates)) 
data = cbind(data, normal_bedrooms = scale(data$bedrooms)) 
data = cbind(data, normal_bathrooms = scale(data$bathrooms))
data = cbind(data, normal_number_of_reviews = scale(data$number_of_reviews))
summary(data$number_of_reviews)

#feature selection
-----------------------------------------------------------------------------------
  #testing all possible subsets - failed
  install.packages('leaps')
library(leaps)
subsets = regsubsets(price~accommodates+neighbourhood_group_cleansed+number_of_reviews+bathrooms+bedrooms+availability_90+availability_365+availability_30+number_of_reviews+guests_included+cancellation_policy+room_type,data=data,nvmax=5,really.big = T)
summary(subsets) 
names(summary(subsets))
subsets_measures = data.frame(model=1:length(summary(subsets)$cp),cp=summary(subsets)$cp,bic=summary(subsets)$bic, adjr2=summary(subsets)$adjr2)
subsets_measures

#forward stepwise
start_mod = lm(price~1,data=data)
empty_mod = lm(price~1,data=data)
full_mod = lm(price~accommodates+neighbourhood_group_cleansed+number_of_reviews+bathrooms+bedrooms+availability_90+availability_365+availability_30+number_of_reviews+guests_included+cancellation_policy+room_type,data=data)
forwardStepwise = step(start_mod,scope=list(upper=full_mod,lower=empty_mod),direction='forward')
summary(forwardStepwise)

#decision trees and random forest
-----------------------------------------------------------------------------------
  library(rpart); library(rpart.plot)
library(ROCR)

tree1 = rpart(price~accommodates+review_scores_rating+latitude+longitude+neighbourhood_group_cleansed+availability_365+availability_30+guests_included+number_of_reviews+room_type,data)
pred = predict(tree1)
rmse4 = sqrt(mean((pred-data$price)^2))
rmse4

install.packages('randomForest')
library(randomForest)
library(caret)
trControl=trainControl(method="cv",number=10)
tuneGrid = expand.grid(mtry=1:5)
cvForest = train(price~accommodates+review_scores_rating+latitude+longitude+neighbourhood_group_cleansed+availability_365+availability_30+guests_included+number_of_reviews+room_type,data=data,method="rf",ntree=1000,trControl=trControl,tuneGrid=tuneGrid )
cvForest

#add more feature (host_is_superhost+bedrooms+bathrooms+review_scores_cleanliness+review_scores_checkin+review_scores_communication+review_scores_value+is_business_travel_ready)
forest = randomForest(price~accommodates+review_scores_rating+latitude+longitude+neighbourhood_group_cleansed+availability_365+availability_30+guests_included+number_of_reviews+room_type+host_is_superhost+bedrooms+bathrooms+review_scores_cleanliness+review_scores_checkin+review_scores_communication+review_scores_value+is_business_travel_ready,data,ntree = 100)
predForest = predict(forest)
rmse4 = sqrt(mean((predForest-data$price)^2))
rmse4

pred = predict(forest,newdata=scoringData)
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'submission_8.csv',row.names = F)

#try to use ntree=1000, add more features
forest2 = randomForest(price~accommodates+review_scores_rating+latitude+longitude+neighbourhood_group_cleansed+availability_365+availability_30+guests_included+number_of_reviews+room_type+host_is_superhost+host_response_time+extra_people+minimum_nights+bedrooms+bathrooms+review_scores_cleanliness+review_scores_checkin+review_scores_communication+review_scores_value+review_scores_location+is_business_travel_ready+cancellation_policy,data,ntree = 1000)
predForest2 = predict(forest2)
rmse_f2 = sqrt(mean((predForest2-data$price)^2))
rmse_f2

pred = predict(forest2,newdata=scoringData)
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'submission_9.csv',row.names = F)

#try to use boosting (with differnt parameters)
library(gbm)
boost = gbm(price~accommodates+review_scores_rating+latitude+longitude+neighbourhood_group_cleansed+availability_365+availability_30+guests_included+number_of_reviews+room_type+bedrooms+bathrooms,data,distribution="gaussian",n.trees = 500,interaction.depth = 5,shrinkage = 0.04)
predBoost = predict(boost,n.trees = 500,newdata=scoringData,type='response')
rmse4 = sqrt(mean((predBoost-data$price)^2))
rmse4

boost = gbm(price~.,data,distribution="gaussian",n.trees = 100,interaction.depth = 3,shrinkage = 0.001)
predBoost = predict(boost,n.trees = 100,newdata=scoringData,type='response')
rmse4 = sqrt(mean((predBoost-data$price)^2))
rmse4

