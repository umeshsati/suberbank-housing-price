library(readr)
library(psych)
library(xgboost)

trainset <- read_csv("D:/Data Science/Kaggle/Russian house prize/train.csv")



######## macro data
#macro <- read_csv("D:/Data Science/Kaggle/Russian house prize/macro.csv")

''' timestamp,balance_trade,balance_trade_growth,eurrub,
average_provision_of_build_contract,
micex_rgbi_tr,
micex_cbi_tr, 
deposits_rate, 
mortgage_value, mortgage_rate,
income_per_cap, 
museum_visitis_per_100_cap,
cpi,
apartment_build)'''













#######################
trainset$max_floor[is.na(trainset$max_floor)] <- median(trainset$max_floor,na.rm = T)

trainset$build_year <- as.numeric(trainset$build_year)
trainset$max_floor <- as.numeric(trainset$max_floor)
trainset$num_room <- as.numeric(trainset$num_room)
trainset$kitch_sq <- as.numeric(trainset$kitch_sq)


trainset$max_floor[is.na(trainset$max_floor)] <- median(trainset$max_floor,na.rm = T)
names(trainset)[which(sapply(trainset,is.character) == 1)]


trainset$material[is.na(trainset$material)] <-  "Missing"
trainset$state[is.na(trainset$state)] <-  "Missing"
trainset$product_type[is.na(trainset$product_type)] <-  "Missing"
trainset$sub_area[is.na(trainset$sub_area)] <-  "Missing"
trainset$culture_objects_top_25[is.na(trainset$culture_objects_top_25)] <-  "Missing"
trainset$thermal_power_plant_raion[is.na(trainset$thermal_power_plant_raion)] <-  "Missing"
trainset$incineration_raion[is.na(trainset$incineration_raion)] <-  "Missing"
trainset$oil_chemistry_raion[is.na(trainset$oil_chemistry_raion)] <-  "Missing"
trainset$radiation_raion[is.na(trainset$radiation_raion)] <-  "Missing"
trainset$railroad_terminal_raion[is.na(trainset$railroad_terminal_raion)] <-  "Missing"
trainset$big_market_raion[is.na(trainset$big_market_raion)] <-  "Missing"
trainset$nuclear_reactor_raion[is.na(trainset$nuclear_reactor_raion)] <-  "Missing"
trainset$detention_facility_raion[is.na(trainset$detention_facility_raion)] <-  "Missing"
trainset$water_1line[is.na(trainset$water_1line)] <-  "Missing"
trainset$big_road1_1line[is.na(trainset$big_road1_1line)] <-  "Missing"
trainset$railroad_1line[is.na(trainset$railroad_1line)] <-  "Missing"
trainset$ecology[is.na(trainset$ecology)] <-  "Missing"

for (i in names(trainset)[which(sapply(trainset,is.character) == 1)]) {
  if(is.na(trainset[[i]])==1){}
  if (class(trainset[[i]])=="character") {
    levels <- unique(c(trainset[[i]]))
    trainset[[i]] <- as.numeric(factor(trainset[[i]], levels=levels))
  }
}



######################### Build year data exploration 

trainset$build_year[trainset$build_year==20052009] <- 2007 ### missing entry in data
trainset$build_year[trainset$build_year==4965] <- 1965  ### missing entry in data
#trainset$build_year[trainset$build_year==0] <- 2000  ### missing entry in data
#trainset$build_year[trainset$build_year==1] <- 2001  ### missing entry in data
#trainset$build_year[trainset$build_year==3] <- 2003  ### missing entry in data
#trainset$build_year = 2017 - trainset$build_year   #### convert build year into age of house


####  life sq ,full sq  andkitchen_sq
trainset$full_sq[trainset$full_sq <10] <- median(trainset$full_sq,na.rm = T)
trainset$full_sq[which(trainset$full_sq < trainset$life_sq)] <- median(trainset$full_sq,na.rm = T) 
trainset$life_sq[which(trainset$full_sq < trainset$life_sq)] <- median(trainset$life_sq,na.rm = T)

trainset$kitch_sq[which(trainset$full_sq < trainset$kitch_sq)] <- median(trainset$kitch_sq,na.rm = T)




trainset <- trainset[!(names(trainset) %in% c("timestamp","id"))]


######### split data into training and CV dataset

library(caTools)

split = sample.split(trainset, SplitRatio = 0.75)

train_set = subset(trainset,split==TRUE)
cv_set = subset(trainset,split==FALSE)



labl_train <- train_set$price_doc
labl_cv <- cv_set$price_doc

train_set <- train_set[!(names(train_set) %in% c("price_doc"))]
cv_set <- cv_set[!(names(cv_set) %in% c("price_doc"))]
train_set = train_set[imp_features]
cv_set = cv_set[imp_features]
###### convert data into matrix

train_set <- xgb.DMatrix(data.matrix(train_set),label= data.matrix(labl_train) )
cv_set <- xgb.DMatrix(data.matrix(cv_set))



##################### model building 
##### model with best prediction so far
'''#param = list(eta = 0.07, max_depth = 10,
             nround=2500,subsample = 0.5,colsample_bytree = 0.8,seed = 1,
             eval_metric = "rmse",objective = "reg:linear",min_child_weight=5,
             alpha=0.1, gama= 5, lambda=5 )
xgbm1 <- xgb.cv(params = param, data=train_set,nround=500, nfold = 5)

################### latest 
param = list(eta = 0.07, max_depth = 10,
             nround=2500,subsample = 0.5,colsample_bytree = 0.8,seed = 1,
             eval_metric = "rmse",objective = "reg:linear",min_child_weight=5,
             alpha=0.1, gama= 4, lambda=4)
xgbm1 <- xgb.cv(params = param, data=train_set,nround=500, nfold = 5)

min(xgbm1$evaluation_log$test_rmse_mean)

xgbmodel1 <- xgb.train(params = param, data=train_set,nround=137, nfold = 5)

features = 40

latest with 35 

param = list(eta = 0.07, max_depth = 10,
             nround=2500,subsample = 0.5,colsample_bytree = 0.8,seed = 1,
eval_metric = "rmse",objective = "reg:linear",min_child_weight=5,
alpha=0.1, gama= 4, lambda=4)
xgbm1 <- xgb.cv(params = param, data=train_set,nround=500, nfold = 5)
'''

##################################################



param = list(eta = 0.07, max_depth = 10,
             nround=2500,subsample = 0.5,colsample_bytree = 0.8,seed = 1,
             eval_metric = "rmse",objective = "reg:linear",min_child_weight=5,
             alpha=0.1, gama= 4, lambda=4)
xgbm1 <- xgb.cv(params = param, data=train_set,nround=500, nfold = 5)

min(xgbm1$evaluation_log$test_rmse_mean)

xgbmodel1 <- xgb.train(params = param, data=train_set,nround=190, nfold = 5)


pred = predict(xgbmodel1,cv_set)

head(pred)
head(labl_cv)
model_rmse = sqrt(mean((labl_cv-pred)^2))
sum(labl_cv-pred)
model_remse1 =model_rmse

#####################################Feature importance #################

features <- xgb.importance(feature_names = colnames(train_set), model = xgbmodel1)

xgb.plot.importance(importance_matrix = features[1:30])

imp_features = features$Feature[1:35]

sum(features$Gain[1:30])









############ test data #####
test <- read_csv("D:/Data Science/Kaggle/Russian house prize/test.csv")

test$build_year <- as.numeric(test$build_year)
test$max_floor <- as.numeric(test$max_floor)
test$num_room <- as.numeric(test$num_room)
test$kitch_sq <- as.numeric(test$kitch_sq)

names(test)[which(sapply(test,is.character) == 1)]

for (i in names(test)[which(sapply(test,is.character) == 1)]) {
  if (class(test[[i]])=="character") {
    levels <- unique(c(test[[i]]))
    test[[i]] <- as.numeric(factor(test[[i]], levels=levels))
  }
}



test <- test[!(names(test) %in% c("timestamp"))]

id = test$id

test= test[imp_features]
testset <- xgb.DMatrix(data.matrix(test))
pred1 = predict(xgbmodel1,testset)

View(as.data.frame(pred1))


price_doc = pred1


######### export prediction to csv file 

write.csv(cbind(id,price_doc), "D:/Data Science/Kaggle/Russian house prize/pred/prediction0623_3.csv",  row.names=FALSE)
