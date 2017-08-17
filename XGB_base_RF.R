library(readr)
library(xgboost)


trainset <- read_csv("D:/Data Science/Kaggle/Russian house prize/forXGB1906.csv")

testset <- read_csv("D:/Data Science/Kaggle/Russian house prize/fortestXGB1906.csv")

test <- read_csv("D:/Data Science/Kaggle/Russian house prize/test.csv")

testset <- read_csv("D:/Data Science/Kaggle/Russian house prize/train.csv")

median(test$life_sq, na.rm = T)
median(testset$life_sq, na.rm = T)


names(trainset)[which(sapply(trainset,is.character) == 1)]

trainset$build_year= as.numeric(trainset$build_year)

trainset$kitch_sq = as.numeric(trainset$kitch_sq)


trainset$build_year[trainset$build_year==20052009] <- 2007 ### missing entry in data
trainset$build_year[trainset$build_year==4965] <- 1965  ### missing entry in data
trainset$build_year[trainset$build_year==0] <- 2000  ### missing entry in data
trainset$build_year[trainset$build_year==1] <- 2001  ### missing entry in data
trainset$build_year[trainset$build_year==3] <- 2003  ### missing entry in data
trainset$build_year = 2017 - trainset$build_year   #### convert build year into age of house

trainset$full_sq[trainset$full_sq <10] <- NA


trainset <- trainset[!(names(trainset) %in% c("timestamp"))]

#trainset[is.na(trainset)] <- -999




library(caTools)

split = sample.split(trainset, SplitRatio = 0.75)

train_set = subset(trainset,split==TRUE)
cv_set = subset(trainset,split==FALSE)



labl_train <- train_set$price_doc
labl_cv <- cv_set$price_doc
#labl <- data_train$price_doc


train_set <- train_set[!(names(train_set) %in% c("price_doc"))]
cv_set <- cv_set[!(names(cv_set) %in% c("price_doc"))]

train_set <- xgb.DMatrix(data.matrix(train_set),label= data.matrix(labl_train) )
cv_set <- xgb.DMatrix(data.matrix(cv_set))

##xgbm1 <- xgboost(data= data.matrix(train_set),
#                 label = labl_train, 
#                 eta = 0.06,
#                 max_depth = 4, 
 #                nround=4000, 
#                 subsample = 0.5,
 #                colsample_bytree = 0.5,
 #                seed = 1,
 #                eval_metric = "rmse",
#                 objective = "reg:linear"
                 
#)


param = list(eta = 0.07, max_depth = 100,
             nround=2500,subsample = 0.5,colsample_bytree = 0.8,seed = 1,
             eval_metric = "rmse",objective = "reg:linear",min_child_weight=5,
             alpha=0.01, gama= 0, lambda=0 )
xgbm1 <- xgb.cv(params = param, data=train_set,nround=200, nfold = 5)

min(xgbm1$evaluation_log$test_rmse_mean)

xgbmodel1 <- xgb.train(params = param, data=train_set,nround=80, nfold = 5)

#1010584 .5#4000   2738078
#802175
#796149
#939272  train-rmse:0.278192
pred = predict(xgbmodel1,cv_set)

head(pred)
head(labl_cv)
model_rmse = sqrt(mean((labl_cv-pred)^2))

model_remse1 =model_rmse


############################# prediction on test data
testset$build_year[testset$build_year==0] <- NA

testset <- testset[!(names(testset) %in% c("timestamp"))]

testset <- xgb.DMatrix(data.matrix(testset))
pred1 = predict(xgbmodel1,testset)

View(as.data.frame(pred1))

id = test$id
price_doc = pred1


######### export prediction to csv file 

write.csv(cbind(id,price_doc), "D:/prediction_0621_1.csv")
