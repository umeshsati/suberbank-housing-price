library(readr)
library(xgboost)
#library(data.table)

#test <- read_csv("D:/Data Science/Kaggle/Russian house prize/test.csv")

#data_test <- read_csv("D:/Data Science/Kaggle/Russian house prize/data_test_xgb.csv")


data_train <- read_csv("D:/Data Science/Kaggle/Russian house prize/data_train_xgb.csv")


data_train <- data_train[!(names(data_train) %in% c("timestamp"))]

#data_test <- data_test[!(names(data_test)%in% c("timestamp"))]
data_train[is.na(data_train)] <- -999.9 

#setDT(data_train) 

#sapply(data_train, class)

#for (i in names(data_train)[which(sapply(data_train,is.integer) == 1)]) {
#  if (class(data_train[[i]])=="integer") {
 #       data_train[[i]] <- as.numeric(data_train[[i]])
 # }
#}


library(caTools)

split = sample.split(data_train, SplitRatio = 0.75)

train_set = subset(data_train,split==TRUE)
cv_set = subset(data_train,split==FALSE)



labl_train <- train_set$price_doc
labl_cv <- cv_set$price_doc
#labl <- data_train$price_doc


train_set <- train_set[!(names(train_set) %in% c("price_doc","id"))]
cv_set <- cv_set[!(names(cv_set) %in% c("price_doc","id"))]
#data_train <- data_train[,-c("price_doc")]


xgbm1 <- xgboost(data= data.matrix(train_set),
                 label = labl_train, 
                 eta = 0.001,
                 max_depth = 15, 
                 nround=2500, 
                 subsample = 0.5,
                 colsample_bytree = 0.5,
                 seed = 1,
                 eval_metric = "rmse",
                 objective = "reg:linear"
                 
)

min(xgbm1$evaluation_log$train_rmse)

param = list(eta = 0.01, max_depth = 15,nround=2500,subsample = 0.5,colsample_bytree = 0.5,seed = 1,eval_metric = "rmse",objective = "reg:linear")




#xgb1 <- xgb.train(params = param, data= data.matrix(data_train), label= labl, nfolds =5 )

pred = predict(xgbm1,data.matrix(cv_set))

#id1 =test$id

#write.csv(cbind(id1,predxgb), "D:/prediction_xgb.csv")

model_rmse = sqrt(mean((labl_cv-pred)^2))

features <- xgb.importance(feature_names = colnames(train_set), model = xgbm1)

xgb.plot.importance(importance_matrix = features[1:30])

features$Feature[1:30]

#### implement the model on top 30 features

xgbm12 <- xgboost(data= data.matrix(train_set[features$Feature[1:30]]),
                 label = labl_train, 
                 eta = 0.001,
                 max_depth = 15, 
                 nround=2500, 
                 subsample = 0.5,
                 colsample_bytree = 0.5,
                 seed = 1,
                 eval_metric = "rmse",
                 objective = "reg:linear"
                 
)

min(xgbm12$evaluation_log$train_rmse)

pred1 = predict(xgbm12,data.matrix(cv_set[features$Feature[1:30]]))

model_rmse1 = sqrt(mean((labl_cv-pred1)^2))

