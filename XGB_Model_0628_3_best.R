library(readr)
library(psych)
library(xgboost)
trainset <- read_csv("D:/Data Science/Kaggle/Russian house prize/train.csv")


trainset$build_year <- as.numeric(trainset$build_year)
trainset$max_floor <- as.numeric(trainset$max_floor)
trainset$num_room <- as.numeric(trainset$num_room)
trainset$kitch_sq <- as.numeric(trainset$kitch_sq)

names(trainset)[which(sapply(trainset,is.character) == 1)]

for (i in names(trainset)[which(sapply(trainset,is.character) == 1)]) {
  if (class(trainset[[i]])=="character") {
    levels <- unique(c(trainset[[i]]))
    trainset[[i]] <- as.numeric(factor(trainset[[i]], levels=levels))
  }
}


##### mode funcation
getmode <- function(v) {
  v<-na.omit(v)
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


######################### Build year data exploration 

trainset$build_year[trainset$build_year==20052009] <- 2007 ### missing entry in data
trainset$build_year[trainset$build_year==4965] <- 1965  ### missing entry in data
#trainset$build_year[trainset$build_year==0] <- 2000  ### missing entry in data
#trainset$build_year[trainset$build_year==1] <- 2001  ### missing entry in data
#trainset$build_year[trainset$build_year==3] <- 2003  ### missing entry in data
#trainset$build_year = 2017 - trainset$build_year   #### convert build year into age of house



####  life sq ,full sq  and kitchen_sq

trainset = trainset[which(trainset$full_sq <5326),]
trainset$full_sq[trainset$full_sq <10] <- median(trainset$full_sq,na.rm = T)

trainset$life_sq[which(trainset$life_sq < trainset$kitch_sq)] <-  trainset$kitch_sq[which(trainset$life_sq < trainset$kitch_sq)]

trainset$full_sq[which(trainset$full_sq < trainset$life_sq)] <-  trainset$life_sq[which(trainset$full_sq < trainset$life_sq)]

#trainset$kitch_sq[which(trainset$full_sq < trainset$kitch_sq)] <- median(trainset$kitch_sq,na.rm = T)
trainset$state[which(trainset$state ==33)] = 3
trainset$build_year[which(trainset$build_year < 1691)] <- NA




trainset$max_floor[which(trainset$max_floor < trainset$floor)] <- NA
                                                                                              
                                                                                              
                                                                                                                                                                                           
# Date features
trainset$timestamp <- as.Date(trainset$timestamp)
trainset$date_month = months(trainset$timestamp)
trainset$date_year = year(trainset$timestamp)
trainset$date_week = week(trainset$timestamp)

trainset$quater = ifelse(trainset$date_month >0 & trainset$date_month <4, 1,
                         ifelse(trainset$date_month >3 & trainset$date_month <7, 2,
                                ifelse(trainset$date_month >6 & trainset$date_month <10, 3,4
                                       )))
                                
table(trainset$quater)

plot(trainset$quater, trainset$price_doc, type="p")

#################### quaterly rate ##############
rate_2016_q2 = 1
rate_2016_q1 = rate_2016_q2 / .99903
rate_2015_q4 = rate_2016_q1 / .9831
rate_2015_q3 = rate_2015_q4 / .9834
rate_2015_q2 = rate_2015_q3 / .9815
rate_2015_q1 = rate_2015_q2 / .9932
rate_2014_q4 = rate_2015_q1 / 1.0112
rate_2014_q3 = rate_2014_q4 / 1.0169
rate_2014_q2 = rate_2014_q3 / 1.0086
rate_2014_q1 = rate_2014_q2 / 1.0126
rate_2013_q4 = rate_2014_q1 / 0.9902
rate_2013_q3 = rate_2013_q4 / 1.0041
rate_2013_q2 = rate_2013_q3 / 1.0044
rate_2013_q1 = rate_2013_q2 / 1.0104
rate_2012_q4 = rate_2013_q1 / 0.9832
rate_2012_q3 = rate_2012_q4 / 1.0277
rate_2012_q2 = rate_2012_q3 / 1.0279
rate_2012_q1 = rate_2012_q2 / 1.0279
rate_2011_q4 = rate_2012_q1 / 1.076
rate_2011_q3 = rate_2011_q4 / 1.0236
rate_2011_q2 = rate_2011_q3 / 1
rate_2011_q1 = rate_2011_q2 / 1.011

trainset$rate= 1
### 2011
trainset$rate[which(trainset$date_year==2011 & trainset$quater ==1)] <- rate_2011_q1
trainset$rate[which(trainset$date_year==2011 & trainset$quater ==2)] <- rate_2011_q2
trainset$rate[which(trainset$date_year==2011 & trainset$quater ==3)] <- rate_2011_q3
trainset$rate[which(trainset$date_year==2011 & trainset$quater ==4)] <- rate_2011_q4

##### 2012

trainset$rate[which(trainset$date_year==2012 & trainset$quater ==1)] <- rate_2012_q1
trainset$rate[which(trainset$date_year==2012 & trainset$quater ==2)] <- rate_2012_q2
trainset$rate[which(trainset$date_year==2012 & trainset$quater ==3)] <- rate_2012_q3
trainset$rate[which(trainset$date_year==2012 & trainset$quater ==4)] <- rate_2012_q4

######## 2013

trainset$rate[which(trainset$date_year==2013 & trainset$quater ==1)] <- rate_2013_q1
trainset$rate[which(trainset$date_year==2013 & trainset$quater ==2)] <- rate_2013_q2
trainset$rate[which(trainset$date_year==2013 & trainset$quater ==3)] <- rate_2013_q3
trainset$rate[which(trainset$date_year==2013 & trainset$quater ==4)] <- rate_2013_q4

###### 2014

trainset$rate[which(trainset$date_year==2014 & trainset$quater ==1)] <- rate_2014_q1
trainset$rate[which(trainset$date_year==2014 & trainset$quater ==2)] <- rate_2014_q2
trainset$rate[which(trainset$date_year==2014 & trainset$quater ==3)] <- rate_2014_q3
trainset$rate[which(trainset$date_year==2014 & trainset$quater ==4)] <- rate_2014_q4

########### 2015
trainset$rate[which(trainset$date_year==2015 & trainset$quater ==1)] <- rate_2015_q1
trainset$rate[which(trainset$date_year==2015 & trainset$quater ==2)] <- rate_2015_q2
trainset$rate[which(trainset$date_year==2015 & trainset$quater ==3)] <- rate_2015_q3
trainset$rate[which(trainset$date_year==2015 & trainset$quater ==4)] <- rate_2015_q4


###update Price on training set
#trainset$price_doc <- trainset$price_doc * trainset$rate

#plot(trainset$price_doc)
#tail(sort(trainset$price_doc))
trainset = trainset[which(trainset$price_doc < 91066096 ),]
#trainset <- trainset[!(names(trainset) %in% c("timestamp","id"))]


######### split data into training and CV dataset

#library(caTools)

#split = sample.split(trainset, SplitRatio = 0.75)

#train_set = subset(trainset,split==TRUE)
#cv_set = subset(trainset,split==FALSE)

train_set = trainset

labl_train <- train_set$price_doc
#labl_cv <- cv_set$price_doc

train_set <- train_set[!(names(train_set) %in% c("price_doc","id"))]
#cv_set <- cv_set[!(names(cv_set) %in% c("price_doc"))]
train_set = train_set[imp_features]
#cv_set = cv_set[imp_features]
###### convert data into matrix

train_set <- xgb.DMatrix(data.matrix(train_set),label= data.matrix(labl_train) )
#cv_set <- xgb.DMatrix(data.matrix(cv_set))



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


'''

##################################################


param = list(eta = 0.05, max_depth = 10,
             nround=2500,subsample = 0.5,colsample_bytree = 0.8,seed = 1,
             eval_metric = "rmse",objective = "reg:linear",min_child_weight=4,
             lambda_bias=5, gama=0)

xgbm1 <- xgb.cv(params = param, data=train_set,nround=300, nfold = 5)

min(xgbm1$evaluation_log$test_rmse_mean)



xgbmodel1 <- xgb.train(params = param, data=train_set,nround=181, nfold = 5)




'''
pred = predict(xgbmodel1,cv_set)

pred = expm1(pred)
labl_cv = expm1(labl_cv)
head(pred)
head(labl_cv)
model_rmse = sqrt(mean((labl_cv-pred)^2))
sum(labl_cv-pred)
model_remse1 =model_rmse
'''
#####################################Feature importance #################

features <- xgb.importance(feature_names = colnames(train_set), model = xgbmodel1)

xgb.plot.importance(importance_matrix = features[1:70])

imp_features = features$Feature[1:70]

sum(features$Gain[1:35])

sum(features$Gain[1:70])







############ test data #####
test <- read_csv("D:/Data Science/Kaggle/Russian house prize/test.csv")

id = test$id
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



test$build_year[which(test$build_year < 1875)] <- NA

#### full Sq life Sq
test$full_sq[test$full_sq <10] <- median(test$full_sq,na.rm = T)

test$life_sq[which(test$life_sq < test$kitch_sq)] <-  test$kitch_sq[which(test$life_sq < test$kitch_sq)]

test$full_sq[which(test$full_sq < test$life_sq)] <-  test$life_sq[which(test$full_sq < test$life_sq)]

###########max floor
test$max_floor[which(test$max_floor < test$floor)] <- NA



# Date features
test$timestamp <- as.Date(test$timestamp)
test$date_month = month(test$timestamp)
test$date_year = year(test$timestamp)
test$date_week = week(test$timestamp)

test$quater = ifelse(test$date_month >0 & test$date_month <4, 1,
                     ifelse(test$date_month >3 & test$date_month <7, 2,
                            ifelse(test$date_month >6 & test$date_month <10, 3,4
                            )))

########################### conver rate on qquaterly bases
test$rate= 1
### 2011
test$rate[which(test$date_year==2011 & test$quater ==1)] <- rate_2011_q1
test$rate[which(test$date_year==2011 & test$quater ==2)] <- rate_2011_q2
test$rate[which(test$date_year==2011 & test$quater ==3)] <- rate_2011_q3
test$rate[which(test$date_year==2011 & test$quater ==4)] <- rate_2011_q4

##### 2012

test$rate[which(test$date_year==2012 & test$quater ==1)] <- rate_2012_q1
test$rate[which(test$date_year==2012 & test$quater ==2)] <- rate_2012_q2
test$rate[which(test$date_year==2012 & test$quater ==3)] <- rate_2012_q3
test$rate[which(test$date_year==2012 & test$quater ==4)] <- rate_2012_q4

######## 2013

test$rate[which(test$date_year==2013 & test$quater ==1)] <- rate_2013_q1
test$rate[which(test$date_year==2013 & test$quater ==2)] <- rate_2013_q2
test$rate[which(test$date_year==2013 & test$quater ==3)] <- rate_2013_q3
test$rate[which(test$date_year==2013 & test$quater ==4)] <- rate_2013_q4

###### 2014

test$rate[which(test$date_year==2014 & test$quater ==1)] <- rate_2014_q1
test$rate[which(test$date_year==2014 & test$quater ==2)] <- rate_2014_q2
test$rate[which(test$date_year==2014 & test$quater ==3)] <- rate_2014_q3
test$rate[which(test$date_year==2014 & test$quater ==4)] <- rate_2014_q4

########### 2015
test$rate[which(test$date_year==2015 & test$quater ==1)] <- rate_2015_q1
test$rate[which(test$date_year==2015 & test$quater ==2)] <- rate_2015_q2
test$rate[which(test$date_year==2015 & test$quater ==3)] <- rate_2015_q3
test$rate[which(test$date_year==2015 & test$quater ==4)] <- rate_2015_q4


########### 2016
test$rate[which(test$date_year==2016 & test$quater ==1)] <- rate_2016_q1
test$rate[which(test$date_year==2016 & test$quater ==2)] <- rate_2016_q2













test <- test[!(names(test) %in% c("id"))]


test= test[imp_features]
#test <- test[!(names(test) %in% c("life_sq"))]
testset <- xgb.DMatrix(data.matrix(test))
pred1 = predict(xgbmodel1,testset)

View(as.data.frame(pred1))
#perd_all = pred1
sum(pred1)
mean(pred1)
price_doc = pred1
mean(perd_all)
plot(pred1)
options(scipen = 999)
######### export prediction to csv file 

write.csv(cbind(id,price_doc), "D:/Data Science/Kaggle/Russian house prize/pred/prediction_0628_3.csv",row.names = F)
