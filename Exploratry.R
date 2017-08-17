library(readr)
library(car)
library(psych)
library(robustHD)
library(corpcor)
library(GPArotation)
library(ggplot2)
library(ggfortify)
library(nFactors)
library(plyr)
library(glpot)
library(reshape)
## read dataset
train123 <- read_csv("D:/Data Science/Kaggle/Russian house prize/train.csv")

View (as.data.frame(train$id[which(train$`build_year'=="NA"]))

factvar <- c("num_room","kitch_sq","sub_area","max_floor","build_year")

for (i in factvar){train[[i]]<- as.numeric(train[[i]])}
charvar <- names(train)[which(sapply(train,is.character) == 1)]
for (i in names(train)[which(sapply(train,is.character) == 1)]) {
  if (class(train[[i]])=="character") {
    levels <- unique(c(train[[i]]))
    train[[i]] <- as.factor(train[[i]])
  }
}

#### deal with the missing data

train$state[is.na(train$state)] <- tail(names(sort(table(train$state))),1)  ### fill with Median of state
plot(table(train$material))
train$material[is.na(train$material)] <- 1  #### fill missing value with most common type


#train$build_year[is.na(train$build_year)] <- median(train$build_year, na.rm= TRUE)  ### fill with Median


prop_live = mean(train$life_sq, na.rm = TRUE)/mean(train$full_sq)
prop_kitchen = mean(train$kitch_sq, na.rm =TRUE)/mean(train$life_sq, na.rm = TRUE)
prop_room = mean(train$life_sq, na.rm = TRUE)/mean(train$num_room, na.rm = TRUE)

for (i in 1:length(train$full_sq)){
  if (sapply(train$life_sq[i], is.na))
    train$life_sq[i] <- round(train$full_sq[i]*prop_live , digits =0)
}

for (i in 1:length(train$life_sq)){
  if (sapply(train$kitch_sq[i], is.na))
    train$kitch_sq[i] <- round(train$life_sq[i]*prop_kitchen,digits =0)
}

for (i in 1:length(train$life_sq)){
  if (sapply(train$num_room[i], is.na))
    train$num_room[i] <- round(train$life_sq[i]/prop_room,digits =0)
}


train$floor[is.na(train$floor)] <- 1

for (i in 1:length(train$floor)){
  if (train$floor[i] <= 5 )
    train$max_floor[is.na(train$max_floor)] <- 5
  else
    train$max_floor[is.na(train$max_floor)] <- train$floor[i] + 1
}

train[is.na(train)]<- 0


train <- train[!(names(train) %in% c("timestamp"))]

y = train$price_doc

train <- train[!(names(train) %in% c("price_doc","id"))]




model.1 <- lm(log(y) ~ ., data=train)

summary(model.1)
######## model residuals plot


p_val <- as.data.frame(coef(summary(model.1))[, "Pr(>|t|)"])
p_val$var <- row.names(p_val)
colnames(p_val)[1] <- "pvalue"
signvar <- p_val$var[which(p_val > 0.5)]
train_model <- train[,!(names(train)%in% signvar)]
train_model$area_m <- scale(train_model$area_m)

model.1 <- lm(log(y) ~ ., data=train_model)

plot(x=fitted(model.1), y=residuals(model.1),
     panel.last = abline(h=0, lty=2))

qqnorm(residuals(model.1), main="", datax=TRUE)
qqline(residuals(model.1), datax=TRUE)

plot(predict(model.1), resid(model.1), pch ='.')

fitted(model.1)








####### exploratry data analysis
qplot(train$full_sq, train$life_sq, shape=am, color= am, facets= train$life_sq ~ train$full_sq, size=I(3),xlab="full house size", ylab="living area")

ggplot(train, aes(x= train$full_sq)) + geom_histogram()

plot(train$price_doc ~ train$timestamp, data=train)

points(train$price_doc ~ train$build_year, col="red")

points(train$state ~ train$build_year, col="blue")

ggplot(train,aes(x=train$timestamp, y=train$price_doc, col = "red"))+
  geom_line()

ggplot(train,aes(x=train$full_sq, y=train$num_room, col = "red"))+
  geom_point()

densityplot(train$max_floor, train)

plot(train$max_floor) 
boxplot(train$max_floor)


ggplot(train,aes(x=train$product_type, y=train$price_doc, col = "red"))+
  geom_point()

plot(train$product_type, train$price_doc)
































library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)



