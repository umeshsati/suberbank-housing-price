library(readr)
library(car)
library(psych)
library(robustHD)
library(corpcor)
library(GPArotation)
library(ggplot2)
library(ggfortify)
library(plyr)
library(glpot)
library(reshape)
## read dataset
train <- read_csv("D:/Data Science/Kaggle/Russian house prize/train.csv")

train$build_year <- as.numeric(train$build_year)


for (i in names(train)[which(sapply(train,is.character) == 1)]) {
  if (class(train[[i]])=="character") {
    levels <- unique(c(train[[i]]))
    train[[i]] <- as.numeric(factor(train[[i]], levels=levels))
  }
}

### function to get mode

getmode <- function(v) {
  v<-na.omit(v)
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#### deal with the missing data
train$state[is.na(train$state)] <- getmode(train$state) ### fill with Median of state
plot(table(train$material))
train$material[is.na(train$material)] <- 1  #### fill missing value with most fabured matrial


names(sort(table(train$build_year)))[nchar(names(sort(table(train$build_year)))) != 4]
train$build_year[which(train$build_year==215)] <-2015
train$build_year[which(train$build_year==0)] <-2000
train$build_year[which(train$build_year==20052009)] <-2005
train$build_year[which(train$build_year==20)] <-2000
train$build_year[which(train$build_year==71)] <-1971
train$build_year[which(train$build_year==3)] <-2003
train$build_year[which(train$build_year==1)] <-2001



#train$build_year[is.na(train$build_year)] <- 0  ### fill with Mode


prop_live = mean(train$life_sq, na.rm = TRUE)/mean(train$full_sq)
prop_kitchen = mean(train$kitch_sq, na.rm =TRUE)/mean(train$life_sq, na.rm = TRUE)
prop_room = mean(train$life_sq, na.rm = TRUE)/mean(train$num_room, na.rm = TRUE)

for (i in 1:length(train$full_sq)){
  if (sapply(train$life_sq[i], is.na))
    train$life_sq[i] <- train$full_sq[i]*prop_live
}

for (i in 1:length(train$life_sq)){
  if (sapply(train$kitch_sq[i], is.na))
    train$kitch_sq[i] <- train$life_sq[i]*prop_kitchen
}

for (i in 1:length(train$life_sq)){
  if (sapply(train$num_room[i], is.na))
    train$num_room[i] <- train$life_sq[i]/prop_room
}


train$floor[is.na(train$floor)] <- 1

for (i in 1:length(train$floor)){
  if (train$floor[i] <= 5 )
    train$max_floor[is.na(train$max_floor)] <- 5
else
  train$max_floor[is.na(train$max_floor)] <- train$floor[i] + 1
}

train[is.na(train)]<- 0


#### Exploretry analysis

train <- train[!(names(train) %in% c("timestamp"))]

y = train$price_doc

train <- train[!(names(train) %in% c("price_doc"))]


##### AIC regression ###

regressor <- lm(y ~ ., data= train)

p_val <- as.data.frame(coef(summary(regressor))[, "Pr(>|t|)"])
p_val$var <- row.names(p_val)
colnames(p_val)[1] <- "pvalue"
signvar <- p_val$var[which(p_val > 0.5)]
train_model <- train[,!(names(train)%in% signvar)]

pvaluemax <- 1
while(pvaluemax > 0.05){
  regressor <- lm(y ~ ., data = train_model)
  p_val <- as.data.frame(coef(summary(regressor))[, "Pr(>|t|)"])
  p_val$var <- row.names(p_val)
  colnames(p_val)[1] <- "pvalue"
  signvar <- p_val$var[which(p_val == max(p_val$pvalue))]
  pvaluemax <- max(p_val$pvalue)
  if (pvaluemax > 0.05)
    train_model <- train_model[,!(names(train_model)%in% signvar)]
  else
    break()
}
  

cormat <- cor(train_model)

##### regression analysis

##y = train$price_doc

#train <- train[!(names(train) %in% c("price_doc"))]

#regressor <- lm(y ~ ., data= train)

#summary(regressor)

#### plot error and residuals

#plot(x=fitted(regressor), y=residuals(regressor), pannel.last = abline(h=0, lty= 2))

#######  Normality check and transformation of data set
sand_x <- standardize(train)
cormats <- cor(sand_x)
hist(sand_x[,"max_floor"])

scaled_X <- scale(train)
scaled_y <- scale(y)
hist(scaled_X[,"max_floor"])
hist(log(train$full_sq))
hist(scaled_y)

##### check for diamension reducation

#calculating manually the Bartlett's sphericity test
n <- nrow(sand_x)
p <- ncol(sand_x)
chi2 <- -(n-1-(2*p+5)/6)*log(det(cormat))
ddl <- p*(p-1)/2
print(chi2)
print(ddl)
print(pchisq(chi2,ddl,lower.tail=F))


print(cortest.bartlett(cormat, nrow(train_model)))

####KMO Test###
dim(cormat)
invR <- solve(cormat)
#partial correlation matrix (-1 * spss anti-image matrix, unless the diagonal)
A <- matrix(1,nrow(invR),ncol(invR))
for (i in 1:nrow(invR)-1){
  for (j in (i+1):ncol(invR)){
    #above the diagonal
    A[i,j] <- -invR[i,j]/sqrt(invR[i,i]*invR[j,j])
    #below the diagonal
    A[j,i] <- A[i,j]
  }
}
colnames(A) <- colnames(train)
rownames(A) <- colnames(train)
print(A)
#KMO
kmo.num <- sum(cormat^2) - sum(diag(cormat^2))
kmo.denom <- kmo.num + (sum(A^2) - sum(diag(A^2)))
kmo <- kmo.num/kmo.denom
# Reporting the conclusion
print(kmo)
{
  if (kmo >= 0.00 && kmo < 0.50){test <- 'The KMO test yields a degree of common variance unacceptable for FA.'} 
  else if (kmo >= 0.50 && kmo < 0.60){test <- 'The KMO test yields a degree of common variance miserable.'} 
  else if (kmo >= 0.60 && kmo < 0.70){test <- 'The KMO test yields a degree of common variance mediocre.'} 
  else if (kmo >= 0.70 && kmo < 0.80){test <- 'The KMO test yields a degree of common variance middling.' } 
  else if (kmo >= 0.80 && kmo < 0.90){test <- 'The KMO test yields a degree of common variance meritorious.' }
  else { test <- 'The KMO test yields a degree of common variance marvelous.' }
  
  
}
test





train.pca <- princomp(train_model,cor=T)