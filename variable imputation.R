library(readr)
library(psych)
library(dplyr)

options(scipen = 999)
#train123 <- read_csv("D:/Data Science/Kaggle/Russian house prize/train.csv")
train_data <- read_csv("D:/Data Science/Kaggle/Russian house prize/train.csv")
train_data$build_year <- as.numeric(train_data$build_year)
train_data$kitch_sq <- as.numeric(train_data$kitch_sq)


### fULL sQUARE data
train_data$full_sq[which(train_data$full_sq <= (train_data$life_sq+train_data$kitch_sq))] <- NA
train_data$full_sq[which(train_data$full_sq < train_data$life_sq)] <- NA
#train_data$life_sq[which(is.na(train_data$full_sq)==1)] <- NA
#train_data$kitch_sq[which(is.na(train_data$full_sq)==1)] <- NA



View(train_data)
####  Number of Rooms#####
typeof(train_data$num_room)
train_data$num_room<- as.numeric(train_data$num_room)


attach(train_data)
#View(as.data.frame(aggregate(x=train_data$life_sq, by=list(sub_area),FUN = function(room_median){median(train_data$life_sq,na.rm = TRUE)})))

#####Area waise life_sq median is devided by  median no of room in NUM_room to achive no of room in missing values of num room
train_data$material = as.numeric(train_data$material)
train_data$max_floor = as.numeric(train_data$max_floor)
avg_life_sq <- as.data.frame(aggregate(train_data[,c(3:7,9)], by=(list(sub_area,ecology)), FUN = median,na.rm=TRUE))
#avg_life_sq$num-room <- as.data.frame(aggregate(train_data$num_room, by=(list(sub_area,ecology)), FUN = median,na.rm=TRUE))
train_data$max_floor[which(train_data$max_floor < train_data$floor)] <- NA


table(train_data$build_year)
table(is.na(train_data$max_floor))

View(as.data.frame(cor(train_data)))








for (i in 1:length(train_data)){
  if (is.na(train_data$max_floor[[i]])==1)
    train_data$max_floor[[i]] = avg_life_sq$max_floor[which(avg_life_sq$Group.1==train_data$sub_area[[i]] & avg_life_sq$Group.2==train_data$ecology[[i]])]

}


train_data$floor[which(train_data$max_floor < train_data$floor)] <- NA

for (i in 1:length(train_data)){
  if (is.na(train_data$floor[[i]])==1)
    train_data$floor[[i]] = avg_life_sq$floor[which(avg_life_sq$Group.1==train_data$sub_area[[i]] & avg_life_sq$Group.2==train_data$ecology[[i]])]
  
}







View(as.data.frame(train_data$max_floor[which(train_data$max_floor < train_data$floor)]))
table(train_data$max_floor[which(train_data$max_floor < train_data$floor)])



#### Avg one room size area wise
avg_life_sq$x <- round(avg_life_sq$x/2,0)
colnames(avg_life_sq)[1] = "sub_area"
colnames(avg_life_sq)[2] = "area_wise_room_size"
####


table(is.na(train_d$sub_area))
attach(train_data)
dim(train_data)
#train_d1 <- (merge(x=avg_life_sq, y=train_data, by="sub_area",all=TRUE))
train_data = (inner_join(train_data, avg_life_sq, by = c("sub_area", "sub_area")))
dim(train_data)
table(is.na(train_data$life_sq))
#View(train_data)
#colnames(train_data)
train_data$life_sq[which(train_data$life_sq < (as.integer(train_data$num_room) * train_data$x))] <- NA
View(train_data)

area_eco_df1 = (as.data.frame(aggregate(train_data[,3:10], by=(list(sub_area)), FUN = median,na.rm=TRUE)))
View(area_eco_df)





for (k in 1:length(avg_life_sq$sub_area)){
  for(i in 1:length(train_data$life_sq)){
    if (is.na(train_data$life_sq[[i]])==1)
      train_data$life_sq[[i]] = avg_life_sq$x[which(train_data$sub_area[[i]] == avg_life_sq$sub_area[[k]])] 
  }
}




###### Fill NA with median 
for (i in  1:length(train_data$life_sq)){
  if (is.na(train_data$life_sq[[i]]) ==1 )
    train_data$life_sq[[i]] <- avg_life_sq$x[which(avg_life_sq$Group.1==train_data$sub_area[[i]] & avg_life_sq$Group.2==train_data$ecology[[i]])]
  
}
table(is.na(train_data$life_sq))
#train_data$num_room[which(round(train_data$life_sq/train_data$x,0) < train_data$num_room)] <- NA

table(is.na(train_data$num_room))
temp_df <-as.data.frame(aggregate(train_data$num_room, by=(list(sub_area)), FUN = median,na.rm=TRUE))
temp_df$x <- round(temp_df$x,0)
#View(as.data.frame(round((aggregate(train_data$num_room, by=(list(sub_area)), FUN = median,na.rm=TRUE))[,2],0)))
###### Run hypothesis testing if there is diffrence in groups 
summary(aov(train_data$num_room ~ train_data$sub_area+train_data$ecology))
summary(aov(train_data$life_sq ~ train_data$sub_area+train_data$ecology))
summary(aov(train_data$full_sq ~ train_data$sub_area+train_data$ecology))


for (i in  1:length(train_data$num_room)){
  if (is.na(train_data$num_room[[i]]) ==1 | train_data$num_room[[i]] > round(train_data$life_sq[[i]]/train_data$x[[i]]))
    train_data$num_room[[i]] <- temp_df$x[which(temp_df==train_data$sub_area[[i]])]
    
}







### R Code

library(readr)
library(psych)
library(dplyr)

options(scipen = 999)
#train123 <- read_csv("D:/Data Science/Kaggle/Russian house prize/train.csv")
train_data <- read_csv("D:/Data Science/Kaggle/Russian house prize/train.csv")
train_data$build_year <- as.numeric(train_data$build_year)
train_data$kitch_sq <- as.numeric(train_data$kitch_sq)


### fULL sQUARE data
train_data$full_sq[which(train_data$full_sq <= (train_data$life_sq+train_data$kitch_sq))] <- NA
train_data$full_sq[which(train_data$full_sq < train_data$life_sq)] <- NA
train_data$life_sq[which(is.na(train_data$full_sq)==1)] <- NA
train_data$kitch_sq[which(is.na(train_data$full_sq)==1)] <- NA



View(train_data)
####  Number of Rooms#####
typeof(train_data$num_room)
train_data$num_room<- as.numeric(train_data$num_room)


attach(train_data)
#View(as.data.frame(aggregate(x=train_data$life_sq, by=list(sub_area),FUN = function(room_median){median(train_data$life_sq,na.rm = TRUE)})))

#####Area waise life_sq median is devided by  median no of room in NUM_room to achive no of room in missing values of num room
avg_life_sq <- as.data.frame(aggregate(train_data$life_sq, by=(list(sub_area)), FUN = median,na.rm=TRUE))


#### Avg one room size area wise
avg_life_sq$x <- round(avg_life_sq$x/2,0)
colnames(avg_life_sq)[1] = "sub_area"
colnames(avg_life_sq)[2] = "area_wise_room_size"
####


table(is.na(train_d$sub_area))
attach(train_data)
dim(train_data)
#train_d1 <- (merge(x=avg_life_sq, y=train_data, by="sub_area",all=TRUE))
train_data = (inner_join(train_data, avg_life_sq, by = c("sub_area", "sub_area")))
dim(train_data)
table(is.na(train_data$life_sq))
#View(train_data)
#colnames(train_data)
train_data$life_sq[which(train_data$life_sq < (as.integer(train_data$num_room) * train_data$x))] <- NA
View(train_data)

area_eco_df = (as.data.frame(aggregate(train_data[,3:10], by=(list(sub_area,ecology)), FUN = median,na.rm=TRUE)))




