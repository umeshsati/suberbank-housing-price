library(readr)
library(psych)

## read dataset
train <- read_csv("D:/Data Science/Kaggle/Russian house prize/train.csv")

train$build_year <- as.numeric(train$build_year)

for (i in names(train)[which(sapply(train,is.character) == 1)]) {
  if (class(train[[i]])=="character") {
    levels <- unique(c(train[[i]]))
    train[[i]] <- as.numeric(factor(train[[i]], levels=levels))
  }
}
#### mode function

getmode <- function(v) {
  v<-na.omit(v)
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


# Outlier Analysis - Varaiable

outlier_upper=function(x){
  q = quantile(x)
  names(q) = NULL
  q1 = q[2]
  q3 = q[4]
  QR = q3-q1
  return(q3+1.5*QR);
}

outlier_lower=function(x){
  q = quantile(x)
  names(q) = NULL
  q1 = q[2]
  q3 = q[4]
  QR = q3-q1
  return(q1-1.5*QR);
}

train1 = na.omit(train)
y <- train1$price_doc
train1 <- train1[,!names(train1)%in% c("price_doc","timestamp","id","incineration_raion")]
for (i in names(train1)){ train1[[i]] = as.numeric(as.character(train1[[i]]))}
trainset <- train1



###Normality check variable wise
par(mfrow= c(1,2))

#### life_sq normality check
trainset$life_sq[which(trainset$life_sq==0)] <- getmode(trainset$life_sq)
hist(log(trainset$life_sq) ,main ="life_sq")## log normal
qqnorm(log(trainset$life_sq))
boxplot(trainset$life_sq ,main ="life_sq",col = "orange")
trainset$life_sq = log(trainset$life_sq)
####  normality check Build_year

table(trainset$build_year)
trainset$build_year[trainset$build_year==20052009] <- 2007 ### missing entry in data
trainset$build_year[trainset$build_year==4965] <- 1965  ### missing entry in data
trainset$build_year[trainset$build_year==0] <- 2000  ### missing entry in data
trainset$build_year[trainset$build_year==1] <- 2001  ### missing entry in data
trainset$build_year[trainset$build_year==3] <- 2003  ### missing entry in data
trainset$build_year = 2017 - trainset$build_year   #### convert build year into age of house
#trainset$build_year[trainset$build_year==0] <- 1
hist((trainset$build_year) ,main ="build_year") #### not normal it needs to be treated some other way
qqnorm(scale(train1$build_year) ,main ="build_year") #### not normal it needs to be treated some other way
boxplot(trainset$build_year ,main ="life_sq",col = "orange")
#### Kitchen_sq

hist(sqrt(train1$kitch_sq) ,main ="kitch_sq") ## not normal
qqnorm(sqrt(train1$kitch_sq) ,main ="kitch_sq") ## 
boxplot(trainset$kitch_sq ,main ="Kitch_sq",col = "orange")


#### product_type

hist(trainset$product_type ,main ="product_type")
table(trainset$product_type)

#### Area_m not  normal

hist(log(trainset$area_m) ,main ="area_m")   ## log normal
qqnorm(log(trainset$area_m) ,main ="kitch_sq") ## 
boxplot(trainset$kitch_sq ,main ="Kitch_sq",col = "orange")

trainset$area_m = log(trainset$area_m)

### green zone

hist(log(trainset$green_zone_part) ,main ="green_zone_part")
qqnorm(sqrt(trainset$green_zone_part) ,main ="green_zone_part")
boxplot(sqrt(trainset$green_zone_part),main ="green_zone_part", col ="orange")
trainset$green_zone_part = sqrt(trainset$green_zone_part)


## children pre school
hist(sqrt(trainset$children_preschool) ,main ="children_preschool")  ## Sqrt normal
qqnorm(sqrt(trainset$children_preschool) ,main ="children_preschool")
boxplot(sqrt(trainset$children_preschool),main ="children_preschool", col ="orange")
trainset$children_preschool=sqrt(trainset$children_preschool)


###preschool_education_centers_raion 
hist(scale(trainset$preschool_education_centers_raion) ,main ="preschool_education_centers_raion")
qqnorm(scale(trainset$preschool_education_centers_raion) ,main ="preschool_education_centers_raion")
trainset$preschool_education_centers_raion = scale(trainset$preschool_education_centers_raion)


## school_quota
hist(sqrt(trainset$school_quota) ,main ="school_quota")
qqnorm(sqrt(trainset$school_quota) ,main ="school_quota")
trainset$school_quota = sqrt(trainset$school_quota)

###school_education_centers_top_20_raion
hist(trainset$school_education_centers_top_20_raion ,main ="school_education_centers_top_20_raion")
qqnorm(trainset$school_education_centers_top_20_raion ,main ="school_education_centers_top_20_raion")

##### healthcare_centers_raion
hist(scale(trainset$healthcare_centers_raion) ,main ="healthcare_centers_raion")
qqnorm(scale(trainset$healthcare_centers_raion) ,main ="healthcare_centers_raion")
trainset$healthcare_centers_raion = scale(trainset$healthcare_centers_raion)

####sport_objects_raion
hist(sqrt(trainset$sport_objects_raion) ,main ="sport_objects_raion")  ## log normal     
qqnorm(sqrt(trainset$sport_objects_raion) ,main ="sport_objects_raion")  ## log normal     
trainset$sport_objects_raion = sqrt(trainset$sport_objects_raion)

### culture_objects_top_25
hist(trainset$culture_objects_top_25 ,main ="culture_objects_top_25")

###shopping_centers_raion
hist(sqrt(trainset$shopping_centers_raion) ,main ="shopping_centers_raion")
qqnorm(sqrt(trainset$shopping_centers_raion),main ="shopping_centers_raion")
trainset$shopping_centers_raion = sqrt(trainset$shopping_centers_raion)

##thermal_power_plant_raion all binomial no tranformation possible
hist(sqrt(trainset$thermal_power_plant_raion) ,main ="thermal_power_plant_raion")
hist(trainset$oil_chemistry_raion ,main ="oil_chemistry_raion")
hist(trainset$railroad_terminal_raion ,main ="railroad_terminal_raion")
hist(trainset$nuclear_reactor_raion ,main ="nuclear_reactor_raion")

##### full_all
hist(log(trainset$full_all) ,main ="full_all")
qqnorm(log(trainset$full_all) ,main ="full_all")
trainset$full_all = log(trainset$full_all)

hist(log(trainset$female_f) ,main ="female_f")
trainset$female_f = log(trainset$female_f)

hist(sqrt(trainset$young_male) ,main ="young_male")
trainset$young_male=sqrt(trainset$young_male)

hist(trainset$work_all ,main ="work_all")


hist((trainset$work_female) ,main ="work_female")
hist(trainset$ekder_male ,main ="ekder_male")
qqnorm(trainset$ekder_male ,main ="ekder_male")

hist(sqrt(trainset$`0_6_all`),main ="0_6_all")
qqnorm(sqrt(trainset$`0_6_all`),main ="0_6_all")
trainset$`0_6_all` = sqrt(trainset$`0_6_all`)

hist(sqrt(trainset$`0_6_female`) ,main ="0_6_female")
qqnorm(sqrt(trainset$`0_6_female`) ,main ="0_6_female")
trainset$`0_6_female` = sqrt(trainset$`0_6_female`)

hist(trainset$`7_14_male` ,main ="7_14_male")
qqnorm(trainset$`7_14_male` ,main ="7_14_male")


hist(sqrt(trainset$`0_17_all`) ,main ="0_17_all")
qqnorm(sqrt(trainset$`0_17_all`) ,main ="0_17_all")
trainset$`0_17_all` = sqrt(trainset$`0_17_all`)

hist(sqrt(trainset$`0_17_female`) ,main ="0_17_female")
qqnorm(sqrt(trainset$`0_17_female`) ,main ="0_17_female")
trainset$`0_17_female`=sqrt(trainset$`0_17_female`)


hist(log(trainset$`16_29_male`) ,main ="16_29_male")
qqnorm(log(trainset$`16_29_male`) ,main ="16_29_male")
trainset$`16_29_male`= log(trainset$`16_29_male`)

hist(sqrt(trainset$`0_13_all`) ,main ="0_13_all")
qqnorm(sqrt(trainset$`0_13_all`) ,main ="0_13_all")
trainset$`0_13_all` = sqrt(trainset$`0_13_all`)

hist(sqrt(trainset$`0_13_female`) ,main ="0_13_female")
qqnorm(sqrt(trainset$`0_13_female`) ,main ="0_13_female")
trainset$`0_13_female` = sqrt(trainset$`0_13_female`)

hist(sqrt(trainset$build_count_block) ,main ="build_count_block")
qqnorm(sqrt(trainset$build_count_block) ,main ="build_count_block")
trainset$build_count_block=sqrt(trainset$build_count_block)

hist(trainset$build_count_frame ,main ="build_count_frame")

hist(sqrt(trainset$build_count_monolith) ,main ="build_count_monolith")
qqnorm((trainset$build_count_monolith) ,main ="build_count_monolith")

hist(trainset$build_count_foam ,main ="build_count_foam")
hist(trainset$build_count_mix ,main ="build_count_mix")
hist(trainset$build_count_before_1920 ,main ="build_count_before_1920")

hist(scale(trainset$`build_count_1946-1970`) ,main ="build_count_1946-1970")
qqnorm(scale(trainset$`build_count_1946-1970`) ,main ="build_count_1946-1970")
trainset$`build_count_1946-1970`= scale(trainset$`build_count_1946-1970`)

hist(log(trainset$build_count_after_1995) ,main ="build_count_after_1995")
qqnorm(log(trainset$build_count_after_1995) ,main ="build_count_after_1995")
trainset$build_count_after_1995 = log(trainset$build_count_after_1995)

hist(log(trainset$metro_min_avto) ,main ="metro_min_avto")
qqnorm(log(trainset$metro_min_avto) ,main ="metro_min_avto")
trainset$metro_min_avto = log(trainset$metro_min_avto)

hist(log(trainset$metro_min_walk) ,main ="metro_min_walk")
qqnorm(log(trainset$metro_min_walk) ,main ="metro_min_walk")
trainset$metro_min_walk = log(trainset$metro_min_walk)

hist(sqrt(sqrt(trainset$kindergarten_km)) ,main ="kindergarten_km")
qqnorm(sqrt(sqrt(trainset$kindergarten_km)) ,main ="kindergarten_km")
trainset$kindergarten_km= sqrt(sqrt(trainset$kindergarten_km))


hist(sqrt(trainset$park_km) ,main ="park_km")
qqnorm(sqrt(trainset$park_km) ,main ="park_km")
trainset$park_km = sqrt(trainset$park_km)

hist(sqrt(trainset$industrial_km) ,main ="industrial_km")
qqnorm(sqrt(trainset$industrial_km) ,main ="industrial_km")
trainset$industrial_km = sqrt(trainset$industrial_km)

hist(sqrt(trainset$cemetery_km) ,main ="cemetery_km")
qqnorm(sqrt(trainset$cemetery_km) ,main ="cemetery_km")
trainset$cemetery_km = sqrt(trainset$cemetery_km)


hist(sqrt(trainset$railroad_station_walk_km) ,main ="railroad_station_walk_km")
qqnorm(sqrt(trainset$railroad_station_walk_km) ,main ="railroad_station_walk_km")
trainset$railroad_station_walk_km = sqrt(trainset$railroad_station_walk_km)


hist(sqrt(trainset$ID_railroad_station_walk) ,main ="ID_railroad_station_walk")
qqnorm(sqrt(trainset$ID_railroad_station_walk) ,main ="ID_railroad_station_walk")
trainset$ID_railroad_station_walk = sqrt(trainset$ID_railroad_station_walk)

hist(sqrt(trainset$ID_railroad_station_avto) ,main ="ID_railroad_station_avto")
qqnorm(sqrt(trainset$ID_railroad_station_avto) ,main ="ID_railroad_station_avto")
trainset$ID_railroad_station_avto = sqrt(trainset$ID_railroad_station_avto)


hist(sqrt(trainset$public_transport_station_min_walk) ,main ="public_transport_station_min_walk")
qqnorm(sqrt(trainset$public_transport_station_min_walk) ,main ="public_transport_station_min_walk")
trainset$public_transport_station_min_walk = sqrt(trainset$public_transport_station_min_walk)


hist(trainset$water_1line ,main ="water_1line")

hist(sqrt(trainset$ttk_km) ,main ="ttk_km")
qqnorm(sqrt(trainset$ttk_km) ,main ="ttk_km")
trainset$ttk_km = sqrt(trainset$ttk_km)

hist((trainset$bulvar_ring_km) ,main ="bulvar_ring_km")
qqnorm((trainset$bulvar_ring_km) ,main ="bulvar_ring_km")


hist(sqrt(trainset$big_road1_km) ,main ="big_road1_km")
qqnorm(sqrt(trainset$big_road1_km) ,main ="big_road1_km")
trainset$big_road1_km = sqrt(trainset$big_road1_km)

hist(trainset$big_road1_1line ,main ="big_road1_1line")

hist(sqrt(trainset$ID_big_road2 ),main ="ID_big_road2")
qqnorm(sqrt(trainset$ID_big_road2) ,main ="ID_big_road2")
trainset$ID_big_road2 = sqrt(trainset$ID_big_road2)

hist(trainset$railroad_1line ,main ="railroad_1line")
hist(trainset$ID_railroad_terminal ,main ="ID_railroad_terminal")
hist(trainset$ID_bus_terminal ,main ="ID_bus_terminal")


hist(sqrt(trainset$nuclear_reactor_km) ,main ="nuclear_reactor_km")
qqnorm(sqrt(trainset$nuclear_reactor_km) ,main ="nuclear_reactor_km")
trainset$nuclear_reactor_km = sqrt(trainset$nuclear_reactor_km)

hist(log(trainset$power_transmission_line_km) ,main ="power_transmission_line_km")
qqnorm(log(trainset$power_transmission_line_km) ,main ="power_transmission_line_km")
trainset$power_transmission_line_km = log(trainset$power_transmission_line_km)


hist(sqrt(trainset$ts_km) ,main ="ts_km")
qqnorm(sqrt(trainset$ts_km) ,main ="ts_km")
trainset$ts_km = sqrt(trainset$ts_km)

hist(sqrt(trainset$market_shop_km) ,main ="market_shop_km")
qqnorm(sqrt(trainset$market_shop_km) ,main ="market_shop_km")
trainset$market_shop_km = sqrt(trainset$market_shop_km)

hist(sqrt(trainset$swim_pool_km) ,main ="swim_pool_km")
qqnorm(sqrt(trainset$swim_pool_km) ,main ="swim_pool_km")
trainset$swim_pool_km = sqrt(trainset$swim_pool_km)


hist(sqrt(trainset$stadium_km) ,main ="stadium_km")
qqnorm(sqrt(trainset$stadium_km) ,main ="stadium_km")
trainset$stadium_km = sqrt(trainset$stadium_km)

hist(sqrt(trainset$hospice_morgue_km) ,main ="hospice_morgue_km")
qqnorm(sqrt(trainset$hospice_morgue_km) ,main ="hospice_morgue_km")
trainset$hospice_morgue_km = sqrt(trainset$hospice_morgue_km)

hist(sqrt(trainset$public_healthcare_km) ,main ="public_healthcare_km")
qqnorm(sqrt(trainset$public_healthcare_km) ,main ="public_healthcare_km")
trainset$public_healthcare_km = sqrt(trainset$public_healthcare_km)

hist(sqrt(trainset$workplaces_km) ,main ="workplaces_km")
qqnorm(sqrt(trainset$workplaces_km),main ="workplaces_km")

hist(sqrt(trainset$office_km) ,main ="office_km")
qqnorm(sqrt(trainset$office_km) ,main ="office_km")
trainset$office_km = sqrt(trainset$office_km)



hist(sqrt(trainset$preschool_km) ,main ="preschool_km")
qqnorm(sqrt(trainset$preschool_km) ,main ="preschool_km")
trainset$preschool_km = sqrt(trainset$preschool_km)

hist(trainset$church_synagogue_km ,main ="church_synagogue_km")
qqnorm(sqrt(trainset$church_synagogue_km) ,main ="church_synagogue_km")
trainset$church_synagogue_km = sqrt(trainset$church_synagogue_km)

hist(sqrt(sqrt(trainset$theater_km)) ,main ="theater_km")
qqnorm(sqrt(sqrt(trainset$theater_km)) ,main ="theater_km")
trainset$theater_km = sqrt(sqrt(trainset$theater_km))


hist(sqrt(trainset$exhibition_km) ,main ="exhibition_km")
qqnorm(sqrt(trainset$exhibition_km) ,main ="exhibition_km")
trainset$exhibition_km = sqrt(trainset$exhibition_km)



hist(trainset$ecology ,main ="ecology")
qqnorm(trainset$ecology ,main ="ecology")



hist(sqrt(trainset$prom_part_500) ,main ="prom_part_500")
qqnorm(sqrt(trainset$prom_part_500) ,main ="prom_part_500")


hist(trainset$office_sqm_500 ,main ="office_sqm_500")
qqnorm(sqrt(trainset$office_sqm_500) ,main ="office_sqm_500")


hist((trainset$trc_sqm_500) ,main ="trc_sqm_500")
qqnorm((trainset$trc_sqm_500) ,main ="trc_sqm_500")


hist(sqrt(trainset$cafe_sum_500_min_price_avg) ,main ="cafe_sum_500_min_price_avg")
qqnorm(sqrt(trainset$cafe_sum_500_min_price_avg) ,main ="cafe_sum_500_min_price_avg")
trainset$cafe_sum_500_min_price_avg = sqrt(trainset$cafe_sum_500_min_price_avg)


hist(log(trainset$cafe_avg_price_500) ,main ="cafe_avg_price_500")
hist(sqrt(scale(trainset$cafe_count_500_price_500)) ,main ="cafe_count_500_price_500")
hist((trainset$cafe_count_500_price_1500) ,main ="cafe_count_500_price_1500")
hist((trainset$cafe_count_500_price_4000) ,main ="cafe_count_500_price_4000")
hist((trainset$big_church_count_500) ,main ="big_church_count_500")
hist((trainset$mosque_count_500) ,main ="mosque_count_500")
hist(trainset$sport_count_500 ,main ="sport_count_500")



hist(sqrt(trainset$green_part_1000) ,main ="green_part_1000")
qqnorm(sqrt(trainset$green_part_1000) ,main ="green_part_1000")
trainset$green_part_1000 = sqrt(trainset$green_part_1000)

hist(trainset$office_count_1000 ,main ="office_count_1000")
hist(trainset$trc_count_1000 ,main ="trc_count_1000")
hist(trainset$cafe_count_1000 ,main ="cafe_count_1000")
hist(trainset$cafe_sum_1000_max_price_avg ,main ="cafe_sum_1000_max_price_avg")
hist(trainset$cafe_count_1000_na_price ,main ="cafe_count_1000_na_price")
hist(trainset$cafe_count_1000_price_1000 ,main ="cafe_count_1000_price_1000")
hist(trainset$cafe_count_1000_price_2500 ,main ="cafe_count_1000_price_2500")
hist(trainset$cafe_count_1000_price_high ,main ="cafe_count_1000_price_high")
hist(trainset$church_count_1000 ,main ="church_count_1000")
hist(trainset$leisure_count_1000 ,main ="leisure_count_1000")
hist(trainset$market_count_1000 ,main ="market_count_1000")
hist(trainset$prom_part_1500 ,main ="prom_part_1500")
hist(trainset$office_sqm_1500 ,main ="office_sqm_1500")
hist(trainset$trc_sqm_1500 ,main ="trc_sqm_1500")
hist(trainset$cafe_sum_1500_min_price_avg ,main ="cafe_sum_1500_min_price_avg")
hist(trainset$cafe_avg_price_1500 ,main ="cafe_avg_price_1500")
hist(trainset$cafe_count_1500_price_500 ,main ="cafe_count_1500_price_500")
hist(trainset$cafe_count_1500_price_1500 ,main ="cafe_count_1500_price_1500")
hist(trainset$cafe_count_1500_price_4000 ,main ="cafe_count_1500_price_4000")
hist(trainset$big_church_count_1500 ,main ="big_church_count_1500")
hist(trainset$mosque_count_1500 ,main ="mosque_count_1500")
hist(trainset$sport_count_1500 ,main ="sport_count_1500")



hist(scale(trainset$green_part_2000) ,main ="green_part_2000")
qqnorm(sqrt(trainset$green_part_2000) ,main ="green_part_2000")
trainset$green_part_2000 = sqrt(trainset$green_part_2000) 


hist(log(trainset$office_count_2000) ,main ="office_count_2000")




hist(trainset$trc_count_2000 ,main ="trc_count_2000")
hist(trainset$cafe_count_2000 ,main ="cafe_count_2000")

hist((trainset$cafe_sum_2000_max_price_avg) ,main ="cafe_sum_2000_max_price_avg")
hist((trainset$cafe_count_2000_na_price) ,main ="cafe_count_2000_na_price")
hist(((trainset$cafe_count_2000_price_1000)) ,main ="cafe_count_2000_price_1000")
hist(trainset$cafe_count_2000_price_2500 ,main ="cafe_count_2000_price_2500")
hist(trainset$cafe_count_2000_price_high ,main ="cafe_count_2000_price_high")
hist(trainset$church_count_2000 ,main ="church_count_2000")
hist(trainset$leisure_count_2000 ,main ="leisure_count_2000")
hist(trainset$market_count_2000 ,main ="market_count_2000")


hist(sqrt(trainset$prom_part_3000) ,main ="prom_part_3000")
qqnorm(sqrt(trainset$prom_part_3000) ,main ="prom_part_3000")
trainset$prom_part_3000 = sqrt(trainset$prom_part_3000)

hist((trainset$office_sqm_3000) ,main ="office_sqm_3000")

hist(sqrt(trainset$trc_sqm_3000) ,main ="trc_sqm_3000")
qqnorm(sqrt(trainset$trc_sqm_3000) ,main ="trc_sqm_3000")


hist(log(trainset$cafe_sum_3000_min_price_avg) ,main ="cafe_sum_3000_min_price_avg")
qqnorm(log(trainset$cafe_sum_3000_min_price_avg) ,main ="cafe_sum_3000_min_price_avg")
trainset$cafe_sum_3000_min_price_avg = log(trainset$cafe_sum_3000_min_price_avg)

hist(log(trainset$cafe_avg_price_3000) ,main ="cafe_avg_price_3000")
qqnorm(log(trainset$cafe_avg_price_3000) ,main ="cafe_avg_price_3000")
trainset$cafe_avg_price_3000 = log(trainset$cafe_avg_price_3000)


hist(trainset$cafe_count_3000_price_500 ,main ="cafe_count_3000_price_500")
hist(trainset$cafe_count_3000_price_1500 ,main ="cafe_count_3000_price_1500")
hist(trainset$cafe_count_3000_price_4000 ,main ="cafe_count_3000_price_4000")
hist(trainset$big_church_count_3000 ,main ="big_church_count_3000")
hist(trainset$mosque_count_3000 ,main ="mosque_count_3000")

hist(sqrt(trainset$sport_count_3000) ,main ="sport_count_3000")
qqnorm(sqrt(trainset$sport_count_3000) ,main ="sport_count_3000")
trainset$sport_count_3000 =sqrt(trainset$sport_count_3000)

hist(sqrt(trainset$green_part_5000) ,main ="green_part_5000")
qqnorm(sqrt(trainset$green_part_5000) ,main ="green_part_5000")
trainset$green_part_5000 = sqrt(trainset$green_part_5000)

hist(log(trainset$office_count_5000) ,main ="office_count_5000")
qqnorm(log(trainset$office_count_5000) ,main ="office_count_5000")
trainset$office_count_5000 = log(trainset$office_count_5000)

hist(sqrt(trainset$trc_count_5000) ,main ="trc_count_5000")
qqnorm(sqrt(trainset$trc_count_5000) ,main ="trc_count_5000")
trainset$trc_count_5000 = sqrt(trainset$trc_count_5000)

hist(log(trainset$cafe_count_5000) ,main ="cafe_count_5000")
qqnorm(log(trainset$cafe_count_5000) ,main ="cafe_count_5000")
trainset$cafe_count_5000 = log(trainset$cafe_count_5000)


hist(log(trainset$cafe_sum_5000_max_price_avg) ,main ="cafe_sum_5000_max_price_avg")
qqnorm(log(trainset$cafe_sum_5000_max_price_avg) ,main ="cafe_sum_5000_max_price_avg")
trainset$cafe_sum_5000_max_price_avg = log(trainset$cafe_sum_5000_max_price_avg)

hist(sqrt(trainset$cafe_count_5000_na_price) ,main ="cafe_count_5000_na_price")
qqnorm(sqrt(trainset$cafe_count_5000_na_price) ,main ="cafe_count_5000_na_price")
trainset$cafe_count_5000_na_price = sqrt(trainset$cafe_count_5000_na_price)

hist(sqrt(sqrt(trainset$cafe_count_5000_price_1000)) ,main ="cafe_count_5000_price_1000")
qqnorm(sqrt(sqrt(trainset$cafe_count_5000_price_1000)) ,main ="cafe_count_5000_price_1000")
trainset$cafe_count_5000_price_1000 = sqrt(sqrt(trainset$cafe_count_5000_price_1000))

hist(sqrt(trainset$cafe_count_5000_price_2500) ,main ="cafe_count_5000_price_2500")
qqnorm(sqrt(trainset$cafe_count_5000_price_2500) ,main ="cafe_count_5000_price_2500")
trainset$cafe_count_5000_price_2500 = sqrt(trainset$cafe_count_5000_price_2500)

hist((trainset$cafe_count_5000_price_high) ,main ="cafe_count_5000_price_high")

hist(log(trainset$church_count_5000) ,main ="church_count_5000")
qqnorm(log(trainset$church_count_5000) ,main ="church_count_5000")
trainset$church_count_5000 = log(trainset$church_count_5000)

hist(trainset$leisure_count_5000 ,main ="leisure_count_5000")

hist(scale(trainset$market_count_5000) ,main ="market_count_5000")
qqnorm(scale(trainset$market_count_5000) ,main ="market_count_5000")
trainset$market_count_5000 =scale(trainset$market_count_5000)

###### ########

hist(trainset$full_sq,main ="full_sq")
qqnorm(trainset$full_sq,main ="full_sq")
hist(sqrt(trainset$floor),main ="floor")
qqnorm(sqrt(trainset$floor),main ="floor")


hist(trainset$material,main ="material")

hist(log(trainset$num_room),main ="num_room")
qqnorm(log(trainset$num_room),main ="num_room")

hist(trainset$state,main ="state")

hist(sqrt(scale(trainset$sub_area)),main ="sub_area")
qqnorm(sqrt(trainset$sub_area),main ="sub_area")


hist(scale(trainset$raion_popul),main ="raion_popul")
qqnorm(scale(trainset$raion_popul),main ="raion_popul")
trainset$raion_popul = scale(trainset$raion_popul)

hist(sqrt(trainset$indust_part),main ="indust_part")
qqnorm(sqrt(trainset$indust_part),main ="indust_part")
trainset$indust_part = sqrt(trainset$indust_part)


hist(sqrt(trainset$preschool_quota),main ="preschool_quota")
qqnorm(sqrt(trainset$preschool_quota),main ="preschool_quota")
trainset$preschool_quota = sqrt(trainset$preschool_quota)

hist(sqrt(trainset$children_school),main ="children_school")
qqnorm(sqrt(trainset$children_school),main ="children_school")
trainset$children_school = sqrt(trainset$children_school)

hist(sqrt(trainset$school_education_centers_raion),main ="school_education_centers_raion")
qqnorm(sqrt(trainset$school_education_centers_raion),main ="school_education_centers_raion")
trainset$school_education_centers_raion = sqrt(trainset$school_education_centers_raion)


hist(sqrt(trainset$hospital_beds_raion),main ="hospital_beds_raion")
qqnorm(sqrt(trainset$hospital_beds_raion),main ="hospital_beds_raion")
trainset$hospital_beds_raion = sqrt(trainset$hospital_beds_raion)


hist(trainset$university_top_20_raion,main ="university_top_20_raion")
hist(trainset$additional_education_raion,main ="additional_education_raion")
hist(trainset$culture_objects_top_25_raion,main ="culture_objects_top_25_raion")
hist(trainset$office_raion,main ="office_raion")

hist(trainset$radiation_raion,main ="radiation_raion")
hist(trainset$big_market_raion,main ="big_market_raion")
hist(trainset$detention_facility_raion,main ="detention_facility_raion")


hist((trainset$male_f),main ="male_f")
qqnorm(sqrt(trainset$male_f))
hist((trainset$young_all),main ="young_all")

hist(trainset$young_female,main ="young_female")
hist(trainset$work_male,main ="work_male")
hist(trainset$ekder_all,main ="ekder_all")
hist(trainset$ekder_female,main ="ekder_female")
hist(trainset$`0_6_male`,main ="0_6_male")
hist(trainset$`7_14_all`,main ="7_14_all")
hist(trainset$`7_14_female`,main ="7_14_female")
hist(trainset$`0_17_male`,main ="0_17_male")
hist(trainset$`16_29_all`,main ="16_29_all")
hist(trainset$`16_29_female`,main ="16_29_female")
hist(trainset$`0_13_male`,main ="0_13_male")
hist(trainset$raion_build_count_with_material_info,main ="raion_build_count_with_material_info")
hist(trainset$build_count_wood,main ="build_count_wood")
hist(trainset$build_count_brick,main ="build_count_brick")
hist(trainset$build_count_panel,main ="build_count_panel")
hist(trainset$build_count_slag,main ="build_count_slag")
hist(trainset$raion_build_count_with_builddate_info,main ="raion_build_count_with_builddate_info")
hist(trainset$ID_metro,main ="ID_metro")
hist(trainset$metro_km_avto,main ="metro_km_avto")
hist(trainset$metro_km_walk,main ="metro_km_walk")
hist(trainset$school_km,main ="school_km")
hist(trainset$green_zone_km,main ="green_zone_km")
hist(trainset$water_treatment_km,main ="water_treatment_km")
hist(trainset$incineration_km,main ="incineration_km")
hist(trainset$railroad_station_walk_min,main ="railroad_station_walk_min")
hist(trainset$railroad_station_avto_km,main ="railroad_station_avto_km")






























model.1 <- lm(log(y)~ ., data=trainset)
summary(model.1)
plot(model.1)
plot(x=fitted(model.1), y=residuals(model.1),pannel.last = abline(h=0, lty= 2))
qqnorm(residuals(model.1))




trainset_bak <- trainset
#trainset <- trainset_bak

p_val <- as.data.frame(coef(summary(model.1))[, "Pr(>|t|)"])
p_val$var <- row.names(p_val)
colnames(p_val)[1] <- "pvalue"
signvar <- p_val$var[which(p_val > 0.2)]
trainset <- trainset[,!(names(trainset)%in% signvar)]

model12 <- lm(log(y)~., data=trainset)
summary(model12)
qqnorm((sqrt(sqrt(y))))
plot(model12)
plot(x=fitted(model12), y=residuals(model12),pannel.last = abline(h=0, lty= 2))
qqnorm(residuals(model12))