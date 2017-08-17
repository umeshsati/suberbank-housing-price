library(readr)

## read dataset
train <- read_csv("D:/Data Science/Kaggle/Russian house prize/train.csv")

train$build_year <- as.numeric(train$build_year)


for (i in names(train)[which(sapply(train,is.character) == 1)]) {
  if (class(train[[i]])=="character") {
    levels <- unique(c(train[[i]]))
    train[[i]] <- as.numeric(factor(train[[i]], levels=levels))
  }
}


##### remove missing values

train1 = na.omit(train)
train1 <- train1
str(train1)

y <- train1$price_doc
train1 <- train1[,!names(train1)%in% c("price_doc","timestamp")]

# Validating Normality of data  - Qnorm plot
for (i in names(train1)){
  train1[[i]] = as.numeric(as.character(train1[[i]]))
  input <- readline(prompt="Press [enter] to continue; Press Q for quit")
  if (input == "Q")
    stop()
  par(mfrow= c(1,2))
  hist(train1[[i]], main=i)
  qqnorm(train1[[i]], main=i)
  trns <- readline(prompt = "type L for log: type SQ for sqrt and type S for scale")
  if (trns== "L")
    hist(log(train1[[i]], main=i))
    qqnorm(log(train1[[i]]), main=i)
    uplog <- readline(prompt = "press y if wish to update, n for no")
    if (uplog == "y")
      train1[[i]] = log(train1[[i]])
  if (trns== "SQ")
    hist(sqrt(train1[[i]], main=i))
    qqnorm(sqrt(train1[[i]]), main=i)
    uplog <- readline(prompt = "press y if wish to update, n for no")
    if (uplog == "y")
      train1[[i]] = sqrt(train1[[i]])
    
  if (trns== "S")
    hist(scale(train1[[i]], main=i))
    qqnorm(scale(train1[[i]]), main=i)
    uplog <- readline(prompt = "press y if wish to update, n for no")
    if (uplog == "y")
      train1[[i]] = scale(train1[[i]])
} 


par(mfrow= c(1,2))
#hist(train1$id ,main ="id")
hist(log(train1$life_sq) ,main ="life_sq")## log normal
hist(sqrt(train1$max_floor) ,main ="max_floor")   #### not normal it needs to be treated some other way
hist(train1$build_year ,main ="build_year") #### not normal it needs to be treated some other way
qqnorm(log(train1$kitch_sq) ,main ="kitch_sq") ## log normal
hist(train1$product_type ,main ="product_type")
hist(log(train1$area_m) ,main ="area_m")   ## log normal
hist(train1$green_zone_part ,main ="green_zone_part")
hist(sqrt(train1$children_preschool) ,main ="children_preschool")  ## Sqrt normal
hist(train1$preschool_education_centers_raion ,main ="preschool_education_centers_raion")
hist(train1$school_quota ,main ="school_quota")
hist(train1$school_education_centers_top_20_raion ,main ="school_education_centers_top_20_raion")
hist(train1$healthcare_centers_raion ,main ="healthcare_centers_raion")
hist(log(train1$sport_objects_raion) ,main ="sport_objects_raion")  ## log normal     
hist(train1$culture_objects_top_25 ,main ="culture_objects_top_25")
hist(sqrt(train1$shopping_centers_raion) ,main ="shopping_centers_raion")
hist(sqrt(train1$thermal_power_plant_raion) ,main ="thermal_power_plant_raion")
hist(train1$oil_chemistry_raion ,main ="oil_chemistry_raion")
hist(train1$railroad_terminal_raion ,main ="railroad_terminal_raion")
hist(train1$nuclear_reactor_raion ,main ="nuclear_reactor_raion")
hist(train1$full_all ,main ="full_all")
hist(train1$female_f ,main ="female_f")
hist(log(train1$young_male) ,main ="young_male")  ### log normal
hist(sqrt(train1$work_all) ,main ="work_all")
hist(sqrt(train1$work_female) ,main ="work_female")
hist(sqrt(train1$ekder_male) ,main ="ekder_male")
#hist(train1$0_6_all ,main ="0_6_all")
#hist(train1$0_6_female ,main ="0_6_female")
#hist(train1$7_14_male ,main ="7_14_male")
#hist(train1$0_17_all ,main ="0_17_all")
#hist(train1$0_17_female ,main ="0_17_female")
#hist(train1$16_29_male ,main ="16_29_male")
#hist(train1$0_13_all ,main ="0_13_all")
#hist(train1$0_13_female ,main ="0_13_female")
hist(sqrt(train1$build_count_block) ,main ="build_count_block")
hist(train1$build_count_frame ,main ="build_count_frame")
hist(train1$build_count_monolith ,main ="build_count_monolith")
hist(train1$build_count_foam ,main ="build_count_foam")
hist(train1$build_count_mix ,main ="build_count_mix")
hist(train1$build_count_before_1920 ,main ="build_count_before_1920")
hist(train1$build_count_1946-1970 ,main ="build_count_1946-1970")
hist(train1$build_count_after_1995 ,main ="build_count_after_1995")
hist(train1$metro_min_avto ,main ="metro_min_avto")
hist(train1$metro_min_walk ,main ="metro_min_walk")
hist(train1$kindergarten_km ,main ="kindergarten_km")
hist(train1$park_km ,main ="park_km")
hist(train1$industrial_km ,main ="industrial_km")
hist(train1$cemetery_km ,main ="cemetery_km")
hist(train1$railroad_station_walk_km ,main ="railroad_station_walk_km")
hist(train1$ID_railroad_station_walk ,main ="ID_railroad_station_walk")
hist(train1$ID_railroad_station_avto ,main ="ID_railroad_station_avto")
hist(train1$public_transport_station_min_walk ,main ="public_transport_station_min_walk")
hist(train1$water_1line ,main ="water_1line")
hist(train1$ttk_km ,main ="ttk_km")
hist(train1$bulvar_ring_km ,main ="bulvar_ring_km")
hist(train1$big_road1_km ,main ="big_road1_km")
hist(train1$big_road1_1line ,main ="big_road1_1line")
hist(train1$ID_big_road2 ,main ="ID_big_road2")
hist(train1$railroad_1line ,main ="railroad_1line")
hist(train1$ID_railroad_terminal ,main ="ID_railroad_terminal")
hist(train1$ID_bus_terminal ,main ="ID_bus_terminal")
hist(train1$nuclear_reactor_km ,main ="nuclear_reactor_km")
hist(train1$power_transmission_line_km ,main ="power_transmission_line_km")
hist(train1$ts_km ,main ="ts_km")
hist(train1$market_shop_km ,main ="market_shop_km")
hist(train1$swim_pool_km ,main ="swim_pool_km")
hist(train1$stadium_km ,main ="stadium_km")
hist(train1$hospice_morgue_km ,main ="hospice_morgue_km")
hist(train1$public_healthcare_km ,main ="public_healthcare_km")
hist(train1$workplaces_km ,main ="workplaces_km")
hist(train1$office_km ,main ="office_km")
hist(train1$preschool_km ,main ="preschool_km")
hist(train1$church_synagogue_km ,main ="church_synagogue_km")
hist(train1$theater_km ,main ="theater_km")
hist(train1$exhibition_km ,main ="exhibition_km")
hist(train1$ecology ,main ="ecology")
hist(train1$prom_part_500 ,main ="prom_part_500")
hist(train1$office_sqm_500 ,main ="office_sqm_500")
hist(train1$trc_sqm_500 ,main ="trc_sqm_500")
hist(train1$cafe_sum_500_min_price_avg ,main ="cafe_sum_500_min_price_avg")
hist(train1$cafe_avg_price_500 ,main ="cafe_avg_price_500")
hist(train1$cafe_count_500_price_500 ,main ="cafe_count_500_price_500")
hist(train1$cafe_count_500_price_1500 ,main ="cafe_count_500_price_1500")
hist(train1$cafe_count_500_price_4000 ,main ="cafe_count_500_price_4000")
hist(train1$big_church_count_500 ,main ="big_church_count_500")
hist(train1$mosque_count_500 ,main ="mosque_count_500")
hist(train1$sport_count_500 ,main ="sport_count_500")
hist(train1$green_part_1000 ,main ="green_part_1000")
hist(train1$office_count_1000 ,main ="office_count_1000")
hist(train1$trc_count_1000 ,main ="trc_count_1000")
hist(train1$cafe_count_1000 ,main ="cafe_count_1000")
hist(train1$cafe_sum_1000_max_price_avg ,main ="cafe_sum_1000_max_price_avg")
hist(train1$cafe_count_1000_na_price ,main ="cafe_count_1000_na_price")
hist(train1$cafe_count_1000_price_1000 ,main ="cafe_count_1000_price_1000")
hist(train1$cafe_count_1000_price_2500 ,main ="cafe_count_1000_price_2500")
hist(train1$cafe_count_1000_price_high ,main ="cafe_count_1000_price_high")
hist(train1$church_count_1000 ,main ="church_count_1000")
hist(train1$leisure_count_1000 ,main ="leisure_count_1000")
hist(train1$market_count_1000 ,main ="market_count_1000")
hist(train1$prom_part_1500 ,main ="prom_part_1500")
hist(train1$office_sqm_1500 ,main ="office_sqm_1500")
hist(train1$trc_sqm_1500 ,main ="trc_sqm_1500")
hist(train1$cafe_sum_1500_min_price_avg ,main ="cafe_sum_1500_min_price_avg")
hist(train1$cafe_avg_price_1500 ,main ="cafe_avg_price_1500")
hist(train1$cafe_count_1500_price_500 ,main ="cafe_count_1500_price_500")
hist(train1$cafe_count_1500_price_1500 ,main ="cafe_count_1500_price_1500")
hist(train1$cafe_count_1500_price_4000 ,main ="cafe_count_1500_price_4000")
hist(train1$big_church_count_1500 ,main ="big_church_count_1500")
hist(train1$mosque_count_1500 ,main ="mosque_count_1500")
hist(train1$sport_count_1500 ,main ="sport_count_1500")
hist(train1$green_part_2000 ,main ="green_part_2000")
hist(train1$office_count_2000 ,main ="office_count_2000")
hist(train1$trc_count_2000 ,main ="trc_count_2000")
hist(train1$cafe_count_2000 ,main ="cafe_count_2000")
hist(train1$cafe_sum_2000_max_price_avg ,main ="cafe_sum_2000_max_price_avg")
hist(train1$cafe_count_2000_na_price ,main ="cafe_count_2000_na_price")
hist(train1$cafe_count_2000_price_1000 ,main ="cafe_count_2000_price_1000")
hist(train1$cafe_count_2000_price_2500 ,main ="cafe_count_2000_price_2500")
hist(train1$cafe_count_2000_price_high ,main ="cafe_count_2000_price_high")
hist(train1$church_count_2000 ,main ="church_count_2000")
hist(train1$leisure_count_2000 ,main ="leisure_count_2000")
hist(train1$market_count_2000 ,main ="market_count_2000")
hist(train1$prom_part_3000 ,main ="prom_part_3000")
hist(train1$office_sqm_3000 ,main ="office_sqm_3000")
hist(train1$trc_sqm_3000 ,main ="trc_sqm_3000")
hist(train1$cafe_sum_3000_min_price_avg ,main ="cafe_sum_3000_min_price_avg")
hist(train1$cafe_avg_price_3000 ,main ="cafe_avg_price_3000")
hist(train1$cafe_count_3000_price_500 ,main ="cafe_count_3000_price_500")
hist(train1$cafe_count_3000_price_1500 ,main ="cafe_count_3000_price_1500")
hist(train1$cafe_count_3000_price_4000 ,main ="cafe_count_3000_price_4000")
hist(train1$big_church_count_3000 ,main ="big_church_count_3000")
hist(train1$mosque_count_3000 ,main ="mosque_count_3000")
hist(train1$sport_count_3000 ,main ="sport_count_3000")
hist(train1$green_part_5000 ,main ="green_part_5000")
hist(train1$office_count_5000 ,main ="office_count_5000")
hist(train1$trc_count_5000 ,main ="trc_count_5000")
hist(train1$cafe_count_5000 ,main ="cafe_count_5000")
hist(train1$cafe_sum_5000_max_price_avg ,main ="cafe_sum_5000_max_price_avg")
hist(train1$cafe_count_5000_na_price ,main ="cafe_count_5000_na_price")
hist(train1$cafe_count_5000_price_1000 ,main ="cafe_count_5000_price_1000")
hist(train1$cafe_count_5000_price_2500 ,main ="cafe_count_5000_price_2500")
hist(train1$cafe_count_5000_price_high ,main ="cafe_count_5000_price_high")
hist(train1$church_count_5000 ,main ="church_count_5000")
hist(train1$leisure_count_5000 ,main ="leisure_count_5000")
hist(train1$market_count_5000 ,main ="market_count_5000")










































##### fit the model

model.1 <- lm(y~., data=train1)

summary(model.1)


p_val <- as.data.frame(coef(summary(model.1))[, "Pr(>|t|)"])
p_val$var <- row.names(p_val)
colnames(p_val)[1] <- "pvalue"
signvar <- p_val$var[which(p_val > 0.5)]
train_model <- train1[,!(names(train1)%in% signvar)]


model.2 <- lm(scale(y)~., data=train_model)
summary(model.2)



p_val <- as.data.frame(coef(summary(model.2))[, "Pr(>|t|)"])
p_val$var <- row.names(p_val)
colnames(p_val)[1] <- "pvalue"
signvar <- p_val$var[which(p_val > 0.1)]
train_model <- train_model[,!(names(train_model)%in% signvar)]

model.2 <- lm(y~., data=train_model)
summary(model.2)

insig <- c("`0_6_male`","`0_6_female`","`7_14_all`","`7_14_female`","`0_17_female`","`0_13_female`","`16_29_female`","railroad_station_walk_min","cafe_count_1500_price_high
")
train_model <- train_model[,!(names(train_model)%in% insig)]
hist(residuals(model.1))
plot(x=fitted(model.2), y=residuals(model.2), pannel.last = abline(h=0, lty= 2))
### omit missing values and build a model
train1 <- train[,!names(train)%in% c("build_year","state","kitch_sq","material","num_room")]
table(is.na(train1$floor))
table(is.na(train1$num_room))
table(is.na(train1$material))
table(is.na(train1$kitch_sq))

train1 <- is.na(train1)






table(is.na(train$build_year))
table(is.na(train$floor))
table(is.na(train$num_room))
table(is.na(train$material))
table(is.na(train$state))
table(is.na(train$kitch_sq))

