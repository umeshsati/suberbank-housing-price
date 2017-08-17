# -*- coding: utf-8 -*-
"""
Created on Fri Jun 16 14:04:15 2017

@author: Umesh.Sati
"""

# Import necessary packages and data
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
get_ipython().magic(u'matplotlib inline')

#read training dataset
filePath=r"D:/Data Science/Kaggle/Russian house prize/"
data_train=pd.read_csv(filePath+"train.csv", sep=',')
data_test = pd.read_csv(filePath+"test.csv", sep=',')
macro = pd.read_csv(filePath+"macro.csv", sep=',')


data_train.shape


############ macro data based on VIF
# From here: https://www.kaggle.com/robertoruiz/sberbank-russian-housing-market/dealing-with-multicollinearity/notebook

feature_macro = ["timestamp","balance_trade", "balance_trade_growth", "eurrub", "average_provision_of_build_contract",
"micex_rgbi_tr", "micex_cbi_tr", "deposits_rate", "mortgage_value", "mortgage_rate",
"income_per_cap", "rent_price_4+room_bus", "museum_visitis_per_100_cap", "apartment_build"]

macro = macro[feature_macro]

##### merge the macro data with train and test dataset

data_train = pd.merge(data_train,macro, on="timestamp", how="left")
data_test = pd.merge(data_test,macro, on="timestamp", how="left")

##### convert timestamp 
data_train.timestamp = data_train.timestamp.astype('datetime64[ns]')
data_test.timestamp = data_test.timestamp.astype('datetime64[ns]')

##### month year
data_train['year'] =data_train.timestamp.dt.year
data_train['month'] =data_train.timestamp.dt.month
data_train['dow'] = data_train.timestamp.dt.dayofweek

data_test['year'] = data_test.timestamp.dt.year
data_test['month'] =data_test.timestamp.dt.month
data_test['dow'] = data_test.timestamp.dt.dayofweek




############################## Feature exploration in training data set


data_train.shape

data_train.describe().round(2)

cat_train=data_train.select_dtypes(include=['object'])
cat_train.head()


#count number of NA in each column
NA_count=data_train.isnull().sum()
NA_count=pd.DataFrame(NA_count,columns=['count']).sort_values(by="count",axis=0, 
                                                                  ascending=False, inplace=False, kind='quicksort')
print (NA_count)

### ### fULL sQUARE data

data_train.full_sq.loc[data_train.full_sq <= (data_train.life_sq+data_train.kitch_sq)] = np.nan
data_train.full_sq.loc[data_train.full_sq < data_train.life_sq]=np.nan
data_train.life_sq.loc[data_train.full_sq.isnull()==True]= np.nan
data_train.kitch_sq.loc[data_train.full_sq.isnull()==True]= np.nan


####Area waise life_sq median is devided by  median no of room in NUM_room to achive no of room in missing values of num room
avg_life_sq =pd.DataFrame((data_train.groupby(["sub_area"]).median()).life_sq)
avg_life_sq =avg_life_sq.reset_index()
##### devide the life_sq by 2 because the median num_room is 2 
avg_life_sq.life_sq = (avg_life_sq.life_sq/2).round(0)


avg_life_sq =avg_life_sq.rename(columns={'life_sq': 'area_wise_room_size'})
data_train =pd.merge(data_train,avg_life_sq, on=["sub_area"],how="inner")                   


data_train.life_sq[data_train.life_sq < (data_train.num_room*data_train.area_wise_room_size)]=np.nan

data_train.loc[(data_train["build_year"]>2018) | (data_train["build_year"]<1900),"build_year"] = np.nan
data_train["build_year"] = data_train["build_year"].apply(lambda x : 2017-x) # execute only once
data_train.loc[(data_train["max_floor"]<1 ) | (data_train["floor"] > data_train["max_floor"]),"max_floor"] = np.nan
data_train.loc[(data_train["floor"]<1 ) ,"floor"] = np.nan
data_train.loc[data_train["state"] > 4, "state"] = np.nan


#data_train = data_train[data_train["full_sq"].notnull()]
#data_train = data_train[(data_train["life_sq"].notnull()) | (data_train["kitch_sq"].notnull())]


data_train.describe().round(2)






# function to impute kitchen and life sq NA
def fillNA(row,tofill, fromfill):
    if pd.isnull(row[tofill]):
        return row["full_sq"] - row[fromfill]
    else:
        return row[tofill]
data_train["kitch_sq"] = data_train.apply(lambda x: fillNA(x,"kitch_sq","life_sq"), axis=1)
#data_train[data_train["kitch_sq"]==0].shape
data_train["life_sq"] = data_train.apply(lambda x: fillNA(x,"life_sq","kitch_sq"), axis=1)

NA_fill_df = data_train.groupby(["sub_area","ecology"]).median().iloc[:,2:10]
NA_fill_df = NA_fill_df.reset_index()
NA_fill_df.shape


data_train = pd.merge(data_train,NA_fill_df, on=["sub_area","ecology"],how="inner")


#function to impute NA
def fillNA_by_area_median(row,tofill, fromfill):
    if pd.isnull(row[tofill]):
        return row[fromfill]
    else:
        return row[tofill]


### fill NA
data_train["build_year_x"] = data_train.apply(lambda x: fillNA_by_area_median(x,"build_year_x","build_year_y"), axis=1)
data_train["floor_x"] = data_train.apply(lambda x: fillNA_by_area_median(x,"floor_x","floor_y"), axis=1)


import math
#function to impute NA
def fillNA_for_num_rooms(row,tofill, fromfill):
    if pd.isnull(row[tofill]):
        return math.floor(row["life_sq_x"]/row[fromfill])
    else:
        return row[tofill]
#data_train["num_room_x"] = data_train.apply(lambda x: fillNA_for_num_rooms(x,"num_room_x","area_wise_room_size"), axis=1)


data_train = data_train.drop(["max_floor_x","state_x","hospital_beds_raion","material_x","floor_y","build_year_y","state_y"],1)



NA_count=data_train.isnull().sum()
NA_count=pd.DataFrame(NA_count,columns=['count']).sort_values(by="count",axis=0, 
                                                                  ascending=False, inplace=False, kind='quicksort')
print (NA_count)




data_train = data_train.drop(["cafe_sum_500_min_price_avg","cafe_avg_price_500", "cafe_sum_500_max_price_avg",
                             "office_sqm_500", "id", "ID_metro", "ID_railroad_station_walk", "ID_railroad_station_avto", 
                              "ID_big_road1", "ID_big_road2", "ID_railroad_terminal","ID_bus_terminal"],1)




#fill rest of the columns NA with median
def fillNaWithMedian(columnList):
    for i in columnList:
        data_train[i]=data_train[i].fillna(data_train[i].median())
        
fillNA_withMedian_List=["cafe_sum_1000_max_price_avg","cafe_avg_price_1000","cafe_sum_1000_min_price_avg","preschool_quota",
                        "school_quota","build_count_wood","build_count_after_1995",
                        "build_count_before_1920","raion_build_count_with_builddate_info",
                        "build_count_frame", "build_count_mix", "build_count_slag","build_count_block","build_count_panel",
                        "build_count_monolith","build_count_brick", "build_count_foam", "raion_build_count_with_material_info",
                       "cafe_sum_1500_min_price_avg","cafe_avg_price_1500","cafe_sum_1500_max_price_avg","cafe_sum_2000_min_price_avg",
                       "cafe_sum_2000_max_price_avg","cafe_avg_price_2000","cafe_sum_3000_min_price_avg","cafe_avg_price_3000",
                       "cafe_sum_3000_max_price_avg","cafe_sum_5000_min_price_avg","cafe_sum_5000_max_price_avg","cafe_avg_price_5000",
                       "prom_part_5000","metro_km_walk","metro_min_walk","railroad_station_walk_km","railroad_station_walk_min"]

fillNaWithMedian(fillNA_withMedian_List)

feature_macro1 = ["balance_trade", "balance_trade_growth", "eurrub", "average_provision_of_build_contract",
"micex_rgbi_tr", "micex_cbi_tr", "deposits_rate", "mortgage_value", "mortgage_rate",
"income_per_cap", "rent_price_4+room_bus", "museum_visitis_per_100_cap", "apartment_build"]
fillNaWithMedian(feature_macro1)

#cleaned_data=data_train.dropna()
#cleaned_data.describe().round(2)

cleaned_data=data_train
#y= cleaned_data["price_doc"]
data_train.max_floor_y

cleaned_data = cleaned_data.drop(['timestamp','area_wise_room_size', 'life_sq_y','max_floor_y','material_y', 'num_room_y', 'kitch_sq_y'],1)

X_all.to_csv('D:/Data Science/Kaggle/Russian house prize/data_train_xgb.csv',index=False)
#encoding

cat_X=cleaned_data.select_dtypes(include=['object'])
cat_X_temp=cat_X
num_X=cleaned_data.select_dtypes(include=['float64'])
int64_X=cleaned_data.select_dtypes(include=['int64'])
X_all.balance_trade.dtype
print (cat_X.shape, num_X.shape, int64_X.shape)


#label encoder
from sklearn import preprocessing
le = preprocessing.LabelEncoder()
cat_X=cat_X.apply(le.fit_transform)

X_all=pd.merge(cat_X,num_X,right_index=True, left_index=True)
X_all=pd.merge(X_all,int64_X,right_index=True, left_index=True)

X_all.describe().round(2)


################### test data ######################


id_test = data_test.id
data_test.describe().round(2)

cat_test=data_test.select_dtypes(include=['object'])
cat_test.head()


#count number of NA in each column
NA_count=data_test.isnull().sum()
NA_count=pd.DataFrame(NA_count,columns=['count']).sort_values(by="count",axis=0, 
                                                                  ascending=False, inplace=False, kind='quicksort')
print (NA_count)

### ### fULL sQUARE data

data_test.full_sq.loc[data_test.full_sq <= (data_test.life_sq+data_test.kitch_sq)] = np.nan
data_test.full_sq.loc[data_test.full_sq < data_test.life_sq]=np.nan
data_test.life_sq.loc[data_test.full_sq.isnull()==True]= np.nan
data_test.kitch_sq.loc[data_test.full_sq.isnull()==True]= np.nan


####Area waise life_sq median is devided by  median no of room in NUM_room to achive no of room in missing values of num room
avg_life_sq =pd.DataFrame((data_test.groupby(["sub_area"]).median()).life_sq)
avg_life_sq =avg_life_sq.reset_index()
##### devide the life_sq by 2 because the median num_room is 2 
avg_life_sq.life_sq = (avg_life_sq.life_sq/2).round(0)


avg_life_sq =avg_life_sq.rename(columns={'life_sq': 'area_wise_room_size'})
data_test =pd.merge(data_test,avg_life_sq, on=["sub_area"],how="inner")                   


data_test.life_sq[data_test.life_sq < (data_test.num_room*data_test.area_wise_room_size)]=np.nan


data_test.describe().round(2)

cat_train=data_test.select_dtypes(include=['object'])
cat_train.head()


#count number of NA in each column
NA_count=data_test.isnull().sum()
NA_count=pd.DataFrame(NA_count,columns=['count']).sort_values(by="count",axis=0, 
                                                                  ascending=False, inplace=False, kind='quicksort')
print (NA_count)


data_test.loc[(data_test["build_year"]>2018) | (data_test["build_year"]<1900),"build_year"] = np.nan
data_test["build_year"] = data_test["build_year"].apply(lambda x : 2017-x) # execute only once
data_test.loc[(data_test["max_floor"]<1 ) | (data_test["floor"] > data_test["max_floor"]),"max_floor"] = np.nan
data_test.loc[(data_test["floor"]<1 ) ,"floor"] = np.nan
data_test.loc[data_test["state"] > 4, "state"] = np.nan





data_test.describe().round(2)




# function to impute kitchen and life sq NA
def fillNA1(row,tofill, fromfill):
    if pd.isnull(row[tofill]):
        return row["full_sq"] - row[fromfill]
    else:
        return row[tofill]
data_test["kitch_sq"] = data_test.apply(lambda x: fillNA1(x,"kitch_sq","life_sq"), axis=1)
#data_test[data_test["kitch_sq"]==0].shape
data_test["life_sq"] = data_test.apply(lambda x: fillNA1(x,"life_sq","kitch_sq"), axis=1)

NA_fill_df = data_test.groupby(["sub_area","ecology"]).median().iloc[:,2:10]
NA_fill_df = NA_fill_df.reset_index()
NA_fill_df.shape


data_test = pd.merge(data_test,NA_fill_df, on=["sub_area","ecology"],how="inner")


#function to impute NA
def fillNA_by_area_median1(row,tofill, fromfill):
    if pd.isnull(row[tofill]):
        return row[fromfill]
    else:
        return row[tofill]


### fill NA
data_test["build_year_x"] = data_test.apply(lambda x: fillNA_by_area_median1(x,"build_year_x","build_year_y"), axis=1)
data_test["floor_x"] = data_test.apply(lambda x: fillNA_by_area_median1(x,"floor_x","floor_y"), axis=1)


import math
#function to impute NA
def fillNA_for_num_rooms(row,tofill, fromfill):
    if pd.isnull(row[tofill]):
        return math.floor(row["life_sq_x"]/row[fromfill])
    else:
        return row[tofill]
#data_test["num_room_x"] = data_test.apply(lambda x: fillNA_for_num_rooms(x,"num_room_x","area_wise_room_size"), axis=1)


data_test = data_test.drop(["timestamp","max_floor_x","state_x","hospital_beds_raion","material_x","floor_y","build_year_y","state_y"],1)



NA_count=data_test.isnull().sum()
NA_count=pd.DataFrame(NA_count,columns=['count']).sort_values(by="count",axis=0, 
                                                                  ascending=False, inplace=False, kind='quicksort')
print (NA_count)

id_test = data_test.id
data_test.describe().round(2)

cat_test=data_test.select_dtypes(include=['object'])
cat_test.head()


#count number of NA in each column
NA_count=data_test.isnull().sum()
NA_count=pd.DataFrame(NA_count,columns=['count']).sort_values(by="count",axis=0, 
                                                                  ascending=False, inplace=False, kind='quicksort')
print (NA_count)

### ### fULL sQUARE data

data_test.full_sq.loc[data_test.full_sq <= (data_test.life_sq+data_test.kitch_sq)] = np.nan
data_test.full_sq.loc[data_test.full_sq < data_test.life_sq]=np.nan
data_test.life_sq.loc[data_test.full_sq.isnull()==True]= np.nan
data_test.kitch_sq.loc[data_test.full_sq.isnull()==True]= np.nan


####Area waise life_sq median is devided by  median no of room in NUM_room to achive no of room in missing values of num room
avg_life_sq =pd.DataFrame((data_test.groupby(["sub_area"]).median()).life_sq)
avg_life_sq =avg_life_sq.reset_index()
##### devide the life_sq by 2 because the median num_room is 2 
avg_life_sq.life_sq = (avg_life_sq.life_sq/2).round(0)


avg_life_sq =avg_life_sq.rename(columns={'life_sq': 'area_wise_room_size'})
data_test =pd.merge(data_test,avg_life_sq, on=["sub_area"],how="inner")                   


data_test.life_sq[data_test.life_sq < (data_test.num_room*data_test.area_wise_room_size)]=np.nan


data_test.describe().round(2)

cat_train=data_test.select_dtypes(include=['object'])
cat_train.head()


#count number of NA in each column
NA_count=data_test.isnull().sum()
NA_count=pd.DataFrame(NA_count,columns=['count']).sort_values(by="count",axis=0, 
                                                                  ascending=False, inplace=False, kind='quicksort')
print (NA_count)


data_test.loc[(data_test["build_year"]>2018) | (data_test["build_year"]<1900),"build_year"] = np.nan
data_test["build_year"] = data_test["build_year"].apply(lambda x : 2017-x) # execute only once
data_test.loc[(data_test["max_floor"]<1 ) | (data_test["floor"] > data_test["max_floor"]),"max_floor"] = np.nan
data_test.loc[(data_test["floor"]<1 ) ,"floor"] = np.nan
data_test.loc[data_test["state"] > 4, "state"] = np.nan





data_test.describe().round(2)




# function to impute kitchen and life sq NA
def fillNA(row,tofill, fromfill):
    if pd.isnull(row[tofill]):
        return row["full_sq"] - row[fromfill]
    else:
        return row[tofill]
data_test["kitch_sq"] = data_test.apply(lambda x: fillNA(x,"kitch_sq","life_sq"), axis=1)
#data_test[data_test["kitch_sq"]==0].shape
data_test["life_sq"] = data_test.apply(lambda x: fillNA(x,"life_sq","kitch_sq"), axis=1)

NA_fill_df = data_test.groupby(["sub_area","ecology"]).median().iloc[:,2:10]
NA_fill_df = NA_fill_df.reset_index()
NA_fill_df.shape


data_test = pd.merge(data_test,NA_fill_df, on=["sub_area","ecology"],how="inner")


#function to impute NA
def fillNA_by_area_median(row,tofill, fromfill):
    if pd.isnull(row[tofill]):
        return row[fromfill]
    else:
        return row[tofill]


### fill NA
data_test["build_year_x"] = data_test.apply(lambda x: fillNA_by_area_median(x,"build_year_x","build_year_y"), axis=1)
data_test["floor_x"] = data_test.apply(lambda x: fillNA_by_area_median(x,"floor_x","floor_y"), axis=1)


import math
#function to impute NA
def fillNA_for_num_rooms(row,tofill, fromfill):
    if pd.isnull(row[tofill]):
        return math.floor(row["life_sq_x"]/row[fromfill])
    else:
        return row[tofill]
#data_test["num_room_x"] = data_test.apply(lambda x: fillNA_for_num_rooms(x,"num_room_x","area_wise_room_size"), axis=1)


data_test = data_test.drop(["timestamp","max_floor_x","state_x","hospital_beds_raion","material_x","floor_y","build_year_y","state_y"],1)



NA_count=data_test.isnull().sum()
NA_count=pd.DataFrame(NA_count,columns=['count']).sort_values(by="count",axis=0, 
                                                                  ascending=False, inplace=False, kind='quicksort')
print (NA_count)





data_test = data_test.drop(["cafe_sum_500_min_price_avg","cafe_avg_price_500", "cafe_sum_500_max_price_avg",
                             "office_sqm_500", "id", "ID_metro", "ID_railroad_station_walk", "ID_railroad_station_avto", 
                              "ID_big_road1", "ID_big_road2", "ID_railroad_terminal","ID_bus_terminal"],1)




#fill rest of the columns NA with median
def fillNaWithMediantest(columnList):
    for i in columnList:
        data_test[i]=data_test[i].fillna(data_test[i].median())
        
fillNA_withMedian_List=["cafe_sum_1000_max_price_avg","cafe_avg_price_1000","cafe_sum_1000_min_price_avg","preschool_quota",
                        "school_quota","build_count_wood","build_count_after_1995",
                        "build_count_before_1920","raion_build_count_with_builddate_info",
                        "build_count_frame", "build_count_mix", "build_count_slag","build_count_block","build_count_panel",
                        "build_count_monolith","build_count_brick", "build_count_foam", "raion_build_count_with_material_info",
                       "cafe_sum_1500_min_price_avg","cafe_avg_price_1500","cafe_sum_1500_max_price_avg","cafe_sum_2000_min_price_avg",
                       "cafe_sum_2000_max_price_avg","cafe_avg_price_2000","cafe_sum_3000_min_price_avg","cafe_avg_price_3000",
                       "cafe_sum_3000_max_price_avg","cafe_sum_5000_min_price_avg","cafe_sum_5000_max_price_avg","cafe_avg_price_5000",
                       "prom_part_5000","metro_km_walk","metro_min_walk","railroad_station_walk_km","railroad_station_walk_min"]

fillNaWithMediantest(fillNA_withMedian_List)

fillNaWithMediantest(feature_macro1)



data_test.build_year_x =data_test.build_year_x.fillna(data_test.build_year_x.median())
data_test.product_type = data_test.product_type.fillna("Investment")


data_test.full_sq = data_test.full_sq.fillna(data_test.full_sq.median())
data_test.life_sq_x = data_test.life_sq_x.fillna(data_test.life_sq_x.median())
data_test.floor_x = data_test.floor_x.fillna(data_test.floor_x.median())
data_test.kitch_sq_x = data_test.kitch_sq_x.fillna(data_test.kitch_sq_x.median())
data_test.num_room_x = data_test.num_room_x.fillna(data_test.num_room_x.median())

X_test.to_csv('D:/Data Science/Kaggle/Russian house prize/data_test_xgb.csv',index=False)
#### drop variables

data_test =data_test.drop(['area_wise_room_size', 'life_sq_y','max_floor_y','material_y', 'num_room_y', 'kitch_sq_y'],1)
#encoding

cat_X=data_test.select_dtypes(include=['object'])

num_X=data_test.select_dtypes(include=['float64'])
int64_X=data_test.select_dtypes(include=['int64'])

print (cat_X.shape, num_X.shape, int64_X.shape)



#label encoder
le_test = preprocessing.LabelEncoder()
cat_X=cat_X.apply(le_test.fit_transform)

X_test=pd.merge(cat_X,num_X,right_index=True, left_index=True)
X_test=pd.merge(X_test,int64_X,right_index=True, left_index=True)

X_test= X_test.fillna(0)


X_test.id = id_test
X_all.y = y

X_test.to_csv('D:/Data Science/Kaggle/Russian house prize/data_test.csv',index=False)

X_all.to_csv('D:/Data Science/Kaggle/Russian house prize/data_train.csv',index=False)























###### model Random forest
from sklearn import cross_validation
from sklearn.ensemble import RandomForestRegressor

alg=RandomForestRegressor(random_state=1, n_estimators=100,min_samples_split=60,min_samples_leaf=50)
kf = cross_validation.KFold(X_all.shape[0], n_folds=3, random_state=1)

scores = cross_validation.cross_val_score(alg, X_all, np.log(y), cv=3)

print (scores)
print (scores.mean())




