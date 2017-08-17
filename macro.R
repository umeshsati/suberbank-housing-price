library(readr)


macro <- read_csv("D:/Data Science/Kaggle/Russian house prize/macro.csv")
miss_no <- as.data.frame(names(macro))
for (i in names(macro)){
  miss_no <-cbind(miss_no,as.data.frame(sort(table(is.na(macro[[i]])))[1]))
}

miss_no <- miss_no[,!(names(miss_no)%in% c("names(macro)"))]
names(miss_no) <- c(names(macro))

