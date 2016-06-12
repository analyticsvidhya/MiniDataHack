library(zoo)
library(data.table)
library(lubridate)
library(forecast)

train_data <- as.data.table(read.csv("Train_KQyJ5eh.csv"))
test_data <- as.data.table(read.csv("Test.csv"))

train_data$Date <- dmy(train_data$Date)
train_data$Date <- as.Date(train_data$Date)
Date <- as.Date(seq(as.Date("2007-01-01"), as.Date("2008-12-31"), by="days"))
new_train <- as.data.frame(Date)
new_train <- merge(new_train,train_data,by="Date",all.x=T)
new_train[is.na(new_train)] <- 0

x.ts = ts(new_train$Number_SKU_Sold, freq=365, start=c(2007))
hw1 <- HoltWinters(x.ts,seasonal = "additive",beta = FALSE)
test_data$Number_SKU_Sold <- predict(hw1,n.ahead = 365)
write.csv(test_data,file ="submission.csv",row.names = FALSE)
