install.packages("neuralnet") 
library(neuralnet)


#Loading the Rossman Datasets into the dataframes:
#Laoding Train Data in dataframe
train_data<- read.csv(file="train.csv/train.csv")
train_data

#Loading Store Data

store_data<-read.csv(file = "store.csv/store.csv")
store_data

#Loading test data in dataframe:
test_data <- read.csv(file="test.csv/test.csv")
test_data

#Exploring the train data set. 

summary(train_data)

#Checking for Null values, if present in the dataset

unique(is.na(train_data))

#There are no null values present. 
#Exploring Open and Promo variables which seems to factor levels.

unique(train_data$Open)
unique(train_data$Promo)
unique(train_data$StateHoliday)
unique(train_data$SchoolHoliday)


#Transforming the above mentioned columns to TRUE or FALSE values:
  

train_data["Open"] <- train_data["Open"]==1
train_data["Promo"] <- train_data["Promo"]==1
train_data["SchoolHoliday"]<- train_data["SchoolHoliday"]==1



summary(train_data)

# Exploring the Store Dataset
 
summary(store_data)
 

Exploring the Test Dataset
 
summary(test_data)
 
#Test_data$Open columns have 11 missing values. we will have to use the imputation techniques to replace the missing values for this column.
#We will transform the other columns as we did in train_data dataframe.

 
test_data["Promo"] <- test_data["Promo"]==1
test_data["SchoolHoliday"]<- test_data["SchoolHoliday"]==1
new_data<-subset(test_data, is.na(test_data$Open))
 
# Replacing the missing value in the Open Column with value generated at random from the observed distribution.
# We can also use mode to replace the missing values of the given categorical variable.


 
obs_Open<-sample(na.omit(test_data$Open), 1)
new_data$Open<-obs_Open
test_data$Open[is.na(test_data$Open)] <- new_data$Open
test_data$Open<- test_data$Open==1
#Checking for null columns 
test_data$Open[is.na(test_data$Open)] 
 
# Since the test datasets  are not using the Customers Column to predict Sales values, we will have to remove it from our train dataset. 
# Otherwise, The Customers Column in train dataset can contribute towards predicting Sales value which can lead to wrong prediction as we dont have customers column as feature in test dataset.
# We will have to predict the sales in test dataset based on the available columns in the test dataset.
# We also need to decompose the date column to some useful data so that it can be fed to machine learning model. 
# Otherwise, it wont be useful as date is not processed by many Machine Learning algorithms.
# Thus we will decompose the date to day,month,year and week of the year and will drop the redundant columns.
 
#Decomposing Date
train_data$DateDay<- as.numeric(strftime(train_data$Date,format="%d"))
train_data$DateMonth<- as.numeric(strftime(train_data$Date,format="%m"))
train_data$DateYear<- as.numeric(strftime(train_data$Date,format="%y"))
train_data$DateWeek<- as.numeric(strftime(train_data$Date,format="%w"))
 
# Dropping the Date and Customers Column from the train dataset as they are redundant.
 
#Dropping the Date and Customers Column
train_data <- subset(train_data,select=-c(Customers,Date))
#names(train_data)
#train_data[c("Customers","Date")]
 
# Performing similar operations in test dataframe for date column:
 
test_data$DateDay<- as.numeric(strftime(test_data$Date,format="%d"))
test_data$DateMonth<- as.numeric(strftime(test_data$Date,format="%m"))
test_data$DateYear<- as.numeric(strftime(test_data$Date,format="%y"))
test_data$DateWeek<- as.numeric(strftime(test_data$Date,format="%w"))
#Dropping Date column
test_data <- subset(test_data,select=-c(Date))
 
# Also Dropping the Id Column in the test dataset.
 
#Dropping Id column as It is redundant here
test_data <- subset(test_data,select=-c(Id))
 

# Exploratory Data Analysis

# 1. Here we visualize a barplot of sales vs days of the week
 
agg<-aggregate(train_data$Sales, by=list(train_data$DayOfWeek), FUN=mean)
barplot(agg$x, main="sales vs day of week", xlab="Days of week", names.arg=c("1", "2", "3","4","5","6","7"),col="red",ylab="Mean Sales")
 

# From the above barplot, we can see that mean sale for the first day of week is highest.
# 2. Histogram for Sales analysis
 
hist(train_data$Sales,xlab="Sales Amount",main="Histogram of Sales")
 
# From the above histogram, we can see sales distribution from 0 to approximately 28000. Sales range majorly lies between 5000 to 6000 units.
# 3. Plot for average sale per month
 
agg2<-aggregate(train_data$Sales, by=list(train_data$DateMonth), FUN=mean)
plot(agg2$x, main="sales vs Month", xlab="Months", col="blue")
 
# In this case, we plot the average sale for every month. It is observed that December has the highest average sale and January has least.
# 4.  Store type impact on sale
 
#Aggregate data of sales of each year and combine it with the store data 
agg_storeSales<-aggregate(train_data$Sales, by=list(train_data$Store), FUN=mean)
colnames(agg_storeSales)<-c("Store","Sales")
merge_salesstore<-merge(x=agg_storeSales,y=store_data,by = "Store", all = TRUE)
write.csv(merge_salesstore,file="merge_salesstore.csv")

#How store type is affecting the sales
agg_type<-aggregate(merge_salesstore$Sales, by=list(merge_salesstore$StoreType), FUN=mean)
barplot(agg_type$x, main="type vs sales", col=c("blue","black","red","green"), ylab="average sales", xlab="store type", names.arg=c("a","b","c","d"))
 
# From the above barplot, we can observe the impact of store type on sales. There are 4 store types a,b,c and d. From the above visualization, it is observed that store type b has maximum mean average sale, while a, c and d have same mean average sale.
# 5. Impact of assortment on sales
 
agg_assort<-aggregate(merge_salesstore$Sales, by=list(merge_salesstore$Assortment), FUN=mean)
barplot(agg_assort$x, main="Assortment vs sales",col=c("blue","red","green"), ylab="average sales", xlab="assortment", names.arg=c("basic","extra","extended"))
 
# Here we analyse impact of assortment on sales. There are 3 types of assortments in the dataset (basic, extra and extended). We plot the mean average sale for these assortment types. As we can see from the above bar plot, highest average sale is observed for extra assortment, followed by extended and basic.

# Impact of state holiday on sale
 
agg_stateholi<-aggregate(train_data$Sales, by=list(train_data$StateHoliday), FUN=mean)
barplot(agg_stateholi$x, main="state holiday vs sales", ylab="average sales", xlab="State holiday", col=c("blue","purple","yellow","orange"), names.arg=c("None","Public","Easter","Christmas"))
 
# From the above plot, we observe that highest average sale was observed when there were no holidays, whereas for all other holidays, the mean average sale is approximately the same and far less than the mean average sale for non holiday.

# Density plot for overall sale
 
d <- density(train_data$Sales) # returns the density data 
plot(d, col="brown")
 
# Here we have the density plot for overall sale distribution. We can see sales distribution from 0 to approximately 28000.

# Impact of promos and distancewise competition on sales
 
merge_salesstore<-merge_salesstore[complete.cases(merge_salesstore[,"CompetitionDistance"]),]
#summary(merge_salesstore)
#Aggregate the sales by same distance of competititon stores
compdis<-aggregate(merge_salesstore$Sales,by= list(merge_salesstore$CompetitionDistance),FUN=mean)
colnames(compdis)<-c("Distance to competition","Mean Sales")
#plot(x=compdis$`Distance to competition`,y=compdis$`Mean Sales`)
#Create range of the distances and aggregate them
generalcompetitiondistance<-aggregate(compdis$`Mean Sales`, list(cut(compdis$`Distance to competition`, breaks=c(0,5000,10000,15000,20000,25000,30000))), mean)
#plot(x=generalcompetitiondistance$Group.1,y=generalcompetitiondistance$x)
with(merge_salesstore, symbols(x=merge_salesstore$CompetitionDistance, y=merge_salesstore$Sales, circles=merge_salesstore$Promo2, inches=1/15,
ann=F, bg="steelblue2", fg=NULL))
title(main="Promo and Distance impact", 
xlab="Competition Distance", ylab="Sale")
legend("topright",title = "Promo",c("0","1"),pch=c(20,19), col ="steelblue2")
 
# In the above plot, smaller circle represents there is no promo offered whereas the larger circle implies promos offered. We can observe here that smaller the competition distance, more promos are offered. We can assume that more promos are offered when competition distance is less to improve the sale. We can see a two sample t-test to justify that promo is affecting sales.
 
#As probability us low, we can justify that promo is highly affecting the sales.
t.test(train_data[train_data$Promo,]$Sales,train_data[!train_data$Promo,]$Sales)
 
# Data Modelling and Result Analysis

# 1. Linear Regression
#Replacing State Holiday values with numeric values
 
levels(train_data$StateHoliday) <- 0:3
str(train_data)
 
# Converting all non mumeric values to numeric for ease of model building
 
train_data$Open <- as.numeric(train_data$Open)
typeof(train_data$Open)
train_data$Promo <- as.numeric(train_data$Promo)
typeof(train_data$Promo)
train_data$SchoolHoliday <- as.numeric(train_data$SchoolHoliday)
typeof(train_data$SchoolHoliday)
train_data$StateHoliday <- as.numeric(train_data$StateHoliday)
typeof(train_data$StateHoliday)
train_data$Store <- as.numeric(train_data$Store)
typeof(train_data$Store)
train_data$DayOfWeek <- as.numeric(train_data$DayOfWeek)
typeof(train_data$DayOfWeek)
train_data$Sales <- as.numeric(train_data$Sales)
typeof(train_data$Sales)
str(train_data)
 
# Linear Regression
# 
# Dividing Data into train and test sets
 
set.seed(100)  
trainingRowIndex <- sample(1:nrow(train_data), 0.8*nrow(train_data)) 
trainingData <- train_data[trainingRowIndex, ] 
testData  <- train_data[-trainingRowIndex, ]
 

 
lmMod <- lm(Sales ~ Store + Open + DayOfWeek + SchoolHoliday + StateHoliday + Promo, data=trainingData)  # build the model
Pred <- predict(lmMod, testData)
summary (lmMod)
AIC (lmMod)
 
# We can also see that model's p value (0.00000000000000022204) is less than significane level and hence we have built a statistically significant model. The Adjusted R-squared and R squared are also comparable. From the above results we can see that std.error is 22.72.

#Calculating Prediction Accuracy
 
actuals_preds <- data.frame(cbind(actuals=testData$Sales, predicteds=Pred))
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy
 
# It is observed that prediction accuracy is 73.12% approx

# 2. ARIMA TIMESERIES
install.packages("ts", repos = "https://cran.r-project.org")
install.packages("forecast", repos = "https://cran.r-project.org")
library(forecast)
 
train_data<- read.csv(file="train.csv/train.csv")
train_data$Date<-as.Date(train_data$Date, format = "%Y-%m-%d")
train_data$Month_Yr <- format(as.Date(train_data$Date), "%Y-%m")
agg_sales<-aggregate(train_data$Sales, by=list(train_data$Month_Yr), FUN=sum)
colnames(agg_sales)<-c("Month_year","Total_sales")

x = ts(agg_sales[,2], start = c(2013,1), frequency=12) # frequency is the number of time per year we observed the data points. start is the time of the first observation (month) in the data.
x
plot(x)
z = log10(x)
plot(z)
y = diff(z)
plot(y)
PP.test(y) # Phillips-Perron Unit Root Test
#The p-value is 0.01 which suggest the rejection of the null hypothesis.
# Therefore, this time series is not random walk.
par(mfrow = c(1,2))
acf(y,main='ACF Tractor Sales')
pacf(y,main='PACF Tractor Sales')

ARIMAfit = auto.arima(z, approximation=FALSE,trace=TRUE)               
summary(ARIMAfit)
pred = predict(ARIMAfit, n.ahead = 36)
pred
par(mfrow = c(1,1))
plot(x,type='l',xlim=c(2013,2018),xlab = 'Year',ylab = 'Sales')
lines(10^(pred$pred),col='blue') 
 


# 3. Neural Network
# Splitting data for train and test
 
train_data$Open <- as.numeric(train_data$Open)
typeof(train_data$Open)
train_data$Promo <- as.numeric(train_data$Promo)
typeof(train_data$Promo)
train_data$SchoolHoliday <- as.numeric(train_data$SchoolHoliday)
typeof(train_data$SchoolHoliday)
train_data$StateHoliday <- as.numeric(train_data$StateHoliday)
typeof(train_data$StateHoliday)
train_data$Store <- as.numeric(train_data$Store)
typeof(train_data$Store)
train_data$DayOfWeek <- as.numeric(train_data$DayOfWeek)
typeof(train_data$DayOfWeek)
train_data$Sales <- as.numeric(train_data$Sales)
typeof(train_data$Sales)
str(train_data)
samplesize = 0.60 * nrow(train_data)
set.seed(80)
index = sample( seq_len ( nrow ( train_data ) ), size = samplesize )
maxs <- apply(train_data,2,max)
maxs
mins <- apply(train_data,2,min)
mins
#scale = as.data.frame(scale(train_data, center = mins, scale = maxs - mins))
dtrain = train_data[index,]
dtest = train_data[-index,]
#datatrain = scale[index, ]
#datatest = scale[-index, ]
 

 
#n <- names(datatrain)
#n
#f <- as.formula(paste("Sales ~", paste(n[!n %in% "Sales"], collapse = " + ")))
#f
#nn <- neuralnet(f,data=datatrain,hidden=3,linear.output=TRUE)
nn <- neuralnet(formula = Sales ~ Store + Open +Promo + SchoolHoliday + StateHoliday, data=dtrain, hidden =3, linear.output=FALSE)
nn$result.matrix
plot(nn)
 

 
temp_test <- subset(dtest,select = c("Store","Open","Promo","SchoolHoliday","StateHoliday"))
pr.nn <- compute(nn,temp_test)
pr.nn_ <- pr.nn$net.result*(max(train_data$Sales)-min(train_data$Sales))+min(train_data$Sales)
test.r <- (dtest$Sales)*(max(train_data$Sales)-min(train_data$Sales))+min(train_data$Sales)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(dtest)
MSE.nn
 

 
par(mfrow=c(1,2))
plot(dtest$Sales,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')