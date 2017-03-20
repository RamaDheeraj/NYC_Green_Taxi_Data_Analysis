#Question-1
#Programmatically download and load into your favorite analytical tool the trip data for September 2015.
nyc_green <- read.csv("C:/Users/Dheeraj/Desktop/Files/Material/Capitolone/green_tripdata_2015-09.csv")
class(nyc_green)
nyc_green <- data.frame(nyc_green)
#Report how many rows and columns of data you have loaded.
cat('There are', nrow(nyc_green), 'rows and ',ncol(nyc_green),'columns in the data')
View(nyc_green)

#Question-2
#Plot a histogram of the number of the trip distance ("Trip Distance").
#Report any structure you find and any hypotheses you have about that structure.
hist(nyc_green$Trip_distance, col = "grey", main = "Distribution of Trip_distance")
sd(nyc_green$Trip_distance)
mean(nyc_green$Trip_distance)
median(nyc_green$Trip_distance)

#Calculating extreme outliers
ext_outlier_cutoff = round(mean(nyc_green$Trip_distance)+3*sd(nyc_green$Trip_distance),1)
ext_outlier_cutoff
#Number of outlier values
length(nyc_green$Trip_distance[nyc_green$Trip_distance>ext_outlier_cutoff])
length(nyc_green$Trip_distance[nyc_green$Trip_distance<=ext_outlier_cutoff])/length(nyc_green$Trip_distance)
hist(nyc_green$Trip_distance[nyc_green$Trip_distance<=ext_outlier_cutoff], 
     col = "grey", main = "Distribution of Trip_distance after removing outliers")

#Question-3
#Report mean and median trip distance grouped by hour of day.

#installig and loading lubridate package
install.packages("lubridate")
library(lubridate)
nyc_green$hours=hour(nyc_green$lpep_pickup_datetime)
attach(nyc_green)#attaching nyc_green for accessing variables by their names
ab=do.call(cbind.data.frame,aggregate(Trip_distance,list(nyc_green$hours),
                                      function(Trip_distance) c(mean=mean(Trip_distance),median=median(Trip_distance))))
names(ab)<- c("Hour","Mean","Median")
View(ab)
plot(ab$Hour,ab$Median, type = "l", lwd=3, col = "blue", xlim = c(0,24), ylim = c(1.5,4.5), xlab = "Hour of the day", ylab = "Mean/Median Trip Distance", main = "Average trip distance for each hour of the day")
lines(ab$Hour,ab$Mean, type = "l", lwd=3, col = "orange")
text(8,4,"Mean")
text(8.5,2.6,"Median")

#We'd like to get a rough sense of identifying trips that originate or terminate at one of the NYC area airports. 
#Can you provide a count of how many transactions fit this criteria, the average fair, and any other interesting characteristics of these trips.
jfk<-nyc_green[(Dropoff_latitude < 40.660 & Dropoff_latitude > 40.635 & Dropoff_longitude < -73.775 & Dropoff_longitude > -73.815)|
                 (Pickup_latitude < 40.660 & Pickup_latitude > 40.635 & Pickup_longitude < -73.775 & Pickup_longitude > -73.815),] 

View(jfk)
cat('There are', nrow(jfk),'trips originated or terminated at JFK airport' )
cat('Average fare of these trips is ',round(mean(jfk$Total_amount),2))
y=hour(jfk$lpep_pickup_datetime)
trips = aggregate(jfk$lpep_pickup_datetime,list(y),length)
names(trips)<- c("Hour of the day","Number of trips")
View(trips)

plot(trips$`Hour of the day`,trips$`Number of trips`, type = "l", lwd=3, col = "blue", xlab = "Hour of the day", ylab = "Number of trips", main = "Average # of trips to/from JFK for each hour of the day")
detach(nyc_green)#detaching nyc_green


#Question-4
#Build a derived variable for tip as a percentage of the total fare.

nyc_green$tip_pct<-ifelse(nyc_green$Tip_amount==0.00 | nyc_green$Total_amount==0.00 ,
                          0.00,round((nyc_green$Tip_amount/nyc_green$Total_amount)*100,2))

#Build a predictive model for tip as a percentage of the total fare. 
#Use as much of the data as you like (or all of it). We will validate a sample.

###############data Preparation############

str(nyc_green)#Structure

#Converting into factors
nyc_green$VendorID <- factor(nyc_green$VendorID)
nyc_green$RateCodeID <- factor(nyc_green$RateCodeID)
nyc_green$Payment_type <- factor(nyc_green$Payment_type)
nyc_green$Trip_type <- factor(nyc_green$Trip_type)
nyc_green$hours <- factor(nyc_green$hours)

cor(nyc_green[,c(10,11,12,13,14,15,16,18,19)])#correlations


#checking for missing values
missing=function(ipds)
{
  a=colnames(ipds)
  c=nrow(ipds)
  x=seq(1,ncol(ipds),by=1)
  op<-options(stringsAsFactors  = FALSE)
  opds=data.frame()
  for(i in x)
  {
    cn=a[i]
    nm=sum(is.na(ipds[,i]))
    pm=nm/c
    nu=length(unique(ipds[,i]))
    vec=c(cn,nm,pm,nu)
    opds=rbind(opds,vec)
  }
  colnames(opds)=c("Column Name","# Missing Values","% Missing Values","# Unique Values")
  options(op)
  return(opds)
}

missing(nyc_green)

nyc_green$Ehail_fee <- NULL
nyc_green <- nyc_green[!is.na(nyc_green$Trip_type),]
nyc_green <- nyc_green[!nyc_green$RateCodeID==99,]

#mahalanobis distance to find outlier
green_outlier <- nyc_green[,c(10,11,12,13,14,15,16,18)]
m.dist.order <- order(mahalanobis(green_outlier, colMeans(green_outlier), cov(green_outlier)), decreasing=TRUE)
is.outlier   <- rep(FALSE, nrow(green_outlier))
is.outlier[m.dist.order[1:2]] <- TRUE # Marked as outliers, the 2 most extreme points
col <- is.outlier + 1
nyc_green$outlier <- col
nyc_green <- nyc_green[nyc_green$outlier==1,]
nyc_green$outlier<-NULL
rm(green_outlier,col,is.outlier,m.dist.order)

#tip amount, toll amount, total_amount being less than 0
nyc_green <- nyc_green[!nyc_green$Tip_amount < 0|!nyc_green$Tolls_amount < 0|!nyc_green$Total_amount < 0,]

#deleting ambigious trip  distances
nyc_green <- nyc_green[!nyc_green$Trip_distance %in% c(112.60,603.10),]
#delete where tip amount is greater than fare amount
nyc_green <- nyc_green[nyc_green$Tip_amount < nyc_green$Fare_amount,]

cat("Initial number of records are 1494926, and after cleaning there are", nrow(nyc_green),"records")

#creating training and testing data
smple_size <- floor(0.7* nrow(nyc_green))
set.seed(33)
train_ind <- sample(seq_len(nrow(nyc_green)), size = smple_size)
lm_train <- nyc_green[train_ind, ]
lm_test <- nyc_green[-train_ind, ]

#Linear Model
lm_model <- lm(tip_pct ~ VendorID+RateCodeID+Passenger_count+
                 Extra+Tolls_amount+improvement_surcharge+
                 Total_amount+Payment_type+Trip_type+hours,data = lm_train)
summary(lm_model) #Adjusted R-squared: 0.68
predicted=predict(lm_model,lm_test)

rmse <- sqrt(mean((predicted-lm_test$tip_pct)^2)) 
rmse#4.78
mae <- mean(abs(lm_test$tip_pct-predicted)) 
mae#2.7

#Regression Trees
#installig and loading rpart package
install.packages("rpart")
library(rpart)
fit<-rpart(tip_pct ~ VendorID+RateCodeID+Passenger_count+
             Extra+Tolls_amount+improvement_surcharge+
             Total_amount+Payment_type+Trip_type+hours,data = lm_train,method="anova")
summary(fit)
predictr=predict(fit,lm_test)
rmse <- sqrt(mean((predictr-lm_test$tip_pct)^2)) 
rmse#4.81
mae <- mean(abs(lm_test$tip_pct-predictr)) 
mae#2.58
rms=nanstd()

#random forest
#installig and loading randomForest package
install.packages("randomForest")
library(randomForest) 
rf_model <- randomForest(tip_pct ~ VendorID+RateCodeID+Passenger_count+
                           Extra+Tolls_amount+improvement_surcharge+
                           Total_amount+Payment_type+Trip_type+hours,
                         data = lm_train,importance=TRUE,ntree=20)
summary(rf_model) #Adjusted R-squared: 0.68
predicted=predict(rf_model,lm_test)

rmse <- sqrt(mean((predicted-lm_test$tip_pct)^2)) 
rmse#4.43
mae <- mean(abs(lm_test$tip_pct-predicted)) 
mae#2.33


#Question-5
#Option A: Distributions
#Build a derived variable representing the average speed over the course of a trip

attach(nyc_green)#attaching nyc_green for accessing variables by their names
#Finding time taken for each trip
time_travelled <- as.numeric(difftime(strptime(Lpep_dropoff_datetime, "%Y-%m-%d %H:%M:%S"), 
                            strptime(lpep_pickup_datetime, "%Y-%m-%d %H:%M:%S"), units = "mins"))
                              
speed <- ifelse(Trip_distance==0.00 | time_travelled==0.00,0.00,
                round((Trip_distance/time_travelled)*60,2))

mean(speed)#15.31342

#Can you perform a test to determine if the average trip speeds are materially the same in all weeks of September? 
#If you decide they are not the same, can you form a hypothesis regarding why they differ?

day <- as.numeric(format(strptime(lpep_pickup_datetime, "%Y-%m-%d %H:%M:%S"),"%d"))#Extracting date of the trip
week_num <- ifelse(day<=7,1,ifelse(day<= 14,2,ifelse(day<=21,3,ifelse(day<=28,4,5))))#finfing week
avgspeed=aggregate(speed,list(week_num),mean)#Grouping speed of the trips by week 
names(avgspeed)<-c("Week","Average speed")
avgspeed

df=data.frame(speed,week_num)
fit = lm(formula = speed ~ week_num,data=df)
anova (fit)#performing one-way anova test(Null Hypothesis: mean of the speeds are equal in all weeks of september)

boxplot(speed~week_num,data=nyc_green,ylim=c(0,20),main='Average speed of trips'
        ,xlab='Week',ylab='Speed')
detach(nyc_green)#detaching nyc_green
