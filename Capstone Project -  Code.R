#CAPSTONE PROJECT - REAL ESTATE - BOSTON HOUSING DATASET 
#MULTIPLE LINEAR REGRESSION APPROACH

#Loading the essential packages
library(car) #To check for multicollinearity
install.packages("corrplot")
library(corrplot) #for correlation analysis

#Importing the Boston housing dataset from file location
boston <- read.csv("HousingData.csv") #506 observations of 14 variables in the dataframe

#DATA EXPLORATION
names(boston)
head(boston)
str(boston)
summary(boston)

#Histograms
hist(boston$CRIM, main = "Histogram of CRIM") 
hist(boston$ZN, main = "Histogram of ZN")  
hist(boston$INDUS, main = "Histogram of INDUS")
hist(boston$CHAS, main = "Histogram of CHAS")
hist(boston$NOX, main = "Histogram of NOX")
hist(boston$RM, main = "Histogram of RM")
hist(boston$AGE, main = "Histogram of AGE")
hist(boston$DIS, main = "Histogram of DIS")
hist(boston$RAD, main = "Histogram of RAD")
hist(boston$TAX, main = "Histogram of TAX")
hist(boston$PTRATIO,main = "Histogram of PTRATIO")
hist(boston$B,main = "Histogram of B")
hist(boston$LSTAT,main = "Histogram of LSTAT")
hist(boston$MEDV,main = "Histogram of MEDV")

#Boxplots
boxplot(boston$CRIM,main = "Boxplot of CRIM") 
boxplot(boston$ZN,main = "Boxplot of ZN") 
boxplot(boston$INDUS,main = "Boxplot of INDUS")
boxplot(boston$CHAS,main = "Boxplot of CHAS")
boxplot(boston$NOX,main = "Boxplot of NOX")
boxplot(boston$RM,main = "Boxplot of RM")
boxplot(boston$AGE,main = "Boxplot of AGE")
boxplot(boston$DIS,main = "Boxplot of DIS")
boxplot(boston$RAD,main = "Boxplot of RAD")
boxplot(boston$TAX,main = "Boxplot of TAX")
boxplot(boston$PTRATIO,main = "Boxplot of PTRATIO")
boxplot(boston$B,main = "Boxplot of B")
boxplot(boston$LSTAT,main = "Boxplot of LSTAT")
boxplot(boston$MEDV,main = "Boxplot of MEDV")

#Studying potential variables 
#Crime rate per town (CRIM)
hist(boston$CRIM)
nrow(subset(boston, CRIM < 10))#435 observations have crime rate less than 10
nrow(subset(boston, CRIM >50)) #4 observations have crime rate greater than 50
nrow(subset(boston, CRIM >80))#1 observation has crime rate greater than 80

#Average number of rooms (RM)
hist(boston$RM)
nrow(subset(boston, RM > 7))#64 observations have rooms more than 7
nrow(subset(boston, RM > 8))#13 observations have rooms more than 8
nrow(subset(boston, RM < 4)) #2 observations have rooms less than 4
nrow(subset(boston, RM < 3)) #No observations have rooms less than 3

#Bound the Charles river or not(CHAS)
hist(boston$CHAS)
nrow(subset(boston, CHAS ==1)) #34 observations bound the Charles river
nrow(subset(boston, CHAS==0))#452 observations don't bound the Charles river

#full-value property-tax rate per $10,000 (TAX)
hist(boston$TAX)
nrow(subset(boston, TAX> 600)) #137 observations pay tax rates higher than 600
nrow(subset(boston, TAX< 600)) #369 observations pay tax rates lower than 600

#DATA PREPARATION
#Copy the dataset boston to a new dataframe boston_new
boston_new <- boston

#Replace the missing values (NA's) from the variables with their respective median values
boston_new$CRIM[which(is.na(boston_new$CRIM))] <- median(boston_new$CRIM, na.rm= T)
boston_new$ZN[which(is.na(boston_new$ZN))] <- median(boston_new$ZN, na.rm= T)
boston_new$INDUS[which(is.na(boston_new$INDUS))] <- median(boston_new$INDUS, na.rm= T)
boston_new$CHAS[which(is.na(boston_new$CHAS))] <- median(boston_new$CHAS, na.rm= T)
boston_new$AGE[which(is.na(boston_new$AGE))] <- median(boston_new$AGE, na.rm= T)
boston_new$LSTAT[which(is.na(boston_new$LSTAT))] <- median(boston_new$LSTAT, na.rm= T)
summary(boston_new)

#correlation analysis
boston_cor <- cor(boston_new) #All 14 variables
corrplot(boston_cor, method = "circle") 

#Positive correlations are displayed in blue and negative correlations in red color.
#Color intensity and the size of the circle are proportional to the correlation coefficients.
#Variables are highly correlated to itelf.(example: CRIM with CRIM, ZN with ZN, etc.)

#OUTLIER TREATMENT USING IQR
#creating a vector containing names of variables we wish to remove the outliers if present.
variables <- c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", "RAD", 
               "TAX", "PTRATIO", "B", "LSTAT", "MEDV")

#creating the object outliers to store the row id's containing  outliers for removal
outliers <-  c()

#creating a boundary for each variable (0.95 and 0.05 for 3 S.D from the mean)
#To loop through the columns specified
for(i in variables){
  
  #Get the min/max values(Boundaries for each variable)
  max <- quantile(boston_new[,i], 0.95, na.rm=TRUE) + (IQR(boston_new[,i],na.rm=TRUE))
  min <- quantile(boston_new[,i],0.05,na.rm=TRUE) -  (IQR(boston_new[,i],na.rm=TRUE))
  
  #Get row ids which contain outliers
  id <- which(boston_new[,i] < min| boston_new[,i] > max)
  
  
  #Print the number of outliers in each variable
  print(paste(i, length(id), sep =''))
  
  #Append the outliers list
  outliers <- c(outliers, id)
  
} #loop closure

#sorting the outliers
outliers <- sort(outliers)

#remove the outliers from the dataset
boston_new <- boston_new[-outliers,]

#Convert variables CHAS and RAD from integer to factor
boston_new$CHAS <- as.factor(boston_new$CHAS)
boston_new$RAD <- as.factor(boston_new$RAD)
levels(boston_new$RAD) <- c(1,2,3,4,5,6,7,8,24) 
str(boston_new)
summary(boston_new)

#MODEL BUILDING
#Split the dataset boston_new into train and test data (70% for train and 30% for test)
set.seed(1234)
training <- sample(1:nrow(boston_new), 0.7*nrow(boston_new))
train <- boston_new[training,]
test <- boston_new[-training,]

#Model building with all the independant variables
fit <- lm(log(MEDV) ~., data= train)
summary(fit) #Adjusted R-squared:  0.7652 

#Model building by dropping insignificant variables ZN, INDUS, AGE
fit1 <- update(fit, ~. - ZN - INDUS - AGE)
summary(fit1)#Adjusted R-squared:  0.7664

#Checking for multicollinearity
vif(fit1, th=5)#RAD and TAX have VIF greater than 5

#Model building by dropping RAD and TAX
fit2 <- update(fit1, ~. - RAD - TAX)
summary(fit2)#Adjusted R-squared:  0.7482 

#PERFORMANCE OF THE MODEL
predict_train <- predict(fit2, train) #for known data
predict_test <- predict(fit2, test) #for unseen data

#Calculating Mean square error as performance metrics for regression
mse_train <- mean((exp(predict_train) - train$MEDV)^2)
mse_train #16.81506

mse_test <- mean((exp(predict_test) - test$MEDV)^2)
mse_test # 21.24562

#Model diagnostics of fit2
durbinWatsonTest(fit2) #DW statistic is 2.067375
#Errors are independent. They aren't autocorrelation. Failed to reject null hypothesis.

hist(fit2$residuals) #errors are normally distributed

plot(train$MEDV , fit2$residuals) #variance is constant i.e Homoscedasticity of variance
abline(h=0, col = "red") 

par(mfrow = c(2,2)) 
plot(fit2) #Residual versus fitted values

#Predicted results for our model
train$predicted_MEDV <- exp(predict_train) #known data #Predicted values are now in train data frame
test$predicted_MEDV <- exp(predict_test) #unseen data #Predicted values are now in test data frame

