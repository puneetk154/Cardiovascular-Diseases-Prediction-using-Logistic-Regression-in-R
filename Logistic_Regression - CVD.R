# Import some libraries
library(htmlTable)
library(tidyverse)
library(ggplot2)
library(rvest)
library(naniar)

# Import the data from below mentioned online source
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"

df<- read.csv(url, header = F)

# Change the Column names
colnames(df) <- c('Age','Sex','Chest_Pain_Type','Resting_BP','Cholestrol','Fasting_BP','Resting_ECG','Thal_Achieved','Exercise_Angina','Oldpeak','Slope','Ca','Thal','Heart_Disease')

# Check the Datatypes of the Columns
str(df)

# Check the Missing Values
sum(is.na(df))

# Visualize the Missing Values
vis_miss(df)

# Alternate Way to check the Missing Values
sapply(df, function(x)sum(is.na(x)))

# Change the Numeric Values to Factors (Categorical)
df$Sex <- as.factor(df$Sex)
df$Chest_Pain_Type <- as.factor(df$Chest_Pain_Type)
df$Fasting_BP <- as.factor(df$Fasting_BP)
df$Resting_ECG <- as.factor(df$Resting_ECG)
df$Exercise_Angina <- as.factor(df$Exercise_Angina)
df$Slope <- as.factor(df$Slope)
df$Ca <- as.factor(df$Ca)
df$Thal <- as.factor(df$Thal)

df$Heart_Disease <- ifelse(df$Heart_Disease == 0, yes = 0, no = 1)

str(df)


# Replace the Question Marks and special characters in the dataframe
df[df == '?'] <- NA

sum(is.na(df))

sapply(df, function(x)sum(is.na(x)))

# Replace the Missing Values
df$Ca <- as.numeric(df$Ca)
df$Ca <-  ifelse(is.na(df$Ca), ave(df$Ca,FUN=function(x)mean(x,na.rm=T)),df$Ca)

df$Thal <- as.numeric(df$Thal)
df$Thal <-  ifelse(is.na(df$Thal), ave(df$Thal,FUN=function(x)mean(x,na.rm=T)),df$Thal)

# Convert back Ca and Thal to Factor
df$Ca <- as.factor(df$Ca)
df$Thal <- as.factor(df$Thal)

# Check the Missing Values
sapply(df, function(x)sum(is.na(x)))

# Data Analysis
xtabs(~Sex+Heart_Disease, data= df)

xtabs(~Chest_Pain_Type+Heart_Disease, data= df)
xtabs(~Fasting_BP+Heart_Disease, data= df)
xtabs(~Resting_ECG+Heart_Disease, data= df)
xtabs(~Ca+Heart_Disease, data= df)

str(df)

sum(is.na(df))


# Model Building
logistic <- glm(Heart_Disease~Sex, data=df, family = "binomial")

summary(logistic)

# Model Building for the entire dataset
logistic <- glm(Heart_Disease~., data=df, family = "binomial")

summary(logistic)


# Train - Test split
library(caTools)
split = sample.split(df$Heart_Disease, SplitRatio = 0.8)

train_data <- subset(df, split == T)
test_data <- subset(df, split == F)
view(test_data)

nrow(train_data)

# Model Building on Train dataset
logistic <- glm(Heart_Disease~., data=train_data, family = "binomial")
summary(logistic)


# From the above stats  we can see that variables : slope, sex, cp, ca, thal are factors which play important role in the model 
# Hence we can use only these variables and remove other : 
# Now our model formulas will be : 
logistic_m <- glm(Heart_Disease~Sex+Slope+Chest_Pain_Type+Ca+Thal, family = "binomial", data = train_data)
summary(logistic_m)

# Prediction :
p1 <- predict(logistic_m, train_data, type = "response")

# Lets compare it with Train and Test dataset 
head(train_data)
pred1 <- ifelse(p1>0.5, 1,0)
table(pred=pred1,Actual = train_data$Heart_Disease)

p2 <- predict(logistic_m, test_data, type = "response")
pred2 <- ifelse(p2>0.5, 1,0)

table(prediction=pred2,Actual = test_data$Heart_Disease)


