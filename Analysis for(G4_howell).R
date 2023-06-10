#..........................................................................
#Set your R work directory.
#setwd('C:\Users\lenovo\Desktop\Data\data2\r\R\datasets')
#getwd()

#Read your dataset.
df1<-read.csv('G4_howell.csv')

#Deal with NA and empty data.
df1 <- read.csv('G4_howell.csv', na.strings = c('','NA'))

#View your dataset on R environment.
View(df1)
#..........................................................................



#..........................................................................
#Dataset Sense
nrow(df1)
ncol(df1)
colnames(df1)
rownames(df1)

#Remove some columns from dataset
newdf1<-df1[-c(1,2)]
newdf2<-df1[-c(1:2)]

#Select specific data.
s1<-df1[2,4]
s2<-df1[2,'age']

#Get a specific column.
col1<-df1[,1]
col2<-df1[,'age']
col3<-df1[['age']]
col4<-df1['age']

#Get specific rows and columns.
rc <- df1[ c(1:10), c("age", "sex")]
#..........................................................................



#..........................................................................
#Filter the dataset according to specific criteria.
filter1 <- df1[df1$sex == 'M' , ]
filter1
filter2 <- df1[df1$age > 30, c(1,2,3)]
filter2
filter3 <- df1[df1$sex=='M'& df1$height>160,c(4,2)]
filter3

#Re-coding means use different values for a variable.
df1$bodystatus[df1$sex =='M'& df1$height>150] = 'Long Man'
df1$bodystatus[df1$sex =='M'& df1$height<150] = 'Short Man'
df1$bodystatus[df1$sex =='F'& df1$height>150] = 'Long Woman'
df1$bodystatus[df1$sex =='F'& df1$height<150] = 'Short Woman'


#Sorting the dataset according to specific criteria.
sortIncome1 <- df1[order(df1$age), ]
sortIncome2 <- df1[order(df1$weight), ]
sortIncome3 <- df1[order(-df1$height), ]
sortIncome4 <- df1[order(df1$height, df1$bodystatus), ]


#Computing new variable from existing once data.
df1$weight[df1$weight == "NA"] <- NA
df1$height[df1$height == "NA"] <- NA
df1$weight <- as.numeric(gsub(" kg", "", df1$weight))
df1$height <- as.numeric(gsub(" cm", "", df1$height))
df1$BMI <- df1$weight / ((df1$height/100)^2)
df1$Age_Group <- ifelse(df1$age <= 18, "Child",
                        ifelse(df1$age <= 30, "Young Adult",
                               ifelse(df1$age <= 50, "Adult", "Senior")))
df1$Mean_Weight_Height <- (df1$weight + df1$height) / 2

#Delete a column.
df1$Overweight<-NULL
#..........................................................................



#..........................................................................
# way the solve the problem of missing values
#Multiple Imputation Technique
install.packages('mice')
library(mice)
imputation <- mice(df1,
                   m = 5,
                   meth = c("","","pmm","pmm","","pmm","", "pmm"),
                   maxit = 20)
print(imputation$imp)
mynewdata<-complete(imputation,5)

#another way the solve the problem of missing values
#Use tidyr library used to drop NA values
install.packages('tidyr')
library(tidyr)
clean <- drop_na(df1)
x<-mean(clean$weight)
clean$newweight <- as.factor(ifelse(clean$weight > x, "precious", "thin"))
#..........................................................................



#..........................................................................

#To get first 20 rows, we can see NA and <NA
head(df1,20)
tail(df1,20)

#To get all locations of NA
complete.cases(df1)   #which FALSE referce to existing of NA
!complete.cases(df1)   #which TRUE referce to existing of NA

#Get all rows contain missing data
df1[! complete.cases(df1), ]
df1[! complete.cases(df1), c(1,3)]

#Remove text in numeric values
df1$weight <- as.numeric(gsub(" kg", "", df1$weight))
df1$height <- as.numeric(gsub(" cm", "", df1$height))

# structure of dataset
str(df1)

#Display some statics about the data
summary(df1)

#Change Any variable from int to chr Which it will not used in analysis/visualization
# there is not any variable need this step
#..........................................................................




#..........................................................................
#Data visualization
install.packages("tidyverse")
library(tidyverse)
library(ggplot2)

#Dot plot
draw1 <- ggplot(df1)
draw1 <- ggplot(df1, aes(x=weight, y=BMI))
draw1+geom_point()

#Histogram chart
ggplot(df1, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  labs(x = "Age", y = "Frequency") +
  ggtitle("Histogram of Age")

ggplot(df1, aes(x = weight)) +
  geom_histogram(binwidth = 5, fill = "green", color = "white") +
  labs(x = "Weight", y = "Frequency") +
  ggtitle("Histogram of Weight")

ggplot(df1, aes(x = height)) +
  geom_histogram(binwidth = 5, fill = "orange", color = "white") +
  labs(x = "Height", y = "Frequency") +
  ggtitle("Histogram of Height")

#Bar chart 
ggplot(df1, aes(x = sex)) +
  geom_bar(fill = "steelblue") +
  labs(x = "Sex", y = "Frequency") +
  ggtitle("Bar Chart of Sex Frequency")

#Scatter plot
ggplot(df1, aes(x = age, y = weight)) +
  geom_point() +
  labs(x = "Age", y = "Weight (kg)") +
  ggtitle("Scatter Plot of Age vs Weight")
#..........................................................................