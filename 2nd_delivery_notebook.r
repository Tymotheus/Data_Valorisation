# Get all the necessary libraries
library(tidyverse)
library(ggplot2)
library(plotly) #interactive visualisation
library(lattice) #data exploration (corellations)
library(dplyr)
library(countrycode) # For adding continent variable
library(corrplot)
library(rpart)
library(rpart.plot)

# Palettes
palette1 <- colorRampPalette(c("#803934","#3f67ab","#613fab"))
palette2 <-colorRampPalette(c("#5000ff","#d900ff","#ff0077"))
palette3 <-colorRampPalette(c("#5e9159","#599190","#645991"))


# Import the dataset from your local filesystem
# Paste your path to .csv below
df <- read.csv('/Users/marinabuenogarcia/Documents/MASTER/UCA/DataValorisation/Kaggle project/Levels_Fyi_Salary_Data.csv')
df_raw <- df


## 1st Delivery
#Data preprocessing, plots and first analysis

# We eliminate variables that do not contain any values for all the records, or the ones that are irrelevant
df <- subset(df, select = -c(rowNumber,
                             Some_College,
                             Highschool,
                             tag,
                             otherdetails,
                             dmaid, 
                             cityid,
                             stockgrantvalue,
                             level)
             )


# Splitting country location into states and cities - this one could probably have some more efficient map-reduce rewriting
for (i in 1:length(df$location)) {
  aux <- strsplit(df$location[i], ", ")[[1]]
  if(length(aux) > 2){
    df$country[i] = aux[3]
  }
  else{
    df$country[i] = "US" # American records contain only city and state. Other records are splitted into 3
  }
  df$state[i] <- aux[2]
  df$city[i] <- aux[1]
}


# We create a lookup table to match countries with their continents
country_continent <- data.frame(
  country <- c("US", "United Kingdom", "Ireland", "India", "Belarus", "Canada", "Russia", "Netherlands", "Switzerland", "Singapore", "Germany", "Japan", "Sweden", "Australia", "United States", "Israel", "Poland", "China", "Austria", "Luxembourg", "Czech Republic", "France", "Pakistan", "New Zealand", "Denmark", "Hong Kong (SAR)", "South Africa", "Spain", "United Arab Emirates", "Hungary", "Brazil", "Bulgaria", "Philippines", "Indonesia", "Puerto Rico", "Taiwan", "Romania", "Mexico", "Costa Rica", "Marshall Islands", "Vietnam", "Panama", "Argentina", "Norway", "Moldova", "Estonia", "Kenya", "Turkey", "Italy", "Lithuania", "Nigeria", "Korea", "Ukraine", "Jordan", "Thailand", "Colombia", "Serbia", "Portugal", "Guatemala", "Yugoslavia", "Uruguay", "Slovakia", "Bangladesh", "Finland", "Chile", "Malaysia", "Latvia", "Saudi Arabia", "Peru", "Netherlands Antilles", "Belgium", "Burma", "Qatar", "Ghana", "Kazakhstan", "Uzbekistan", "Armenia", "Morocco", "Iraq", "Trinidad and Tobago", "Egypt"),
  continent <- c("North America", "Europe", "Europe", "Asia", "Europe", "North America", "Europe", "Europe", "Europe", "Asia", "Europe", "Asia", "Europe", "Oceania", "North America", "Asia", "Europe", "Asia", "Europe", "Europe", "Europe", "Europe", "Asia", "Oceania", "Europe", "Asia", "Africa", "Europe", "Asia", "Europe", "South America", "Europe", "Asia", "Asia", "North America", "Asia", "Europe", "North America", "North America", "Oceania", "Asia", "North America", "South America", "Europe", "Europe", "Europe", "Africa", "Europe", "Europe", "Europe", "Africa", "Asia", "Europe", "Asia", "Asia", "South America", "Europe", "Europe", "North America", "Europe", "South America", "Europe", "Asia", "Europe", "South America", "Asia", "Europe", "Asia", "South America", "North America", "Europe", "Asia", "Asia", "Africa", "Asia", "Asia", "Asia", "Africa", "Asia", "North America", "Africa")
)


# We add a continent variable to df, using our lookup table
df$continent <- country_continent$continent[match(df$country, country_continent$country)]


# We extract the year value
df$timestamp <- sapply(strsplit(as.character(df$timestamp), " "), `[`, 1) # First we filter out the time - we will not use it
sapply(strsplit(df$timestamp ,"/"), `[`, 3) -> df$timestamp # We retrieve the year

timestamp_proportions <- 100*table(df$timestamp)/length(df$timestamp)
sort(timestamp_proportions)
# 2017      2018      2019      2021      2020 
# 0.295329  6.217873 17.861818 37.398231 38.226749 

length(unique(df$company))#1633
tolower(df$company)->df$company #1102 Values


# We detect that there are 2,3 names for several companies i.e microsoft,
# microsoft services, microsoft corp. We unify them to one
df[grepl("moody", df$company),]["company"] <- "moody"
df[grepl("microsoft", df$company),]["company"] <- "microsoft"
df[grepl("amazon", df$company),]["company"] <- "amazon"
df[grepl("deloitte", df$company),]["company"] <- "deloitte"
df[grepl("verizon", df$company),]["company"] <- "verizon"
df[grepl("apple", df$company),]["company"] <- "apple"
df[grepl("genomics", df$company),]["company"] <- "genomics"
df[grepl("accenture", df$company),]["company"] <- "accenture"
df[grepl("adobe", df$company),]["company"] <- "adobe"
df[grepl("alibaba", df$company),]["company"] <- "alibaba"
df[grepl("akamai", df$company),]["company"] <- "akamai"
df[grepl("walmart", df$company),]["company"] <- "walmart"
df[grepl("jpmorgan", df$company),]["company"] <- "jpmorgan"
df[grepl("google", df$company),]["company"] <- "google"
df[grepl("facebook", df$company),]["company"] <- "facebook"
df[grepl("oracle", df$company),]["company"] <- "oracle"
df[grepl("intel", df$company),]["company"] <- "intel"
df[grepl("salesforce", df$company),]["salesforce"] <- "intel"

length(unique(df$company)) #1094

company_proportions <- 100*table(df$company)/length(df$company)
sort(company_proportions,decreasing=TRUE)[1:9] # We print 9 most popular companies
# amazon     microsoft     google   facebook      apple     oracle salesforce      intel       ibm
# 13.142939   8.384151   6.972957   4.795505   3.283739   1.824654   1.700137   1.583602  1.479838
 
selected_companies <- sort(company_proportions,decreasing=TRUE)[1:9]
df$company[!(df$company %in% names(selected_companies))] <- "Other" # Now we change name of all companies that do not belong to top 9 to "Other"
company_proportions <- 100*table(df$company)/length(df$company)
selected_companies <- sort(company_proportions,decreasing=TRUE)[1:10] # And include it as the 10th one 


# Time to check the yearly compensation and filter out the outliers
min(df$totalyearlycompensation)
max(df$totalyearlycompensation)
sum(is.na(df["totalyearlycompensation"])) # There should be no records with missing salary 
df <- df[df$totalyearlycompensation <= 2500000,] # We filter out people that earn more than 2.5 million $ per year as outliers
max(df$totalyearlycompensation)


# Select the main titles and define the rest as "Other"
title_proportions <- 100*table(df$title)/length(df$title)
sort(title_proportions,decreasing=TRUE)[1:5]
# Software Engineer              Product Manager Software Engineering Manager  Data Scientist            Hardware Engineer 
#         65.822664                     7.457135                     5.696223        4.115713                     3.512245 
title_proportions <- sort(title_proportions,decreasing=TRUE)[1:5]
df$title[!(df$title %in% names(title_proportions))] <- "Other"


# Plots the companies that the employees were hired in
unique(df$company)
q <- ggplot(data.frame(df$company), aes(x=df$company)) + geom_bar(fill = "steelblue")
q + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  ggtitle("Companies") + xlab("Company Name")
q

# Visualization of total yearly compensation and years of experience
d <- ggplot(df, aes(x = totalyearlycompensation, y = yearsofexperience)) + 
  geom_point()
d + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# Some more plots for countries versus companies
# We only work with US, India, United Kingdom, Germany, Canada
b <- subset(df, country %in% c("US", "India", "United Kingdom", "Germany", "Canada"))
unique(b$country)
q <- ggplot(data.frame(b$country), aes(x=b$country)) + geom_bar()
q + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
a <- subset(df, country != "US")
c <- ggplot(a, aes(x=country)) + geom_bar()
c + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# Create a scatter plot of location against country
d <- ggplot(a, aes(x = company, y = country)) + 
  geom_point()
d + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
df <- subset(df, select = -c(location))

# Histogram with years of experience
df <- df[df$yearsofexperience < 60,]
hist(df$yearsofexperience, main = "Years of experience", xlab="Years of experience", col=palette1(5))

# yearsatcompany [0,40]
min(df$yearsatcompany)
max(df$yearsatcompany)
sum(is.na(df$yearsatcompany))
hist(df$yearsatcompany, main = "Years at company", xlab="Years at comapny",xlim=c(0,40), col=palette3(5))

# basesalary
min(df$basesalary)
max(df$basesalary)
summary(df$basesalary)
count(df[df$basesalary > 1000000,])

df <- df[df$basesalary <= 1000000,]
hist(df$basesalary, main = "Base salary", xlab="Base salary", col=palette2(5))
d <- ggplot(df, aes(x = basesalary, y = yearsofexperience)) + 
  geom_point()
d + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
d


## 2nd Delivery
# As we prepare to perform statistical analysis we want to convert some variables
# to one-hot encoded form we start by renaming some columns

# We check if among remaining variables there are some NA values and we will check what's the percentage of records with those
for (a in names(df)){
  if (sum(is.na(df[a]))>0){
    print(paste(a," : ",100*(sum(is.na(df[a])))/nrow(df)))
  }
}
# We drop the records with NA values for the following fields
df <- df[!is.na(df$gender),]
df <- df[!is.na(df$Education),]
df <- df[!is.na(df$Race),]

df_onehot <-df[c("Bachelors_Degree","Masters_Degree","Doctorate_Degree","Race_Asian","Race_White","Race_Hispanic","Race_Black","Race_Two_Or_More","gender","yearsofexperience","yearsatcompany","timestamp","continent","title","company","totalyearlycompensation")]
names(df_onehot)

names(df_onehot)<- tolower(names(df_onehot))
df_onehot<-rename(df_onehot,  ed_bachelor=bachelors_degree)
df_onehot<-rename(df_onehot, ed_master=masters_degree)
df_onehot<-rename(df_onehot,  ed_doctor=doctorate_degree)
df_onehot<-rename(df_onehot,  year=timestamp)
names(df_onehot)


## ONE-HOT ENCODING
require(tidyr)
require(dplyr)

# Education
# We will have only 4 columns for education, if someone is not bsc, msc or phd he is other
#df_onehot['ed_other'] = replicate(nrow(df_onehot), 0) # We create new column of zeros
#df_onehot <- within(df_onehot, ed_other[df_onehot$ed_bachelor==0 & df_onehot$ed_master == 0 & df_onehot$ed_doctor ==0] <- 1)
# We verify if we do not have unclassified records
#stopifnot(nrow(df_onehot[df_onehot$ed_bachelor==0 & 
                         #df_onehot$ed_other==0 & 
#                         df_onehot$ed_doctor==0 & 
 #                        df_onehot$ed_master==0,])
#                         == 0)

# Gender
# We filter out all records that do not contain valid gender
unique(df_onehot$gender)
df_onehot <- subset(df_onehot, gender !="Title: Senior Software Engineer")
unique(df_onehot$gender)
df_onehot %>% 
  mutate(gender_female = ifelse(gender=='Female', 1, 0),
         #gender_male = ifelse(gender=='Male', 1, 0),
         gender_other = ifelse(gender=='Other', 1, 0),
         )  -> df_onehot
#stopifnot(nrow(df_onehot[df_onehot$gender_female==0 &
 #                        #df_onehot$gender_other==0 &
  #                       df_onehot$gender_male==0,])
   #                      == 0)
#we eliminate the variable gender now that it is encoded
df_onehot <-subset(df_onehot, select = -c(gender) )

#Year
unique(df_onehot$year)
df_onehot %>% 
  mutate(year_2018 = ifelse(year==2018, 1, 0),
         year_2019 = ifelse(year==2019, 1, 0),
         year_2020 = ifelse(year==2020, 1, 0),
         year_2021 = ifelse(year==2021, 1, 0),
  )  -> df_onehot
#stopifnot(nrow(df_onehot[df_onehot$year_2018 == 0 & df_onehot$year_2019 == 0 & df_onehot$year_2020 == 0 & df_onehot$year_2021==0,])==0) # There should be no records without any year
#we eliminate the variable year now that it is encoded
df_onehot <-subset(df_onehot, select = -c(year) )

# Company
unique(df_onehot$company)
df_onehot %>% 
  mutate(com_oracle = ifelse(company=="oracle", 1, 0),
         com_other = ifelse(company=="Other", 1, 0),
         com_amazon= ifelse(company=="amazon", 1, 0),
         com_apple = ifelse(company=="apple", 1, 0),
         com_salesforce = ifelse(company=="salesforce", 1, 0),
         com_facebook = ifelse(company=="facebook", 1, 0),
         com_google = ifelse(company=="google", 1, 0),
         com_intel = ifelse(company=="intel", 1, 0),
         com_microsoft = ifelse(company=="microsoft", 1, 0),
         com_ibm = ifelse(company=="ibm", 1, 0),
  )  -> df_onehot
stopifnot(nrow(df_onehot[df_onehot$com_amazon==0 &
                         df_onehot$com_apple==0 &
                         df_onehot$com_facebook==0 &
                         df_onehot$com_google==0 &
                         df_onehot$com_ibm==0 &
                         df_onehot$com_intel==0 &
                         df_onehot$com_microsoft==0 &
                         df_onehot$com_oracle==0 &
                         df_onehot$com_other==0 &
                         df_onehot$com_salesforce==0,])
                         ==0)
#we eliminate the variable now that it is encoded
df_onehot <-subset(df_onehot, select = -c(company) )

# Titles
unique(df_onehot$title)
df_onehot %>% 
  mutate(title_pmanager = ifelse(title=="Product Manager", 1, 0),
         title_sweng = ifelse(title=="Software Engineer", 1, 0),
         title_swengman = ifelse(title=="Software Engineering Manager", 1, 0),
         title_datasci = ifelse(title=="Data Scientist", 1, 0),
         title_other = ifelse(title=="Other", 1, 0),
         title_hweng = ifelse(title=="Hardware Engineer", 1, 0),
  )  -> df_onehot
stopifnot(nrow(df_onehot[df_onehot$title_datasci==0 & 
                         df_onehot$title_hweng==0 & 
                         df_onehot$title_other==0 & 
                         df_onehot$title_pmanager==0 & 
                         df_onehot$title_sweng==0 & 
                         df_onehot$title_swengman==0,])==0)
#we eliminate the variable now that it is encoded
df_onehot <-subset(df_onehot, select = -c(title) )

# Continents
unique(df_onehot$continent)
df_onehot %>% 
  mutate(continent_namerica = ifelse(continent=="North America", 1, 0),
         continent_samerica = ifelse(continent== "South America", 1, 0),
         continent_africa = ifelse(continent==  "Africa" , 1, 0),
         continent_oceania = ifelse(continent== "Oceania", 1, 0),
         continent_asia = ifelse(continent=="Asia", 1, 0),
         continent_europe = ifelse(continent=="Europe", 1, 0),
  )  -> df_onehot
stopifnot(nrow(df_onehot[df_onehot$continent_namerica==0 & 
                    df_onehot$continent_samerica==0 & 
                    df_onehot$continent_africa==0 & 
                    df_onehot$continent_oceania==0 & 
                    df_onehot$continent_asia==0 & 
                    df_onehot$continent_europe==0,]) 
                    == 0)
#we eliminate the variable now that it is encoded
df_onehot <-subset(df_onehot, select = -c(continent) )

# Final set of columns, we one more time check if it is clean
names(df_onehot)
sum(is.na(df_onehot))


# TC: Why would we choose those specific columns?
#df_categorical <-df_onehot[c("bachelors_degree","totalyearlycompensation","ed_master","ed_doctor","race_asian","race_white","race_hispanic","race_black","race_two_or_more","gender_female","gender_male","gender_other","year_2018","year_2019","year_2020","com_oracle","com_amazon","com_apple","com_salesforce","com_facebook","com_google","com_intel",             "com_cisco","com_ibm","title_pmanager","title_sweng","title_swengman","title_datasci","title_hweng","continent_namerica","continent_samerica","continent_africa","continent_oceania","continent_asia")]
#names(df_categorical)
# df<-df_onehot
# names(df)


## Regression tree

#build the initial tree
treeFitted <-rpart(totalyearlycompensation~.,data=df_onehot,method='anova')
printcp(treeFitted)

#identify best cp value to use
best <- treeFitted$cptable[which.min(treeFitted$cptable[,"xerror"]),"CP"]

#produce a pruned tree based on the best cp value
pruned_tree <- prune(treeFitted, cp=best)

#plot the pruned tree
prp(pruned_tree,
    faclen=0, #use full names for factor labels
    extra=1, #display number of obs. for each terminal node
    roundint=F, #don't round to integers in output
    digits=5) #display 5 decimal places in output

# rpart.plot(treeFitted,type=3)
# 
# plotcp(treeFitted)


# SVM 

library(e1071)

df_social<-df_onehot[c("totalyearlycompensation","ed_bachelor","ed_master","ed_doctor","race_asian","race_hispanic","race_black","race_two_or_more","gender_female","gender_other")]

# Define your outcome variable
Y <- df_social[c("totalyearlycompensation")]

df_social <- subset(df_social, select = -c(totalyearlycompensation))

# Split the data into training and testing sets
trainIndex <- sample(1:nrow(df_social), 0.7 * nrow(df_social))
Ytrain <- Y[trainIndex,]
trainData <- df_social[trainIndex,]
Ytest <- Y[-trainIndex,]
testData <- df_social[-trainIndex,]

# Train your SVM model
svmModel_nu <- svm(Ytrain ~ ., data = trainData, kernel = "linear", type = "nu-regression")

summary(svmModel_nu)
coef(svmModel_nu)

# Test your SVM model on the testing set
svmPred_nu <- predict(svmModel_nu, testData)

# Evaluate the accuracy of your SVM model: RMSE
RMSE_nu <- sqrt(mean((svmPred_nu - Ytest)^2))
RMSE_nu


svmModel_eps <- svm(Ytrain ~ ., data = trainData, kernel = "linear", type="eps-regression")
summary(svmModel_eps)
svmPred_eps <- predict(svmModel_eps, testData)
RMSE_eps <- sqrt(mean((svmPred_eps - Ytest)^2))
RMSE_eps


#The RBF kernel is also a non-linear function that maps the data into a higher dimensional space. It is commonly used in SVMs as it can approximate any non-linear function.
# Radial Basis Function (RBF) kernel: K(x,y) = exp(-gamma ||x-y||^2), where gamma is a parameter that determines the width of the kernel.

svmModel_nu_ra <- svm(Ytrain ~ ., data = trainData, kernel = "radial", type = "nu-regression")
summary(svmModel_nu_ra)
svmPred_nu_ra <- predict(svmModel_nu_ra, testData)
RMSE_nu_ra <- sqrt(mean((svmPred_nu_ra - Ytest)^2))
RMSE_nu_ra

svmModel_eps_ra <- svm(Ytrain ~ ., data = trainData, kernel = "radial", type = "eps-regression")
summary(svmModel_eps_ra)
svmPred_eps_ra <- predict(svmModel_eps_ra, testData)
RMSE_eps_ra <- sqrt(mean((svmPred_eps_ra - Ytest)^2))
RMSE_eps_ra



svmModel_nu_po <- svm(Ytrain ~ ., data = trainData, kernel = "polynomial", type = "nu-regression")
summary(svmModel_nu_po)
svmPred_nu_po <- predict(svmModel_nu_po, testData)
RMSE_nu_po <- sqrt(mean((svmPred_nu_po - Ytest)^2))
RMSE_nu_po


svmModel_eps_po <- svm(Ytrain ~ ., data = trainData, kernel = "polynomial", type = "eps-regression")
summary(svmModel_eps_po)
svmPred_eps_po <- predict(svmModel_eps_po, testData)
RMSE_eps_po <- sqrt(mean((svmPred_eps_po - Ytest)^2))
RMSE_eps_po



max(Y)
min(Y)
summary(Y)

summary(Ytest)


residuals <- Ytest - svmPred_nu
plot(svmPred_nu, residuals, main="Residual Plot", xlab="Predicted Values", ylab="Residuals")
plot(Ytest, residuals, main="Residual Plot", xlab="Test Values", ylab="Residuals")

# Calculate the limits based on the range of the actual and predicted values
lims <- range(c(Ytest, svmPred_nu))

# Plot the data with the same scale for x and y axes
plot(Ytest, svmPred_nu, main = "Predicted vs Actual Values", xlab = "Actual Values", ylab = "Predicted Values", xlim = lims, ylim = lims)
abline(0, 1, col = "red") # Add line for perfect predictions (y = x)


# LINEAR REGRESSION MODEL

# Import libraries and load data
library(dplyr)
library(caret)

# Split data into training and testing sets
df_social<-df_onehot[c("totalyearlycompensation","ed_bachelor","ed_master","ed_doctor","race_asian","race_hispanic","race_black","race_two_or_more","gender_female","gender_other")]

# Define your outcome variable
Y <- df_social[c("totalyearlycompensation")]

df_social <- subset(df_social, select = -c(totalyearlycompensation))

# Split the data into training and testing sets
set.seed(123)
trainIndex <- sample(1:nrow(df_social), 0.7 * nrow(df_social))
Ytrain <- Y[trainIndex,]
trainData <- df_social[trainIndex,]
Ytest <- Y[-trainIndex,]
testData <- df_social[-trainIndex,]

# Check correlation matrix
cor(trainData)


# Fit a linear regression model using the training data
linearModel <- lm(Ytrain ~ ., data = trainData)

# Predict the output variable using the testing data and calculate the residuals
predictions <- predict(linearModel, newdata = testData)
residuals <- Ytest - predictions

# Plot the residuals
plot(residuals ~ predictions, xlab = "Predictions", ylab = "Residuals",
     main = "Residuals vs. Predictions")
abline(h = 0, col = "red")

# Calculate the mean squared error (MSE)
mse <- mean(residuals^2)
cat("Mean Squared Error:", mse, "\n")



