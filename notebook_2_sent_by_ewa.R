#Libraries
library(tidyverse)

#Opening the dataset
df <- read.csv('C:/Users/48533/Desktop/Data Valorisation/Levels_Fyi_Salary_Data.csv')

df = subset(df, select = -c(rowNumber,Some_College,Highschool,tag,otherdetails,dmaid))

names(df)
# 
# "timestamp"               "company"                 "level"                   "title"                   "totalyearlycompensation"
#  [6] "location"                "yearsofexperience"       "yearsatcompany"          "tag"                     "basesalary"             
# [11] "stockgrantvalue"         "bonus"                   "gender"                  "otherdetails"            "cityid"                 
# [16] "dmaid"                   "rowNumber"               "Masters_Degree"          "Bachelors_Degree"        "Doctorate_Degree"       
# [21] "Highschool"              "Some_College"            "Race_Asian"              "Race_White"              "Race_Two_Or_More"       
# [26] "Race_Black"              "Race_Hispanic"           "Race"                    "Education"              
# 
# 

# VARIABLES WE WILL REMOVE AND WHY
# "rowNumber" : doesnt give relevant info
# "Some_college": its empty
# "Highschool" : no one has it (every data scientist has a higher level of education)
# "Race","Education" : repeated information from other variables -> ignore
#  "Tag"-> redundant info with level and title
# "Other details" -> very varied
# "dmaid"  Designated Market Area: US zone -> repeated information
#
# REST OF VARIABLES
#
# About the company
# "company" "stockgrantvalue" 
# 
# About the position
# "tag"/HR,IT
# "level" / M3/L2
# "title"/Software Engineer,Product Manager
# "location/cityid"
#   
# About the user: demographic
# "gender"
#  "Race_Hispanic" "Race_Black" "Race_Asian""Race_White""Race_Two_Or_More"
# About the user: preparation
# "Education"   "Highschool"  "Some_College" ("Bachelors_Degree" "Masters_Degree" "Doctorate_Degree"). Is it what you study or the max study level?
# "yearsofexperience" 
# "yearsatcompany" 
# 
# Target
# "totalyearlycompensation"
# "basesalary"
# "bonus" 
# "otherdetails"


#  Print the names of the variables that contain missing values and their percentage
# [1] "level  :  0.024% 15 people   -> eliminate the rows X
# [1] "tag  :  1.28% -> we will eliminate the column X
# [1] "gender  :  31.19% -> create a value unknown X 
# [1] "otherdetails  :  35.92% -> we will eliminate the column X
# [1] "dmaid  :  0.003% It is 0 when the country is not in the US, but 0 =! NA X
# [1] "Race  :  64.19%  -> we will eliminate it 
# [1] "Education  :  51.518% -> we will eliminate it 

for (a in names(df)){
  if (sum(is.na(df[a]))>0){
    print(paste(a," : ",100*(sum(is.na(df[a])))/nrow(df)))
  }
}

#Level
df=df[!is.na(df$level),]
sum(is.na(df["level"]))

sum(is.na(df["tag"]))
print(df[df["gender"]=="0ther"])


#Race

sum(is.na(df$Race))
unique(df$Race)

df$Race[df$Race %in% c("Asian", "White", "Black", "Two Or More","Hispanic")] <- 1
df$Race[is.na(df$Race)] <- 0

sum(is.na(df$Race))
unique(df$Race)


# Education
sum(is.na(df$Education))
unique(df$Education)
df$Education[df$Education %in% c("PhD", "Master's Degree", "Bachelor's Degree", "Some College","Highschool")] <- 1
df$Education[is.na(df$Education)] <- 0
sum(is.na(df$Education))
unique(df$Education)

#Gender 
# Female                            Male                           Other Title: Senior Software Engineer 
# 11.173014910                    56.993710290                     0.638549216                     0.001596373 
df[!is.na(df$gender),]
(unique(df$gender))
proportions <- table(df$gender)/length(df$gender)
percentages <- proportions*100
percentages
df$gender[df$gender == "Title: Senior Software Engineer"] <- "Unknown"
df$gender[is.na(df$gender)] <- "Unknown"

#For the rows without dmaid:
#they are american
# we can give them values based on their location here https://www.bluelinemedia.com/list-of-cities-by-dma-in-all-united-states-america
# one is rome, ny -> Syracuse NY  555
#the other troy, michigan ->Detroit MI 505

df[is.na(df$dmaid),]
#to substitute (we know the ids from the previous cell)
df$dmaid[df$location=="Rome, NY"] <- 555
df$dmaid[df$location=="Troy, MI"] <- 505


#Eliminate finally unwanted variables
#df = subset(df, select = -c(rowNumber,Some_College,Highschool,Race,Education,tag,otherdetails) )

names(df)


# Location 
# Create 3 new columns, city, state, country
df$location
# Print out the location and country values for the first 10 rows
for (i in 1:length(df$location)) {
  aux <- strsplit(df$location[i], ", ")[[1]]
  if(length(aux) > 2){
    df$country[i] = aux[3]
  }
  else{
    df$country[i] = "US"
  }
  df$state[i] <- aux[2]
  df$city[i] <- aux[1]
}

length(unique(df$country)) # 81 different countries

# We only work with US, India, United Kingdom, Germany, Canada
df = subset(df, country %in% c("US", "India", "United Kingdom", "Germany", "Canada"))
unique(df$country)


library(ggplot2)
q <- ggplot(data.frame(df$country), aes(x=df$country)) + geom_bar()
q + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

a = subset(df, country != "US")
c <- ggplot(a, aes(x=country)) + geom_bar()
c + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# Create a scatter plot of location against country
d <- ggplot(a, aes(x = company, y = country)) + 
  geom_point()
d + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


# yearsofexperience [0,45]
min(df$yearsofexperience)
max(df$yearsofexperience)

d <- ggplot(df, aes(x = totalyearlycompensation, y = yearsofexperience)) + 
  geom_point()
d + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


names(df)

# We delete the sample with more than 60 years of experience
df = df[df$yearsofexperience < 60,]
max(df$yearsofexperience)

# totalyearlycompensation
min(df$totalyearlycompensation)
max(df$totalyearlycompensation)
count(df[df$totalyearlycompensation > 2500000,])

df = df[df$totalyearlycompensation <= 2500000,]

count(df[df$totalyearlycompensation > 2500000,])
hist(df$totalyearlycompensation)

# yearsatcompany [0,40]
min(df$yearsatcompany)
max(df$yearsatcompany)

q <- ggplot(data.frame(df$yearsatcompany), aes(x=df$yearsatcompany)) + geom_bar()
q + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


# basesalary
min(df$basesalary)
summary(df$basesalary)
hist(df$basesalary)
count(df[df$basesalary > 600000,])
max(df$basesalary)
hist(df$basesalary)

# Eliminate basesalary higher than 500000
df = df[df$basesalary <= 500000,]

max(df$basesalary)
hist(df$basesalary)


# stockgrantvalue
min(df$stockgrantvalue)
max(df$stockgrantvalue)

hist(df$stockgrantvalue)

count(df[df$stockgrantvalue >= 1000000,])

# Eliminate
df = df[df$stockgrantvalue <= 1000000,]
max(df$stockgrantvalue)

hist(df$stockgrantvalue)

d <- ggplot(df, aes(x = stockgrantvalue, y = basesalary)) + 
  geom_point()
d + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

names(df)

df$level