#df <- read.csv('/Users/ltcmu/OneDrive/Documentos/Master/Data Valorisation/Proyecto/Levels_Fyi_Salary_Data.csv')
df <- read.csv('C:/Users/Tymoteusz/Desktop/DataValorisation/Project/Levels_Fyi_Salary_Data.csv')

#We eliminate variables
df <- subset(df, select = -c(rowNumber,Some_College,Highschool,tag,otherdetails,dmaid, cityid,stockgrantvalue,level))

# country/continent
# Add continent variable
library(countrycode)

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
"8/25/2018 19:23:14"
df$timestamp <- sapply(strsplit(as.character(df$timestamp), " "), `[`, 1) 
#didnt work on my computer
#sapply(strsplit(df$timestamp ," "), `[`, 1) -> df$timestamp
"8/25/2018"
sapply(strsplit(df$timestamp ,"/"), `[`, 3) -> df$timestamp

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
sort(company_proportions,decreasing=TRUE)[1:10]
# amazon     microsoft     google   facebook      apple     oracle salesforce      intel       ibm     cisco 
# 13.142939   8.384151   6.972957   4.795505   3.283739   1.824654   1.700137   1.583602  1.479838  1.473452 
 
selected_companies <- sort(company_proportions,decreasing=TRUE)[1:10]
df$company[!(df$company %in% names(selected_companies))] <- "Other"

library(dplyr)

min(df$totalyearlycompensation)
max(df$totalyearlycompensation)
sum(is.na(df["totalyearlycompensation"])) # There should be no records with missing salary 
count(df[df$totalyearlycompensation > 2500000,])
df <- df[df$totalyearlycompensation <= 2500000,] # We filter out people that earn more than 2.5 million $ per year as outliers
min(df$totalyearlycompensation)
max(df$totalyearlycompensation)

count(df[df$totalyearlycompensation > 2500000,])

title_proportions <- 100*table(df$title)/length(df$title)
sort(title_proportions,decreasing=TRUE)[1:5]
# Software Engineer              Product Manager Software Engineering Manager  Data Scientist            Hardware Engineer 
#         65.822664                     7.457135                     5.696223        4.115713                     3.512245 
title_proportions <- sort(title_proportions,decreasing=TRUE)[1:5]
df$title[!(df$title %in% names(title_proportions))] <- "Other"

df$race

# 
# x year (one-hot)->
# Company (one-hot)->
# Title (one-hot) -> "Product Manager" "Software Engineer" "Software Engineering Manager""Data Scientist"  "Other"  "Hardware Engineer"  
# continent (one-hot) -> 6 continents europe,asia,africa,oceania,north_america,south_america
# xgender((one-hot) -> Male, Female, Other
# xyearsofexperience
# xyearsatcompany
# x"Masters_Degree""Bachelors_Degree""Doctorate_Degree"
# x"Race_Two_Or_More"        "Race_Black"              "Race_Hispanic"      "Race_Asian"   "Race_White"    
# 
# 
# totalyearlycompensation
# 

# 
# "timestamp"               "company"                 "level"                   "title"                  
# [5] "totalyearlycompensation" 
# "location"                     
# [9] 
# 
# "gender"                  "Masters_Degree"         
# [13] "Bachelors_Degree"        "Doctorate_Degree"        "Race_Asian"              "Race_White"             
# [17] "Race_Two_Or_More"        "Race_Black"              "Race_Hispanic"           "Race"                   
# [21] "Education"


names(df)
df_onehot <-df[c("Bachelors_Degree","Masters_Degree","Doctorate_Degree","Race_Asian","Race_White","Race_Hispanic","Race_Black","Race_Two_Or_More","gender","yearsofexperience","yearsatcompany","timestamp","continent","title","company","totalyearlycompensation")]
names(df_onehot)

#make them lowercase
names(df_onehot)<- tolower(names(df_onehot))

# Education
#df_onehot<-rename(df_onehot,  ed_bachelor=bachelors_degree)
df_onehot<-rename(df_onehot, ed_master=masters_degree)
df_onehot<-rename(df_onehot,  ed_doctor=doctorate_degree)
names(df_onehot)

# Race- ok
#year
df_onehot<-rename(df_onehot,  year=timestamp)

names(df_onehot)

#ONE-HOT ENCODING
require(tidyr)
require(dplyr)

#Gender
df_onehot$gender[df_onehot$gender == "Title: Senior Software Engineer"] <- "Unknown"
df_onehot$gender[df_onehot$gender == "<NA>"] <- "Unknown"
df_onehot$gender[is.na(df_onehot$gender)] <- "Unknown"
unique(df_onehot$gender)
#[1] "Unknown" "Male"    "Female"  "Other"  
df_onehot %>% 
  mutate(gender_female = ifelse(gender=='Female', 1, 0),
         gender_male = ifelse(gender=='Male', 1, 0),
         #gender_unknown = ifelse(gender=='Unknown', 1, 0),
         gender_other = ifelse(gender=='Other', 1, 0),
         )  -> df_onehot

#we eliminate the variable gender now that it is encoded
df_onehot <-subset(df_onehot, select = -c(gender) )

#Year
unique(df_onehot$year)
df_onehot %>% 
  mutate(year_2018 = ifelse(year==2018, 1, 0),
         year_2019 = ifelse(year==2019, 1, 0),
         year_2020 = ifelse(year==2020, 1, 0),
        # year_2021 = ifelse(year==2021, 1, 0),
  )  -> df_onehot

#we eliminate the variable gender now that it is encoded
df_onehot <-subset(df_onehot, select = -c(year) )

# Company (one-hot)->
unique(df_onehot$company)
df_onehot %>% 
  mutate(com_oracle = ifelse(company=="oracle", 1, 0),
         #com_other = ifelse(company=="Other", 1, 0),
         com_amazon= ifelse(company=="amazon", 1, 0),
         com_apple = ifelse(company=="apple", 1, 0),
         com_salesforce = ifelse(company=="salesforce", 1, 0),
         com_facebook = ifelse(company=="facebook", 1, 0),
         com_google = ifelse(company=="google", 1, 0),
         com_intel = ifelse(company=="intel", 1, 0),
         com_cisco = ifelse(company=="cisco", 1, 0),
         com_ibm = ifelse(company=="ibm", 1, 0),
  )  -> df_onehot

#we eliminate the variable now that it is encoded
df_onehot <-subset(df_onehot, select = -c(company) )

# Titles
#5 most important -> "Product Manager" "Software Engineer" "Software Engineering Manager""Data Scientist"  "Other"  "Hardware Engineer"  
unique(df_onehot$title)

df_onehot %>% 
  mutate(title_pmanager = ifelse(title=="Product Manager", 1, 0),
         title_sweng = ifelse(title=="Software Engineer", 1, 0),
         title_swengman = ifelse(title=="Software Engineering Manager", 1, 0),
         title_datasci = ifelse(title=="Data Scientist", 1, 0),
         #title_other = ifelse(title=="Other", 1, 0),
         title_hweng = ifelse(title=="Hardware Engineer", 1, 0),
  )  -> df_onehot

#we eliminate the variable now that it is encoded
df_onehot <-subset(df_onehot, select = -c(title) )

# continent (one-hot) -> 6 continents europe,asia,africa,oceania,north_america,south_america
#"North America" "Europe"        "Asia"          "Oceania"       "Africa"        "South America"

df_onehot %>% 
  mutate(continent_namerica = ifelse(continent=="North America", 1, 0),
         continent_samerica = ifelse(continent== "South America", 1, 0),
         continent_africa = ifelse(continent==  "Africa" , 1, 0),
         continent_oceania = ifelse(continent== "Oceania", 1, 0),
         continent_asia = ifelse(continent=="Asia", 1, 0),
         #continent_europe = ifelse(continent=="Europe", 1, 0),
  )  -> df_onehot
df_onehot <-subset(df_onehot, select = -c(continent) )

library(corrplot)
names(df_onehot)


df_categorical <-df_onehot[c("bachelors_degree","totalyearlycompensation","ed_master","ed_doctor","race_asian","race_white","race_hispanic","race_black","race_two_or_more","gender_female","gender_male","gender_other","year_2018","year_2019","year_2020","com_oracle","com_amazon","com_apple","com_salesforce","com_facebook","com_google","com_intel",             "com_cisco","com_ibm","title_pmanager","title_sweng","title_swengman","title_datasci","title_hweng","continent_namerica","continent_samerica","continent_africa","continent_oceania","continent_asia")]

names(df_categorical)

df<-df_categorical

df<-df[!is.na(df$Education),]
df<-df[!is.na(df$gender),]
df<-df[!is.na(df$Race),]
df<-df[!is.na(df$level),]

names(df)
sum(is.na(df))

library(rpart)
library(rpart.plot)

treeFitted <-rpart(totalyearlycompensation~.,data=df,method='anova')

treeFitted
rpart.plot(treeFitted,type=3)

plotcp(treeFitted)

# Tymson starts here


