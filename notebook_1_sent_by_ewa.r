#Libraries
library(tidyverse)
library(ggplot2)
#install.packages("plotly")
library(plotly) #interactive visualization
library(lattice) #data exploration (corellations)
#install.packages("corrplot")
library(dplyr)
#Opening the dataset
df <- read.csv('C:/Users/48533/Desktop/Data Valorisation/Levels_Fyi_Salary_Data.csv')

#We eliminate variables
df <- subset(df, select = -c(rowNumber,Some_College,Highschool,tag,otherdetails,dmaid, cityid, stockgrantvalue))
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
#creating color palettes for visualizations
palette1 <- colorRampPalette(c("#803934","#3f67ab","#613fab"))
palette2 <-colorRampPalette(c("#5000ff","#d900ff","#ff0077"))
palette3 <-colorRampPalette(c("#5e9159","#599190","#645991"))
for (a in names(df)){
  if (sum(is.na(df[a]))>0){
    print(paste(a," : ",100*(sum(is.na(df[a])))/nrow(df)))
  }
}

#??? to delete because tag doesnt exist?
#sum(is.na(df["tag"]))
#print(df[df["gender"]=="0ther"])

#Timestamp
#Get the first 4 digits X
#get the percentage X
#plot the digits

"8/25/2018 19:23:14"
df$timestamp <- sapply(strsplit(as.character(df$timestamp), " "), `[`, 1)
#didnt work on my computer
#sapply(strsplit(df$timestamp ," "), `[`, 1) -> df$timestamp
"8/25/2018"
sapply(strsplit(df$timestamp ,"/"), `[`, 3) -> df$timestamp
"2018"
timestamp_proportions <- 100*table(df$timestamp)/length(df$timestamp)
timestamp_proportions
# 2017       2018       2019       2020       2021 
# 0.2953997  6.2193623 17.8660961 38.2295176 37.3896243 
#hist(as.integer(df$timestamp))
hist(as.integer(df$timestamp), col = palette2(120), main = "Timestamp", xlab = "Years", ylab = "Frequency")

#Company
length(unique(df$company))#1633
tolower(df$company)->df$company #1102 Values
#we detect there are 2,3 names for several companies i.e microsoft,
#microsoft services, microsoft corp
df[grepl("moody", df$company),]["company"] <- "moody"
df[grepl("microsoft", df$company),]["company"] <- "microsoft"
df[grepl("microsoft", df$company),]["company"] <- "amazon"
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
length(unique(df$company)) #1094
company_proportions <- 100*table(df$company)/length(df$company)
sort(company_proportions,decreasing=TRUE)[1:9]
# amazon     google   facebook      apple     oracle salesforce      intel        ibm      cisco 
# 21.477957   6.965047   4.796653   3.284526   1.825091   1.700544   1.542466   1.480192   1.473805 
selected_companies <- sort(company_proportions,decreasing=TRUE)[1:9]
df$company[!(df$company %in% names(selected_companies))] <- "Other"
unique(df$company)
q <- ggplot(data.frame(df$company), aes(x=df$company)) + geom_bar(fill = "steelblue")
q + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  ggtitle("Companies") + xlab("Company Name")
q

#Level
sum(is.na(df["level"]))

# totalyearlycompensation [10000, 2500000]
min(df$totalyearlycompensation)
max(df$totalyearlycompensation)
sum(is.na(df["totalyearlycompensation"]))
count(df[df$totalyearlycompensation > 2000000,])
df <- df[df$totalyearlycompensation <= 2000000,]
min(df$totalyearlycompensation)
max(df$totalyearlycompensation)
#Visualization of Total yearly compensation variable
count(df[df$totalyearlycompensation > 2500000,])
hist(df$totalyearlycompensation, main = "Total yearly compensation", xlab="Total yearly compensation", col="chocolate")

#Visualization of total yearly compensation and years of experience
d <- ggplot(df, aes(x = totalyearlycompensation, y = yearsofexperience)) + 
  geom_point()
d + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# Location 
# Create 3 new columns, city, state, country
sum(is.na(df$location))
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
b <- subset(df, country %in% c("US", "India", "United Kingdom", "Germany", "Canada"))
unique(b$country)

library(ggplot2)
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

# yearsofexperience [0,45]
min(df$yearsofexperience)
max(df$yearsofexperience)
sum(is.na(df$yearsofexperience))
hist(df$yearsofexperience, main = "Years of experience", xlab="Years of experience", col=palette1(5))
# We delete the sample with more than 60 years of experience
df <- df[df$yearsofexperience < 60,]
max(df$yearsofexperience)


# yearsatcompany [0,40]
min(df$yearsatcompany)
max(df$yearsatcompany)
sum(is.na(df$yearsatcompany))
hist(df$yearsatcompany, main = "Years at company", xlab="Years at comapny",xlim=c(0,40), col=palette3(5))

# basesalary
min(df$basesalary)
summary(df$basesalary)
count(df$basesalary)
hist(df$basesalary, main = "Base salary", xlab="Base salary", col=palette2(5))
d <- ggplot(df, aes(x = basesalary, y = yearsofexperience)) + 
  geom_point()
d + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
d
count(df[df$basesalary > 1000000,])
max(df$basesalary)

# Eliminate basesalary higher than 1000000
df <- df[df$basesalary <= 1000000,]
d <- ggplot(df, aes(x = basesalary, y= yearsofexperience)) + 
  geom_point()
d + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
summary(df$basesalary)
# stockgrantvalue
#min(df$stockgrantvalue)
#max(df$stockgrantvalue)
#sum(is.na(df$stockgrantvalue))
#hist(df$stockgrantvalue)
#count(df[df$stockgrantvalue >= 1000000,])
#d <- ggplot(df, aes(x = stockgrantvalue, y = basesalary)) + 
# geom_point()
#d + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
# Eliminate
#df <- df[df$stockgrantvalue <= 1000000,]
#max(df$stockgrantvalue)
#hist(df$stockgrantvalue)

#Bonus
#Analysis of variable bonus
#Looking for Na values
sum(is.na(df$bonus))
summary(df$bonus)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0    1000   14000   19335   26000 1000000
#number of people who got a bonus and number of people without bonus
nrow(subset(df, bonus == 0))
#15424
nrow(subset(df, bonus != 0))
#47207
d <- ggplot(df, aes(x = bonus, y = basesalary)) + 
  geom_point()
d + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
df <- df[df$bonus <= 750000,]

#histogram for the variables
hist(df$bonus,
     main="Bonus to salary",
     xlab="Bonus to salary",
     xlim=c(0,200000),
     col=palette1(3)
     ) # nolint

#Gender 
# Female                            Male                           Other Title: Senior Software Engineer 
# 11.173014910                    56.993710290                     0.638549216                     0.001596373 
(unique(df$gender))
sum(is.na(df$gender))
proportions <- table(df$gender)/length(df$gender)
percentages <- proportions*100
percentages

df$gender[df$gender == "Title: Senior Software Engineer"] <- "Unknown"
df$gender[is.na(df$gender)] <- "Unknown"
#df <- df[df$gender != "Title: Senior Software Engineer",]
(unique(df$gender))
#df <- df[!is.na(df$gender),]
#df$gender <- ifelse(df$gender %in% c("PhD", "Master's Degree", "Bachelor's Degree", "Some College","Highschool"), 1, 0)

# Education
sum(is.na(df$Education))
unique(df$Education)

df <- df[!is.na(df$Education),]
#From data that we have we display the distribution of Education category
Education1 <- df$Education
counts <- table(Education1)
counts
pie(counts, labels = names(counts), main = "The pie chart for education")
#Let me know if we want to do more of interactive displays (this is from library plotly)
Education2 <- plot_ly(x=Education1, type = "histogram")
Education2

#df$Education[df$Education %in% c("PhD", "Master's Degree", "Bachelor's Degree", "Some College","Highschool")] <- 1
#df$Education[is.na(df$Education)] <- 0
#df$Education <- ifelse(df$Education %in% c("PhD", "Master's Degree", "Bachelor's Degree", "Some College","Highschool"), 1, 0)

length(df$level[grepl("L[1-9][0-9]", df$level)]) #numbers with 2 cyphers
length(df$level[grepl("L[0-9]", df$level)]) #numbers of 1 cypher

# Race
df$Race[is.na(df$Race)] <- "Unknown"
#Race
sum(!is.na(df$Race))
unique(df$Race)
#From data that we have we display the distribution of Race category
Race1 <- df$Race
counts <- table(Race1)
counts
pie(counts, labels = names(counts), main = "Race")
Race2 <- plot_ly(x=Race1, type = "histogram")
Race2
#df <- df[!is.na(df$Race),]
#this worked for puting 0,1 values (0 for Na as well)
#df$Race <- ifelse(df$Race %in% c("Asian", "White", "Black", "Two Or More","Hispanic"), 1, 0)
#df$Race[is.na(df$Race)] <- 0
#df$Race[df$Race %in% c("Asian", "White", "Black", "Two Or More","Hispanic")] <- 1
#df$Race[is.na(df$Race)] <- 0
#count(df[df$Race == 0,])
#unique(df$Race)

race1 <- df$Race
counts <- table(race1)
percentages <- round(counts/sum(counts) * 100, 1)
labels <- paste(names(counts), "(", percentages, "%)", sep = " ")

pie(counts, labels = labels, main = "The pie chart for gender")
pie(counts,labels)

cor(cbind(df$totalyearlycompensation, df$bonus+df$basesalary))

top10_comp <- df[df$gender %in% names(sort(tapply(df$totalyearlycompensation, df$gender, mean), decreasing = TRUE))[1:10], ]
# Create the box plot
boxplot(totalyearlycompensation ~ gender, data = top10_comp, 
        main = "Top 10 Companies by Total Yearly Compensation", 
        xlab = "Gender", ylab = "Total Yearly Compensation")

library(tidyverse)
library(plotly)
# Gender vs totalyearlycompensation
# Do men earn more than women in the tech industry? 
# Has the disparity increased or decreased during the years? Is it different in several countries?
df_top10comp <- df %>%
  filter(gender %in% c("Male", "Female", "Other", "Unknown")) %>%
  group_by(gender) %>%
  summarize(mean_comp = mean(totalyearlycompensation)) %>%
  arrange(desc(mean_comp)) %>%
  slice_head(n = 10) %>%
  pull(gender)
fig <- df %>%
  filter(gender %in% df_top10comp) %>%
  select(gender, totalyearlycompensation) %>%
  arrange(desc(totalyearlycompensation)) %>%
  plot_ly(x = ~gender, y = ~totalyearlycompensation, type = "box", color = ~gender)
fig

library(ggplot2)
#install.packages("ggpubr")
#install.packages("ggpubr")
#library(ggpubr)
library(ggpubr)
ggplot(df, aes(x = timestamp, y = totalyearlycompensation, color = gender)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  labs(title = "Total Annual Compensation along the years for every gender") +
  theme_pubclean()
lines(timestamp, avg_time$se, type="o", col="blue", pch=1)

library(plotly)
fig <- plot_ly(df, y = ~country, x = ~totalyearlycompensation, type = "box", 
               color = ~country, orientation = "h") %>%
  layout(title = "Country vs Total Yearly Compensation",
         xaxis = list(title = "Total Annual Compensation"),
         yaxis = list(title = "Country"))
fig
# we work on a subset of countries
b <- subset(df, country %in% c("US", "India", "United Kingdom", "Germany", "Canada"))
fig <- plot_ly(b, y = ~country, x = ~totalyearlycompensation, type = "box", 
               color = ~country, orientation = "h") %>%
  layout(title = "Country vs Total Yearly Compensation",
         xaxis = list(title = "Total Annual Compensation"),
         yaxis = list(title = "Country"))
fig
# Only US: some states
b <- subset(df, country %in% c("US"))
d <- ggplot(b, aes(x = totalyearlycompensation, y = state)) + 
  geom_point()
d + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
df_top_states <- b %>%
  group_by(state) %>%
  summarize(total_compensation = sum(totalyearlycompensation)) %>%
  arrange(desc(total_compensation)) %>%
  head(10)
df_top_states
b <- subset(df, state %in% c("WA", "NY", "CA", "MA", "TX", "VA", "IL", "OR", "DC", "GA"))
fig <- plot_ly(b, y = ~state, x = ~totalyearlycompensation, type = "box", 
               color = ~state, orientation = "h") %>%
  layout(title = "Country vs Total Yearly Compensation",
         xaxis = list(title = "Total Annual Compensation"),
         yaxis = list(title = "Country"))
fig
# In california
b <- subset(df, state %in% c("CA"))
df_top_cities <- b %>%
  group_by(city) %>%
  summarize(total_compensation = sum(totalyearlycompensation)) %>%
  arrange(desc(total_compensation)) %>%
  head(10)
df_top_cities
b <- subset(b, city %in% c("San Francisco", "Mountain View", "Sunnyvale", "San Jose", "Menlo Park", "Cupertino", "Santa Clara", "Palo Alto", "Los Angeles", "San Diego"))
fig <- plot_ly(b, y = ~city, x = ~totalyearlycompensation, type = "box", 
               color = ~city, orientation = "h") %>%
  layout(title = "Country vs Total Yearly Compensation",
         xaxis = list(title = "Total Annual Compensation"),
         yaxis = list(title = "Country"))
fig
b <- subset(b, city %in% c("Menlo Park"))

df_top10comp <- b %>%
  filter(gender %in% c("Male", "Female", "Other", "Unknown")) %>%
  group_by(gender) %>%
  summarize(mean_comp = mean(totalyearlycompensation)) %>%
  arrange(desc(mean_comp)) %>%
  slice_head(n = 10) %>%
  pull(gender)
fig <- b %>%
  filter(gender %in% df_top10comp) %>%
  select(gender, totalyearlycompensation) %>%
  arrange(desc(totalyearlycompensation)) %>%
  plot_ly(x = ~gender, y = ~totalyearlycompensation, type = "box", color = ~gender)
fig

length(unique(df$state))

# Race vs totalyearlycompensation
#Does race affect salaries in the tech industry? Has the disparity increased or decreased during the years?  
#Is it different in several countries?
unique(df$Race)
df_top10comp <- df %>%
  filter(Race %in% c("Asian", "Two Or More", "White", "Hispanic", "Black", "Unknown")) %>%
  group_by(Race) %>%
  summarize(mean_comp = mean(totalyearlycompensation)) %>%
  arrange(desc(mean_comp)) %>%
  slice_head(n = 10) %>%
  pull(Race)
fig <- df %>%
  filter(Race %in% df_top10comp) %>%
  select(Race, totalyearlycompensation) %>%
  arrange(desc(totalyearlycompensation)) %>%
  plot_ly(x = ~Race, y = ~totalyearlycompensation, type = "box", color = ~Race)
fig

# Education vs totalyearlycompensation
unique(df$Education)
df_top10comp <- df %>%
  filter(Education %in% c("PhD", "Master's Degree", "Bachelor's Degree", "Some College", "Highschool")) %>%
  group_by(Education) %>%
  summarize(mean_comp = mean(totalyearlycompensation)) %>%
  arrange(desc(mean_comp)) %>%
  slice_head(n = 10) %>%
  pull(Education)
fig <- df %>%
  filter(Education %in% df_top10comp) %>%
  select(Education, totalyearlycompensation) %>%
  arrange(desc(totalyearlycompensation)) %>%
  plot_ly(x = ~Education, y = ~totalyearlycompensation, type = "box", color = ~Education)
fig







#box plots???
bwplot(df$basesalary,
       pch=12,
       lwd = 6,
       main = "boxplot",
       par.settings=box2_settings,
       xlab = 'basesalary')
box2_settings <- list(box.rectangle=list(col="yellow",
                                         lwd=1.8,
                                         fill="pink",
                                         alpha=0.8), 
                      box.umbrella=list(col="blue",
                                        lwd=1.2),
                      plot.symbol=list(col="green",
                                       pch=22))

korelacja <- cor(df[1:6])
round(korelacja,2)
corrplot(korelacja, 
         method = "circle", 
         mar=c(1,1,2,1),
         tl.col = "black", 
         tl.cex = 0.6, 
         col = moja(100), 
         outline = TRUE, 
         addgrid.col = "black", 
         bg = "white", 
         cl.pos = "b", 
         tl.pos = "d",
         main="Korelacja",
         order="hclust",
         kclust.method="ward.D2",
         addrect=4,
         rect.col="black",
         rect.lwd=3)

#example corelation between 2 variables
Wykres <- plot_ly(data = Dane, x = ~Lata.pracy, y = ~Wynagrodzenie, marker = list(size = 10, color = 'rgba(255, 182, 193, .9)', line = list(color = 'rgba(152, 0, 0, .8)', width = 2)))
Wykres