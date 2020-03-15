# ---- this a first R Code ----

x <- 5
y <- 8
plot(x,y)
3 < 5
3==4
sqrt(81)

ls()

returend <- TRUE
a <- c(4,5,7)
b <- c("apple","sex","banana")
c <- c(TRUE, FALSE,TRUE)
MyDataFrame <- data.frame(mynumbers=a,mynames= b,mylogic= c)
MyDataFrame[1]  #we navigate throuh first object
mylist = list(mydataframe = MyDataFrame, isdataframe = c(TRUE), mytext = "Hello")
mylist[2]  #should return True

mean(a)
sum(a)
3 %in% a  # is 3 found in vector ----
4 %in% a 

mean(a, na.rm = TRUE)
letters
# Square Bracket are for navigating through function  
LETTERS 
ali <- LETTERS[c(1,2,4)]


# install.packages("gapminder")
library(gapminder)
data("gapminder")
gapminder <- gapminder::gapminder
gapminder
names(gapminder)
head(gapminder)
str(gapminder)
gapminder$continent #gives the column 
# install.packages("ggplot2")  was 



levels(gapminder$continent)
gapminder$continent
as.numeric(gapminder$continent)


# packages  ggplot2:: 
library(ggplot2)
library(dplyr)
library(tidyr)  # needed for gather
myfld <- gapminder %>% filter(year < 2001 & year > 1990) %>% filter(gdpPercap < 3000, continent=="Asia")
ggplot(myfld, aes(x=year, y=gdpPercap, col= continent)) + geom_point()

irangpdframe <- gapminder %>% filter(country=="Iran")
iranfigure <- ggplot(irangpdframe) + geom_point(aes(x=year, y=gdpPercap, size = gdpPercap, col = country, col = gdpPercap < mean(gdpPercap)))
iranfigure + xlab("YEAR") + ylab("GDP Per Capita") + theme_classic() 
ggsave("iranfigure.pdf", iranfigure)

colombiagdpframe <- gapminder %>% filter(country=="Colombia")
colombiafigure <- ggplot(colombiagdpframe, aes(x=year, y=gdpPercap)) + geom_point()



# Tyding Class 4
library(dplyr)
setwd("/Users/alibadiei/Desktop/DataScience/RCode")
getwd()
list.files()
US_data <- read.csv("US_data.csv")
nonUS_income <- read.delim("nonUS_income.txt")
nonUS_profession <- read.delim("nonUS_profession.txt")
nonUS_demographics <- read.delim("nonUS_demographics.txt")
nonUS_data <- left_join(nonUS_profession, nonUS_demographics)
nonUS_data_long <- gather(nonUS_income, key=country, value=income, -id)
nonUS_income_long <- drop_na(nonUS_data_long)
nonUS_data <- left_join(nonUS_data, nonUS_income_long)
head(nonUS_income_long)
US_data <- US_data %>% mutate(country = "United States")
income <- bind_rows(US_data, nonUS_data)
income <- income %>%  select(-id)
income <- income %>% rename(government_work = governement_work)
unique(income$country)
income <- income %>% mutate(country = gsub(pattern = "\\.", replacement = " ", country))
income <- income %>% mutate_if(is.character, as.factor)
levels(income$relationship)
income %>% mutate(relationship = recode(relationship, "Not in family" = "Unrelated")) %>% glimpse()
# Export the data as a .txt ("tab delimited values")


# install.packages("Rlab")
# install.packages("caTools") 

library(Rlab)
library(caTools)


#assignment 7 
library(dplyr) 
library(ggplot2) 
library(corrplot)
library(forcats)
library(tidyr)   #needed for drop_na
setwd("/Users/alibadiei/Desktop/DataScience/RCode")
getwd()
# Import Data

dat <- read.csv("movie_metadata.csv")
# Subset Data  

dat <- dat %>% filter(budget < 150000000) %>% select(color, duration, budget, imdb_score, aspect_ratio, movie_facebook_likes)  


#question 1: Color is the only one with factor and ansd average aspec ration: 2.1
#question 2: most films are between 80 and 120

ggplot(dat) + geom_boxplot(aes(x=color,y=budget))

#question 3 some moveis not labellet as either b&C and or and color has higher budget than b&w

ggplot(dat) + geom_histogram(aes(x=budget/1000000))
#question 4: most movies are less than 50m budget

cor(dat$budget, dat$imdb_score )


# question 5 .there is no correlation.
# dat1 <- dat %>%  select_if(is.numeric)
# dat2 <- dat1 %>% drop_na()
dat1 <- dat %>% select_if(is.numeric) %>% drop_na()

cor(dat1)  #search for correlation

# Question 6: There is a positive correlation between IMDB score and the number of Facebook likes for the movie


# Assignment 8  linear regression.
library(Ecdat)
library(ggplot2)
library(dplyr)

# Question 4  --> 400
age <- c(20,25,25,30,35,45)
income <-c(25000,35000,80000,40000, 65000,65000)
age_income <- data.frame(age, income)
naive_model <- lm(age ~ 1, age_income)
summary(naive_model)
naive_sse <- sum(resid(naive_model)^2)


# Module 9 Logistic regression ----
library(caTools) 
library(MASS)
library(pROC)
library(dplyr)
setwd("/Users/alibadiei/Desktop/DataScience/RCode")
logistic_income_data <- read.csv("income_7.csv")
head(logistic_income_data)

str(logistic_income_data)
glimpse(logistic_income_data)
#

## Take a look at your data
head(logistic_income_data)

summary(logistic_income_data)
income_reduced <- logistic_income_data %>%
  dplyr::select(-income, -country, -education, -government_work,
                -marital_status, -occupation_grouped, -gov_work_grouped)
income_reduced <- na.omit(income_reduced)
#

str(income_reduced)
head(income_reduced)
set.seed(20)  ###  making sure this is reproducible
sample <- sample.split(income_reduced$income_recoded, SplitRatio = .50)
train <- filter(income_reduced, sample == TRUE)
test <- filter(income_reduced, sample == FALSE)
names(train)

reg1 <- glm(income_recoded ~ 
              grouped_marital +
              grouped_gov_work +
              grouped_education +
              grouped_country +
              age +
              gender,
            family = binomial(), train)
summary(reg1)


library(titanic) # contains the data set
library(caTools) # contains function for splitting the data
library(caret) # contains function for creating a confusion matrix
library(dplyr) # data manipulation
library(e1071)
# Access `titanic_train` data set from the {titanic} package and call it `titanic_data`

titanic_data <- titanic::titanic_train

# Rename the columns to only use lower case letters

names(titanic_data) <- tolower(names(titanic_data))

# Explore the data frame object

glimpse(titanic_data)

## Note: In the `survived` column, 0 indicates death, 1 indicates survival

# Change `pclass` and `sex` columns to factors

titanic_data <- titanic_data %>%
  mutate(pclass = as.factor(pclass),
         sex = as.factor(sex))

# Split the data into training set (80%) and testing set (20%)

set.seed(123) # ensure reproducibility
sample <- sample.split(titanic_data$passengerid, SplitRatio = .80)
train <- titanic_data[sample == TRUE,]
test <- titanic_data[sample == FALSE,]

# Apply a logistic model using `sex` and `pclass` to predict `survived` using the `train` set

logreg <- glm(formula = survived ~ pclass + sex,
              family = binomial(link = "logit"),
              data = train)

# Generate predictions using the `test` set and store in column called `prediction`

test <- test %>%
  mutate(prediction = predict(logreg, newdata = test, type = "response"))

# Update the `prediction` column

## Apply a decision rule: if `prediction` > 0.5, change it to 1, else change it to 0

test <- test %>%
  mutate(prediction = ifelse(prediction > 0.5, 1, 0))

# Change `prediction` and `survived` columns to factors

test <- test %>%
  mutate(prediction = as.factor(prediction),
         survived = as.factor(survived))

# Create and print confusion matrix

confusion_matrix <- confusionMatrix(data = test$prediction, reference = test$survived)
confusion_matrix

# Summarize the model

summary(logreg)

# answer 1 : type error 1 : 22
# answer 2: type error 2: 



# Assignment 10: GUI
library(shiny)
library(dplyr)
library(ggplot2)
setwd("/Users/alibadiei/Desktop/DataScience/RCode")
income <- read.csv("income.csv")


# Module 11: 
library(DBI)
setwd("/Users/alibadiei/Desktop/DataScience/RCode")

# Download the SQLite database file I'm hosting on GitHub
# download.file("https://github.com/KristenKehrer/datamovesme-sqlcourse/raw/gh-pages/assets/sqlcourse.db",
#              "./sqlcourse.db",
#              mode = "wb")

# Open a connection to the SQLite database
con <- dbConnect(RSQLite::SQLite(), "./sqlcourse.db")

## List all of the tables
dbListTables(con)

## read the salescall table
dbReadTable(con, "salescall")

# Run the following query:
queryResult <- dbSendQuery(con, "select max(customer_id) from salescall")
dbFetch(queryResult)
## read the salescall table
dbReadTable(con, "salescall")

