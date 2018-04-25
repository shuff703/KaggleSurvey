getwd()
setwd('Documents/R/KaggleSurvey')
kaggle_data <- read.csv('multipleChoiceResponses.csv')

#Filter out Data Scientists

#WHOLESETT
whole_set <- kaggle_data

data_science <- kaggle_data[kaggle_data$CurrentJobTitleSelect == 'Data Scientist',]
#Filter Data by Country
usa <- kaggle_data[kaggle_data$Country == 'United States',]
germany <- kaggle_data[kaggle_data$Country == 'Germany',]
india <- kaggle_data[kaggle_data$Country == 'India',]

#Filter Countries by Gender
usa_male <- usa[usa$GenderSelect == 'Male',]
usa_fem <- usa[usa$GenderSelect == 'Female',]

germany_male <- germany[germany$GenderSelect == 'Male',]
germany_fem <- germany[germany$GenderSelect == 'Female',]

india_male <- india[india$GenderSelect == 'Male',]
india_fem <- india[india$GenderSelect == 'Female',]

#See how many records we got
print(nrow(usa_male))
print(nrow(usa_fem))
print(nrow(germany_male))
print(nrow(germany_fem))
print(nrow(india_male))
print(nrow(india_fem))
print(nrow(whole_set))


#Clean the Data
#Clean Function
clean <- function(df, includeJob = TRUE){
  if(!require(tidyverse)) install.packages('tidyverse')
  library(tidyverse)
  if(!require(splitstackshape)) install.packages('splitstackshape')
  library(splitstackshape)
  if(includeJob) df <- select(df, 'CurrentJobTitleSelect', 'LanguageRecommendationSelect', 'LearningPlatformSelect', 'CoursePlatformSelect', 'MLSkillsSelect', 'MLTechniquesSelect', 'WorkAlgorithmsSelect', 'WorkToolsSelect', 'WorkMethodsSelect')
  else df <- select(df, 'LanguageRecommendationSelect', 'LearningPlatformSelect', 'CoursePlatformSelect', 'MLSkillsSelect', 'MLTechniquesSelect', 'WorkAlgorithmsSelect', 'WorkToolsSelect', 'WorkMethodsSelect')
  flat_data <- cSplit(df, c('LearningPlatformSelect', 'CoursePlatformSelect', 'MLSkillsSelect', 'MLTechniquesSelect', 'WorkAlgorithmsSelect', 'WorkToolsSelect', 'WorkMethodsSelect'), sep = ',')
  flat_data$ID <- seq.int(nrow(flat_data))
  # uncomment this to see printed format of data print(flat_data)
  transactions <- gather(flat_data, key, Item , -ID, na.rm = TRUE) %>%
    filter(Item != '') %>%
    select(-key) %>%
    arrange(ID)
  return(transactions)
}

clean_usa_m <- clean(usa_male)
clean_usa_f <- clean(usa_fem)

clean_india_m <- clean(india_male)
clean_india_f <- clean(india_fem)

clean_germany_m <- clean(germany_male)
clean_germany_f <- clean(germany_fem)

clean_data_science <- clean(data_science, FALSE)

clean_whole_set <- clean(whole_set)

#TEST THE RULES FELLERS
#Function to avoid repetition
getRules <- function (df, supp, conf){
  #Write the transactions to .csv
  # Create a temporary directory
  dir.create(path = 'tmp', showWarnings = FALSE)
  
  # Write our data.frame to a csv
  write.csv(df, './tmp/transactions.csv')
  #Run Association Rules
  if(!require(arules)) install.packages('arules')
  library(arules)
  
  transactions <- read.transactions('tmp/transactions.csv', format = 'single', cols = c('ID', 'Item'), sep = ',', rm.duplicates = T)
  
  # Frequent itemsets
  frqisets <- apriori(transactions, parameter=list(minlen = 2, supp = supp, conf = conf, target = 'frequent itemsets'))
  inspect(sort(frqisets, by = 'support'))
  
  # Association rules
  rules<-apriori(transactions, parameter = list(minlen = 2, supp = supp, conf = conf, target = 'rules'))
  
  #Change this to alter the sort and how many rules are displayed
  rules.sorted<-sort(rules, by = 'lift')
  inspect(rules.sorted[1:50])
  
  #quality(rules)<-round(quality(rules), digits=3)
  #inspect(sort(rules,by="confidence"))
  
  return(rules)
}

getRules(clean_usa_m, 0.2, .95)
getRules(clean_usa_f, 0.2, .95)
getRules(clean_germany_m, 0.2, .95)
getRules(clean_germany_f, 0.2, .95)
getRules(clean_india_m, 0.1, .95)
getRules(clean_india_f, 0.1, .95)
getRules(clean_data_science, 0.1, .95)

rules <- getRules(clean_whole_set, .05, .9)

#DONT RUN THESE YET IT MAY BLOW YOUR ENV UP
baseGraph <- function (rules){
  if(!require(arulesViz)) install.packages("arulesViz")
  library(arulesViz)
  rules.sorted <- sort(rules, by='lift')
  plot(rules.sorted)
}

groupedGraph <- function (rules){
  if(!require(arulesViz)) install.packages("arulesViz")
  library(arulesViz)
  rules.sorted <- sort(rules, by='lift')
  plot(rules.sorted, method="grouped")
}

itemGraph <- function (rules){
  if(!require(arulesViz)) install.packages("arulesViz")
  library(arulesViz)
  rules.sorted <- sort(rules, by='lift')
  plot(rules.sorted, method="graph", control=list(type="items"))
}

paracordGraph <- function (rules){
  if(!require(arulesViz)) install.packages("arulesViz")
  library(arulesViz)
  rules.sorted <- sort(rules, by='lift')
  plot(rules.sorted, method="paracoord")
}

plotSingleRule(rule, rules){
  if(!require(arulesViz)) install.packages("arulesViz")
  library(arulesViz)
  rules.sorted <- sort(rules, by='lift')
  set.seed(6201)
  onerule<-sample(rules.sorted, 1)
  inspect(onerule)
  plot(onerule, method="doubledecker", data=rules)
}
# Different plots
baseGraph(rules)






