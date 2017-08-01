rm(list=ls())
setwd("~/MSBA/Summer - B Term/Fundamentals of Data Analytics/Data Project")

# Reads in data as data.table. *Much* faster than read.table
library(data.table)
passData <- fread("password_data.txt", col.names = c("Usernames", "Passwords"), fill=TRUE)
head(passData)
tail(passData)

###  PASSWORD ANALYSIS  ###

# Number of times each password is used
sorted_pass <- passData[,.N,by=list(Passwords)]
tail(sorted_pass)

# Most repeated passwords
most_repeated_pass <- sorted_pass[order(-N)]$Passwords[1:5]
most_repeated_pass

# Counts the number of times a given passwords are used (cumulative)
length(which(passData$Passwords %in% most_repeated_pass ))

###  USERNAME ANALYSIS  ###

# Number of times each username is used
sorted_usernames <- passData[,.N,by=list(Usernames)]

# Most repeated usernames
most_repeated_usernames <- sorted_usernames[order(-N)]$Usernames[1:5]
most_repeated_usernames

# Counts the number of times given usernames are used (cumulative)
length(which(passData$Passwords %in% most_repeated_usernames ))

# Very slow but counts the number of password/username pairs that match exactly
# count = 0
# for (i in 1:length(passData$Passwords)){
#   if (passData$Passwords[i] == passData$Usernames[i]){
#     count = count + 1
#   }
# }

# Vectorized the above loop. Much faster
matches <- passData[Passwords == Usernames]
head(matches, 50)

# % of matching usernames/passwords
nrow(matches)/nrow(passData)*100

# Find pairs where the username is in the password. Doesn't work as expected
unameInPass <- passData[passData$Usernames %in% passData$Passwords]

# Random sample of 25 in unameInPass
unameInPass[sample(1:nrow(unameInPass),size=25, replace=FALSE)]

###  ENTROPY ANALYSIS  ###

# entropy.empirical() --> Calculates Shannon Entropy. Not sure how to use it yet
# log2(R^L) where R = total options (eg 26 for all lowercase) and L is length of string

# 0-9
ifelse((grepl("[0-9]", most_repeated_pass)),10, 0)
# lowercase
ifelse((grepl("[a-z]", most_repeated_pass)),26, 0)
# Uppercase
ifelse((grepl("[A-Z]", most_repeated_pass)),26, 0)
# Lower and upper case
ifelse((grepl("[A-z]", most_repeated_pass)),52, 0)
# lower/uppercase and digits
ifelse((grepl("[A-z0-9]", most_repeated_pass)),62, 0)
# Punctuation only
ifelse((grepl("[^A-z0-9]", most_repeated_pass)),10, 0)




######### Testing BPA package
#install.packages("bpa")

library(bpa)

partialData <- passData[passData$Passwords == ''] # 2017 rows with username but no password!!
head(partialData)

# Takes a few minutes to run
passData$Passwords %>%
  get_pattern %>% # extract patterns
  table %>%        # tabulate frequencies
  as.data.frame

partialData %>%
  basic_pattern_analysis %>%
  head(10)
