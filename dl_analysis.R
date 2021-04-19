rm(list=ls())

options(stringsAsFactors = FALSE)
library(dplyr)
library(stargazer)
library(ggplot2)
library(gridExtra)
library(ggeffects)
library(sjPlot)
library(sjmisc) #??


# Set Working Directory

setwd("C:/Users/kevin/Desktop/rap_replication")

# Import data 
q = read.csv("data/Digital_Literacy_Replication_October 26, 2020_09.23.csv")

## delete the first two rows
q<-q[c(-1:-2),]
colnames(q)
dim(q) # 24749 x  102

# Remove those who did not complete the survey 
table(q$Finished)
q <- filter(q, Finished == "True")
dim(q)  # 22260 x 102

# Filter out records who chose not to participate in the study
q<-filter(q, Q1.1 == "Yes" )
dim(q)  # 20903 x 102

# Filter out participants who failed the 1st attention check
q<-filter(q, Q1.2 == "I have a question" )
dim(q)  # 13135 x 102

# Filter out participants who failed the 2nd attention check
q<-filter(q, Q1.3 == "Climate change" )
dim(q)  # 11904 x 102

# Mid attention check
q$mid_attention<-0
q$mid_attention[q$Q5.7 == "Extremely interested,Slightly interested"]<-1
q<-filter(q, mid_attention == 1)
dim(q) # 10868 x 103

# Filter the start date 
q$date<-substr(x = q$StartDate, start = 1, stop = 10)
table(q$date)
# Drop initial ppl -- starts from on Sept 30
q <-filter(q, date >= "2020-09-30")
dim(q)  # 10782 x 104

# Time taken to complete the survey
q$Duration_in_seconds<-as.numeric(q$`Duration..in.seconds.`)

#png(filename="plots/survey_duration.png")
plot(q$Duration_in_seconds, xlab = 'Respondent', ylab = "Duration in seconds")
#dev.off()

# Self reported age
q$age_self<- as.numeric(as.character(q$Q2.1)) # NAS introduced to replace non numeric data!
q <- q[!is.na(q$age_self),]
dim(q)  # 10778 x 106

# Only keep people above the age of 18 
q <-filter(q, age_self >= 18)
dim(q)  # 10759 x 106

png(filename="plots/age_dist.png")
hist(q$age_self, 
     xlab = "Age", 
     ylab = "Frequency")
abline(v = mean(q$age_self, na.rm = TRUE), 
       col = "blue", 
       lwd = 2)
dev.off()

# Code digital literacy - Question 2.7_
# Selct digital literacy columns 
dl_cols <- colnames(q)[grepl('Q2.7', colnames(q)) == TRUE][1:14]
# Replace "Full understanding" with "5" 
q[dl_cols] <- apply(q[dl_cols],2,function(x) gsub("Full understanding",'5',x))
# Replace "No understanding" with "1" 
q[dl_cols] <- apply(q[dl_cols],2,function(x) gsub("No understanding",'1',x))

# Convert all dl_cols to numeric 
q[dl_cols] <- sapply(q[dl_cols],as.numeric)
#sapply(q[dl_cols], class)

# Calculate mean Hagard Digital Literacy score
# First 10 questions are the one in Hagard's list 
q$harg_mean<- rowSums(q[dl_cols[1:10]])/10

# No NAs 
table(is.na(q$harg_mean))

# Plot mean score for each respondant - drop rows NAs

#png(filename="plots/harg_mean_dist.png")
hist(q$harg_mean, 
     xlab = "Mean(Hargittai digital literacy score)", 
     ylab = "Frequency")
abline(v = mean(q$harg_mean, na.rm = TRUE), 
       col = "blue", 
       lwd = 2)
#dev.off()
#boxplot(q$harg_mean)


# Code power users - Question Q2.8_
power_cols = colnames(q)[grepl('Q2.8', colnames(q)) == TRUE]
q[power_cols] = apply(q[power_cols],2,function(x) gsub("[^0-9\\-]", "",x))
# Convert all power_cols to numeric 
q[power_cols] <- sapply(q[power_cols],as.numeric)

# Reserse code questions: 1,4,5,6,7 
# so -4 to 4 represents unadvanced to advanced user
q$power_mean<-(-q$Q2.8_1 + q$Q2.8_2 + q$Q2.8_3 - q$Q2.8_4 - q$Q2.8_5 - q$Q2.8_6 - q$Q2.8_7)/ 7

# No NAs 
table(is.na(q$power_mean)) 

#png(filename="plots/powerUser_mean_dist.png")
hist(q$power_mean, xlab = "Mean(Power user score)", ylab = "Frequency")
abline(v = mean(q$power_mean, na.rm = TRUE), col = "blue", lwd = 2)
#dev.off()

#boxplot(q$power_mean)

# Political leaning --> 1 = Republican, -1 = Democrat, 0 = Independent
q$leaning<-0
q$leaning[grepl("Republican", q$Q2.4, fixed = T)] <- 1
q$leaning[grepl("Democrat", q$Q2.4, fixed = T)] <- -1
table(q$leaning)

#png(filename="plots/pol_leaning.png")
hist(q$leaning, xlab = 'Party', ylab = 'Frequency')
#dev.off()

# create indicator for Female: male=1; female=2
q$female <- 0
q$female[q$Q2.2 == 'Male'] <- 1
q$female[q$Q2.2 == 'Female'] <- 2
q$female <- as.factor(q$female)

# create an indicator for Male
q$male <- as.factor(q$Q2.2)

# create factor indicator for race - white or not 
q$white <- 0
q$white[trimws(q$Q2.3) == "White"] <- 1
q$white <- as.factor(q$white)

# select 1st 9000 respondants 
# q <- head(q, 9000)

# Data on which to analysze data
save(q, file="data/q.Rdata")

# Data on which to analysze data -- without mid attention check
#save(q, file="data/q_noMid.Rdata")


# Filtered q - 1st 9k (regardless of mid attention check)
#q_9k <- head(q, 9000)
#save(q_9k, file="data/q_first_9k.Rdata")
