#ASSIGNMENT 4
#CODING IN R LANGUAGE
#Summer term 2024
#VIAN TRAN

#PLAN
#1. Read the data into a dataframe

#2. Check and correct structural issues
#datetime: character - change to POSIX
#separate datetime into date and time column - we need the date column for further use downstream
#date_posted: character - change to POSIX
#change column name of duration.seconds to duration_seconds - all columns names should use "_" as their spaces
#same with duration.hours.minutes

#3. Identify the possible data (content) issues
#the duration.hours.minute column is unorganized, remove it to avoid redundancy since we already know the duration
#in seconds
#if you're interested in keeping it - we can remove it and create an new column with duration_minutes 
#and duration_hours if necessary
#remove duplicates

#4. Your downstream analysis will require the variables 'country', 'shape' and 'duration seconds'
#Analyze these columns, identify any problems (e.g. missingness, outliers, inconsistency etc) and 
#implement your solution (e.g. remove, impute etc).

#there are a lot of empty spaces in country
#for known American states - add us to the country
#for rows without states or country, look at city, and in brackets (), there is usually the country, add country
#abbreviation to the country column 
#seconds min is 0 seconds (which means no sighting) and max is 82800000 seconds 
#both seem unrealistic so we should only include sightings that are above 0 seconds and below 24 hours (86400 seconds)

#5. NUFORC officials comment on sightings that may be hoax. Identify the best way to identify and 
#remove these sightings from the dataset.
#hoax or NUFORC side note in the comment section suggests they may be hoax

#6. Add another column to the dataset (report_delay) and populate with the time difference in days, 
#between the date of the sighting and the date it was reported.
#date posted - date time (of sighting)

#7. Remove the rows where the sighting was reported before it happened.

#8. Create a table with the average report_delay per country.

#9. Create a histogram using the 'duration seconds' column.

#set working directory
setwd("/Users/viantran/Downloads/MBiotech/Code")

#clearing the environment
rm(list=ls())

#loading useful packages (install them if necessary)
library(dplyr)
library(tidyverse)


#read the ufo_subset csv file
martians <- read.csv(file="ufo_subset.csv",header = T)
#verify it is a dataframe
class(martians)

#check the dimensions of the data set
dim(martians)

#view the column names
names(martians)

#view the structure of the data
str(martians)

#look at the structure using dplyr's glimpse()
glimpse(martians)

#view a summary of the data
summary(martians)

#take a closer look at the data by viewing the top and bottom rows
head(martians)
tail(martians)
#column name, datatype and sample


