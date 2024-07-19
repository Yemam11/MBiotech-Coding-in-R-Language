#ASSIGNMENT 4
#CODING IN R LANGUAGE
#Summer term 2024
#VIAN TRAN

#PLAN
#1. Read the data into a dataframe

#2. Check and correct structural issues
#datetime: character - change to POSIX
#only need the date column for further use downstream (don't need time)
#date_posted: character - change to POSIX
#change column name of duration.seconds to duration_seconds - all columns names should use "_" as their spaces
#same with duration.hours.minutes

#3. Identify the possible data (content) issues
#the duration.hours.minute column is unorganized, remove it to avoid redundancy since we already know the duration
#in seconds
#if you're interested in keeping it - we can remove it and create an new column with duration_minutes 
#and duration_hours if necessary

#4. Your downstream analysis will require the variables 'country', 'shape' and 'duration seconds'
#Analyze these columns, identify any problems (e.g. missingness, outliers, inconsistency etc) and 
#implement your solution (e.g. remove, impute etc).

#remove duplicates

#there are a lot of empty spaces in country
#its difficult to find the country its from so we are going to remove all rows with an empty space in country

#seconds min is 0 seconds (which means no sighting) and max is 82800000 seconds 
#both seem unrealistic so we should only include sightings that are above 0 seconds and below 24 hours (86400 seconds)

#if the output for shape is empty (for each observation), assign it to "unknown"

#5. NUFORC officials comment on sightings that may be hoax. Identify the best way to identify and 
#remove these sightings from the dataset.
#hoax or NUFORC side note in the comment section suggests they may be hoax
#or empty comment
#having an empty comment, or HOAX/NUFORC in the comment suggests it was filtered by NUFORC and identified as 
#a potential hoax
#we can remove confidently remove these rows
#the creators of the dataset should've made a column that identified if the sighting may be a hoax or not, putting
#it in the comment section is difficult to identify 

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
library(tidyverse)
library(lubridate)

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

#view a summary of the data
summary(martians)
#summary shows duration_seconds is numeric with a min of 0 seconds (which means no sighting) and 
#max is 82800000 seconds 
#both seem unrealistic to have a sighting for 0 seconds or less and longer than ta day (24 hours)
#so we should only include sightings that are above 0 seconds and below 24 hours (86400 seconds)


#keep the original file untouched 
#we will be working with the martians_tidy file
#check and correct structural issues
#identify the possible data (content) issues
martians_tidy <- martians %>% 
  mutate(date_sighting = as.Date(datetime)) %>% #convert datetime (character) to POSIX - only interested in the 
  #date so make a new column labelled as date_sighting
  mutate(date_posted = as.Date(format(dmy(date_posted), "%Y-%m-%d"))) %>% #covert the date_posted to a POSIX, 
  #it is currently formatted as dmy but we are interested in ymd similar to date_sighting
  select(date_sighting, 
         city, 
         state, 
         country, 
         shape, 
         duration_seconds = duration.seconds, 
         comments,
         date_posted,
         latitude,
         longitude) #re-organize the table to remove datetime and include date_sighting instead
  #change the column name of duration.seconds to match the trend of spaces are displayed as "_"
  #remove the duration.hours.minutes column - it is unorganized, inconsistent - we get a summary of it via duration_seconds

#check if the dates are dates
class(martians_tidy$date_sighting)
class(martians_tidy$date_posted)

#preparing the variables "country", "shape" and "duration_seconds" for downstream applications including removing 
#sightings that may be hoax 

martians_tidy2 <- martians_tidy %>% 
  distinct() %>% #remove duplicates using the distinct function
  mutate(country = ifelse(country == "", NA, country)) %>% #replace all empty strings in country with NA
  #ifelse statement checks if a cell in country is an empty string (== ""), if true
  #assign it NA, if false, keep it as the original value (country)
  drop_na(country) %>% #remove rows where the column "country" contains a missing value (NA)
  mutate(state = ifelse(state == "", NA, state)) %>% #replace all empty strings in state with NA, similar to the 
  #method above (making the data frame tidy)
  mutate(shape = ifelse(shape == "", NA, shape)) %>% #replace all empty strings in shape with NA, similar to the 
  #method above
  mutate(shape = ifelse(is.na(shape), "unknown", shape)) %>% #for rows where shape is NA,replace it with 
  #"unknown"
  #the ifelse statement checks if a cell value is NA, if true, assign it to "unknown", if false, keep it as the
  #original value (shape)
  filter(duration_seconds < 86400) %>% #filtering for rows where duration_seconds is below 24 hours (86400 seconds)
  #and above 0 seconds as it seems unrealistic to have a UFO sighting for more than a day 
  filter(duration_seconds > 0) %>%
  mutate(comments = ifelse(comments == "", NA, comments)) %>% #replace all empty strings in shape with NA, similar to the 
  #method above
  filter(!is.na(comments)) %>%
  filter(!str_detect(tolower(comments), "hoax|nuforc"))
  #having an empty comment, the word "HOAX" or "NUFORC" (to lower case) in the comment suggests it was filtered 
  #by NUFORC and identified as a potential hoax
  #a note by NUFORC typically corresponded with a mention of what the sighting actually was (ie. a star or meteor, etc.)
  #filter function will filter out the empty comments and comments with hoax or nuforc in them
  #the downside of this method is since it is a big data set, we are hoping the creators of the data set were 
  #thorough in mentioning hoax or commenting what the actual sighting was
  #if the creator, mentioned a potential hoax via another method that does not jump out (ie. not UFO), it is difficult 
  #to identify 

#the creators of the dataset should've made a column that identified if the sighting may be a hoax or not, putting
#it in the comment section is difficult to identify, especially when they are not consistent 
  
#add another column to the dataset (report_delay) and populate with the time difference in days, 
#between the date of the sighting and the date it was reported
#negative values indicates an event that was posted before it happened
martians_tidy3 <- martians_tidy2 %>%
  mutate(report_delay = as.numeric(date_posted - date_sighting)) %>%  
  filter(report_delay >= 0) #remove rows where the report delay is negative 

#create a table with the average report_delay per country
avg_delay <- martians_tidy3 %>%
  group_by(country) %>%
  summarise(average_delay = mean(report_delay, na.rm = TRUE))
view(avg_delay)

#create a histogram using the "duration_seconds" column
summary(martians_tidy3)
#wide range for the duration_seconds, we should log duration_seconds

hist(log(martians_tidy3$duration_seconds), main = "Frequency of UFO Sightings vs. log(Duration)", 
     xlab = "log(Duration of Sighting (seconds))", ylab = "Frequnec of UFO Sightings",
     xlim = c(-2, 12), ylim = c(0, 6000))

