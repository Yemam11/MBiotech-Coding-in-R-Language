#ASSIGNMENT 4
#CODING IN R LANGUAGE
#Summer term 2024
#VIAN TRAN

#PLAN
#1. Read the data into a dataframe

#2. Check and correct structural issues
#datetime: character - change to date
#only need the date (in ymd not time) for further use downstream 
#date_posted: character - change to date
#change column name of duration.seconds to duration_seconds - all columns names should use "_" as their spaces 
#be consistent 

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
#i tried to updating the country based on the state or city columns (the city column usually had the country in bracket)
#but I don't know how to do it properly

#seconds: minimum is 0 seconds (which means no sighting) and maximum is 82800000 seconds 
#both seem unrealistic so we should only include sightings that are above 0 seconds and below 24 hours (86400 seconds)
#I don't think the sighting will go on for more than a day, it'll be more likely its not a UFO sighting and something
#normal instead 

#if the output for shape is empty (for each observation), assign it to "unknown"

#5. NUFORC officials comment on sightings that may be hoax. Identify the best way to identify and 
#remove these sightings from the dataset.


#having an empty comment, or HOAX/NUFORC in the comment suggests it was filtered by NUFORC officials and identified as 
#a potential hoax
#we can remove these rows
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
#install.packages("tidyverse")
library(tidyverse)

#read the ufo_subset csv file, ensure it is in your working directory
martians <- read.csv(file="ufo_subset.csv",header = T)
#verify it is a dataframe
class(martians)

#check the dimensions of the data set
dim(martians)

#view the column names
#ensure the column names follow the same patterns (duration.seconds vs. date_posted)
names(martians)

#view the structure of the data (check if its character, numeric, etc.)
str(martians)

#view a summary of the data
summary(martians)
#duration_seconds is numeric with a min of 0 seconds (which means no sighting) and 
#max is 82800000 seconds 
#both seem unrealistic to have a sighting for 0 seconds or less and longer than a day (24 hours)
#NOTE: we should only include sightings that are above 0 seconds and below 24 hours (86400 seconds)

#Reviewer comment:
#' Nicely done so far. I like your justification for including and omiting certain observations. 
#' Code runs as expected so far with no errors

#keep the original file untouched 
#we will be working with the martians_tidy file with different versions 
#keep multiple files as you edit, you can look back and compare if the right/wrong changes were made

#check and correct structural issues
#identify the possible data (content) issues
martians_tidy <- martians %>% 
  mutate(date_sighting = as.Date(datetime)) %>% #convert datetime (character) to date (no time) - only interested in the 
  #date so make a new column labelled as date_sighting
  mutate(date_posted = as.Date(format(dmy(date_posted), "%Y-%m-%d"))) %>% #covert the date_posted (character) to date, 
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
  #change the column name of duration.seconds to be consistent with spaces displayed as "_"
  #remove the duration.hours.minutes column - it is unorganized, inconsistent - we get a summary of it via duration_seconds

#Reviewer comments:
#' An alternative way to do this: convert both to POSIXct, which counts seconds since jan 1st 1970. This makes it easy to compare dates/ do math with as well
#' Converting it to date and dropping the datetime column also deletes information. What if you were interested in comparing when 2 sightings happened on the same day?
#' 

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
  #having an empty comment, the word "HOAX" or "NUFORC" (case sensitive) in the comment suggests it was filtered 
  #by NUFORC and identified as a potential hoax
  #a note by NUFORC typically corresponded with a mention of what the sighting actually was (ie. a star or meteor, etc.)
  #filter function will filter out(!) the empty comments and comments with hoax or nuforc in them
  #the downside of this method is since it is a big data set, we are hoping the creators of the data set were 
  #thorough in mentioning hoax or commenting what the actual sighting was
  #if the creator, mentioned a potential hoax via another method that does not jump out (ie. not UFO), it is difficult 
  #to identify and remove

#Reviewer comments:
#' Nicely done, very thourough cleaning of the data set
#' Suggestion: You can change all the "" values in the df to an NA in one line:
#' martians_tidy[martians_tidy == ""] <- NA 
#' This will do the same thing that all your mutate functions did

#the creators of the dataset should've made a column that identified if the sighting may be a hoax or not, putting
#it in the comment section is difficult to identify, especially when they are not consistent 
  
#add another column to the dataset (report_delay) and populate with the time difference in days, 
#between the date of the sighting and the date it was reported
#negative values indicates an event that was posted before it happened
martians_tidy3 <- martians_tidy2 %>%
  mutate(report_delay = as.numeric(date_posted - date_sighting)) %>% #convert the differences to a numeric number
  filter(report_delay >= 0) #filter and remove rows where the report delay is negative 


#' Reviewer comment:
#' Works as expected, but see the comment above about converting to POSIXct. this would have removed the need to convert to numeric
#' Either way well done!

#create a table with the average report_delay per country
avg_delay <- martians_tidy3 %>%
  group_by(country) %>%
  summarise(average_delay = mean(report_delay, na.rm = TRUE))
view(avg_delay)

#create a histogram using the "duration_seconds" column
summary(martians_tidy3)
#wide range for the duration_seconds, we should log duration_seconds

#histogram displaying the frequency of UFO sightings at the log(duration), the x and y axis are adjusted to 
#fit all the data nicely, appropriate titles were given
hist(log(martians_tidy3$duration_seconds), main = "Frequency of UFO Sightings vs. log(Duration)", 
     xlab = "log(Duration of Sighting (seconds))", ylab = "Frequnec of UFO Sightings",
     xlim = c(-2, 12), ylim = c(0, 6000))


#Reviewer comments:
#' Code works as expected, runs without errors
#' Nice histogram! good idea to log the values
#' Overall very well done. You tidied the data very thoroughly and met all the required analysis!

