###############################################################################
# Data prep for survey data from YouGov regarding COVID from different countries. 
# Each country has its own csv file. I Break it down into one dataframe.
# 
# By: Zack Johnson
# Last Edited: July 15, 2020
###############################################################################

#clear global environment
rm(list = ls())

#import libraries
library(tidyverse)
library(plyr)

#set working directory
setwd("~/covid_analysis/covid data")

#make a vector with the country names
country_names <- c('australia','brazil','canada','china','denmark','france','germany','hong-kong','india','indonesia','italy','malaysia','mexico','norway','philippines','saudi-arabia','spain','sweden','taiwan','thailand','united-arab-emirates','united-kingdom','united-states','vietnam')

#create a vector of file names
covid_fears_files <- list.files(pattern = "\\.csv$")
covid_fears_data_list <- map(covid_fears_files, read.csv, fileEncoding = "latin1")

#rename the elements of the lists to match their countries
names(covid_fears_data_list) <- country_names

#function that selects the variables that are wanted from an inputted dataframes
covid_fears_selector <- function(df){
  df <- df %>%
    select(RecordNo,
           
           #interpersonal behavior
           i12_health_1, #face mask wearing (5 cat ordinal)*
           i12_health_5, #keeping away from symptom ridden people (5 cat ordinal)*
           i12_health_6, #avoid going into public (5 cat ordinal)*
           i12_health_11, #avoid having people over (5 cat ordinal)*
           i12_health_13, #avoid medium gatherings (5 cat ordinal)*
           i12_health_14, #avoid large gatherings (5 cat ordinal)*
           i12_health_16, #avoid going to shops (5 cat ordinal)*
           
           #willingness to comply
           m1_1, #willingness to comply given government rec ~1=verywilling - 5=veryunwilling~
           m1_2, #willingness to comply given LAW ~1=verywilling - 5=veryunwilling~
           m1_3, #willingness to comply given peer pressure ~1=verywilling - 5=veryunwilling~
           m1_4, #willingness to comply given Internat Org Rec ~1=verywilling - 5=veryunwilling~
           
           #public behavior
           i12_health_21, #worn a facemask in home ~1=always - 5=never~
           i12_health_22, #worn a facemask in grocery store ~1=always - 5=never~
           i12_health_23, #worn a facemask in clothing store ~1=always - 5=never~
           i12_health_24, #worn a facemask in your place of work ~1=always - 5=never~
           i12_health_25,  #worn a facemask on public transportation ~1=always - 5=never~
           
           #fear of threat of covid (No Finland)
           r1_1, #covid is dangerous to me ~1=disagree - 7=agree~
           r1_2, #i am likely to get covid ~1=disagree - 7=agree~
           r1_3, #wearing a mask protects me from covid ~1=disagree - 7=agree~
           r1_4, #wearing a mask protects others from covid ~1=disagree - 7=agree~
           r1_5, #its not possible for me to wear a mask ~1=disagree - 7=agree~
           r1_6, #its important for me to make decisions for my health ~1=disagree - 7=agree~
           r1_7 #my life has been effected by coronavirus ~1=disagree - 7=agree~
           ) 
  # * denotes same levels
  return(df)
}

#apply the function to the list of dataframes
covid_fears_data_list <- lapply(covid_fears_data_list, covid_fears_selector)

#for loops that iterate through all the variables and turn them in to numeric variables
for(i in 1:length(covid_fears_data_list)){
  covid_fears_data_list[[i]] <- covid_fears_data_list[[i]] %>%
    mutate_at(vars(i12_health_1,i12_health_5,i12_health_6,i12_health_11,i12_health_13,i12_health_14,i12_health_16), 
              function(x)as.ordered(mapvalues(x , from = c("Not at all","Rarely","Sometimes","Frequently","Always", " "),to = c(1:5, NA))))
}
for(i in 1:length(covid_fears_data_list)){
  covid_fears_data_list[[i]] <- covid_fears_data_list[[i]] %>%
    mutate_at(vars(i12_health_21,i12_health_22,i12_health_23,i12_health_24,i12_health_25), 
              function(x)as.ordered(mapvalues(x , from = c("Not at all","Rarely","Sometimes","Frequently","Always", " "),to = c(1:5,NA))))
}
for(i in 1:length(covid_fears_data_list)){
  covid_fears_data_list[[i]] <- covid_fears_data_list[[i]] %>%
    mutate_at(vars(r1_1,r1_2,r1_3,r1_4,r1_5,r1_6,r1_7), 
              function(x)as.ordered(mapvalues(x , from = c( " ", "1 \u0096 Disagree", "2", "3", "4", "5", "6","7 - Agree"), to = c(NA,1:7))))
}
for(i in 1:length(covid_fears_data_list)){
  covid_fears_data_list[[i]] <- covid_fears_data_list[[i]] %>%
    mutate_at(vars(m1_1,m1_2,m1_3,m1_4), 
              function(x)as.ordered(mapvalues(x , from = c("Very willing","Quite willing","Neither willing or unwilling","Quite unwilling","Very unwilling","Not sure"," "), to = c(5,4,3,2,1,NA,NA))))
}

#add a country variable to use when we merge all the data to one dataframe
for(i in 1:length(covid_fears_data_list)){
  covid_fears_data_list[[i]] <- covid_fears_data_list[[i]] %>%
    dplyr::mutate(country = names(covid_fears_data_list[i]))
}

covid_fears_data <- do.call("rbind", covid_fears_data_list)

save(covid_fears_data, file = "covid_fears_v2.RDATA")