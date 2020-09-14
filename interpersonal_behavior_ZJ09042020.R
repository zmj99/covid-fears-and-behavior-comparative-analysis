###############################################################################
# rStan model generation for the latent trait 'interpersonal behavior' from the YouGov survey data. Data prepped in a seperate script.
# 
# By: Zack Johnson
# Last Edited: July 16th, 2020
###############################################################################

#clear global environment
rm(list = ls())

#set working directory
setwd("~/covid_analysis/COVID data")

#import needed libraries
library(rstan)
library(rstantools)
library(tidyverse)

#import data
load("covid_fears_v2.RDATA")

#subset data and make a new dataframe with only the variables for the interpersonal behavior trait
ib_data <-
  covid_fears_data %>%
  select(i12_health_1,i12_health_5,i12_health_6,i12_health_11,i12_health_13,i12_health_14,i12_health_16)

#number of rows in our data and number of variables
n <- nrow(ib_data)
k5 <- ncol(ib_data)

#set up the 5 category ordinal variables for rstan
y_k5 <- cbind(ib_data$i12_health_1,ib_data$i12_health_5,ib_data$i12_health_6,ib_data$i12_health_11,ib_data$i12_health_13,ib_data$i12_health_14,ib_data$i12_health_16)
y_k5 <- as.numeric(y_k5)
y_k5_nomis <- which(!is.na(y_k5))
y_k5 <- y_k5[y_k5_nomis] 
n_k5 <- length(y_k5)
item_k5<- matrix(c(1:k5), ncol=k5, nrow=n, byrow=F)
item_k5 <- c(item_k5)
item_k5 <- item_k5[y_k5_nomis]
id_k5 <- matrix(1:n,ncol=k5,nrow=n, byrow=F)
id_k5 <- c(id_k5)
ib_data <- ib_data %>%
  mutate(id = 1:n)
id_k5 <- id_k5[y_k5_nomis]

#put the data in a list to be passes into rstan
ib_data.list <- list(n=n, id_k5=id_k5, item_k5=item_k5, y_k5=y_k5, k5=k5, n_k5=n_k5)

interpersonal_behavior_model <- "
data {
  int<lower=0> n; //number of rows
	
	int<lower=0> n_k5; //number of country-item observations w/ 5 cat
	int<lower=0> k5; //number of items w/ 5 cat
	int<lower=1,upper=5> y_k5[n_k5]; // manifest variables w/ 5 cat
	int<lower=0> item_k5[n_k5]; //index of which item among 5 cat vars
	int<lower=0> id_k5[n_k5]; //index of which subject for 5 cat
  
}

parameters {
	vector<lower=0>[k5] beta_k5; //betas for vars w/ 5 cat
	ordered[4] c_k5[k5]; //matrix of cutpoints for vars w/ 5 cat
  vector[n] theta;
}

model{
  for(j in 1:k5){
    c_k5[j,1] ~ normal(0,5); //priors for 5cat
    c_k5[j,2] ~ normal(0,5);
    c_k5[j,3] ~ normal(0,5);
    c_k5[j,4] ~ normal(0,5);
} 
  theta ~ normal(0,1); //prior on latent variable
  beta_k5 ~ gamma(4,2); //priors on beta values
  
  //linear component
 	for(ii in 1:n_k5){
		y_k5[ii] ~ ordered_logistic(beta_k5[item_k5[ii]] * theta[id_k5[ii]], 
		c_k5[item_k5[ii]]);
	}  
}

"

fit_interpersonal <- stan(model_code = interpersonal_behavior_model, data = ib_data.list, iter = 200, chains = 4)

