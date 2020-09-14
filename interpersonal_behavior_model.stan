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
