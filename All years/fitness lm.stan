// model is to understand the fitness of BRTE by examining the effects of other 
//species presence/abundance


data {
  //int<lower=0> B; // number of barrels
  int<lower=0> yrs; // number of years
  int<lower=0> nB; // number of replicates BRTE
  int<lower=0> nL; // number of replicates LAGL
  int<lower=0> nE; // number of replicates ELEL
  int<lower=0> nA; // number of replicates ARTR

  vector[nB] BRTE; // fitness (seeds) of BRTE
  vector[nL] LAGL; // LAGL
  vector[nE] ELEL; // ELEL
  vector[nA] ARTR; // ARTR
  
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real b0;
  real b1; // LAGL coefficient
  real b2; // ELEL coefficient
  real b3; // ARTR coefficient
  
  real<lower=0> sigma_e; //
  vector[yrs] yearRE; //random effect intercept for each year
  real sigma_RE; //random effect standard deviation survival

}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  y  ~ neg_binomial_2(b0 + b1*LAGL + b2*ELEL + b3*ARTR + yearRE*sigma_RE, sigma);
}

