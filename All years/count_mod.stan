
data {
  int<lower=1> N;                 // total number of observations
  int<lower=1> S;                 // number of species
  int<lower=1> B;                 // number of barrels (experimental units)

  int<lower=0> count[N];          // observed plant counts
  vector[N] trt;                  // treatment indicator (0 = control, 1 = repeated)
  int<lower=1, upper=S> species[N]; // species ID for each observation
  int<lower=1, upper=B> barrel[N];  // barrel ID for each observation
}

parameters {
  // -------------------------
  // Hyperparameters
  // -------------------------

  real mu_alpha;                  // global mean log-abundance across species
  real mu_beta;                   // global mean treatment effect across species

  real<lower=0> sigma_alpha;      // among-species SD in baseline abundance
  real<lower=0> sigma_beta;       // among-species SD in treatment effects

  // -------------------------
  // Species-level effects
  // -------------------------

  vector[S] alpha_raw;            // standardized species intercepts
  vector[S] beta_raw;             // standardized species treatment effects

  // -------------------------
  // Barrel-level random effects
  // -------------------------

  vector[B] u_barrel;             // barrel-specific deviations (random intercepts)
  real<lower=0> sigma_barrel;     // SD of barrel-level variation

  // -------------------------
  // Count overdispersion
  // -------------------------

  real<lower=0> phi;              // negative binomial dispersion parameter
}

transformed parameters {
  // Species-specific intercepts (baseline log-counts)
  vector[S] alpha = mu_alpha + sigma_alpha * alpha_raw;

  // Species-specific treatment effects (log-scale)
  vector[S] beta  = mu_beta  + sigma_beta  * beta_raw;
}

model {

  // Priors


  mu_alpha ~ normal(0, 2);         // weakly informative prior on baseline abundance
  mu_beta  ~ normal(0, 1);         // weakly informative prior on treatment effect

  sigma_alpha ~ exponential(1);   // regularizes species differences
  sigma_beta  ~ exponential(1);

  alpha_raw ~ normal(0, 1);        // standard normal for non-centered parameterization
  beta_raw  ~ normal(0, 1);

  sigma_barrel ~ exponential(1);  // barrel-to-barrel variability
  u_barrel ~ normal(0, sigma_barrel);

  phi ~ exponential(1);            // controls overdispersion in counts


  // Likelihood


  for (i in 1:N) {
    real eta;

    // linear predictor on log scale
    eta =
      alpha[species[i]] +          // species baseline abundance
      beta[species[i]] * trt[i] +  // species-specific treatment effect
      u_barrel[barrel[i]];         // barrel-level random effect

    // negative binomial likelihood
    count[i] ~ neg_binomial_2_log(eta, phi);
  }
}


