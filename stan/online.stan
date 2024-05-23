functions {

  real skellam_lpmf(int k, real lambda1, real lambda2) {
    real total;
    real log_prob;
    total = (- lambda1 - lambda2) + (log(lambda1) - log(lambda2)) * k / 2;
    log_prob = total + log(modified_bessel_first_kind(k, 2 * sqrt(lambda1*lambda2)));
    return log_prob;
  }

  real zero_inflated_skellam_lpmf(int k, real lambda1, real lambda2, real p) {
    real base_prob;
    real prob;
    real log_prob;
    base_prob = exp(skellam_lpmf(k| lambda1, lambda2));
    if (k == 0){
      prob = p + (1 - p) * base_prob;
    }
    else{
      prob = (1 - p) * base_prob;
    }
    log_prob = log(prob);
    return log_prob;
  }

}

data {
  int<lower=1> n_teams;
  int<lower=1> n_games;
  array[n_games] int<lower=1, upper=n_teams> home_team;
  array[n_games] int<lower=1, upper=n_teams> away_team;
  array[n_games] int goal_difference;
  // Previous estimates and sd
  array[n_teams-1] real prev_att_MAP;
  array[n_teams-1] real prev_def_MAP;
  real prev_mu_MAP;
  real prev_home_advantage_MAP;
  array[n_teams-1] real<lower=0> prev_att_sd;
  array[n_teams-1] real<lower=0> prev_def_sd;
  real<lower=0> prev_mu_sd;
  real<lower=0> prev_home_advantage_sd;
}

parameters {
  real<lower=0, upper=1> p;
  real mu;
  real home_advantage;
  array[n_teams-1] real att_raw;
  array[n_teams-1] real def_raw;
}

transformed parameters {
  // Sum-to-zero constraint
  array[n_teams] real att;
  array[n_teams] real def;

  for (t in 1:(n_teams-1)) {
    att[t] = att_raw[t];
    def[t] = def_raw[t];
  }

  att[n_teams] = -sum(att_raw);
  def[n_teams] = -sum(def_raw);
}

model {
  array[n_games] real theta_H;
  array[n_games] real theta_A;
  // Priors
  p ~ uniform(0, 1);
  for(a in 1:(n_teams-1)){
    att_raw[a] ~ normal(prev_att_MAP[a], prev_att_sd[a]);
    def_raw[a] ~ normal(prev_def_MAP[a], prev_def_sd[a]);
  }
  home_advantage ~ normal(prev_home_advantage_MAP, prev_home_advantage_sd);
  mu ~ normal(prev_mu_MAP, prev_mu_sd);
  // Likelihood
  for (g in 1:n_games) {
    theta_H[g] = exp(mu + home_advantage +att[home_team[g]] + def[away_team[g]]);
    theta_A[g] = exp(mu + att[away_team[g]] + def[home_team[g]]);
    goal_difference[g] ~ zero_inflated_skellam(theta_H[g],theta_A[g],p);
  }
}
