functions {

  real skellam_log(int k, real lambda1, real lambda2) {
    real total;
    real log_prob;
    total = (- lambda1 - lambda2) + (log(lambda1) - log(lambda2)) * k / 2;
    log_prob = total + log(modified_bessel_first_kind(k, 2 * sqrt(lambda1*lambda2)));
    return log_prob;
  }

  real zero_inflated_skellam_log(int k, real lambda1, real lambda2, real p) {
    real base_prob;
    real prob;
    real log_prob;
    base_prob = exp(skellam_log(k, lambda1, lambda2));
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
  int<lower=1, upper=n_teams> home_team[n_games];
  int<lower=1, upper=n_teams> away_team[n_games];
  int goal_difference[n_games];
  // Previous estimates and sd
  real prev_att_means[n_teams];
  real prev_def_means[n_teams];
  real prev_mu_mean;
  real prev_home_advantage_mean;
  real<lower=0> prev_att_sd[n_teams];
  real<lower=0> prev_def_sd[n_teams];
  real<lower=0> prev_mu_sd;
  real<lower=0> prev_home_advantage_sd;
}

parameters {
  real<lower=0, upper=1> p;
  real mu;
  real home_advantage;
  real att_raw[n_teams - 1];
  real def_raw[n_teams - 1];
}

transformed parameters {
  // Sum-to-zero constraint
  vector[n_teams] att;
  vector[n_teams] def;

  for (t in 1:(n_teams-1)) {
    att[t] = att_raw[t];
    def[t] = def_raw[t];
  }

  att[n_teams] = -sum(att_raw);
  def[n_teams] = -sum(def_raw);
}

model {
  vector[n_games] theta_H;
  vector[n_games] theta_A;
  vector[n_games] expected_goal_difference;
  // Priors
  p ~ uniform(0, 1);
  for(a in 1:n_teams){
    att[a] ~ normal(prev_att_means[a], prev_att_sd[a]);
    def[a] ~ normal(prev_def_means[a], prev_def_sd[a]);
  }
  home_advantage ~ normal(prev_home_advantage_mean, prev_home_advantage_sd);
  mu ~ normal(prev_mu_mean, prev_mu_sd);
  // Likelihood
  for (g in 1:n_games) {
    theta_H[g] = exp(mu + home_advantage +att[home_team[g]] + def[away_team[g]]);
    theta_A[g] = exp(mu + att[away_team[g]] + def[home_team[g]]);
    goal_difference[g] ~ zero_inflated_skellam(theta_H[g],theta_A[g],p);
  }
}
