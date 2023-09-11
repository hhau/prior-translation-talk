functions {
  vector pb_model (vector times, real h0, real h1, real s0, real s1, real theta) {
    int N_obs = num_elements(times);
    vector [N_obs] result;

    for (ii in 1 : N_obs) {
      real denom_one = exp(s0 * (times[ii] - theta));
      real denom_two = exp(s1 * (times[ii] - theta));
      real numerator = 2 * (h1 - h0);
      result[ii] = h1 - (numerator / (denom_one + denom_two));
    }

    return(result);
  }
}

data {
  int <lower = 1> N_indivs;
  int <lower = 1> N_obs_per_indiv;
  vector <lower = 1, upper = 20> [N_obs_per_indiv] obs_times;
  matrix <lower = 0, upper = 250> [N_obs_per_indiv, N_indivs] heights;
}

transformed data {
  real <lower = 0> min_obs_time = min(obs_times);
  real <lower = 0> max_obs_time = max(obs_times);
}

parameters {
  real <lower = 0> h0_mu;
  real <lower = 0> h0_sigma;
  vector <lower = 0> [N_indivs] h0;

  real <lower = 0> delta_h1_mu;
  real <lower = 0> delta_h1_sigma;
  vector <lower = 0> [N_indivs] delta_h1;

  real <lower = 0> s0_mu;
  real <lower = 0> s0_sigma;
  vector <lower = 0> [N_indivs] s0;

  real <lower = 0> delta_s1_mu;
  real <lower = 0> delta_s1_sigma;
  vector <lower = 0> [N_indivs] delta_s1;

  real <lower = min_obs_time, upper = max_obs_time> theta_mu;
  real <lower = 0> theta_sigma;
  vector <lower = min_obs_time, upper = max_obs_time> [N_indivs] theta;

  real <lower = 0> noise_sd;
}

model {
  for (ii in 1 : N_indivs) {
    vector [N_obs_per_indiv] mu = pb_model(
      obs_times,
      h0[ii],
      h0[ii] + delta_h1[ii],
      s0[ii],
      s0[ii] + delta_s1[ii],
      theta[ii]
    );

    target += normal_lpdf(heights[, ii]' | mu, noise_sd);
  }

  target += normal_lpdf(h0 | h0_mu, h0_sigma);
  target += normal_lpdf(delta_h1 | delta_h1_mu, delta_h1_sigma);
  target += normal_lpdf(s0 | s0_mu, s0_sigma);
  target += normal_lpdf(delta_s1 | delta_s1_mu, delta_s1_sigma);
  target += normal_lpdf(theta | theta_mu, theta_sigma);

  target += lognormal_lpdf(h0_mu | log(150), 5.0);
  target += lognormal_lpdf(delta_h1_mu | log(20), 5.0);
  target += lognormal_lpdf(s0_mu | log(0.1), 0.5);
  target += lognormal_lpdf(delta_s1_mu | log(0.8), 2);
  target += lognormal_lpdf(theta_mu | log(9), 5);

  target += lognormal_lpdf(h0_sigma | 0.0, 1.0);
  target += lognormal_lpdf(delta_h1_sigma | 0.0, 1.0);
  target += lognormal_lpdf(s0_sigma | 0.0, 1.0);
  target += lognormal_lpdf(delta_s1_sigma | 0.0, 1.0);
  target += lognormal_lpdf(theta_sigma | 0.0, 1.0);

  target += lognormal_lpdf(noise_sd | log(0.25), 0.1);
}
