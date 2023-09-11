library(ggplot2)
library(dplyr)
library(cmdstanr)
library(tidybayes)

source("scripts/common/setup-ggplot-theme.R")

n_indivs <- ncol(fda::growth$hgtf)
indivs <- 1 : n_indivs
indiv_t <- fda::growth$age
indiv_data <- as.numeric(fda::growth$hgtf[, indivs])
pb_params_init <- list(h0 = 160, dh1 = 15, s0 = 0.08, ds1 = 0.78, gam = 13)

pb_model <- function(x, theta) {
  with(as.list(theta), {
    h0 + dh1 - (2 * dh1) / (exp(s0 * (x - gam)) + exp((s0 + ds1) * (x - gam)))
  })
}

prefit <- cmdstan_model("scripts/models/pb-indiv.stan")
stan_res <- prefit$sample(
  data = list(
    N_indivs = n_indivs,
    N_obs_per_indiv = nrow(fda::growth$hgtf),
    obs_times = indiv_t,
    heights = fda::growth$hgtf
  ),
  parallel_chains = 4,
  adapt_delta = 0.8,
  max_treedepth = 10
)

stan_summary <- stan_res$summary()
stan_draws <- stan_res$draws()

n_data_to_plot <- 10
pred_interval_width <- 0.864
pred_age <- indiv_t[13]
pred_plot_ages <- seq(from = 1, to = pred_age, length.out = 251)
n_pred_samples <- 5e4

plot_indiv <- function(indiv_id) {
  # get relevant individual parameter draws + noise
  # use the draws to sample the means / model mu
  # add the noise on
  param_names <- dimnames(stan_draws)$variable |>
    grep(x = _, pattern = sprintf("\\[%d\\]", indiv_id), value = TRUE)

  param_names <- c(param_names, "noise_sd")
  indiv_param_samples <- stan_draws[
    sample(1 : dim(stan_draws)[1], n_pred_samples, replace = TRUE),
    sample(1 : 4, 1),
    param_names,
    drop = TRUE
  ]

  colnames(indiv_param_samples) <- c(names(pb_params_init), "noise_sd")
  indiv_mu_samples <- indiv_param_samples |>
    apply(1, function(row) pb_model(pred_plot_ages, row)) |>
    t()

  indiv_noise_samples <- rnorm(
    n = prod(dim(indiv_param_samples)[1], length(pred_plot_ages)),
    mean = 0,
    sd = rep(indiv_param_samples[, "noise_sd"], each = length(pred_plot_ages))
  ) |>
    matrix(nrow = dim(indiv_param_samples)[1], ncol = length(pred_plot_ages))

  indiv_preds <- indiv_mu_samples + indiv_noise_samples

  point_plot_tbl <- tibble(
    x = fda::growth$age[1 : n_data_to_plot],
    y = fda::growth$hgtf[1 : n_data_to_plot, indiv_id]
  )

  pred_area_tbl <- tibble(
    x = pred_plot_ages,
    median = apply(indiv_preds, 2, quantile, 0.5),
    lower = apply(indiv_preds, 2, quantile, (1 - pred_interval_width) / 2),
    upper = apply(indiv_preds, 2, quantile, 1 - (1 - pred_interval_width) / 2)
  )

  segment_tbl <- pred_area_tbl |>
    filter(x == pred_age)

  ggplot() +
    geom_point(
      data = point_plot_tbl,
      mapping = aes(x = x, y = y),
      pch = "+",
      size = 3,
      colour = highlight_col
    ) +
    geom_ribbon(
      data = pred_area_tbl,
      mapping = aes(
        x = x,
        ymin = lower,
        ymax = upper
      ),
      alpha = 0.5
    ) +
    geom_segment(
      data = segment_tbl,
      mapping = aes(
        x = x,
        xend = x,
        y = lower,
        yend = upper
      ),
      colour = blues[2],
      size = 2,
      alpha = 0.6
    ) +
    xlab(expression(italic("t"))) +
    ylab(expression(bolditalic("Y"))) +
    ggtitle(
      label = paste0("Prediction interval is ", round(pred_interval_width * 100, 2), "% wide")#,
      # subtitle = paste0(
      #   "Bayes prediction statement: 'I am ",
      #   round(pred_interval_width * 100, 2),
      #   "% sure \nthat this person will be between ",
      #   round(segment_tbl$lower, 2),
      #   " and ",
      #   round(segment_tbl$upper, 2),
      #   " cm tall at age ",
      #   round(segment_tbl$x),
      #   "."
      # )
    )# +
    #scale_x_continuous(limits = c(1, 18)) +
    #scale_y_continuous(limits = c(70, 190), breaks = seq(from = 75, to = 175, length.out = 5))
}

p1 <- plot_indiv(26)

ggsave_presentation(
  filename = "figures/pb-model/ex-bayes-forecast.pdf",
  plot = p1
)
