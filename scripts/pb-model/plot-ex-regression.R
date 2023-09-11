library(ggplot2)
library(dplyr)
library(cmdstanr)

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

plot_indiv <- function(indiv_id) {
  opt_res_par <- stan_summary |>
    filter(
      variable != "lp__",
      grepl(pattern = sprintf("\\[%d\\]", indiv_id), x = variable)
    ) |>
    pull(mean)

  names(opt_res_par) <- names(pb_params_init)

  ggplot() +
    stat_function(
      xlim = c(1, 18),
      fun = pb_model,
      args = list(theta = as.list(opt_res_par)),
      n = 251
    ) +
    geom_point(
      data = tibble(
        x = indiv_t,
        y = fda::growth$hgtf[, indiv_id]
      ),
      mapping = aes(x = x, y = y),
      pch = "+",
      size = 3,
      colour = highlight_col
    ) +
    xlab(expression(italic("t"))) +
    ylab(expression(bolditalic("Y"))) +
    ggtitle(
      expression(italic(h)[0] == 166 * "," ~ italic(h)[1] == 15 * "," ~ italic(s)[0] == 0.1 * "," ~ italic(s)[1] == 0.9 * "," ~ gamma == 13)
    )
}

ggsave_presentation(
  filename = "figures/pb-model/indiv-25.pdf",
  plot = plot_indiv(25)
)
