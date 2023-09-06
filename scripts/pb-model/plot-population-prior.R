source("scripts/common/setup-ggplot-theme.R")

library(tibble)

target_population_marginal <- function(x) {
  mix_weight <- c(0.38, 0.36, 0.27)
  gamma_shape <- c(45.49, 115.41, 277.51)
  gamma_rate <- c(0.44, 0.81, 1.64)
  N_components <- length(mix_weight)
  N_points <- length(x)
  values <- matrix(data = NA, nrow = N_points, ncol = N_components)

  for (cc in 1 : N_components) {
    values[, cc] <- mix_weight[cc] *
      dgamma(as.numeric(x) - 5, gamma_shape[cc], rate = gamma_rate[cc])
  }

  res_vec <- apply(values, 1, sum)
  return(res_vec)
}

p1 <- ggplot() +
  stat_function(
    fun = target_population_marginal,
    n = 2e3,
    colour = highlight_col
  ) +
  xlim(c(40, 210)) +
  xlab("height (cm)") +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank()
  ) +
  ylab("p(height)")

ggsave_presentation(
  plot = p1,
  filename = "figures/pb-model/pop-target-marginal.pdf"
)

gamma_samples <- tibble(x = c(
  rgamma(n = 380, shape = 45.49, rate = 0.44),
  rgamma(n = 360, shape = 115.41, rate = 0.81),
  rgamma(n = 270, shape = 277.51, rate = 1.64)
) + 5)

p2 <- p1 +
  geom_density(
    data = gamma_samples,
    colour = blues[2],
    mapping = aes(x = x)
  )

ggsave_presentation(
  plot = p2,
  filename = "figures/pb-model/pop-target-marginal-with-samples.pdf"
)
