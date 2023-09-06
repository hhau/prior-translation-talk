library(dplyr)

source("scripts/common/setup-ggplot-theme.R")

ages_of_interest <- c(2, 8, 13, 18)
age_specific_pars <- list(
  "2" = c(loc = 88, scale = 3.5),
  "8" = c(loc = 130, scale = 5.5),
  "13" = c(loc = 160, scale = 8),
  "18" = c(loc = 172, scale = 9.5)
)

x_vals <- seq(from = 60, to = 210, length.out = 500)
plot_df <- lapply(ages_of_interest, function(an_age) {
  tibble(
    x = x_vals,
    y = dnorm(
      x = x_vals - 5,
      mean = age_specific_pars[[as.character(an_age)]][["loc"]],
      sd = age_specific_pars[[as.character(an_age)]][["scale"]]
    ),
    age = an_age
  )
}) %>%
  bind_rows() %>%
  mutate(
    plot_age_fact = factor(
      x = age,
      levels = ages_of_interest,
      labels = sprintf("'age' == %d", ages_of_interest)
    )
  )

p_1 <- ggplot(plot_df) +
  geom_line(aes(x = x, y = y), colour = highlight_col) +
  facet_wrap(vars(plot_age_fact), labeller = label_parsed, nrow = 2) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank()
  ) +
  xlab("height (cm)") +
  ylab("p(height | age)")

ggsave_presentation(
  plot = p_1,
  filename = "figures/pb-model/cov-target-prior.pdf"
)
