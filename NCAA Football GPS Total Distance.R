#### First time collecting new data on your team? Bayes can help!

library(tidyverse)

## set a prior for the mean
mu_prior <- 4455
mu_sd <- 500
tau_prior <- 1/mu_sd^2

## To use the normal-normal conjugate we will make an assumption that the standard deviation is "known"
assumed_sd <- 750
assumed_tau <- 1 / assumed_sd^2

## Create a data frame of observations
df <- data.frame(
  training_day = 1:10,
  dist = c(3800, 3250, 3900, 3883, 3650, 3132, 3300, 3705, 3121, 3500)
)

## create a running/cumulative sum of the outcome of interest
df <- df %>%
  mutate(total_dist = cumsum(dist))

df

## Create a vector to store results from the normal-normal conjugate model
N <- length(df$dist) + 1
mu <- c(mu_prior, rep(NA, N - 1))
tau <- c(tau_prior, rep(NA, N - 1))
SD <- c(assumed_sd, rep(NA, N - 1))

## For loop to continuously update the prior with every new observation
for(i in 2:N){
  
  ## Set up vectors for the variance, denominator, and newly observed values
  numerator <- tau[i - 1] * mu[i - 1] + assumed_tau * df$total_dist[i - 1]
  denominator <- tau[i - 1] + df$training_day[i - 1] * assumed_tau
  
  mu[i] <- numerator / denominator
  tau[i] <- denominator
  SD[i] <- sqrt(1 / denominator)
  
}

df$mu_posterior <- round(mu[-1], 0)
df$SD_posterior <- round(SD[-1], 0)
df$tau_posterior <- tau[-1]

df

### look at the summary stats after 10 sessions
mean(df$dist)            # Mean
sd(df$dist) / sqrt(10)   # Standard Error of the Mean
sd(df$dist)              # Standard Deviation


## Plot results
df_plot <- data.frame(day = 0:10,
                      dist = c(NA, df$dist), 
                      mu,
                      SD)

df_plot %>%
  mutate(low95 = mu - 1.96 * SD,
         high95 = mu + 1.96 * SD) %>%
  ggplot(aes(x = as.factor(day), group = 1)) +
  geom_ribbon(aes(ymin = low95, ymax = high95),
              fill = "light grey",
              alpha = 0.7) +
  geom_line(aes(y = dist),
            size = 1.3) +
  geom_line(aes(y = mu),
            color = "red",
            size = 1.2) +
  geom_hline(aes(yintercept = mean(dist, na.rm = TRUE)),
            linetype = "dashed",
            size = 1.2) +
  geom_point(aes(y = dist),
             shape = 21,
             fill = "white",
             size = 5) +
  ylim(1500, 6000) +
  theme_minimal() +
  annotate(geom = "text",
           label = "--- Avg Total Distance",
           x = 8,
           y = 5000) +
  annotate(geom = "text",
           label = "-- Bayesian Updating of Avg Distance",
           color = "red",
           x = 8,
           y = 5300) +
  labs(x = "Training Day",
       y = "Total Distance (yards)",
       title = "Skill Players' Total Training Distance",
       subtitle = "Grey region reflects ± 95% CI of Bayesian Updating for Total Distance")

