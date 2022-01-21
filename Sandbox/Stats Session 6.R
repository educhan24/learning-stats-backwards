library(tidyverse)
library(broom)
library(knitr)
library(here)
library(palmerpenguins)
library(lubridate)
library(janitor)

slot_machine <- function(n_trials, p_win, pay_if_win){
  wins <- rbinom(n = n_trials, size =1, prob = p_win)
  payouts <- wins * pay_if_win
  return(payouts)
}
set.seed(07062021)
gambles <- slot_machine(n_trials = 1000, p_win = 0.5, pay_if_win =2)
gambles %>%
  head()
gambles %>%
  enframe() %>%
  ggplot(aes(x=value)) +
  geom_histogram()
gambles%>%
  sum()
gambles%>%
  enframe(name = "trial", value = "payout")%>%
  mutate(payout_per_trial = cumsum(payout)/trial)%>%
  ggplot(aes(x=trial, y=payout_per_trial)) +
  geom_hline(yintercept = 1, color="red") +
  geom_line()
# mean = total sum/number of items, and expected value 
# is average value of probability-weighted value, with
# empirical distribution of the data

penguins %>%
  lm(body_mass_g ~ sex, data = .)%>%
  tidy() %>%
  kable()
penguins%>%
  drop_na() %>%
  ggplot(aes(x=body_mass_g, color=sex, fill=sex))+
  facet_wrap(~species, ncol=1)+
  geom_histogram(position="identity", alpha=0.2)
penguins%>%
  lm(body_mass_g~sex*species, data=.)%>%
  tidy() %>%
  kable()

return_gaussian <- function(mu, sigma, n_samples=1000){
  rnorm(n=n_samples, mean=mu, sd=sigma)%>%
    enframe(name = NULL)%>%
    mutate(parameters = str_c("Mu=", mu, ", Sigma=", sigma))
}
bind_rows(
  return_gaussian(mu=0, sigma=1),
  return_gaussian(mu=5, sigma=1),
  return_gaussian(mu=-5, sigma=1)
)%>%
  ggplot(aes(x=value, fill=parameters))+
  geom_histogram(position = "identity",
                 alpha = 0.5, bins = 30)
bind_rows(
  return_gaussian(mu=0, sigma=1),
  return_gaussian(mu=0, sigma=10),
  return_gaussian(mu=0, sigma=30)
)%>%
  ggplot(aes(x=value, fill=parameters))+
  geom_histogram(position = "identity",
                 alpha = 0.5, bins = 30)
penguins%>%
  drop_na()%>%
  lm(body_mass_g ~ 1, data = .)%>%
  tidy() %>%
  kable()
penguins%>%
  drop_na() %>%
  summarise(Mean = mean(body_mass_g, na.rm=TRUE),
            SD = sd(body_mass_g, na.rm = TRUE),
            N = n(),
            SE = SD/sqrt(N))%>%
  kable()
