install.packages("palmerpenguins")
library(tidyverse)
library(broom)
library(knitr)
library(here)
library(palmerpenguins)

penguins%>%
  ggplot(aes(x=flipper_length_mm, y= body_mass_g)) +
  geom_point()
penguins %>%
  lm(body_mass_g ~ flipper_length_mm, data = .)%>%
  tidy()
penguins%>%
  ggplot(aes(x=bill_length_mm, y= body_mass_g)) +
  geom_point()
penguins %>%
  lm(body_mass_g ~ bill_length_mm, data = .)%>%
  tidy()

penguins %>%
  filter(species %in% c("Adelie", "Gentoo")) %>%
  group_by(species)%>%
  summarise(body_mass_g = mean(body_mass_g, na.rm = TRUE))%>%
  ggplot(aes(x=species, y=body_mass_g))+
  geom_bar(stat="identity")
penguins%>%
  filter(species %in% c("Adelie", "Gentoo")) %>%
  lm(body_mass_g ~ species, data = .)%>%
  tidy()
with(penguins, cor.test(flipper_length_mm, body_mass_g))%>%
  tidy()
0.8712018 * with(penguins, sd(body_mass_g, na.rm = TRUE) / sd(flipper_length_mm, na.rm = TRUE))
penguins %>%
  lm(body_mass_g ~ flipper_length_mm,
     data = .) %>%
  tidy()
with(penguins %>% filter(species %in% c("Adelie", "Gentoo")),
     t.test(body_mass_g ~species, var.equal = TRUE))%>%
  tidy()
penguins %>%
  filter(species %in% c("Chinstrap", "Gentoo", "Adelie"))%>%
  mutate(x = fct_relevel(species, "Gentoo"))%>%
  lm(body_mass_g ~ x, data = .)%>%
  tidy()

#question: How does the type of question received influence
#people's willingness to self-isolate during covid-19?
covid_intervention <- here("Documents", "GitHub", "learning-stats-backwards","Data", "covid_intervention.csv")%>%
  read_csv() %>%
  mutate(keep = if_else(sub%%2 ==0, "threat", "prosocial"))%>%
  filter(keep==intervention)%>%
  select(sub, intervention, willingness, change, valence, arousal)

covid_intervention%>%
  filter(intervention %in% c("threat", "prosocial")) %>%
  mutate(n = fct_relevel(intervention, "threat"))%>%
  lm(willingness ~ n, data = .)%>%
  tidy()
covid_intervention

covid_intervention%>%
  filter(intervention %in% c("threat", "prosocial")) %>%
  group_by(intervention) %>%
  summarise(willingness = mean(willingness)) %>%
  ggplot(aes(x=intervention, y= willingness)) +
  geom_bar(stat = "identity")
covid_intervention
# The data tells us that the p value for threat is 0.117, 
# suggesting that the difference in willingness for a prosocial response is -0.117 greater.
# Using the equation Willingness = ß0+ß1Intervention,
# Willingness = 93.8 + (.117 * (smallest value)), where ß0 is 93.8, the intercept estimate. 
# Would two numerical items provide more concrete results?

with(covid_intervention, cor.test(valence, willingness))%>%
  tidy()
