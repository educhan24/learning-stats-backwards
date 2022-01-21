library(tidyverse)
library(broom)
library(knitr)
library(here)
library(palmerpenguins)

penguins%>%
  glimpse
lm_bd <- penguins %>%
  lm(body_mass_g ~ bill_depth_mm, data = .)
tidy(lm_bd)
summary(lm_bd)
glance(lm_bd)

lm_fl <- penguins %>%
  lm(body_mass_g ~ flipper_length_mm, data = .)
tidy(lm_fl)
lm_bl <- penguins %>%
  lm(body_mass_g ~ bill_length_mm, data = .)
tidy(lm_bl)

lm_fl %>% glance()
lm_bl %>% glance()
lm_fl_bl <- penguins %>%
  lm(body_mass_g ~ flipper_length_mm + bill_length_mm, data = .)
lm_fl_bl %>% glance()
lm_fl_bl %>% tidy()

penguins %>%
  ggplot(aes(x=flipper_length_mm, y=body_mass_g, color=bill_length_mm))+
  geom_point() +
  scale_colour_viridis_b(option = "plasma")

lm_bl_bd <- penguins %>%
  lm(body_mass_g ~ bill_length_mm+ bill_depth_mm, data = .)
lm_bl_bd %>% glance()
lm_bl %>% glance()
lm_bd %>% glance()
lm_bl_bd %>% tidy()

covid_intervention <- here("Documents", "GitHub", "learning-stats-backwards", "Data", "covid_intervention.csv") %>%
  read_csv() %>%
  mutate(keep = if_else(sub %% 2 == 0, "threat", "prosocial")) %>%
  filter(keep == intervention) %>%
  select(sub, willingness, valence, arousal, extraversion = bfi_extraversion, neuroticism = bfi_neuroticism)

lm_val <- covid_intervention%>%
  lm(willingness ~ valence, data = .)
lm_val %>% tidy()

lm_arous <- covid_intervention%>%
  lm(willingness ~ arousal, data = .)
lm_arous %>% tidy()

lm_val_arous <- covid_intervention%>%
  lm(willingness ~ valence + arousal, data = .)
lm_val_arous %>% tidy()

lm_val%>% glance()
lm_arous %>% glance()
lm_val_arous %>% glance()

covid_intervention%>%
  ggplot(aes(x=arousal, y= willingness, color=valence)) +
  geom_point() +
  scale_color_viridis_b(option = "plasma")

#ß0: the intercept where valence and arousal are both 0: 91.8.
#ß1: for every 1 unit increase in valence, 
# a person's willingness increases by 0.00759 (p value), adjusted by arousal.
#ß2: for every 1 unit increase in arousal, 
# a person's willingness increases by 0.0216 (p value), adjusted by valence.

# The hypothesis suggests lower valence and higher arousal= more willingness.
# This seems supported: graph shows the greatest concentration of high willingness 
# with arousal approaching 200 and valence approaching -200.

# Their R^2 tests show that shared variance is less than valence's impact
# by itself. This suggests that the amount of overlap in variation
# between the two variables is less than valence's impact alone, and only
# slightly higher than arousal's impact.

# This is because when valence and arousal become semi-partial estimates of the
# relationship strength, they reflect each predictor’s unique contribution 
# to the overall variance.


# Exercise 2
# Willingness=ß0+ß1Extraversion +ß2Neuroticism.
# Plausible hypotheses: The lesser the extraversion and greater the neuroticism,
# the more likely an individual is willing to self-isolate. 
# Alternatively, the more extraverted and less neurotic a person is, the more
# willing they are to self-isolate.

lm_extra <- covid_intervention%>%
  lm(willingness ~ extraversion, data = .)
lm_extra %>% tidy()

lm_neurotic <- covid_intervention%>%
  lm(willingness ~ neuroticism, data = .)
lm_neurotic %>% tidy()

lm_extra_neurotic <- covid_intervention%>%
  lm(willingness ~ extraversion + neuroticism, data = .)
lm_extra_neurotic %>% tidy()

lm_extra%>% glance()
lm_neurotic %>% glance()
lm_extra_neurotic %>% glance()

covid_intervention%>%
  ggplot(aes(x= extraversion, y= willingness, color=neuroticism)) +
  geom_point() +
  scale_color_viridis_b(option = "plasma")

#ß0: the intercept where valence and arousal are both 0: 92.9.
#ß1: for every 1 unit increase in extraversion, 
# a person's willingness increases by 0.509 (p value), adjusted by neuroticism.
#ß2: for every 1 unit increase in neuroticism, 
# a person's willingness decreases by 0.241 (p value), adjusted by extraversion.

lm_arousal <- covid_intervention%>%
  lm(willingness ~ arousal, data = .)
lm_arousal %>% tidy()

lm_neurotic_arousal <- covid_intervention%>%
  lm(willingness ~ neuroticism + arousal, data = .)
lm_neurotic_arousal %>% tidy()

lm_arousal %>% glance()
lm_neurotic %>% glance()
lm_neurotic_arousal %>% glance()

lm_arousal <- covid_intervention%>%
  lm(willingness ~ arousal, data = .)
lm_arousal %>% tidy()

lm_extra_arousal <- covid_intervention%>%
  lm(willingness ~ extraversion + arousal, data = .)
lm_extra_arousal %>% tidy()

lm_arousal %>% glance()
lm_extra %>% glance()
lm_extra_arousal %>% glance()

# the model seems to suggest that higher extraversion correlates to lower neuroticism,
# and also that willingness is increased by extraversion and 
# decreased by neuroticism. 


# To control for those personality variables (neuroticism and extraversion),
# one might find a group with consistently 'middle' levels of neuroticism and extraversion,
# so that all participants fall in the same category. However, these personality
# traits may feed directly into the emotion categories. For instance, there might
# be a lot of shared variance between high neuroticism and arousal, as people more concerned
# and prone to guilt would have higher activation of emotional reactions. 
# 
# Regarding shared variance: On its own, arousal seems to account significantly
# for predicted willingness. Adding neuroticism doesn't seem to help that much, which is the problem
# of shared variance. The same holds true for extraversion. (this is using r^2.)
# The revised model seems to suggest that there is a lot of shared variance
# between emotion and personality variables.