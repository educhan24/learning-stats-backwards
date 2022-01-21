library(tidyverse)
library(broom)
library(knitr)
library(here)
library(palmerpenguins)
library(lubridate)
library(janitor)

lm_bl_bd_add <- penguins %>%
  lm(body_mass_g~ bill_length_mm + bill_depth_mm, data = .)
expand_grid(
  bill_length_mm = seq(30, 60, 1),
  bill_depth_mm = seq(13, 22, 1)) %>%
  mutate(predicted_body_mass_g = predict(lm_bl_bd_add, newdata = .))%>%
  ggplot(aes(x=bill_length_mm, y = predicted_body_mass_g,
             color= bill_depth_mm, group= bill_depth_mm))+
  geom_point()+
  geom_line()

lm_bl_bd_add <- penguins %>%
  lm(body_mass_g~ bill_length_mm * bill_depth_mm, data = .)
expand_grid(
  bill_length_mm = seq(30, 60, 1),
  bill_depth_mm = seq(13, 22, 1)) %>%
  mutate(predicted_body_mass_g = predict(lm_bl_bd_add, newdata = .))%>%
  ggplot(aes(x=bill_length_mm, y = predicted_body_mass_g,
             color= bill_depth_mm, group= bill_depth_mm))+
  geom_point()+
  geom_line()

penguins%>%
  ggplot(aes(x=bill_length_mm, y=body_mass_g, color=species))+
  geom_point()+
  geom_smooth(method = "lm")
penguins %>%
  lm(body_mass_g ~ bill_length_mm*species,
     data = .)%>%
  tidy()%>%
  kable()
# comparing Gentoo to Adelie:
# BodyMass=ß0+ß1BillLength+ß2SpeciesGentoo+ß4(BillLength×SpeciesGento)
# -> non zero terms: 35 + 95B1 +811B2 + B4(95*-35)
# B0 = 35, 94*50 = 4700, add to intercept: 4735 - 159 = 4576
# 50*15 = 750: 4576 + 750 = 5326 grams
# predicting body mass of 50-mm long Gentoo

penguins %>%
  select(body_mass_g, species, sex)%>%
  drop_na() %>%
  ggplot(aes(x=species, y=body_mass_g, color=sex))+
  geom_point(alpha = 0.5, position = position_jitterdodge
             (jitter.width = 0.2, dodge.width = 0.5)) +
  stat_summary(geom = "crossbar", fun = mean, width = 0.75,
                position = position_dodge(width = 0.5))

penguins%>%
  lm(body_mass_g ~ sex*species,
     data = .)%>%
  tidy() %>%
  kable()
# female Gentoo:3369 + 1311 = 4680
# male Gentoo: 3369 + 675 + 1311 - 130 = 5225
# sex of penguin body mass: male increas
# but effect less strong for Chinstrap than Gentoo

penguins %>%
  ggplot(aes(x=bill_length_mm, y=body_mass_g, color=bill_depth_mm))+
  geom_point() +
  scale_colour_viridis_b(option = "plasma", end = 0.8)+
  geom_smooth(method = "lm")
penguins %>%
  mutate(bill_depth_groups = cut_number(bill_depth_mm, n =5))%>%
  ggplot(aes(x=bill_length_mm, y=body_mass_g, color=bill_depth_groups))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)
penguins %>%
  lm(body_mass_g ~ bill_length_mm * bill_depth_mm,
     data = .)%>%
  tidy() %>%
  kable()
# Outcome=ß0 + ß1billlength + ß2billdepth + ß3(billlength * billdepth)
-25583 + (715*40) + (1485 * 16) + (-36 *40 * 16)
# = 3737


pioneer_riders <- here("Documents", "GitHub", "learning-stats-backwards", "Data", "pioneer_riders.csv") %>%
  read_csv() %>%
  mutate(season = case_when(
    date %within% interval(ymd("2005-03-20"), ymd("2005-06-19")) ~ "Spring",
    date %within% interval(ymd("2005-06-20"), ymd("2005-09-21")) ~ "Summer",
    date %within% interval(ymd("2005-09-22"), ymd("2005-12-20")) ~ "Fall"
  )) %>%
  select(riders, season, day, hi, lo, precip, clouds, weekday)
 # pivot_longer(cols = hi:lo, names_to = "temptype", values_to = "tempval")
  
  
pioneer_riders
#how much temperature effects bikers in seasons

pioneer_riders%>%
  ggplot(aes(x = hi, y= riders, color = season)) +
  geom_point() +
  geom_smooth(method = "lm")
pioneer_riders%>%
  mutate(season = fct_relevel(season, "Fall"))%>%
  lm(riders ~ hi * season,
     data = .)%>%
  tidy() %>%
  kable()
# Shows Spring to have steepest line of best fit, showing
# how change in temp sharply effects it compared to Fall and Summer.
# Regression analysis: summer compared to fall if 80 degrees
# -11 + (5*80) + (-264) + (4*80) = 445 riders in spring
# -11 + (5*80) + (346) + (-4*80) = 415 riders in summer
# Thus, in the spring, there are more hikers on warmer days; hypothesis supported.

covid <- here("Documents", "GitHub", "learning-stats-backwards", "Data", "covid_intervention.csv")%>%
  read_csv() %>%
  mutate(keep = if_else(sub %% 2 == 0, "threat", "prosocial")) %>%
  filter(keep == intervention) %>%
  select(-keep) %>%
  mutate(intervention = fct_relevel(intervention, "threat"))
covid %>%
  ggplot(aes(x=valence, y=willingness, color=intervention)) +
  geom_point() +
  geom_smooth(method = "lm")
# the visual analysis supports that an increase in valence
# has a steep increase in willingness for the prosocial, and slight negative slope for the threatening option.

covid %>%
  mutate(intervention = fct_relevel(intervention, "threat"))%>%
  lm(willingness ~ valence * intervention,
     data = .) %>%
  tidy() %>%
  kable()
# if valence is high at 240:
# threat: 89 + (.03411 * 240) + 4.16 + (-0.035 * 240) = 93
# prosocial: 93 + (-0.0009693*240) - 4.1619999 + (0.0350835*240) = 97
# thus, prosocial shows higher willingness with high valence

covid%>%
  ggplot(aes(x=arousal, y=willingness, color=intervention)) +
  geom_point() +
  geom_smooth(method = "lm")
# the visual analysis supports that an increase in arousal
# has a slightly steeper increase in willingness for the prosocial
# than the threatening option, but the lines of best fit are very similar.

covid %>%
  mutate(intervention = fct_relevel(intervention, "threat"))%>%
  lm(willingness ~ arousal * intervention,
     data = .) %>%
  tidy() %>%
  kable()
# if arousal is high at 240:
# threat: 92 + (.0244863 * 240) -0.2134147 + (-0.0065621 * 240) = 96.08839
# prosocial: 92 + (0.0179242*240) + 0.2134147 + (0.0065621*240) = 98.09013
# thus, prosocial shows slightly higher willingness with high arousal

# could you combine valence + arousal? would probably involve another layer of multiplying,
# prefer two separate regressions