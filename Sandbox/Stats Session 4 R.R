library(tidyverse)
library(broom)
library(knitr)
library(here)
library(palmerpenguins)
library(lubridate)
library(janitor)

penguins %>%
  lm(body_mass_g ~ species, data = .) %>%
  tidy()
penguins%>%
  lm(body_mass_g ~ 0 + species,
     data = .)%>%
  tidy()
penguins %>%
  group_by(species, sex)%>%
  summarise(body_mass_g = mean(body_mass_g, na.rm=T), .groups = "drop")%>%
  drop_na()%>%
  ggplot(aes(x = species, y = body_mass_g, fill = sex))+ 
  geom_bar(stat = "identity", position =position_dodge())
penguins%>%
  lm(body_mass_g ~ species + sex, data = .) %>%
  tidy()
# compared to male Adelie, boddy mass dif for male Chinstrap =  3345.1 grams.
# in absolute terms, this is 6717.1 grams.

penguins %>%
  lm(body_mass_g ~ island, data = .) %>%
  tidy()
penguins%>%
  lm(body_mass_g ~ 0 + island,
     data = .)%>%
  tidy()
#hypothesis is confirmed, Biscoe seems to have larger body mass (4716>3713>3706)
penguins %>%
  group_by(island, species)%>%
  summarise(body_mass_g = mean(body_mass_g, na.rm=T), .groups = "drop")%>%
  drop_na()%>%
  ggplot(aes(x = island, y = body_mass_g, fill = species))+ 
  geom_bar(stat = "identity", position = position_dodge())
penguins%>%
  lm(body_mass_g ~ island + species , data = .) %>%
  tidy()
# The intercept shows reference categories island Biscoe and species Adelie.
# The other rows show the difference from the intercept, and adding the other species estimates would show the difference between those particular species.
# The data suggests that the islands are all less than Biscoe, since they are negative
# Findings a mixture of Dream and Chinstrap would be -21.3 + 44.7 = 23.4 g heavier than Biscoe and Adelie,
# which in absolute value is 3733.4.

# The barplot results suggest that all islands have Adelie species, but Biscoe and Dream
# seem to be split between Adelie and one other species. Since Adelie is constant in all,
# consider getting rid of this predictor?

pioneer_riders<- here("Documents", "GitHub", "learning-stats-backwards", "Data", "pioneer_riders.csv")%>%
  read_csv() %>%
  select(date, day, riders) %>%
  mutate(season = case_when(
    date %within% interval(ymd("2005-03-20"), ymd("2005-06-19")) ~ "Spring",
    date %within% interval(ymd("2005-06-20"), ymd("2005-09-21")) ~ "Summer",
    date %within% interval(ymd("2005-09-22"), ymd("2005-12-20")) ~ "Fall"
  )) %>%
  mutate(part_of_week = case_when(day == "Friday" ~ "Friday",
                                  day %in% c("Saturday", "Sunday") ~ "Weekend",
                                  TRUE ~ "Weekday")) %>%
  mutate(season = fct_relevel(season, "Spring", "Summer", "Fall"),
         part_of_week = fct_relevel(part_of_week, "Weekday", "Friday", "Weekend"))
pioneer_riders
glimpse(pioneer_riders)
pioneer_riders%>%
  mutate(day = fct_relevel(day, "Saturday"))%>%
  lm(riders ~ day, data = .)%>%
  tidy()

contr.treatment((7))
contr.helmert(3)

pioneer_riders%>%
  lm(riders ~ part_of_week, contrasts = list(part_of_week= contr.helmert),
     data = .)%>%
  tidy()
# part_of_week1 is 7.95 less than the intercept 375, and part_of_week2 is 16.2 more.
# the p value shows how likely the situation would be given the null hypothesis is true.

pioneer_riders%>%
  group_by(part_of_week)%>%
  summarise(riders=mean(riders))
pioneer_riders%>%
  summarise(riders = mean(riders))

pioneer_riders%>%
  lm(riders~season, contrasts = list(season = MASS::contr.sdif),
     data = .)%>%
  tidy()
# The term season3-2 corresponds to level 3 (fall) minus level 2 (summer), 
# and the negative estimate means that there was an average decrease in daily bike riding

pioneer_riders%>%
  mutate(day = fct_relevel(day,
                           "Monday", "Tuesday", "Wednesday", "Thursday",
                           "Friday", "Saturday", "Sunday")) %>%
  lm(riders ~ day + season,
     contrasts = list(day = contr.sum,
                      season = MASS::contr.sdif),
     data = .) %>%
  tidy()
pioneer_riders%>%
  mutate(day = fct_relevel(day, "Monday", "Tuesday", "Wednesday", "Thursday",
                           "Friday", "Saturday", "Sunday"))%>%
  group_by(day, season)%>%
  summarise(riders = mean(riders), .groups = "drop")%>%
  ggplot(aes(x=day, y=riders, fill = day))+
  facet_grid(cols = vars(season))+
  geom_bar(stat = "identity", position = position_dodge(), show.legend = FALSE)+
  scale_x_discrete(guide = guide_axis(n.dodge =2))

# plotting data/ replicating graph

us_regions <- tibble(state_name = state.name,
                     state_abb = state.abb,
                     state_region = state.region)
school <- here("Documents", "GitHub", "learning-stats-backwards", "Data", "school_diversity.csv") %>%
  read_csv() %>%
  clean_names() %>%
  rename(school_id = leaid,
         district_id = lea_name,
         state = st,
         district_type = d_locale_txt,
         native = aian,
         total_students = total) %>%
  left_join(us_regions, by = c("state"="state_abb")) %>%
  separate(district_type, into = c("urbanicity", "district_size")) %>%
  filter(school_year == "2016-2017") %>%
  mutate(urbanicity = fct_relevel(urbanicity, "rural", "town", "suburban"))%>%
  pivot_longer(cols = native:multi, names_to = "race", values_to = "proportion")%>%
  filter(state_region %in% c("Northeast", "South", "North Central", "West"))%>%
  select(urbanicity, state_region, race, proportion)

school
glimpse(school)

school %>%
  mutate(urbanicity = fct_relevel(urbanicity, "city", "suburban", "town", "rural"))%>%
  mutate(state_region = fct_relevel(state_region, "Northeast", "South", "North Central", "West"))%>%
  group_by(urbanicity, race, state_region)%>%
  summarise(proportion = mean(proportion, na.rm=T), .groups = "drop")%>%
  ggplot(aes(x= urbanicity, y = proportion)) +
  coord_flip()+
  facet_grid(rows = vars(race), cols = vars(state_region))+
  geom_bar(stat = "identity", position = position_dodge(), show.legend = FALSE)

# Hypotheses
us_regions <- tibble(state_name = state.name,
                     state_abb = state.abb,
                     state_region = state.region)

school <- here("Documents", "GitHub", "learning-stats-backwards", "Data", "school_diversity.csv") %>%
  read_csv() %>%
  clean_names() %>%
  rename(school_id = leaid,
         district_id = lea_name,
         state = st,
         district_type = d_locale_txt,
         native = aian,
         total_students = total) %>%
  select(school_id:total_students) %>%
  left_join(us_regions, by = c("state"="state_abb")) %>%
  separate(district_type, into = c("urbanicity", "district_size")) %>%
  filter(school_year == "2016-2017") %>%
  mutate(urbanicity = fct_relevel(urbanicity, "rural", "town", "suburban"))
school %>%
  mutate(state_region= fct_relevel(state_region, "West"))%>%
  lm(black ~ 0 + state_region,
     data = .) %>%
  tidy()
school %>%
  mutate(state_region= fct_relevel(state_region, "West"))%>%
  mutate(urbanicity= fct_relevel(urbanicity, "city"))%>%
  lm(black ~ state_region + urbanicity,
     data = .) %>%
  tidy()


# The data supports the hypothesis; all of the beta coefficients/estimates
# are greater than the intercept (the estimated proportion of
# black population in the West), showing the difference between the intercept
# and regional populations. Same pattern holds true with dummy variable: 
# they reflect that the proportion of black people in West is lowest, showing the actual
# values and not the difference.
# The p value is also the closest to 0, suggesting stronger evidence that you should reject the null hypothesis.
# Accounting for urbanicity: suggests that increased urbanicity does increase proportion of students at school.
# The western rural community has the least proportion of black students,
# but the western city community makes trends hard to discern, though it is clear that increased urbanicity increases school attendees.

school %>%
  mutate(state_region= fct_relevel(state_region, "West"))%>%
  lm(asian ~ state_region,
     data = .) %>%
  tidy()
school %>%
  mutate(state_region= fct_relevel(state_region, "West"))%>%
  mutate(urbanicity= fct_relevel(urbanicity, "city"))%>%
  lm(asian ~ state_region + urbanicity,
     data = .) %>%
  tidy()

# The estimates show that the Northeast actually has greater enrollment, and is
# .09 greater than the West, which seems to have the second highest. The Northeast 
# and west have the p values closest to zero, which seems to refute the null hypothesis.
# Taking urbanicity into account, a Western city
# However, taking urbanicity into account, The Western city has the greatest proportions of asian
# students. This suggests that urbanicity plays a large role/shared variance and 
# has a greater impact than just region alone.

# The plotted graph also shows an upwards trend with urbanicity with all races except whites and natives.
# Difference contrast seems to work best for both.

us_regions <- tibble(state_name = state.name,
                     state_abb = state.abb,
                     state_region = state.region)
us_regions
school
school <- here("Documents", "GitHub", "learning-stats-backwards", "Data", "school_diversity.csv") %>%
  read_csv() %>%
  clean_names() %>%
  rename(school_id = leaid,
         district_id = lea_name,
         state = st,
         district_type = d_locale_txt,
         native = aian,
         total_students = total) %>%
  select(school_id:total_students) %>%
  left_join(us_regions, by = c("state"="state_abb")) %>%
  separate(district_type, into = c("urbanicity", "district_size")) %>%
  filter(school_year == "2016-2017") %>%
  mutate(urbanicity = fct_relevel(urbanicity, "rural", "town", "suburban"))
school %>%
  mutate(urbanicity = fct_relevel(urbanicity, "rural"))%>%
  lm(white + native ~ urbanicity,
    # contrasts = list(urbanicity = contr.helmert),
     data = .) %>%
  tidy()
school %>%
  mutate(state_region= fct_relevel(state_region, "West"))%>%
  mutate(urbanicity= fct_relevel(urbanicity, "rural"))%>%
  lm(white + native ~ state_region + urbanicity,
     data = .) %>%
  tidy()

# Data shows that white and native populations have greatest proportion
# in rural areas, both independently and together. All other types of areas are
# increasingly less as it goes from rural --> urban.
# When taking region into account, North Central regions have the greatest
# proportions, followed by Northeast. Every other scenario is less.
# Intercept p value is 0, disproving the null hypothesis.


# Note: not sure how to incorporate other contrasts here. 
# No single best contrast, but difference contrasts seem to work best
# because only numerical values come from race, which makes groupings difficult

# Hypothesis 5: North central and Northeast grouped by area of country/climate,
# South and West grouped as well. Higher proportion of latinos in South and West
# because of migration and recent surge of job opportunities.

school %>%
  mutate(territory = case_when(state_region %in% c("Northeast", "North Central") ~ "GroupA",
          state_region %in% c("South", "West") ~ "GroupB")) %>%
  mutate(territory = fct_relevel(territory, "GroupA"))%>%
  lm(hispanic ~ territory,
     contrasts = list(territory = contr.helmert),
     data = .) %>%
  tidy()
# The data uses Helmert contrast and compares the average of Group A against Group B.
# It suggests that territory 1, Group B, has a higher average proportion, supporting the hypothesis.
# Both p values are 0, dissproving the null hypothesis.
