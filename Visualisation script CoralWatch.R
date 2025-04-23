#####CoralWatch visualisations and statistics###
library(ggplot2)
library(tidyverse)

##########CORAL WATCH###########################
CWsub <- read_csv(file = "coralwatch_sub.csv")

######proportion of colour scores per site and year
CWsub %>%
  group_by(SITE, year, Colour_Code_num) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(SITE, year) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = factor(Colour_Code_num), y = prop, fill = factor(Colour_Code_num))) +
  geom_col() +
  facet_grid(SITE ~ year, scales = "free_y") +
  labs(
    title = "Proportion of Colour Scores per Site and Year",
    x = "Colour Score",
    y = "Proportion") +
  scale_fill_brewer(palette = "YlOrRd", name = "Score") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#####detect bleaching if 1 is more than 20% per survey
CW_bleach_detect <- CWsub %>%
  group_by(survey_id, SITE, DATE, year) %>%
  summarise(
    observations = n(),
    n_bleach = sum(Colour_Code_num == 1),
    prop_bleach = n_bleach / observations
  ) %>% ungroup %>%
  mutate(bleached = prop_bleach > 0.2)

###as above but merging surveys by year
CW_bleach_detect_year <- CW_bleach_detect %>%
  group_by(year, SITE, bleached) %>%
  reframe(
    n_surveys = n_distinct(survey_id, na.rm = TRUE),
    n_observations = sum(observations),
    n_bleached_obs = sum(n_bleach),
    prop_bleach_obs = n_bleached_obs / n_observations)

write_csv(CW_bleach_detect_year, "CW_bleach_detect_year.csv")

###plot (above) bleaching by year (sites combined)
CW_bleach_detect_year %>%
  group_by(year, bleached) %>%
  summarise(total_surveys = sum(n_surveys), .groups = "drop") %>%
  ggplot(aes(x = factor(year), y = total_surveys, fill = bleached)) +
  geom_col(position = "stack") +
  labs(title = "Surveys with Bleaching Detected",
       x = "Year",
       y = "Number of Surveys",
       fill = "Bleached (>20% 1s)") +
  scale_fill_manual(values = c("tan2", "tomato2")) +
  theme_minimal()


###each survey plotted to visualise prop bleached corals per survey
ggplot(CW_bleach_detect_year, aes(x = factor(year), y = prop_bleach_obs)) +
  geom_line(group = 1, color = "darkorange2", linewidth = 1.2) +
  geom_point(aes(color = prop_bleach_obs), size = 4) +
  scale_color_gradient(high = "#FFE5B4", low = "#D35400") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    #title = "",
    x = "Year",
    y = "Proportion of Bleached Corals Per Survey",
    color = "Bleaching Rate"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#only looking at proportion bleached by year (surveys combined)
ggplot(CW_bleach_detect_year, aes(x = year, y = prop_bleach_obs, group = SITE, color = SITE)) +
  geom_line() +
  geom_point() +
  labs(title = "Bleaching Proportion Over Time by Site",
       x = "Year", y = "Proportion Bleached") +
  theme_minimal() +
  theme(legend.position = "none") +  
  facet_wrap(~ SITE)  

##heatmap of proportion bleached by year
ggplot(CW_bleach_detect_year, aes(x = factor(year), y = (SITE), fill = prop_bleach_obs)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient(low = "#FFFFCC", high = "#E31A1C", na.value = "grey90") +
  labs(title = "Bleaching Heatmap by Site and Year",
       x = "Year", y = "Site", fill = "Prop. Bleached") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#as above but boxplots (no site), might be good to see range
ggplot(CW_bleach_detect_year, aes(x = factor(year), y = prop_bleach_obs)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Distribution of Bleaching Across Sites by Year",
       x = "Year", y = "Proportion Bleached") +
  theme_minimal()

##############################################################
##APPLY TIME SERIES LINEAR MODEL
CW_bleach_detect_year <- CW_bleach_detect_year %>%
  mutate(prop_bleach_adj = ifelse(prop_bleach_obs == 0, 0.0001,
                                  ifelse(prop_bleach_obs == 1, 0.9999, prop_bleach_obs)))

library(glmmTMB)
m_beta_cw <- glmmTMB(prop_bleach_adj ~ year,
                data = CW_bleach_detect_year,
                family = beta_family(link = "logit"))
summary(m_beta_cw)
###not significant over time at site/year level.