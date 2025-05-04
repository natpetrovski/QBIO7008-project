#####CoralWatch visualisations and statistics###
library(ggplot2)
library(tidyverse)

##########CORAL WATCH###########################
##read correct file in
CWsub <- read_csv(file = "coralwatch_sub.csv")
CWsub <- read_csv(file = "coralwatch_sub_plus_autumn.csv") 

######proportion of colour scores per site and year
CWsub <- CWsub %>%
  arrange(site_type, SITE) %>%
  mutate(SITE_label = factor(SITE_label, levels = unique(SITE_label)))

p1 <- CWsub %>%
  group_by(SITE_label, year, Colour_Code_num) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(SITE_label, year) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = factor(Colour_Code_num), y = prop, fill = factor(Colour_Code_num))) +
  geom_col() +
  facet_grid(SITE_label ~ year, scales = "free_y") +
  labs(x = "Colour Score",
    y = "Proportion") +
  scale_fill_brewer(palette = "YlOrRd", name = "Score") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(strip.text.y.right = element_text(angle = 0))

ggsave(path = "figs", filename = "CW_ColourScore_S_Y1.jpeg", p1, width = 11, height = 9, dpi = 300)

#stacked bars with each year as bar and facet wrap sites
p1b <- CWsub %>%
  group_by(SITE_label, year, Colour_Code_num) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(SITE_label, year) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = factor(year), y = prop, fill = factor(Colour_Code_num))) +
  geom_col(position = "stack") +
  facet_wrap(~SITE_label) +
  labs(x = "Year",
       y = "Proportion") +
  scale_fill_brewer(palette = "YlOrRd", name = "Score") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(strip.text.y.right = element_text(angle = 0))
ggsave(path = "figs", filename = "CW_ColourScore_stacked.jpeg", p1b, width = 13, height = 9, dpi = 300)


#####detect bleaching if 1 is more than 20% per survey
CW_bleach_detect <- CWsub %>%
  group_by(survey_id, SITE, site_type, SITE_label, DATE, year) %>%
  summarise(
    observations = n(),
    n_bleach = sum(Colour_Code_num == 1),
    prop_bleach = n_bleach / observations
  ) %>% ungroup %>%
  mutate(bleached = prop_bleach > 0.2)

###as above but merging surveys by year
CW_bleach_detect_year <- CW_bleach_detect %>%
  group_by(year, SITE, site_type, SITE_label, bleached) %>%
  reframe(
    n_surveys = n_distinct(survey_id, na.rm = TRUE),
    n_observations = sum(observations),
    n_bleached_obs = sum(n_bleach),
    prop_bleach_obs = n_bleached_obs / n_observations)

#Group RF and RS sites - dont run this line if you dont want grouped by RS or RF
CW_bleach_detect_year <- CW_bleach_detect_year %>%
  arrange(site_type, SITE) %>%
  mutate(SITE_label = factor(SITE_label, levels = unique(SITE_label)))

write_csv(CW_bleach_detect_year, "CW_bleach_detect_year.csv")

###plot (above) bleaching by year (sites combined)
p2 <- CW_bleach_detect_year %>%
  group_by(year, bleached) %>%
  summarise(total_surveys = sum(n_surveys), .groups = "drop") %>%
  ggplot(aes(x = factor(year), y = total_surveys, fill = bleached)) +
  geom_col(position = "stack") +
  labs(x = "Year",
       y = "Number of Surveys",
       fill = "Bleached (>20% 1s)") +
  scale_fill_manual(values = c("tan2", "tomato2")) +
  theme_minimal()

ggsave(path = "figs", filename = "CW_surveys_with_20perc_bleach.jpeg", p2, width = 7, height = 5, dpi = 300)
ggsave(path = "figs", filename = "CW_surveys_with_20perc_bleach_plus_autmun2024.jpeg", p2, width = 7, height = 5, dpi = 300)

###each survey plotted to visualise prop bleached corals per survey
ggplot(CW_bleach_detect_year, aes(x = factor(year), y = prop_bleach_obs)) +
  geom_jitter(width = 0.2, aes(color = prop_bleach_obs), size = 3, alpha = 0.7) +
  scale_color_gradient(low = "#FFE5B4", high = "#D35400") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Year",
    y = "Proportion of Bleached Corals Per Survey",
    color = "Bleaching Rate"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#as above but with boxplots
p3 <- ggplot(CW_bleach_detect_year, aes(x = factor(year), y = prop_bleach_obs)) +
  geom_boxplot(outlier.shape = NA, fill = "#FFD580", alpha = 0.4) +
  geom_jitter(aes(color = prop_bleach_obs), width = 0.2, size = 3, alpha = 0.9) +
  scale_color_gradient(high = "#FFE5B4", low = "#D35400", guide = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Year",
    y = "Proportion of Bleached Corals (per Survey)",
    color = "Bleaching Rate"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(path = "figs", filename = "CW_boxplot_prop_survey_bleach.jpeg", p3, width = 9, height = 7, dpi = 300)


#only looking at proportion bleached by year (surveys combined)
p4 <- ggplot(CW_bleach_detect_year, aes(x = factor(year), y = prop_bleach_obs, group = SITE_label, color = SITE_label)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Proportion Bleached") +
  theme_minimal() +
  theme(legend.position = "none") +  
  facet_wrap(~ SITE_label)  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(path = "figs", filename = "CW_prop_bleach_SITE_year.jpeg", p4, width = 11, height = 7, dpi = 300)
ggsave(path = "figs", filename = "CW_prop_bleach_SITE_year_plus_autumn2024.jpeg", p4, width = 11, height = 7, dpi = 300)

##heatmap of proportion bleached by year
p5 <- ggplot(CW_bleach_detect_year, aes(x = factor(year), y = (SITE_label), fill = prop_bleach_obs)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient(low = "#FFFFCC", high = "#E31A1C", na.value = "grey90") +
  labs(x = "Year", y = "Site", fill = "Prop. Bleached") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(path = "figs", filename = "CW_heatmap_bleach_SITE_year.jpeg", p5, width = 11, height = 7, dpi = 300)
ggsave(path = "figs", filename = "CW_heatmap_bleach_SITE_year_plus_autumn2024.jpeg", p5, width = 11, height = 7, dpi = 300)

#as above but boxplots (no site), might be good to see range
ggplot(CW_bleach_detect_year, aes(x = factor(year), y = prop_bleach_obs)) +
  geom_boxplot(fill = "lightblue") +
  labs(x = "Year", y = "Proportion Bleached") +
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

