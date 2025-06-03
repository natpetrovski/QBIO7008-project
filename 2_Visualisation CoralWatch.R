#####CoralWatch visualisations and statistics###
#author: Natalie Petrovski
#date: May 2025

##Load packages
library(ggplot2)
library(tidyverse)
library(here)

##############################################################################
#SECTION 1: VISUALISATIONS OF CORALWATCH DATA WITH ALL COLOUR SCORES
##############################################################################

##read in files
#Option 1: spring only data
CWsublong_s <- read_csv(here("output_data/coralwatch_sub_s.csv"))

#Option 2: Spring + autumn 2024 data
CWsublong_a <- read_csv(here("output_data/coralwatch_sub_a.csv"))

# #sort by site type if not already
# CWsublong <- CWsublong %>%
#   mutate(site_type = factor(site_type, levels = c("NF", "SF", "NS", "SS"))) %>%
#   arrange(site_type, SITE) %>%
#   mutate(SITE_label = factor(SITE_label, levels = unique(SITE_label)))

#set manual scale colour for plots
cw.cols <- c("#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#B10026")

######PLOT: Proportion of colour scores per site and year
##Option 1: Spring only months
p1 <- CWsublong_s %>%
  group_by(SITE_label, year, Colour_Code_num) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(SITE_label, year) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = factor(Colour_Code_num), y = prop, fill = factor(Colour_Code_num))) +
  geom_col() +
  facet_grid(SITE_label ~ year, scales = "free_y") +
  labs(x = "Colour Score",
    y = "Proportion") +
  scale_fill_manual(values = cw.cols, name = "Score") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(strip.text.y.right = element_text(angle = 0))
ggsave(path = "figs", filename = "CW_ColourScore_S.jpeg", p1, width = 11, height = 9, dpi = 300)

##Option 2: Spring + autumn 2024
##aggregates the 
p2 <- CWsublong_a %>%
  group_by(SITE_label, year_season, Colour_Code_num) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(SITE_label, year_season) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = factor(Colour_Code_num), y = prop, fill = factor(Colour_Code_num))) +
  geom_col() +
  facet_grid(SITE_label ~ year_season, scales = "free_y") +
  labs(x = "Colour Score",
       y = "Proportion") +
  scale_fill_manual(values = cw.cols, name = "Score") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(strip.text.y.right = element_text(angle = 0))
ggsave(path = "figs", filename = "CW_ColourScore_A.jpeg", p2, width = 15, height = 9, dpi = 300)

#####PLOT: Proportion of colour scores per site and year AS STACKED BARS
##Option 1: Spring only months
p3 <- CWsublong_s %>%
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
ggsave(path = "figs", filename = "CW_ColourScore_S_stacked.jpeg", p3, width = 13, height = 9, dpi = 300)

##############################################################################
#SECTION 2: VISUALISATIONS OF CORALWATCH BLEACHED PROPORTIONS
##############################################################################

#read in files
##Option 1: spring only data by YEAR
CW_bleach_s <- read_csv(here("output_data/CW_bleach_s.csv"))
CW_bleach_s_y <- read_csv(here("output_data/CW_bleach_s_y.csv"))

##Option 2: Spring + autumn 2024 data by YEAR
CW_bleach_a <- read_csv(here("output_data/CW_bleach_a.csv"))
CW_bleach_a_y <- read_csv(here("output_data/CW_bleach_a_y.csv"))

##look at distribution of data
hist(CW_bleach_s_y$prop_bleach_obs)
hist(CW_bleach_a_y$prop_bleach_obs)

# #sort by site type for plotting
CW_bleach_s_y <- CW_bleach_s_y %>%
  mutate(site_type = factor(site_type, levels = c("NF", "SF", "NS", "SS"))) %>%
  arrange(site_type, SITE) %>%
  mutate(SITE_label = factor(SITE_label, levels = unique(SITE_label)))

CW_bleach_a_y <- CW_bleach_a_y %>%
  mutate(site_type = factor(site_type, levels = c("NF", "SF", "NS", "SS"))) %>%
  arrange(site_type, SITE) %>%
  mutate(SITE_label = factor(SITE_label, levels = unique(SITE_label)))

#####PLOT: bleaching proportions per survey grouped by year
##Option 1: Spring only surveys
p4 <- CW_bleach_s %>%
  group_by(year, bleached) %>%
  summarise(n_surveys = n(), .groups = "drop") %>%
  ggplot(aes(x = factor(year), y = n_surveys, fill = bleached)) +
  geom_col(position = "stack") +
  labs(x = "Year",
       y = "Number of Surveys",
       fill = "Bleached (>20% 1s)") +
  scale_fill_manual(values = c("tan2", "tomato2")) +
  theme_minimal()
ggsave(path = "figs", filename = "CW_s_surveys_20perc_bleach.jpeg", p4, width = 7, height = 5, dpi = 300)

##Option 2: Spring + autumn 2024 survey
p5 <- CW_bleach_a %>%
  group_by(year_season, bleached) %>%
  summarise(n_surveys = n(), .groups = "drop") %>%
  ggplot(aes(x = factor(year_season), y = n_surveys, fill = bleached)) +
  geom_col(position = "stack") +
  labs(x = "Year",
       y = "Number of Surveys",
       fill = "Bleached (>20% 1s)") +
  scale_fill_manual(values = c("tan2", "tomato2")) +
  theme_minimal()
ggsave(path = "figs", filename = "CW_a_surveys_20perc_bleach.jpeg", p5, width = 14, height = 7, dpi = 300)

#####PLOT: each survey plotted to visualise prop bleached corals per survey
##Option 1: Spring only
p6 <- ggplot(CW_bleach_s, aes(x = factor(year), y = prop_bleach)) +
  geom_boxplot(outlier.shape = NA, fill = "#FFD580", alpha = 0.4) +
  geom_jitter(aes(color = prop_bleach), width = 0.2, size = 3, alpha = 0.9) +
  scale_color_gradient(high = "#FBD5AB", low = "#D35400", guide = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Year",
    y = "Proportion of Bleached Corals (per Survey)",
    color = "Bleaching Rate"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(path = "figs", filename = "CW_boxplot_prop_s_survey_bleach.jpeg", p6, width = 10, height = 7, dpi = 300)

##Option 2: Spring + autumn 2024
p7 <- ggplot(CW_bleach_a, aes(x = factor(year_season), y = prop_bleach)) +
  geom_boxplot(outlier.shape = NA, fill = "#FFD580", alpha = 0.4) +
  geom_jitter(aes(color = prop_bleach), width = 0.2, size = 3, alpha = 0.9) +
  scale_color_gradient(high = "#FBD5AB", low = "#D35400", guide = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Year",
    y = "Proportion of Bleached Corals (per Survey)",
    color = "Bleaching Rate") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(path = "figs", filename = "CW_boxplot_prop_a_survey_bleach.jpeg", p7, width = 10, height = 7, dpi = 300)


######PLOT: Line plot looking at proportion bleached by year (surveys combined)
##Option 1: Spring only 
p8 <- ggplot(CW_bleach_s_y, aes(x = factor(year), y = prop_bleach_obs, group = SITE_label, color = SITE_label)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Proportion Bleached") +
  theme_minimal() +
  theme(legend.position = "none") +  
  facet_wrap(~ SITE_label)  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(path = "figs", filename = "CW_bleach_s_SITE.jpeg", p8, width = 11, height = 7, dpi = 300)

##Option 2: spring + autumn 2024
p9 <- ggplot(CW_bleach_a_y, aes(x = factor(year_season), y = prop_bleach_obs, group = SITE_label, color = SITE_label)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Proportion Bleached") +
  theme_minimal() +
  theme(legend.position = "none") +  
  facet_wrap(~ SITE_label)  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(path = "figs", filename = "CW_bleach_a_SITE.jpeg", p9, width = 11, height = 7, dpi = 300)

######PLOT: Heatmaps of bleaching proportions by year
##Option 1: Spring only
p10 <- ggplot(CW_bleach_s_y, aes(x = factor(year), y = (SITE_label), fill = prop_bleach_obs)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient(low = "#FFFFCC", high = "#E31A1C", na.value = "grey90") +
  labs(x = "Year", y = "Site", fill = "Prop. Bleached") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_discrete(limits=rev)
ggsave(path = "figs", filename = "CW_heatmap_s.jpeg", p10, width = 11, height = 7, dpi = 300)

##Option 2: Spring + autumn 2024
p11 <- ggplot(CW_bleach_a_y, aes(x = year_season, y = (SITE_label), fill = prop_bleach_obs)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient(low = "#FFFFCC", high = "#E31A1C", na.value = "grey90") +
  labs(x = "Year", y = "Site", fill = "Prop. Bleached") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_discrete(limits=rev)
ggsave(path = "figs", filename = "CW_heatmap_A.jpeg", p11, width = 11, height = 7, dpi = 300)

###PLOT: Spread of proportions across sites (which fluctuated the most)
p_site_spread <- ggplot(CW_bleach_s_y, aes(x = SITE_label, y = prop_bleach_obs)) +
  geom_boxplot(outlier.shape = NA, ) +
  geom_jitter(aes(color = factor(year)), width = 0.2, size = 2, alpha = 0.8) +
  labs(x = "Site", y = "Proportion Bleached", color = "Year") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(path = "figs", filename = "CW_site_spread_S.jpeg", p_site_spread, width = 11, height = 7, dpi = 300)

##############################################################################
#SECTION 3: GENERALIZED LINEAR MIXED MODEL
#Model bleaching trends over time
##############################################################################

####Look at distribution of data to remove any points that may not be suitable for modelling
##how many years sampled per site
CW_bleach_a_y %>%
  group_by(SITE) %>%
  summarise(years_sampled = n_distinct(year)) %>%
  arrange(years_sampled)

table(CW_bleach_a_y$SITE, CW_bleach_a_y$year)

##Load packages you will need
library(glmmTMB)
library(DHARMa)
library(broom.mixed)
library(gt)
library(tibble)
library(ggeffects)
library(RColorBrewer)

##########Using Spring + Autumn 2024 data
##merge 2024 for modelling
CW_bleach_merge <- CW_bleach_a_y %>%
  group_by(SITE, SITE_label, site_type, year) %>%
  summarise(
    n_bleached_obs = sum(n_bleached_obs),
    n_observations = sum(n_observations),
    prop_bleach_obs = n_bleached_obs / n_observations,
    .groups = "drop") %>%
  mutate(year = as.numeric(as.character(year)))
str(CW_bleach_merge)

write_csv(CW_bleach_merge, file = here::here("output_data", "CW_bleach_merge.csv"))

##FIT THE MODEL
cw_m <- glmmTMB(
  cbind(n_bleached_obs, n_observations - n_bleached_obs) ~ poly(year,2) + (1 | SITE),
  family = betabinomial(link = "logit"),
  data = CW_bleach_merge)

summary(cw_m) 
ranef(cw_m) 
confint(cw_m) 

####Export fixed effects results
##Tidy results first
cw_fixed_effects <- tidy(cw_m, effects = "fixed", conf.int = TRUE) %>%
  mutate(estimate = round(estimate, 2), std.error = round(std.error, 2),
    statistic = round(statistic, 2), p.value = round(p.value, 3),
    conf.low = round(conf.low, 3), conf.high = round(conf.high, 3))

##create gt table
cw_fixed_table <- cw_fixed_effects %>%
  select(term, estimate, std.error, statistic, p.value, conf.low, conf.high) %>%
  gt() %>%
  tab_header(title = "GLMM Fixed Effects for CoralWatch Bleaching Proportion") %>%
  cols_label(term = "Term", estimate = "Estimate", std.error = "Std. Error",
    statistic = "z value", p.value = "p-value", conf.low = "95% CI (Low)", conf.high = "95% CI (High)")
##save as .docx table
gtsave(cw_fixed_table, filename = "output_data/CW_Fixed_Effects.docx")

####Export random effects results
##Tidy results first
cw_random_effects <- ranef(cw_m)$cond$SITE %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Site") %>%
  rename(Intercept = `(Intercept)`) %>%
  mutate(Intercept = round(Intercept, 2))

##create gt table
cw_random_table <- cw_random_effects %>%
  gt() %>%
  tab_header(title = "Site-level Random Intercepts for CoralWatch Bleaching Model") %>%
  cols_label(Site = "Site", Intercept = "Random Intercept")
#save as docx
gtsave(cw_random_table, filename = "output_data/CW_Random_Intercepts.docx")

#####PLOT: Random Effects
##extract ranef values
cw_ranef_tidy <- broom.mixed::tidy(cw_m, effects = "ran_vals", conf.int = TRUE)

##Plot random effect estimates with CI
cw_ranef_plot <- ggplot(cw_ranef_tidy, aes(x = reorder(level, estimate), y = estimate)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  labs(x = "Site",
    y = "Estimated Random Intercept") +
  theme_minimal()
ggsave(path = "figs", filename = "CW_ranef.jpeg", cw_ranef_plot, width = 11, height = 7, dpi = 300)


####USE DHARMa to check residuals for fitted model
#export plots needed using 'Export' in plots window
res <- simulateResiduals(cw_m)
plot(res)
testResiduals(res)
plotResiduals(res, form = cw_m$fitted.values)
par(mfrow=c(1,2))
testDispersion(res)
testZeroInflation(res)
testOutliers(res)
plotResiduals(res, form = CW_bleach_merge$year)
hist(predict(cw_m, type = "response"))

##################################
######Model predictions

##Predicted bleaching over time
pred_CW <- ggpredict(cw_m, terms = "year [all]", type = "fixed")
plot(pred_CW) + ggtitle("CoralWatch predicted bleaching trend over time") +
  ylab("Bleaching probability")

###Visualise site specific predicted bleaching
CW_bleach_merge$predicted_bleach <- predict(cw_m, type = "response", re.form = NULL)
ggplot(CW_bleach_merge, aes(x = year, y = predicted_bleach, color = SITE)) +
  geom_line(size = 1) +
  ylab("Bleaching probability") +
  xlab("Year") + 
  scale_color_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(13)) +
  theme_minimal()

##Calculate mean bleaching proportions by year to add points to predictions plot
CW_bleach_means <- CW_bleach_merge %>%
  group_by(year) %>%
  summarise(prop_bleach = sum(n_bleached_obs) / sum(n_observations))

##PLOT: Predicted bleaching proportion over time (averaging over sites)
##same as above plot but with ggplot to add colour and mean points
pred_CW <- ggpredict(cw_m, terms = "year [all]", type = "fixed")
plot_pred_cw <- ggplot(pred_bleach, aes(x = x, y = predicted)) +
  geom_line(size = 1.2, color = "coral") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "coral") +
  geom_point(data = CW_bleach_means, aes(x = year, y = prop_bleach), color = "lightsalmon4", size = 2) +
  labs(
    x = "Year",
    y = "Bleaching Proportion") +
  scale_x_continuous(breaks = seq(2013, 2024, by = 1)) +
  theme_minimal()
ggsave(path = "figs", filename = "CW_model_predictions.jpeg", plot_pred_cw, width = 11, height = 7, dpi = 300)

