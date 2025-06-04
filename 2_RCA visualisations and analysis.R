#####Reef Check Australia visualisations and statistics###
#author: Natalie Petrovski
#date: May 2025

##Load packages
library(ggplot2)
library(tidyverse)
library(here)

##############################################################################
#SECTION 1: VISUALISATIONS OF RCA SUBSTRATE PROPORTIONS
##############################################################################

##Load datasets
RC_substrate_long_grouped <- read_csv(here("output_data/RC_substrate_long_grouped.csv"))
RC_bleach_counts <- read_csv(here("output_data/RC_bleach_counts.csv"))

##Custom colors for plotting substrate groups
substrate_colours <- c(
  "HC" = "darkorange",
  "HCB" = "orangered",
  "SC" = "pink1",
  "RKC" = "grey",
  "NL" = "wheat",
  "SP" = "slateblue1",
  "NIA" = "palegreen3",
  "OT" = "peru")

substrate_colours_2 <- c(
  "HC" = "darkorange",
  "HCB" = "orangered")

##sort by site_type if needed for plotting
RC_substrate_long_grouped <- RC_substrate_long_grouped %>%
  mutate(site_type = factor(site_type, levels = c("NF", "SF", "NS", "SS"))) %>%
  arrange(site_type, SITE) %>%
  mutate(SITE_label = factor(SITE_label, levels = unique(SITE_label)))

#####PLOT: Bar plot with substrate proportions
p12 <- ggplot(RC_substrate_long_grouped, aes(x = factor(year), y = Proportion, fill = substrate_group)) +
  geom_col() +
  facet_wrap(~ SITE_label) +
  scale_fill_manual(values = substrate_colours, name = "Substrate Type") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y = "Proportion",
       x = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(path = "figs", filename = "RCA_substr_prop_site.jpeg", p12, width = 11, height = 7, dpi = 300)

###PLOT: Bar plot with HC and HCB proportions only
p13 <- RC_substrate_long_grouped %>%
  filter(substrate_group %in% c("HCB", "HC")) %>%
  ggplot(aes(x = factor(year), y = Proportion, fill = substrate_group)) +
  geom_col(position = "stack") +
  facet_wrap(~ SITE_label) +
  scale_fill_manual(values = substrate_colours_2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y = "Proportion",
       x = "Year")  +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(path = "figs", filename = "RCA_HC_prop_site.jpeg", p13, width = 11, height = 7, dpi = 300)


###Custom colour for plotting by site
site_colours <- c("blue", "darkgreen", "gold", "mediumpurple", "pink1", "orangered2", "orange", "snow4",
                      "sienna", "olivedrab2", "khaki", "turquoise", "maroon")


#####PLOT: line plot - proportion of HCB by year with a line for each site
p14 <- RC_substrate_long_grouped %>%
  filter(substrate_group == "HCB") %>%
  ggplot(aes(x = factor(year), y = Proportion, color = SITE_label, group = SITE)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(x = "Year", y = "Proportion", color = "Site") +
  scale_color_manual(values = site_colours) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    panel.spacing = unit(1, "lines"))
ggsave(path = "figs", filename = "RCA_HCBonly_prop_site.jpeg", p14, width = 11, height = 7, dpi = 300)

####PLOT: box plot - proportion of HCB by year for each site
p_rca_site_spread <- RC_substrate_long_grouped %>%
  filter(substrate_group == "HCB") %>%
  ggplot(aes(x = SITE_label, y = Proportion)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = factor(year)), width = 0.2, size = 2, alpha = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Site", y = "Proportion Bleached (HCB)", color = "Year") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right")
ggsave(path = "figs", filename = "RC_site_spread.jpeg", p_rca_site_spread, width = 11, height = 7, dpi = 300)

##############################################################################
#SECTION 2: VISUALISATIONS OF RCA BLEACHED PROPORTIONS
##############################################################################
##Calculate HC and HCB proportions over sites and years to see increase/decrease respectively
# ##This code doesn't combine HC + HCB to get total coral
# HCtrend <- RC_substrate_long_grouped %>%
#   filter(substrate_group %in% c("HC", "HCB")) %>%
#   group_by(SITE_label, year, substrate_group) %>%
#   summarise(total = sum(Proportion), .groups = "drop")
# 
# ##Reshape data to wide format so HC and HCB are in their own columns
# HCtrend_wide <- RC_substrate_long_grouped %>%
#   filter(substrate_group %in% c("HC", "HCB")) %>%
#   pivot_wider(names_from = substrate_group, values_from = Proportion)
# 
# #####PLOT: HC and HCB proportions over time by site
# p15 <- ggplot(HCtrend_wide, aes(x = year)) +
#   geom_line(aes(y = HC, color = "Healthy")) +
#   geom_line(aes(y = HCB, color = "Bleached")) +
#   facet_wrap(~ SITE_label) +
#   scale_color_manual(values = c("Healthy" = "darkorange", "Bleached" = "orangered")) +
#   labs(y = "Proportion Cover", color = "Hard Coral") +
#   theme_minimal() +
#   scale_x_continuous(breaks = seq(2013, 2024, by = 1)) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
# ggsave(path = "figs", filename = "RCA_HCvHCB_actual_props.jpeg", p12, width = 12, height = 9, dpi = 300)

##Load datasets
RC_bleach <- read_csv(here("output_data/RC_bleach.csv"))
RC_HC_bleach <- read_csv(here("output_data/RC_HC_bleach.csv"))
RC_HC_bleach_dummy <- read_csv(here("output_data/RC_HC_bleach_dummy.csv"))

#####PLOT: proportion of bleached corals out of total coral (not all substrate)
p16 <- ggplot(RC_bleach, aes(x = year, y = prop_bleached)) +
  geom_line(color = "orangered") +
  geom_point(color = "orangered") +
  facet_wrap(~ SITE_label) +
  labs(x = "Year",
       y = "Proportion Bleached (of Total Hard Coral)") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(2013, 2024, by = 1)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none")
ggsave(path = "figs", filename = "RCA_bleaching_proportions_site.jpeg", p16, width = 12, height = 7, dpi = 300)

#####PLOT: Barplot HCB proportion out of HC
p17 <- RC_HC_bleach %>%
  ggplot(aes(x = factor(year), y = prop_bleached, fill = substrate_group)) +
  geom_col(position = "stack") +
  facet_wrap(~ SITE_label) +
  scale_fill_manual(values = substrate_colours_2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y = "Proportion", x = "Year")  +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(path = "figs", filename = "RCA_bleach_bar.jpeg", p17, width = 12, height = 7, dpi = 300)

#####PLOT: HC and HCB proportions as line plot but with points
##Total cover is HC + HCB combined
p18 <- ggplot(RC_HC_bleach_dummy, aes(x = year)) +
  geom_line(aes(y = HC, color = "Healthy")) +
  geom_point(aes(y = HC, color = "Healthy")) +
  geom_line(aes(y = HCB, color = "Bleached")) +
  geom_point(aes(y = HCB, color = "Bleached")) +
  facet_wrap(~ SITE_label) +
  scale_color_manual(values = c("Healthy" = "darkorange", "Bleached" = "orangered")) +
  labs(y = "Proportion of Hard Coral", color = "Hard Coral Condition") +
  scale_x_continuous(breaks = seq(2013, 2024, by = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(path = "figs", filename = "RCA_HCvHCB_site_year.jpeg", p18, width = 12, height = 9, dpi = 300)

##############################################################################
#SECTION 3: GENERALIZED LINEAR MIXED MODEL
#Model bleaching trends over time
##############################################################################

##Load packages
library(glmmTMB)
library(DHARMa)
library(broom.mixed)
library(gt)
library(tibble)
library(ggeffects)
library(RColorBrewer)

##make sure values are numeric
str(RC_bleach_counts)

###FIT MODEL
rc_m <- glmmTMB(
  cbind(n_bleach, total_coral - n_bleach) ~ poly(year,3) + (1 | SITE),
  family = betabinomial(link = "logit"),
  data = RC_bleach_counts)

summary(rc_m)
ranef(rc_m)
confint(rc_m) 

####Export fixed effects results
##Tidy results first
rc_fixed_effects <- tidy(rc_m, effects = "fixed", conf.int = TRUE) %>%
  mutate(estimate = round(estimate, 2), std.error = round(std.error, 2),
         statistic = round(statistic, 2), p.value = round(p.value, 3),
         conf.low = round(conf.low, 3), conf.high = round(conf.high, 3))

##create gt table
rc_fixed_table <- rc_fixed_effects %>%
  select(term, estimate, std.error, statistic, p.value, conf.low, conf.high) %>%
  gt() %>%
  tab_header(title = "GLMM Fixed Effects for RCA Bleaching Proportion") %>%
  cols_label(term = "Term", estimate = "Estimate", std.error = "Std. Error",
             statistic = "z value", p.value = "p-value", conf.low = "95% CI (Low)", conf.high = "95% CI (High)")
##save as .docx table
gtsave(rc_fixed_table, filename = "output_data/RC_Fixed_Effects.docx")

####Export random effects results
##Tidy results first
rc_random_effects <- ranef(rc_m)$cond$SITE %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Site") %>%
  rename(Intercept = `(Intercept)`) %>%
  mutate(Intercept = round(Intercept, 2))

##create gt table
rc_random_table <- rc_random_effects %>%
  gt() %>%
  tab_header(title = "Site-level Random Intercepts for RCA Bleaching Model") %>%
  cols_label(Site = "Site", Intercept = "Random Intercept")
##save as docx
gtsave(rc_random_table, filename = "output_data/RC_Random_Intercepts.docx")

#####PLOT: Random Effects
##extract ranef values
RC_ranef_tidy <- broom.mixed::tidy(rc_m, effects = "ran_vals", conf.int = TRUE)

##Plot random effect estimates with CI
RC_ranef_plot <- ggplot(RC_ranef_tidy, aes(x = reorder(level, estimate), y = estimate)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  labs(x = "Site",
       y = "Estimated Random Intercept") +
  theme_minimal()
ggsave(path = "figs", filename = "RC_ranef.jpeg", RC_ranef_plot , width = 11, height = 7, dpi = 300)


####USE DHARMa to check residuals for fitted model
#export plots needed using 'Export' in plots window
res2 <- simulateResiduals(rc_m)
plot(res2)
par(mfrow=c(1,2))
testDispersion(res2)
testZeroInflation(res2)


##################################
######Model predictions

##Predicted bleaching over time
pred_RC <- ggpredict(rc_m, terms = "year [all]", type = "fixed")
plot(pred_RC) + ggtitle("RCA predicted bleaching trend over time") +
  ylab("Bleaching probability")

###Visualise site specific predicted bleaching
RC_bleach_counts$predicted_bleach <- predict(rc_m, type = "response", re.form = NULL)
ggplot(RC_bleach_counts, aes(x = year, y = predicted_bleach, color = SITE)) +
  geom_line(size = 1) +
  ylab("Bleaching probability") +
  xlab("Year") + 
  scale_color_manual(values = colorRampPalette(brewer.pal(8, "Dark2"))(13)) +
  theme_minimal()

##Calculate mean bleaching proportions by year to add points to predictions plot
RC_bleach_means <- RC_bleach_counts %>%
  group_by(year) %>%
  summarise(prop_bleach = sum(n_bleach) / sum(total_coral))

##PLOT: Predicted bleaching proportion over time (averaging over sites)
##same as above plot but with ggplot to add colour and mean points
pred_RC <- ggpredict(rc_m, terms = "year [all]", type = "fixed")
plot_pred_rc <- ggplot(pred_RC, aes(x = x, y = predicted)) +
  geom_line(size = 1.2, color = "coral") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "coral") +
  geom_point(data = RC_bleach_means, aes(x = year, y = prop_bleach), color = "lightsalmon4", size = 2) +
  labs(x = "Year", y = "Bleaching Proportion") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2013, 2024, by = 1))
ggsave(path = "figs", filename = "RC_model_predictions.jpeg", plot_pred_rc, width = 11, height = 7, dpi = 300)

  