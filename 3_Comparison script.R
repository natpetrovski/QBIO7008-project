#####CoralWatch and RCA Comparison visualisations and statistics###
#author: Natalie Petrovski
#date: May 2025

##Load packages
library(ggplot2)
library(tidyverse)
library(here)

##Load in data
RC_HC_bleach_dummy <- read_csv(here("output_data/RC_HC_bleach_dummy.csv"))
CW_bleach_merge <- read_csv(here("output_data/CW_bleach_merge.csv"))

##############################################################################
#STEP 1: Merge CoralWatch and RCA datasets
##############################################################################

##Merge
bleach_comparison <- RC_HC_bleach_dummy %>%
  select(SITE, SITE_label, year, site_type, HC, HCB, total_coral, rc_bleach = prop_bleached) %>%
  left_join(CW_bleach_merge %>%
      select(SITE_label, year, cw_bleach = prop_bleach_obs),
    by = c("SITE_label", "year"))

nrow(bleach_comparison)

##sort by site type
bleach_comparison <- bleach_comparison %>%
  mutate(site_type = factor(site_type, levels = c("NF", "SF", "NS", "SS"))) %>%
  arrange(site_type, SITE_label) %>%
  mutate(SITE_label = factor(SITE_label, levels = unique(SITE_label)))

##see how many NA's
na <- bleach_comparison %>%
  filter(if_any(everything(), is.na))

###filter out NA's. CW only has 35 NA's but it will remove that survey for RCA too - so dont apply unless needed
# bleach_comparison_filter <- bleach_comparison %>%
#   filter(!is.na(cw_bleach))
# nrow(bleach_comparison_filter)

##export as csv
write_csv(bleach_comparison, file = here::here("output_data", "bleach_comparison.csv"))

##Make long format
bleach_comparison_long <- bleach_comparison %>%
  pivot_longer(cols = c(rc_bleach, cw_bleach),
               names_to = "method", values_to = "bleach_prop") %>%
  mutate(method = recode(method,
                         rc_bleach = "RCA",
                         cw_bleach = "CW"))

##sort by site type
bleach_comparison_long <- bleach_comparison_long %>%
  mutate(site_type = factor(site_type, levels = c("NF", "SF", "NS", "SS"))) %>%
  arrange(site_type, SITE_label) %>%
  mutate(SITE_label = factor(SITE_label, levels = unique(SITE_label)))

##export as csv
write_csv(bleach_comparison_long, file = here::here("output_data", "bleach_comparison_long.csv"))

##############################################################################
#STEP 2: Plot CoralWatch and RCA bleaching data
##############################################################################
##Load in data if already exported csv files from step 1
bleach_comparison <- read_csv(here("output_data/bleach_comparison.csv"))
bleach_comparison_long <- read_csv(here("output_data/bleach_comparison_long.csv"))

####PLOT: Line plot bleaching proportions
p19 <- ggplot(bleach_comparison_long, aes(x = factor(year), y = bleach_prop, color = method)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_line(aes(group = method), linewidth = 1) +
  facet_wrap(~SITE_label) +
  scale_color_manual(values = c("tomato", "skyblue")) +
  labs(
    y = "Bleaching Proportion", x = "Year", color = "CS program") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
ggsave(path = "figs", filename = "RCA_CW_bleaching_comparison.jpeg", p19, width = 13, height = 9, dpi = 300)

###PLOT: as above but cleaner
p20 <- ggplot(bleach_comparison, aes(x = year)) +
  geom_line(aes(y = rc_bleach, color = "RCA bleaching")) +
  geom_point(aes(y = rc_bleach, color = "RCA bleaching")) +
  geom_line(aes(y = cw_bleach, color = "CoralWatch bleaching")) +
  geom_point(aes(y = cw_bleach, color = "CoralWatch bleaching")) +
  facet_wrap(~ SITE_label) +
  scale_color_manual(values = c("RCA bleaching" = "skyblue", "CoralWatch bleaching" = "lightcoral")) +
  labs(color = "", x = "Year") +
  scale_x_continuous(breaks = seq(2013, 2024, by = 1)) +
  scale_y_continuous(
    name = "Proportion Bleached Corals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom", legend.text=element_text(size=14))
ggsave(path = "figs", filename = "RCA_CW_bleaching_comparison2.jpeg", p20, width = 13, height = 9, dpi = 300)

#####PLOT: Coral cover + bleached with actual proportions
p21 <- ggplot(bleach_comparison, aes(x = year)) +
  geom_line(aes(y = total_coral, color = "Coral Cover"), size = 0.4) +
  geom_point(aes(y = total_coral, color = "Coral Cover"), size = 0.8) +
  geom_line(aes(y = rc_bleach, color = "RCA bleaching")) +
  geom_point(aes(y = rc_bleach, color = "RCA bleaching")) +
  geom_line(aes(y = cw_bleach, color = "CW bleaching")) +
  geom_point(aes(y = cw_bleach, color = "CW bleaching")) +
  facet_wrap(~ SITE_label) +
  scale_color_manual(values = c("Coral Cover" = "darkorange", "RCA bleaching" = "skyblue", "CW bleaching" = "lightcoral")) +
  labs(color = "", x = "Year") +
  scale_x_continuous(breaks = seq(2013, 2024, by = 1)) +
  scale_y_continuous(
    name = "Hard Coral Cover (Proportion of Total Substrate)",
    sec.axis = sec_axis(~ .,name = "Proportion Bleached Corals (of Coral Cover")
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
ggsave(path = "figs", filename = "RCA_CW_bleach_CoralCover_Props.jpeg", p21, width = 13, height = 9, dpi = 300)

####PLOT: coral cover with bleached but scaled to same scale - most accurate
##scale bleached proportions first
bleach_nested <- bleach_comparison %>%
  mutate(RCA_bleach_within_HC = rc_bleach * total_coral,
         CW_bleach_within_HC = cw_bleach * total_coral)

##all as line plots
p22 <- ggplot(bleach_nested, aes(x = year)) +
  geom_line(aes(y = total_coral, color = "Coral Cover"), size = 0.6) +
  geom_point(aes(y = total_coral, color = "Coral Cover"), size = 1) +
  geom_line(aes(y = RCA_bleach_within_HC, color = "RCA Bleaching"), size = 0.6) +
  geom_point(aes(y = RCA_bleach_within_HC, color = "RCA Bleaching"), size = 1) +
  geom_line(aes(y = CW_bleach_within_HC, color = "CW Bleaching"), size = 0.6) +
  geom_point(aes(y = CW_bleach_within_HC, color = "CW Bleaching"), size = 1) +
  facet_wrap(~ SITE_label) +
  scale_color_manual(values = c("Coral Cover" = "darkorange", "RCA Bleaching" = "skyblue", "CW Bleaching" = "lightcoral")) +
  labs(y = "Proportion of Total Substrate", x = "Year", color = "") +
  scale_x_continuous(breaks = seq(2013, 2024, by = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")
ggsave(path = "figs", filename = "RCA_CW_bleach_CoralCover_scaled.jpeg", p22, width = 13, height = 9, dpi = 300)

##Line for Coral Cover and Bars for bleaching proportions
##Need to merge bleaching points to dodge bars
bleach_bar_long <- bleach_nested %>%
  pivot_longer(cols = c(RCA_bleach_within_HC, CW_bleach_within_HC),
               names_to = "Method",
               values_to = "Bleaching") %>%
  mutate(Method = recode(Method,
                         RCA_bleach_within_HC = "RCA Bleaching",
                         CW_bleach_within_HC = "CW Bleaching"))
#plot
p23 <- ggplot(bleach_bar_long, aes(x = year)) +
  geom_line(aes(y = total_coral, color = "Coral Cover"), size = 0.6) +
  geom_point(aes(y = total_coral, color = "Coral Cover"), size = 1) +
  geom_col(aes(y = Bleaching, fill = Method),
           position = position_dodge(width = 0.8), width = 0.8, alpha = 0.8) +
  facet_wrap(~ SITE_label) +
  scale_color_manual(values = c("Coral Cover" = "darkorange")) +
  scale_fill_manual(values = c("RCA Bleaching" = "skyblue", "CW Bleaching" = "lightcoral")) +
  labs(y = "Proportion of Total Substrate", x = "Year", color = "", fill = "") +
  scale_x_continuous(breaks = seq(2013, 2024, by = 1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")
ggsave(path = "figs", filename = "RCA_CW_bleach_CoralCover_Bars.jpeg", p23, width = 13, height = 9, dpi = 300)

#####PLOT: Bleaching proportions as bar plot
p24 <- ggplot(bleach_comparison_long, aes(x = factor(year), y = bleach_prop, fill = method)) +
  geom_col(position = "dodge") +
  facet_wrap(~SITE_label) +
  scale_fill_manual(values = c("tomato", "skyblue")) +
  labs(x = "Year", y = "Bleaching Proportion", fill = "CS program") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
ggsave(path = "figs", filename = "RCA_CW_bleaching_comparison_barplot.jpeg", p24, width = 13, height = 9, dpi = 300)


##############################################################################
#STEP 3: Comparative analysis on CoralWatch and RCA data
##############################################################################

##Run a spearman correlation test on bleaching proportions
cor.test(bleach_comparison$cw_bleach, bleach_comparison$rc_bleach, method = "spearman")
#rho value shows no correlation between bleaching props BUT not statistically significant

####Contingency table
##Make thresholds - adjust as needed
bleach_comparison <- bleach_comparison %>%
  mutate(CW_bleach_detected = case_when(is.na(cw_bleach) ~ "No survey",
                                        cw_bleach >= 0.20 ~ "Yes",
                                        TRUE ~ "No"),
         RCA_bleach_detected = case_when(is.na(rc_bleach) ~ "No survey",
                                         rc_bleach >= 0.20 ~ "Yes",
                                         TRUE ~ "No")) %>%
  mutate(CW_bleach_detected = factor(CW_bleach_detected, levels = c("No", "Yes", "No survey")),
         RCA_bleach_detected = factor(RCA_bleach_detected, levels = c("No", "Yes", "No survey")))

##Create table
bleach_table <- table(CW = bleach_comparison$CW_bleach_detected, RC = bleach_comparison$RCA_bleach_detected)
print(bleach_table)
##convert table to matrix dataframe
bleach_table_df <- as.data.frame.matrix(bleach_table)
#export as csv
write.csv(bleach_table_df, "output_data/comp_bleach_table.csv", row.names = FALSE)

#####PLOT: Comparison plot with scale
bleach_comp_plot <- as.data.frame(bleach_table) %>%
  ggplot(aes(x = CW, y = RC, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 6) +
  scale_fill_viridis_c() +
  labs(
    x = "CW Bleaching Detected",
    y = "RCA Bleaching Detected",
    fill = "Count") +
  theme_minimal(base_size = 14)
ggsave(path = "figs", filename = "bleach_comp_plot.jpeg", bleach_comp_plot, width = 13, height = 9, dpi = 300)

###PLOT: comparison plot without scale
bleach_comp_plot <- as.data.frame(bleach_table) %>%
  ggplot(aes(x = CW, y = RC)) +
  geom_tile(fill = "white", color = "black") +
  geom_text(aes(label = Freq), color = "black", size = 8) +
  labs(
    x = "CW Bleaching Detected",
    y = "RCA Bleaching Detected") +
  theme_minimal(base_size = 16)
ggsave(path = "figs", filename = "bleach_comp_plot2.jpeg", bleach_comp_plot, width = 13, height = 9, dpi = 300)

##show which sites/years bleaching is detected
bleach_list <- bleach_comparison %>%
  select(SITE, year, CW_bleach_detected, RCA_bleach_detected,cw_bleach, rc_bleach) %>%
  filter(CW_bleach_detected == "Yes" |
         RCA_bleach_detected =="Yes")
write.csv(bleach_list, "output_data/comp_bleach_list.csv", row.names = FALSE)

##############################################################################
#STEP 4: Merge prediction plots from GLMMs to plot
##############################################################################
###Need to go to Visualisations CoralWatch and Visualisations RCA scripts to run model if not yet in Global Enironment

##Extract predictions for CW
pred_bleach_CW <- ggpredict(cw_m, terms = "year [all]") %>%
  as_tibble() %>%
  mutate(source = "CoralWatch")

##Extract mean year points CW
CW_bleach_means <- CW_bleach_merge %>%
  group_by(year) %>%
  summarise(prop_bleach = sum(n_bleached_obs) / sum(n_observations)) %>%
  mutate(source = "CoralWatch")

##Extract predictions for RCA
pred_bleach_RCA <- ggpredict(rc_m, terms = "year [all]") %>%
  as_tibble() %>%
  mutate(source = "RCA")

##Extract mean year points RCA
RCA_bleach_means <- RC_bleach_counts %>%
  group_by(year) %>%
  summarise(prop_bleach = sum(n_bleach) / sum(total_coral)) %>%
  mutate(source = "RCA")

##Merge predictions and mean observation points
##predictions
all_pred_bleach <- bind_rows(pred_bleach_CW, pred_bleach_RCA)
#observations
all_bleach_means <- bind_rows(CW_bleach_means, RCA_bleach_means)

#####PLOT: merged predictions
pred_plot_merged <- ggplot(all_pred_bleach, aes(x = x, y = predicted, color = source, fill = source)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3, color = NA) +
  geom_point(data = all_bleach_means, aes(x = year, y = prop_bleach, shape = source), size = 2) +
  scale_color_manual(values = c("CoralWatch" = "lightcoral", "RCA" = "skyblue")) +
  scale_fill_manual(values = c("CoralWatch" = "lightcoral", "RCA" = "skyblue")) +
  scale_shape_manual(values = c("CoralWatch" = 16, "RCA" = 17)) +
  labs(x = "Year",
    y = "Bleaching Proportion",
    color = "Predictions",
    shape = "Observed",
    fill = "95% CI") +
  theme_minimal() +
  scale_x_continuous(breaks = 2013:2024) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(path = "figs", filename = "model_predictions_merged.jpeg", pred_plot_merged, width = 11, height = 7, dpi = 300)


##############################################################################
#STEP 5: See if there is any correlation between coral cover and bleaching 
##############################################################################

############
#RCA

##Import data
RC_bleach <- read_csv(here("output_data/RC_bleach.csv"))
##summarise mean bleaching and mean coral cover
rc_site_summary <- RC_bleach %>%
  group_by(SITE_label) %>%
  summarise(
    mean_bleaching = mean(prop_bleached, na.rm = TRUE),
    mean_coral_cover = mean(total_coral, na.rm = TRUE))

##run spearman correlation
cor.test(rc_site_summary$mean_bleaching, rc_site_summary$mean_coral_cover, method = "spearman")

############
#CW
##Use data in bleach_comparison dataset as CoralWatch doesn't have coral cover and this is based on RCA coral cover
cw_site_summary <- bleach_comparison %>%
  group_by(SITE_label) %>%
  summarise(
    mean_bleaching = mean(cw_bleach, na.rm = TRUE),
    mean_coral_cover = mean(total_coral, na.rm = TRUE))
##run spearman correlation
cor.test(cw_site_summary$mean_bleaching, cw_site_summary$mean_coral_cover, method = "spearman")

