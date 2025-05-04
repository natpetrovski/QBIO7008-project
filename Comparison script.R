#####Comparsion visualisations and statistics###
library(ggplot2)
library(tidyverse)

RC_bleach <- read_csv(file = "RC_bleach.csv")
CW_bleach_detect_year <- read_csv(file = "CW_bleach_detect_year.csv")


##merge CW and RC data into one table
bleach_comparison <- RC_bleach %>%
  select(SITE, year, site_type, SITE_label, rc_bleach = prop_bleached) %>%
  left_join(
    CW_bleach_detect_year %>%
      select(SITE, year, cw_bleach = prop_bleach_obs),
    by = c("SITE", "year")
  )

nrow(bleach_comparison)

##see how many NA's
na <- bleach_comparison %>%
  filter(if_any(everything(), is.na))

###filter out NA's. CW only has 35 NA's - will remove that survey for RCA too
###not applied below
bleach_comparison_filter <- bleach_comparison %>%
  filter(!is.na(cw_bleach))
nrow(bleach_comparison_filter)

#make long format
bleach_comparison_long <- bleach_comparison %>%
  pivot_longer(cols = c(rc_bleach, cw_bleach),
               names_to = "method", values_to = "bleach_prop") %>%
  mutate(method = recode(method,
                         rc_bleach = "RCA",
                         cw_bleach = "CW"))

#group RF and RS
bleach_comparison_long <- bleach_comparison_long %>%
  arrange(site_type, SITE) %>%
  mutate(SITE_label = factor(SITE_label, levels = unique(SITE_label)))

#line plot
p10 <- ggplot(bleach_comparison_long, aes(x = factor(year), y = bleach_prop, color = method)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_line(aes(group = method), linewidth = 1) +
  facet_wrap(~SITE_label) +
  scale_color_manual(values = c("tomato", "skyblue")) +
  labs(
    y = "Bleaching Proportion", x = "Year", color = "CS program"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
ggsave("RCA_CW_bleaching_comparison.jpeg", p10, width = 13, height = 9, dpi = 300)

#barplot
p11 <- ggplot(bleach_comparison_long, aes(x = factor(year), y = bleach_prop, fill = method)) +
  geom_col(position = "dodge") +
  facet_wrap(~SITE_label) +
  scale_fill_manual(values = c("tomato", "skyblue")) +
  labs(x = "Year", y = "Bleaching Proportion", fill = "CS program") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
ggsave("RCA_CW_bleaching_comparison_barplot.jpeg", p11, width = 13, height = 9, dpi = 300)


#scatterplot
ggplot(bleach_comparison, aes(x = cw_bleach, y = rc_bleach)) +
  geom_point(aes(color = SITE_label), size = 3, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "CW vs RCA Bleaching Proportion",
       x = "CW Bleaching (%)",
       y = "RCA Bleaching (%)",
       color = "Site") +
  theme_minimal()
#shows weak trend between methods


#spearman correlation
cor.test(bleach_comparison$cw_bleach, bleach_comparison$rc_bleach, method = "spearman")
#rho value shows no correlation between bleaching props BUT not statistically significant


####Contingency table
#Make thresholds - adjust as needed
bleach_comparison <- bleach_comparison %>%
  mutate(CW_bleach_detected = ifelse(cw_bleach > 0.2, "Yes", "No"),
         RCA_bleach_detected = ifelse(rc_bleach > 0.2, "Yes", "No"))
#table
bleach_table <- table(CW = bleach_comparison$CW_bleach_detected, RC = bleach_comparison$RCA_bleach_detected)
print(bleach_table)

#plot
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

##show which sites/years bleaching is detected
bleach_list <- bleach_comparison %>%
  select(SITE, year, CW_bleach_detected, RCA_bleach_detected) %>%
  filter(CW_bleach_detected == "Yes" |
         RCA_bleach_detected =="Yes")
####CW blanks are not showing in table above, look into fixing this. maybe change NA's to something else?  

