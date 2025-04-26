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

#make long format
bleach_comparison_long <- bleach_comparison %>%
  pivot_longer(cols = c(rc_bleach, cw_bleach),
               names_to = "source", values_to = "bleach_prop") %>%
  mutate(source = recode(source,
                         rc_bleach = "RCA",
                         cw_bleach = "CW"))

#group RF and RS
bleach_comparison_long <- bleach_comparison_long %>%
  arrange(site_type, SITE) %>%
  mutate(SITE_label = factor(SITE_label, levels = unique(SITE_label)))

#line plot
p10 <- ggplot(bleach_comparison_long, aes(x = factor(year), y = bleach_prop, color = source)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_line(aes(group = source), linewidth = 1) +
  facet_wrap(~SITE_label) +
  scale_color_manual(values = c("tomato", "skyblue")) +
  labs(
    y = "Bleaching Proportion", x = "Year", color = "CS program"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
ggsave("RCA_CW_bleaching_comparison.jpeg", p10, width = 13, height = 9, dpi = 300)

#barplot
p11 <- ggplot(bleach_comparison_long, aes(x = factor(year), y = bleach_prop, fill = source)) +
  geom_col(position = "dodge") +
  facet_wrap(~SITE_label) +
  scale_fill_manual(values = c("tomato", "skyblue")) +
  labs(x = "Year", y = "Bleaching Proportion", fill = "CS program") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
ggsave("RCA_CW_bleaching_comparison_barplot.jpeg", p11, width = 13, height = 9, dpi = 300)

