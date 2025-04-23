#####Comparsion visualisations and statistics###
library(ggplot2)
library(tidyverse)

RC_bleach <- read_csv(file = "RC_bleach.csv")
CW_bleach_detect_year <- read_csv(file = "CW_bleach_detect_year.csv")


##merge CW and RC data into one table
bleach_comparison <- RC_bleach %>%
  select(SITE, year, rc_bleach = prop_bleached) %>%
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

#line plot
ggplot(bleach_comparison_long, aes(x = factor(year), y = bleach_prop, color = source)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_line(aes(group = source), linewidth = 1) +
  facet_wrap(~SITE) +
  scale_color_manual(values = c("tomato", "skyblue")) +
  labs(
    title = "Comparison of Coral Bleaching Estimates (Reef Check vs Coral Watch)",
    y = "Bleaching Proportion", x = "Year", color = "CS program"
  ) +
  theme_minimal()

#barplot
ggplot(bleach_comparison_long, aes(x = factor(year), y = bleach_prop, fill = source)) +
  geom_col(position = "dodge") +
  facet_wrap(~SITE) +
  scale_fill_manual(values = c("tomato", "skyblue")) +
  labs(title = "Comparison of Bleaching Detection: RC vs CW",
       x = "Year", y = "Bleaching Level", fill = "Dataset") +
  theme_minimal()

