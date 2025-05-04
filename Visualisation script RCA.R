#####RCA visualisations and statistics###
library(ggplot2)
library(tidyverse)

##########Reef Check Australia###########################
RC_SubPropSY <- read_csv(file = "RC_Substrate_Proportions.csv")
RC_substrate_long_grouped <- read_csv(file = "RC_substrate_long_grouped.csv")

#Custom colors for substrate groups
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

#####################################################

###barplot with substrate proportions
p6 <- ggplot(RC_substrate_long_grouped, aes(x = factor(year), y = Proportion, fill = substrate_group)) +
  geom_col() +
  facet_wrap(~ SITE_label) +
  scale_fill_manual(values = substrate_colours) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y = "Proportion",
       x = "Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(path = "figs", filename = "RCA_substr_prop_site.jpeg", p6, width = 11, height = 7, dpi = 300)

###example barplot with HC vs HCB proportions only
p7 <- RC_substrate_long_grouped %>%
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

ggsave(path = "figs", filename = "RCA_HC_prop_site.jpeg", p7, width = 11, height = 7, dpi = 300)

##############################################################

###line plot to show trend of substrate proportions over year? need to clean if used
ggplot(RC_substrate_long_grouped, aes(x = factor(year), y = Proportion, color = substrate_group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.5) +
  facet_wrap(~ SITE_label, scales = "free_y") +
  scale_color_manual(values = substrate_colours) +
  labs(x = "Year", y = "Proportion", color = "Substrate Group"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


####line plot showing proportion of HCB per site and year
distinct_colours <- c("blue", "darkgreen", "gold", "mediumpurple", "pink1", "orangered2", "orange", "snow4",
                      "sienna", "olivedrab2", "khaki", "turquoise", "maroon")

p8 <- RC_substrate_long_grouped %>%
  filter(substrate_group == "HCB") %>%
  ggplot(aes(x = factor(year), y = Proportion, color = SITE_label, group = SITE)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(x = "Year", y = "Proportion", color = "Site"
  ) +
  scale_color_manual(values = distinct_colours) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    panel.spacing = unit(1, "lines")
  )

ggsave(path = "figs", filename = "RCA_HCBonly_prop_site_year.jpeg", p8, width = 11, height = 7, dpi = 300)

#####################################################
###plot barplot of substrate proportions by site as individual plots and merge
library(cowplot)
library(patchwork)

##Get unique sites
site_list <- unique(RC_substrate_long_grouped$SITE_label)

##create labels
labels <- site_list

##Loop over each group and make a separate plot
plots <- map2(site_list,labels, function(site, label) {
  p <- RC_substrate_long_grouped %>%
    filter(SITE_label == site) %>%
    ggplot(aes(x = factor(year), y = Proportion, fill = substrate_group)) +
    geom_col() +
    scale_fill_manual(values = substrate_colours, name = "Substrate") +
    #scale_fill_brewer(palette = "RdYlGn", name = "Substrate") + 
    labs(title = label, x = NULL, y = NULL) +
    theme_minimal(base_size = 10) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      strip.text = element_text(face = "bold"),
      panel.spacing = unit(1.5, "lines"),
      legend.position = "none",
      plot.margin = margin(10, 10, 10, 10)  # Add space around the plots
    )
  
  return(p)
})

##see plot before merging
print(plots[[2]])

##Combine all plots using patchwork
combined_patchwork <- wrap_plots(plots, ncol = 3, guides = "collect") +
  plot_annotation() & 
  theme(
    legend.position = "right",
    legend.direction = "vertical",
    axis.title.x = element_blank(),  # Remove individual axis labels
    axis.title.y = element_blank()
  )

##create blank label plot to get axis names
blanklabelplot<-ggplot()+labs(y="Proportion", x = "Year") + theme_classic()+ 
  guides(x = "none", y = "none")

##merge plots - need to fix. X axis still not working properly
finalplot <- (blanklabelplot|combined_patchwork) + 
  plot_layout(widths = c(1,1000), heights = c(1,0.1))

 print(finalplot)

ggsave(path = "figs", filename = "RCSubstProps.jpg", plot = finalplot, width = 14, height = 20, dpi = 300)

#####################################################
###try above but with HC vs HCB only

#filter data to make loop easier
filtered <- RC_substrate_long_grouped %>%
  filter(substrate_group %in% c("HC", "HCB"))

##Loop over each group and make a separate plot
plotsfilter <- map2(site_list,labels, function(site, label) {
  pfilt <- filtered %>%
    filter(SITE_label == site) %>%
    ggplot(aes(x = factor(year), y = Proportion, fill = substrate_group)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = substrate_colours_2, name = "Substrate") +
    #scale_fill_brewer(palette = "RdYlGn", name = "Substrate") + 
    labs(title = label, x = NULL, y = NULL) +
    theme_minimal(base_size = 10) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      strip.text = element_text(face = "bold"),
      panel.spacing = unit(1.5, "lines"),
      legend.position = "none",
      plot.margin = margin(10, 10, 10, 10)  # Add space around the plots
    )
  
  return(pfilt)
})
print(plotsfilter[[2]])

##Combine all plots using patchwork
combined_patchwork_filt <- wrap_plots(plotsfilter, ncol = 3, guides = "collect") +
  plot_annotation() & 
  theme(
    legend.position = "right",
    legend.direction = "vertical",
    axis.title.x = element_blank(),  # Remove individual axis labels
    axis.title.y = element_blank()
  )

##create blank label plot to get axis names
blanklabelplot<-ggplot()+labs(y="Proportion", x = "Year") + theme_classic()+ 
  guides(x = "none", y = "none")

##merge plots - need to fix. X axis still not working properly
finalplotfilter <- (blanklabelplot|combined_patchwork_filt) + 
  plot_layout(widths = c(1,1000), heights = c(1,0.1))

print(finalplotfilter)

ggsave(path = "figs", filename = "RC_HCvHCBProps.jpg", plot = finalplotfilter, width = 14, height = 20, dpi = 300)


###############################################################################
##############SUMMARY TABLES

#mean and stdev of substrate proportions by site. any use???
substrate_summary <- RC_substrate_long_grouped %>%
  group_by(SITE_label, substrate_group) %>%
  summarise(
    mean_prop = mean(Proportion),
    sd_prop = sd(Proportion),
    .groups = "drop"
  )

#HC and HCB prop over sites and years to see increase/decrease
HCtrend <- RC_substrate_long_grouped %>%
  filter(substrate_group %in% c("HC", "HCB")) %>%
  group_by(SITE_label, year, substrate_group) %>%
  summarise(total = sum(Proportion), .groups = "drop")

###see if HC and HCB prop changes respectively i.e. is there a relationship between them
# Reshape to wide format so HC and HCB are in their own columns
HCtrend_wide <- RC_substrate_long_grouped %>%
  filter(substrate_group %in% c("HC", "HCB")) %>%
  pivot_wider(names_from = substrate_group, values_from = Proportion)

#look at proporton of HC and HCB trend over time
p9 <- ggplot(HCtrend_wide, aes(x = year)) +
  geom_line(aes(y = HC, color = "Healthy")) +
  geom_line(aes(y = HCB, color = "Bleached")) +
  facet_wrap(~ SITE_label) +
  scale_color_manual(values = c("Healthy" = "darkorange", "Bleached" = "orangered")) +
  labs(y = "Proportion Cover", color = "Hard Coral") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2013, 2024, by = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
ggsave(path = "figs", filename = "RCA_HCvHCB_site_year.jpeg", p9, width = 12, height = 9, dpi = 300)


##############looking at relationship between HC and HCB - only for showing trends as data is not independent
####not sure if this is somethign valuable to look at??????????
ggplot(HCtrend_wide, aes(x = HCB, y = HC)) +
  geom_point() +
  geom_smooth(method = "loess") + #can change to "lm" if want a straight line and se = FALSE if dont want CI. 
  facet_wrap(~ year)

ggplot(HCtrend_wide, aes(x = HCB, y = HC)) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_wrap(~ SITE)


##########################################################################################
###################TIME SERIES LINEAR MODEL to look at if substrate cover has changed over time

###########DONT THINK LME4/LMER model fits assumptions (normality and linearity) so might not be accurate???
library(lme4)
library(lmerTest)

# Fit mixed model for one substrate group (HC and HCB)
hard_coral <- RC_substrate_long_grouped %>%
  filter(substrate_group == "HC")

m1 <- lmer(Proportion ~ year + (1 | SITE), data = hard_coral)
summary(m1)

hard_coral_b <- RC_substrate_long_grouped %>%
  filter(substrate_group == "HCB")

m2 <- lmer(Proportion ~ year + (1 | SITE), data = hard_coral_b)
summary(m2)

#check model performance
library(performance)
check_model(m2) 

#re run the model with dummmy for harrys bommie 2024
hard_coral_b <- hard_coral_b %>%
  mutate(bleach_event = if_else(SITE == "Harry's Bommie" & year == 2024, 1, 0))
#model runs better 
m3 <- lmer(Proportion ~ year + bleach_event + (1 | SITE), data = hard_coral_b)
summary(m3)

########################################
#this is better for assumptions

library(glmmTMB)
# Make sure 0s and 1s are slightly nudged
hard_coral_b <- hard_coral_b %>%
  mutate(Proportion_adj = ifelse(Proportion == 0, 0.0001,
                                 ifelse(Proportion == 1, 0.9999, Proportion)))

#fit glmmTMB model(removed site as random factor as it ruined the model - little site variation)
m_beta_simple <- glmmTMB(Proportion_adj ~ year + bleach_event,
                         data = hard_coral_b,
                         family = beta_family(link = "logit"))
summary(m_beta_simple)
#significant negative effect of year - bleaching decreases. Very strong positive effect for Harry's Bommie 2024

#get predicted values
hard_coral_b$predicted <- predict(m_beta_simple, type = "response")

#plot bleaching data with predicted lines
ggplot(hard_coral_b, aes(x = year, y = Proportion_adj, color = SITE)) +
  geom_point(alpha = 0.4) +
  geom_line(aes(y = predicted), color = "black", linewidth = 1.2) +
  labs(y = "Adjusted Bleached Proportion",
    x = "Year"
  ) +
  theme_minimal()

#plot above but by site, all sites are stacked on to each other.. not really a good plot
ggplot(hard_coral_b, aes(x = year, y = predicted, color = SITE)) +
  geom_line(size = 1) +
  geom_point(aes(y = Proportion_adj), alpha = 0.5) +  # add actual observed values
  labs(x = "Year",
    y = "Predicted Proportion Bleached",
    color = "Site"
  ) +
  theme_minimal()

########################################################################
#######################################################################
#calculate prop bleached (HCB) to match CW props
RC_bleach <- RC_substrate_long_grouped %>%
  filter(substrate_group %in% c("HC", "HCB")) %>%
  group_by(SITE_label, year) %>%
  mutate(total_coral = sum(Proportion)) %>%
  filter(substrate_group == "HCB") %>%
  mutate(prop_bleached = Proportion / total_coral) %>%
  ungroup() %>%
  arrange(site_type, SITE) %>%
  mutate(SITE_label = factor(SITE_label, levels = unique(SITE_label)))

write_csv(RC_bleach, "RC_bleach.csv")


###Plot prop bleached - proportion of bleached corals/total coral (not all substrate)
p12 <- ggplot(RC_bleach, aes(x = year, y = prop_bleached)) +
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
ggsave(path = "figs", filename = "RCA_bleaching_proportion_by_site.jpeg", p12, width = 12, height = 7, dpi = 300)


#######as above but retain HC proportions
RC_HC_bleach<- RC_substrate_long_grouped %>%
  filter(substrate_group %in% c("HC", "HCB")) %>%
  group_by(SITE_label, year) %>%
  mutate(total_coral = sum(Proportion)) %>%
  mutate(prop_bleached = Proportion / total_coral) %>%
  ungroup() %>%
  arrange(site_type, SITE) %>%
  mutate(SITE_label = factor(SITE_label, levels = unique(SITE_label)))

###barplot of HC proportions (excluding all other substrate proportions)
RC_HC_bleach %>%
  ggplot(aes(x = factor(year), y = prop_bleached, fill = substrate_group)) +
  geom_col(position = "stack") +
  facet_wrap(~ SITE_label) +
  scale_fill_manual(values = substrate_colours_2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y = "Proportion",
       x = "Year")  +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


####same as p9 but different style with point
ggplot(RC_HC_bleach, aes(x = year, y = Proportion, color = substrate_group)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ SITE_label) +
  labs(x = "Year", y = "Proportion", color = "Substrate Type") +
  scale_color_manual(
    values = c("HC" = "darkorange",   # blue for healthy coral
               "HCB" = "orangered")  # red for bleached coral
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2013, 2024, by = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

