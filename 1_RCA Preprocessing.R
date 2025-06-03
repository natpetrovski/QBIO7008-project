###Cleaning and pre-processing Reef Check Australia data
#author: Natalie Petrovski
#date: May 2025

##Load packages
library(tidyverse)
library(here)

##############################################################################
#STEP 1: Clean and structure dataset. Filter and retain variables need
##############################################################################

##read in dataset
RC_S <- read_csv(here("RC_Substrate_cleaned.csv"))

#look at data
str(RC_S)
glimpse(RC_S)
names(RC_S)

##convert date to date format
RC_Sb <- RC_S %>%
  mutate(DATE = dmy(DATE))

##remove columns not needed for analysis
#check first
names(RC_S)

RC_Sb <- RC_Sb %>%
  select(-datetime, -REEF, -RESEARCH_SITE)

##create a new column for year
RC_Sb <- RC_Sb %>%
  mutate(year = year(DATE)) %>%
  relocate(year, .before=1 )
#check
unique(RC_Sb$year)

##filter years used in analysis
RC_Sb <- RC_Sb %>%
  filter(year(DATE) >= 2013)
unique(RC_Sb$year)

##check months - should be only spring months
RC_Sb %>%
  mutate(month = month(DATE)) %>%
  distinct(month)

#inspect sites
unique(RC_Sb$SITE)

##remove any sites not used in analysis
RC_Sb <- RC_Sb %>%
  filter(!SITE %in% c("Blue Pools", "Gorgonian Hole", "Pancakes", "Coral Gardens", "Stevos Carbonara"))

#change site names to match CW site names
RC_Sb <- RC_Sb %>%
  mutate(SITE = case_when(
    SITE == "Half Way (Doug's Place)" ~ "Half Way",
    SITE == "Cappuccino Express" ~ "Cappuccino",
    TRUE ~ SITE))
#check
RC_Sb %>%
  count(SITE)

##see if any NA's
RC_Sb %>%
  filter(if_any(everything(), is.na))

##set site as factor
RC_Sb <- RC_Sb %>%
  mutate(SITE = as.factor(SITE))

# ##add reef flat and slope to sites
# RC_Sb <- RC_Sb %>%
#   mutate(site_type = if_else(
#     SITE %in% c("Cappuccino", "Jetty Flat", "Last Resort", "Research Zone", "Shark Bay", "White Wedding"), 
#     "RF", "RS")) %>%
#   relocate(site_type, .after = SITE)

##add north/south reef flat and slope to sites
RC_Sb <- RC_Sb %>%
  mutate(site_type = case_when(
    SITE %in% c("Cappuccino", "Last Resort", "White Wedding") ~ "NF", 
    SITE %in% c("Jetty Flat", "Research Zone", "Shark Bay") ~ "SF",
    SITE %in% c("Coral Cascade", "Coral Grotto", "Libby's Lair") ~ "NS",
    SITE %in% c("Harry's Bommie", "Heron Bommie", "Half Way", "Canyons") ~ "SS")) %>%
  relocate(site_type, .after = SITE)

##sort by site type for visualisations
RC_Sb <- RC_Sb %>%
  mutate(SITE_label = paste0(SITE, " (", site_type, ")")) %>%
  relocate(SITE_label, .after = site_type) %>%
  mutate(site_type = factor(site_type, levels = c("NF", "SF", "NS", "SS"))) %>%
  arrange(site_type, SITE) %>%
  mutate(SITE_label = factor(SITE_label, levels = unique(SITE_label)))

view(RC_Sb)
str(RC_Sb)

##################################################################################
#STEP 2: RESTRUCTURE DATA FORMATS
##################################################################################

##Identify all columns that start with "SUBSTRATE_"
substrate_cols <- RC_Sb %>%
  select(starts_with("SUBSTRATE_")) %>%
  colnames()

##Reshape to long format - all substrate types in rows
RCA_long <- RC_Sb %>%
  pivot_longer(
    cols = all_of(substrate_cols),
    names_to = "point",
    values_to = "substrate_code")

##Sort by site and date to get count/proportions of substrate for each survey
RC_substrate_prop <- RCA_long %>%
  group_by(SITE, DATE, year, site_type, SITE_label, substrate_code) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(SITE, year) %>%
  mutate(
    total_points = sum(count),
    proportion = count / total_points
  ) %>%
  ungroup()

###Reshape to wide format with substrate proportions - all substrates as their own column
#Sorted by year and site
RC_SubProp_wide <- RCA_long %>%
  group_by(SITE, year, site_type, SITE_label, substrate_code) %>%
  summarise(count = n(), .groups = "drop") %>% #counts all instances of substrate code, ungroups
  group_by(SITE, year, site_type, SITE_label) %>% #groups again by survey
  mutate(
    total_points = sum(count),
    proportion = round(count / total_points, 3)
  ) %>%
  select(SITE, year, site_type, SITE_label, substrate_code, proportion) %>% #keeps only columns we need 
  pivot_wider( #turns table wide format
    names_from = substrate_code,
    values_from = proportion,
    values_fill = 0 #fill instead of NA
  )

#sort by site type for visualisations
RC_SubProp_wide <- RC_SubProp_wide %>%
  mutate(site_type = factor(site_type, levels = c("NF", "SF", "NS", "SS"))) %>%
  arrange(site_type, SITE) %>%
  mutate(SITE_label = factor(SITE_label, levels = unique(SITE_label)))
#export as csv
write_csv(RC_SubProp_wide, file = here("output_data","RC_Substrate_Proportions.csv"))

############################################################
#Pivot back to long format, might be easier for ggplot, retains 0 prop values
RC_substrate_long <- RC_SubProp_wide %>%
  pivot_longer(
    cols = -c(SITE, year, site_type, SITE_label),
    names_to = "Substrate",
    values_to = "Proportion")

##################################################################################
#STEP 3: REFINE SUBSTRATE GROUPS 
##################################################################################

#check substrate types
unique(RC_substrate_long$Substrate)

###Group substrates
RC_substrate_long_grouped <- RC_substrate_long %>%
  mutate(substrate_group = case_when(
    Substrate %in% c("HC", "HCBR", "HCE", "HCP", "HCM", "HCF") ~ "HC",
    Substrate == "HCB" ~ "HCB",
    Substrate %in% c("RB", "RCCA", "RCTA", "SD", "RC", "SI") ~ "NL",
    Substrate %in% c("RKCTA", "RKC", "RKCNIA") ~ "RKC",
    Substrate %in% c("SC", "SCL", "SCZ", "SCB") ~ "SC",
    Substrate %in% c("SP", "SPE") ~ "SP",
    Substrate == "OT" ~ "OT",
    Substrate == "NIA" ~ "NIA",
    Substrate == "NR" ~ "NR",
    TRUE ~ "Unknown")) %>%
  group_by(SITE, site_type, SITE_label, year, substrate_group) %>%
  summarise(Proportion =sum(Proportion), .groups = "drop") ##make sure to run this part or the props dont make sense, need to sum them

##sort by site type
RC_substrate_long_grouped <- RC_substrate_long_grouped %>%
  mutate(site_type = factor(site_type, levels = c("NF", "SF", "NS", "SS"))) %>%
  arrange(site_type, SITE) %>%
  mutate(SITE_label = factor(SITE_label, levels = unique(SITE_label)))

##see if any codes were missed
RC_substrate_long_grouped %>%
  filter(substrate_group == "Unknown") %>%
  distinct(substrate_group)

##check counts of NR - assume it means not recorded?
numNR <- RC_substrate_long_grouped %>%
  filter(substrate_group == "NR")
unique(numNR$Proportion)

#export as csv
write.csv(RC_substrate_long_grouped, file = here("output_data", "RC_substrate_long_grouped.csv"), row.names = FALSE)

##################################################################################
#STEP 4: GET COUNT DATA INSTEAD OF PROPORTION DATA FOR MODELLING
#Use initial long format - RCAlong
##################################################################################

##substrate counts
RC_substrate_counts <- RCA_long %>%
  group_by(SITE, year, site_type, SITE_label, substrate_code) %>%
  summarise(count = n(), .groups = "drop")

##Group substrates again
RC_substrate_counts_grouped <- RC_substrate_counts %>%
  mutate(substrate_group = case_when(
    substrate_code %in% c("HC", "HCBR", "HCE", "HCP", "HCM", "HCF") ~ "HC",
    substrate_code == "HCB" ~ "HCB",
    substrate_code %in% c("RB", "RCCA", "RCTA", "SD", "RC", "SI") ~ "NL",
    substrate_code %in% c("RKCTA", "RKC", "RKCNIA") ~ "RKC",
    substrate_code %in% c("SC", "SCL", "SCZ", "SCB") ~ "SC",
    substrate_code %in% c("SP", "SPE") ~ "SP",
    substrate_code == "OT" ~ "OT",
    substrate_code == "NIA" ~ "NIA",
    substrate_code == "NR" ~ "NR",
    TRUE ~ "Unknown")) %>%
  group_by(SITE, site_type, SITE_label, year, substrate_group) %>%
  summarise(count = sum(count), .groups = "drop")

##sort by site type
RC_substrate_counts_grouped <- RC_substrate_counts_grouped %>%
  mutate(site_type = factor(site_type, levels = c("NF", "SF", "NS", "SS"))) %>%
  arrange(site_type, SITE) %>%
  mutate(SITE_label = factor(SITE_label, levels = unique(SITE_label)))

str(RC_substrate_counts_grouped)

write.csv(RC_substrate_counts_grouped, file = here("output_data", "RC_substrate_counts_grouped.csv"), row.names = FALSE)

###filter substrate groups to hard coral types only for analysis
RC_HC_counts <- RC_substrate_counts_grouped %>%
  filter(substrate_group %in% c("HC", "HCB"))

##Pivot - all HC counts all bleach counts in their own column - to match CW data
##add new columns to calculate total coral and number bleached
RC_bleach_counts <- RC_HC_counts %>%
  pivot_wider(
    names_from = substrate_group,
    values_from = count,
    values_fill = 0) %>%
  mutate(total_coral = HC + HCB, n_bleach = HCB) 
#export as csv
write.csv(RC_bleach_counts, file = here("output_data", "RC_bleach_counts.csv"), row.names = FALSE)

##################################################################################
#STEP 5: SUMMARY TABLES OF SUBSTRATE TYPES
##################################################################################

##Calculate average substrate covers by site
mean_sub_props <- RC_substrate_long_grouped %>%
  group_by(SITE_label, substrate_group) %>%
  summarise(mean = mean(Proportion * 100, na.rm = TRUE),
            sd = sd(Proportion * 100, na.rm = TRUE),
            .groups = "drop")

##Calculate average healthy coral cover and min max by site
hc_summary <- RC_HC_bleach %>%
  group_by(SITE_label) %>%
  filter(substrate_group == "HC") %>%
  summarise(
    mean = mean(Proportion *100, na.rm = TRUE),
    sd = sd(Proportion *100, na.rm = TRUE),
    min = min(Proportion *100, na.rm = TRUE),
    max = max(Proportion *100, na.rm = TRUE),
    .groups = "drop")

##Calculate average healthy coral cover and min max by year
hc_year_summary <- RC_HC_bleach %>%
  group_by(year) %>%
  filter(substrate_group == "HC") %>%
  summarise(
    mean = mean(Proportion *100, na.rm = TRUE),
    sd = sd(Proportion *100, na.rm = TRUE),
    min = min(Proportion *100, na.rm = TRUE),
    max = max(Proportion *100, na.rm = TRUE),
    .groups = "drop")

##Calculate average bleached coral cover and min max
hcb_summary <- RC_HC_bleach %>%
  group_by(SITE_label) %>%
  filter(substrate_group == "HCB") %>%
  summarise(
    mean = mean(prop_bleached *100, na.rm = TRUE),
    sd = sd(prop_bleached *100, na.rm = TRUE),
    min = min(prop_bleached *100, na.rm = TRUE),
    max = max(prop_bleached *100, na.rm = TRUE),
    .groups = "drop")  

###Site level bleaching per year
hcb_site_year <- RC_HC_bleach %>%
  filter(substrate_group == "HCB") %>%
  group_by(SITE_label, year) %>%
  summarise(
    prop_bleach = mean(prop_bleached, na.rm = TRUE),
    .groups = "drop")

##Then aggregate sites to get average bleach over time (based on sites)
hcb_site_year_means <- hcb_site_year %>%
  group_by(year) %>%
  summarise(
    mean_bleach = mean(prop_bleach, na.rm = TRUE) *100,
    sd_bleach = sd(prop_bleach, na.rm = TRUE)*100)

##Get >20% bleaching events
hcb_detect <- RC_bleach %>%
  group_by(site_type, SITE_label, year) %>%
  reframe(percentage_bleached = prop_bleached *100) %>%
  mutate(RC_bleach_detected = ifelse(percentage_bleached >= 20, "Yes", "No"))

##Calculate mean bleaching proportions by year only
hcb_year_means <- RC_bleach_counts %>%
  group_by(year) %>%
  summarise(prop_bleach = sum(n_bleach) / sum(total_coral) *100,
            sd_bleach = sd(n_bleach) / sum(total_coral) *100)

##summary stats on data used for analyses - number of sites, surveys and observations
RC_summary <- RC_bleach_counts %>%
  group_by(year) %>%
  summarise(
    RC_Sites = n_distinct(SITE),
    RC_Surveys = n(),
    RC_Observations = sum(total_coral, na.rm = TRUE))
##export as csv
write_csv(RC_summary, file = here("output_data", "RC_summary.csv"))

#####merge CoralWatch and RCA summary tables if needed
##find summary table code for CoralWath in 1_CoralWatch Preprocessing script
merged_summary <- full_join(CW_s_summary, RC_summary, by = "year") %>%
  arrange(year)
#export as csv
write_csv(merged_data_summary, file = here("output_data", "merged_data_summary.csv"))

##################################################################################
#STEP 6: CALCULATE PROPORTION BLEACHING (HCB) TO MATCH CORALWATCH STRUCTURE
##################################################################################

####Calculate HCB proportions only
RC_bleach<- RC_substrate_long_grouped %>%
  filter(substrate_group %in% c("HC", "HCB")) %>%
  group_by(SITE_label, year) %>%
  mutate(total_coral = sum(Proportion)) %>%
  mutate(prop_bleached = Proportion / total_coral) %>%
  ungroup() %>%
  filter(substrate_group %in% c("HCB")) %>%
  mutate(site_type = factor(site_type, levels = c("NF", "SF", "NS", "SS"))) %>%
  arrange(site_type, SITE) %>%
  mutate(SITE_label = factor(SITE_label, levels = unique(SITE_label)))
#export as csv
write_csv(RC_bleach, file = here("output_data", "RC_bleach.csv"))

####Calculate HCB proportions retaining HC proportions for coral cover (total_coral)
RC_HC_bleach<- RC_substrate_long_grouped %>%
  filter(substrate_group %in% c("HC", "HCB")) %>%
  group_by(SITE_label, year) %>%
  mutate(total_coral = sum(Proportion)) %>%
  mutate(prop_bleached = if_else(substrate_group == "HCB", Proportion / total_coral, NA_real_)) %>%
  ungroup() %>%
  mutate(site_type = factor(site_type, levels = c("NF", "SF", "NS", "SS"))) %>%
  arrange(site_type, SITE) %>%
  mutate(SITE_label = factor(SITE_label, levels = unique(SITE_label)))
#export as csv
write_csv(RC_HC_bleach, file = here("output_data", "RC_HC_bleach.csv"))


#####################################################
#RESTRUCTURE TO BE ABLE TO MERGE WITH CORALWATCH DATA AND TO ENSURE LINE PLOTS DONT CREATE A LINE BETWEEN UNSAMPLED YEARS

###change format to wide for plotting
RC_HC_bleach_wide <- RC_HC_bleach %>%
  select(SITE, SITE_label, site_type, year, substrate_group, Proportion, total_coral) %>%
  pivot_wider(
    names_from = substrate_group,
    values_from = Proportion,
    values_fill = 0) %>%
  mutate(prop_bleached = HCB / total_coral)

##add dummy variables to have gaps in line for NA
dummy_years <- tibble(
  SITE = c("Coral Grotto", "Half Way", "Half Way", "Half Way"),
  site_type = c("NS", "SS", "SS", "SS"),
  SITE_label = c("Coral Grotto (NS)", "Half Way (SS)", "Half Way (SS)", "Half Way (SS)"),
  year = c(2019, 2013, 2018, 2019))

##add dummy rows to dataset
RC_HC_bleach_dummy <- bind_rows(RC_HC_bleach_wide, dummy_years)

##sort by site_label for plotting
RC_HC_bleach_dummy <- RC_HC_bleach_dummy %>%
  mutate(site_type = factor(site_type, levels = c("NF", "SF", "NS", "SS"))) %>%
  arrange(site_type, SITE_label) %>%
  mutate(SITE_label = factor(SITE_label, levels = unique(SITE_label)))
write_csv(RC_HC_bleach_dummy, file = here::here("output_data", "RC_HC_bleach_dummy.csv"))


