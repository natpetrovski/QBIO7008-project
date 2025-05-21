###Cleaning and pre-processing CoralWatch data
#author: Natalie Petrovski
#date: May 2025

#Packages you need
library(tidyverse)
library(here)

##############################################################################
#STEP 1: Clean and structure dataset. Filter and retain variables need
##############################################################################

##read in dataset
coral_watch <- read_csv(here("coral-watch-heron-cleaned.csv"))

#look at data
names(coral_watch)
head(coral_watch)
str(coral_watch)
summary(coral_watch)

##select columns to retain
CWsub <- coral_watch %>%
  select("SITE", "DATE", "Activity", "Coral_No", "Colour_Code_Lightest", "Colour_Code_Darkest",
         "Average_Colour", "Coral_Type")

##convert date to date format
CWsub <- CWsub %>%
  mutate(DATE = dmy(DATE))
#check
str(CWsub)

##set categorical variables as factors
CWsub <- CWsub %>%
  mutate(SITE = as.factor(SITE),
         Activity = as.factor(Activity),
         Coral_Type = as.factor(Coral_Type))
str(CWsub)

#check coral types
levels(CWsub$Coral_Type)

##remove soft corals from analysis
CWsub <- CWsub %>%
  filter(Coral_Type != "Soft corals") %>%
  droplevels()
levels(CWsub$Coral_Type)

##create new columns for colour scores with just number score
CWsub <- CWsub %>%
  mutate(
    Lightest_num = str_extract(`Colour_Code_Lightest`, "[1-6]") %>% as.numeric(),
    Darkest_num = str_extract(`Colour_Code_Darkest`, "[1-6]") %>% as.numeric())

#inspect sites
unique(CWsub$SITE)
levels(CWsub$SITE)

##remove sites not used for analysis
CWsub <- CWsub %>%
  filter(!SITE %in% c("Gorgonian Hole", "Gantry", "Pam's Point", "Plate Ledge", "North Beach", "North West")) %>%
  droplevels()
#check
CWsub %>%
  count(SITE)

##filter years
CWsub <- CWsub %>%
  filter(year(DATE) >= 2013)

#create a new column for year
CWsub <- CWsub %>%
  mutate(year = year(DATE)) %>%
  relocate(year, .before=1 ) %>%
  mutate(year = as.factor(year))
unique(CWsub$year)

##############################################################################
#STEP 2: Filter to retain months needed for analysis. We will be creating two options:
#Option 1: Spring months (Sept-Nov) to match RCA data.
#Option 2: Spring months + autumn 2024 - to include a recorded bleaching event for comparison
##############################################################################

####Option 1: spring months only
CWsub_s <- CWsub %>%
  filter(month(DATE) >= 9 & month(DATE) <= 11)
#check it worked
CWsub_s %>%
  mutate(month = month(DATE)) %>%
  distinct(month)
##skip to STEP 3 now for spring only output

#####Option 2: spring months + autumn 2024
CWsub_a <- CWsub %>%
  filter(
  (month(DATE) >= 9 & month(DATE) <= 11) |
  (year(DATE) == 2024 & month(DATE) %in% c(3:5)))
#check filtering worked
work_months <- CWsub_a %>%
  mutate(year = year(DATE),
         month_num = month(DATE),
         month = month(DATE, label = TRUE)) %>%
  distinct(year, month) %>%
  arrange(year, month) %>%
  select(year, month)
view(work_months)

###add new column to specify season
CWsub_a <- CWsub_a %>%
  mutate(season = case_when(
    month(DATE) %in% c(9, 10, 11) ~ "Spring",
    month(DATE) %in% c(3, 4, 5) ~ "Autumn",)) %>%
  relocate(season, .before = 4)
unique(CWsub_a$season)

###new year_season column
CWsub_a <- CWsub_a %>%
  mutate(year_season = paste0(year, "_", season)) %>%
  relocate(year_season, .before = 5)
unique(CWsub_a$year_season)

#######################################################################################################################
#STEP 3: Restructure data so 'light' and 'dark' observations become single observations, include survey ID and site type
#select code according to option 1 or 2 above (spring or spring + autumn)
#######################################################################################################################

##################################################################
####Option 1: if used spring only months
##pivot data
CWsublong_s <- CWsub_s %>%
  pivot_longer(
    cols = c(Lightest_num, Darkest_num),
    names_to = "Type",
    values_to = "Colour_Code_num") %>%
  mutate(health = ifelse(Colour_Code_num <= 1, "bleached", "not bleached"))

##create column for survey ID
CWsublong_s <- CWsublong_s %>%
  unite(survey_id, SITE, DATE, sep = "_", remove = FALSE)

##add site type column
# CWsublong_s <- CWsublong_s %>%
#   mutate(site_type = if_else(
#   SITE %in% c("Cappuccino", "Jetty Flat", "Last Resort", "Research Zone", "Shark Bay", "White Wedding"), 
#   "RF", "RS")) %>%
#   relocate(site_type, .after = SITE)

##site type but with north and south
CWsublong_s <- CWsublong_s %>%
  mutate(site_type = case_when(
    SITE %in% c("Cappuccino", "Last Resort", "White Wedding") ~ "NF", 
    SITE %in% c("Jetty Flat", "Research Zone", "Shark Bay") ~ "SF",
    SITE %in% c("Coral Cascade", "Coral Grotto", "Libby's Lair") ~ "NS",
    SITE %in% c("Harry's Bommie", "Heron Bommie", "Half Way", "Canyons") ~ "SS")) %>%
  relocate(site_type, .after = SITE)

##sort by site type to group for plotting
CWsublong_s <- CWsublong_s %>%
  mutate(SITE_label = paste0(SITE, " (", site_type, ")")) %>%
  relocate(SITE_label, .after = site_type) %>%
  arrange(site_type, SITE) %>%
  mutate(SITE_label = factor(SITE_label, levels = unique(SITE_label)))

##export csv
write_csv(CWsublong_s, file = here("output_data", "coralwatch_sub_s.csv"))

##################################################################
###Option 2: if used spring months + autumn 2024
##pivot data
CWsublong_a <- CWsub_a %>%
  pivot_longer(
    cols = c(Lightest_num, Darkest_num),
    names_to = "Type",
    values_to = "Colour_Code_num") %>%
  mutate(health = ifelse(Colour_Code_num <= 1, "bleached", "not bleached"))

##create column for survey ID
CWsublong_a <- CWsublong_a %>%
  unite(survey_id, SITE, DATE, sep = "_", remove = FALSE)

##add site type column
# CWsublong_a <- CWsublong_a %>%
#   mutate(site_type = if_else(
#   SITE %in% c("Cappuccino", "Jetty Flat", "Last Resort", "Research Zone", "Shark Bay", "White Wedding"), 
#   "RF", "RS")) %>%
#   relocate(site_type, .after = SITE)

##site type but with north and south
CWsublong_a <- CWsublong_a %>%
  mutate(site_type = case_when(
    SITE %in% c("Cappuccino", "Last Resort", "White Wedding") ~ "NF", 
    SITE %in% c("Jetty Flat", "Research Zone", "Shark Bay") ~ "SF",
    SITE %in% c("Coral Cascade", "Coral Grotto", "Libby's Lair") ~ "NS",
    SITE %in% c("Harry's Bommie", "Heron Bommie", "Half Way", "Canyons") ~ "SS")) %>%
  relocate(site_type, .after = SITE)

##Create site label site (type), then sort by site type to group for plotting
CWsublong_a <- CWsublong_a %>%
  mutate(SITE_label = paste0(SITE, " (", site_type, ")")) %>%
  relocate(SITE_label, .after = site_type) %>%
  mutate(site_type = factor(site_type, levels = c("NF", "SF", "NS", "SS"))) %>%
  arrange(site_type, SITE) %>%
  mutate(SITE_label = factor(SITE_label, levels = unique(SITE_label)))

##export csv
write_csv(CWsublong_a, file = here("output_data", "coralwatch_sub_a.csv"))

##############################################################################
#STEP 4: CALCULATE CORAL BLEACHING
#detect bleaching if count of '1' score is more than 20% of total corals per survey
#code blocks available for spring only and spring + autumn filtering
##############################################################################

#####################################
####Option 1: FOR SPRING ONLY months
##count and proportion of bleached corals FOR EACH SURVEY in new columns
CW_bleach_s <- CWsublong_s %>%
  group_by(survey_id, SITE, site_type, SITE_label, DATE, year) %>%
  summarise(
    observations = n(),
    n_bleach = sum(Colour_Code_num == 1),
    prop_bleach = n_bleach / observations
  ) %>% ungroup %>%
  mutate(bleached = prop_bleach >= 0.20)

##export csv
write_csv(CW_bleach_s, file = here("output_data", "CW_bleach_s.csv"))

###count and proportion of bleached corals but aggregating surveys by YEAR
CW_bleach_s_y <- CW_bleach_s %>%
  group_by(year, SITE, site_type, SITE_label) %>%
  reframe(
    n_surveys = n_distinct(survey_id, na.rm = TRUE),
    n_observations = sum(observations),
    n_bleached_obs = sum(n_bleach),
    prop_bleach_obs = n_bleached_obs / n_observations) %>%
  mutate(year = as.numeric(as.character(year)))

##Group RF and RS sites
CW_bleach_s_y <- CW_bleach_s_y %>%
  mutate(site_type = factor(site_type, levels = c("NF", "SF", "NS", "SS"))) %>%
  arrange(site_type, SITE) %>%
  mutate(SITE_label = factor(SITE_label, levels = unique(SITE_label)))

##export csv
write_csv(CW_bleach_s_y, file = here("output_data", "CW_bleach_s_y.csv"))

##################################################################
####Option 2: FOR spring + AUTUMN 2024
##count and proportion of bleached corals in new columns
CW_bleach_a <- CWsublong_a %>%
  group_by(survey_id, SITE, site_type, SITE_label, DATE, year, season, year_season) %>%
  summarise(
    observations = n(),
    n_bleach = sum(Colour_Code_num == 1),
    prop_bleach = n_bleach / observations) %>% 
  ungroup %>%
  mutate(bleached = prop_bleach >= 0.20)

##export csv
write_csv(CW_bleach_a, file = here("output_data", "CW_bleach_a.csv"))

##count and proportion of bleached corals but aggregating surveys by YEAR
CW_bleach_a_y <- CW_bleach_a %>%
  group_by(year_season, season, year, SITE, site_type, SITE_label) %>%
  reframe(
    n_surveys = n_distinct(survey_id, na.rm = TRUE),
    n_observations = sum(observations),
    n_bleached_obs = sum(n_bleach),
    prop_bleach_obs = n_bleached_obs / n_observations)

##Group RF and RS sites
CW_bleach_a_y <- CW_bleach_a_y %>%
  mutate(site_type = factor(site_type, levels = c("NF", "SF", "NS", "SS"))) %>%
  arrange(site_type, SITE) %>%
  mutate(SITE_label = factor(SITE_label, levels = unique(SITE_label)))

#export bleaching csv with autumn season
write_csv(CW_bleach_a_y, file = here::here("output_data", "CW_bleach_a_y.csv"))


#############################################################################
#STEP 5: SUMMARY TABLE
##############################################################################

##summary stats on data used for analyses - number of sites, surveys and observations

##Option 1: Spring only months
CW_s_summary <- CWsublong_s %>%
  group_by(year) %>%
  summarise(
    CW_Sites = n_distinct(SITE),
    CW_Surveys = n_distinct(survey_id),
    CW_Observations = n())

write_csv(CW_s_summary, file = here("output_data", "CW_s_summary.csv"))

##Option 2: Spring + Autumn 2024 months
CW_a_summary <- CWsublong_a %>%
  group_by(year) %>%
  summarise(
    CW_Sites = n_distinct(SITE),
    CW_Surveys = n_distinct(survey_id),
    CW_Observations = n())

write_csv(CW_a_summary, file = here("output_data", "CW_a_summary.csv"))

