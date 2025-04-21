##Cleaning and pre-processing CoralWatch and RCA data script

library(tidyverse)

setwd("~/Desktop/QBIOL/QBIO7008-ResearchProject/WorkingData")

########CORAL WATCH CLEANING#########################
#####################################################
coral_watch <- read_csv("coral-watch-heron-cleaned.csv")

names(coral_watch)
head(coral_watch)
str(coral_watch)
summary(coral_watch)

#select columns
CWsub <- coral_watch %>%
  select("SITE", "DATE", "Activity", "Coral_No", "Colour_Code_Lightest", "Colour_Code_Darkest",
         "Average_Colour", "Coral_Type")

#change data to date format
CWsub <- CWsub %>%
  mutate(DATE = dmy(DATE))

str(CWsub)

#set categorical variables as factors
CWsub <- CWsub %>%
  mutate(SITE = as.factor(SITE),
         Activity = as.factor(Activity),
         Coral_Type = as.factor(Coral_Type))

levels(CWsub$Coral_Type)

##remove soft corals
CWsub <- CWsub %>%
  filter(Coral_Type != "Soft corals") %>%
  droplevels()

#create new columns for colour scores with just number
CWsub <- CWsub %>%
  mutate(
    Lightest_num = str_extract(`Colour_Code_Lightest`, "[1-6]") %>% as.numeric(),
    Darkest_num = str_extract(`Colour_Code_Darkest`, "[1-6]") %>% as.numeric()
  )

# #LIGHT AND DARK CODES??? factor?
# CWsub <- CWsub %>% mutate(Colour_Code_Lightest = as.factor(Colour_Code_Lightest),
#   Colour_Code_Darkest = as.factor(Colour_Code_Darkest))

unique(CWsub$SITE)
levels(CWsub$SITE)

##Remove sites
CWsub <- CWsub %>%
  filter(!SITE %in% c("Gorgonian Hole", "Gantry", "Pam's Point", "Plate Ledge", "North Beach", "North West")) %>%
  droplevels()

CWsub %>%
  count(SITE)

#select month range
CWsub <- CWsub %>%
  filter(month(DATE) >= 9 & month(DATE) <= 11)

#check it worked
CWsub %>%
  mutate(month = month(DATE)) %>%
  distinct(month)

#now filter years
CWsub <- CWsub %>%
  filter(year(DATE) >= 2013)

#create a column just for year
CWsub <- CWsub %>%
  mutate(year = year(DATE)) %>%
  relocate(year, .before=1 ) %>%
  mutate(year = as.factor(year))

unique(CWsub$year)

##see how many NA's
CWsub %>%
  filter(if_any(everything(), is.na))

#change format so 'light and 'dark' colours are in one column
CWsublong <- CWsub %>%
  pivot_longer(
    cols = c(Lightest_num, Darkest_num),
    names_to = "Type",
    values_to = "Colour_Code_num"
  )

##add bleaching column?????
CWsublong <- CWsublong %>%
  mutate(health = ifelse(Colour_Code_num <= 1, "bleached", "not bleached"))

#create survey ID
CWsublong<- CWsublong %>%
  unite(survey_id, SITE, DATE, sep = "_", remove = FALSE)

write_csv(CWsublong, "coralwatch_sub.csv")

#############REEF CHECK CLEANING#################################
#################################################################

RC_S <- read_csv("RC_Substrate_cleaned.csv")

str(RC_S)
glimpse(RC_S)

#convert date as date
RC_Sb <- RC_S %>%
  mutate(DATE = dmy(DATE))

names(RC_S)
#remove unwanted columns
RC_Sb <- RC_Sb %>%
  select(-datetime, -REEF, -RESEARCH_SITE)

#create a column just for year
RC_Sb <- RC_Sb %>%
  mutate(year = year(DATE)) %>%
  relocate(year, .before=1 ) %>%
  mutate(year = as.factor(year))

unique(RC_Sb$year)

#now filter years
RC_Sb <- RC_Sb %>%
  filter(year(DATE) >= 2013)

#check months
RC_Sb %>%
  mutate(month = month(DATE)) %>%
  distinct(month)

##Remove sites
unique(RC_Sb$SITE)

RC_Sb <- RC_Sb %>%
  filter(!SITE %in% c("Blue Pools", "Gorgonian Hole", "Pancakes", "Coral Gardens", "Stevos Carbonara"))

#change any dive site names to match CW
RC_Sb <- RC_Sb %>%
  mutate(SITE = case_when(
    SITE == "Half Way (Doug's Place)" ~ "Half Way",
    SITE == "Cappuccino Express" ~ "Cappuccino",
    TRUE ~ SITE
  ))

#check counts
RC_Sb %>%
  count(SITE)

##see how many NA's
RC_Sb %>%
  filter(if_any(everything(), is.na))

#set site as factor
RC_Sb <- RC_Sb %>%
  mutate(SITE = as.factor(SITE))

str(RC_Sb)

####################################################

#Pivot RCA data
# Identify all columns that start with "SUBSTRATE_"
substrate_cols <- RC_Sb %>%
  select(starts_with("SUBSTRATE_")) %>%
  colnames()

# Reshape
RCA_long <- RC_Sb %>%
  pivot_longer(
    cols = all_of(substrate_cols),
    names_to = "point",
    values_to = "substrate_code")


#Sorts by site and date to get count/proportions of substrate. Might be better for plotting, can select columns out if needed.
substrate_prop <- RCA_long %>%
  group_by(SITE, DATE, year, substrate_code) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(SITE, year) %>%
  mutate(
    total_points = sum(count),
    proportion = count / total_points
  ) %>%
  ungroup()

#Merge site and date rows, substr codes as columns
RC_SubPropSY <- RCA_long %>%
  group_by(SITE, year, substrate_code) %>%
  summarise(count = n(), .groups = "drop") %>% #counts all instances of substrate code, ungroups
  group_by(SITE, year) %>% #groups again by survey
  mutate(
    total_points = sum(count),
    proportion = round(count / total_points, 3)
  ) %>%
  select(SITE, year, substrate_code, proportion) %>% #keeps only columns we need 
  pivot_wider( #turns table wide format
    names_from = substrate_code,
    values_from = proportion,
    values_fill = 0 #fill instead of NA
  )

write_csv(RC_SubPropSY, "RC_Substrate_Proportions.csv")

############################################################
#Pivot back to long format, might be easier for ggplot, retains 0 prop values
substrate_long <- SubPropSY %>%
  pivot_longer(
    cols = -c(SITE, year),
    names_to = "Substrate",
    values_to = "Proportion"
  )

####refine codes to visualise easier

unique(substrate_long$Substrate)

substrate_long_grouped <- substrate_long %>%
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
  group_by(SITE, year, substrate_group) %>%
  summarise(Proportion =sum(Proportion), .groups = "drop") ##make sure to run this part or the props dont make sense, need to sum them

write.csv(RC_substrate_long_grouped, "RC_substrate_long_grouped.csv", row.names = FALSE)

#see if any codes were missed
substrate_long_grouped %>%
  filter(substrate_group == "Unknown") %>%
  distinct(substrate_group)

#counts of NR - assume it means not recorded????????
numNR <- substrate_long_grouped %>%
  filter(substrate_group == "NR")
unique(numNR$Proportion)

###############################################################
##read in RCA GPS data
library(foreign)
RCAsites <- read.dbf("ReefCheckAUS_HeronData.dbf")
