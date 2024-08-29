# TITLE: Identify min/max values for sensitivity analysis
# AUTHOR: Brooke Bell
# DATE: 8-28-24

rm(list = ls())

options(scipen=999)

library(tidyverse)
library(readxl)

export_date <- "082824"

# MIN/MAX VALUES -----

# import flr scores (land)
fl <- read_csv(paste0("data/temp_output/FL_scores_FCID_", export_date, ".csv"))

# remove 'dried' items
fl1 <- fl %>% filter(!grepl("dried|paste", FCID_Desc))

# identify min and max FL scores
# and calculate rank
fl_sens <- fl1 %>%
  filter(Weight_Conversion > 0 & FL_Score_Value > 0) %>% 
  mutate(FL_Score_Value_grams_converted = FL_Score_Value_grams * Weight_Conversion*100) %>% 
  group_by(Foodgroup) %>%
  filter(FL_Score_Value_grams_converted == min(FL_Score_Value_grams_converted) | FL_Score_Value_grams_converted == max(FL_Score_Value_grams_converted)) %>%
  mutate(Rank = ifelse(FL_Score_Value_grams_converted == min(FL_Score_Value_grams_converted), "Min", "Max")) %>% 
  ungroup() %>% 
  arrange(Foodgroup, desc(Rank)) %>% 
  select(FCID_Code, FCID_Desc, Foodgroup, FL_Score, FL_Score_Value_grams_converted, Rank)

# import flr scores (seafood)
sea_scores <- read_csv(paste0("data/temp_output/seafood_FL_scores_FNDDS_", export_date, ".csv"))

# remove 'dried' items
sea_scores1 <- sea_scores %>% filter(!grepl("dried", FNDDS_Desc))

# identify min and max FL scores
# and calculate rank
sea_sens <- sea_scores1 %>% 
  filter(FL_Score_Value > 0) %>% 
  mutate(FL_Score_Value_grams_converted = FL_Score_Value_grams*100) %>% 
  filter(FL_Score_Value_grams_converted == min(FL_Score_Value_grams_converted) | FL_Score_Value_grams_converted == max(FL_Score_Value_grams_converted)) %>% 
  mutate(Rank = ifelse(FL_Score_Value_grams_converted == min(FL_Score_Value_grams_converted), "Min", "Max")) %>% 
  arrange(desc(Rank)) %>% 
  select(FNDDS_Code, FNDDS_Desc, FL_Score, FL_Score_Value_grams_converted, Rank)

# export
write_csv(fl_sens, paste0("data/temp_output/sensitivity_values_", export_date, ".csv"))
write_csv(sea_sens, paste0("data/temp_output/sensitivity_values_seafood_", export_date, ".csv"))

# MERGE WITH WEIGHTED MEAN FL IMPACT FACTORS -----

# create distinct min/max values
fl_sens1 <- fl_sens %>% 
  select(Foodgroup, FL_Score_Value_grams_converted, Rank) %>% 
  distinct()

fl_min <- fl_sens1 %>% 
  filter(Rank == "Min") %>% 
  select(-Rank) %>% 
  rename(FLper100gram_min = FL_Score_Value_grams_converted)

fl_max <- fl_sens1 %>% 
  filter(Rank == "Max") %>% 
  select(-Rank) %>% 
  rename(FLper100gram_max = FL_Score_Value_grams_converted)

sea_sens1 <- sea_sens %>% 
  select(FL_Score, FL_Score_Value_grams_converted, Rank) %>% 
  distinct() %>% 
  mutate(Foodgroup = "pf_seafood")

sea_min <- sea_sens1 %>% 
  filter(Rank == "Min") %>% 
  rename(FLper100gram_min = FL_Score_Value_grams_converted) %>% 
  select(Foodgroup, FLper100gram_min)

sea_max <- sea_sens1 %>% 
  filter(Rank == "Max") %>% 
  rename(FLper100gram_max = FL_Score_Value_grams_converted) %>% 
  select(Foodgroup, FLper100gram_max)

# now merge
min_dat <- rbind(fl_min, sea_min) %>% rename(foodgroup = Foodgroup)
max_dat <- rbind(fl_max, sea_max) %>% rename(foodgroup = Foodgroup)

sens_dat <- left_join(min_dat, max_dat, by = "foodgroup")

# change diet factor names
sens_dat1 <- sens_dat %>% 
  mutate(foodgroup = ifelse(foodgroup == "dairy", "dairy_tot", foodgroup),
         foodgroup = ifelse(foodgroup == "pf_redm", "pf_redm_tot", foodgroup),
         foodgroup = ifelse(foodgroup == "pf_poultry", "pf_poultry_tot", foodgroup))

# import FLR impact factors
impacts <- read_csv(paste0("data/temp_output/FL_impact_factors_", export_date, ".csv")) %>% 
  select(foodgroup, FLper100gram, FLper100gram_nofeed) %>% 
  arrange(foodgroup)

# merge all
all_dat <- left_join(impacts, sens_dat1, by = "foodgroup")

# check
all_dat[,2:5]*1000 #everything looks good

# check
all_dat %>% mutate(check_min = ifelse(FLper100gram_min > FLper100gram, 1, 0),
                   check_max = ifelse(FLper100gram_max < FLper100gram, 1, 0)) %>% 
  filter(check_min == 1 | check_max == 1) #good

# export
write_csv(all_dat, paste0("data/temp_output/FL_impact_factors_sensitivity_", export_date, ".csv"))



         