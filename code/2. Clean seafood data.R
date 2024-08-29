# TITLE: Clean seafood FL scores and join with mapping
# AUTHOR: Brooke Bell
# DATE: 8-28-24

rm(list = ls())

options(scipen=999)

library(tidyverse)
library(readxl)

# today's date
export_date <- "082824"

# import seafood scores
scores <- read_xlsx("data/fl_scores/fl_scores_final.xlsx",
                    sheet = "seafood_FL_risk") %>% 
  select(`Lasting ICS code upstream 0`, 
         `Ext. Rate`,
         `Food primary weight (ton)`,
         `Weighted risk (mrh-eq per ton) Producer (Reporter)`,
         `Weighted risk (mrh-eq per ton) Process (Reporter)`,
         `weighted risk feed (mrh-eq/ton) Producer (Reporter)`,
         `Weighted risk (mrh-eq per ton) AGG+Feed (Reporter)`, 
         Exclude, New_Category) %>% 
  filter(is.na(Exclude)) %>% 
  select(-Exclude) %>% 
  rename(LASTING_Name = `Lasting ICS code upstream 0`,
         Ext_Rate = `Ext. Rate`,
         Primary_Weight_Ton = `Food primary weight (ton)`,
         Weighted_Risk_Producer = `Weighted risk (mrh-eq per ton) Producer (Reporter)`,
         Weighted_Risk_Process = `Weighted risk (mrh-eq per ton) Process (Reporter)`,
         Weighted_Risk_Feed = `weighted risk feed (mrh-eq/ton) Producer (Reporter)`,
         Weighted_Risk_Total = `Weighted risk (mrh-eq per ton) AGG+Feed (Reporter)`)

# calculate weighted risk without feed
scores_ <- scores %>% 
  mutate(Weighted_Risk_Total_NOFEED = Weighted_Risk_Total - Weighted_Risk_Feed)
  
# calculate primary weight proportion
scores1 <- scores_ %>% 
  group_by(New_Category) %>% 
  mutate(Primary_Weight_Ton_Sum = sum(Primary_Weight_Ton)) %>% 
  relocate(Primary_Weight_Ton_Sum, .after = Primary_Weight_Ton) %>% 
  ungroup()

scores2 <- scores1 %>% 
  mutate(Primary_Weight_Prop = Primary_Weight_Ton / Primary_Weight_Ton_Sum) %>% 
  relocate(Primary_Weight_Prop, .after = Primary_Weight_Ton_Sum)

scores3 <- scores2 %>% 
  select(LASTING_Name, New_Category, Primary_Weight_Prop, Weighted_Risk_Total, Weighted_Risk_Total_NOFEED) %>% 
  arrange(New_Category)

# calculate weighted risk for averages
scores4 <- scores3 %>% 
  mutate(Risk_Total_subparts = Primary_Weight_Prop * Weighted_Risk_Total,
         Risk_Total_NOFEED_subparts = Primary_Weight_Prop * Weighted_Risk_Total_NOFEED)

wgtd_avg <- scores4 %>% 
  group_by(New_Category) %>% 
  summarise(Risk_Total_sum = sum(Risk_Total_subparts),
            Risk_Total_NOFEED_sum = sum(Risk_Total_NOFEED_subparts)) %>% 
  mutate(LASTING_Name = paste0(New_Category, ", average"),
         Final_Score_Yes = 1,
         Primary_Weight_Prop = 1) %>% 
  filter(New_Category != "Aquatic animals")

# join
scores5 <- full_join(scores4, wgtd_avg, by = c("New_Category",
                                               "LASTING_Name",
                                               "Primary_Weight_Prop",
                                               "Weighted_Risk_Total" = "Risk_Total_sum",
                                               "Weighted_Risk_Total_NOFEED" = "Risk_Total_NOFEED_sum")) %>% 
  arrange(New_Category, LASTING_Name)

# round to 3 digits
scores6 <- scores5 %>% 
  mutate(Weighted_Risk_Total = round(Weighted_Risk_Total),
         Weighted_Risk_Total_NOFEED = round(Weighted_Risk_Total_NOFEED),
         Primary_Weight_Prop = round(Primary_Weight_Prop, digits = 3)) %>% 
  select(LASTING_Name, New_Category, Primary_Weight_Prop, starts_with("Weighted_Risk_"))

# import new names
new_names <- read_xlsx("data/mappings/FNDDS_to_FLR_mapping_seafood_final.xlsx",
                      sheet = "Final scores")

# join
scores7 <- full_join(scores6, new_names, by = c("LASTING_Name" = "Final list of FL scores")) %>% 
  rename(Seafood_Label = `New Name`) %>% 
  relocate(Seafood_Label) %>% 
  filter(!(is.na(Seafood_Label)))

# import mixed dish scores
mixed <- read_xlsx("data/mappings/FNDDS_to_FLR_mapping_seafood_final.xlsx",
                       sheet = "Mixed dishes") %>% 
  select(FNDDS_description, FL_Score_Weighted)

mixed_NOFEED <- read_xlsx("data/mappings/FNDDS_to_FLR_mapping_seafood_final.xlsx",
                          sheet = "Mixed dishes NOFEED") %>% 
  select(FNDDS_description, FL_Score_Weighted_NOFEED)

# join
mixed_comb <- left_join(mixed, mixed_NOFEED, by = "FNDDS_description")

# join
scores8 <- left_join(scores7, mixed_comb, by = c("LASTING_Name" = "FNDDS_description"))

# fill in missing scores
scores9 <- scores8 %>% 
  mutate(Weighted_Risk_Total = case_when(is.na(Weighted_Risk_Total) & !(is.na(FL_Score_Weighted)) ~ FL_Score_Weighted,
                                         TRUE ~ Weighted_Risk_Total),
         Weighted_Risk_Total_NOFEED = case_when(is.na(Weighted_Risk_Total_NOFEED) & !(is.na(FL_Score_Weighted_NOFEED)) ~ FL_Score_Weighted_NOFEED,
                                                TRUE ~ Weighted_Risk_Total_NOFEED)) %>% 
  select(-c(LASTING_Name, New_Category, FL_Score_Weighted, FL_Score_Weighted_NOFEED))

# calculate score for unspecified fish
unspecified <- read_xlsx("data/mappings/FNDDS_to_FLR_mapping_seafood_final.xlsx",
                   sheet = "Unspecified fish") %>% 
  select(Species, Weight, Category)

unspecified1 <- 
  left_join(unspecified, scores9, by = c("Category" = "Seafood_Label")) %>% 
  select(Species, Weight, Category, Weighted_Risk_Total, Weighted_Risk_Total_NOFEED)

my_sum <- sum(unspecified$Weight)

unspecified2 <- unspecified1 %>% 
  rowwise() %>% 
  mutate(Weight_prop = Weight / my_sum,
         Score_weighted = Weight_prop * Weighted_Risk_Total,
         Score_weighted_NOFEED = Weight_prop * Weighted_Risk_Total_NOFEED) 

us_score <- sum(unspecified2$Score_weighted)
us_score_NOFEED <- sum(unspecified2$Score_weighted_NOFEED)

# round to 2 digits
scores10 <- scores9 %>% 
  mutate(Weighted_Risk_Total = ifelse(Seafood_Label == "Unspecified fish", round(us_score, digits = 2), Weighted_Risk_Total),
         Weighted_Risk_Total_NOFEED = ifelse(Seafood_Label == "Unspecified fish", round(us_score_NOFEED, digits = 2), Weighted_Risk_Total_NOFEED)) %>% 
  select(-Primary_Weight_Prop)

# import fndds-flr seafood mapping
my_map <- read_xlsx("data/mappings/FNDDS_to_FLR_mapping_seafood_final.xlsx",
                   sheet = "Mapping - seafood dishes")

# update mapping
my_map1 <- my_map %>% 
  left_join(scores10, by = c("FL Score" = "Seafood_Label")) %>% 
  arrange(Species, `FL Score`, `FNDDS description`) %>% 
  relocate(Notes, .after = last_col())

# export scores
write_csv(scores10, 
          paste0("data/temp_output/Seafood_scores_", export_date, ".csv"), 
          na = "")

# export mapping
write_csv(my_map1, 
          paste0("data/temp_output/Seafood_mapping_merged_", export_date, ".csv"), 
          na = "")

# create final dataset

# join
my_map2 <- my_map1 %>%  
  rename(FNDDS_Code = `FNDDS code`,
         FNDDS_Desc = `FNDDS description`,
         FL_Score = `FL Score`,
         FL_Score_Value_chr = Weighted_Risk_Total,
         FL_Score_Value_NOFEED = Weighted_Risk_Total_NOFEED) %>% 
  mutate(FL_Score_Value = as.numeric(FL_Score_Value_chr))

# change mrh/ton to mrh/gram
# 1 ton = 1,000,000 grams
my_map3 <- my_map2 %>% 
  mutate(FL_Score_Value_grams = FL_Score_Value / 1000000,
         FL_Score_Value_NOFEED_grams = FL_Score_Value_NOFEED / 1000000) %>% 
  select(FNDDS_Code, FNDDS_Desc, FL_Score, starts_with("FL_Score_Value"))

# export FL scores
write_csv(my_map3, paste0("data/temp_output/seafood_FL_scores_FNDDS_", export_date, ".csv"))

