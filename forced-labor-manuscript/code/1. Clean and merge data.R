# TITLE: Clean and merge data
# AUTHOR: Brooke Bell
# DATE: 8-28-24

rm(list = ls())

options(scipen=999)

library(tidyverse)
library(readxl)
library(survey)

# today's date
export_date <- "082824"

# import mappings
map_a <- read_xlsx("data/mappings/FCID_to_food_mapping_final.xlsx",
          sheet = "Mapping") %>% 
  select(FCID_Code, FCID_Desc, Foodgroup)

# mapping
map_b <- read_xlsx("data/mappings/FCID_to_FLR_mapping_land_final.xlsx",
          sheet = "Updated Mapping",
          skip = 1) %>% 
  select(`FCID Code`, `FCID Description`, `FLR Score (Food Item)`, `Weight Conversion Factor`) %>% 
  rename(FCID_Code = `FCID Code`,
         FCID_Desc = `FCID Description`,
         FL_Score = `FLR Score (Food Item)`,
         Weight_Conversion = `Weight Conversion Factor`) %>% 
  filter(!(is.na(FCID_Code)))

# scores (no feed)
scores_a <- read_xlsx("data/fl_scores/fl_scores_final.xlsx",
                    sheet = "Summary_tabular") %>% 
  rename(FL_Score_Item = `Item`,
         Country = `Partner Countries`,
         FL_Score_Value = `weighted risk (mrh per ton)`) %>% 
  select(FL_Score_Item, Country, FL_Score_Value) %>% 
  filter(Country == "Total")

# scores (with feed)
scores_b <- read_xlsx("data/fl_scores/fl_scores_final.xlsx",
                      sheet = "pivot_results_feed",
                      range = "C84:E126") %>% 
  select(Item, 
         `Sum of weighted risk (mrh per ton)`,
         `Sum of weighted risk (mrh per ton) (TOTAL including feed)`) %>% 
  rename(FL_Score_NoFeed = `Sum of weighted risk (mrh per ton)`,
         FL_Score_WithFeed = `Sum of weighted risk (mrh per ton) (TOTAL including feed)`) %>% 
  mutate(FL_Score_NoFeed = round(FL_Score_NoFeed, digits = 0),
         FL_Score_WithFeed = round(FL_Score_WithFeed, digits = 0))

# join maps
map_join <- left_join(map_a, map_b)

# get rid of babyfood and water
map_join1 <- map_join %>% filter(!(Foodgroup %in% c("babyfood", "water")))

# fix apple name
map_join2 <- map_join1 %>% mutate(FL_Score = ifelse(FL_Score == "Apple", "Apples", FL_Score))

# join scores
scores_join <- full_join(scores_a, scores_b, by = c("FL_Score_Item" = "Item")) %>% 
  mutate(FL_Score_Value_Final = ifelse(is.na(FL_Score_WithFeed), FL_Score_Value, FL_Score_WithFeed),
         FL_Score_Value_NOFEED = ifelse(is.na(FL_Score_NoFeed), FL_Score_Value, FL_Score_NoFeed)) %>%
  select(FL_Score_Item, Country, FL_Score_Value_Final, FL_Score_Value_NOFEED) %>% 
  rename(FL_Score_Value = FL_Score_Value_Final) %>% 
  mutate(FL_Score_Value = round(FL_Score_Value, digits = 0),
         FL_Score_Value_NOFEED = round(FL_Score_Value_NOFEED, digits = 0))

# need to manually fix turkey and rabbit (doesn't have feed score)
# use chicken as proxy for both
chicken_feed <- scores_b %>% 
  rowwise() %>% 
  mutate(feed_only = FL_Score_WithFeed - FL_Score_NoFeed) %>% 
  filter(Item == "Meat, chicken") %>% 
  select(feed_only) %>% 
  unlist() %>% 
  as.vector()

scores_join1 <- scores_join %>% 
  rowwise() %>% 
  mutate(FL_Score_Value = ifelse(FL_Score_Item == "Meat, turkey" | FL_Score_Item == "Meat, rabbit", FL_Score_Value + chicken_feed, FL_Score_Value)) %>% 
  arrange(FL_Score_Item)

# merge map and scores
map_total <- left_join(map_join2, scores_join1, by = c("FL_Score" = "FL_Score_Item")) %>% 
  select(-Country)

# which ones are missing
map_total %>% filter(is.na(FL_Score_Value)) %>% View()

# none
map_total1 <- map_total %>% mutate(FL_Score_Value = ifelse(FL_Score == "None", 0, FL_Score_Value))

# which ones are missing now
map_total1 %>% filter(is.na(FL_Score_Value)) %>% View()

# Manually calculate the averages

# first, create tropical fruit average
trop_frt <- scores_join1 %>% 
  filter(FL_Score_Item %in% c("Mangoes, mangosteens, guavas", "Pineapples", "Avocados", "Papayas")) %>% 
  group_by(Country) %>% 
  summarise(tropical_fruit = mean(FL_Score_Value)) %>% 
  select(tropical_fruit) %>% 
  unlist()

# then apply
map_total2 <- map_total1 %>% mutate(FL_Score_Value = ifelse(FL_Score == "AVERAGE(Mangoes, mangosteens, guavas; Pineapples; Avocados; Papayas)",
                                              trop_frt,
                                              FL_Score_Value))

# what's left?
map_total2 %>% filter(is.na(FL_Score_Value)) %>% View()

# create seed average
seed <- scores_join1 %>% 
  filter(FL_Score_Item %in% c("Sesame seed", "Poppy seed", "Sunflower seed", "Mustard seed")) %>% 
  group_by(Country) %>% 
  summarise(seed = mean(FL_Score_Value)) %>% 
  select(seed) %>% 
  unlist()

# then apply
map_total3 <- map_total2 %>% 
  mutate(FL_Score_Value = ifelse(FL_Score == "AVERAGE(Sesame seed; Poppy seed; Sunflower seed; Mustard seed)",
                                                            seed,
                                                            FL_Score_Value))

# what's left?
map_total3 %>% filter(is.na(FL_Score_Value)) %>% View()

# create grain average
grain <- scores_join1 %>% 
  filter(FL_Score_Item %in% c("Rice, milled", "Oats rolled", "Fonio", "Quinoa",
                              "Bulgur", "Barley, pearled", "Sorghum")) %>% 
  group_by(Country) %>% 
  summarise(grain = mean(FL_Score_Value)) %>% 
  select(grain) %>% 
  unlist()

# then apply
map_total4 <- map_total3 %>% 
  mutate(FL_Score_Value = ifelse(FL_Score == "AVERAGE(Rice, milled; Oats rolled; Fonio; Quinoa; Bulgur; Barley, pearled; Sorghum)",
                                 grain,
                                 FL_Score_Value))

# what's left?
map_total4 %>% filter(is.na(FL_Score_Value)) %>% View()

# create beans average
beans <- scores_join1 %>% 
  filter(FL_Score_Item %in% c("Beans, green", "String beans")) %>% 
  group_by(Country) %>% 
  summarise(beans = mean(FL_Score_Value)) %>% 
  select(beans) %>% 
  unlist()

# then apply
map_total5 <- map_total4 %>% 
  mutate(FL_Score_Value = ifelse(FL_Score == "AVERAGE(Beans, green; String beans)",
                                 beans,
                                 FL_Score_Value))

# what's left?
map_total5 %>% filter(is.na(FL_Score_Value)) %>% View()

# create roots average
roots <- scores_join1 %>% 
  filter(FL_Score_Item %in% c("Chicory roots", "Roots and tubers nes")) %>% 
  group_by(Country) %>% 
  summarise(roots = mean(FL_Score_Value)) %>% 
  select(roots) %>% 
  unlist()

# then apply
map_total6 <- map_total5 %>% 
  mutate(FL_Score_Value = ifelse(FL_Score == "AVERAGE(Chicory roots; Roots and tubers nes)",
                                 roots,
                                 FL_Score_Value))

# what's left?
map_total6 %>% filter(is.na(FL_Score_Value)) %>% View()

# create flour average
flour <- scores_join1 %>% 
  filter(FL_Score_Item %in% c("Flour, wheat", "Flour, maize", "Flour, buckwheat",
                              "Flour, rye", "Flour, cereals")) %>% 
  group_by(Country) %>% 
  summarise(flour = mean(FL_Score_Value)) %>% 
  select(flour) %>% 
  unlist()

# then apply
map_total7 <- map_total6 %>% 
  mutate(FL_Score_Value = ifelse(FL_Score == "AVERAGE(Flour, wheat; Flour, maize; Flour, buckwheat; Flour, rye; Flour, cereals)",
                                 flour,
                                 FL_Score_Value))

# what's left?
map_total7 %>% filter(is.na(FL_Score_Value)) %>% View()

# create poultry average
poultry <- scores_join1 %>% 
  filter(FL_Score_Item %in% c("Meat, chicken", "Meat, turkey")) %>% 
  group_by(Country) %>% 
  summarise(poultry = mean(FL_Score_Value)) %>% 
  select(poultry) %>% 
  unlist()

poultry_nofeed <- scores_join1 %>% 
  filter(FL_Score_Item %in% c("Meat, chicken", "Meat, turkey")) %>% 
  group_by(Country) %>% 
  summarise(poultry = mean(FL_Score_Value_NOFEED)) %>% 
  select(poultry) %>% 
  unlist()

# then apply

# feed
map_total8 <- map_total7 %>% 
  mutate(FL_Score_Value = ifelse(FL_Score == "AVERAGE(Meat, chicken; Meat, turkey)",
                                 poultry,
                                 FL_Score_Value))

# no feed
map_total9 <- map_total8 %>% 
  mutate(FL_Score_Value_NOFEED = ifelse(FL_Score == "AVERAGE(Meat, chicken; Meat, turkey)",
                                 poultry_nofeed,
                                 FL_Score_Value_NOFEED))

# what's left?
map_total9 %>% filter(is.na(FL_Score_Value)) %>% View() # none! (except seafood - deal with in following script)

# double check all averages
map_total9 %>% filter(str_detect(FL_Score, "^AVERAGE")) %>% View() # looks good

# what about scores without feed
map_total9 %>% filter(is.na(FL_Score_Value_NOFEED)) %>% View() 

# apply fl score to missing
map_total10 <- map_total9 %>% 
  mutate(FL_Score_Value_NOFEED = ifelse(is.na(FL_Score_Value_NOFEED), FL_Score_Value, FL_Score_Value_NOFEED))

# check
map_total10 %>% filter(is.na(FL_Score_Value_NOFEED)) %>% View() # good

# Handle units

# first, change mrh/ton to mrh/gram
# 1 ton = 1,000,000 grams
map_total11 <- map_total10 %>% 
  mutate(FL_Score_Value_grams = FL_Score_Value / 1000000,
         FL_Score_Value_NOFEED_grams = FL_Score_Value_NOFEED / 1000000)

# create vector of food items (i.e., dietary factors)
diet_factors <- c("fruit_exc_juice", # whole fruit
                  "fruit_juice", #100% fruit juice
            
                  "veg_dg", # dark green vegetables
                  "veg_sta", # starchy vegetables
                  "veg_ro", # red orange vegetables
                  "veg_oth", # other vegetables
                  
                  "gr_whole", # whole grains
                  "gr_refined", # refined grains
                  
                  "dairy", # dairy
                  
                  "pf_redm", # red meat
                  "pf_poultry", # poultry 
                  "pf_egg", # eggs
                  "pf_seafood", # seafood
                  "pf_ns", # nuts and seeds
                  "leg_tot", # legumes (incl. soy food)
                  
                  "sat_fat", # saturated fat
                  "added_sugar", # added sugar
                  "oil") # unsaturated fat

# only keep diet factors we want
map_sub <- map_total11 %>% 
  filter(Foodgroup %in% diet_factors) %>% 
  mutate(FCID_Code_chr = as.character(FCID_Code))

# are there any  missing scores?
map_sub %>% filter(is.na(FL_Score_Value_grams)) %>% View() # none - good! (other than seafood for now)

# export FL scores
write_csv(map_sub, paste0("data/temp_output/FL_scores_FCID_", export_date, ".csv"))





