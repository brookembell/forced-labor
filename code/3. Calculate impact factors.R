# TITLE: Calculate impact factors for the forced labor risk scores
# AUTHOR: Brooke Bell
# DATE: 8-28-24

rm(list = ls())

options(scipen=999)

library(tidyverse)
library(readxl)
library(survey)

# today's date
export_date <- "082824"

# NHANES FOOD DATA -----

# first, read in nhanes food-level data

day1 <- read_rds("data/nhanes/foods_day1_clean.rds")

# select vars
day1_sub <- day1 %>% select(SEQN, DR1ILINE, DR1IFDCD, DR1IGRMS, DESCRIPTION, nhanes_cycle, dayrec) %>% 
  rename(seqn = SEQN,
         line = DR1ILINE,
         foodcode = DR1IFDCD,
         grams = DR1IGRMS,
         description = DESCRIPTION)

day2 <- read_rds("data/nhanes/foods_day2_clean.rds")

# select vars
day2_sub <- day2 %>% select(SEQN, DR2ILINE, DR2IFDCD, DR2IGRMS, DESCRIPTION, nhanes_cycle, dayrec) %>% 
  rename(seqn = SEQN,
         line = DR2ILINE,
         foodcode = DR2IFDCD,
         grams = DR2IGRMS,
         description = DESCRIPTION)

rm(day1, day2)

# combine day 1 and day 2
both_days <- rbind(day1_sub, day2_sub) %>% arrange(seqn, dayrec, line)

# MAPPING FROM FNDDS FOODCODE TO DIETARY FACTOR -----

# read in mappings
map_a <- read_csv("data/mappings/FNDDS_to_food_mapping.csv")
map_b <- read_csv("data/mappings/FNDDS_to_food_mapping_wholegrains.csv")

# create all foodcodes vector
all_foodcodes <- both_days %>% select(foodcode) %>% distinct()

# join
my_join <- full_join(mutate(all_foodcodes, i=1), 
                         mutate(map_a, i=1)) %>% 
  select(-i) %>% 
  filter(str_detect(foodcode, paste0("^", foodcode_prefix))) %>% 
  select(-foodcode_prefix)

# now join with whole grain map
my_join1 <- rbind(my_join, map_b) %>% arrange(foodcode)

# join
both_days1 <- left_join(both_days, my_join1, by = "foodcode")

# which fndds codes have missing grams
both_days1 %>% filter(is.na(grams)) %>% select(description) %>% table()

# remove if missing grams value - just human breast milk
both_days2 <- both_days1 %>% filter(!(is.na(grams)))

# MAPPING FROM FNDDS FOODCODE TO FCID CODE -----

# import 
map <- read_csv("data/mappings/FNDDS_to_FCID_mapping_final.csv")

# select vars
map1 <- map %>% select(foodcode, fcidcode, fcid_desc, wt)

# combine with food data
both_days3 <- full_join(both_days2, map1, by = "foodcode")

# remove if missing SEQN - foodcodes that no one ate
both_days3 %>% filter(is.na(grams)) 

both_days4 <- both_days3 %>% 
  filter(!(is.na(seqn))) %>% 
  arrange(seqn, dayrec, line) %>% 
  relocate(c(nhanes_cycle, dayrec), .after = last_col())

# which ones are missing?
both_days4 %>% filter(is.na(fcidcode)) %>% select(foodcode, description) %>% distinct() %>% View()

# manually assign proxies for corn and dasheen
both_days5 <- both_days4 %>% mutate(fcidcode = ifelse(foodcode == 71962020 | foodcode == 71962020,
                                                        103139000,
                                                        fcidcode),
                                    
                                    fcidcode = ifelse(foodcode == 75215990,
                                                        1500127000,
                                                        fcidcode),
                                    
                                    wt = ifelse(foodcode == 71962020 | foodcode == 71962020,
                                                      100,
                                                      wt),
                                    
                                    wt = ifelse(foodcode == 75215990,
                                                      100,
                                                      wt))

# LOSS WASTE DATA -----

# import wasted and inedible coefficients
losswaste_complete <- read_csv("data/food_waste/waste_ined_coefficients_final.csv") %>% 
  select(-c(fcid_desc, Foodgroup, Proxy, Notes))

# merge with bothdays
both_days6 <- left_join(both_days5, losswaste_complete, by = "fcidcode")

# check missing
both_days6 %>% filter(is.na(waste_coef)) %>% select(fcidcode, fcid_desc) %>% distinct() %>% View()
# this is fine - very minor

# FCID TO DIETARY FACTOR MAPPING -----

# import fcid-diet factor mapping
new_map <- read_xlsx("data/mappings/FCID_to_food_mapping_final.xlsx") %>% 
  select(FCID_Code, Foodgroup) %>% 
  rename(fcidcode = FCID_Code,
         Foodgroup_FCID = Foodgroup)

# merge
both_days7 <- left_join(both_days6, new_map, by = "fcidcode")

# import new mapping
fcids_new <- read_csv("data/mappings/FNDDS_to_food_mapping_missing.csv") %>% 
  select(-description) %>% 
  rename(Foodgroup_FCID = Foodgroup)

both_days8 <- rows_patch(both_days7, fcids_new, unmatched = "ignore")

# check missing fcid
both_days8 %>% filter(is.na(fcidcode)) %>% View()
both_days8 %>% filter(is.na(fcidcode)) %>% select(description) %>% table() %>% View()
# this is fine - we're not using the majority of these anyway

# check missing waste coef
both_days8 %>% filter(is.na(waste_coef)) %>% View()
both_days8 %>% filter(is.na(waste_coef)) %>% select(fcid_desc) %>% distinct()
# there's very few foodcodes left that don't have waste and inedible coefficients

# calculate consumed_amt_g, inedible_amt_g, wasted_amt_g, purchased_amt_g
both_days9 <- both_days8 %>% 
  mutate(consumed_amt_FCID = grams * (wt / 100),
         inedible_amt_FCID = consumed_amt_FCID * ined_coef,
         wasted_amt_FCID = consumed_amt_FCID * waste_coef,
         purchased_amt_FCID = consumed_amt_FCID + inedible_amt_FCID + wasted_amt_FCID)

# FL RISK SCORES (LAND) -----

# import fl scores
fl <- read_csv(paste0("data/temp_output/FL_scores_FCID_", export_date, ".csv")) %>% 
  select(FCID_Code_chr, Weight_Conversion, FL_Score_Value_grams, FL_Score_Value_NOFEED_grams)

# now merge
both_days10 <- left_join(both_days9, fl, by = c("fcidcode" = "FCID_Code_chr"))

# any missing fl scores?
both_days10 %>% filter(is.na(FL_Score_Value_grams)) %>% View() #fine
both_days10 %>% filter(is.na(FL_Score_Value_grams)) %>% select(Foodgroup_FCID) %>% distinct() #fine

# create new var
both_days11 <- both_days10 %>%
  mutate(nhanes1516 = ifelse(nhanes_cycle == "2015-2016", 1, 0))

# remove if fcid food group is not relevant to analysis
both_days11 %>% select(Foodgroup_FCID) %>% distinct() %>% unlist()

both_days12 <- both_days11 %>% 
  filter(!(Foodgroup_FCID %in% c("babyfood", "coffee_tea", "water", "other")))

# check missing
both_days12 %>% filter(is.na(FL_Score_Value_grams)) %>% View()
both_days12 %>% filter(is.na(FL_Score_Value_grams)) %>% select(Foodgroup_FCID) %>% distinct() #fine
both_days12 %>% filter(is.na(FL_Score_Value_grams)) %>% select(foodcode, description) %>% distinct() %>% View() #mostly mixed dishes

# FL RISK SCORES (SEAFOOD) -----

both_days12 %>% filter(Foodgroup == "pf_seafood") %>% View()
both_days12 %>% filter(Foodgroup_FCID == "pf_seafood") %>% View()
both_days12 %>% filter(Foodgroup == "pf_seafood" & Foodgroup_FCID == "pf_seafood") %>% View()

# create seafood and no seafood datasets
seafood <- both_days12 %>% 
  filter(Foodgroup == "pf_seafood" & Foodgroup_FCID == "pf_seafood") %>% 
  select(-c(FL_Score_Value_grams, FL_Score_Value_NOFEED_grams))

no_seafood <- both_days12 %>% filter(!(Foodgroup == "pf_seafood" & Foodgroup_FCID == "pf_seafood"))

# mixed seafood dishes
both_days12 %>% filter(Foodgroup != "pf_seafood" & Foodgroup_FCID == "pf_seafood") %>% select(-FL_Score_Value_grams) %>% View()

# create seafood-mixed dishes dataset
seafood_mixed <- both_days12 %>% 
  filter(Foodgroup != "pf_seafood" & Foodgroup_FCID == "pf_seafood") %>% 
  select(foodcode, description, Foodgroup) %>% 
  distinct() %>% 
  arrange(foodcode)

# look at sushi
both_days12 %>% 
  filter(Foodgroup != "pf_seafood" & Foodgroup_FCID == "pf_seafood") %>% 
  filter(grepl("Sushi", description)) %>% 
  select(foodcode, description, fcidcode, fcid_desc, wt) %>% 
  distinct() %>% 
  arrange(foodcode, fcidcode) %>% 
  View()

# export
write_csv(seafood_mixed, paste0("data/temp_output/seafood_mixed_dishes_", export_date, ".csv"))

# check
nrow(seafood) + nrow(no_seafood)
nrow(both_days12)
(nrow(seafood) + nrow(no_seafood)) == nrow(both_days12) #good

# import seafood fl scores
sea_scores <- read_csv(paste0("data/temp_output/seafood_FL_scores_FNDDS_", export_date, ".csv"))

# join
sea_join <- left_join(seafood, sea_scores, by = c("foodcode" = "FNDDS_Code",
                                                  "description" = "FNDDS_Desc"))

# missing
sea_join %>% filter(is.na(FL_Score_Value_grams)) # bouillabaise

# import bouillabaise scores
bou <- read_xlsx("data/mappings/FNDDS_to_FLR_mapping_seafood_final.xlsx",
                 sheet = "Bouillabaisse scores") %>% 
  select(FNDDS_code, FCID_code, FL_Score_FCID) %>% 
  rename(foodcode = FNDDS_code,
         fcidcode = FCID_code) %>% 
  mutate(FL_Score_Value_bou = FL_Score_FCID / 1000000) %>% 
  select(-FL_Score_FCID)

bou_NOFEED <- read_xlsx("data/mappings/FNDDS_to_FLR_mapping_seafood_final.xlsx",
                       sheet = "Bouillabaisse scores NOFEED") %>% 
  select(FNDDS_code, FCID_code, FL_Score_FCID_NOFEED) %>% 
  rename(foodcode = FNDDS_code,
         fcidcode = FCID_code) %>% 
  mutate(FL_Score_Value_bou_NOFEED = FL_Score_FCID_NOFEED / 1000000) %>% 
  select(-FL_Score_FCID_NOFEED)

bou_join <- left_join(bou, bou_NOFEED)

# join
sea_join1 <- left_join(sea_join, bou_join)

# add bouillabaise score to dataset
sea_join2 <- sea_join1 %>% 
  mutate(FL_Score_Value_grams = ifelse(foodcode == 27350110 & is.na(FL_Score_Value_grams), FL_Score_Value_bou, FL_Score_Value_grams),
         FL_Score_Value_NOFEED_grams = ifelse(foodcode == 27350110 & is.na(FL_Score_Value_NOFEED_grams), FL_Score_Value_bou_NOFEED, FL_Score_Value_NOFEED_grams)) %>% 
  select(-c(FL_Score, FL_Score_Value_chr, FL_Score_Value, FL_Score_Value_NOFEED, FL_Score_Value_bou, FL_Score_Value_bou_NOFEED))

# any missing?
sea_join2 %>% filter(is.na(FL_Score_Value_grams)) #none

# check bou
sea_join2 %>% filter(foodcode == 27350110) %>% View() #looks good

# LASTLY
# combine back with no seafood
both_days13 <- rbind(no_seafood, sea_join2) %>% arrange(seqn, dayrec, line)

# set weight conversion to 1 for seafood
both_days14 <- both_days13 %>% mutate(Weight_Conversion = ifelse(Foodgroup_FCID == "pf_seafood" & is.na(Weight_Conversion),
                                                                 1,
                                                                 Weight_Conversion))

# check missing
both_days14 %>% filter(is.na(Weight_Conversion)) %>% View() #fine
both_days14 %>% filter(is.na(FL_Score_Value_grams)) %>% View() 

both_days14 %>% filter(is.na(FL_Score_Value_grams)) %>% 
  select(description, Foodgroup, fcid_desc, Foodgroup_FCID) %>% 
  distinct() %>% 
  View() # these must be missing from FNDDS-FCID mapping

# FL RISK IMPACTS, PER FCID code -----

# calculate amount of FLR attributable to consumed amount of FCID food
my_fl_table <- 
  both_days14 %>% 
  mutate(
    FL_Score_Value_grams_converted = FL_Score_Value_grams * Weight_Conversion,
    FL_Score_Value_NOFEED_grams_converted = FL_Score_Value_NOFEED_grams * Weight_Conversion,
    
    FL_impact_per_FCID_Consumed = FL_Score_Value_grams_converted * consumed_amt_FCID,
    FL_impact_per_FCID_Consumed_NOFEED = FL_Score_Value_NOFEED_grams_converted * consumed_amt_FCID)
    
# what food groups have missing impact factors?
my_fl_table %>% 
  filter(is.na(FL_Score_Value_grams_converted)) %>%
  select(foodcode, description, fcidcode, fcid_desc, Foodgroup_FCID) %>%
  distinct() %>%
  View() # 88 dishes - fine

my_fl_table %>% 
  filter(is.na(FL_Score_Value_NOFEED_grams_converted)) %>%
  select(foodcode, description, fcidcode, fcid_desc, Foodgroup_FCID) %>%
  distinct() %>%
  View() # 88 dishes - fine

# summarise FLR and consumed amount (per person, per fcid code, per day)
fl_impact <- my_fl_table %>% 
  group_by(seqn, fcidcode, dayrec) %>% 
  summarise(FL_per_day = sum(FL_impact_per_FCID_Consumed),
            FL_per_day_NOFEED = sum(FL_impact_per_FCID_Consumed_NOFEED),
            purchased_per_day = sum(purchased_amt_FCID),
            consumed_per_day = sum(consumed_amt_FCID),
            wasted_per_day = sum(wasted_amt_FCID),
            inedible_per_day = sum(inedible_amt_FCID)) %>% 
  filter(!(is.na(fcidcode)))

# any missing purchased amt?
fl_impact %>% filter(is.na(purchased_per_day)) %>% View() #none

# any missing fl?
fl_impact %>% ungroup() %>% filter(is.na(FL_per_day)) %>% select(fcidcode) %>% distinct() #some mixed seafood dishes

# average day 1 and day 2 impacts for each fcid code
fl_wide <- pivot_wider(fl_impact, 
                           names_from = dayrec,
                           values_from = c(contains("per_day")))

# how many recalls does each person have?
num_recalls <- read_rds("data/nhanes/foods_day1_clean.rds") %>% 
  select(SEQN, DRDINT) %>% 
  distinct()

# join
fl_wide1 <- left_join(fl_wide, num_recalls, by = c("seqn" = "SEQN"))

# fix manually
fl_wide2 <- fl_wide1 %>% mutate(DRDINT = ifelse(seqn == "93792", 1, DRDINT))

# split out 
fl_wide_1day <- fl_wide2 %>% filter(DRDINT == 1)
fl_wide_2days <- fl_wide2 %>% filter(DRDINT == 2)

# if 2 days of recall, replace remaining NAs with 0
fl_wide_2days_1 <- fl_wide_2days %>% replace(is.na(.), 0)

# combine back together
fl_wide3 <- rbind(fl_wide_1day, fl_wide_2days_1) %>% arrange(DRDINT, seqn)

# calculate daily averages
fl_wide4 <- fl_wide3 %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(fl_impact_avg = mean(c(FL_per_day_1, FL_per_day_2), na.rm=TRUE),
         fl_impact_avg_NOFEED = mean(c(FL_per_day_NOFEED_1, FL_per_day_NOFEED_2), na.rm = TRUE),
         purchased_avg = mean(c(purchased_per_day_1, purchased_per_day_2), na.rm=TRUE),
         consumed_avg = mean(c(consumed_per_day_1, consumed_per_day_2), na.rm=TRUE),
         wasted_avg = mean(c(wasted_per_day_1, wasted_per_day_2), na.rm=TRUE),
         inedible_avg = mean(c(inedible_per_day_1, inedible_per_day_2), na.rm=TRUE)) %>% 
  select(-DRDINT)

# merge with map
fl_wide5 <- fl_wide4 %>% left_join(new_map, by = "fcidcode")

# add up FL impacts, by food group
fl_wide6 <- fl_wide5 %>% 
  group_by(seqn, Foodgroup_FCID) %>% 
  summarise(fl_impact_sum = sum(fl_impact_avg),
            fl_impact_sum_NOFEED = sum(fl_impact_avg_NOFEED),
            purchased_sum = sum(purchased_avg),
            consumed_sum = sum(consumed_avg),
            wasted_sum = sum(wasted_avg),
            inedible_sum = sum(inedible_avg))

# transform to wide
fl_wide7 <- fl_wide6 %>% 
  pivot_wider(id_cols = seqn,
              names_from = Foodgroup_FCID,
              values_from = !c(seqn, Foodgroup_FCID)) %>% 
  replace(is.na(.), 0)

# CALCULATE FL RISK IMPACT, BY FOOD GROUP -----

# import subgroup-seqn mapping
nhanes <- read_rds("data/nhanes/nhanes1518_adj_clean_wide.rds")

# subset vars
subgroup_dat <- nhanes %>% select(SEQN, subgroup, SDMVPSU, SDMVSTRA, wtnew, inAnalysis)

# join
fl_wide8 <- full_join(fl_wide7, subgroup_dat, by = c("seqn" = "SEQN"))

# Define survey design for fl dataset 
my_fl_svy <- svydesign(data=fl_wide8, 
                           id=~SDMVPSU, # Masked Variance Unit Pseudo-PSU 
                           strata=~SDMVSTRA, # Masked Variance Unit Pseudo-Stratum 
                           weights=~wtnew, # New sample weight
                           nest=TRUE)

# Create a survey design object for the subset of interest 
my_fl_svy_sub <- subset(my_fl_svy, inAnalysis==1)

# CALCULATE FL IMPACT
allfoods_fl <- svymean(reformulate(names(fl_wide8) %>% str_subset("fl")),
                       my_fl_svy_sub)

allfoods_fl %>% as.data.frame() %>% View()

# CALCULATE PURCHASED AMOUNTS
allfoods_purchased <- svymean(reformulate(names(fl_wide8) %>% str_subset("purchased")),
                             my_fl_svy_sub)

# CALCULATE CONSUMED AMOUNTS
allfoods_consumed <- svymean(reformulate(names(fl_wide8) %>% str_subset("consumed")),
                                  my_fl_svy_sub)

# CALCULATE WASTED AMOUNTS
allfoods_wasted <- svymean(reformulate(names(fl_wide8) %>% str_subset("wasted")),
                                  my_fl_svy_sub)

# CALCULATE INEDIBLE AMOUNTS
allfoods_inedible <- svymean(reformulate(names(fl_wide8) %>% str_subset("inedible")),
                           my_fl_svy_sub)

# turn into data frame
fl_means <- allfoods_fl %>% 
  as.data.frame() %>% 
  rownames_to_column("foodgroup") %>% 
  mutate(foodgroup = gsub("fl_impact_sum_", "", foodgroup)) %>% 
  select(-SE) %>% 
  rename(fl_mean = mean)

fl_means_NOFEED <- fl_means %>% 
  filter(str_detect(foodgroup, "NOFEED")) %>% 
  mutate(foodgroup = gsub("NOFEED_", "", foodgroup)) %>% 
  rename(fl_nofeed_mean = fl_mean)

fl_means1 <- fl_means %>% 
  filter(!(str_detect(foodgroup, "NOFEED")))

purchased_means <- allfoods_purchased %>% 
  as.data.frame() %>% 
  rownames_to_column("foodgroup") %>% 
  mutate(foodgroup = gsub("purchased_sum_", "", foodgroup)) %>% 
  select(-SE) %>% 
  rename(purchased_mean = mean)

consumed_means <- allfoods_consumed %>% 
  as.data.frame() %>% 
  rownames_to_column("foodgroup") %>% 
  mutate(foodgroup = gsub("consumed_sum_", "", foodgroup)) %>% 
  select(-SE) %>% 
  rename(consumed_mean = mean)

wasted_means <- allfoods_wasted %>% 
  as.data.frame() %>% 
  rownames_to_column("foodgroup") %>% 
  mutate(foodgroup = gsub("wasted_sum_", "", foodgroup)) %>% 
  select(-SE) %>% 
  rename(wasted_mean = mean)

inedible_means <- allfoods_inedible %>% 
  as.data.frame() %>% 
  rownames_to_column("foodgroup") %>% 
  mutate(foodgroup = gsub("inedible_sum_", "", foodgroup)) %>% 
  select(-SE) %>% 
  rename(inedible_mean = mean)

# merge together

all_means <- left_join(fl_means1, fl_means_NOFEED, by = "foodgroup") %>% 
  left_join(purchased_means, by = "foodgroup") %>% 
  left_join(consumed_means, by = "foodgroup") %>% 
  left_join(wasted_means, by = "foodgroup") %>% 
  left_join(inedible_means, by = "foodgroup")

# check 
all_means %>% 
  rowwise() %>% 
  mutate(temp_sum = consumed_mean + wasted_mean + inedible_mean) %>% 
  relocate(temp_sum, .after = purchased_mean) %>% 
  View() # looks good

# CALCULATE IMPACT FACTORS PER 100 GRAMS AND PER 1 SERVING -----

# first, import conversion units
units <- read_csv("data/unit_conversions/unit_conversions_final.csv") %>% 
  select(Food_group, Conversion_to_grams) %>% 
  mutate(Food_group = ifelse(Food_group == "dairy_tot", "dairy", Food_group)) # fix dairy label

# merge with units
all_means1 <- all_means %>% 
  left_join(units, by = c("foodgroup" = "Food_group"))

# calculate impact factors
impacts <- all_means1 %>% 
  rowwise() %>% 
  mutate(FLper100gram = (fl_mean / (consumed_mean + inedible_mean)) * 100,
         FLper100gram_nofeed = (fl_nofeed_mean / (consumed_mean + inedible_mean)) * 100,
         
         FLperDGA = FLper100gram * (Conversion_to_grams / 100),
         FLperDGA_nofeed = FLper100gram_nofeed * (Conversion_to_grams / 100),
         
         wasted_coef = wasted_mean / consumed_mean,
         inedible_coef = inedible_mean / consumed_mean) %>% 
  select(-Conversion_to_grams)

# fix foodgroup labels
impacts1 <- impacts %>% mutate(foodgroup = ifelse(foodgroup == "dairy", "dairy_tot", foodgroup),
                   foodgroup = ifelse(foodgroup == "pf_redm", "pf_redm_tot", foodgroup),
                   foodgroup = ifelse(foodgroup == "pf_poultry", "pf_poultry_tot", foodgroup))

# EXPORT
write_csv(impacts1, paste0("data/temp_output/FL_impact_factors_", export_date, ".csv"))

# CALCULATE FL RISK IMPACT, BY FCID CODE (For Manuscript Figure 3, Supplementary Figure 1) -----

x <- fl_wide5 %>% select(seqn, fcidcode, fl_impact_avg, consumed_avg, inedible_avg)

# pivot to wide
x_wide <- pivot_wider(x, 
                  id_cols = seqn,
                  names_from = fcidcode,
                  values_from = c(fl_impact_avg, consumed_avg, inedible_avg)) %>% 
  replace(is.na(.), 0)

# join
x_wide1 <- full_join(x_wide, subgroup_dat, by = c("seqn" = "SEQN"))

# Define survey design for fl dataset 
my_fcid_svy <- svydesign(data=x_wide1, 
                       id=~SDMVPSU, # Masked Variance Unit Pseudo-PSU 
                       strata=~SDMVSTRA, # Masked Variance Unit Pseudo-Stratum 
                       weights=~wtnew, # New sample weight
                       nest=TRUE)

# Create a survey design object for the subset of interest 
my_fcid_svy_sub <- subset(my_fcid_svy, inAnalysis==1)

# CALCULATE FL IMPACT
allfcids_fl <- svymean(reformulate(names(x_wide1) %>% str_subset("fl")),
                       my_fcid_svy_sub)

# CALCULATE CONSUMED AMOUNT
allfcids_consumed <- svymean(reformulate(names(x_wide1) %>% str_subset("consumed")),
                             my_fcid_svy_sub)

# CALCULATE INEDIBLE AMOUNT
allfcids_inedible <- svymean(reformulate(names(x_wide1) %>% str_subset("inedible")),
                                  my_fcid_svy_sub)

# turn into data frame
fl_fcid_means <- allfcids_fl %>% 
  as.data.frame() %>% 
  rownames_to_column("fcidcode") %>% 
  mutate(fcidcode = gsub("fl_impact_avg_", "", fcidcode)) %>% 
  select(-SE) %>% 
  rename(fl_mean = mean)

consumed_fcid_means <- allfcids_consumed %>% 
  as.data.frame() %>% 
  rownames_to_column("fcidcode") %>% 
  mutate(fcidcode = gsub("consumed_avg_", "", fcidcode)) %>% 
  select(-SE) %>% 
  rename(consumed_mean = mean)

inedible_fcid_means <- allfcids_inedible %>% 
  as.data.frame() %>% 
  rownames_to_column("fcidcode") %>% 
  mutate(fcidcode = gsub("inedible_avg_", "", fcidcode)) %>% 
  select(-SE) %>% 
  rename(inedible_mean = mean)

# merge together
all_fcid_means <- left_join(consumed_fcid_means, inedible_fcid_means, by = "fcidcode") %>% 
  left_join(fl_fcid_means, by = "fcidcode")

# CACULATE IMPACT FACTORS
fcid_IFs <- all_fcid_means %>% 
  rowwise() %>% 
  mutate(FLper100gram = (fl_mean / (consumed_mean + inedible_mean)) * 100)

# merge with food groups and fcid descs
fcids <- map %>% select(fcidcode, fcid_desc) %>% distinct() %>% mutate(fcidcode = as.character(fcidcode))

# change fcidcode to character format
new_map <- new_map %>% mutate(fcidcode = as.character(fcidcode))

fcid_IFs_join <- fcid_IFs %>% 
  left_join(fcids, by = "fcidcode") %>% 
  left_join(new_map, by = "fcidcode") %>% 
  mutate(FLper100gram = ifelse(FLper100gram == "NaN", 0, FLper100gram)) %>% 
  arrange(Foodgroup_FCID)

# create dataset for figures
# calculate sums and proportions
fcid_IFs_join1 <- fcid_IFs_join %>% 
  group_by(Foodgroup_FCID) %>% 
  mutate(my_sum = sum(fl_mean),
         my_consumed_sum = sum(consumed_mean),
         my_inedible_sum = sum(inedible_mean)) %>% 
  ungroup() %>% 
  mutate(my_prop = (fl_mean / my_sum) * 100,
         my_consumed_prop = (consumed_mean / my_consumed_sum) * 100,
         my_inedible_prop = (inedible_mean / my_inedible_sum) * 100) %>% 
  arrange(Foodgroup_FCID, fcid_desc)

# export
write_csv(fcid_IFs_join1, "figures/source_data/Figure_3_Supp_Figure_1_data.csv")

