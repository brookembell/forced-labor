# TITLE: Calculate results
# AUTHOR: BROOKE BELL
# DATE: 8-28-24

rm(list = ls())

options(scipen=999)

library(tidyverse)
library(readxl)
library(xlsx)
library(data.table)

export_date <- "082824"

# IMPORT DIETARY PATTERNS -----

dga <- read_xlsx("data/rec_patterns/2000_kcal_patterns.xlsx", sheet = "DGA Patterns")

lancet <- read_xlsx("data/rec_patterns/2000_kcal_patterns.xlsx", sheet = "EAT-Lancet Pattern")

convert <- read_csv("data/unit_conversions/unit_conversions_final.csv")

convert1 <- convert %>% select(Food_group, DGA_unit, Conversion_to_grams, Equation)

dga1 <- left_join(dga, convert1, by = c("Diet_Factor" = "Food_group"))

# convert servings to grams for DGA patterns
dga2 <- dga1 %>% mutate(HUS_consumed_grams = ifelse(Intake_Unit != "grams/day",
                                                   US_2000kcal * Conversion_to_grams,
                                                   US_2000kcal),
                        
                        MED_consumed_grams = ifelse(Intake_Unit != "grams/day",
                                                    MED_2000kcal * Conversion_to_grams,
                                                    MED_2000kcal),
                        
                        VEG_consumed_grams = ifelse(Intake_Unit != "grams/day",
                                                    VEG_2000kcal * Conversion_to_grams,
                                                    VEG_2000kcal))

# select vars
dga3 <- dga2 %>% select(Diet_Factor, HUS_consumed_grams, MED_consumed_grams, VEG_consumed_grams)

lancet1 <- lancet %>% select(Diet_Factor, LANCET_2000kcal)

# join
patterns <- left_join(dga3, lancet1, by = "Diet_Factor")

# IMPORT NHANES INTAKE -----

# import nhanes intake data
nhanes <- read_csv("data/nhanes/NHANES_1518_summary_allfoods_adj.csv")

nhanes1 <- nhanes %>% mutate(food = gsub("_adj", "", food))

nhanes2 <- nhanes1 %>% left_join(convert1, by = c("food" = "Food_group"))

# convert servings to grams
nhanes3 <- nhanes2 %>% 
  mutate(mean_grams = mean * Conversion_to_grams,
         se_grams = SE * Conversion_to_grams)

# look at fruit
nhanes3 %>% filter(str_detect(food, "fruit")) %>% View() #looks good

nhanes_sub <- nhanes3 %>%
  filter(!(food %in% c("sodium", "ssb"))) %>% 
  select(food, mean_grams, se_grams)

# join with patterns
# create dat for manuscript Table 1
patterns1 <- left_join(patterns, nhanes_sub, by = c("Diet_Factor" = "food")) %>% 
  rename(CURRENT_consumed_grams = mean_grams,
         PHD_consumed_grams = LANCET_2000kcal,
         foodgroup = Diet_Factor) %>% 
  arrange(foodgroup)

# export
write_csv(patterns1, "tables/Table_1_diet_patterns.csv")

# INCORPORATE INEDIBLE AND WASTED AMOUNTS -----

# get wasted and inedible coefficients
my_coefs <- read_csv(paste0("data/temp_output/FL_impact_factors_", export_date, ".csv")) %>% 
  arrange(foodgroup) %>% 
  select(foodgroup, wasted_coef, inedible_coef)

# import impact factors
# import data with min/max values
impacts <- read_csv(paste0("data/temp_output/FL_impact_factors_sensitivity_", export_date, ".csv")) %>% 
  arrange(foodgroup)

# create dat for manuscript Table 1
flrscore_only <- impacts %>% 
  select(foodgroup, FLper100gram)

# export
write_csv(flrscore_only, "tables/Table_1_flrscores.csv")

# join
impacts1 <- left_join(impacts, my_coefs, by = "foodgroup")

# join with patterns
patterns2 <- left_join(patterns1, impacts1, by = "foodgroup")

# remove pf_leg and pf_soy because leg_tot = pf_leg + pf_soy
patterns3 <- patterns2 %>% filter(!(foodgroup %in% c("pf_leg", "pf_soy")))

# calculate wasted and inedible amts of food
patterns4 <- patterns3 %>% 
  mutate(HUS_wasted_grams = HUS_consumed_grams * wasted_coef,
         MED_wasted_grams = MED_consumed_grams * wasted_coef,
         VEG_wasted_grams = VEG_consumed_grams * wasted_coef,
         PHD_wasted_grams = PHD_consumed_grams * wasted_coef,
         CURRENT_wasted_grams = CURRENT_consumed_grams * wasted_coef,
         
         HUS_inedible_grams = HUS_consumed_grams * inedible_coef,
         MED_inedible_grams = MED_consumed_grams * inedible_coef,
         VEG_inedible_grams = VEG_consumed_grams * inedible_coef,
         PHD_inedible_grams = PHD_consumed_grams * inedible_coef,
         CURRENT_inedible_grams = CURRENT_consumed_grams * inedible_coef,
         
         HUS_purchased_grams = HUS_consumed_grams + HUS_wasted_grams + HUS_inedible_grams,
         MED_purchased_grams = MED_consumed_grams + MED_wasted_grams + MED_inedible_grams,
         VEG_purchased_grams = VEG_consumed_grams + VEG_wasted_grams + VEG_inedible_grams,
         PHD_purchased_grams = PHD_consumed_grams + PHD_wasted_grams + PHD_inedible_grams,
         CURRENT_purchased_grams = CURRENT_consumed_grams + CURRENT_wasted_grams + CURRENT_inedible_grams)

# check
patterns4 %>% 
  filter(foodgroup == "veg_sta") %>% 
  select(foodgroup, HUS_consumed_grams:CURRENT_consumed_grams,
         wasted_coef, inedible_coef,
         HUS_wasted_grams:CURRENT_inedible_grams) %>% 
  View()

# CALCULATE RESULTS -----

patterns5 <- patterns4 %>% mutate(HUS_total_FL = HUS_purchased_grams * (FLper100gram / 100),
                                  MED_total_FL = MED_purchased_grams * (FLper100gram / 100),
                                  VEG_total_FL = VEG_purchased_grams * (FLper100gram / 100),
                                  PHD_total_FL = PHD_purchased_grams * (FLper100gram / 100),
                                  CURRENT_total_FL = CURRENT_purchased_grams * (FLper100gram / 100),
                                  
                                  # no feed
                                  HUS_total_FL_nofeed = HUS_purchased_grams * (FLper100gram_nofeed / 100),
                                  MED_total_FL_nofeed = MED_purchased_grams * (FLper100gram_nofeed / 100),
                                  VEG_total_FL_nofeed = VEG_purchased_grams * (FLper100gram_nofeed / 100),
                                  PHD_total_FL_nofeed = PHD_purchased_grams * (FLper100gram_nofeed / 100),
                                  CURRENT_total_FL_nofeed = CURRENT_purchased_grams * (FLper100gram_nofeed / 100)
                                  )

# create results table
results <- patterns5 %>% 
  select(foodgroup,
         HUS_total_FL,
         MED_total_FL,
         VEG_total_FL,
         PHD_total_FL,
         CURRENT_total_FL) %>% 
  arrange(foodgroup)

results1 <- results %>%
  # calculate Total
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~"Total"))) %>% 
  # round to 3 digits
  mutate(across(where(is.numeric), round, 3))


# Export for manuscript Table 2
write_csv(results1, "tables/Table_2.csv")

results_nofeed <- patterns5 %>% 
  select(foodgroup,
         HUS_total_FL_nofeed,
         MED_total_FL_nofeed,
         VEG_total_FL_nofeed,
         PHD_total_FL_nofeed,
         CURRENT_total_FL_nofeed) %>% 
  arrange(foodgroup)

# merge with food categories mapping
food_cat <- read_csv("data/mappings/Dietary_factor_categories.csv")

results_a <- left_join(results, food_cat, by = "foodgroup") %>% 
  relocate(broad_group, broad_group1, .after = foodgroup)

results_nofeed_a <- left_join(results_nofeed, food_cat, by = "foodgroup") %>% 
  relocate(broad_group, broad_group1, .after = foodgroup)

# figure 1 source data
write_csv(results_a, "figures/source_data/Figure_1a_data.csv")

# same data is used to create supp figures 2 and 3
write_csv(results_a, "figures/source_data/Supp_Figure_2_3_data_withfeed.csv")
write_csv(results_nofeed_a, "figures/source_data/Supp_Figure_2_3_data_nofeed.csv")

# summary
results_sum <- results %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(simulation_name = "baseline")

results_sum_nofeed <- results_nofeed %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(simulation_name = "no feed")

# REARRANGE DATA FOR TABLEAU FIGURES -----

# long format
results_long <- results %>% 
  pivot_longer(cols = -foodgroup,
               names_to = "pattern",
               values_to = "value")

results_nofeed_long <- results_nofeed %>% 
  pivot_longer(cols = -foodgroup,
               names_to = "pattern",
               values_to = "value")

results_long1 <- results_long %>% 
  mutate(pattern = str_extract(pattern, ".+?(?=_)"),
         data_type = "FL")

results_nofeed_long1 <- results_nofeed_long %>% 
  mutate(pattern = str_extract(pattern, ".+?(?=_)"),
         data_type = "FL (no feed)")

intake_distr <- patterns4 %>% 
  rename(HUS = HUS_purchased_grams,
         MED = MED_purchased_grams,
         VEG = VEG_purchased_grams,
         PHD = PHD_purchased_grams,
         CURRENT = CURRENT_purchased_grams) %>% 
  select(foodgroup, HUS, MED, VEG, PHD, CURRENT) %>% 
  pivot_longer(cols = -foodgroup,
               names_to = "pattern",
               values_to = "value") %>% 
  mutate(data_type = "Consumed, inedible, and wasted (g)")

units <- read_csv("data/unit_conversions/unit_conversions_final.csv") %>% 
  select(-Equation)

serving_distr <- left_join(intake_distr, units, by = c("foodgroup" = "Food_group")) %>% 
  mutate(value_DGA = value / Conversion_to_grams,
         data_type = "Consumed, inedible, and wasted (FPED)") %>% 
  select(-c(value, Conversion_to_grams, DGA_unit)) %>% 
  rename(value = value_DGA)

results_long_comb <- rbind(results_long1, results_nofeed_long1, intake_distr, serving_distr) %>% 
  relocate(data_type, .before = value) %>% 
  arrange(foodgroup, pattern)

results_long_comb1 <- results_long_comb %>% 
  group_by(pattern, data_type) %>% 
  mutate(sum = sum(value)) %>% 
  ungroup() %>% 
  mutate(prop = (value / sum) * 100)
  
# create source data for figure 1b
fig1b <- results_long_comb1 %>% 
  filter(data_type == "FL") %>% 
  arrange(pattern)

write_csv(fig1b, "figures/source_data/Figure_1b_data.csv")

# restrict to protein foods
results_protein <- results_long_comb %>% 
  filter(str_detect(foodgroup, "pf_") | foodgroup == "leg_tot") %>% 
  group_by(pattern, data_type) %>% 
  mutate(sum = sum(value)) %>% 
  ungroup() %>% 
  mutate(prop = (value / sum) * 100) %>% 
  arrange(pattern, data_type)

# create source data for figure 2
fig2 <- results_protein %>% 
  filter(data_type %in% c("FL", "Consumed, inedible, and wasted (g)"))

# export
write_csv(fig2, "figures/source_data/Figure_2_data.csv")

# CONDUCT SENSITIVITY ANALYSIS -----

# Construct the datasets I need for to run the sensitivity analysis

# min
min_list <- list()

for (i in 1:nrow(patterns4)) {
  min_list[[paste0("min_", patterns4$foodgroup[i])]] <- 
    patterns4 %>% 
    mutate(FLper100gram_final = ifelse(foodgroup == foodgroup[i], FLper100gram_min, FLper100gram))
}

# max
max_list <- list()

for (i in 1:nrow(patterns4)) {
  max_list[[paste0("max_", patterns4$foodgroup[i])]] <- 
    patterns4 %>% 
    mutate(FLper100gram_final = ifelse(foodgroup == foodgroup[i], FLper100gram_max, FLper100gram))
}

# check
min_list[[1]] %>% View()
max_list[[1]] %>% View()

# Calculate results for all simulations

# min
min_results_list <- list()
min_results_summary_list <- list()

for(i in 1:length(min_list)){
  
  temp <- 
    min_list[[i]] %>% mutate(HUS_total_FL = HUS_purchased_grams * (FLper100gram_final / 100),
                             MED_total_FL = MED_purchased_grams * (FLper100gram_final / 100),
                             VEG_total_FL = VEG_purchased_grams * (FLper100gram_final / 100),
                             PHD_total_FL = PHD_purchased_grams * (FLper100gram_final / 100),
                             CURRENT_total_FL = CURRENT_purchased_grams * (FLper100gram_final / 100))
  
  # create results table
  min_results_list[[i]] <- temp %>% 
    select(foodgroup, 
           HUS_total_FL, 
           MED_total_FL,
           VEG_total_FL, 
           PHD_total_FL, 
           CURRENT_total_FL) %>% 
    arrange(foodgroup)

  min_results_summary_list[[i]] <-
    min_results_list[[i]] %>%
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
    mutate(simulation_name = paste0(names(min_list)[[i]]))
  
  # export
  write.xlsx(min_results_list[[i]], 
             file = paste0("data/sensitivity_analysis/Sensitivity_results_min_", export_date, ".xlsx"), 
             sheetName = paste0(names(min_list)[[i]]),
             append = TRUE)
  
}

# max
max_results_list <- list()
max_results_summary_list <- list()

for(i in 1:length(max_list)){
  
  temp <- 
    max_list[[i]] %>% mutate(HUS_total_FL = HUS_purchased_grams * (FLper100gram_final / 100),
                             MED_total_FL = MED_purchased_grams * (FLper100gram_final / 100),
                             VEG_total_FL = VEG_purchased_grams * (FLper100gram_final / 100),
                             PHD_total_FL = PHD_purchased_grams * (FLper100gram_final / 100),
                             CURRENT_total_FL = CURRENT_purchased_grams * (FLper100gram_final / 100))
  
  # create results table
  max_results_list[[i]] <- temp %>% 
    select(foodgroup, 
           HUS_total_FL,
           MED_total_FL, 
           VEG_total_FL, 
           PHD_total_FL, 
           CURRENT_total_FL) %>% 
    arrange(foodgroup)
  
  max_results_summary_list[[i]] <-
    max_results_list[[i]] %>%
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% 
    mutate(simulation_name = paste0(names(max_list)[[i]]))
  
  # export
  write.xlsx(max_results_list[[i]], 
             file = paste0("data/sensitivity_analysis/Sensitivity_results_max_", export_date, ".xlsx"), 
             sheetName = paste0(names(max_list)[[i]]),
             append = TRUE)
  
}

# summary values
min_summary <- bind_rows(min_results_summary_list)
max_summary <- bind_rows(max_results_summary_list)

all_summary <- rbind(min_summary, max_summary)

all_summary1 <- rbind(results_sum, all_summary)

# export
write_csv(all_summary1, paste0("data/sensitivity_analysis/Sensitivity_summary_", export_date, ".csv"))

# Calculate ranks

t <- transpose(all_summary1)

# get row and colnames in order
rownames(t) <- colnames(all_summary1)
colnames(t) <- all_summary1$simulation_name %>% as.vector()

t1 <- t[1:5,]

t2 <- t1 %>% rownames_to_column(var = "diet")

t3 <- pivot_longer(t2, 
                   cols = !diet,
                   names_to = "sim",
                   values_to = "value")

t4 <- t3 %>% 
  arrange(sim, value) %>%
  group_by(sim) %>% 
  mutate(rank = rank(value, ties.method = "first"))

# export for tableau
write_csv(t4, "figures/source_data/Figure_4_data.csv")




