# Forced Labor Repository

This repo contains code and data for the manuscript titled, "The human cost of current and recommended diets in the U.S.".

This repo is also an R project, and the "forced-labor.Rproj" file must be opened in R Studio first in order for the code to run properly.

## Organization

There are four main folders:

- code: contains the R code needed to run all analyses that were conducted for the manuscript. the code should be run in numerical order (i.e., run the script with "1" in the file name first, then "2", etc.)
- data: contains all datasets needed to run the analyses. there are sub-folders in order to distinguish the datasets further:
  * fl_scores: contains the forced labor risk scores for each food commodity.
  * food_waste: contains food waste and inedible coefficients for each FCID code.
  * mappings: contains multiple mapping datasets that are used to link together various datasets together in order to conduct the analyses.
  * nhanes: contains multiple NHANES food intake datasets.
  * rec_patterns: contains the four recommended dietary patterns, including the three U.S. Dietary Guidelines  patterns and the Planetary Health Diet pattern.
  * sensitivity_analysis: contains the datasets needed to conduct the sensitivity analysis.
  * temp_output: contains temporary datasets that are created while running the code.
  * unit_conversions: contains conversion units needed to transform all of the food intake units from teaspoon, cup, or ounce to gram.
- figures: contains the manuscript figures.
  * source_data: contains the input data files that are needed to create the figures.
  * tableau: contains the tableau files used to create the figures.
- tables: contains the manuscript tables that are created with the R scripts.
