##
## Calculate MSOA-level deprivation
##
library(tidyverse)
library(Hmisc)

# ---- Functions for aggregating IMD into higher level geographies ----
#' Invert deciles, ranks, percentiles etc.
#' e.g. with decile, a score of 10 --> 1 and score of 1 --> 10
#' @param x Vector of data to invert
invert_this = function(x) (max(x, na.rm = TRUE) + 1) - x

#' Calculate the Extent and population-weighted average scores for small areas
#' 
#' "Extent" is the proportion of the local population that live in areas classified as among the most deprived in the higher geography.
#' To calculate this, we need to first calculate a weighted score based on the population in the most deprived 30% of areas
#' 
#' @param d Dataframe containing ranks, scores and population estimates
#' @param domain Which domain to calculate population-weighted scores for
#' @param score_suffix The wording of the variable containing scores
#' @param rank_suffix The wording of the variable containing ranks
#' @param decile_suffix The wording of the variable containing deciles
#' @param aggregate_by Name of the variable containing the higher-level geography names/codes
#' @param population_col Name of the column containing population estimates
#' 
pop_weighted_scores = function(d, 
                               domain = "Index of Multiple Deprivation (IMD)", 
                               score_suffix = "Score",
                               rank_suffix = "Rank (where 1 is most deprived)",
                               decile_suffix = "Decile (where 1 is most deprived 10% of LSOAs)",
                               aggregate_by, 
                               population_col = "No. people") {
  
  rank_col = paste0(domain, " ", rank_suffix)
  score_col = paste0(domain, " ", score_suffix)
  
  d %>% 
    group_by(!!sym(aggregate_by)) %>%
    
    mutate(Percentile = round((!!sym(rank_col) / max(!!sym(rank_col))) * 100, 0)) %>% 
    
    # invert percentiles because, in the Vulnerability Index, higher percentiles mean higher vulnerability - but the extent score calculation below assumes lower percentiles mean higher vulnerability
    # mutate(Percentile = invert_this(Percentile)) %>%
    
    # calculate extent: a weighted score based on the population in the most deprived 30% of areas
    # from the IMD technical report Appendix N:
    # "The population living in the most deprived 11 to 30 per cent of Lower-layer Super Output Areas 
    # receive a sliding weight, ranging from 0.95 for those in the most deprived eleventh percentile, 
    # to 0.05 for those in the most deprived thirtieth percentile. 
    # In practice this means that the weight starts from 0.95 in the most deprived eleventh percentile, 
    # and then decreases by (0.95-0.05)/19 for each of the subsequent nineteen percentiles 
    # until it reaches 0.05 for the most deprived thirtieth percentile, and zero for areas outside the most deprived 30 per cent"
    mutate(Extent = case_when(
      Percentile <= 10 ~ !!sym(population_col),
      Percentile == 11 ~ !!sym(population_col) * 0.95,
      Percentile > 11 & Percentile <= 30 ~ !!sym(population_col) * (0.95 - ((0.9/19) * (Percentile - 11))),
      TRUE ~ 0
    )) %>% 
    
    # calculate population-weighted average scores
    mutate(Score = !!sym(score_col) * !!sym(population_col)) %>% 
    
    ungroup()
}

#' Aggregate IMD into higher-level geographies, calculating:
#' - proportion of highly deprived areas
#' - extent (proportion of the local population that live the most deprived areas)
#' - population-weighted average score
#'
#' @param d Dataframe containing deciles, ranks, scores and population estimates
#' @param domain Which domain to calculate population-weighted scores for
#' @param score_suffix The wording of the variable containing scores
#' @param rank_suffix The wording of the variable containing ranks
#' @param decile_suffix The wording of the variable containing deciles
#' @param aggregate_by Name of the column to use for higher-level aggregation (e.g. "LAD19CD")
#' @param population_col Name of the column containing population estimates
#'
aggregate_scores = function(d, 
                            domain = "Index of Multiple Deprivation (IMD)", 
                            score_suffix = "Score",
                            rank_suffix = "Rank (where 1 is most deprived)",
                            decile_suffix = "Decile (where 1 is most deprived 10% of LSOAs)",
                            aggregate_by, 
                            population_col = "No. people") {
  
  decile_col = paste0(domain, " ", decile_suffix)
  
  # calculate proportions of highly deprived MSOAs in the higher-level geography
  d_props = d %>% 
    # label MSOAs by whether they're in top 20% most-deprived then summarise by this label
    mutate(Top20 = ifelse(!!sym(decile_col) <= 2, "Top20", "Other")) %>% 
    janitor::tabyl(!!sym(aggregate_by), Top20) %>% 
    
    # calculate proportion of most deprived LSOAs
    mutate(Proportion = Top20 / (Top20 + Other)) %>% 
    select(!!sym(aggregate_by), Proportion)
  
  # calculate population-weighted scores and extent for the higher-level geography
  d_scores = d %>% 
    pop_weighted_scores(domain = domain, 
                        score_suffix = score_suffix, rank_suffix = rank_suffix, 
                        aggregate_by = aggregate_by, population_col = population_col) %>% 
    
    group_by(!!sym(aggregate_by)) %>%
    summarise(Extent = sum(Extent) / sum(!!sym(population_col)),
              Score = sum(Score) / sum(!!sym(population_col)))
  
  # combine and return all aggregated measures
  left_join(d_props, d_scores, by = aggregate_by)
}


# ---- Load data ----
# Output Area to LSOA to MSOA to Local Authority District (December 2017) Lookup with Area Classifications in Great Britain
# source: http://geoportal.statistics.gov.uk/datasets/fe6c55f0924b4734adf1cf7104a0173e_0
lsoa_msoa = read_csv("https://opendata.arcgis.com/datasets/fe6c55f0924b4734adf1cf7104a0173e_0.csv") %>% 
    select(starts_with("LSOA"), starts_with("MSOA")) %>% 
    distinct()

# Load IMD
# source: File 7: all ranks, deciles and scores for the indices of deprivation, and population denominators from https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019
imd = read_csv("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/845345/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv")


# ---- Aggregate IMD into MSOAs ----
imd_lsoa = imd %>% 
  select(LSOA11CD = `LSOA code (2011)`, `Index of Multiple Deprivation (IMD) Score`, `Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`, `Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)`, n = `Total population: mid 2015 (excluding prisoners)`) %>% 
  left_join(lsoa_msoa, by = "LSOA11CD")

imd_msoa = imd_lsoa %>% 
  aggregate_scores(domain = "Index of Multiple Deprivation (IMD)", aggregate_by = "MSOA11CD", population_col = "n")

# Calculate deciles for each measure (where 1 = most deprived)
imd_msoa = imd_msoa %>% 
  mutate(Proportion_Decile = as.integer(cut2(Proportion, g = 10)),
         Extent_Decile     = as.integer(cut2(Extent, g = 10)),
         Score_Decile      = as.integer(cut2(Score, g = 10))) %>% 
  
  mutate(Proportion_Decile = invert_this(Proportion_Decile),
         Extent_Decile     = invert_this(Extent_Decile),
         Score_Decile      = invert_this(Score_Decile))

write_csv(imd_msoa, "data/IMD by MSOA.csv")
