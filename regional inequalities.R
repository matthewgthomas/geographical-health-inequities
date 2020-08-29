library(tidyverse)
library(readxl)
library(janitor)

# ---- Load and wrangle health data ----
# Load health indicators from Public Health England
# source: https://fingertips.phe.org.uk/search/life%20expectancy/page-options/ovw-do-0#page/9/gid/1/pat/101/par/E07000223/ati/3/are/E02006534/cid/4/tbm/1/page-options/ovw-do-0
health = read_csv("data/indicators-MSOA.data.csv")

# Get healthy life expectancy (2009-2013)
hle = health %>% 
  filter(`Indicator Name` == "Healthy life expectancy, (upper age band 85+)" & 
           `Area Type` == "MSOA")

# Get life expectancy (2013-2017)
le = health %>% 
  filter(`Indicator Name` == "Life expectancy at birth, (upper age band 90+)" & 
           `Area Type` == "MSOA")


# ---- Load and wrangle deprivation, region and rural-urban classification ----
imd = read_csv("data/IMD by MSOA.csv")

# MSOA to region lookup from https://github.com/drkane/geo-lookups
msoa_region = read_csv("https://github.com/drkane/geo-lookups/raw/master/msoa_la.csv") %>% 
  select(MSOA11CD, RGNNM)

# Rural-urban classifications for LSOAs
ruc = read_csv("https://github.com/matthewgthomas/IMD/raw/master/data/UK%20IMD%20domains.csv") %>% 
  select(LSOA, RUC)

# Output Area to LSOA to MSOA to Local Authority District (December 2017) Lookup with Area Classifications in Great Britain
# source: http://geoportal.statistics.gov.uk/datasets/fe6c55f0924b4734adf1cf7104a0173e_0
lsoa_msoa = read_csv("https://opendata.arcgis.com/datasets/fe6c55f0924b4734adf1cf7104a0173e_0.csv") %>% 
  select(starts_with("LSOA"), starts_with("MSOA")) %>% 
  distinct()

# Calculate proportion of rural/urban LSOAs in each MSOA
ruc_msoa = ruc %>% 
  filter(str_sub(LSOA, 1, 1) == "E") %>% 
  left_join(lsoa_msoa, by = c("LSOA" = "LSOA11CD")) %>% 
  
  tabyl(MSOA11CD, RUC) %>% 
  mutate(Proportion_Urban = Urban / (Urban + Rural)) %>% 
  mutate(RUC = if_else(Urban > 0.5, "Mostly urban", "Mostly rural"))


# ---- Plot life expectancy by IMD, RUC, region ----
le = le %>% 
  select(MSOA11CD = `Area Code`, Sex, `Life expectancy` = Value) %>% 
  left_join(imd, by = "MSOA11CD") %>% 
  left_join(msoa_region, by = "MSOA11CD") %>% 
  left_join(ruc_msoa, by = "MSOA11CD")

le %>%
  filter(Sex == "Male") %>% 
  
  ggplot(aes(x = Score_Decile, y = `Life expectancy`, colour = RGNNM)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm") +
  facet_wrap(~RUC)

