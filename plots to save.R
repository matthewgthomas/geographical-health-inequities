library(tidyverse)

source("regional inequalities.R")

# ---- Male HLE by region and rurality ----
hle %>%
  filter(Sex == "Male") %>% 
  filter(RGNNM %in% c("East of England", "North East")) %>% 
  
  ggplot(aes(x = Score_Decile, y = `Healthy life expectancy`, colour = RGNNM, fill = RGNNM)) +
  geom_smooth(method = "lm") +
  facet_wrap(~RUC) +
  scale_x_continuous(breaks = 1:10) +
  scale_color_manual(values = c("#a6611a", "#018571")) + # Colours from ColorBrewer
  scale_fill_manual(values = c("#dfc27d", "#80cdc1")) +
  labs(
    caption = "Source: British Red Cross analysis of PHE and MHCLG data",
    x = "Deprivation decile (1 = most deprived)", 
    colour = "", 
    fill = ""
  ) +
  theme_classic() +
  theme(legend.position = "bottom")

ggsave("charts/male healthy life expectancy by region and rurality.png", height = 80, width = 150, units = "mm")

# ---- Male HLE by region and rurality ----
hle %>%
  filter(Sex == "Female") %>% 
  filter(RGNNM %in% c("East of England", "North East")) %>% 
  
  ggplot(aes(x = Score_Decile, y = `Healthy life expectancy`, colour = RGNNM, fill = RGNNM)) +
  geom_smooth(method = "lm") +
  facet_wrap(~RUC) +
  scale_x_continuous(breaks = 1:10) +
  scale_color_manual(values = c("#a6611a", "#018571")) + # Colours from ColorBrewer
  scale_fill_manual(values = c("#dfc27d", "#80cdc1")) +
  labs(x = "Deprivation decile (1 = most deprived)", colour = "", fill = "") +
  theme_classic() +
  theme(legend.position = "bottom")

# ---- HLE by deprivation and rurality ----
hle %>%
  ggplot(aes(x = factor(Score_Decile), y = `Healthy life expectancy`, colour = RUC)) +
  geom_violin(draw_quantiles = 0.5) +
  
  facet_wrap(~Sex) +
  
  scale_color_manual(values = c("#a6611a", "#018571")) + # Colours from ColorBrewer
  # scale_fill_manual(values = c("#dfc27d", "#80cdc1")) +
  labs(x = "Deprivation decile (1 = most deprived)", colour = "", fill = "") +
  theme_classic() +
  theme(legend.position = "bottom")

ggsave("charts/healthy life expectancy by deprivation and rurality.png", height = 80, width = 150, units = "mm")
