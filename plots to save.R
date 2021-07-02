library(tidyverse)

source("regional inequalities.R")

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

ggsave("charts/male healthy life expectancy by region and rurality.png", height = 100, width = 150, units = "mm")

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
