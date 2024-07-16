#Articles with CH4, N2O, or both
library(tidyverse)

#Read data
GHG.CH4 <- read.csv("input/Canada GHG storage lit review data 20240611.csv",header = T)

#Obtain studies with storage measurement 
GHG.storage <- GHG.CH4[grepl("Storage", GHG.CH4$GHG.source),]
GHG.storage <- GHG.storage %>%
  subset(GHG.storage$CH4==TRUE | GHG.storage$N2O==TRUE) %>%
  select(Region, Manure.type, CH4, N2O) 
GHG.storage$Region[grep(".*,.*", GHG.storage$Region)] <- "Multiple"
GHG.storage$Region[GHG.storage$Region == 'National'] <- "Multiple"
GHG.storage <- GHG.storage %>%
  mutate(GHG = case_when(
    CH4 & !N2O ~ "CH₄",
    !CH4 & N2O ~ "N₂O",
    CH4 & N2O ~ "CH₄, N₂O",
    TRUE ~ "None"))
GHG.storage <- GHG.storage %>%
  select(-CH4, -N2O) %>%
  drop_na(Manure.type)

#Table for CH4, N2O, and both
GHG.storage <- GHG.storage %>%
  group_by(Region, Manure.type, GHG) %>%
  summarise(Number = n()) %>%
  ungroup() 
GHG.storage$Region <- factor(GHG.storage$Region,
                             levels = c("British Columbia", "Alberta", "Saskatchewan",
                                        "Manitoba", "Ontario","Quebec", "New Brunswick",
                                        "Nova Scotia", "Prince Edward Island",
                                        "Newfoundland", "Multiple"))
GHG.storage$GHG <- factor(GHG.storage$GHG, levels = c("CH₄","N₂O","CH₄, N₂O"))

# Create bubble chart
Figure2 <- ggplot(GHG.storage, aes(x = Region, y = paste(GHG), color = Manure.type, size = Number)) +
  geom_point() +
  labs(x = "Region", y = "GHG", color = "Manure type", size = "Number") +
  scale_color_manual(values = c("Liquid" ="deepskyblue2", "Liquid, Solid" = "goldenrod",
                                "Solid" = "violetred1")) +
  theme_classic() +
  theme(axis.text = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_text(size = 14),  # Adjust legend title font size
        legend.text = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  scale_y_discrete(limits = c("CH₄, N₂O","N₂O","CH₄")) +
  scale_x_discrete(labels = c("BC", "AB","SK","MB","ON", "QC","NB","NS",
                              "PE","NL", "Multiple"),
                   limits = c("British Columbia", "Alberta",
                              "Saskatchewan","Manitoba",
                              "Ontario","Quebec",
                              "New Brunswick",
                              "Nova Scotia",
                              "Prince Edward Island",
                              "Newfoundland", "Multiple")) +
  scale_size_continuous(range = c(3,10)) +
  guides(color = guide_legend(override.aes = list(size = 5)))
Figure2

#Export file
ggsave("output/Figure 2 - Bubble chart articles CH4 and N2O.png", Figure2,
       width = 24, height = 10, units = "cm",
       dpi = 300)
