#Figure for GHG types
#Figure 7
library(tidyverse); library(ggpubr)

#read data
GHG.CH4 <- read.csv("input/Canada GHG storage lit review data 20240611.csv",header = T)
#obtain studies with storage measurement 
GHG.storage <- GHG.CH4[grepl("Storage", GHG.CH4$GHG.source),]

# #GHG types
count(unique(GHG.storage,vars = c(ID, feature)),vars=ID) #181 studies
sum(GHG.storage$CO2)#58, 32.0%
sum(GHG.storage$CH4) #140, 77.3%
sum(GHG.storage$N2O) #72, 39.8%
sum(GHG.storage$NH3) #67, 37.0%

#GHG types 
GHG_data <- GHG.storage %>%
  separate_rows(Manure.type, sep = ",\\s*") %>%
  group_by(Pub..year, Manure.type) %>%
  summarize(CH4_count = sum(CH4),N2O_count = sum(N2O),NH3_count = sum(NH3)) %>%
  ungroup() %>%
  na.omit()

# #Liquid-Solid
GHG_data %>%   
  group_by(Manure.type) %>%
  summarize(CH4_count = sum(CH4_count),N2O_count = sum(N2O_count),NH3_count = sum(NH3_count)) %>%
  ungroup() %>%
  na.omit()

# Reshape the data to long format
GHG_stacked <- GHG_data %>%
  pivot_longer(cols = c(N2O_count, CH4_count, NH3_count),
               names_to = "gas", values_to = "count")
GHG_stacked$gas <- str_remove(GHG_stacked$gas, "_count")

#Set the order for the gases
GHG_stacked$gas <- factor(GHG_stacked$gas,
                          levels = c("CH4", "NH3", "N2O"))
                               
# Liquid-Solid
GHG_liquid <- GHG_stacked[grep("Liquid", GHG_stacked$Manure.type),]
GHG_liquid <- GHG_liquid %>%
  group_by(gas) %>%
  arrange(Pub..year) %>%
  mutate(GHG = cumsum(count))
GHG_solid <- GHG_stacked[grep("Solid", GHG_stacked$Manure.type),]
GHG_solid <- GHG_solid %>%
  group_by(gas) %>%
  arrange(Pub..year) %>%
  mutate(GHG = cumsum(count))



#Make graph
Figure7a <- ggplot(GHG_liquid, aes(x = Pub..year, y = GHG, color = gas)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("CH4" = "darkmagenta",
                               "CO2" = "limegreen",
                               "N2O" = "goldenrod",
                               "NH3" = "violetred1")) +
  labs(x = "Publication Year", y = "Study Count", color = "GHG type", title = "(a) CH₄") +
  theme_classic()+
  theme(axis.text = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = c(0.1, 0.9),  # Adjust the legend position
        legend.justification = c(0, 1),  # Align the legend to top-left
        legend.title = element_text(size = 14),  # Adjust legend title font size
        legend.text = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  theme(axis.title.x = element_text(size = 12)) +
  scale_y_continuous(limits = c(0, 115), 
                     breaks = seq(0, 115, by = 5),
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(1990, 2024), 
                     breaks = seq(1990, 2024 , by = 5),
                     expand = c(0, 0))
Figure7a

Figure7b <- ggplot(GHG_solid, aes(x = Pub..year, y = GHG, color = gas)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("CH4" = "darkmagenta",
                                "CO2" = "limegreen",
                                "N2O" = "goldenrod",
                                "NH3" = "violetred1")) +
  labs(x = "Publication Year", y = "Study Count", color = "GHG type", title = "(b) N₂O") +
  theme_classic()+
  theme(axis.text = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = c(0.1, 0.9),  # Adjust the legend position
        legend.justification = c(0, 1),  # Align the legend to top-left
        legend.title = element_text(size = 14),  # Adjust legend title font size
        legend.text = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  theme(axis.title.x = element_text(size = 12)) +
  scale_y_continuous(limits = c(0, 115), 
                     breaks = seq(0, 115, by = 5),
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(1990, 2024), 
                     breaks = seq(1990, 2024 , by = 5),
                     expand = c(0, 0))
Figure7b


#Export figure
ggsave("output/Figure 7 - GHG (no CO2) type by solid and liquid manure.png", 
       ggarrange(Figure7a, Figure7b,
                 ncol = 2, nrow = 1),
       width = 24, height = 12, units = "cm",
       dpi = 300)
