#To prepare figure for GHG types
#Figure 5
library(tidyverse);library(ggpubr)

#read data
GHG.CH4 <- read.csv("input/Canada GHG storage lit review data 20240611.csv",header = T)
#obtain studies with field measurement 
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
  summarize(
    CO2_count = sum(CO2),
    CH4_count = sum(CH4),
    N2O_count = sum(N2O),
    NH3_count = sum(NH3)) %>%
  ungroup() %>%
  na.omit()

# #Liquid-Solid
GHG_data %>%   
  group_by(Manure.type) %>%
  summarize(
    CO2_count = sum(CO2_count),
    CH4_count = sum(CH4_count),
    N2O_count = sum(N2O_count),
    NH3_count = sum(NH3_count)) %>%
  ungroup() %>%
  na.omit()

# Reshape the data to long format
GHG_stacked <- GHG_data %>%
  pivot_longer(cols = c(CO2_count, N2O_count, CH4_count, NH3_count),
               names_to = "gas", values_to = "count")

# Concatenate ManureType and Gas
GHG_stacked$variable <- paste(GHG_stacked$Manure.type,
                              GHG_stacked$gas, sep = " - ")
GHG_stacked$variable <- str_remove(GHG_stacked$variable, "_count")

#Percentage
GHG_stacked$Percentage <- round(GHG_stacked$count / 
                                  ave(GHG_stacked$count, 
                                      GHG_stacked$Pub..year, FUN = sum) * 100,1)
GHG_stacked <- GHG_stacked[!(GHG_stacked$count == '0'), ]



#Make graph
Figure_GHG_type <- ggplot(GHG_stacked, aes(x = Pub..year, y = count, fill = variable)) +
  geom_col() +
  scale_fill_manual(values = c("Liquid - CH4" = "violet",
                               "Liquid - CO2" = "limegreen",
                               "Liquid - N2O" = "gold",
                               "Liquid - NH3" = "lightpink1",
                               "Solid - CH4" = "darkmagenta",
                               "Solid - CO2" = "darkolivegreen4",
                               "Solid - N2O" = "goldenrod",
                               "Solid - NH3" = "violetred1")) +
  labs(x = "Publication Year", y = "Study Count", fill = "GHG type") +
  theme_classic()+
  theme(axis.text = element_text(size = 14, colour = "black"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = c(0.1, 0.9),  # Adjust the legend position
        legend.justification = c(0, 1),  # Align the legend to top-left
        legend.title = element_text(size = 14),  # Adjust legend title font size
        legend.text = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  theme(axis.title.x = element_text(size = 12)) +
  scale_y_continuous(limits = c(0, 40), 
                     breaks = seq(0, 40, by = 5),
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(1990, 2024), 
                     breaks = seq(1990, 2024 , by = 5),
                     expand = c(0, 0)) +
  geom_text(aes(label = ifelse(Percentage == 0, NA, 
                               paste0(round(Percentage, 0), "%"))),
            position = position_stack(vjust = 0.5),
            size = 2) 
Figure_GHG_type



#Export figure
ggsave("output/Figure - GHG type by solid and liquid manure.png", Figure_GHG_type,
       width = 24, height = 18, units = "cm",
       dpi = 300)
