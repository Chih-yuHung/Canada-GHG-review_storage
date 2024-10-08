#To prepare figure for GHG types
#Figure 5
library(tidyverse);library(ggplot2)
library(reshape2);library(scatterpie)
library(stringr);library(ggpubr)
#read data
GHG.CH4 <- read.csv("input/Canada GHG storage lit review data 20240513.csv",header = T)
#obtain studies with field measurement 
GHG.storage <- GHG[grepl("Storage", GHG$GHG.source),]

# Define custom colors, cold for indoor, warm for outdoor
cold_colors <- c("#999999", "#009E73", "#0072B2", "#56B4E9")
warm_colors <- c("#D55E00", "#E69F00", "#F0E442", "#CC79A7")

# #GHG types
sum(GHG.storage$CO2) #61, 44.8%
sum(GHG.storage$CH4) #37, 27.2%
sum(GHG.storage$N2O) #119, 87.5%
sum(GHG.storage$NH3) #15, 

#GHG types 
GHG_data <- GHG.storage %>%
  # filter(str_detect(Technique, "Micrometeorology")|  #Include all methods
  # str_detect(Technique, "Soil chamber")) %>%
  separate_rows(Manure.type, sep = ",\\s*") %>%
  group_by(Pub..year, Manure.type) %>%
  summarize(
    CO2_count = sum(CO2),
    CH4_count = sum(CH4),
    N2O_count = sum(N2O),
    NH3_count = sum(NH3)
  ) %>%
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



Fig5 <- ggplot(GHG_stacked, aes(x = Pub..year, y = count, fill = variable)) +
  geom_col() +
  scale_fill_manual(values = c(warm_colors, cold_colors)) +
  labs(x = "Publication Year", y = "Study Count", fill = "GHG type") +
  theme_classic()+
  theme(axis.text = element_text(size = 14, colour = "black"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position.inside = c(0.1, 0.9),  # Adjust the legend position
        legend.justification = c(0, 1),  # Align the legend to top-left
        legend.title = element_text(size = 14),  # Adjust legend title font size
        legend.text = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  theme(axis.title.x = element_text(size = 12)) +
  scale_y_continuous(limits = c(0, 45), 
                     breaks = seq(0, 45, by = 5),
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(1990, 2023), 
                     breaks = seq(1990, 2023 , by = 5),
                     expand = c(0, 0)) +
  geom_text(aes(label = ifelse(Percentage == 0, NA, 
                               paste0(round(Percentage, 0), "%"))),
            position = position_stack(vjust = 0.5),
            size = 2) 


#To see the relationship between solid and liquid studies
plot(GHG_stacked$Pub..year[GHG_stacked$Manure.type=="Liquid"],
     GHG_stacked$count[GHG_stacked$Manure.type=="Liquid"])

points(GHG_stacked$Pub..year[GHG_stacked$Manure.type=="Solid"],
       GHG_stacked$count[GHG_stacked$Manure.type=="Solid"],
       pch = 17)

ggsave("output/GHG_Types.png" ,
       width = 4800, height = 7200, units = "px",
       dpi = 600)

