#To prepare figure for livestock, CH4 only
#Figure 1
library(tidyverse);library(ggpubr)

#read data
GHG.CH4 <- read.csv("input/Canada GHG storage lit review data 20240611.csv",header = T)
#obtain studies with field measurement 
GHG.storage <- GHG.CH4[grepl("Storage", GHG.CH4$GHG.source),]
GHG.storage <- GHG.storage[GHG.storage$CH4==TRUE,]
GHG.storage <- GHG.storage %>%
  drop_na(Livestock)

#Livestock types
livestock_data <- GHG.storage %>%
  separate_rows(Livestock, sep = ",\\s*") %>%
  group_by(Pub..year,Livestock) %>%
  summarise(Number = n()) %>%
  ungroup() %>%
  group_by(Pub..year) %>%
  mutate(Percentage = Number / sum(Number) * 100)
livestock_data$Livestock <- factor(livestock_data$Livestock, 
                                          levels=c("Beef Cattle", "Dairy Cattle",
                                                   "Swine", "Poultry","Horse",
                                                   "Sheep"))

#Livestock count
livestock_count <- GHG.storage %>%
  separate_rows(Livestock, sep = ",\\s*") %>%
  group_by(Livestock) %>%
  summarise(Number = n())

#Figure on livestock
Figure_Livestock <- ggplot(data = na.omit(livestock_data), 
                aes(x = Pub..year, y = Number, fill = Livestock, group = Livestock)) +
  geom_bar(stat = "identity",
           linetype = 0) +
  labs(x = "Publication Year", y = "Study Count", fill = "Animal type") +
  scale_fill_manual(values = c("Beef Cattle" = "violetred1", "Dairy Cattle" = "deepskyblue2", 
                                "Swine" = "lightpink1", "Poultry" = "goldenrod",
                                "Horse" = "darkmagenta", "Sheep" = "limegreen")) +
  theme_classic() +
  theme(axis.text = element_text(size = 14, color = "black"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = c(0.1, 0.9),  # Adjust the legend position
        legend.justification = c(0, 1),  # Align the legend to top-left
        legend.title = element_text(size = 14),  # Adjust legend title font size
        legend.text = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  geom_text(aes(label = paste0(round(Percentage, 0), "%")),
            position = position_stack(vjust = 0.5),
            size = 3,
            color = "black") +
  scale_y_continuous(limits = c(0, 18), 
                     breaks = seq(0, 18, by = 2),
                     expand = c(0, 0))+
  scale_x_continuous(limits = c(1990, 2024), 
                     breaks = seq(1990, 2024 , by = 5),
                     expand = c(0, 0))

Figure_Livestock

#Export figure
ggsave("output/Figure - Livestock.png", Figure_Livestock,
       width = 24, height = 18, units = "cm",
       dpi = 300)
