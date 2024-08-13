#To prepare figures for study period and study length
#Figure 6
library(tidyverse); library(ggpubr)

#read data
GHG.CH4 <- read.csv("input/Canada GHG storage lit review data 20240611.csv",header = T)



#CH4
#obtain studies with field measurement 
GHG.storage <- GHG.CH4[grepl("Storage", GHG.CH4$GHG.source),]
GHG.storage <- GHG.storage[GHG.storage$CH4 == TRUE,]
GHG.storage <- GHG.storage %>%
  filter(str_detect(Technique, "Micrometeorology")|
           str_detect(Technique, "Animal chamber")|
           str_detect(Technique, "Soil chamber"))
sum(na.omit(GHG.storage$Manure.type == "Liquid, Solid")) #12
sum(na.omit(GHG.storage$Manure.type == "Liquid")) #38
sum(na.omit(GHG.storage$Manure.type == "Solid")) #18
sum(na.omit(GHG.storage$Season == "Growing")) #31
sum(na.omit(GHG.storage$Season == "Non-Growing")) #8
sum(na.omit(GHG.storage$Season == "Year-Round")) #25



#Figure 6c
#study period
season_data <- GHG.storage %>%
  separate_rows(Season, sep = ",\\s*") %>%
  filter(str_detect(Technique, "Micrometeorology")|
           str_detect(Technique, "Animal chamber")|
           str_detect(Technique, "Soil chamber")) %>%
  group_by(Pub..year,Season) %>%
  summarise(Number = n ()) %>%
  ungroup() %>%
  group_by(Pub..year) %>%
  na.omit()%>%
  mutate(Percentage = Number / sum(Number) * 100)



#Make graph
Figure6c  <- ggplot(data = na.omit(season_data), 
                aes(x = Pub..year, y = Number, fill = Season)) +
  geom_bar(stat = "identity") +
  labs(x = "Publication Year", y = "Study Count", title = "(c) CH₄") +
  scale_fill_manual(values = c("Growing" = "limegreen", 
                               "Non-Growing" = "deepskyblue2",
                               "Year-Round" = "violet")) +
  theme_classic() +
  theme(axis.text = element_text(size = 14, colour = "black"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = c(0.1, 0.9),  # Adjust the legend position
        legend.justification = c(0, 1),  # Align the legend to top-left
        legend.title = element_text(size = 14),  # Adjust legend title font size
        legend.text = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  theme(axis.title.x = element_text(size = 12)) +
  geom_text(aes(label = paste0(round(Percentage, 0), "%")),
            position = position_stack(vjust = 0.5),
            angle = 90, size = 3) +
  scale_y_continuous(limits = c(0, 8), 
                     breaks = seq(0, 8, by = 2),
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(1990, 2024), 
                     breaks = seq(1990, 2024 , by = 5),
                     expand = c(0, 0))
Figure6c

#Figure 6a
#study length
period_data <- GHG.storage %>%
  separate_rows(Period, sep = ",\\s*") %>%
  filter(str_detect(Technique, "Micrometeorology")|
           str_detect(Technique, "Animal chamber")|
           str_detect(Technique, "Soil chamber")) %>%
  group_by(Pub..year,Period) %>%
  summarise(Number = n ()) %>%
  ungroup() %>%
  group_by(Pub..year) %>%
  na.omit()%>%
  mutate(Percentage = Number / sum(Number) * 100)

sum(period_data$Number[period_data$Period == "Single Year"]) #57 83.8%
sum(period_data$Number[period_data$Period == "Multiple Years"]) #11 16.2%



#Figure for period
Figure6a <- ggplot(data = na.omit(period_data), 
                aes(x = Pub..year, y = Number, fill = Period)) +
  geom_bar(stat = "identity") +
  labs(x = "Publication Year", y = "Study Count", fill = "Duration", title = "(a) CH₄") +
  scale_fill_manual(values = c("Single Year" = "deepskyblue2",
                               "Multiple Years" = "limegreen")) +
  theme_classic() +
  theme(axis.text = element_text(size = 14, colour = "black"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = c(0.1, 0.9),  # Adjust the legend position
        legend.justification = c(0, 1),  # Align the legend to top-left
        legend.title = element_text(size = 14),  # Adjust legend title font size
        legend.text = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  theme(axis.title.x = element_text(size = 12)) +
  geom_text(aes(label = paste0(round(Percentage, 0), "%")),
            position = position_stack(vjust = 0.5),
            angle = 90, size = 3) +
  scale_y_continuous(limits = c(0, 8), 
                     breaks = seq(0, 8, by = 2),
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(1990, 2024), 
                     breaks = seq(1990, 2024 , by = 5),
                     expand = c(0, 0))
Figure6a



#N2O
#obtain studies with field measurement 
GHG.storage.N2O <- GHG.CH4[grepl("Storage", GHG.CH4$GHG.source),]
GHG.storage.N2O <- GHG.storage.N2O[GHG.storage.N2O$N2O == TRUE,]
GHG.storage.N2O <- GHG.storage.N2O %>%
  filter(str_detect(Technique, "Micrometeorology")|
           str_detect(Technique, "Animal chamber")|
           str_detect(Technique, "Soil chamber"))
sum(na.omit(GHG.storage.N2O$Manure.type == "Liquid, Solid")) #3
sum(na.omit(GHG.storage.N2O$Manure.type == "Liquid")) #18
sum(na.omit(GHG.storage.N2O$Manure.type == "Solid")) #19
sum(na.omit(GHG.storage.N2O$Season == "Growing")) #24
sum(na.omit(GHG.storage.N2O$Season == "Non-Growing")) #6
sum(na.omit(GHG.storage.N2O$Season == "Year-Round")) #9



#Figure 6d
#study period
season_data_N2O <- GHG.storage.N2O %>%
  separate_rows(Season, sep = ",\\s*") %>%
  filter(str_detect(Technique, "Micrometeorology")|
           str_detect(Technique, "Animal chamber")|
           str_detect(Technique, "Soil chamber")) %>%
  group_by(Pub..year,Season) %>%
  summarise(Number = n ()) %>%
  ungroup() %>%
  group_by(Pub..year) %>%
  na.omit()%>%
  mutate(Percentage = Number / sum(Number) * 100)



#Make graph
Figure6d  <- ggplot(data = na.omit(season_data_N2O), 
                 aes(x = Pub..year, y = Number, fill = Season)) +
  geom_bar(stat = "identity") +
  labs(x = "Publication Year", y = "Study Count", title = "(d) N₂O") +
  scale_fill_manual(values = c("Growing" = "limegreen", 
                               "Non-Growing" = "deepskyblue2",
                               "Year-Round" = "violet")) +
  theme_classic() +
  theme(axis.text = element_text(size = 14, colour = "black"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = "none",
        axis.line = element_line(color = "black")) +
  theme(axis.title.x = element_text(size = 12)) +
  geom_text(aes(label = paste0(round(Percentage, 0), "%")),
            position = position_stack(vjust = 0.5),
            angle = 90, size = 3) +
  scale_y_continuous(limits = c(0, 8), 
                     breaks = seq(0, 8, by = 2),
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(1990, 2024), 
                     breaks = seq(1990, 2024 , by = 5),
                     expand = c(0, 0))
Figure6d

#Figure 6b
#study length
period_data_N2O <- GHG.storage.N2O %>%
  separate_rows(Period, sep = ",\\s*") %>%
  filter(str_detect(Technique, "Micrometeorology")|
           str_detect(Technique, "Animal chamber")|
           str_detect(Technique, "Soil chamber")) %>%
  group_by(Pub..year,Period) %>%
  summarise(Number = n ()) %>%
  ungroup() %>%
  group_by(Pub..year) %>%
  na.omit()%>%
  mutate(Percentage = Number / sum(Number) * 100)

sum(period_data_N2O$Number[period_data_N2O$Period == "Single Year"]) #37 92.5%
sum(period_data_N2O$Number[period_data_N2O$Period == "Multiple Years"]) #3 7.5%



#Figure for period
Figure6b <- ggplot(data = na.omit(period_data_N2O), 
                aes(x = Pub..year, y = Number, fill = Period)) +
  geom_bar(stat = "identity") +
  labs(x = "Publication Year", y = "Study Count", fill = "Duration", title = "(b) N₂O") +
  scale_fill_manual(values = c("Single Year" = "deepskyblue2",
                               "Multiple Years" = "limegreen")) +
  theme_classic() +
  theme(axis.text = element_text(size = 14, colour = "black"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = "none",
        axis.line = element_line(color = "black")) +
  theme(axis.title.x = element_text(size = 12)) +
  geom_text(aes(label = paste0(round(Percentage, 0), "%")),
            position = position_stack(vjust = 0.5),
            angle = 90, size = 3) +
  scale_y_continuous(limits = c(0, 8), 
                     breaks = seq(0, 8, by = 2),
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(1990, 2024), 
                     breaks = seq(1990, 2024 , by = 5),
                     expand = c(0, 0))
Figure6b

# combine figures together
ggsave("output/Figure 6 - Study period and length CH4 and N2O.png",
       ggarrange(Figure6a, Figure6b, Figure6c, Figure6d,
                 ncol = 2, nrow = 2),
       width = 24, height = 24, units = "cm",
       dpi = 300)
