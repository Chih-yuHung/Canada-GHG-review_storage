#To prepare figures for study period and study length
#Figure 4
library(tidyverse); library(ggpubr)

#read data
GHG.CH4 <- read.csv("input/Canada GHG storage lit review data 20240611.csv",header = T)
#obtain studies with field measurement 
GHG.storage <- GHG.CH4[grepl("Storage", GHG.CH4$GHG.source),]
GHG.storage <- GHG.storage[GHG.storage$CH4 == TRUE,]
GHG.storage <- GHG.storage %>%
  filter(str_detect(Technique, "Micrometeorology")|
           str_detect(Technique, "Incubation")|
           str_detect(Technique, "Animal chamber")|
           str_detect(Technique, "Soil chamber"))
sum(na.omit(GHG.storage$Manure.type == "Liquid, Solid")) #18
sum(na.omit(GHG.storage$Manure.type == "Liquid")) #68
sum(na.omit(GHG.storage$Manure.type == "Solid")) #22
sum(na.omit(GHG.storage$Season == "Growing")) #34
sum(na.omit(GHG.storage$Season == "Non-Growing")) #8
sum(na.omit(GHG.storage$Season == "Year-Round")) #30



#Figure 4a
#study period
season_data <- GHG.storage %>%
  separate_rows(Season, sep = ",\\s*") %>%
  filter(str_detect(Technique, "Micrometeorology")|
           str_detect(Technique, "Incubation")|
           str_detect(Technique, "Animal chamber")|
           str_detect(Technique, "Soil chamber")) %>%
  group_by(Pub..year,Season) %>%
  summarise(Number = n ()) %>%
  ungroup() %>%
  group_by(Pub..year) %>%
  na.omit()%>%
  mutate(Percentage = Number / sum(Number) * 100)



#Make graph
Fig4a  <- ggplot(data = na.omit(season_data), 
                aes(x = Pub..year, y = Number, fill = Season)) +
  geom_bar(stat = "identity") +
  labs(x = "Publication Year", y = "Study Count") +
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
            size = 2) +
  scale_y_continuous(limits = c(0, 8), 
                     breaks = seq(0, 8, by = 2),
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(1990, 2024), 
                     breaks = seq(1990, 2024 , by = 5),
                     expand = c(0, 0)) +
  annotate("text", x = 1991, y = 7.75, 
          label = "(a)",size = 6)
Fig4a

#Figure 4b
#study length
period_data <- GHG.storage %>%
  separate_rows(Period, sep = ",\\s*") %>%
  filter(str_detect(Technique, "Micrometeorology")|
           str_detect(Technique, "Incubation")|
           str_detect(Technique, "Animal chamber")|
           str_detect(Technique, "Soil chamber")) %>%
  group_by(Pub..year,Period) %>%
  summarise(Number = n ()) %>%
  ungroup() %>%
  group_by(Pub..year) %>%
  na.omit()%>%
  mutate(Percentage = Number / sum(Number) * 100)

sum(period_data$Number[period_data$Period == "Single Year"]) #96 88.9%
sum(period_data$Number[period_data$Period == "Multiple Years"]) #12 11.1%



#Figure for period
Fig4b <- ggplot(data = na.omit(period_data), 
                aes(x = Pub..year, y = Number, fill = Period)) +
  geom_bar(stat = "identity") +
  labs(x = "Publication Year", y = "Study Count") +
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
            size = 2) +
  scale_y_continuous(limits = c(0, 10), 
                     breaks = seq(0, 10, by = 2),
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(1990, 2024), 
                     breaks = seq(1990, 2024 , by = 5),
                     expand = c(0, 0)) +
  annotate("text", x = 1991, y = 9.5, 
           label = "(b)",size = 6)
Fig4b

# combine figures together
ggsave("output/Figure - Study period and length.png",
       ggarrange(Fig4a, Fig4b,
                 ncol = 1, nrow = 2),
       width = 24, height = 18, units = "cm",
       dpi = 300)
