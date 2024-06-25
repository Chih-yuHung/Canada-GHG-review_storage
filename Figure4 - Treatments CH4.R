#To prepare figure for treatments, CH4 only
#Figure 4
library(tidyverse); library(ggpubr)

#read data
GHG.CH4 <- read.csv("input/Canada GHG storage lit review data 20240611.csv",header = T)
#obtain studies from storage measurement 
GHG.storage <- GHG.CH4[grepl("Storage", GHG.CH4$GHG.source),]
GHG.storage <- GHG.storage[GHG.storage$CH4 == TRUE,]

#Treatments 
Treat_data <- GHG.storage %>%
  separate_rows(Treatment, sep = ",\\s*") %>%
  separate_rows(Manure.type, sep = ",\\s*") %>%
  group_by(Pub..year, Manure.type, Treatment, ID)
Treat_data <- Treat_data[!(Treat_data$ID %in% c(79,100,101,217) & Treat_data$Manure.type == "Liquid" & Treat_data$Treatment == "Compost"), ] 
Treat_data <- Treat_data[!(Treat_data$ID == '79' & Treat_data$Manure.type == "Solid" & Treat_data$Treatment == "Agitation"), ]
Treat_data <- Treat_data[!(Treat_data$ID == '101' & Treat_data$Manure.type == "Liquid" & Treat_data$Treatment == "Stockpile"), ]
Treat_data[Treat_data$ID %in% c(70,217,225) & Treat_data$Manure.type == "Liquid", "Treatment"] <- 'No treatment'
Treat_data$Treatment[Treat_data$Treatment == "Antibiotic"] <- "Antibiotics"
Treat_data <- Treat_data %>%
  group_by(Pub..year, Manure.type, Treatment) %>%
  summarise(Number = n()) %>%
  ungroup()
Treat_data <- Treat_data %>% 
  mutate(Treatment = as.character(Treatment),
         Treatment = ifelse(is.na(Treatment), "No treatment", Treatment),
         Treatment = as.factor(Treatment))
Treat_data[Treat_data=="Digester "] <- "Digester"
Treat_data[Treat_data=="Solid-Liquid Separation "] <- "Solid-Liquid Separation"


#Set the order for the techniques
Treat_data$Treatment <- factor(Treat_data$Treatment, 
                               levels = c("No treatment", "Acidification", "Additives", 
                                          "Agitation","Antibiotics","Compost", "Cover", 
                                          "Digester", "Hydrolysis","Nutrient recovery",
                                          "Removal","Solid-Liquid Separation","Stockpile"))

tapply(Treat_data$Number,Treat_data$Treatment,sum)
#Changes in Excel
#No treatment #50 22.8%, Acidification #5 2.3%, Additives #1 0.5%, Agitation #22 10.0%, 
#Antibiotics #3 1.4%, Compost #22 10.0%, Cover #5 2.3%, Digester #55 25.1%, 
#Hydrolysis #1 0.5%, Nutrient recovery #1 0.5%, Removal #21 9.6%, 
#Solid-Liquid Separation #20 9.1%, Stockpile #13 5.9%, Total #219 100%

#separate to liquid and solid manure.
Treat_data_liquid <- Treat_data[grep("Liquid", Treat_data$Manure.type),]
Treat_data_liquid <- Treat_data_liquid %>%
  group_by(Pub..year) %>%
  mutate(Percentage = Number / sum(Number) * 100) %>%
  ungroup() %>%
  group_by(Treatment) %>%
  arrange(Pub..year) %>%
  mutate(Treat = cumsum(Number))
tapply(Treat_data_liquid$Number,Treat_data_liquid$Treatment,sum)
#Changes in Excel
#No treatment #17 19.3%, Acidification #3 3.4%, Additives #1 1.1%, Agitation #14 15.9%, 
#Antibiotics #3 3.4%, Compost #3 3.4%, Cover #4 4.5%, Digester #19 21.6%, 
#Hydrolysis #0 0.0%, Nutrient recovery #1 1.1%, Removal #11 12.5%, 
#Solid-Liquid Separation #12 13.6%, Stockpile #0 0%, Total #88 100%

Treat_data_solid <- Treat_data[grep("Solid", Treat_data$Manure.type),]
Treat_data_solid <- Treat_data_solid %>%
  group_by(Pub..year) %>%
  mutate(Percentage = Number / sum(Number) * 100) %>%
  ungroup() %>%
  group_by(Treatment) %>%
  arrange(Pub..year) %>%
  mutate(Treat = cumsum(Number))
tapply(Treat_data_solid$Number,Treat_data_solid$Treatment,sum)
#Changes in Excel
#No treatment #17 19.3%, Acidification #3 3.4%, Additives #1 1.1%, Agitation #14 15.9%, 
#Antibiotics #3 3.4%, Compost #3 3.4%, Cover #4 4.5%, Digester #19 21.6%, 
#Hydrolysis #0 0.0%, Nutrient recovery #1 1.1%, Removal #11 12.5%, 
#Solid-Liquid Separation #12 13.6%, Stockpile #0 0%, Total #88 100%

#Make graph
Figure4a <- ggplot(Treat_data_liquid, aes(x = Pub..year, y = Treat, color = Treatment)) +
  geom_line() +
  geom_point() +
  labs(x = "Publication Year", y = "Study count") +
  scale_color_manual(values = c("No treatment" = "black", "Acidification" = "turquoise1", 
                               "Additives" = "deepskyblue2" , "Agitation" = "violet",
                               "Antibiotics" = "darkmagenta","Compost" = "violetred1",
                               "Cover" = "lightpink1", "Digester" = "tomato1", 
                               "Hydrolysis" = "goldenrod","Nutrient recovery" = "gold",
                               "Removal" = "limegreen","Solid-Liquid Separation" = "darkolivegreen4",
                               "Stockpile" = "lightsalmon4")) +
  theme_classic() +
  theme(axis.text = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = c(0.1, 1),  # Adjust the legend position
        legend.justification = c(0, 1),  # Align the legend to top-left
        legend.title = element_text(size = 14),  # Adjust legend title font size
        legend.text = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  theme(axis.title.x = element_text(size = 12)) +
  scale_y_continuous(limits = c(0, 44), 
                     breaks = seq(0, 44, by = 2),
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(1990, 2024), 
                     breaks = seq(1990, 2024 , by = 5),
                     expand = c(0, 0)) +
  annotate("text", x = 1992, y = 42, 
           label = "(a)",size = 6)
Figure4a

Figure4b <- ggplot(Treat_data_solid, aes(x = Pub..year, y = Treat, color = Treatment)) +
  geom_line() +
  geom_point() +
  labs(x = "Publication Year", y = "Study count") +
  scale_color_manual(values = c("No treatment" = "black", "Acidification" = "turquoise1", 
                               "Additives" = "deepskyblue2" , "Agitation" = "violet",
                               "Antibiotics" = "darkmagenta","Compost" = "violetred1",
                               "Cover" = "lightpink1", "Digester" = "tomato1", 
                               "Hydrolysis" = "goldenrod","Nutrient recovery" = "gold",
                               "Removal" = "limegreen","Solid-Liquid Separation" = "darkolivegreen4",
                               "Stockpile" = "lightsalmon4")) +
  theme_classic() +
  theme(axis.text = element_text(size = 14, colour = "black"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = c(0.1, 1),  # Adjust the legend position
        legend.justification = c(0, 1),  # Align the legend to top-left
        legend.title = element_text(size = 14),  # Adjust legend title font size
        legend.text = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  theme(axis.title.x = element_text(size = 12)) +
  scale_y_continuous(limits = c(0, 25), 
                     breaks = seq(0, 25, by = 2),
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(1990, 2024), 
                     breaks = seq(1990, 2024 , by = 5),
                     expand = c(0, 0)) +
  annotate("text", x = 1992, y = 23, 
           label = "(b)",size = 6)
Figure4b



#Export figure
ggsave("output/Figure4 - Treatments (solid or liquid) CH4.png",
       ggarrange(Figure4a, Figure4b,
                 ncol = 1, nrow = 2),
       width = 24, height = 24, units = "cm",
       dpi = 300)
