#To prepare figure for treatments, CH4 and N2O
#Figure 5
library(tidyverse); library(ggpubr)

#read data
GHG.CH4 <- read.csv("input/Canada GHG storage lit review data 20240611.csv",header = T)
#obtain studies from storage measurement 
GHG.storage <- GHG.CH4[grepl("Storage", GHG.CH4$GHG.source),]
GHG.storage <- GHG.storage %>%
  subset(GHG.storage$CH4==TRUE | GHG.storage$N2O==TRUE)

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
Treat_data[Treat_data=="Digester "] <- "Digester"
Treat_data[Treat_data=="Solid-Liquid Separation "] <- "Solid-Liquid Separation"
Treat_data <- Treat_data %>% 
  mutate(Treatment = as.character(Treatment),
         Treatment = ifelse(is.na(Treatment), "No treatment", Treatment),
         Treatment = as.factor(Treatment))



#CH4
GHG.storage.CH4 <- Treat_data[Treat_data$CH4 == TRUE,]

#Total treatments 
Treat_data_CH4 <- GHG.storage.CH4 %>%
  group_by(Pub..year, Manure.type, Treatment) %>%
  summarise(Number = n()) %>%
  ungroup()

#Set the order for the techniques
Treat_data_CH4$Treatment <- factor(Treat_data_CH4$Treatment, 
                                   levels = c("No treatment", "Acidification", "Additives", 
                                              "Agitation","Antibiotics","Compost", "Cover", 
                                              "Digester", "Hydrolysis","Nutrient recovery",
                                              "Removal","Solid-Liquid Separation","Stockpile"))

tapply(Treat_data_CH4$Number,Treat_data_CH4$Treatment,sum)

#separate to liquid and solid manure.
Treat_data_liquid <- GHG.storage.CH4[grep("Liquid", GHG.storage.CH4$Manure.type),]
Treat_data_liquid <- Treat_data_liquid %>%
  group_by(Pub..year, Manure.type, Treatment) %>%
  summarise(Number = n()) %>%
  ungroup()
Treat_data_liquid <- Treat_data_liquid %>%
  group_by(Pub..year) %>%
  mutate(Percentage = Number / sum(Number) * 100) %>%
  ungroup() %>%
  group_by(Treatment) %>%
  arrange(Pub..year) %>%
  mutate(Treat = cumsum(Number))
Treat_data_liquid$Treatment <- factor(Treat_data_liquid$Treatment, 
                                     levels = c("No treatment", "Acidification", "Additives", 
                                                "Agitation","Antibiotics","Compost", "Cover", 
                                                "Digester", "Hydrolysis","Nutrient recovery",
                                                "Removal","Solid-Liquid Separation","Stockpile"))
tapply(Treat_data_liquid$Number,Treat_data_liquid$Treatment,sum)
#Changes in Excel
#No treatment #17 19.3%, Acidification #3 3.4%, Additives #1 1.1%, Agitation #14 15.9%, 
#Antibiotics #3 3.4%, Compost #3 3.4%, Cover #4 4.5%, Digester #19 21.6%, 
#Hydrolysis #0 0.0%, Nutrient recovery #1 1.1%, Removal #11 12.5%, 
#Solid-Liquid Separation #12 13.6%, Stockpile #0 0%, Total #88 100%

Treat_data_solid <- Treat_data[grep("Solid", Treat_data$Manure.type),]
Treat_data_solid <- Treat_data_solid %>%
  group_by(Pub..year, Manure.type, Treatment) %>%
  summarise(Number = n()) %>%
  ungroup()
Treat_data_solid <- Treat_data_solid %>%
  group_by(Pub..year) %>%
  mutate(Percentage = Number / sum(Number) * 100) %>%
  ungroup() %>%
  group_by(Treatment) %>%
  arrange(Pub..year) %>%
  mutate(Treat = cumsum(Number))
Treat_data_solid$Treatment <- factor(Treat_data_solid$Treatment, 
                                   levels = c("No treatment", "Acidification", "Additives", 
                                              "Agitation","Antibiotics","Compost", "Cover", 
                                              "Digester", "Hydrolysis","Nutrient recovery",
                                              "Removal","Solid-Liquid Separation","Stockpile"))
tapply(Treat_data_solid$Number,Treat_data_solid$Treatment,sum)
#Changes in Excel
#No treatment #17 19.3%, Acidification #3 3.4%, Additives #1 1.1%, Agitation #14 15.9%, 
#Antibiotics #3 3.4%, Compost #3 3.4%, Cover #4 4.5%, Digester #19 21.6%, 
#Hydrolysis #0 0.0%, Nutrient recovery #1 1.1%, Removal #11 12.5%, 
#Solid-Liquid Separation #12 13.6%, Stockpile #0 0%, Total #88 100%



#N2O
GHG.storage.N2O <- Treat_data[Treat_data$N2O == TRUE,]

#Total treatments 
Treat_data_N2O <- GHG.storage.N2O %>%
  group_by(Pub..year, Manure.type, Treatment) %>%
  summarise(Number = n()) %>%
  ungroup()

#Set the order for the techniques
Treat_data_N2O$Treatment <- factor(Treat_data_N2O$Treatment, 
                               levels = c("No treatment", "Acidification", "Additives", 
                                          "Agitation","Antibiotics","Compost", "Cover", 
                                          "Digester", "Hydrolysis","Nutrient recovery",
                                          "Removal","Solid-Liquid Separation","Stockpile"))

tapply(Treat_data_N2O$Number,Treat_data_N2O$Treatment,sum)
#Changes in Excel
#No treatment #41 37.6%, Acidification #4 3.7%, Additives #0 0.0%, Agitation #8 7.3%, 
#Antibiotics #0 0.0%, Compost #19 17.4%, Cover #4 3.7%, Digester #7 6.4%, 
#Hydrolysis #0 0.0%, Nutrient recovery #1 0.9%, Removal #6 5.5%, 
#Solid-Liquid Separation #7 6.4%, Stockpile #12 11.0%, Total #109 100%

#separate to liquid and solid manure.
Treat_data_liquid_N2O <- GHG.storage.N2O[grep("Liquid", GHG.storage.N2O$Manure.type),]
Treat_data_liquid_N2O <- Treat_data_liquid_N2O %>%
  group_by(Pub..year, Manure.type, Treatment) %>%
  summarise(Number = n()) %>%
  ungroup()
Treat_data_liquid_N2O <- Treat_data_liquid_N2O %>%
  group_by(Pub..year) %>%
  mutate(Percentage = Number / sum(Number) * 100) %>%
  ungroup() %>%
  group_by(Treatment) %>%
  arrange(Pub..year) %>%
  mutate(Treat = cumsum(Number))
Treat_data_liquid_N2O$Treatment <- factor(Treat_data_liquid_N2O$Treatment, 
                                   levels = c("No treatment", "Acidification", "Additives", 
                                              "Agitation","Antibiotics","Compost", "Cover", 
                                              "Digester", "Hydrolysis","Nutrient recovery",
                                              "Removal","Solid-Liquid Separation","Stockpile"))
tapply(Treat_data_liquid_N2O$Number,Treat_data_liquid_N2O$Treatment,sum)
#Changes in Excel
#No treatment #26 49.1%, Acidification #3 5.7%, Additives #0 0.0%, Agitation #7 13.2%, 
#Antibiotics #0 0.0%, Compost #1 1.9%, Cover #3 5.7%, Digester #5 9.4%, 
#Hydrolysis #0 0.0%, Nutrient recovery #1 1.9%, Removal #3 5.7%, 
#Solid-Liquid Separation #4 7.5%, Stockpile #0 0%, Total #53 100%

Treat_data_solid_N2O <- GHG.storage.N2O[grep("Solid", GHG.storage.N2O$Manure.type),]
Treat_data_solid_N2O <- Treat_data_solid_N2O %>%
  group_by(Pub..year, Manure.type, Treatment) %>%
  summarise(Number = n()) %>%
  ungroup()
Treat_data_solid_N2O <- Treat_data_solid_N2O %>%
  group_by(Pub..year) %>%
  mutate(Percentage = Number / sum(Number) * 100) %>%
  ungroup() %>%
  group_by(Treatment) %>%
  arrange(Pub..year) %>%
  mutate(Treat = cumsum(Number))
Treat_data_solid_N2O$Treatment <- factor(Treat_data_solid_N2O$Treatment, 
                                         levels = c("No treatment", "Acidification", "Additives", 
                                                    "Agitation","Antibiotics","Compost", "Cover", 
                                                    "Digester", "Hydrolysis","Nutrient recovery",
                                                    "Removal","Solid-Liquid Separation","Stockpile"))
tapply(Treat_data_solid_N2O$Number,Treat_data_solid_N2O$Treatment,sum)
#Changes in Excel
#No treatment #17 26.8%, Acidification #3 1.8%, Additives #0 0.0%, Agitation #1 1.8%, 
#Antibiotics #0 0.0%, Compost #18 32.1%, Cover #1 1.8%, Digester #2 3.6%, 
#Hydrolysis #0 0.0%, Nutrient recovery #0 0.0%, Removal #3 5.4%, 
#Solid-Liquid Separation #3 5.4%, Stockpile #12 21.4%, Total #56 100%



#Count technique for removal
Treat_data_removal_CH4 <- Treat_data[grep("Removal", Treat_data$Treatment),]
Treat_data_removal_CH4 <- Treat_data_removal_CH4 %>%
  group_by(Technique) %>%
  summarise(Number = n()) %>%
  ungroup()



#Make graph
Figure5a <- ggplot(Treat_data_liquid, aes(x = Pub..year, y = Treat, color = Treatment)) +
  geom_line(show.legend = TRUE) +
  geom_point(show.legend = TRUE) +
  labs(x = "Publication Year", y = "Study count", title = "(a) CH₄ from liquid manure") +
  scale_color_manual(values = c("No treatment" = "black", "Acidification" = "turquoise1", 
                               "Additives" = "deepskyblue2" , "Agitation" = "violet",
                               "Antibiotics" = "darkmagenta","Compost" = "violetred1",
                               "Cover" = "lightpink1", "Digester" = "tomato1", 
                               "Hydrolysis" = "goldenrod","Nutrient recovery" = "gold",
                               "Removal" = "limegreen","Solid-Liquid Separation" = "darkolivegreen4",
                               "Stockpile" = "lightsalmon4"),
                     drop = FALSE) +
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
  scale_y_continuous(limits = c(0, 42), 
                     breaks = seq(0, 42, by = 2),
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(1990, 2024), 
                     breaks = seq(1990, 2024 , by = 5),
                     expand = c(0, 0))
Figure5a

Figure5b <- ggplot(Treat_data_solid, aes(x = Pub..year, y = Treat, color = Treatment)) +
  geom_line() +
  geom_point() +
  labs(x = "Publication Year", y = "Study count", title = "(b) CH₄ from solid manure") +
  scale_color_manual(values = c("No treatment" = "black", "Acidification" = "turquoise1", 
                               "Additives" = "deepskyblue2" , "Agitation" = "violet",
                               "Antibiotics" = "darkmagenta","Compost" = "violetred1",
                               "Cover" = "lightpink1", "Digester" = "tomato1", 
                               "Hydrolysis" = "goldenrod","Nutrient recovery" = "gold",
                               "Removal" = "limegreen","Solid-Liquid Separation" = "darkolivegreen4",
                               "Stockpile" = "lightsalmon4"),
                     drop = FALSE) +
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
  scale_y_continuous(limits = c(0, 42), 
                     breaks = seq(0, 42, by = 2),
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(1990, 2024), 
                     breaks = seq(1990, 2024 , by = 5),
                     expand = c(0, 0))
Figure5b

Figure5c <- ggplot(Treat_data_liquid_N2O, aes(x = Pub..year, y = Treat, color = Treatment)) +
  geom_line() +
  geom_point() +
  labs(x = "Publication Year", y = "Study count", title = "(c) N₂O from liquid manure") +
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
  scale_y_continuous(limits = c(0, 42), 
                     breaks = seq(0, 42, by = 2),
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(1990, 2024), 
                     breaks = seq(1990, 2024 , by = 5),
                     expand = c(0, 0))
Figure5c

Figure5d <- ggplot(Treat_data_solid_N2O, aes(x = Pub..year, y = Treat, color = Treatment)) +
  geom_line() +
  geom_point() +
  labs(x = "Publication Year", y = "Study count", title = "(d) N₂O from solid manure") +
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
  scale_y_continuous(limits = c(0, 42), 
                     breaks = seq(0, 42, by = 2),
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(1990, 2024), 
                     breaks = seq(1990, 2024 , by = 5),
                     expand = c(0, 0)) 
Figure5d



#Export figure
ggsave("output/Figure 5 - Treatments (solid or liquid) CH4 and N2O.png",
       ggarrange(Figure5a, Figure5b, Figure5c, Figure5d,
                 ncol = 2, nrow = 2,
                 common.legend = TRUE, legend = "bottom"),
       width = 24, height = 24, units = "cm",
       dpi = 300)
