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
#No treatment #63 27.4%, Acidification #5 2.2%, Additives #1 0.4%, Agitation #21 9.1%, 
#Antibiotics #3 1.3%, Compost #22 9.6%, Cover #4 1.7%, Digester #55 23.9%, 
#Hydrolysis #1 0.4%, Nutrient recovery #1 0.4%, Removal #21 9.1%, 
#Solid-Liquid Separation #20 8.7%, Stockpile #13 5.7%, Total #230 100%

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
#No treatment #40 27.0%, Acidification #4 2.7%, Additives #1 0.7%, Agitation #18 12.2%, 
#Antibiotics #3 2.0%, Compost #3 2.0%, Cover #3 2.0%, Digester #42 28.4%, 
#Hydrolysis #0 0.0%, Nutrient recovery #1 0.7%, Removal #17 11.5%, 
#Solid-Liquid Separation #16 10.8%, Stockpile #0 0%, Total #148 100%

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
#No treatment #24 28.2%, Acidification #1 1.2%, Additives #0 0.0%, Agitation #3 3.5%, 
#Antibiotics #0 0.0%, Compost #19 22.4%, Cover #1 1.2%, Digester #13 15.3%, 
#Hydrolysis #1 1.2%, Nutrient recovery #0 0.0%, Removal #4 4.7%, 
#Solid-Liquid Separation #4 4.7%, Stockpile #15 17.6%, Total #85 100%



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
#No treatment #44 38.6%, Acidification #4 3.5%, Additives #0 0.0%, Agitation #8 7.0%, 
#Antibiotics #0 0.0%, Compost #19 16.7%, Cover #4 3.5%, Digester #7 6.1%, 
#Hydrolysis #0 0.0%, Nutrient recovery #1 0.9%, Removal #6 5.3%, 
#Solid-Liquid Separation #7 6.1%, Stockpile #14 12.3%, Total #114 100%

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
#No treatment #27 50.0%, Acidification #3 5.6%, Additives #0 0.0%, Agitation #7 13.0%, 
#Antibiotics #0 0.0%, Compost #1 1.9%, Cover #3 5.6%, Digester #5 9.3%, 
#Hydrolysis #0 0.0%, Nutrient recovery #1 1.9%, Removal #3 5.6%, 
#Solid-Liquid Separation #4 7.4%, Stockpile #0 0%, Total #54 100%

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
#No treatment #16 27.1%, Acidification #1 1.7%, Additives #0 0.0%, Agitation #1 1.7%, 
#Antibiotics #0 0.0%, Compost #18 30.5%, Cover #1 1.7%, Digester #2 3.4%, 
#Hydrolysis #0 0.0%, Nutrient recovery #0 0.0%, Removal #3 5.1%, 
#Solid-Liquid Separation #3 5.1%, Stockpile #14 23.7%, Total #59 100%



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
                     breaks = seq(0, 42, by = 5),
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(1990, 2024), 
                     breaks = seq(1990, 2024 , by = 5),
                     expand = c(0, 0))
Figure5a

Figure5c <- ggplot(Treat_data_solid, aes(x = Pub..year, y = Treat, color = Treatment)) +
  geom_line() +
  geom_point() +
  labs(x = "Publication Year", y = "Study count", title = "(c) CH₄ from solid manure") +
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
                     breaks = seq(0, 42, by = 5),
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(1990, 2024), 
                     breaks = seq(1990, 2024 , by = 5),
                     expand = c(0, 0))
Figure5c

Figure5b <- ggplot(Treat_data_liquid_N2O, aes(x = Pub..year, y = Treat, color = Treatment)) +
  geom_line() +
  geom_point() +
  labs(x = "Publication Year", y = "Study count", title = "(b) N₂O from liquid manure") +
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
                     breaks = seq(0, 42, by = 5),
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(1990, 2024), 
                     breaks = seq(1990, 2024 , by = 5),
                     expand = c(0, 0))
Figure5b

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
                     breaks = seq(0, 42, by = 5),
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
