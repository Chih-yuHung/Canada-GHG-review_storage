#Figure 8 Bubble chart for removal time and spatial distribution
library(tidyverse);library(ggpubr)

#read data
GHG.CH4 <- read.csv("input/Canada GHG storage lit review data 20240611.csv",header = T)

#CH4
#obtain studies with field measurement 
GHG.storage <- GHG.CH4[grepl("Storage", GHG.CH4$GHG.source),]
GHG.storage <- GHG.storage[grepl("Removal", GHG.storage$Treatment),]
GHG.storage <- GHG.storage %>%
  subset(GHG.storage$CH4==TRUE | GHG.storage$N2O==TRUE)
#replace "National" to every provinces
National <- c('British Columbia, Alberta, Saskatchewan, Manitoba, Ontario, Quebec, New Brunswick, Nova Scotia, Prince Edward Island, Newfoundland')
GHG.storage$Region[GHG.storage$Region == "National"] <- National

#Removal timing CH4
GHG.removal <- GHG.storage %>%
  separate_rows(Region, sep =", ") %>%
  separate_rows(Manure.type, sep = ", ") %>%
  separate_rows(Removal.season, sep = ", ")
GHG.removal$Removal.frequency[GHG.removal$Removal.frequency %in% c("4", "5", "7", "104")] <- "> 3"
GHG.removal <- GHG.removal %>%
  group_by(Region, Removal.season, Manure.type) %>%
  summarise('Count' = n()) %>%
  ungroup()

GHG.removal$Region <- factor(GHG.removal$Region,
                                 levels = c("British Columbia", "Alberta",
                                            "Saskatchewan","Manitoba",
                                            "Ontario","Quebec",
                                            "New Brunswick",
                                            "Nova Scotia",
                                            "Prince Edward Island",
                                            "Newfoundland"))
GHG.removal$Removal.season <- factor(GHG.removal$Removal.season,
                                     levels = c("Spring", "Summer", "Fall", "Not specified"))



#N2O
GHG.storage.N2O <- GHG.CH4[grepl("Storage", GHG.CH4$GHG.source),]
GHG.storage.N2O <- GHG.storage.N2O[GHG.storage.N2O$N2O==TRUE,]
GHG.storage.N2O <- GHG.storage.N2O[grepl("Removal", GHG.storage.N2O$Treatment),]

#Removal timing N2O
GHG.removal.N2O <- GHG.storage.N2O %>%
  separate_rows(Region, sep =", ") %>%
  separate_rows(Manure.type, sep = ", ") %>%
  separate_rows(Removal.season, sep = ", ")
GHG.removal.N2O$Removal.frequency[GHG.removal.N2O$Removal.frequency %in% c("4", "5", "7", "104")] <- "> 3"
GHG.removal.N2O <- GHG.removal.N2O %>%
  group_by(Region, Removal.season, Manure.type) %>%
  summarise('Count' = n()) %>%
  ungroup()

GHG.removal.N2O$Region <- factor(GHG.removal.N2O$Region,
                             levels = c("British Columbia", "Alberta",
                                        "Saskatchewan","Manitoba",
                                        "Ontario","Quebec",
                                        "New Brunswick",
                                        "Nova Scotia",
                                        "Prince Edward Island",
                                        "Newfoundland"))
GHG.removal.N2O$Removal.season <- factor(GHG.removal.N2O$Removal.season,
                                     levels = c("Spring", "Summer", "Fall", "Not specified"))



# Create bubble chart
Figure8a <- ggplot(GHG.removal, aes(x = Region, y = paste(Removal.season), color = Manure.type, size = Count)) +
  geom_point(alpha = 0.7) +
  labs(x = "Region", y = "Removal season", color = "Manure Type", size = "Count", title = "(a) CH₄") +
  scale_color_manual(values = c("deepskyblue2", "goldenrod")) +
  theme_classic() +
  theme(axis.text = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_text(size = 14),  # Adjust legend title font size
        legend.text = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  scale_y_discrete(limits = c("Not specified", "Fall", "Summer", "Spring")) +
  scale_x_discrete(labels = c("BC", "AB","SK","MB",
                              "ON", "QC","NB","NS",
                              "PE","NL")) +
  scale_size_continuous(range = c(3,9)) +
  guides(color = guide_legend(override.aes = list(size = 5)))
Figure8a

Figure8b <- ggplot(GHG.removal.N2O, aes(x = Region, y = paste(Removal.season), color = Manure.type, size = Count)) +
  geom_point(alpha = 0.7) +
  labs(x = "Region", y = "Removal season", color = "Manure Type", size = "Count", title = "(b) N₂O") +
  scale_color_manual(values = c("deepskyblue2", "goldenrod")) +
  theme_classic() +
  theme(axis.text = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_text(size = 14),  # Adjust legend title font size
        legend.text = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  scale_y_discrete(limits = c("Not specified", "Fall", "Summer", "Spring")) +
  scale_x_discrete(labels = c("BC", "AB","SK","MB",
                              "ON", "QC","NB","NS",
                              "PE","NL"),
                   limits = c("British Columbia", "Alberta",
                              "Saskatchewan","Manitoba",
                              "Ontario","Quebec",
                              "New Brunswick",
                              "Nova Scotia",
                              "Prince Edward Island",
                              "Newfoundland")) +
  scale_size_continuous(range = c(3,5),
                        breaks = c(1, 2)) +
  guides(color = guide_legend(override.aes = list(size = 5)))
Figure8b

#Export file
ggsave("output/Figure 8 - Bubble chart manure removal CH4 and N2O.png",
       ggarrange(Figure8a, Figure8b,
                 ncol = 1, nrow = 2,
                 common.legend = TRUE, legend = "bottom"),
       width = 24, height = 18, units = "cm",
       dpi = 300)
