#To create Bubble chart to know the removal time and spatial distribution
library(tidyverse); library(ggplot2)
library(openxlsx2)

#The color in this study
cold_colors <- c("#999999", "#009E73", "#0072B2", "#56B4E9")
warm_colors <- c("#D55E00", "#E69F00", "#F0E442", "#CC79A7")

#read data
GHG.CH4 <- read.csv("input/Canada GHG storage lit review data 20240611.csv",header = T)

#obtain studies with field measurement 
GHG.storage <- GHG.CH4[grepl("Storage", GHG.CH4$GHG.source),]
GHG.storage <- GHG.storage[grepl("Removal", GHG.storage$Treatment),]
GHG.storage <- GHG.storage[GHG.storage$CH4==TRUE,]
#replace "National" to every provinces
National <- c('British Columbia, Alberta, Saskatchewan, Manitoba, Ontario, Quebec, New Brunswick, Nova Scotia, Prince Edward Island, Newfoundland')
GHG.storage$Region[GHG.storage$Region == "National"] <- National

#Removal timing
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

# Create bubble chart
ggplot(GHG.removal, aes(x = Region, y = paste(Removal.season), color = Manure.type, size = Count)) +
  geom_point(alpha = 0.7) +
  labs(x = "Region", y = "Removal season", color = "Manure Type", size = "Count") +
  scale_color_manual(values = c("deepskyblue2", "gold")) +
  theme_classic()+
  scale_x_discrete(labels = c("BC", "AB","SK","MB",
                              "ON", "QC","NB","NS",
                              "PE","NL"))+
  guides(color = guide_legend(override.aes = list(size = 4)))

#Export file
write_xlsx(GHG.removal,
           file = "output/Removal time.xlsx",
           col.names = T, row.names = T)
ggsave("output/Figure - Bubble chart manure removal CH4.png",
       width = 24, height = 12, units = "cm",
       dpi = 300)
