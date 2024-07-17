#To prepare figure for techniques, CH4 and N2O
#Figure 4
library(tidyverse); library(ggpubr)

#CH4
#read data
GHG.CH4 <- read.csv("input/Canada GHG storage lit review data 20240611.csv",header = T)
#obtain studies from storage measurement 
GHG.storage <- GHG.CH4[grepl("Storage", GHG.CH4$GHG.source),]
GHG.storage <- GHG.storage %>%
  drop_na(Livestock)
GHG.storage[GHG.storage$ID==9,"Technique"] <- 'Incubation'
GHG.storage[GHG.storage$ID==33,"Technique"] <- 'Incubation'
GHG.storage[GHG.storage$ID==225,"Technique"] <- 'Soil chamber'
GHG.storage$Collection.Setting[GHG.storage$Collection.Setting == "Mixed"] <- "Indoor, Outdoor"
#replace "National" to every provinces
National <- c('British Columbia, Alberta, Saskatchewan, Manitoba, Ontario, Quebec, New Brunswick, Nova Scotia, Prince Edward Island, Newfoundland and Labrador')
GHG.storage$Region[GHG.storage$Region == "National"] <- National

#Create a table to change regions
province_abbreviations <- c("Alberta" = "AB",
                            "British Columbia" = "BC",
                            "Manitoba" = "MB",
                            "New Brunswick" = "NB",
                            "Newfoundland and Labrador" = "NL",
                            "Nova Scotia" = "NS",
                            "Ontario" = "ON",
                            "Prince Edward Island" = "PE",
                            "Quebec" = "QC",
                            "Saskatchewan" = "SK",
                            "Northwest Territories" = "NT",
                            "Nunavut" = "NU",
                            "Yukon" = "YT")

#Count
Tech_data_both <- GHG.storage %>%
  separate_rows(Technique, sep = ",\\s*")
Tech_data_both[Tech_data_both$Technique=='Soil chamber',"Technique"] <- "Chamber"
Tech_data_both[Tech_data_both$Technique=='Animal chamber',"Technique"] <- "Chamber"
Tech_data_both <- Tech_data_both %>%
  group_by(Technique) %>%
  summarise(Number = n()) 
  


#CH4
GHG.storage.CH4 <- GHG.storage[GHG.storage$CH4 == TRUE,]

#Techniques 
Tech_data <- GHG.storage.CH4 %>%
  separate_rows(Technique, sep = ",\\s*")
Tech_data[Tech_data$Technique=='Soil chamber',"Technique"] <- "Chamber"
Tech_data[Tech_data$Technique=='Animal chamber',"Technique"] <- "Chamber"
Tech_data <- Tech_data %>%
  group_by(Pub..year,Technique) %>%
  summarise(Number = n()) %>%
  ungroup() %>%
  group_by(Technique) %>%
  arrange(Pub..year) %>%
  mutate(Tech = cumsum(Number)) %>%
  ungroup() %>%
  group_by(Pub..year) %>%
  mutate(Percentage = Number / sum(Number) * 100)

#Set the order for the techniques
Tech_data$Technique <- factor(Tech_data$Technique, 
                              levels = c("Chamber", "Incubation", "Micrometeorology", 
                                         "Mixed", "Modelling"))
tapply(Tech_data$Number,Tech_data$Technique,sum)
#Chamber #44 30.6%, Incubation #42 29.2%, Micrometeorology #23 16.0%, 
#Mixed #19 13.2%, Modelling #16 11.1%.

#separate indoor and outdoor.
GHG.storage.CH4 <- GHG.storage.CH4 %>%
  drop_na(Collection.Setting)
Tech_method <- GHG.storage.CH4 %>%
  separate_rows(Collection.Setting, sep = ",\\s*") %>%
  group_by(Pub..year,Collection.Setting) %>%
  summarise(Number = n()) %>%
  ungroup() %>%
  group_by(Pub..year) %>%
  mutate(Percentage = Number / sum(Number) * 100)
tapply(Tech_method$Number,Tech_method$Collection.Setting,sum)
#Indoor #128 82.6%, Outdoor #27 17.4%.

#Separate by province
Tech_province_CH4 <- GHG.storage.CH4 %>%
  separate_rows(Region, sep = ",\\s*") %>%
  separate_rows(Technique, sep = ",\\s*")
Tech_province_CH4[Tech_province_CH4$Technique=='Soil chamber',"Technique"] <- "Chamber"
Tech_province_CH4[Tech_province_CH4$Technique=='Animal chamber',"Technique"] <- "Chamber"
Tech_province_CH4 <- Tech_province_CH4 %>%
  group_by(Region,Technique) %>%
  summarise(Number = n()) %>%
  ungroup() %>%
  group_by(Region) %>%
  arrange(Technique) %>%
  mutate(Percentage = Number / sum(Number) * 100)

#Set the order for the techniques
Tech_province_CH4$Region <- factor(Tech_province_CH4$Region, 
                              levels = c("British Columbia","Alberta","Saskatchewan",
                                    "Manitoba","Ontario","Quebec","New Brunswick",
                                    "Nova Scotia","Prince Edward Island",
                                    "Newfoundland and Labrador"))

#N2O
GHG.storage.N2O <- GHG.storage[GHG.storage$N2O == TRUE,]
GHG.storage.N2O <- GHG.storage.N2O %>%
  drop_na(Livestock)
GHG.storage.N2O[GHG.storage.N2O$ID==225,"Technique"] <- 'Soil chamber'

#Techniques 
Tech_data_N2O <- GHG.storage.N2O %>%
  separate_rows(Technique, sep = ",\\s*")
Tech_data_N2O[Tech_data_N2O$Technique=='Soil chamber',"Technique"] <- "Chamber"
Tech_data_N2O[Tech_data_N2O$Technique=='Animal chamber',"Technique"] <- "Chamber"
Tech_data_N2O <- Tech_data_N2O %>%
  group_by(Pub..year,Technique) %>%
  summarise(Number = n()) %>%
  ungroup() %>%
  group_by(Technique) %>%
  arrange(Pub..year) %>%
  mutate(Tech = cumsum(Number)) %>%
  ungroup() %>%
  group_by(Pub..year) %>%
  mutate(Percentage = Number / sum(Number) * 100)

#Set the order for the techniques
Tech_data_N2O$Technique <- factor(Tech_data_N2O$Technique, 
                                  levels = c("Chamber", "Incubation", "Micrometeorology", 
                                             "Mixed", "Modelling"))
tapply(Tech_data_N2O$Number,Tech_data_N2O$Technique,sum)
#Chamber #35 47.9%, Incubation #4 5.5%, Micrometeorology #6 8.2%, 
#Mixed #19 26.0%, Modelling #9 12.3%.

#separate indoor and outdoor.
GHG.storage.N2O <- GHG.storage.N2O %>%
  drop_na(Collection.Setting)
Tech_method_N2O <- GHG.storage.N2O %>%
  separate_rows(Collection.Setting, sep = ",\\s*") %>%
  group_by(Pub..year,Collection.Setting) %>%
  summarise(Number = n()) %>%
  ungroup() %>%
  group_by(Pub..year) %>%
  mutate(Percentage = Number / sum(Number) * 100)
tapply(Tech_method_N2O$Number,Tech_method_N2O$Collection.Setting,sum)
#Indoor #63 75.0%, Outdoor #21 25.0%.

#Separate by province
Tech_province_N2O <- GHG.storage.N2O %>%
  separate_rows(Region, sep = ",\\s*") %>%
  separate_rows(Technique, sep = ",\\s*")
Tech_province_N2O[Tech_province_N2O$Technique=='Soil chamber',"Technique"] <- "Chamber"
Tech_province_N2O[Tech_province_N2O$Technique=='Animal chamber',"Technique"] <- "Chamber"
Tech_province_N2O <- Tech_province_N2O %>%
  group_by(Region,Technique) %>%
  summarise(Number = n()) %>%
  ungroup() %>%
  group_by(Region) %>%
  arrange(Technique) %>%
  mutate(Percentage = Number / sum(Number) * 100) %>%
  ungroup() %>%
  group_by(Region)

#Set the order for the techniques
Tech_province_N2O$Region <- factor(Tech_province_N2O$Region, 
                                   levels = c("British Columbia","Alberta","Saskatchewan",
                                              "Manitoba","Ontario","Quebec","New Brunswick",
                                              "Nova Scotia","Prince Edward Island",
                                              "Newfoundland and Labrador"))

#Add province abbreviation
#Function to replace the provinces
replace_provinces <- function(province) {
  abbreviations <- sapply(province, function(x) province_abbreviations[x])
  return(paste(abbreviations, collapse = ", "))}
# Apply the function to the method column in your dataframe
Tech_province_CH4$Region <- sapply(Tech_province_CH4$Region, replace_provinces)
Tech_province_N2O$Region <- sapply(Tech_province_N2O$Region, replace_provinces)

#Figure on techniques
Figure4a <- ggplot(Tech_data, aes(x = Pub..year, y = Tech, color = Technique)) +
  geom_line() +
  geom_point() +
  labs(x = "Publication Year", y = "Study count", color = "Method", title = "(a) CH₄") +
  scale_color_manual(values = c("Chamber" = "deepskyblue2", "Incubation" = "darkmagenta", 
                                "Micrometeorology" = "violetred1", "Mixed" = "goldenrod", 
                                "Modelling" = "darkolivegreen4")) +
  theme_classic() +
  theme(axis.text = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = c(0.1, 0.9),  # Adjust the legend position
        legend.justification = c(0, 1),  # Align the legend to top-left
        legend.title = element_text(size = 14),  # Adjust legend title font size
        legend.text = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(limits = c(0, 45), 
                     breaks = seq(0, 45, by = 2),
                     expand = c(0, 0))+
  scale_x_continuous(limits = c(1990, 2024), 
                     breaks = seq(1990, 2024 , by = 5),
                     expand = c(0, 0))
Figure4a

Figure4b <- ggplot(Tech_data_N2O, aes(x = Pub..year, y = Tech, color = Technique)) +
  geom_line() +
  geom_point() +
  labs(x = "Publication Year", y = "Study count", color = "Method", title = "(b) N₂O") +
  scale_color_manual(values = c("Chamber" = "deepskyblue2", "Incubation" = "darkmagenta", 
                                "Micrometeorology" = "violetred1", "Mixed" = "goldenrod", 
                                "Modelling" = "darkolivegreen4")) +
  theme_classic() +
  theme(axis.text = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = c(0.1, 0.9),  # Adjust the legend position
        legend.justification = c(0, 1),  # Align the legend to top-left
        legend.title = element_text(size = 14),  # Adjust legend title font size
        legend.text = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  scale_y_continuous(limits = c(0, 45), 
                     breaks = seq(0, 45, by = 2),
                     expand = c(0, 0))+
  scale_x_continuous(limits = c(1990, 2024), 
                     breaks = seq(1990, 2024 , by = 5),
                     expand = c(0, 0))
Figure4b

Figure4c <- ggplot(Tech_province_CH4, 
                   aes(x = Region, y = Number, fill = Technique, group = Technique)) +
  geom_bar(stat = "identity",
           linetype = 0) +
  labs(x = "Province", y = "Study Count", fill = "Method", title = "(c) CH₄") +
  scale_fill_manual(values = c("Chamber" = "deepskyblue2", "Incubation" = "darkmagenta", 
                               "Micrometeorology" = "violetred1", "Mixed" = "goldenrod", 
                               "Modelling" = "darkolivegreen4")) +
  theme_classic() +
  theme(axis.text = element_text(size = 12, color = "black"),
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
  scale_y_continuous(limits = c(0, 45), 
                     breaks = seq(0, 45, by = 2),
                     expand = c(0, 0))+
  scale_x_discrete(limits = c("BC","AB","SK","MB","ON","QC","NB","NS","PE","NL"),
                   labels = function(x) stringr::str_wrap(x, width = 20))
Figure4c

Figure4d <- ggplot(Tech_province_N2O, 
                   aes(x = Region, y = Number, fill = Technique, group = Technique)) +
  geom_bar(stat = "identity",
           linetype = 0) +
  labs(x = "Province", y = "Study Count", fill = "Method", title = "(d) N₂O") +
  scale_fill_manual(values = c("Chamber" = "deepskyblue2", "Incubation" = "darkmagenta", 
                               "Micrometeorology" = "violetred1", "Mixed" = "goldenrod", 
                               "Modelling" = "darkolivegreen4")) +
  theme_classic() +
  theme(axis.text = element_text(size = 12, color = "black"),
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
  scale_y_continuous(limits = c(0, 45), 
                     breaks = seq(0, 45, by = 2),
                     expand = c(0, 0))+
  scale_x_discrete(limits = c("BC","AB","SK","MB","ON","QC","NB","NS","PE","NL"),
                   labels = function(x) stringr::str_wrap(x, width = 20))
Figure4d

#Export figure
ggsave("output/Figure 4 - Methods CH4 and N2O.png", 
       ggarrange(Figure4a, Figure4b, Figure4c, Figure4d,
                 ncol = 2, nrow = 2,
                 common.legend = TRUE, legend = "bottom"),
       width = 24, height = 24, units = "cm",
       dpi = 300)
