#To prepare figure for techniques, CH4 only
#Figure 2
library(tidyverse); library(ggpubr)

#read data
GHG.CH4 <- read.csv("input/Canada GHG storage lit review data 20240611.csv",header = T)
#obtain studies from storage measurement 
GHG.storage <- GHG.CH4[grepl("Storage", GHG.CH4$GHG.source),]
GHG.storage <- GHG.storage[GHG.storage$CH4 == TRUE,]
GHG.storage <- GHG.storage %>%
  drop_na(Livestock)
GHG.storage[GHG.storage$ID==9,"Technique"] <- 'Incubation'
GHG.storage[GHG.storage$ID==33,"Technique"] <- 'Incubation'
GHG.storage[GHG.storage$ID==225,"Technique"] <- 'Soil chamber'

# Define custom colors, cold for indoor, warm for outdoor
cold_colors <- c("deepskyblue2", "violet", "violetred1", "goldenrod")
warm_colors <- c("darkolivegreen4", "limegreen", "turquoise1", "gold")



#Techniques 
Tech_data <- GHG.storage %>%
  separate_rows(Technique, sep = ",\\s*")
Tech_data[Tech_data$Technique=='Soil chamber',"Technique"] <- "Chamber"
Tech_data[Tech_data$Technique=='Animal chamber',"Technique"] <- "Chamber"
Tech_data <- Tech_data %>%
  group_by(Pub..year,Technique) %>%
  summarise(Number = n()) %>%
  ungroup() %>%
  group_by(Pub..year) %>%
  mutate(Percentage = Number / sum(Number) * 100)

#Set the order for the techniques
Tech_data$Technique <- factor(Tech_data$Technique, 
                       levels = c("Chamber", "Incubation", "Micrometeorology", 
                                  "Mixed", "Modelling"))
tapply(Tech_data$Number,Tech_data$Technique,sum)
#Animal chamber #6 4.2%, Incubation #43 29.2%, Micrometeorology #23 16.0%, 
#Mixed #17 13.2%, Modelling #16 11.1%, Soil chamber #38 26.4%.



#separate indoor and outdoor.
GHG.storage <- GHG.storage %>%
  drop_na(Collection.Setting)
GHG.storage$Collection.Setting[GHG.storage$Collection.Setting == "Mixed"] <- "Indoor, Outdoor"
Tech_method <- GHG.storage %>%
  separate_rows(Collection.Setting, sep = ",\\s*") %>%
  group_by(Pub..year,Collection.Setting) %>%
  summarise(Number = n()) %>%
  ungroup() %>%
  group_by(Pub..year) %>%
  mutate(Percentage = Number / sum(Number) * 100)
tapply(Tech_method$Number,Tech_method$Collection.Setting,sum)
#Indoor #128 82.6%, Outdoor #27 17.4%.


#Figure on techniques
Fig2a <- ggplot(Tech_data, aes(x = Pub..year, y = Number, fill = Technique)) +
  geom_bar(stat = "identity") +
  labs(x = "Publication Year", y = "Study count") +
  scale_fill_manual(values = c(cold_colors[1:4],warm_colors[1:4])) +
  theme_classic() +
  theme(axis.text = element_text(size = 14, colour = "black"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = c(0.1, 0.9),  # Adjust the legend position
        legend.justification = c(0, 1),  # Align the legend to top-left
        legend.title = element_text(size = 14),  # Adjust legend title font size
        legend.text = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  geom_text(aes(label = paste0(round(Percentage, 0), "%")),
            position = position_stack(vjust = 0.5),
            size = 2) +
  scale_y_continuous(limits = c(0, 14), 
                     breaks = seq(0, 14, by = 2),
                     expand = c(0, 0))+
  scale_x_continuous(limits = c(1990, 2024), 
                     breaks = seq(1990, 2024 , by = 5),
                     expand = c(0, 0))
Fig2a

Fig2b <- ggplot(Tech_method, aes(x = Pub..year, y = Number, fill = Collection.Setting)) +
  geom_bar(stat = "identity") +
  labs(x = "Publication Year", y = "Study count", fill = "Collection setting") +
  scale_fill_manual(values = c("gold", "turquoise1")) +
  theme_classic() +
  theme(axis.text = element_text(size = 14, colour = "black"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = c(0.1, 0.9),  # Adjust the legend position
        legend.justification = c(0, 1),  # Align the legend to top-left
        legend.title = element_text(size = 14),  # Adjust legend title font size
        legend.text = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  geom_text(aes(label = paste0(round(Percentage, 0), "%")),
            position = position_stack(vjust = 0.5),
            size = 2) +
  scale_y_continuous(limits = c(0, 18), 
                     breaks = seq(0, 18, by = 2),
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(1990, 2024), 
                     breaks = seq(1990, 2024 , by = 5),
                     expand = c(0, 0))
Fig2b


#Export figure
ggsave("output/Figure - Techniques CH4.png", Fig2a,
        width = 24, height = 18, units = "cm",
        dpi = 300)
