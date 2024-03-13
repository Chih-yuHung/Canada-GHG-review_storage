#To prepare figure for techniques, N2O only!
#Figure 1
library(tidyverse);library(ggplot2)
library(reshape2);library(scatterpie)
library(stringr);library(ggpubr)
#read data
GHG <- read.csv("input/Canada GHG storage lit review data.csv",header = T)
#obtain studies with field measurement 
GHG.storage <- GHG[grepl("Storage", GHG$GHG.source),]
#GHG.field <- GHG.field[!grepl("^Grass$", GHG.field$Field.crop),]
#GHG.field <- GHG.field[GHG.field$N2O == TRUE,]


# Define custom colors, cold for indoor, warm for outdoor
cold_colors <- c("#878787", "#4984eb", "#7d2fa3", "#2eb392")
warm_colors <- c("#de8a5b", "#b82e1c", "#e0a26c", "#e8734d")

#Techniques 
Tech_data <- GHG.storage %>%
  separate_rows(Technique, sep = ",\\s*") %>%
  group_by(Pub..year,Technique) %>%
  summarise(Number = n()) %>%
  ungroup() %>%
  group_by(Pub..year) %>%
  mutate(Percentage = Number / sum(Number) * 100)

#Set the order for the techniques
Tech_data$Technique <- factor(Tech_data$Technique, 
                       levels = c("Incubation", "Mixed", "Modelling",
                                "Micrometeorology", "Floating chamber", "Encapsulating chamber"))
tapply(Tech_data$Number,Tech_data$Technique,sum)
#Incu 30, mixed 16, modelling 18, micromete 8, soil chamber 55.

#To know the count after 2016. 
#For mixed
sum(Tech_data$Number[Tech_data$Pub..year >=2016 & Tech_data$Technique == "Mixed"])
#11
#For Modelling
sum(Tech_data$Number[Tech_data$Pub..year >=2016 & Tech_data$Technique == "Modelling"])
#16
#(11+16)/(16+18)  # 0.794

#separate to dry and wet lab method.
Tech_method <- Tech_data %>%
  mutate(method = ifelse(Technique %in% c("Incubation", "Mixed", "Modelling"), 
                         "Indoor", "Outdoor")) %>%
  group_by(Pub..year,method) %>%
  summarise(method.n = sum(Number)) %>%
  ungroup()

png(file = "output/Techniques.png",
    width = 4800, height = 3600, res = 600)
ggplot(Tech_data, aes(x = Pub..year, y = Number, fill = Technique)) +
  geom_bar(stat = "identity") +
  labs(x = "Publication Year", y = "Study count") +
  scale_fill_manual(values = c(cold_colors[1:3],warm_colors[1:3])) +
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
  scale_y_continuous(limits = c(0, 18), 
                     breaks = seq(0, 18, by = 2),
                     expand = c(0, 0))+
  scale_x_continuous(limits = c(1990, 2023), 
                     breaks = seq(1990, 2023 , by = 5),
                     expand = c(0, 0))
dev.off()

ggsave("output/Techniques.png" ,
       width = 4800, height = 7200, units = "px",
       dpi = 600)



# used to combine figures together
# ggsave("output/Fig1.png", 
#        ggarrange(Fig1a, Fig1b, Fig1c, Fig1d,
#                  ncol = 2, nrow = 2),
#        width = 48, height = 24, units = "cm",
#        dpi = 300)

