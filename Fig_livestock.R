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


cold_colors <- c("#878787", "#4984eb", "#7d2fa3", "#2eb392")
warm_colors <- c("#de8a5b", "#b82e1c", "#e0a26c", "#e8734d")

#Livestock types
livestock_data <- GHG.storage %>%
  separate_rows(Livestock, sep = ",\\s*") %>%
  group_by(Pub..year,Livestock) %>%
  summarise(Number = n()) %>%
  ungroup() %>%
  group_by(Pub..year) %>%
  mutate(Percentage = Number / sum(Number) * 100)

Fig1b <- ggplot(data = na.omit(livestock_data), 
                aes(x = Pub..year, y = Number, fill = Livestock)) +
  geom_bar(stat = "identity") +
  labs(x = "Publication Year", y = "Study Count") +
  scale_fill_manual(values = c(warm_colors,cold_colors)) +
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
            size = 3) +
  scale_y_continuous(limits = c(0, 30), 
                     breaks = seq(0, 30, by = 2),
                     expand = c(0, 0))+
  scale_x_continuous(limits = c(1990, 2023), 
                     breaks = seq(1990, 2023 , by = 5),
                     expand = c(0, 0))


ggsave("output/Fig2_livestock.png", Fig1b,
       width = 4800, height = 7200, units = "px",
       dpi = 600)


