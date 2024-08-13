#To prepare figure for treatments, CH4 and N2O
#Table 4
library(tidyverse); library(gt)

#read data
GHG.CH4 <- read.csv("input/Canada GHG storage lit review data 20240611.csv",header = T)



#CH4
#obtain studies from storage measurement 
GHG.storage <- GHG.CH4[grepl("Storage", GHG.CH4$GHG.source),]
GHG.storage <- GHG.storage[GHG.storage$CH4 == TRUE,]

#Removal season 
Treat_data_removal_season <- GHG.storage %>%
  separate_rows(Treatment, sep = ",\\s*") %>%
  separate_rows(Removal.season, sep = ",\\s*") %>%
  group_by(Removal.season, Treatment, ID)
Treat_data_removal_season <- Treat_data_removal_season[Treat_data_removal_season$Treatment == "Removal", ] 
Treat_data_removal_season <- Treat_data_removal_season %>%
  drop_na(Treatment) %>%
  group_by(Removal.season) %>%
  summarise(Number = n()) %>%
  ungroup()
Treat_data_removal_season$Removal.season <- factor(Treat_data_removal_season$Removal.season,
                                                   levels=c("Spring", "Summer","Fall", "Not specified"))

tapply(Treat_data_removal_season$Number,Treat_data_removal_season$Removal.season,sum)
#Fall #16 Not specified #1, Spring #17, Summer #7

#Removal frequency 
Treat_data_removal_frequency <- GHG.storage %>%
  separate_rows(Treatment, sep = ",\\s*") %>%
  filter(grepl('Removal',Treatment)) %>%
  separate_rows(Removal.frequency, sep = ",\\s*") %>%
  group_by(Removal.frequency, Treatment, ID)
Treat_data_removal_frequency$Removal.frequency[Treat_data_removal_frequency$Removal.frequency %in% c("4", "5", "6", "7", "104")] <- "> 3"
Treat_data_removal_frequency <- Treat_data_removal_frequency %>%
  drop_na(Treatment) %>%
  group_by(Removal.frequency) %>%
  summarise(Number = n()) %>%
  ungroup() %>%
  drop_na(Removal.frequency) %>%
  mutate(Percentage = Number / sum(Number) * 100)
tapply(Treat_data_removal_frequency$Number,Treat_data_removal_frequency$Removal.frequency,sum)
#1 time #8, 2 time #11, 3 time #3, > 3 time #6

#Emptiness
Treat_data_removal_empty <- GHG.storage %>%
  separate_rows(Treatment, sep = ",\\s*") %>%
  separate_rows(X..Emptied, sep = ",\\s*") %>%
  group_by(X..Emptied, Treatment, ID) 
Treat_data_removal_empty <- Treat_data_removal_empty[Treat_data_removal_empty$Treatment == "Removal", ] 
Treat_data_removal_empty[, 'X..Emptied'] <- sapply(Treat_data_removal_empty[, 'X..Emptied'], as.numeric)
Treat_data_removal_empty <- Treat_data_removal_empty %>%
  drop_na(Treatment) %>%
  group_by( X..Emptied = cut(X..Emptied, breaks = c(-Inf, 0, 35, 60, 85, 100, Inf))) %>%
  summarise(Number = n()) %>%
  ungroup() %>%
  mutate(Percentage = Number / sum(Number) * 100)
Treat_data_removal_empty[, 'X..Emptied'] <- sapply(Treat_data_removal_empty[, 'X..Emptied'], as.character)
Treat_data_removal_empty <- Treat_data_removal_empty %>%
  replace_na(list(X..Emptied = "Not specified"))
tapply(Treat_data_removal_empty$Number,Treat_data_removal_empty$X..Emptied,sum)
#(0, 35] #5, (35, 60] #7, (60, 85] #11, (85, 100] #9, Not specified #3




#N2O
#obtain studies from storage measurement 
GHG.storage.N2O <- GHG.CH4[grepl("Storage", GHG.CH4$GHG.source),]
GHG.storage.N2O <- GHG.storage.N2O[GHG.storage.N2O$N2O == TRUE,]

#Removal season 
Treat_remov_season_N2O <- GHG.storage.N2O %>%
  separate_rows(Treatment, sep = ",\\s*") %>%
  separate_rows(Removal.season, sep = ",\\s*") %>%
  group_by(Removal.season, Treatment, ID)
Treat_remov_season_N2O <- Treat_remov_season_N2O[Treat_remov_season_N2O$Treatment == "Removal", ] 
Treat_remov_season_N2O <- Treat_remov_season_N2O %>%
  drop_na(Treatment) %>%
  group_by(Removal.season) %>%
  summarise(Number = n()) %>%
  ungroup()
Treat_remov_season_N2O$Removal.season <- factor(Treat_remov_season_N2O$Removal.season,
                                                   levels=c("Spring", "Summer","Fall", "Not specified"))

tapply(Treat_remov_season_N2O$Number,Treat_remov_season_N2O$Removal.season,sum)
#Fall #3 Not specified #0, Spring #5, Summer #1

#Removal frequency 
Treat_remov_frequency_N2O <- GHG.storage.N2O %>%
  separate_rows(Treatment, sep = ",\\s*") %>%
  filter(grepl('Removal',Treatment)) %>%
  separate_rows(Removal.frequency, sep = ",\\s*") %>%
  group_by(Removal.frequency, Treatment, ID)
Treat_remov_frequency_N2O$Removal.frequency[Treat_remov_frequency_N2O$Removal.frequency %in% c("4", "5", "6", "7", "104")] <- "> 3"
Treat_remov_frequency_N2O <- Treat_remov_frequency_N2O %>%
  drop_na(Treatment) %>%
  group_by(Removal.frequency) %>%
  summarise(Number = n()) %>%
  ungroup() %>%
  drop_na(Removal.frequency) %>%
  mutate(Percentage = Number / sum(Number) * 100)
tapply(Treat_remov_frequency_N2O$Number,Treat_remov_frequency_N2O$Removal.frequency,sum)
#1 time #4, 2 time #2, 3 time #0, > 3 time #1

#Emptiness
Treat_removal_empty_N2O <- GHG.storage.N2O %>%
  separate_rows(Treatment, sep = ",\\s*") %>%
  separate_rows(X..Emptied, sep = ",\\s*") %>%
  group_by(X..Emptied, Treatment, ID) 
Treat_removal_empty_N2O <- Treat_removal_empty_N2O[Treat_removal_empty_N2O$Treatment == "Removal", ] 
Treat_removal_empty_N2O[, 'X..Emptied'] <- sapply(Treat_removal_empty_N2O[, 'X..Emptied'], as.numeric)
Treat_removal_empty_N2O <- Treat_removal_empty_N2O %>%
  drop_na(Treatment) %>%
  group_by( X..Emptied = cut(X..Emptied, breaks = c(-Inf, 0, 35, 60, 85, 100, Inf))) %>%
  summarise(Number = n()) %>%
  ungroup() %>%
  mutate(Percentage = Number / sum(Number) * 100)
Treat_removal_empty_N2O[, 'X..Emptied'] <- sapply(Treat_removal_empty_N2O[, 'X..Emptied'], as.character)
Treat_removal_empty_N2O <- Treat_removal_empty_N2O %>%
  replace_na(list(X..Emptied = "Not specified"))
tapply(Treat_removal_empty_N2O$Number,Treat_removal_empty_N2O$X..Emptied,sum)
#(0, 35] #0, (35, 60] #1, (60, 85] #2, (85, 100] #2, Not specified #2



#Make table
Table.freq.empt.CH4 <- bind_rows(Treat_data_removal_frequency, Treat_data_removal_empty)
Table.freq.empt.CH4 <- Table.freq.empt.CH4 %>%
  mutate(mycol = coalesce(Removal.frequency, X..Emptied)) %>%
  select(mycol, Number, Percentage)
colnames(Table.freq.empt.CH4) <- c("Variable.all", "Number of studies", "Percentage of studies (%)")
Table.freq.empt.N2O <- bind_rows(Treat_remov_frequency_N2O, Treat_removal_empty_N2O)
Table.freq.empt.N2O <- Table.freq.empt.N2O %>%
  mutate(mycol = coalesce(Removal.frequency, X..Emptied)) %>%
  select(mycol, Number, Percentage)
colnames(Table.freq.empt.N2O) <- c("Variable", "Number of studies", "Percentage of studies (%)")

Table.freq.empt <- merge(Table.freq.empt.CH4, Table.freq.empt.N2O, by = 1, all = TRUE)
Table.freq.empt <- Table.freq.empt %>%
  replace_na(list("Number of studies.y" = 0)) %>%  
  replace_na(list("Percentage of studies (%).y" = 0))
Table.freq.empt <- Table.freq.empt %>%
  arrange(factor(Variable.all, levels = c("(0,35]","(35,60]","(60,85]","(85,100]","1","2","3","> 3","Not specified")))

gt_removal_freq_empt <-
  gt(Table.freq.empt)%>%
  tab_row_group(
    label = "Emptying efficiency (%)",
    rows = 1:4) %>%
  tab_row_group(
    label = "Removal frequency per year",
    rows = 5:9) %>%
  row_group_order(groups = c("Emptying efficiency (%)", "Removal frequency per year")) %>%
  tab_spanner(
    label = html("CH<sub>4</sub>"),
    columns = c("Number of studies.x", "Percentage of studies (%).x")) %>%
  tab_spanner(
    label = html("N<sub>2</sub>O"),
    columns = c("Number of studies.y", "Percentage of studies (%).y")) %>%
  cols_label(
    "Variable.all" = "Variable", 
    "Number of studies.x" = "Number of studies", 
    "Percentage of studies (%).x" = "Percentage of studies (%)", 
    "Number of studies.y" = "Number of studies", 
    "Percentage of studies (%).y" = "Percentage of studies (%)") %>%
  fmt_number(
    columns = c("Percentage of studies (%).x", "Percentage of studies (%).y"),
    rows = 1:9,
    decimals = 1,
    use_seps = TRUE)
gt_removal_freq_empt
  
#Export file
gt_removal_freq_empt %>%
  gtsave("Table 4 - Frequency of removal and proportion removed.docx")
