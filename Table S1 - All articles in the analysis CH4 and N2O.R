#for the table of all studies used for analysis
library(tidyverse); library(gt)

#Read data
GHG.CH4 <- read.csv("input/Canada GHG storage lit review data 20240611.csv",header = T)

#Obtain studies with storage measurement 
GHG.storage <- GHG.CH4[grepl("Storage", GHG.CH4$GHG.source),]
GHG.storage <- GHG.storage %>%
  subset(GHG.storage$CH4==TRUE | GHG.storage$N2O==TRUE)

#Select columns
GHG.table.CH4 <- GHG.storage %>%
  select(ID, Author = First.author, Year = Pub..year, Region, Scale = Research.type, 
         Method = Technique, Livestock, Manure = Manure.type, CH4, N2O, NH3)

#Put the pub year with authors
GHG.table.CH4$Authors <- paste(GHG.table.CH4$Author, "et al.,", GHG.table.CH4$Year)
GHG.table.CH4 <- GHG.table.CH4 %>%
  select(-Author, Year) %>%
  select(ID, Authors, Region, Scale, Livestock, Manure, Method, CH4, N2O, NH3, Year)

#Change name for 1 or 2 authors
GHG.table.CH4$Authors[GHG.table.CH4$ID == '9'] <- "Benchaar and Hassanat, 2021"
GHG.table.CH4$Authors[GHG.table.CH4$ID == '33'] <- "Benchaar and Hassanat, 2020"
GHG.table.CH4$Authors[GHG.table.CH4$ID == '47'] <- "Boh and Clark, 2020"
GHG.table.CH4$Authors[GHG.table.CH4$ID == '53'] <- "Hassanat and Benchaar, 2019"
GHG.table.CH4$Authors[GHG.table.CH4$ID == '54'] <- "Benchaar and Hassanat, 2019"
GHG.table.CH4$Authors[GHG.table.CH4$ID == '56'] <- "McVoitte and Clark, 2019"
GHG.table.CH4$Authors[GHG.table.CH4$ID == '64'] <- "Dimitrov and Wang, 2019"
GHG.table.CH4$Authors[GHG.table.CH4$ID == '68'] <- "Huang and Guo, 2019"
GHG.table.CH4$Authors[GHG.table.CH4$ID == '89'] <- "Hao and Larney, 2017"
GHG.table.CH4$Authors[GHG.table.CH4$ID == '107'] <- "Baldé et al., 2016a"
GHG.table.CH4$Authors[GHG.table.CH4$ID == '108'] <- "Saady and Massé, 2016"
GHG.table.CH4$Authors[GHG.table.CH4$ID == '110'] <- "Baldé et al., 2016b"
GHG.table.CH4$Authors[GHG.table.CH4$ID == '117'] <- "Baldé et al., 2016c"
GHG.table.CH4$Authors[GHG.table.CH4$ID == '125'] <- "Massé and Saady, 2015"
GHG.table.CH4$Authors[GHG.table.CH4$ID == '126'] <- "Saady and Massé, 2015"
GHG.table.CH4$Authors[GHG.table.CH4$ID == '139'] <- "Jayasundara and Wagner-Riddle, 2014"
GHG.table.CH4$Authors[GHG.table.CH4$ID == '150'] <- "Saady and Massé, 2013"
GHG.table.CH4$Authors[GHG.table.CH4$ID == '157'] <- "McGinn and Beauchemin, 2012"
GHG.table.CH4$Authors[GHG.table.CH4$ID == '178'] <- "Park and Wagner-Riddle, 2010"
GHG.table.CH4$Authors[GHG.table.CH4$ID == '208'] <- "Hao, 2007"
GHG.table.CH4$Authors[GHG.table.CH4$ID == '241'] <- "Massé and Droste, 2000"
GHG.table.CH4$Authors[GHG.table.CH4$ID == '261'] <- "Bhatt and Abbassi, 2022"
GHG.table.CH4$Authors[GHG.table.CH4$ID == '265'] <- "VanderZaag and Baldé, 2022"

#These are the articles with only 1 or 2 authors. 
# 9 Benchaar and Hassanat, 2021
# 33 Benchaar and Hassanat, 2020
# 47 Boh and Clark, 2020
# 53 Hassanat and Benchaar, 2019
# 54 Benchaar and Hassanat, 2019
# 56 McVoitte and Clark, 2019
# 64 Dimitrov and Wang, 2019
# 68 Huang and Guo, 2019
# 89 Hao and Larney, 2017
# 108 Saady and Massé, 2016
# 125 Massé and Saady, 2015
# 126 Saady and Massé, 2015
# 139 Jayasundara and Wagner-Riddle, 2013
# 150 Saady and Massé, 2013
# 157 McGinn and Beauchemin, 2012
# 178 Park and Wagner-Riddle, 2010
# 208 Hao, 2007
# 241 Massé and Droste, 2000
# 261 Bhatt and Abbassi, 2022
# 265 VanderZaag and Baldé, 2022

#Add the GHG types
GHG.table.CH4 <- GHG.table.CH4 %>%
  mutate(GHG = case_when(
    !CH4 & !N2O & NH3 ~ "NH₃",
    CH4 & !N2O & !NH3 ~ "CH₄",
    !CH4 & N2O & !NH3 ~ "N₂O",
    !CH4 & N2O & NH3 ~ "N₂O, NH₃",
    CH4 & !N2O & NH3 ~ "CH₄, NH₃",
    CH4 & N2O & !NH3 ~ "CH₄, N₂O",
    CH4 & N2O & NH3 ~ "CH₄, N₂O, NH₃",
    TRUE ~ "None"))

GHG.table.CH4 <- GHG.table.CH4 %>%
  select(-CH4, -N2O, -NH3)

#Replace the Method
#Create a table for conversion
method_abbreviations <- c("Mixed" = "MX",
                          "Incubation" = "OB (I)",
                          "Modelling" = "MD",
                          "Animal chamber" = "OB (C)",
                          "Soil chamber" = "OB (C)",
                          "Collar" = "OB (O)",
                          "Micrometeorology" = "OB (M)")

# Function to replace the methods.
replace_methods <- function(method) {
  method <- strsplit(method, ", ")[[1]]
  abbreviations <- sapply(method, function(x) method_abbreviations[x])
  return(paste(abbreviations, collapse = ", "))}
# Apply the function to the method column in your dataframe
GHG.table.CH4$Method <- sapply(GHG.table.CH4$Method, replace_methods)
#Synthesize observations
GHG.table.CH4$Method[GHG.table.CH4$Method == 'OB (C), OB (I)'] <- "OB (C, I)"
GHG.table.CH4$Method[GHG.table.CH4$Method == 'OB (M), OB (C)'] <- "OB (C, M)"
GHG.table.CH4$Method[GHG.table.CH4$Method == 'OB (O), OB (C)'] <- "OB (C, O)"

#Method count
GHG.table.CH4$count.OB <- str_count(GHG.table.CH4$Method, "OB")
GHG.method.count.OB <- GHG.table.CH4 %>%
  group_by(count.OB) %>%
  summarise(Number = n())
GHG.table.CH4$count.MD <- str_count(GHG.table.CH4$Method, "MD")
GHG.method.count.MD <- GHG.table.CH4 %>%
  group_by(count.MD) %>%
  summarise(Number = n())
GHG.table.CH4$count.MX <- str_count(GHG.table.CH4$Method, "MX")
GHG.method.count.MX <- GHG.table.CH4 %>%
  group_by(count.MX) %>%
  summarise(Number = n())

# Arrange Region column
GHG.table.CH4$Region[GHG.table.CH4$Region == 'National'] <- "Multiple"
GHG.table.CH4$Region[GHG.table.CH4$Region == 'Alberta, Ontario'] <- "Multiple"
GHG.table.CH4$Region[GHG.table.CH4$Region == 'Alberta, Quebec'] <- "Multiple"

#Replace Livestock
#Create a table for conversion
livestock_abbreviations <- c("Beef Cattle" = "Beef", "Dairy Cattle" = "Dairy", "Horse" = "Horse",
                             "Sheep" = "Sheep", "Swine" = "Swine", "Poultry" = "Poultry")

# Function to replace livestock
replace_livestock <- function(livestock) {
  livestock <- strsplit(livestock, ", ")[[1]]
  abbreviations <- sapply(livestock, function(x) livestock_abbreviations[x])
  return(paste(abbreviations, collapse = ", "))}
# Apply the function to the method column in your dataframe
GHG.table.CH4$Livestock <- sapply(GHG.table.CH4$Livestock, replace_livestock)

#Replace Manure
#Create a table for conversion
manure_abbreviations <- c("Liquid" = "L", "Solid" = "S")

# Function to replace manure
replace_manure <- function(manure) {
  manure <- strsplit(manure, ", ")[[1]]
  abbreviations <- sapply(manure, function(x) manure_abbreviations[x])
  return(paste(abbreviations, collapse = ", "))}
# Apply the function to the method column in your dataframe
GHG.table.CH4$Manure <- sapply(GHG.table.CH4$Manure, replace_manure)

#Replace field scale with farm scale
GHG.table.CH4$Scale[GHG.table.CH4$Scale == 'Field'] <- "Farm"

#Change the column names
names(GHG.table.CH4) <- c("ID", "Authors and year", "Region", "Scale", "Livestock", 
                          "Manure", "Method", "Year", "GHG", "OB", "MD", "MX")
GHG.table.CH4 <- GHG.table.CH4 %>%
  arrange(factor(Region, levels = c("British Columbia","Alberta","Saskatchewan",
                                    "Manitoba","Ontario","Quebec","New Brunswick",
                                    "Nova Scotia","Prince Edward Island",
                                    "Newfoundland and Labrador", "Alberta, Ontario",
                                    "Alberta, Quebec", "Multiple")), `Authors and year`) %>%
  arrange(Year,Region,`Authors and year`,Livestock) %>%
  select(-Year, -ID, -OB, -MD, -MX)



#Format table
gt_GHG.Table.CH4 <-
  gt(GHG.table.CH4) %>%
  tab_row_group(
    label = "British Columbia",
    rows = GHG.table.CH4$Region == "British Columbia") %>%
  tab_row_group(
    label = "Alberta",
    rows = GHG.table.CH4$Region == "Alberta") %>%
  tab_row_group(
    label = "Saskatchewan",
    rows = GHG.table.CH4$Region == "Saskatchewan") %>%
  tab_row_group(
    label = "Manitoba",
    rows = GHG.table.CH4$Region == "Manitoba") %>%
  tab_row_group(
    label = "Ontario",
    rows = GHG.table.CH4$Region == "Ontario") %>%
  tab_row_group(
    label = "Quebec",
    rows = GHG.table.CH4$Region == "Quebec") %>%
  tab_row_group(
    label = "Nova Scotia",
    rows = GHG.table.CH4$Region == "Nova Scotia") %>%
  tab_row_group(
    label = "Multiple",
    rows = GHG.table.CH4$Region %in% c("Alberta, Ontario","Alberta, Quebec","Multiple")) %>%
  row_group_order(groups = c(
    "British Columbia", "Alberta", "Saskatchewan", "Manitoba", "Ontario", "Quebec", "Nova Scotia")) %>%
  cols_hide(Region) %>%
  tab_footnote(
    footnote = md("Three or more authors are summarized in *et al.*"),
    locations = cells_column_labels(columns = "Authors and year")) %>%
  tab_footnote(
    footnote = "Pilot scale studies are studies conducted measurement from manure 
    storages smaller than local regular manure storage.",
    locations = cells_column_labels(columns = "Scale")) %>%
  tab_footnote(
    footnote = "NA: Not applicable") %>%
  opt_footnote_marks(marks = "letters")
gt_GHG.Table.CH4

#Export file
gt_GHG.Table.CH4 %>%
  gtsave("Table S1 - Articles.docx")
