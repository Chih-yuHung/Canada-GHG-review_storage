#for the table of all studies used for analysis
library(tidyverse); library(gt)

#Read data
GHG.CH4 <- read.csv("input/Canada GHG storage lit review data 20240611.csv",header = T)

#Obtain studies with storage measurement 
GHG.storage <- GHG.CH4[grepl("Storage", GHG.CH4$GHG.source),]
GHG.storage <- GHG.storage %>%
  subset(GHG.storage$CH4==TRUE | GHG.storage$N2O==TRUE)

#out table prepare
GHG.table.CH4 <- GHG.storage %>%
  select(ID, Author = First.author, Year = Pub..year, Region, Method = Technique, 
         Livestock, CO2, CH4, N2O,NH3, Manure.type)

#put the pub year with authors
GHG.table.CH4$Authors <- paste(GHG.table.CH4$Author, "et al.,",
                           GHG.table.CH4$Year)
GHG.table.CH4 <- GHG.table.CH4 %>%
  select(-Author, Year) %>%
  select(ID, Authors, Region, Livestock, Manure.type, Method, CO2, CH4, N2O, Year) %>%
  arrange(Year,Region,Livestock)

#Province count
GHG.count.N2O <- GHG.table.CH4 %>%
  separate_rows(Region, sep = ",\\s*") %>%
  group_by(Region, N2O) %>%
  summarise(Number = n()) %>%
  filter(!grepl("FALSE", N2O))

#Add the GHG types
GHG.table.CH4 <- GHG.table.CH4 %>%
  mutate(GHG = case_when(
      CO2 & !CH4 & !N2O ~ "CO₂",
      !CO2 & CH4 & !N2O ~ "CH₄",
      !CO2 & !CH4 & N2O ~ "N₂O",
      CO2 & !CH4 & N2O ~ "CO₂, N₂O",
      CO2 & CH4 & !N2O ~ "CO₂, CH₄",
      !CO2 & CH4 & N2O ~ "CH₄, N₂O",
      CO2 & CH4 & N2O ~ "CO₂, CH₄, N₂O",
      TRUE ~ "None"))

GHG.table.CH4 <- GHG.table.CH4 %>%
  select(-CO2, -CH4, -N2O)

#Replace the Method
#Create a table for conversion
method_abbreviations <- c("Mixed" = "MX",
                          "Incubation" = "OB",
                          "Modelling" = "MD",
                          "Animal chamber" = "OB",
                          "Soil chamber" = "OB",
                          "Collar" = "OB",
                          "Micrometeorology" = "OB")

#a function to replace the methods.
replace_methods <- function(method) {
  method <- strsplit(method, ", ")[[1]]
  abbreviations <- sapply(method, function(x) method_abbreviations[x])
  return(paste(abbreviations, collapse = ", "))}
# Apply the function to the method column in your dataframe
GHG.table.CH4$Method <- sapply(GHG.table.CH4$Method, replace_methods)

#Replace the methods in double
GHG.table.CH4 <- data.frame(lapply(GHG.table.CH4,function(x) {gsub("OB, OB", "OB", x)}))

#Replace the province names
# Create a mapping between province names and their abbreviations
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

# Function to replace province names with abbreviations
replace_province <- function(region) {
  if (region == "National") {
    return("Multiple")
  } else {
    provinces <- strsplit(region, ", ")[[1]]
    abbreviations <- sapply(provinces, function(x) province_abbreviations[x])
    return(paste(abbreviations, collapse = ", "))
  }
}

# Apply the function to the Region column in your dataframe
GHG.table.CH4$Region <- sapply(GHG.table.CH4$Region, replace_province)
GHG.table.CH4 <- GHG.table.CH4 %>%
  arrange(factor(Region, levels = c("BC","AB","SK","MB","ON","QC","NB","NS","PE","NL")), Year)

#Change the name
names(GHG.table.CH4) <- c("ID", html("Authors and year"), "Region", "Animal types","Manure type", "Methods", "Year", "GHG types")
#Change name for 1 or 2 authors
GHG.table.CH4$`Authors and year`[GHG.table.CH4$ID == '9'] <- "Benchaar and Hassanat, 2021"
GHG.table.CH4$`Authors and year`[GHG.table.CH4$ID == '33'] <- "Benchaar and Hassanat, 2020"
GHG.table.CH4$`Authors and year`[GHG.table.CH4$ID == '47'] <- "Boh and Clark, 2020"
GHG.table.CH4$`Authors and year`[GHG.table.CH4$ID == '53'] <- "Hassanat and Benchaar, 2019"
GHG.table.CH4$`Authors and year`[GHG.table.CH4$ID == '54'] <- "Benchaar and Hassanat, 2019"
GHG.table.CH4$`Authors and year`[GHG.table.CH4$ID == '56'] <- "McVoitte and Clark, 2019"
GHG.table.CH4$`Authors and year`[GHG.table.CH4$ID == '64'] <- "Dimitrov and Wang, 2019"
GHG.table.CH4$`Authors and year`[GHG.table.CH4$ID == '68'] <- "Huang and Guo, 2019"
GHG.table.CH4$`Authors and year`[GHG.table.CH4$ID == '89'] <- "Hao and Larney, 2017"
GHG.table.CH4$`Authors and year`[GHG.table.CH4$ID == '107'] <- "Baldé et al., 2016a"
GHG.table.CH4$`Authors and year`[GHG.table.CH4$ID == '108'] <- "Saady and Massé, 2016"
GHG.table.CH4$`Authors and year`[GHG.table.CH4$ID == '110'] <- "Baldé et al., 2016b"
GHG.table.CH4$`Authors and year`[GHG.table.CH4$ID == '117'] <- "Baldé et al., 2016c"
GHG.table.CH4$`Authors and year`[GHG.table.CH4$ID == '125'] <- "Massé and Saady, 2015"
GHG.table.CH4$`Authors and year`[GHG.table.CH4$ID == '126'] <- "Saady and Massé, 2015"
GHG.table.CH4$`Authors and year`[GHG.table.CH4$ID == '139'] <- "Jayasundara and Wagner-Riddle, 2014"
GHG.table.CH4$`Authors and year`[GHG.table.CH4$ID == '150'] <- "Saady and Massé, 2013"
GHG.table.CH4$`Authors and year`[GHG.table.CH4$ID == '157'] <- "McGinn and Beauchemin, 2012"
GHG.table.CH4$`Authors and year`[GHG.table.CH4$ID == '178'] <- "Park and Wagner-Riddle, 2010"
GHG.table.CH4$`Authors and year`[GHG.table.CH4$ID == '208'] <- "Hao, 2007"
GHG.table.CH4$`Authors and year`[GHG.table.CH4$ID == '241'] <- "Massé and Droste, 2000"
GHG.table.CH4$`Authors and year`[GHG.table.CH4$ID == '261'] <- "Bhatt and Abbassi, 2022"
GHG.table.CH4$`Authors and year`[GHG.table.CH4$ID == '265'] <- "VanderZaag and Baldé, 2022"

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

GHG.table.CH4 <- GHG.table.CH4 %>%
  arrange(factor(Region, levels = c("BC","AB","SK","MB","ON","QC","NB","NS","PE","NL","AB, ON","AB, QC","Multiple")), Year, `Authors and year`) %>%
  select(-Year, -ID)

  

#Format table
gt_GHG.Table.CH4 <-
  gt(GHG.table.CH4) %>%
  tab_row_group(
    label = "British Columbia",
    rows = GHG.table.CH4$Region == "BC") %>%
  tab_row_group(
    label = "Alberta",
    rows = GHG.table.CH4$Region == "AB") %>%
  tab_row_group(
    label = "Saskatchewan",
    rows = GHG.table.CH4$Region == "SK") %>%
  tab_row_group(
    label = "Manitoba",
    rows = GHG.table.CH4$Region == "MB") %>%
  tab_row_group(
    label = "Ontario",
    rows = GHG.table.CH4$Region == "ON") %>%
  tab_row_group(
    label = "Quebec",
    rows = GHG.table.CH4$Region == "QC") %>%
  tab_row_group(
    label = "Nova Scotia",
    rows = GHG.table.CH4$Region == "NS") %>%
  tab_row_group(
    label = "Multiple",
    rows = GHG.table.CH4$Region %in% c("AB, ON","AB, QC","Multiple")) %>%
  row_group_order(groups = c(
    "British Columbia", "Alberta", "Saskatchewan", "Manitoba", "Ontario", "Quebec", "Nova Scotia", "Multiple")) %>%
  tab_footnote(
    footnote = md("Three or more authors are summarized with *et al.*"),
    locations = cells_column_labels(columns = "Authors and year")) %>%
  tab_footnote(
    footnote = "NA: Not applicable") %>%
  opt_footnote_marks(marks = "letters")
gt_GHG.Table.CH4

#Export file
gt_GHG.Table.CH4 %>%
  gtsave("Table1 - Studies.docx")
