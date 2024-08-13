#Table of articles used for analysis
library(tidyverse); library(gt)

#Read data
GHG.CH4 <- read.csv("input/Canada GHG storage lit review data 20240611.csv",header = T)

#Obtain studies with storage measurement 
GHG.storage.CH4 <- GHG.CH4[grepl("Storage", GHG.CH4$GHG.source),]
GHG.table.CH4 <- GHG.storage.CH4 %>%
  subset(GHG.storage.CH4$CH4==TRUE) %>%
  select(ID, Region, Method = Technique, Manure = Manure.type, -CH4)

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
  return(paste(abbreviations, collapse = ", "))
}
# Apply the function to the method column in your dataframe
GHG.table.CH4$Method <- sapply(GHG.table.CH4$Method, replace_methods)
GHG.table.CH4$Method[GHG.table.CH4$Method == 'OB, OB'] <- "OB"

#Count of articles
Articles.CH4 <- GHG.table.CH4 %>%
  group_by(Region, Manure, Method) %>%
  summarise(Number = n()) %>%
  ungroup() %>%
  group_by(Region) %>%
  pivot_wider(names_from = "Region", values_from = "Number")
Articles.CH4[is.na(Articles.CH4)] <- 0
Articles.CH4 <- Articles.CH4 %>%
  group_by(Manure) %>%
  bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total"))

gt_articles.CH4 <-
  gt(Articles.CH4)
 gt_articles.CH4

 #Export file
 gt_articles.CH4 %>%
   gtsave("Table1 - New.docx") 
 