#This script is to analyze the data for N2O emissions from manure management
library(tidyverse);library(gt)

#NIR N2O data, convert to kT CO2e
N2O <- read.csv("input/N2O_emissions_inventory_2022.csv", header = TRUE)
N2O.eq <- N2O %>%
  mutate(across(where(is.numeric), function(x) x*298))
N2O.eq <- N2O.eq %>%
  data.frame(Provinces = c("Direct N2O Emissions", "Liquid storage", "Solid storage", 
                           "Composting", "Other", "Indirect N2O Emissions", 
                           "Ammonia volatilization", "N leaching")) %>%
  filter(Provinces != 'Composting') %>%
  select("Provinces", "Canada", "BC",	"AB",	"SK",	"MB",	"ON",	"QC",	"NB",	"NS",	"PE",	"NL")
N2O.eq <- N2O.eq %>%
  add_row(Provinces = "Total", !!!colSums(subset(N2O.eq, Provinces %in% c("Direct N2O Emissions", "Indirect N2O Emissions"), 
                                                 c("Canada", "BC",	"AB",	"SK",	"MB",	"ON",	"QC",	"NB",	"NS",	"PE",	"NL"))))
N2O.canada <- N2O.eq %>%
  select("Provinces", "Canada")
N2O.p <- as.data.frame(round(proportions(as.matrix(N2O.eq[,3:12]),1)*100,1))
N2O.p <- bind_cols(N2O.canada, N2O.p)
colnames(N2O.p) <- c('Manure type','Total', 'BC','AB','SK','MB','ON','QC','NB','NS','PE','NL')
N2O.p <- N2O.p %>%
  slice(1:5,8)



#Study frequency data
GHG.N2O <- read.csv("input/Canada GHG storage lit review data 20240611.csv",header = T)

#Obtain studies with storage measurement 
GHG.storage <- GHG.N2O[grepl("Storage", GHG.N2O$GHG.source),]
GHG.storage <- GHG.storage[GHG.storage$N2O==TRUE,]
#exclude National studies because national studies used modelling or other method only
#This substantially increased the study count in some provinces
GHG.storage <- GHG.storage[GHG.storage$Region!="National",]
GHG.storage <- GHG.storage %>%
  drop_na(Manure.type)

#Province frequency
Study.province <- GHG.storage %>%
  separate_rows(Region, sep = ", ") %>%
  mutate(Manure.type = str_split(Manure.type, ",\\s*")) %>%
  unnest(Manure.type) %>%
  count(Region, Manure.type) %>%
  pivot_wider(names_from = Region, values_from = n, values_fill = 0) %>%
  mutate(Total = rowSums(across(-Manure.type, .names = "n_{.col}")))
 
#Add missing provinces
missing_provinces <- c("NB","PE","NL")
Study.province[,missing_provinces] <- 0
Study.province <- Study.province %>%
  select(c('Manure.type','Total','British Columbia','Alberta','Saskatchewan','Manitoba','Ontario','Quebec','NB','Nova Scotia','PE','NL'))
colnames(Study.province) <- c('Manure type','Total','BC','AB','SK','MB','ON','QC','NB','NS','PE','NL')

#Get total studies per province
Study.province <- Study.province %>%
  add_row("Manure type" = "Total", !!!colSums(.[2:12]))
Study.canada <- Study.province %>%
  select(c("Manure type", "Total"))
Study.total <- Study.province %>%
  slice(n())

#convert to percentage
Study.province.p <- as.data.frame(round(proportions(as.matrix(Study.province[,3:12]),1)*100,1))
Study.province.p <- bind_cols(Study.canada, Study.province.p)
Study.province.p$`Manure type`[Study.province.p$`Manure type` == 'Liquid'] <- "Liquid storage"
Study.province.p$`Manure type`[Study.province.p$`Manure type` == 'Solid'] <- "Solid storage"



#Make Table 2
N2O.Table5 <- rbind(Study.total, Study.province.p, N2O.p)
gt_N2O.Table5 <-
  gt(N2O.Table5) %>%
  tab_row_group(
    label = "Studies (n)",
    rows = 1) %>%
  tab_row_group(
    label = "N2O emissions (%)",
    rows = 5:10) %>%
  tab_row_group(
    label = "Study (%)",
    rows = 2:4) %>%
  row_group_order(groups = c("Studies (n)", "Study (%)", "N2O emissions (%)")) %>%
  tab_stubhead(label = "Studies (n)") %>%
  cols_label(
    'Manure type' = "",
    Total = "Province") %>%
  fmt_integer(
    columns = 3:12,
    rows = 1,
    use_seps = TRUE) %>%
  fmt_number(
    columns = 2:12,
    rows = 2:10,
    decimals = 1,
    use_seps = TRUE) %>%
  fmt_integer(
    columns = "Total",
    rows = 1:4,
    use_seps = TRUE)
gt_N2O.Table5


#Export file
gt_N2O.Table5 %>%
  gtsave("Table 2 - Manure management emission N2O.docx")
