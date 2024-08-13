#This script is to analyze the data for GHG emissions from manure management
library(tidyverse);library(gt)

#NIR CH4 data, convert to kT CO2e
CH4 <- read.csv("input/CH4_Provincial_summary_manure.csv", header = TRUE)
CH4.2022 <- CH4[grepl("2022", CH4$Inv_Year_ID),]
CH4.2022 <- CH4.2022 %>%
  mutate(CH4_kTCO2eq = MM_MTCO2e * 1000, CH4_kTCO2eq = round(CH4_kTCO2eq, digits = 1))
CH4.2022 <- CH4.2022 %>%
  select(Province_ID, EF_Category_Name, CH4_kTCO2eq) %>%
  arrange(EF_Category_Name)

#Make table with EF_Category_Names as rows and Province_ID as columns
CH4.2022 <- CH4.2022 %>%
  group_by(Province_ID) %>%
  pivot_wider(names_from = "Province_ID", values_from = "CH4_kTCO2eq")
CH4.2022 <- CH4.2022 %>%
  group_by(EF_Category_Name) %>%
  summarize(across(where(is.numeric), ~ mean(.x, na.rm = T)),
            across(where(is.character), ~.x[!is.na(.x)][1])) %>% 
  relocate(colnames(CH4.2022))

#Make rows "Other" and "Total"
CH4_Livestock_Other <- CH4.2022 %>%
  filter(grepl('Buffalo|Deer|Elk|Fox|Goat|Mink|Llama and alpaca|Mules and Asses|Rabbit|Wild boars',EF_Category_Name)) %>%
  add_row(EF_Category_Name = "Other", !!!colSums(.[2:11]))
CH4_Livestock_Other <- CH4_Livestock_Other %>%
  filter(grepl('Other',EF_Category_Name))
CH4.2022 <- CH4.2022 %>%
  filter(grepl('Non-Dairy Cattle|Dairy Cattle|Swine|Poultry|Horse|Sheep and Lamb|Other',EF_Category_Name))
CH4_Livestock <- bind_rows(CH4.2022,CH4_Livestock_Other) %>%
  add_row(EF_Category_Name = "Total", !!!colSums(.[2:11])) 

#Change names of livestocks
Livestock_Name <- c("Non-Dairy Cattle"="Beef Cattle",
                    "Dairy Cattle"="Dairy Cattle",
                    "Swine"="Swine",
                    "Poultry"="Poultry", 
                    "Horse"="Horse",
                    "Other"="Other",
                    "Sheep and Lamb"="Sheep",
                    "Total"="Total")
CH4_Livestock$Livestock <- as.character(Livestock_Name[CH4_Livestock$EF_Category_Name])
CH4_Livestock$Total <- rowSums(CH4_Livestock[2:11])
CH4_Livestock <- CH4_Livestock %>%
  select(c('Livestock','Total','BC','AB','SK','MB','ON','QC','NB','NS','PE','NL'))
CH4_Livestock <- CH4_Livestock %>%
  arrange(factor(Livestock, levels = c("Beef Cattle","Dairy Cattle","Swine","Poultry","Horse","Sheep","Other","Total")))

#Convert to percentage
CH4.p.Livestock <- as.data.frame(round(proportions(as.matrix(CH4_Livestock[,3:12]),1)*100,1))
CH4.p.Livestock <- CH4.p.Livestock %>%
  mutate(across(.cols = everything(),
                .fns = ~ case_when(.x <= 0.0 ~ '< 0.1',
                                   TRUE ~ as.character(.x))))
CH4_Livestock <- CH4_Livestock %>%
  select(c('Livestock','Total'))
CH4.p.Livestock <- bind_cols(CH4_Livestock,CH4.p.Livestock)



#Study frequency data
GHG.CH4 <- read.csv("input/Canada GHG storage lit review data 20240611.csv",header = T)

#Obtain studies with storage measurement 
GHG.storage <- GHG.CH4[grepl("Storage", GHG.CH4$GHG.source),]
GHG.storage <- GHG.storage[GHG.storage$CH4==TRUE,]
#exclude National studies because national studies used modelling or other method only
#This substantially increased the study count in some provinces
GHG.storage <- GHG.storage[GHG.storage$Region!="National",]
GHG.storage <- GHG.storage %>%
  drop_na(Livestock)

#Province frequency
Study.prov <- GHG.storage %>%
  separate_rows(Region, sep = ", ") %>%
  mutate(Livestock = str_split(Livestock, ",\\s*")) %>%
  unnest(Livestock) %>%
  count(Region, Livestock) %>%
  pivot_wider(names_from = Region, values_from = n, values_fill = 0) %>%
  mutate(Total = rowSums(across(-Livestock, .names = "n_{.col}"))) %>%
  # add a row for other, no other in my study
  {.[7, 1] <- "Other"; .} %>%
  {.[7, 2:9] <- 0; .} %>%
  add_row(Livestock = "Total", !!!colSums(.[2:9]))
Study.province <- Study.prov %>%
  arrange(factor(Livestock, levels = c("Beef Cattle","Dairy Cattle","Swine","Poultry","Horse","Sheep","Other","Total"))) 

#Add missing provinces
missing_provinces <- c("NB","PE","NL")
Study.province[,missing_provinces] <- 0
Study.province <- Study.province %>%
  select(c('Livestock','British Columbia','Alberta','Saskatchewan','Manitoba','Ontario','Quebec','NB','Nova Scotia','PE','NL','Total'))
colnames(Study.province) <- c('Livestock','BC','AB','SK','MB','ON','QC','NB','NS','PE','NL','Total')

#Get total studies per province
Study.province.total <- Study.province %>%   
  filter(grepl('Total',Livestock)) %>%
  select(c('Livestock','Total','BC','AB','SK','MB','ON','QC','NB','NS','PE','NL'))

#convert to percentage
Study.province.p <- as.data.frame(round(proportions(as.matrix(Study.province[,2:11]),1)*100,1))
rownames(Study.province.p) <- row.names(CH4.p.Livestock)
Study.province.p[7,] <- 0 
Study.province <- Study.province %>%
  select(c('Livestock','Total'))
Study.province.p <- bind_cols(Study.province,Study.province.p)



#Make Table 1
CH4.Table1 <- rbind(Study.province.total,Study.province.p,CH4.p.Livestock)
gt_CH4.Table1 <-
  gt(CH4.Table1) %>%
  tab_row_group(
    label = "Studies (n)",
    rows = 1) %>%
  tab_row_group(
    label = "CH₄ emissions (%)",
    rows = 10:17) %>%
  tab_row_group(
    label = "Study (%)",
    rows = 2:9) %>%
  row_group_order(groups = c("Studies (n)", "Study (%)", "CH₄ emissions (%)")) %>%
  tab_stubhead(label = "Studies (n)") %>%
  cols_label(
    Livestock = "",
    Total = "Province") %>%
  cols_align(
    align = "right",
    columns = everything()) %>%
  fmt_integer(
    columns = "Total",
    rows = 1:9,
    use_seps = TRUE) %>%
  fmt_integer(
    columns = 3:12,
    rows = 1,
    use_seps = TRUE) %>%
  fmt_number(
    columns = 2:12,
    rows = 2:17,
    decimals = 1,
    use_seps = TRUE) %>%
  tab_footnote(
    footnote = "There were 140 articles adressing CH₄ emissions across Canada.
                  Eight articles with national studies from mixed and modelling methods 
                  and one article with unspecified livestock type were excluded from Table 1.",
    locations = cells_stubhead()) %>%
  opt_footnote_marks(marks = "letters")
gt_CH4.Table1



#Export file
gt_CH4.Table1 %>%
  gtsave("Table 1 - Province and Livestock emission.docx")
