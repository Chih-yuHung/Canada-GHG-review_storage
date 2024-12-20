#This script looks at the evolution of emissions between 1990 and 2022
library(tidyverse); library(gt)

#NIR N2O data, need to convert to kT CO2e by multiplying 265
N2O <- read_csv("input/N2O_summary_Canada.csv")

#Make rows "Other" and "Total"
N2O_Livestock_Other <- N2O %>%
  filter(grepl('Buffalo|Camels|Deer|Elk|Fox|Fur-bearing Animals|Goat|Mink|
               Llama and alpaca|Mules and Asses|Rabbit|Wild boars|Others',Livestock)) %>%
  add_row(Livestock = "Other", !!!colSums(.[5:37], na.rm = TRUE)) %>%
  slice(n())
N2O <- N2O %>%
  filter(grepl('Non-dairy cattle|Dairy cattle|Swine|Poultry|Horses|Sheep',Livestock))
N2O_Evolution <- bind_rows(N2O,N2O_Livestock_Other) %>%
  add_row(Livestock = "Total", !!!colSums(.[2:37]))

#Change names of livestock
Livestock_Name <- c("Non-dairy cattle"="Beef cattle",
                    "Dairy cattle"="Dairy cattle",
                    "Swine"="Swine",
                    "Poultry"="Poultry", 
                    "Horses"="Horse",
                    "Other"="Other",
                    "Sheep"="Sheep",
                    "Total"="Total")
N2O_Evolution$Livestock <- as.character(Livestock_Name[N2O_Evolution$Livestock]) 

N2O_Change <- N2O_Evolution %>%
  select('Livestock', '2005', '2022') %>%
  mutate(across('2005':'2022', ~ .x * 265)) # converted to kt CO2e

# Make column for the change in emissions of N2O
N2O_Change$'Change in N₂O emissions (kt CO₂e)' <- (N2O_Change$'2022' - N2O_Change$'2005')
N2O_Change$'Change in N₂O emissions (%)' <- (N2O_Change$'Change in N₂O emissions (kt CO₂e)' / N2O_Change$'2005' * 100)
N2O_Change <- N2O_Change %>%
  select(c('Livestock','N₂O emissions in 2005 (kt CO₂e)' = '2005', 'N₂O emissions in 2022 (kt CO₂e)' = '2022',
           'Change in N₂O emissions (%)'))
N2O_Change <- N2O_Change %>%
  arrange(factor(Livestock, levels = c("Beef Cattle","Dairy Cattle","Swine","Poultry","Horse","Sheep","Other","Total")))

#NIR CH4 data, convert to kT CO2e
CH4 <- read.csv("input/CH4_Provincial_summary_manure.csv", header = TRUE)
CH4 <- CH4 %>%
  mutate(CH4_kTCO2eq = MM_MTCO2e * 1000, CH4_kTCO2eq = round(CH4_kTCO2eq, digits = 1))
CH4 <- CH4 %>%
  select(Inv_Year_ID, Province_ID, EF_Category_Name, CH4_kTCO2eq) %>%
  arrange(Inv_Year_ID, EF_Category_Name)

#Make table with EF_Category_Names as rows and Province_ID as columns
CH4 <- CH4 %>%
  group_by(Province_ID) %>%
  pivot_wider(names_from = "Inv_Year_ID", values_from = "CH4_kTCO2eq")
CH4 <- CH4 %>%
  group_by(EF_Category_Name) %>%
  summarize(across(where(is.numeric), ~ sum(.x, na.rm = T)),
            across(where(is.character), ~.x[!is.na(.x)][1])) %>%  
  relocate(colnames(CH4))

#Make rows "Other" and "Total"
CH4_Livestock_Other <- CH4 %>%
  filter(grepl('Buffalo|Deer|Elk|Fox|Goat|Mink|Llama and alpaca|Mules and Asses|Rabbit|Wild boars',EF_Category_Name)) %>%
  add_row(EF_Category_Name = "Other", !!!colSums(.[3:35]))
CH4_Livestock_Other <- CH4_Livestock_Other %>%
  filter(grepl('Other',EF_Category_Name))
CH4 <- CH4 %>%
  filter(grepl('Non-Dairy Cattle|Dairy Cattle|Swine|Poultry|Horse|Sheep and Lamb|Other',EF_Category_Name))
CH4_Evolution <- bind_rows(CH4,CH4_Livestock_Other) %>%
  add_row(EF_Category_Name = "Total", !!!colSums(.[3:35]))

#Change names of livestock
Livestock_Name <- c("Non-Dairy Cattle"="Beef cattle",
                    "Dairy Cattle"="Dairy cattle",
                    "Swine"="Swine",
                    "Poultry"="Poultry", 
                    "Horse"="Horse",
                    "Other"="Other",
                    "Sheep and Lamb"="Sheep",
                    "Total"="Total")
CH4_Evolution$Livestock <- as.character(Livestock_Name[CH4_Evolution$EF_Category_Name])
CH4_Evolution <- CH4_Evolution %>%
  select(c('Livestock','1990','1991','1992','1993','1994','1995','1996','1997','1998',
           '1999','2000','2001','2002','2003','2004','2005','2006','2007','2008',
           '2009','2010','2011','2012','2013','2014','2015','2016','2017','2018',
           '2019','2020','2021','2022'))



#Table 3 Change between 2005 and 2022
CH4_Change <- CH4_Evolution %>%
  select(c('Livestock', '2005', '2022'))

# Add animal count
AGR_census <- read_csv("input/Canada_Census_of_Agriculture_2006_2021.csv",
                       col_types = cols(`Animal (2006)` = col_number(),
                                        `Animal (2021)` = col_number()))
AGR_census <- AGR_census %>%
  filter(grepl('Beef Cattle|Dairy Cattle|Swine|Poultry|Horse|Sheep',Livestock)) %>%
  add_row(Livestock = "Other") %>%
  add_row(Livestock = "Total")

#Change names of livestock
Livestock_Name <- c("Beef Cattle"="Beef cattle",
                    "Dairy Cattle"="Dairy cattle",
                    "Swine"="Swine",
                    "Poultry"="Poultry", 
                    "Horse"="Horse",
                    "Other"="Other",
                    "Sheep"="Sheep",
                    "Total"="Total")
AGR_census$Livestock <- as.character(Livestock_Name[AGR_census$Livestock])

# Make columns for the changes
AGR_census$'Change in number of animals' <- (AGR_census$'Animal (2021)' - AGR_census$'Animal (2006)')
AGR_census$'Change in animal population (%)' <- (AGR_census$'Change in number of animals' / AGR_census$'Animal (2006)' * 100)
CH4_Change$'Change in emissions (kt CO2 eq)' <- (CH4_Change$'2022' - CH4_Change$'2005')
CH4_Change$'Change in CH₄ emissions (%)' <- (CH4_Change$'Change in emissions (kt CO2 eq)' / CH4_Change$'2005' * 100)
Change <- merge(CH4_Change, N2O_Change, by="Livestock")
Change <- merge(Change, AGR_census, by="Livestock") 
Change <- Change %>%
  select(c('Livestock','CH₄ emissions in 2005 (kt CO₂e)' = '2005','CH₄ emissions in 2022 (kt CO₂e)' = '2022',
           'Change in CH₄ emissions (%)', 'N₂O emissions in 2005 (kt CO₂e)',
           'N₂O emissions in 2022 (kt CO₂e)','Change in N₂O emissions (%)',
           'Animal population in 2006' = 'Animal (2006)', 'Animal population in 2021' = 'Animal (2021)',
           'Change in animal population (%)'))

Change <- Change %>%
  arrange(factor(Livestock, levels = c("Beef cattle","Dairy cattle","Swine","Poultry","Horse","Sheep","Other","Total")))

#GT Table 3
gt_Change <- 
  gt(Change) %>%
  fmt_number(
    columns = 2:10,
    decimals = 1,
    use_seps = TRUE) %>%
  fmt_integer(
    columns = c(8,9),
    use_seps = TRUE) %>%
  tab_footnote(
    footnote = md("To avoid any confusion, animals were not counted for other 
                  because of the large diversity in animal size within this category."),
    locations = cells_body(columns = Livestock,  rows = 8)) %>%
  tab_footnote(
    footnote = "NA: Not available")%>%
  opt_footnote_marks(marks = "letters")

gt_Change

#Export table 3 Change between 2005 and 2022
#gt_Change %>%
#  gtsave("Table3 - Change in CH4 and N2O emissions and animals between 2005 and 2022.docx")



#Make graph of methane emissions by livestock from 1990 to 2022
#Make table on the long format
CH4_Evolution_Graph <- CH4_Evolution %>%
  pivot_longer(
    cols = '1990':'2022',
    names_to = "Year",
    values_to = "CH4_emissions")
CH4_Evolution_Graph$Livestock <- factor(CH4_Evolution_Graph$Livestock, 
                                        levels=c("Beef cattle","Dairy cattle",
                                                 "Swine","Poultry", "Horse", 
                                                 "Sheep","Other","Total"))
CH4_Evolution_Graph <- CH4_Evolution_Graph %>%
  as.data.frame() 
CH4_Evolution_Graph$Year <- as.numeric(CH4_Evolution_Graph$Year)

#Make graph
Graph_Evolution_CH4 <-  
  ggplot(data = na.omit(CH4_Evolution_Graph),
         aes(x = factor(Year), 
             y = CH4_emissions,
             group = Livestock,
             color = Livestock,
             factor(group))) +
  geom_line(lwd =1.1) + 
  labs(x = "Year", y = "CH₄ emissions (kT CO₂e)") +
  scale_color_manual(values = c("Beef cattle" = "violetred1", "Dairy cattle" = "deepskyblue2", 
                                "Swine" = "grey", "Poultry" = "goldenrod",
                                "Horse" = "darkmagenta", "Sheep" = "limegreen", 
                                "Other" = "violet", "Total" = "gold")) +
  guides(color = guide_legend(title = "Animal type")) +
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.5)) +
  theme(axis.text = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_text(size = 14),  # Adjust legend title font size
        legend.text = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  scale_x_discrete(breaks = seq(1990, 2030 , by = 5),
                   expand = expansion(mult = c(0, 0.04))) +
  geom_vline(xintercept = c('2005','2022'),
             lwd = 1.1,
             color = "grey",
             linetype="dashed") +
  annotate("text", x = -Inf, y = Inf, label = "(a) CH₄", hjust = -0.2, vjust = 1.2, size = 5)

#Make graph of N2O emissions by livestock from 1990 to 2022
#Make table on the long format
N2O_Evolution_Graph <- N2O_Evolution %>%
  select(-c(`Population 1990`,`Population 2005`, `Population 2021`)) %>%
  mutate(across('1990':'2022', ~ .x * 265)) %>% # converted to kt CO2e 
  pivot_longer(
    cols = '1990':'2022',
    names_to = "Year",
    values_to = "N2O_emissions")
N2O_Evolution_Graph$Livestock <- factor(N2O_Evolution_Graph$Livestock, 
                                        levels=c("Beef cattle","Dairy cattle",
                                                 "Swine","Poultry", "Horse", 
                                                 "Sheep","Other","Total"))
N2O_Evolution_Graph <- N2O_Evolution_Graph %>%
  as.data.frame() 
N2O_Evolution_Graph$Year <- as.numeric(N2O_Evolution_Graph$Year)

#Make the grapgh
Graph_Evolution_N2O <-  
  ggplot(data = na.omit(N2O_Evolution_Graph),
         aes(x = factor(Year), 
             y = N2O_emissions,
             group = Livestock,
             color = Livestock,
             factor(group))) +
  geom_line(lwd =1.1) + 
  labs(x = "Year", y = "N₂O emissions (kT CO₂e)") +
  scale_color_manual(values = c("Beef cattle" = "violetred1", "Dairy cattle" = "deepskyblue2", 
                                "Swine" = "grey", "Poultry" = "goldenrod",
                                "Horse" = "darkmagenta", "Sheep" = "limegreen", 
                                "Other" = "violet", "Total" = "gold")) +
  guides(color = guide_legend(title = "Animal type")) +
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.5)) +
  theme(axis.text = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_text(size = 14),  # Adjust legend title font size
        legend.text = element_text(size = 12),
        axis.line = element_line(color = "black")) +
  scale_x_discrete(breaks = seq(1990, 2030 , by = 5),
                   expand = expansion(mult = c(0, 0.04))) +
  geom_vline(xintercept = c('2005','2022'),
             lwd = 1.1,
             color = "grey",
             linetype="dashed")  +
  annotate("text", x = -Inf, y = Inf, label = "(b) N₂O", hjust = -0.2, vjust = 1.2, size = 5)

#Export figure
ggsave("output/Figure 1 - Evolution between 1990 and 2022 CH4.png", Graph_Evolution_CH4,
       width = 24, height = 14, units = "cm",
       dpi = 300)
ggsave("output/Figure 1 - Evolution between 1990 and 2022 N2O.png", Graph_Evolution_N2O,
       width = 24, height = 14, units = "cm",
       dpi = 300)
