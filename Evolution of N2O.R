#This script looks at the evolution of emissions between 1990 and 2022
library(tidyverse); library(gt)

#NIR N2O data, convert to kT CO2e
N2O <- read_csv("input/N2O_summary_Canada.csv")

#Make rows "Other" and "Total"
N2O_Livestock_Other <- N2O %>%
  filter(grepl('Buffalo|Camels|Deer|Elk|Fox|Fur-bearing Animals|Goat|Mink|
               Llama and alpaca|Mules and Asses|Rabbit|Wild boars|Others',Livestock)) %>%
  add_row(Livestock = "Other", !!!colSums(.[5:36], na.rm = TRUE)) %>%
  slice(n())
N2O <- N2O %>%
  filter(grepl('Non-dairy cattle|Dairy cattle|Swine|Poultry|Horses|Sheep',Livestock))
N2O_Evolution <- bind_rows(N2O,N2O_Livestock_Other) %>%
  add_row(Livestock = "Total", !!!colSums(.[2:36]))

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
  select('Livestock','Population 2005', 'Population 2021', '2005', '2022') %>%
  mutate(across('2005':'2021', ~ .x * 265))
N2O_Evolution <- N2O_Evolution %>%
  select(c('Livestock','1990','1991','1992','1993','1994','1995','1996','1997','1998',
           '1999','2000','2001','2002','2003','2004','2005','2006','2007','2008',
           '2009','2010','2011','2012','2013','2014','2015','2016','2017','2018',
           '2019','2020','2021')) %>%
  mutate(across('1990':'2021', ~ .x * 265))
N2O_Evolution <- N2O_Evolution %>%
  arrange(factor(Livestock, levels = c("Total","Beef cattle","Dairy cattle","Swine","Poultry","Horse","Sheep","Other")))

#Make graph of N2O emissions by livestock from 1990 to 2022
#Make table on the long format
N2O_Evolution_Graph <- N2O_Evolution %>%
  pivot_longer(
    cols = '1990':'2021',
    names_to = "Year",
    values_to = "N2O_emissions")

#Make graph
Graph_Evolution_N2O <-  
  ggplot(data = na.omit(N2O_Evolution_Graph),
         aes(x = factor(Year), 
             y = N2O_emissions,
             group = Livestock,
             color = Livestock,
             factor(group))) +
  geom_line(lwd =1.1) + 
  labs(x = "Year", y = html("N₂O emissions (kT CO₂e)")) +
  scale_color_manual(values = c("Beef cattle" = "violetred1", "Dairy cattle" = "deepskyblue2", 
                                "Swine" = "lightpink1", "Poultry" = "goldenrod",
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
  geom_vline(xintercept = c('2005','2021'),
             lwd = 1.1,
             color = "grey") 
Graph_Evolution_N2O

#Export figure
ggsave("output/Figure - Evolution between 1990 and 2022 N2O.png", Graph_Evolution_N2O,
       width = 24, height = 14, units = "cm",
       dpi = 300)



#Table 3 Change between 2005 and 2022
# Make columns for the changes
N2O_Change <- N2O_Change %>%
  mutate('Population 2005' = `Population 2005`*1000, 'Population 2021' = `Population 2021`*1000)
N2O_Change$'Change in number of animals' <- (N2O_Change$'Population 2021' - N2O_Change$'Population 2005')
N2O_Change$'Change in number of animals (%)' <- (N2O_Change$'Change in number of animals' / N2O_Change$'Population 2005' * 100)
N2O_Change$'Change in emissions (kT CO₂e)' <- (N2O_Change$'2021' - N2O_Change$'2005')
N2O_Change$'Change in emissions (%)' <- (N2O_Change$'Change in emissions (kT CO₂e)' / N2O_Change$'2005' * 100)
N2O_Change <- N2O_Change %>%
  select(c('Livestock','Population 2005', 'Population 2021', 'Change in number of animals (%)',
           'N₂O emissions in 2005 (kT CO₂e)' = '2005', 'N₂O emissions in 2021 (kT CO₂e)' = '2021',
           'Change in emissions (%)'))

#GT Table
gt_N2O_Change <- 
  gt(N2O_Change) %>%
  fmt_number(
    columns = 2:7,
    decimals = 1,
    use_seps = TRUE) %>%
  fmt_integer(
    columns = c(2,3),
    use_seps = TRUE) %>%
  tab_footnote(
    footnote = md("To avoid any confusion, animals were not counted for other 
                  because of the large diversity in animal size within this category."),
    locations = cells_body(columns = Livestock,  rows = 7)) %>%
  tab_footnote(
    footnote = "NA: Not available")%>%
  opt_footnote_marks(marks = "letters")

gt_N2O_Change

#Export table 3 Change between 2005 and 2022
gt_N2O_Change %>%
  gtsave("Table3 - Change in N2O emissions and animals between 2005 and 2022.docx")
