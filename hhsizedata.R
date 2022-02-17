# Household size

# Data source: socioeconomic household survey 2018-2019

# Sections 
# 1 time household roster, used individual_id column

rosterdata <- "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Data/Paper1/Socioeconomic Survey 2018-2019/sect1_hh_w4.csv"

# load libraries
library(dplyr)

# load data
roster <- read.csv(rosterdata)

# use household_id and individual_id to count hh members per household
roster <- roster[roster$saq14 == "1. RURAL",]

hh <- roster %>% group_by(household_id) %>% summarise(nmem = max(individual_id))

hist(hh$nmem, 
     breaks = seq(0,20, by = 1), 
     #labels = TRUE,
     #xaxp = c(0,20,1),
     main = "Household size", 
     col = "goldenrod", #ylim = c(0,3000), xlim = c(0,220),
     xlab = "Number of members per household")

quantile(hh$nmem, probs = seq(0,1, by = 0.1))

abline(v = mean(hh$nmem), col = "red", lty = 4, lwd = 3)
text(y = 450, x = 9.2, "mean: 5 members", col = "red")

abline(v = median(hh$nmem), col = "darkgreen", lty = 5, lwd = 3)
text(y = 500, x = 9, "median: 5 members", col = "darkgreen")

hhsizepath <- "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Stylized model - clean/rscripts/abm_stylised/summary_data/hhsize.csv"

write.csv(hh, file = hhsizepath, row.names = FALSE)
