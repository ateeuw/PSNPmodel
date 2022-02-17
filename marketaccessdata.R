# Market access data

# Data source: socioeconomic household survey 2018-2019

# Sections 
# 5a savings, 26. <- this is very limited, but the only info we have as far as I've seen


accessdata <- "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Data/Paper1/Socioeconomic Survey 2018-2019/sect5a_hh_w4.csv"
rosterdata <- "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Data/Paper1/Socioeconomic Survey 2018-2019/sect1_hh_w4.csv"

# load data
data <- read.csv(accessdata)
roster <- read.csv(rosterdata)

data <- data[data$saq14 == "1. RURAL",]

hist(data$s5aq26, main = "market access", col = "lightblue", breaks = 200,
     xlab = "distance to formal financial institution (km)")

hist(data$s5aq26, main = "market access (zoom)", col = "lightblue", xlim = c(0,100), 
     breaks = 200, xlab = "distance to formal financial institution (km)")

# distances given per individual, will ignore NAs and calculate average per household
data <- data[!is.na(data$s5aq26),]

data <- data %>% group_by(household_id) %>% summarize(market_access = mean(s5aq26))

accesspath <- "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Stylized model - clean/rscripts/abm_stylised/summary_data/marketaccess.csv"
write.csv(data, file = accesspath, row.names=FALSE)


