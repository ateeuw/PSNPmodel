# Education

# Data source: socioeconomic household survey 2018-2019

# Sections 
# 1 education, questions 4 and 6, used education of household head

eddata <- "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Data/Paper1/Socioeconomic Survey 2018-2019/sect2_hh_w4.csv"
rosterdata <- "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Data/Paper1/Socioeconomic Survey 2018-2019/sect1_hh_w4.csv"

# load data
data <- read.csv(eddata)
roster <- read.csv(rosterdata)

# get data with household head coupled to individual and household id to merge with education data
roster$s1q01 <- as.character(roster$s1q01)

roster <- data.frame(household_id = roster$household_id,
                     individual_id = roster$individual_id,
                     membertype = roster$s1q01)

data <- merge(data, roster)

# reduce data down to household heads only
data$membertype <- as.character(data$membertype)
data <- data[data$membertype == "1. Head",]

unique(sort(data$s2q06))

data$s2q06 <- as.character(data$s2q06)
data$s2q06[which(data$s2q04 == "2. NO")] <- "-1. No education" 

par(mar=c(18,4,2,2))
barplot(table(data$s2q06), las=2, cex.names=0.7)
axis(1, las=1)
