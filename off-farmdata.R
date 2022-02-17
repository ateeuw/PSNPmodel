# Non-farm income

# Data source: socioeconomic household survey 2018-2019

# Sections 
# 12a + 12b, credit, 1. & 16.

# load data
datapath1 <- "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Data/Paper1/Socioeconomic Survey 2018-2019/sect12a_hh_w4.csv"
datapath2 <- "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Data/Paper1/Socioeconomic Survey 2018-2019/sect12b1_hh_w4.csv"

data1 <- read.csv(datapath1)
data2 <- read.csv(datapath2)

colnames(data1)[13:20] <- c("off1 vocational service company", "off2 food processing business", "off3 trading business", 
                            "off4 non-food raw material & crafts sales", "off5 professional services company",
                            "off6 transportation business", "off7 restaurant/bar", "off8 other")

data <- full_join(data1, data2)
data <- data[data$saq14 == "1. RURAL",]

par(mfrow = c(2,4))

pie(table(data1[,13]), main = "vocational service company")
pie(table(data1[,14]), main = "food processing businessy")
pie(table(data1[,15]), main = "trading business")
pie(table(data1[,16]), main = "non-food raw material & crafts sales")
pie(table(data1[,17]), main = "professional services company")
pie(table(data1[,18]), main = "transportation business")
pie(table(data1[,19]), main = "restaurant/bar")
pie(table(data1[,20]), main = "other")


