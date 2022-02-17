# Credit & dept

# Data source: socioeconomic household survey 2018-2019

# Sections 
# 15a + 15b, credit, 1., 8., 10.

# load data
datapath1 <- "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Data/Paper1/Socioeconomic Survey 2018-2019/sect15a_hh_w4.csv"
datapath2 <- "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Data/Paper1/Socioeconomic Survey 2018-2019/sect15b_hh_w4.csv"

data1 <- read.csv(datapath1)
data2 <- read.csv(datapath2)

data <- full_join(data1, data2)
data <- data[data$saq14 == "1. RURAL",]

data$s15q10c[data$s15q01 == "2. NO"] <- 0
data$s15q10c[data$s15q08 == "1. YES"] <- 0
quantile(data$s15q10c, probs = seq(0,1,by=0.1))

table(data$s15q01)
data$s15q01 <- as.character(data$s15q01)

hist(data$s15q10c[data$s15q01 == "1. YES"], main = "Debt", col = "black", breaks = seq(0,100800,by=100),
     xlab = "Household debt of households with loans (birr)")

hist(data$s15q10c[data$s15q01 == "1. YES"], main = "Debt (zoom)", col = "black", breaks = seq(0,100800,by=100),
     xlim = c(0,10000), xlab = "Household debt of households with loans (birr)")

hist(data$s15q10c, main = "Debt", col = "black", breaks = seq(0,100800,by=100),
     xlab = "Household debt (birr) of all households")

data$s15q01[data$s15q01 == "1. YES"] <- 1
data$s15q01[data$s15q01 == "2. NO"] <- 0
data$s15q01 <- as.numeric(data$s15q01)

hhdata <- data %>% group_by(household_id) %>% summarise(credit = sum(s15q01),
                                                        debt = sum(s15q10c))
creditpath <- "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Stylized model - clean/rscripts/abm_stylised/summary_data/credit.csv"
write.csv(hhdata, file = creditpath, row.names=FALSE)

