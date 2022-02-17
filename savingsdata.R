# Financial assets

# Data source: socioeconomic household survey 2018-2019

# Sections 
# 5b, financial assets, 9

assetdata <- "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Data/Paper1/Socioeconomic Survey 2018-2019/sect5b2_hh_w4.csv"

# load data
data <- read.csv(assetdata)

data <- data[data$saq14 == "1. RURAL",]
str(data$s5bq09)
data$s5bq09 <- as.numeric(as.character(data$s5bq09))

hhdata <- data %>% group_by(household_id) %>% summarise(savings = sum(s5bq09))

hist(hhdata$savings/1000, main = "Financial assets", col = "black", breaks = 0:650,
     xlab = "Financial assets per household (1000*birr)")

quantile(hhdata$savings/1000, probs = seq(0,1,by=0.1), na.rm = TRUE)

hist(hhdata$savings/1000, main = "Financial assets (zoom)", col = "black", breaks = 0:650,
     xlim = c(0,30), xlab = "Financial assets per household (1000*birr)")

assetpath <- "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Stylized model - clean/rscripts/abm_stylised/summary_data/savings.csv"
write.csv(hhdata, file = assetpath, row.names=FALSE)
