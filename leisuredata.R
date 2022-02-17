# Leisure

# Data source: socioeconomic household survey 2018-2019

# Sections 
  # 4 time use and labour, 3., 4., 5., 6., 9., 11., 13., 15., 39.-41.,

leisuredata <- "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Data/Paper1/Socioeconomic Survey 2018-2019/sect4_hh_w4.csv"
rosterdata <- "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Data/Paper1/Socioeconomic Survey 2018-2019/sect1_hh_w4.csv"

# load data
data <- read.csv(leisuredata)
roster <- read.csv(rosterdata)

roster$s1q03b[is.na(roster$s1q03b)] <- 0
roster$age <- round(roster$s1q03a + roster$s1q03b/12)

agedata <- data.frame(household_id = roster$household_id, 
                      individual_id = roster$individual_id, 
                      age = roster$age)

data <- merge(data, agedata, by = c("household_id", "individual_id"))
data$saq14 <- as.character(data$saq14)
data <- data[data$saq14 == "1. RURAL",]

# explore data
pdf(file = "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Data/Paper1/Socioeconomic Survey 2018-2019/leisure/leisure.pdf")

colnames(data)[colnames(data) == "s4q03a"] <- "water_fetching_h"
colnames(data)[colnames(data) == "s4q03b"] <- "water_fetching_m"
data$water_fetching <- data$water_fetching_h*60 + data$water_fetching_m
data$water_fetching[data$water_fetching > 12*60] <- NA

hist(data$water_fetching, main = "water fetching per person", col = "lightblue", breaks = 50,
     xlab = "time spent fetching water per person per day (minutes)")

hist(data$water_fetching[data$age %in% c(16:70)], main = "water fetching per adult", col = "lightblue", breaks = 50,
     xlab = "time spent fetching water per adult per day (minutes)")

waterhh <- data %>% group_by(household_id) %>% summarise(hh_water_fetching = sum(water_fetching))
hist(waterhh$hh_water_fetching, main = "water fetching per household", col = "lightblue", breaks = 50,
     xlab = "time spent fetching water per household per day (minutes)")

colnames(data)[colnames(data) == "s4q04a"] <- "fuel_collecting_h"
colnames(data)[colnames(data) == "s4q04b"] <- "fuel_collecting_m"
data$fuel_collecting <- data$fuel_collecting_h*60 + data$fuel_collecting_m
data$fuel_collecting[data$fuel_collecting > 12*60] <- NA

hist(data$fuel_collecting, main = "fuel collecting per person", col = "darksalmon", breaks = 50,
     xlab = "time spent collecting fuel per person per day (minutes)")

hist(data$fuel_collecting[data$age %in% c(16:70)], main = "fuel collecting per adult", col = "darksalmon", breaks = 50,
     xlab = "time spent collecting fuel per adult per day (minutes)")

fuelhh <- data %>% group_by(household_id) %>% summarise(hh_fuel_collecting = sum(fuel_collecting))
hist(fuelhh$hh_fuel_collecting, main = "fuel collecting per household", col = "darksalmon", breaks = 50,
     xlab = "time spent collecting fuel per household per day (minutes)")

# Agriculture
colnames(data)[colnames(data) == "s4q05"] <- "agriculture_YN"
colnames(data)[colnames(data) == "s4q06"] <- "agriculture_h"
data$agriculture <- data$agriculture_h
data$agriculture_YN <- as.character(data$agriculture_YN)
data$agriculture[data$agriculture_YN == "2. NO"] <- 0

hist(data$agriculture, main = "agriculture per person", col = "burlywood", breaks = 50,
     xlab = "time spent on agriculture per person per week (hours)")

hist(data$agriculture[data$age %in% c(16:70)], main = "agriculture per adult", col = "burlywood", breaks = 50,
     xlab = "time spent on agriculture per adult per week (hours)")

agriculturehh <- data %>% group_by(household_id) %>% summarise(hh_agriculture = sum(agriculture))
hist(agriculturehh$hh_agriculture, main = "agriculture per household", col = "burlywood", breaks = 50,
     xlab = "time spent on agriculture per household per day (hours)")

# Business
colnames(data)[colnames(data) == "s4q08"] <- "business_YN"
colnames(data)[colnames(data) == "s4q09"] <- "business_h"
data$business <- data$business_h
data$business_YN <- as.character(data$business_YN)
data$business[data$business_YN == "2. NO"] <- 0

hist(data$business, main = "business per person", col = "beige", breaks = 50,
     xlab = "time spent on household business per person per week (hours)")

hist(data$business[data$age %in% c(16:70)], main = "business per adult", col = "beige", breaks = 50,
     xlab = "time spent on household business per adult per week (hours)")

businesshh <- data %>% group_by(household_id) %>% summarise(hh_business = sum(business))
hist(businesshh$hh_business, main = "business per household", col = "beige", breaks = 50,
     xlab = "time spent on household business per household per day (hours)")

# Casual
colnames(data)[colnames(data) == "s4q11"] <- "casual_h"
colnames(data)[colnames(data) == "s4q10"] <- "casual_YN"
data$casual <- data$casual_h
data$casual_YN <- as.character(data$casual_YN)
data$casual[data$casual_YN == "2. NO"] <- 0

hist(data$casual, main = "casual work per person", col = "beige", breaks = 50,
     xlab = "time spent on household business per person per week (hours)")

hist(data$casual[data$age %in% c(16:70)], main = "casual work per adult", col = "aquamarine", breaks = 50,
     xlab = "time spent on casual/part-time/temporary work per adult per week (hours)")

casualhh <- data %>% group_by(household_id) %>% summarise(hh_casual = sum(casual))
hist(casualhh$hh_casual, main = "casual work per household", col = "aquamarine", breaks = 50,
     xlab = "time spent on casual/part-time/temporary work per household per day (hours)")

# Wage
colnames(data)[colnames(data) == "s4q13"] <- "wage_h"
colnames(data)[colnames(data) == "s4q12"] <- "wage_YN"
data$wage <- data$wage_h
data$wage_YN <- as.character(data$wage_YN)
data$wage[data$wage_YN == "2. NO"] <- 0

hist(data$wage, main = "wage work per person", col = "brown1", breaks = 50,
     xlab = "time spent on wage work per person per week (hours)")

hist(data$wage[data$age %in% c(16:70)], main = "wage work per adult", col = "brown1", breaks = 50,
     xlab = "time spent on wage work per adult per week (hours)")

wagehh <- data %>% group_by(household_id) %>% summarise(hh_wage = sum(wage))
hist(wagehh$hh_wage, main = "wage work per household", col = "brown1", breaks = 50,
     xlab = "time spent on wage work per household per day (hours)")

# Apprenticeship
colnames(data)[colnames(data) == "s4q15"] <- "apprenticeship_h"
colnames(data)[colnames(data) == "s4q14"] <- "apprenticeship_YN"
data$apprenticeship <- data$apprenticeship_h
data$apprenticeship_YN <- as.character(data$apprenticeship_YN)
data$apprenticeship[data$apprenticeship_YN == "2. NO"] <- 0

hist(data$apprenticeship, main = "apprenticeship per person", col = "cadetblue1", breaks = 50,
     xlab = "time spent on unpaid apprenticeship per person per week (hours)")

hist(data$apprenticeship[data$age %in% c(16:70)], main = "apprenticeship per adult", col = "cadetblue1", breaks = 50,
     xlab = "time spent on unpaid apprenticeship per adult per week (hours)")

apprenticeshiphh <- data %>% group_by(household_id) %>% summarise(hh_apprenticeship = sum(apprenticeship))
hist(apprenticeshiphh$hh_apprenticeship, main = "apprenticeship per household", col = "cadetblue1", breaks = 50,
     xlab = "time spent on unpaid apprenticeship per household per day (hours)")

# Sum of all labour
data$laboursum <- (data$water_fetching*7)/60 + (data$fuel_collecting*7)/60 + #given in minutes per day
  data$agriculture + data$business + data$casual + data$apprenticeship # given in hours per week

hist(data$laboursum, main = "total workload per person", col = "chartreuse", breaks = 50,
     xlab = "time spent working per person per week (hours)")

hist(data$laboursum[data$age %in% c(16:70)], main = "total workload per adult", col = "chartreuse", breaks = 50,
     xlab = "time spent working per adult per week (hours)")

laboursumhh <- data %>% group_by(household_id) %>% summarise(hh_laboursum = sum(laboursum))
hist(laboursumhh$hh_laboursum, main = "total workload per household", col = "chartreuse", breaks = 50,
     xlab = "time spent working per household per day (hours)")

dev.off()

hist(data$laboursum[data$age %in% c(16:70)], main = "total workload per adult", col = "chartreuse", breaks = 50,
     xlab = "time spent working per adult per week (hours)")

laboursum <- data$laboursum[data$age %in% c(16:70)]
laboursum <- laboursum[!is.na(laboursum)]
quantile(round(laboursum), probs = seq(0, 1, 0.1))

abline(v = mean(laboursum), col = "red", lty = 3)
text(x = 35, y = 1500, "mean: 22.1 hours", col = "red")

#axis(side=1, at=seq(0,30000, by = 1000), labels=paste(seq(0,30,1), "k"))

library(MASS)
expfit <- fitdistr(laboursum, "exponential")
expfit$estimate

gamfit <- fitdistr(laboursum, "gamma")
gamfit$estimate

# curve(dexp(x, rate = expfit$estimate), add = TRUE, col = "blue", lwd = 4, lty = 2)
# text(x = 18000, y = 0.0001, "exponential distribution, rate = 0.045", col = "blue")

laboursum_split <- colMeans(data.frame(apprenticeship = data$apprenticeship, 
                                       wage = data$wage, 
                                       casual = data$casual, 
                                       agriculture = data$agriculture, 
                                       fuel_collecting = data$fuel_collecting*7/60, 
                                       water_fetching = data$water_fetching*7/60), 
                            na.rm = TRUE) 
laboursum_split

slices <- laboursum_split
pie(slices, radius = 2, col = c("cadetblue1", "brown1", "aquamarine", "burlywood", "darksalmon", "lightblue"), #apprentice, wage, casual, agri, fuel, water
    main="mean work allocation (hours per week per person)")

labourpath <- "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Stylized model - clean/rscripts/abm_stylised/summary_data/hhlabour.csv"

write.csv(laboursumhh, file = labourpath, row.names = FALSE)

