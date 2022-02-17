# Livestock data

livestockdata <- "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Data/Paper1/Socioeconomic Survey 2018-2019/sect8_1_ls_w4.csv"
rosterdata <- "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Data/Paper1/Socioeconomic Survey 2018-2019/sect1_hh_w4.csv"

# load data
data <- read.csv(livestockdata)
roster <- read.csv(rosterdata)

data$ls_code <- as.character(data$ls_code)
(livestocktypes <- unique(data$ls_code))

CF <- c(1.2, 1.42, 1, 0.85, 0.78, 0.2, 0.2, 0.2, 1.1, 0.04, 0.04, 0.04, 0.8, 1.15, 0.8, 0.01)

data$TLUnumcode <- as.numeric(gsub(".*?([0-9]+).*", "\\1", data$ls_code))     
data$TLUkept <- data$ls_s8_1q01*CF[data$TLUnumcode]

data$household_id <- as.character(data$household_id)
data$household_id <- as.factor(data$household_id)

datahh <- aggregate(data$TLUkept, by=list(household=data$household_id), FUN=sum)

mybins <- c(seq(0, ceiling(max(datahh$x)), by = 0.1))
hist(datahh$x, main = "livestock kept", col = "brown", breaks = mybins,
     xlab = "tropical livestock units") #, xlim = c(0,10))

hist(datahh$x, main = "livestock kept (zoom)", col = "brown", breaks = mybins,
     xlab = "tropical livestock units", xlim = c(0,20))
