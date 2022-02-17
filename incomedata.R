# income

# income data
incomedata <- "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Data/Paper1/Teff Value Chain in Ethiopia Producer Household Survey//031_SectionM_Non-CropIncome.dta"

#load libraries
library(readstata13) #needed to read .dta (stata) files
library(ggplot2)
library(dplyr)
library(bbmle)
library(scales)
library(ggpubr)
library(cowplot)

# load data
data31 <- read.dta13(incomedata, generate.factors=T, nonint.factors = TRUE)
colnames(data31) <- c("hhid", "source", "relevant", "amount")

data31$amount <- as.numeric(as.character(data31$amount))

income_ag <- aggregate(amount ~ hhid, data31, sum)

hist(income_ag$amount, 
     probability = TRUE,
     #ylim = c(0,200), 
     xlim = c(0,30000), 
     breaks = 1000,
     col = "lightblue",
     main = "total household income (birr)",
     xaxt="n", xlab = "annual income (ETB)")

abline(v = mean(income_ag$amount), col = "red", lty = 3)
text(x = 7000, y = 0.0002, "mean", col = "red")

axis(side=1, at=seq(0,30000, by = 1000), labels=paste(seq(0,30,1), "k"))

library(MASS)
expfit <- fitdistr(income_ag$amount, "exponential")
expfit$estimate
curve(dexp(x, rate = expfit$estimate), add = TRUE, col = "blue", lwd = 4, lty = 2)
text(x = 18000, y = 0.0001, "exponential distribution, rate = 0.0002", col = "blue")

# LL3 <- function(a, s) {
#   -sum(dgamma(income_ag$amount, shape = a, scale = s))
# }
# 
# mle3 <- mle2(LL3, start = list(a = 0.5, s = length(income_ag$amount)))

#curve(dgamma(x, shape = 990, scale = 204), col = "gold", lwd = 4, lty = 4)


quantile(income_ag$amount, probs = seq(0, 1, 0.25))
quantile(income_ag$amount, probs = seq(0, 1, 0.2))
quantile(income_ag$amount, probs = seq(0, 1, 0.1))
mean(income_ag$amount)

# Now income from ESS 18/19
# Not given directly, but check consumption as indication?
cons <- "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Data/Paper1/Socioeconomic Survey 2018-2019/cons_agg_w4.csv" # 

cons <- read.csv(cons)
cons <- cons[cons$saq14 == "RURAL",]

quantile(cons$total_cons_ann, probs = c(seq(0,1,0.1)))

hist(cons$total_cons_ann/1000, 
     breaks = 100, 
     #labels = TRUE,
     #xaxp = c(0,20,1),
     main = "Household spending", 
     col = "goldenrod", #ylim = c(0,3000), xlim = c(0,220),
     xlab = "Household spending (1000* birr per year)")

quantile(cons$total_cons_ann/1000, probs = seq(0,1, by = 0.1))

abline(v = mean(cons$total_cons_ann/1000), col = "red", lty = 4, lwd = 3)
text(y = 400, x = 190, "mean: 48K birr", col = "red")

abline(v = median(cons$total_cons_ann/1000), col = "darkgreen", lty = 5, lwd = 3)
text(y = 500, x = 190, "median: 38K birr", col = "darkgreen")

hist(cons$total_cons_ann/1000, 
     breaks = 100, 
     #labels = TRUE,
     main = "Household spending (zoom)", 
     col = "goldenrod", xlim = c(0,400),
     xlab = "Household spending (1000* birr per year)")

quantile(cons$total_cons_ann/1000, probs = seq(0,1, by = 0.1))

abline(v = mean(cons$total_cons_ann/1000), col = "red", lty = 4, lwd = 3)
text(y = 400, x = 110, "mean: 48K birr", col = "red")

abline(v = median(cons$total_cons_ann/1000), col = "darkgreen", lty = 5, lwd = 3)
text(y = 500, x = 100, "median: 38K birr", col = "darkgreen")

hhspending <- data.frame(household_id = cons$household_id,
                         spending = cons$total_cons_ann)

spendingpath <- "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Stylized model - clean/rscripts/abm_stylised/summary_data/spending.csv"

write.csv(hhspending, file = spendingpath, row.names=FALSE)
