# Food self-sufficiency

eatdata <- "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Data/Paper1/Socioeconomic Survey 2018-2019/sect6a_hh_w4.csv"
rosterdata <- "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Data/Paper1/Socioeconomic Survey 2018-2019/sect1_hh_w4.csv"
conversiondata <- "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Data/Paper1/Socioeconomic Survey 2018-2019/Food_CF_Wave4.csv" # Food_CF_Wave4

data <- read.csv(eatdata)
roster <- read.csv(rosterdata)
conversion <- read.csv(conversiondata)

# Maize consumption
data$item_cd <- as.character(data$item_cd)
maize <- data[data$item_cd == "104. Maize",]
maize$saq14 <- as.character(maize$saq14)
maize <- maize[maize$saq14=="1. RURAL",]
maize$s6aq01 <- as.character(maize$s6aq01)
maize$s6aq02a[maize$s6aq01 != "1. YES"] <- 0

colnames(maize)[colnames(maize) == "s6aq02a"] <- "quantity"
colnames(maize)[colnames(maize) == "s6aq02b"] <- "unit"

maize$unitcode <- as.numeric(gsub("([0-9]+).*$", "\\1", maize$unit))

colnames(conversion)

# # conversion for 9. Melekiya is not given specifically for maize, so calculate for other
# conversion_9 <- conversion[conversion$unit_cd == 9,]
# 
#conversion <- conversion[conversion$item_cd == 4,]
conversion <- conversion[!is.na(conversion$item_cd),]

conversion$mean <- conversion$mean_cf_nat
conversion4 <- conversion[conversion$item_cd_cf == 4,]

maize$consum_kg <- maize$quantity

maize$household_id <- as.character(maize$household_id)
i = 0

while(i < nrow(maize)){
  i = i + 1
  unitcode <- maize$unitcode[i]
  if(is.na(unitcode)){
    print(paste("interation:", i))
    print(paste("unitcode is NA"))
    print("remove households with NA")
    hhr <- maize$household_id[which(is.na(maize$unitcode))]
    print(paste("household to remove:", hhr))
    maize <- maize[-which(maize$household_id %in% hhr),]
    print("households removed from dataset")
    print("====================================================")
  }else if(!(unitcode %in% unique(conversion$unit_cd[conversion$item_cd_cf == 4]))){
    print(paste("interation:", i))
    print(paste("unit", unitcode, "has no value for maize"))
    if(length(conversion$mean[which(conversion$unit_cd==unitcode)]) == 0){
      print(paste("unit", unitcode, "has no value for other food items either, household should be removed"))
      hhr <- maize$household_id[which(maize$unitcode == unitcode)]
      print(paste("household to remove:", hhr))
      maize <- maize[-which(maize$household_id %in% hhr),]
      print("households removed from dataset")
      print("====================================================")
    }else{
      print(paste("using median of values for", conversion$item_cd_cf[which(conversion$unit_cd==unitcode)], ":", conversion$mean[which(conversion$unit_cd==unitcode)]))
      print(paste("which is", median(conversion$mean[which(conversion$unit_cd==unitcode)])))
      cf <- median(conversion$mean[which(conversion$unit_cd==unitcode)])
      consum_kg <- maize$quantity[i]*cf
      print(paste("kg maize consumed:", round(consum_kg, digits = 2)))
      maize$consum_kg[i] = consum_kg
      print("====================================================")
    }

  }
  else{
    consum_kg <- maize$quantity[i]*conversion4$mean[conversion4$unit_cd == unitcode]
    if(maize$consum_kg[i]>0){maize$consum_kg[i] = consum_kg}else{maize$consum_kg[i] = 0}
  }
}

mybins <- c(0, 0.5, seq(1,4, by = 1), seq(5,15, by = 2.5), seq(20,50, by = 5), 100, 200, 300)

hist(maize$consum_kg, breaks = seq(0,300, by = 5), main = "Maize consumption", 
     col = "goldenrod", #ylim = c(0,3000), xlim = c(0,220),
     xlab = "maize consumption (kg per household per week)")

hist(maize$consum_kg, breaks = mybins, main = "Maize consumption", 
     col = "goldenrod", #ylim = c(0,3000), xlim = c(0,220),
     xlab = "maize consumption (kg per household per week)")

table(maize$consum_kg)
mean(maize$consum_kg)
median(maize$consum_kg)
# maize purchase

colnames(maize)[colnames(maize) == "s6aq03a"] <- "purch_quantity"
colnames(maize)[colnames(maize) == "s6aq03b"] <- "purch_unit"

maize$purch_kg <- maize$purch_quantity
maize$purch_kg[is.na(maize$purch_kg)] <- 0
maize$purch_kg <- maize$purch_quantity

maize$household_id <- as.character(maize$household_id)
i = 0

while(i < nrow(maize)){
  i = i + 1
  unitcode <- maize$unitcode[i]
  if(is.na(unitcode)){
    maize$purch_kg[i] <- 0
  }else if(!(unitcode %in% unique(conversion$unit_cd[conversion$item_cd_cf == 4]))){
    print(paste("interation:", i))
    print(paste("unit", unitcode, "has no value for maize"))
    if(length(conversion$mean[which(conversion$unit_cd==unitcode)]) == 0){
      print(paste("unit", unitcode, "has no value for other food items either, household should be removed"))
      hhr <- maize$household_id[which(maize$unitcode == unitcode)]
      print(paste("household to remove:", hhr))
      maize <- maize[-which(maize$household_id %in% hhr),]
      print("households removed from dataset")
      print("====================================================")
    }else{
      print(paste("using median of values for", conversion$item_cd_cf[which(conversion$unit_cd==unitcode)], ":", conversion$mean[which(conversion$unit_cd==unitcode)]))
      print(paste("which is", median(conversion$mean[which(conversion$unit_cd==unitcode)])))
      cf <- median(conversion$mean[which(conversion$unit_cd==unitcode)])
      purch_kg <- maize$purch_quantity[i]*cf
      print(paste("kg maize purchased:", round(purch_kg, digits = 2)))
      maize$purch_kg[i] = purch_kg
      print("====================================================")
    }
    
  }
  else{
    purch_kg <- maize$purch_quantity[i]*conversion4$mean[conversion4$unit_cd == unitcode]
    if(maize$purch_kg[i]>0){maize$purch_kg[i] = purch_kg}else{maize$purch_kg[i] = 0}
  }
}

# maize production
colnames(maize)[colnames(maize) == "s6aq05a"] <- "prod_quantity"
colnames(maize)[colnames(maize) == "s6aq05b"] <- "prod_unit"

maize$prod_kg <- maize$prod_quantity
maize$prod_kg[is.na(maize$prod_kg)] <- 0
maize$prod_kg <- maize$prod_quantity
i = 0

while(i < nrow(maize)){
  i = i + 1
  unitcode <- maize$unitcode[i]
  if(is.na(unitcode)){
    maize$prod_kg[i] <- 0
  }else if(!(unitcode %in% unique(conversion$unit_cd[conversion$item_cd_cf == 4]))){
    print(paste("interation:", i))
    print(paste("unit", unitcode, "has no value for maize"))
    if(length(conversion$mean[which(conversion$unit_cd==unitcode)]) == 0){
      print(paste("unit", unitcode, "has no value for other food items either, household should be removed"))
      hhr <- maize$household_id[which(maize$unitcode == unitcode)]
      print(paste("household to remove:", hhr))
      maize <- maize[-which(maize$household_id %in% hhr),]
      print("households removed from dataset")
      print("====================================================")
    }else{
      print(paste("using median of values for", conversion$item_cd_cf[which(conversion$unit_cd==unitcode)], ":", conversion$mean[which(conversion$unit_cd==unitcode)]))
      print(paste("which is", median(conversion$mean[which(conversion$unit_cd==unitcode)])))
      cf <- median(conversion$mean[which(conversion$unit_cd==unitcode)])
      prod_kg <- maize$prod_quantity[i]*cf
      print(paste("kg maize produced:", round(prod_kg, digits = 2)))
      maize$prod_kg[i] = prod_kg
      print("====================================================")
    }
    
  }
  else{
    prod_kg <- maize$prod_quantity[i]*conversion4$mean[conversion4$unit_cd == unitcode]
    if(maize$prod_kg[i]>0){maize$prod_kg[i] = prod_kg}else{maize$prod_kg[i] = 0}
  }
}

pdf(file = "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Data/Paper1/Socioeconomic Survey 2018-2019/leisure/foodselfsuff.pdf")

gamfit <- fitdist(maize$consum_kg, distr = "gamma")
gamfit$estimate

hist(maize$consum_kg, breaks = mybins, main = "Maize consumption", 
     col = "goldenrod", xlim = c(0,220),
     xlab = "maize consumption (kg per household per week)")

abline(v = mean(maize$consum_kg), col = "red", lty = 4)
text(y = 0.1, x = 26, "mean: 9.9 kg", col = "red")

abline(v = median(maize$consum_kg), col = "darkgreen", lty = 5)
text(y = 0.12, x = 24, "median: 5.5 kg", col = "darkgreen")

curve(dgamma(x, shape = gamfit$estimate[1], rate = gamfit$estimate[2]), add = TRUE, col = "blue", lwd = 4, lty = 2)
text(x = 50, y = 0.04, "gamma distribution,\nshape = 1.11, rate = 0.11", col = "blue")

hist(maize$consum_kg, breaks = mybins, main = "Maize consumption (zoom)", 
     col = "goldenrod", xlim = c(0,50),
     xlab = "maize consumption (kg per household per week)")

abline(v = mean(maize$consum_kg), col = "red", lty = 4)
text(y = 0.1, x = 14, "mean: 9.9 kg", col = "red")

abline(v = median(maize$consum_kg), col = "darkgreen", lty = 5)
text(y = 0.12, x = 10, "median: 5.5 kg", col = "darkgreen")

curve(dgamma(x, shape = gamfit$estimate[1], rate = gamfit$estimate[2]), add = TRUE, col = "blue", lwd = 4, lty = 2)
text(x = 20, y = 0.04, "gamma distribution,\nshape = 1.11, rate = 0.11", col = "blue")

hist(maize$purch_kg, breaks = mybins, main = "Maize purchases", 
     col = "goldenrod", #ylim = c(0,3000), 
     xlim = c(0,220),
     xlab = "maize purchased for consumption (kg per household per week)")

hist(maize$purch_kg, breaks = mybins, main = "Maize purchases (zoom)", 
     col = "goldenrod", #ylim = c(0,3000), 
     xlim = c(0,50),
     xlab = "maize purchased for consumption (kg per household per week)")

maize$selfprod_kg <- maize$consum_kg - maize$purch_kg
hist(maize$selfprod_kg, breaks = mybins, main = "Maize self-production (consume - purchase)", 
     col = "goldenrod", #ylim = c(0,3000), 
     xlim = c(0,220),
     xlab = "maize produced for consumption (kg per household per week)")

hist(maize$prod_kg, breaks = mybins, main = "Maize self-production", 
     col = "goldenrod", #ylim = c(0,3000), 
     xlim = c(0,220),
     xlab = "maize produced for consumption (kg per household per week)")

hist(maize$prod_kg, breaks = mybins, main = "Maize self-production (zoom)", 
     col = "goldenrod", #ylim = c(0,3000), 
     xlim = c(0,50),
     xlab = "maize produced for consumption (kg per household per week)")

maize$self_suff_ratio <- maize$selfprod_kg*100/maize$consum_kg

hist(maize$self_suff_ratio, breaks = 50, main = "Maize self-sufficiency", 
     col = "goldenrod", xlim = c(0,100),
     xlab = "% of maize consumed from own production")

dev.off()

maize <- data.frame(household_id = maize$household_id,
                    selfproduction = maize$prod_kg,
                    selfproductionratio = maize$self_suff_ratio,
                    consumption = maize$consum_kg)

maizepath <- "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Stylized model - clean/rscripts/abm_stylised/summary_data/maize.csv"

write.csv(maize, file = maizepath, row.names=FALSE)

