# Assets

# Data source: socioeconomic household survey 2018-2019

# Sections 
# 11a, assets, 1

# load libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# load data
assetdata <- "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Data/Paper1/Socioeconomic Survey 2018-2019/sect11_hh_w4.csv"
data <- read.csv(assetdata)

data <- data[data$saq14 == "1. RURAL",]

data$asset_cd <- as.character(data$asset_cd)
unique(data$asset_cd)

communication_cd <- c("7. Fixed line telephone",
                      "8. Radio/ tape recorder",
                      "9. Television",
                      "11. Satelite Dish")

transport_cd <- c("13. Bicycle",
                  "14. Motor cycle",
                  "15. Cart (Hand pushed)",
                  "16. Cart (animal drawn)- for transporting people and goods",
                  "22. Private car")

store_cd <- c("21. Refrigerator",
              "26. Shelf for storing goods")

tools_cd <- c("29. Sickle (Machid)",
              "30. Axe (Gejera)",
              "31. Pick Axe (Geso)",
              "32. Plough (Traditional)",
              "33. Plough (Modern)")

other <- c("34. Water Pump",
           "35. Solar device ")  
  
data <- data[data$asset_cd %in% c(communication_cd, transport_cd, store_cd, tools_cd, other),]
unique(data$asset_cd)

data$s11q01[data$s11q00 == "2. NO"] <- 0

hhdata <- data %>% group_by(household_id) %>% summarise(nr_items = sum(s11q01),
                                                        nr_telecom = sum(s11q01[asset_cd %in% communication_cd]),
                                                        nr_transport = sum(s11q01[asset_cd %in% transport_cd]),
                                                        nr_store = sum(s11q01[asset_cd %in% store_cd]),
                                                        nr_tool = sum(s11q01[asset_cd %in% tools_cd]))

mybins <- 0:35

all <- hist(hhdata$nr_items, main = "Assets", col = "black", breaks = mybins,
     xlab = "number of (interesting) assets per household")

telecom <- hist(hhdata$nr_telecom, main = "Telecommunication", col = "yellow", breaks = mybins,
                xlab = "number of telecommunication devices per household")

transport <- hist(hhdata$nr_transport, main = "Means of transport", col = "deeppink", breaks = mybins,
                xlab = "number of means of transport per household")

store <- hist(hhdata$nr_store, main = "Food storage", col = "orange", breaks = mybins,
                  xlab = "number of storage facilities per household")

tools <- hist(hhdata$nr_tool, main = "Agricultural tools", col = "green", breaks = mybins,
              xlab = "number of agricultural tools per household")

hhdatalong <- hhdata %>% pivot_longer(cols = nr_telecom:nr_tool)

hhdatalong <- hhdatalong %>% arrange(nr_items)

ggplot(hhdatalong, aes(fill=name, y=value, x=reorder(household_id, -value))) + 
  geom_bar(position="stack", stat="identity") + xlab("household") + 
  ylab("total number of assets") + scale_fill_manual(values=c("orange", "yellow", "green", "deeppink")) + theme_classic()

ggplot(hhdatalong, aes(fill=name, y=value, x=reorder(household_id, -value))) + 
  geom_bar(position="stack", stat="identity") + 
  facet_wrap(~ name, ncol = 4, scales = "free") + xlab("household") + ylab("number of assets per type") + 
  scale_fill_manual(values=c("orange", "yellow", "green", "deeppink")) + theme_classic()
  
data$s11q01[data$s11q00 == "2. NO"] <- 0
datawide <- data[,c(1,2,15)] %>% pivot_wider(names_from = asset_cd, values_from = s11q01)

pdf(file = "./results/asset_correlation.pdf")

for(i in 2:(ncol(datawide)-1)){
  for(j in (i + 1):ncol(datawide)){
    
    xname <- colnames(datawide)[i]
    yname <- colnames(datawide)[j]
    
    colnames(datawide)[i] <- "x"
    colnames(datawide)[j] <- "y"
    
    print(xname)
    table(datawide$x)
    print(yname)
    table(datawide$y)
    
    res <- cor.test(datawide$x, datawide$y, method = "pearson")
    
    scattercolour <- ifelse(res$p.value < 0.05, "darkred", "black")
    
    g <- ggplot(datawide, aes(x = x, y = y)) + 
      geom_jitter(col = scattercolour) + geom_smooth(method = "lm") + ggtitle(paste("correlation coefficient =", round(res$estimate, digits = 3), "\np-value = ", round(res$p.value, digits = 3))) +
      ylab(yname) + xlab(xname) + theme_classic2()
    
    if(res$p.value < 0.05){print(g)}
    
    colnames(datawide)[i] <- xname
    colnames(datawide)[j] <- yname  
    
  }
}

dev.off()

colnames(datawide) <- abbreviate(colnames(datawide), minlength = 10)

library("Hmisc")
res2 <- rcorr(as.matrix(datawide[,c(-1)]))
res2

library(corrplot)

# Insignificant correlations are left blank
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "blank")

corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "pch")

remove_these <- c("30.Ax(Gjr)", "29.S(Mchd)", "21.Rfrgrtr", "11.StltDsh", "8.Rd/tprcr", "35.Solrdvc")

datawide <- datawide[,-which(colnames(datawide) %in% remove_these)]

colnames(datawide) <- c("household_id", "asset_phone", "asset_TV", "asset_bike", "asset_motorcycle", "asset_handcart", "asset_animalcart", 
                        "asset_car", "asset_shelf", "asset_pickaxe", "asset_traditionalplough", "asset_modermplough", "asset_waterpump")

assetpath <- "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Stylized model - clean/rscripts/abm_stylised/summary_data/assets.csv"
write.csv(datawide, file = assetpath, row.names=FALSE)

