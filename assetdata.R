# Assets

# Data source: socioeconomic household survey 2018-2019

# Sections 
# 11a, assets, 1

assetdata <- "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Data/Paper1/Socioeconomic Survey 2018-2019/sect11_hh_w4.csv"
rosterdata <- "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Data/Paper1/Socioeconomic Survey 2018-2019/sect1_hh_w4.csv"

# load data
data <- read.csv(assetdata)
roster <- read.csv(rosterdata)

data <- data[data$saq14 == "1. RURAL",]

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
           "35. Solar device")  
  
data <- data[data$asset_cd %in% c(communication_cd, transport_cd, store_cd, tools_cd),]

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

library(tidyr)
hhdatalong <- hhdata %>% pivot_longer(cols = nr_telecom:nr_tool)

hhdatalong <- hhdatalong %>% arrange(nr_items)

ggplot(hhdatalong, aes(fill=name, y=value, x=reorder(household_id, -value))) + 
  geom_bar(position="stack", stat="identity") + xlab("household") + 
  ylab("total number of assets") + scale_fill_manual(values=c("orange", "yellow", "green", "deeppink")) + theme_classic()

ggplot(hhdatalong, aes(fill=name, y=value, x=reorder(household_id, -value))) + 
  geom_bar(position="stack", stat="identity") + 
  facet_wrap(~ name, ncol = 4, scales = "free") + xlab("household") + ylab("number of assets per type") + 
  scale_fill_manual(values=c("orange", "yellow", "green", "deeppink")) + theme_classic()
  

