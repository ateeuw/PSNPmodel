# Combined farmer characteristics

# Data source: socioeconomic household survey 2018-2019

# Libraries
library(dplyr)
library(ggplot2)
library(gridExtra)
library(FactoMineR)
library(missMDA)

# Characteristics::::::::::::::::::::::::: 

# HH size (section 1, column individual_id, merge by household_id, name: nmem)
hhsizepath <- "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Stylized model - clean/rscripts/abm_stylised/summary_data/hhsize.csv"

hhsize <- read.csv(hhsizepath)

data <- hhsize
data$household_id <- as.character(data$household_id)

rm(list = c("hhsize", "hhsizepath"))

# Market access (section 5a, column s5aq26)
accesspath <- "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Stylized model - clean/rscripts/abm_stylised/summary_data/marketaccess.csv"

access <- read.csv(accesspath)
access$household_id <- as.character(access$household_id)

data <- full_join(data, access)

# Labour supply capacity
labourpath <- "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Stylized model - clean/rscripts/abm_stylised/summary_data/labourcap.csv"
labour <- read.csv(labourpath)
labour$household_id <- as.character(labour$household_id)
data <- full_join(data, labour)

# Assets
assetpath <- "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Stylized model - clean/rscripts/abm_stylised/summary_data/assets.csv"
asset <- read.csv(assetpath )
asset$household_id <- as.character(asset$household_id)
data <- full_join(data, asset)

# Savings
savingpath <- "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Stylized model - clean/rscripts/abm_stylised/summary_data/savings.csv"
saving <- read.csv(savingpath )
saving$household_id <- as.character(saving$household_id)
data <- full_join(data, saving)

# Credit
creditpath <- "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Stylized model - clean/rscripts/abm_stylised/summary_data/credit.csv"
credit <- read.csv(creditpath )
credit$household_id <- as.character(credit$household_id)
data <- full_join(data, credit)


# Aspiration outcomes:::::::::::::::::::::

# Income/spending
spendingpath <- "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Stylized model - clean/rscripts/abm_stylised/summary_data/spending.csv"
spending <- read.csv(spendingpath)
spending$household_id <- as.character(spending$household_id)
data <- merge(data, spending)

# Food self-sufficiency/ self-production
maizepath <- "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Stylized model - clean/rscripts/abm_stylised/summary_data/maize.csv"
maize <- read.csv(maizepath)
maize$household_id <- as.character(maize$household_id)
data <- full_join(data, maize)

# Leisure
labourpath <- "C:/Users/TeeuwenAS/OneDrive - Universiteit Twente/Twente/Thesis shared - ABM value chains food security/Stylized model - clean/rscripts/abm_stylised/summary_data/hhlabour.csv"
labour <- read.csv(labourpath)
labour$household_id <- as.character(labour$household_id)
data <- full_join(data, labour)

p1 <- ggplot(data, aes(y = spending, x = selfproduction)) +
  geom_point(size = data$nmem, alpha = 0.3) + geom_smooth(method = "loess") +
  ylab("INCOME (1000* birr spent yearly by household)") +
  xlab("FOOD SELF-SUFFICIENCY (kg maize produced weekly by and for household)") +
  ggtitle("income vs food self-sufficiency") +
  theme_classic()

p1zoom <- ggplot(data, aes(y = spending, x = selfproduction)) +
  geom_point(size = data$nmem, alpha = 0.3) + xlim(0,30) + ylim(0, 200000) +
  geom_smooth(method = "loess") +
  ylab("INCOME (1000* birr spent yearly by household)") +
  xlab("FOOD SELF-SUFFICIENCY (kg maize produced weekly by and for household)") +
  theme_classic()

p2 <- ggplot(data, aes(y = spending, x = hh_laboursum)) +
  geom_point(size = data$nmem, alpha = 0.3) + geom_smooth(method = "loess") +
  ylab("INCOME (1000* birr spent yearly by household)") +
  xlab("LABOUR (hours per week per household)") +
  ggtitle("income vs labour") +
  theme_classic()

p2zoom <- ggplot(data, aes(y = spending, x = hh_laboursum)) +
  geom_point(size = data$nmem, alpha = 0.3) + xlim(0,300) + ylim(0, 200000) +
  geom_smooth(method = "loess") +
  ylab("INCOME (1000* birr spent yearly by household)") +
  xlab("LABOUR (hours per week per household)") +
  theme_classic()

p3 <- ggplot(data, aes(y = hh_laboursum, x = selfproduction)) +
  geom_point(size = data$nmem, alpha = 0.3) + geom_smooth(method = "loess") +
  ylab("LABOUR (hours per week per household)") +
  xlab("FOOD SELF-SUFFICIENCY (kg maize produced weekly by and for household)") +
  ggtitle("labour vs food self-sufficiency") +
  theme_classic()

p3zoom <- ggplot(data, aes(y = hh_laboursum, x = selfproduction)) +
  geom_point(size = data$nmem, alpha = 0.3) + xlim(0,30) + ylim(0, 300) +
  geom_smooth(method = "loess") +
  ylab("LABOUR (hours per week per household)") +
  xlab("FOOD SELF-SUFFICIENCY (kg maize produced weekly by and for household)") +
  theme_classic()

grid.arrange(p1, p2, p3, p1zoom, p2zoom, p3zoom, ncol=3)

res.pca = imputePCA(data[,c(3,4,7)], scale.unit=TRUE, graph=T)

data2 <- data[,c(2,3,4,7)]
res.pca2 = imputePCA(data2, scale.unit=TRUE, graph=T, maxiter = 1000000)
res.pca <- PCA(res.pca2$completeObs, quanti.sup = 1)

head(dimdesc(res.pca, axes=c(1,2)))

hcpcout <- HCPC(res.pca, nb.clust=-1)
hcpcout

out <- hcpcout$data.clust
b1 <- ggplot(out, aes(y = spending, x = clust, fill=clust)) +
  geom_boxplot(outlier.colour="grey", outlier.shape=8,
               outlier.size=1) + ylab("spending") + ggtitle("INCOME") +
  theme_classic()

b2 <- ggplot(out, aes(y = selfproduction, x = clust, fill=clust)) +
  geom_boxplot(outlier.colour="grey", outlier.shape=8,
               outlier.size=1) + ylab("maize produced for consumption") + ggtitle("FOOD") +
  theme_classic()

b3 <- ggplot(out, aes(y = hh_laboursum, x = clust, fill=clust)) +
  geom_boxplot(outlier.colour="grey", outlier.shape=8,
               outlier.size=1) + ylab("time spent working") + ggtitle("LABOUR") +
  theme_classic()

grid.arrange(b1, b2, b3, ncol=3)

quicksum <- out %>% group_by(clust) %>% summarise(income = mean(spending),
                                                  food = mean(selfproduction),
                                                  labour = mean(hh_laboursum),
                                                  nhh = n())

ggplot(out, aes(spending, group = clust, fill = clust)) + 
  geom_histogram() +
  facet_grid(clust ~ .) +
  theme_classic()

h1 <- ggplot(out, aes(spending, group = clust, fill = clust)) + 
  geom_histogram(aes(y = stat(density) * 10000), binwidth = 9000) + 
  ylab("density") + ylim(0,1) +
  facet_grid(clust ~ .) + ggtitle("INCOME") +
  theme_classic()

h2 <- ggplot(out, aes(selfproduction, group = clust, fill = clust)) + 
  geom_histogram(aes(y = stat(density) * 1), binwidth = 2) + 
  ylab("density") + ylim(0,1) +
  facet_grid(clust ~ .) + ggtitle("FOOD") +
  theme_classic()

h3 <- ggplot(out, aes(hh_laboursum, group = clust, fill = clust)) + 
  geom_histogram(aes(y = stat(density) * 10), binwidth = 10) + 
  ylab("density") + ylim(0,1) +
  facet_grid(clust ~ .) + ggtitle("LABOUR") +
  theme_classic()

grid.arrange(h1, h2, h3, ncol=3)

data <- data[data$spending < 300000,]
data <- data[data$consumption < 100,]
data <- data[data$hh_laboursum < 500,]

# Do analysis again without outliers
  
plot(hcpcout)

