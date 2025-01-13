library(dplyr)
## STEP 2: Exploring and Preparing the data ---- 
data = read.csv("C:\\Users\\Riechie Rendy\\OneDrive\\Dokumen\\Semester 6\\Data Mining dan Analisis Prediktif\\Project Clustering\\marketing_campaign.csv", sep = '\t')
head(data)
dim(data)
sum(is.na(data))
summary(data[,2:8])
names(data)

# Check for Year_Birth
summary(data$Year_Birth)
hist(data$Year_Birth)
plot(density(data$Year_Birth))

# Removing the outliers
outlier <- boxplot(data$Year_Birth)
str(outlier)
data <- filter(data, !(data$Year_Birth %in% outlier$out))
hist(data$Year_Birth)
plot(density(data$Year_Birth))
summary(data$Year_Birth)
dim(data)

# Check for Education
unique(data$Education)
table(data$Education)
sum(is.na(data$Education))

# Check for Marital_Status
unique(data$Marital_Status)
table(data$Marital_Status)

aggregate(data = data, Year_Birth ~ Marital_Status, mean)

sum(is.na(data$Marital_Status))
data$Marital_Status <- ifelse(data$Marital_Status == 'Alone' | data$Marital_Status == 'Absurd' 
                              | data$Marital_Status == 'YOLO', 'Single',data$Marital_Status)

table(data$Marital_Status)
aggregate(data = data, Year_Birth ~ Marital_Status, mean)

# Check for Income
# Removing the outliers
summary(data$Income)
hist(data$Income)
outlier <- boxplot(data$Income)
data <- filter(data, !(data$Income %in% outlier$out))
hist(data$Income)
dim(data)

# Imputing Missing Values
aggregate(data = data, Income ~ Education, mean, na.rm = TRUE )
ave_income <- ave(data$Income, data$Education,
                  FUN = function(x) mean(x, na.rm = TRUE))
ave_income
data$Income <- ifelse(is.na(data$Income), ave_income, data$Income)
summary(data$Income)
sum(is.na(data))

# Check for Kidhome and Teenhome
summary(data$Kidhome)
summary(data$Teenhome)

# Check for Dt_customer
unique(data$Dt_Customer)

# Check for Recency
summary(data$Recency)
hist(data$Recency)
boxplot(data$Recency)

#Check for Complain
summary(data$Complain)
table(data$Complain)

## STEP 3: Training a model on the data ----
products <- c("MntWines","MntFruits","MntMeatProducts","MntFishProducts","MntSweetProducts","MntGoldProds")
promotion <- c("AcceptedCmp1","AcceptedCmp2","AcceptedCmp3","AcceptedCmp4","AcceptedCmp5","Response")
place <- c("NumWebPurchases","NumCatalogPurchases","NumStorePurchases")

# Clustering by Products Bought
set.seed(123)
data_products <- data[products]
data_products_z <- as.data.frame(lapply(data_products, scale))
summary(data_products)
summary(data_products_z)

products_wss <- numeric(15) 
for (k in 1:15) products_wss[k] <- sum(kmeans(data_products_z, centers=k)$withinss)
products_wss
k.values <- 1:15

plot(k.values, products_wss,
     type="o", pch = 19, frame = FALSE, 
     xlab="Number of clusters K for products",
     ylab="Total within-clusters sum of squares")

set.seed(123)
cluster_products <- kmeans(data_products_z, 7)
cluster_products
cluster_products$tot.withinss

# Clustering by Promotion
set.seed(123)
data_promotion <- data[promotion]
data_promotion_z <- as.data.frame(lapply(data_promotion, scale))
summary(data_promotion)
summary(data_promotion_z)

promotion_wss <- numeric(15) 
for (k in 1:15) promotion_wss[k] <- sum(kmeans(data_promotion, centers=k)$withinss)
promotion_wss
k.values <- 1:15

plot(k.values, promotion_wss,
     type="o", pch = 19, frame = FALSE, 
     xlab="Number of clusters K for promotion",
     ylab="Total within-clusters sum of squares")

set.seed(123)
cluster_promotion <- kmeans(data_promotion, 7)
cluster_promotion
cluster_promotion$tot.withinss

#Clustering by Place
set.seed(123)
data_place <- data[place]
data_place_z <- as.data.frame(lapply(data_place, scale))
summary(data_place)
summary(data_place_z)

place_wss <- numeric(15) 
for (k in 1:15) place_wss[k] <- sum(kmeans(data_place_z, centers=k)$withinss)
place_wss
k.values <- 1:15

plot(k.values, place_wss,
     type="o", pch = 19, frame = FALSE, 
     xlab="Number of clusters K for place",
     ylab="Total within-clusters sum of squares")

set.seed(123)
cluster_place <- kmeans(data_place_z, 4)
cluster_place
cluster_place$tot.withinss

## STEP 4: Evaluation Model Performance
#Sizes
cluster_products$size
cluster_promotion$size
cluster_place$size

# Determining characteristic of each cluster
cluster_products$centers
ifelse(apply(cluster_products$centers, 1, max) > 0, colnames(cluster_products$centers)[max.col(cluster_products$centers, ties.method='first')], "infrequent" )
clust_products_names <- c("MeatProducts","GoldProducts","Wines","SweetProducts","FishProducts","Fruits","Infrequent")

cluster_promotion$centers
rowSums(cluster_promotion$centers)
rank(-rowSums(cluster_promotion$centers))
clust_promotion_names <- c("4th","1st","3rd","6th","7th","5th","2nd")

cluster_place$centers
ifelse(apply(cluster_place$centers, 1, max) > 0, colnames(cluster_place$centers)[max.col(cluster_place$centers, ties.method='first')], "infrequent" )
clust_place_names <- c("Store", "Infrequent", "Web", "Catalog")

## STEP 5: Improving Model Performance ----
library(DescTools)
data$cluster_products <- cluster_products$cluster
data$cluster_promotion <- cluster_promotion$cluster
data$cluster_place <- cluster_place$cluster

aggregate(data = data, cbind(Year_Birth,Income,Kidhome, Teenhome, NumDealsPurchases, Complain) ~ cluster_products, mean)
aggregate(data = data, cbind(Year_Birth,Income,Kidhome, Teenhome, NumDealsPurchases, Complain) ~ cluster_place, mean)
aggregate(data = data, cbind(Year_Birth,Income,Kidhome, Teenhome, NumDealsPurchases, Complain) ~ cluster_promotion, mean)

aggregate(data = data, cluster_place ~ cluster_products, Mode)
