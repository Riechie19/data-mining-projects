# Name: Riechie
# NIM: 01112210038
library(tidyverse)
library(magrittr)
library(dplyr)
library(ggplot2)
library(readxl)

# ====================================================================================================
# I. DATA UNDERSTANDING
# ====================================================================================================
data = read_excel("C:\\Users\\Riechie Rendy\\OneDrive\\Dokumen\\Semester 6\\Data Mining dan Analisis Prediktif\\01112210038-Riechie-UAS Data Mining dan Prediktif Analitik\\Heart_Attack (Training).xlsx")
data = as.data.frame(data)
dim(data)
names(data)
summary(data)
library(Hmisc)
describe(data)

# ====================================================================================================
# II. DATA CLEANSING
# ====================================================================================================

# II. a) Checking for duplicated rows
duplicated_data = data[duplicated(data),]
nrow(duplicated_data)
# There are 11 duplicated rows in the dataset
data_distinct <- data[!duplicated(data), ]

duplicated_data = data_distinct[duplicated(data_distinct),]
nrow(duplicated_data)
dim(data_distinct)
# Duplicated rows have been succesfully removed

# II. b) Dropping NULL Values
describe(data_distinct)
data_distinct[!complete.cases(data_distinct),] %>% nrow()
# There are no NULL values in the dataset

# II. c) Outlier Removal
par(mfrow = c(2,2))
for (i in c(1,5,7))
{
  boxplot(data_distinct[,i], main = names(data_distinct)[i])
}
par(mfrow = c(2,2))
for (i in c(1,5,7))
{
  hist(data_distinct[,i], main = names(data_distinct)[i])
}
par(mfrow = c(1,1))
# It looks like the only outlier can be seen in the Cholestrol feature.
# However, it could be observed that the outliers are a natural part of the sample.
# So i have opted to not remove any outliers from the dataset.


# ==================================================================================================
# III. EXPLORATORY DATA ANALYSIS
# ==================================================================================================
library(gridExtra)
library(corrplot)
library(ggcorrplot)

fig1 <- data_distinct %>% ggplot(aes(x = Age, y = Cholesterol, color = `2nd_Heart_Attack` )) + geom_point() + labs(color = "2nd_Heart_Attack")
fig2 <- data_distinct %>% ggplot(aes(x = Age, y = Trait_Anxiety, color = `2nd_Heart_Attack` )) + geom_point() + labs(color = "2nd_Heart_Attack")
fig3 <- data_distinct %>% ggplot(aes(x = Trait_Anxiety, y = Cholesterol, color = `2nd_Heart_Attack` )) + geom_point() + labs(color = "2nd_Heart_Attack")
grid.arrange(fig1, fig2, fig3, ncol = 2)

fig4 <- data_distinct %>% ggplot(aes(x = Marital_Status, fill = `2nd_Heart_Attack`)) + geom_bar(position = position_dodge()) + labs(x = "Marital_Status")
fig5 <- data_distinct %>% ggplot(aes(x = factor(Gender) , fill = `2nd_Heart_Attack`)) + geom_bar(position = position_dodge()) + labs(x = "Gender")
fig6 <- data_distinct %>% ggplot(aes(x = Weight_Category , fill = `2nd_Heart_Attack`)) + geom_bar(position = position_dodge()) + labs(x = "Weight_Category")
fig7 <- data_distinct %>% ggplot(aes(x = factor(Stress_Management) , fill = `2nd_Heart_Attack`)) + geom_bar(position = position_dodge()) + labs(x = "Stress_Management")
grid.arrange(fig4, fig5, fig6, fig7, ncol = 2)

data_distinct["Second_Heart_Attack"] <- as.numeric(factor(data_distinct$`2nd_Heart_Attack`, exclude = NULL)) - 1
data_distinct <- subset(data_distinct, select = -`2nd_Heart_Attack`)

corr <- round(cor(data_distinct), 1)
# Compute a matrix of correlation p-values
p.mat <- cor_pmat(data_distinct)
# Visualize the lower triangle of the correlation matrix
# Barring the no significant coefficient
corr.plot <- ggcorrplot(
  corr, hc.order = TRUE, type = "lower", outline.col = "white",
  p.mat = p.mat, title = "Correlation Plot"
)
corr.plot

# ==================================================================================================
# IV. LOGISTIC REGRESSION MODEL
# ==================================================================================================
# IV. a) Min-Max Scaling on the features
library(caret)
process <- preProcess(data_distinct, method=c("range"))
data_norm <- predict(process, data_distinct)

# IV. b) Train-Test Split
n=nrow(data_norm)
n
n1=floor(n*(0.9))
n1
n2=n-n1
n2
train= sort(sample(1:n,n1))

X <- model.matrix(Second_Heart_Attack~.,data=data_norm)[,-1]
X[1:3,]
xtrain <- X[train,]
class(xtrain)
xtest <- X[-train,]
ytrain <- data_norm$Second_Heart_Attack[train]
ytest <- data_norm$Second_Heart_Attack[-train]

# IV. c) Model Creation and Evaluation
set.seed(1)
model=glm(Second_Heart_Attack~.,family=binomial,data=data.frame(Second_Heart_Attack=ytrain,xtrain))
summary(model)

# Predict the Testing Data
ptest <- predict(model,newdata=data.frame(xtest),type="response")
data.frame(ytest,ptest)[1:10,]

# Create Confusion Matrix
predictions=floor(ptest+0.5)	## floor function; see help command
ttt=table(ytest,predictions)
ttt

predictions_class <- factor(predictions, levels = c(0, 1))
ytest <- factor(ytest, levels = c(0, 1))
confusionMatrix(predictions_class, ytest)

coefficients <- coef(model)[-1]  # Exclude intercept
names <- names(coefficients)

plot_data <- data.frame(
  Features = names,
  Coefficients = abs(coefficients)
)

# Plot feature importance using ggplot
ggplot(plot_data, aes(x = Coefficients, y = reorder(Features, Coefficients))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Feature Importance of Logistic Regression Model",
       x = "Absolute Coefficients",
       y = "Features") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))


# ==================================================================================================
# V. PREDICTING SCORING DATA
# ==================================================================================================
data_scoring = read_excel("C:\\Users\\Riechie Rendy\\OneDrive\\Dokumen\\Semester 6\\Data Mining dan Analisis Prediktif\\01112210038-Riechie-UAS Data Mining dan Prediktif Analitik\\Heart_Attack (Scoring).xlsx")
data_scoring = as.data.frame(data_scoring)
dim(data_scoring)
names(data_scoring)

# Apply Scaling
process_test <- preProcess(data_scoring, method=c("range"))
data_scoring_norm <- predict(process_test, data_scoring)

# Predict
score_ptest <- predict(model,newdata=data_scoring_norm,type="response")
score_predictions=floor(score_ptest+0.5)
score_predictions <- as.data.frame(score_predictions)
score_predictions

write.csv(score_predictions, "C:\\Users\\Riechie Rendy\\OneDrive\\Dokumen\\Semester 6\\Data Mining dan Analisis Prediktif\\01112210038-Riechie-UAS Data Mining dan Prediktif Analitik\\Score_Data_Predictions.csv", row.names=FALSE)
