# Name: Riechie
# NIM: 01112210038
library(tidyverse)
library(magrittr)
library(dplyr)
library(ggplot2)

# ====================================================================================================
# I. DATA UNDERSTANDING
# ====================================================================================================
data = read.csv("C:\\Users\\Riechie Rendy\\OneDrive\\Dokumen\\Semester 6\\Data Mining dan Analisis Prediktif\\01112210038-Riechie-UTS Data Mining dan Predictive Analytics\\heart.csv")
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
# There seems to be no duplicated rows in the dataset

# II. b) Dropping NULL Values
describe(data)
data[!complete.cases(data),] %>% nrow()

# II. c) Outlier Removal
par(mfrow = c(2,3))
for (i in 1:(dim(data)[2] - 1))
{
  if ((class(data[,i]) == "integer"|class(data[,i]) =="numeric") & n_distinct(data[,i]) > 2)
  {
    boxplot(data[,i], main = names(data)[i])
  }
}
par(mfrow = c(2,3))
for (i in 1:(dim(data)[2] - 1))
{
  if ((class(data[,i]) == "integer"|class(data[,i]) =="numeric") & n_distinct(data[,i]) > 2)
  {
    hist(data[,i], main = names(data)[i])
  }
}
par(mfrow = c(1,1))

data_no_outlier = data
for (i in 1:(dim(data)[2] - 1))
{
  if ((class(data[,i]) == "integer"|class(data[,i]) =="numeric") & n_distinct(data[,i]) > 2)
  {
    outlier <- boxplot(data_no_outlier[,i], main = names(data_no_outlier)[i])
    str(outlier)
    data_no_outlier <- filter(data_no_outlier, !(data_no_outlier[,i] %in% outlier$out))
  }
}

# We'll check if the outlier have been successfully removed or not
par(mfrow = c(2,3))
for (i in 1:(dim(data_no_outlier)[2] - 1))
{
  if ((class(data[,i]) == "integer"|class(data[,i]) =="numeric") & n_distinct(data_no_outlier[,i]) > 2)
  {
    boxplot(data_no_outlier[,i], main = names(data_no_outlier)[i])
  }
}
par(mfrow = c(2,3))
for (i in 1:(dim(data_no_outlier)[2] - 1))
{
  if ((class(data[,i]) == "integer"|class(data[,i]) =="numeric") & n_distinct(data_no_outlier[,i]) > 2)
  {
    hist(data_no_outlier[,i], main = names(data_no_outlier)[i])
  }
}
par(mfrow = c(1,1))
# From the boxplots, we can see that the outlier have been successfully removed

nrow(data_no_outlier)
nrow(data)

# ==================================================================================================
# III. EXPLORATORY DATA ANALYSIS
# ==================================================================================================
library(gridExtra)

fig1 <- data_no_outlier %>% ggplot(aes(x = Age, y = Cholesterol, color = factor(HeartDisease))) + geom_point() + labs(color = "HeartDisease")
fig2 <- data_no_outlier %>% ggplot(aes(x = Age, y = RestingBP, color = factor(HeartDisease))) + geom_point() + labs(color = "HeartDisease")
fig3 <- data_no_outlier %>% ggplot(aes(x = Age, y = MaxHR, color = factor(HeartDisease))) + geom_point() + labs(color = "HeartDisease")
fig4 <- data_no_outlier %>% ggplot(aes(x = Age, y = Oldpeak, color = factor(HeartDisease))) + geom_point() + labs(color = "HeartDisease")
grid.arrange(fig1, fig2, fig3, fig4, ncol = 2)

fig5 <- data_no_outlier %>% ggplot(aes(x = factor(HeartDisease), fill = Sex)) + geom_bar(position = position_dodge()) + labs(x = "HeartDisease")
fig6 <- data_no_outlier %>% ggplot(aes(x = factor(HeartDisease), fill = ChestPainType)) + geom_bar(position = position_dodge()) + labs(x = "HeartDisease")
fig7 <- data_no_outlier %>% ggplot(aes(x = factor(HeartDisease), fill = factor(FastingBS))) + geom_bar(position = position_dodge()) + labs(x = "HeartDisease", fill = "FastingBS")
fig8 <- data_no_outlier %>% ggplot(aes(x = factor(HeartDisease), fill = RestingECG)) + geom_bar(position = position_dodge()) + labs(x = "HeartDisease")
fig9 <- data_no_outlier %>% ggplot(aes(x = factor(HeartDisease), fill = ExerciseAngina)) + geom_bar(position = position_dodge()) + labs(x = "HeartDisease")
fig10 <- data_no_outlier %>% ggplot(aes(x = factor(HeartDisease), fill = ST_Slope)) + geom_bar(position = position_dodge()) + labs(x = "HeartDisease")
grid.arrange(fig5, fig6, fig7, fig8, fig9, fig10, ncol = 2)

corr <- round(cor(data_clean), 1)
# Compute a matrix of correlation p-values
p.mat <- cor_pmat(data_clean)
# Visualize the lower triangle of the correlation matrix
# Barring the no significant coefficient
corr.plot <- ggcorrplot(
  corr, hc.order = TRUE, type = "lower", outline.col = "white",
  p.mat = p.mat, title = "Correlation Plot data.car.csv"
)
corr.plot
# ==================================================================================================
# IV. LOGISTIC REGRESSION MODEL
# ==================================================================================================

# IV. a) Encode the Categorical features
library(fastDummies)
# One-Hot Encoding
data_encode <- dummy_cols(data_no_outlier, select_columns = c("ChestPainType","RestingECG","ST_Slope"))

# Binary Encoding
data_encode["Sex"] <- as.numeric(factor(data_encode$Sex, levels = unique(data_encode$Sex), exclude = NULL)) - 1
data_encode["ExerciseAngina"] <- as.numeric(factor(data_encode$ExerciseAngina, levels = unique(data_encode$ExerciseAngina), exclude = NULL)) - 1

data_clean <- data_encode %>% select("Age","Sex","ChestPainType_ATA","ChestPainType_NAP","ChestPainType_ASY",
                                     "RestingBP","Cholesterol","FastingBS","RestingECG_LVH","RestingECG_Normal",
                                     "MaxHR", "ExerciseAngina","Oldpeak","ST_Slope_Flat","ST_Slope_Up","HeartDisease")

# IV. b) Min-Max Scaling on the features
library(caret)
process <- preProcess(data_clean, method=c("range"))
data_norm <- predict(process, data_clean)

# IV. c) Train-Test Split
n=nrow(data_norm)
n
n1=floor(n*(0.8))
n1
n2=n-n1
n2
train= sort(sample(1:n,n1))

X <- model.matrix(HeartDisease~.,data=data_norm)[,-1]
X[1:3,]
xtrain <- X[train,]
class(xtrain)
xtest <- X[-train,]
ytrain <- data_norm$HeartDisease[train]
ytest <- data_norm$HeartDisease[-train]

# IV. d) Model Creation and Evaluation
set.seed(1)
model=glm(HeartDisease~.,family=binomial,data=data.frame(HeartDisease=ytrain,xtrain))
summary(model)

# Implement Backward Stepwise Regression
backward_model <- step(model, direction = "backward")
summary(backward_model)

ptest <- predict(backward_model,newdata=data.frame(xtest),type="response")
data.frame(ytest,ptest)[1:10,]

# Create Confusion Matrix
predictions=floor(ptest+0.5)	## floor function; see help command
ttt=table(ytest,predictions)
ttt

predictions_class <- factor(predictions, levels = c(0, 1))
ytest <- factor(ytest, levels = c(0, 1))
confusionMatrix(predictions_class, ytest)

coefficients <- coef(backward_model)[-1]  # Exclude intercept
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
