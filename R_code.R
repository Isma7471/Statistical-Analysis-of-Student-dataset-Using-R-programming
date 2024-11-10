# Load required libraries
library(ggplot2)

# Load the dataset
student_data <- read.csv("C:/Users/smrc/Desktop/StudentData.csv", na.strings = c("", "NA"))

# View the structure of the dataset
str(student_data)
summary(student_data)


# Check for duplicate rows
sum(duplicated(student_data))

# Remove duplicate rows
student_data <- unique(student_data)
sum(is.na(student_data))

# Check for missing values in each column
colSums(is.na(student_data))

# Impute missing numerical values with the mean
student_data$TOEFL_Score[is.na(student_data$TOEFL_Score)] <- mean(student_data$TOEFL_Score, na.rm = TRUE)
student_data$CGPA[is.na(student_data$CGPA)] <- mean(student_data$CGPA, na.rm = TRUE)

# Impute missing categorical values with the most frequent level
student_data$University_Rating[is.na(student_data$University_Rating)] <- names(which.max(table(student_data$University_Rating)))
student_data$Stetement_of_Purpose[is.na(student_data$Stetement_of_Purpose)] <- names(which.max(table(student_data$Stetement_of_Purpose)))
student_data$Letter_of_Recommendation[is.na(student_data$Letter_of_Recommendation)] <- names(which.max(table(student_data$Letter_of_Recommendation)))
student_data$Research[is.na(student_data$Research)] <- names(which.max(table(student_data$Research)))

# Correcting inconsistencies in 'Status_of_Admission'
student_data$Status_of_Admission <- ifelse(student_data$Status_of_Admission == "Rejectecd", "Rejected", student_data$Status_of_Admission)
sum(is.na(student_data))


# Convert ordinal categorical variables to factors with ordered levels

student_data$University_Rating <- factor(student_data$University_Rating, levels = c("Very Poor", "Poor", "Good", "Very Good", "Excellent"), ordered = TRUE)
student_data$Stetement_of_Purpose <- factor(student_data$Stetement_of_Purpose, levels = c("Poor", "Good", "Very Good", "Excellent"), ordered = TRUE)
student_data$Letter_of_Recommendation <- factor(student_data$Letter_of_Recommendation, levels = c("Poor", "Good", "Very Good", "Excellent"), ordered = TRUE)
student_data$Research <- factor(student_data$Research, levels = c("No", "Yes"), ordered = TRUE)
student_data$Status_of_Admission <- factor(student_data$Status_of_Admission, levels = c("Rejected", "Accepted"), ordered = TRUE)



# Generate boxplots for numerical variables
boxplot(student_data$TOEFL_Score, main = "Boxplot of TOEFL Score (Before Outlier Removal)")
boxplot(student_data$CGPA, main = "Boxplot of CGPA (Before Outlier Removal)")

# remove outliers using the IQR method
outlier_remover <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- IQR(x)
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  x[x < lower_bound | x > upper_bound]
}
outliers_CGPA <- outlier_remover(student_data$CGPA)

student_data <- subset(student_data, !(TOEFL_Score %in% outliers_TOEFL | CGPA %in% outliers_CGPA))

# Generate boxplots after outlier removal
boxplot(student_data$CGPA, main = "Boxplot of CGPA (After Outlier Removal)")

# Installing the modeest package 
install.packages("modeest")
# Loading the modeest package
library(modeest)
# Grouping by admission status
admission_groups <- split(student_data, student_data$Status_of_Admission)

# Calculate descriptive statistics for each group
descriptive_stats <- lapply(admission_groups, function(group) {
  summary_data <- summary(group[, c("TOEFL_Score", "CGPA")])
  mode_TOEFL <- mfv(group$TOEFL_Score)
  mode_CGPA <- mfv(group$CGPA)
  sd_data <- apply(group[, c("TOEFL_Score", "CGPA")], 2, sd)
  var_data <- apply(group[, c("TOEFL_Score", "CGPA")], 2, var)
  return(list(Summary = summary_data, Mode_TOEFL = mode_TOEFL, Mode_CGPA = mode_CGPA, 
              Standard_Deviation = sd_data, Variance = var_data))
})

# Print the descriptive statistics
print(descriptive_stats)


#Data Visualization
# Boxplot for TOEFL Score by Admission Status
ggplot(student_data, aes(x = Status_of_Admission, y = TOEFL_Score, fill = Status_of_Admission)) +
  geom_boxplot() +
  labs(title = "TOEFL Score Distribution by Admission Status", x = "Admission Status", y = "TOEFL Score") +
  theme_minimal()

# Histogram for CGPA by Admission Status
ggplot(student_data, aes(x = CGPA, fill = Status_of_Admission)) +
  geom_histogram(binwidth = 0.1, position = "dodge") +
  labs(title = "CGPA Distribution by Admission Status", x = "CGPA", y = "Count") +
  theme_minimal()

# Bar plot for University Rating by Admission Status
ggplot(student_data, aes(x = University_Rating, fill = Status_of_Admission)) +
  geom_bar(position = "dodge") +
  labs(title = "University Rating by Admission Status", x = "University Rating", y = "Count") +
  theme_minimal()

# Pie chart for Research by Admission Status
ggplot(student_data, aes(x = "", fill = Research)) +
  geom_bar(position = "fill") +
  coord_polar("y", start = 0) +
  facet_wrap(~ Status_of_Admission) +
  labs(title = "Research Experience by Admission Status", x = NULL, y = NULL, fill = "Research Experience") +
  theme_minimal() +
  theme(axis.text.x = element_blank())

# Stacked bar plot for Statement of Purpose by Admission Status
ggplot(student_data, aes(x = Stetement_of_Purpose, fill = Status_of_Admission)) +
  geom_bar(position = "stack") +
  labs(title = "Statement of Purpose Quality by Admission Status", x = "Statement of Purpose Quality", y = "Count") +
  theme_minimal()

# Stacked bar plot for Letter of Recommendation by Admission Status
ggplot(student_data, aes(x = Letter_of_Recommendation, fill = Status_of_Admission)) +
  geom_bar(position = "stack") +
  labs(title = "Letter of Recommendation Quality by Admission Status", x = "Letter of Recommendation Quality", y = "Count") +
  theme_minimal()

library(DataExplorer)
create_report(student_data)

library(e1071)
install.package(caTools)
library(caTools)
library(caret)
split<-sample.split(student_data, SplitRatio=0.7)
train_cl<-subset(student_data, split=="TRUE")
test_cl<-subset(student_data, split=="FALSE")
str(train_cl)
str(test_cl)

# feature scalling
train_scale<-scale(train_cl[, 1:4])
test_scale<-scale(test_cl[, 1:4])
train_scale

# fitting naive bayes model to training dataset
set.seed(120)
classifier_cl<-naiveBayes(Species~ ., data=train_cl)
classifier_cl


y_pred<-predict(classifier_cl, newdata = test_cl) #predicting on test data

cm<-table(test_cl$Species, y_pred) # confusion matrix
cm
confusionMatrix(cm)
