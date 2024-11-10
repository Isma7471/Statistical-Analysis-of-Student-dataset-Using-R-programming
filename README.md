
# Statistical Analysis of Student Dataset using R

This project performs statistical analysis and data visualization on a student dataset to gain insights into factors that influence admission status. Using R programming, the analysis covers data cleaning, handling missing values, and visualizing relationships between various variables and admission outcomes.

## Project Overview

- **Data Cleaning**: The dataset is loaded and checked for duplicate rows and missing values. Missing numerical and categorical values are imputed, and inconsistencies in labels are corrected.
- **Statistical Analysis**: Descriptive statistics, including mean, mode, standard deviation, and variance, are calculated for TOEFL scores and CGPA based on admission status.
- **Data Visualization**: Visualizations like boxplots, histograms, bar plots, and pie charts are generated to explore relationships between variables (e.g., TOEFL score, CGPA, university rating) and admission status.
- **Machine Learning**: A Naive Bayes classifier is used to predict student admission status based on selected features.

## Key R Libraries

- **ggplot2**: For creating various visualizations.
- **DataExplorer**: For generating exploratory data analysis reports.
- **e1071**: For implementing the Naive Bayes model.
- **caret**: For data preprocessing and model evaluation.

## Usage

1. **Data Loading and Preprocessing**: Ensure the dataset is loaded from the specified path and cleaned by running the preprocessing code.
2. **Statistical Analysis and Visualizations**: Run the code to perform statistical analysis and generate visualizations.
3. **Model Training and Evaluation**: Train the Naive Bayes model on the preprocessed dataset and evaluate it using the confusion matrix.

## Dependencies

- R packages: `ggplot2`, `DataExplorer`, `e1071`, `caTools`, `caret`

## Example Plots

- **Boxplot** of TOEFL Score by Admission Status
- **Histogram** of CGPA by Admission Status
- **Bar Plot** for University Rating by Admission Status
