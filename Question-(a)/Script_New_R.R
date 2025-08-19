library(tidyverse)

# --- 1. Load and Inspect Data ---
data <- read_csv('UQR.csv')
print('--- Data Structure (str) ---')
str(data)

print('--- Data Summary ---')
summary(data)

print('--- Head of Data ---')
head(data)

data <- data %>%
  rename_with(~ str_replace_all(., ' ', '_')) %>%
  rename_with(~ str_replace_all(., '%', 'Perc')) %>%
  rename_with(~ str_replace_all(., '\\(Avg.\\)', 'Avg')) %>%
  rename_with(~ str_replace_all(., '\\(Million_USD\\)', 'Million_USD')) %>%
  rename_with(~ str_replace_all(., '\\(USD\\)', 'USD'))

print('--- Data Structure after Renaming Columns ---')
str(data)

# --- 2. Check for Missing Values & Clean Data ---
print('--- Missing Values Check ---')
missing_values <- colSums(is.na(data))
print(missing_values)

if (any(missing_values > 0)) {
  print('--- Removing rows with missing values ---')
  data_cleaned <- na.omit(data)
  print(paste('Number of rows before cleaning:', nrow(data)))
  print(paste('Number of rows after cleaning:', nrow(data_cleaned)))
} else {
  print('No missing values found. Data is clean.')
  data_cleaned <- data 
}

numerical_cols <- data_cleaned %>%
  select(-Institution_Name, -Institution_Type) %>%
  select_if(is.numeric) %>%
  names()


# --- 3. Normality Tests (Shapiro-Wilk) ---
print('--- Shapiro-Wilk Normality Tests ---')
for (col in numerical_cols) {
  if (length(unique(data_cleaned[[col]])) > 1) { 
    print(paste('Shapiro-Wilk Test for:', col))
    shapiro_test <- shapiro.test(data_cleaned[[col]])
    print(shapiro_test)
  } else {
    print(paste('Skipping Shapiro-Wilk Test for', col, 'due to lack of variance.'))
  }
}


library(nortest)
# --- Anderson-Darling Normality Test ---
print('--- Anderson-Darling Normality Tests ---')
for (col in numerical_cols) {
  if (length(unique(data_cleaned[[col]])) > 1) {
    print(paste('Anderson-Darling Test for:', col))
    ad_test <- ad.test(data_cleaned[[col]])
    print(ad_test)
  } else {
    print(paste('Skipping Anderson-Darling Test for', col, 'due to lack of variance.'))
  }
}

# --- Lilliefor's (Kolmogorov-Smirnov) Normality Test ---
print('--- Lilliefor\'s Normality Tests ---')
for (col in numerical_cols) {
  if (length(unique(data_cleaned[[col]])) > 1) {
    print(paste('Lilliefor\'s Test for:', col))
    lillie_test <- lillie.test(data_cleaned[[col]])
    print(lillie_test)
  } else {
    print(paste('Skipping Lilliefor\'s Test for', col, 'due to lack of variance.'))
  }
}


# --- 4. Visualizations: Histograms and Q-Q Plots ---
print('--- Generating Histograms and Q-Q Plots ---')


pdf('UQR_EDA_Plots.pdf', width = 10, height = 7)

# Histograms
for (col in numerical_cols) {
  p <- ggplot(data_cleaned, aes(x = !!sym(col))) +
    geom_histogram(binwidth = diff(range(data_cleaned[[col]], na.rm = TRUE)) / 30,
                   fill = 'skyblue', color = 'black') +
    labs(title = paste('Histogram of', col), x = col, y = 'Frequency') +
    theme_minimal()
  print(p)
}

# Q-Q Plots
for (col in numerical_cols) {
  p <- ggplot(data_cleaned, aes(sample = !!sym(col))) +
    stat_qq() +
    stat_qq_line(color = 'red') +
    labs(title = paste('Q-Q Plot of', col), x = 'Theoretical Quantiles', y = 'Sample Quantiles') +
    theme_minimal()
  print(p)
}


dev.off()


# --- 5. Correlation Tests (Pearson and Spearman) ---
print('--- Correlation Tests with University_Ranking_Score ---')


dependent_var <- 'University_Ranking_Score'


independent_numerical_vars <- numerical_cols[numerical_cols != dependent_var]

# Pearson correlation tests
print('--- Pearson Correlation ---')
for (col in independent_numerical_vars) {
  cor_test_pearson <- cor.test(data_cleaned[[col]], data_cleaned[[dependent_var]], method = 'pearson')
  print(paste('Pearson Correlation between', col, 'and', dependent_var))
  print(cor_test_pearson)
}

# Spearman correlation tests
print('--- Spearman Correlation ---')
for (col in independent_numerical_vars) {
  cor_test_spearman <- cor.test(data_cleaned[[col]], data_cleaned[[dependent_var]], method = 'spearman')
  print(paste('Spearman Correlation between', col, 'and', dependent_var))
  print(cor_test_spearman)
}


# --- 6. Correlation Matrices (Pearson and Spearman) ---
library(corrplot)
print('--- Correlation Matrices ---')

# Pearson Correlation Matrix
print('--- Pearson Correlation Matrix ---')
correlation_matrix_pearson <- cor(data_cleaned[numerical_cols], method = 'pearson')
print(correlation_matrix_pearson)

# Spearman Correlation Matrix
print('--- Spearman Correlation Matrix ---')
correlation_matrix_spearman <- cor(data_cleaned[numerical_cols], method = 'spearman')
print(correlation_matrix_spearman)

png('UQR_Correlation_Matrix_Pearson.png', width = 800, height = 600)
corrplot(correlation_matrix_pearson, method = "circle", type = "lower",
         tl.cex = 0.8, number.cex = 0.7, title = "Pearson Correlation Matrix")
dev.off()

png('UQR_Correlation_Matrix_Spearman.png', width = 800, height = 600)
corrplot(correlation_matrix_spearman, method = "circle", type = "lower",
         tl.cex = 0.8, number.cex = 0.7, title = "Spearman Correlation Matrix")
dev.off()



# --- 7. Scatter Plots with Regression Lines ---
library(ggplot2)
library(rlang)

print('--- Generating Scatter Plots with Regression Lines ---')

for (col in independent_numerical_vars) {

  safe_col_name <- gsub("[^A-Za-z0-9_]", "_", col)
  file_name <- paste0('Scatter_', dependent_var, '_vs_', safe_col_name, '.png')
  

  if (length(unique(na.omit(data_cleaned[[col]]))) > 1) {
    
    png(filename = file_name, width = 800, height = 600)
    
    p <- ggplot(data_cleaned, aes(x = !!sym(col), y = !!sym(dependent_var))) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = 'lm', col = 'blue', se = TRUE) +
      labs(title = paste('Scatter Plot of', dependent_var, 'vs.', col),
           x = col, y = dependent_var) +
      theme_minimal()
    
    print(p)
    dev.off()
    
  } else {
    print(paste('Skipping', col, '- not enough variation to plot.'))
  }
}



# --- 8. Simple Linear Regression ---
print('--- Simple Linear Regression ---')


simple_lm_model <- lm(`University_Ranking_Score` ~ `Graduation_Rate_(Perc)`, data = data_cleaned)

print('Simple Linear Regression Model: University_Ranking_Score ~ Graduation_Rate_(Perc)')
print(summary(simple_lm_model)) 


lm_model_faculty <- lm(`University_Ranking_Score` ~ `Faculty_Salary_Avg`, data = data_cleaned)
summary(lm_model_faculty)



# --- 9. Multiple Linear Regression ---
print('--- Multiple Linear Regression ---')


safe_names <- function(col_names) {
  sapply(col_names, function(name) {
    if (grepl("[^[:alnum:]_]", name)) {
      paste0("`", name, "`")
    } else {
      name
    }
  })
}

predictors_for_mlr <- numerical_cols[numerical_cols != dependent_var]
escaped_dep_var <- safe_names(dependent_var)
escaped_preds <- safe_names(predictors_for_mlr)

mlr_formula <- as.formula(
  paste(escaped_dep_var, "~", paste(escaped_preds, collapse = " + "))
)

multiple_lm_model <- lm(mlr_formula, data = data_cleaned)
print('Multiple Linear Regression Model:')
print(mlr_formula)
print(summary(multiple_lm_model))




# --- 10. Model Summary and Interpretation (Code Only) ---
print('--- Model Summaries for Interpretation ---')

# Summary of the simple linear regression model
print('Summary of Simple Linear Regression Model:')
print(summary(simple_lm_model))

# Summary of the multiple linear regression model
print('Summary of Multiple Linear Regression Model:')
print(summary(multiple_lm_model))

# Accessing coefficients
print('Coefficients of Simple Linear Regression Model:')
print(coef(simple_lm_model))

print('Coefficients of Multiple Linear Regression Model:')
print(coef(multiple_lm_model))

# Accessing residuals
print('Residuals of Simple Linear Regression Model (first 6):')
print(head(residuals(simple_lm_model)))

print('Residuals of Multiple Linear Regression Model (first 6):')
print(head(residuals(multiple_lm_model)))

# Accessing fitted values
print('Fitted Values of Simple Linear Regression Model (first 6):')
print(head(fitted(simple_lm_model)))

print('Fitted Values of Multiple Linear Regression Model (first 6):')
print(head(fitted(multiple_lm_model)))













