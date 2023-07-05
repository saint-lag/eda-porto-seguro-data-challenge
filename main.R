# Importing data
metadata=read.csv("metadata.csv")
train=read.csv("train.csv")

# Initialize the column index
col_index<-1

# Initialize an empty list to store column data
column_data_list <- list()

# Missing Values List
missing_values_list <- list()

# Iterate while the column index is less than or equal to 68
while (col_index <= 69) {
  # Create the variable name
  
  column_name <- paste0("var", col_index)
  
  # Access the column using the variable name
  column_data <- train[[column_name]]
  
  # Perform operations on the column
  
  column_data_list[[col_index]] <- column_data
  
  # Missing Values
  current_missing_value <- sum(is.na(column_data))
  missing_values_list[[col_index]] <- current_missing_value
  
  print(current_missing_value)
  
  # Increment the column index
  col_index <- col_index + 1
}

# Adding Y data to the list
y_column_data <- train[["y"]]

column_data_list[["y"]] <- y_column_data

y_missing_values <- sum(is.na(y_column_data))

missing_values_list[["y"]] <- y_missing_values

## Summary Statistics:

## Data Distribution:

## Pair Correlation:

# Utils
column_data_list_length <-length(column_data_list)

# Empty correlation matrix
correlation_matrix <- matrix(NA, nrow = column_data_list_length, ncol = column_data_list_length)

for(i in 1:column_data_list_length) {
  for(j in 1:column_data_list_length) {
    correlation_matrix[i, j] <- cor(column_data_list[[i]], column_data_list[[j]])
  }
}


## Outliers:

# Function to detect outliers using Tukey's fences
detect_outliers <- function(matrix_data) {
  outliers <- list()
  
  for (i in 1:column_data_list_length) {
    matrix_values <- matrix_data[[i]]
    
    # Calculate the lower and upper fences using Tukey's fences
    q1 <- quantile(matrix_values, 0.25)
    q3 <- quantile(matrix_values, 0.75)
    iqr <- q3 - q1
    lower_fence <- q1 - (1.5 * iqr)
    upper_fence <- q3 + (1.5 * iqr)
    
    # Identify outliers based on the fences
    outliers[[i]] <- matrix_values[matrix_values < lower_fence | matrix_values > upper_fence]
  }
  
  return(outliers)
}

# Find outliers in the list of matrices
outliers_list <- detect_outliers(column_data_list)


boxplot_outlier <- function(x) {
  current_matrix_value <- column_data_list[[x]]
  current_outliers <- outliers_list[[x]]
  boxplot(current_matrix_value, main = paste("Matrix", x), outline = FALSE)
  if (length(current_outliers) > 0) {
    points(rep(1, length(current_outliers)), current_outliers, col = "red", pch = 16)
  }
}

boxplot_outlier(68)

