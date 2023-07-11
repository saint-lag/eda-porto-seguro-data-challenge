# libraries
library(ggplot2)
library(corrplot)

# Importing data
metadata=read.csv("metadata.csv")
train=read.csv("train.csv")

# Initialize the column index
col_index<-1

# Initialize an empty list to store column data
column_data_list <- list()

# Missing Values List
missing_values_list <- list()

# Running through train data
while (col_index <= length(col(train))) {
  # Create the variable name
  column_name <- paste0("var", col_index)
  
  # Access the column using the variable name
  column_data <- train[[column_name]]
  
  # Perform operations on the column
  column_data_list[[col_index]] <- column_data
  
  # Searching for missing values 
  current_missing_value <- sum(is.na(column_data))
  # Filling missing_values_list
  missing_values_list[[col_index]] <- current_missing_value
  
  # Increment the column index
  col_index <- col_index + 1
}

# Adding Y data to the list
y_column_data <- train[["y"]]

column_data_list[["y"]] <- y_column_data

y_missing_values <- sum(is.na(y_column_data))

missing_values_list[["y"]] <- y_missing_values

## Data Distribution:

# Creating a new dataframe for each data type
quali_nom <- metadata[metadata$Variavel.tipo=="Qualitativo nominal" & metadata$Variavel.cod != 'id',]
quali_ord <- metadata[metadata$Variavel.tipo=="Qualitativo ordinal", ]
quant_disc <- metadata[metadata$Variavel.tipo=="Quantitativo discreto", ]
quant_cont <- metadata[metadata$Variavel.tipo=="Quantitativo continua", ]

# Creating a list for each new dataframe
quali_nom_list <- list()
quali_ord_list <- list()
quant_disc_list <- list()
quant_cont_list <- list()

create_list_for_df <- function(df) {
  col_index <- 1
  
  # Temporary list
  df_list <- list()
  
  while (col_index <= length(col(df[1]))) {
    column_name <- df$Variavel.cod[col_index]
    
    # Access the column using the variable name
    column_data <- train[[column_name]]
    
    # Increment the list
    df_list[[col_index]] <- column_data
    
    # Counter
    col_index <- col_index + 1 
  }
  return(df_list)
}

# Populating lists
quali_ord_list <- create_list_for_df(quali_ord)
quali_nom_list <- create_list_for_df(quali_nom)
quant_cont_list <- create_list_for_df(quant_cont)
quant_disc_list <- create_list_for_df(quant_disc)

# Naming list vectors
names(quali_nom_list) <- quali_nom$Variavel.cod
names(quali_ord_list) <- quali_ord$Variavel.cod
names(quant_cont_list) <- quant_cont$Variavel.cod
names(quant_disc_list) <- quant_disc$Variavel.cod

## Summary Statistics:
quant_cont_summary <- list()
for(i in seq(length(quant_cont_list))) {
  quant_cont_summary[[i]] <- summary(quant_cont_list[[i]])
}

## Pair Correlation:

# With relation to y:
cor_quant_cont_y <- sapply(quant_cont_list, function(x) cor(x, train$y))
cor_quant_disc_y <- sapply(quant_disc_list, function(x) cor(x, train$y))
cor_quali_ord_y <- sapply(quali_ord_list, function(x) cor(x, train$y))
cor_quali_nom_y <- sapply(quali_nom_list, function(x) cor(x, train$y))

# Pairs:
cor_matrix_quant_cont <- cor(do.call(cbind, quant_cont_list))
cor_matrix_quant_disc <- cor(do.call(cbind, quant_disc_list))
cor_matrix_quali_nom <- cor(do.call(cbind, quali_nom_list))
cor_matrix_quali_ord <- cor(do.call(cbind, quali_ord_list))

# Selecting high correlations

# Threshold
threshold <- 0.7

# Plotting
corrplot.mixed(cor_matrix_quant_cont, tl.col="black", tl.pos="lt")
corrplot.mixed(cor_matrix_quant_disc, tl.col="black", tl.pos="lt")

## Utils
column_data_list_length <-length(column_data_list)

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


boxplot_outlier <- function(x, df) {
  current_matrix_value <- column_data_list[[x]]
  current_outliers <- outliers_list[[x]]
  if(missing(df)) {
    boxplot(current_matrix_value, main = paste("Column", x), outline = FALSE) 
  } else {
    boxplot(current_matrix_value, main = paste(df$Variavel.cod[x]))
  }
  if (length(current_outliers) > 0) {
    points(rep(1, length(current_outliers)), current_outliers, col = "red", pch = 16)
  }
}

# Outliers for each data type:

boxplot_outlier_of_df_list <- function(df_list, df){
  for (i in seq(length(df_list))) {
    boxplot_outlier(i, df)
  }
}  

boxplot_outlier_of_df_list(quant_disc_list, quant_disc)
boxplot_outlier_of_df_list(quant_cont_list, quant_cont)
boxplot_outlier_of_df_list(quali_nom_list, quali_nom)
boxplot_outlier_of_df_list(quali_ord_list, quali_ord)



