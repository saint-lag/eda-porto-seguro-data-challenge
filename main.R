# libraries
library(ggplot2)
library(corrplot)
library(outliers)
library(corrr)
library(ggcorrplot)
library(factoextra)
library(openxlsx)
library(writexl)
library(aplpack)

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

# Removing 'id' column
train_without_id <- train
train_without_id['id'] <- NULL


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

# Dataframes:
quant_cont_df <- data.frame(quant_cont_list)
quant_disc_df <- data.frame(quant_disc_list)
quali_nom_df <- data.frame(quali_nom_list)
quali_ord_df <- data.frame(quali_nom_list)

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

## Summaries
sd_train <- sapply(train_without_id, sd)
summary_train <- summary(train_without_id)

# write.xlsx(summary_train, file = "train_summary.xlsx")
# write_xlsx(data.frame(sd_train), path = "sd_train.xlsx")

means <- colMeans(train_without_id)
minimums <- -apply(train_without_id, 2, min)
maximums <- apply(train_without_id, 2, max)
sd_values <- apply(train_without_id, 2, sd)

quali_nom_means <- colMeans(quali_nom_df)
quali_nom_minimums <- -apply(quali_nom_df, 2, min)
quali_nom_maximums <- apply(quali_nom_df, 2, max)
quali_nom_sd_values <- apply(quali_nom_df, 2, sd)

quant_cont_means <- colMeans(quant_cont_df)
quant_cont_minimums <- -apply(quant_cont_df, 2, min)
quant_cont_maximums <- apply(quant_cont_df, 2, max)
quant_cont_sd_values <- apply(quant_cont_df, 2, sd)

quant_disc_means <- colMeans(quant_disc_df)
quant_disc_minimums <- -apply(quant_disc_df, 2, min)
quant_disc_maximums <- apply(quant_disc_df, 2, max)
quant_disc_sd_values <- apply(quant_disc_df, 2, sd)

quali_ord_means <- colMeans(quali_ord_df)
quali_ord_minimums <- -apply(quali_ord_df, 2, min)
quali_ord_maximums <- apply(quali_ord_df, 2, max)
quali_ord_sd_values <- apply(quali_ord_df, 2, sd)

barplot(rbind(quali_nom_means, quali_nom_minimums, quali_nom_maximums, quali_nom_sd_values),
        beside = TRUE,
        names.arg = colnames(quali_nom_df),
        legend.text = c("Mean", "Minimum", "Maximum", "Standard Deviation"),
        col = c("steelblue", "orange", "green", "red"),
        ylab = "Values",
        main = "Summary Statistics of Variables (Qualitative Nominal)")

barplot(rbind(quant_cont_means, quant_cont_minimums, quant_cont_maximums, quant_cont_sd_values),
        beside = TRUE,
        names.arg = colnames(quant_cont_df),
        legend.text = c("Mean", "Minimum", "Maximum", "Standard Deviation"),
        col = c("steelblue", "orange", "green", "red"),
        ylab = "Values",
        main = "Summary Statistics of Variables (Quantitative Continuos)")

barplot(rbind(quant_disc_means, quant_disc_minimums, quant_disc_maximums, quant_disc_sd_values),
        beside = TRUE,
        names.arg = colnames(quant_disc_df),
        legend.text = c("Mean", "Minimum", "Maximum", "Standard Deviation"),
        col = c("steelblue", "orange", "green", "red"),
        ylab = "Values",
        main = "Summary Statistics of Variables (Quantitative Discrete)")

barplot(rbind(quali_ord_means, quali_ord_minimums, quali_ord_maximums, quali_ord_sd_values),
        beside = TRUE,
        names.arg = colnames(quali_ord_df),
        legend.text = c("Mean", "Minimum", "Maximum", "Standard Deviation"),
        col = c("steelblue", "orange", "green", "red"),
        ylab = "Values",
        main = "Summary Statistics of Variables (Qualitative Ordinal)")

## Pair Correlation:

# With relation to y:

# Removing y for this analysis
temp_quant_disc_list <- quant_disc_list
temp_quant_disc_list$y <- NULL

cor_quant_cont_y <- sapply(quant_cont_list, function(x) cor(x, train$y))
cor_quant_disc_y <- sapply(temp_quant_disc_list, function(x) cor(x, train$y))
cor_quali_ord_y <- sapply(quali_ord_list, function(x) cor(x, train$y))
cor_quali_nom_y <- sapply(quali_nom_list, function(x) cor(x, train$y))

# Summaries in relation to y:
summary(cor_quali_nom_y)
summary(cor_quali_ord_y)
summary(cor_quant_cont_y)
summary(cor_quant_disc_y)

# Pairs type by type:
# Continuos X Discrete
cor_matrix_quant_cont_disc <- cor(do.call(cbind, quant_cont_list), do.call(cbind, temp_quant_disc_list))
# Continuos X Nominal
cor_matrix_quant_cont_nom <- cor(do.call(cbind, quant_cont_list), do.call(cbind, quali_nom_list))
# Discrete X Nominal
cor_matrix_quant_disc_nom <- cor(do.call(cbind, quant_disc_list), do.call(cbind, quali_nom_list))

# Summaries type by type:
corrplot.mixed(cor_matrix_quant_cont_disc, tl.col="black", tl.pos="lt")
corrplot.mixed(cor_matrix_quant_cont_nom, tl.col="black", tl.pos="lt")
corrplot.mixed(cor_matrix_quant_disc_nom, tl.col="black", tl.pos="lt")


# Pairs in each type:
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
corrplot.mixed(cor_matrix_quali_nom, tl.col="black", tl.pos="lt")

## PCA
# Normalization
train_normalized <- scale(train_without_id)
quali_nom_normalized <- scale(quali_nom_df)
quali_ord_normalized <- scale(quali_ord_df)
quant_cont_normalized <- scale(quant_cont_df)
quant_disc_normalized <- scale(quant_disc_df)


# PCA: Correlation matrix
corr_train_matrix <- cor(train_normalized)
ggcorrplot(corr_matrix)

# Applying PCA
data.pca <- princomp(corr_matrix)
summary(data.pca)
fviz_eig(data.pca, addlabels=TRUE)

# PCA: Biplot
fviz_pca_var(data.pca, col.var = "black", repel=TRUE)

# Contribution of each variable
num_vars = 23 # One third of the total of vars[69/3]

# 10 components, since they add up to 87% of relevance 
dim = 10
fviz_cos2(data.pca, top = num_vars, choice = "var", axes = 1:dim)
fviz_cos2(data.pca, top = num_vars, choice = "var", axes = 1:2)

# Biplot & cos2
fviz_pca_var(data.pca, col.var = 'cos2',
             gradient.cols = c('blue', 'purple', 'magenta'),
             repel = TRUE)

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

