# Part 3: Combining Data Frames Created in Parts 1 and 2, then creating a correlation matrix
install.packages("corrplot")
library(tidyverse)
library(dplyr)
library(corrplot)
library(RColorBrewer)

Combined_df <- Pct_Expenditures_df
Combined_df <- bind_cols(Combined_df, CrimeRate_df[, 4:12])


numerical_data <- Combined_df[,-c(1,2)]

# Creating a correlation matrix to determine possible insights into variable relationships

Combined_df_matrix <- cor(numerical_data)


corrplot(Combined_df_matrix, method = 'square', tl.cex = 0.45, cl.cex = 0.45, addCoef.col = "black", number.cex = 0.35)


