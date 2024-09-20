# Part 3: Combining Data Frames Created in Parts 1 and 2, then calculating 5-Year and 10-Year Percent Changes
# install.packages("vctrs")
install.packages("corrplot")
library(tidyverse)
library(dplyr)
library(corrplot)
library(RColorBrewer)

Combined_df <- Pct_Expenditures_df
Combined_df <- bind_cols(Combined_df, CrimeRate_df[, 4:12])
colnames(Combined_df)[3]<- "AllGovExpendituresPerCapita"

# Creating data frames for each individual year

df_2007_Combined <- Combined_df %>%
  filter(Year == 2007)

df_2012_Combined <- Combined_df %>%
  filter(Year == 2012)

df_2017_Combined <- Combined_df %>%
  filter(Year == 2017)

PercentDiff_5Year <- df_2017_Combined %>%
  select(State) 

col_names <- names(df_2017_Combined)[3:16]


for (col_name in col_names) {
  PercentDiff_5Year[[col_name]] <- NA
}

# 5-Year percent Change for columns 3 through 16 (2012-2017_)
for (i in 3:16) {
  col_name <- names(df_2017_Combined)[i]
  PercentDiff_5Year[[col_name]] <- ((df_2017_Combined[[i]] - 
                                       df_2012_Combined[[i]]) / 
                                      df_2012_Combined[[i]]) * 100
}


# 10-Year Percent Changes (2007-2017)

PercentDiff_10Year <- df_2017_Combined %>%
  select(State) 

for (col_name in col_names) {
  PercentDiff_10Year[[col_name]] <- NA
}


for (i in 3:16) {
  col_name <- names(df_2017_Combined)[i]
  PercentDiff_10Year[[col_name]] <- ((df_2017_Combined[[i]] - 
                                        df_2007_Combined[[i]]) / 
                                       df_2007_Combined[[i]]) * 100
}

#
# Part 4: Correlation Matrixes
#



# Creating a correlation matrix to determine possible insights into variable relationships
numerical_data <- Combined_df[,-c(1,2)]


Combined_df_matrix <- cor(numerical_data)


corrplot(Combined_df_matrix, method = 'square', tl.cex = 0.45, cl.cex = 0.45, addCoef.col = "black", number.cex = 0.35)


# Creating a 5-Year Percent Change correlation matrix

FiveYearNumerical_df <- PercentDiff_5Year[,-1]

FiveYear_df_matrix <- cor(FiveYearNumerical_df)

corrplot(FiveYearNumerical_df, method = 'square', tl.cex = 0.45, cl.cex = 0.45, addCoef.col = "black", number.cex = 0.35)

# title = "Percent Change, 2012-2017", (Title cut off screen when included)

# Creating a 10-Year Percent Change correlation matrix

TenYearNumerical_df <- PercentDiff_10Year[,-1]

TenYear_df_matrix <- cor(TenYearNumerical_df)

corrplot(TenYear_df_matrix, method = 'square', tl.cex = 0.45, cl.cex = 0.45, addCoef.col = "black", number.cex = 0.35)
 
# title = "Percent Change, 2007-2017", title.cex = 2,








