# Part 5: Visualizing Relationships
install.packages("gridExtra")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(gridExtra)


# 
df3 <- Combined_df
df3$Year <- factor(Combined_df$Year, levels = c("2007", "2012", "2017"))
df3$JusticeFromTotalGov <- 100 * df3$JusticeFromTotalGov

# Summary tables for each year

Summary2007 <- df3 %>%
  filter(Year == 2007) %>%
  summarize_if(is.numeric, list(Mean = mean, Median = median, 'Standard Deviation' = sd, Q1 = ~quantile(., 0.25), Q3 = ~quantile(., 0.75), Min = min, Max = max )) %>%
  pivot_longer(cols = everything(), names_to = c("Variable", "Statistic"),
               names_sep = "_", values_to = "Value") %>%
  pivot_wider(names_from = "Variable", values_from = "Value") %>%
  mutate(Year = 2007) %>% select(Year, everything())

Summary2012 <- df3 %>%
  filter(Year == 2012) %>%
  summarize_if(is.numeric, list(Mean = mean, Median = median, 'Standard Deviation' = sd, Q1 = ~quantile(., 0.25), Q3 = ~quantile(., 0.75), Min = min, Max = max )) %>%
  pivot_longer(cols = everything(), names_to = c("Variable", "Statistic"),
               names_sep = "_", values_to = "Value") %>%
  pivot_wider(names_from = "Variable", values_from = "Value") %>%
  mutate(Year = 2012) %>% select(Year, everything())

Summary2017 <- df3 %>%
  filter(Year == 2017) %>%
  summarize_if(is.numeric, list(Mean = mean, Median = median, 'Standard Deviation' = sd, Q1 = ~quantile(., 0.25), Q3 = ~quantile(., 0.75), Min = min, Max = max )) %>%
  pivot_longer(cols = everything(), names_to = c("Variable", "Statistic"),
               names_sep = "_", values_to = "Value") %>%
  pivot_wider(names_from = "Variable", values_from = "Value") %>%
  mutate(Year = 2017) %>% select(Year, everything())

Summary_Expenditures <- bind_rows(Summary2007, Summary2012, Summary2017)
write.csv(Summary_Expenditures, "Summary_Expenditures.csv", row.names = FALSE)

SummaryExpendTypes <- Summary_Expenditures %>% select(-c(3,4))
write.csv(SummaryExpendTypes, "SummaryExpendTypes.csv", row.names = FALSE)




# Boxplot_1 <- ggplot(Combined_df, aes(x = Year, y = AllGovExpendituresPerCapita)) + geom_boxplot() + labs(title = "Boxplot", x = "Year", y = "Total Government Expenditures Per Capita")
  
Boxplot_1 <- ggplot(df3) + geom_boxplot(aes(x = Year, y = AllGovExpendituresPerCapita)) +
             labs(title = "Government Spending Per Capita by State", y = "U.S. Dollars") +
             ylim(0, 25000) + theme(plot.title = element_text(hjust = .5, size = 12))

Boxplot_2 <- ggplot(df3) + geom_boxplot(aes(x = Year, y = JusticeFromTotalGov)) +
              labs(title = "% of Total Government Expenditures \nDevoted to Justice System", y = "Percentage") +
              ylim(0, 15) + theme(plot.title = element_text(hjust = .5, size = 12))

# 2017 does not include an outlier dot for the District of Columbia due because it is more than 2x larger than the next highest value


# library(dplyr)
# library(gridExtra)

# Scatter Plot: Comparing Population Size and Violent Crime

df4 <- Combined_df[, 1:7]
df4$Population <- Crime_and_Expenditures_Data$Population


# Creating ViolentCrimeRate to show violent crime incidence per 10,000 residents

df4$ViolentCrimeRate <- (10000 * (Crime_and_Expenditures_Data[, 9]) / Crime_and_Expenditures_Data$Population)
df4$ViolentCrimeRate <- as.numeric(df4$ViolentCrimeRate[, 1])
df4$AllGovExpendituresPerCapita <- as.numeric(as.character(df4$AllGovExpendituresPerCapita))

df_2007 <- df4 %>% filter(Year == 2007)
df_2012 <- df4 %>% filter(Year == 2012) 
df_2017 <- df4 %>% filter(Year == 2017)

ScatterPlot_VCrime <- ggplot(df4, aes(x = AllGovExpendituresPerCapita, y = ViolentCrimeRate)) + geom_point()

ScatterPlot_VCrime_2 <- ggplot(df4, aes(x = JusticeFromTotalGov, y = ViolentCrimeRate)) + geom_point()

#
# Examining the impact of the 3 different types of justice system spending (Judicial/Legal; Police; Corrections) on Violent Crime Rates
#

 ScatterPlot_VCrime_3 <- ggplot(df4, aes(x = PoliceFromJustice, y = ViolentCrimeRate)) + 
                         geom_point() + labs(x = "Police Spending : Total Justice System Spending", y = "Violent Crime Incidences Per 10,000 Residents") +
                         geom_smooth(method = "lm", se = FALSE, color = "blue") + 
                         theme(axis.title.x = element_text(size = 9), axis.title.y = element_text(size = 9) )
#                        + scale_x_continuous(limits = c(0,1))

 ScatterPlot_VCrime_4 <- ggplot(df4, aes(x = JudicialLegalFromJustice , y = ViolentCrimeRate)) + 
                         geom_point() + labs(x = "Judicial & Legal Spending : Total Justice System Spending", y = "Violent Crime Incidences Per 10,000 Residents") +
                         geom_smooth(method = "lm", se = FALSE, color = "red") +
                         theme(axis.title.x = element_text(size = 9), axis.title.y = element_text(size = 9) )
#                       + scale_x_continuous(limits = c(0,1))Sc

 ScatterPlot_VCrime_5 <- ggplot(df4, aes(x = CorrectionsFromJustice , y = ViolentCrimeRate)) + 
                         geom_point() + labs(x = "Corrections Spending : Total Justice System Spending", y = "Violent Crime Incidences Per 10,000 Residents") +
                         geom_smooth(method = "lm", se = FALSE, color = "purple") +
                         theme(axis.title.x = element_text(size = 9), axis.title.y = element_text(size = 9) )
#                       + scale_x_continuous(limits = c(0,1))

# Plot all 3 types of justice system spending together
# grid.arrange(ScatterPlot_VCrime_3, ScatterPlot_VCrime_4, ScatterPlot_VCrime_5, ncol = 3)




# Examining how the portion of spending on the justice system changes as total government expenditures increases
 


ScatterPlot_1 <- ggplot(df4, aes(x = JusticeFromTotalGov, y = AllGovExpendituresPerCapita)) + geom_point()

  





                        
                        , aes(x = AllGovExpendituresPerCapita, y = ViolentCrimeRate)) + 
                 geom_point() # + ylim(0,15)

print(ScatterPlot_1)

CrimeRate_df




