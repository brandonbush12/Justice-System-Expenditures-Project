install.packages("readxl")
library(readxl)

Crime_and_Expenditures_Data <- read_excel("~/Desktop/Incarceration Data Project/Consolidated Data Crime and Expenditures.xlsx")
Pct_Expenditures_df <- Crime_and_Expenditures_Data[, c("Year", "State", "Expenditures Per Capita, All government functions")]

df1 <- Crime_and_Expenditures_Data[, 1:8]

# Creating new column to show justice sysytem expenditures as portions of total government expenditures
Pct_Expenditures_df$JusticeFromTotalGov <- 
  df1$`Expenditures Per Capita, Total justice system`/ df1$`Expenditures Per Capita, All government functions`

# Creating new columns to show different proportions of total justice system spending that each state devoted to police protection, judicial/legal, and corrections
Pct_Expenditures_df$PoliceFromJustice <- 
  df1$`Expenditures Per Capita, Police protection`/ df1$`Expenditures Per Capita, Total justice system`

Pct_Expenditures_df$JudicialLegalFromJustice <-
  df1$`Expenditures Per Capita, Judicial and legal functions` / df1$`Expenditures Per Capita, Total justice system`

Pct_Expenditures_df$CorrectionsFromJustice <-
  df1$`Expenditures Per Capita, Corrections` / df1$`Expenditures Per Capita, Total justice system`

#
# Part 2
#

df2 <- Crime_and_Expenditures_Data[, -c(4:8)]

# Creating new data frame to calculate crime rate per 1,000 residents for each crime type
CrimeRate_df <- df2[,1:3]

CrimeRate_df$ViolentCrimeRate <- (1000 * (df2[, 4])) / df2$Population

CrimeRate_df$MurderManslaughterIncidence <- (1000 * (df2[, 5])) / df2$Population

CrimeRate_df$RapeIncidence <- (1000 * (df2[, 6])) / df2$Population

CrimeRate_df$RobberyIncidence <- (1000 * (df2[, 7])) / df2$Population

CrimeRate_df$AggravatedAssaultIncidence <- (1000 * (df2[, 8])) / df2$Population

CrimeRate_df$PropertyCrimeIncidence <- (1000 * (df2[, 9])) / df2$Population

CrimeRate_df$BurglaryIncidence <- (1000 * (df2[, 10])) / df2$Population

CrimeRate_df$LarcenyTheftIncidence <- (1000 * (df2[, 11])) / df2$Population

CrimeRate_df$MotorVehicleIncidence <- (1000 * (df2[, 12])) / df2$Population

