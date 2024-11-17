
## Created by Mickey Rice
## October 22nd, 2024

# Using SQL to combine datasets and extract specific values---------------------

install.packages("readxl")  # For reading Excel files
install.packages("sqldf")   # For running SQL queries
install.packages("openxlsx") # For writing xcel files


library(readxl)
library(sqldf)
library(openxlsx)

# Load the participant and screener files (replace with your actual file paths)
participants <- read_excel("/Users/mickey.rice/Desktop/Important/PMA docs/Project with Josh/ParticipantInfo.xlsx")
data <- read_excel("/Users/mickey.rice/Desktop/Important/PMA docs/Project with Josh/DEX_FA_Data.xlsx")
screener <- read_excel("/Users/mickey.rice/Desktop/Important/PMA docs/Project with Josh/Data_NoNames.xlsx")

# SQL query to join the tables based on the ID_Number and add Idnumber to the data
query <- "
  SELECT d.*, 
         p.[Idnumber] as CombinedID
  FROM data d
  LEFT JOIN participants p
  ON p.ID_Number = d.ID_Number
"

# Run the SQL query to combine the data and add the 'Idnumber' to 'data'
combined_data1 <- sqldf(query)


# Create a string of all Q8.* columns from Q8.1 to Q8.20
q8_columns <- paste0("s.[Q8.", 1:32, "] as Schizo", 1:32, collapse = ",\n         ")

# Construct the full SQL query
query <- paste0("
  SELECT p.*, 
         ", q8_columns, "
  FROM combined_data1 p
  JOIN screener s
  ON p.CombinedID = s.Idnumber
")
# Run the SQL query to combine the data
combined_data <- sqldf(query)

library(dplyr)

combined_data <- combined_data %>%
  mutate(across(starts_with("Schizo"), ~ case_when(
    . == "Strongly Disagree" ~ 1,
    . == "Disagree" ~ 2,
    . == "Neutral" ~ 3,
    . == "Agree" ~ 4,
    . == "Strongly Agree" ~ 5
  )))

# Save to a new Excel file
write.xlsx(combined_data, "/Users/mickey.rice/Desktop/combined_data.xlsx")


# Run stats! ------------------------------------------------------------------

library(psych)
library(tidyverse)
library(apaTables)
library(readxl)

data <- read_excel("/Users/mickey.rice/Desktop/Important/PMA docs/Project with Josh/combined_data.xlsx")

data$Sex1<- 0
data$Sex1<-ifelse(data$Gender=="Male",-1, 1)

data$Gender1<- 0
data$Gender1<-ifelse(data$Gender=="Male", 0, 
                   ifelse(data$Gender=="Female", 1, 2))

# Overall schizophrenia --------------------------------------------------------

model <- lm(Accuracy ~ TotalSchizo + Gender1, data = data)
summary(model)

model <- lm(N300 ~ TotalSchizo + Gender1, data = data)
summary(model)

model <- lm(PP ~ TotalSchizo + Gender1, data = data)
summary(model)

# 4 Model Factor Analysis ------------------------------------------------------

# SF1CP
model <- lm(Accuracy ~ SF1CP + Gender1, data = data)
summary(model)

model <- lm(N300 ~ SF1CP + Gender1, data = data)
summary(model)

model <- lm(PP ~ SF1CP + Gender1, data = data)
summary(model)

# SF3IP2
model <- lm(Accuracy ~ SF3IP2 + Gender1, data = data)
summary(model)

model <- lm(N300 ~ SF3IP2 + Gender1, data = data)
summary(model)

model <- lm(PP ~ SF3IP2 + Gender1, data = data)
summary(model)

# SF3DO
model <- lm(Accuracy ~ SF3DO + Gender1, data = data)
summary(model)

model <- lm(N300 ~ SF3DO + Gender1, data = data)
summary(model)

model <- lm(PP ~ SF3DO + Gender1, data = data)
summary(model)

# SF4SA
model <- lm(Accuracy ~ SF4SA + Gender1, data = data)
summary(model)

model <- lm(N300 ~ SF4SA + Gender1, data = data)
summary(model)

model <- lm(PP ~ SF4SA + Gender1, data = data)
summary(model)

# 9 Model Factor Analysis ------------------------------------------------------
# SFIR
model <- lm(Accuracy ~ SFIR + Gender1, data = data)
summary(model)

model <- lm(N300 ~ SFIR + Gender1, data = data)
summary(model)

model <- lm(PP ~ SFIR + Gender1, data = data)
summary(model)

# SFSU
model <- lm(Accuracy ~ SFSU + Gender1, data = data)
summary(model)

model <- lm(N300 ~ SFSU + Gender1, data = data)
summary(model)

model <- lm(PP ~ SFSU + Gender1, data = data)
summary(model)

# SFCF
model <- lm(Accuracy ~ SFCF + Gender1, data = data)
summary(model)

model <- lm(N300 ~ SFCF + Gender1, data = data)
summary(model)

model <- lm(PP ~ SFCF + Gender1, data = data)
summary(model)

# SFCA
model <- lm(Accuracy ~ SFCA + Gender1, data = data)
summary(model)

model <- lm(N300 ~ SFCA + Gender1, data = data)
summary(model)

model <- lm(PP ~ SFCA + Gender1, data = data)
summary(model)

# SFEB
model <- lm(Accuracy ~ SFEB + Gender1, data = data)
summary(model)

model <- lm(N300 ~ SFEB + Gender1, data = data)
summary(model)

model <- lm(PP ~ SFEB + Gender1, data = data)
summary(model)

# SFSA
model <- lm(Accuracy ~ SFSA + Gender1, data = data)
summary(model)

model <- lm(N300 ~ SFSA + Gender1, data = data)
summary(model)

model <- lm(PP ~ SFSA + Gender1, data = data)
summary(model)

# SFMT
model <- lm(Accuracy ~ SFMT + Gender1, data = data)
summary(model)

model <- lm(N300 ~ SFMT + Gender1, data = data)
summary(model)

model <- lm(PP ~ SFMT + Gender1, data = data)
summary(model)

# SFOS
model <- lm(Accuracy ~ SFOS + Gender1, data = data)
summary(model)

model <- lm(N300 ~ SFOS + Gender1, data = data)
summary(model)

model <- lm(PP ~ SFOS + Gender1, data = data)
summary(model)

# SFUP
model <- lm(Accuracy ~ SFUP + Gender1, data = data)
summary(model)

model <- lm(N300 ~ SFUP + Gender1, data = data)
summary(model)

model <- lm(PP ~ SFUP + Gender1, data = data)
summary(model)

## centering the variable - most important for interaction
TotalSchizo <- data$TotalSchizo
mean_TotalSchizo <- mean(TotalSchizo)
TotalSchizo_c <- TotalSchizo - mean_TotalSchizo
data$TotalSchizo_c <- TotalSchizo_c

STAIS <- data$STAIS
mean_STAIS <- mean(STAIS)
STAIS_c <- STAIS - mean_STAIS
data$STAIS_c <- STAIS_c

# T-test ----------------------------------------------------------------------

# Low depression subset
low <- data[data$TotalSchizo <= 70, ]

describe(low)

# High depression subset
high <- data[data$TotalSchizo >= 110, ]

describe(high)

t_test_result1 <- t.test(low$N300, high$N300, paired = FALSE)

# View the result
print(t_test_result1)


# Trying to run our own factor analysis ----------------------------------------

# Select data for Factor Analysis ----------------------------------------------
df <- select(data, Schizo1, Schizo2, Schizo3, Schizo4, Schizo5, Schizo6, Schizo7,
             Schizo8, Schizo9, Schizo10, Schizo11, Schizo12, Schizo13, Schizo14,
             Schizo15, Schizo16, Schizo17, Schizo18, Schizo19, Schizo20, Schizo21,
             Schizo22, Schizo23, Schizo24, Schizo25, Schizo26, Schizo27, Schizo28,
             Schizo29, Schizo30, Schizo31, Schizo32)
describe(df)

# extract the correlation matrix
cor_df <- cor(df)
print(cor_df)

## Factor analysis using the fa() function--------------------------------------

# Use fa.parallel to produce scree plot
fa2 <- fa.parallel(df, fm = "pa", fa = "fa", 
                   main="Parallel Analysis Scree Plots", n.iter=100)
print(fa2)

# run factor analysis based on scree plot
library(GPArotation)
fa1 <- fa(df, nfactors = 4, rotate = "oblimin", fm = "pa")
print(fa1)

fa1 <- fa(df, nfactors = 4, rotate = "oblimin", fm = "pa", scores = "regression")
factor_scores <- fa1$scores  # Extracts the factor scores

data <- cbind(data, factor_scores)

# Loadings above 0.4
loadings_matrix <- fa1$loadings
significant_loadings <- loadings_matrix[abs(loadings_matrix) > 0.4]
print(significant_loadings)

loadings_df <- as.data.frame.matrix(loadings_matrix)
for (factor in colnames(loadings_df)) {
  cat("\n", factor, "\n")
  print(loadings_df[order(-abs(loadings_df[[factor]])), factor, drop = FALSE][abs(loadings_df[[factor]]) > 0.4, ])
}

model <- lm(Accuracy ~ PA1 + Gender1, data = data)
summary(model)

model <- lm(N300 ~ PA1 + Gender1, data = data)
summary(model)

model <- lm(PP ~ PA1 + Gender1, data = data)
summary(model)
