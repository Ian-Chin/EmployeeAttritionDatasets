# ============================================================
# Employee Attrition Dataset - Analysis & Data Cleaning in R
# ============================================================

# Load required libraries
library(dplyr)
library(stringr)
library(rstudioapi) #changed

# 1. LOAD THE DATASET

# Set working directory to the folder where this script is located
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

df <- read.csv("4. dataset_employee_attrition.csv",
               stringsAsFactors = FALSE)

df[df == ""] <- NA #changed: convert empty strings to NA

# =========================
# 2. CHECK MISSING VALUES
# =========================
missing_counts <- colSums(is.na(df))

# =========================
# 3. REMOVE DUPLICATES
# =========================
df <- df[!duplicated(df), ]

# =========================
# 4. DEFINE COLUMNS
# =========================
categorical_cols <- c("Attrition", "BusinessTravel", "Department",
                      "EducationField", "Gender", "JobRole",
                      "MaritalStatus", "Over18", "OverTime")

# =========================
# 5–12 CLEAN CATEGORICAL DATA
# =========================
# Attrition
df$Attrition <- tolower(trimws(df$Attrition))
df$Attrition <- case_when(
  df$Attrition %in% c("yes", "1") ~ "Yes",
  df$Attrition %in% c("no", "0") ~ "No",
  TRUE ~ NA_character_
)

# BusinessTravel
df$BusinessTravel <- tolower(trimws(df$BusinessTravel))
df$BusinessTravel <- case_when(
  grepl("rarely", df$BusinessTravel) ~ "Travel_Rarely",
  grepl("frequent", df$BusinessTravel) ~ "Travel_Frequently",
  grepl("^non", df$BusinessTravel) ~ "Non-Travel", #changed: improve matching
  TRUE ~ NA_character_
)

# Department
df$Department <- tolower(trimws(df$Department))
df$Department <- case_when(
  grepl("r&d|research", df$Department) ~ "Research & Development",
  grepl("sale", df$Department) ~ "Sales",
  grepl("human|hr", df$Department) ~ "Human Resources",
  TRUE ~ NA_character_
)

# EducationField
df$EducationField <- tolower(trimws(df$EducationField))
df$EducationField <- case_when(
  grepl("life", df$EducationField) ~ "Life Sciences",
  grepl("medical", df$EducationField) ~ "Medical",
  grepl("marketing", df$EducationField) ~ "Marketing",
  grepl("technical|^td$", df$EducationField) ~ "Technical Degree",
  grepl("other", df$EducationField) ~ "Other",
  grepl("human|^hr$", df$EducationField) ~ "Human Resources",
  TRUE ~ NA_character_
)

# Gender
df$Gender <- tolower(trimws(df$Gender))
df$Gender <- case_when(
  df$Gender %in% c("male", "m") ~ "Male",
  df$Gender %in% c("female", "f") ~ "Female",
  TRUE ~NA_character_
)

# JobRole
df$JobRole <- tolower(trimws(df$JobRole))
df$JobRole <- case_when(
  grepl("sales exe", df$JobRole) ~ "Sales Executive",
  grepl('sales rep', df$JobRole) ~ "Sales Representative",
  grepl("research sci", df$JobRole) ~ "Research Scientist",
  grepl("lab", df$JobRole) ~ "Laboratory Technician",
  grepl("manufacturing", df$JobRole) ~ "Manufacturing Director",
  grepl("healthcare", df$JobRole) ~ "Healthcare Representative",
  grepl("manager", df$JobRole) & !grepl("research dir", df$JobRole) ~ "Manager",
  grepl("research dir", df$JobRole) ~ "Research Director",
  grepl("human|^hr$", df$JobRole) ~ "Human Resources",
  TRUE ~ NA_character_
)

# MaritalStatus
df$MaritalStatus <- tolower(trimws(df$MaritalStatus))
df$MaritalStatus <- case_when(
  grepl("single", df$MaritalStatus) ~ "Single",
  grepl("married", df$MaritalStatus) ~ "Married",
  grepl("divorced", df$MaritalStatus) ~ "Divorced",
  TRUE ~ NA_character_
)

# OverTime
df$OverTime <- tolower(trimws(df$OverTime))
df$OverTime <- case_when(
  df$OverTime %in% c("yes", "1") ~ "Yes",
  df$OverTime %in% c("no", "0") ~ "No",
  TRUE ~ NA_character_
)

# =========================
# 13. CLEAN NUMERIC COLUMNS
# =========================
numeric_cols <- c("Age", "DailyRate", "DistanceFromHome",
                  "Education", "EmployeeNumber",
                  "EnvironmentSatisfaction", "HourlyRate",
                  "JobInvolvement", "JobLevel", "JobSatisfaction",
                  "MonthlyIncome", "MonthlyRate", "NumCompaniesWorked",
                  "PercentSalaryHike", "PerformanceRating",
                  "RelationshipSatisfaction", "StockOptionLevel",
                  "TotalWorkingYears", "TrainingTimesLastYear",
                  "WorkLifeBalance", "YearsAtCompany",
                  "YearsInCurrentRole", "YearsSinceLastPromotion",
                  "YearsWithCurrManager")
for (col in numeric_cols) {
  df[[col]] <- gsub("[^0-9.\\-]", "", as.character(df[[col]]))
  df[[col]][df[[col]] == ""] <- NA   #added: handle empty values
  df[[col]] <- as.numeric(df[[col]])
}

df$WorkLifeBalance <- as.numeric(df$WorkLifeBalance)  #added: to ensure numeric

# Mode function
get_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Mode (categorical)
for (col in categorical_cols) {
  if (col %in% names(df)) {
    if (any(is.na(df[[col]]))) {
      df[[col]][is.na(df[[col]])] <- get_mode(df[[col]])
    }
  }
}

# =========================
# 15. REMOVE CONSTANT COLUMNS
# =========================
df <- df %>% select(-EmployeeCount, -StandardHours, -Over18)

# =========================
# 16–17 FINAL CHECK
# =========================
final_missing <- colSums(is.na(df))

# =========================
# 18. SAVE CLEAN DATA
# =========================
write.csv(df, "employee_attrition_cleaned.csv", row.names = FALSE)
