# ============================================================
# Employee Attrition Dataset - Analysis & Data Cleaning in R
# ============================================================

# Load required libraries
library(dplyr)
library(stringr)


# 1. LOAD THE DATASET

# Set working directory to the folder where this script is located
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

df <- read.csv("4. dataset_employee_attrition.csv",
               stringsAsFactors = FALSE)

cat("========== INITIAL DATASET OVERVIEW ==========\n")
cat("Rows:", nrow(df), "\n")
cat("Columns:", ncol(df), "\n\n")

str(df)
cat("\n")
summary(df)


# 2. CHECK FOR MISSING VALUES

cat("\n========== MISSING VALUES PER COLUMN ==========\n")
missing_counts <- colSums(is.na(df))
print(missing_counts[missing_counts > 0])
if (all(missing_counts == 0)) cat("No NA values detected.\n")

# Check for empty strings (hidden missing values)
cat("\n========== EMPTY STRINGS PER COLUMN ==========\n")
empty_counts <- sapply(df, function(x) sum(x == "", na.rm = TRUE))
print(empty_counts[empty_counts > 0])


# 3. CHECK FOR DUPLICATES

cat("\n========== DUPLICATE ROWS ==========\n")
dup_count <- sum(duplicated(df))
cat("Number of duplicate rows:", dup_count, "\n")

# Remove duplicates if any
if (dup_count > 0) {
  df <- df[!duplicated(df), ]
  cat("Duplicates removed. New row count:", nrow(df), "\n")
}


# 4. INSPECT UNIQUE VALUES FOR CATEGORICAL COLUMNS

cat("\n========== UNIQUE VALUES BEFORE CLEANING ==========\n")

categorical_cols <- c("Attrition", "BusinessTravel", "Department",
                       "EducationField", "Gender", "JobRole",
                       "MaritalStatus", "Over18", "OverTime")

for (col in categorical_cols) {
  cat("\n---", col, "---\n")
  print(table(df[[col]], useNA = "ifany"))
}


# 5. CLEAN COLUMN: Attrition
# Standardize to "Yes" / "No"
# Values found: yes, no, YES, 1, 0, etc.

df$Attrition <- tolower(trimws(df$Attrition))
df$Attrition <- case_when(
  df$Attrition %in% c("yes", "1") ~ "Yes",
  df$Attrition %in% c("no", "0")  ~ "No",
  TRUE ~ NA_character_
)


# 6. CLEAN COLUMN: BusinessTravel
# Standardize to: "Travel_Rarely", "Travel_Frequently", "Non-Travel"

df$BusinessTravel <- tolower(trimws(df$BusinessTravel))
df$BusinessTravel <- case_when(
  grepl("rarely", df$BusinessTravel)      ~ "Travel_Rarely",
  grepl("frequent", df$BusinessTravel)    ~ "Travel_Frequently",
  grepl("non", df$BusinessTravel)         ~ "Non-Travel",
  TRUE ~ NA_character_
)


# 7. CLEAN COLUMN: Department
# Standardize to: "Sales", "Research & Development", "Human Resources"

df$Department <- tolower(trimws(df$Department))
df$Department <- case_when(
  grepl("r&d|r & d|research", df$Department)  ~ "Research & Development",
  grepl("sale", df$Department)                ~ "Sales",
  grepl("human|hr", df$Department)            ~ "Human Resources",
  TRUE ~ NA_character_
)


# 8. CLEAN COLUMN: EducationField
# Standardize to proper case

df$EducationField <- tolower(trimws(df$EducationField))
df$EducationField <- case_when(
  grepl("life", df$EducationField)       ~ "Life Sciences",
  grepl("medical", df$EducationField)    ~ "Medical",
  grepl("marketing", df$EducationField)  ~ "Marketing",
  grepl("technical", df$EducationField) |
    grepl("^td$", df$EducationField)     ~ "Technical Degree",
  grepl("other", df$EducationField)      ~ "Other",
  grepl("human", df$EducationField) |
    grepl("^hr$", df$EducationField)     ~ "Human Resources",
  grepl("^ls$", df$EducationField)       ~ "Life Sciences",
  TRUE ~ NA_character_
)


# 9. CLEAN COLUMN: Gender
# Standardize to "Male" / "Female"
# ============================================================
df$Gender <- tolower(trimws(df$Gender))
df$Gender <- case_when(
  df$Gender %in% c("male", "m")   ~ "Male",
  df$Gender %in% c("female", "f") ~ "Female",
  TRUE ~ NA_character_
)


# 10. CLEAN COLUMN: JobRole
# Standardize to proper title case
# ============================================================
df$JobRole <- tolower(trimws(df$JobRole))
df$JobRole <- case_when(
  grepl("sales exe", df$JobRole)          ~ "Sales Executive",
  grepl("sales rep", df$JobRole)          ~ "Sales Representative",
  grepl("research sci", df$JobRole)       ~ "Research Scientist",
  grepl("lab", df$JobRole)                ~ "Laboratory Technician",
  grepl("manufacturing", df$JobRole)      ~ "Manufacturing Director",
  grepl("healthcare", df$JobRole)         ~ "Healthcare Representative",
  grepl("manager", df$JobRole) &
    !grepl("research dir", df$JobRole)    ~ "Manager",
  grepl("research dir", df$JobRole)       ~ "Research Director",
  grepl("human", df$JobRole) |
    grepl("^hr$", df$JobRole)             ~ "Human Resources",
  TRUE ~ NA_character_
)


# 11. CLEAN COLUMN: MaritalStatus
# Standardize to "Single", "Married", "Divorced"
# ============================================================
df$MaritalStatus <- tolower(trimws(df$MaritalStatus))
df$MaritalStatus <- case_when(
  grepl("single", df$MaritalStatus)   ~ "Single",
  grepl("married", df$MaritalStatus)  ~ "Married",
  grepl("divorced", df$MaritalStatus) ~ "Divorced",
  TRUE ~ NA_character_
)


# 12. CLEAN COLUMN: OverTime
# Standardize to "Yes" / "No"
# ============================================================
df$OverTime <- tolower(trimws(df$OverTime))
df$OverTime <- case_when(
  df$OverTime %in% c("yes", "1") ~ "Yes",
  df$OverTime %in% c("no", "0")  ~ "No",
  df$OverTime == ""               ~ NA_character_,
  TRUE ~ NA_character_
)


# 13. CLEAN NUMERIC COLUMNS
# Remove non-numeric characters (e.g., "1?", "11_") and convert
# ============================================================
numeric_cols <- c("Age", "DailyRate", "DistanceFromHome", "Education",
                  "EmployeeCount", "EmployeeNumber",
                  "EnvironmentSatisfaction", "HourlyRate",
                  "JobInvolvement", "JobLevel", "JobSatisfaction",
                  "MonthlyIncome", "MonthlyRate", "NumCompaniesWorked",
                  "PercentSalaryHike", "PerformanceRating",
                  "RelationshipSatisfaction", "StandardHours",
                  "StockOptionLevel", "TotalWorkingYears",
                  "TrainingTimesLastYear", "WorkLifeBalance",
                  "YearsAtCompany", "YearsInCurrentRole",
                  "YearsSinceLastPromotion", "YearsWithCurrManager")

for (col in numeric_cols) {
  # Strip any non-numeric characters except minus and decimal point
  df[[col]] <- gsub("[^0-9.\\-]", "", as.character(df[[col]]))
  df[[col]] <- as.numeric(df[[col]])
}

cat("\n========== NUMERIC COLUMNS: NA COUNT AFTER CLEANING ==========\n")
numeric_na <- colSums(is.na(df[, numeric_cols]))
print(numeric_na[numeric_na > 0])
if (all(numeric_na == 0)) cat("No NA values in numeric columns.\n")


# 14. HANDLE REMAINING MISSING VALUES
# ============================================================
cat("\n========== TOTAL MISSING VALUES AFTER CLEANING ==========\n")
total_missing <- colSums(is.na(df))
print(total_missing[total_missing > 0])

# Impute numeric NAs with median
for (col in numeric_cols) {
  if (any(is.na(df[[col]]))) {
    median_val <- median(df[[col]], na.rm = TRUE)
    df[[col]][is.na(df[[col]])] <- median_val
    cat("Imputed", col, "NAs with median:", median_val, "\n")
  }
}

# Impute categorical NAs with mode
get_mode <- function(x) {
  x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

for (col in categorical_cols) {
  if (any(is.na(df[[col]]))) {
    mode_val <- get_mode(df[[col]])
    df[[col]][is.na(df[[col]])] <- mode_val
    cat("Imputed", col, "NAs with mode:", mode_val, "\n")
  }
}


# 15. REMOVE CONSTANT / IRRELEVANT COLUMNS
# EmployeeCount = always 1, StandardHours = always 80, Over18 = always Y
# ============================================================
cat("\n========== REMOVING CONSTANT COLUMNS ==========\n")
cat("EmployeeCount unique values:", length(unique(df$EmployeeCount)), "\n")
cat("StandardHours unique values:", length(unique(df$StandardHours)), "\n")
cat("Over18 unique values:", length(unique(df$Over18)), "\n")

df <- df %>% select(-EmployeeCount, -StandardHours, -Over18)
cat("Removed: EmployeeCount, StandardHours, Over18\n")


# 16. VERIFY CLEANED DATA
# ============================================================
cat("\n========== FINAL DATASET OVERVIEW ==========\n")
cat("Rows:", nrow(df), "\n")
cat("Columns:", ncol(df), "\n\n")

str(df)

cat("\n========== UNIQUE VALUES AFTER CLEANING ==========\n")
clean_categorical <- c("Attrition", "BusinessTravel", "Department",
                        "EducationField", "Gender", "JobRole",
                        "MaritalStatus", "OverTime")

for (col in clean_categorical) {
  cat("\n---", col, "---\n")
  print(table(df[[col]], useNA = "ifany"))
}

cat("\n========== NUMERIC COLUMN SUMMARY ==========\n")
summary(df %>% select(where(is.numeric)))


# 17. FINAL MISSING VALUE CHECK
# ============================================================
cat("\n========== FINAL MISSING VALUE CHECK ==========\n")
final_missing <- colSums(is.na(df))
if (all(final_missing == 0)) {
  cat("SUCCESS: No missing values remain in the dataset.\n")
} else {
  print(final_missing[final_missing > 0])
}


# 18. SAVE CLEANED DATASET
# ============================================================
write.csv(df, "employee_attrition_cleaned.csv",
          row.names = FALSE)
cat("\nCleaned dataset saved to: employee_attrition_cleaned.csv\n")
cat("========== DATA CLEANING COMPLETE ==========\n")
