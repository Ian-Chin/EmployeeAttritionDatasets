# ============================================================
# Objective 1: Ian Chin Jun Sheng (TP076218)
# To examine whether monthly income has a significant impact
# on employee attrition by comparing income distributions
# and attrition rates across income brackets.
# ============================================================

library(dplyr)
library(ggplot2)

# Set working directory to script location
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load cleaned dataset
df <- read.csv("employee_attrition_cleaned.csv", stringsAsFactors = FALSE)

# ============================================================
# DESCRIPTIVE ANALYSIS 1-1:
# What is the distribution of MonthlyIncome between employees
# who left (Attrition = Yes) and those who stayed (Attrition = No)?
# ============================================================

# --- Summary Statistics ---
income_summary <- df %>%
  group_by(Attrition) %>%
  summarise(
    Count  = n(),
    Mean   = round(mean(MonthlyIncome,   na.rm = TRUE), 2),
    Median = round(median(MonthlyIncome, na.rm = TRUE), 2),
    SD     = round(sd(MonthlyIncome,     na.rm = TRUE), 2),
    Min    = min(MonthlyIncome, na.rm = TRUE),
    Max    = max(MonthlyIncome, na.rm = TRUE)
  )

print(income_summary)

# --- Boxplot: MonthlyIncome by Attrition ---
ggplot(df, aes(x = Attrition, y = MonthlyIncome, fill = Attrition)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  stat_summary(fun = mean, geom = "point", shape = 23,
               size = 3, fill = "white", colour = "black") +
  scale_fill_manual(values = c("No" = "#4CAF50", "Yes" = "#F44336")) +
  labs(
    title    = "Distribution of Monthly Income by Attrition Status",
    subtitle = "Diamond = Mean  |  Red dots = Outliers",
    x        = "Attrition",
    y        = "Monthly Income (USD)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# ============================================================
# DESCRIPTIVE ANALYSIS 1-2:
# Which monthly income bracket has the highest attrition rate?
# Low: < 3,000  |  Mid: 3,000 – 9,000  |  High: > 9,000
# ============================================================

# --- Create Income Bracket Column ---
# Missing MonthlyIncome values are kept as "Unknown" (treated as a separate
# category rather than deleted, to preserve all records in the analysis)
df <- df %>%
  mutate(IncomeBracket = case_when(
    is.na(MonthlyIncome)                                      ~ "Unknown",
    MonthlyIncome < 3000                                      ~ "Low (<3k)",
    MonthlyIncome >= 3000 & MonthlyIncome <= 9000             ~ "Mid (3k–9k)",
    MonthlyIncome > 9000                                      ~ "High (>9k)"
  ))

# Lock bracket order for plotting
df$IncomeBracket <- factor(df$IncomeBracket,
                           levels = c("Low (<3k)", "Mid (3k–9k)", "High (>9k)", "Unknown"))

# --- Attrition Rate per Bracket ---
bracket_summary <- df %>%
  group_by(IncomeBracket, Attrition) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(IncomeBracket) %>%
  mutate(
    Total          = sum(Count),
    AttritionRate  = round(Count / Total * 100, 1)
  ) %>%
  filter(Attrition == "Yes")

print(bracket_summary)

# --- Grouped Bar Chart: Attrition Count by Income Bracket ---
bracket_count <- df %>%
  group_by(IncomeBracket, Attrition) %>%
  summarise(Count = n(), .groups = "drop")

ggplot(bracket_count, aes(x = IncomeBracket, y = Count, fill = Attrition)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Count),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("No" = "#4CAF50", "Yes" = "#F44336")) +
  labs(
    title = "Employee Count by Income Bracket and Attrition Status",
    x     = "Monthly Income Bracket",
    y     = "Number of Employees",
    fill  = "Attrition"
  ) +
  theme_minimal(base_size = 13)

# --- Stacked Bar Chart: Attrition Rate (%) by Income Bracket ---
bracket_pct <- df %>%
  group_by(IncomeBracket, Attrition) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(IncomeBracket) %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 1))

ggplot(bracket_pct, aes(x = IncomeBracket, y = Percentage, fill = Attrition)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(Percentage, "%")),
            position = position_stack(vjust = 0.5), size = 4, colour = "white") +
  scale_fill_manual(values = c("No" = "#4CAF50", "Yes" = "#F44336")) +
  labs(
    title = "Attrition Rate (%) by Monthly Income Bracket",
    x     = "Monthly Income Bracket",
    y     = "Percentage (%)",
    fill  = "Attrition"
  ) +
  theme_minimal(base_size = 13)