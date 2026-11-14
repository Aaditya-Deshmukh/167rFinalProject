# Load data
data <- read.csv("data.csv", sep = ";",
                 check.names = FALSE)

dim(data)
head(data)
table(data$Target)
sum(is.na(data))

# Remove enrolled students
students <- data[
  data$Target == "Graduate" |
    data$Target == "Dropout", ]

# Keep the columns we need for our question
students <- students[, c(
  "Age at enrollment",
  "Admission grade",
  "Previous qualification (grade)",
  "Curricular units 1st sem (grade)",
  "Gender",
  "Scholarship holder",
  "Debtor",
  "Tuition fees up to date",
  "Target"
)]

# Rename columns
names(students) <- c(
  "age", "admission_grade", "previous_grade",
  "first_sem_grade", "gender", "scholarship",
  "debtor", "tuition_current", "outcome"
)

# Make categories factors
students$gender <- factor(students$gender)
students$scholarship <- factor(students$scholarship)
students$debtor <- factor(students$debtor)
students$tuition_current <- factor(students$tuition_current)
students$outcome <- factor(students$outcome)

# 1 = dropout, 0 = graduate
students$dropout <- ifelse(
  students$outcome == "Dropout", 1, 0
)

# Check cleaned data
dim(students)
sum(is.na(students))
table(students$outcome)

# Get numeric columns
numeric_data <- students[, c(
  "age", "admission_grade",
  "previous_grade", "first_sem_grade"
)]

# Basic stats
summary(numeric_data)


# Compare dropout and graduate stats
for (variable in names(numeric_data)) {
  cat("\n", variable, "\n")
  
  for (group in c("Graduate", "Dropout")) {
    x <- students[
      students$outcome == group, variable
    ]
    
    cat("\n", group, "\n")
    cat("Mean:", mean(x), "\n")
    cat("Median:", median(x), "\n")
    cat("SD:", sd(x), "\n")
    cat("Variance:", var(x), "\n")
    cat("Min:", min(x), "\n")
    cat("Q1:", quantile(x, 0.25), "\n")
    cat("Q2:", quantile(x, 0.50), "\n")
    cat("Q3:", quantile(x, 0.75), "\n")
    cat("Max:", max(x), "\n")
    cat("IQR:", IQR(x), "\n")
  }
}

# Count categories
table(students$outcome)
table(students$gender, students$outcome)
table(students$scholarship, students$outcome)
table(students$debtor, students$outcome)
table(students$tuition_current, students$outcome)

# Save cleaned data
write.csv(students, "students_cleaned.csv",
          row.names = FALSE)
