# MATH 167R Group 8: Predicting Student Dropout and Academic Success
# Part 2: Data Visualization and Statistical Testing (Kevin Xu)
# input: data.csv (UCI dataset) in the same folder as this script
# output: plots saved to figures/, test results saved to results/

library(ggplot2)

# create output folders if they don't exist
dir.create("figures", showWarnings = FALSE)
dir.create("results", showWarnings = FALSE)


# Part 1: Load data

# read data, UCI file uses ";" as the separator
raw <- read.csv("data.csv", sep = ";", check.names = FALSE)

# remove extra spaces from column names (one column has a tab in its name)
names(raw) <- trimws(names(raw))

# only keep dropout and graduate students, enrolled students don't have a final outcome yet
raw <- raw[raw$Target %in% c("Dropout", "Graduate"), ]


# Part 2: Set up variables

# continuous variables, label shown on plots = original column name
contMap <- c(
  "Age at enrollment"       = "Age at enrollment",
  "Admission grade (0-200)" = "Admission grade",
  "1st-sem grade (0-20)"    = "Curricular units 1st sem (grade)",
  "2nd-sem grade (0-20)"    = "Curricular units 2nd sem (grade)",
  "1st-sem units passed"    = "Curricular units 1st sem (approved)",
  "2nd-sem units passed"    = "Curricular units 2nd sem (approved)",
  "Unemployment rate (%)"   = "Unemployment rate"
)

# only keep columns that exist in the file
contMap <- contMap[contMap %in% names(raw)]
contVars <- names(contMap)

# convert 0/1 codes to "No"/"Yes" labels
yesNo <- function(x) {
  return(factor(x, levels = c(0, 1), labels = c("No", "Yes")))
}

# init data frame with outcome
df <- data.frame(Outcome = factor(raw$Target, levels = c("Graduate", "Dropout")))

# add continuous variables
df[contVars] <- raw[contMap]

# add categorical variables with readable labels
df$Gender <- factor(raw$Gender, levels = c(0, 1), labels = c("Female", "Male"))
df$`Scholarship holder` <- yesNo(raw[["Scholarship holder"]])
df$Debtor <- yesNo(raw$Debtor)
df$`Tuition up to date` <- yesNo(raw[["Tuition fees up to date"]])
catVars <- c("Gender", "Scholarship holder", "Debtor", "Tuition up to date")

# course codes from UCI and their names (same order)
courseCodes <- c(33, 171, 8014, 9003, 9070, 9085, 9119, 9130, 9147, 9238,
                 9254, 9500, 9556, 9670, 9773, 9853, 9991)
courseNames <- c("Biofuel Production Tech.", "Animation & Multimedia Design",
                 "Social Service (evening)", "Agronomy", "Communication Design",
                 "Veterinary Nursing", "Informatics Engineering", "Equiniculture",
                 "Management", "Social Service", "Tourism", "Nursing",
                 "Oral Hygiene", "Advertising & Marketing Mgmt.",
                 "Journalism & Communication", "Basic Education",
                 "Management (evening)")

# replace each course code with its name
df$Course <- courseNames[match(raw$Course, courseCodes)]

# print number of students in each group
cat("Students analysed:", nrow(df), "\n")
print(table(df$Outcome))

# same colors for graduate and dropout in every plot
outcomeCols <- c(Graduate = "steelblue", Dropout = "firebrick")


# Part 3: Plots

# outcome counts (not in report, the 60.9% / 39.1% split is stated in the text)
pOutcome <- ggplot(df, aes(Outcome, fill = Outcome)) +
  geom_bar(width = 0.6) +
  geom_text(stat = "count",
            aes(label = sprintf("%d (%.1f%%)", after_stat(count),
                                100 * after_stat(count) / nrow(df))),
            vjust = -0.4) +
  scale_fill_manual(values = outcomeCols, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(title = "Student outcomes (Dropout vs Graduate)", x = NULL, y = "Number of students") +
  theme_minimal()
ggsave("figures/fig0_outcome_counts.png", pOutcome, width = 5, height = 4, dpi = 300)

# Figure 1: boxplots of each continuous variable by outcome

# reshape to long format so one ggplot call can draw every panel
longCont <- data.frame(
  Outcome  = rep(df$Outcome, times = length(contVars)),
  Variable = factor(rep(contVars, each = nrow(df)), levels = contVars),
  Value    = unlist(df[contVars], use.names = FALSE)
)

pBox <- ggplot(longCont, aes(Outcome, Value, fill = Outcome)) +
  geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.3) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 4) +
  scale_fill_manual(values = outcomeCols, guide = "none") +
  labs(title = "Figure 1. Continuous variables by outcome", x = NULL, y = NULL) +
  theme_minimal() + theme(strip.text = element_text(face = "bold"))
ggsave("figures/fig1_continuous_by_outcome.png", pBox, width = 10, height = 5.5, dpi = 300)

# Figure 2: dropout share for each categorical variable

# init empty vector for the level of each student
levelVals <- character(0)

# iterate through each categorical variable and add its levels to the vector
for (v in catVars) {
  levelVals <- c(levelVals, as.character(df[[v]]))
}

# reshape to long format
longCat <- data.frame(
  Outcome  = rep(df$Outcome, times = length(catVars)),
  Variable = factor(rep(catVars, each = nrow(df)), levels = catVars),
  Level    = levelVals
)

# count students in each group so we can show n above each bar
nLabels <- aggregate(Outcome ~ Variable + Level, data = longCat, FUN = length)
names(nLabels)[3] <- "n"

# 100% stacked bars so groups of different sizes can be compared
pCat <- ggplot(longCat, aes(Level, fill = Outcome)) +
  geom_bar(position = "fill", width = 0.7) +
  geom_text(data = nLabels, aes(Level, 1.05, label = paste0("n = ", n)),
            inherit.aes = FALSE, size = 3) +
  facet_wrap(~ Variable, scales = "free_x", ncol = 4) +
  scale_fill_manual(values = outcomeCols) +
  scale_y_continuous(labels = function(x) paste0(100 * x, "%"),
                     breaks = seq(0, 1, 0.25)) +
  labs(title = "Figure 2. Outcome proportions by categorical variable",
       x = NULL, y = "Share of students", fill = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
ggsave("figures/fig2_categorical_by_outcome.png", pCat, width = 9, height = 4.2, dpi = 300)

# Figure 3: dropout rate by course

# calc dropout rate and number of students for each course
courseTab <- aggregate(Outcome ~ Course, data = df,
                       FUN = function(x) c(rate = mean(x == "Dropout"), n = length(x)))
courseTab <- data.frame(Course = courseTab$Course,
                        rate = courseTab$Outcome[, "rate"],
                        n = courseTab$Outcome[, "n"])

# sort courses by dropout rate
courseTab$Course <- reorder(courseTab$Course, courseTab$rate)

# overall dropout rate for the dashed line
overallRate <- mean(df$Outcome == "Dropout")

pCourse <- ggplot(courseTab, aes(rate, Course)) +
  geom_col(fill = "firebrick", width = 0.7) +
  geom_vline(xintercept = overallRate, linetype = "dashed") +
  # white background on labels so the dashed line doesn't cover them
  geom_label(aes(label = sprintf("%.0f%%  (n = %d)", 100 * rate, n)),
             hjust = -0.05, size = 3, fill = "white", label.size = 0,
             label.padding = unit(0.1, "lines")) +
  annotate("text", x = overallRate, y = nrow(courseTab) + 0.9, hjust = -0.05,
           size = 3, label = sprintf("overall dropout rate %.0f%%", 100 * overallRate)) +
  scale_y_discrete(expand = expansion(add = c(0.6, 1.3))) +
  scale_x_continuous(labels = function(x) paste0(100 * x, "%"),
                     limits = c(0, 1.05), expand = c(0, 0)) +
  labs(title = "Figure 3. Dropout rate by course", x = "Dropout rate", y = NULL) +
  theme_minimal()
ggsave("figures/fig3_dropout_by_course.png", pCourse, width = 8, height = 5.5, dpi = 300)

# Spearman correlations between continuous variables, used since some variables are skewed
corMat <- cor(df[contVars], method = "spearman")


# Part 4: Hypothesis tests

# continuous variables: compare graduates vs dropouts
contTest <- function(v) {

  # split values by outcome
  g <- df[[v]][df$Outcome == "Graduate"]
  d <- df[[v]][df$Outcome == "Dropout"]

  # calc pooled standard deviation for Cohen's d
  pooledSd <- sqrt(((length(g) - 1) * var(g) + (length(d) - 1) * var(d)) /
                     (length(g) + length(d) - 2))

  # Shapiro-Wilk checks normality, it rejects so we use Wilcoxon instead of a t-test
  # Cohen's d is the effect size (0.2 small, 0.5 medium, 0.8 large)
  return(data.frame(
    Variable        = v,
    Median_Graduate = median(g),
    Median_Dropout  = median(d),
    Mean_Graduate   = mean(g),
    Mean_Dropout    = mean(d),
    Shapiro_p_max   = max(shapiro.test(g)$p.value, shapiro.test(d)$p.value),
    Wilcoxon_p      = wilcox.test(g, d, exact = FALSE)$p.value,
    Cohens_d        = (mean(g) - mean(d)) / pooledSd
  ))
}

# init empty results table
contResults <- NULL

# iterate through each continuous variable and add its test results to the table
for (v in contVars) {
  contResults <- rbind(contResults, contTest(v))
}

# categorical variables: chi-square test of independence with outcome
catTest <- function(v) {

  # contingency table of variable vs outcome
  tab <- table(df[[v]], df$Outcome)

  # run chi-square test, one small course has expected count < 5 so we hide the warning
  test <- suppressWarnings(chisq.test(tab))

  # dropout rate for each level
  rates <- prop.table(tab, 1)[, "Dropout"]

  # Cramer's V is the effect size (0 = no association, 1 = perfect association)
  return(data.frame(
    Variable     = v,
    Dropout_rate = paste(sprintf("%s %.1f%%", names(rates), 100 * rates),
                         collapse = " | "),
    Chi_sq       = unname(test$statistic),
    df           = unname(test$parameter),
    p_value      = test$p.value,
    Cramers_V    = sqrt(unname(test$statistic) / (sum(tab) * (min(dim(tab)) - 1))),
    Min_expected = min(test$expected)
  ))
}

# init empty results table
catResults <- NULL

# iterate through each categorical variable and course and add its test results
for (v in c(catVars, "Course")) {
  catResults <- rbind(catResults, catTest(v))
}

# course has 17 levels so point to Figure 3 instead of listing every rate
catResults$Dropout_rate[catResults$Variable == "Course"] <- "see Figure 3"

# Bonferroni correction since we run 12 tests at once
allP <- p.adjust(c(contResults$Wilcoxon_p, catResults$p_value), method = "bonferroni")
contResults$p_adj <- allP[seq_len(nrow(contResults))]
catResults$p_adj <- allP[-seq_len(nrow(contResults))]

# round numbers to 3 significant digits
roundTable <- function(x) {

  # iterate through each column
  for (col in names(x)) {

    # only round numeric columns
    if (is.numeric(x[[col]])) {
      x[[col]] <- signif(x[[col]], 3)
    }
  }

  # return rounded table
  return(x)
}
contResults <- roundTable(contResults)
catResults <- roundTable(catResults)

# save results
write.csv(contResults, "results/continuous_tests.csv", row.names = FALSE)
write.csv(catResults, "results/categorical_tests.csv", row.names = FALSE)
write.csv(round(corMat, 3), "results/spearman_correlations.csv")

# print results
cat("\n--- Continuous variables: Wilcoxon rank-sum (Graduate vs Dropout) ---\n")
print(contResults, row.names = FALSE)
cat("\n--- Categorical variables: chi-square test of independence ---\n")
print(catResults, row.names = FALSE)

# find pairs of continuous variables with |rho| > 0.7 (possible multicollinearity for the model)
high <- which(abs(corMat) > 0.7 & upper.tri(corMat), arr.ind = TRUE)

if (nrow(high) > 0) {
  cat("\nPairs with |rho| > 0.7 (possible multicollinearity in the model):\n")

  # iterate through each pair and print it
  for (i in seq_len(nrow(high))) {
    cat(sprintf("  %s ~ %s: %.2f\n", rownames(corMat)[high[i, 1]],
                colnames(corMat)[high[i, 2]], corMat[high[i, 1], high[i, 2]]))
  }
}

# note: 2nd semester variables are partly caused by dropping out, so 1st semester is a fairer predictor
