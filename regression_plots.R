# MATH 167R Group 8
# Part 3: Logistics Regression and Model Evaluation + ANOVA (Alex Giang)

# Binary Logistic Regression: Graduates (0), Student Dropouts (1)


# install.packages("ggplot2", "car")

library(ggplot2)
library(car)

cleaned_data <- read.csv("students_cleaned.csv") # Stores the cleaned data set
save(cleaned_data, file = "students_cleaned.RData") # Converts the cleaned data set (csv) into a .RData file
load("students_cleaned.RData")  # creates the object "students" from the cleaned dataset .RData file
str(students) # Shows the data frame of "students", data type, values

model <- glm( # Creates a generalized linear model
  dropout ~ age + admission_grade + previous_grade + first_sem_grade + # binary output (graduates (0), dropouts (1)), predicted by multiple numeric values
    gender + scholarship + debtor + tuition_current,
  data   = students, # the data set/data frame containing the variables/values
  family = binomial(link = "logit") # setting the binomial model with logit link for logistic regression
)
summary(model) # outputs the coefficients, std errors, z-values, p-values, degrees of freedom

students$pred_prob <- predict(model, type = "response") # add an column with of each student's predicted probability of dropout

# Predicted probabilities by actual outcome
students$actual <- factor(students$dropout, levels = c(0, 1), # Converts the "dropout" column in the students data set into labeled factors
                          labels = c("Actual Graduates", "Actual Dropouts"))
plt1 <- ggplot(students, aes(pred_prob, fill = actual)) + # creates the ggplot, with the x-axis being the predicted probability and bar fill color to represent the actual outcomes
  geom_histogram(breaks = seq(0, 1, by = 0.05),       # 20 bins of width 0.05
                 colour = "white", alpha = 0.6, # white borders for each bar to distinguish better
                 position = "identity") + # overlays the two value groups 
  scale_fill_manual(values = c("Actual Graduates" = "steelblue", # sets blue for Graduates values
                               "Actual Dropouts"  = "firebrick"), # sets red color for Dropouts values
                    name = NULL) + # "name = NULL" keeps the legend title to be empty
  scale_x_continuous(breaks = seq(0, 1, by = 0.2), expand = c(0.02, 0)) +
  labs(title = "Predicted Probabilities by Actual Outcome", # plot title, x axis, and y axis
       x = "Predicted Probability of Dropout",
       y = "Number of Students") +
  theme_classic() +
  theme(
    panel.border      = element_rect(colour = "black", fill = NA),  # plot with a border box 
    axis.line         = element_blank(), # removes the axis line due to having a border
    plot.title        = element_text(face = "bold", hjust = 0.5), # plot title is bold and centered
    legend.position   = c(0.5, 0.92),                               # top centre, inside plot
    legend.background = element_blank()  # removes the background of the legend box
  )

print(plt1) # shows the plot "plt1"

# Two-way ANOVA: Grade Gap between Graduates and Dropouts differ by scholarship status

grade_aov <- aov(first_sem_grade ~ scholarship * outcome, data = students) # Two-way ANOVA: first-sem grade by scholarship, outcome
summary(grade_aov) # outputs the ANOVA table with F-values and p-values

