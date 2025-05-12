library(readr)
student_depression_dataset <- read_csv("C:/Users/a12u/Downloads/archive (1)/student_depression_dataset.csv")
View(student_depression_dataset)

class12 <- subset(student_depression_dataset, Degree == "'Class 12'")

# Basic summary

summary(class12$Age)
summary(class12$`Academic Pressure`)
summary(class12$`Work Pressure`)
summary(class12$CGPA)
summary(class12$`Study Satisfaction`)
summary(class12$`Sleep Duration`)
summary(class12$`Work/Study Hours`)
summary(class12$`Financial Stress`)

#Depression 
depress <- table(class12$Depression)
depresslable = c("No","Yes")
barplot(depress,beside = TRUE, col = c("green", "red" ),main = "Depression", ylab = "Count", names.arg = depresslable)

# Gender vs Depression
gender_depression <- table(class12$Gender, class12$Depression)
y=c("No Depression", "Depression")

# For Males
male_data <- gender_depression["Male", ]
male_labels <- paste(y, round(100 * male_data/sum(male_data), 1), "%")
pie(male_data, 
    labels = male_labels, 
    col = c("lightblue", "lightsalmon"),
    main = "Depression among Male Students")

# For Females
female_data <- gender_depression["Female", ]
female_labels <- paste(y, round(100 * female_data/sum(female_data), 1), "%")
pie(female_data, 
    labels = female_labels, 
    col = c("pink", "lightgreen"),
    main = "Depression among Female Students")


# Academic pressure  vs Depression
academic_depression <- table(class12$`Academic Pressure`, class12$Depression)

barplot(academic_depression, beside = TRUE, 
        col = c("green", "red","blue","yellow","pink","black"), 
        main = "Depression by Academic Pressure", 
        xlab = "Academic Pressure Level", 
        ylab = "Number of Students", 
        names.arg = y,
        legend = rownames(academic_depression),
        args.legend = list(title = "Academic Pressure", x = "topleft"))


# CGPA vs Depression
boxplot(CGPA ~ Depression, data = class12, col = c("yellow", "pink"),
        main = "CGPA vs Depression", xlab = "Depression (0 = No, 1 = Yes)", ylab = "CGPA")

boxplot_stats <- by(class12$CGPA, class12$Depression, summary)
boxplot_stats

# Study Satisfaction vs Depression
study_sat_depression <- table(class12$'Study Satisfaction', class12$Depression)

barplot(study_sat_depression, beside = TRUE, 
        col = c("green", "red", "blue", "yellow", "pink", "black"),
        main = "Depression by Study Satisfaction", 
        ylab = "Number of Students",
        names.arg = c("No Depression", "Depression"),
        legend.text = rownames(study_sat_depression),
        args.legend = list(title = "Study Satisfaction", x = "topleft"))


# Sleep Duration vs Depression
sleep_depression <- table(class12$`Sleep Duration`, class12$Depression)

barplot(sleep_depression, beside = TRUE, 
        col = c("green", "red","blue","yellow"), 
        main = "Depression by sleep duration Pressure", 
        ylab = "Number of Students", 
        names.arg = y,
        legend = rownames(sleep_depression),
        args.legend = list(title = "Sleep duration", x = "topleft"))

# Dietary Habits vs Depression
diet_depression <- table(class12$'Dietary Habits', class12$Depression)
barplot(diet_depression, 
        beside = TRUE, 
        col = c("green", "red","blue","yellow"), 
        main = "Depression by Dietary Habits", 
        ylab = "Count", 
        names.arg = y,
        legend.text = rownames(diet_depression),
        args.legend = list(title = "Dietary Habits", x = "topleft"))


# Suicidal Thoughts vs Depression
suicide_depression <- table(class12$'Have you ever had suicidal thoughts ?', class12$Depression)
barplot(suicide_depression, 
        beside = TRUE, 
        col = c("lavender", "red"), 
        main = "Depression by Suicidal Thoughts", 
        ylab = "Count", 
        names.arg = y,
        legend.text = rownames(suicide_depression),
        args.legend = list(title = "Suicidal Thoughts", x = "topleft"))


# Study Hours vs Depression
boxplot(class12$`Work/Study Hours` ~ class12$Depression, 
        data = class12, 
        col = c("lightsteelblue", "lightsalmon"),
        main = "Study Hours vs Depression", 
        xlab = "Depression Status", 
        ylab = "Study Hours",
        names = c("No Depression", "Depression"))

by(class12$`Work/Study Hours`, class12$Depression, summary)


# Financial Stress vs Depression
fin_stress_depression <- table(class12$'Financial Stress', class12$Depression)
barplot(fin_stress_depression, 
        beside = TRUE, 
        col = c("cyan", "pink", "red", "green", "blue"),
        main = "Depression by Financial Stress", 
        ylab = "Count", 
        names.arg = y,
        legend.text = rownames(fin_stress_depression),
        args.legend = list(title = "Financial Stress Level", x = "topleft"))


# Family History vs Depression
family_history_depression <- table(class12$'Family History of Mental Illness', class12$Depression)
barplot(family_history_depression, 
        beside = TRUE, 
        col = c("tan", "purple"),
        main = "Depression by Family History", 
        ylab = "Count", 
        names.arg = y,
        legend.text = rownames(family_history_depression),
        args.legend = list(title = "Family History", x = "topleft"))




