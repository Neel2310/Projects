
library(dplyr)

student_df <- read.csv("C:\\Users\\Neel Joshi\\Desktop\\NMIMS\\SEM 2\\PS\\R program\\StudentsPerformance.csv")
View(student_df)
nrow(student_df)
ncol(student_df)
# student_df = na.omit(student_df)
sum(is.na(student_df))

student_df$gender_male = as.integer(student_df$gender == 'male')

#random sample of n = 20
random_sample <- student_df %>% sample_n(20)
population_mean <- mean(student_df$math.score)
population_mean

sample_mean <- mean(random_sample$math.score)
sample_mean

d = table(student_df$math.score)

barplot(d, main = "Maths score", xlab = "Math score", ylab = "No of Student")
t.test(random_sample$math.score, mu = mean(student_df$math.score))

p_value = 0.8333
alpha = 0.05

if(p_value<alpha){
    print("Reject Null Hypothesis. Hence There is statistical difference 
          between the population mean and sample mean for math.score variable.")
} else {
    print("Accept Null Hypothesis. Hence There is no statistical difference 
          between the population mean and sample mean for math.score variable.")
}

#sample of male students from student_df
male_students <- student_df %>% filter(gender == "male")

#sample of female student from student_df
female_students <- student_df %>% filter(gender == "female")
var(male_students$reading.score)
var(female_students$reading.score)
sd(male_students$reading.score)
sd(female_students$reading.score)

d1= c(mean(male_students$reading.score),mean(female_students$reading.score))
group= c("Male_Student", "Female_Students")
barplot(d1,names.arg= group, main="Reading Score", xlab= "Gender", ylab= "Mean reading", 
        col= c("sky Blue", "light Pink"))
t.test(reading.score ~ gender, data = student_df)
p_value = 4.376e-15
alpha = 0.05

if(p_value<alpha){
    print("Reject Null Hypothesis. Hence There is a difference 
          between the average reading score of male and female students.")
} else {
    print("Accept Null Hypothesis. Hence There is no a difference 
          between the average reading score of male and female students.")
}
