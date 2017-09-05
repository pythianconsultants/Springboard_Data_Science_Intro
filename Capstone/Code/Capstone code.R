library(tidyverse)

d_math <- read.csv2("student-mat.csv")
d_port <- read.csv2("student-por.csv")

# How many students are enrolled in both math and Portuguese? Merge based on attributes not associated with course.
d_both <- merge(d_math, d_port, by=c("school", "sex", "age", "address", 
                "famsize", "Pstatus", "Medu", "Fedu", "Mjob", "Fjob",
                "reason", "guardian", "traveltime", "activities", "nursery",
                "higher", "internet", "romantic", "famrel", "freetime", 
                "goout", "Dalc", "Walc", "health", "absences"))
print(nrow(d_both)) # There are 100 students enrolled in both courses

# Tidy up variable types, labels of variables
d_math$studytime <- factor(d_math$studytime, labels = c("<2 hrs", "2-5 hrs", "5-10 hrs", ">10 hrs"))
d_port$studytime <- factor(d_port$studytime, labels = c("<2 hrs", "2-5 hrs", "5-10 hrs", ">10 hrs"))
d_math$failures <- factor(d_math$failures, labels = c("0", "1", "2", "3"))
d_port$failures <- factor(d_port$failures, labels = c("0", "1", "2", "3"))
d_math$famsize <- factor(d_math$famsize, labels = c(">3", "<=3"))
d_port$famsize <- factor(d_port$famsize, labels = c(">3", "<=3"))
d_math$Medu <- factor(d_math$Medu, labels = c("None", "Primary", "Middle", "Secondary", "Higher"))
d_port$Medu <- factor(d_port$Medu, labels = c("None", "Primary", "Middle", "Secondary", "Higher"))
d_math$Fedu <- factor(d_math$Fedu, labels = c("None", "Primary", "Middle", "Secondary", "Higher"))
d_port$Fedu <- factor(d_port$Fedu, labels = c("None", "Primary", "Middle", "Secondary", "Higher"))
d_math$traveltime <- factor(d_math$traveltime, labels = c("<15 min", "15-30 min", "30-60 min", ">60 min"))
d_port$traveltime <- factor(d_port$traveltime, labels = c("<15 min", "15-30 min", "30-60 min", ">60 min"))
d_math$famrel <- factor(d_math$famrel, labels = c("Very Bad", "Poor", "Fair", "Good", "Excellent"))  
d_port$famrel <- factor(d_port$famrel, labels = c("Very Bad", "Poor", "Fair", "Good", "Excellent")) 
d_math$freetime <- factor(d_math$freetime, labels = c("Very Low", "Low", "Medium", "High", "Very High"))
d_port$freetime <- factor(d_port$freetime, labels = c("Very Low", "Low", "Medium", "High", "Very High"))
d_math$goout <- factor(d_math$goout, labels = c("Very Low", "Low", "Medium", "High", "Very High"))
d_port$goout <- factor(d_port$goout, labels = c("Very Low", "Low", "Medium", "High", "Very High"))
d_math$Dalc <- factor(d_math$Dalc, labels = c("Very Low", "Low", "Medium", "High", "Very High"))
d_port$Dalc <- factor(d_port$Dalc, labels = c("Very Low", "Low", "Medium", "High", "Very High"))
d_math$Walc <- factor(d_math$Walc, labels = c("Very Low", "Low", "Medium", "High", "Very High"))
d_port$Walc <- factor(d_port$Walc, labels = c("Very Low", "Low", "Medium", "High", "Very High"))
d_math$health <- factor(d_math$health, labels = c("Very Bad", "Poor", "Fair", "Good", "Excellent"))  
d_port$health <- factor(d_port$health, labels = c("Very Bad", "Poor", "Fair", "Good", "Excellent")) 

# Create complete dataset d_total: combine math and Portuguese datasets with "course" as variable

course <- rep("math", times = length(d_math$school))
d_math <- cbind(d_math, course)

course <- rep("port", times = length(d_port$school))
d_port <- cbind(d_port, course)

d_total <- rbind(d_math, d_port)
d_total$course <- factor(d_total$course, labels = c("Math", "Portuguese"))
View(d_total)

rm(d_both, course)

# EXPLORATORY DATA ANALYSIS

# Distribution of G3 grades
mean(d_total$G3, na.rm = TRUE) # ~11.34
median(d_total$G3, na.rm = TRUE) # 11

ggplot(d_total, aes(x = G3)) +
  geom_histogram(aes(y = ..density..), binwidth = 1) +
  stat_function(fun = dnorm,
                args = list(mean = mean(d_total$G3, na.rm = TRUE), sd = sd(d_total$G3, na.rm = TRUE)),
                lwd = 1,
                col = "red") +
  xlab("Final Grade")

# How do distributions of math and Portuguese grades compare?
ggplot(d_total, aes(x = G3)) +
  geom_histogram(aes(y = ..density..), binwidth = 1) +
  stat_function(fun = dnorm,
                args = list(mean = mean(d_total$G3, na.rm = TRUE), sd = sd(d_total$G3, na.rm = TRUE)),
                lwd = 1,
                col = "red") +
  xlab("Final Grade") +
  facet_grid(. ~ course) 

ggplot(d_total, aes(x = G3, y = ..density.., col = course)) +
  geom_freqpoly(binwidth = 1, position = "identity") +
  xlab("Final Grade")

ggplot(d_total, aes(x=course, y=G3,colour=course)) + 
  geom_boxplot(notch = T) +
  xlab("Math vs. Portuguese") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.1))

mean(d_math$G3, na.rm = TRUE) # ~10.42
median(d_math$G3, na.rm = TRUE) # 11
mean(d_port$G3, na.rm = TRUE) # ~11.91
median(d_port$G3, na.rm = TRUE) # 12

nrow(d_math[d_math$G3 < 2, ]) / nrow(d_math) # ~9.6% 
nrow(d_port[d_port$G3 < 2, ]) / nrow(d_port) # ~2.5% 

nrow(d_math[d_math$G3 >= 18, ]) / nrow(d_math) # ~4.6%
nrow(d_port[d_port$G3 >= 18, ]) / nrow(d_port) # ~2.6%

# Mean and median of math grades are lower, and math students failed at almost four times the rate of Portuguese students.
# However, math students received highest marks at nearly twice the rate that Portuguese students did.

t.test(G3 ~ course, data = d_total) # p-value = 2.215e-08, significant

# Who are the students with the absolute failing grades (G3 = 0 or 1)?
abs_fail <- d_total[d_total$G3 < 2, ]
print(nrow(abs_fail)) # There are 54 students in this group. I wonder what they have in common?

# Who are the students with the highest grades (G3 >= 18)?
highest_grades <- d_total[d_total$G3 >= 18, ]
View(highest_grades) # There are 35 students in this group.

# Dataset filtered by school
d_GP <- filter(d_total, school == "GP")
d_MS <- filter(d_total, school == "MS")

# Correlations between final grade and the attributes associated with course

# STUDYTIME: weekly study time within the course subject (math or Portuguese)
ggplot(d_total, aes(x=studytime, y=G3,colour=studytime)) + 
  geom_boxplot(notch = T) +
  xlab("Study Time Per Week") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.1))
# Highest median grades among students who study 5-10 hrs/week.

ggplot(d_total, aes(x=studytime, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) + 
  xlab("Study Time Per Week") + ylab("Final Grade")
# Fewer extremely low grades among students who study more; no Portuguese failures among those who study more.

summary(aov(G3 ~ studytime, data = d_total)) # p-value is 9.92e-07, significant.

# Is it different for math and Portuguese?
summary(aov(G3 ~ studytime, data = d_math)) # p-value = ~5.2%, not significant (barely)
summary(aov(G3 ~ studytime, data = d_port)) # p-value = 1.09e-10, significant
# Study time has a bigger impact on Portuguese grades than on math grades.

# FAILURES: number of past class failures within the course subject (math or Portuguese)
ggplot(d_total, aes(x=failures, y=G3,colour=failures)) + 
  geom_boxplot(notch = T) +
  xlab("# Courses Previously Failed") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.3))
# Higher median grades among students with 0 failures. No high grades among students with several failures.

# Previous failures appear rare. What % of students never previously failed?
nrow(d_total[d_total$failures == 0, ]) / nrow(d_total) # ~82.5% of students.

# A portion of students absolutely fail regardless of whether they've failed before, but this is much more likely for students who have previously failed.
nrow(abs_fail[abs_fail$failures == 0, ]) / nrow(d_total[d_total$failures == 0, ]) # ~2.8%
nrow(abs_fail[abs_fail$failures == 1, ]) / nrow(d_total[d_total$failures == 1, ]) # 15%
nrow(abs_fail[abs_fail$failures == 2, ]) / nrow(d_total[d_total$failures == 2, ]) # ~18.2%
nrow(abs_fail[abs_fail$failures == 3, ]) / nrow(d_total[d_total$failures == 3, ]) # 20%

summary(aov(G3 ~ failures, data = d_total)) # p-value < 2e-16, significant.

# Is there a difference between math and Portuguese?
summary(aov(G3 ~ failures, data = d_math)) # p-value = 1.47e-13
summary(aov(G3 ~ failures, data = d_port)) # p-value < 2e-16
# No, the relationship between failures and final grades is significant for both.

# SCHOOLSUP: extra educational support within the course subject (math or Portuguese)
ggplot(d_total, aes(x=schoolsup, y=G3,color=schoolsup)) + 
  geom_boxplot(notch = T) +
  xlab("Extra Educational Support") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.3))
# Students not getting support have higher median and mean grades.

t.test(G3 ~ schoolsup, data = d_total) # p-value = 0.0007223, significant.

# Is there a difference between math and Portuguese? 
ggplot(d_total, aes(x=schoolsup, y=G3, color=course)) +
  geom_boxplot(notch = T) +
  xlab("Extra Educational Support") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.3)) +
  facet_grid(. ~ course) 
t.test(G3 ~ schoolsup, data = d_math) # p-value = 0.01974, significant.
t.test(G3 ~ schoolsup, data = d_port) # p-value = 0.02675, significant.
# No, school support seems to have a similar impact on Portuguese and math students.

# FAMSUP: family educational support within the course subject (math or Portuguese) # REVISE TO STUDENT T TEST
ggplot(d_total, aes(x=famsup, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Family Educational Support") + ylab("Final Grade")
  
# No apparent correlations here, confirmed by t-test giving p-value = 67.3%.
t.test(G3 ~ famsup, data = d_total)

# PAID: extra paid classes within the course subject (math or Portuguese) 
ggplot(d_total, aes(x=paid, y=G3,color=paid)) + 
  geom_boxplot(notch = T) +
  xlab("Extra Paid Classes") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.3))
# Higher median grades among students who don't get support.

ggplot(d_total, aes(x=paid, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Extra Paid Classes") + ylab("Final Grade")
# A lot of math students pay for extra help! What percent? Compared to Portuguese students?
sum(d_math$paid == "yes") / nrow(d_math) # ~46%
sum(d_port$paid == "yes") / nrow(d_port) # ~6%

# Does paid help improve outcomes for math students?
nrow(d_math[d_math$G3 < 2 & d_math$paid == "yes", ]) / sum(d_math$paid == "yes") # ~4.4%
nrow(d_math[d_math$G3 < 2 & d_math$paid == "no", ]) / sum(d_math$paid == "no") # ~14%
# Yes, paid math help decreases the rate of math failure substantially.

# T-test analysis of impact of paid help shows it only helps for math.
t.test(G3 ~ paid, data = d_total) # p-value = 9.6%, not significant.
t.test(G3 ~ paid, data = d_math) # p-value = ~3.8%, significant.
t.test(G3 ~ paid, data = d_port) # p-value = ~12.3%, not significant.

# Correlations between final grade and the attributes NOT associated with course

# SCHOOL: student's school # REVISE TO STUDENT T TEST
ggplot(d_total, aes(x=school, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("School") + ylab("Final Grade")
# It looks like a lot more students fail math at GP than at MS
nrow(d_GP[d_GP$G3 < 2 & d_GP$course == "math", ]) / nrow(d_GP) # ~4.4%
nrow(d_MS[d_MS$G3 < 2 & d_MS$course == "math", ]) / nrow(d_MS) # ~1.5%
# GP has about 3 times the math failure rate of MS.
nrow(d_GP[d_GP$G3 < 2 & d_GP$course == "port", ]) / nrow(d_GP) # ~.26%
nrow(d_MS[d_MS$G3 < 2 & d_MS$course == "port", ]) / nrow(d_MS) # ~5.1%
# MS has a much higher rate of Portuguese failure than GP.

# Is there a significant relationship between school and final grade? Yes.
t.test(G3 ~ school, data = d_total) # p-value = 5.168e-05, significant

# SEX: student's sex 
ggplot(d_total, aes(x=sex, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Sex") + ylab("Final Grade")
# No apparent relationship, confirmed by t-test.
t.test(G3 ~ sex, data = d_total) # p-value = ~30.9%

# Do female and male students perform similarly in math and Portuguese classes?
ggplot(d_total, aes(x=sex, y=G3, col = sex)) +
  geom_point(position = "jitter", alpha = 0.6) +
  facet_grid(. ~ course) +
  xlab("Sex") + ylab("Final Grade")
t.test(G3 ~ sex, data = d_math) # p-value = 0.03958,  significant
t.test(G3 ~ sex, data = d_port) # p-value = 0.001125, significant
# I don't understand this outcome. If sex is not a significant contributor to final grade in the whole dataset, 
# how can it be a significant contributor to final grades in the datasets that make up the whole dataset?

ggplot(d_total, aes(x=sex, y=G3, color=sex)) +
  geom_boxplot(notch = T) +
  xlab("Sex") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.3)) +
  facet_grid(. ~ course) 
# It appears that female students do better in Portuguese and male students do better in math.
# Maybe the impact of sex on math grades "cancels out" the impact of sex on Portuguese grades in the total dataset?

# Compare percent of abs_fail students who are female to abs_fail students who are male.
nrow(abs_fail[abs_fail$sex == "F", ]) / nrow(abs_fail) # ~55.6%
nrow(abs_fail[abs_fail$sex == "M", ]) / nrow(abs_fail) # ~44.4%
# Compare percent of highest_grades students who are female to highest_grades students who are male.
nrow(highest_grades[highest_grades$sex == "F", ]) / nrow(highest_grades) # ~54.3.6%
nrow(highest_grades[highest_grades$sex == "M", ]) / nrow(highest_grades) # ~44.7%
# A higher percentage of females are among the most successful and also among the least successful students.
# Or, a higher percentage of male students receive middle grades.

# AGE: student's age
# How many students of each age?
table(d_total$age)

ggplot(d_total, aes(x=age, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Age") + ylab("Final Grade")
# No failures among students older than 19, though there aren't many of these. Is age significant? Yes.
summary(aov(G3 ~ age, data = d_total)) # p-value = 4.93e-05

# What age student is most likely to fail?
ggplot(abs_fail, aes(x = age)) +
  geom_histogram(aes(y = ..density.., fill = course), binwidth = 1, position = "dodge") +
  xlab("Age")
# 18-year-olds are most likely to fail both subjects.
# Not a single 15-year-old failed Portuguese.
# Failure rates in math seem less affected by age than failures rates in Portuguese.

# ADDRESS: student's home address type (rural or urban) # REVISE TO STUDENT T TEST
ggplot(d_total, aes(x=address, y=G3, color=address)) +
  geom_boxplot(notch = T) +
  xlab("Rural or Urban") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.3))
# Higher median grades among urban students.
t.test(G3 ~ address, data = d_total) # p-value = 0.0002071, significant

# Maybe a higher percentage of rural students fail?
nrow(d_total[d_total$address == "R" & d_total$G3 < 2, ]) / nrow(d_total[d_total$address == "R", ]) # ~7.0%
nrow(d_total[d_total$address == "U" & d_total$G3 < 2, ]) / nrow(d_total[d_total$address == "U", ]) # ~4.5%
# Yes, a higher percentage of rural students fail than urban students.
# Significant? No. P-value = 32.46%
t.test(G3 ~ address, data = abs_fail)
# Address is also not significant among students who receive the highest grades. P-value = 96.24%
t.test(G3 ~ address, data = highest_grades)

# FAMSIZE: family size
ggplot(d_total, aes(x=famsize, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Family Size") + ylab("Final Grade")
# No obvious relationship, but t-test shows significance.
t.test(G3 ~ famsize, data = d_total) # p-value = 0.03691, significant.

ggplot(d_total, aes(x=famsize, y=G3,color=famsize)) +
  geom_boxplot(notch = T) +
  xlab("Family Size") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.3))
# Higher median grades among students with smaller family size.

# PSTATUS: parents' cohabitation status 
ggplot(d_total, aes(x=Pstatus, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Parents' Cohabitation Status") + ylab("Final Grade")
# No obvious relationship, confirmed by t-test.
t.test(G3 ~ Pstatus, data = d_total) # p-value = 29.57%, not significant.

# MEDU: mother's education
ggplot(d_total, aes(x=Medu, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Mother's Education") + ylab("Final Grade")
# It appears students are more likely to be enrolled if their mother has a higher level of education.

# But is mother's education correlated with final grade? Yes!
summary(aov(G3 ~ Medu, data = d_total)) # p-value = 2.9e-10, significant.

# Maybe students whose mothers have less education are more likely to fail? Yes.
nrow(d_total[d_total$Medu %in% c("none", "primary", "upper primary") & d_total$G3 < 2, ]) / 
  nrow(d_total[d_total$Medu %in% c("none", "primary", "upper primary"), ]) # 6.6%
nrow(d_total[d_total$Medu %in% c("secondary", "higher") & d_total$G3 < 2, ]) / 
  nrow(d_total[d_total$Medu %in% c("secondary", "higher"), ]) # ~3.9%

ggplot(d_total, aes(x=Medu, y=G3,color=Medu)) +
  geom_boxplot(notch = F) +
  xlab("Mother's Education") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.1))
# Impact of mother's education on median grade shows here.

# Does mother's level of education make more of a difference to female students?
ggplot(d_total, aes(x=Medu, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  facet_grid(. ~ sex) +
  xlab("Mother's Education") + ylab("Final Grade")
# It appears that more female students whose mothers have little education are enrolled.
nrow(d_total[d_total$Medu %in% c("none", "primary", "upper primary") & d_total$sex == "F", ]) / nrow(d_total[d_total$Medu %in% c("none", "primary", "upper primary"), ])
# Yes, 62% of students whose mothers have little education are female.

# But does mother's level of education make more of a difference to female students' grades?
summary(aov(G3 ~ Medu, data = d_total[d_total$sex == "F", ])) # p-value = 2.5e-05
summary(aov(G3 ~ Medu, data = d_total[d_total$sex == "M", ])) # p-value = 1.92e-05
# No, it is significant for both male and female students' grades.

# FEDU: father's education
ggplot(d_total, aes(x=Fedu, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Father's Education") + ylab("Final Grade")
# No obvious relationship, but one-way ANOVA test shows there is one.
summary(aov(G3 ~ Fedu, data = d_total)) # p-value = 2.67e-06

ggplot(d_total, aes(x=Fedu, y=G3,color=Fedu)) +
  geom_boxplot(notch = F) +
  xlab("Father's Education") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.1))
# As with Medu, students whose fathers have higher education levels, on average, get higher grades.

# Interesting that for both Medu and Fedu, students whose parents have no education have higher median grades
# than students whose parents have a primary education. Maybe these are especially motivated students.

# MJOB: Mother's job
ggplot(d_total, aes(x=Mjob, y=G3,color=Mjob)) +
  geom_boxplot(notch = T) +
  xlab("Mother's Job") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.1))
# Students whose mothers work as teachers or in health careers have higher average grades.

summary(aov(G3 ~ Mjob, data = d_total)) # p-value = 2.92e-06, signficant

# Seems to make sense that impact of Mjob correlates with impact of Medu...

# FJOB: Father's job
ggplot(d_total, aes(x=Fjob, y=G3,color=Fjob)) +
  geom_boxplot(notch = T) +
  xlab("Father's Job") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.1))
# Except for students whose fathers are teachers, there is not as apparent an impact on average grades as for Mjob.

summary(aov(G3 ~ Fjob, data = d_total)) # p-value = ~0.90%, significant

# REASON: Reason for choosing this school
ggplot(d_total, aes(x=reason, y=G3,color=reason)) +
  geom_boxplot(notch = T) +
  xlab("Reason for Choosing School") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.1))
# Some different in median grades based on this variable.

summary(aov(G3 ~ reason, data = d_total)) # p-value = ~0.05%, significant

# GUARDIAN: Student's guardian
ggplot(d_total, aes(x=guardian, y=G3,color=guardian)) +
  geom_boxplot(notch = T) +
  xlab("Guardian") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.2))
# Students' average grades are higher when their guardian is their father.

summary(aov(G3 ~ guardian, data = d_total)) # p-value = ~1.10%, significant

# TRAVELTIME: Home to school travel time
ggplot(d_total, aes(x=traveltime, y=G3,color=traveltime)) +
  geom_boxplot(notch = F) +
  xlab("Travel Time to School") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.2))
# Higher median grades among students who travel <15 minutes to get to school.

summary(aov(G3 ~ traveltime, data = d_total)) # p-value = ~1.10%, significant

# ACTIVITIES: Extra-curricular activities # REVISE TO STUDENT T TEST
ggplot(d_total, aes(x=activities, y=G3,color=activities)) +
  geom_boxplot(notch = T) +
  xlab("Extra Curricular Activities") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.3))
# Higher median grades among students who do extra-curricular activities.

t.test(G3 ~ activities, data = d_total) # p-value = 27.26%, not significant

# NURSERY: Attended nursery school
ggplot(d_total, aes(x=nursery, y=G3,color=nursery)) +
  geom_boxplot(notch = T) +
  xlab("Attended Nursery School") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.3))
# Higher median grades among students who attended nursery school.

t.test(G3 ~ nursery, data = d_total) # p-value = 19.02%, not significant

# HIGHER: Wants to pursue higher education 
ggplot(d_total, aes(x=higher, y=G3,color=higher)) +
  geom_boxplot(notch = T) +
  xlab("Wants to Pursue Higher Ed") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.3))
# Significantly higher median grades among students who want to pursue higher education.

t.test(G3 ~ higher, data = d_total) # p-value = 3.68e-13, significant

# INTERNET: Internet access at home 
ggplot(d_total, aes(x=internet, y=G3,color=internet)) +
  geom_boxplot(notch = T) +
  xlab("Internet Access at Home") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.3))
# Higher median grades among students who have internet access.

t.test(G3 ~ internet, data = d_total) # p-value = 0.0005902, significant

# ROMANTIC: In a romantic relationship 
ggplot(d_total, aes(x=romantic, y=G3,color=romantic)) +
  geom_boxplot(notch = T) +
  xlab("In a Romantic Relationship") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.3))
# Higher median grades among students who are not in a romantic relationship.

t.test(G3 ~ romantic, data = d_total) # p-value = 0.002203, significant

# FAMREL: Quality of family relationships
ggplot(d_total, aes(x=famrel, y=G3,color=famrel)) +
  geom_boxplot(notch = T) +
  xlab("Quality of Family Relationships") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.1))
# Better family relationships correlated with higher median grades.

summary(aov(G3 ~ famrel, data = d_total)) # p-value = ~7.9%, not significant

# FREETIME: Free time after school
ggplot(d_total, aes(x=freetime, y=G3,color=freetime)) +
  geom_boxplot(notch = T) +
  xlab("Free Time After School") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.1))
# Highest median grades among students with "low" amount of free time after school.

summary(aov(G3 ~ freetime, data = d_total)) # p-value = 3.61%, significant

# GOOUT: Going out with friends
ggplot(d_total, aes(x=goout, y=G3,color=goout)) +
  geom_boxplot(notch = T) +
  xlab("Going Out with Friends") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.1))
# Higher median grades among students with "low" and "medium" time to go out with friends.

summary(aov(G3 ~ goout, data = d_total)) # p-value = ~0.15%, significant

# DALC: Workday alcohol consumption
ggplot(d_total, aes(x=Dalc, y=G3,color=Dalc)) +
  geom_boxplot(notch = F) +
  xlab("Workday Alcohol Consumption") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.1))
# Highest average grades among students with lowest workday alcohol consumption.

summary(aov(G3 ~ Dalc, data = d_total)) # p-value = 2.65e-05, significant

# WALC: Weekend alcohol consumption
ggplot(d_total, aes(x=Walc, y=G3,color=Walc)) +
  geom_boxplot(notch = T) +
  xlab("Weekend Alcohol Consumption") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.1))
# Lower average grades among students with higher weekend alcohol consumption.

summary(aov(G3 ~ Walc, data = d_total)) # p-value = ~0.02%, significant

# HEALTH: Current health status
ggplot(d_total, aes(x=health, y=G3,color=health)) +
  geom_boxplot(notch = T) +
  xlab("Current Health Status") + ylab("Final Grade")+
  geom_jitter(shape=16, position=position_jitter(0.1))
# Unclear impact?

summary(aov(G3 ~ health, data = d_total)) # p-value = ~0.97%, significant

# ABSENCES: Number of school absences
ggplot(d_total, aes(x=absences, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Number of School Absences") + ylab("Final Grade")

summary(aov(G3 ~ absences, data = d_total)) # p-value = 14%, not significant
# Is this a proper use of the ANOVA test?

nrow(d_total[d_total$absences == 0 & d_total$G3 < 2, ]) # 54
# All of the students who absolutely failed had 0 absences. Surprising!
# And the (few) students with very high numbers of absences got near-average grades.


