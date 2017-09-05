library(ggplot2)
library(dplyr)

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
d_math$Medu <- factor(d_math$Medu, labels = c("none", "primary", "upper primary", "secondary", "higher"))
d_port$Medu <- factor(d_port$Medu, labels = c("none", "primary", "upper primary", "secondary", "higher"))
d_math$Fedu <- factor(d_math$Fedu, labels = c("none", "primary", "upper primary", "secondary", "higher"))
d_port$Fedu <- factor(d_port$Fedu, labels = c("none", "primary", "upper primary", "secondary", "higher"))
d_math$traveltime <- factor(d_math$traveltime, labels = c("<15 min", "15-30 min", "30 min-1 hr", ">1 hr"))
d_port$traveltime <- factor(d_port$traveltime, labels = c("<15 min", "15-30 min", "30 min-1 hr", ">1 hr"))
  
# Create complete dataset d_total: combine math and Portuguese datasets with "course" as variable

course <- rep("math", times = length(d_math$school))
d_math <- cbind(d_math, course)

course <- rep("port", times = length(d_port$school))
d_port <- cbind(d_port, course)

d_total <- rbind(d_math, d_port)
View(d_total)

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

mean(d_math$G3, na.rm = TRUE) # ~10.42
median(d_math$G3, na.rm = TRUE) # 11
mean(d_port$G3, na.rm = TRUE) # ~11.91
median(d_port$G3, na.rm = TRUE) # 12

nrow(d_math[d_math$G3 < 2, ]) / nrow(d_math) # ~9.6% 
nrow(d_port[d_port$G3 < 2, ]) / nrow(d_port) # ~2.5% 

nrow(d_math[d_math$G3 >= 18, ]) / nrow(d_math) # ~4.6%
nrow(d_math[d_port$G3 >= 18, ]) / nrow(d_port) # ~2.6%
# Mean and median of math grades are lower, and math students failed at almost four times the rate of Portuguese students.
# However, math students received highest marks at nearly twice the rate that Portuguese students did.

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
ggplot(d_total, aes(x=studytime, y=G3)) +
  geom_boxplot(fill = "grey80", color = "blue") +
  xlab("Study Time Per Week") + ylab("Final Grade")
# No obvious correlation here
ggplot(d_total, aes(x=studytime, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) + 
  xlab("Study Time Per Week") + ylab("Final Grade")
# There do appear to be fewer extremely low grades among students who study more. 

# Significant? Try one-way ANOVA test.
summary(aov(G3 ~ studytime, data = d_total)) # p-value is 9.92e-07, significant.

# Is it different for math and Portuguese?
summary(aov(G3 ~ studytime, data = d_math)) # p-value = ~5.2%, not significant (barely)
summary(aov(G3 ~ studytime, data = d_port)) # p-value = 1.09e-10, significant
# Interesting that study time has a  bigger impact on Portuguese grades than on math grades!

# How about among the students who earn the highest grades?
summary(aov(G3 ~ studytime, data = highest_grades)) # p-value = ~40.3%, not significant.
# This doesn't make intuitive sense to me...
ggplot(highest_grades, aes(x=studytime, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Study Time Per Week") + ylab("Final Grade")
# I see! When I look at the scatterplot of just the highest grades group, I can see no apparent correlation between studytime and grades.
# Which makes sense because the range of grades in this group is (by definition) very small.
# Among the students with the highest grades, some study a lot and some don't.
# However, among students as a whole, those who study a lot tend to get higher grades. One-way ANOVA tests now make sense to me.

# FAILURES: number of past class failures within the course subject (math or Portuguese)
ggplot(d_total, aes(x=failures, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) + 
  xlab("# Courses Previously Failed") + ylab("Final Grade")
# There are no high grades among students with several failures.

# A portion of students absolutely fail regardless of whether they've failed before.
nrow(abs_fail[abs_fail$failures == 0, ]) / nrow(d_total[d_total$failures == 0, ]) # ~2.8%
nrow(abs_fail[abs_fail$failures == 1, ]) / nrow(d_total[d_total$failures == 1, ]) # 15%
nrow(abs_fail[abs_fail$failures == 2, ]) / nrow(d_total[d_total$failures == 2, ]) # ~18.2%
nrow(abs_fail[abs_fail$failures == 3, ]) / nrow(d_total[d_total$failures == 3, ]) # 20%
# Students who have failed one or more times previously are much more likely to absolutely fail then those who haven't.
summary(aov(G3 ~ failures, data = d_total)) # p-value < 2e-16, significant.

# Is there a difference between math and Portuguese?
summary(aov(G3 ~ failures, data = d_math)) # p-value = 1.47e-13
summary(aov(G3 ~ failures, data = d_port)) # p-value < 2e-16
# No, the relationship between failures and final grades is significant for both.

# SCHOOLSUP: extra educational support within the course subject (math or Portuguese)
ggplot(d_total, aes(x=schoolsup, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Extra Educational Support") + ylab("Final Grade")
# It looks like students receiving school support get fewer high grades and fewer fails.

summary(aov(G3 ~ schoolsup, data = d_total)) # p-value = ~1.0%, significant.

# Is there a difference between math and Portuguese?
summary(aov(G3 ~ schoolsup, data = d_math)) # p-value = 10%, not significant.
summary(aov(G3 ~ schoolsup, data = d_port)) # p-value = 9.1%, not significant.
# I'm confused. How can the p-value of d_total be lower than the p-values of the two datasets that make it up?

# FAMSUP: family educational support within the course subject (math or Portuguese)
ggplot(d_total, aes(x=famsup, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Family Educational Support") + ylab("Final Grade")
  
# No apparent correlations here, confirmed by one-way ANOVA test giving p-value = 66.7%.
summary(aov(G3 ~ famsup, data = d_total))

# PAID: extra paid classes within the course subject (math or Portuguese)
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

# One-way ANOVA analysis of impact of paid help shows it only helps for math.
summary(aov(G3 ~ paid, data = d_total)) # p-value = 11.1%, not significant.
summary(aov(G3 ~ paid, data = d_math)) # p-value = ~4.3%, significant.
summary(aov(G3 ~ paid, data = d_port)) # p-value = 16.2%, not significant.

# Correlations between final grade and the attributes NOT associated with course

# SCHOOL: student's school
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
summary(aov(G3 ~ school, data = d_total)) # p-value = 3.81e-05, significant

# SEX: student's sex
ggplot(d_total, aes(x=sex, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Sex") + ylab("Final Grade")
# No apparent relationship, confirmed by one-way ANOVA test.
summary(aov(G3 ~ sex, data = d_total)) # p-value = 31%

# Do female and male students perform similarly in math and Portuguese classes?
ggplot(d_total, aes(x=sex, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  facet_grid(. ~ course) +
  xlab("Sex") + ylab("Final Grade")
summary(aov(G3 ~ sex, data = d_math)) # p-value = ~4.0%,  significant
summary(aov(G3 ~ sex, data = d_port)) # p-value = ~.10%, significant
# I don't understand this outcome. If sex is not a significant contributor to final grade in the whole dataset, 
# how can it be a significant contributor to final grades in the datasets that make up the whole dataset?

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

ggplot(d_total, aes(x = age)) +
  geom_histogram(aes(y = ..count..), binwidth = .6) +
  xlab("Age")

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

# ADDRESS: student's home address type (rural or urban)
ggplot(d_total, aes(x=address, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Rural or Urban") + ylab("Final Grade")
# No obvious relationship, but one-way ANOVA test shows there is one.
summary(aov(G3 ~ address, data = d_total)) # p-value = ~.01%, significant

# Maybe a higher percentage of rural students fail?
nrow(d_total[d_total$address == "R" & d_total$G3 < 2, ]) / nrow(d_total[d_total$address == "R", ]) # ~7.0%
nrow(d_total[d_total$address == "U" & d_total$G3 < 2, ]) / nrow(d_total[d_total$address == "U", ]) # ~4.5%
# Yes, a higher percentage of rural students fail than urban students.
# Significant? No. P-value = 44.8%
summary(aov(G3 ~ address, data = abs_fail))
# Address is also not significant among students who receive the highest grades. P-value = 96.5%
summary(aov(G3 ~ address, data = highest_grades))

ggplot(d_total, aes(x=address, y=G3)) +
  geom_boxplot(fill = "grey80", color = "blue") +
  xlab("Rural or Urban") + ylab("Final Grade")
# Nothing obvious stands out in a boxplot, either.

# I do not know how to understand the signficance of address.

# FAMSIZE: family size
ggplot(d_total, aes(x=famsize, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Family Size") + ylab("Final Grade")
# No obvious relationship, but one-way ANOVA test shows significance.
summary(aov(G3 ~ famsize, data = d_total)) # p-value = 4.8%, significant.

ggplot(d_total, aes(x=famsize, y=G3)) +
  geom_boxplot(fill = "grey80", color = "blue") +
  xlab("Family Size") + ylab("Final Grade")

# I do not know how to understand the significance of family size.

# PSTATUS: parents' cohabitation status
ggplot(d_total, aes(x=Pstatus, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Parents' Cohabitation Status") + ylab("Final Grade")
# No obvious relationship, confirmed by one-way ANOVA test.
summary(aov(G3 ~ Pstatus, data = d_total)) # p-value = 32.2%, not significant.

# MEDU: mother's education
ggplot(d_total, aes(x=Medu, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Mother's Education") + ylab("Final Grade")
# It appears students are more likely to be enrolled if their mother has a higher level of education.

# But is mother's education correlated with final grade? Yes!
summary(aov(G3 ~ Medu, data = d_total)) # p-value = 2.9e-10, significant.

# Maybe students whose mothers have less education are more likely to fail? Yes.
nrow(d_total[d_total$Medu %in% c("none", "primary", "upper primary") & d_total$G3 < 2, ]) / nrow(d_total[d_total$Medu %in% c("none", "primary", "upper primary"), ]) # 6.6%
nrow(d_total[d_total$Medu %in% c("secondary", "higher") & d_total$G3 < 2, ]) / nrow(d_total[d_total$Medu %in% c("secondary", "higher"), ]) # ~3.9%

# Does mother's level of education make more of a difference to female students?
ggplot(d_total, aes(x=Medu, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  facet_grid(. ~ sex) +
  xlab("Mother's Education") + ylab("Final Grade")
# It appears that more female students whose mothers have little education are enrolled.
nrow(d_total[d_total$Medu %in% c("none", "primary", "upper primary") & d_total$sex == "F", ]) / nrow(d_total[d_total$Medu %in% c("none", "primary", "upper primary"), ])
# Yes, 62% of students whose mothers have little education are female.

ggplot(d_total, aes(x=Medu, y=G3)) +
  geom_boxplot(fill = "grey80", color = "blue") +
  xlab("Mother's Education") + ylab("Final Grade")

# FEDU: father's education
ggplot(d_total, aes(x=Fedu, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Father's Education") + ylab("Final Grade")
# No obvious relationship, but one-way ANOVA test shows there is one.
summary(aov(G3 ~ Fedu, data = d_total)) # p-value = 2.67e-06
# Not sure how to explain this statistical result.

# MJOB: Mother's job
ggplot(d_total, aes(x=Mjob, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Mother's Job") + ylab("Final Grade")

summary(aov(G3 ~ Mjob, data = d_total)) # p-value = 2.92e-06, signficant

# FJOB: Father's job
ggplot(d_total, aes(x=Fjob, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Father's Job") + ylab("Final Grade")

summary(aov(G3 ~ Fjob, data = d_total)) # p-value = ~0.90%, significant

# REASON: Reason for choosing this school
ggplot(d_total, aes(x=reason, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Reason") + ylab("Final Grade")

summary(aov(G3 ~ reason, data = d_total)) # p-value = ~0.05%, significant

# GUARDIAN: Student's guardian
ggplot(d_total, aes(x=guardian, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Guardian") + ylab("Final Grade")

summary(aov(G3 ~ guardian, data = d_total)) # p-value = ~1.10%, significant

# TRAVELTIME: Home to school travel time
ggplot(d_total, aes(x=traveltime, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Travel Time to School") + ylab("Final Grade")

summary(aov(G3 ~ traveltime, data = d_total)) # p-value = ~1.10%, significant

# ACTIVITIES: Extra-curricular activities
ggplot(d_total, aes(x=activities, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Activities") + ylab("Final Grade")

summary(aov(G3 ~ activities, data = d_total)) # p-value = 27.2%, not significant

# NURSERY: Attended nursery school
ggplot(d_total, aes(x=nursery, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Attended Nursery School") + ylab("Final Grade")

summary(aov(G3 ~ nursery, data = d_total)) # p-value = 19.7%, not significant

# HIGHER: Wants to pursue higher education
ggplot(d_total, aes(x=higher, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Wants to Pursue Higher Ed") + ylab("Final Grade")

summary(aov(G3 ~ higher, data = d_total)) # p-value = 9.55e-15, significant

# INTERNET: Internet access at home
ggplot(d_total, aes(x=internet, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Has Internet at Home") + ylab("Final Grade")

summary(aov(G3 ~ internet, data = d_total)) # p-value = 0.053%, significant

# ROMANTIC: In a romantic relationship
ggplot(d_total, aes(x=romantic, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("In a Romantic Relationship") + ylab("Final Grade")

summary(aov(G3 ~ romantic, data = d_total)) # p-value = ~0.15%, significant

# FAMREL: Quality of family relationships
ggplot(d_total, aes(x=famrel, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Quality of family relationships") + ylab("Final Grade")

summary(aov(G3 ~ famrel, data = d_total)) # p-value = ~7.9%, not significant

# FREETIME: Free time after school
ggplot(d_total, aes(x=freetime, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Free Time After School") + ylab("Final Grade")

summary(aov(G3 ~ freetime, data = d_total)) # p-value = 3.61%, significant

# GOOUT: Going out with friends
ggplot(d_total, aes(x=goout, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Going Out with Friends") + ylab("Final Grade")

summary(aov(G3 ~ goout, data = d_total)) # p-value = ~0.15%, significant

# DALC: Workday alcohol consumption
ggplot(d_total, aes(x=Dalc, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Workday Alcohol Consumption") + ylab("Final Grade")

summary(aov(G3 ~ Dalc, data = d_total)) # p-value = 2.65e-05, significant

# WALC: Weekend alcohol consumption
ggplot(d_total, aes(x=Walc, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Weekend Alcohol Consumption") + ylab("Final Grade")

summary(aov(G3 ~ Walc, data = d_total)) # p-value = ~0.02%, significant

# HEALTH: Current health status
ggplot(d_total, aes(x=health, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Current Health Status") + ylab("Final Grade")

summary(aov(G3 ~ health, data = d_total)) # p-value = ~0.97%, significant

# ABSENCES: Number of school absences
ggplot(d_total, aes(x=absences, y=G3, col = course)) +
  geom_point(position = "jitter", alpha = 0.6) +
  xlab("Number of School Absences") + ylab("Final Grade")

summary(aov(G3 ~ absences, data = d_total)) # p-value = 14%, not significant

nrow(d_total[d_total$absences == 0 & d_total$G3 < 2, ]) # 54
# All of the students who absolutely failed had 0 absences. Surprising!


