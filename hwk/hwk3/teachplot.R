# visualization for #3
library(tidyverse)

teach <- read.table('teach.csv', sep = ',', header = TRUE)

# create factor variables
teach <- teach %>% 
  mutate(marry = as.factor(ifelse(marry == TRUE, 'yes', 'no'))) %>% 
  mutate(degree = as.factor(degree)) %>% 
  mutate(train = as.factor(ifelse(train == TRUE, 'yes', 'no'))) %>% 
  mutate(type = as.factor(type)) %>% 
  mutate(brk = as.factor(ifelse(brk == TRUE, 'yes', 'no'))) %>% 
  mutate(sex = as.factor(sex))

# i
ggplot(data = teach) +
  geom_jitter(mapping = aes(x = months, y = salary, color = sex)) +
  ggtitle('Salary Versus Number of Months By Sex')
# appears that males earn more on average than female based on months working
# some male observations are outliers

# ii
p <- ggplot(data = teach)
p + geom_boxplot(mapping = aes(sex, salary)) + ggtitle('Salary Distribution by Sex')
# appears that males earn more than female on average, median male greater than
# female distribution skewed by couple outliers
p + geom_boxplot(mapping = aes(marry, salary)) + ggtitle('Salary Distribution by Marriage')
# appears that not married earn more than married on average
p + geom_boxplot(mapping = aes(degree, salary)) + ggtitle('Salary Distribution by Degree')
# degree 3 earns most with little variability
# degree 2 has significant variability, mean = median, non skewed
# degree 1 has outlier, skewed since median greater than mean
# degree 0 symmetric and bell shaped
# salary increases as degree type increases
p + geom_boxplot(mapping = aes(type, salary)) + ggtitle('Salary Distribution by Type')
# type a,b
# type a more on average, near symmetric
# type b skewed
p + geom_boxplot(mapping = aes(train, salary)) + ggtitle('Salary Distribution by Training')
# train earns ore on average, slight skew
# not train symmetric
p + geom_boxplot(mapping = aes(brk, salary)) + ggtitle('Salary Distribution by Break')
# no break earns less on average with more variability, slight skew
# yes break tighter distribution, median less than mean, two outliers

# violin plots
p + geom_violin(mapping = aes(sex, salary)) + ggtitle('Salary Distribution by Sex')
p + geom_violin(mapping = aes(marry, salary)) + ggtitle('Salary Distribution by Marriage')
p + geom_violin(mapping = aes(degree, salary)) + ggtitle('Salary Distribution by Degree')
p + geom_violin(mapping = aes(type, salary)) + ggtitle('Salary Distribution by Type')
p + geom_violin(mapping = aes(train, salary)) + ggtitle('Salary Distribution by Training')
p + geom_violin(mapping = aes(brk, salary)) + ggtitle('Salary Distribution by Break')

# bar charts
p + geom_bar(mapping = aes(sex:marry)) + ggtitle('Salary Distribution by Marriage')
p + geom_bar(mapping = aes(sex)) + ggtitle('Salary Distribution by Sex')
p + geom_bar(mapping = aes(marry)) + ggtitle('Salary Distribution by Marriage')
p + geom_bar(mapping = aes(degree)) + ggtitle('Salary Distribution by Degree')
p + geom_bar(mapping = aes(type)) + ggtitle('Salary Distribution by Type')
p + geom_bar(mapping = aes(train)) + ggtitle('Salary Distribution by Training')
p + geom_bar(mapping = aes(brk)) + ggtitle('Salary Distribution by Break')

# area plots
# no males be married yo

ggplot(data = teach, mapping = aes(x = months, y = salary, fill = marry:sex)) +
  geom_area()

ggplot(data = teach) +
  geom_jitter(mapping = aes(x = months, y = salary, color = sex:marry)) +
  ggtitle('Salary Versus Number of Months By Sex')

# filter out males and look at them closer
males <- teach %>% 
  filter(sex == "M")
summary(males)
males %>% 
  filter(brk == "no") %>%
  summary()

# look at marriage




# months > 100 ------------------------------------------------------------

senior <- teach %>% 
  filter(months > 100)

p <- ggplot(data = senior)
p + geom_boxplot(mapping = aes(sex, salary)) + ggtitle('Salary Distribution by Sex')
# males make more than females, significant, single outlier, both distributions skewed
p + geom_boxplot(mapping = aes(marry, salary)) + ggtitle('Salary Distribution by Marriage')
# appears that not married earn more than married on average
p + geom_boxplot(mapping = aes(degree, salary)) + ggtitle('Salary Distribution by Degree')
# salary increases as degree type increases
# more variability in degree 1,2
# outlier in degree 3,1 also have less variability
p + geom_boxplot(mapping = aes(type, salary)) + ggtitle('Salary Distribution by Type')
# b earns more with more variability
p + geom_boxplot(mapping = aes(train, salary)) + ggtitle('Salary Distribution by Training')
# train earns more on average
p + geom_boxplot(mapping = aes(brk, salary)) + ggtitle('Salary Distribution by Break')
# no break earns less on average with more variability, slight skew
# both skewed distributions

# cross filtering the above
p + geom_boxplot(mapping = aes(sex, salary, fill = degree)) 
p + geom_boxplot(mapping = aes(sex, salary, fill = type)) 
p + geom_boxplot(mapping = aes(sex, salary, fill = train))
p + geom_boxplot(mapping = aes(sex, salary, fill = brk))
p + geom_boxplot(mapping = aes(sex, salary, fill = marry))
