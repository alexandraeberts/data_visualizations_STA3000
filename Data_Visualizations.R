#NBA
#Height, weight and age of NBA players
nba = read.csv('http://users.stat.ufl.edu/%7Ewinner/data/nba_ht_wt.csv')

head(nba)
str(nba)

#libraries used
library(tidyverse)

#Create a plot which displays the relationship between height and weight of the players
ggplot(nba) + aes(x = Weight, y = Height) +
  geom_point() + ggtitle('Relationship Between Height and Weight of NBA Players') +
  #gets rid of grid and centers title
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

#Create a plot that shows the heights and weights of players by position
ggplot(nba) + aes(x = Weight, y = Height, color = Pos) +
  geom_point() + ggtitle('Heights and Weights of Players by Position') +
  theme_minimal() + scale_color_manual(name = 'Position', 
                                       labels = c('Center', 'Forward', 'Guard'),
                                       values = c("#000000","#E69F00","#56B4E9")) +
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5))

#Create a plot that shows BMI players by position. Do you see any association between variables?
#calculate BMI
nba = nba %>% mutate(bmi = (Weight/(Height)^2)*703)
head(nba)
str(nba)

#creating a boxplot to show relationship between bmi and position
ggplot(nba) + aes(x = Pos, y = bmi, fill = Pos) +
  geom_boxplot(alpha = 0.5) + 
  stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 3, fill = "black") +
  ggtitle('Player BMI by Position') +
  scale_x_discrete(labels = c('Center', 'Forward', 'Guard')) +
  theme_light() + theme(plot.title = element_text(face = 'bold', size = 15, hjust = 0.5),
                        legend.position = 'none') +
  scale_fill_brewer(palette="BuPu") +
  xlab('Position') +
  ylab('BMI')

#College Majors

#libraries used
library(fivethirtyeight)

#read in Collge Major data from Five Thirty Eight
data("college_all_ages")

#Data Exploration
head(college_all_ages)
str(college_all_ages)

#What major has the highest median full-time earnings? Which has the lowest?
#grouping data by major and getting sum
#highest median full-time earnings is Petroleum Engineering at $125,000
college_all_ages %>% group_by(major) %>% summarize(median) %>% 
  arrange(desc(median))

#lowest median full-time earnings is Neuroscience at $35,000
college_all_ages %>% group_by(major) %>% summarize(median) %>% 
  arrange(median)

#Which major has the highest unemployment rate? Which has lowest?
college_all_ages %>% group_by(major) %>% summarize(unemployment_rate) %>%
  arrange(unemployment_rate)
#lowest is Educational Administration and Supervision and Geological and Geophysical Engineering at 0%

college_all_ages %>% group_by(major) %>% summarize(unemployment_rate) %>%
  arrange(desc(unemployment_rate))
#highest is Miscellaneous Fine Arts at 15.6%

#Find the median full time earnings and unemployment rate for your major
college_all_ages %>% select(major, median, unemployment_rate) %>% 
  filter(major == "Statistics And Decision Science") 
#Median salary is $70,000 and Unemployment Rate is about 6%

#subsetting data into majors in the categories: “Business”, 
#“Computers & Mathematics”, “Engineering”, and “Health"
data_subset = college_all_ages %>% 
  filter(major_category %in% c('Business', 'Computers & Mathematics',
                               'Engineering', 'Health'))

#Create a graph that shows the distribution of median full-time earnings by major
#category. Explain what you see
ggplot(data_subset) + aes(x = major_category, y = median) + 
  theme_classic() +
  geom_boxplot() + ggtitle('Distribution of Median Full-Time Earnings by Major Category') +
  theme(plot.title = element_text(face = 'bold', size = 15, hjust = 0.5)) +
  ylab('Median Full-Time Salary') +
  xlab('Major Category')

#Create a graph that shows the distribution of unemployment rates by major category.
#Explain what you see.
ggplot(data_subset) + aes(x = major_category, y = unemployment_rate*100) + 
  theme_classic() +
  geom_boxplot() + 
  ggtitle('Distribution of Unemployment Rate by Major Category') +
  theme(plot.title = element_text(face = 'bold', size = 15, hjust = 0.5)) +
  ylab('Unemployment Rate') +
  xlab('Major Category')

#Bechdel Test Data
data('bechdel')

#data exploration
head(bechdel)
str(bechdel)

#Create a plot that tracks the % of movies passing the Bechdel test for every year in the
#sample.

b_test = bechdel %>% group_by(year, binary) %>% summarize(n = n())
b_test

ggplot(b_test) + aes(x = year, y = n, fill = binary) +
  geom_bar(position = 'dodge', stat = 'identity') +
  coord_flip() +
  ggtitle('Percentage of Movies that Pass the Bechdel Test by Year') +
  theme(plot.title = element_text(face = 'bold', size = 15, hjust = 0.5)) +
  labs(fill = 'Key') +
  xlab('Year') +
  ylab('Percentage of Pass/Fail') +
  theme_minimal()


#Plot the budget for movies that pass the test and for those that don’t. Do you see any
#differences?
head(bechdel)

b_test2 = bechdel %>% group_by(binary) %>% summarize(Avg_budget = mean(budget))
b_test2

ggplot(b_test2) + aes(x = binary, y = Avg_budget) +
  geom_bar(stat = 'identity', fill = c('light blue', 'orchid')) +
  coord_flip() + 
  ggtitle('Average Budget for Movies that \n Passed and Failed Bechdel Test') +
  theme_minimal() +
  xlab('Pass/Fail') +
  ylab('Average Budget') +
  theme(plot.title = element_text(size = 15, hjust = 0.5))

#The average budget for movies that failed is larger than for movies that passed

#baby names visualizations

install.packages('babynames')
library(babynames)

#Create a plot (or separate plots) tracking the frequency of your favorite female and male
#names over time. Comment on the trends. 

head(babynames)

fav_names = babynames %>% filter(name %in% c('Alexandra', 'Max', 'Christine'))
fav_names

ggplot(fav_names) + aes(x = year, y = n, color = name) +
  geom_line(size = 1, alpha = 0.7) + 
  ggtitle('Frequency of the Names Alexandra, Max \n and Christine Over the Years') +
  theme_classic() + theme(plot.title = element_text(size = 15, hjust = 0.5)) +
  xlab('Year') +
  ylab('Total') +
  labs(color = 'Names')

#gender neutral name plots
#Some names are gender-neutral. Create a plot that tracks the frequencies of the given
#names Alex, Blake, Drew, and Jordan by biological sex over time. Comment on the
#trends you see.

g_names = babynames %>% filter(name %in% c('Alex', 'Blake', 'Drew', 'Jordan'))

ggplot(g_names) + aes(x = year, y = n, color = sex, alpha = sex) +
  geom_line(size = 1) + ggtitle('Gender-Neutral Names Over Time') +
  theme_classic() + theme(plot.title = element_text(size = 15, hjust = 0.5)) +
  scale_alpha_manual(values = c(1, 0.3))

#All four of these names have historically been mostly male names, however in the 80s some of these
#names also began being used for females.
