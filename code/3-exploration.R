# load libraries
library(kableExtra)                     # for printing tables
library(cowplot)                        # for side by side plots
library(tidyverse)
library(readr)


# read in the cleaned data
nhanes_train = read_csv("data/clean/nhanes_train.csv", 
                    col_types = "iififfdffifddfffffffffffffffffffffiifffffff")

                   

# calculate mean mental health score  (4.9)
mean_mh_score = nhanes_train %>%
  summarise(mean(mental_score)) %>%
  pull()

# calculate median mental health score (3)
median_mh_score = nhanes_train %>%
  summarise(median(mental_score)) %>%
  pull()

# create histogram of mental health score
mh_histogram = nhanes_train %>%
  ggplot(aes(x = mental_score)) + 
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = median_mh_score,
             color = "red",
             linetype = "dashed") +
  labs(x = "Mental health screening scores", 
       y = "Frequency") +
  theme_bw()

#create histogram of ratio_income

ratio_income_histogram = nhanes_train %>%
  ggplot(aes(x = ratio_income)) + 
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = median(nhanes_train$ratio_income),
             color = "red",
             linetype = "dashed") +
  labs(x = "Ratio of family income to poverty", 
       y = "Frequency") +
  theme_bw()


# save the histogram of mental health score
ggsave(filename = "results/mental-health-score-histogram.png", 
       plot = mh_histogram, 
       device = "png", 
       width = 5, 
       height = 3)

# examine relationship between ratio of family income to poverty and mental health risk
p1 = nhanes_train %>%  #data points and binscatter 
  ggplot(aes(x = ratio_income, y = mental_score)) +
  geom_point() + 
  geom_smooth(method = "lm", formula = "y~x", se = FALSE) +
  stat_summary_bin(fun='mean', bins=20,
                   color='orange', size=2, geom='point') +
  labs(x = "Ratio of family income to poverty", 
       y = "Mental health screening score",
  title = bquote (~bold("Income and mental health risk")))+
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

plot1 = nhanes_train %>%  #just with binscatter for easier visualization 
  ggplot(aes(x = ratio_income, y = mental_score)) +
 
  geom_smooth(method = "lm", formula = "y~x", se = FALSE) +
  stat_summary_bin(fun='mean', bins=20,
                   color='orange', size=2, geom='point')+
  
  labs(x = "Ratio of family income to poverty", 
       y = "Mental health screening score",
       title = bquote (~bold("Income and mental health risk")))+
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

# save plot1
ggsave(filename = "results/income-and-mental-health.png", 
       plot = plot1, 
       device = "png", 
       width = 5, 
       height = 3)

# examine relationship between age and mental health risk

p2 = nhanes_train %>%  #data points and binscatter 
  ggplot(aes(x = age, y = mental_score)) +
  geom_point() + 
  geom_smooth(method = "lm", formula = "y~x", se = FALSE) +
  stat_summary_bin(fun='mean', bins=20,
                   color='orange', size=2, geom='point') +
  labs(x = "Age", 
       y = "Mental health screening score",
       title = bquote (~bold("Age and mental health risk")))+
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

plot2 = nhanes_train %>%  #just with binscatter for easier visualization 
  ggplot(aes(x = age, y = mental_score)) +
  
  geom_smooth(method = "lm", formula = "y~x", se = FALSE) +
  stat_summary_bin(fun='mean', bins=20,
                   color='orange', size=2, geom='point')+
  
  labs(x = "Age", 
       y = "Mental health screening score",
       title = bquote (~bold("Age and mental health risk")))+
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

# save plot2
ggsave(filename = "results/age-and-mental-health.png", 
       plot = plot2, 
       device = "png", 
       width = 5, 
       height = 3)


# examine relationship between age first started smoking and mental health risk

p3 = nhanes_train %>%  #data points and binscatter 
  ggplot(aes(x = age_first_smoke, y = mental_score)) +
  geom_point() + 
  geom_smooth(method = "lm", formula = "y~x", se = FALSE) +
  stat_summary_bin(fun='mean', bins=20,
                   color='orange', size=2, geom='point') +
  labs(x = "Age started smoking regularly", 
       y = "Mental health screening score",
       title = bquote (~bold("Age of smoking and mental health risk")))+
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

plot3 = nhanes_train %>%  #just with binscatter for easier visualization 
  ggplot(aes(x = age_first_smoke, y = mental_score)) +
  
  geom_smooth(method = "lm", formula = "y~x", se = FALSE) +
  stat_summary_bin(fun='mean', bins=20,
                   color='orange', size=2, geom='point')+
  
  labs(x = "Age started smoking regularly", 
       y = "Mental health screening score",
       title = bquote (~bold("Age of smoking and mental health risk")))+
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

# save plot3
ggsave(filename = "results/age-smoke-and-mental-health.png", 
       plot = plot3, 
       device = "png", 
       width = 5, 
       height = 3)


# examine relationship between sleep during weekday and mental health risk
p4 = nhanes_train %>%  #data points and binscatter 
  ggplot(aes(x = sleep_weekday, y = mental_score)) +
  geom_point() + 
  geom_smooth(method = "lm", formula = "y~x", se = FALSE) +
  stat_summary_bin(fun='mean', bins=20,
                   color='orange', size=2, geom='point') +
  labs(x = "Hours of weekday sleep", 
       y = "Mental health screening score",
       title = bquote (~bold("Hours of weekday sleep and mental health risk")))+
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

plot4 = nhanes_train %>%  #just with binscatter for easier visualization 
  ggplot(aes(x = sleep_weekday, y = mental_score)) +
  
  geom_smooth(method = "lm", formula = "y~x", se = FALSE) +
  stat_summary_bin(fun='mean', bins=20,
                   color='orange', size=2, geom='point')+
  
  labs(x = "Hours of weekday sleep", 
       y = "Mental health screening score",
       title = bquote (~bold("Hours of weekday sleep and mental health risk")))+
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

# save plot4
ggsave(filename = "results/weekday-sleep-and-mental-health.png", 
       plot = plot4, 
       device = "png", 
       width = 5, 
       height = 3)

# examine relationship between sleep during weekend and mental health risk
p5 = nhanes_train %>%
  ggplot(aes(x = sleep_weekend, y = mental_score)) +
  geom_point() + 
  geom_smooth(method = "lm", formula = "y~x", se = FALSE) +
  labs(x = "Hours of sleep during weekend", 
       y = "Mental health screening score",
title = bquote (~bold("Hours of sleep during weekend and mental health risk"))) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

plot5 = nhanes_train %>%  #just with binscatter for easier visualization 
  ggplot(aes(x = sleep_weekend, y = mental_score)) +
  
  geom_smooth(method = "lm", formula = "y~x", se = FALSE) +
  stat_summary_bin(fun='mean', bins=20,
                   color='orange', size=2, geom='point')+
  
  labs(x = "Hours of weekend sleep", 
       y = "Mental health screening score",
       title = bquote (~bold("Hours of weekend sleep and mental health risk")))+
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

# save plot 5
ggsave(filename = "results/weekend-sleep-and-mental-health.png", 
       plot = plot5, 
       device = "png", 
       width = 5, 
       height = 3)

# examine relationship between hours worked and mental health risk
p6 = nhanes_train %>%
  ggplot(aes(x = hours_worked, y = mental_score)) +
  geom_point() + 
  geom_smooth(method = "lm", formula = "y~x", se = FALSE) +
  labs(x = "Number of hours worked last week", 
       y = "Mental health screening score",
       title = "Number of work hours and mental health risk") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))


# examine relationship between minutes of sedentary activity and mental health risk
p7 = nhanes_train %>%
  ggplot(aes(x = min_sedentary, y = mental_score)) +
  geom_point() + 
  geom_smooth(method = "lm", formula = "y~x", se = FALSE) +
  labs(x = "Minutes of sedentary activity", 
       y = "Mental health screening score",
       title = "Minutes of sedentary activity and mental health risk") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))


# examine relationship between gender and mental health risk

p8 = nhanes_train %>%
  ggplot(aes(x = female, y = mental_score, fill = female)) + 
  geom_boxplot() +
  scale_x_discrete(labels = c('Female', 'Male')) +
  labs(x = "Gender", 
       y = "Mental health screening score", 
       title = bquote (~bold("Gender and mental health risk"))) + 
  theme_bw() + theme(legend.position = "none")


# save p8
ggsave(filename = "results/gender-and-mental-health.png", 
       plot = p8, 
       device = "png", 
       width = 5, 
       height = 3)

# examine relationship between race/ethnicity and mental health risk 
p9 = nhanes_train %>%
  ggplot(aes(x = race, y = mental_score, fill = race)) + 
  geom_boxplot() +
  scale_x_discrete(labels = c('Non-Hispanic\nWhite', 
                              'Mexican\nAmerican',
                              'Other\nHispanic',
                              'Non-Hispanic\nBlack',
                              'Other\nRace',
                              'Non-Hispanic\nAsian')) +
  labs(x = "Race/ethnicity", 
       y = "Mental health screening score",
       title = bquote (~bold("Race and mental health"))) + 
  theme_bw() + theme(legend.position = "none")

ggsave(filename = "results/race-and-mental-health.png", 
       plot = p9, 
       device = "png", 
       width = 5, 
       height = 3)

#examine relationship between marriage status and mental health risk 
p10= nhanes_train %>%
  ggplot(aes(x = marriage_status, y = mental_score, fill = marriage_status)) + 
  geom_boxplot() +
  labs(x = "Marriage status", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")


#examine relationship between being born in the US and mental health risk
p11= nhanes_train %>%
  ggplot(aes(x = born_us, y = mental_score, fill = born_us)) + 
  geom_boxplot() +
  labs(x = "Born in the US", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")


#examine relationship between being education and mental health risk 
p12= nhanes_train %>%
  ggplot(aes(x = edu, y = mental_score, fill = edu)) + 
  geom_boxplot() +
  scale_x_discrete(labels = c('College\ngraduate\nor above', 
                              'High school\n/GED',
                              'Some college\n/AA degree',
                              '9th-11th\ngrade',
                              'Less than\n9th grade')) +
  labs(x = "Education status", 
       y = "Mental health screening score", 
       tittle = bquote (~bold("Education and mental health risk"))) + 
  theme_bw() + theme(legend.position = "none")

# save p12
ggsave(filename = "results/education-and-mental-health.png", 
       plot = p12, 
       device = "png", 
       width = 5, 
       height = 3)

#examine relationship between smoking and mental health risk 
p13= nhanes_train %>%
  ggplot(aes(x = now_smoke, y = mental_score, fill = now_smoke)) + 
  geom_boxplot() +
  labs(x = "Smoking status", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")


#examine relationship between sleep disorder and mental health risk 
p14= nhanes_train %>%
  ggplot(aes(x = dr_sleep, y = mental_score, fill = dr_sleep)) + 
  geom_boxplot() +
  scale_x_discrete(labels = c('No', 'Yes')) +
  labs(x = "Ever told a doctor about trouble sleeping", 
       y = "Mental health screening score",
       title = bquote (~bold("Sleep trouble and mental health"))) + 
  theme_bw() + theme(legend.position = "none")

# save p14
ggsave(filename = "results/drsleep-and-mental-health.png", 
       plot = p14, 
       device = "png", 
       width = 5, 
       height = 3)

#examine relationship between asthma and mental health risk 
p15= nhanes_train %>%
  ggplot(aes(x = told_asthma, y = mental_score, fill = told_asthma)) + 
  geom_boxplot() +
  labs(x = "Told by a doctor to have asthma", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")


#examine relationship between arthritis and mental health risk 
p16= nhanes_train %>%
  ggplot(aes(x = told_arthritis, y = mental_score, fill = told_arthritis)) + 
  geom_boxplot() +
  labs(x = "Told by a doctor to have arthritis", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")


#examine relationship between coronary heart disease and mental health risk 
p17= nhanes_train %>%
  ggplot(aes(x = told_coronary, y = mental_score, fill = told_coronary)) + 
  geom_boxplot() +
  labs(x = "Told by a doctor to have coronary heart disease", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")

#examine relationship between heart attack and mental health risk 
p18 = nhanes_train %>%
  ggplot(aes(x = told_attack, y = mental_score, fill = told_attack)) + 
  geom_boxplot() +
  labs(x = "Told by a doctor to have heart attack", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")

#examine relationship between stroke and mental health risk 
p19 = nhanes_train %>%
  ggplot(aes(x = told_stroke, y = mental_score, fill = told_stroke)) + 
  geom_boxplot() +
  labs(x = "Told by a doctor to have stroke", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")


#examine relationship between thyroid and mental health risk 
p20 = nhanes_train %>%
  ggplot(aes(x = told_thyroid, y = mental_score, fill = told_thyroid)) + 
  geom_boxplot() +
  labs(x = "Told by a doctor to have thyroid problem", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")


#examine relationship between cancer and mental health risk 
p21 = nhanes_train %>%
  ggplot(aes(x = told_cancer, y = mental_score, fill = told_cancer)) + 
  geom_boxplot() +
  labs(x = "Told by a doctor to have cancer", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")


#examine relationship between being told by doctor to lose weight and mental health risk 
  ggplot(aes(x = told_weight, y = mental_score, fill = told_weight)) + 
  geom_boxplot() +
  labs(x = "Told by a doctor to lose weight", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")


#examine relationship between being told by doctor to exercise and mental health risk 
p23 = nhanes_train %>%
  ggplot(aes(x = told_exe, y = mental_score, fill = told_exe)) + 
  geom_boxplot() +
  labs(x = "Told by a doctor to exercise", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")



#examine relationship between being told by doctor to reduce salt and mental health risk 
p24 = nhanes_train %>%
  ggplot(aes(x = told_salt, y = mental_score, fill = told_salt)) + 
  geom_boxplot() +
  labs(x = "Told by a doctor to reduce salt", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")



#examine relationship between being told by doctor to reduce fat and mental health risk 
p25 = nhanes_train %>%
  ggplot(aes(x = told_fat, y = mental_score, fill = told_fat)) + 
  geom_boxplot() +
  labs(x = "Told by a doctor to lose fat", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")


#examine relationship between now try to control weight and mental health risk 
p26 = nhanes_train %>%
  ggplot(aes(x = now_lose_weight, y = mental_score, fill = now_lose_weight)) + 
  geom_boxplot() +
  labs(x = "Now lose weight", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")


#examine relationship between now try to increase exercise and mental health risk 
p27 = nhanes_train %>%
  ggplot(aes(x = now_increase_ex, y = mental_score, fill = now_increase_ex)) + 
  geom_boxplot() +
  labs(x = "Now increase exercise", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")


#examine relationship between now try to reduce salt and mental health risk 
p28 = nhanes_train %>%
  ggplot(aes(x = now_reduce_salt, y = mental_score, fill = now_reduce_salt)) + 
  geom_boxplot() +
  labs(x = "Now reduce salt", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")


#examine relationship between now try to reduce fat and mental health risk 
p29 = nhanes_train %>%
  ggplot(aes(x = now_reduce_fat, y = mental_score, fill = now_reduce_fat)) + 
  geom_boxplot() +
  labs(x = "Now reduce fat", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")


#examine relationship between weight self-perception and mental health risk 
p30 = nhanes_train %>%
  ggplot(aes(x = weight_self_percept, y = mental_score, 
             fill = weight_self_percept)) + 
  geom_boxplot() +
  scale_x_discrete(labels = c('Overweight', 
                              'About the right weight',
                              'Underweight')) +
  labs(x = "How do you consider your weight?", 
       y = "Mental health screening score",
       title = bquote (~bold("Weight self-perception and mental health"))) + 
  theme_bw() + theme(legend.position = "none")

#save p30

ggsave(filename = "results/weight-percept-and-mental-health.png", 
       plot = p30, 
       device = "png", 
       width = 5, 
       height = 3)

#examine relationship between wanting to lose weight and mental health risk
p31 = nhanes_train %>%
  ggplot(aes(x = want_lose_weight, y = mental_score, 
             fill = want_lose_weight)) + 
  geom_boxplot() +
  labs(x = "Like to weigh more, less or the same", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")

#examine relationship between feeling overly sleepy during the day and mental health risk

p32 = nhanes_train %>%
  ggplot(aes(x = sleepy, y = mental_score, 
             fill = sleepy)) + 
  geom_boxplot() +
  scale_x_discrete(labels = c('Sometimes', 
                              'Rarely',
                              'Often',
                              'Almost always',
                              'Never')) +
  labs(x = "In the past month how often feeling overly sleepy during the day?", 
       y = "Mental health screening score",
       title = bquote (~bold("Excessive sleepiness and mental health"))) + 
  theme_bw() + theme(legend.position = "none")

#save p32

ggsave(filename = "results/sleepy-and-mental-health.png", 
       plot = p32, 
       device = "png", 
       width = 5, 
       height = 3)

##examine the relationship between vigorous-intensity recreational activity and mental health risks
p33 = nhanes_train %>%
  ggplot(aes(x = vigor_rec, y = mental_score, 
             fill = vigor_rec)) + 
  geom_boxplot() +
  scale_x_discrete(labels = c('Yes', 'No'))+
                             
  labs(x = "In a typical week do you do vigorous-intensity recreational activity?", 
       y = "Mental health screening score",
  title = bquote (~bold
          ("Recreational activity and mental health "))) + 
  theme_bw() + theme(legend.position = "none")

#save p33

ggsave(filename = "results/vigor-rec-and-mental-health.png", 
       plot = p33, 
       device = "png", 
       width = 5, 
       height = 3)


##examine the relationship between ratio income and ever telling a doctor about trouble with sleeping
p34 = nhanes_train %>%
  ggplot(aes(x = dr_sleep, y = ratio_income, 
             fill = dr_sleep)) + 
  geom_boxplot() +
  scale_x_discrete(labels = c('No', 'Yes'))+
  
  labs(x = "Ever told a doctor about trouble sleeping", 
       y = "Ratio of family income to poverty",
       title = bquote (~bold("Sleep trouble and income"))) + 
  theme_bw() + theme(legend.position = "none")

#save p34

ggsave(filename = "results/income-dr-sleep.png", 
       plot = p34, 
       device = "png", 
       width = 5, 
       height = 3)

#examine relationship between ratio income and frequency of feeling overly sleepy during the day

p35 = nhanes_train %>%
  ggplot(aes(x = sleepy, y = ratio_income, 
             fill = sleepy)) + 
  geom_boxplot() +
  #scale_x_discrete(labels = c('Yes', 'No'))+
  
  labs(x = "Frequency of feeling overly sleepy during the day", 
       y = "Ratio of family income to poverty") + 
  theme_bw() + theme(legend.position = "none")

#relationship between ratio family income and engage in vigorous recreational activity
p36 = nhanes_train %>%
  ggplot(aes(x = vigor_rec, y = ratio_income, 
             fill = vigor_rec)) + 
  geom_boxplot() +
  scale_x_discrete(labels = c('Yes', 'No'))+
  
  labs(x = "Whether or not engage in vigorous-intensity recreational activity", 
       y = "Ratio of family income to poverty",
title = bquote (~bold("Recreational activity and income"))) + 
  theme_bw() + theme(legend.position = "none")


#save p36

ggsave(filename = "results/income-vigor-rec.png", 
       plot = p36, 
       device = "png", 
       width = 5, 
       height = 3)

#gender and hours of sleep weekday
p37 = nhanes_train %>%
  ggplot(aes(x = female, y = sleep_weekday, 
             fill = female)) + 
  geom_boxplot() +
  scale_x_discrete(labels = c('Female', 'Male'))+
  
  labs(x = "Gender", 
       y = "Weekday hours of sleep") + 
  theme_bw() + theme(legend.position = "none")

#gender and weekend hours of sleep
p38 = nhanes_train %>%
  ggplot(aes(x = female, y = sleep_weekend, 
             fill = female)) + 
  geom_boxplot() +
  scale_x_discrete(labels = c('Female', 'Male'))+
  
  labs(x = "Gender", 
       y = "Weekend hours of sleep") + 
  theme_bw() + theme(legend.position = "none")

#race and weekday sleep
p40 = nhanes_train %>%
  ggplot(aes(x = race, y = sleep_weekday, 
             fill = race)) + 
  geom_boxplot() +
  scale_x_discrete(labels = c('Non-Hispanic\nWhite', 
                              'Mexican\nAmerican',
                              'Other\nHispanic',
                              'Non-Hispanic\nBlack',
                              'Other\nRace',
                              'Non-Hispanic\nAsian'))+
  
  labs(x = "Race", 
       y = "Weekday hours of sleep",
       title = bquote (~bold("Race and weekday hours of sleep"))) + 
  theme_bw() + theme(legend.position = "none")

ggsave(filename = "results/race-weekday-sleep.png", 
       plot = p40, 
       device = "png", 
       width = 5, 
       height = 3)

#race and weekend hours of sleep
p41 = nhanes_train %>%
  ggplot(aes(x = race, y = sleep_weekend, 
             fill = race)) + 
  geom_boxplot() +
  scale_x_discrete(labels = c('Non-Hispanic\nWhite', 
                              'Mexican\nAmerican',
                              'Other\nHispanic',
                              'Non-Hispanic\nBlack',
                              'Other\nRace',
                              'Non-Hispanic\nAsian'))+
  
  labs(x = "Race", 
       y = "Weekend hours of sleep") + 
  theme_bw() + theme(legend.position = "none")




