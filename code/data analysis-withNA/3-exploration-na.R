# load libraries
library(kableExtra)                     # for printing tables
library(cowplot)                        # for side by side plots
library(tidyverse)


# read in the cleaned data
nhanes_train = read_csv("data/clean/nhanes_train.csv", 
                    col_types = "iififfdffifddfffffffffffffffffffffiifffffff")

# calculate mean mental health score  (4.9)
mean_mh_score = nhanes_train %>%
  summarise(mean(mental_score)) %>%
  pull()

# calculate median case fatality rate (3)
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

ratio_income_histogram = nhanes_train %>%
  ggplot(aes(x = ratio_income)) + 
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = median(nhanes_train$ratio_income),
             color = "red",
             linetype = "dashed") +
  labs(x = "Ratio of family income to poverty", 
       y = "Frequency") +
  theme_bw()


# save the histogram
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
       title = "Hours of sleep during weekend and mental health risk") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

# save p5
ggsave(filename = "results/age-and-mental-health.png", 
       plot = p5, 
       device = "png", 
       width = 5, 
       height = 3)

# examine relationship between sleep during weekend and mental health risk
p6 = nhanes_train %>%
  ggplot(aes(x = hours_worked, y = mental_score)) +
  geom_point() + 
  geom_smooth(method = "lm", formula = "y~x", se = FALSE) +
  labs(x = "Number of hours worked last week", 
       y = "Mental health screening score",
       title = "Number of work hours and mental health risk") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

# save p6
ggsave(filename = "results/age-and-mental-health.png", 
       plot = p5, 
       device = "png", 
       width = 5, 
       height = 3)

# examine relationship between sleep during weekend and mental health risk
p7 = nhanes_train %>%
  ggplot(aes(x = min_sedentary, y = mental_score)) +
  geom_point() + 
  geom_smooth(method = "lm", formula = "y~x", se = FALSE) +
  labs(x = "Minutes of sedentary activity", 
       y = "Mental health screening score",
       title = "Hours of sleep during weekend and mental health risk") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

# save p7
ggsave(filename = "results/age-and-mental-health.png", 
       plot = p5, 
       device = "png", 
       width = 5, 
       height = 3)


# examine relationship between gender and mental health risk
p8 = nhanes_train %>%
  ggplot(aes(x = female, y = mental_score, fill = female)) + 
  geom_boxplot() +
  labs(x = "Gender", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")

p8_hist = nhanes_train %>%
  ggplot(aes(x = female, y = mental_score)) + 
  geom_histogram() +
  labs(x = "Gender", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")

# save p8
ggsave(filename = "results/gender-and-mental-health.png", 
       plot = p8, 
       device = "png", 
       width = 5, 
       height = 3)

# examine relationship between gender and mental health risk (some dif)
p8 = nhanes_train %>%
  ggplot(aes(x = female, y = mental_score, fill = female)) + 
  geom_boxplot() +
  labs(x = "Gender", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")

# save p8
ggsave(filename = "results/gender-and-mental-health.png", 
       plot = p8, 
       device = "png", 
       width = 5, 
       height = 3)

# examine relationship between race/ethnicity and mental health risk (some dif)
p9 = nhanes_train %>%
  ggplot(aes(x = nonhisp_white, y = mental_score, fill = nonhisp_white)) + 
  geom_boxplot() +
  labs(x = "Race/ethnicity", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")

# save p9
ggsave(filename = "results/gender-and-mental-health.png", 
       plot = p8, 
       device = "png", 
       width = 5, 
       height = 3)

#examine relationship between marriage status and mental health risk (some dif)
p10= nhanes_train %>%
  ggplot(aes(x = never_married, y = mental_score, fill = never_married)) + 
  geom_boxplot() +
  labs(x = "Marriage status", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")

# save p10
ggsave(filename = "results/gender-and-mental-health.png", 
       plot = p8, 
       device = "png", 
       width = 5, 
       height = 3)

#examine relationship between being born in ths US and mental health risk
p11= nhanes_train %>%
  ggplot(aes(x = born_us, y = mental_score, fill = born_us)) + 
  geom_boxplot() +
  labs(x = "Born in the US", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")

# save p11
ggsave(filename = "results/gender-and-mental-health.png", 
       plot = p8, 
       device = "png", 
       width = 5, 
       height = 3)

#examine relationship between being education and mental health risk (some dif)
p12= nhanes_train %>%
  ggplot(aes(x = edu_college, y = mental_score, fill = edu_college)) + 
  geom_boxplot() +
  labs(x = "Some college or higher", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")

# save p12
ggsave(filename = "results/gender-and-mental-health.png", 
       plot = p8, 
       device = "png", 
       width = 5, 
       height = 3)

#examine relationship between smoking and mental health risk (some dif - those who smoke every day)
p13= nhanes_train %>%
  ggplot(aes(x = now_smoke, y = mental_score, fill = now_smoke)) + 
  geom_boxplot() +
  labs(x = "Smoking status", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")

# save p13
ggsave(filename = "results/gender-and-mental-health.png", 
       plot = p8, 
       device = "png", 
       width = 5, 
       height = 3)

#examine relationship between sleep disorder and mental health risk (some dif - those who told doctor)
p14= nhanes_train %>%
  ggplot(aes(x = dr_sleep, y = mental_score, fill = dr_sleep)) + 
  geom_boxplot() +
  labs(x = "Told doctor about trouble sleeping", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")

# save p14
ggsave(filename = "results/gender-and-mental-health.png", 
       plot = p8, 
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

# save p15
ggsave(filename = "results/gender-and-mental-health.png", 
       plot = p8, 
       device = "png", 
       width = 5, 
       height = 3)

#examine relationship between asthma and mental health risk 
p16= nhanes_train %>%
  ggplot(aes(x = told_arthritis, y = mental_score, fill = told_arthritis)) + 
  geom_boxplot() +
  labs(x = "Told by a doctor to have arthritis", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")

# save p16
ggsave(filename = "results/gender-and-mental-health.png", 
       plot = p8, 
       device = "png", 
       width = 5, 
       height = 3)

#examine relationship between coronary heart disease and mental health risk (no dif)
p17= nhanes_train %>%
  ggplot(aes(x = told_coronary, y = mental_score, fill = told_coronary)) + 
  geom_boxplot() +
  labs(x = "Told by a doctor to have coronary heart disease", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")

#examine relationship between heart attack and mental health risk (no dif)
p18 = nhanes_train %>%
  ggplot(aes(x = told_attack, y = mental_score, fill = told_attack)) + 
  geom_boxplot() +
  labs(x = "Told by a doctor to have heart attack", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")

#examine relationship between coronary heart disease and mental health risk (no dif)
p17= nhanes_train %>%
  ggplot(aes(x = told_coronary, y = mental_score, fill = told_coronary)) + 
  geom_boxplot() +
  labs(x = "Told by a doctor to have coronary heart disease", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")

#examine relationship between stroke and mental health risk (quite a big dif)
p19 = nhanes_train %>%
  ggplot(aes(x = told_stroke, y = mental_score, fill = told_stroke)) + 
  geom_boxplot() +
  labs(x = "Told by a doctor to have stroke", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")
# save p17
ggsave(filename = "results/gender-and-mental-health.png", 
       plot = p8, 
       device = "png", 
       width = 5, 
       height = 3)

#examine relationship between thyroid and mental health risk (some dif)
p20 = nhanes_train %>%
  ggplot(aes(x = told_thyroid, y = mental_score, fill = told_thyroid)) + 
  geom_boxplot() +
  labs(x = "Told by a doctor to have thyroid problem", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")

# save p20
ggsave(filename = "results/gender-and-mental-health.png", 
       plot = p8, 
       device = "png", 
       width = 5, 
       height = 3)


#examine relationship between stroke and mental health risk (a little dif)
p21 = nhanes_train %>%
  ggplot(aes(x = told_cancer, y = mental_score, fill = told_cancer)) + 
  geom_boxplot() +
  labs(x = "Told by a doctor to have cancer", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")

# save p21
ggsave(filename = "results/gender-and-mental-health.png", 
       plot = p8, 
       device = "png", 
       width = 5, 
       height = 3)

#examine relationship between being told by doctor to lose weight and mental health risk (a little dif)
p22 = nhanes_train %>%
  ggplot(aes(x = told_weight, y = mental_score, fill = told_weight)) + 
  geom_boxplot() +
  labs(x = "Told by a doctor to lose weight", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")

# save p22
ggsave(filename = "results/gender-and-mental-health.png", 
       plot = p8, 
       device = "png", 
       width = 5, 
       height = 3)


#examine relationship between being told by doctor to exercise and mental health risk (a little dif)
p23 = nhanes_train %>%
  ggplot(aes(x = told_exe, y = mental_score, fill = told_exe)) + 
  geom_boxplot() +
  labs(x = "Told by a doctor to exercise", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")

# save p23
ggsave(filename = "results/gender-and-mental-health.png", 
       plot = p8, 
       device = "png", 
       width = 5, 
       height = 3)


#examine relationship between being told by doctor to reduce salt and mental health risk (a little dif)
p24 = nhanes_train %>%
  ggplot(aes(x = told_salt, y = mental_score, fill = told_salt)) + 
  geom_boxplot() +
  labs(x = "Told by a doctor to exercise", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")

# save p24
ggsave(filename = "results/gender-and-mental-health.png", 
       plot = p8, 
       device = "png", 
       width = 5, 
       height = 3)


#examine relationship between being told by doctor to reduce salt and mental health risk (a little dif)
p25 = nhanes_train %>%
  ggplot(aes(x = told_fat, y = mental_score, fill = told_fat)) + 
  geom_boxplot() +
  labs(x = "Told by a doctor to lose fat", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")

# save p25
ggsave(filename = "results/gender-and-mental-health.png", 
       plot = p8, 
       device = "png", 
       width = 5, 
       height = 3)


#examine relationship between now try to control weight and mental health risk 
p26 = nhanes_train %>%
  ggplot(aes(x = now_lose_weight, y = mental_score, fill = now_lose_weight)) + 
  geom_boxplot() +
  labs(x = "Now lose weight", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")

# save p26
ggsave(filename = "results/gender-and-mental-health.png", 
       plot = p8, 
       device = "png", 
       width = 5, 
       height = 3)

#examine relationship between now try to increase exercise and mental health risk (no dif)
p27 = nhanes_train %>%
  ggplot(aes(x = now_increase_ex, y = mental_score, fill = now_increase_ex)) + 
  geom_boxplot() +
  labs(x = "Now increase exercise", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")

# save p27
ggsave(filename = "results/gender-and-mental-health.png", 
       plot = p8, 
       device = "png", 
       width = 5, 
       height = 3)

#examine relationship between now try to reduce salt and mental health risk 
p28 = nhanes_train %>%
  ggplot(aes(x = now_reduce_salt, y = mental_score, fill = now_reduce_salt)) + 
  geom_boxplot() +
  labs(x = "Now reduce salt", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")

# save p28
ggsave(filename = "results/gender-and-mental-health.png", 
       plot = p8, 
       device = "png", 
       width = 5, 
       height = 3)

#examine relationship between now try to reduce fat and mental health risk (some dif)
p29 = nhanes_train %>%
  ggplot(aes(x = now_reduce_fat, y = mental_score, fill = now_reduce_fat)) + 
  geom_boxplot() +
  labs(x = "Now reduce fat", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")

# save p29
ggsave(filename = "results/gender-and-mental-health.png", 
       plot = p8, 
       device = "png", 
       width = 5, 
       height = 3)

#examine relationship between weight self-perception and mental health risk (some dif)
p30 = nhanes_train %>%
  ggplot(aes(x = weight_self_percept, y = mental_score, 
             fill = weight_self_percept)) + 
  geom_boxplot() +
  labs(x = "Weight perception", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")

#examine relationship between weight self-perception and mental health risk (some dif)
p31 = nhanes_train %>%
  ggplot(aes(x = want_lose_weight, y = mental_score, 
             fill = want_lose_weight)) + 
  geom_boxplot() +
  labs(x = "Like to weigh more, less or the same", 
       y = "Mental health screening score") + 
  theme_bw() + theme(legend.position = "none")
# save p29
ggsave(filename = "results/gender-and-mental-health.png", 
       plot = p8, 
       device = "png", 
       width = 5, 
       height = 3)
# create a heatmap of case fatality rate across the U.S.
p = map_data("county") %>%
  as_tibble() %>% 
  left_join(case_data %>% 
              rename(region = state, 
                     subregion = county,
                     `Case Fatality Rate` = case_fatality_rate) %>% 
              mutate(region = str_to_lower(region), 
                     subregion = str_to_lower(subregion)), 
            by = c("region", "subregion")) %>%
  ggplot() + 
  geom_polygon(data=map_data("state"), 
               aes(x=long, y=lat, group=group),
               color="black", fill=NA,  size = 1, alpha = .3) + 
  geom_polygon(aes(x=long, y=lat, group=group, fill = `Case Fatality Rate`),
               color="darkblue", size = .1) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_void()

ggsave(filename = "results/response-map.png", 
       plot = p, 
       device = "png", 
       width = 7, 
       height = 4)