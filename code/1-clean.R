
### load library
library(tidyverse)
library(magrittr)
library(readr)
library(haven) 
library(naniar) 


### start reading (raw) datasets in


demo_raw <- read_xpt(file = "data/raw/P_DEMO.XPT"); names(demo_raw) %<>% tolower #demographics

mental_raw <- read_xpt(file = "data/raw/P_DPQ.XPT"); names(mental_raw) %<>% tolower #mental health

physical_raw <- read_xpt(file = "data/raw/P_PAQ.XPT"); names(physical_raw) %<>% tolower #physical activity

smoking_raw <- read_xpt(file = "data/raw/P_SMQ.XPT"); names(smoking_raw) %<>% tolower #smoking 

sleep_raw <- read_xpt(file = "data/raw/P_SLQ.XPT"); names(sleep_raw) %<>% tolower #sleep

medical_raw <- read_xpt(file = "data/raw/P_MCQ.XPT"); names(medical_raw) %<>% tolower #medical conditions

diabetes_raw <- read_xpt(file = "data/raw/P_DIQ.XPT"); names(diabetes_raw) %<>% tolower #diabetes

bp_raw <- read_xpt(file = "data/raw/P_BPQ.XPT"); names(bp_raw) %<>% tolower #blood pressure and cholesterol

occupation_raw<- read_xpt(file = "data/raw/P_OCQ.XPT"); names(occupation_raw) %<>% tolower #occupation

weight_hist_raw<- read_xpt(file = "data/raw/P_WHQ.XPT"); names(weight_hist_raw) %<>% tolower #weight history


### handle demographics dataset 
demo <- demo_raw %>% 
  rename(subject="seqn",
         age="ridageyr", 
         ratio_income="indfmpir",
         race = "ridreth3",
         edu = dmdeduc2,
         marriage_status = "dmdmartz") %>%
  
  replace_with_na(replace = list(marriage_status = c(77,99),  #77 and 99 mean Refused and DK
                                 dmdborn4 = c(77,99),
                                edu = c(7,9))) %>% #7 and 9 mean Refused and DK
  
  mutate(female=ifelse(is.na(riagendr), NA, riagendr-1), ## female=1, male=0
         
         born_us=ifelse(is.na(dmdborn4), NA, as.numeric(dmdborn4==1))) %>%## born in the US = 1, others = 0
         
  filter(age>=20) %>% ## take only 20+ year-old adults because only they have education recorded 
  
  select(subject, female, age, race, 
         marriage_status, ratio_income, 
         born_us, edu) 


  
### mental health/depression --- calculate a composite score that is the total of all scores
### for simplicity add all score together, ignore question with NA value
### mental_score is the outcome variable (response)
mental <- mental_raw %>% 
   
   replace_with_na(replace = list(dpq010 = c(7,9),  #7 and 9 mean Refused and DK
                                  dpq020 = c(7,9),
                                  dpq030 = c(7,9),
                                  dpq040 = c(7,9),
                                  dpq050 = c(7,9),
                                  dpq060 = c(7,9),
                                  dpq070 = c(7,9),
                                  dpq080 = c(7,9),
                                  dpq090 = c(7,9),
                                  dpq100 = c(7,9))) %>%
  rename(subject="seqn") %>%
  mutate(mental_score=rowSums(cbind(dpq010, dpq020, dpq030, dpq040, dpq050, 
                              dpq060, dpq070, dpq080, dpq090, dpq100))) %>% 
  
  select(subject, mental_score) 


### smoking
smoking <- smoking_raw %>%
  rename(subject="seqn",
         age_first_smoke = "smd030",
         now_smoke = "smq040") %>%  #whether smoking now
  replace_with_na(replace = list(age_first_smoke = c(0,777,999),
                                 now_smoke = c(7,9))) %>%
  
  select(subject, age_first_smoke, now_smoke)



### sleep disorder
sleep <- sleep_raw %>%
  rename(subject="seqn", 
         sleep_weekday="sld012",
         sleep_weekend="sld013",
         sleepy = "slq120") %>%  #overly sleepy during day
  replace_with_na(replace = list(slq050 = c(7,9),
                                 sleepy = c(7,9))) %>%
  mutate(dr_sleep=ifelse(is.na(slq050), NA, as.numeric(slq050==1))) %>% ## told doctor about trouble sleeping
  select(subject, sleep_weekday, sleep_weekend, dr_sleep, sleepy)

### medical condition

medical <- medical_raw %>%
  rename(subject="seqn") %>%
  replace_with_na(replace = list(mcq010 = c(7.9),
                                 mcq160a = c(7,9),
                                 mcq160c = c(7,9),
                                 mcq160l = c(7,9),
                                 mcq160f = c(7,9),
                                 mcq160m = c(7,9),
                                 mcq160p = c(7,9),
                                 mcq220 = c(7,9),
                                 mcq366a = c(7,9),
                                 mcq366b = c(7,9),
                                 mcq366c = c(7,9),
                                 mcq371a = c(7,9),
                                 mcq371b = c(7,9),
                                 mcq371c = c(7,9),
                                 mcq371d = c(7,9),
                                 mcq366d = c(7,9))) %>%
  mutate(told_asthma=ifelse(is.na(mcq010), NA, as.numeric(mcq010==1)), ## ever told you had asthma
         told_arthritis=ifelse(is.na(mcq160a), NA, as.numeric(mcq160a==1)), ## ever told to have arthritis
         told_coronary=ifelse(is.na(mcq160c), NA, as.numeric(mcq160c==1)), ## ever told to have coronary heart disease
         told_liver=ifelse(is.na(mcq160l), NA, as.numeric(mcq160l==1)), ## ever told to have liver disease
         told_copd=ifelse(is.na(mcq160p), NA, as.numeric(mcq160p==1)), #ever told by doctor to have copd
         told_stroke=ifelse(is.na(mcq160f), NA, as.numeric(mcq160f==1)), ## ever told to have a stroke
         told_thyroid=ifelse(is.na(mcq160m), NA, as.numeric(mcq160m==1)), ## ever told to have thyroid problem
         told_cancer=ifelse(is.na(mcq220), NA, as.numeric(mcq220==1)), ## ever told to have cancer or malignancy 
         told_weight=ifelse(is.na(mcq366a), NA, as.numeric(mcq366a==1)), ## ever told by health professional to lose weight
         told_exe=ifelse(is.na(mcq366b), NA, as.numeric(mcq366b==1)), ## ever told by health professional to exercise
         told_salt=ifelse(is.na(mcq366c), NA, as.numeric(mcq366c==1)), ## ever told by health professional to reduce salt in diet
         now_lose_weight=ifelse(is.na(mcq371a), NA, as.numeric(mcq371a==1)), #now try to control weight
         now_increase_ex=ifelse(is.na(mcq371b), NA, as.numeric(mcq371b==1)), #now try to increase exercise
         now_reduce_salt=ifelse(is.na(mcq371c), NA, as.numeric(mcq371c==1)), #now try to reduce salt
         now_reduce_fat=ifelse(is.na(mcq371d), NA, as.numeric(mcq371d==1)), #now try to reduce fat
         told_fat=ifelse(is.na(mcq366d), NA, as.numeric(mcq366d==1))) %>% ## ever told by health professional to lose fat/calories
  select(subject, told_asthma, told_arthritis, told_coronary,
         told_liver, told_copd, told_stroke, told_thyroid, 
         told_cancer, told_weight, told_exe, told_salt, told_fat,
         now_lose_weight, now_increase_ex, now_reduce_salt, now_reduce_fat) 

## diabetes
diabetes <- diabetes_raw %>%
  rename(subject="seqn") %>%
  replace_with_na(replace = list(diq010 = c(7,9))) %>%
  mutate(dr_diabetes=ifelse(is.na(diq010), NA, as.numeric(diq010==1))) %>% ## doctor told participant - have diabetes
  select(subject, dr_diabetes)

### blood pressure and cholesterol
bp <- bp_raw %>%
  rename(subject="seqn") %>%
  replace_with_na(replace = list(bpq020 = c(7,9),  
                                 bpq080 = c(7,9))) %>%
  mutate(dr_highbp=ifelse(is.na(bpq020), NA, as.numeric(bpq020==1)), ## dr told having high blood pressure
         dr_highchol=ifelse(is.na(bpq080), NA, as.numeric(bpq080==1))) %>% ## dr told having high cholesterol
  select(subject, dr_highbp, dr_highchol) 


### occupation

occupation <- occupation_raw %>%
  rename(subject="seqn",
         hours_worked = "ocq180") %>%
  naniar::replace_with_na (replace = list (hours_worked = c(77777, 99999))) %>%
  select(subject, hours_worked) 

###physical activity
physical_activity <- physical_raw %>%
  rename(subject="seqn",
         min_sedentary = "pad680") %>%
  
  replace_with_na(replace = list(min_sedentary = c(7777,9999),
                                 paq605 = c(7,9),
                                 paq620 = c(7,9),
                                 paq635 = c(7,9),
                                 paq650 = c(7,9),
                                 paq665 = c(7,9))) %>%
  
  mutate(vigor_work=ifelse(is.na(paq605), NA, as.numeric(paq605==1)), ##do vigorous activity at work
         moderate_work=ifelse(is.na(paq620), NA, as.numeric(paq620==1)),##moderate activity at work
         walk_bike=ifelse(is.na(paq635), NA, as.numeric(paq635==1)), #walk or bicycle
         vigor_rec=ifelse(is.na(paq650), NA, as.numeric(paq650==1)), #vigorous recreational activity
  moderate_rec=ifelse(is.na(paq665), NA, as.numeric(paq665==1))) %>% #moderate rec activities
  select(subject, min_sedentary, vigor_work, moderate_work, walk_bike,
         vigor_rec, moderate_rec)

###weight history
weight_history <- weight_hist_raw%>%
  rename(subject="seqn",
         weight_self_percept = "whq030", #consider oneself as overweight
         want_lose_weight = "whq040") %>%  #want to lose weight
  
  replace_with_na(replace = list(weight_self_percept = c(7,9),
                                 
                                 want_lose_weight = c(7,9))) %>%
  select(subject, weight_self_percept, want_lose_weight)



### merge everything
final_clean_data_for_analysis <- Reduce(function(x,y) left_join(x, y, 
                                  by = "subject"), 
         list(mental, demo, smoking, sleep, medical, diabetes, bp, occupation, 
              physical_activity, weight_history)) %>%
  drop_na() ## remove all survey participants with missing data. 

write_csv(final_clean_data_for_analysis, 
          file="final_clean_data_for_analysis.csv", na="")



