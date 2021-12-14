
### load library
library(tidyverse)
library(magrittr)
library(readr)
library(haven) ## to read xpt file
library(naniar)



### obtain the path for the datasets you want 
path_demo <- file.path(path_this, "P_DEMO.XPT") ## demographics data
path_mental <- file.path(path_survey, "P_DPQ.XPT") ## mental health
path_diabetes <- file.path(path_survey, "P_DIQ.XPT") ## diabetes
path_bp <- file.path(path_survey, "P_BPQ.XPT") ## blood pressure and cholesterol
path_cardio <- file.path(path_survey, "P_CDQ.XPT") ## cardiovascular health
path_body <- file.path(path_exam, "P_BMX.XPT") ## body measures
path_insurance <- file.path(path_survey, "P_HIQ.XPT") ## health insurance
path_smoking <- file.path(path_survey, "P_SMQ.XPT") ## smoking
path_sleep <- file.path(path_survey, "P_SLQ.XPT") ## sleep disorders
path_medical <- file.path(path_survey, "P_MCQ.XPT") ## medical condition

### start reading (raw) datasets in

#clean_data <- read_csv(file = "data/clean/db(1).csv")

demo_raw <- read_xpt(file = "data/raw/P_DEMO.XPT"); names(demo_raw) %<>% tolower ## this is to make all names lower-case, easier to type later

mental_raw <- read_xpt(file = "data/raw/P_DPQ.XPT"); names(mental_raw) %<>% tolower #mental health

diabetes_raw <- read_xpt(file = "data/raw/P_DIQ.XPT"); names(diabetes_raw) %<>% tolower #diabetes

bp_raw <- read_xpt(file = "data/raw/P_BPQ.XPT"); names(bp_raw) %<>% tolower #blood pressure and cholesterol

cardio_raw <- read_xpt(file = "data/raw/P_CDQ.XPT"); names(cardio_raw) %<>% tolower #cardiovascular health

body_raw <- read_xpt(file = "data/raw/P_BMX.XPT"); names(body_raw) %<>% tolower #body measures

insurance_raw <- read_xpt(file = "data/raw/P_HIQ.XPT"); names(insurance_raw) %<>% tolower #health insurance

physical_raw <- read_xpt(file = "data/raw/P_PAQ.XPT"); names(physical_raw) %<>% tolower #physical activity

smoking_raw <- read_xpt(file = "data/raw/P_SMQ.XPT"); names(smoking_raw) %<>% tolower #smoking cig

sleep_raw <- read_xpt(file = "data/raw/P_SLQ.XPT"); names(sleep_raw) %<>% tolower #sleep

medical_raw <- read_xpt(file = "data/raw/P_MCQ.XPT"); names(medical_raw) %<>% tolower #medical conditions

occupation_raw<- read_xpt(file = "data/raw/P_OCQ.XPT"); names(occupation_raw) %<>% tolower #occupation

dermatology_raw<- read_xpt(file = "data/raw/P_DEQ.XPT"); names(dermatology_raw) %<>% tolower #dermatology

weight_hist_raw<- read_xpt(file = "data/raw/P_WHQ.XPT"); names(weight_hist_raw) %<>% tolower #weight history

### handle demo dataset --- the one in the quotations is the original variables 
demo <- demo_raw %>% 
  rename(subject="seqn",
         age="ridageyr", ## take only age in year instead of age in months
         ratio_income="indfmpir") %>%
  
  replace_with_na(replace = list(dmdmartz = c(77,99),  #77 and 99 mean Refused and DK
                                 dmdborn4 = c(77,99),
                                 dmdeduc2 = c(7,9))) %>% #7 and 9 mean Refused and DK
  ## to facilitate lasso and random forest analysis later, I'm going to create some new variables
  mutate(female=ifelse(is.na(riagendr), NA, riagendr-1), ## female=1, male=0
         nonhisp_white=ifelse(is.na(ridreth3), NA, as.numeric(ridreth3==3)), ## non-hispanic white=1, all other=0
         never_married=ifelse(is.na(dmdmartz), NA, as.numeric(dmdmartz==3)), ## never married = 1, all others = 0
         born_us=ifelse(is.na(dmdborn4), NA, as.numeric(dmdborn4==1)),## born in the US = 1, others = 0
         edu_college=ifelse(is.na(dmdeduc2), NA, as.numeric(dmdeduc2 %in% c(4, 5)))) %>% ## education some college or above = 1, others = 0
  filter(age>=20) %>% ## take only 20+ year-old adults because only they have education recorded 
  
  select(subject, female, age, nonhisp_white, never_married, ratio_income, born_us, edu_college) 


  
### mental health/depression --- calculate a composite score that is the total of all scores
### for simplicity we add all score together, ignore question with NA value
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

### diabetes
diabetes <- diabetes_raw %>%
  rename(subject="seqn") %>%
  replace_with_na(replace = list(diq010 = c(7,9),
                                 diq160 = c(7,9))) %>%
  mutate(dr_diabetes=ifelse(is.na(diq010), NA, as.numeric(diq010==1)),## doctor told participant have diabetes
  dr_prediabetes=ifelse(is.na(diq160), NA, as.numeric(diq160==1))) %>% #told by doctor to have prediabetes
  select(subject, dr_diabetes, dr_prediabetes)

### blood pressure and cholesterol
bp <- bp_raw %>%
  rename(subject="seqn") %>%
  replace_with_na(replace = list(bpq020 = c(7,9),
                                 bpq080 = c(7,9))) %>%
  mutate(dr_highbp=ifelse(is.na(bpq020), NA, as.numeric(bpq020==1)), ## dr told having high blood pressure
         dr_highchol=ifelse(is.na(bpq080), NA, as.numeric(bpq080==1))) %>% ## dr told having high cholesterol
  select(subject, dr_highbp, dr_highchol) 

### cardiovascular health
cardio <- cardio_raw %>%
  rename(subject="seqn") %>% 
  replace_with_na(replace = list(cdq001 = c(7,9),
                                cdq010 = c(7,9))) %>%
  mutate(pain_chest=ifelse(is.na(cdq001), NA, as.numeric(cdq001==1)), ## ever have chest pain
         short_breath=ifelse(is.na(cdq010), NA, as.numeric(cdq010==1))) %>% ## shortness of breath
  select(subject, pain_chest, short_breath)

### body measures

body <- body_raw %>%
  rename(subject="seqn", 
         weight="bmxwt", ## this is in kg
         height="bmxht", ## this is in cm
         bmi="bmxbmi", 
         waist="bmxwaist") %>% ## waist circumference in cm 
  select(subject, weight, height, bmi, waist)

### insurance
insurance <- insurance_raw %>%
  rename(subject="seqn") %>%
  replace_with_na(replace = list(hiq011 = c(7,9))) %>%
  mutate(insurance=ifelse(is.na(hiq011), NA, as.numeric(hiq011==1))) %>%
  select(subject, insurance)

### smoking
smoking <- smoking_raw %>%
  rename(subject="seqn",
         age_first_smoke = "smd030",
         now_smoke = "smq040") %>%
  replace_with_na(replace = list(age_first_smoke = c(0,777,999),
                                 now_smoke = c(7,9),
                                 smq020 = c(7,9))) %>%
  mutate(smoke100=ifelse(is.na(smq020), NA, as.numeric(smq020==1))) %>% ## have smoked at least 100 cigarette in life (this variable is the most promising in there)
  select(subject, smoke100, age_first_smoke, now_smoke)

### did not use physical activity because too many variables to work with right now


### sleep disorder
sleep <- sleep_raw %>%
  rename(subject="seqn", 
         sleep_weekday="sld012",
         sleep_weekend="sld013",
         sleepy_during_day = "slq120") %>%
  replace_with_na(replace = list(slq050 = c(7,9),
                                 sleepy_during_day = c(7,9))) %>%
  mutate(dr_sleep=ifelse(is.na(slq050), NA, as.numeric(slq050==1))) %>% ## ever told doctor had trouble sleeping
  select(subject, sleep_weekday, sleep_weekend, dr_sleep, sleepy_during_day)

### medical condition
medical <- medical_raw %>%
  rename(subject="seqn") %>%
  replace_with_na(replace = list(mcq010 = c(7.9),
                                 mcq160a = c(7,9),
                                 mcq160c = c(7,9),
                                 mcq160e = c(7,9),
                                 mcq160f = c(7,9),
                                 mcq160m = c(7,9),
                                 mcq220 = c(7,9),
                                 mcq366a = c(7,9),
                                 mcq366b = c(7,9),
                                 mcq366c = c(7,9),
                                 mcq053 = c(7,9),
                                 mcq160b = c(7,9),
                                 mcq160d = c(7,9),
                                 mcq160p = c(7,9),
                                 mcq160l = c(7,9),
                                 mcq520 = c(7,9),
                                 mcq550 = c(7,9),
                                 mcq560 = c(7,9),
                                 mcq366d = c(7,9))) %>%
  mutate(told_asthma=ifelse(is.na(mcq010), NA, as.numeric(mcq010==1)), ## ever told you had asthma
         told_arthritis=ifelse(is.na(mcq160a), NA, as.numeric(mcq160a==1)), ## ever told to have arthritis
         told_coronary=ifelse(is.na(mcq160c), NA, as.numeric(mcq160c==1)), ## ever told to have coronary heart disease
         told_attack=ifelse(is.na(mcq160e), NA, as.numeric(mcq160e==1)), ## ever told to have heart attack
         told_stroke=ifelse(is.na(mcq160f), NA, as.numeric(mcq160f==1)), ## ever told to have a stroke
         told_thyroid=ifelse(is.na(mcq160m), NA, as.numeric(mcq160m==1)), ## ever told to have thyroid problem
         told_cancer=ifelse(is.na(mcq220), NA, as.numeric(mcq220==1)), ## ever told to have cancer or magnilancy 
         told_weight=ifelse(is.na(mcq366a), NA, as.numeric(mcq366a==1)), ## ever told by health professional to lose weight
         told_exe=ifelse(is.na(mcq366b), NA, as.numeric(mcq366b==1)), ## ever told by health professional to exercise
         told_salt=ifelse(is.na(mcq366c), NA, as.numeric(mcq366c==1)), ## ever told by health professional to reduce salt in diet
         anemia_treatment=ifelse(is.na(mcq053), NA, as.numeric(mcq053==1)),#anemia treatment past 3 months
         congestive_heart=ifelse(is.na(mcq160b), NA, as.numeric(mcq160b==1)), #congestive heart failure
         angina=ifelse(is.na(mcq160d), NA, as.numeric(mcq160d==1)), #angina
         copd=ifelse(is.na(mcq160p), NA, as.numeric(mcq160p==1)), #copd
         liver_cond=ifelse(is.na(mcq160l), NA, as.numeric(mcq160l==1)), #liver
         abdominal_pain=ifelse(is.na(mcq520), NA, as.numeric(mcq520==1)), #abdominal pain
         told_gallstones=ifelse(is.na(mcq550), NA, as.numeric(mcq550==1)), #gallstones
         gallbladder_surgery=ifelse(is.na(mcq560), NA, as.numeric(mcq560==1)), #gallbladder surgery
         told_fat=ifelse(is.na(mcq366d), NA, as.numeric(mcq366d==1))) %>% ## ever told by health professional to lose fat/calories
  select(subject, told_asthma, told_arthritis, told_coronary,
         told_attack, told_stroke, told_thyroid, 
         told_cancer, told_weight, told_exe, told_salt, 
         anemia_treatment,congestive_heart, angina,copd,
         liver_cond, abdominal_pain, told_gallstones, gallbladder_surgery,
         told_fat) 

occupation <- occupation_raw %>%
  rename(subject="seqn",
         hours_worked = "ocq180") %>%
  naniar::replace_with_na (replace = list (hours_worked = c(77777, 99999))) %>%
  select(subject, hours_worked) 

dermatology <- dermatology_raw %>%
  rename(subject="seqn",
         min_outdoors_wd = "ded120",
         min_outdoors_not_wd = "ded125") %>%
  replace_with_na(replace = list(min_outdoors_wd = c (3333,7773,9999),
                              
                                 min_outdoors_not_wd = c (3333,7773,9999))) %>%
  select(subject,min_outdoors_wd, min_outdoors_not_wd)
  

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
  select(subject, min_sedentary,vigor_work,moderate_work,walk_bike,
         vigor_rec, moderate_rec)

weight_history <- weight_hist_raw%>%
  rename(subject="seqn",
         weight_self_percept = "whq030", #consider oneself as overweight
         want_lose_weight = "whq040") %>%  #want to lose weight
  
  replace_with_na(replace = list(weight_self_percept = c(7,9),
                                 
                                 want_lose_weight = c(7,9))) %>%
  select(subject, weight_self_percept, want_lose_weight)

### merge everything
final_clean <- Reduce(function(x,y) left_join(x, y, by = "subject"), 
         list(mental, demo, diabetes, bp, cardio, body,
              insurance, smoking, sleep, occupation, physical_activity)) %>%
  drop_na() ## remove all survey participants with missing data. 

write_csv(clean_data, file="final_clean.csv", na="")

db <- Reduce(function(x,y) left_join(x, y, by = "subject"), 
             list(mental, demo, diabetes, bp, cardio, body,
                  insurance, smoking, sleep, medical)) %>%
  drop_na() ## remove all survey participants with missing data. 

write_csv(db, file="db.csv", na="")

