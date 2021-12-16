
### load library
library(tidyverse)
library(magrittr)
library(readr)
library(haven) 
library(naniar) 


### start reading (raw) datasets in


demo_raw <- read_xpt(file = "data/raw/P_DEMO.XPT"); names(demo_raw) %<>% tolower #demographics

acculturation_raw <- read_xpt(file = "data/raw/P_ACQ.XPT"); names(acculturation_raw) %<>% tolower

audiometry_raw <- read_xpt(file = "data/raw/P_AUQ.XPT"); names(audiometry_raw) %<>% tolower

cardio_raw <-read_xpt(file = "data/raw/P_CDQ.XPT"); names(cardio_raw) %<>% tolower

health_status_raw <- read_xpt(file = "data/raw/P_HSQ.XPT"); names(health_status_raw) %<>% tolower

dermatology_raw <- read_xpt(file = "data/raw/P_CDQ.XPT"); names(dermatology_raw) %<>% tolower

diet_raw <- read_xpt(file = "data/raw/P_DBQ.XPT"); names(diet_raw) %<>% tolower

childhood_raw <- read_xpt(file = "data/raw/P_ECQ.XPT"); names(childhood_raw) %<>% tolower

insurance_raw <- read_xpt(file = "data/raw/P_HIQ.XPT"); names(insurance_raw) %<>% tolower

hepatitis_raw <- read_xpt(file = "data/raw/P_HEQ.XPT"); names(hepatitis_raw) %<>% tolower

hospitalization_raw <- read_xpt(file = "data/raw/P_HUQ.XPT"); names(hospitalization_raw) %<>% tolower

immunization_raw <- read_xpt(file = "data/raw/P_IMQ.XPT"); names(immunization_raw) %<>% tolower

kidney_raw <- read_xpt(file = "data/raw/P_KIQ_U.XPT"); names(kidney_raw) %<>% tolower

oral_raw <- read_xpt(file = "data/raw/P_OHQ.XPT"); names(oral_raw) %<>% tolower

osteoporosis_raw <- read_xpt(file = "data/raw/P_OSQ.XPT"); names(osteoporosis_raw) %<>% tolower

pesticide_raw <- read_xpt(file = "data/raw/P_PUQMEC.XPT"); names(pesticide_raw) %<>% tolower

prescription_raw <- read_xpt(file = "data/raw/P_RXQ_RX.XPT"); names(prescription_raw) %<>% tolower

aspirin_raw <- read_xpt(file = "data/raw/P_RXQASA.XPT"); names(aspirin_raw) %<>% tolower

reproductive_raw <- read_xpt(file = "data/raw/P_RHQ.XPT"); names (reproductive_raw) %<>% tolower

tobacco_raw <- read_xpt(file = "data/raw/P_SMQRTU.XPT"); names(tobacco_raw) %<>% tolower

secondhand_smoke_raw <- read_xpt(file = "data/raw/P_SMQSHS.XPT"); names(secondhand_smoke_raw) %<>% tolower

toxicant_raw <- read_xpt(file = "data/raw/P_VTQ.XPT"); names(toxicant_raw) %<>% tolower

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
  
  select(subject, female, age, nonhisp_white, 
         never_married, ratio_income, 
         born_us, edu_college) 


  
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
  #rename(subject="seqn") %>%
  mutate(mental_score=rowSums(cbind(dpq010, dpq020, dpq030, dpq040, dpq050, 
                              dpq060, dpq070, dpq080, dpq090, dpq100))) %>% 
  
  select(seqn, mental_score) 


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
         sleep_weekend="sld013") %>%
  replace_with_na(replace = list(slq050 = c(7,9))) %>%
  mutate(dr_sleep=ifelse(is.na(slq050), NA, as.numeric(slq050==1))) %>% ## told doctor about trouble sleeping
  select(subject, sleep_weekday, sleep_weekend, dr_sleep)

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
                                 mcq366d = c(7,9))) %>%
  mutate(told_asthma=ifelse(is.na(mcq010), NA, as.numeric(mcq010==1)), ## ever told you had asthma
         told_arthritis=ifelse(is.na(mcq160a), NA, as.numeric(mcq160a==1)), ## ever told to have arthritis
         told_coronary=ifelse(is.na(mcq160c), NA, as.numeric(mcq160c==1)), ## ever told to have coronary heart disease
         told_attack=ifelse(is.na(mcq160e), NA, as.numeric(mcq160e==1)), ## ever told to have heart attack
         told_stroke=ifelse(is.na(mcq160f), NA, as.numeric(mcq160f==1)), ## ever told to have a stroke
         told_thyroid=ifelse(is.na(mcq160m), NA, as.numeric(mcq160m==1)), ## ever told to have thyroid problem
         told_cancer=ifelse(is.na(mcq220), NA, as.numeric(mcq220==1)), ## ever told to have cancer or malignancy 
         told_weight=ifelse(is.na(mcq366a), NA, as.numeric(mcq366a==1)), ## ever told by health professional to lose weight
         told_exe=ifelse(is.na(mcq366b), NA, as.numeric(mcq366b==1)), ## ever told by health professional to exercise
         told_salt=ifelse(is.na(mcq366c), NA, as.numeric(mcq366c==1)), ## ever told by health professional to reduce salt in diet
         told_fat=ifelse(is.na(mcq366d), NA, as.numeric(mcq366d==1))) %>% ## ever told by health professional to lose fat/calories
  select(subject, told_asthma, told_arthritis, told_coronary,
         told_attack, told_stroke, told_thyroid, 
         told_cancer, told_weight, told_exe, told_salt, told_fat) 

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
data_all <- Reduce(function(x,y) left_join(x, y, by = "seqn"), 
         list(acculturation_raw, audiometry_raw, cardio_raw, health_status_raw, 
              dermatology_raw, diet_raw, childhood_raw, insurance_raw, 
              hepatitis_raw, hospitalization_raw, 
              immunization_raw, kidney_raw, oral_raw, osteoporosis_raw, 
              pesticide_raw, prescription_raw, aspirin_raw, reproductive_raw,
              tobacco_raw, secondhand_smoke_raw, toxicant_raw, 
              mental, demo_raw, smoking_raw, sleep_raw, medical_raw, 
              diabetes_raw, bp_raw, occupation_raw, 
              physical_raw, weight_hist_raw)) %>%
  na.omit()
  
  drop_na() ## remove all survey participants with missing data. 
  
  
join_1 <- merge(acculturation_raw, audiometry_raw, cardio_raw, health_status_raw, 
                     dermatology_raw, diet_raw, childhood_raw, insurance_raw, 
                     hepatitis_raw, hospitalization_raw, 
                     immunization_raw, kidney_raw, oral_raw, osteoporosis_raw, 
                     pesticide_raw, prescription_raw, aspirin_raw, reproductive_raw,
                     tobacco_raw, secondhand_smoke_raw, toxicant_raw, 
                     mental, demo_raw, smoking_raw, sleep_raw, medical_raw, 
                     diabetes_raw, bp_raw, occupation_raw, 
                     physical_raw, weight_hist_raw, by = "seqn", all = TRUE)

write_csv(final_clean_data, file="final_clean_data.csv", na="")


