# DATA SETUP FOR OPTIMAL WEIGHT GAIN

# load data 
df <- read_csv("data/nuMoM2b_Merged_Dataset.csv")

# inputs
kg_2_lb=2.20462

# process data 
df %>% 
  rename(visit1_BMI=`BMI`) %>%
  mutate(height=case_when(!is.na(V1BA02a) & !is.na(V1BA02b) ~ 
                            (as.numeric(V1BA02a)+as.numeric(V1BA02b))/2,
                          !is.na(V1BA02a) ~ as.numeric(V1BA02a),
                          !is.na(V1BA02b) ~ as.numeric(V1BA02b),
                          !is.na(V1BA02c) ~ as.numeric(V1BA02c),
                          TRUE ~ NA_real_)) %>%
  mutate(prepreg_weight = case_when(!is.na(V1AD01b) ~ as.numeric(V1AD01b),
                                   !is.na(V1AD01a) ~ as.numeric(V1AD01a)*kg_2_lb,
                                   TRUE ~ NA_real_)) %>%
  mutate(visit1_weight = case_when(!is.na(V1BA01_LB) ~ V1BA01_LB,
                                   !is.na(V1BA01_KG) ~ V1BA01_KG*kg_2_lb,
                                   TRUE ~ NA_real_)) %>%
  mutate(visit2_weight = case_when(!is.na(V2BA01_LB) ~ as.numeric(V2BA01_LB),
                                   !is.na(V2BA01_KG) ~ as.numeric(V2BA01_KG)*kg_2_lb,
                                   TRUE ~ NA_real_)) %>%
  mutate(visit3_weight = case_when(!is.na(V3BA01_LB) ~ as.numeric(V3BA01_LB),
                                   !is.na(V3BA01_KG) ~ V3BA01_KG*kg_2_lb,
                                   TRUE ~ NA_real_)) %>%
  mutate(final_weight = case_when(!is.na(CMAB01a2) ~ as.numeric(CMAB01a2),
                                   !is.na(CMAB01a1) ~ as.numeric(CMAB01a1)*kg_2_lb,
                                   TRUE ~ NA_real_)) %>% 
  # Edit or remove outliers 
  mutate(
    # below were identified from outliers code at bottom
    prepreg_weight = case_when(
      PublicID %in% c("04706I","03422E","09292H","06779R","05733A") ~ prepreg_weight*kg_2_lb,
      TRUE ~ prepreg_weight),
    final_weight = case_when(
      PublicID %in% c("14833S","14921V","04105L","03867M","02965R","17009J", 
                      "08210R","05064R","13087J","08928Q","09853Q","07702B") ~ final_weight/kg_2_lb,
      TRUE ~ final_weight),
    visit2_weight = case_when(
      PublicID %in% c("07176T","17314E","13190Q","04230I","03051N","03334B") ~ visit2_weight/kg_2_lb,
      TRUE ~ visit2_weight),
    visit3_weight = case_when(
      PublicID %in% c("12791S") ~ visit3_weight*kg_2_lb,
      TRUE ~ visit3_weight),
    # below were identified when looking at gam model output - 12 extreme outliers (lost weight) 
    prepreg_weight = case_when(
      PublicID %in% c("16314J","13956H","07014W","15070Q","02130W",
                      "04748P","01241S","15218K","15836H") ~ prepreg_weight/kg_2_lb,
      TRUE ~ prepreg_weight),
    prepreg_weight = case_when(
      PublicID %in% c("00919L","00955H","13238O","08785M","16251H","16281V") ~ NA_real_,
      TRUE ~ prepreg_weight),
    visit2_weight = case_when(
      PublicID %in% c("13238O") ~ NA_real_,
      TRUE ~ visit2_weight)
    ) %>%
  # Calculate BMI 
  mutate(prepreg_BMI=((prepreg_weight/kg_2_lb)/(height/100)^2)) %>%
  mutate(prepreg_time=0,
         #2.5-97.5th: 8-14 weeks
         visit1_time=V1BDATE_INT+280, 
         #2.5-97.5th: 16-22 weeks
         visit2_time=V2BDATE_INT+280, 
         #2.5-97.5th: 23-30 weeks
         visit3_time=V3BDATE_INT+280, 
         #2.5-97.5th: 30-42 weeks
         final_time=CMAB01B_INT+280) %>%
  mutate(num_measure = (as.numeric(!is.na(prepreg_weight)) + 
                          as.numeric(!is.na(visit1_weight)) + 
                          as.numeric(!is.na(visit2_weight)) + 
                          as.numeric(!is.na(visit3_weight)) +
                          as.numeric(!is.na(final_weight)))) %>% 
  # Create key predictors
  ## slope between visits
  mutate(slope_visit2 = (visit2_weight-prepreg_weight)/visit2_time,
         slope_visit1 = (visit1_weight-prepreg_weight)/visit1_time,
         slope_visit1_2 = (visit2_weight-visit1_weight)/(visit2_time-visit1_time)) %>%
  mutate(slope_visit2_per_month=slope_visit2*7*4) %>%
  # Create weight gain outcome variables
  ## ACOG guidelines - https://www.acog.org/clinical/clinical-guidance/committee-opinion/articles/2013/01/weight-gain-during-pregnancy
  mutate(max_wg_goal = case_when(prepreg_BMI < 18.5 ~ 40,
                                 prepreg_BMI < 25 ~ 35,
                                 prepreg_BMI < 30 ~ 25,
                                 prepreg_BMI >= 30 ~ 20,
                                 TRUE ~ NA_real_),
         min_wg_goal = case_when(prepreg_BMI < 18.5 ~ 28,
                                 prepreg_BMI < 25 ~ 25,
                                 prepreg_BMI < 30 ~ 15,
                                 prepreg_BMI >= 30 ~ 11,
                                 TRUE ~ NA_real_)) %>%
  ## Final weight gain measures
  mutate(final_wg = final_weight-prepreg_weight) %>% 
  mutate(final_wg_diff = final_wg-max_wg_goal,
         final_wg_prop = (final_wg-max_wg_goal)/max_wg_goal) %>%
  mutate(outcome_wg_over = as.numeric(final_wg_diff > 0),
         outcome_wg_over25 = as.numeric(final_wg_prop > 0.25),
         outcome_wg_under = as.numeric(final_wg_diff < 0)) %>%
  # create factor for outcome
  mutate(outcome_wg_over_factor = case_when(outcome_wg_over == 1 ~ "Over",
                                            outcome_wg_over == 0 ~ "Within",
                                            TRUE ~ NA_character_)) %>%
  mutate(outcome_wg_over_factor = as.factor(outcome_wg_over_factor)) %>%
  # If exceeds weight gain, when does this occur?
  mutate(max_weight_goal = max_wg_goal + prepreg_weight) %>% 
  mutate(outcome_wg_over_visit = case_when(visit1_weight > max_weight_goal ~ "Visit 1",
                                           visit2_weight > max_weight_goal ~ "Visit 2",
                                           visit3_weight > max_weight_goal ~ "Visit 3",
                                           final_weight  > max_weight_goal ~ "Final",
                                           TRUE ~ "Never exceeds")) %>% 
  # Create categories for all important variables 
  mutate(
    chrnhtn = case_when(
      ChronHTN == 2 ~ "No",
      ChronHTN == 1 ~ "Yes",
    ),
    pregestdm = case_when(
      PreGestDM == 2 ~ "No",
      PreGestDM == 1 ~ "Yes",
    ),
    age_cat = case_when(
      Age_at_V1 < 20 ~ "<20 years",
      Age_at_V1 < 30 ~ "20-29 years",
      Age_at_V1 < 36 ~ "30-35 years",
      Age_at_V1 >= 36 ~ ">35 years",
      TRUE ~ NA_character_
    ),
    race_cat = case_when(
      CRace == 1 ~ "non-Hispanic white",
      CRace == 2 ~ "non-Hispanic Black",
      CRace == 3 ~ "Hispanic",
      CRace == 4 ~ "non-Hispanic Asian",
      CRace == 5 ~ "other",
      TRUE ~ NA_character_
    ),
    prepreg_bmi_cat = case_when(
      prepreg_BMI < 18.5 ~ "Underweight (<18.5)",
      prepreg_BMI < 25 ~ "Normal weight (18.5-24.9)",
      prepreg_BMI < 30 ~ "Overweight (25.0-29.9)",
      prepreg_BMI < 35 ~ "Obese (30.0-34.9)",
      prepreg_BMI >= 35 ~ "Morbidly obese (>= 35.0)",
      TRUE ~ NA_character_
    ),
    slope_visit2_cat = case_when(
      slope_visit2 < 0 ~ "Decreasing",
      slope_visit2 < 0.5/7 ~ "<0.5 lbs/week",
      slope_visit2 < 1/7 ~ "0.5-1 lbs/week",
      slope_visit2 >= 1/7 ~ "1+ lbs/week",
      TRUE ~ NA_character_
    )
  ) -> df.w

# Create inclusion flags
df.w %>%
  # Main exclusion criteria
  mutate(include_main = case_when(
    #1. PTB
    GAwksEND < 37 ~ "1. Preterm birth",
    #2. Missing final time 
    is.na(final_time) | is.na(final_weight) ~ "2. Missing final time measure",
    #3. Final weight less than 37 weeks
    final_time < 37*7 & !is.na(final_weight) ~ "3. Final weight before 37 weeks",
    #4. Missing pre-preg BMI (weight and/or height)
    is.na(prepreg_BMI) ~ "4. Missing pre-pregnancy BMI",
    #5. Missing GA at pregnancy end
    is.na(GAwksEND) ~ "5. Missing GA at pregnancy end",
    TRUE ~ "Include")
    ) %>%
  # Exclusion criteria for Prepreg - Visit 1 slope
  mutate(include_visit1 = case_when(
    # 1. Missing visit 1 weight
    is.na(visit1_weight) ~ "1. Missing Visit 1 weight",
    # 2. Visit 1 occurs after 14 weeks
    visit1_time > 14*7 ~ "2. Visit 1 occurs after 14 weeks", 
    # 3. Exceeded weight gain by Visit 1
    outcome_wg_over_visit == "Visit 1" ~ "3. Exceeded weight gain by Visit 1",
    # 4. Visit 1 time is equal to zero
    visit1_time == 0 ~ "4. Visit 1 gestational age is zero",
    TRUE ~ "Include")
  ) %>% 
  # Exclusion criteria for Prepreg - Visit 2 slope
  mutate(include_visit2 = case_when(
    # 1. Missing visit 2 weight
    is.na(visit2_weight) ~ "1. Missing Visit 2 weight",
    # 2. Visit 2 occurs after 24 weeks
    visit2_time > 24*7 | visit2_time < 16*7 ~ "2. Visit 2 occurs before 16 or after 24 weeks", 
    # 3. Exceeded weight gain by Visit 2 
    outcome_wg_over_visit %in% c("Visit 1","Visit 2") ~ "3. Exceeded weight gain by Visit 2",
    TRUE ~ "Include") 
  ) %>% 
  # Missing demographic data 
  mutate(include_cov = case_when(
    # 1. Missing chronic htn
    is.na(ChronHTN) ~ "1. Missing chronic htn",
    # 2. Missing diabetes 
    is.na(PreGestDM) ~ "2. Missing pre-gestational diabetes",
    TRUE ~ "Include")
  ) -> df.w.excl


# Inclusion summaries
df.w.excl %>% 
  group_by(include_main) %>% 
  dplyr::summarize(n())

df.w.excl %>% 
  filter(include_main == "Include") %>%
  group_by(include_visit1) %>% 
    dplyr::summarize(n())

df.w.excl %>% 
  filter(include_main == "Include") %>%
  group_by(include_visit2) %>% 
  dplyr::summarize(n())

df.w.excl %>% 
  filter(include_main == "Include") %>%
  group_by(include_cov) %>% 
  dplyr::summarize(n())
  
# Create one hot encoded variables (if needed for training)
df.w.excl %>% 
  # Chronic hypertension and pregest diabetes
  mutate(
    chronhtn_bin = case_when(ChronHTN == 2 ~ 0,
                             ChronHTN == 1 ~ 1,
                             # <10 missing
                             is.na(ChronHTN) ~ 0),
    pregestdm_bin = case_when(PreGestDM == 2 ~ 0,
                              PreGestDM == 1 ~ 1,
                              # <10 missing
                              is.na(PreGestDM) ~ 0)
    ) %>% 
  # Age categories
  mutate(
    age_cat_under20 = ifelse(Age_at_V1<20,1,0),
    age_cat_20_29 = ifelse(Age_at_V1>=20 & Age_at_V1<30,1,0),
    age_cat_30_35 = ifelse(Age_at_V1>=30 & Age_at_V1<36,1,0),
    age_cat_over35 = ifelse(Age_at_V1>=36,1,0)
  ) %>%
  # Race categories
  mutate(
    race_nhw = ifelse(CRace==1,1,0),
    race_nhb = ifelse(CRace==2,1,0),
    race_hisp = ifelse(CRace==3,1,0),
    race_asian = ifelse(CRace==4,1,0),
    race_other = ifelse(CRace==5,1,0) # includes missing?
  ) %>%
  # BMI pre-pregnancy
  mutate(
    prepreg_bmi_under = ifelse(prepreg_BMI<18.5,1,0),
    prepreg_bmi_normal = ifelse(prepreg_BMI>=18.5 & prepreg_BMI<25,1,0),
    prepreg_bmi_over = ifelse(prepreg_BMI>=25 & prepreg_BMI<30,1,0),
    prepreg_bmi_obese = ifelse(prepreg_BMI>=30 & prepreg_BMI<35,1,0),
    prepreg_bmi_vobese = ifelse(prepreg_BMI>=35,1,0) 
  ) %>%
  # Insurance (note: very few missing, coding as zero)
  mutate(
    ins_comm_bin = case_when(Ins_Comm == 2 ~ 0,
                             Ins_Comm == 1 ~ 1,
                             is.na(Ins_Comm) ~ 0),
    ins_mil_bin = case_when(Ins_Mil == 2 ~ 0,
                            Ins_Mil == 1 ~ 1,
                            is.na(Ins_Mil) ~ 0),
    ins_gov_bin = case_when(Ins_Govt == 2 ~ 0,
                            Ins_Govt == 1 ~ 1,
                            is.na(Ins_Govt) ~ 0),
    ins_pers_bin = case_when(Ins_Pers == 2 ~ 0,
                            Ins_Pers == 1 ~ 1,
                            is.na(Ins_Pers) ~ 0),
    ins_oth_bin = case_when(Ins_Othr == 2 ~ 0,
                            Ins_Othr == 1 ~ 1,
                            is.na(Ins_Othr) ~ 0)
  ) %>%
  # Slope - Prepreg-Visit 1
  mutate(
    slope_visit1_dec = ifelse(slope_visit1 < 0,1,0),
    slope_visit1_lesshalflb = ifelse(slope_visit1 >= 0 & slope_visit1 < .5/7,1,0),
    slope_visit1_half_1lb = ifelse(slope_visit1 >= .5/7 & slope_visit1 < 1/7,1,0),
    slope_visit1_1_2lb = ifelse(slope_visit1 >= 1/7 & slope_visit1 < 2/7,1,0),
    slope_visit1_over2lb = ifelse(slope_visit1 >= 2/7,1,0),
  ) %>%
  # Slope - Prepreg-Visit 2
  mutate(
    slope_visit2_dec = ifelse(slope_visit2 < 0,1,0),
    slope_visit2_lesshalflb = ifelse(slope_visit2 >= 0 & slope_visit2 < .5/7,1,0),
    slope_visit2_half_1lb = ifelse(slope_visit2 >= .5/7 & slope_visit2 < 1/7,1,0),
    slope_visit2_over1lb = ifelse(slope_visit2 >= 1/7,1,0),
  ) %>%
  # Slope - Visit 1-Visit 2
  mutate(
    slope_visit12_dec = ifelse(slope_visit1_2 < 0,1,0),
    slope_visit12_lesshalflb = ifelse(slope_visit1_2 >= 0 & slope_visit1_2 < .5/7,1,0),
    slope_visit12_half_1lb = ifelse(slope_visit1_2 >= .5/7 & slope_visit1_2 < 1/7,1,0),
    slope_visit12_1_2lb = ifelse(slope_visit1_2 >= 1/7 & slope_visit1_2 < 2/7,1,0),
    slope_visit12_over2lb = ifelse(slope_visit1_2 >= 2/7,1,0),
  ) -> df.final


saveRDS(df.final,"data/numom2b_weightgain_allvars.rds") # with original values