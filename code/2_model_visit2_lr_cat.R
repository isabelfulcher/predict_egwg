###############################################
##### LOGISTIC REGRESSION WITH CAT TERM #######
###############################################

library(caret)
library(tidyverse)
library(plotROC)
library(cowplot)
library(pROC)
library(predtools)

################################
############ INPUTS ############
################################

# unique model name
model_name = "lr_cat"

# load data
data <- readRDS("data/numom2b_weightgain_allvars.rds")

# load helper functions
source("code/functions.R")

################################
######## PREPARE DATA ##########
################################

# Apply exclusion criteria
df <- data %>% filter(include_main == "Include" & 
                        include_visit2 == "Include" & 
                        include_cov == "Include")

# Create test and train for this population
set.seed(123)
train_ind <- createDataPartition(df$outcome_wg_over_factor, 
                                 p = .8, 
                                 list = FALSE, 
                                 times = 1)

df_train <- df[ train_ind,]
df_test  <- df[-train_ind,]

################################
######### TRAIN MODEL ##########
################################

# Fit logistic regresion model # 
lr <- glm(outcome_wg_over ~ 
            slope_visit2_dec +
            slope_visit2_half_1lb +
            slope_visit2_1_2lb + 
            slope_visit2_over2lb +
            age_cat_under20 + 
            age_cat_30_35 + 
            age_cat_over35 + 
            prepreg_bmi_under + 
            prepreg_bmi_over + 
            prepreg_bmi_obese + 
            prepreg_bmi_vobese + 
            chronhtn_bin + 
            pregestdm_bin,
           family = "binomial",
           data = df_train)

# Coefficients #
data.frame(est = coefficients(lr),
           se = summary(lr)$coefficients[,2]) %>%
  mutate(or_est = round(exp(est),2),
         ci_low = round(exp(est + qnorm(.025)*se),2),
         ci_up = round(exp(est + qnorm(.975)*se),2),
         pval = round(summary(lr)$coefficients[,4])) %>%
  dplyr::select(-est,-se) -> results_coeff

# Create results data frame #  
pred <- predict(object=lr,newdata=df_test,type="response")
obs <- ifelse(df_test$outcome_wg_over==1,"Over","Within")
results_pred <- data.frame(id=df_test$PublicID,
                           bmi=df_test$prepreg_bmi_cat,
                           race=df_test$CRace,
                           obs=obs,
                           obs_bin=ifelse(obs=="Over",1,0),
                           pred=pred)

################################
####### GENERATE PLOTS #########
################################

# ROC #
ggplot(results_pred, aes(m=pred,d=obs_bin)) + 
  geom_roc(n.cuts=0) +  
  theme_bw() + 
  xlab("false positives (1-spec)") + 
  ylab("true positives (sens)") -> plot_roc

# CALIBRATION # 
calibration_plot(data = results_pred, 
                 obs = "obs_bin", 
                 pred = "pred", 
                 title = "") -> plot_calibration


################################
######### PERFORMANCE ##########
################################

# OVERALL #
auc = pROC::ci.auc(df_test$outcome_wg_over, pred, conf.level = 0.95)[c(2,1,3)]

# BY BMI CAT # 
auc_by_bmi = auc_by_var(data=results_pred,
                        var_name="bmi",
                        ref_cat="Normal weight (18.5-24.9)")

# BY RACE CAT # 
auc_by_race = auc_by_var(data=results_pred,
                         var_name="race",
                         ref_cat=1) #Non-Hispanic White


################################
############# SAVE #############
################################

saveRDS(results_coeff,paste0("output/",model_name,"/coeff.rds"))
saveRDS(results_pred,paste0("output/",model_name,"/preds.rds"))
saveRDS(auc,paste0("output/",model_name,"/auc.rds"))
saveRDS(auc_by_bmi,paste0("output/",model_name,"/auc_bmi.rds"))
saveRDS(auc_by_race,paste0("output/",model_name,"/auc_race.rds"))

ggsave(paste0("output/",model_name,"/roc.png"),plot_roc)
ggsave(paste0("output/",model_name,"/calibration.png"),plot_calibration$calibration_plot)
