###############################################
############ RANDOM FOREST MODEL ##############
###############################################

library(caret)
library(tidyverse)
library(plotROC)
library(cowplot)
library(pROC)
library(predtools)
library(randomForest)

################################
############ INPUTS ############
################################

# unique model name
model_name = "rf"

# load data
data <- readRDS("data/numom2b_weightgain_allvars.rds")

# load helper functions
source("code/functions.R")

################################
######## PREPARE DATA ##########
################################

# Apply exclusion criteria
df <- data %>% 
  filter(include_main == "Include" & 
           include_visit2 == "Include" & 
           include_cov == "Include") %>%
  dplyr::select(slope_visit2,
                age_cat_under20,
                age_cat_30_35,
                age_cat_over35,
                prepreg_bmi_under,
                prepreg_bmi_over,
                prepreg_bmi_obese,
                prepreg_bmi_vobese,
                chronhtn_bin,
                pregestdm_bin,
                outcome_wg_over,
                outcome_wg_over_factor)

# Create test and train for this population
set.seed(123)
train_ind <- createDataPartition(df$outcome_wg_over_factor, 
                                 p = .8, 
                                 list = FALSE, 
                                 times = 1)

df_train <- df[ train_ind,] %>% dplyr::select(-outcome_wg_over_factor) 
df_test  <- df[-train_ind,] %>% dplyr::select(-outcome_wg_over_factor)

################################
######### TRAIN MODEL ##########
################################

# Fit random forest model # 
rf <- randomForest(
  outcome_wg_over ~ .,
  data=as.matrix(df_train)
)

# Create results data frame #  
pred <- predict(rf, newdata=df_test[-11])
obs <- ifelse(df_test$outcome_wg_over==1,"Over","Within")
results_pred <- data.frame(obs=obs,
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
auc = pROC::ci.auc(df_test$outcome_wg_over,
                   pred,
                   conf.level = 0.95)[c(2,1,3)]


################################
############# SAVE #############
################################

saveRDS(results_coeff,paste0("output/",model_name,"/coeff.rds"))
saveRDS(results_pred,paste0("output/",model_name,"/preds.rds"))
saveRDS(auc,paste0("output/",model_name,"/auc.rds"))

ggsave(paste0("output/",model_name,"/roc.png"),plot_roc)
ggsave(paste0("output/",model_name,"/calibration.png"),plot_calibration$calibration_plot)
