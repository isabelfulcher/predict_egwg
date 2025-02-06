################################
########## LOAD DATA ###########
################################

preds_gam <- readRDS("output/gam/preds.rds")
preds_lr_cat <- readRDS("output/lr_cat/preds.rds")
preds_lr_cont <- readRDS("output/lr_cont/preds.rds")
preds_rf <- readRDS("output/rf/preds.rds")

readRDS("output/gam/auc.rds")
readRDS("output/lr_cat/auc.rds")
readRDS("output/lr_cont/auc.rds")
readRDS("output/rf/auc.rds")

roc_lr_cont = pROC::roc(response = preds_lr_cont$obs_bin, 
                        predictor = preds_lr_cont$pred)

roc_lr_cat = pROC::roc(response = preds_lr_cat$obs_bin, 
                        predictor = preds_lr_cat$pred)

roc_gam = pROC::roc(response = preds_gam$obs_bin, 
                        predictor = preds_gam$pred)

roc_rf = pROC::roc(response = preds_rf$obs_bin, 
                        predictor = preds_rf$pred)

pROC::roc.test(roc_lr_cont,
               roc_lr_cat, 
               method = "delong", 
               boot.n = 1000, 
               paired = T)

pROC::roc.test(roc_gam,
               roc_lr_cont,
               method = "delong", 
               boot.n = 1000, 
               paired = T)

pROC::roc.test(roc_lr_cont,
               roc_rf, 
               method = "delong", 
               boot.n = 1000, 
               paired = T)

# PLOT ALL CURVES
ggplot() + 
  geom_roc(data=preds_gam, aes(m=pred,d=obs_bin),n.cuts=0,color="lightblue") +  
  geom_roc(data=preds_lr_cont, aes(m=pred,d=obs_bin),color="purple",n.cuts=0) +  
  theme_bw() + 
  xlab("false positives (1-spec)") + 
  ylab("true positives (sens)") -> plot_roc
