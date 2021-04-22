library(survival)
library(survminer)
library(tidyverse)

clin_summ <- read_rds('Peres IF_AACES_NCOCS full data 12092020.rds')
################################################################################
#Only look at high grade serous rois
data = clin_summ %>% filter(histotype == 'high-grade serous' & is.na(TMA))


#Find the optimal cut point for cd3
non_zero = data %>% filter(percent_cd3_opal_650_positive_cells > 0) %>%
  select(suid, timelastfu_new, vitalstatus_new, percent_cd3_opal_650_positive_cells) #-- data without 0

#Optimal cut point that is not adjusted for race, and does not address the #-- done on the measures
#repeated measures
cut = surv_cutpoint(data = non_zero, time = 'timelastfu_new', event = 'vitalstatus_new', 
                    variables = "percent_cd3_opal_650_positive_cells")
cut
#Optimal cut point that is not adjusted for race, and does not address the #-- done on the average ALL P+I
#averages across the rois
non_zero_averaged = data %>% filter(percent_cd3_opal_650_positive_cells > 0) %>%
  select(suid, timelastfu_new, vitalstatus_new, percent_cd3_opal_650_positive_cells) %>%
  group_by(suid) %>%
  summarize(percent_cd3_opal_650_positive_cells = mean(percent_cd3_opal_650_positive_cells)) %>%
  inner_join(data %>% select(suid, timelastfu_new, vitalstatus_new)) %>%
  filter(!duplicated(.))

cut_average = surv_cutpoint(data = non_zero_averaged, time = 'timelastfu_new', event = 'vitalstatus_new', 
                            variables = "percent_cd3_opal_650_positive_cells")

#These cutpoints are very different.
cut
cut_average

#################################################################################

#Below is the rigorous way to do it. We did this because surv_cutpoint can
#not handle repeated measure or adjust for covariates

non_zero = data %>% filter(percent_cd3_opal_650_positive_cells > 0) %>%
  select(suid, race, timelastfu_new, vitalstatus_new, percent_cd3_opal_650_positive_cells)

#Start grid search by defining a values to loop through, here I am considering the deciles
cutpoint = quantile(non_zero$percent_cd3_opal_650_positive_cells, seq(0.1,0.9,0.01))

#The rownames correspond the quantile
results = data.frame(cutpoint = cutpoint, p.value = '')
for(i in 1:length(cutpoint)){
  temp = data %>% mutate(cat_cd3 = case_when(percent_cd3_opal_650_positive_cells == 0 ~ #-- function to take each survival p value between high and low and none 
                                               'None',
                                             percent_cd3_opal_650_positive_cells > cutpoint[i] ~
                                               'High',
                                             percent_cd3_opal_650_positive_cells <= cutpoint[i] ~
                                               'Low'),
                         cat_cd3 = factor(cat_cd3, levels = c('None', 'Low', 'High')))
  
  #The cluster option treats the patients as fixed effect
  surv_model_fixed_effect = coxph(Surv(timelastfu_new, vitalstatus_new) ~ race + cat_cd3 + 
                                    cluster(suid), data = temp)
  results$p.value[i] = coefficients(summary(surv_model_fixed_effect))[1,6]
}

#Best cut point is the smallest effect due to race #--extract the lowest p value and create the group corresponding
opt_cut = results$cutpoint[which.min(results$p.value)]

opt_cut_data = data %>% mutate(cat_cd3 = case_when(percent_cd3_opal_650_positive_cells == 0 ~
                                                     'None',
                                                   percent_cd3_opal_650_positive_cells > opt_cut ~
                                                     'High',
                                                   percent_cd3_opal_650_positive_cells <= opt_cut ~
                                                     'Low'),
                               cat_cd3 = factor(cat_cd3, levels = c('None', 'Low', 'High'))) %>%
  data.frame()

final_model = coxph(Surv(timelastfu_new, vitalstatus_new) ~ race + cat_cd3 + 
                      cluster(suid), data = opt_cut_data)

#Plotting results for adjusted for cd3 abundance
ggadjustedcurves(fit = final_model, variable = 'race', data =  opt_cut_data)



##################################################################################
#Additional information about frailty models

#The fraility model treats the patients as a random sample
surv_model_fraility = coxph(Surv(timelastfu_new, vitalstatus_new) ~ race + cat_cd3 + 
                              frailty(suid), data = opt_cut_data)

#the coxme package is another package to do these (It seems to be prefered to 
#coxph function), but I was never able to get the answers to agree
library(coxme)
surv_model_fraility_coxme <- coxme(Surv(timelastfu_new, vitalstatus_new) ~ race + cat_cd3 + (1|suid),
                                   data = opt_cut_data)
surv_model_fraility_coxme

surv_model_fraility_coxme1 <- coxme(Surv(timelastfu_new, vitalstatus_new) ~ race + cat_cd3 + (1| suid /race),
                                   data = opt_cut_data)
surv_model_fraility_coxme1




