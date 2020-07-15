# Start with clinical mining


# Proportion cancer characteristic between Black and White
clinical_data %>% 
  group_by(cancersite, race) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=cancersite, y=count, fill=race))+
  geom_bar(stat = "identity", position=position_dodge2(preserve = "single"))+
  theme_minimal()
 
clinical_data %>% 
  group_by(histology, race) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=histology, y=count, fill=race))+
  geom_bar(stat = "identity", position=position_dodge2(preserve = "single"))+
  theme_minimal()+
  coord_flip()

# clinical_data %>%  # behavior doesn't seem relavant as almost all are invasive
#   group_by(behavior, race) %>% 
#   summarise(count=n()) %>% 
#   ggplot(aes(x=behavior, y=count, fill=race))+
#   geom_bar(stat = "identity", position=position_dodge2(preserve = "single"))+
#   theme_minimal()

clinical_data %>% 
  group_by(grade, race) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=grade, y=count, fill=race))+
  geom_bar(stat = "identity", position=position_dodge2(preserve = "single"))+
  theme_minimal()+
  coord_flip()

# clinical_data %>%  # histotype doesn't seem relavant as almost all are high grade serous
#   group_by(histotype, race) %>% 
#   summarise(count=n()) %>% 
#   ggplot(aes(x=histotype, y=count, fill=race))+
#   geom_bar(stat = "identity", position=position_dodge2(preserve = "single"))+
#   theme_minimal()+
#   coord_flip()

clinical_data %>% 
  group_by(education, race) %>% 
  summarise(count=n()) %>% 
  mutate(percent=(count/sum(count)*100)) %>% 
  ggplot(aes(x=education, y=percent, fill=race))+
  geom_bar(stat = "identity", position=position_dodge2(preserve = "single"))+
  theme_minimal()+
  coord_flip()

clinical_data %>% 
  group_by(married, race) %>% 
  summarise(count=n()) %>% 
  mutate(percent=(count/sum(count)*100)) %>% 
  ggplot(aes(x=married, y=percent, fill=race))+
  geom_bar(stat = "identity", position=position_dodge2(preserve = "single"))+
  theme_minimal()+
  coord_flip()

clinical_data %>% 
  group_by(pregever, race) %>% 
  summarise(count=n()) %>% 
  mutate(percent=(count/sum(count)*100)) %>% 
  ggplot(aes(x=pregever, y=percent, fill=race))+
  geom_bar(stat = "identity", position=position_dodge2(preserve = "single"))+
  theme_minimal()+
  coord_flip()

# BMI----
clinical_data %>% 
  ggplot(aes(BMI_recent, color=race))+
  geom_freqpoly()+
  theme_minimal()
clinical_data %>% 
  ggplot(aes(BMI_YA, color=race))+
  geom_freqpoly()+
  theme_minimal()
clinical_data %>% 
  mutate(BMI_fold_increase = BMI_recent/BMI_YA) %>% 
  ggplot(aes(BMI_fold_increase, color=race))+
  geom_freqpoly()+
  theme_minimal()

clinical_data$range_BMI <- as.factor(findInterval(clinical_data$BMI_recent,c(0,25,30,35,40,45,50,55,60,65,70,75,80)))
levels(clinical_data$range_BMI) <-  
  c("<26","26-30","31-35","36-40","41-45","46-50","51-55","56-60","61-65","65-70", "70-75", 
    "75-80", ">80")
clinical_data %>% 
  group_by(range_BMI, race) %>% 
  summarise(count=n()) %>% 
  mutate(percent=(count/sum(count)*100)) %>% 
  ggplot(aes(x=range_BMI, y=percent, fill=race))+
  geom_bar(stat = "identity", position=position_dodge2(preserve = "single"))+
  theme_minimal()+
  coord_flip()


# Age at diag between black and white----
ggplot(clinical_data, aes(x=race, y=refage, color=race))+
  geom_violin(scale = "count") # choose count to refect areas are scaled proportionally to the number of observations.



# Survival plot----##############################################################################################################
# Limiting to only the case_match ids
clin_surv <- clinical_data %>% 
  filter(!is.na(pair_id))
# no event should be 0, when event happened should be 1
# so will use the var surv_vital that I created alive=0, death=1
mysurv <- Surv(time = clin_surv$timelastfu, event = clin_surv$surv_vital)
mysurv
median(clin_surv$timelastfu)# do not use this value as the median survival is tbe time at the survivalship aka function = .5

# Plot
# survfit(mysurv~1, type= "kaplan-meier", conf.type = "log-log") # if log
# can do fleming-harrington or fh2 with log-log too
# For whole population, default mysurv~1, type= "kaplan-meier")
myplot <- survfit(mysurv~1) 
myplot
plot(myplot)
plot(myplot, conf.int = "none") # without confidence interval
abline(h=0.5)
abline(v=1576) # Here put the median value in the my surv
# can get a restricted mean 
print(myplot,print.mean=TRUE)

# For black and white
myplot <- survfit(mysurv~clin_surv$race)
myplot
table(clin_surv$race)
plot(myplot, col= c("red", "blue"))
surv_pvalue(myplot)

ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on matched patient",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "Race", legend.labs = c("Black", "White"),
           pval = TRUE, pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           # Add risk table
           risk.table = TRUE,
           tables.height = 0.2,
           # tables.theme = theme_cleantable(),
           risk.table.title = "Risk table",
           # Color
           # palette = c("#E7B800", "#2E9FDF"),
           conf.int = TRUE
           )

survdiff(mysurv~clin_surv$race) # Would be 0.02 if take all data


# Plot cumhaz for cumulative hazard or event
plot(myplot, fun="cumhaz")
plot(myplot, fun="event")

# What if we look at only Black and comparing lower vs higher BMI
median(clin_surv1$BMI_fold_increase, na.rm = TRUE)
clin_surv1 <- clin_surv %>% 
  filter(race == "Black") %>% 
  mutate(BMI_fold_increase = BMI_recent/BMI_YA) %>% 
  mutate(BMI_grp = ifelse(BMI_fold_increase >=1.454545, "higher", "lower")) %>%  # 1.454545 is the median
  mutate(BMI_grp = factor(BMI_grp, levels = c("lower", "higher")))

myplot <- survfit(Surv(time = clin_surv1$timelastfu, event = clin_surv1$surv_vital)~clin_surv1$BMI_grp) 
ggsurvplot(myplot, data = clin_surv1,
           title = "Survival analysis on matched patient",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "Race", legend.labs = c("higher", "lower"),
           pval = TRUE, pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           # Add risk table
           risk.table = TRUE,
           tables.height = 0.2,
           # tables.theme = theme_cleantable(),
           risk.table.title = "Risk table",
           # Color
           # palette = c("#E7B800", "#2E9FDF"),
           conf.int = TRUE
)

# BMI_recent
median(clin_surv1$BMI_recent, na.rm = TRUE)
clin_surv1 <- clin_surv %>% 
  filter(race == "Black") %>% 
  mutate(BMI_grp = ifelse(BMI_recent >=30.66646, "higher", "lower")) %>%  # 30.66646 is the median
  mutate(BMI_grp = factor(BMI_grp, levels = c("lower", "higher")))

myplot <- survfit(Surv(time = clin_surv1$timelastfu, event = clin_surv1$surv_vital)~clin_surv1$BMI_grp) 
ggsurvplot(myplot, data = clin_surv1,
           title = "Survival analysis on matched patient",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "Race", # legend.labs = c("higher", "lower"),
           pval = TRUE, pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           # Add risk table
           risk.table = TRUE,
           tables.height = 0.2,
           # tables.theme = theme_cleantable(),
           risk.table.title = "Risk table",
           # Color
           # palette = c("#E7B800", "#2E9FDF"),
           conf.int = TRUE
)

rm(clin_surv1, clin_surv, myplot, mysurv)
