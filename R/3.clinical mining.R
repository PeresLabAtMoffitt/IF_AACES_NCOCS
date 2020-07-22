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



######################################################################################## Survival plot----
# Limiting to only the case_match ids----
clin_surv <- markers_match
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

# For black and white----
myplot <- survfit(mysurv~clin_surv$race)
myplot
table(clin_surv$race)
plot(myplot, col= c("red", "blue"))

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/Survival paired ID.jpg"))
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
# dev.off()
survdiff(mysurv~clin_surv$race)

# With all patients----
clin_surv <- markers
mysurv <- Surv(time = clin_surv$timelastfu, event = clin_surv$surv_vital)
myplot <- survfit(mysurv~clin_surv$race)
# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/Survival all patients.jpg"))
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis all patient",
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
# dev.off()
survdiff(mysurv~clin_surv$race)


# Plot cumhaz for cumulative hazard or event
plot(myplot, fun="cumhaz")
plot(myplot, fun="event")

# What if we look at only Black and comparing lower vs higher BMI----all black patients
clin_surv <- markers %>% 
  mutate(BMI_fold_increase = BMI_recent/BMI_YA)

clin_surv1 <- clin_surv %>% 
  filter(race == "Black") %>% 
  mutate(BMI_fold_increase = BMI_recent/BMI_YA) %>% 
  mutate(BMI_grp = ifelse(BMI_fold_increase >=1.444444, "higher", "lower")) %>%  # 1.444444 is the median
  mutate(BMI_grp = factor(BMI_grp, levels = c("lower", "higher")))
median(clin_surv1$BMI_fold_increase, na.rm = TRUE)

myplot <- survfit(Surv(time = clin_surv1$timelastfu, event = clin_surv1$surv_vital)~clin_surv1$BMI_grp) 
ggsurvplot(myplot, data = clin_surv1,
           title = "Survival analysis on matched patient comparing BMI fold increase",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "grouped by BMI (median)", legend.labs = c("higher", "lower"),
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
  mutate(BMI_grp = ifelse(BMI_recent >=30.08423, "higher", "lower")) %>%  # 30.08423 is the median
  mutate(BMI_grp = factor(BMI_grp, levels = c("lower", "higher")))

myplot <- survfit(Surv(time = clin_surv1$timelastfu, event = clin_surv1$surv_vital)~clin_surv1$BMI_grp) 
ggsurvplot(myplot, data = clin_surv1,
           title = "Survival analysis on matched patient comparing recent BMI",
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

# BMI_YA
median(clin_surv1$BMI_YA, na.rm = TRUE)
clin_surv1 <- clin_surv %>% 
  filter(race == "Black") %>% 
  mutate(BMI_grp = ifelse(BMI_YA >=21.03354, "higher", "lower")) %>%  # 21.03354 is the median
  mutate(BMI_grp = factor(BMI_grp, levels = c("lower", "higher")))

myplot <- survfit(Surv(time = clin_surv1$timelastfu, event = clin_surv1$surv_vital)~clin_surv1$BMI_grp) 
ggsurvplot(myplot, data = clin_surv1,
           title = "Survival analysis on matched patient comparing recent BMI young",
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




# per BMI----
# Black
clin_surv1 <- clin_surv %>% 
  filter(race == "Black") %>% 
  mutate(BMI_classification = case_when(
    BMI_recent < 18.5	~ "underweight",
    BMI_recent >=18.5 & BMI_recent <25 ~ "normal",
    BMI_recent >=25.0 & BMI_recent <30 ~ "overweight",
    BMI_recent >=30.0 & BMI_recent <35 ~ "obesity I",
    BMI_recent >=35.0 & BMI_recent <40 ~ "obesity II",
    BMI_recent >= 40.0 ~ "obesity III"
  ))
myplot <- survfit(Surv(time = clin_surv1$timelastfu, event = clin_surv1$surv_vital)~clin_surv1$BMI_classification) 
ggsurvplot(myplot, data = clin_surv1,
           title = "Survival analysis on Black population comparing recent BMI",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "BMI classification", # legend.labs = c("higher", "lower"),
           pval = TRUE, pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           # Add risk table
           risk.table = TRUE,
           tables.height = 0.2,
           # tables.theme = theme_cleantable(),
           risk.table.title = "Risk table",
           # Color
           # palette = c("#E7B800", "#2E9FDF"),
           conf.int = FALSE
)

# BMI	Classification[16]
# < 18.5	underweight
# 18.5–24.9	normal weight
# 25.0–29.9	overweight
# 30.0–34.9	class I obesity
# 35.0–39.9	class II obesity
# ≥ 40.0	  class III obesity  

# Black vs White
clin_surv <- markers_match
clin_surv1 <- clin_surv %>% 
  mutate(BMI_classification = case_when(
    BMI_recent < 18.5	~ "underweight",
    BMI_recent >=18.5 & BMI_recent <25 ~ "normal",
    BMI_recent >=25.0 & BMI_recent <30 ~ "overweight",
    BMI_recent >=30.0 & BMI_recent <35 ~ "obesity I",
    BMI_recent >=35.0 & BMI_recent <40 ~ "obesity II",
    BMI_recent >= 40.0 ~ "obesity III"
  ))
myplot <- survfit(Surv(time = clin_surv1$timelastfu, event = clin_surv1$surv_vital)~clin_surv1$BMI_classification+clin_surv1$race) 
ggsurv <- ggsurvplot(myplot, data = clin_surv1,
           title = "Survival analysis on Black population comparing recent BMI",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)",
           legend.title = "BMI classification",# legend.labs = c("higher", "lower"),
           pval = TRUE, pval.coord = c(2100, .53),
           surv.median.line = c("hv"),
           # Add risk table
           risk.table = TRUE, tables.height = 0.2, # tables.theme = theme_cleantable(),
           risk.table.title = "Risk table",
           # Color
           # palette = c("#E7B800", "#2E9FDF"),
           conf.int = FALSE
)
# Faceting survival curves
curv_facet <- ggsurv$plot + facet_grid( ~ race)
curv_facet

clin_surv1 <- clin_surv %>% 
  mutate(BMI_classification = case_when(
    BMI_recent < 18.5	~ "underweight",
    BMI_recent >=18.5 & BMI_recent <25 ~ "normal",
    BMI_recent >=25.0 & BMI_recent <30 ~ "overweight",
    BMI_recent >=30.0 & BMI_recent <35 ~ "obesity I",
    BMI_recent >=35.0 & BMI_recent <40 ~ "obesity II",
    BMI_recent >= 40.0 ~ "obesity III"
  ))

myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~BMI_classification + race, data = clin_surv1) 
ggsurvplot(myplot, data = clin_surv1,
                     title = "Survival analysis on Black vs White population comparing recent BMI",
                     font.main = c(16, "bold", "black"),
                     xlab = "Time (days)",
                     legend.title = "BMI classification",
                     # legend.labs = c("normal", "obesity I", "obesity II", "obesity III", "overweight", "underweight"),
                     # panel.labs = list(BMI_classification = c("underweight", "normal", "overweight", "obesity I", "obesity II", "obesity III")),
                     pval = TRUE, pval.coord = c(2100, .53),
                     surv.median.line = c("hv"),
                     # Add risk table
                     risk.table = TRUE, tables.height = 0.2, # tables.theme = theme_cleantable(),
                     risk.table.title = "Risk table",
                     # Color
                     # palette = "jco",
                     color = "BMI_classification",
                     conf.int = FALSE
) %>% 
  .$plot +theme_bw() + 
  theme (legend.position = "right")+
  facet_grid(race ~ .)



# per stage----
clin_surv <- markers_match
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~stage + race, data = clin_surv) 
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on Black vs White population comparing recent BMI",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)",
           legend.title = "Stage",
           pval = TRUE, pval.coord = c(2100, .53),
           surv.median.line = c("hv"),
           # Color
           # palette = "jco",
           color = "stage",
           conf.int = FALSE
) %>% 
  .$plot +theme_bw() + 
  theme (legend.position = "right")+
  facet_grid(race ~ .)

# Per histology
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~histology + race, data = clin_surv) 
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on Black vs White population comparing recent BMI",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)",
           legend.title = "Histology",
           pval = TRUE, pval.coord = c(2100, .53),
           surv.median.line = c("hv"),
           # Color
           # palette = "jco",
           color = "histology",
           conf.int = FALSE
) %>% 
  .$plot +theme_bw() + 
  theme (legend.position = "right")+
  facet_grid(race ~ .)

# Per behavior
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~behavior + race, data = clin_surv) 
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on Black vs White population comparing recent BMI",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)",
           legend.title = "Behavior",
           pval = TRUE, pval.coord = c(2100, .53),
           surv.median.line = c("hv"),
           # Color
           # palette = "jco",
           color = "behavior",
           conf.int = FALSE
) %>% 
  .$plot +theme_bw() + 
  theme (legend.position = "right")+
  facet_grid(race ~ .)

# Per grade
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~grade + race, data = clin_surv) 
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on Black vs White population comparing recent BMI",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)",
           legend.title = "Grade",
           pval = TRUE, pval.coord = c(2100, .53),
           surv.median.line = c("hv"),
           # Color
           # palette = "jco",
           color = "grade",
           conf.int = FALSE
) %>% 
  .$plot +theme_bw() + 
  theme (legend.position = "right")+
  facet_grid(race ~ .)

# Per histotype
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~histotype + race, data = clin_surv) 
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on Black vs White population comparing recent BMI",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)",
           legend.title = "Histotype",
           pval = TRUE, pval.coord = c(2100, .53),
           surv.median.line = c("hv"),
           # Color
           # palette = "jco",
           color = "histotype",
           conf.int = FALSE
) %>% 
  .$plot +theme_bw() + 
  theme (legend.position = "right")+
  facet_grid(race ~ .)

# Per education
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~education + race, data = clin_surv) 
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on Black vs White population comparing recent BMI",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)",
           legend.title = "Education",
           pval = TRUE, pval.coord = c(2100, .53),
           surv.median.line = c("hv"),
           # Color
           # palette = "jco",
           color = "education",
           conf.int = FALSE
) %>% 
  .$plot +theme_bw() + 
  theme (legend.position = "right")+
  facet_grid(race ~ .)















# Cleaning
rm(clin_surv1, clin_surv, myplot, mysurv)
