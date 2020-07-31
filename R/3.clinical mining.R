################################################################################################################# I ### Basic plots----


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
  mutate(percent=(count/NROW(!is.na(clinical_data$education))*100)) %>% 
  ggplot(aes(x=education, y=percent, fill=race))+
  geom_bar(stat = "identity", position=position_dodge2(preserve = "single"))+
  theme_minimal()+
  coord_flip()

clinical_data %>% 
  group_by(married, race) %>% 
  summarise(count=n()) %>% 
  mutate(percent=(count/NROW(!is.na(clinical_data$married))*100)) %>% 
  ggplot(aes(x=married, y=percent, fill=race))+
  geom_bar(stat = "identity", position=position_dodge2(preserve = "single"))+
  theme_minimal()+
  coord_flip()

clinical_data %>% 
  group_by(pregever, race) %>% 
  summarise(count=n()) %>% 
  mutate(percent=(count/NROW(!is.na(clinical_data$pregever))*100)) %>% 
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
  mutate(percent=(count/NROW(!is.na(clinical_data$range_BMI))*100)) %>% 
  ggplot(aes(x=range_BMI, y=percent, fill=race))+
  geom_bar(stat = "identity", position=position_dodge2(preserve = "single"))+
  theme_minimal()+
  coord_flip()


# Age at diag between black and white----
ggplot(clinical_data, aes(x=race, y=refage, color=race))+
  geom_violin(scale = "count") 
# choose count to refect areas are scaled proportionally to the number of observations.



################################################################################################################# II ### Survival plot----
# Limiting to only the case_match ids----
clin_surv <- markers_match
# no event should be 0, when event happened should be 1
# so will use the var surv_vital that I created alive=0, death=1
mysurv <- Surv(time = clin_surv$timelastfu, event = clin_surv$surv_vital)
# median(clin_surv$timelastfu)
# do not use this value as the median survival is tbe time at the survivalship aka function = .5

# Plot
# survfit(mysurv~1, type= "kaplan-meier", conf.type = "log-log") # if log
# can do fleming-harrington or fh2 with log-log too
# For whole population, default mysurv~1, type= "kaplan-meier")
myplot <- survfit(mysurv~1) 
plot(myplot)
# plot(myplot, conf.int = "none") # without confidence interval
# abline(h=0.5)
# abline(v=1576) # Here put the median value in the my surv
# can get a restricted mean 
# print(myplot,print.mean=TRUE)

# For black and white----
myplot <- survfit(mysurv~clin_surv$race)
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
           conf.int = FALSE
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
           conf.int = FALSE
)
# dev.off()
survdiff(mysurv~clin_surv$race)


# Plot cumhaz for cumulative hazard or event
# plot(myplot, fun="cumhaz")
# plot(myplot, fun="event")

# What if we look at only Black and comparing lower vs higher BMI----all black patients
clin_surv <- markers %>% 
  mutate(BMI_fold_increase = BMI_recent/BMI_YA)

clin_surv <- clin_surv %>% 
  filter(race == "Black") %>% 
  mutate(BMI_fold_increase = BMI_recent/BMI_YA) %>% 
  mutate(BMI_grp = ifelse(BMI_fold_increase >=1.442222, "higher", "lower")) %>%  # 1.442222 is the median
  mutate(BMI_grp = factor(BMI_grp, levels = c("lower", "higher")))
median(clin_surv$BMI_fold_increase, na.rm = TRUE)

myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~BMI_grp, data = clin_surv) 
ggsurvplot(myplot, data = clin_surv,
           title = "Effect of BMI increase on Survival within Black population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "grouped by BMI (< or > median)", legend.labs = c("higher", "lower"),
           pval = TRUE, pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           # Add risk table
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = TRUE
)

# BMI_recent
median(clin_surv$BMI_recent, na.rm = TRUE)
clin_surv <- clin_surv %>% 
  mutate(BMI_grp = ifelse(BMI_recent >=31.00538, "higher", "lower")) %>%  # 31.00538 is the median
  mutate(BMI_grp = factor(BMI_grp, levels = c("lower", "higher")))

myplot <- survfit(Surv(time = timelastfu, event = $surv_vital)~BMI_grp, data = clin_surv) 
ggsurvplot(myplot, data = clin_surv,
           title = "Effect of recent BMI on Survival within Black population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "grouped by recent BMI", legend.labs = c("lower", "higher"),
           pval = TRUE, pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           # Add risk table
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = TRUE
)

# BMI_YA
median(clin_surv$BMI_YA, na.rm = TRUE)
clin_surv <- clin_surv %>% 
  mutate(BMI_grp = ifelse(BMI_YA >=21.14375, "higher", "lower")) %>%  # 21.14375 is the median
  mutate(BMI_grp = factor(BMI_grp, levels = c("lower", "higher")))

myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~BMI_grp, data = clin_surv) 
ggsurvplot(myplot, data = clin_surv,
           title = "Effect of BMI at young age on Survival within Black population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "grouped by BMI at young age", legend.labs = c("lower", "higher"),
           pval = TRUE, pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           # Add risk table
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = TRUE
)

# per BMI classification----
# Black
# clin_surv1 <- clin_surv %>% 
#   filter(race == "Black") %>% 
  # mutate(BMI_classification = case_when(
  #   BMI_recent < 18.5	~ "underweight",
  #   BMI_recent >=18.5 & BMI_recent <25 ~ "normal",
  #   BMI_recent >=25.0 & BMI_recent <30 ~ "overweight",
  #   BMI_recent >=30.0 & BMI_recent <35 ~ "obesity I",
  #   BMI_recent >=35.0 & BMI_recent <40 ~ "obesity II",
  #   BMI_recent >= 40.0 ~ "obesity III"
  # ))
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~BMI_classification, data = clin_surv) 
ggsurvplot(myplot, data = clin_surv,
           title = "Effect of BMI classification on Survival within Black population",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)", legend.title = "BMI classification", # legend.labs = c("higher", "lower"),
           pval = TRUE, pval.coord = c(2100,.53),
           surv.median.line = c("hv"),
           # Add risk table
           risk.table = TRUE,
           tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)

# Black vs White
clin_surv <- markers_match
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~BMI_classification+ race, data = clin_surv) 
ggsurv <- ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on Black population comparing recent BMI",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)",
           legend.title = "BMI classification",# legend.labs = c("higher", "lower"),
           pval = TRUE, pval.coord = c(2100, .53),
           surv.median.line = c("hv"),
           # Add risk table
           risk.table = TRUE, tables.height = 0.2,
           risk.table.title = "Risk table",
           conf.int = FALSE
)
# Faceting survival curves
curv_facet <- ggsurv$plot + facet_grid( ~ race)
curv_facet

ggsurvplot(myplot, data = clin_surv,
                     title = "Survival analysis on Black vs White population comparing BMI classification",
                     font.main = c(16, "bold", "black"),
                     xlab = "Time (days)",
                     legend.title = "BMI classification",
                     pval = TRUE, pval.coord = c(2100, .53),
                     surv.median.line = c("hv"),
                     # Add risk table
                     risk.table = TRUE, tables.height = 0.2,
                     risk.table.title = "Risk table",
                     color = "race",
                     conf.int = FALSE
) %>% 
  .$plot +theme_bw() + 
  theme (legend.position = "right")+
  facet_grid(. ~ BMI_classification)



# per stage----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~stage + race, data = clin_surv) 
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on Black vs White population comparing tumor Stage",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)",
           legend.title = "Race",
           pval = TRUE, pval.coord = c(2100, .53),
           surv.median.line = c("hv"),
           # Color
           # palette = "jco",
           color = "race",
           conf.int = FALSE
) %>% 
  .$plot +theme_bw() + 
  theme (legend.position = "right")+
  facet_grid(. ~ stage)

# Per histology
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~histology + race, data = clin_surv) 
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on Black vs White population comparing tumor Histology",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)",
           legend.title = "Race",
           pval = TRUE, pval.coord = c(2100, .53),
           surv.median.line = c("hv"),
           # Color
           # palette = "jco",
           color = "race",
           conf.int = FALSE
) %>% 
  .$plot +theme_bw() + 
  theme (legend.position = "right")+
  facet_grid(. ~ histology)

# Per behavior
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~behavior + race, data = clin_surv) 
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on Black vs White population comparing tumor Behavior",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)",
           legend.title = "Race",
           pval = TRUE, pval.coord = c(2100, .53),
           surv.median.line = c("hv"),
           # Color
           # palette = "jco",
           color = "race",
           conf.int = FALSE
) %>% 
  .$plot +theme_bw() + 
  theme (legend.position = "right")+
  facet_grid(. ~ behavior)

# Per grade
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~grade + race, data = clin_surv) 
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on Black vs White population comparing tumor Grade",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)",
           legend.title = "Race",
           pval = TRUE, pval.coord = c(2100, .53),
           surv.median.line = c("hv"),
           # Color
           # palette = "jco",
           color = "race",
           conf.int = FALSE
) %>% 
  .$plot +theme_bw() + 
  theme (legend.position = "right")+
  facet_grid(. ~ grade)

# Per histotype
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~histotype + race, data = clin_surv) 
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on Black vs White population comparing Histotype",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)",
           legend.title = "Race",
           pval = TRUE, pval.coord = c(2100, .53),
           surv.median.line = c("hv"),
           # Color
           # palette = "jco",
           color = "race",
           conf.int = FALSE
) %>% 
  .$plot +theme_bw() + 
  theme (legend.position = "right")+
  facet_grid(. ~ histotype)

# Per birthplace----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~birthplace + race, data = clin_surv) 
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on Black vs White population comparing Country of birth",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)",
           legend.title = "Race",
           pval = TRUE, pval.coord = c(2100, .53),
           surv.median.line = c("hv"),
           # Color
           # palette = "jco",
           color = "race",
           conf.int = FALSE
) %>% 
  .$plot +theme_bw() + 
  theme (legend.position = "right")+
  facet_grid(. ~ birthplace)

# Per education----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~education + race, data = clin_surv) 
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on Black vs White population comparing Education",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)",
           legend.title = "Race",
           pval = TRUE, pval.coord = c(2100, .53),
           surv.median.line = c("hv"),
           # Color
           # palette = "jco",
           color = "race",
           conf.int = FALSE
) %>% 
  .$plot +theme_bw() + 
  theme (legend.position = "right")+
  facet_grid(. ~ education)

# Per married----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~married + race, data = clin_surv) 
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on Black vs White population comparing Marrital status",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)",
           legend.title = "Race",
           pval = TRUE, pval.coord = c(2100, .53),
           surv.median.line = c("hv"),
           # Color
           # palette = "jco",
           color = "race",
           conf.int = FALSE
) %>% 
  .$plot +theme_bw() + 
  theme (legend.position = "right")+
  facet_grid(. ~ married)

# Per pregever----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~pregever + race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "pregever",
                 title = "Survival analysis on Black vs White population comparing Pregnacy status",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# Per agefbirth----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~agefbirth + race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "agefbirth",
                 title = "Survival analysis on Black vs White population comparing Age at first birth",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

clin_surv$range_agefbirth <- as.factor(findInterval(clin_surv$agefbirth,c(14,20,25,30,35,40)))
levels(clin_surv$range_agefbirth) <- c("<20","20-24","25-29","30-34","35-39")
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~range_agefbirth + race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "range_agefbirth",
                 title = "Survival analysis on Black vs White population comparing Age at first birth",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# Per hyster----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~hyster +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "hyster",
                 title = "Survival analysis on Black vs White population comparing Hysterectomy status",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# Per oophor----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~oophor +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "oophor",
                 title = "Survival analysis on Black vs White population comparing Oophorectomy status",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# Per tubelig----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~tubelig +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "tubelig",
                 title = "Survival analysis on Black vs White population comparing Tubal ligation status",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# Per ocever----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~ocever +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "ocever",
                 title = "Survival analysis on Black vs White population comparing Ever use oral contraceptives",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# Per breastfedever----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~breastfedever +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "breastfedever",
                 title = "Survival analysis on Black vs White population comparing Ever breastfed",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# Per menarch_agecat----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~menarch_agecat +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "menarch_agecat",
                 title = "Survival analysis on Black vs White population comparing Age at menarche",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# Per menopause----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~menopause +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "menopause",
                 title = "Survival analysis on Black vs White population comparing Menopausal status",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# Per talcever----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~talcever +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "talcever",
                 title = "Survival analysis on Black vs White population comparing Ever regular use of body powder (at least 1 time per month for 6 months)",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# Per brcancer----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~brcancer +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "brcancer",
                 title = "Survival analysis on Black vs White population comparing Ever diagnosed with breast cancer",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# Per brcancermom----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~brcancermom +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "brcancermom",
                 title = "Survival analysis on Black vs White population comparing Biological mother had breast cancer",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# Per famhxbr----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~famhxbr +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "famhxbr",
                 title = "Survival analysis on Black vs White population comparing Family history of breast cancer in any first-degree relative",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# Per ovcancermom----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~ovcancermom +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "ovcancermom",
                 title = "Survival analysis on Black vs White population comparing Biological mother had ovarian cancer",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# Per ovcancersis----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~ovcancersis +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "ovcancersis",
                 title = "Survival analysis on Black vs White population comparing Any biological sister diagnosed with ovarian cancer",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# Per famhxov----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~famhxov +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "famhxov",
                 title = "Survival analysis on Black vs White population comparing Family history of ovarian cancer in any first-degree relative",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# Per ovcancerdaughter----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~ovcancerdaughter +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "ovcancerdaughter",
                 title = "Survival analysis on Black vs White population comparing ovcancerdaughter",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# Per endomet----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~endomet +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "endomet",
                 title = "Survival analysis on Black vs White population comparing diagnosed with endometriosis",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# Per fibroids----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~fibroids +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "fibroids",
                 title = "Survival analysis on Black vs White population comparing diagnosed with uterine fibroids",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# Per pid----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~pid +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "pid",
                 title = "Survival analysis on Black vs White population comparing diagnosed with pelvic inflammatory disease",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# Per pcos----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~pcos +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "pcos",
                 title = "Survival analysis on Black vs White population comparing diagnosed with polycystic ovarian syndrome",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# Per ovcyst----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~ovcyst +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "ovcyst",
                 title = "Survival analysis on Black vs White population comparing diagnosed with an ovarian cyst",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# Per anyfhever----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~anyfhever +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "anyfhever",
                 title = "Survival analysis on Black vs White population comparing use any female hormones (pill patch or injection only)",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# Per eonlyever----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~eonlyever +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "eonlyever",
                 title = "Survival analysis on Black vs White population comparing use estrogen only (pills, patches, or injections)",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# Per epever----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~epever +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "epever",
                 title = "Survival analysis on Black vs White population comparing use estrogen + progestin (pills, patches, or injections)",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# Per smokever----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~smokever +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "smokever",
                 title = "Survival analysis on Black vs White population comparing smoker status",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# Per diab----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~diab +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "diab",
                 title = "Survival analysis on Black vs White population comparing diabetes diagnosis",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# Per hrtdis----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~hrtdis +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "hrtdis",
                 title = "Survival analysis on Black vs White population comparing heart disease diagnosis",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# Per hbp----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~hbp +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "hbp",
                 title = "Survival analysis on Black vs White population comparing high blood pressure diagnosis ",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# Per hchol----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~hchol +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "hchol",
                 title = "Survival analysis on Black vs White population comparing high cholesterol diagnosis",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# Per osteo----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~osteo +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "osteo",
                 title = "Survival analysis on Black vs White population comparing osteoporosis diagnosis",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# Per thyrd----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~thyrd +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "thyrd",
                 title = "Survival analysis on Black vs White population comparing thyroid condition",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# Per infert----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~infert +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "infert",
                 title = "Survival analysis on Black vs White population comparing infertility for 1+ year",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# Per cancersite----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~cancersite +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "cancersite",
                 title = "Survival analysis on Black vs White population comparing cancer site",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# Per aspirin----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~aspirin +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "aspirin",
                 title = "Survival analysis on Black vs White population comparing aspirin",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# Per NSAID----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~NSAID +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "NSAID",
                 title = "Survival analysis on Black vs White population comparing NSAID",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# Per aceta----
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~aceta +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "aceta",
                 title = "Survival analysis on Black vs White population comparing aceta",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))


# Lymphocytes no race for intratumoral stromal+tumor using quartiles----
clin_surv <- markers
# CD3
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~CD3_grp, data = clin_surv) 
ggsurvplot(myplot, data = clin_surv,
                 title = "Survival analysis on overall population \nseparated by CD3+ lymphocyte occupancy",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "CD3+",
                 surv.median.line = c("hv"),
           pval = TRUE, pval.coord = c(2600, .52))
# Cox CD3
myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~CD3_grp, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~percent_CD3_total.i, data = clin_surv) 
summary(myplot)

myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~CD3_grp + refage + race + stage, data = clin_surv) 
summary(myplot)

myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~percent_CD3_total.i + refage + race + stage, data = clin_surv) 
summary(myplot)

# CD3
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~CD3t_grp, data = clin_surv) 
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on overall population \nseparated by tumoral CD3+ lymphocyte occupancy",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)",
           legend.title = "tumoral CD3+",
           surv.median.line = c("hv"),
           pval = TRUE, pval.coord = c(2600, .52))
# Cox CD3
myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~CD3t_grp, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~percent_CD3_tumor.i, data = clin_surv) 
summary(myplot)

myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~CD3t_grp + refage + race + stage, data = clin_surv) 
summary(myplot)

myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~percent_CD3_tumor.i + refage + race + stage, data = clin_surv) 
summary(myplot)

## CD3 stroma
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~CD3s_grp, data = clin_surv) 
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on overall population \nseparated by stromal CD3+ lymphocyte occupancy",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)",
           legend.title = "stromal CD3+",
           surv.median.line = c("hv"),
           pval = TRUE, pval.coord = c(2600, .52))
#
myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~CD3s_grp, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~percent_CD3_stroma.i, data = clin_surv) 
summary(myplot)

myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~CD3s_grp + refage + race + stage, data = clin_surv) 
summary(myplot)

myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~percent_CD3_stroma.i + refage + race + stage, data = clin_surv) 
summary(myplot)

# CD3_CD8
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~CD3_CD8_grp, data = clin_surv) 
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on overall population \nseparated by CD3+CD8+ lymphocyte occupancy",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)",
           legend.title = "CD3+CD8+",
           surv.median.line = c("hv"),
           pval = TRUE, pval.coord = c(2500, .52))
# Cox
myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~CD3_CD8_grp, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~percent_CD3_CD8_total.i, data = clin_surv) 
summary(myplot)

myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~CD3_CD8_grp + refage + race + stage, data = clin_surv) 
summary(myplot)

myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~percent_CD3_CD8_total.i + refage + race + stage, data = clin_surv) 
summary(myplot)

# CD3_CD8 tumor
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~CD3_CD8t_grp, data = clin_surv) 
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on overall population \nseparated by tumoral CD3+CD8+ lymphocyte occupancy",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)",
           legend.title = "tumor CD3+CD8+",
           surv.median.line = c("hv"),
           pval = TRUE, pval.coord = c(2500, .52))
# Cox
myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~CD3_CD8t_grp, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~percent_CD3_CD8_tumor.i, data = clin_surv) 
summary(myplot)

myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~CD3_CD8t_grp + refage + race + stage, data = clin_surv) 
summary(myplot)

myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~percent_CD3_CD8_tumor.i + refage + race + stage, data = clin_surv) 
summary(myplot)

# CD3_CD8 stroma
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~CD3_CD8s_grp, data = clin_surv) 
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on overall population \nseparated by stromal CD3+CD8+ lymphocyte occupancy",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)",
           legend.title = "stroma CD3+CD8+",
           surv.median.line = c("hv"),
           pval = TRUE, pval.coord = c(2500, .52))
# Cox
myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~CD3_CD8s_grp, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~percent_CD3_CD8_stroma.i, data = clin_surv) 
summary(myplot)

myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~CD3_CD8s_grp + refage + race + stage, data = clin_surv) 
summary(myplot)

myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~percent_CD3_CD8_stroma.i + refage + race + stage, data = clin_surv) 
summary(myplot)


# CD3_FoxP3
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~CD3_FoxP3_grp, data = clin_surv) 
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on overall population \nseparated by tumoral CD3+FoxP3+ lymphocyte occupancy",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)",
           legend.title = "CD3+FoxP3+",
           surv.median.line = c("hv"),
           pval = TRUE, pval.coord = c(2100, .52))
# Cox
myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~CD3_FoxP3_grp, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~percent_CD3_FoxP3_total.i, data = clin_surv) 
summary(myplot)

myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~CD3_FoxP3_grp + refage + race + stage, data = clin_surv) 
summary(myplot)

myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~percent_CD3_FoxP3_total.i + refage + race + stage, data = clin_surv) 
summary(myplot)

# CD3_FoxP3 tumor
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~CD3_FoxP3t_grp, data = clin_surv) 
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on overall population \nseparated by tumoral CD3+FoxP3+ lymphocyte occupancy",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)",
           legend.title = "tumor CD3+FoxP3+",
           surv.median.line = c("hv"),
           pval = TRUE, pval.coord = c(2100, .52))
# Cox
myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~CD3_FoxP3t_grp, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~percent_CD3_FoxP3_tumor.i, data = clin_surv) 
summary(myplot)

myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~CD3_FoxP3t_grp + refage + race + stage, data = clin_surv) 
summary(myplot)

myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~percent_CD3_FoxP3_tumor.i + refage + race + stage, data = clin_surv) 
summary(myplot)

# CD3_FoxP3 stroma
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~CD3_FoxP3s_grp, data = clin_surv) 
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on overall population \nseparated by stromal CD3+FoxP3+ lymphocyte occupancy",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)",
           legend.title = "stroma CD3+FoxP3+",
           surv.median.line = c("hv"),
           pval = TRUE, pval.coord = c(2100, .52))
# Cox
myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~CD3_FoxP3s_grp, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~percent_CD3_FoxP3_stroma.i, data = clin_surv) 
summary(myplot)

myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~CD3_FoxP3s_grp + refage + race + stage, data = clin_surv) 
summary(myplot)

myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~percent_CD3_FoxP3_stroma.i + refage + race + stage, data = clin_surv) 
summary(myplot)

# CD11b+
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~CD11b_grp, data = clin_surv) 
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on overall population \nseparated by CD11b+ lymphocyte occupancy",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)",
           legend.title = "CD11b+",
           surv.median.line = c("hv"),
           pval = TRUE, pval.coord = c(2500, .52))
# Cox
myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~CD11b_grp, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~percent_CD11b_total.i, data = clin_surv) 
summary(myplot)

myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~CD11b_grp + refage + race + stage, data = clin_surv) 
summary(myplot)

myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~percent_CD11b_total.i + refage + race + stage, data = clin_surv) 
summary(myplot)

# CD11b+ tumor
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~CD11bt_grp, data = clin_surv) 
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on overall population \nseparated by tumoral CD11b+ lymphocyte occupancy",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)",
           legend.title = "tumoral CD11b+",
           surv.median.line = c("hv"),
           pval = TRUE, pval.coord = c(2500, .52))
# Cox
myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~CD11bt_grp, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~percent_CD11b_tumor.i, data = clin_surv) 
summary(myplot)

myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~CD11bt_grp + refage + race + stage, data = clin_surv) 
summary(myplot)

myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~percent_CD11b_tumor.i + refage + race + stage, data = clin_surv) 
summary(myplot)

# CD11b+ stroma
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~CD11bs_grp, data = clin_surv) 
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on overall population \nseparated by stromal CD11b+ lymphocyte occupancy",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)",
           legend.title = "stromal CD11b+",
           surv.median.line = c("hv"),
           pval = TRUE, pval.coord = c(2500, .52))
# Cox
myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~CD11bs_grp, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~percent_CD11b_stroma.i, data = clin_surv) 
summary(myplot)

myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~CD11bs_grp + refage + race + stage, data = clin_surv) 
summary(myplot)

myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~percent_CD11b_stroma.i + refage + race + stage, data = clin_surv) 
summary(myplot)

# CD11b+CD15+
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~CD11b_CD15_grp, data = clin_surv) 
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on overall population \nseparated by CD11b+CD15+ lymphocyte occupancy",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)",
           legend.title = "CD11b+CD15+",
           surv.median.line = c("hv"),
           pval = TRUE, pval.coord = c(3000, .52))
# Cox
myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~CD11b_CD15_grp, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~percent_CD11b_CD15_total.i, data = clin_surv) 
summary(myplot)

myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~CD11b_CD15_grp + refage + race + stage, data = clin_surv) 
summary(myplot)

myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~percent_CD11b_CD15_total.i + refage + race + stage, data = clin_surv) 
summary(myplot)

# CD11b+CD15+ tumor
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~CD11b_CD15t_grp, data = clin_surv) 
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on overall population \nseparated by tumoral CD11b+CD15+ lymphocyte occupancy",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)",
           legend.title = "tumor CD11b+CD15+",
           surv.median.line = c("hv"),
           pval = TRUE, pval.coord = c(3000, .52))
# Cox
myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~CD11b_CD15t_grp, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~percent_CD11b_CD15_tumor.i, data = clin_surv) 
summary(myplot)

myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~CD11b_CD15t_grp + refage + race + stage, data = clin_surv) 
summary(myplot)

myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~percent_CD11b_CD15_tumor.i + refage + race + stage, data = clin_surv) 
summary(myplot)

# CD11b+CD15+ stroma
myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~CD11b_CD15s_grp, data = clin_surv) 
ggsurvplot(myplot, data = clin_surv,
           title = "Survival analysis on overall population \nseparated by stromal CD11b+CD15+ lymphocyte occupancy",
           font.main = c(16, "bold", "black"),
           xlab = "Time (days)",
           legend.title = "stromal CD11b+CD15+",
           surv.median.line = c("hv"),
           pval = TRUE, pval.coord = c(3000, .52))
# Cox
myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~CD11b_CD15s_grp, data = clin_surv) 
summary(myplot)
myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~percent_CD11b_CD15_stroma.i, data = clin_surv) 
summary(myplot)

myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~CD11b_CD15s_grp + refage + race + stage, data = clin_surv) 
summary(myplot)

myplot <- coxph(Surv(time = timelastfu, event = surv_vital)~percent_CD11b_CD15_stroma.i + refage + race + stage, data = clin_surv) 
summary(myplot)


# Lymphocytes keep----
clin_surv <- markers_match
# CD3
median(clin_surv$percent_CD3_tumor.i)
clin_surv <- clin_surv %>% 
  mutate(CD3 = ifelse(percent_CD3_tumor.i >=1.035165, "higher", "lower")) %>%  # median
  mutate(CD3 = factor(CD3, levels = c("lower", "higher")))

myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~CD3 +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "CD3",
                 title = "Survival analysis on Black vs White population",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))
median(clin_surv$percent_CD3_tumor.p, na.rm = TRUE)
clin_surv <- clin_surv %>% 
  mutate(CD3 = ifelse(percent_CD3_tumor.p >=1.864843, "higher", "lower")) %>%  # median
  mutate(CD3 = factor(CD3, levels = c("lower", "higher")))

myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~CD3 +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "CD3",
                 title = "Survival analysis on Black vs White population",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))
# CD8
median(clin_surv$percent_CD8_tumor.i)
clin_surv <- clin_surv %>% 
  mutate(CD8 = ifelse(percent_CD8_tumor.i >=0.560704, "higher", "lower")) %>%  # median
  mutate(CD8 = factor(CD3, levels = c("lower", "higher")))

myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~CD8 +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "CD8",
                 title = "Survival analysis on Black vs White population",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))
median(clin_surv$percent_CD8_tumor.p, na.rm = TRUE)
clin_surv <- clin_surv %>% 
  mutate(CD8 = ifelse(percent_CD8_tumor.p >=0.9094123, "higher", "lower")) %>%  # median
  mutate(CD8 = factor(CD3, levels = c("lower", "higher")))

myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~CD8 +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "CD8",
                 title = "Survival analysis on Black vs White population",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# CD3CD8
median(clin_surv$percent_CD3_CD8_tumor.i)
clin_surv <- clin_surv %>% 
  mutate(CD3CD8 = ifelse(percent_CD3_CD8_tumor.i >=0.3548757, "higher", "lower")) %>%  # median
  mutate(CD3CD8 = factor(CD3CD8, levels = c("lower", "higher")))

myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~CD3CD8 +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "CD3CD8",
                 title = "Survival analysis on Black vs White population",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))
median(clin_surv$percent_CD3_CD8_tumor.p, na.rm = TRUE)
clin_surv <- clin_surv %>% 
  mutate(CD3CD8 = ifelse(percent_CD3_CD8_tumor.p >=0.6122158, "higher", "lower")) %>%  # median
  mutate(CD3CD8 = factor(CD3CD8, levels = c("lower", "higher")))

myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~CD3CD8 +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "CD3CD8",
                 title = "Survival analysis on Black vs White population",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# FoxP3
median(clin_surv$percent_FoxP3_tumor.i)
clin_surv <- clin_surv %>% 
  mutate(FoxP3 = ifelse(percent_FoxP3_tumor.i >=0.2734768, "higher", "lower")) %>%  #edian
  mutate(FoxP3 = factor(FoxP3, levels = c("lower", "higher")))

myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~FoxP3 +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "FoxP3",
                 title = "Survival analysis on Black vs White population",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))
median(clin_surv$percent_FoxP3_tumor.p, na.rm = TRUE)
clin_surv <- clin_surv %>% 
  mutate(FoxP3 = ifelse(percent_FoxP3_tumor.p >=0.4100425, "higher", "lower")) %>%  # median
  mutate(FoxP3 = factor(FoxP3, levels = c("lower", "higher")))

myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~FoxP3 +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "FoxP3",
                 title = "Survival analysis on Black vs White population",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# CD3FoxP3
median(clin_surv$percent_CD3_FoxP3_tumor.i)
clin_surv <- clin_surv %>% 
  mutate(CD3FoxP3 = ifelse(percent_CD3_FoxP3_tumor.i >=0.127227, "higher", "lower")) %>%  # median
  mutate(CD3FoxP3 = factor(CD3FoxP3, levels = c("lower", "higher")))

myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~CD3FoxP3 +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "CD3FoxP3",
                 title = "Survival analysis on Black vs White population",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))
median(clin_surv$percent_CD3_FoxP3_tumor.p, na.rm = TRUE)
clin_surv <- clin_surv %>% 
  mutate(CD3FoxP3 = ifelse(percent_CD3_FoxP3_tumor.p >=0.1929595, "higher", "lower")) %>%  # median
  mutate(CD3FoxP3 = factor(CD3FoxP3, levels = c("lower", "higher")))

myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~CD3FoxP3 +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "CD3FoxP3",
                 title = "Survival analysis on Black vs White population",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# CD11b
median(clin_surv$percent_CD11b_tumor.i)
clin_surv <- clin_surv %>% 
  mutate(CD11b = ifelse(percent_CD11b_tumor.i >=0.03248308, "higher", "lower")) %>%  # median
  mutate(CD11b = factor(CD11b, levels = c("lower", "higher")))

myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~CD11b +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "CD11b",
                 title = "Survival analysis on Black vs White population",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))
median(clin_surv$percent_CD11b_tumor.p, na.rm = TRUE)
clin_surv <- clin_surv %>% 
  mutate(CD11b = ifelse(percent_CD11b_tumor.p >=0.03932533, "higher", "lower")) %>%  # median
  mutate(CD11b = factor(CD11b, levels = c("lower", "higher")))

myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~CD11b +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "CD11b",
                 title = "Survival analysis on Black vs White population",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# CD15
mean(clin_surv$percent_CD15_tumor.i)
clin_surv <- clin_surv %>% 
  mutate(CD15 = ifelse(percent_CD15_tumor.i >=0.08935341, "higher", "lower")) %>%  # mean
  mutate(CD15 = factor(CD15, levels = c("lower", "higher")))

myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~CD15 +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "CD15",
                 title = "Survival analysis on Black vs White population",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))
mean(clin_surv$percent_CD15_tumor.p, na.rm = TRUE)
clin_surv <- clin_surv %>% 
  mutate(CD15 = ifelse(percent_CD15_tumor.p >=0.09734356, "higher", "lower")) %>%  # mean
  mutate(CD15 = factor(CD15, levels = c("lower", "higher")))

myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~CD15 +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "CD15",
                 title = "Survival analysis on Black vs White population",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# CD11bCD15
mean(clin_surv$percent_CD11b_CD15_tumor.i)
clin_surv <- clin_surv %>% 
  mutate(CD11bCD15 = ifelse(percent_CD11b_CD15_tumor.i >=0.03799938, "higher", "lower")) %>%  # mean
  mutate(CD11bCD15 = factor(CD11bCD15, levels = c("lower", "higher")))

myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~CD11bCD15 +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "CD11bCD15",
                 title = "Survival analysis on Black vs White population",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

mean(clin_surv$percent_CD11b_CD15_tumor.p, na.rm = TRUE)
clin_surv <- clin_surv %>% 
  mutate(CD11bCD15 = ifelse(percent_CD11b_tumor.p >=0.03022437, "higher", "lower")) %>%  # mean
  mutate(CD11bCD15 = factor(CD11bCD15, levels = c("lower", "higher")))

myplot <- survfit(Surv(time = timelastfu, event = surv_vital)~CD11bCD15 +race, data = clin_surv) 
ggsurvplot_facet(myplot, data = clin_surv, facet.by = "CD11bCD15",
                 title = "Survival analysis on Black vs White population",
                 font.main = c(16, "bold", "black"),
                 xlab = "Time (days)",
                 legend.title = "Race",
                 surv.median.line = c("hv"))

# Cleaning
rm(clin_surv, myplot, mysurv)
