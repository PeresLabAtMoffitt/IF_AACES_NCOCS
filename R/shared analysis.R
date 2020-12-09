# For Jose PO1----

a <- markers %>% filter(slide_type_tma == "TMA") %>% 
  select(c("slide_type_tma", `Age at diagnosis` = "refage", "refage_cat", "stage", "histotype11", "histotype12")) %>% 
  tbl_summary(by = slide_type_tma, statistic = list(all_continuous() ~ "{mean} ({sd})")) %>% 
  as_gt()
gt::gtsave(a, expand = 1, zoom = 2, 
           paste0(
             path, 
             "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/output/AACES table summary.pdf"))

# for Nurses P01---- 

AACES_data <- markers %>% 
  mutate(AACES = str_count(markers$suid)) %>% 
  filter(AACES == 6)

a <- AACES_data %>% filter(slide_type_tma == "TMA") %>% 
  select(c("slide_type_tma", BMI_recent_grp, BMI_YA_grp)) %>% 
  tbl_summary(by = slide_type_tma, statistic = list(all_continuous() ~ "{mean} ({sd})")) %>% 
  as_gt()
gt::gtsave(a, expand = 1, zoom = 1, 
           paste0(
             path, 
             "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/output/AACES BMI in TMA table summary.pdf"))
a <- AACES_data %>% filter(slide_type == "ROI") %>% 
  select(c("slide_type", BMI_recent_grp, BMI_YA_grp)) %>% 
  tbl_summary(by = slide_type, statistic = list(all_continuous() ~ "{mean} ({sd})")) %>% 
  as_gt()
gt::gtsave(a, expand = 1, zoom = 1, 
           paste0(
             path, 
             "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/output/AACES BMI in ROI table summary.pdf"))



# logistic regression to examine the association between BMI 
# ROI intratumoral total
model <- glm(med.CD3_total.i ~ BMI_recent_grp + refage + stage, data = AACES_data, family = binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))
model <- glm(med.CD3_CD8_total.i ~ BMI_recent_grp + refage + stage, data = AACES_data, family = binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))
model <- glm(med.CD3_FoxP3_total.i ~ BMI_recent_grp + refage + stage, data = AACES_data, family = binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))
model <- glm(med.CD3_total.i ~ BMI_YA_grp + refage + stage, data = AACES_data, family = binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))
model <- glm(med.CD3_CD8_total.i ~ BMI_YA_grp + refage + stage, data = AACES_data, family = binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))
model <- glm(med.CD3_FoxP3_total.i ~ BMI_YA_grp + refage + stage, data = AACES_data, family = binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))

# TMA
model <- glm(med.CD3_total_tma ~ BMI_recent_grp + refage + stage + histotype1, data = AACES_data, family = binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))

model <- glm(med.CD3_CD8_total_tma ~ BMI_recent_grp + refage + stage + histotype1, data = AACES_data, family = binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))

model <- glm(med.CD3_FoxP3_total_tma ~ BMI_recent_grp + refage + stage + histotype1, data = AACES_data, family = binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))

model <- glm(med.CD3_total_tma ~ BMI_YA_grp + refage + stage + histotype1, data = AACES_data, family = binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))

model <- glm(med.CD3_CD8_total_tma ~ BMI_YA_grp + refage + stage + histotype1, data = AACES_data, family = binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))

model <- glm(med.CD3_FoxP3_total_tma ~ BMI_YA_grp + refage + stage + histotype1, data = AACES_data, family = binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))

# association between smoking status 
# ROI intratumoral total
model <- glm(med.CD3_total.i ~ smoker, data = AACES_data, family = binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))
model <- glm(med.CD3_CD8_total.i ~ smoker, data = AACES_data, family = binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))
model <- glm(med.CD3_FoxP3_total.i ~ smoker, data = AACES_data, family = binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))


# TMA
model <- glm(med.CD3_total_tma ~ smoker, data = AACES_data, family = binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))
model <- glm(med.CD3_total_tma ~ smoker + refage + stage, data = AACES_data, family = binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))
model <- glm(med.CD3_total_tma ~ smoker + refage + stage + histotype1, data = AACES_data, family = binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))

model <- glm(med.CD3_CD8_total_tma ~ smoker, data = AACES_data, family = binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))
model <- glm(med.CD3_CD8_total_tma ~ smoker + refage + stage, data = AACES_data, family = binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))
model <- glm(med.CD3_CD8_total_tma ~ smoker + refage + stage + histotype1, data = AACES_data, family = binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))

model <- glm(med.CD3_FoxP3_total_tma ~ smoker, data = AACES_data, family = binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))
model <- glm(med.CD3_FoxP3_total_tma ~ smoker + refage + stage, data = AACES_data, family = binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))
model <- glm(med.CD3_FoxP3_total_tma ~ smoker + refage + stage + histotype1, data = AACES_data, family = binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))

# association between aspirin status 
# ROI intratumoral total
model <- glm(med.CD3_total.i ~ aspirin_use, data = AACES_data, family = binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))
model <- glm(med.CD3_CD8_total.i ~ aspirin_use, data = AACES_data, family = binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))
model <- glm(med.CD3_FoxP3_total.i ~ aspirin_use, data = AACES_data, family = binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))


# TMA
model <- glm(med.CD3_total_tma ~ aspirin_use, data = AACES_data, family = binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))
model <- glm(med.CD3_total_tma ~ aspirin_use + refage + stage, data = AACES_data, family = binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))
model <- glm(med.CD3_total_tma ~ aspirin_use + refage + stage + histotype1, data = AACES_data, family = binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))

model <- glm(med.CD3_CD8_total_tma ~ aspirin_use, data = AACES_data, family = binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))
model <- glm(med.CD3_CD8_total_tma ~ aspirin_use + refage + stage, data = AACES_data, family = binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))
model <- glm(med.CD3_CD8_total_tma ~ aspirin_use + refage + stage + histotype1, data = AACES_data, family = binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))

model <- glm(med.CD3_FoxP3_total_tma ~ aspirin_use, data = AACES_data, family = binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))
model <- glm(med.CD3_FoxP3_total_tma ~ aspirin_use + refage + stage, data = AACES_data, family = binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))
model <- glm(med.CD3_FoxP3_total_tma ~ aspirin_use + refage + stage + histotype1, data = AACES_data, family = binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))


# Associations between immune markers and survival----
AACES_tx <- readxl::read_xlsx(paste0(path,
                  "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/data/AACES_tx_12082020.xlsx"))
AACES_tx <- AACES_tx %>% 
  mutate(dblkstat_CA125 = case_when(
    dblkstat_CA125 == 1              ~ "Optimal debulking",
    dblkstat_CA125 == 2              ~ "Suboptimal debulking",
    dblkstat_CA125 == 88             ~ "No surgery",
    TRUE                             ~ NA_character_
  )) %>% 
  mutate(dblkstat_CA125 = factor(dblkstat_CA125, levels = c("Optimal debulking", "Suboptimal debulking", "No surgery"))) %>% 
  mutate(chemotherapy = case_when(
    neoadj == 1 |
      adj == 1                       ~ "Received chemotherapy",
    neoadj == 2 &
      adj == 2                       ~ "No chemotherapy",
  )) %>% 
  filter(!is.na(dblkstat_CA125)) %>% 
  mutate(neoadj = case_when(
    neoadj == 1                      ~ "Received neoadjuvant chemotherapy",
    neoadj == 2                      ~ "No neoadjuvant chemotherapy",
    TRUE                             ~ NA_character_
  )) %>% 
  mutate(adj = case_when(
    adj == 1                         ~ "Received adjuvant chemotherapy",
    adj == 2                         ~ "No adjuvant chemotherapy",
    TRUE                             ~ NA_character_
  )) %>% 
  mutate(suid = as.character(suid))

# TMA
AACES_data <- markers %>% 
  mutate(AACES = str_count(markers$suid)) %>% 
  filter(AACES == 6)
AACES_tma <- AACES_data %>% filter(slide_type_tma == "TMA") %>% 
  inner_join(., AACES_tx, by = "suid")

table(AACES_tma$neoadj)
table(AACES_tma$adj)

table(AACES_tma$med.CD3_total_tma)
# Cox CD3
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_total_tma + refage + stage + histotype1, data = AACES_tma) 
summary(surv_tma)
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_total_tma + refage + stage + histotype1 + dblkstat_CA125, data = AACES_tma) 
summary(surv_tma)
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_total_tma + refage + stage + histotype1 + dblkstat_CA125 + neoadj, data = AACES_tma) 
summary(surv_tma)
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_total_tma + refage + stage + histotype1 + dblkstat_CA125 + adj, data = AACES_tma) 
summary(surv_tma)
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_total_tma + refage + stage + histotype1 + dblkstat_CA125 + chemotherapy, data = AACES_tma) 
summary(surv_tma)

table(AACES_tma$med.CD3_tumor_tma)
# Cox CD3t
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_tumor_tma + refage + stage + histotype1, data = AACES_tma) 
summary(surv_tma)
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_tumor_tma + refage + stage + histotype1 + dblkstat_CA125, data = AACES_tma) 
summary(surv_tma)
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_tumor_tma + refage + stage + histotype1 + dblkstat_CA125 + neoadj, data = AACES_tma) 
summary(surv_tma)
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_tumor_tma + refage + stage + histotype1 + dblkstat_CA125 + adj, data = AACES_tma) 
summary(surv_tma)
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_tumor_tma + refage + stage + histotype1 + dblkstat_CA125 + chemotherapy, data = AACES_tma) 
summary(surv_tma)

table(AACES_tma$med.CD3_stroma_tma)
# Cox CD3s
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_stroma_tma + refage + stage + histotype1, data = AACES_tma) 
summary(surv_tma)
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_stroma_tma + refage + stage + histotype1 + dblkstat_CA125, data = AACES_tma) 
summary(surv_tma)
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_stroma_tma + refage + stage + histotype1 + dblkstat_CA125 + neoadj, data = AACES_tma) 
summary(surv_tma)
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_stroma_tma + refage + stage + histotype1 + dblkstat_CA125 + adj, data = AACES_tma) 
summary(surv_tma)
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_stroma_tma + refage + stage + histotype1 + dblkstat_CA125 + chemotherapy, data = AACES_tma) 
summary(surv_tma)

table(AACES_tma$med.CD3_FoxP3_total_tma)
# Cox CD3FoxP3
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_FoxP3_total_tma + refage + stage + histotype1, data = AACES_tma) 
summary(surv_tma)
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_FoxP3_total_tma + refage + stage + histotype1 + dblkstat_CA125, data = AACES_tma) 
summary(surv_tma)
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_FoxP3_total_tma + refage + stage + histotype1 + dblkstat_CA125 + neoadj, data = AACES_tma) 
summary(surv_tma)
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_FoxP3_total_tma + refage + stage + histotype1 + dblkstat_CA125 + adj, data = AACES_tma) 
summary(surv_tma)
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_FoxP3_total_tma + refage + stage + histotype1 + dblkstat_CA125 + chemotherapy, data = AACES_tma) 
summary(surv_tma)

table(AACES_tma$med.CD3_FoxP3_tumor_tma)
# Cox CD3FoxP3t
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_FoxP3_tumor_tma + refage + stage + histotype1, data = AACES_tma) 
summary(surv_tma)
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_FoxP3_tumor_tma + refage + stage + histotype1 + dblkstat_CA125, data = AACES_tma) 
summary(surv_tma)
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_FoxP3_tumor_tma + refage + stage + histotype1 + dblkstat_CA125 + neoadj, data = AACES_tma) 
summary(surv_tma)
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_FoxP3_tumor_tma + refage + stage + histotype1 + dblkstat_CA125 + adj, data = AACES_tma) 
summary(surv_tma)
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_FoxP3_tumor_tma + refage + stage + histotype1 + dblkstat_CA125 + chemotherapy, data = AACES_tma) 
summary(surv_tma)

table(AACES_tma$med.CD3_FoxP3_stroma_tma)
# Cox CD3FoxP3s
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_FoxP3_stroma_tma + refage + stage + histotype1, data = AACES_tma) 
summary(surv_tma)
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_FoxP3_stroma_tma + refage + stage + histotype1 + dblkstat_CA125, data = AACES_tma) 
summary(surv_tma)
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_FoxP3_stroma_tma + refage + stage + histotype1 + dblkstat_CA125 + neoadj, data = AACES_tma) 
summary(surv_tma)
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_FoxP3_stroma_tma + refage + stage + histotype1 + dblkstat_CA125 + adj, data = AACES_tma) 
summary(surv_tma)
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_FoxP3_stroma_tma + refage + stage + histotype1 + dblkstat_CA125 + chemotherapy, data = AACES_tma) 
summary(surv_tma)

table(AACES_tma$med.CD3_CD8_total_tma)
# Cox CD3CD8
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_CD8_total_tma + refage + stage + histotype1, data = AACES_tma) 
summary(surv_tma)
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_CD8_total_tma + refage + stage + histotype1 + dblkstat_CA125, data = AACES_tma) 
summary(surv_tma)
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_CD8_total_tma + refage + stage + histotype1 + dblkstat_CA125 + neoadj, data = AACES_tma) 
summary(surv_tma)
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_CD8_total_tma + refage + stage + histotype1 + dblkstat_CA125 + adj, data = AACES_tma) 
summary(surv_tma)
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_CD8_total_tma + refage + stage + histotype1 + dblkstat_CA125 + chemotherapy, data = AACES_tma) 
summary(surv_tma)

table(AACES_tma$med.CD3_CD8_tumor_tma)
# Cox CD3CD8t
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_CD8_tumor_tma + refage + stage + histotype1, data = AACES_tma) 
summary(surv_tma)
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_CD8_tumor_tma + refage + stage + histotype1 + dblkstat_CA125, data = AACES_tma) 
summary(surv_tma)
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_CD8_tumor_tma + refage + stage + histotype1 + dblkstat_CA125 + neoadj, data = AACES_tma) 
summary(surv_tma)
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_CD8_tumor_tma + refage + stage + histotype1 + dblkstat_CA125 + adj, data = AACES_tma) 
summary(surv_tma)
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_CD8_tumor_tma + refage + stage + histotype1 + dblkstat_CA125 + chemotherapy, data = AACES_tma) 
summary(surv_tma)

table(AACES_tma$med.CD3_CD8_stroma_tma)
# Cox CD3CD8s
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_CD8_stroma_tma + refage + stage + histotype1, data = AACES_tma) 
summary(surv_tma)
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_CD8_stroma_tma + refage + stage + histotype1 + dblkstat_CA125, data = AACES_tma) 
summary(surv_tma)
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_CD8_stroma_tma + refage + stage + histotype1 + dblkstat_CA125 + neoadj, data = AACES_tma) 
summary(surv_tma)
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_CD8_stroma_tma + refage + stage + histotype1 + dblkstat_CA125 + adj, data = AACES_tma) 
summary(surv_tma)
surv_tma <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_CD8_stroma_tma + refage + stage + histotype1 + dblkstat_CA125 + chemotherapy, data = AACES_tma) 
summary(surv_tma)











# ROI
AACES_roi <- AACES_data %>% filter(slide_type == "ROI") %>% 
  inner_join(., AACES_tx, by = "suid")

table(AACES_roi$neoadj)
table(AACES_roi$adj)

table(AACES_roi$med.CD3_total.i)
# Cox CD3
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_total.i + refage + stage, data = AACES_roi) 
summary(surv.i)
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_total.i + refage + stage + dblkstat_CA125, data = AACES_roi) 
summary(surv.i)
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_total.i + refage + stage + dblkstat_CA125 + neoadj, data = AACES_roi) 
summary(surv.i)
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_total.i + refage + stage + dblkstat_CA125 + adj, data = AACES_roi) 
summary(surv.i)
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_total.i + refage + stage + dblkstat_CA125 + chemotherapy, data = AACES_roi) 
summary(surv.i)

table(AACES_roi$med.CD3_tumor.i)
# Cox CD3t
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_tumor.i + refage + stage, data = AACES_roi) 
summary(surv.i)
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_tumor.i + refage + stage + dblkstat_CA125, data = AACES_roi) 
summary(surv.i)
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_tumor.i + refage + stage + dblkstat_CA125 + neoadj, data = AACES_roi) 
summary(surv.i)
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_tumor.i + refage + stage + dblkstat_CA125 + adj, data = AACES_roi) 
summary(surv.i)
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_tumor.i + refage + stage + dblkstat_CA125 + chemotherapy, data = AACES_roi) 
summary(surv.i)

table(AACES_roi$med.CD3_stroma.i)
# Cox CD3s
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_stroma.i + refage + stage, data = AACES_roi) 
summary(surv.i)
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_stroma.i + refage + stage + dblkstat_CA125, data = AACES_roi) 
summary(surv.i)
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_stroma.i + refage + stage + dblkstat_CA125 + neoadj, data = AACES_roi) 
summary(surv.i)
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_stroma.i + refage + stage + dblkstat_CA125 + adj, data = AACES_roi) 
summary(surv.i)
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_stroma.i + refage + stage + dblkstat_CA125 + chemotherapy, data = AACES_roi) 
summary(surv.i)

table(AACES_roi$med.CD3_FoxP3_total.i)
# Cox CD3FoxP3
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_FoxP3_total.i + refage + stage, data = AACES_roi) 
summary(surv.i)
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_FoxP3_total.i + refage + stage + dblkstat_CA125, data = AACES_roi) 
summary(surv.i)
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_FoxP3_total.i + refage + stage + dblkstat_CA125 + neoadj, data = AACES_roi) 
summary(surv.i)
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_FoxP3_total.i + refage + stage + dblkstat_CA125 + adj, data = AACES_roi) 
summary(surv.i)
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_FoxP3_total.i + refage + stage + dblkstat_CA125 + chemotherapy, data = AACES_roi) 
summary(surv.i)

table(AACES_roi$med.CD3_FoxP3_tumor.i)
# Cox CD3FoxP3t
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_FoxP3_tumor.i + refage + stage, data = AACES_roi) 
summary(surv.i)
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_FoxP3_tumor.i + refage + stage + dblkstat_CA125, data = AACES_roi) 
summary(surv.i)
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_FoxP3_tumor.i + refage + stage + dblkstat_CA125 + neoadj, data = AACES_roi) 
summary(surv.i)
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_FoxP3_tumor.i + refage + stage + dblkstat_CA125 + adj, data = AACES_roi) 
summary(surv.i)
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_FoxP3_tumor.i + refage + stage + dblkstat_CA125 + chemotherapy, data = AACES_roi) 
summary(surv.i)

table(AACES_roi$med.CD3_FoxP3_stroma.i)
# Cox CD3FoxP3s
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_FoxP3_stroma.i + refage + stage, data = AACES_roi) 
summary(surv.i)
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_FoxP3_stroma.i + refage + stage + dblkstat_CA125, data = AACES_roi) 
summary(surv.i)
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_FoxP3_stroma.i + refage + stage + dblkstat_CA125 + neoadj, data = AACES_roi) 
summary(surv.i)
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_FoxP3_stroma.i + refage + stage + dblkstat_CA125 + adj, data = AACES_roi) 
summary(surv.i)
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_FoxP3_stroma.i + refage + stage + dblkstat_CA125 + chemotherapy, data = AACES_roi) 
summary(surv.i)

table(AACES_roi$med.CD3_CD8_total.i)
# Cox CD3CD8
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_CD8_total.i + refage + stage, data = AACES_roi) 
summary(surv.i)
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_CD8_total.i + refage + stage + dblkstat_CA125, data = AACES_roi) 
summary(surv.i)
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_CD8_total.i + refage + stage + dblkstat_CA125 + neoadj, data = AACES_roi) 
summary(surv.i)
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_CD8_total.i + refage + stage + dblkstat_CA125 + adj, data = AACES_roi) 
summary(surv.i)
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_CD8_total.i + refage + stage + dblkstat_CA125 + chemotherapy, data = AACES_roi) 
summary(surv.i)

table(AACES_roi$med.CD3_CD8_tumor.i)
# Cox CD3CD8t
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_CD8_tumor.i + refage + stage, data = AACES_roi) 
summary(surv.i)
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_CD8_tumor.i + refage + stage + dblkstat_CA125, data = AACES_roi) 
summary(surv.i)
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_CD8_tumor.i + refage + stage + dblkstat_CA125 + neoadj, data = AACES_roi) 
summary(surv.i)
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_CD8_tumor.i + refage + stage + dblkstat_CA125 + adj, data = AACES_roi) 
summary(surv.i)
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_CD8_tumor.i + refage + stage + dblkstat_CA125 + chemotherapy, data = AACES_roi) 
summary(surv.i)

table(AACES_roi$med.CD3_CD8_stroma.i)
# Cox CD3CD8s
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_CD8_stroma.i + refage + stage, data = AACES_roi) 
summary(surv.i)
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_CD8_stroma.i + refage + stage + dblkstat_CA125, data = AACES_roi) 
summary(surv.i)
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_CD8_stroma.i + refage + stage + dblkstat_CA125 + neoadj, data = AACES_roi) 
summary(surv.i)
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_CD8_stroma.i + refage + stage + dblkstat_CA125 + adj, data = AACES_roi) 
summary(surv.i)
surv.i <- coxph(Surv(time = timelastfu_new, event = surv_vital)~med.CD3_CD8_stroma.i + refage + stage + dblkstat_CA125 + chemotherapy, data = AACES_roi) 
summary(surv.i)















