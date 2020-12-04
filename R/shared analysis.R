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


