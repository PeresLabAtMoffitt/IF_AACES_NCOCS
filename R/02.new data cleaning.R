# Import library

library(tidyverse)


############################################################################## I ### Load new ROIs data----
path <- fs::path("","Volumes","Peres_Research")
fct_name_repair <- function(colnms) {
  tolower(gsub("[ ():]", "_", colnms))
}
#-----------------------------------------------------------------------------------------------------------------
aaces_clinical <- 
  read_csv(
    paste0(path,
           "/K99_R00/Image analysis data//AACES MCC18207 mIF Data/aaces_all.csv"))
ncocs_clinical <- 
  read_csv(
    paste0(path,
           "/K99_R00/Image analysis data//AACES MCC18207 mIF Data/ncocs_all.csv"))
#-----------------------------------------------------------------------------------------------------------------
ROI_tumor <-
  readxl::read_xlsx(
    paste0(path,
           "/K99_R00/Image analysis data//AACES MCC18207 mIF Data/L.Peres_P1_AACES MCC18207_ROI_Results.xlsx"
  ), sheet = "Tumor (PCK+)", .name_repair = fct_name_repair)
ROI_stroma <-
  readxl::read_xlsx(
    paste0(path,
           "/K99_R00/Image analysis data//AACES MCC18207 mIF Data/L.Peres_P1_AACES MCC18207_ROI_Results.xlsx"
  ), sheet = "Stroma (PCK-)", .name_repair = fct_name_repair)
ROI_total <-
  readxl::read_xlsx(
    paste0(path,
           "/K99_R00/Image analysis data//AACES MCC18207 mIF Data/L.Peres_P1_AACES MCC18207_ROI_Results.xlsx"
    ), sheet = "AACES MCC18207 ROI Counts", .name_repair = fct_name_repair)


############################################################################## II ### Cleaning new ROIs data----
ROI_tumor$suid <- str_match(ROI_tumor$image_tag, 
                            "(Peres_P1_AACES.|Peres_P1_AACEES.|Peres_P1_OV|Peres_P1.|)([:digit:]*)")[,3]
ROI_stroma$suid <- str_match(ROI_stroma$image_tag, 
                             "(Peres_P1_AACES.|Peres_P1_AACEES.|Peres_P1_OV|Peres_P1.|)([:digit:]*)")[,3]

ROI_tumor <- ROI_tumor %>% 
  mutate(image_tag1 = str_remove_all(image_tag, "-"),
         suid = str_match(image_tag1, 
                           "(L.Peres_P1_OV|L.Peres_P1_)([:digit:]*)")[,3]) %>% 
  select(image_tag, image_tag1, suid, everything())

ROI_stroma <- ROI_stroma %>% 
  mutate(image_tag1 = str_remove_all(image_tag, "-"),
         suid = str_match(image_tag1, 
                          "(L.Peres_P1_OV|L.Peres_P1_)([:digit:]*)")[,3]) %>% 
  select(image_tag, image_tag1, suid, everything())

ROI_total <- ROI_total %>% 
  mutate(image_tag1 = str_remove_all(image_tag, "-"),
         suid = str_match(image_tag1, 
                          "(L.Peres_P1_OV|L.Peres_P1_)([:digit:]*)")[,3]) %>% 
  select(image_tag, image_tag1, suid, everything())


str_match(ROI_tumor$image_tag, 
          "(L.Peres_P1_)([:digit:]*)")[,3]


# 110334? or 110334-3
# 16-1213??
L.Peres_P1_OV16-1084

############################################################################## III ### Cleaning clinical data----
clinical_data <- bind_rows(aaces_clinical, ncocs_clinical)


mutate(suid = factor(suid)) %>% 
  mutate(casecon = case_when(
    casecon == 1                                       ~ "Case",
    casecon == 2                                       ~ "Control"
  )) %>% 
  mutate(vitalstatus_old = case_when(
    vitalstatus_new == 1                               ~ "Alive",
    vitalstatus_new == 2                               ~ "Deceased",
    TRUE                                               ~ NA_character_
  )) %>% 
  mutate(surv_vital_old = case_when(
    vitalstatus_old == "Alive"                         ~ 0,
    vitalstatus_old == "Deceased"                      ~ 1,
    TRUE                                               ~ NA_real_
  )) %>% 
  mutate(cancersite = case_when(
    cancersite == 1                                    ~ "Ovarian",
    cancersite == 2                                    ~ "Tubal",
    cancersite == 3                                    ~ "Peritoneal",
    cancersite == 4                                    ~ "Ovarian or tubal, can't distinguish",
    cancersite == 5                                    ~ "Ovarian, tubal or peritoneal, can't distinguish",
    TRUE                                               ~ NA_character_
  )) %>% 
  mutate_at(c("timelastfu_new", "morphology", "hysteryear", "oophoryear", "tubeligyear",
              "anyfhdur", "eonlydur", "epdur"), 
            ~ case_when(
              . %in% c("8888","9998", "9999")          ~ NA_real_,
              TRUE                                     ~ as.numeric(.)
            )) %>% 
  mutate_at(c("refage", "pregmos", "agefbirth", "agelbirth", 
              "height", "wt_recent", "wt_YA", "wtgain", "BMI_recent", "BMI_YA",
              "hysterage", "oophorage", "tubeligage", "ocdur", "ocstart", "ocstop",
              "breastfedtimes", "breastfeddur", "menarch_age", "menopause_age", "talcgenfreq", 
              "talcgendur", "talcnongenfreq", "talcnongendur", "talcagegen", "talcagenongen",
              "endomet_age", "fibroids_age", "pid_age", "pcos_age", "ovcyst_age", "anyfhstart",
              "anyfhstop", "eonlystart", "eonlystop", "epstart", "epstop", "smokstart", "smokstop",
              "cigday", "smokyrs", "packyrs", "diabage", "hrtdisage", "hbpage", "hcholage", "osteoage",
              "thyrdage", "prvcanage", "prbreastage", "prcolage", "prcervage", "prlungage", 
              "prmelage", "prutage"), 
            ~ case_when(
              . %in% c("888","998", "999")             ~ NA_real_,
              TRUE                                     ~ as.numeric(.)
            )) %>% 
  mutate(BMI_classification = case_when(
    BMI_recent < 18.5	~ "underweight",
    BMI_recent >=18.5 & BMI_recent <25 ~ "normal",
    BMI_recent >=25.0 & BMI_recent <30 ~ "overweight",
    BMI_recent >=30.0 & BMI_recent <35 ~ "obesity I",
    BMI_recent >=35.0 & BMI_recent <40 ~ "obesity II",
    BMI_recent >= 40.0 ~ "obesity III"
  )) %>% 
  mutate_at(c("menopause_age"), 
            ~ case_when(
              . %in% c("777")                          ~ NA_real_,
              TRUE                                     ~ as.numeric(.)
            )) %>% 
  mutate_at(c("pregnum", "fullpregnum", "numsis", "numbro", "numsibs", "strenpa"), 
            ~ case_when(
              . %in% c("88","98", "99")                ~ NA_real_,
              TRUE                                     ~ as.numeric(.)
            )) %>% 
  mutate(histology = case_when(
    histology == 1                                     ~ "Serous",
    histology == 2                                     ~ "Endometrioid",
    histology == 3                                     ~ "Clear cell",
    histology == 4                                     ~ "Mucinous",
    histology == 5                                     ~ "Carcinosarcoma",
    histology == 6                                     ~ "Carcinoma, NOS",
    histology == 7                                     ~ "Other specified epithelial ovarian cancer \n(e.g. Malignant Brenner, mixed)",
    histology == 8                                     ~ "Epithelial, NOS",
    histology == 9                                     ~ "Synchronous",
    TRUE                                                ~ NA_character_
  )) %>% 
  mutate(behavior = case_when(
    behavior == 1                                      ~ "Borderline",
    behavior == 2                                      ~ "Invasive",
    TRUE                                                ~ NA_character_
  )) %>% 
  mutate(stage = case_when(
    stage == 1                                         ~ "Localized",
    stage == 2                                         ~ "Regional",
    stage == 3                                         ~ "Distant",
    TRUE                                                ~ NA_character_
  )) %>% 
  mutate(grade = case_when(
    grade == 1                                         ~ "well differentiated",
    grade == 2                                         ~ "moderately differentiated",
    grade == 3                                         ~ "poorly differentiated",
    grade == 4                                         ~ "undifferentiated",
    TRUE                                               ~ NA_character_
  )) %>% 
  mutate(histotype1 = case_when(
    histotype == 1                                     ~ "high-grade serous",
    histotype == 2                                     ~ "low-grade serous",
    histotype == 5 |
      histotype == 10                                  ~ "mucinous",
    histotype == 3                                     ~ "endometrioid",
    histotype == 4                                     ~ "clear cell",
    histotype %in% (6:13)                              ~ "other epithelial", # Will not take the 10
    TRUE                                               ~ NA_character_
  )) %>% 
  mutate(histotype2 = case_when(
    histotype == 1                                     ~ "high-grade serous",
    histotype %in% (2:13)                              ~ "non-high-grade serous",
    TRUE                                               ~ NA_character_
  )) %>%
  mutate(histotype = case_when(
    histotype == 1                                     ~ "high-grade serous",
    histotype == 2                                     ~ "low-grade serous",
    histotype == 3                                     ~ "endometrioid",
    histotype == 4                                     ~ "clear cell",
    histotype == 5                                     ~ "mucinous",
    histotype == 6                                     ~ "carcinosarcoma",
    histotype == 7                                     ~ "other epithelial ovarian cancer \n(e.g. Malignant Brenner, mixed, carcinoma, NOS)",
    histotype == 9                                     ~ "serous borderline",
    histotype == 10                                    ~ "mucinous borderline",
    histotype == 11                                    ~ "other epithelial borderline",
    histotype == 13                                    ~ "synchronous",
    TRUE                                               ~ NA_character_
  )) %>% 
  mutate(race = case_when(
    race == 1                                          ~ "White",
    race == 2                                          ~ "Black",
    race == 3                                          ~ "Biracial",
    TRUE                                                ~ NA_character_
  )) %>% 
  mutate(hispanic = case_when(
    hispanic == 1                                      ~ "Hispanic",
    hispanic == 2                                      ~ "Non-hispanic",
    TRUE                                                ~ NA_character_
  )) %>% 
  mutate(birthplace = case_when(
    birthplace == 1                                    ~ "born in United States",
    birthplace == 2                                    ~ "born outside of United States",
    TRUE                                                ~ NA_character_
  )) %>% 
  mutate(education = case_when(
    education == 1                                      ~ "high school graduate/GED or less",
    education == 2                                      ~ "some college",
    education == 3                                      ~ "college graduate",
    education == 4                                      ~ "graduate/professional school",
    TRUE                                                ~ NA_character_
  )) %>% 
  mutate(married = case_when(
    married == 1                                        ~ "single/never married",
    married == 2                                        ~ "married/living as married",
    married == 3                                        ~ "divorced/separated",
    married == 4                                        ~ "widowed"
  )) %>% 
  mutate_at(c("pregever", "nulliparous", "hyster", "hyster1yr", "hyster2yr", "oophor", 
              "oophor1yr", "tubelig", "tubelig1yr", "ocever", "breastfedever", 
              "talcever", "talcgen", "talcnongen", "talcpartner", "talc_occ", "brcancer",
              "brcancermom", "brcancersis", "brcancergrandma", "brcanceraunt", "brcancerdau",
              "brcancerdad", "brcancerbro", "brcancerson", "brcancerchildren", "ovcancermom",
              "ovcancersis", "ovcancergrandma", "ovcanceraunt", "ovcancerdaughter", "famhxbr",
              "famhxov", "endomet", "fibroids", "pid", "pcos", "ovcyst", "anyfhever", "eonlyever",
              "epever", "eonlyever", "aspirin", "NSAID", "aceta", "diab", "hrtdis", "hbp", 
              "hchol", "osteo", "prvcan", "prbreast",  "prcol", "prcerv", "prlung", "prmel", 
              "prut", "infert", "trypreg1yr", "mdvisit", "fertmed", "infertsurgivf"), 
            ~ case_when(
              . == 1                                              ~ "yes",
              . == 2                                              ~ "no",
              TRUE                                                ~ NA_character_
            )) %>%
  mutate(aspirin_use = factor(aspirin, levels = c("no", "yes"))) %>% 
  mutate(hysterreason = case_when(
    hysterreason == 1                                   ~ "Ovarian/FT/peritoneal cancer diagnosis",
    hysterreason == 2                                   ~ "Any reason not due to ovarian/FT/peritoneal cancer diagnosis",
    TRUE                                                ~ NA_character_
  )) %>% 
  mutate_at(c("hystertype", "menopause"), ~ case_when(
    . == 1                                              ~ "Premenopausal",
    . == 2                                              ~ "Postmenopausal",
    TRUE                                                ~ NA_character_
  )) %>% 
  mutate(oophortype = case_when(
    oophortype == 1                                     ~ "Unilateral",
    oophortype == 2                                     ~ "Bilateral",
    TRUE                                                ~ NA_character_
  )) %>% 
  mutate(ocindication = case_when(
    ocever == 1                                         ~ "birth control",
    ocever == 2                                         ~ "regulate menstrual periods",
    ocever == 3                                         ~ "acne or skin problems",
    ocever == 4                                         ~ "endometriosis",
    ocever == 5                                         ~ "premenstrual syndrome (PMS)",
    ocever == 6                                         ~ "menopausal symptoms",
    ocever == 7                                         ~ "vaginal bleeding",
    ocever == 8                                         ~ "other",
    TRUE                                                ~ NA_character_
  )) %>% 
  mutate(menarch_agecat = case_when(
    menarch_agecat == 1                                 ~ "<11 years",
    menarch_agecat == 2                                 ~ "11-12 years",
    menarch_agecat == 3                                 ~ "13-14 years",
    menarch_agecat == 4                                 ~ "15-16 years",
    menarch_agecat == 5                                 ~ "17 or older",
    TRUE                                                ~ NA_character_
  )) %>% 
  mutate(regperiod_agecat = case_when(
    regperiod_agecat == 1                               ~ "<11 years",
    regperiod_agecat == 2                               ~ "11-12 years",
    regperiod_agecat == 3                               ~ "13-14 years",
    regperiod_agecat == 4                               ~ "15-16 years",
    regperiod_agecat == 5                               ~ "17 or older",
    regperiod_agecat == 6                               ~ "never became regular",
    TRUE                                                ~ NA_character_
  )) %>% 
  mutate(periodstopreason = case_when(
    periodstopreason == 1                               ~ "stopped naturally",
    periodstopreason == 2                               ~ "stopped due to surgery",
    periodstopreason == 3                               ~ "stopped due to radiation or chemotherapy",
    periodstopreason == 4                               ~ "stopped due to hormonal medications (OC or menopausal hormone therapy)",
    periodstopreason == 5                               ~ "stopped due to other reason",
    TRUE                                                ~ NA_character_
  )) %>% 
  mutate_at(c("brcancermom_age", "brcancersis_age", "brcancergrandma_age", 
              "brcancerdau_age", "ovcancermom_age", "ovcancersis_age", 
              "ovcancergrandma_age", "ovcancerdaughter_age"), ~ case_when(
                . == 1                                              ~ "<50 years",
                . == 2                                              ~ "50+ years",
                TRUE                                                ~ NA_character_
              )) %>% 
  mutate(smokever = case_when(
    smokever == 1                                       ~ "ever",
    smokever == 2                                       ~ "never",
    TRUE                                                ~ NA_character_
  )) %>% 
  mutate(smokcurrent = case_when(
    smokcurrent == 1                                    ~ "current",
    smokcurrent == 2                                    ~ "never",
    smokcurrent == 3                                    ~ "former",
    TRUE                                                ~ NA_character_
  )) %>% 
  mutate(smoker = factor(smokcurrent, levels = c("never", "former", "current"))) %>% 
  mutate(thyrd = case_when(
    thyrd == 1                                          ~ "yes unspecified",
    thyrd == 2                                          ~ "no",
    thyrd == 3                                          ~ "yes hyperthyroid/overreactive",
    thyrd == 4                                          ~ "yes hypothyroid/underreactive",
    TRUE                                                ~ NA_character_
  )) %>% 
  mutate(mdvisitrsn = case_when(
    mdvisitrsn == 1                                     ~ "Female problem",
    mdvisitrsn == 2                                     ~ "Partner problem",
    mdvisitrsn == 3                                     ~ "Both female and partner problem",
    mdvisitrsn == 4                                     ~ "No problem was found",
    TRUE                                                ~ NA_character_
  )) %>% 
  mutate(diab = factor(diab, levels = c("no", "yes"))) %>% 
  # Calculate follow up time as time from interview to follow up
  mutate(timeint_fu = surv_days - timeint)


clinical_data$refage_cat <- as.factor(findInterval(clinical_data$refage, c(0,50,60,70,80)))
levels(clinical_data$refage_cat) <-  
  c("<50", "50-59", "60-69", "70-79", ">80")

clinical_data$BMI_recent_grp <- as.factor(findInterval(clinical_data$BMI_recent, c(0, 25, 30, 35)))
levels(clinical_data$BMI_recent_grp) <-
  c("<25", "25-29", "30-35", ">=35")


clinical_data$BMI_YA_grp <- as.factor(findInterval(clinical_data$BMI_YA, c(0, 20, 25)))
levels(clinical_data$BMI_YA_grp) <-  
  c("<20","20-24",">=25")
clinical_data$BMI_YA_grp <- factor(clinical_data$BMI_YA_grp, levels = c("20-24", "<20", ">=25"))

######################################################################################## Ib ### Add paired_id to clinical----
# Add paired_id to clinical----
cases_match <- cases_match %>% mutate(suid = as.character(suid))
clinical_data <- full_join(cases_match, 
                           clinical_data,
                           by= "suid")

saveRDS(clinical_data, file = "clinical_data.rds")







# End cleaning