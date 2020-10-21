########################################################################################## I ### Recode clinical data----
clinical_data <- clinical_data %>% 
  mutate(suid = factor(suid)) %>% 
  mutate(casecon = case_when(
    casecon == 1                                       ~ "Case",
    casecon == 2                                       ~ "Control"
  )) %>% 
  mutate(vitalstatus = case_when(
    vitalstatus_new == 1                                   ~ "Alive",
    vitalstatus_new == 2                                   ~ "Deceased",
    TRUE                                               ~ NA_character_
  )) %>% 
  mutate(surv_vital = case_when(
    vitalstatus == "Alive"                             ~ 0,
    vitalstatus == "Deceased"                          ~ 1,
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
    TRUE                                                ~ NA_character_
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
    TRUE                                                ~ NA_character_
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
    married == 2                                        ~ "married/living as married"
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
  ))

# x <- clinical_data %>% drop_na("menopause_age") %>% # To answer why is there 777 in menopause_age -> removed
#   select("suid", "hyster", "menopause_age", "menopause", "periodstopreason")


######################################################################################## Ia ### Add paired_id to clinical----
# Add paired_id to clinical----
cases_match <- cases_match %>% mutate(suid = as.character(suid))
clinical_data <- full_join(cases_match, 
                           clinical_data,
                           by= "suid")


######################################################################################### II ### Cleaning TMAs, ROIs data----
# Keep the control slides for comparison?----
# other_tissue = 1 are controls used for QC purposes
# They may be benign or cancerous tissue from other anatomical sites - like tonsil or spleen
# TMA_Tctrl <- TMA_tumor %>% filter(!is.na(other_tissue))
# TMA_Sctrl <- TMA_stroma %>% filter(!is.na(other_tissue))


# 2.1.Remove the TMA IDs of excluded patient from the study----
# Should only be done for TMAs
# Plus remove TMA with no IDs = controls images
uid <- paste(unique(TMAcases_remove$Subject_IDs), collapse = "|")
TMA_tumor <-
  TMA_tumor[(!grepl(uid, TMA_tumor$suid)), ] %>% 
  filter(!is.na(suid))
TMA_stroma <-
  TMA_stroma[(!grepl(uid, TMA_stroma$suid)),] %>% 
  filter(!is.na(suid))
TMA_total <-
  TMA_total[(!grepl(uid, TMA_total$suid)),] %>% 
  filter(!is.na(suid))
# Did it for ROIs too in case, confirm no cases to remove


# 2.2.Create suid for ROIs----
ROI_tumor$suid <- str_match(ROI_tumor$image_tag, 
                            "(Peres_P1_AACES.|Peres_P1_AACEES.|Peres_P1_OV|Peres_P1.|)([:digit:]*)")[,3]
ROI_stroma$suid <- str_match(ROI_stroma$image_tag, 
                             "(Peres_P1_AACES.|Peres_P1_AACEES.|Peres_P1_OV|Peres_P1.|)([:digit:]*)")[,3]
ROI_total$suid <- str_match(ROI_total$image_tag, 
                             "(Peres_P1_AACES.|Peres_P1_AACEES.|Peres_P1_OV|Peres_P1.|)([:digit:]*)")[,3]


# 2.3.Merging stroma and tumor for TMAs and ROIs----
# Add % tumor cells and % stroma cells within each ROI/TMA core
ROI_global <- 
  full_join(ROI_tumor, ROI_stroma %>% select(-c("intratumoral_i_vs_peripheral_p_", "suid")),
                               by = "image_tag") %>% 
  full_join(., ROI_total %>% select(-c("intratumoral_i_vs_peripheral_p_", "suid")),
                   by = "image_tag") %>% 
  mutate(percent_tumor = round((tumor_total_cells / total_cells)*100, 2) # Calculate percent of tumor cell
         ) %>% 
  mutate(percent_stroma = round((stroma_total_cells / total_cells)*100, 2) # Calculate percent of stromal cell
         ) %>% 
  mutate(percent_total = round((total_cells / total_cells)*100, 2) # Calculate percent of stromal cell
  ) %>% 
  mutate_at(("intratumoral_i_vs_peripheral_p_"), ~ case_when(
    intratumoral_i_vs_peripheral_p_ == "p" ~ "Peripheral",
    intratumoral_i_vs_peripheral_p_ == "i" ~ "Intratumoral")
    ) %>% 
  mutate(suid = as.character(suid)) %>% 
  select(suid, everything())#  %>% 
  # mutate(CD3_tumor_mm2 = (tumor_cd3_opal_650_positive_cells/tumor_area_analyzed_mm2_)) %>% # density of marker per mm2
  # mutate(CD3_stroma_mm2 = (stroma_cd3_opal_650_positive_cells/stroma_area_analyzed_mm2_)) %>% 
  # mutate(CD3_CD8_tumor_mm2 = (tumor_cd3plus_cd8plus_cells/tumor_area_analyzed_mm2_)) %>% 
  # mutate(CD3_CD8_stroma_mm2 = (stroma_cd3plus_cd8plus_cells/stroma_area_analyzed_mm2_)) %>% 
  # mutate(CD3_FoxP3_tumor_mm2 = (tumor_cd3plus_foxp3plus_cells/tumor_area_analyzed_mm2_)) %>% 
  # mutate(CD3_FoxP3_stroma_mm2 = (stroma_cd3plus_foxp3plus_cells/stroma_area_analyzed_mm2_)) %>% 
  # mutate(CD11b_tumor = (tumor_cd11b_opal_620_positive_cells/tumor_area_analyzed_mm2_)) %>% 
  # mutate(CD11b_stroma = (stroma_cd11b_opal_620_positive_cells/stroma_area_analyzed_mm2_)) %>% 
  # mutate(CD11b_CD15_tumor_mm2 = (tumor_cd11bplus_cd15plus_cells/tumor_area_analyzed_mm2_)) %>% 
  # mutate(CD11b_CD15_stroma_mm2 = stroma_cd11bplus_cd15plus_cells/stroma_area_analyzed_mm2_) # %>% 
  
  # mutate(CD3perc_tumor_mm2 = (tumor_percent_cd3_opal_650_positive_cells/tumor_area_analyzed_mm2_)) %>% # percent of marker per mm2
  # mutate(CD3perc_stroma_mm2 = (stroma_percent_cd3_opal_650_positive_cells/stroma_area_analyzed_mm2_)) %>%
  # mutate(CD3_CD8perc_tumor_mm2 = (tumor_percent_cd3plus_cd8plus_positive_cells/tumor_area_analyzed_mm2_)) %>%
  # mutate(CD3_CD8perc_stroma_mm2 = (stroma_percent_cd3plus_cd8plus_positive_cells/stroma_area_analyzed_mm2_)) %>%
  # mutate(CD3_FoxP3perc_tumor_mm2 = (tumor_percent_cd3plus_foxp3plus_positive_cells/tumor_area_analyzed_mm2_)) %>%
  # mutate(CD3_FoxP3perc_stroma_mm2 = (stroma_percent_cd3plus_foxp3plus_positive_cells/stroma_area_analyzed_mm2_)) %>%
  # mutate(CD11bperc_tumor = (tumor_percent_cd11b_opal_620_positive_cells/tumor_area_analyzed_mm2_)) %>%
  # mutate(CD11bperc_stroma = (stroma_percent_cd11b_opal_620_positive_cells/stroma_area_analyzed_mm2_)) %>%
  # mutate(CD11b_CD15perc_tumor_mm2 = (tumor_percent_cd11bplus_cd15plus_positive_cells/tumor_area_analyzed_mm2_)) %>%
  # mutate(CD11b_CD15perc_stroma_mm2 = stroma_percent_cd11bplus_cd15plus_positive_cells/stroma_area_analyzed_mm2_)


TMA_global <- 
  full_join(TMA_tumor, TMA_stroma %>% select(-suid),
            by = "image_tag") %>% 
  full_join(., TMA_total %>% select(-suid),
            by = "image_tag") %>% 
  mutate(percent_tumor = round((tumor_total_cells / total_cells)*100, 2)
         ) %>% 
  mutate(percent_stroma = round((stroma_total_cells / total_cells)*100, 2)
         ) %>% 
  mutate(percent_total = round((total_cells / total_cells)*100, 2)
  ) %>% 
  mutate(suid = as.character(suid)) %>% 
  select(suid, everything())# %>% 
  # mutate(CD3_tumor_mm2 = (tumor_cd3_opal_650_positive_cells/tumor_area_analyzed_mm2_)) %>% 
  # mutate(CD3_stroma_mm2 = (stroma_cd3_opal_650_positive_cells/stroma_area_analyzed_mm2_)) %>% 
  # mutate(CD3_CD8_tumor_mm2 = (tumor_cd3plus_cd8plus_cells/tumor_area_analyzed_mm2_)) %>% 
  # mutate(CD3_CD8_stroma_mm2 = (stroma_cd3plus_cd8plus_cells/stroma_area_analyzed_mm2_)) %>% 
  # mutate(CD3_FoxP3_tumor_mm2 = (tumor_cd3plus_foxp3plus_cells/tumor_area_analyzed_mm2_)) %>% 
  # mutate(CD3_FoxP3_stroma_mm2 = (stroma_cd3plus_foxp3plus_cells/stroma_area_analyzed_mm2_)) %>% 
  # mutate(CD11b_tumor = (tumor_cd11b_opal_620_positive_cells/tumor_area_analyzed_mm2_)) %>% 
  # mutate(CD11b_stroma = (stroma_cd11b_opal_620_positive_cells/stroma_area_analyzed_mm2_)) %>% 
  # mutate(CD11b_CD15_tumor_mm2 = (tumor_cd11bplus_cd15plus_cells/tumor_area_analyzed_mm2_)) %>% 
  # mutate(CD11b_CD15_stroma_mm2 = stroma_cd11bplus_cd15plus_cells/stroma_area_analyzed_mm2_) # %>% 

  # mutate(CD3perc_tumor_mm2 = (tumor_percent_cd3_opal_650_positive_cells/tumor_area_analyzed_mm2_)) %>%
  # mutate(CD3perc_stroma_mm2 = (stroma_percent_cd3_opal_650_positive_cells/stroma_area_analyzed_mm2_)) %>%
  # mutate(CD3_CD8perc_tumor_mm2 = (tumor_percent_cd3plus_cd8plus_positive_cells/tumor_area_analyzed_mm2_)) %>%
  # mutate(CD3_CD8perc_stroma_mm2 = (stroma_percent_cd3plus_cd8plus_positive_cells/stroma_area_analyzed_mm2_)) %>%
  # mutate(CD3_FoxP3perc_tumor_mm2 = (tumor_percent_cd3plus_foxp3plus_positive_cells/tumor_area_analyzed_mm2_)) %>%
  # mutate(CD3_FoxP3perc_stroma_mm2 = (stroma_percent_cd3plus_foxp3plus_positive_cells/stroma_area_analyzed_mm2_)) %>%
  # mutate(CD11bperc_tumor = (tumor_percent_cd11b_opal_620_positive_cells/tumor_area_analyzed_mm2_)) %>%
  # mutate(CD11bperc_stroma = (stroma_percent_cd11b_opal_620_positive_cells/stroma_area_analyzed_mm2_)) %>%
  # mutate(CD11b_CD15perc_tumor_mm2 = (tumor_percent_cd11bplus_cd15plus_positive_cells/tumor_area_analyzed_mm2_)) %>%
  # mutate(CD11b_CD15perc_stroma_mm2 = stroma_percent_cd11bplus_cd15plus_positive_cells/stroma_area_analyzed_mm2_)


######################################################################################### III ### Summarize new var----
# Summarize immune markers using % Cell
markers_TMA <- group_by(TMA_global, suid) %>%
  summarize(
    mean_tumor = mean(percent_tumor),
    mean_stroma = mean(percent_stroma),
    mean_total = mean(percent_total),
    # variance_tumor = var(percent_tumor),
    # variance_stroma = var(percent_stroma),
    percent_CD3_tumor = mean(tumor_percent_cd3_opal_650_positive_cells),
    percent_CD8_tumor = mean(tumor_percent_cd8_opal_570_positive_cells),
    percent_CD3_CD8_tumor = mean(tumor_percent_cd3plus_cd8plus_positive_cells),
    percent_FoxP3_tumor = mean(tumor_percent_foxp3_opal_540_positive_cells),
    percent_CD3_FoxP3_tumor = mean(tumor_percent_cd3plus_foxp3plus_positive_cells),
    percent_CD11b_tumor = mean(tumor_percent_cd11b_opal_620_positive_cells),
    percent_CD15_tumor = mean(tumor_percent_cd15_opal_520_positive_cells),
    percent_CD11b_CD15_tumor = mean(tumor_percent_cd11bplus_cd15plus_positive_cells),
    percent_CD3_stroma = mean(stroma_percent_cd3_opal_650_positive_cells),
    percent_CD8_stroma = mean(stroma_percent_cd8_opal_570_positive_cells),
    percent_CD3_CD8_stroma = mean(stroma_percent_cd3plus_cd8plus_positive_cells),
    percent_FoxP3_stroma = mean(stroma_percent_foxp3_opal_540_positive_cells),
    percent_CD3_FoxP3_stroma = mean(stroma_percent_cd3plus_foxp3plus_positive_cells),
    percent_CD11b_stroma = mean(stroma_percent_cd11b_opal_620_positive_cells),
    percent_CD15_stroma = mean(stroma_percent_cd15_opal_520_positive_cells),
    percent_CD11b_CD15_stroma = mean(stroma_percent_cd11bplus_cd15plus_positive_cells),
    
    percent_CD3_total = mean(percent_cd3_opal_650_positive_cells),
    percent_CD8_total = mean(percent_cd8_opal_570_positive_cells),
    percent_CD3_CD8_total = mean(percent_cd3plus_cd8plus_positive_cells),
    percent_FoxP3_total = mean(percent_foxp3_opal_540_positive_cells),
    percent_CD3_FoxP3_total = mean(percent_cd3plus_foxp3plus_positive_cells),
    percent_CD11b_total = mean(percent_cd11b_opal_620_positive_cells),
    percent_CD15_total = mean(percent_cd15_opal_520_positive_cells),
    percent_CD11b_CD15_total = mean(percent_cd11bplus_cd15plus_positive_cells)
  )
markers_TMA$tumor_variation <- markers_TMA$mean_tumor - mean(TMA_global$percent_tumor)
markers_TMA$stroma_variation <- markers_TMA$mean_stroma - mean(TMA_global$percent_stroma)
markers_TMA$total_variation <- markers_TMA$mean_stroma - mean(TMA_global$percent_stroma)
# Create SQRT transformation
sqrt.markers <- sqrt(markers_TMA[,c(5:28)])
colnames(sqrt.markers) <- c("sqrt_CD3_tumor", "sqrt_CD8_tumor", "sqrt_CD3_CD8_tumor", "sqrt_FoxP3_tumor",
                            "sqrt_CD3_FoxP3_tumor", "sqrt_CD11b_tumor", "sqrt_CD15_tumor", 
                            "sqrt_CD11b_CD15_tumor", 
                            "sqrt_CD3_stroma", "sqrt_CD8_stroma",
                            "sqrt_CD3_CD8_stroma", "sqrt_FoxP3_stroma", "sqrt_CD3_FoxP3_stroma",
                            "sqrt_CD11b_stroma", "sqrt_CD15_stroma", "sqrt_CD11b_CD15_stroma",
                            "sqrt_CD3_total", "sqrt_CD8_total",
                            "sqrt_CD3_CD8_total", "sqrt_FoxP3_total", "sqrt_CD3_FoxP3_total",
                            "sqrt_CD11b_total", "sqrt_CD15_total", "sqrt_CD11b_CD15_total")
markers_TMA <- cbind(markers_TMA, sqrt.markers)

#
markers_ROIi <- ROI_global %>% 
  filter(intratumoral_i_vs_peripheral_p_ == "Intratumoral") %>% 
  group_by(suid) %>% 
  summarize(
    mean_tumor = mean(percent_tumor),
    mean_stroma = mean(percent_stroma),
    mean_total = mean(percent_total),
    # variance_tumor = var(percent_tumor),
    # variance_stroma = var(percent_stroma),
    percent_CD3_tumor = mean(tumor_percent_cd3_opal_650_positive_cells),
    percent_CD8_tumor = mean(tumor_percent_cd8_opal_570_positive_cells),
    percent_CD3_CD8_tumor = mean(tumor_percent_cd3plus_cd8plus_positive_cells),
    percent_FoxP3_tumor = mean(tumor_percent_foxp3_opal_540_positive_cells),
    percent_CD3_FoxP3_tumor = mean(tumor_percent_cd3plus_foxp3plus_positive_cells),
    percent_CD11b_tumor = mean(tumor_percent_cd11b_opal_620_positive_cells),
    percent_CD15_tumor = mean(tumor_percent_cd15_opal_520_positive_cells),
    percent_CD11b_CD15_tumor = mean(tumor_percent_cd11bplus_cd15plus_positive_cells),
    percent_CD3_stroma = mean(stroma_percent_cd3_opal_650_positive_cells),
    percent_CD8_stroma = mean(stroma_percent_cd8_opal_570_positive_cells),
    percent_CD3_CD8_stroma = mean(stroma_percent_cd3plus_cd8plus_positive_cells),
    percent_FoxP3_stroma = mean(stroma_percent_foxp3_opal_540_positive_cells),
    percent_CD3_FoxP3_stroma = mean(stroma_percent_cd3plus_foxp3plus_positive_cells),
    percent_CD11b_stroma = mean(stroma_percent_cd11b_opal_620_positive_cells),
    percent_CD15_stroma = mean(stroma_percent_cd15_opal_520_positive_cells),
    percent_CD11b_CD15_stroma = mean(stroma_percent_cd11bplus_cd15plus_positive_cells),
    
    percent_CD3_total = mean(percent_cd3_opal_650_positive_cells),
    percent_CD8_total = mean(percent_cd8_opal_570_positive_cells),
    percent_CD3_CD8_total = mean(percent_cd3plus_cd8plus_positive_cells),
    percent_FoxP3_total = mean(percent_foxp3_opal_540_positive_cells),
    percent_CD3_FoxP3_total = mean(percent_cd3plus_foxp3plus_positive_cells),
    percent_CD11b_total = mean(percent_cd11b_opal_620_positive_cells),
    percent_CD15_total = mean(percent_cd15_opal_520_positive_cells),
    percent_CD11b_CD15_total = mean(percent_cd11bplus_cd15plus_positive_cells)
  )
markers_ROIi$tumor_variation <- markers_ROIi$mean_tumor - mean(ROI_global$percent_tumor)
markers_ROIi$stroma_variation <- markers_ROIi$mean_stroma - mean(ROI_global$percent_stroma)
markers_ROIi$total_variation <- markers_ROIi$mean_stroma - mean(ROI_global$percent_stroma)
#
markers_ROIp <- ROI_global %>% 
  filter(intratumoral_i_vs_peripheral_p_ == "Peripheral") %>% 
  group_by(suid) %>% 
  summarize(
    mean_tumor = mean(percent_tumor),
    mean_stroma = mean(percent_stroma),
    mean_total = mean(percent_total),
    # variance_tumor = var(percent_tumor),
    # variance_stroma = var(percent_stroma),
    percent_CD3_tumor = mean(tumor_percent_cd3_opal_650_positive_cells),
    percent_CD8_tumor = mean(tumor_percent_cd8_opal_570_positive_cells),
    percent_CD3_CD8_tumor = mean(tumor_percent_cd3plus_cd8plus_positive_cells),
    percent_FoxP3_tumor = mean(tumor_percent_foxp3_opal_540_positive_cells),
    percent_CD3_FoxP3_tumor = mean(tumor_percent_cd3plus_foxp3plus_positive_cells),
    percent_CD11b_tumor = mean(tumor_percent_cd11b_opal_620_positive_cells),
    percent_CD15_tumor = mean(tumor_percent_cd15_opal_520_positive_cells),
    percent_CD11b_CD15_tumor = mean(tumor_percent_cd11bplus_cd15plus_positive_cells),
    percent_CD3_stroma = mean(stroma_percent_cd3_opal_650_positive_cells),
    percent_CD8_stroma = mean(stroma_percent_cd8_opal_570_positive_cells),
    percent_CD3_CD8_stroma = mean(stroma_percent_cd3plus_cd8plus_positive_cells),
    percent_FoxP3_stroma = mean(stroma_percent_foxp3_opal_540_positive_cells),
    percent_CD3_FoxP3_stroma = mean(stroma_percent_cd3plus_foxp3plus_positive_cells),
    percent_CD11b_stroma = mean(stroma_percent_cd11b_opal_620_positive_cells),
    percent_CD15_stroma = mean(stroma_percent_cd15_opal_520_positive_cells),
    percent_CD11b_CD15_stroma = mean(stroma_percent_cd11bplus_cd15plus_positive_cells),
    percent_CD3_total = mean(percent_cd3_opal_650_positive_cells),
    percent_CD8_total = mean(percent_cd8_opal_570_positive_cells),
    percent_CD3_CD8_total = mean(percent_cd3plus_cd8plus_positive_cells),
    percent_FoxP3_total = mean(percent_foxp3_opal_540_positive_cells),
    percent_CD3_FoxP3_total = mean(percent_cd3plus_foxp3plus_positive_cells),
    percent_CD11b_total = mean(percent_cd11b_opal_620_positive_cells),
    percent_CD15_total = mean(percent_cd15_opal_520_positive_cells),
    percent_CD11b_CD15_total = mean(percent_cd11bplus_cd15plus_positive_cells)
  )
markers_ROIp$tumor_variation <- markers_ROIp$mean_tumor - mean(ROI_global$percent_tumor)
markers_ROIp$stroma_variation <- markers_ROIp$mean_stroma - mean(ROI_global$percent_stroma)
markers_ROIp$total_variation <- markers_ROIp$mean_total - mean(ROI_global$percent_total)

# Join Intratumoral and Peropheral ROI
markers_ROI <- full_join(markers_ROIi, markers_ROIp,
                         by= "suid", suffix= c(".i", ".p"))
# Create SQRT transformation
sqrt.markers <- sqrt(markers_ROI[,c(5:28, 35:58)])
colnames(sqrt.markers) <- c("sqrt_CD3_tumor.i", "sqrt_CD8_tumor.i", "sqrt_CD3_CD8_tumor.i", "sqrt_FoxP3_tumor.i",
                            "sqrt_CD3_FoxP3_tumor.i", "sqrt_CD11b_tumor.i", "sqrt_CD15_tumor.i", 
                            "sqrt_CD11b_CD15_tumor.i", "sqrt_CD3_stroma.i", "sqrt_CD8_stroma.i",
                            "sqrt_CD3_CD8_stroma.i", "sqrt_FoxP3_stroma.i", "sqrt_CD3_FoxP3_stroma.i",
                            "sqrt_CD11b_stroma.i", "sqrt_CD15_stroma.i", "sqrt_CD11b_CD15_stroma.i",
                            "sqrt_CD3_total.i", "sqrt_CD8_total.i", "sqrt_CD3_CD8_total.i",
                            "sqrt_FoxP3_total.i", "sqrt_CD3_FoxP3_total.i", "sqrt_CD11b_total.i",      
                            "sqrt_CD15_total.i", "sqrt_CD11b_CD15_total.i",
                            
                            "sqrt_CD3_tumor.p", "sqrt_CD8_tumor.p", "sqrt_CD3_CD8_tumor.p", "sqrt_FoxP3_tumor.p",
                            "sqrt_CD3_FoxP3_tumor.p", "sqrt_CD11b_tumor.p", "sqrt_CD15_tumor.p", 
                            "sqrt_CD11b_CD15_tumor.p", "sqrt_CD3_stroma.p", "sqrt_CD8_stroma.p",
                            "sqrt_CD3_CD8_stroma.p", "sqrt_FoxP3_stroma.p", "sqrt_CD3_FoxP3_stroma.p",
                            "sqrt_CD11b_stroma.p", "sqrt_CD15_stroma.p", "sqrt_CD11b_CD15_stroma.p",
                            "sqrt_CD3_total.p", "sqrt_CD8_total.p", "sqrt_CD3_CD8_total.p",
                            "sqrt_FoxP3_total.p", "sqrt_CD3_FoxP3_total.p", "sqrt_CD11b_total.p",      
                            "sqrt_CD15_total.p", "sqrt_CD11b_CD15_total.p")
markers_ROI <- cbind(markers_ROI, sqrt.markers)


######################################################################################## IV ### Merging all df----
colnames(markers_TMA)[2:ncol(markers_TMA)] <- paste(colnames(markers_TMA)[2:ncol(markers_TMA)], "tma", sep = "_")
markers <- full_join(markers_TMA, markers_ROI,
                     by= "suid")
markers <- left_join(markers, clinical_data, by="suid")


######################################################################################## IV ### Create tertile group with immune cells----
markers <- markers %>% 
  # mutate(temp= case_when(
  #   sqrt_CD3_total.i > 0 ~ sqrt_CD3_total.i
  # )) %>% 
  mutate(tertile = ntile(sqrt_CD3_total.i, 3)) %>% 
  mutate(CD3_grp = case_when(
    # sqrt_CD3_total.i == 0 ~ "Absent",
    tertile == 1 ~ "Low",
    tertile == 2 ~ "Medium",
    tertile == 3 ~ "High",
  )) %>% 
  mutate(CD3_grp = factor(.$CD3_grp, levels = c("Low","Medium","High"))) %>% 
  mutate(tertile = ntile(sqrt_CD3_tumor.i, 3)) %>% 
  mutate(CD3t_grp = case_when(
    tertile == 1 ~ "Low",
    tertile == 2 ~ "Medium",
    tertile == 3 ~ "High",
  )) %>% 
  mutate(CD3t_grp = factor(.$CD3t_grp, levels = c("Low","Medium","High"))) %>% 
  mutate(tertile = ntile(sqrt_CD3_stroma.i, 3)) %>% 
  mutate(CD3s_grp = case_when(
    tertile == 1 ~ "Low",
    tertile == 2 ~ "Medium",
    tertile == 3 ~ "High",
  )) %>% 
  mutate(CD3s_grp = factor(.$CD3s_grp, levels = c("Low","Medium","High"))) %>% 
  # mutate(temp= case_when(
  #   sqrt_CD3_CD8_total.i > 0 ~ sqrt_CD3_CD8_total.i
  # )) %>% 
  mutate(tertile = ntile(sqrt_CD3_CD8_total.i, 3)) %>% 
  mutate(CD3_CD8_grp = case_when(
    tertile == 1 ~ "Low",
    tertile == 2 ~ "Medium",
    tertile == 3 ~ "High",
  )) %>% 
  mutate(CD3_CD8_grp = factor(.$CD3_CD8_grp, levels = c("Low","Medium","High"))) %>% 
  mutate(tertile = ntile(sqrt_CD3_CD8_tumor.i, 3)) %>% 
  mutate(CD3_CD8t_grp = case_when(
    tertile == 1 ~ "Low",
    tertile == 2 ~ "Medium",
    tertile == 3 ~ "High",
  )) %>% 
  mutate(CD3_CD8t_grp = factor(.$CD3_CD8t_grp, levels = c("Low","Medium","High"))) %>% 
  mutate(tertile = ntile(sqrt_CD3_CD8_stroma.i, 3)) %>% 
  mutate(CD3_CD8s_grp = case_when(
    tertile == 1 ~ "Low",
    tertile == 2 ~ "Medium",
    tertile == 3 ~ "High",
  )) %>% 
  mutate(CD3_CD8s_grp = factor(.$CD3_CD8s_grp, levels = c("Low","Medium","High"))) %>% 
  # mutate(temp= case_when(
  #   sqrt_CD3_FoxP3_total.i > 0 ~ sqrt_CD3_FoxP3_total.i
  # )) %>% 
  mutate(tertile = ntile(sqrt_CD3_FoxP3_total.i, 3)) %>% 
  mutate(CD3_FoxP3_grp = case_when(
    tertile == 1 ~ "Low",
    tertile == 2 ~ "Medium",
    tertile == 3 ~ "High",
  )) %>% 
  mutate(CD3_FoxP3_grp = factor(.$CD3_FoxP3_grp, levels = c("Low","Medium","High"))) %>% 
  mutate(tertile = ntile(sqrt_CD3_FoxP3_tumor.i, 3)) %>% 
  mutate(CD3_FoxP3t_grp = case_when(
    tertile == 1 ~ "Low",
    tertile == 2 ~ "Medium",
    tertile == 3 ~ "High",
  )) %>% 
  mutate(CD3_FoxP3t_grp = factor(.$CD3_FoxP3t_grp, levels = c("Low","Medium","High"))) %>% 
  mutate(tertile = ntile(sqrt_CD3_FoxP3_stroma.i, 3)) %>% 
  mutate(CD3_FoxP3s_grp = case_when(
    tertile == 1 ~ "Low",
    tertile == 2 ~ "Medium",
    tertile == 3 ~ "High",
  )) %>% 
  mutate(CD3_FoxP3s_grp = factor(.$CD3_FoxP3s_grp, levels = c("Low","Medium","High"))) %>% 
  # mutate(temp= case_when(
  #   sqrt_CD11b_total.i > 0 ~ sqrt_CD11b_total.i
  # )) %>% 
  mutate(tertile = ntile(sqrt_CD11b_total.i, 2)) %>% 
  mutate(CD11b_grp = case_when(
    tertile == 1 ~ "Low",
    tertile == 2 ~ "High"
  )) %>% 
  mutate(CD11b_grp = factor(.$CD11b_grp, levels = c("Low","High"))) %>% 
  mutate(tertile = ntile(sqrt_CD11b_tumor.i, 2)) %>% 
  mutate(CD11bt_grp = case_when(
    tertile == 1 ~ "Low",
    tertile == 2 ~ "High",
  )) %>% 
  mutate(CD11bt_grp = factor(.$CD11bt_grp, levels = c("Low","High"))) %>% 
  mutate(tertile = ntile(sqrt_CD11b_stroma.i, 2)) %>% 
  mutate(CD11bs_grp = case_when(
    tertile == 1 ~ "Low",
    tertile == 2 ~ "High",
  )) %>% 
  mutate(CD11bs_grp = factor(.$CD11bs_grp, levels = c("Low","High"))) %>% 
  
  mutate(temp= case_when(
    sqrt_CD11b_CD15_total.i > 0 ~ sqrt_CD11b_CD15_total.i
  )) %>% 
  mutate(tertile = ntile(sqrt_CD11b_CD15_total.i, 2)) %>% 
  mutate(CD11b_CD15_grp = case_when(
    tertile == 1 ~ "Low",
    tertile == 2 ~ "High"
  )) %>% 
  mutate(CD11b_CD15_grp = factor(.$CD11b_CD15_grp, levels = c("Low","High"))) %>% 
  mutate(tertile = ntile(sqrt_CD11b_CD15_tumor.i, 2)) %>% 
  mutate(CD11b_CD15t_grp = case_when(
    tertile == 1 ~ "Low",
    tertile == 2 ~ "High",
  )) %>% 
  mutate(CD11b_CD15t_grp = factor(.$CD11b_CD15t_grp, levels = c("Low","High"))) %>% 
  mutate(tertile = ntile(sqrt_CD11b_CD15_stroma.i, 2)) %>% 
  mutate(CD11b_CD15s_grp = case_when(
    tertile == 1 ~ "Low",
    tertile == 2 ~ "High",
  )) %>% 
  mutate(CD11b_CD15s_grp = factor(.$CD11b_CD15s_grp, levels = c("Low","High"))) %>% 
  select(-c("temp", "tertile"))

# Tertile for peripheral marker
markers <- markers %>% 
  mutate(tertile = ntile(sqrt_CD3_total.p, 3)) %>% 
  mutate(CD3_grp_p = case_when(
    tertile == 1 ~ "Low",
    tertile == 2 ~ "Medium",
    tertile == 3 ~ "High",
  )) %>% 
  mutate(CD3_grp_p = factor(.$CD3_grp_p, levels = c("Low","Medium","High"))) %>% 
  mutate(tertile = ntile(sqrt_CD3_tumor.p, 3)) %>% 
  mutate(CD3t_grp_p = case_when(
    tertile == 1 ~ "Low",
    tertile == 2 ~ "Medium",
    tertile == 3 ~ "High",
  )) %>% 
  mutate(CD3t_grp_p = factor(.$CD3t_grp_p, levels = c("Low","Medium","High"))) %>% 
  mutate(tertile = ntile(sqrt_CD3_stroma.p, 3)) %>% 
  mutate(CD3s_grp_p = case_when(
    tertile == 1 ~ "Low",
    tertile == 2 ~ "Medium",
    tertile == 3 ~ "High",
  )) %>% 
  mutate(CD3s_grp_p = factor(.$CD3s_grp_p, levels = c("Low","Medium","High"))) %>% 
  mutate(tertile = ntile(sqrt_CD3_CD8_total.p, 3)) %>% 
  mutate(CD3_CD8_grp_p = case_when(
    tertile == 1 ~ "Low",
    tertile == 2 ~ "Medium",
    tertile == 3 ~ "High",
  )) %>% 
  mutate(CD3_CD8_grp_p = factor(.$CD3_CD8_grp_p, levels = c("Low","Medium","High"))) %>% 
  mutate(tertile = ntile(sqrt_CD3_CD8_tumor.p, 3)) %>% 
  mutate(CD3_CD8t_grp_p = case_when(
    tertile == 1 ~ "Low",
    tertile == 2 ~ "Medium",
    tertile == 3 ~ "High",
  )) %>% 
  mutate(CD3_CD8t_grp_p = factor(.$CD3_CD8t_grp_p, levels = c("Low","Medium","High"))) %>% 
  mutate(tertile = ntile(sqrt_CD3_CD8_stroma.p, 3)) %>% 
  mutate(CD3_CD8s_grp_p = case_when(
    tertile == 1 ~ "Low",
    tertile == 2 ~ "Medium",
    tertile == 3 ~ "High",
  )) %>% 
  mutate(CD3_CD8s_grp_p = factor(.$CD3_CD8s_grp_p, levels = c("Low","Medium","High"))) %>% 
  mutate(tertile = ntile(sqrt_CD3_FoxP3_total.p, 3)) %>% 
  mutate(CD3_FoxP3_grp_p = case_when(
    tertile == 1 ~ "Low",
    tertile == 2 ~ "Medium",
    tertile == 3 ~ "High",
  )) %>% 
  mutate(CD3_FoxP3_grp_p = factor(.$CD3_FoxP3_grp_p, levels = c("Low","Medium","High"))) %>% 
  mutate(tertile = ntile(sqrt_CD3_FoxP3_tumor.p, 3)) %>% 
  mutate(CD3_FoxP3t_grp_p = case_when(
    tertile == 1 ~ "Low",
    tertile == 2 ~ "Medium",
    tertile == 3 ~ "High",
  )) %>% 
  mutate(CD3_FoxP3t_grp_p = factor(.$CD3_FoxP3t_grp_p, levels = c("Low","Medium","High"))) %>% 
  mutate(tertile = ntile(sqrt_CD3_FoxP3_stroma.p, 3)) %>% 
  mutate(CD3_FoxP3s_grp_p = case_when(
    tertile == 1 ~ "Low",
    tertile == 2 ~ "Medium",
    tertile == 3 ~ "High",
  )) %>% 
  mutate(CD3_FoxP3s_grp_p = factor(.$CD3_FoxP3s_grp_p, levels = c("Low","Medium","High"))) %>% 
  mutate(tertile = ntile(sqrt_CD11b_total.p, 2)) %>% 
  mutate(CD11b_grp_p = case_when(
    tertile == 1 ~ "Low",
    tertile == 2 ~ "High"
  )) %>% 
  mutate(CD11b_grp_p = factor(.$CD11b_grp_p, levels = c("Low","High"))) %>% 
  mutate(tertile = ntile(sqrt_CD11b_tumor.p, 2)) %>% 
  mutate(CD11bt_grp_p = case_when(
    tertile == 1 ~ "Low",
    tertile == 2 ~ "High",
  )) %>% 
  mutate(CD11bt_grp_p = factor(.$CD11bt_grp_p, levels = c("Low","High"))) %>% 
  mutate(tertile = ntile(sqrt_CD11b_stroma.p, 2)) %>% 
  mutate(CD11bs_grp_p = case_when(
    tertile == 1 ~ "Low",
    tertile == 2 ~ "High",
  )) %>% 
  mutate(CD11bs_grp_p = factor(.$CD11bs_grp_p, levels = c("Low","High"))) %>% 
  
  mutate(temp= case_when(
    sqrt_CD11b_CD15_total.p > 0 ~ sqrt_CD11b_CD15_total.p
  )) %>% 
  mutate(tertile = ntile(sqrt_CD11b_CD15_total.p, 2)) %>% 
  mutate(CD11b_CD15_grp_p = case_when(
    tertile == 1 ~ "Low",
    tertile == 2 ~ "High"
  )) %>% 
  mutate(CD11b_CD15_grp_p = factor(.$CD11b_CD15_grp_p, levels = c("Low","High"))) %>% 
  mutate(tertile = ntile(sqrt_CD11b_CD15_tumor.p, 2)) %>% 
  mutate(CD11b_CD15t_grp_p = case_when(
    tertile == 1 ~ "Low",
    tertile == 2 ~ "High",
  )) %>% 
  mutate(CD11b_CD15t_grp_p = factor(.$CD11b_CD15t_grp_p, levels = c("Low","High"))) %>% 
  mutate(tertile = ntile(sqrt_CD11b_CD15_stroma.p, 2)) %>% 
  mutate(CD11b_CD15s_grp_p = case_when(
    tertile == 1 ~ "Low",
    tertile == 2 ~ "High",
  )) %>% 
  mutate(CD11b_CD15s_grp_p = factor(.$CD11b_CD15s_grp_p, levels = c("Low","High"))) %>% 
  
  select(-c("tertile"))

# 4.1. Create variation data ----
# Look at the variation between each patient and the global mean # Should we mot compare Black and White?
# Here compare the mean of % cells to global study % cells

var_markers <- data.frame(
  suid = markers$suid,
  tumor_variation_tma = c(markers$mean_tumor_tma - mean(markers$mean_tumor_tma, na.rm = TRUE)),
  stroma_variation_tma = c(markers$mean_stroma_tma - mean(markers$mean_stroma_tma, na.rm = TRUE)),
  var_CD3_tumor_tma = c(markers$percent_CD3_tumor_tma - mean(markers$percent_CD3_tumor_tma, na.rm = TRUE)),
  var_CD8_tumor_tma = c(markers$percent_CD8_tumor_tma - mean(markers$percent_CD8_tumor_tma, na.rm = TRUE)),
  var_CD3_CD8_tumor_tma = c(markers$percent_CD3_CD8_tumor_tma - mean(markers$percent_CD3_CD8_tumor_tma, na.rm = TRUE)),
  var_FoxP3_tumor_tma = c(markers$percent_FoxP3_tumor_tma - mean(markers$percent_FoxP3_tumor_tma, na.rm = TRUE)),
  var_CD3_FoxP3_tumor_tma = c(markers$percent_CD3_FoxP3_tumor_tma - mean(markers$percent_CD3_FoxP3_tumor_tma, na.rm = TRUE)),
  var_CD11b_tumor_tma = c(markers$percent_CD11b_tumor_tma - mean(markers$percent_CD11b_tumor_tma, na.rm = TRUE)),
  var_CD15_tumor_tma = c(markers$percent_CD15_tumor_tma - mean(markers$percent_CD15_tumor_tma, na.rm = TRUE)),
  var_CD11b_CD15_tumor_tma = c(markers$percent_CD11b_CD15_tumor_tma - mean(markers$percent_CD11b_CD15_tumor_tma, na.rm = TRUE)),
  var_CD3_stroma_tma = c(markers$percent_CD3_stroma_tma - mean(markers$percent_CD3_stroma_tma, na.rm = TRUE)),
  var_CD8_stroma_tma = c(markers$percent_CD8_stroma_tma - mean(markers$percent_CD8_stroma_tma, na.rm = TRUE)),
  var_CD3_CD8_stroma_tma = c(markers$percent_CD3_CD8_stroma_tma - mean(markers$percent_CD3_CD8_stroma_tma, na.rm = TRUE)),
  var_FoxP3_stroma_tma = c(markers$percent_FoxP3_stroma_tma - mean(markers$percent_FoxP3_stroma_tma, na.rm = TRUE)),
  var_CD3_FoxP3_stroma_tma = c(markers$percent_CD3_FoxP3_stroma_tma - mean(markers$percent_CD3_FoxP3_stroma_tma, na.rm = TRUE)),
  var_CD11b_stroma_tma = c(markers$percent_CD11b_stroma_tma - mean(markers$percent_CD11b_stroma_tma, na.rm = TRUE)),
  var_CD15_stroma_tma = c(markers$percent_CD15_stroma_tma - mean(markers$percent_CD15_stroma_tma, na.rm = TRUE)),
  var_CD11b_CD15_stroma_tma = c(markers$percent_CD11b_CD15_stroma_tma - mean(markers$percent_CD11b_CD15_stroma_tma, na.rm = TRUE)),
  var_CD3_total_tma = c(markers$percent_CD3_total_tma - mean(markers$percent_CD3_total_tma, na.rm = TRUE)),
  var_CD8_total_tma = c(markers$percent_CD8_total_tma - mean(markers$percent_CD8_total_tma, na.rm = TRUE)),
  var_CD3_CD8_total_tma = c(markers$percent_CD3_CD8_total_tma - mean(markers$percent_CD3_CD8_total_tma, na.rm = TRUE)),
  var_FoxP3_total_tma = c(markers$percent_FoxP3_total_tma - mean(markers$percent_FoxP3_total_tma, na.rm = TRUE)),
  
  tumor_variation.i = c(markers$mean_tumor.i - mean(markers$mean_tumor.i, na.rm = TRUE)),
  stroma_variation.i = c(markers$mean_stroma.i - mean(markers$mean_stroma.i, na.rm = TRUE)),
  var_CD3_tumor.i = c(markers$percent_CD3_tumor.i - mean(markers$percent_CD3_tumor.i, na.rm = TRUE)),
  var_CD8_tumor.i = c(markers$percent_CD8_tumor.i - mean(markers$percent_CD8_tumor.i, na.rm = TRUE)),
  var_CD3_CD8_tumor.i = c(markers$percent_CD3_CD8_tumor.i - mean(markers$percent_CD3_CD8_tumor.i, na.rm = TRUE)),
  var_FoxP3_tumor.i = c(markers$percent_FoxP3_tumor.i - mean(markers$percent_FoxP3_tumor.i, na.rm = TRUE)),
  var_CD3_FoxP3_tumor.i = c(markers$percent_CD3_FoxP3_tumor.i - mean(markers$percent_CD3_FoxP3_tumor.i, na.rm = TRUE)),
  var_CD11b_tumor.i = c(markers$percent_CD11b_tumor.i - mean(markers$percent_CD11b_tumor.i, na.rm = TRUE)),
  var_CD15_tumor.i = c(markers$percent_CD15_tumor.i - mean(markers$percent_CD15_tumor.i, na.rm = TRUE)),
  var_CD11b_CD15_tumor.i = c(markers$percent_CD11b_CD15_tumor.i - mean(markers$percent_CD11b_CD15_tumor.i, na.rm = TRUE)),
  var_CD3_stroma.i = c(markers$percent_CD3_stroma.i - mean(markers$percent_CD3_stroma.i, na.rm = TRUE)),
  var_CD8_stroma.i = c(markers$percent_CD8_stroma.i - mean(markers$percent_CD8_stroma.i, na.rm = TRUE)),
  var_CD3_CD8_stroma.i = c(markers$percent_CD3_CD8_stroma.i - mean(markers$percent_CD3_CD8_stroma.i, na.rm = TRUE)),
  var_FoxP3_stroma.i = c(markers$percent_FoxP3_stroma.i - mean(markers$percent_FoxP3_stroma.i, na.rm = TRUE)),
  var_CD3_FoxP3_stroma.i = c(markers$percent_CD3_FoxP3_stroma.i - mean(markers$percent_CD3_FoxP3_stroma.i, na.rm = TRUE)),
  var_CD11b_stroma.i = c(markers$percent_CD11b_stroma.i - mean(markers$percent_CD11b_stroma.i, na.rm = TRUE)),
  var_CD15_stroma.i = c(markers$percent_CD15_stroma.i - mean(markers$percent_CD15_stroma.i, na.rm = TRUE)),
  var_CD11b_CD15_stroma.i = c(markers$percent_CD11b_CD15_stroma.i - mean(markers$percent_CD11b_CD15_stroma.i, na.rm = TRUE)),
  var_CD3_total.i = c(markers$percent_CD3_total.i - mean(markers$percent_CD3_total.i, na.rm = TRUE)),
  var_CD8_total.i = c(markers$percent_CD8_total.i - mean(markers$percent_CD8_total.i, na.rm = TRUE)),
  var_CD3_CD8_total.i = c(markers$percent_CD3_CD8_total.i - mean(markers$percent_CD3_CD8_total.i, na.rm = TRUE)),
  var_FoxP3_total.i = c(markers$percent_FoxP3_total.i - mean(markers$percent_FoxP3_total.i, na.rm = TRUE)),
  
  tumor_variation.p = c(markers$mean_tumor.p - mean(markers$mean_tumor.p, na.rm = TRUE)),
  stroma_variation.p = c(markers$mean_stroma.p - mean(markers$mean_stroma.p, na.rm = TRUE)),
  var_CD3_tumor.p = c(markers$percent_CD3_tumor.p - mean(markers$percent_CD3_tumor.p, na.rm = TRUE)),
  var_CD8_tumor.p = c(markers$percent_CD8_tumor.p - mean(markers$percent_CD8_tumor.p, na.rm = TRUE)),
  var_CD3_CD8_tumor.p = c(markers$percent_CD3_CD8_tumor.p - mean(markers$percent_CD3_CD8_tumor.p, na.rm = TRUE)),
  var_FoxP3_tumor.p = c(markers$percent_FoxP3_tumor.p - mean(markers$percent_FoxP3_tumor.p, na.rm = TRUE)),
  var_CD3_FoxP3_tumor.p = c(markers$percent_CD3_FoxP3_tumor.p - mean(markers$percent_CD3_FoxP3_tumor.p, na.rm = TRUE)),
  var_CD11b_tumor.p = c(markers$percent_CD11b_tumor.p - mean(markers$percent_CD11b_tumor.p, na.rm = TRUE)),
  var_CD15_tumor.p = c(markers$percent_CD15_tumor.p - mean(markers$percent_CD15_tumor.p, na.rm = TRUE)),
  var_CD11b_CD15_tumor.p = c(markers$percent_CD11b_CD15_tumor.p - mean(markers$percent_CD11b_CD15_tumor.p, na.rm = TRUE)),
  var_CD3_stroma.p = c(markers$percent_CD3_stroma.p - mean(markers$percent_CD3_stroma.p, na.rm = TRUE)),
  var_CD8_stroma.p = c(markers$percent_CD8_stroma.p - mean(markers$percent_CD8_stroma.p, na.rm = TRUE)),
  var_CD3_CD8_stroma.p = c(markers$percent_CD3_CD8_stroma.p - mean(markers$percent_CD3_CD8_stroma.p, na.rm = TRUE)),
  var_FoxP3_stroma.p = c(markers$percent_FoxP3_stroma.p - mean(markers$percent_FoxP3_stroma.p, na.rm = TRUE)),
  var_CD3_FoxP3_stroma.p = c(markers$percent_CD3_FoxP3_stroma.p - mean(markers$percent_CD3_FoxP3_stroma.p, na.rm = TRUE)),
  var_CD11b_stroma.p = c(markers$percent_CD11b_stroma.p - mean(markers$percent_CD11b_stroma.p, na.rm = TRUE)),
  var_CD15_stroma.p = c(markers$percent_CD15_stroma.p - mean(markers$percent_CD15_stroma.p, na.rm = TRUE)),
  var_CD11b_CD15_stroma.p = c(markers$percent_CD11b_CD15_stroma.p - mean(markers$percent_CD11b_CD15_stroma.p, na.rm = TRUE)),
  var_CD3_total.p = c(markers$percent_CD3_total.p - mean(markers$percent_CD3_total.p, na.rm = TRUE)),
  var_CD8_total.p = c(markers$percent_CD8_total.p - mean(markers$percent_CD8_total.p, na.rm = TRUE)),
  var_CD3_CD8_total.p = c(markers$percent_CD3_CD8_total.p - mean(markers$percent_CD3_CD8_total.p, na.rm = TRUE)),
  var_FoxP3_total.p = c(markers$percent_FoxP3_total.p - mean(markers$percent_FoxP3_total.p, na.rm = TRUE))
)
uid <- paste(unique(common_ROITMA_IDs$Subject_IDs), collapse = '|')
var_markers28 <- var_markers[(grepl(uid, var_markers$suid)),]

# variation <- group_by(markers, suid) %>%
#   summarize(mean_tumor = mean(percent_tumor), mean_stroma = mean(percent_stroma), mean_total = mean(percent_total),
#             variance_tumor = var(percent_tumor), variance_stroma = var(percent_stroma)) %>%
#   mutate(ID = seq(1:nrow(.)))



# variations_TMA <- group_by(markers, suid) %>%
#   summarize(mean_tumor = mean(percent_tumor), mean_stroma = mean(percent_stroma), mean_total = mean(percent_total),
#             variance_tumor = var(percent_tumor), variance_stroma = var(percent_stroma)) %>%
#   mutate(ID = seq(1:nrow(.)))

# variations_ROIip <- group_by(ROI_global, suid, intratumoral_i_vs_peripheral_p_) %>% # mean of % cells separated by intra or perip
#   summarize(mean_tumor = mean(percent_tumor), mean_stroma = mean(percent_stroma),
#             variance_tumor = var(percent_tumor), variance_stroma = var(percent_stroma))
# setDT(variations_ROIip)[, ID := .GRP, .(suid)]
# variations_ROIip$tumor_variation <- variations_ROIip$mean_tumor - mean(ROI_global$percent_tumor)
# variations_ROIip$stroma_variation <- variations_ROIip$mean_stroma - mean(ROI_global$percent_stroma)
# 
# # Will commented because, after checking, it is not a good idea to combined all suid (I + P)
# variations_ROI <- group_by(variations_ROIip, suid) %>%
#   # summarize(mean_tumor = mean(mean_tumor), mean_stroma = mean(mean_stroma)) %>% # mean of % cells merging intra or perip
#   mutate(ID = seq(1:nrow(.)))
# # variations_ROI$tumor_variation <- variations_ROI$mean_tumor - mean(ROI_global$percent_tumor)
# # variations_ROI$stroma_variation <- variations_ROI$mean_stroma - mean(ROI_global$percent_stroma)


######################################################################################## V ### Create df for common 28 patients----
uid <- paste(unique(common_ROITMA_IDs$Subject_IDs), collapse = '|')
markers_28 <- markers[(grepl(uid, markers$suid)),]


######################################################################################## VI ### Create df----
global_28 <- left_join(TMA_global, ROI_global, by = "suid")
global_28 <- global_28[(grepl(uid, global_28$suid)),]





# Cleaning
rm(uid, TMAcases_remove, TMA_tumor, TMA_stroma, TMA_total,
   ROI_tumor, ROI_stroma, ROI_total,
   sqrt.markers, markers_ROIi, markers_ROIp,
   cases_match,
   common_ROITMA_IDs
   )



# End data cleaning----
