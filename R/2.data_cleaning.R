########################################################################################## I ### Recode clinical data----
clinical_data <- clinical_data %>% 
  mutate(suid = factor(suid)) %>% 
  mutate(casecon = case_when(
    casecon == 1                                       ~ "Case",
    casecon == 2                                       ~ "Control"
  )) %>% 
  mutate(vitalstatus = case_when(
    vitalstatus == 1                                   ~ "Alive",
    vitalstatus == 2                                   ~ "Deceased",
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
  mutate_at(c("timelastfu", "morphology", "hysteryear", "oophoryear", "tubeligyear",
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
    histology == 7                                     ~ "Other specified epithelial ovarian cancer (e.g. Malignant Brenner, mixed)",
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
    histotype == 7                                     ~ "other epithelial ovarian cancer (e.g. Malignant Brenner, mixed, carcinoma, NOS)",
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
  mutate(ocever = case_when(
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
    thyrd == 1                                          ~ "yes, unspecified",
    thyrd == 2                                          ~ "no",
    thyrd == 3                                          ~ "yes, hyperthyroid/overreactive",
    thyrd == 4                                          ~ "yes, hypothyroid/underreactive",
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


######################################################################################## III ### Add paired_id to clinical----
# Add paired_id to clinical----
cases_match <- cases_match %>% mutate(suid = as.character(suid))
clinical_data <- full_join(cases_match, 
                           clinical_data,
                           by= "suid")


######################################################################################### II ### Cleaning TMAs, ROIs data----
# Keep the control slides for comparaison?----
# other_tissue = 1 are controls used for QC purposes
# They may be benign or cancerous tissue from other anatomical sites - like tonsil or spleen
# TMA_Tctrl <- TMA_tumor %>% filter(!is.na(other_tissue))
# TMA_Sctrl <- TMA_stroma %>% filter(!is.na(other_tissue))

# 2.1.Remove the TMA IDs from patient excluded from the study----
# Should only be done for TMAs
# Plus remove TMA with no IDs = controls images
uid <- paste(unique(TMAcases_remove$Subject_IDs), collapse = "|")
TMA_tumor <-
  TMA_tumor[(!grepl(uid, TMA_tumor$suid)), ] %>% 
  filter(!is.na(suid))
TMA_stroma <-
  TMA_stroma[(!grepl(uid, TMA_stroma$suid)),] %>% 
  filter(!is.na(suid))
# Did it for ROIs too in case but no cases to remove

# 2.2.Create suid for ROIs----
ROI_tumor$suid <- str_match(ROI_tumor$image_tag, 
                            "(Peres_P1_AACES.|Peres_P1_AACEES.|Peres_P1_OV|Peres_P1.|)([:digit:]*)")[,3]
ROI_stroma$suid <- str_match(ROI_stroma$image_tag, 
                             "(Peres_P1_AACES.|Peres_P1_AACEES.|Peres_P1_OV|Peres_P1.|)([:digit:]*)")[,3]


# 2.3.Merging stroma and tumor for TMAs and ROIs, Add % tumor cells and % stroma cells within each ROI/TMA core----
# Provide the mean, median, and range of the % tumor and % stroma 
# for the TMA cores, intratumoral ROIs, and peripheral ROIs. 
# Also assess the variation by case in terms of the % tumor and % stroma. 

ROI_global <- merge.data.frame(ROI_tumor, ROI_stroma %>% select(-c("intratumoral_i_vs_peripheral_p_", "suid")),
                               by.x = "image_tag", by.y = "image_tag",
                               all = TRUE) %>% 
  mutate(total_cell_number = tumor_total_cells + stroma_total_cells
         ) %>% 
  mutate(percent_tumor = round((tumor_total_cells / total_cell_number)*100, 2) # Calculate percent of tumor cell
         ) %>% 
  mutate(percent_stroma = round((stroma_total_cells / total_cell_number)*100, 2) # Calculate percent of stromal cell
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


TMA_global <- merge.data.frame(TMA_tumor, TMA_stroma %>% select(-suid),
                               by.x = "image_tag", by.y = "image_tag",
                               all = TRUE) %>% 
  mutate(total_cell_number = tumor_total_cells + stroma_total_cells
         ) %>% 
  mutate(percent_tumor = round((tumor_total_cells / total_cell_number)*100, 2)
         ) %>% 
  mutate(percent_stroma = round((stroma_total_cells / total_cell_number)*100, 2)
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
# 3.1.Summarize immune markers----
# For each case, calculate the average cell density (number of cells positive 
# for each marker per mm2 of tumor/stroma) of the markers below across the TMA cores, 
# intratumoral ROIs, and peripheral ROIs (separately – overall, tumor, and stroma). 
# Potentially create some sort of variable denoting an immune hot or immune cold tumor – 
# or something along those lines?

# Using % Cell
## Add sqrt transformation if better with 0
markers_TMA <- group_by(TMA_global, suid) %>%
  summarize(
    mean_tumor = mean(percent_tumor),
    mean_stroma = mean(percent_stroma),
    variance_tumor = var(percent_tumor),
    variance_stroma = var(percent_stroma),
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
    percent_CD11b_CD15_stroma = mean(stroma_percent_cd11bplus_cd15plus_positive_cells)
  )
markers_TMA$tumor_variation <- markers_TMA$mean_tumor - mean(TMA_global$percent_tumor)
markers_TMA$stroma_variation <- markers_TMA$mean_stroma - mean(TMA_global$percent_stroma)
sqrt.markers <- sqrt(markers_TMA[,c(6:21)])
colnames(sqrt.markers) <- c("sqrt_CD3_tumor", "sqrt_CD8_tumor", "sqrt_CD3_CD8_tumor", "sqrt_FoxP3_tumor",
                            "sqrt_CD3_FoxP3_tumor", "sqrt_CD11b_tumor", "sqrt_CD15_tumor", 
                            "sqrt_CD11b_CD15_tumor", "sqrt_CD3_stroma", "sqrt_CD8_stroma",
                            "sqrt_CD3_CD8_stroma", "sqrt_FoxP3_stroma", "sqrt_CD3_FoxP3_stroma",
                            "sqrt_CD11b_stroma", "sqrt_CD15_stroma", "sqrt_CD11b_CD15_stroma")
markers_TMA <- cbind(markers_TMA, sqrt.markers)

#
markers_ROIi <- ROI_global %>% 
  filter(intratumoral_i_vs_peripheral_p_ == "Intratumoral") %>% 
  group_by(suid) %>% 
  summarize(
    mean_tumor = mean(percent_tumor),
    mean_stroma = mean(percent_stroma),
    variance_tumor = var(percent_tumor),
    variance_stroma = var(percent_stroma),
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
    percent_CD11b_CD15_stroma = mean(stroma_percent_cd11bplus_cd15plus_positive_cells)
  )
markers_ROIi$tumor_variation <- markers_ROIi$mean_tumor - mean(ROI_global$percent_tumor)
markers_ROIi$stroma_variation <- markers_ROIi$mean_stroma - mean(ROI_global$percent_stroma)
#
markers_ROIp <- ROI_global %>% 
  filter(intratumoral_i_vs_peripheral_p_ == "Peripheral") %>% 
  group_by(suid) %>% 
  summarize(
    mean_tumor = mean(percent_tumor),
    mean_stroma = mean(percent_stroma),
    variance_tumor = var(percent_tumor),
    variance_stroma = var(percent_stroma),
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
    percent_CD11b_CD15_stroma = mean(stroma_percent_cd11bplus_cd15plus_positive_cells)
  )
markers_ROIp$tumor_variation <- markers_ROIp$mean_tumor - mean(ROI_global$percent_tumor)
markers_ROIp$stroma_variation <- markers_ROIp$mean_stroma - mean(ROI_global$percent_stroma)

markers_ROI <- full_join(markers_ROIi, markers_ROIp,
                         by= "suid", suffix= c(".i", ".p"))

sqrt.markers <- sqrt(markers_ROI[,c(6:21, 28:43)])
colnames(sqrt.markers) <- c("sqrt_CD3_tumor.i", "sqrt_CD8_tumor.i", "sqrt_CD3_CD8_tumor.i", "sqrt_FoxP3_tumor.i",
                            "sqrt_CD3_FoxP3_tumor.i", "sqrt_CD11b_tumor.i", "sqrt_CD15_tumor.i", 
                            "sqrt_CD11b_CD15_tumor.i", "sqrt_CD3_stroma.i", "sqrt_CD8_stroma.i",
                            "sqrt_CD3_CD8_stroma.i", "sqrt_FoxP3_stroma.i", "sqrt_CD3_FoxP3_stroma.i",
                            "sqrt_CD11b_stroma.i", "sqrt_CD15_stroma.i", "sqrt_CD11b_CD15_stroma.i",
                            "sqrt_CD3_tumor.p", "sqrt_CD8_tumor.p", "sqrt_CD3_CD8_tumor.p", "sqrt_FoxP3_tumor.p",
                            "sqrt_CD3_FoxP3_tumor.p", "sqrt_CD11b_tumor.p", "sqrt_CD15_tumor.p", 
                            "sqrt_CD11b_CD15_tumor.p", "sqrt_CD3_stroma.p", "sqrt_CD8_stroma.p",
                            "sqrt_CD3_CD8_stroma.p", "sqrt_FoxP3_stroma.p", "sqrt_CD3_FoxP3_stroma.p",
                            "sqrt_CD11b_stroma.p", "sqrt_CD15_stroma.p", "sqrt_CD11b_CD15_stroma.p")
markers_ROI <- cbind(markers_ROI, sqrt.markers)


######################################################################################## IV ### Merging all df----
colnames(markers_TMA)[2:39] <- paste(colnames(markers_TMA)[2:39], "tma", sep = "_")
markers <- full_join(markers_TMA, markers_ROI,
                     by= "suid")
markers <- left_join(markers, clinical_data, by="suid")


# # 4.1. Create variation data ----
# # Look at the variation between each patient and the global mean # Should we mot compare Black and White?
# # Here compare the mean of % cells to global study % cells
# variations_TMA <- group_by(TMA_global, suid) %>% 
#   # summarize(mean_tumor = mean(percent_tumor), mean_stroma = mean(percent_stroma),
#   #           variance_tumor = var(percent_tumor), variance_stroma = var(percent_stroma)) %>% 
#   mutate(ID = seq(1:nrow(.)))
# 
# 
# variations_ROIip <- group_by(ROI_global, suid, intratumoral_i_vs_peripheral_p_) %>% # mean of % cells separated by intra or perip
#   # summarize(mean_tumor = mean(percent_tumor), mean_stroma = mean(percent_stroma),
#   #           variance_tumor = var(percent_tumor), variance_stroma = var(percent_stroma))
# setDT(variations_ROIip)[, ID := .GRP, .(suid)]
# # variations_ROIip$tumor_variation <- variations_ROIip$mean_tumor - mean(ROI_global$percent_tumor)
# # variations_ROIip$stroma_variation <- variations_ROIip$mean_stroma - mean(ROI_global$percent_stroma)
# 
# # Will commented because, after checking, it is not a good idea to combined all suid (I + P)
# variations_ROI <- group_by(variations_ROIip, suid) %>%
#   # summarize(mean_tumor = mean(mean_tumor), mean_stroma = mean(mean_stroma)) %>% # mean of % cells merging intra or perip
#   mutate(ID = seq(1:nrow(.)))
# # variations_ROI$tumor_variation <- variations_ROI$mean_tumor - mean(ROI_global$percent_tumor)
# # variations_ROI$stroma_variation <- variations_ROI$mean_stroma - mean(ROI_global$percent_stroma)


######################################################################################## III ### Create df 28 patients----
uid <- paste(unique(common_ROITMA_IDs$Subject_IDs), collapse = '|')
markers_28 <- markers[(grepl(uid, markers$suid)),]


######################################################################################## VI ### Create df for pair_id----
markers_match <-  markers %>% drop_na(pair_id) %>% 
  group_by(pair_id) %>% filter( n() > 1 )


######################################################################################## VII ### to look----
# cases_match <- cases_match %>% mutate(suid = as.character(suid))
# # 3.2.Add case_match to clinical_data
# clinical_data <- full_join(cases_match, 
#                            clinical_data,
#                            by= "suid")
# # 3.3.Add case_match and race to ROI data
cases_match <- left_join(cases_match,
                         clinical_data %>% select("suid", "race"),
                         by= "suid")
cases_match1 <- dcast(setDT(cases_match), pair_id ~ rowid(pair_id),
                      value.var = c("suid", "race")) %>%
  drop_na("race_1", "race_2") # We all have the matching



# Cleaning
rm(uid, TMAcases_remove, TMA_tumor, TMA_stroma, ROI_tumor, ROI_stroma,
   common_ROITMA_IDs, cases_match1, sqrt.markers, markers_ROIi, markers_ROIp)



# End----