# Import library

library(tidyverse)


############################################################################## I ### Load new ROIs data----
path <- fs::path("","Volumes","Peres_Research")
fct_name_repair <- function(colnms) {
  tolower(gsub("[ ():]", "_", colnms))
}
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


# End cleaning