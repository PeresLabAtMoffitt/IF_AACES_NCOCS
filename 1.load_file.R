# Packages
library(tidyverse)


# Need create draek
# Change var name aka remove spaces



# I  ### Load data

path <- fs::path("","Volumes","Peres_Research")
#-----------------------------------------------------------------------------------------------------------------
clinical_data <-
  read_csv(paste0(path,
      "/K99_R00/Image analysis data/AACES and NCOCS data/aaces_ncocs_03042020.csv"
    ))
#-----------------------------------------------------------------------------------------------------------------
ROI_tumor <-
  readxl::read_xlsx(paste0(path,
      "/K99_R00/Image analysis data/Immune marker count data/Peres P1 ROI Analysis with location_updated 1-24-2020.xlsx"
    ), sheet = "Tumor")
ROI_stroma <-
  readxl::read_xlsx(paste0(path,
      "/K99_R00/Image analysis data/Immune marker count data/Peres P1 ROI Analysis with location_updated 1-24-2020.xlsx"
  ), sheet = "Stroma")
ROI_remove <-
  readxl::read_xlsx(paste0(path,
                           "/K99_R00/Image analysis data/Immune marker count data/Peres P1 ROI Analysis with location_updated 1-24-2020.xlsx"
  ), sheet = "Analysis Info", skip = 24) %>% 
  select(c(1, 5)) %>% 
  `colnames<-`(c("image_tag", "REMOVED")) %>% 
  drop_na("image_tag") # 96 - 6 = 90 tag to remove/keep
#-----------------------------------------------------------------------------------------------------------------
TMA_tumor <-
  readxl::read_xlsx(paste0(path,
      "/K99_R00/Image analysis data/Immune marker count data/Peres P1 AACES 2017 TMA Summary.xlsx"
  ), sheet = "Tumor")
TMA_stroma <-
  readxl::read_xlsx(paste0(path,
      "/K99_R00/Image analysis data/Immune marker count data/Peres P1 AACES 2017 TMA Summary.xlsx"
  ), sheet = "Stroma")

TMA2_tumor <-
  readxl::read_xlsx(paste0(path,
      "/K99_R00/Image analysis data/Immune marker count data/Peres P1 AACES 2018 TMA Summary.xlsx"
  ), sheet = "Tumor")
TMA2_stroma <-
  readxl::read_xlsx(paste0(path,
      "/K99_R00/Image analysis data/Immune marker count data/Peres P1 AACES 2018 TMA Summary.xlsx"
  ), sheet = "Stroma")
#-----------------------------------------------------------------------------------------------------------------
# No need to read core removed (already removed from data)
TMAremove_tumor <-
  readxl::read_xlsx(paste0(path,
      "/K99_R00/Image analysis data/Immune marker count data/Peres P1 AACES 2017 TMA Summary.xlsx"
  ), sheet = "Analysis Information", skip = 19)
TMA2remove_tumor <-
  readxl::read_xlsx(paste0(path,
      "/K99_R00/Image analysis data/Immune marker count data/Peres P1 AACES 2018 TMA Summary.xlsx"
  ), sheet = "Analysis Information", skip = 19)
#-----------------------------------------------------------------------------------------------------------------
TMAcases_remove <-
  read_csv(paste0(path,
                  "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/Subject_IDs to remove from TMA.csv"
  ))
#-----------------------------------------------------------------------------------------------------------------
# coor_immune_marker <-
#   read_csv(paste0(path,
#       "/K99_R00/Image analysis data/Per-Cell Object Data Table - Image Analysis/TMA AACES 2017/Peres_P1_AACES_TMA 2017_[1,A].tif_51355_job32199.object_results.csv"
#   ))
#-----------------------------------------------------------------------------------------------------------------
# cases_match <-
#   readxl::read_xlsx(paste0(path,
#       "/K99_R00/Image analysis data/AACES and NCOCS data/Case matches_12312019.xlsx"
#     ))
#-----------------------------------------------------------------------------------------------------------------
common_ROITMA_IDs <-
  read_csv(paste0(path,
                  "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/Subject_IDs common TMA ROI.csv"
  ))
#-----------------------------------------------------------------------------------------------------------------


# II  ### Data Cleaning

# IIa ### TMA data
# Fisrt need to 
## 1-verify that core removed ARE removed from TMA data
uid <- paste(unique(TMAremove_tumor[1]), collapse = "|")
TMA_tumor <-
  TMA_tumor[(!grepl(uid, TMA_tumor$`Image Tag`)), ]
TMA_stroma <-
  TMA_stroma[(!grepl(uid, TMA_stroma$`Image Tag`)),]

uid <- paste(unique(TMA2remove_tumor[1]), collapse = "|")
TMA2_tumor <-
  TMA2_tumor[(!grepl(uid, TMA2_tumor$`Image Tag`)),]
TMA2_stroma <-
  TMA2_stroma[(!grepl(uid, TMA2_stroma$`Image Tag`)),]
## 2-bind TMA together
TMA_tumor <- 
  bind_rows(TMA_tumor,TMA2_tumor, .id = "TMA") %>% 
  rename(image_tag = `Image Tag`)
TMA_stroma <- 
  bind_rows(TMA_stroma,TMA2_stroma, .id = "TMA") %>% 
  rename(image_tag = `Image Tag`)


# IIb ### ROI data
# 1-verify that core removed ARE removed from ROI data
ROI_remove <- ROI_remove %>% filter(REMOVED == "REMOVE" | is.na(REMOVED)) # only 84 after removin the to keep tag
uid <- paste(unique(ROI_remove[1]), collapse = "|")
ROI_tumor <-
  ROI_tumor[(!grepl(uid, ROI_tumor$`Image Tag`)), ] %>% 
  rename(image_tag = `Image Tag`)
ROI_stroma <-
  ROI_stroma[(!grepl(uid, ROI_stroma$`Image Tag`)),] %>% 
  rename(image_tag = `Image Tag`)


# III-Remove the TMA IDs from patient excluded from the study
# Should only be done for TMAs
# Remove TMA with no IDs = controls images
uid <- paste(unique(TMAcases_remove$Subject_IDs), collapse = "|")
TMA_tumor <-
  TMA_tumor[(!grepl(uid, TMA_tumor$suid)), ] %>% 
  filter(!is.na(suid))
TMA_stroma <-
  TMA_stroma[(!grepl(uid, TMA_stroma$suid)),] %>% 
  filter(!is.na(suid))

# Did it for ROIs too in case -> no change / good
ROI_tumor$suid <- substr(ROI_tumor$image_tag, start = 10, stop = 14)
# ROI_tumor <-
#   ROI_tumor[(!grepl(uid, ROI_tumor$suid)), ]
ROI_stroma$suid <- substr(ROI_stroma$image_tag, start = 10, stop = 14)
# ROI_stroma <-
#   ROI_stroma[(!grepl(uid, ROI_stroma$suid)), ]

# Cleaning
rm(TMAremove_tumor, TMA2remove_tumor, TMA2_tumor, TMA2_stroma, ROI_remove, TMAcases_remove, uid)







# write.csv(a, paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/Subject_IDs to remove from TMA.csv"))
# 
# write.csv(a, paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/Subject_IDs common TMA ROI.csv"))






