# Packages
library(tidyverse)


# I REALLY need to change the var name !!! :|



# I  ### Load data

path <- fs::path("","Volumes","Peres_Research")
#-----------------------------------------------------------------------------------------------------------------
clinical_data <-
  read_csv(paste0(path,
      "/K99_R00/Image analysis data/AACES and NCOCS data/aaces_ncocs_03042020.csv"
    ))
#-----------------------------------------------------------------------------------------------------------------
cases_match <-
  readxl::read_xlsx(paste0(path,
      "/K99_R00/Image analysis data/AACES and NCOCS data/Case matches_12312019.xlsx"
    ))
#-----------------------------------------------------------------------------------------------------------------
ROIimmune_marker_tumor <-
  readxl::read_xlsx(paste0(path,
      "/K99_R00/Image analysis data/Immune marker count data/Peres P1 ROI Analysis with location_updated 1-24-2020.xlsx"
    ), sheet = "Tumor")
ROIimmune_marker_stroma <-
  readxl::read_xlsx(paste0(path,
      "/K99_R00/Image analysis data/Immune marker count data/Peres P1 ROI Analysis with location_updated 1-24-2020.xlsx"
  ), sheet = "Stroma")
ROI_remove <-
  readxl::read_xlsx(paste0(path,
      "/K99_R00/Image analysis data/Immune marker count data/Peres P1 ROI Analysis with location_updated 1-24-2020.xlsx"
  ), sheet = "Analysis Info", skip = 24)
#-----------------------------------------------------------------------------------------------------------------
TMAimmune_marker_tumor <-
  readxl::read_xlsx(paste0(path,
      "/K99_R00/Image analysis data/Immune marker count data/Peres P1 AACES 2017 TMA Summary.xlsx"
  ), sheet = "Tumor")
TMAimmune_marker_stroma <-
  readxl::read_xlsx(paste0(path,
      "/K99_R00/Image analysis data/Immune marker count data/Peres P1 AACES 2017 TMA Summary.xlsx"
  ), sheet = "Stroma")

TMA2immune_marker_tumor <-
  readxl::read_xlsx(paste0(path,
      "/K99_R00/Image analysis data/Immune marker count data/Peres P1 AACES 2018 TMA Summary.xlsx"
  ), sheet = "Tumor")
TMA2immune_marker_stroma <-
  readxl::read_xlsx(paste0(path,
      "/K99_R00/Image analysis data/Immune marker count data/Peres P1 AACES 2018 TMA Summary.xlsx"
  ), sheet = "Stroma")
#-----------------------------------------------------------------------------------------------------------------
# No need to read core removed (already removed from data)
TMAremove_core_tumor <-
  readxl::read_xlsx(paste0(path,
      "/K99_R00/Image analysis data/Immune marker count data/Peres P1 AACES 2017 TMA Summary.xlsx"
  ), sheet = "Analysis Information", skip = 19)
TMA2remove_core_tumor <-
  readxl::read_xlsx(paste0(path,
      "/K99_R00/Image analysis data/Immune marker count data/Peres P1 AACES 2018 TMA Summary.xlsx"
  ), sheet = "Analysis Information", skip = 19)
#-----------------------------------------------------------------------------------------------------------------
coor_immune_marker <-
  read_csv(paste0(path,
      "/K99_R00/Image analysis data/Per-Cell Object Data Table - Image Analysis/TMA AACES 2017/Peres_P1_AACES_TMA 2017_[1,A].tif_51355_job32199.object_results.csv"
  ))
#-----------------------------------------------------------------------------------------------------------------
TMA_remove <-
  read_csv(paste0(path,
                  "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/Subject_IDs to remove from TMA.csv"
  ))
#-----------------------------------------------------------------------------------------------------------------
common_ROITMA_IDs <-
  read_csv(paste0(path,
                  "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/Subject_IDs common TMA ROI.csv"
  ))
#-----------------------------------------------------------------------------------------------------------------



# II  ### Data Cleaning

# IIa ### TMA data
# Fisrt need to 
# 1-verify that core removed ARE removed from TMA data
uid <- paste(unique(TMAremove_core_tumor[1]), collapse = '|')
TMAimmune_marker_tumor <-
  TMAimmune_marker_tumor[(!grepl(uid, TMAimmune_marker_tumor$`Image Tag`)), ]
TMAimmune_marker_stroma <-
  TMAimmune_marker_stroma[(!grepl(uid, TMAimmune_marker_stroma$`Image Tag`)),]

uid <- paste(unique(TMA2remove_core_tumor[1]), collapse = '|')
TMA2immune_marker_tumor <-
  TMA2immune_marker_tumor[(!grepl(uid, TMA2immune_marker_tumor$`Image Tag`)),]
TMA2immune_marker_stroma <-
  TMAi2mmune_marker_stroma[(!grepl(uid, TMA2immune_marker_stroma$`Image Tag`)),]

rm(TMAremove_core_tumor, TMA2remove_core_tumor)


# 2-bind TMA together
TMAimmune_marker_tumor <- 
  bind_rows(TMAimmune_marker_tumor,TMA2immune_marker_tumor, .id = "TMA")
TMAimmune_marker_stroma <- 
  bind_rows(TMAimmune_marker_stroma,TMA2immune_marker_stroma, .id = "TMA")

rm(TMA2immune_marker_tumor, TMA2immune_marker_stroma)


# 3-Remove the TMA IDs from patient excluded
uid <- paste(unique(TMA_remove$Subject_IDs), collapse = '|')
TMAimmune_marker_tumor <-
  TMAimmune_marker_tumor[(!grepl(uid, TMAimmune_marker_tumor$suid)), ]

TMAimmune_marker_stroma <-
  TMAimmune_marker_stroma[(!grepl(uid, TMAimmune_marker_stroma$suid)),]

##

# IIb ### ROI data
# 1-verify that core removed ARE removed from ROI data
ROI_remove <- ROI_remove %>% filter(REMOVE == "REMOVE" | is.na(REMOVE))

uid <- paste(unique(ROI_remove[1]), collapse = '|')
ROIimmune_marker_tumor <-
  ROIimmune_marker_tumor[(!grepl(uid, ROIimmune_marker_tumor$`Image Tag`)), ]

ROIimmune_marker_stroma <-
  ROIimmune_marker_stroma[(!grepl(uid, ROIimmune_marker_stroma$`Image Tag`)),]










write.csv(a, paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/Subject_IDs to remove from TMA.csv"))

write.csv(a, paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/Subject_IDs common TMA ROI.csv"))






