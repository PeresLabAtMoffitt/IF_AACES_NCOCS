# Packages
library(tidyverse)

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
ROIimmune_marker <-
  readxl::read_xlsx(paste0(path,
      "/K99_R00/Image analysis data/Immune marker count data/Peres P1 ROI Analysis with location_updated 1-24-2020.xlsx"
    ))
#-----------------------------------------------------------------------------------------------------------------
TMAimmune_marker <-
  readxl::read_xlsx(paste0(path,
      "/K99_R00/Image analysis data/Immune marker count data/Peres P1 AACES 2017 TMA Summary.xlsx"
  ))
TMA2immune_marker <-
  readxl::read_xlsx(paste0(path,
      "/K99_R00/Image analysis data/Immune marker count data/Peres P1 AACES 2018 TMA Summary.xlsx"
  ))
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







write.csv(a, paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/Subject_IDs to remove from TMA.csv"))

write.csv(a, paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/Subject_IDs common TMA ROI.csv"))






