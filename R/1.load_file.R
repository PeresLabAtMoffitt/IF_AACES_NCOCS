# Packages
library(tidyverse)


# Need create draek
# Change var name aka remove spaces



# I  ### Load data

path <- fs::path("","Volumes","Peres_Research")
fct_name_repair <- function(colnms) {
  tolower(gsub("[ ():]", "_", colnms))
}
#-----------------------------------------------------------------------------------------------------------------
clinical_data <-
  read_csv(paste0(path,
      "/K99_R00/Image analysis data/AACES and NCOCS data/aaces_ncocs_03042020.csv"
    ))
#-----------------------------------------------------------------------------------------------------------------
ROI_tumor <-
  readxl::read_xlsx(paste0(path,
      "/K99_R00/Image analysis data/Immune marker count data/Peres P1 ROI Analysis with location_updated 1-24-2020.xlsx"
    ), sheet = "Tumor", .name_repair = fct_name_repair)
ROI_stroma <-
  readxl::read_xlsx(paste0(path,
      "/K99_R00/Image analysis data/Immune marker count data/Peres P1 ROI Analysis with location_updated 1-24-2020.xlsx"
  ), sheet = "Stroma", .name_repair = fct_name_repair)
#-----------------------------------------------------------------------------------------------------------------
# No need to read core removed (already removed from data)
# ROI_remove <-
#   readxl::read_xlsx(paste0(path,
#                            "/K99_R00/Image analysis data/Immune marker count data/Peres P1 ROI Analysis with location_updated 1-24-2020.xlsx"
#   ), sheet = "Analysis Info", skip = 24) %>% 
#   select(c(1, 5)) %>% 
#   `colnames<-`(c("image_tag", "REMOVED")) %>% 
#   drop_na("image_tag")
#-----------------------------------------------------------------------------------------------------------------
TMA_tumor <-
  readxl::read_xlsx(paste0(path,
      "/K99_R00/Image analysis data/Immune marker count data/Peres P1 AACES 2017 TMA Summary.xlsx"
  ), sheet = "Tumor", .name_repair = fct_name_repair)
TMA_stroma <-
  readxl::read_xlsx(paste0(path,
      "/K99_R00/Image analysis data/Immune marker count data/Peres P1 AACES 2017 TMA Summary.xlsx"
  ), sheet = "Stroma", .name_repair = fct_name_repair)

TMA2_tumor <-
  readxl::read_xlsx(paste0(path,
      "/K99_R00/Image analysis data/Immune marker count data/Peres P1 AACES 2018 TMA Summary.xlsx"
  ), sheet = "Tumor", .name_repair = fct_name_repair)
TMA2_stroma <-
  readxl::read_xlsx(paste0(path,
      "/K99_R00/Image analysis data/Immune marker count data/Peres P1 AACES 2018 TMA Summary.xlsx"
  ), sheet = "Stroma", .name_repair = fct_name_repair)
#-----------------------------------------------------------------------------------------------------------------
# No need to read core removed (already removed from data)
# TMAremove_tumor <-
#   readxl::read_xlsx(paste0(path,
#       "/K99_R00/Image analysis data/Immune marker count data/Peres P1 AACES 2017 TMA Summary.xlsx"
#   ), sheet = "Analysis Information", skip = 19)
# TMA2remove_tumor <-
#   readxl::read_xlsx(paste0(path,
#       "/K99_R00/Image analysis data/Immune marker count data/Peres P1 AACES 2018 TMA Summary.xlsx"
#   ), sheet = "Analysis Information", skip = 19)
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




