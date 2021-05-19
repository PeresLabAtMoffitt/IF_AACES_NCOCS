# Packages
library(tidyverse)


# Do not run if using drake

# I  ### Load data

path <- fs::path("","Volumes","Peres_Research")
fct_name_repair <- function(colnms) {
  tolower(gsub("[ ():]", "_", colnms))
}
#-----------------------------------------------------------------------------------------------------------------
clinical_data <-
  read_csv(paste0(path,
      "/K99_R00/Image analysis data/AACES and NCOCS data/aaces_ncocs_08142020.csv"
    ))
tx_data_aaces <- readxl::read_xlsx(paste0(path,
                           "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/data/AACES_tx_12082020.xlsx"
))
tx_data_ncocs <- read_csv(paste0(data_path,
                                 "/K99_R00/Image analysis data/AACES and NCOCS data/ncocstreat.csv"
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
cases_match <-
  readxl::read_xlsx(paste0(path,
      "/K99_R00/Image analysis data/AACES and NCOCS data/Case matches_12312019.xlsx"
    ))
#-----------------------------------------------------------------------------------------------------------------
common_ROITMA_IDs <-
  read_csv(paste0(path,
                  "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/Subject_IDs common TMA ROI.csv"
  ))
#-----------------------------------------------------------------------------------------------------------------

# Start Data Cleaning

# II  ### Data Cleaning

# IIa ### TMA data

## 1-verify that core removed ARE removed from TMA data  #-------------- All good so not run anymore
# uid <- paste(unique(TMAremove_tumor[1]), collapse = "|")
# TMA_tumor <-
#   TMA_tumor[(!grepl(uid, TMA_tumor$image_tag)), ]
# TMA_stroma <-
#   TMA_stroma[(!grepl(uid, TMA_stroma$image_tag)),]
# 
# uid <- paste(unique(TMA2remove_tumor[1]), collapse = "|")
# TMA2_tumor <-
#   TMA2_tumor[(!grepl(uid, TMA2_tumor$image_tag)),]
# TMA2_stroma <-
#   TMA2_stroma[(!grepl(uid, TMA2_stroma$image_tag)),]


## 2-bind TMA together
TMA_tumor <-
  bind_rows(TMA1_tumor,TMA2_tumor, .id = "TMA")
TMA_stroma <-
  bind_rows(TMA1_stroma,TMA2_stroma, .id = "TMA")


# Update variable names
TMA_tumor <- TMA_tumor %>%
  rename_all(. %>% gsub("__", "_", .) %>%
               gsub("%", "percent", .) %>%
               gsub("²", "2", .) %>%
               gsub("\\+", "plus", .))
TMA_stroma <- TMA_stroma %>%
  rename_all(. %>% gsub("__", "_", .) %>%
               gsub("%", "percent", .) %>%
               gsub("²", "2", .) %>%
               gsub("\\+", "plus", .))
ROI_tumor <- ROI_tumor %>%
  rename_all(. %>% gsub("__", "_", .) %>%
               gsub("%", "percent", .) %>%
               gsub("²", "2", .) %>%
               gsub("\\+", "plus", .))
ROI_stroma <- ROI_stroma %>%
  rename_all(. %>% gsub("__", "_", .) %>%
               gsub("%", "percent", .) %>%
               gsub("²", "2", .) %>%
               gsub("\\+", "plus", .))


# IIb ### ROI data
# 1-verify that core removed ARE removed from ROI data  #--------------- All good so not run anymore
# ROI_remove <- ROI_remove %>% 
#   filter(REMOVED == "REMOVE" | is.na(REMOVED)) # only 84 after removin the to keep tag
# uid <- paste(unique(ROI_remove[1]), collapse = "|")
# ROI_tumor <-
#   ROI_tumor[(!grepl(uid, ROI_tumor$image_tag)), ]
# ROI_stroma <-
#   ROI_stroma[(!grepl(uid, ROI_stroma$image_tag)),]




