
# I  ### Load data

path <- fs::path("","Volumes","Peres_Research")
fct_name_repair <- function(colnms) {
  tolower(gsub("[ ():+²]", "_", colnms))
}
data_import <- function(data_path){
  read_csv(paste0(data_path,
              "/K99_R00/Image analysis data/AACES and NCOCS data/aaces_ncocs_03042020.csv"))
}
#-----------------------------------------------------------------------------------------------------------------
roit_import <- function(path){
  readxl::read_xlsx(paste0(path,
              "/K99_R00/Image analysis data/Immune marker count data/Peres P1 ROI Analysis with location_updated 1-24-2020.xlsx"
  ), sheet = "Tumor", .name_repair = fct_name_repair)
}
rois_import <- function(path){
  readxl::read_xlsx(paste0(path,
              "/K99_R00/Image analysis data/Immune marker count data/Peres P1 ROI Analysis with location_updated 1-24-2020.xlsx"
  ), sheet = "Stroma", .name_repair = fct_name_repair)
}
# roir_import <- function(path){
#   readxl::read_xlsx(paste0(path,
#               "/K99_R00/Image analysis data/Immune marker count data/Peres P1 ROI Analysis with location_updated 1-24-2020.xlsx"
#   ), sheet = "Analysis Info", skip = 24) %>% 
#     select(c(1, 5)) %>% 
#     `colnames<-`(c("image_tag", "REMOVED")) %>% 
#     drop_na("image_tag")
# }
#-----------------------------------------------------------------------------------------------------------------
tmat_import <- function(path){
  readxl::read_xlsx(paste0(path,
                "/K99_R00/Image analysis data/Immune marker count data/Peres P1 AACES 2017 TMA Summary.xlsx"
  ), sheet = "Tumor", .name_repair = fct_name_repair)
}
tmas_import <- function(path){
  readxl::read_xlsx(paste0(path,
                "/K99_R00/Image analysis data/Immune marker count data/Peres P1 AACES 2017 TMA Summary.xlsx"
  ), sheet = "Stroma", .name_repair = fct_name_repair)
}
tma2t_import <- function(path){
  readxl::read_xlsx(paste0(path,
                "/K99_R00/Image analysis data/Immune marker count data/Peres P1 AACES 2018 TMA Summary.xlsx"
  ), sheet = "Tumor", .name_repair = fct_name_repair)
}
tma2s_import <- function(path){
  readxl::read_xlsx(paste0(path,
                "/K99_R00/Image analysis data/Immune marker count data/Peres P1 AACES 2018 TMA Summary.xlsx"
  ), sheet = "Stroma", .name_repair = fct_name_repair)
}
#-----------------------------------------------------------------------------------------------------------------
case_remove_import <- function(path){
  read_csv(paste0(path,
                "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/Subject_IDs to remove from TMA.csv"))
}
#-----------------------------------------------------------------------------------------------------------------
common_ROITMA_import <- function(path){
  read_csv(paste0(path,
                  "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/Subject_IDs common TMA ROI.csv"))
}
#-----------------------------------------------------------------------------------------------------------------

# Start Data Cleaning
binding <- function(data1, data2){
  bind_rows(data1, data2, .id = "TMA")
}
 