# For help with drake
# https://ropenscilabs.github.io/drake-manual/projects.html#safer-interactivity
# and
# https://ropensci.github.io/drake/reference/r_make.html

# Load your packages from packages.R
source("R/packages.R")
# Load the code as function that drake need to run
source("R/function.R") 
# Load the plan that drake has to execute
source("R/plan.R")    

# End drake
config <- drake_config(plan, parallelism = "future", jobs = 4, verbose = 1)
if (!interactive()) config

make(plan)
loadd(clinical_data, ROI_tumor ,ROI_stroma ,TMA_tumor ,TMA_stroma, 
      TMAcases_remove,
      common_ROITMA_IDs, cases_match)

# Cleaning
rm(fct_name_repair, var_names, # roir_import,
   data_import, roit_import, rois_import, tmat_import, tmas_import,
   tma2t_import, tma2s_import, common_ROITMA_import, # tmar_import, 
   case_remove_import, binding, match_cases_import)

