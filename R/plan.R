plan <- drake_plan(

  clinical_data = data_import(fs::path("","Volumes","Peres_Research")),
  #-----------------------------------------------------------------------------------------------------------------
  ROI_tumor = roit_import(fs::path("","Volumes","Peres_Research")),
  
  ROI_stroma = rois_import(fs::path("","Volumes","Peres_Research")),
  
  ROI_remove = roir_import(fs::path("","Volumes","Peres_Research")),
  #-----------------------------------------------------------------------------------------------------------------
  TMA_tumor = tmat_import(fs::path("","Volumes","Peres_Research")),
  TMA_stroma = tmas_import(fs::path("","Volumes","Peres_Research")),
  
  TMA2_tumor = tma2t_import(fs::path("","Volumes","Peres_Research")),
  TMA2_stroma = tma2s_import(fs::path("","Volumes","Peres_Research")),
  #-----------------------------------------------------------------------------------------------------------------
  TMAcases_remove = tmar_import(fs::path("","Volumes","Peres_Research"))

)