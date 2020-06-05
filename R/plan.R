plan <- drake_plan(

  clinical_data = data_import(fs::path("","Volumes","Peres_Research")),
  #-----------------------------------------------------------------------------------------------------------------
  ROI_tumor = roit_import(fs::path("","Volumes","Peres_Research")),
  
  ROI_stroma = rois_import(fs::path("","Volumes","Peres_Research")),
  
  # ROI_remove = roir_import(fs::path("","Volumes","Peres_Research")),
  #-----------------------------------------------------------------------------------------------------------------
  TMA1_tumor = tmat_import(fs::path("","Volumes","Peres_Research")),
  TMA1_stroma = tmas_import(fs::path("","Volumes","Peres_Research")),
  
  TMA2_tumor = tma2t_import(fs::path("","Volumes","Peres_Research")),
  TMA2_stroma = tma2s_import(fs::path("","Volumes","Peres_Research")),
  #-----------------------------------------------------------------------------------------------------------------
  TMAcases_remove = case_remove_import(fs::path("","Volumes","Peres_Research")),
  common_ROITMA_IDs = common_ROITMA_import(fs::path("","Volumes","Peres_Research")),
  
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
  # TMA_tumor <- 
  #   bind_rows(TMA_tumor,TMA2_tumor, .id = "TMA")
  # TMA_stroma <- 
  #   bind_rows(TMA_stroma,TMA2_stroma, .id = "TMA")
  TMA_tumor = binding(TMA1_tumor,TMA2_tumor),
  TMA_stroma = binding(TMA1_stroma,TMA2_stroma) 

)