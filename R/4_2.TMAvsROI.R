
# Test normality
qqnorm(variations$mean_tumor_tma)
qqnorm(variations$mean_tumor_roi_i)
# Correlation
library(psych)
pairs.panels(variations)
pairs.panels(variations[c("mean_tumor_tma", "mean_tumor_roi_i")])

library(corrplot)
library(ggcorrplot)

mat <- cor(variations[, c("mean_tumor_tma", "mean_tumor_roi_i", "mean_tumor_roi_p", 
                          "mean_stroma_tma", "mean_stroma_roi_i", "mean_stroma_roi_p")], 
           use = "pairwise.complete.obs")
corrplot(mat)
corrplot.mixed(mat)

mat1 <- cor(variations[, c("tumor_variation_tma", "tumor_variation_roi_i", "tumor_variation_roi_p", 
                           "stroma_variation_tma", "stroma_variation_roi_i", "stroma_variation_roi_p")], 
            use = "pairwise.complete.obs")
corrplot(mat1)
corrplot.mixed(mat1)
ggcorrplot(mat, hc.order = TRUE, method = "circle", 
           # outline.col = "darkblue", # the outline of the circle or sqare
           # hc.method = "complete",
           type = "lower", # show the top half panel
           lab = TRUE, # add correlation nbr
           title = "Correlation between collection type",
           show.legend = TRUE, legend.title = "Correlation", show.diag = TRUE,
           colors = viridis::inferno(n=3),
           lab_col = "darkblue", lab_size = 3, # col and size of the correlation nbr
           # p.mat = pmat, # Add correlation significance
           # sig.level = 0.05, insig = c("pch", "blank"), pch = 4, pch.col = "black", pch.cex = 10, 
           tl.cex = 10, tl.col = "red", tl.srt = 270,
           digits = 1
)



# End








# Cleaning
rm(common_ROITMA_IDs, variations_TMA, variations_ROI, variations_ROIip)
