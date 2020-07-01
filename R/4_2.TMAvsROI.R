
# Test normality
qqnorm(variation$mean_tumor)
qqnorm(variation$mean_tumor_roi_i)
# Correlation
library(psych)
colnames(variation)
pairs.panels(variation[c("mean_tumor", "mean_stroma", "tumor_variation", "stroma_variation",
                          "mean_tumor_roi_i", "mean_stroma_roi_i", "tumor_variation_roi_i", "stroma_variation_roi_i",
                          "mean_tumor_roi_p", "mean_stroma_roi_p", "tumor_variation_roi_p", "stroma_variation_roi_p")])
pairs.panels(variation[c("mean_tumor", "mean_tumor_roi_i", "tumor_variation", "tumor_variation_roi_i")])

library(corrplot)
library(ggcorrplot)

mat <- cor(variation[, c("mean_tumor", "mean_tumor_roi_i", "mean_tumor_roi_p", 
                          "mean_stroma", "mean_stroma_roi_i", "mean_stroma_roi_p")], 
           use = "pairwise.complete.obs")
corrplot(mat)
corrplot.mixed(mat)

mat1 <- cor(variation[, c("tumor_variation", "tumor_variation_roi_i", "tumor_variation_roi_p", 
                           "stroma_variation", "stroma_variation_roi_i", "stroma_variation_roi_p")], 
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
