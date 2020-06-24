
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

m1 <- variations[,"mean_tumor_tma"]
m2 <- variations[,"mean_tumor_roi_i"]
mat2 <- cor(x=m1, y=m2, use = "all.obs")
corrplot(mat2)
sapply(seq.int(dim(m1)[1]), function(i) cor(m2[i,], m1[i,]))
d <- as.data.frame(cbind(m1,m2))
cor(d$m1, as.matrix(d[sapply(d, is.numeric)]))

tvariations <- as.data.frame(base::t(variations)) %>% 
  `colnames<-`(tvariations[1,])
tvariations <- tvariations[c("mean_tumor_tma", "mean_tumor_roi_i"),]
tvariations <- mutate_all(tvariations, function(x) as.numeric(as.character(x)))
mat <- cor(tvariations["mean_tumor_tma", ], 
           use = "pairwise.complete.obs")
corrplot(mat)
corrplot.mixed(mat)





forcorr <- transpose(variations) %>% 
  mutate(rowname = colnames(variations)) %>% 
  mutate(bind = "1") %>% 
  `colnames<-`(.[1,])
forcorr <- forcorr[-1,]
  
forcorr <- merge.data.frame(forcorr %>% filter(suid == "mean_tumor_tma"),
                            forcorr %>% filter(suid == "mean_tumor_roi_i"),
                            by.x = "1", by.y = "1",
                            suffixes = c("_tma", "_roii")
                            ) %>% 
  select(-c(1, suid_tma, suid_roii)) %>% 
  mutate_all(function(x) as.numeric(as.character(x)))

mat <- cor(forcorr, 
           use = "complete.obs")








install.packages("polyPK")
library(polyPK)
m1 <- tvariations[1,]
m2 <- tvariations[2,]
cor(x=m1, y=m2, use = "all.obs")
corrplot(m1)

# End








# Cleaning
rm(common_ROITMA_IDs, variations_TMA, variations_ROI, variations_ROIip)
