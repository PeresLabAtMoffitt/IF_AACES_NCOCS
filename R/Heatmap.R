library(gplots)
library(heatmap.plus)
library(RColorBrewer)
# Plot SQRT
df1 <- as.data.frame(markers[,c(116, 118:124, 126:131)]) %>% 
  `row.names<-`(markers$suid) %>% drop_na(.)
df1$suid <- NULL

df2 <- t(scale(t(df1)))
df2 <- df2[complete.cases(df2 * 0), , drop=FALSE]

heatmap.2(df2, main = "Immune Marker Presentation",
          
          trace = "none", density="none", col=bluered(20), cexRow=1, cexCol = 1, 
          margins = c(10,5), # bottom, right
          ColSideColors = ,
          scale = "row")



# Tertiles----
# Create df
df1 <- as.data.frame(markers[,c("percent_CD3_total_tma", "percent_CD8_total_tma", "percent_CD3_CD8_total_tma", 
                                "percent_CD3_FoxP3_total_tma", "percent_FoxP3_total_tma", 
                                "percent_CD11b_total_tma", "percent_CD15_total_tma", "percent_CD11b_CD15_total_tma",
                                "CD11b_CD15_grp_tma", "suid")]) %>% 
  drop_na(CD11b_CD15_grp_tma) %>% 
  unite(suid, c(suid, CD11b_CD15_grp_tma), sep = "_", remove = TRUE) %>% 
  `row.names<-`(.$suid) %>% 
  select(-suid) 
# Create matrix and drop NA
df2 <- t(scale(t(as.matrix(df1))))
df2 <- df2[complete.cases(df2 * 0), , drop=FALSE]
# Create colors for cluster
condition_colors <- unlist(lapply(rownames(df2), function(x){
  if(grepl("Low", x)) "pink"
  else if(grepl("High", x)) "grey"
}))
# Transpose
df2 <- t(df2)
# Plot
heatmap.2(df2, main = "Immune Marker Presentation",
          
          trace = "none", density="none", col=bluered(20), cexRow=1, cexCol = 1, 
          margins = c(10,5), # bottom, right
          ColSideColors = condition_colors,
          scale = "row")
dev.off()

heatmap.2(df2, trace="none", density="none", col=bluered(20), cexRow=1, cexCol=0.2, margins = c(20,13),
          ColSideColors=condition_colors, scale="row")
dev.off()

heatmap.2(df2, trace="none", density="none", col=heat.colors(250), 
          # Colv = NULL,
          cexRow=1, cexCol=0.2,  # margins = c(3,15),# bottom, right
          ColSideColors=condition_colors, scale="row",
          hclust=function(x) hclust(x,method="average"),distfun=function(x) as.dist((1-cor(t(x)))/2))
dev.off()


# Heatmap abundance W/B

# Intratumoral

df1 <- features %>% select(-date, -segmented_by) %>% 
  unite(patient_id, c(patient_id, lesion_id), sep = "_", remove = TRUE) %>% 
  `row.names<-`(.$patient_id) %>% 
  select(-patient_id) 
df1 <- as.matrix(df1)
df2 <- t(df1)

heatmap.2(df2, main = "",
          
          trace = "none", density="none", col=bluered(20), cexRow=.5, cexCol = .8, 
          lwid=c(1.1, 10), lhei=c(1.1, 10), 
          margins = c(8,10), # bottom, right
          RowSideColors = condition_colors,
          scale = "column")




df1 <- as.data.frame(markers_ROI[,c("percent_CD3_tumor.i", "percent_CD3_CD8_tumor.i", "percent_CD3_FoxP3_tumor.i", "percent_CD11b_tumor.i", "percent_CD11b_CD15_tumor.i",
                                    "percent_CD3_stroma.i", "percent_CD3_CD8_stroma.i", "percent_CD3_FoxP3_stroma.i", "percent_CD11b_stroma.i", "percent_CD11b_CD15_stroma.i",
                                    "suid")]) %>% 
  # drop_na(percent_CD3_tumor.i) %>% 
  # unite(suid, c(suid, CD11b_CD15_grp_tma), sep = "_", remove = TRUE) %>% 
  `row.names<-`(.$suid) %>% 
  select(-suid) 
df1 <- as.matrix(df1)
df2 <- t(df1)

heatmap.2(df2, main = "",
          
          trace = "none", density="none", col=bluered(20), cexRow=.5, cexCol = .8, 
          lwid=c(1.1, 10), lhei=c(1.1, 10), 
          margins = c(8,10), # bottom, right
          RowSideColors = ,
          scale = "column")
















