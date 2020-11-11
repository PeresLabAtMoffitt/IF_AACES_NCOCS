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


heatplot
complexheatmap

