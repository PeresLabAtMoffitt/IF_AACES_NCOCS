library(gplots)
library(heatmap.plus)
library(ComplexHeatmap)


df1 <- as.data.frame(markers_ROI[,c("percent_CD3_tumor.i", "percent_CD3_CD8_tumor.i", "percent_CD3_FoxP3_tumor.i", 
                                    "percent_CD11b_tumor.i", "percent_CD11b_CD15_tumor.i",
                                    "percent_CD3_stroma.i", "percent_CD3_CD8_stroma.i", "percent_CD3_FoxP3_stroma.i",
                                    "percent_CD11b_stroma.i", "percent_CD11b_CD15_stroma.i",
                                    "suid", "race", "clusters_all_IandP")]) %>% 
  
  arrange(clusters_all_IandP, percent_CD3_stroma.i, percent_CD3_CD8_stroma.i) %>%
  unite(suid, c(suid, race, clusters_all_IandP), sep = "_", remove = TRUE) %>%
  `row.names<-`(.$suid) %>%
  select(-c(suid, starts_with("CD")))
# Create colors for cluster
condition_colors <- unlist(lapply(rownames(df1), function(x){
  if(grepl("White", x)) "pink"
  else if(grepl("Black", x)) "grey"
}))
condition_colors1 <- unlist(lapply(rownames(df1), function(x){
  case_when(
    str_detect(x, "mid-low") ~ "red",
    str_detect(x, "mid-high") ~ "grey",
    str_detect(x, "low") ~ "steelblue1",
    str_detect(x, "mid") ~ "black",
    str_detect(x, "high") ~ "tan1",
    TRUE          ~ NA_character_)
}))
mycols <- cbind(condition_colors, condition_colors1, deparse.level = 2)
colnames(mycols)[1] <- "reaceth"
colnames(mycols)[2] <- "clust"

# a <- scale(df1)
sc_df1 <- df1
class(sc_df1) <- "data.frame"
for(i in 1:length(colnames(sc_df1))) {
  if(class(sc_df1[,i]) == "numeric" | class(sc_df1[,i]) == "integer") {
    sc_df1[,i] <- scales::rescale(sc_df1[,i], to=c(-2,2)) }
}


df2 <- t((((sc_df1)))) # scale for standardizing the data to make variables comparable
df2 <- df2[complete.cases(df2 * 0), , drop=FALSE]

heatmap.plus(df2, 
             main = "Heatmap",
             col = bluered(20),
             cexRow = 1,
             margins = c(1, 17), # bottom, right
             # ColSideColors = mycols,
             scale = "none",
             Rowv = NA, Colv = NA
)

Heatmap(df2)

library(circlize)
col_fun = colorRamp2(c(-2, 0, 2), c("green", "white", "red"))
col_fun(seq(-2, 2))
Heatmap(df2, name = "mat", col = col_fun)
Heatmap(df2, name = "mat", col = rainbow(10), 
        column_title = "set a color vector for a continuous matrix")



# Removing median
df3 <- as.data.frame(markers_ROI[,c("percent_CD3_tumor.i", "percent_CD3_CD8_tumor.i", "percent_CD3_FoxP3_tumor.i", 
                                    "percent_CD11b_tumor.i", "percent_CD11b_CD15_tumor.i",
                                    "percent_CD3_stroma.i", "percent_CD3_CD8_stroma.i", "percent_CD3_FoxP3_stroma.i",
                                    "percent_CD11b_stroma.i", "percent_CD11b_CD15_stroma.i",
                                    "suid", "race", "clusters_all_IandP")]) %>% 
  mutate(percent_CD3_tumor.i = (percent_CD3_tumor.i - median(percent_CD3_tumor.i, na.rm = TRUE))) %>% 
  mutate(percent_CD3_CD8_tumor.i = (percent_CD3_CD8_tumor.i - median(percent_CD3_CD8_tumor.i, na.rm = TRUE))) %>% 
  mutate(percent_CD3_FoxP3_tumor.i = (percent_CD3_FoxP3_tumor.i - median(percent_CD3_FoxP3_tumor.i, na.rm = TRUE))) %>% 
  mutate(percent_CD11b_tumor.i = (percent_CD11b_tumor.i - median(percent_CD11b_tumor.i, na.rm = TRUE))) %>% 
  mutate(percent_CD11b_CD15_tumor.i = (percent_CD11b_CD15_tumor.i - median(percent_CD11b_CD15_tumor.i, na.rm = TRUE))) %>% 
  mutate(percent_CD3_stroma.i = (percent_CD3_stroma.i - median(percent_CD3_stroma.i, na.rm = TRUE))) %>% 
  mutate(percent_CD3_CD8_stroma.i = (percent_CD3_CD8_stroma.i - median(percent_CD3_CD8_stroma.i, na.rm = TRUE))) %>% 
  mutate(percent_CD3_FoxP3_stroma.i = (percent_CD3_FoxP3_stroma.i - median(percent_CD3_FoxP3_stroma.i, na.rm = TRUE))) %>% 
  mutate(percent_CD11b_stroma.i = (percent_CD11b_stroma.i - median(percent_CD11b_stroma.i, na.rm = TRUE))) %>% 
  mutate(percent_CD11b_CD15_stroma.i = (percent_CD11b_CD15_stroma.i - median(percent_CD11b_CD15_stroma.i, na.rm = TRUE))) %>% 

  arrange(clusters_all_IandP, percent_CD3_stroma.i, percent_CD3_CD8_stroma.i) %>%
  unite(suid, c(suid, race, clusters_all_IandP), sep = "_", remove = TRUE) %>%
  `row.names<-`(.$suid) %>%
  select(-c(suid, starts_with("CD")))

df4 <- t(((as.matrix(df3)))) # scale for standardizing the data to make variables comparable
df4 <- df4[complete.cases(df4 * 0), , drop=FALSE]

heatmap.plus(df4, 
             main = "Heatmap",
             col = bluered(20),
             cexRow = 1,
             margins = c(1, 17), # bottom, right
             ColSideColors = mycols,
             scale = "row",
             Rowv = NA, Colv = NA,
             
)
Heatmap(df4)

library(circlize)
col_fun = colorRamp2(c(-2, 0, 2), c("green", "white", "red"))
col_fun(seq(-2, 2))
Heatmap(df4, name = "mat", col = col_fun)

sc_df1 <- df3
class(sc_df1) <- "data.frame"
for(i in 1:length(colnames(sc_df1))) {
  if(class(sc_df1[,i]) == "numeric" | class(sc_df1[,i]) == "integer") {
    sc_df1[,i] <- scales::rescale(sc_df1[,i], to=c(-2,2)) }
}


df2 <- t((((sc_df1)))) # scale for standardizing the data to make variables comparable
df2 <- df2[complete.cases(df2 * 0), , drop=FALSE]

heatmap.plus(df2, 
             main = "Heatmap",
             col = bluered(20),
             cexRow = 1,
             margins = c(1, 17), # bottom, right
             # ColSideColors = mycols,
             scale = "none",
             Rowv = NA, Colv = NA
)

Heatmap(df2)

library(circlize)
col_fun = colorRamp2(c(-2, 0, 2), c("green", "white", "red"))
col_fun(seq(-2, 2))
Heatmap(df2, name = "mat", col = col_fun)
Heatmap(df2, name = "mat", col = rainbow(10), 
        column_title = "set a color vector for a continuous matrix")

## Using mad

marker.exp <- ((markers_ROI[,c("suid", "percent_CD3_tumor.i", "percent_CD3_CD8_tumor.i", "percent_CD3_FoxP3_tumor.i", 
                               "percent_CD11b_tumor.i", "percent_CD11b_CD15_tumor.i",
                               "percent_CD3_stroma.i", "percent_CD3_CD8_stroma.i", "percent_CD3_FoxP3_stroma.i",
                               "percent_CD11b_stroma.i", "percent_CD11b_CD15_stroma.i",
                               "race", "clusters_all_IandP")])) %>% 
  arrange(clusters_all_IandP, percent_CD3_stroma.i, percent_CD3_CD8_stroma.i) %>%
  unite(suid, c(suid, race, clusters_all_IandP), sep = "_", remove = TRUE) %>%
  `row.names<-`(.$suid) %>%
  select(-c(suid, starts_with("CD")))

sqrt.markers <- sqrt(marker.exp[,c(1:length(marker.exp))])

z_sqrt.markers <- sqrt.markers %>% 
  mutate(mad_percent_CD3_tumor.i = mad((percent_CD3_tumor.i)) + 0.0001
  ) %>% 
  mutate(mad_percent_CD3_CD8_tumor.i = mad((percent_CD3_CD8_tumor.i)) + 0.0001
  ) %>% 
  mutate(mad_percent_CD3_FoxP3_tumor.i = mad((percent_CD3_FoxP3_tumor.i)) + 0.0001
  ) %>% 
  mutate(mad_percent_CD11b_tumor.i = mad((percent_CD11b_tumor.i)) + 0.0001
  ) %>% 
  mutate(mad_percent_CD11b_CD15_tumor.i = mad((percent_CD11b_CD15_tumor.i)) + 0.0001
  ) %>% 
  mutate(mad_percent_CD3_stroma.i = mad((percent_CD3_stroma.i)) + 0.0001
  ) %>% 
  mutate(mad_percent_CD3_CD8_stroma.i = mad((percent_CD3_CD8_stroma.i)) + 0.0001
  ) %>% 
  mutate(mad_percent_CD3_FoxP3_stroma.i = mad((percent_CD3_FoxP3_stroma.i)) + 0.0001
  ) %>% 
  mutate(mad_percent_CD11b_stroma.i = mad((percent_CD11b_stroma.i)) + 0.0001
  ) %>% 
  mutate(mad_percent_CD11b_CD15_stroma.i = mad((percent_CD11b_CD15_stroma.i)) + 0.0001
  ) %>% 
  
  mutate(z_percent_CD3_tumor.i = (percent_CD3_tumor.i - median(percent_CD3_tumor.i, na.rm = TRUE))/
           mad_percent_CD3_tumor.i) %>% 
  mutate(z_percent_CD3_CD8_tumor.i = (percent_CD3_CD8_tumor.i - median(percent_CD3_CD8_tumor.i, na.rm = TRUE))/
           mad_percent_CD3_CD8_tumor.i) %>% 
  mutate(z_percent_CD3_FoxP3_tumor.i = (percent_CD3_FoxP3_tumor.i - median(percent_CD3_FoxP3_tumor.i, na.rm = TRUE))/
           mad_percent_CD3_FoxP3_tumor.i) %>% 
  mutate(z_percent_CD11b_tumor.i = (percent_CD11b_tumor.i - median(percent_CD11b_tumor.i, na.rm = TRUE))/
           mad_percent_CD11b_tumor.i) %>% 
  mutate(z_percent_CD11b_CD15_tumor.i = (percent_CD11b_CD15_tumor.i - median(percent_CD11b_CD15_tumor.i, na.rm = TRUE))/
           mad_percent_CD11b_CD15_tumor.i) %>% 
  mutate(z_percent_CD3_stroma.i = (percent_CD3_stroma.i - median(percent_CD3_stroma.i, na.rm = TRUE))/
           mad_percent_CD3_stroma.i) %>% 
  mutate(z_percent_CD3_CD8_stroma.i = (percent_CD3_CD8_stroma.i - median(percent_CD3_CD8_stroma.i, na.rm = TRUE))/
           mad_percent_CD3_CD8_stroma.i) %>% 
  mutate(z_percent_CD3_FoxP3_stroma.i = (percent_CD3_FoxP3_stroma.i - median(percent_CD3_FoxP3_stroma.i, na.rm = TRUE))/
           mad_percent_CD3_FoxP3_stroma.i) %>% 
  mutate(z_percent_CD11b_stroma.i = (percent_CD11b_stroma.i - median(percent_CD11b_stroma.i, na.rm = TRUE))/
           mad_percent_CD11b_stroma.i) %>% 
  mutate(z_percent_CD11b_CD15_stroma.i = (percent_CD11b_CD15_stroma.i - median(percent_CD11b_CD15_stroma.i, na.rm = TRUE))/
           mad_percent_CD11b_CD15_stroma.i) %>% 
  # mutate(across(where(is.numeric)), abs(. - median(CD3, na.rm = TRUE))
  #        )
  select(c(starts_with("z_")))

df5 <- (t(scale(as.matrix(z_sqrt.markers)))) # scale for standardizing the data to make variables comparable
df5 <- df5[complete.cases(df5 * 0), , drop=FALSE]

col = c("#000004FF", "#51127CFF", "#B63679FF", "#FB8861FF", "#FCFDBFFF")
breaks <- c(-2, -1, 0, 1, 2, 10)
heatmap.plus(df5, 
             main = "Heatmap",
             breaks = breaks, col = col,
             cexRow = 1,
             margins = c(1, 17), # bottom, right
             ColSideColors = mycols,
             scale = "none",
             Rowv = NA, Colv = NA,
             
)

heatmap.2(df5, breaks = breaks, col = col)
Heatmap(df5)

library(circlize)
col_fun = colorRamp2(c(-2, 0, 2), c("green", "white", "red"))
col_fun(seq(-3, 3))
Heatmap(df5, name = "mat", col = col_fun)
Heatmap(df5, name = "mat", col = rainbow(10), 
        column_title = "set a color vector for a continuous matrix")

f1 = colorRamp2(seq(min(df5), max(df5), length = 3), c("blue", "#EEEEEE", "red"))
f2 = colorRamp2(seq(min(df5), max(df5), length = 3), c("blue", "#EEEEEE", "red"), space = "RGB")
Heatmap(df5, name = "mat1", col = f1, column_title = "LAB color space")
Heatmap(df5, name = "mat2", col = f2, column_title = "RGB color space",
        cluster_rows = FALSE, cluster_columns = FALSE)

sc_df1 <- z_sqrt.markers
class(sc_df1) <- "data.frame"
for(i in 1:length(colnames(sc_df1))) {
  if(class(sc_df1[,i]) == "numeric" | class(sc_df1[,i]) == "integer") {
    sc_df1[,i] <- scales::rescale(sc_df1[,i], to=c(-2,2)) }
}


df2 <- t((((sc_df1)))) # scale for standardizing the data to make variables comparable
df2 <- df2[complete.cases(df2 * 0), , drop=FALSE]

heatmap.plus(df2, 
             main = "Heatmap",
             col = bluered(20),
             cexRow = 1,
             margins = c(1, 17), # bottom, right
             # ColSideColors = mycols,
             scale = "none",
             Rowv = NA, Colv = NA
)

Heatmap(df2)

library(circlize)
col_fun = colorRamp2(c(-2, 0, 2), c("blue", "white", "red"))
col_fun(seq(-3, 1))
Heatmap(df2, name = "mat", col = col_fun)


