###################################################################################### I ### Within patient corr----
# Intraclass Correlation Coefficient----
# The ICC is a measure of the reliability of measurements or ratings, for the purpose of assessing inter-rater reliability
# How to choose the correct ICC forms
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2628443/
# https://www.datanovia.com/en/lessons/intraclass-correlation-coefficient-in-r/#:~:text=Generally%20speaking%2C%20the%20ICC%20determines,all%20ratings%20and%20all%20individuals.
ICC_ROIi <- ROI_global %>% 
  filter(intratumoral_i_vs_peripheral_p_ == "Intratumoral") %>%
  select(c("suid", "tumor_total_cells")) %>% 
  mutate(suid = as.numeric(suid))
ICC_ROIi <- dcast(setDT(ICC_ROIi), suid ~ rowid(suid), 
                 value.var = "tumor_total_cells") %>% 
  select(c(2:4))

ICC_ROIp <- ROI_global %>% 
  filter(intratumoral_i_vs_peripheral_p_ == "Peripheral") %>%
  select(c("suid", "tumor_total_cells"))
ICC_ROIp <- dcast(setDT(ICC_ROIp), suid ~ rowid(suid), 
                  value.var = "tumor_total_cells") %>% 
  select(c(2:4))

ICC_TMA <- TMA_global[, c("suid", "tumor_total_cells")] 
ICC_TMA <- dcast(setDT(ICC_TMA), suid ~ rowid(suid), 
                 value.var = "tumor_total_cells") %>% 
  select(c(2:4))
# Check density
# ICC_d_ROIi <- ROI_global %>% 
#   filter(intratumoral_i_vs_peripheral_p_ == "Intratumoral") %>%
#   select(c("suid", "CD3_tumor_mm2", "CD3_stroma_mm2"))
# ICC_d_ROIi <- dcast(setDT(ICC_d_ROIi), suid ~ rowid(suid), 
#                   value.var = c("CD3_tumor_mm2", "CD3_stroma_mm2")) %>% 
#   select(c(2:4, 14:16))
# vs %
ICC_p_ROIi <- ROI_global %>% 
  filter(intratumoral_i_vs_peripheral_p_ == "Intratumoral") %>%
  select(c("suid", "tumor_percent_cd3_opal_650_positive_cells", 
           "stroma_percent_cd3_opal_650_positive_cells"))
ICC_p_ROIi <- dcast(setDT(ICC_p_ROIi), suid ~ rowid(suid), 
                    value.var = c("tumor_percent_cd3_opal_650_positive_cells", 
                    "stroma_percent_cd3_opal_650_positive_cells")) %>% 
  select(c(2:4, 14:16))

# loop
ICC_roi_periph <- ROI_global %>%
  filter(intratumoral_i_vs_peripheral_p_ == "Peripheral") %>% 
  select(suid, image_tag, #tumor_total_cells, stroma_total_cells, 
         contains("percent"))
class(ICC_roi_periph) <- "data.frame"
ICC_data <- data.frame(matrix(nrow=1, ncol=0))

for(i in 1:length(colnames(ICC_roi_periph))) {
  
  rad <- ICC_roi_periph %>% select(suid)
  
  if(class(ICC_roi_periph[,i]) == "numeric" | class(ICC_roi_periph[,i]) == "integer") {
    
    ICC_df <- cbind(rad, value = ICC_roi_periph[,i])
    ICC_df <- ICC_df %>%
      mutate(Slide = "Slide0") %>%
      group_by(suid) %>%
      mutate(n = row_number(suid)) %>%
      ungroup() %>%
      unite(slide_id, Slide:n, sep = "", remove = TRUE, na.rm = TRUE) %>%
      pivot_wider(names_from = slide_id, values_from = value) %>%
      select(c(starts_with("slide")))
    
    ICC <- ICC(ICC_df)$results[4,2]
    ICC_data <- cbind(ICC_data,ICC)
    
  }
  
}
colnames(ICC_data) <- colnames(ICC_roi_periph)[2:ncol(ICC_roi_periph)]

rm(ICC_df, rad)
# If test for intra-rater (because for each patient the 3 TMA/ROI was done by the same person)
# If test for inter-rater (because not the same rater between patients)
# Two-way mixed effect, fixed raters are defined. Each subject is measured by the k raters.
library(psych) # Koo and Li (2016)
ICC(ICC_TMA)  # between 0.75 and 0.90: good
ICC(ICC_ROIi)
ICC(ICC_ROIp) #between 0.50 and 0.75: moderate
# ICC(ICC_d_ROIi)
# ICC(ICC_p_ROIi)
library(irr) # Does not count patient with NA
icc(
  ICC_TMA, model = "twoway", 
  type = "consistency", unit = "average"
)
icc(
  ICC_ROIi, model = "twoway", 
  type = "consistency", unit = "average"# Reliability applied where measures of k raters will be averaged for each subject.
)
icc(
  ICC_ROIp, model = "twoway", 
  type = "consistency", unit = "average" # Consistency: for repeated measurements by the same rater,
)

# ICC immune markers- look at ppt for more
ICC_ROIi_CD11bCD15_ov <- ROI_global %>% 
  filter(intratumoral_i_vs_peripheral_p_ == "Intratumoral") %>%
  select(c("suid", "cd11bplus_cd15plus_cells"))
ICC_ROIi_CD11bCD15_ov <- dcast(setDT(ICC_ROIi_CD11bCD15_ov), suid ~ rowid(suid), 
                  value.var = "cd11bplus_cd15plus_cells") %>% 
  select(c(2:4))

ICC(ICC_ROIi_CD11bCD15_ov)
icc(
  ICC_ROIi_CD11bCD15_ov, model = "twoway", 
  type = "consistency", unit = "average"
)


# Cleaning
rm(ICC_TMA, ICC_ROIi, ICC_ROIp, ICC_d_ROIi, ICC_p_ROIi)


###################################################################################### II ### TMA vs ROI corr----
# Kappa stat----
# https://www.datanovia.com/en/lessons/cohens-kappa-in-r-for-two-categorical-variables/#:~:text=Cohen's%20Kappa%20in%20R%3A%20For%20Two%20Categorical%20Variables,-20%20mins&text=Cohen's%20kappa%20(Jacob%20Cohen%201960,methods%20rating%20on%20categorical%20scales.&text=The%20Cohen's%20kappa%20is%20a,that%20removes%20this%20chance%20agreement.

# To get categorical var, create tertile on whole population
df.tertile <- data.frame(
  suid = markers$suid,
  tumor_tert_tma = ntile(markers$mean_tumor_tma, 4),
  stroma_tert_tma = ntile(markers$mean_stroma_tma, 4),
  tert_CD3_tumor_tma = ntile(markers$percent_CD3_tumor_tma, 4),
  tert_CD8_tumor_tma = ntile(markers$percent_CD8_tumor_tma, 4),
  tert_CD3_CD8_tumor_tma = ntile(markers$percent_CD3_CD8_tumor_tma, 4),
  tert_FoxP3_tumor_tma = ntile(markers$percent_FoxP3_tumor_tma, 4),
  tert_CD3_FoxP3_tumor_tma = ntile(markers$percent_CD3_FoxP3_tumor_tma, 4),
  tert_CD11b_tumor_tma = ntile(markers$percent_CD11b_tumor_tma, 4),
  tert_CD15_tumor_tma = ntile(markers$percent_CD15_tumor_tma, 4),
  tert_CD11b_CD15_tumor_tma = ntile(markers$percent_CD11b_CD15_tumor_tma, 4),
  tert_CD3_stroma_tma = ntile(markers$percent_CD3_stroma_tma, 4),
  tert_CD8_stroma_tma = ntile(markers$percent_CD8_stroma_tma, 4),
  tert_CD3_CD8_stroma_tma = ntile(markers$percent_CD3_CD8_stroma_tma, 4),
  tert_FoxP3_stroma_tma = ntile(markers$percent_FoxP3_stroma_tma, 4),
  tert_CD3_FoxP3_stroma_tma = ntile(markers$percent_CD3_FoxP3_stroma_tma, 4),
  tert_CD11b_stroma_tma = ntile(markers$percent_CD11b_stroma_tma, 4),
  tert_CD15_stroma_tma = ntile(markers$percent_CD15_stroma_tma, 4),
  tert_CD11b_CD15_stroma_tma = ntile(markers$percent_CD11b_CD15_stroma_tma, 4),
  tert_CD3_total_tma = ntile(markers$percent_CD3_total_tma, 4),
  tert_CD8_total_tma = ntile(markers$percent_CD8_total_tma, 4),
  tert_CD3_CD8_total_tma = ntile(markers$percent_CD3_CD8_total_tma, 4),
  tert_FoxP3_total_tma = ntile(markers$percent_FoxP3_total_tma, 4),
  
  tumor_tert.i = ntile(markers$mean_tumor.i, 4),
  stroma_tert.i = ntile(markers$mean_stroma.i, 4),
  tert_CD3_tumor.i = ntile(markers$percent_CD3_tumor.i, 4),
  tert_CD8_tumor.i = ntile(markers$percent_CD8_tumor.i, 4),
  tert_CD3_CD8_tumor.i = ntile(markers$percent_CD3_CD8_tumor.i, 4),
  tert_FoxP3_tumor.i = ntile(markers$percent_FoxP3_tumor.i, 4),
  tert_CD3_FoxP3_tumor.i = ntile(markers$percent_CD3_FoxP3_tumor.i, 4),
  tert_CD11b_tumor.i = ntile(markers$percent_CD11b_tumor.i, 4),
  tert_CD15_tumor.i = ntile(markers$percent_CD15_tumor.i, 4),
  tert_CD11b_CD15_tumor.i = ntile(markers$percent_CD11b_CD15_tumor.i, 4),
  tert_CD3_stroma.i = ntile(markers$percent_CD3_stroma.i, 4),
  tert_CD8_stroma.i = ntile(markers$percent_CD8_stroma.i, 4),
  tert_CD3_CD8_stroma.i = ntile(markers$percent_CD3_CD8_stroma.i, 4),
  tert_FoxP3_stroma.i = ntile(markers$percent_FoxP3_stroma.i, 4),
  tert_CD3_FoxP3_stroma.i = ntile(markers$percent_CD3_FoxP3_stroma.i, 4),
  tert_CD11b_stroma.i = ntile(markers$percent_CD11b_stroma.i, 4),
  tert_CD15_stroma.i = ntile(markers$percent_CD15_stroma.i, 4),
  tert_CD11b_CD15_stroma.i = ntile(markers$percent_CD11b_CD15_stroma.i, 4),
  tert_CD3_total.i = ntile(markers$percent_CD3_total.i, 4),
  tert_CD8_total.i = ntile(markers$percent_CD8_total.i, 4),
  tert_CD3_CD8_total.i = ntile(markers$percent_CD3_CD8_total.i, 4),
  tert_FoxP3_total.i = ntile(markers$percent_FoxP3_total.i, 4))

uid <- paste(unique(common_ROITMA_IDs$Subject_IDs), collapse = '|') # Restrict to the 28 patients
df.tertile <- df.tertile[(grepl(uid, df.tertile$suid)),]

table(df.tertile$tert_CD3_tumor_tma, df.tertile$tert_CD3_tumor.i)
table(df.tertile$tert_CD3_CD8_tumor_tma, df.tertile$tert_CD3_CD8_tumor.i)
table(df.tertile$tert_CD3_FoxP3_tumor_tma, df.tertile$tert_CD3_FoxP3_tumor.i)
table(df.tertile$tert_CD3_stroma_tma, df.tertile$tert_CD3_stroma.i)
table(df.tertile$tert_CD3_CD8_stroma_tma, df.tertile$tert_CD3_CD8_stroma.i)
table(df.tertile$tert_CD3_FoxP3_stroma_tma, df.tertile$tert_CD3_FoxP3_stroma.i)



xtab <- table(df.tertile$tumor_tert_tma, df.tertile$tumor_tert.i)
diagonal.counts <- diag(xtab)
N <- sum(xtab)
row.marginal.props <- rowSums(xtab)/N
col.marginal.props <- colSums(xtab)/N
Po <- sum(diagonal.counts)/N
Pe <- sum(row.marginal.props*col.marginal.props)
kappa <- (Po - Pe)/(1 - Pe)
kappa # 0.2595978

library(irr)
# If the data is ordinal, then it may be appropriate to use a weighted Kappa. 
# For example, if the possible values are low, medium, and high, then if a case were rated 
# medium and high by the two coders, they would be in better agreement than if the ratings were low and high.
#   VALUE OF K	    STRENGTH OF AGREEMENT
#   < 0	            Poor
#   0.01 - 0.20	    Slight
#   0.21-0.40	      Fair
#   0.41-0.60	      Moderate
#   0.61-0.80	      Substantial
#   0.81 - 1.00	    Almost perfect
kappa2(df.tertile[,c("tumor_tert_tma","tumor_tert.i")], "unweighted") # Higher with unweighted, probably bc 1 can be 4

kappa2(df.tertile[,c("tumor_tert_tma","tumor_tert.i")], "unweighted")$value

df.tertile_kappa <- data.frame(
  type = "kappa",
  tumor_tert = kappa2(df.tertile[,c("tumor_tert_tma","tumor_tert.i")], "unweighted")$value,
  stroma_tert = kappa2(df.tertile[,c("stroma_tert_tma","stroma_tert.i")], "unweighted")$value,
  tert_CD3_tumor = kappa2(df.tertile[,c("tert_CD3_tumor_tma","tert_CD3_tumor.i")], "unweighted")$value,
  tert_CD8_tumor = kappa2(df.tertile[,c("tert_CD8_tumor_tma","tert_CD8_tumor.i")], "unweighted")$value,
  tert_CD3_CD8_tumor = kappa2(df.tertile[,c("tert_CD3_CD8_tumor_tma","tert_CD3_CD8_tumor.i")], "unweighted")$value,
  tert_FoxP3_tumor = kappa2(df.tertile[,c("tert_FoxP3_tumor_tma","tert_FoxP3_tumor.i")], "unweighted")$value,
  tert_CD3_FoxP3_tumor = kappa2(df.tertile[,c("tert_CD3_FoxP3_tumor_tma","tert_CD3_FoxP3_tumor.i")], "unweighted")$value,
  tert_CD11b_tumor = kappa2(df.tertile[,c("tert_CD11b_tumor_tma","tert_CD11b_tumor.i")], "unweighted")$value,
  tert_CD15_tumor = kappa2(df.tertile[,c("tert_CD15_tumor_tma","tert_CD15_tumor.i")], "unweighted")$value,
  tert_CD11b_CD15_tumor = kappa2(df.tertile[,c("tert_CD11b_CD15_tumor_tma","tert_CD11b_CD15_tumor.i")], "unweighted")$value,
  tert_CD3_stroma = kappa2(df.tertile[,c("tert_CD3_stroma_tma","tert_CD3_stroma.i")], "unweighted")$value,
  tert_CD8_stroma = kappa2(df.tertile[,c("tert_CD8_stroma_tma","tert_CD8_stroma.i")], "unweighted")$value,
  tert_CD3_CD8_stroma = kappa2(df.tertile[,c("tert_CD3_CD8_stroma_tma","tert_CD3_CD8_stroma.i")], "unweighted")$value,
  tert_FoxP3_stroma = kappa2(df.tertile[,c("tert_FoxP3_stroma_tma","tert_FoxP3_stroma.i")], "unweighted")$value,
  tert_CD3_FoxP3_stroma = kappa2(df.tertile[,c("tert_CD3_FoxP3_stroma_tma","tert_CD3_FoxP3_stroma.i")], "unweighted")$value,
  tert_CD11b_stroma = kappa2(df.tertile[,c("tert_CD11b_stroma_tma","tert_CD11b_stroma.i")], "unweighted")$value,
  tert_CD15_stroma = kappa2(df.tertile[,c("tert_CD15_stroma_tma","tert_CD15_stroma.i")], "unweighted")$value,
  tert_CD11b_CD15_stroma = kappa2(df.tertile[,c("tert_CD11b_CD15_stroma_tma","tert_CD11b_CD15_stroma.i")], "unweighted")$value,
  tert_CD3_total = kappa2(df.tertile[,c("tert_CD3_total_tma","tert_CD3_total.i")], "unweighted")$value,
  tert_CD8_total = kappa2(df.tertile[,c("tert_CD8_total_tma","tert_CD8_total.i")], "unweighted")$value,
  tert_CD3_CD8_total = kappa2(df.tertile[,c("tert_CD3_CD8_total_tma","tert_CD3_CD8_total.i")], "unweighted")$value,
  tert_FoxP3_total = kappa2(df.tertile[,c("tert_FoxP3_total_tma","tert_FoxP3_total.i")], "unweighted")$value
) %>% 
  t(.) %>% as.data.frame(.)

rm(df.tertile, df.tertile_kappa)

# ICC stat----
# Can use the continuous data
ICC_ <- markers_28[ , c("mean_tumor_tma","mean_tumor.i")]
ICC(ICC_)  
icc(
  ICC_, model = "twoway", 
  type = "consistency", unit = "average"
)

ICC_ <- markers_28[ , c("percent_CD3_tumor_tma","percent_CD3_tumor.i")]
ICC(ICC_)  
icc(
  ICC_, model = "twoway", 
  type = "consistency", unit = "average"
)

ICC_ <- markers_28[ , c("percent_CD3_CD8_tumor_tma","percent_CD3_CD8_tumor.i")]

ICC(ICC_)  
icc(
  ICC_, model = "twoway", 
  type = "consistency", unit = "average"# between 0.50 and 0.75: moderate
)


# var_markers28 => does,'t change anything
var_markers28[ , c("var_CD3_CD8_tumor_tma","var_CD3_CD8_tumor.i")] %>% 
  ICC()

colnames(var_markers28)

t.test(markers_28$percent_CD3_CD8_tumor_tma, markers_28$percent_CD3_CD8_tumor.i, alternative = "two.sided",
       paired = TRUE,
       var.equal= TRUE)


# Take patients per quartile to calculate ICC

markers_28$CD3_CD8_grp

markers_28 %>% 
  filter(CD3_CD8_grp == "Low") %>% 
  select("percent_CD3_CD8_tumor_tma","percent_CD3_CD8_tumor.i") %>% 
  ICC()

markers_28 %>% 
  filter(CD3_CD8_grp == "Medium") %>% 
  select("percent_CD3_CD8_tumor_tma","percent_CD3_CD8_tumor.i") %>% 
  ICC()

markers_28 %>% 
  filter(CD3_CD8_grp == "High") %>% 
  select("percent_CD3_CD8_tumor_tma","percent_CD3_CD8_tumor.i") %>% 
  ICC()


# Other ICC per patient...
ICC_ <- markers_28 %>% gather(key = "marker", value = "value", c("percent_CD3_CD8_tumor_tma","percent_CD3_CD8_tumor.i")) %>% 
  select("suid", "value")
ICC::Nest("p", x = suid, y = value, data = ICC_)
ICC::ICCest(suid, value, ICC_)

a <- global_28 %>% filter(suid == "110188")
cor(a$tumor_percent_cd3plus_cd8plus_positive_cells.x, a$tumor_percent_cd3plus_cd8plus_positive_cells.y,
    method = "pearson", use = "complete.obs")

rm(global_28)


# Cor plot----
# Test normality
qqnorm(markers$mean_tumor_tma)
qqnorm(markers$mean_tumor.i)
pairs.panels(markers[c("mean_tumor_tma", "mean_stroma_tma", # "tumor_variation_tma", "stroma_variation_tma",
                       "mean_tumor.i", "mean_stroma.i", # "tumor_variation_roi_i", "stroma_variation_roi_i",
                       "mean_tumor.p", "mean_stroma.p"# , "tumor_variation_roi_p", "stroma_variation_roi_p"
)])

# Correlation----
# var_cor <- markers[, c("mean_tumor_tma", "mean_tumor.i", "mean_tumor.p")]

mat <- cor(markers[, c("mean_tumor_tma", "mean_tumor.i", "mean_tumor.p", 
                       "mean_stroma_tma", "mean_stroma.i", "mean_stroma.p")], 
           use = "pairwise.complete.obs")
corrplot(mat)
corrplot.mixed(mat)

# mat1 <- cor(markers[, c("tumor_variation", "tumor_variation_roi_i", "tumor_variation_roi_p", 
#                         "stroma_variation", "stroma_variation_roi_i", "stroma_variation_roi_p")], 
#             use = "pairwise.complete.obs")
# corrplot(mat1)
# corrplot.mixed(mat1)
ggcorrplot(mat, hc.order = TRUE, method = "circle", 
           # outline.col = "darkblue", # the outline of the circle or sqare
           # hc.method = "complete",
           type = "lower", # show the top half panel
           lab = TRUE, # add correlation nbr
           title = "Correlation between collection type",
           show.legend = TRUE, legend.title = "Correlation", show.diag = TRUE,
           # colors = viridis::inferno(n=3),
           lab_col = "darkblue", lab_size = 3, # col and size of the correlation nbr
           # p.mat = pmat, # Add correlation significance
           sig.level = 0.05, insig = c("pch", "blank"), pch = 4, pch.col = "black", pch.cex = 10, 
           tl.cex = 10, tl.col = "red", tl.srt = 40,
           digits = 2
)

mat <- cor(markers[, c("percent_CD3_tumor_tma", "percent_CD3_tumor.i", "percent_CD8_tumor_tma", "percent_CD8_tumor.i",
                       "percent_CD3_CD8_tumor_tma", "percent_CD3_CD8_tumor.i", "percent_FoxP3_tumor_tma", "percent_FoxP3_tumor.i",
                       "percent_CD3_FoxP3_tumor_tma", "percent_CD3_FoxP3_tumor.i", "percent_CD11b_tumor_tma", "percent_CD11b_tumor.i",
                       "percent_CD15_tumor_tma", "percent_CD15_tumor.i", "percent_CD11b_CD15_tumor_tma", "percent_CD11b_CD15_tumor.i",
                       "percent_CD3_stroma_tma", "percent_CD3_stroma.i", "percent_CD8_stroma_tma", "percent_CD8_stroma.i",
                       "percent_CD3_CD8_stroma_tma", "percent_CD3_CD8_stroma.i", "percent_FoxP3_stroma_tma", "percent_FoxP3_stroma.i",
                       "percent_CD3_FoxP3_stroma_tma", "percent_CD3_FoxP3_stroma.i", "percent_CD11b_stroma_tma", "percent_CD11b_stroma.i", 
                       "percent_CD15_stroma_tma", "percent_CD15_stroma.i", "percent_CD11b_CD15_stroma_tma", "percent_CD11b_CD15_stroma.i")],
           use = "pairwise.complete.obs")
corrplot.mixed(mat)
ggcorrplot(mat, hc.order = FALSE, method = "circle", 
           # outline.col = "darkblue", # the outline of the circle or sqare
           # hc.method = "complete",
           type = "upper", # show the top half panel
           lab = TRUE, lab_col = "darkblue", lab_size = 3, # add correlation nbr, col and size of the correlation nbr
           title = "Immune markers correlation in TMA vs intratumoral ROI",
           show.legend = TRUE, legend.title = "Correlation", show.diag = FALSE,
           # colors = viridis::inferno(n=3),
           # p.mat = pmat, # Add correlation significance
           sig.level = 0.05, insig = c("pch", "blank"), pch = 4, pch.col = "black", pch.cex = 10, 
           tl.cex = 10, tl.col = "red", tl.srt = 40,
           digits = 2
)

# For the 28 patients----
mat <- cor(markers_28[, c("percent_CD3_tumor_tma", "percent_CD3_tumor.i", "percent_CD8_tumor_tma", "percent_CD8_tumor.i",
                          "percent_CD3_CD8_tumor_tma", "percent_CD3_CD8_tumor.i", "percent_FoxP3_tumor_tma", "percent_FoxP3_tumor.i",
                          "percent_CD3_FoxP3_tumor_tma", "percent_CD3_FoxP3_tumor.i", "percent_CD11b_tumor_tma", "percent_CD11b_tumor.i",
                          "percent_CD15_tumor_tma", "percent_CD15_tumor.i", "percent_CD11b_CD15_tumor_tma", "percent_CD11b_CD15_tumor.i",
                          "percent_CD3_stroma_tma", "percent_CD3_stroma.i", "percent_CD8_stroma_tma", "percent_CD8_stroma.i",
                          "percent_CD3_CD8_stroma_tma", "percent_CD3_CD8_stroma.i", "percent_FoxP3_stroma_tma", "percent_FoxP3_stroma.i",
                          "percent_CD3_FoxP3_stroma_tma", "percent_CD3_FoxP3_stroma.i", "percent_CD11b_stroma_tma", "percent_CD11b_stroma.i", 
                          "percent_CD15_stroma_tma", "percent_CD15_stroma.i", "percent_CD11b_CD15_stroma_tma", "percent_CD11b_CD15_stroma.i")],
           use = "pairwise.complete.obs")
corrplot.mixed(mat)

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/Correlation Immune markers for 28 patients.jpg"),
#      width = 1300, height = 800, quality = 100)
ggcorrplot(mat, hc.order = FALSE, method = "square", 
           # outline.col = "darkblue", # the outline of the circle or sqare
           # hc.method = "complete",
           type = "upper", # show the top half panel
           lab = TRUE, lab_col = "darkblue", lab_size = 2.5, # add correlation nbr, col and size of the correlation nbr
           title = "Immune markers correlation in TMA vs Intratumoral ROI",
           show.legend = TRUE, legend.title = "Correlation", show.diag = TRUE,
           # colors = viridis::inferno(n=3),
           # p.mat = pmat, # Add correlation significance
           sig.level = 0.05, insig = c("pch", "blank"), pch = 4, pch.col = "black", pch.cex = 10, 
           tl.cex = 10, tl.col = "red", tl.srt = 40,
           digits = 2
)
# dev.off()

mat <- cor(markers_28[, c("percent_CD3_tumor_tma", "percent_CD3_tumor.i", "percent_CD8_tumor_tma", "percent_CD8_tumor.i",
                          "percent_CD3_CD8_tumor_tma", "percent_CD3_CD8_tumor.i", "percent_FoxP3_tumor_tma", "percent_FoxP3_tumor.i",
                          "percent_CD3_FoxP3_tumor_tma", "percent_CD3_FoxP3_tumor.i",
                          "percent_CD3_stroma_tma", "percent_CD3_stroma.i", "percent_CD8_stroma_tma", "percent_CD8_stroma.i",
                          "percent_CD3_CD8_stroma_tma", "percent_CD3_CD8_stroma.i", "percent_FoxP3_stroma_tma", "percent_FoxP3_stroma.i",
                          "percent_CD3_FoxP3_stroma_tma", "percent_CD3_FoxP3_stroma.i")],
           use = "pairwise.complete.obs")
mat1 <- cor(markers_28[, c("percent_CD11b_tumor_tma", "percent_CD11b_tumor.i",
                           "percent_CD15_tumor_tma", "percent_CD15_tumor.i", "percent_CD11b_CD15_tumor_tma", "percent_CD11b_CD15_tumor.i",
                           "percent_CD11b_stroma_tma", "percent_CD11b_stroma.i", 
                           "percent_CD15_stroma_tma", "percent_CD15_stroma.i", "percent_CD11b_CD15_stroma_tma", "percent_CD11b_CD15_stroma.i")],
            use = "pairwise.complete.obs")

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/Correlation Immune markers for 28 patients separated.jpg"),
#      width = 1300, height = 800, quality = 100)
p1 <- ggcorrplot(mat, hc.order = FALSE, method = "square", 
                 # outline.col = "darkblue", # the outline of the circle or sqare
                 # hc.method = "complete",
                 type = "upper", # show the top half panel
                 lab = TRUE, lab_col = "darkblue", lab_size = 2.5, # add correlation nbr, col and size of the correlation nbr
                 title = "CD3/CD8/FoxP3: TMA vs Intratumoral ROI",
                 show.legend = TRUE, legend.title = "Correlation", show.diag = TRUE,
                 # colors = viridis::inferno(n=3),
                 # p.mat = pmat, # Add correlation significance
                 sig.level = 0.05, insig = c("pch", "blank"), pch = 4, pch.col = "black", pch.cex = 10, 
                 tl.cex = 10, tl.col = "red", tl.srt = 40,
                 digits = 2
)
p2 <- ggcorrplot(mat1, hc.order = FALSE, method = "square", 
                 # outline.col = "darkblue", # the outline of the circle or sqare
                 # hc.method = "complete",
                 type = "upper", # show the top half panel
                 lab = TRUE, lab_col = "darkblue", lab_size = 2.5, # add correlation nbr, col and size of the correlation nbr
                 title = "CD11b/CD15: TMA vs Intratumoral ROI",
                 show.legend = TRUE, legend.title = "Correlation", show.diag = TRUE,
                 # colors = viridis::inferno(n=3),
                 # p.mat = pmat, # Add correlation significance
                 sig.level = 0.05, insig = c("pch", "blank"), pch = 4, pch.col = "black", pch.cex = 10, 
                 tl.cex = 10, tl.col = "red", tl.srt = 40,
                 digits = 2
)
gridExtra::grid.arrange(p1, p2, ncol = 2, top="Immune markers correlation in TMA vs Intratumoral ROI")
# dev.off()

mat <- cor(markers_28[, c("percent_CD11b_tumor_tma", "percent_CD11b_tumor.i",
                          "percent_CD15_tumor_tma", "percent_CD15_tumor.i", "percent_CD11b_CD15_tumor_tma", "percent_CD11b_CD15_tumor.i",
                          "percent_CD11b_stroma_tma", "percent_CD11b_stroma.i", 
                          "percent_CD15_stroma_tma", "percent_CD15_stroma.i", "percent_CD11b_CD15_stroma_tma", "percent_CD11b_CD15_stroma.i")],
           use = "pairwise.complete.obs")
mat1 <- cor(markers_28[, c("percent_CD11b_tumor_tma", "percent_CD11b_tumor.p",
                           "percent_CD15_tumor_tma", "percent_CD15_tumor.p", "percent_CD11b_CD15_tumor_tma", "percent_CD11b_CD15_tumor.p",
                           "percent_CD11b_stroma_tma", "percent_CD11b_stroma.p", 
                           "percent_CD15_stroma_tma", "percent_CD15_stroma.p", "percent_CD11b_CD15_stroma_tma", "percent_CD11b_CD15_stroma.p")],
            use = "pairwise.complete.obs")

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/Correlation CD11bCD15 for 28 patients intra or periph vs TMA.jpg"),
#      width = 1300, height = 800, quality = 100)
p1 <- ggcorrplot(mat, hc.order = FALSE, method = "square", 
                 # outline.col = "darkblue", # the outline of the circle or sqare
                 # hc.method = "complete",
                 type = "upper", # show the top half panel
                 lab = TRUE, lab_col = "darkblue", lab_size = 3, # add correlation nbr, col and size of the correlation nbr
                 title = "CD11b/CD15: TMA vs Intratumoral ROI",
                 show.legend = TRUE, legend.title = "Correlation", show.diag = TRUE,
                 # colors = viridis::inferno(n=3),
                 # p.mat = pmat, # Add correlation significance
                 sig.level = 0.05, insig = c("pch", "blank"), pch = 4, pch.col = "black", pch.cex = 10, 
                 tl.cex = 10, tl.col = "red", tl.srt = 40,
                 digits = 2
)
p2 <- ggcorrplot(mat1, hc.order = FALSE, method = "square", 
                 # outline.col = "darkblue", # the outline of the circle or sqare
                 # hc.method = "complete",
                 type = "upper", # show the top half panel
                 lab = TRUE, lab_col = "darkblue", lab_size = 3, # add correlation nbr, col and size of the correlation nbr
                 title = "CD11b/CD15: TMA vs Peripheral ROI",
                 show.legend = TRUE, legend.title = "Correlation", show.diag = TRUE,
                 # colors = viridis::inferno(n=3),
                 # p.mat = pmat, # Add correlation significance
                 sig.level = 0.05, insig = c("pch", "blank"), pch = 4, pch.col = "black", pch.cex = 10, 
                 tl.cex = 10, tl.col = "red", tl.srt = 40,
                 digits = 2
)
gridExtra::grid.arrange(p1, p2, ncol = 2, top="Immune markers correlation in TMA vs Intratumoral or Peripheral ROI")
# dev.off()

# Pairwise
# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/Pairwise tumor stromal cells for 28 patients.jpg"),
#      width = 1300, height = 800, quality = 100)
pairs.panels(markers_28[c("mean_tumor_tma", "mean_stroma_tma",
                          "mean_tumor.i", "mean_stroma.i",
                          "mean_tumor.p", "mean_stroma.p"
)])
# dev.off()

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/Pairwise Immune markers CD3CD8FoxP3 for 28 patients.jpg"),
#      width = 1300, height = 800, quality = 100)
pairs.panels(markers_28[c("percent_CD3_tumor_tma", "percent_CD3_tumor.i", "percent_CD8_tumor_tma", "percent_CD8_tumor.i",
                          "percent_CD3_CD8_tumor_tma", "percent_CD3_CD8_tumor.i", "percent_FoxP3_tumor_tma", "percent_FoxP3_tumor.i",
                          "percent_CD3_FoxP3_tumor_tma", "percent_CD3_FoxP3_tumor.i",
                          "percent_CD3_stroma_tma", "percent_CD3_stroma.i", "percent_CD8_stroma_tma", "percent_CD8_stroma.i",
                          "percent_CD3_CD8_stroma_tma", "percent_CD3_CD8_stroma.i", "percent_FoxP3_stroma_tma", "percent_FoxP3_stroma.i",
                          "percent_CD3_FoxP3_stroma_tma", "percent_CD3_FoxP3_stroma.i"
)])
# dev.off()
# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/Pairwise Immune markers CD11bCD15 for 28 patients.jpg"),
#      width = 1300, height = 800, quality = 100)
pairs.panels(markers_28[c("percent_CD11b_tumor_tma", "percent_CD11b_tumor.i",
                          "percent_CD15_tumor_tma", "percent_CD15_tumor.i", "percent_CD11b_CD15_tumor_tma", "percent_CD11b_CD15_tumor.i",
                          "percent_CD11b_stroma_tma", "percent_CD11b_stroma.i", 
                          "percent_CD15_stroma_tma", "percent_CD15_stroma.i", "percent_CD11b_CD15_stroma_tma", "percent_CD11b_CD15_stroma.i"
)])
# dev.off()

# Why is there a difference?
table <- as.data.table(markers_28[c("percent_CD3_tumor_tma", "percent_CD3_tumor.i", "percent_CD8_tumor_tma", "percent_CD8_tumor.i",
                                    "percent_CD3_CD8_tumor_tma", "percent_CD3_CD8_tumor.i", "percent_FoxP3_tumor_tma", "percent_FoxP3_tumor.i",
                                    "percent_CD3_FoxP3_tumor_tma", "percent_CD3_FoxP3_tumor.i",
                                    "percent_CD3_stroma_tma", "percent_CD3_stroma.i", "percent_CD8_stroma_tma", "percent_CD8_stroma.i",
                                    "percent_CD3_CD8_stroma_tma", "percent_CD3_CD8_stroma.i", "percent_FoxP3_stroma_tma", "percent_FoxP3_stroma.i",
                                    "percent_CD3_FoxP3_stroma_tma", "percent_CD3_FoxP3_stroma.i", 
                                    "percent_CD11b_tumor_tma", "percent_CD11b_tumor.i",
                                    "percent_CD15_tumor_tma", "percent_CD15_tumor.i", "percent_CD11b_CD15_tumor_tma", "percent_CD11b_CD15_tumor.i",
                                    "percent_CD11b_stroma_tma", "percent_CD11b_stroma.i", 
                                    "percent_CD15_stroma_tma", "percent_CD15_stroma.i", "percent_CD11b_CD15_stroma_tma", "percent_CD11b_CD15_stroma.i"
)])
table

# Correlation with total cells
mat <-
  cor(markers_28[, c("percent_CD3_total_tma", "percent_CD8_total_tma", "percent_CD3_CD8_total_tma", "percent_FoxP3_total_tma",
                     "percent_CD3_FoxP3_total_tma", "percent_CD11b_total_tma", "percent_CD15_total_tma", "percent_CD11b_CD15_total_tma",
                     "percent_CD3_total.i", "percent_CD8_total.i", "percent_CD3_CD8_total.i", "percent_FoxP3_total.i", 
                     "percent_CD3_FoxP3_total.i", "percent_CD11b_total.i", "percent_CD15_total.i", "percent_CD11b_CD15_total.i")],
      use = "pairwise.complete.obs")
corrplot.mixed(mat)

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/Correlation .jpg"),
#      width = 1300, height = 800, quality = 100)
ggcorrplot(mat, hc.order = FALSE, method = "square", 
           # outline.col = "darkblue", # the outline of the circle or sqare
           # hc.method = "complete",
           type = "upper", # show the top half panel
           lab = TRUE, lab_col = "darkblue", lab_size = 2.5, # add correlation nbr, col and size of the correlation nbr
           title = "Immune markers correlation in TMA vs Intratumoral ROI",
           show.legend = TRUE, legend.title = "Correlation", show.diag = TRUE,
           # colors = viridis::inferno(n=3),
           # p.mat = pmat, # Add correlation significance
           sig.level = 0.05, insig = c("pch", "blank"), pch = 4, pch.col = "black", pch.cex = 10, 
           tl.cex = 10, tl.col = "red", tl.srt = 40,
           digits = 2
)
# dev.off()

# sqrt
mat <-
  cor(markers_28[, c("sqrt_CD3_total_tma", "sqrt_CD8_total_tma", "sqrt_CD3_CD8_total_tma", "sqrt_FoxP3_total_tma",
                     "sqrt_CD3_FoxP3_total_tma", "sqrt_CD11b_total_tma", "sqrt_CD15_total_tma", "sqrt_CD11b_CD15_total_tma",
                     "percent_CD3_total.i", "percent_CD8_total.i", "percent_CD3_CD8_total.i", "percent_FoxP3_total.i",
                     "percent_CD3_FoxP3_total.i", "percent_CD11b_total.i", "percent_CD15_total.i", "percent_CD11b_CD15_total.i")],
      use = "pairwise.complete.obs")
corrplot.mixed(mat)

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/Correlation .jpg"),
#      width = 1300, height = 800, quality = 100)
ggcorrplot(mat, hc.order = FALSE, method = "square", 
           # outline.col = "darkblue", # the outline of the circle or sqare
           # hc.method = "complete",
           type = "upper", # show the top half panel
           lab = TRUE, lab_col = "darkblue", lab_size = 2.5, # add correlation nbr, col and size of the correlation nbr
           title = "Immune markers correlation in TMA vs Intratumoral ROI",
           show.legend = TRUE, legend.title = "Correlation", show.diag = TRUE,
           # colors = viridis::inferno(n=3),
           # p.mat = pmat, # Add correlation significance
           sig.level = 0.05, insig = c("pch", "blank"), pch = 4, pch.col = "black", pch.cex = 10, 
           tl.cex = 10, tl.col = "red", tl.srt = 40,
           digits = 2
)
# dev.off()

# var
colnames(var_markers28)
mat <-
  cor(var_markers28[, c("var_CD3_tumor_tma", "var_CD8_tumor_tma",
                        "var_CD3_CD8_tumor_tma", "var_FoxP3_tumor_tma", "var_CD3_FoxP3_tumor_tma", "var_CD11b_tumor_tma",
                        "var_CD15_tumor_tma", "var_CD11b_CD15_tumor_tma", "var_CD3_stroma_tma", "var_CD8_stroma_tma",
                        "var_CD3_CD8_stroma_tma", "var_FoxP3_stroma_tma", "var_CD3_FoxP3_stroma_tma", "var_CD11b_stroma_tma",
                        "var_CD15_stroma_tma", "var_CD11b_CD15_stroma_tma", "var_CD3_total_tma", "var_CD8_total_tma",
                        "var_CD3_CD8_total_tma", "var_FoxP3_total_tma",
                        
                        "var_CD3_tumor.i", "var_CD8_tumor.i", "var_CD3_CD8_tumor.i",
                        "var_FoxP3_tumor.i", "var_CD3_FoxP3_tumor.i", "var_CD11b_tumor.i", "var_CD15_tumor.i",
                        "var_CD11b_CD15_tumor.i", "var_CD3_stroma.i", "var_CD8_stroma.i", "var_CD3_CD8_stroma.i",
                        "var_FoxP3_stroma.i", "var_CD3_FoxP3_stroma.i", "var_CD11b_stroma.i", "var_CD15_stroma.i",
                        "var_CD11b_CD15_stroma.i", "var_CD3_total.i", "var_CD8_total.i", "var_CD3_CD8_total.i",
                        "var_FoxP3_total.i")],
      use = "pairwise.complete.obs")
corrplot.mixed(mat)

# jpeg(paste0(path, "/Christelle Colin-Leitzinger/IF_AACES_NCOCS/Correlation .jpg"),
#      width = 1300, height = 800, quality = 100)
ggcorrplot(mat, hc.order = FALSE, method = "square", 
           type = "upper", # show the top half panel
           lab = TRUE, lab_col = "darkblue", lab_size = 2.5, # add correlation nbr, col and size of the correlation nbr
           title = "Immune markers correlation in TMA vs Intratumoral ROI",
           show.legend = TRUE, legend.title = "Correlation", show.diag = TRUE,
           sig.level = 0.05, insig = c("pch", "blank"), pch = 4, pch.col = "black", pch.cex = 10, 
           tl.cex = 10, tl.col = "red", tl.srt = 40,
           digits = 2
)
# dev.off()

# Cleaning
rm(p1, p2, global_28,
   mat, mat1, variations_TMA, variations_ROI, variations_ROIip, var_cor, table)
# End ----







