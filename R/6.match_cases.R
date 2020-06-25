# Match cases

cases_match <- cases_match %>% mutate(suid = factor(suid))
cases_match <- left_join(cases_match, 
                          clinical_data %>% select("suid", "race"),
                          by= "suid")
  

cases_match1 <- dcast(setDT(cases_match), pair_id ~ rowid(pair_id),
                      value.var = c("suid", "race")) %>%
  drop_na("race_1", "race_2") # We all have the matching


markers_ROI <-  left_join(cases_match, # innerjoin for only patient who has a match and ROIs give no white for comparison
                     markers_ROIip %>% filter(intratumoral_i_vs_peripheral_p_ == "Intratumoral"),
                     by= "suid") %>% 
  select(-intratumoral_i_vs_peripheral_p_)
markers_ROI <- markers_ROI %>% mutate(suid = factor(suid))
plot(markers_ROI)
ggplot(markers_ROI, aes(x=race, y=percent_CD3_tumor)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x="Race", y="CD3 tumor", title="CD3 in Race") + geom_jitter(shape=16, position=position_jitter(0.2))


library(gplots)
library(heatmap.plus)
library(RColorBrewer)
df <- markers_ROI %>% 
  `rownames<-`(markers_ROI[,1]) %>% 
  select(-c("suid", "pair_id", "race", "ID")) %>% 
  drop_na("percent_CD3_tumor")
df <- as.matrix(df)
heatmap.2(df, main = "Immune Marker Presentation",
          
          trace = "none", density="none", col=bluered(20), cexRow=1, cexCol = 1, 
          margins = c(10,5), # bottom, right
          ColSideColors = ,
          scale = "row")

df1 <- t(scale(t(df)))
heatmap.2(df1, main = "Immune Marker Presentation",
          
          trace = "none", density="none", col=bluered(20), cexRow=1, cexCol = 1, 
          margins = c(10,5), # bottom, right
          ColSideColors = ,
          scale = "row")
