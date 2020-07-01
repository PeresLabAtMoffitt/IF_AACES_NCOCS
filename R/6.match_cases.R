########################################################################################## I ### Create ROI Match Cases----

cases_match <- cases_match %>% mutate(suid = as.character(suid))
cases_match <- left_join(cases_match, 
                          clinical_data %>% select("suid", "race"),
                          by= "suid")
  

cases_match1 <- dcast(setDT(cases_match), pair_id ~ rowid(pair_id),
                      value.var = c("suid", "race")) %>%
  drop_na("race_1", "race_2") # We all have the matching


cases_match <-  left_join(cases_match, # innerjoin for only patient who has a match and ROIs give no white for comparison
                     markers_ROIi,
                     by= "suid")

# Cleanup both paired ID when one had a missing data----
cases_match <-  cases_match %>% drop_na(.) %>% group_by(pair_id) %>% filter( n() > 1 )


########################################################################################## II ### Plot Matched Cases----

plot(cases_match)
ggplot(cases_match, aes(x=race, y=percent_CD3_tumor)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x="Race", y="CD3 tumor", title="CD3 in Race") + geom_jitter(shape=16, position=position_jitter(0.2))

library(ggpubr)
ggpaired(cases_match, x = "race", y = "percent_CD3_tumor",
         color = "race", line.color = "gray", line.size = 0.4,
         palette = "jco")+
  stat_compare_means(paired = TRUE)

ggpaired(cases_match, x = "race", y = "percent_FoxP3_tumor",
         color = "race", line.color = "gray", line.size = 0.4,
         palette = "jco")+
  stat_compare_means(paired = TRUE)

# library(gplots)
# library(heatmap.plus)
# library(RColorBrewer)
# df <- markers_ROI %>% 
#   `rownames<-`(markers_ROI[,1]) %>% 
#   select(-c("suid", "pair_id", "race", "ID")) %>% 
#   drop_na("percent_CD3_tumor")
# df <- as.matrix(df)
# heatmap.2(df, main = "Immune Marker Presentation",
#           
#           trace = "none", density="none", col=bluered(20), cexRow=1, cexCol = 1, 
#           margins = c(10,5), # bottom, right
#           ColSideColors = ,
#           scale = "row")
# 
# df1 <- t(scale(t(df)))
# heatmap.2(df1, main = "Immune Marker Presentation",
#           
#           trace = "none", density="none", col=bluered(20), cexRow=1, cexCol = 1, 
#           margins = c(10,5), # bottom, right
#           ColSideColors = ,
#           scale = "row")

########################################################################################## III ### ----











