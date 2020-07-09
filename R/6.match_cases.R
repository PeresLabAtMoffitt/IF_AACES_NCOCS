# ########################################################################################## I ### Create ROI Match Cases----
# 
# cases_match <- cases_match %>% mutate(suid = as.character(suid))
# cases_match <- left_join(cases_match, 
#                           clinical_data %>% select("suid", "race"),
#                           by= "suid")
#   
# 
# cases_match1 <- dcast(setDT(cases_match), pair_id ~ rowid(pair_id),
#                       value.var = c("suid", "race")) %>%
#   drop_na("race_1", "race_2") # We all have the matching
# 
# 
# cases_match <-  left_join(cases_match, # innerjoin for only patient who has a match and ROIs give no white for comparison
#                      markers_ROIi,
#                      by= "suid")
# 
# # Cleanup both paired ID when one had a missing data----
# cases_match$suid[is.na(cases_match$percent_CD3_tumor)]
# cases_match2 <-  cases_match %>% drop_na(.) %>% group_by(pair_id) %>% filter( n() > 1 )


########################################################################################## II ### Plot Matched Cases----
# Black vs White
markers_ROI_CM <- markers_ROI %>% 
  left_join(cases_match, ., by = "suid") %>% 
  drop_na(percent_CD3_tumor.i) %>% group_by(pair_id) %>% filter( n() > 1 )

# markers_ROIp <- markers_ROIp %>% 
#   full_join(cases_match, ., by = "suid") %>% 
#   drop_na(.) %>% group_by(pair_id) %>% filter( n() > 1 )

# plots----
# plot(markers_ROI_CM)
ggplot(markers_ROI_CM, aes(x=race, y=percent_CD3_tumor.i)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x="Race", y="CD3 tumor", title="CD3 in Race") + geom_jitter(shape=16, position=position_jitter(0.2))

library(ggpubr)
ggpaired(markers_ROI_CM, x = "race", y = "percent_CD3_tumor.i",
         id = "pair_id",
         color = "race", line.color = "gray", line.size = 0.4,
         palette = "jco")+ # depends of the journal to publish on
  stat_compare_means(paired = TRUE)

ggpaired(markers_ROI_CM, x = "race", y = "percent_FoxP3_tumor.i",
         id = "pair_id",
         color = "race", line.color = "gray", line.size = 0.4,
         palette = "jco")+
  stat_compare_means(paired = TRUE)





markers_ROI_CM %>% 
  gather(key = "race", value = c("percent_CD3_tumor.i", "percent_FoxP3_tumor.i"))
ggpaired(markers_ROI_CM, x = "race", y = "percent_CD3_tumor",
         id = "pair_id",
         color = "race", line.color = "gray", line.size = 0.4,
         palette = "jco",
         facet.by = "marker", short.panel.labs = FALSE)+
  stat_compare_means(paired = TRUE)




# Ratio on case_matched
ggplot(markers_ROI_CM, aes(x=percent_CD3_tumor.p, y=percent_CD3_tumor.i))+
  geom_point()
ggplot(markers_ROI_CM, aes(x=percent_CD8_tumor.p, y=percent_CD8_tumor.i))+
  geom_point()
ggplot(markers_ROI_CM, aes(x=percent_FoxP3_tumor.p, y=percent_FoxP3_tumor.i))+
  geom_point()
ggplot(markers_ROI_CM, aes(x=percent_CD11b_tumor.p, y=percent_CD11b_tumor.i))+
  geom_point()
ggplot(markers_ROI_CM, aes(x=percent_CD15_tumor.p, y=percent_CD15_tumor.i))+
  geom_point()




# heatmaps----same as before


