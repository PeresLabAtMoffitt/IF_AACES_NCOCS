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
markers_ROIi <- markers_ROIi %>% 
  left_join(cases_match, ., by = "suid") %>% 
  drop_na(.) %>% group_by(pair_id) %>% filter( n() > 1 )

markers_ROIp <- markers_ROIp %>% 
  full_join(cases_match, ., by = "suid") %>% 
  drop_na(.) %>% group_by(pair_id) %>% filter( n() > 1 )

# plots----
plot(markers_ROIi)
ggplot(markers_ROIi, aes(x=race, y=percent_CD3_tumor)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x="Race", y="CD3 tumor", title="CD3 in Race") + geom_jitter(shape=16, position=position_jitter(0.2))

library(ggpubr)
ggpaired(markers_ROIi, x = "race", y = "percent_CD3_tumor",
         id = "pair_id",
         color = "race", line.color = "gray", line.size = 0.4,
         palette = "jco")+ # depends of the journal to publish on
  stat_compare_means(paired = TRUE)

ggpaired(markers_ROIi, x = "race", y = "percent_FoxP3_tumor",
         id = "pair_id",
         color = "race", line.color = "gray", line.size = 0.4,
         palette = "jco")+
  stat_compare_means(paired = TRUE)





markers_ROIi %>% 
  gather(key = "race", value = c("percent_CD3_tumor", "percent_FoxP3_tumor"))
ggpaired(markers_ROIi, x = "race", y = "percent_CD3_tumor",
         id = "pair_id",
         color = "race", line.color = "gray", line.size = 0.4,
         palette = "jco",
         facet.by = "marker", short.panel.labs = FALSE)+
  stat_compare_means(paired = TRUE)










# heatmaps----same as before


