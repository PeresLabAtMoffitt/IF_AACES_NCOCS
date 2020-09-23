##################################################################### I ### Immune marker B/W



markers_match %>%
  gather(key = "markers", value = "value", 
         c("percent_CD3_tumor.i", "percent_CD3_total.i", "percent_CD3_stroma.i")) %>%
  ggplot(aes(x=race, y=value))+
  geom_violin()+
  facet_grid(. ~ markers)

markers_match %>%
  gather(key = "markers", value = "value", 
         c("percent_CD11b_CD15_tumor.i", "percent_CD11b_CD15_total.i", "percent_CD11b_CD15_stroma.i")) %>%
  ggplot(aes(x=race, y=value))+
  geom_violin()+
  facet_grid(. ~ markers)



# Paired plot
markers_match %>%
  gather(key = "markers", value = "value", 
         c("percent_CD3_tumor.i", "percent_CD3_total.i", "percent_CD3_stroma.i")) %>% 
  select("markers", "value", "race") %>% 
  ggpaired(x = "race", y = "value",
           color = "markers", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = TRUE)+
  facet_grid(. ~ markers)

markers_match %>%
  gather(key = "markers", value = "value", 
         c("percent_CD3_CD8_tumor.i", "percent_CD3_CD8_total.i", "percent_CD3_CD8_stroma.i")) %>% 
  select("markers", "value", "race") %>% 
  ggpaired(x = "race", y = "value",
           color = "markers", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = TRUE)+
  facet_grid(. ~ markers)

markers_match %>%
  gather(key = "markers", value = "value", 
         c("percent_FoxP3_tumor.i", "percent_FoxP3_total.i", "percent_FoxP3_stroma.i")) %>% 
  select("markers", "value", "race") %>% 
  ggpaired(x = "race", y = "value",
           color = "markers", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = TRUE)+
  facet_grid(. ~ markers)

markers_match %>%
  gather(key = "markers", value = "value", 
         c("percent_CD11b_tumor.i", "percent_CD11b_total.i", "percent_CD11b_stroma.i")) %>% 
  select("markers", "value", "race") %>% 
  ggpaired(x = "race", y = "value",
           color = "markers", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = TRUE)+
  facet_grid(. ~ markers)

markers_match %>%
  gather(key = "markers", value = "value", 
         c("percent_CD15_tumor.i", "percent_CD15_total.i", "percent_CD15_stroma.i")) %>% 
  select("markers", "value", "race") %>% 
  ggpaired(x = "race", y = "value",
           color = "markers", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = TRUE)+
  facet_grid(. ~ markers)

markers_match %>%
  gather(key = "markers", value = "value", 
         c("percent_CD11b_CD15_tumor.i", "percent_CD11b_CD15_total.i", "percent_CD11b_CD15_stroma.i")) %>% 
  select("markers", "value", "race") %>% 
  ggpaired(x = "race", y = "value",
           color = "markers", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = TRUE)+
  facet_grid(. ~ markers)







markers_match %>%
  gather(key = "markers", value = "value", 
         c("percent_CD11b_CD15_tumor.i", "percent_CD11b_CD15_total.i", "percent_CD11b_CD15_stroma.i")) %>% 
  select("markers", "value") %>% 
  ggpaired(x = "markers", y = "value",
           color = "markers", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = TRUE)

p1 <- markers_match %>% 
  ggpaired(x = "race", y = "percent_CD3_tumor.i",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = TRUE)+
  labs(x=NULL, y="percent_CD3_tumor.i")
p2 <- markers_match %>% 
  ggpaired(x = "race", y = "percent_CD3_total.i",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = TRUE)+
  labs(x=NULL, y="percent_CD3_total.i")
p3 <- markers_match %>% 
  ggpaired(x = "race", y = "percent_CD3_stroma.i",
           color = "race", line.color = "gray", line.size = 0.4)+
  stat_compare_means(paired = TRUE)+
  labs(x=NULL, y="percent_CD3_stroma.i")
gridExtra::grid.arrange(p1,p2,p3, ncol=3)




