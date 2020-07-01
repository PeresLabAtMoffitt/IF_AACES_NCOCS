# Start with clinical mining


# Proportion cancer characteristic between Black and White
clinical_data %>% 
  group_by(cancersite, race) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=cancersite, y=count, fill=race))+
  geom_bar(stat = "identity", position=position_dodge2(preserve = "single"))+
  theme_minimal()
 
clinical_data %>% 
  group_by(histology, race) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=histology, y=count, fill=race))+
  geom_bar(stat = "identity", position=position_dodge2(preserve = "single"))+
  theme_minimal()+
  coord_flip()

# clinical_data %>%  # behavior doesn't seem relavant as almost all are invasive
#   group_by(behavior, race) %>% 
#   summarise(count=n()) %>% 
#   ggplot(aes(x=behavior, y=count, fill=race))+
#   geom_bar(stat = "identity", position=position_dodge2(preserve = "single"))+
#   theme_minimal()

clinical_data %>% 
  group_by(grade, race) %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x=grade, y=count, fill=race))+
  geom_bar(stat = "identity", position=position_dodge2(preserve = "single"))+
  theme_minimal()+
  coord_flip()

# clinical_data %>%  # histotype doesn't seem relavant as almost all are high grade serous
#   group_by(histotype, race) %>% 
#   summarise(count=n()) %>% 
#   ggplot(aes(x=histotype, y=count, fill=race))+
#   geom_bar(stat = "identity", position=position_dodge2(preserve = "single"))+
#   theme_minimal()+
#   coord_flip()

clinical_data %>% 
  group_by(education, race) %>% 
  summarise(count=n()) %>% 
  mutate(percent=(count/sum(count)*100)) %>% 
  ggplot(aes(x=education, y=percent, fill=race))+
  geom_bar(stat = "identity", position=position_dodge2(preserve = "single"))+
  theme_minimal()+
  coord_flip()

clinical_data %>% 
  group_by(married, race) %>% 
  summarise(count=n()) %>% 
  mutate(percent=(count/sum(count)*100)) %>% 
  ggplot(aes(x=married, y=percent, fill=race))+
  geom_bar(stat = "identity", position=position_dodge2(preserve = "single"))+
  theme_minimal()+
  coord_flip()

clinical_data %>% 
  group_by(pregever, race) %>% 
  summarise(count=n()) %>% 
  mutate(percent=(count/sum(count)*100)) %>% 
  ggplot(aes(x=pregever, y=percent, fill=race))+
  geom_bar(stat = "identity", position=position_dodge2(preserve = "single"))+
  theme_minimal()+
  coord_flip()

clinical_data %>% 
  ggplot(aes(BMI_recent, color=race))+
  geom_freqpoly()+
  theme_minimal()
clinical_data %>% 
  ggplot(aes(BMI_YA, color=race))+
  geom_freqpoly()+
  theme_minimal()
clinical_data %>% 
  mutate(BMI_fold_increase = BMI_recent-BMI_YA) %>% 
  ggplot(aes(BMI_fold_increase, color=race))+
  geom_freqpoly()+
  theme_minimal()

# Age at diag between black and white
ggplot(clinical_data, aes(x=race, y=refage, color=race))+
  geom_violin(scale = "count") # choose count to refect areas are scaled proportionally to the number of observations.

clinical_data$range_BMI <- as.factor(findInterval(clinical_data$BMI_recent,c(0,25,30,35,40,45,50,55,60,65,70,75,80)))
levels(clinical_data$range_BMI) <-  
  c("<26","26-30","31-35","36-40","41-45","46-50","51-55","56-60","61-65","65-70", "70-75", 
    "75-80", ">80")
clinical_data %>% 
  group_by(range_BMI, race) %>% 
  summarise(count=n()) %>% 
  mutate(percent=(count/sum(count)*100)) %>% 
  ggplot(aes(x=range_BMI, y=percent, fill=race))+
  geom_bar(stat = "identity", position=position_dodge2(preserve = "single"))+
  theme_minimal()+
  coord_flip()





