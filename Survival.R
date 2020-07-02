# Survival


#no event should be 0
# when event happened should be 1
mysurv <- Surv(time = clinical_data$timelastfu, event = clinical_data$surv_vital)
mysurv
median(clinical_data$timelastfu)# do not use this value as the median survival is tbe time at the survivalship aka function = .5

# Plot
# For whole population
# fit_KM <- survfit(mysurv~1, type= "kaplan-meier", conf.type = "log-log") # if log
#can do fleming-harrington or fh2 with log-log too

myplot <- survfit(mysurv~1)
myplot
plot(myplot)
plot(myplot, conf.int = "none") # without confidence interval
abline(h=0.5)
abline(v=) # Here put the median value in the my surv
# can get a restricted mean 
print(fit_KM,print.mean=TRUE)

# For black and white
myplot <- survfit(mysurv~clinical_data$race)
myplot
plot(myplot)
table(clinical_data$race)

plot(myplot, col= c("red", "blue"))

survdiff(mysurv~clinical_data$race)



#can plot cumhaz for cumulative hazard or event
plot(myplot, fun="cumhaz")
plot(myplot, fun="event")



