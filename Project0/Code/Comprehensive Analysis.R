#load dataset
load(file = '/Users/Caroline/Repositories/Data/Project0Data/Project0cleandatanomissing.rda')

#attach data set so just variable names can be used 
attach(Project0_no_missing)

library(car)
#fxn for histogram of studentized residuals
hist_resid <- function(lm){
  require(MASS)
  sresid <- studres(lm) 
  hist(sresid, freq=FALSE, 
       main="Distribution of Studentized Residuals", xlab = "Residuals")
  xfit<-seq(min(sresid),max(sresid),length=40) 
  yfit<-dnorm(xfit) 
  lines(xfit, yfit)
}
logattach <- log(attach1year)
lmlogattach <- lm(logattach~trtgroupFactored)
summary(lmlogattach)#p-value 0.01072

#adjust for baseline measurement
lmlogattach_base <- lm(logattach~trtgroupFactored+attachbase)
qqPlot(lmlogattach_base)
spreadLevelPlot(lmlogattach_base)
hist_resid(lmlogattach_base)

#determine if there is a difference across trt groups when 
#adjusting for baseline measurement
#for attachment loss
lm1yrattach_baseonly <- lm(attach1year ~ attachbase)
qqPlot(lm1yrattach_baseonly)
spreadLevelPlot(lm1yrattach_baseonly)
hist_resid(lm1yrattach_baseonly)
anova(lm1yrattach_baseonly,lm1yrattach_base)#partial f test using anova
#p-value 0.09847 not significant

#adjusting for baseline smoking status and gender
reduced_attach1 <- lm(attach1year ~ attachbase + sex + smoker)
full_attach1 <- lm(attach1year ~ attachbase + sex + smoker + trtgroupFactored)
anova(reduced_attach1,full_attach1)
#p-value 0.1123 not significant

#adjusting for baseline and number of sites
reduced_attach2 <- lm(attach1year ~ attachbase + sites)
full_attach2 <- lm(attach1year ~ attachbase + sites + trtgroupFactored) 
anova(reduced_attach2, full_attach2)
#p-value 0.09903 not significant

#adjusting for sites sex smoking status and baseline
reduced_attach_final <- lm(attach1year ~ attachbase + sex +  smoker + sites)
full_attach_final <- lm(attach1year ~ attachbase + sex +  smoker + sites + trtgroupFactored)
anova(reduced_attach_final, full_attach_final)
#not signifiant p=.1111

#adjusting for all covariates
reduced_attach_all <- lm(attach1year ~ attachbase + sex + age + smoker + 
                           RaceColl + sites)
full_attach_all <- lm(attach1year ~ attachbase + sex + age + smoker + 
                        RaceColl + sites + trtgroupFactored)
anova(reduced_attach_all, full_attach_all)
#not signifiant p=.1068


#and now for pocket depth
#Linear model using pocket depth at 1 year
lm1yrpd <- lm(pd1year~trtgroupFactored)
qqPlot(lm1yrpd)
spreadLevelPlot(lm1yrpd)
hist_resid(lm1yrpd)
summary(lm1yrpd)#p-value: .3556

#log transform pocket depth outcome
logpd <- log(pd1year)
lmlogpd <- lm(logpd~trtgroupFactored)
qqPlot(lmlogpd)
spreadLevelPlot(lmlogpd)
hist_resid(lmlogpd)
summary(lmlogpd)#p-value 0.3322

#adjust for baseline measurement
lm1yrpd_base <- lm(pd1year~trtgroupFactored + pdbase)
qqPlot(lm1yrpd_base)
spreadLevelPlot(lm1yrpd_base)
hist_resid(lm1yrpd_base)
lm1yrpd_baseonly <- lm(pd1year ~ pdbase)
anova(lm1yrpd_baseonly,lm1yrpd_base)#partial f test using anova
#p-value 0.112 not significant

#look at covariates effect on log transformed outcome
summary(lmpdsex <- lm(logpd~sex))#p-value: 0.2027
summary(lmpdrace <- lm(logpd ~ race))# p-value: 0.1104
summary(lmpdage <- lm(logpd ~ age))#p-value:0.2387
summary(lmpdsmoker <- lm(logpd ~ smoker))#p-value: 0.02122
summary(lmpdsites <- lm(logpd ~ sites))# p_value: 0.08656


#adjusting for baseline, number of sites, and smoking status
reduced_pd2 <- lm(pd1year ~ pdbase + sites + smoker)
full_pd2 <- lm(pd1year ~ pdbase + sites + smoker + trtgroupFactored) 
anova(reduced_pd2, full_pd2)
#p-value 0.1046 not significant

#adjusting for all covariates
reduced_pd_all <- lm(pd1year ~ pdbase + sex + age + smoker + 
                           RaceColl + sites)
full_pd_all <- lm(pd1year ~ pdbase + sex + age + smoker + 
                        RaceColl + sites + trtgroupFactored)
anova(reduced_pd_all, full_pd_all)
#not signifiant p=.1057 not significant