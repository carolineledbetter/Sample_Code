#load dataset
load(file = '/Users/CarolineL/Repositories/Data/Project0Data/Project0cleandatanomissing.rda')

#attach data set so just variable names can be used 
attach(Project0_no_missing)

library(car)
#fxn for histogram of studentized residuals
hist_resid <- function(lm){
  require(MASS)
  sresid <- studres(lm) 
  hist(sresid, freq=FALSE, 
       main="Distribution of Studentized Residuals")
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


#Linear model using pocket depth at 1 year
lm1yrpd <- lm(pd1year~trtgroupFactored)
lm1yrpd_summary <-summary(lm1yrpd)

#adjust for baseline measurement
lm1yrpd_base <- lm(pd1year~trtgroupFactored + pdbase)
lm1yrpd_base_summary <- summary(lm1yrpd_base)

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

#adjusting for baseline and number of site
reduced_attach2 <- lm(attach1year ~ attachbase + sites)
full_attach2 <- lm(attach1year ~ attachbase + sites + trtgroupFactored) 
anova(reduced_attach2, full_attach2)
#p-value 0.09903 not significant

#adjusting for all covariates
reduced_attach_all <- lm(attach1year ~ attachbase + sex + age + smoker + 
                           RaceColl + sites)
full_attach_all <- lm(attach1year ~ attachbase + sex + age + smoker + 
                        RaceColl + sites + trtgroupFactored)
anova(reduced_attach_all, full_attach_all)
#not signifiant p=.1068

#and now for pocket depth
#adjusting for baseline measurement
lm1yrpd_baseonly <- lm(pd1year ~ pdbase)
anova(lm1yrpd_baseonly,lm1yrpd_base)#partial f test using anova
#p-value 0.112 not significant

#adjusting for baseline smoking status and gender
reduced_pd1 <- lm(pd1year ~ pdbase + sex + smoker)
full_pd1 <- lm(pd1year ~ pdbase + sex + smoker + trtgroupFactored)
anova(reduced_pd1,full_pd1)
#p-value 0.126 not significant

#adjusting for baseline and number of site
reduced_pd2 <- lm(pd1year ~ pdbase + sites)
full_pd2 <- lm(pd1year ~ pdbase + sites + trtgroupFactored) 
anova(reduced_pd2, full_pd2)
#p-value 0.1055 not significant

#adjusting for all covariates
reduced_pd_all <- lm(pd1year ~ pdbase + sex + age + smoker + 
                           RaceColl + sites)
full_pd_all <- lm(pd1year ~ pdbase + sex + age + smoker + 
                        RaceColl + sites + trtgroupFactored)
anova(reduced_pd_all, full_pd_all)
#not signifiant p=.1057