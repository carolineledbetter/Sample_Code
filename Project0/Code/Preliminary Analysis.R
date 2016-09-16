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
       main="Distribution of Studentized Residuals")
  xfit<-seq(min(sresid),max(sresid),length=40) 
  yfit<-dnorm(xfit) 
  lines(xfit, yfit)
}

#Linear model using attachment at 1 year
lm1yrattach <-lm(attach1year~trtgroupFactored)
lm1yrattach_summary <- summary(lm1yrattach) #p = 0.04837
qqPlot(lm1yrattach)
spreadLevelPlot(lm1yrattach)
hist_resid(lm1yrattach)
#residual vs fitted and Q-Q plot look bad. 

#try log transformation
logattach <- log(attach1year)
lmlogattach <- lm(logattach~trtgroupFactored)
qqPlot(lmlogattach) #a little better
spreadLevelPlot(lmlogattach) #a little better
hist_resid(lmlogattach)# alot better
summary(lmlogattach)#p = 0.01072
 
#adjust for baseline measurement
lm1yrattach_base <- lm(attach1year~trtgroupFactored+attachbase)
lm1yrattach_base_summary <- summary(lm1yrattach_base)
plot(lm1yrattach_base)

lmlogattach_base <- lm(logattach~trtgroupFactored+attachbase)
lmlogattach_base_summary <- summary(lmlogattach_base)
plot(lmlogattach_base)

#Linear model using pocket depth at 1 year
lm1yrpd <- lm(pd1year~trtgroupFactored)
lm1yrpd_summary <-summary(lm1yrpd)
plot(lm1yrpd)

#adjust for baseline measurement
lm1yrpd_base <- lm(pd1year~trtgroupFactored + pdbase)
lm1yrpd_base_summary <- summary(lm1yrpd_base)
plot(lm1yrpd_base)

#look at covariates effect on outcome
summary(lmattachsex <- lm(logattach~sex))#p-value:0.04242
qqPlot(lmattachsex)
hist_resid(lmattachsex)

summary(lmattachrace <- lm(logattach ~ RaceColl))#p-value:0.9975
summary(lmattachage <- lm(logattach ~ age))#p-value: 0.3013
summary(lmattachsmoker <- lm(logattach ~ smoker))#p-value:0.08167
summary(lmattachsites <- lm(attach1year ~ sites))#p-value: 2.76e-05

summary(lmpdsex <- lm(pd1year~sex))
summary(lmpdrace <- lm(pd1year ~ race))
summary(lmpdage <- lm(pd1year ~ age))
summary(lmpdsmoker <- lm(pd1year ~ smoker))
summary(lmpdsites <- lm(pd1year ~ sites))

