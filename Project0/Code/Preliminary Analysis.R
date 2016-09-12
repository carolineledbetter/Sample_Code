#load dataset
load(file = '/Users/CarolineL/Repositories/Data/Project0Data/Project0cleandata.rda')

#attach data set so just variable names can be used 
attach(Project0_dental_data)

#Linear model using attachment at 1 year
lm1yrattach <-lm(attach1year~trtgroupFactored)
summary(lm1yrattach)
plot(lm1yrattach)

#adjust for baseline measurement
lm1yrattach_base <- lm(attach1year~trtgroupFactored+attachbase)
summary(lm1yrattach_base)

#Linear model using pocket depth at 1 year
lm1yrpd<-lm(pd1year~trtgroupFactored)
summary(lm1yrpd)
plot(lm1yrpd)

#adjust for baseline measurement
lm1yrpd_base <- lm(pd1year~trtgroupFactored + pdbase)
summary(lm1yrpd_base)

#look at covariates effect on outcome
summary(lmattachsex <- lm(attach1year~sex))
summary(lmpdsex <- lm(pd1year~sex))
summary(lmattachrace <- lm(attach1year ~ race))
summary(lmpdrace <- lm(pd1year ~ race))
summary(lmattachage <- lm(attach1year ~ age))
summary(lmpdage <- lm(pd1year ~ age))
summary(lmattachsmoker <- lm(attach1year ~ smoker))
summary(lmpdsmoker <- lm(pd1year ~ smoker))
summary(lmattachsites <- lm(attach1year ~ sites))
summary(lmpdsites <- lm(pd1year ~ sites))

#look at model that adjusts for sex (attachment), sites, and smoking status
summary(lm1yrattach_adj <- lm(attach1year~trtgroupFactored+attachbase+sex+sites+smoker))
summary(lm1yrpd_adj <- lm(pd1year ~ trtgroupFactored + pdbase + sites + smoker))
