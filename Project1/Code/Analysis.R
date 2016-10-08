###########analysis of Project 1###################
load(file = '/Users/Caroline/Repositories/Data/Project1Data/analysis_ds.rda')

library(car) #need for qqplots
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
#fxn for diagnostic plots
diag_plots <- function(lm){
  qqPlot(lm)
  spreadLevelPlot(lm)
  hist_resid(lm)
}


#######primary question#######
#unadjusted model
summary(unadj_lm <- lm(analysis$PREFEV ~ analysis$TG_ref))
#p-value: 0.7486 no significant difference

#diagnostics
diag_plots(unadj_lm)#they look good

#adjust for baseline
summary(adj_lm <- lm(analysis$PREFEV ~ analysis$TG_ref + analysis$baselineFEV))
#p-value is <2.2e-16 

#diagnostics
diag_plots(adj_lm)#look good

#look at whether there is a different across treatment groups when adjusting for baseline
#using partial f test
reducedModel <- lm(analysis$PREFEV ~ analysis$baselineFEV)
anova(reducedModel,adj_lm)
#p-value 0.7246 - no significant difference

#adjust for other variables that differed across treatment group (from Table 1)
#only gender
summary(full_lm_gender <- lm(analysis$PREFEV ~ analysis$TG_ref + analysis$baselineFEV + analysis$GENDER))
diag_plots (full_lm_gender)

#look at whether there is a different across treatment groups when adjusting for baseline and gender
#using partial f test
reducedModel_gender <- lm(analysis$PREFEV ~ analysis$baselineFEV + analysis$GENDER)
adj_lm_gender <- anova(reducedModel_gender,full_lm_gender)
adj_lm_gender
#p-value 0.6322 - no significant difference

######sencondary question
#look at interaction of treatment and parent smoking

#full model
summary(secondary_full <- lm(analysis$PREFEV ~ analysis$TG_ref + analysis$everparent + analysis$TG_ref*analysis$everparent))
diag_plots(secondary_full)
#p-value - 0.961 diagplots look good.

#determine if interaction term is significant using partial f test
reducedSecondary <- lm(analysis$PREFEV ~ analysis$TG_ref + analysis$everparent)
secondary <- anova(reducedSecondary, secondary_full)
secondary
#p-value is 0.8133 the interaction term is not significant

######save workspace for use in final report rmd#####
save.image("~/Repositories/bios6623-ledbettc/Project1/Analysis.RData")
