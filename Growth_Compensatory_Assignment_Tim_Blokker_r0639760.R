library(pastecs)    #function stat.desc
library(lattice)    #function xyplot
library(lme4)       #package for fitting the LLM
library(lattice)    # function for the trellis graph
library(arm)
library(car)
library(grid)
library(corrplot)   #correlation plot
library(nlme)       #function lme()
library(dplyr)
library(ggplot2)
library(tidyr)
library(pbkrtest) #get_ddf_Lb() function
library(lmerTest)


#Importing the data set
growth.df<-read.table(file="Project/growth.txt", header=T)
head(growth.df,5)

#Transforming the data set
SEX<-ifelse(growth.df$SEX==1,0,1)
growth_new<-cbind(growth.df, SEX)
growth_new<-growth_new[-4]
GENDER<-ifelse(growth_new$SEX==1,"Female","Male")
growth_new<-cbind(growth_new, GENDER)
head(growth_new,5)

##Descriptives
#Descriptives 1: Overview
count(growth_new, GENDER, AGE)

#Descriptives 2: mean, sd, var, n()
descrip.growth<-growth_new%>%
  group_by(AGE,SEX)%>%
  summarise(mean=mean(MEASURE), sd=sd(MEASURE), var=var(MEASURE), Frequency=n())
head(descrip.growth,8)

#Descriptives 3: Spaghetti Plot of Individuals
#interaction.plot(growth_new$AGE, growth_new$IDNR , growth_new$MEASURE, xlab="Age in years", ylab="Distance in mm", legend=F)
plot <- ggplot(data = growth_new, aes(x = AGE, y = MEASURE, group = IDNR, colour = factor(SEX)))+xlab("Age in years")+ ylab("Distance in mm")
plot2<- plot + geom_line()   +scale_color_manual(values=c("blue", "red")) + labs(col="Gender")
plot3<-plot2 + theme_classic()+geom_text(col="black",data=subset(growth_new, IDNR %in% c(20,24)), aes(x = AGE, y = MEASURE, group = IDNR, colour = factor(SEX), label=IDNR))
plot3

#Descriptives 3.5: Boxplots (not in report)
grid<-matrix(c(1,1,2,3), nrow = 2, ncol = 2, byrow=T)
layout(grid)
boxplot(MEASURE~AGE, data=growth_new, main = "Male+Female", xlab="Age in years", ylab="Measure in mm" )
boxplot(MEASURE[SEX==0]~AGE[SEX==0], main = "Male", data=growth_new, ylim=c(18, 34), xlab="Age in years", ylab="Measure in mm" )
boxplot(MEASURE[SEX==1]~AGE[SEX==1], main = "Female", data=growth_new, ylim=c(18, 34), xlab="Age in years", ylab="Measure in mm" )

#Descriptives 4: Mean evolution
#Error bars
errbar<-function(x,y,height,width,lty=1,col="black"){
  arrows(x,y,x,y+height,angle=90,length=width,lty=lty,
         col=col)
  arrows(x,y,x,y-height,angle=90,length=width,lty=lty,
         col=col)
}

##Plotting mean evolutions
par(mfrow=c(1,1))
plot(growth_new$AGE[growth_new$IDNR==1], descrip.growth$mean[c(1,3,5,7)],type="b",xlim=c(8,14), ylim=c(15,30),col="blue",xlab="Age (in years)",ylab="Measure (in mm)",axes=F, main="Mean evolution (with 1 SE intervals)")
##take the x values from any subject plus mean y values from one gender
axis(side=1,at=c(8, 10, 12, 14),labels=c(8, 10, 12, 14))
axis(side=2,at=seq(15,30,5))
box()
points(growth_new$AGE[growth_new$IDNR==1],descrip.growth$mean[c(2,4,6,8)],type="b",col="red")
errbar(growth_new$AGE[growth_new$IDNR==1]-.005,descrip.growth$mean[c(1,3,5,7)],descrip.growth$sd[c(1,3,5,7)],.1, col="blue")
errbar(growth_new$AGE[growth_new$IDNR==1]+.005,descrip.growth$mean[c(2,4,6,8)],descrip.growth$sd[c(2,4,6,8)],.1,col="red")
#adding and substracting small values from the x values to have both groups visible
legend(12.5, 18, legend= c("Male", "Female"), col=c("blue", "red"), lty=1, cex=0.75)

##Correlation
## Reshaping the data into a wide form
growth_new_wide<- growth_new %>%
  mutate(AGE=paste0('Age_' , AGE))%>%
  spread(AGE, MEASURE)
head(growth_new_wide,5)
##growth_new2 <- reshape(growth_new, timevar = "AGE", idvar = c("IDNR", "SEX"), drop= c("INDIV", ##"GENDER"),direction = "wide")
##growth_new2

#Correlation between the Measurements at different ages
col.order <- c("Age_8","Age_10","Age_12","Age_14")
cor<-cor(growth_new_wide[,col.order], method = "pearson")
cor
corrplot(cor, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt =30)

##Level1 - Changes within individuals
#Creating the time variable
growth_AGE0<-growth_new
growth_AGE0$AGE0<-growth_new$AGE-8
#Displaying the linear regression per person
OLS<-sapply(growth_AGE0$IDNR, function(x) coef(lm(MEASURE~AGE0, data=subset(growth_AGE0, IDNR==x)))) ##linear regression for each IDNR
#Order by intercept
OLS_INT<-reorder(growth_AGE0$IDNR, OLS[1,]) ##order by intercept
#Trellis graph
xyplot(MEASURE ~ AGE0|OLS_INT,groups=SEX,data=growth_AGE0, type=c('p', 'r'),xlab="Age (in years)", ylab="Distance (in mm)",auto.key=T,aspect="xy", par.settings=list(axis.text=list(cex=1.25), fontsize=list(text=10, points=10)), scales=list( x=list( at=c(0,2,4,6), labels=c("0","2","4", "6"))))
#Linear regression per participant of distance on age
#R squared
lin.reg.r.squared <- by(growth_AGE0, growth_AGE0$IDNR,function(x) summary(lm(MEASURE ~ AGE, data=x))$r.squared )
lin.reg.r.squared1<- as.vector(unlist(lin.reg.r.squared))
hist(lin.reg.r.squared1,xlab="R squared",col="lightblue",main="Histogram of individual R squared", cex.lab=1.5, cex.main=1.5)

#Level2 - Changes between individuals
#Histogram for intercepts
hist(OLS[1,],xlab="Intercept",col="lightblue",breaks=6,main="Histogram of individual intercepts",  cex.lab=1.5, cex.main=1.5)

#Histogram for slopes
hist(OLS[2,],xlab="Slope",col="lightblue", breaks = 6, main="Histogram of individual slopes",  cex.lab=1.5, cex.main=1.5)

#Individual Regression lines per group 
attach(growth_AGE0)
lin.reg.coef <- by(growth_AGE0, growth_AGE0$IDNR, function(data) coef(lm(MEASURE ~ AGE0, data=data)))
lin.reg.coef1 <- unlist(lin.reg.coef)
names(lin.reg.coef1) <- NULL
lin.reg.coef2<-matrix(lin.reg.coef1,length(lin.reg.coef1)/2,2, byrow = TRUE)
reg.coef<-cbind(lin.reg.coef2, growth_AGE0[AGE0==0,]$SEX)
mean.int<-tapply(reg.coef[,1],reg.coef[,3],mean)
mean.slope<-tapply(reg.coef[,2],reg.coef[,3],mean)
par(mfrow=c(1,2))
plot(AGE,MEASURE,type="n",xlim=c(0,6),ylim=c(15,35),main="Male", xlab="Age-8 (in years)",ylab="Distane (in mm)",axes=F)
axis(side=1,at=c(0,2,4,6),labels=c(0,2,4, 6))
axis(side=2,at=seq(15,35,5))
box()
for (i in 1:27){if (reg.coef[i,3]==0) curve(cbind(1,x)%*%reg.coef[i,1:2],add=T,col="gray")}
curve(cbind(1,x)%*%c(mean.int[1],mean.slope[1]),add=T,lwd=2)
plot(AGE,MEASURE,type="n",xlim=c(0,6),ylim=c(15,35),main="Female", xlab="Age-8 (in years)",ylab="Distance (in mm)",axes=F)
axis(side=1,at=c(0,2,4,6),labels=c(0,2,4, 6))
axis(side=2,at=seq(15,35,5))
box()
for (i in 1:27) {if (reg.coef[i,3]==1) curve(cbind(1,x)%*%reg.coef[i,1:2],add=T,col="gray")}
curve(cbind(1,x)%*%c(mean.int[2],mean.slope[2]),add=T,lwd=2)

##Fixed effects
growth.lmer1<-lmer(MEASURE~1+AGE0*SEX+(1|IDNR)+(0 + AGE0|IDNR), REML = FALSE, data=growth_AGE0)
fixef(growth.lmer1)

##Confidence intervals
#Calculating confidence intervals for the fixed effects via Wald
confint(growth.lmer1,par=4:7,method="Wald",oldNames = FALSE) 

##new package giving p-values for lmer
anova(as_lmerModLmerTest(growth.lmer1))

##Get the KR-approximated degrees of freedom
growth.lmer1.df.KR <- get_ddf_Lb(growth.lmer1, fixef(growth.lmer1))
## Get p-values from the t-distribution using the t-values and approximated degrees of freedom
growth.lmer1.coef<-coef(summary(growth.lmer1))
growth.lmer1.p.KR <- cbind(growth.lmer1.coef,2 * (1 - pt(abs(growth.lmer1.coef[,3]), growth.lmer1.df.KR)))
growth.lmer1.p.KR

##Comparing different fixed effects models
#Likelihood ratio tests
growth.lmer1.nosex<-lmer(MEASURE~1+AGE0+(1 + AGE0|IDNR), REML = FALSE, data=growth_AGE0)
growth.lmer1.intsex<-lmer(MEASURE~1+AGE0+SEX+(1 +AGE0|IDNR), REML = FALSE, data=growth_AGE0)
anova(growth.lmer1.nosex,growth.lmer1.intsex, growth.lmer1)

##Random Effects
#Introdcuction of a correlation between slope and intercept?
growth.lmer1.corsex<-lmer(MEASURE~1+AGE0*SEX+(1 +AGE0|IDNR), REML = TRUE, data=growth_AGE0)
growth.lmer1_REML<-lmer(MEASURE~1+AGE0*SEX+(1|IDNR)+(0 + AGE0|IDNR), REML = TRUE, data=growth_AGE0)
anova( growth.lmer1.corsex, growth.lmer1_REML, refit=FALSE)

#Random intercept and random slope
#Predicted random effects
growth.lmer1.re=ranef(growth.lmer1_REML)$IDNR
head(growth.lmer1.re,25)

####Plotting the random intercepet and random slope
plot(growth.lmer1.re, main="Random intercept (b0i) versus random slope (b1i)")
##Test for Random slope
growth.lmer_noRdSlo<-lmer(MEASURE~1+AGE0*SEX+(1|IDNR), REML = TRUE, data=growth_AGE0)
logratio_statistic <- -2 * (logLik(growth.lmer_noRdSlo,REML=TRUE) - logLik(growth.lmer1,REML=TRUE))
P_chi_df1 <-as.numeric(pchisq(logratio_statistic, df=1,lower.tail = FALSE))
P_chi_df2 <-as.numeric(pchisq(logratio_statistic, df=2,lower.tail = FALSE))
P_null_model <- 1/2 * (P_chi_df1 + P_chi_df2)
P_null_model

##Test for random intercept
growth.lmer_noRdInt<-lmer(MEASURE~1+AGE0*SEX+(0 + AGE0|IDNR), REML = TRUE, data=growth_AGE0)
logratio_statistic <- -2 * (logLik(growth.lmer_noRdInt,REML=TRUE) - logLik(growth.lmer1,REML=TRUE))
P_chi_df1 <-as.numeric(pchisq(logratio_statistic, df=1,lower.tail = FALSE))
P_chi_df2 <-as.numeric(pchisq(logratio_statistic, df=2,lower.tail = FALSE))
P_null_model <- 1/2 * (P_chi_df1 + P_chi_df2)
P_null_model

##Testing whether there really is a significant random effect
growth.lmer1_REML<-lmer(MEASURE~1+AGE0*SEX+(1|IDNR)+(0 + AGE0|IDNR), REML = TRUE, data=growth_AGE0)
anova(growth.lmer_noRdSlo, growth.lmer_noRdInt, growth.lmer1_REML, refit=FALSE) #we force R to compare methods using REML because we comnparing models using the same fixed effects

###Error terms
meanfull.hom<-lme(MEASURE ~ AGE0*SEX, random = ~1|IDNR, method="REML")
meanfull.het_REML <- lme(MEASURE ~ AGE0*SEX, random = ~1|IDNR, method="REML", weights = varIdent(form = ~1 | SEX))
anova(meanfull.hom,meanfull.het_REML)
summary(meanfull.het_REML)
##residual versus fitted
par(mfrow=c(1,2))
plot(meanfull.het$fitted[,2], meanfull.het$residuals[,2], xlab="fitted vales - Distances (in mm)", ylab="standardized residuals")
identify(meanfull.het$fitted[,2], meanfull.het$residuals[,2])
plot(meanfull.het$fitted[1:44,2], meanfull.het$residuals[1:44,2], col="red", ylim=c(-5,5),xlim=c(16,32),xlab="fitted vales - Distances (in mm)", ylab="standardized residuals")
points(meanfull.het$fitted[45:108,2], meanfull.het$residuals[45:108,2], col="blue")
legend(20, -2, legend=c("Male", "Female"), col=c("blue", "red"), pch=1, cex=0.8)
##Normality of the error term
#get standardised final residuals
final_resid<-(meanfull.het)$residuals
final_resid_f<-final_resid[1:44,2]
final_resid_m<-final_resid[45:108,2]
#QQplot confirming normality of the level 1 residuals (epsilon)
par(mfrow=c(1,2))
qqnorm(final_resid_f, main='QQ plot residuals
       Females',ylab='standardised residuals')
qqline(final_resid_f, col = 2) 

qqnorm(final_resid_m, main='QQ plot residuals
       Males',ylab='standardised residuals')
qqline(final_resid_m, col = 2) 

##Normality of the random effects
par(mfrow=c(1,1))
ranef_sigma<-as.vector(unname(ranef(meanfull.het)))
qqnorm(ranef[,1], main='QQ plot random effects',ylab='standardised residuals')
qqline(ranef[,1], col = 2) 

####Final model
meanfull.het <- lme(MEASURE ~ AGE0*SEX, random = ~1|IDNR, method="ML", weights = varIdent(form = ~1 | SEX))
summary(meanfull.het)
anova(meanfull.het, type = "marginal")
