install.packages("lavaan")
library(lavaan)

### Simulate Data ###
## The population model the data will be simulated from
modVec <- ("LV1 =~ .868*V11 + 
                   .821*V12 +
                   .656*V13 
                   
            LV2 =~ .699*V21 + 
                   .787*V22 +
                   .874*V23 
                   
                   ## latent factor correlation

                     LV1 ~~ .519*LV2")


dat  <- simulateData(modVec,
                     std.lv=T,
                     meanstructure=T, 
                     model.type="cfa",
                     sample.nobs=500,
                     seed=1234567)

### Set the data to be descrete values between 1 and 5
for (i in 1:length(names(dat))) {
        dat[,i] <- dat[,i]+abs(min(dat[,i]))
        dat[,i] <- dat[,i]/max(dat[,i])
        dat[,i] <- (dat[,i]*4)+1
        dat[,i] <- round(dat[,i])
}

dat$ID <- sprintf("%04.0f", 0001:nrow(dat))

### Examine simulated data
head(dat)

### Marker Method ###
mod1 <- ' 
LV1 =~ V11 + V12 + V13
LV2 =~ V21 + V22 + V23
'
fit1<- cfa(model=mod1, 
           data=dat, 
           estimator="ML", 
           std.lv=FALSE, 
           meanstructure=TRUE)

summary(fit1, 
        fit.measures=TRUE,
        standardized=T,
        #modindices=TRUE,
        rsquare=F)

### Marker Method: Moving the Marker Variable
mod1.1 <- ' 
LV1 =~ NA*V11 + 1*V12 + V13
LV2 =~ NA*V21 + 1*V22 + V23
'
fit1.1<- cfa(model=mod1.1, 
           data=dat, 
           estimator="ML", 
           std.lv=FALSE, 
           meanstructure=TRUE)

summary(fit1.1, 
        fit.measures=TRUE,
        standardized=T,
        #modindices=TRUE,
        rsquare=F)

### Fixed Factor ###
mod2 <- ' 
LV1 =~ V11 + V12 + V13
LV2 =~ V21 + V22 + V23 
'


fit2<- cfa(model=mod2, 
           data=dat, 
           estimator="ML", 
           std.lv=TRUE,  # ONLY CHANGE FROM DEFAULT
           meanstructure=TRUE)

summary(fit2, 
        fit.measures=TRUE,
        standardized=T,
        modindices=TRUE,
        rsquare=F)

mods <- modindices(fit2)

mods[order(mods$mi,decreasing = T),]

### Effects Coding ###
mod3 <- ' 

# Loadings
LV1 =~ (L11)*V11 + (L21)*V12 + (L31)*V13
LV2 =~ (L12)*V21 + (L22)*V22 + (L32)*V23

# Latent Variance
LV1~~NA*LV1
LV2~~NA*LV2

# Latent Means
LV1~NA*1
LV2~NA*1

# Means
V11~(T11)*1
V12~(T21)*1
V13~(T31)*1
V21~(T12)*1
V22~(T22)*1
V23~(T32)*1


# Model Constraints
L11==3-L21-L31
L12==3-L22-L32

T11==0-T21-T31
T12==0-T22-T32
'
fit3<- lavaan(model=mod3, 
          data=dat, 
          estimator="ML", 
          std.lv=F, auto.var=T, auto.fix.first=F, auto.cov.lv.x=T, int.ov.free=T,
          meanstructure=TRUE)

summary(fit3, 
        fit.measures=TRUE,
        standardized=T,
        #modindices=TRUE,
        rsquare=F)


#### NOTES ####

# Changing the method of identification did not change the model fit

# Looking at what parameters are fixed and what parameters are estimated
#       is a good method to check that you've defined your model correctly
