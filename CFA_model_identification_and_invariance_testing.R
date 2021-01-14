install.packages("lavaan")
library(lavaan)

### Simulate Data ###
## The population model the data will be simulated from
modVec <- ("LV1 =~ .868*V11 + 
                   .821*V12 +
                   .656*V13 
                   
            LV2 =~ .699*V21 + 
                   .787*V22 +
                   .874*V23 +
                   .717*V24 
                   
                   ## factor correlation

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

### Make a grouping variable 
group_sample <- sample(nrow(dat),nrow(dat)*.5)

dat$G  <- "A"
dat$G[group_sample]  <- "B"


### Examine simulated data
head(dat)

### Marker Method ###
mod1 <- ' 
LV1 =~ V11 + V12 + V13
LV2 =~ V21 + V22 + V23 + V24
'
fit1<- cfa(model=mod1, 
           data=dat, 
           missing="FIML", 
           estimator="ML", 
           std.lv=FALSE, 
           meanstructure=TRUE)

summary(fit1, 
        fit.measures=TRUE,
        standardized=T,
        modindices=TRUE,
        rsquare=F)

fitmeasures(fit1)
fitmeasures(fit1)[c("cfi","tli","rmsea","srmr")]

mod1.1 <- ' 
LV1 =~ NA*V11 + 1*V12 + V13
LV2 =~ NA*V21 + 1*V22 + V23 + V24
'
fit1.1<- cfa(model=mod1.1, 
           data=dat, 
           missing="FIML", 
           estimator="ML", 
           std.lv=FALSE, 
           meanstructure=TRUE)

summary(fit1.1, 
        fit.measures=TRUE,
        standardized=T,
        #modindices=TRUE,
        rsquare=F)

fitmeasures(fit1.1)[c("cfi","tli","rmsea","srmr")]

### Fixed Factor ###
mod2 <- ' 
LV1 =~ V11 + V12 + V13
LV2 =~ V21 + V22 + V23 + V24
'
fit2<- cfa(model=mod2, 
           data=dat, 
           missing="FIML", 
           estimator="ML", 
           std.lv=TRUE,  # ONLY CHANGE
           meanstructure=TRUE)

summary(fit2, 
        fit.measures=TRUE,
        standardized=T,
        #modindices=TRUE,
        rsquare=F)

fitmeasures(fit2)[c("cfi","tli","rmsea","srmr")]


### Fixed Factor Specified###
mod2.1 <- ' 
# Loadings
LV1 =~ V11 + V12 + V13
LV2 =~ V21 + V22 + V23 + V24

# Latent Variance
LV1 ~~ 1*LV1
LV2 ~~ 1*LV2

# Latent Means
LV1 ~0*1
LV2 ~0*1
'
fit2.1<- cfa(model=mod2.1, 
           data=dat, 
           missing="FIML", 
           estimator="ML", 
           std.lv=TRUE,  # ONLY CHANGE
           meanstructure=TRUE)

summary(fit2.1, 
        fit.measures=TRUE,
        standardized=T,
        #modindices=TRUE,
        rsquare=F)

fitmeasures(fit2.1)[c("cfi","tli","rmsea","srmr")]

### Effects Coding ###
mod3 <- ' 
# Loadings
LV1 =~ (L11)*V11 + (L21)*V12 + (L31)*V13
LV2 =~ (L12)*V21 + (L22)*V22 + (L32)*V23 + (L42)*V24

# Latent Variance
LV1~~NA*LV1
LV2~~NA*LV2

# Latent Intercepts
LV1~NA*1
LV2~NA*1

# Item Intercepts
V11~(T11)*1
V12~(T21)*1
V13~(T31)*1
V21~(T12)*1
V22~(T22)*1
V23~(T32)*1
V24~(T42)*1

#Model Constraints
L11==3-L21-L31
L12==4-L22-L32-L42

T11==0-T21-T31
T12==0-T22-T32-T42
'
fit3<- lavaan(model=mod3, 
          data=dat, 
          missing="FIML", 
          estimator="ML", 
          std.lv=F, auto.var=T, auto.fix.first=F, auto.cov.lv.x=T, int.ov.free=T,
          meanstructure=TRUE)

summary(fit3, 
        fit.measures=TRUE,
        standardized=T,
        #modindices=TRUE,
        rsquare=F)

fitmeasures(fit3)[c("cfi","tli","rmsea","srmr")]

### Effects Coding with Phantoms ###

mod4 <- ' 
# Loadings
LV1 =~ (L11)*V11 + (L21)*V12 + (L31)*V13
LV2 =~ (L12)*V21 + (L22)*V22 + (L32)*V23 + (L42)*V24

# Phantoms
Ph_LV1=~LV1
Ph_LV2=~LV2

# Latent Variance
LV1~~0*LV1
LV2~~0*LV2

Ph_LV1~~1*Ph_LV1
Ph_LV2~~1*Ph_LV2

LV1~~0*Ph_LV2
LV2~~0*Ph_LV1

Ph_LV1~~NA*Ph_LV2

# Intercepts
LV1~NA*1
LV2~NA*1

Ph_LV1~0*1
Ph_LV2~0*1

V11~(T11)*1
V12~(T21)*1
V13~(T31)*1
V21~(T12)*1
V22~(T22)*1
V23~(T32)*1
V24~(T42)*1

# Model Constraints
L11==3-L21-L31
L12==4-L22-L32-L42


T11==0-T21-T31
T12==0-T22-T32-T42
'
fit4<- cfa(model=mod4, 
          data=dat, 
          missing="FIML", 
          estimator="ML", 
          std.lv=TRUE, 
          meanstructure=TRUE)

summary(fit4, 
        fit.measures=TRUE,
        standardized=T,
        modindices=TRUE,
        rsquare=F)

fitmeasures(fit4)[c("cfi","tli","rmsea","srmr")]

mods <- modindices(fit4)

mods[order(mods$mi,decreasing = T),]


### Multiple Group Invariance Testing ###
head(dat)
### Fixed Factor ###
### Configural Model
conf <- ' 
# Loadings
LV1 =~ c(L11a,L11b)*V11 + 
       c(L12a,L12b)*V12 + 
       c(L13a,L13b)*V13
LV2 =~ c(L21a,L21b)*V21 + 
       c(L22a,L22b)*V22 + 
       c(L23a,L23b)*V23 + 
       c(L24a,L24b)*V24
# Variance       
LV1 ~~ c(1,1)*LV1
LV2 ~~ c(1,1)*LV2
# Intercepts
LV1 ~ c(0,0)*1
LV2 ~ c(0,0)*1

V11 ~ c(T11a,T11b)*1
V12 ~ c(T12a,T12b)*1
V13 ~ c(T13a,T13b)*1
V21 ~ c(T21a,T21b)*1
V22 ~ c(T22a,T22b)*1
V23 ~ c(T23a,T23b)*1
V24 ~ c(T24a,T24b)*1
'
confit<- cfa(model=conf, 
           data=dat, 
           missing="FIML", 
           estimator="ML", 
           std.lv=TRUE,
           group = "G",
           meanstructure=TRUE)

summary(confit, 
        fit.measures=TRUE,
        standardized=T,
        #modindices=TRUE,
        rsquare=F)

### Weak
weak <- ' 
# Loadings  CONSTRAINED
LV1 =~ c(L11a,L11a)*V11 + 
       c(L12a,L12a)*V12 + 
       c(L13a,L13a)*V13
LV2 =~ c(L21a,L21a)*V21 + 
       c(L22a,L22a)*V22 + 
       c(L23a,L23a)*V23 + 
       c(L24a,L24a)*V24
# Variances  FREED     
LV1 ~~ c(1,NA)*LV1
LV2 ~~ c(1,NA)*LV2
#Intercepts
LV1 ~ c(0,0)*1
LV2 ~ c(0,0)*1

V11 ~ c(T11a,T11b)*1
V12 ~ c(T12a,T12b)*1
V13 ~ c(T13a,T13b)*1
V21 ~ c(T21a,T21b)*1
V22 ~ c(T22a,T22b)*1
V23 ~ c(T23a,T23b)*1
V24 ~ c(T24a,T24b)*1
'
weakfit<- cfa(model=weak, 
             data=dat, 
             missing="FIML", 
             estimator="ML", 
             std.lv=TRUE,
             group = "G",
             meanstructure=TRUE)

summary(weakfit, 
        fit.measures=TRUE,
        standardized=T,
        #modindices=TRUE,
        rsquare=F)

### Strong
strong <- ' 
# Loadings CONSTRAINED
LV1 =~ c(L11a,L11a)*V11 + 
       c(L12a,L12a)*V12 + 
       c(L13a,L13a)*V13
LV2 =~ c(L21a,L21a)*V21 + 
       c(L22a,L22a)*V22 + 
       c(L23a,L23a)*V23 + 
       c(L24a,L24a)*V24
# Variances  FREED     
LV1 ~~ c(1,NA)*LV1
LV2 ~~ c(1,NA)*LV2
# Intercepts FREED
LV1 ~ c(0,NA)*1
LV2 ~ c(0,NA)*1

# CONSTRAINED

V11 ~ c(T11a,T11a)*1
V12 ~ c(T12a,T12a)*1
V13 ~ c(T13a,T13a)*1
V21 ~ c(T21a,T21a)*1
V22 ~ c(T22a,T22a)*1
V23 ~ c(T23a,T23a)*1
V24 ~ c(T24a,T24a)*1
'
strongfit<- cfa(model=strong, 
              data=dat, 
              missing="FIML", 
              estimator="ML", 
              std.lv=TRUE,
              group = "G",
              meanstructure=TRUE)

summary(strongfit, 
        fit.measures=TRUE,
        standardized=T,
        #modindices=TRUE,
        rsquare=F)

### Effects Coding with Phantoms ###
### Configural ###
conf <- ' 
# Loadings
LV1 =~ c(L11a,L11b)*V11 + 
       c(L12a,L12b)*V12 + 
       c(L13a,L13b)*V13
LV2 =~ c(L21a,L21b)*V21 + 
       c(L22a,L22b)*V22 + 
       c(L23a,L23b)*V23 + 
       c(L24a,L24b)*V24

# Phantoms
Ph_LV1=~c(B1a,B1b)*LV1
Ph_LV2=~c(B2a,B2b)*LV2

# Latent Variance and Covariance
LV1~~c(0,0)*LV1
LV2~~c(0,0)*LV2

Ph_LV1~~c(1,1)*Ph_LV1
Ph_LV2~~c(1,1)*Ph_LV2

LV1~~c(0,0)*Ph_LV2
LV2~~c(0,0)*Ph_LV1

Ph_LV1~~c(NA,NA)*Ph_LV2

# Intercepts
LV1~c(NA,NA)*1
LV2~c(NA,NA)*1

Ph_LV1~c(0,0)*1
Ph_LV2~c(0,0)*1

V11 ~ c(T11a,T11b)*1
V12 ~ c(T12a,T12b)*1
V13 ~ c(T13a,T13b)*1
V21 ~ c(T21a,T21b)*1
V22 ~ c(T22a,T22b)*1
V23 ~ c(T23a,T23b)*1
V24 ~ c(T24a,T24b)*1

# Model Constraints
L11a==3-L12a-L13a
L21a==4-L22a-L23a-L24a

L11b==3-L12b-L13b
L21b==4-L22b-L23b-L24b

T11a==0-T12a-T13a
T21a==0-T22a-T23a-T24a

T11b==0-T12b-T13b
T21b==0-T22b-T23b-T24b
'
confit<- cfa(model=conf, 
           data=dat, 
           missing="FIML", 
           estimator="ML", 
           std.lv=TRUE, 
           group = "G",
           meanstructure=TRUE)

summary(confit, 
        fit.measures=TRUE,
        standardized=T,
        #modindices=TRUE,
        rsquare=F)

### Weak ###
weak <- ' 
# Loadings CONSTRAINED
LV1 =~ c(L11a,L11a)*V11 + 
       c(L12a,L12a)*V12 + 
       c(L13a,L13a)*V13
LV2 =~ c(L21a,L21a)*V21 + 
       c(L22a,L22a)*V22 + 
       c(L23a,L23a)*V23 + 
       c(L24a,L24a)*V24

# Phantoms
Ph_LV1=~c(B1a,B1b)*LV1
Ph_LV2=~c(B2a,B2b)*LV2

# Latent Variance and Covariance
LV1~~c(0,0)*LV1
LV2~~c(0,0)*LV2

Ph_LV1~~c(1,1)*Ph_LV1
Ph_LV2~~c(1,1)*Ph_LV2

LV1~~c(0,0)*Ph_LV2
LV2~~c(0,0)*Ph_LV1

Ph_LV1~~c(NA,NA)*Ph_LV2

# Intercepts
LV1~c(NA,NA)*1
LV2~c(NA,NA)*1

Ph_LV1~c(0,0)*1
Ph_LV2~c(0,0)*1

V11 ~ c(T11a,T11b)*1
V12 ~ c(T12a,T12b)*1
V13 ~ c(T13a,T13b)*1
V21 ~ c(T21a,T21b)*1
V22 ~ c(T22a,T22b)*1
V23 ~ c(T23a,T23b)*1
V24 ~ c(T24a,T24b)*1

# Model Constraints
L11a==3-L12a-L13a
L21a==4-L22a-L23a-L24a

# L11b==3-L12b-L13b
# L21b==4-L22b-L23b-L24b

T11a==0-T12a-T13a
T21a==0-T22a-T23a-T24a

T11b==0-T12b-T13b
T21b==0-T22b-T23b-T24b
'
weakfit<- cfa(model=weak, 
             data=dat, 
             missing="FIML", 
             estimator="ML", 
             std.lv=TRUE, 
             group = "G",
             meanstructure=TRUE)

summary(weakfit, 
        fit.measures=TRUE,
        standardized=T,
        #modindices=TRUE,
        rsquare=F)

### Strong ###
strong <- ' 
# Loadings CONSTRAINED
LV1 =~ c(L11a,L11a)*V11 + 
       c(L12a,L12a)*V12 + 
       c(L13a,L13a)*V13
LV2 =~ c(L21a,L21a)*V21 + 
       c(L22a,L22a)*V22 + 
       c(L23a,L23a)*V23 + 
       c(L24a,L24a)*V24

# Phantoms
Ph_LV1=~c(B1a,B1b)*LV1
Ph_LV2=~c(B2a,B2b)*LV2

# Latent Variance and Covariance
LV1~~c(0,0)*LV1
LV2~~c(0,0)*LV2

Ph_LV1~~c(1,1)*Ph_LV1
Ph_LV2~~c(1,1)*Ph_LV2

LV1~~c(0,0)*Ph_LV2
LV2~~c(0,0)*Ph_LV1

Ph_LV1~~c(NA,NA)*Ph_LV2

# Intercepts
LV1~c(NA,NA)*1
LV2~c(NA,NA)*1

Ph_LV1~c(0,0)*1
Ph_LV2~c(0,0)*1

# CONSTRAINED
V11 ~ c(T11a,T11a)*1
V12 ~ c(T12a,T12a)*1
V13 ~ c(T13a,T13a)*1
V21 ~ c(T21a,T21a)*1
V22 ~ c(T22a,T22a)*1
V23 ~ c(T23a,T23a)*1
V24 ~ c(T24a,T24a)*1

# Model Constraints
L11a==3-L12a-L13a
L21a==4-L22a-L23a-L24a

# L11b==3-L12b-L13b
# L21b==4-L22b-L23b-L24b

T11a==0-T12a-T13a
T21a==0-T22a-T23a-T24a

# T11b==0-T12b-T13b
# T21b==0-T22b-T23b-T24b
'
strongfit<- cfa(model=strong, 
              data=dat, 
              missing="FIML", 
              estimator="ML", 
              std.lv=TRUE, 
              group = "G",
              meanstructure=TRUE)

summary(strongfit, 
        fit.measures=TRUE,
        standardized=T,
        #modindices=TRUE,
        rsquare=F)

### Strong Model is Baseline ###

### Latent Means ###

means <- ' 
# Loadings CONSTRAINED
LV1 =~ c(L11a,L11a)*V11 + 
       c(L12a,L12a)*V12 + 
       c(L13a,L13a)*V13
LV2 =~ c(L21a,L21a)*V21 + 
       c(L22a,L22a)*V22 + 
       c(L23a,L23a)*V23 + 
       c(L24a,L24a)*V24

# Phantoms
Ph_LV1=~c(B1a,B1b)*LV1
Ph_LV2=~c(B2a,B2b)*LV2

# Latent Variance and Covariance
LV1~~c(0,0)*LV1
LV2~~c(0,0)*LV2

Ph_LV1~~c(1,1)*Ph_LV1
Ph_LV2~~c(1,1)*Ph_LV2

LV1~~c(0,0)*Ph_LV2
LV2~~c(0,0)*Ph_LV1

Ph_LV1~~c(NA,NA)*Ph_LV2

# Intercepts CONSTRAINED
LV1~c(A1a,A1a)*1
LV2~c(A2a,A2a)*1

Ph_LV1~c(0,0)*1
Ph_LV2~c(0,0)*1

# CONSTRAINED
V11 ~ c(T11a,T11a)*1
V12 ~ c(T12a,T12a)*1
V13 ~ c(T13a,T13a)*1
V21 ~ c(T21a,T21a)*1
V22 ~ c(T22a,T22a)*1
V23 ~ c(T23a,T23a)*1
V24 ~ c(T24a,T24a)*1

# Model Constraints
L11a==3-L12a-L13a
L21a==4-L22a-L23a-L24a

# L11b==3-L12b-L13b
# L21b==4-L22b-L23b-L24b

T11a==0-T12a-T13a
T21a==0-T22a-T23a-T24a

# T11b==0-T12b-T13b
# T21b==0-T22b-T23b-T24b
'
meansfit<- cfa(model=means, 
                data=dat, 
                missing="FIML", 
                estimator="ML", 
                std.lv=TRUE, 
                group = "G",
                meanstructure=TRUE)

summary(meansfit, 
        fit.measures=TRUE,
        standardized=T,
        #modindices=TRUE,
        rsquare=F)

anova(strongfit,meansfit)

### Latent Variance ###

ltVar <- ' 
# Loadings CONSTRAINED
LV1 =~ c(L11a,L11a)*V11 + 
       c(L12a,L12a)*V12 + 
       c(L13a,L13a)*V13
LV2 =~ c(L21a,L21a)*V21 + 
       c(L22a,L22a)*V22 + 
       c(L23a,L23a)*V23 + 
       c(L24a,L24a)*V24

# Phantoms CONSTRAINED
Ph_LV1=~c(B1a,B1a)*LV1
Ph_LV2=~c(B2a,B2a)*LV2

# Latent Variance and Covariance
LV1~~c(0,0)*LV1
LV2~~c(0,0)*LV2

Ph_LV1~~c(1,1)*Ph_LV1
Ph_LV2~~c(1,1)*Ph_LV2

LV1~~c(0,0)*Ph_LV2
LV2~~c(0,0)*Ph_LV1

Ph_LV1~~c(NA,NA)*Ph_LV2

# Intercepts
LV1~c(NA,NA)*1
LV2~c(NA,NA)*1

Ph_LV1~c(0,0)*1
Ph_LV2~c(0,0)*1

# CONSTRAINED
V11 ~ c(T11a,T11a)*1
V12 ~ c(T12a,T12a)*1
V13 ~ c(T13a,T13a)*1
V21 ~ c(T21a,T21a)*1
V22 ~ c(T22a,T22a)*1
V23 ~ c(T23a,T23a)*1
V24 ~ c(T24a,T24a)*1

# Model Constraints
L11a==3-L12a-L13a
L21a==4-L22a-L23a-L24a

# L11b==3-L12b-L13b
# L21b==4-L22b-L23b-L24b

T11a==0-T12a-T13a
T21a==0-T22a-T23a-T24a

# T11b==0-T12b-T13b
# T21b==0-T22b-T23b-T24b
'
ltVarFit<- cfa(model=ltVar, 
                data=dat, 
                missing="FIML", 
                estimator="ML", 
                std.lv=TRUE, 
                group = "G",
                meanstructure=TRUE)

summary(ltVarFit, 
        fit.measures=TRUE,
        standardized=T,
        #modindices=TRUE,
        rsquare=F)

anova(strongfit,ltVarFit)

### Latent Covariance

ltCovar <- ' 
# Loadings CONSTRAINED
LV1 =~ c(L11a,L11a)*V11 + 
       c(L12a,L12a)*V12 + 
       c(L13a,L13a)*V13
LV2 =~ c(L21a,L21a)*V21 + 
       c(L22a,L22a)*V22 + 
       c(L23a,L23a)*V23 + 
       c(L24a,L24a)*V24

# Phantoms
Ph_LV1=~c(B1a,B1b)*LV1
Ph_LV2=~c(B2a,B2b)*LV2

# Latent Variance and Covariance
LV1~~c(0,0)*LV1
LV2~~c(0,0)*LV2

Ph_LV1~~c(1,1)*Ph_LV1
Ph_LV2~~c(1,1)*Ph_LV2

LV1~~c(0,0)*Ph_LV2
LV2~~c(0,0)*Ph_LV1

# CONSTRAINED
Ph_LV1~~c(R1a,R1a)*Ph_LV2

# Intercepts
LV1~c(NA,NA)*1
LV2~c(NA,NA)*1

Ph_LV1~c(0,0)*1
Ph_LV2~c(0,0)*1

# CONSTRAINED
V11 ~ c(T11a,T11a)*1
V12 ~ c(T12a,T12a)*1
V13 ~ c(T13a,T13a)*1
V21 ~ c(T21a,T21a)*1
V22 ~ c(T22a,T22a)*1
V23 ~ c(T23a,T23a)*1
V24 ~ c(T24a,T24a)*1

# Model Constraints
L11a==3-L12a-L13a
L21a==4-L22a-L23a-L24a

# L11b==3-L12b-L13b
# L21b==4-L22b-L23b-L24b

T11a==0-T12a-T13a
T21a==0-T22a-T23a-T24a

# T11b==0-T12b-T13b
# T21b==0-T22b-T23b-T24b
'
LtCovarFit<- cfa(model=ltCovar, 
                data=dat, 
                missing="FIML", 
                estimator="ML", 
                std.lv=TRUE, 
                group = "G",
                meanstructure=TRUE)

summary(LtCovarFit, 
        fit.measures=TRUE,
        standardized=T,
        #modindices=TRUE,
        rsquare=F)

anova(strongfit,LtCovarFit)

