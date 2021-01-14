install.packages("lavaan")
library(lavaan)

### Simulate Data ###
## The population model the data will be simulated from
## the following model. Highlight lines 8 through 42 and run
## to generate dataset for practice.
modVec <- ("LV1 =~ .868*V11 + 
                   .821*V12 +
                   .656*V13 
                   
            LV2 =~ .699*V21 + 
                   .787*V22 +
                   .874*V23 
                   
                   ## factor correlation

                     LV1 ~~ .519*LV2")


dat  <- simulateData(modVec,
                     std.lv=T,
                     meanstructure=T, 
                     model.type="cfa",
                     sample.nobs=500,
                     seed=1234567)

### Set the data to be discrete values between 1 and 5
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


### Effects Coding ###
### Configural ###
## Different groups have different labels within parentheses
## EX: c(L11a,L11b) group A is L11a and B is L11b

conf <- ' 
# Loadings
LV1 =~ c(L11a,L11b)*V11 + 
       c(L12a,L12b)*V12 + 
       c(L13a,L13b)*V13
       
LV2 =~ c(L21a,L21b)*V21 + 
       c(L22a,L22b)*V22 + 
       c(L23a,L23b)*V23

# Latent Variance Covariance
LV1~~c(NA,NA)*LV1
LV2~~c(NA,NA)*LV2

# Latent Covariance
LV1~~c(NA,NA)*LV2

# Latent Intercepts (Means)
LV1~c(NA,NA)*1
LV2~c(NA,NA)*1

# Item Intercepts
V11 ~ c(T11a,T11b)*1
V12 ~ c(T12a,T12b)*1
V13 ~ c(T13a,T13b)*1
V21 ~ c(T21a,T21b)*1
V22 ~ c(T22a,T22b)*1
V23 ~ c(T23a,T23b)*1

# Model Constraints
L11a==3-L12a-L13a
L21a==3-L22a-L23a

L11b==3-L12b-L13b
L21b==3-L22b-L23b

T11a==0-T12a-T13a
T21a==0-T22a-T23a

T11b==0-T12b-T13b
T21b==0-T22b-T23b
'
confit<- lavaan(model=conf, 
             data=dat, 
             missing="FIML", 
             estimator="ML", 
             std.lv=F, auto.var=T, auto.fix.first=F, auto.cov.lv.x=T, int.ov.free=T, 
             group = "G",
             meanstructure=TRUE)

summary(confit, 
        fit.measures=TRUE,
        standardized=T,
        #modindices=TRUE,
        rsquare=F)

### Weak ###
## Constrain all the Loadings to be equal across groups
## change "b" to "a", EX: c(L11a,L11b) becomes c(L11a,L11a)
## comment out or delete model constraints for "b" loadings
weak <- ' 
# Loadings
LV1 =~ c(L11a,L11a)*V11 + 
       c(L12a,L12a)*V12 + 
       c(L13a,L13a)*V13
       
LV2 =~ c(L21a,L21a)*V21 + 
       c(L22a,L22a)*V22 + 
       c(L23a,L23a)*V23

# Latent Variance Covariance
LV1~~c(NA,NA)*LV1
LV2~~c(NA,NA)*LV2

# Latent Covariance
LV1~~c(NA,NA)*LV2

# Latent Intercepts (Means)
LV1~c(NA,NA)*1
LV2~c(NA,NA)*1

# Item Intercepts
V11 ~ c(T11a,T11b)*1
V12 ~ c(T12a,T12b)*1
V13 ~ c(T13a,T13b)*1
V21 ~ c(T21a,T21b)*1
V22 ~ c(T22a,T22b)*1
V23 ~ c(T23a,T23b)*1

# Model Constraints
L11a==3-L12a-L13a
L21a==3-L22a-L23a

# "Comment out" by adding "#" to beginning of line
# L11b==3-L12b-L13b
# L21b==3-L22b-L23b

T11a==0-T12a-T13a
T21a==0-T22a-T23a

T11b==0-T12b-T13b
T21b==0-T22b-T23b
'
weakfit<- lavaan(model=weak, 
              data=dat, 
              missing="FIML", 
              estimator="ML", 
              std.lv=F, auto.var=T, auto.fix.first=F, auto.cov.lv.x=T, int.ov.free=T, 
              group = "G",
              meanstructure=TRUE)

summary(weakfit, 
        fit.measures=TRUE,
        standardized=T,
        #modindices=TRUE,
        rsquare=F)

### Strong ###
## Keep constraints on the loadings
## Add constraints on the item intercepts
## by changing "b" to "a"
## EX: c(T11a,T11b) becomes c(T11a,T11a)
## Comment out or delete un-needed constraints
## at the bottom of the model

strong <- '
# Loadings
LV1 =~ c(L11a,L11a)*V11 + 
       c(L12a,L12a)*V12 + 
       c(L13a,L13a)*V13

LV2 =~ c(L21a,L21a)*V21 + 
       c(L22a,L22a)*V22 + 
       c(L23a,L23a)*V23

# Latent Variance Covariance
LV1~~c(NA,NA)*LV1
LV2~~c(NA,NA)*LV2

# Latent Covariance
LV1~~c(NA,NA)*LV2

# Latent Intercepts (Means)
LV1~c(NA,NA)*1
LV2~c(NA,NA)*1

# Item Intercepts
V11 ~ c(T11a,T11a)*1
V12 ~ c(T12a,T12a)*1
V13 ~ c(T13a,T13a)*1
V21 ~ c(T21a,T21a)*1
V22 ~ c(T22a,T22a)*1
V23 ~ c(T23a,T23a)*1

# Model Constraints
L11a==3-L12a-L13a
L21a==3-L22a-L23a

# L11b==3-L12b-L13b
# L21b==3-L22b-L23b

T11a==0-T12a-T13a
T21a==0-T22a-T23a

# "Comment out" by adding "#" to beginning of line
# T11b==0-T12b-T13b
# T21b==0-T22b-T23b
'
strongfit<- lavaan(model=strong, 
                 data=dat, 
                 missing="FIML", 
                 estimator="ML", 
                 std.lv=F, auto.var=T, auto.fix.first=F, auto.cov.lv.x=T, int.ov.free=T, 
                 group = "G",
                 meanstructure=TRUE)

summary(strongfit, 
        fit.measures=TRUE,
        standardized=T,
        #modindices=TRUE,
        rsquare=F)

## Adding Phantoms to Strong Model ##
## Keep previous constraints ##

## Add a Phantom construct for each construct
## with a single loading for each construct

## Fix lower construct variance to zero
## Fix phantom construct variance to 1

## Fix covariance between unassociated
## lower constructs and higher phantoms to zero
## since lavaan (and all SEM software) will correlate
## these by default

## Estemate correlations betweetween phantoms freely

## Estemate Latent Intercepts (Means)

## Fix Phantom Intercepts (Means) to zero

strongPH <- ' 
# Loadings CONSTRAINED
LV1 =~ c(L11a,L11a)*V11 + 
       c(L12a,L12a)*V12 + 
       c(L13a,L13a)*V13
LV2 =~ c(L21a,L21a)*V21 + 
       c(L22a,L22a)*V22 + 
       c(L23a,L23a)*V23 

# Phantom Loadings
## Latent Standard Deviations ##
Ph_LV1=~c(B1a,B1b)*LV1
Ph_LV2=~c(B2a,B2b)*LV2

# Latent Variance
LV1~~c(0,0)*LV1
LV2~~c(0,0)*LV2

# Phantom Variance
Ph_LV1~~c(1,1)*Ph_LV1
Ph_LV2~~c(1,1)*Ph_LV2

# Covariance between unrelated Latent and Phantoms
LV1~~c(0,0)*Ph_LV2
LV2~~c(0,0)*Ph_LV1

# Correlation between phantoms
Ph_LV1~~c(NA,NA)*Ph_LV2

# Latent Intercepts
LV1~c(NA,NA)*1
LV2~c(NA,NA)*1

# Phantom Intercepts 
Ph_LV1~c(0,0)*1
Ph_LV2~c(0,0)*1

# Item Intercepts 
# CONSTRAINED
V11 ~ c(T11a,T11a)*1
V12 ~ c(T12a,T12a)*1
V13 ~ c(T13a,T13a)*1
V21 ~ c(T21a,T21a)*1
V22 ~ c(T22a,T22a)*1
V23 ~ c(T23a,T23a)*1

# Model Constraints
L11a==3-L12a-L13a
L21a==3-L22a-L23a

# L11b==3-L12b-L13b
# L21b==3-L22b-L23b

T11a==0-T12a-T13a
T21a==0-T22a-T23a

# T11b==0-T12b-T13b
# T21b==0-T22b-T23b
'
strongPHfit<- cfa(model=strongPH, 
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
## Constrain the Latent Intercepts to be equal accross groups

means <- ' 
# Loadings CONSTRAINED
LV1 =~ c(L11a,L11a)*V11 + 
       c(L12a,L12a)*V12 + 
       c(L13a,L13a)*V13
LV2 =~ c(L21a,L21a)*V21 + 
       c(L22a,L22a)*V22 + 
       c(L23a,L23a)*V23 

# Phantom Loadings
## Latent Standard Deviations ##
Ph_LV1=~c(B1a,B1b)*LV1
Ph_LV2=~c(B2a,B2b)*LV2

# Latent Variance
LV1~~c(0,0)*LV1
LV2~~c(0,0)*LV2

# Phantom Variance
Ph_LV1~~c(1,1)*Ph_LV1
Ph_LV2~~c(1,1)*Ph_LV2

# Covariance between unrelated Latent and Phantoms
LV1~~c(0,0)*Ph_LV2
LV2~~c(0,0)*Ph_LV1

# Correlation between phantoms
Ph_LV1~~c(NA,NA)*Ph_LV2

# Latent Intercepts ## CONSTRAINED 
LV1~c(A1a,A1a)*1
LV2~c(A2a,A2a)*1

# Phantom Intercepts 
Ph_LV1~c(0,0)*1
Ph_LV2~c(0,0)*1

# Item Intercepts 
# CONSTRAINED
V11 ~ c(T11a,T11a)*1
V12 ~ c(T12a,T12a)*1
V13 ~ c(T13a,T13a)*1
V21 ~ c(T21a,T21a)*1
V22 ~ c(T22a,T22a)*1
V23 ~ c(T23a,T23a)*1

# Model Constraints
L11a==3-L12a-L13a
L21a==3-L22a-L23a

# L11b==3-L12b-L13b
# L21b==3-L22b-L23b

T11a==0-T12a-T13a
T21a==0-T22a-T23a

# T11b==0-T12b-T13b
# T21b==0-T22b-T23b
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

# Use the anova() function to do a chi-square difference test
anova(strongfit,meansfit)

### Latent Variance ###
## Constrain the Latent Standard Deviations
## to be equal across groups

ltVar <- ' 
# Loadings CONSTRAINED
LV1 =~ c(L11a,L11a)*V11 + 
       c(L12a,L12a)*V12 + 
       c(L13a,L13a)*V13
LV2 =~ c(L21a,L21a)*V21 + 
       c(L22a,L22a)*V22 + 
       c(L23a,L23a)*V23 

# Phantom Loadings
## Latent Standard Deviations ## CONSTRAINED
Ph_LV1=~c(B1a,B1a)*LV1
Ph_LV2=~c(B2a,B2a)*LV2

# Latent Variance
LV1~~c(0,0)*LV1
LV2~~c(0,0)*LV2

# Phantom Variance
Ph_LV1~~c(1,1)*Ph_LV1
Ph_LV2~~c(1,1)*Ph_LV2

# Covariance between unrelated Latent and Phantoms
LV1~~c(0,0)*Ph_LV2
LV2~~c(0,0)*Ph_LV1

# Correlation between phantoms
Ph_LV1~~c(NA,NA)*Ph_LV2

# Latent Intercepts
LV1~c(A1a,A1b)*1
LV2~c(A2a,A2b)*1

# Phantom Intercepts 
Ph_LV1~c(0,0)*1
Ph_LV2~c(0,0)*1

# Item Intercepts 
# CONSTRAINED
V11 ~ c(T11a,T11a)*1
V12 ~ c(T12a,T12a)*1
V13 ~ c(T13a,T13a)*1
V21 ~ c(T21a,T21a)*1
V22 ~ c(T22a,T22a)*1
V23 ~ c(T23a,T23a)*1

# Model Constraints
L11a==3-L12a-L13a
L21a==3-L22a-L23a

# L11b==3-L12b-L13b
# L21b==3-L22b-L23b

T11a==0-T12a-T13a
T21a==0-T22a-T23a

# T11b==0-T12b-T13b
# T21b==0-T22b-T23b
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

# Use the anova() function to do a chi-square difference test
anova(strongfit,ltVarFit)

### Latent Covariance
## Constrain the Correlation between Phantoms
## to be equal between groups

ltCovar <- ' 
# Loadings CONSTRAINED
LV1 =~ c(L11a,L11a)*V11 + 
       c(L12a,L12a)*V12 + 
       c(L13a,L13a)*V13
LV2 =~ c(L21a,L21a)*V21 + 
       c(L22a,L22a)*V22 + 
       c(L23a,L23a)*V23 

# Phantom Loadings
## Latent Standard Deviations 
Ph_LV1=~c(B1a,B1b)*LV1
Ph_LV2=~c(B2a,B2b)*LV2

# Latent Variance
LV1~~c(0,0)*LV1
LV2~~c(0,0)*LV2

# Phantom Variance
Ph_LV1~~c(1,1)*Ph_LV1
Ph_LV2~~c(1,1)*Ph_LV2

# Covariance between unrelated Latent and Phantoms
LV1~~c(0,0)*Ph_LV2
LV2~~c(0,0)*Ph_LV1

# Correlation between phantoms ## CONSTRAINED
Ph_LV1~~c(R1a,R1a)*Ph_LV2 

# Latent Intercepts
LV1~c(A1a,A1b)*1
LV2~c(A2a,A2b)*1

# Phantom Intercepts 
Ph_LV1~c(0,0)*1
Ph_LV2~c(0,0)*1

# Item Intercepts 
# CONSTRAINED
V11 ~ c(T11a,T11a)*1
V12 ~ c(T12a,T12a)*1
V13 ~ c(T13a,T13a)*1
V21 ~ c(T21a,T21a)*1
V22 ~ c(T22a,T22a)*1
V23 ~ c(T23a,T23a)*1

# Model Constraints
L11a==3-L12a-L13a
L21a==3-L22a-L23a

# L11b==3-L12b-L13b
# L21b==3-L22b-L23b

T11a==0-T12a-T13a
T21a==0-T22a-T23a

# T11b==0-T12b-T13b
# T21b==0-T22b-T23b
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

# Use the anova() function to do a chi-square difference test
anova(strongfit,LtCovarFit)

