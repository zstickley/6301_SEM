### Strong ###
strong <- ' 
# Loadings
LV1    =~ c(L11a,L11a)*var1_1 + 
          c(L12a,L12a)*var1_2 + 
          c(L13a,L13a)*var1_3
          
LV2    =~ c(L21a,L21a)*var2_1 + 
          c(L22a,L22a)*var2_2 + 
          c(L23a,L23a)*var2_3 
          
LV3    =~ c(L31a,L31a)*var3_1 + 
          c(L32a,L32a)*var3_2 + 
          c(L33a,L33a)*var3_3 

# Latent Variance and Covariance
LV1~~c(NA,NA)*LV1
LV2~~c(NA,NA)*LV2
LV3~~c(NA,NA)*LV3

# Intercepts
LV1~c(NA,NA)*1
LV2~c(NA,NA)*1
LV3~c(NA,NA)*1

var1_1   ~ c(T11a,T11a)*1
var1_2   ~ c(T12a,T12a)*1
var1_3   ~ c(T13a,T13a)*1
var2_1   ~ c(T21a,T21a)*1
var2_2   ~ c(T22a,T22a)*1
var2_3   ~ c(T23a,T23a)*1
var3_1   ~ c(T31a,T31a)*1
var3_2   ~ c(T32a,T32a)*1
var3_3   ~ c(T33a,T33a)*1

# Model Constraints
L11a==3-L12a-L13a
L21a==3-L22a-L23a
L31a==3-L32a-L33a

## Commented out due to strong invariance
# L11b==3-L12b-L13b
# L21b==3-L22b-L23b
# L31b==3-L32b-L33b

T11a==0-T12a-T13a
T21a==0-T22a-T23a
T31a==0-T32a-T33a

## Commented out due to strong invariance
# T11b==0-T12b-T13b
# T21b==0-T22b-T23b
# T31b==0-T32b-T33b
'
strongfit<- lavaan(model=strong, 
                   data=dat, 
                   missing="FIML", 
                   estimator="ML", 
                   std.lv=F, auto.var=T, auto.fix.first=F, auto.cov.lv.x=T, int.ov.free=T, 
                   group = "class",
                   meanstructure=TRUE)

summary(strongfit, 
        fit.measures=T,
        standardized=T,
        rsquare=F)

### Phantom Constructs ###
phantom <- ' 
# Loadings

LV1    =~ c(L11a,L11a)*var1_1 + 
          c(L12a,L12a)*var1_2 + 
          c(L13a,L13a)*var1_3
          
LV2    =~ c(L21a,L21a)*var2_1 + 
          c(L22a,L22a)*var2_2 + 
          c(L23a,L23a)*var2_3 
          
LV3    =~ c(L31a,L31a)*var3_1 + 
          c(L32a,L32a)*var3_2 + 
          c(L33a,L33a)*var3_3 

# Phantom Constructs
## Latent Construct Standard Deviations ##

Ph_LV1=~c(SD1a,SD1b)*LV1
Ph_LV2=~c(SD2a,SD2b)*LV2
Ph_LV3=~c(SD3a,SD3b)*LV3

# Latent Variance and Covariance

# Fix lower-order latent variance to zero
LV1~~c(0,0)*LV1
LV2~~c(0,0)*LV2
LV3~~c(0,0)*LV3

# Fix higher-order phantom construct variance to 1
# Standardizes the covariances like fixed factor

Ph_LV1~~c(1,1)*Ph_LV1
Ph_LV2~~c(1,1)*Ph_LV2
Ph_LV3~~c(1,1)*Ph_LV3

# Fix all unrelated between level covariances to zero

LV1~~c(0,0)*Ph_LV2
LV1~~c(0,0)*Ph_LV3
LV2~~c(0,0)*Ph_LV1
LV2~~c(0,0)*Ph_LV3
LV3~~c(0,0)*Ph_LV2
LV3~~c(0,0)*Ph_LV1

# Freely estimate the covariance between phantom constructs

## will be the correlations among latent constructs ##

Ph_LV1~~c(NA,NA)*Ph_LV2
Ph_LV1~~c(NA,NA)*Ph_LV3
Ph_LV2~~c(NA,NA)*Ph_LV3

# Intercepts

# Estemate latent variable intercepts
## These are your latent means ##

LV1 ~c(NA,NA)*1
LV2 ~c(NA,NA)*1
LV3 ~c(NA,NA)*1

## Fix phantom intercepts to zero ##

Ph_LV1 ~c(0,0)*1
Ph_LV2 ~c(0,0)*1
Ph_LV3 ~c(0,0)*1

## Label measured intercepts for effects coding

var1_1   ~ c(T11a,T11a)*1
var1_2   ~ c(T12a,T12a)*1
var1_3   ~ c(T13a,T13a)*1
var2_1   ~ c(T21a,T21a)*1
var2_2   ~ c(T22a,T22a)*1
var2_3   ~ c(T23a,T23a)*1
var3_1   ~ c(T31a,T31a)*1
var3_2   ~ c(T32a,T32a)*1
var3_3   ~ c(T33a,T33a)*1

# Model Constraints

L11a==3-L12a-L13a
L21a==3-L22a-L23a
L31a==3-L32a-L33a

## Commented out due to strong invariance
# L11b==3-L12b-L13b
# L21b==3-L22b-L23b
# L31b==3-L32b-L33b

T11a==0-T12a-T13a
T21a==0-T22a-T23a
T31a==0-T32a-T33a

## Commented out due to strong invariance
# T11b==0-T12b-T13b
# T21b==0-T22b-T23b
# T31b==0-T32b-T33b
'

## We can now use cfa() function again
phantomfit<- cfa(model=phantom, 
                 data=dat, 
                 missing="FIML", 
                 estimator="ML", 
                 std.lv=TRUE, 
                 group = "class",
                 meanstructure=TRUE)

summary(phantomfit, 
        fit.measures=T,
        standardized=T,
        rsquare=F)
