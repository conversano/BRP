
rm(list=ls(all=TRUE))
# Load the functions required to compute AEME and related statistics
source("Functions.r")
# Load an example dataset
load("Example_data.RData")

# The example_data contains the outcome variable Y, 
# a categorical variable R which acts as a hard to modify factor,
# a categorical variable G which acts as a non-modifiable factor, 
# two categorical variables C1 and C2 which are potential moderators,
# five numerical covariates Z1, Z2, Z3, Z4 and Z5
# three categorical covariates F1 (2 categories), F2 (3 categories) and F3 (4 categories).

summary(example_data) 

# The function brp grows a single BRP treeas described in Sec. 4.3.
# It requires the following arguments:
# - data: organized in such a way that the exposure variable is in the last but one column
#   and the outcome Y in the last column
# - Nmin2.d: is the minimum number of cases for a node to be split
# - metric: is the splitting criterion in the BRP tree
# - threshold.d: the minimum required decrease in ATT_W (tau parameter in the BRP Algorithm)

# The object OneTree is created in the following
OneTree<-brp(data=example_data[,c(6:13,2,1)], 
                  Nmin2.d = sqrt(nrow(example_data)),
                  metric = "asam",
                  threshold.d = 0.001)

names(OneTree) # It is a list composed of two elements

OneTree$tree # the tree element containes the BRP tree

# node   n  n.c  n.t  mean.Y.c  mean.Y.t sd.Y.c  sd.Y.t  ASAM(P) ASAM(L) ASAM(R) split.value split.var
# 1     800 500  300 -15.5121   -8.7665  6.1797  5.4655   1.156   1.177   0.188    1.99        Z1
# 2     595 451  144 -16.3515   -9.8284  5.7204  5.5385   1.159   1.002   0.322    0.73        Z1
# 3     205  49  156  -7.7861   -7.7863  4.7623  5.2259       0                                 *
# 4     443 414  29  -17.0652   -12.721  5.1919  5.1522    0.84                                 *
# 5     152  37 115   -8.3652   -9.0989  5.3277  5.4122  -0.137                                 *                

# In the brp tree each column denotes:
# node: node number
# n: number of cases in a node
# n.c: number of unexposed subjects
# n.t: number of exposed subjects
# mean.Y.c: average value of Y for the unexposed subjects
# mean.Y.t: average value of Y for the exposed subjects
# sd.Y.c: standard deviation of Y for the unexposed subjects
# sd.Y.t: standard deviation of Y for the exposed subjects
# ASAM(P): ASAM computed in the parent node (Eq. 10 for observations in the splitting node)
# ASAM(L): ASAM computed in the left-child node (Eq. 10 for observations in the left-child node)
# ASAM(R): ASAM computed in the right-child node (Eq. 10 for observations in the right-child node)
# split.value: the value/label used for splitting
# split.var: splitting variable

# The function brp.aeme2 estimates the exposure to causal moderation in the binary factor case (Eq. 4)
# In particular, the next command line estimates AEME(R) for the example_data.
# B is the number of data replications (indicated with r = 1, ..., R in the BRP algorithm)
# treat is the column indicator of the exposure variable
# num.cov is the column indicator of covariates
# trace = TRUE shows the BRP trees obtained in each data replication

xx<-brp.aeme2(example_data, 
              Nmin2.d = sqrt(nrow(example_data)),
              threshold.d = 0.001,
              B = 25,
              treat=2,
              num.cov=6:13,
              outcome = 1,
              trace=TRUE) 

# the element aeme2 of the object xx is the estimated aeme(R), Equation 4.
xx$aeme2

# The function brp.aeme3 estimates the AEME(R,G,C1) (Eq. 5) for the example_data.
# "it" is the number of data replications (indicated with r = 1, ..., R in the BRP algorithm)
# cov.treat is the columns indicator of the variables involved in the estimation of the moderation
# effect and the last one (fourth column) acts as moderator.

aeme3C1<-brp.aeme3(data=example_data,outcome = 1, 
                   num.cov = 6:13, 
                     cov.treat = 2:4,
                     it=25, trace = TRUE)

names(aeme3C1)

aeme3C1$aeme3 # AEME(R,G,C1)
aeme3C1$effR1[25] # AEME(R,C1=1)
aeme3C1$effG1[25] # AEME(G,C1=1)
aeme3C1$effRG1[25] # AEME(R,G,C1=1)
aeme3C1$effR0[25] # AEME(R,C1=0)
aeme3C1$effG0[25] # AEME(G,C1=0)
aeme3C1$effRG0[25] # AEME(R,G,C1=0)

# The function brp.aeme4 estimates the AEME(R,G,C1,C2) (Eq. 7) for the example_data.
# The arguments are the same of those used for brp.aeme3

aeme4C2<-brp.aeme4(data=example_data,outcome=1,
                    num.cov = 6:(ncol(example_data)),
                    cov.treat = 2:5, 
                    it=25, trace=TRUE)
names(aeme4C2)

aeme4C2$aeme4[25] # AEME(R,G,C1,C2)
### Single components of AEME(R,G,C1,C2)
aeme4C2$eff1000[25] # (R=1,G=0,C1=0,C2=0)
aeme4C2$eff0100[25] # (R=0,G=1,C1=0,C2=0)
aeme4C2$eff0010[25] # (R=0,G=0,C1=1,C2=0)
aeme4C2$eff1100[25] # (R=1,G=1,C1=0,C2=0)
aeme4C2$eff1010[25] # (R=1,G=0,C1=1,C2=0)
aeme4C2$eff0110[25] # (R=0,G=1,C1=1,C2=0)
aeme4C2$eff1110[25] # (R=1,G=1,C1=1,C2=0)
aeme4C2$eff1001[25] # (R=1,G=0,C1=0,C2=1)
aeme4C2$eff0101[25] # (R=0,G=1,C1=0,C2=1)
aeme4C2$eff0011[25] # (R=0,G=0,C1=1,C2=1)
aeme4C2$eff1101[25] # (R=1,G=1,C1=0,C2=1)
aeme4C2$eff1011[25] # (R=1,G=0,C1=1,C2=1)
aeme4C2$eff0111[25] # (R=0,G=1,C1=1,C2=1)
aeme4C2$eff1111[25] # (R=1,G=1,C1=1,C2=1)

