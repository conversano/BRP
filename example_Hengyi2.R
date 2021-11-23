
rm(list=ls(all=TRUE))
set.seed(43)

set.seed(kk)
n<-400
Z1<-c(rnorm(n,-2,1),rnorm(n,2,1))
Z2<-c(rnorm(n,-3,1),rnorm(n,-2,1))
Z2<-sample(Z2,2*n, replace = F)
Z3<-c(rnorm(n,3,1),rnorm(n,2,1))
 Z4<-c(rnorm(n,-2,1),rnorm(n,1,1))
 Z5<-c(runif(n,-4,1),runif(n,0,5))
 Z5<-sample(Z5,2*n, replace = F)
a<-rbinom(2*n,1,.25)
b<-as.vector(rmultinom(300, size = 2, prob = c(0.33,0.33,0.34)))[1:(2*n)]
c<-as.vector(rmultinom(300, size = 3, prob = c(0.25,0.25,0.25,0.25)))[1:(2*n)]
t1<-c(rep(0,500),rep(1,300))
t2<-c(rep(0,n/2),rep(1,n/2),rep(0,n/2),rep(1,n/2))
eps<-rnorm(2*n,0,1)

y<-1-2*t1+3*t2+(t1*t2)+1.5*Z1+3.5*Z3+1.5*Z4-4*(I(Z3>0 & Z4<0.75))+eps
summary(y)

summary(lm(y~Z1+Z2+Z3+Z4+Z5+t1*t2))

summary(y)

dd<-data.frame(cbind(y,t1,Z1,Z2,Z3,Z4,Z5,a,b,c,t2))
head(dd)

dd$a<-factor(dd$a, labels=c("a1","a2"))
dd$b<-factor(dd$b, labels=c("b1","b2", "b3"))
dd$c<-factor(dd$c, labels=c("c1","c2","c3","c4"))
dd$t2<-factor(dd$t2, labels=c("t2_1","t2_2"))

head(dd)
OneTree<-brp(data=dd[,c(3:10,2,1)], 
             Nmin2.d = sqrt(nrow(dd)),
             metric = "asam",
             threshold.d = 0.001)

names(OneTree) 

OneTree$tree

names(dd)
xx<-brp.aeme2(data=dd, 
              Nmin2.d = sqrt(nrow(dd)),
              threshold.d = 0.001,
              B = 25,
              treat=2,
              num.cov=3:10,
              outcome = 1,
              trace=TRUE
) # comment

#xx.old<-xx

xx$aeme2[which.min(xx$diff.aeme[-1])]

xx$obj
xx$ind.mat
par(mfrow=c(1,1))
plot(xx$aeme2,type="b")
lines(1:25,rep(xx$obj,25),col="red")

set.seed(17)
t3<-rbinom(n*2,1,.5)
#t3<-dd$t1+t3
#t3[t3>0]<-1

dd$t3<-t3
names(dd)

#dd<-dd[,c(1:2,11:12,3:10)]
#head(dd)


aeme3C1<-brp.aeme3(data=dd,outcome = 1, 
                   num.cov = 3:10, 
                   cov.treat = c(2,11:12),
                   it=25, trace = TRUE)

names(aeme3C1)

aeme3C1$aeme3
aeme3C1$effR1[24]
aeme3C1$effG1[24]
aeme3C1$effRG1[24]
aeme3C1$effR0[24]
aeme3C1$effG0[24]
aeme3C1$effRG0[24]

dd$t4<-rbinom(n*2,1,.5)
names(dd)
#names(dd)[1]<-"Y"

aeme4C2<-brp.aeme4(data=dd,
                   num.cov = 3:10,
                   cov.treat = c(2,11:13), 
                   it=25, trace=TRUE,
                   outcome = 1)
names(aeme4C2)[24]

aeme4C2$aeme4

par(mfrow=c(1,1))
plot(aeme4C2$aeme4,type="b")
lines(1:25,rep(xx$obj,25),col="red")


library(overlapping)

? overlap
example<-overlap(list(dd$Z1[dd$t1==0],dd$Z1[dd$t1==1]))
example<-overlap(list(dd$Z1[dd$t1==0],dd$Z1[dd$t1==1]), plot=TRUE)
example$OV
