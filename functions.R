brp.aeme3 <- function(data,
                      outcome = NULL,
                      num.cov = NULL,
                      cov.treat = NULL,
                      it = 5,
                      trace = FALSE)
{
  if (isFALSE(trace))    tt <- FALSE
  if (isFALSE(trace) == FALSE) tt <- TRUE
  for (i in 1:length(cov.treat))
    if (is.factor(data[, cov.treat[i]]))
      data[, cov.treat[i]] <- as.numeric(data[, cov.treat[i]]) - 1
    dd <- data[, c(outcome, cov.treat, num.cov)]
    colnames(dd)[outcome] <- "Y"
    
    d000 <- dd[dd[, 2] == 0 & dd[, 3] == 0 & dd[, 4] == 0, ]
    d001 <- dd[dd[, 2] == 0 & dd[, 3] == 0 & dd[, 4] == 1, ]
    
    d101 <- dd[dd[, 2] == 1 & dd[, 3] == 0 & dd[, 4] == 1, ]
    if (nrow(d101) == 0)      rrp101 <- rep(0, it)
    if (nrow(d101) > 0) {
      expD <- rbind(d001, d101)
      treat <- expD[, 2] + expD[, 3]
      treat[treat >= 1] <- 1
      dz <- cbind("Y" = expD[, outcome], expD[, -c(outcome, cov.treat)], "treat" = treat)
      cat("Estimating eff101", "\n")
      xx <- brp.aeme2(data = dz, B = it, outcome = 1, num.cov = 2:(ncol(dz) - 1), treat = ncol(dz))
      rrp101 <- xx$aeme2
    }
    cat(as.character(paste(c("eff101 = ", round(xx$aeme2[it], 3)))), "\n")
    effR1 <- rrp101
    d011 <- dd[dd[, 2] == 0 & dd[, 3] == 1 & dd[, 4] == 1, ]
    if (nrow(d011) == 0) rrp011 <- rep(0, it)
    if (nrow(d011) > 0) {
      expD <- rbind(d001, d011)
      treat <- expD[, 2] + expD[, 3]
      treat[treat >= 1] <- 1
      dz <- cbind("Y" = expD[, outcome], expD[, -c(outcome, cov.treat)],"treat" = treat)
      cat("Estimating eff011", "\n")
      xx <- brp.aeme2(data = dz, B = it, outcome = 1, num.cov = 2:(ncol(dz) - 1), treat = ncol(dz))
      rrp011 <- xx$aeme2
    }
    cat(as.character(paste(c("eff011 = ", round(xx$aeme2[it], 3)))), "\n")
    effG1 <- rrp011
    d111 <- dd[dd[, 2] == 1 & dd[, 3] == 1 & dd[, 4] == 1, ]
    if (nrow(d111) == 0) rrp111 <- rep(0, it)
    if (nrow(d111) > 0) {
      expD <- rbind(d111, d001)
      treat <- expD[, 2] + expD[, 3]
      treat[treat >= 1] <- 1
      dz <- cbind("Y" = expD[, outcome], expD[, -c(outcome, cov.treat)], "treat" = treat)
      cat("Estimating eff111", "\n")
      xx <- brp.aeme2(data = dz, B = it, outcome = 1, num.cov = 2:(ncol(dz) - 1),treat = ncol(dz))
      rrp111 <- xx$aeme2
    }
    cat(as.character(paste(c("eff111 = ", round(xx$aeme2[it], 3)))), "\n")
    effRG1 <- rrp111 - rrp101 - rrp011
    d000 <- dd[dd[, 2] == 0 & dd[, 3] == 0 & dd[, 4] == 0, ]
    rrp000 <- rep(mean(d000$Y), it)
    d100 <- dd[dd[, 2] == 1 & dd[, 3] == 0 & dd[, 4] == 0, ]
    if (nrow(d100) == 0) rrp100 <- rep(0, it)
    if (nrow(d100) > 0) {
      expD <- rbind(d000, d100)
      treat <- expD[, 2] + expD[, 3]
      treat[treat >= 1] <- 1
      dz <- cbind("Y" = expD[, outcome], expD[, -c(outcome, cov.treat)], "treat" = treat)
      cat("Estimating eff100", "\n")
      xx <- brp.aeme2(data = dz, B = it, outcome = 1, num.cov = 2:(ncol(dz) - 1),treat = ncol(dz))
      rrp100 <- xx$aeme2
    }
    cat(as.character(paste(c("eff100 = ", round(xx$aeme2[it], 3)))), "\n")
    effR0 <- rrp100
    d010 <- dd[dd[, 2] == 0 & dd[, 3] == 1 & dd[, 4] == 0, ]
    if (nrow(d010) == 0) rrp010 <- rep(0, it)
    if (nrow(d010) > 0) {
      expD <- rbind(d000, d010)
      treat <- expD[, 2] + expD[, 3]
      treat[treat >= 1] <- 1
      dz <- cbind("Y" = expD[, outcome], expD[, -c(outcome, cov.treat)],"treat" = treat)
      cat("Estimating eff010", "\n")
      xx <- brp.aeme2(data = dz, B = it, outcome = 1, num.cov = 2:(ncol(dz) - 1),treat = ncol(dz))
      rrp010 <- xx$aeme2
    }
    cat(as.character(paste(c("eff010 = ", round(xx$aeme2[it], 3)))), "\n")
    effG0 <- rrp010
    d110 <- dd[dd[, 2] == 1 & dd[, 3] == 1 & dd[, 4] == 0, ]
    if (nrow(d110) == 0) rrp110 <- rep(0, it)
    if (nrow(d110) > 0) {
      expD <- rbind(d110, d000)
      treat <- expD[, 2] + expD[, 3]
      treat[treat >= 1] <- 1
      dz <- cbind("Y" = expD[, outcome], expD[, -c(outcome, cov.treat)],"treat" = treat)
      cat("Estimating eff110", "\n")
      xx <- brp.aeme2(data = dz, B = it, outcome = 1, num.cov = 2:(ncol(dz) - 1), treat = ncol(dz))
      rrp110 <- xx$aeme2
    }
    cat(as.character(paste(c("eff110 = ", round(xx$aeme2[it], 3)))), "\n")
    effRG0 <- rrp110 - rrp100 - rrp010
    diff = (effR1[it] + effG1[it] - (effRG1[it])) - (effR0[it] + effG0[it] - (effRG0[it]))
    ris <- list("aeme3" = diff, "effR1" = effR1, "effG1" = effG1, "effRG1" = effRG1, "effR0" = effR0,
      "effG0" = effG0,"effRG0" = effRG0)
    return(ris)
}


brp.aeme2<-function(data, Nmin2.d = NULL, threshold.d = 0.05,
                   B = 50,
                   weight.scheme = "null",
                   psY = 0,treat="null",
                   num.cov="null",
                   outcome = "null",
                   trace=TRUE)
{
  B<-B+1
  rownames(data)<-as.numeric(1:nrow(data))
  Nmin2.d<-floor(sqrt(nrow(data)))
  data.sel<-data[,c(outcome,num.cov,treat)]
  colnames(data.sel)[ncol(data.sel)]<-colnames(data)[treat]<-"group"
  modelli<-nodi<-alberi<-list()
  bestnode.mat<-matrix(0,B,13)
  boot.mat<-pesi.mat<-ind.mat<-matrix(0, nrow(data),B)
  ps.mean.mat<-ps.mat<-matrix(NA,nrow(data),B)
  pesi.mat[,1]<-pesi<-rep(1,nrow(data))
  rrp.att<-rep(0,B)
  zz<-1
  y <- data.sel[,1]
  colnames(data.sel)[ncol(data.sel)]<-"group"
  group<-data.sel[, ncol(data.sel)]
  obj<-ate(y,group)
  while(zz <=B)
  {
    if(zz > 1) cat(paste(c("iteration ", zz-1,"\n")))
    if(zz>1) pesi<- pesi.mat[,zz-1]
    camp<-sample(1:nrow(data.sel), rep=T, prob=pesi)
    ind.mat[unique(camp),zz]<-rep(1,length(unique(camp)))
    dati2<-data.sel[camp,]
    if(psY == 1){
      eqt<-as.formula(dati2[,c(ncol(dati2),2:(ncol(dati2)-1))])
      glm1  <- glm(eqt, family=binomial,data=dati2) 
      modelli[[zz]]<-summary(glm1)
      ps <- glm1$fitted
      weight.scheme = "worst"
    }
    if(psY == 0) {
      ps <- rnorm(nrow(dati2))
      weight.scheme = "null"
      names(ps)<-as.character(camp)
    }
    ps.mat[unique(camp),zz]<-ps[unique(floor(sort(as.numeric(names(ps)))))]
    if(weight.scheme =="best") diff.ps<-abs(0.5-ps)
    if(weight.scheme =="worst") diff.ps<- 0.5-abs(0.5-ps)
    if(weight.scheme =="null") diff.ps<-rep(0,nrow(data))
    pesi.mat[unique(camp),zz]<- 
      pesi.mat[unique(camp),ifelse(zz>1,zz-1,zz)] + diff.ps[unique(camp)] 
    pesi.mat[-unique(camp),zz]<-pesi.mat[-unique(camp),ifelse(zz>1,zz-1,zz)] 
    dati3<-data.frame(cbind(dati2[,-1],"ps"=ps))
#    save(dati3, file = "datiTemp.RData")
    prova<-brp(data=dati3,Nmin2.d=Nmin2.d,metric="asam",
                    threshold.d=threshold.d)
    if(trace==TRUE & zz > 1) print(prova$tree)
    alberi[[zz]]<-prova$tree
    nodi[[zz]]<-prova$nodi
    if(length(prova$nodi) <= 1) 
      obs<-sort(unique(camp))
    if(length(prova$nodi) > 1)
    {
      xx<-as.matrix(prova$tree[prova$tree[,13]=="*",])
      if(dim(as.matrix(xx))[1]>0)
      {
        ind.row<-numeric()
        for(j in 1:nrow(xx))
        {
          if(is.na(as.numeric(xx[j,9]))) xx[j,9]<-"0"
          if(abs(as.numeric(xx[j,9]))>=threshold.d) ind.row<-c(ind.row,j)
        }
        if(length(ind.row)>=nrow(xx) | length(ind.row)==0) obs<-sort(unique(camp))
        if(length(ind.row)>0 & length(ind.row)<nrow(xx))
        {
          bestnode.mat[zz,]<-
            as.numeric(xx[which(abs(as.numeric(xx[,9]))==min(abs(as.numeric(xx[,9]))))[1],])
          node<-as.numeric(xx[-ind.row,1])
          obs<-numeric()
          for(k in 1:length(node)) 
            obs<-unique(sort(c(obs,unique(floor(as.numeric(rownames(prova$nodi[[node[k]]])))))))
          if(length(obs)==0) obs<-sort(unique(camp))
        }
        if(length(obs)>0)
        {
          boot.mat[obs,zz]<-1
          for(r in 1:length(obs)) ps.mean.mat[obs[r],zz]<-mean(ps.mat[obs[r],1:zz], na.rm = T)
          if(zz > 1){
            for(k in 1:nrow(ps.mean.mat)) 
              if(is.na(ps.mean.mat[k,zz])) 
                ps.mean.mat[k,zz]<- ps.mean.mat[k,zz-1]
              freqrel<-apply(boot.mat[,1:zz],1, sum)/zz
              ps<-ps.mean.mat[,zz]
          }
        }
      }
    }
    zz<-zz+1
    
    if(zz == (B+1)){
      zz<-zz-1
      p.mat<-matrix(0, length(data$group[data$group==1]), 
                    length(data$group[data$group==0]))
      nrow_p.mat = nrow(p.mat)
      ll_i <- length(data$group[data$group==1])
      ll_u <- length(data$group[data$group==0])
      for(kk in 2:zz)
      {
        if(ll_u > 1) 
          p.mat <- ind.mat[1:ll_i,1:kk]%*%
            t(ind.mat[(ll_i+1):nrow(ind.mat),1:kk])/kk
        if(ll_u == 1) 
          p.mat <- as.matrix(colSums(t(ind.mat[1:ll_i,1:kk])*
                                       ind.mat[(ll_i+1):nrow(ind.mat),1:kk])/kk)
        tot.row<-apply(p.mat,1,sum)
        p.mat2<-p.mat
        for(i in 1:nrow(p.mat)) 
          p.mat2[i,]<-ifelse(is.na(p.mat[i,]/tot.row[i]),0,
                             p.mat[i,]/tot.row[i])
        terms<-rep(0,nrow(p.mat))
        Y<-data[,1]
        for(i in 1:nrow(p.mat)) terms[i]<- 
          Y[data$group==1][i]-sum(p.mat2[i,]*Y[data$group==0])
        rrp.att[kk]<-sum(terms)/length(terms)
        kk <- kk+1
      }
      ris<-list("aeme2"= rrp.att[-1],
                "boot.mat" = boot.mat,
                "obj" = obj
      )
      return(ris)
    }            
  }
}

ate<-function(y,group){
  if(is.factor(group)) group<-as.numeric(group)-1
  mean(y[group==1], na.rm=T)-mean(y[group==0], na.rm=T)
}

brp<-function(data,Nmin2.d,metric="asam",threshold.d)
{
  colnames(data)[length(colnames(data))-1]<-"group"
  kk<-1
  nodes <- list()
  split.mat <- matrix(1, 1, 13, byrow = T)
  nodes[[1]] <- data
  split.var<-matrix(0,ncol(data)-2,6)
  cut.ind<-list()
  Nmin2.d<-floor(sqrt(nrow(data)))  
  for(i in 1:(ncol(data)-2)){
    if(length(unique(data[,i]))>1){
      d<-data[,c(i,(ncol(data)-1):ncol(data))]
      colnames(d)<-c("x","group","ps")
      x<-splitnodo3(d,Nmin2.d,metric="asam",threshold.d)
      if(is.numeric(x$cut)) split.var[i,]<-c(x$broot, x$best_bleft, 
                                             x$best_bright, min(x$best_bleft, x$best_bright), 
                                             x$cut, colnames(data)[i])
      
      if(is.numeric(x$cut)==F){
        split.var[i,]<-c(x$broot, x$best_bleft, 
                         x$best_bright, max(x$best_bleft, x$best_bright), 
                         NA, colnames(data)[i])
        cut.ind[[i]]<-c(names(data)[i],x$cut)}
    }
    if(length(unique(data[,i]))<=1) split.var[i,4]<-9999
  }  
  var.split<-split.var[which(split.var[,4]==min(split.var[,4])),]
  if(is.matrix(var.split)) var.split<-var.split[1,]
  split.mat[1,]<-c(1, nrow(data), sum(1-data[,"group"]), sum(data[,"group"]), 
                   round(mean(data[data[,"group"]==0,ncol(data)]),4), 	  									
                   round(mean(data[data[,"group"]==1,ncol(data)]),4),
                   round(sd(data[data[,"group"]==0,ncol(data)]),4), 
                   round(sd(data[data[,"group"]==1,ncol(data)]),4), 
                   var.split[-4])
  if(is.factor(data[,split.mat[1,13]])){
    xfact<-NA
    for(j in 1:length(cut.ind)) 
      if(is.null(cut.ind[[j]])==F){ 
        if(cut.ind[[j]][1]==split.mat[1,13]) 
          xfact<-cut.ind[[j]][-1]}
    if(is.na(xfact)==F)
      split.mat[1,12]<-paste(xfact, sep="", collapse="+")
  } 
  split.mat
  if(
    is.na(split.mat[1,12])==F
  ){
    if(
      is.numeric(data[,split.mat[nrow(split.mat),13]])
    ){
      nodes[[kk*2]]<-data[data[,split.mat[nrow(split.mat),13]]<= as.numeric(split.mat[nrow(split.mat),12]),]
      nodes[[kk*2+1]]<-data[data[,split.mat[nrow(split.mat),13]]> as.numeric(split.mat[nrow(split.mat),12]),]
    }
    if(
      is.factor(data[,split.mat[nrow(split.mat),13]])
    ){ 
      if(is.na(xfact)==F){
        xlab<-as.vector(unlist(strsplit(split.mat[nrow(split.mat),12],"[+]")))
        ind<-numeric()
        for(kkk in 1:length(xlab)) 
          ind<-c(ind, which(data[,split.mat[nrow(split.mat),13]]==xlab[kkk]))
        nodes[[kk*2]]<-data[ind,]
        nodes[[kk*2+1]]<-data[-ind,]
      }}
    kk <- 2
    splitnodes<-c(2,3)
    
    while(length(splitnodes) >0){
      n<-splitnodes[1]
      splitnodes<-splitnodes[-1]
      data <- nodes[n][[1]]
      ind.col<-numeric()
      for(i in 1:(ncol(data)-2)) 
        if(length(unique(data[,i]))>1) 
          ind.col<-c(ind.col,i)
      for(i in ind.col)
        if(is.factor(data[,i]) & nrow(table(data[,i]))!=length(unique(data[,i])))
          data[,i]<-factor(as.character(data[,i]))
      split.var<-matrix(NA,(ncol(data)-2),6)
      for(i in ind.col){
        d<-data[,c(i,(ncol(data)-1):ncol(data))]
        colnames(d)<-c("x","group","ps")
        x<-splitnodo3(d,Nmin2.d,metric="asam",threshold.d)
        if(is.numeric(x$cut)) split.var[i,]<-c(x$broot, x$best_bleft, 
                                               x$best_bright, 
                                               min(x$best_bleft, 
                                                   x$best_bright), 
                                               x$cut, colnames(data[i]))
        if(is.numeric(x$cut)==F){
          split.var[i,]<-c(x$broot, x$best_bleft, 
                           x$best_bright, min(x$best_bleft, x$best_bright), 
                           NA, colnames(data[i]))
          cut.ind[[i]]<-c(names(data)[i],x$cut)}
      }  
      if(
        is.matrix(split.var)
      ){
        split.var<-split.var[which(rowSums(is.na(split.var))<6),]
        var.split<-split.var[which(split.var[,4]==min(split.var[,4]))[1],]
      }
      if(
        is.matrix(var.split)
      ){
        if(nrow(var.split)>1) 
          var.split<-
            as.vector(var.split[which(apply(is.na(var.split),1,sum)==1)[1],])
      } 
      csm<-0
      if(
        is.null(var.split)==F
      ) 
        if(
          sum(abs(as.numeric(var.split[1])))>0
        ){
          split.mat<-rbind(split.mat,c(n,nrow(data), sum(1-data[,"group"]), 
                                       sum(data[,"group"]), 
                                       round(mean(data[data[,"group"]==0,ncol(data)]),4), 										round(mean(data[data[,"group"]==1,ncol(data)]),4),
                                       round(sd(data[data[,"group"]==0,ncol(data)]),4), 
                                       round(sd(data[data[,"group"]==1,ncol(data)]),4), 
                                       var.split[-4]))
          csm<-1
        }      
      if(
        sum(as.numeric(var.split[1]))==0 & 
        is.numeric(data[,var.split[6]])==T & 
        csm == 0){
        split.mat<-rbind(split.mat,
                         c(n, nrow(data), sum(1-data[,"group"]), 
                           sum(data[,"group"]), 
                           round(mean(data[data[,"group"]==0,ncol(data)]),4), 										
                           round(mean(data[data[,"group"]==1,ncol(data)]),4),
                           round(sd(data[data[,"group"]==0,ncol(data)]),4), 
                           round(sd(data[data[,"group"]==1,ncol(data)]),4), 
                           rep(NA,5)))
      }      
      
      if(sum(as.numeric(var.split[2:4]))==-Inf & csm==0){
        split.mat<-rbind(split.mat,
                         c(n, nrow(data), sum(1-data[,"group"]), 
                           sum(data[,"group"]), 
                           round(mean(data[data[,"group"]==0,ncol(data)]),4), 										
                           round(mean(data[data[,"group"]==1,ncol(data)]),4),
                           round(sd(data[data[,"group"]==0,ncol(data)]),4), 
                           round(sd(data[data[,"group"]==1,ncol(data)]),4), 
                           rep(NA,5)))
      }
      split.mat
      if(
        csm==1 & sum(as.numeric(var.split[2:4]))!=-Inf
      ){
        if(
          is.na(split.mat[nrow(split.mat),12])==T & 
          is.factor(data[,split.mat[nrow(split.mat),13]])==T
        ){
          xfact<-NA
          for(j in 1:length(cut.ind)) 
            if(is.null(cut.ind[[j]])==F){
              if(cut.ind[[j]][1]==split.mat[nrow(split.mat),13])
                xfact<-cut.ind[[j]][-1]            
            }  
          if(is.na(xfact)==F)
            if(as.numeric(split.mat[nrow(split.mat),9]) >= as.numeric(split.mat[nrow(split.mat),10]) |
               as.numeric(split.mat[nrow(split.mat),9]) >= as.numeric(split.mat[nrow(split.mat),11]))
              split.mat[nrow(split.mat),12]<-paste(xfact, sep="", collapse="+")
        } 
      }
      split.mat
      if(
        csm==1 & sum(as.numeric(var.split[2:4]))!=-Inf
      ){
        if(
          is.na(split.mat[nrow(split.mat),12])==F & split.mat[
            nrow(split.mat),12]!="-Inf"
        ){
          if(
            is.na(split.mat[nrow(split.mat),12])==F & 
            sum(as.numeric(var.split[2:4]))>0
          ){
            if(
              is.numeric(data[,split.mat[nrow(split.mat),13]]) &
              as.numeric(split.mat[nrow(split.mat),10]) < 
              as.numeric(split.mat[nrow(split.mat),9]) |
              as.numeric(split.mat[nrow(split.mat),11]) < 
              as.numeric(split.mat[nrow(split.mat),9])
            ){
              nodes[[n*2]]<-data[data[colnames(data)==var.split[6]]<= 
                                   as.numeric(split.mat[nrow(split.mat),12]),]
              nodes[[(n*2)+1]]<-data[data[colnames(data)==var.split[6]] > 
                                       as.numeric(split.mat[nrow(split.mat),12]),]
              splitnodes<-c(splitnodes,n*2,n*2+1)
            }
            if(
              is.numeric(data[,split.mat[nrow(split.mat),13]]) &
              as.numeric(split.mat[nrow(split.mat),10]) >= 
              as.numeric(split.mat[nrow(split.mat),9]) &
              as.numeric(split.mat[nrow(split.mat),11]) >= 
              as.numeric(split.mat[nrow(split.mat),9]))
              split.mat[nrow(split.mat),12]<-NA
            if(
              is.factor(data[,split.mat[nrow(split.mat),13]])
            )
              if(
                is.na(xfact)==F
              ){
                xlab<-as.vector(unlist(strsplit(split.mat[nrow(split.mat),12],"[+]")))
                ind<-numeric()
                for(kk in 1:length(xlab)) 
                  ind<-c(ind, which(data[,split.mat[nrow(split.mat),13]]==xlab[kk]))
                nodes[[n*2]]<-data[ind,]
                nodes[[(n*2)+1]]<-data[-ind,]
                splitnodes<-c(splitnodes,n*2,n*2+1)
              }
          }
        }
      }
    }
  }
  rownames(split.mat)<-NULL
  for(i in 9:11)
    split.mat[,i]<-round(as.numeric(split.mat[,i]),3)
  colnames(split.mat)<-c("node","n","n.c","n.t","mean.Y.c","mean.Y.t","sd.Y.c","sd.Y.t", "ASAM(P)", "ASAM(L)", "ASAM(R)",
                         "split.value", "split.var")
  for(i in 1:nrow(split.mat))
    if(is.na(split.mat[i,12])==T | split.mat[i,12]=="-Inf")  
      split.mat[i,10:ncol(split.mat)]<-c(rep("",3),"*")	
  for(i in 1:nrow(split.mat))
    for(j in 1:ncol(split.mat))
      if(is.na(split.mat[i,j])==T)  split.mat[i,j]<-"0"	
  split.mat2<-as.data.frame(split.mat)
  output<-list("tree"=split.mat2, "nodes"=nodes)
  return(output)
}	

ns<-function(x){
  if(is.factor(x)){
    xx<-as.numeric(x)
    out<-list()
    for(i in 1:(floor(length(xx)/2)))
      out[[i]]<-t(combn(xx,i))
  }
  ssr<-ssl<-list()
  ii<-1
  for(j in i:1){
    for(k in 1:nrow(as.matrix(out[[j]]))){
      ssr[[ii]]<-x[as.matrix(out[[j]])[k,]]
      ssl[[ii]]<-x[-as.matrix(out[[j]])[k,]]
      ii<-ii+1
    }
  }
  out<-list(ssr,ssl) 
  return(out)
}



splitnodo3<-function(d,Nmin2.d,metric,threshold.d)
{
  cut<-NA
  dleft<-dright<-list();  bleft<-bright<-vector()
  if(metric=="asam"){
    broot<-f_asam2(d)	
    if(is.numeric(d[,"x"])){
      sx<-sort(d[,"x"])
      for (i in 1:length(sx)) {
        dleft[[i]] <-d[d[,"x"]<=sx[i],]
        ifelse(sum(dleft[[i]][,"group"])>Nmin2.d 
               & sum(1-dleft[[i]][,"group"])>Nmin2.d
               & sum(d[,"group"])-sum(dleft[[i]][,"group"])>Nmin2.d 
               & sum(1-d[,"group"])-sum(1-dleft[[i]][,"group"])>Nmin2.d,
               bleft[[i]]<-f_asam2(dleft[[i]]),bleft[[i]]<-NA)
      }
      for (j in 1:length(sx)){
        dright[[j]] <-d[d[,"x"]>sx[j],]
        ifelse(sum(dright[[j]][,"group"])>Nmin2.d & 
                 sum(1-dright[[j]][,"group"])>Nmin2.d
               & sum(d[,"group"])-sum(dright[[j]][,"group"])>Nmin2.d & 
                 sum(1-d[,"group"])-sum(1-dright[[j]][,"group"])>Nmin2.d,
               bright[[j]]<-f_asam2(dright[[j]]),bright[[j]]<-NA )
      }
      xbestleft  <-sx[which(abs(bleft)==min(abs(bleft),na.rm=T))][1]
      xbestright <-sx[which(abs(bright)==min(abs(bright),na.rm=T))][1]
      minsplit<-min(min(abs(bleft),na.rm=T)[1], min(abs(bright),na.rm=T)[1])
      xbest  <-ifelse(min(abs(bright),na.rm=T)>min(abs(bleft),na.rm=T),
                      xbestleft,xbestright)
      cut<-ifelse(broot > minsplit, xbest,NA)
    }
    if(is.factor(d[,"x"])){
      if(length(unique(d[,"x"]))>10) stop("You cannot use categorical variables with more than 10 levels")
      yy<-d[,"x"]; xx<-ns(x=unique(yy)); 
      nsplit<-length(xx[[1]])
      for (i in 1:nsplit) {
        sub<-numeric()
        for(k in 1:length(unlist(xx[[1]][i]))) 
          sub<-c(sub, which(yy==unlist(xx[[1]][i])[k]))
        dleft[[i]] <-d[sub,]
        ifelse(sum(dleft[[i]][,"group"])>Nmin2.d & 
                 sum(1-dleft[[i]][,"group"])>Nmin2.d &
                 sum(d[,"group"])-sum(dleft[[i]][,"group"])>Nmin2.d & 
                 sum(1-d[,"group"])-sum(1-dleft[[i]][,"group"])>Nmin2.d,
               bleft[[i]]<-f_asam2(dleft[[i]]),bleft[i]<-NA )
        dright[[i]] <-d[-sub,]
        ifelse(sum(dright[[i]][,"group"])>Nmin2.d & 
                 sum(1-dright[[i]][,"group"])>Nmin2.d &
                 sum(d[,"group"])-sum(dright[[i]][,"group"])>Nmin2.d & 
                 sum(1-d[,"group"])-sum(1-dright[[i]][,"group"])>Nmin2.d,
               bright[[i]]<-f_asam2(dright[[i]]),bright[i]<-NA )
        rm(sub)
      }
      xbestleft<-c(which(abs(bleft)==min(abs(bleft),na.rm=T))[1],
                   min(abs(bleft),na.rm=T))
      xbestright <-c(which(abs(bright)==min(abs(bright),na.rm=T))[1],
                     min(abs(bright),na.rm=T))
      xbest  <-ifelse(min(abs(bright),na.rm=T)<min(abs(bleft),na.rm=T),
                      xbestright[1],xbestleft[1])
      if(is.na(xbest[1])==F & 
         abs(broot) > ifelse(xbestleft[1]==xbest,xbestleft[2],xbestright[2]))  
        cut<-paste(as.character(unlist(xx[[1]][xbest])), sep="", collapse="+")
      xbestleft<-xbestleft[2]
      xbestright<-xbestright[2]
    }
  }
  output<-list(
    "ps"=d$ps,"bleft"=bleft,"bright"=bright,
    "broot"=broot,"best_bleft"=max(abs(bleft),na.rm=T),
    "best_bright"=max(abs(bright),na.rm=T),
    "xbestleft"=xbestleft,"xbestright"=xbestright,
    "cut"=cut
  )
  return(output)	
}

f_asam2<-function(d){
  x1<-d[d[,"group"]==1,"ps"];x0<-d[d[,"group"]==0,"ps"]
  asam<-(mean(x1)-mean(x0))/sqrt((var(x1)+var(x0))/2)
  return(asam)
}



brp.aeme4<-function(data,outcome=NULL,
                      num.cov = NULL,
                      cov.treat = NULL, 
                      it=5, trace=FALSE)
{
  if(isFALSE(trace)) tt<-FALSE
  if(isFALSE(trace)==FALSE) tt<-TRUE
  for(i in 1:length(cov.treat))
    if(is.factor(data[,cov.treat[i]]))  
      data[,cov.treat[i]]<-as.numeric(data[,cov.treat[i]])-1
    dd<-data[,c(outcome,cov.treat,num.cov)]
    colnames(dd)[outcome]<-"Y"
    
    d0000<-dd[dd[,2]==0 & dd[,3]==0 & dd[,4]==0 & dd[,5]==0,]
    m0000<-mean(d0000$Y)
    
    d0001<-dd[dd[,2]==0 & dd[,3]==0 & dd[,4]==0 & dd[,5]==1,]
    m0001<-mean(d0001$Y)
    
    d1001<-dd[dd[,2]==1 & dd[,3]==0 & dd[,4]==0 & dd[,5]==1,]
    if(nrow(d1001)==0) rrp1001<-rep(0,it)
    if(nrow(d1001)>0){
      expD<-rbind(d0001,d1001)
      treat<-expD[,2]+expD[,3]+expD[,4]
      treat[treat>=1]<-1
      dz<-cbind("Y"=expD[,outcome],expD[,-c(outcome,cov.treat)], 
                "treat"=treat)
      cat("Estimating eff1001", "\n")
      xx<-brp.aeme2(data=dz, B=it, outcome=1,
                   num.cov = 2:(ncol(dz)-1),treat = ncol(dz),
                   threshold.d = 0.05, 
                   Nmin2.d = floor(sqrt(nrow(dz))),
                   weight.scheme = "null",
                   psY = 0, trace=tt)
      rrp1001<-xx$aeme2
    }
    cat(as.character(paste(c("eff1001 = ", 
                             round(xx$aeme2[it],3)))), "\n")
    effR1<-rrp1001
    
    d0101<-dd[dd[,2]==0 & dd[,3]==1 & dd[,4]==0 & dd[,5]==1,]
    if(nrow(d0101)==0) rrp0101<-rep(0,it)
    if(nrow(d0101)>0){
      expD<-rbind(d0001,d0101)
      treat<-expD[,2]+expD[,3]+expD[,4]
      treat[treat>=1]<-1
      dz<-cbind("Y"=expD[,outcome],expD[,-c(outcome,cov.treat)], 
                "treat"=treat)
      cat("Estimating eff0101", "\n")
      xx<-brp.aeme2(data=dz, B=it, outcome=1,
                   num.cov = 2:(ncol(dz)-1),treat = ncol(dz),
                   threshold.d = 0.05, 
                   Nmin2.d = floor(sqrt(nrow(dz))),
                   weight.scheme = "null",
                   psY = 0, trace=tt)
      rrp0101<-xx$aeme2
    }
    cat(as.character(paste(c("eff0101 = ", 
                             round(xx$aeme2[it],3)))), "\n")
    effG1<-rrp0101
    
    d0011<-dd[dd[,2]==0 & dd[,3]==0 & dd[,4]==1 & dd[,5]==1,]
    if(nrow(d0011)==0) rrp0011<-rep(0,it)
    if(nrow(d0011)>0){
      expD<-rbind(d0001,d0011)
      treat<-expD[,2]+expD[,3]+expD[,4]
      treat[treat>=1]<-1
      dz<-cbind("Y"=expD[,outcome],expD[,-c(outcome,cov.treat)], 
                "treat"=treat)
      cat("Estimating eff0011", "\n")
      xx<-brp.aeme2(data=dz, B=it, outcome=1,
                   num.cov = 2:(ncol(dz)-1),treat = ncol(dz),
                   threshold.d = 0.05, 
                   Nmin2.d = floor(sqrt(nrow(dz))),
                   weight.scheme = "null",
                   psY = 0, trace=tt)
      rrp0011<-xx$aeme2
    }
    cat(as.character(paste(c("eff0011 = ", 
                             round(xx$aeme2[it],3)))), "\n")
    effC11<-rrp0011
    
    d1101<-dd[dd[,2]==1 & dd[,3]==1 & dd[,4]==0 & dd[,5]==1,]
    if(nrow(d1101)==0) rrp1101<-rep(0,it)
    if(nrow(d1101)>0){
      expD<-rbind(d0001,d1101)
      treat<-expD[,2]+expD[,3]+expD[,4]
      treat[treat>=1]<-1
      dz<-cbind("Y"=expD[,outcome],expD[,-c(outcome,cov.treat)], 
                "treat"=treat)
      cat("Estimating eff1101", "\n")
      xx<-brp.aeme2(data=dz, B=it, outcome=1,
                   num.cov = 2:(ncol(dz)-1),treat = ncol(dz),
                   threshold.d = 0.05, 
                   Nmin2.d = floor(sqrt(nrow(dz))),
                   weight.scheme = "null",
                   psY = 0, trace=tt)
      rrp1101<-xx$aeme2
    }
    cat(as.character(paste(c("eff1101 = ", 
                             round(xx$aeme2[it],3)))), "\n")
    effRG1 = ((rrp1101+m0001)-(rrp1001+m0001)-(rrp0101+m0001))+m0001
    cat(as.character(paste(c("eff1101 = ", round(effRG1[it],3)))), "\n")
    
    d1011<-dd[dd[,2]==1 & dd[,3]==0 & dd[,4]==1 & dd[,5]==1,]
    if(nrow(d1011)==0) rrp1011<-rep(0,it)
    if(nrow(d1011)>0){
      expD<-rbind(d0001,d1011)
      treat<-expD[,2]+expD[,3]+expD[,4]
      treat[treat>=1]<-1
      dz<-cbind("Y"=expD[,outcome],expD[,-c(outcome,cov.treat)], 
                "treat"=treat)
      cat("Estimating eff1011", "\n")
      xx<-brp.aeme2(data=dz, B=it, outcome=1,
                   num.cov = 2:(ncol(dz)-1),treat = ncol(dz),
                   threshold.d = 0.05, 
                   Nmin2.d = floor(sqrt(nrow(dz))),
                   weight.scheme = "null",
                   psY = 0, trace=tt)
      rrp1011<-xx$aeme2
    }
    cat(as.character(paste(c("eff1011 = ", 
                             round(xx$aeme2[it],3)))), "\n")
    effRC11 = ((rrp1011+m0001)-(rrp1001+m0001)-(rrp0011+m0001))+m0001
    cat(as.character(paste(c("eff1011 = ", round(effRC11[it],3)))), "\n")
    
    d0111<-dd[dd[,2]==0 & dd[,3]==1 & dd[,4]==1 & dd[,5]==1,]
    if(nrow(d0111)==0) rrp0111<-rep(0,it)
    if(nrow(d0111)>0){
      expD<-rbind(d0001,d0111)
      treat<-expD[,2]+expD[,3]+expD[,4]
      treat[treat>=1]<-1
      dz<-cbind("Y"=expD[,outcome],expD[,-c(outcome,cov.treat)], 
                "treat"=treat)
      cat("Estimating eff0111", "\n")
      xx<-brp.aeme2(data=dz, B=it, outcome=1,
                   num.cov = 2:(ncol(dz)-1),treat = ncol(dz),
                   threshold.d = 0.05, 
                   Nmin2.d = floor(sqrt(nrow(dz))),
                   weight.scheme = "null",
                   psY = 0, trace=tt)
      rrp0111<-xx$aeme2
    }
    cat(as.character(paste(c("eff0111 = ", 
                             round(xx$aeme2[it],3)))), "\n")
    effGC11 = ((rrp0111+m0001)-(rrp0101+m0001)-(rrp0011+m0001))+m0001
    cat(as.character(paste(c("eff0111 = ", round(effGC11[it],3)))), "\n")
    
    d1000<-dd[dd[,2]==1 & dd[,3]==0 & dd[,4]==0 & dd[,5]==0,]
    if(nrow(d1000)==0) rrp1000<-rep(0,it)
    if(nrow(d1000)>0){
      expD<-rbind(d0000,d1000)
      treat<-expD[,2]+expD[,3]+expD[,4]
      treat[treat>=1]<-1
      dz<-cbind("Y"=expD[,outcome],expD[,-c(outcome,cov.treat)], 
                "treat"=treat)
      cat("Estimating eff1000", "\n")
      xx<-brp.aeme2(data=dz, B=it, outcome=1,
                   num.cov = 2:(ncol(dz)-1),treat = ncol(dz),
                   threshold.d = 0.05, 
                   Nmin2.d = floor(sqrt(nrow(dz))),
                   weight.scheme = "null",
                   psY = 0, trace=tt)
      rrp1000<-xx$aeme2
    }
    cat(as.character(paste(c("eff1000 = ", 
                             round(xx$aeme2[it],3)))), "\n")
    effR0<-rrp1000
    
    d0100<-dd[dd[,2]==0 & dd[,3]==1 & dd[,4]==0 & dd[,5]==0,]
    if(nrow(d0100)==0) rrp0100<-rep(0,it)
    if(nrow(d0100)>0){
      expD<-rbind(d0000,d0100)
      treat<-expD[,2]+expD[,3]+expD[,4]
      treat[treat>=1]<-1
      dz<-cbind("Y"=expD[,outcome],expD[,-c(outcome,cov.treat)], 
                "treat"=treat)
      cat("Estimating eff0100", "\n")
      xx<-brp.aeme2(data=dz, B=it, outcome=1,
                   num.cov = 2:(ncol(dz)-1),treat = ncol(dz),
                   threshold.d = 0.05, 
                   Nmin2.d = floor(sqrt(nrow(dz))),
                   weight.scheme = "null",
                   psY = 0, trace=tt)
      rrp0100<-xx$aeme2
    }
    cat(as.character(paste(c("eff0100 = ", 
                             round(xx$aeme2[it],3)))), "\n")
    effG0<-rrp0100
    
    d0010<-dd[dd[,2]==0 & dd[,3]==0 & dd[,4]==1 & dd[,5]==0,]
    if(nrow(d0010)==0) rrp0010<-rep(0,it)
    if(nrow(d0010)>0){
      expD<-rbind(d0000,d0010)
      treat<-expD[,2]+expD[,3]+expD[,4]
      treat[treat>=1]<-1
      dz<-cbind("Y"=expD[,outcome],expD[,-c(outcome,cov.treat)], 
                "treat"=treat)
      cat("Estimating eff0010", "\n")
      xx<-brp.aeme2(data=dz, B=it, outcome=1,
                   num.cov = 2:(ncol(dz)-1),treat = ncol(dz),
                   threshold.d = 0.05, 
                   Nmin2.d = floor(sqrt(nrow(dz))),
                   weight.scheme = "null",
                   psY = 0, trace=tt)
      rrp0010<-xx$aeme2
    }
    cat(as.character(paste(c("eff0010 = ", 
                             round(xx$aeme2[it],3)))), "\n")
    effC10<-rrp0010
    
    d1100<-dd[dd[,2]==1 & dd[,3]==1 & dd[,4]==0 & dd[,5]==0,]
    if(nrow(d1100)==0) rrp1100<-rep(0,it)
    if(nrow(d1100)>0){
      expD<-rbind(d0000,d1100)
      treat<-expD[,2]+expD[,3]+expD[,4]
      treat[treat>=1]<-1
      dz<-cbind("Y"=expD[,outcome],expD[,-c(outcome,cov.treat)], 
                "treat"=treat)
      cat("Estimating eff1100", "\n")
      xx<-brp.aeme2(data=dz, B=it, outcome=1,
                   num.cov = 2:(ncol(dz)-1),treat = ncol(dz),
                   threshold.d = 0.05, 
                   Nmin2.d = floor(sqrt(nrow(dz))),
                   weight.scheme = "null",
                   psY = 0, trace=tt)
      rrp1100<-xx$aeme2
    }
    cat(as.character(paste(c("eff1100 = ", 
                             round(xx$aeme2[it],3)))), "\n")
    effRG0 = ((rrp1100+m0000)-(rrp1000+m0000)-(rrp0100+m0000))+m0000
    cat(as.character(paste(c("eff1100 = ", round(effRG0[it],3)))), "\n")
    
    d1010<-dd[dd[,2]==1 & dd[,3]==0 & dd[,4]==1 & dd[,5]==0,]
    if(nrow(d1010)==0) rrp1010<-rep(0,it)
    if(nrow(d1010)>0){
      expD<-rbind(d0000,d1010)
      treat<-expD[,2]+expD[,3]+expD[,4]
      treat[treat>=1]<-1
      dz<-cbind("Y"=expD[,outcome],expD[,-c(outcome,cov.treat)], 
                "treat"=treat)
      cat("Estimating eff1010", "\n")
      xx<-brp.aeme2(data=dz, B=it, outcome=1,
                   num.cov = 2:(ncol(dz)-1),treat = ncol(dz),
                   threshold.d = 0.05, 
                   Nmin2.d = floor(sqrt(nrow(dz))),
                   weight.scheme = "null",
                   psY = 0, trace=tt)
      rrp1010<-xx$aeme2
    }
    cat(as.character(paste(c("eff1010 = ", 
                             round(xx$aeme2[it],3)))), "\n")
    effRC10 = ((rrp1010+m0000)-(rrp1000+m0000)-(rrp0010+m0000))+m0000
    cat(as.character(paste(c("eff1010 = ", round(effRC10[it],3)))), "\n")
    
    d0110<-dd[dd[,2]==0 & dd[,3]==1 & dd[,4]==1 & dd[,5]==0,]
    if(nrow(d0110)==0) rrp0110<-rep(0,it)
    if(nrow(d0110)>0){
      expD<-rbind(d0000,d0110)
      treat<-expD[,2]+expD[,3]+expD[,4]
      treat[treat>=1]<-1
      dz<-cbind("Y"=expD[,outcome],expD[,-c(outcome,cov.treat)], 
                "treat"=treat)
      cat("Estimating eff0110", "\n")
      xx<-brp.aeme2(data=dz, B=it, outcome=1,
                   num.cov = 2:(ncol(dz)-1),treat = ncol(dz),
                   threshold.d = 0.05, 
                   Nmin2.d = floor(sqrt(nrow(dz))),
                   weight.scheme = "null",
                   psY = 0, trace=tt)
      rrp0110<-xx$aeme2
    }
    cat(as.character(paste(c("eff0110 = ", 
                             round(xx$aeme2[it],3)))), "\n")
    effGC10 = ((rrp0110+m0000)-(rrp0100+m0000)-(rrp0010+m0000))+m0000
    cat(as.character(paste(c("eff0110 = ", round(effGC10[it],3)))), "\n")

    effRGC11 = effR1 + effG1 + effC11 - (effRG1 + effRC11 + effGC11)
    cat(as.character(paste(c("eff1111 = ", round(effRGC11[it],3)))), "\n")
    
    effRGC10 = effR0 + effG0 + effC10 - (effRG0 + effRC10 + effGC10)
    cat(as.character(paste(c("eff1110 = ", round(effRGC10[it],3)))), "\n")
    
    ris.it<-data.frame(cbind("eff1000" = effR0, 
                             "eff0100"= effG0, 
                             "eff0010" = effC10,  
                             "eff1100" = effRG0,
                             "eff1010" = effRC10, 
                             "eff0110" = effGC10, 
                             "eff1110" = effRGC10,
                             "eff1001" = effR1, 
                             "eff0101"= effG1, 
                             "eff0011" = effC11,  
                             "eff1101" = effRG1,
                             "eff1011" = effRC11, 
                             "eff0111" = effGC11, 
                             "eff1111" = effRGC11,
                             "aeme4" = effRGC11-effRGC10
    ))
    return(ris.it)
}

