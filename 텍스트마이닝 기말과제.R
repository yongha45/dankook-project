setwd("C:/Users/yonghakim/Desktop/용하/데이터마이닝/dmba")

library(dplyr)

DTM <- read.csv("ReuterDTM.csv", header=T, row.names=1)
df <- DTM[,-1]
lbls <- data.frame(DTM[,1],row.names(DTM))
grps <- sort(unique(lbls[,1]))
grindexes <- lbls[,1]


counts <- function(df, grp) {
  tp <- colSums(grp*df>0)        
  fn <- colSums(grp*(df==0))     
  fp <- colSums((1-grp)*df>0)     
  tn <- colSums((1-grp)*(df==0)) 
  obspos <- tp + fp    
  obsneg <- tn + fn    
  cnts <-cbind(tp, fn, fp, tn, tp+fn, fp+tn, obspos, obsneg)
  colnames(cnts) <- c("TP", "FN", "FP", "TN","Pos", "Neg", "pos", "neg")
  cnts
}

chi2s <- function(df) {
  tp   <- df[,1];     fn   <- df[,2]
  fp   <- df[,3];     tn   <- df[,4]
  n1   <- tp + fn;    n0   <- fp + tn
  obs1 <- tp + fp;    obs0 <- fn + tn
  N    <- n1 + n0
  p11  <- tp / N;     p10  <- fn / N
  p01  <- fp / N;     p00  <- tn / N
  cnts <- cbind(tp, fn, fp, tn, N, p11, p10, p01, p00)
  # chi-squares
  chinum <- (n1+n0) * (tp*tn - fp*fn)^2
  chiden <- (tp+fp) * (fn+tn) * n1 * n0
  chi    <- ifelse(chiden==0, 0, chinum/chiden)
  res    <- cbind(cnts, chi)
  colnames(res) <- c("TP", "FN", "FP", "TN", "N","p11", "p10", "p01", "p00", "Chisq")
  res
}

for (nselw in (1:10)*10) {
slctrms <- matrix(ncol=length(grps),nrow=nselw)
colnames(slctrms) <- grps
for (ii in seq_along(grps)) {
grp   <- grindexes == grps[ii]
trnws <- counts(df, grp)
res   <- chi2s(trnws)
slctrms[,ii] <- names(sort(res[, "Chisq"],decreasing=T))[1:nselw]
}
write.csv(slctrms,paste(nselw," features.csv",sep=""))
print(length(unique(as.vector(slctrms))))
}

features20<-read.csv("20 features.csv")
View(features20)

library(naivebayes)
NBayes <- function(df, tests) {
  dtmat    <- as.matrix(df)
  traindat <- dtmat[-tests,]
  testdat  <- dtmat[tests,]
  trainy   <- lbls[-tests,1]
  testy    <- lbls[tests,1]
  trainnb  <- multinomial_naive_bayes(x=traindat, y=trainy)
  prednb   <- predict(trainnb, testdat)
  tblnb    <- table(data.frame(lbl=testy, pred=prednb))
  }

set.seed(32180871)
nobs <- dim(df)[1]
kcv  <- 4;      
tests <- sample(nobs, nobs/kcv)
acc  <- c();    confmat <- c()

for (nselw in c((1:10)*10,dim(df)[2])) {
  if (nselw <= 100) {
    slctrms <- read.csv(paste(nselw, " features.csv",sep=""), row.names=1)
    } else {
      slctrms <- names(df)
      }
  slctrms <- unique(unlist(slctrms))
  sDTM    <- df[,slctrms]
  tblnb   <- NBayes(sDTM, tests)
  confmat <- cbind(confmat, as.vector(tblnb))
  acc <- c(acc, sum(diag(tblnb)) / sum(tblnb))
  }
acc

library(class)
KNN <- function(df, tests, kk) {
  dtmat    <- as.matrix(df)
  traindat <- dtmat[-tests,]
  testdat  <- dtmat[tests,]
  trainy   <- lbls[-tests,1]
  testy    <- lbls[tests,1]
  predknn  <-knn(train=traindat,test=testdat,cl=trainy,k=kk)
  tblnb    <- table(data.frame(lbl=testy, pred=predknn))
}
set.seed(32180871)
nobs  <- dim(dfr)[1]
kcv   <- 4
tests <- sample(nobs, nobs/kcv)

KNNclass <- function(df, tests, sd) {
  set.seed(sd);    acmat <- c()
  for (nselw in c((1:10)*10,dim(df)[2])) {
    acc <- c();     confmat <- c()
    if (nselw <= 100) {
      slctrms <- read.csv(paste(nselw, " features.csv",sep=""), row.names=1)
    } else { 
        slctrms <- names(df) }
    slctrms <- unique(unlist(slctrms))
    sDTM    <- df[,slctrms]
    for (kk in 1:10) {
      tblknn  <- KNN(sDTM, tests, kk)
      confmat <- cbind(confmat, as.vector(tblknn))
      acc     <- c(acc, sum(diag(tblknn)) / sum(tblknn))
    }
    acmat <- cbind(acmat, acc)
  }
  acmat
}

KNNclass(dfr, tests, 32180871)

library(e1071)
SVM <- function(df, tests, mthd, cst) {
  dtmat    <- as.matrix(df)
  traindat <- dtmat[-tests,]
  testdat  <- dtmat[tests,]
  trainy   <- lbls[-tests,1]
  testy    <- lbls[tests,1]
  trainsvm <- svm(x=traindat, y=as.factor(trainy),kernel=mthd, cost=cst)
  predsvm  <- predict(trainsvm, testdat)
  tblsvm   <- table(data.frame(lbl=testy, pred=predsvm))
  }

SVMclass <- function(df, tests, mthd, cst) {
  acc     <- c()
  confmat <- c()
  for (nselw in c((1:10)*10,dim(df)[2])) {
    if (nselw <= 100) {
      slctrms <- read.csv(paste(nselw, " features.csv",sep=""), row.names=1)
    } else {
        slctrms <- names(df)
    }
    slctrms <- unique(unlist(slctrms))
    sDTM    <- df[,slctrms]
    tblsvm  <- SVM(sDTM, tests, mthd, cst)
    confmat <- cbind(confmat, as.vector(tblsvm))
    acc     <- c(acc, sum(diag(tblsvm)) / sum(tblsvm))
  }
  acc
}

acmat <- c()
for (mthd in c("linear","radial")) {
  for (cst in c(0.01,0.1,1,10)) {
    acc   <- SVMclass(df, tests, mthd, cst)
    acmat <- rbind(acmat, acc)
    }
  }
round(acmat,3)

##dfr
dfr<-prop.table(df)
for (nselw in (1:10)*10) {
  slctrms <- matrix(ncol=length(grps),nrow=nselw)
  colnames(slctrms) <- grps
  for (ii in seq_along(grps)) {
    grp   <- grindexes == grps[ii]
    trnws <- counts(dfr, grp)
    res   <- chi2s(trnws)
    slctrms[,ii] <- names(sort(res[, "Chisq"],decreasing=T))[1:nselw]
  }
  write.csv(slctrms,paste(nselw," features.csv",sep=""))
  print(length(unique(as.vector(slctrms))))
}

features20<-read.csv("20 features.csv")
View(features20)

library(class)
KNN <- function(df, tests, kk) {
  dtmat    <- as.matrix(df)
  traindat <- dtmat[-tests,]
  testdat  <- dtmat[tests,]
  trainy   <- lbls[-tests,1]
  testy    <- lbls[tests,1]
  predknn  <-knn(train=traindat,test=testdat,cl=trainy,k=kk)
  tblnb    <- table(data.frame(lbl=testy, pred=predknn))
}
set.seed(32180871)
nobs  <- dim(df)[1]
kcv   <- 4
tests <- sample(nobs, nobs/kcv)

KNNclass <- function(df, tests, sd) {
  set.seed(sd);    acmat <- c()
  for (nselw in c((1:10)*10,dim(df)[2])) {
    acc <- c();     confmat <- c()
    if (nselw <= 100) {
      slctrms <- read.csv(paste(nselw, " features.csv",sep=""), row.names=1)
    } else { 
      slctrms <- names(df) }
    slctrms <- unique(unlist(slctrms))
    sDTM    <- df[,slctrms]
    for (kk in 1:10) {
      tblknn  <- KNN(sDTM, tests, kk)
      confmat <- cbind(confmat, as.vector(tblknn))
      acc     <- c(acc, sum(diag(tblknn)) / sum(tblknn))
    }
    acmat <- cbind(acmat, acc)
  }
  acmat
}

KNNclass(df, tests, 32180871)

library(e1071)
SVM <- function(df, tests, mthd, cst) {
  dtmat    <- as.matrix(df)
  traindat <- dtmat[-tests,]
  testdat  <- dtmat[tests,]
  trainy   <- lbls[-tests,1]
  testy    <- lbls[tests,1]
  trainsvm <- svm(x=traindat, y=as.factor(trainy),kernel=mthd, cost=cst)
  predsvm  <- predict(trainsvm, testdat)
  tblsvm   <- table(data.frame(lbl=testy, pred=predsvm))
}

SVMclass <- function(df, tests, mthd, cst) {
  acc     <- c()
  confmat <- c()
  for (nselw in c((1:10)*10,dim(df)[2])) {
    if (nselw <= 100) {
      slctrms <- read.csv(paste(nselw, " features.csv",sep=""), row.names=1)
    } else {
      slctrms <- names(df)
    }
    slctrms <- unique(unlist(slctrms))
    sDTM    <- df[,slctrms]
    tblsvm  <- SVM(sDTM, tests, mthd, cst)
    confmat <- cbind(confmat, as.vector(tblsvm))
    acc     <- c(acc, sum(diag(tblsvm)) / sum(tblsvm))
  }
  acc
}

acmat <- c()
for (mthd in c("linear","radial")) {
  for (cst in c(0.01,0.1,1,10)) {
    acc   <- SVMclass(dfr, tests, mthd, cst)
    acmat <- rbind(acmat, acc)
  }
}
round(acmat,3)

##tfidf
tfidf<-function(x){
  tf<-ifelse(x>0,1+log(x),0)
  df<-colSums(x>0)
  idf<-log(dim(x)[1]/df)
  tfidf<-t(t(tf)*idf)
  tfidf
}

tfidf<-tfidf(as.matrix(df))
tfidf(df[,order(colSums(df),decreasing = T)])
View(df[,order(colSums(df),decreasing = T)])
tfidf(dfr)
x<-df
df<-as.matrix(df)
tf<-ifelse(x>0,1+log(x),0)
df<-colSums(x>0)
idf<-log(dim(x)[1]/df)
tfidf<-t(t(tf)*idf)
as.matrix(tf)5
df<-matrix(c(5,3,100,1,0,0,0,0),nrow = 2)
