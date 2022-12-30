setwd("C:/Users/yonghakim/Desktop/용하/데이터마이닝/dmba")

library(caret)
library(randomForest)
library(dplyr)


#데이터 불러오기
pro<-read.csv("project.csv",header = T)

pro$TYPE<-as.factor(pro$TYPE)
pro<-transform(pro,gender = ifelse(gender =="M", 1, 0))


#프레임별로 각도를 열로 만들어 총 30개의 각도를 측정
aa<-seq(0,2240,75)

pro1<-pro[,1:2]
##왼쪽 팔꿈치 각도
for (i in aa) {
  u<-pro[,(i+15):(i+17)]-pro[,(i+18):(i+20)]
  v<-pro[,(i+24):(i+26)]-pro[,(i+18):(i+20)]
  a<-acos(apply(u*v,1,sum)/(sqrt(apply(u*u,1,sum))*sqrt(apply(v*v,1,sum))))
  pro1<-cbind(pro1,a)
}
##오른쪽 팔꿈치 각도
for (i in aa) {
  u<-pro[,(i+27):(i+29)]-pro[,(i+30):(i+32)]
  v<-pro[,(i+36):(i+38)]-pro[,(i+30):(i+32)]
  b<-acos(apply(u*v,1,sum)/(sqrt(apply(u*u,1,sum))*sqrt(apply(v*v,1,sum))))
  pro1<-cbind(pro1,b)
}
##왼쪽 무릎 각도
for (i in aa) {
  u<-pro[,(i+39):(i+41)]-pro[,(i+42):(i+44)]
  v<-pro[,(i+48):(i+50)]-pro[,(i+42):(i+44)]
  c<-acos(apply(u*v,1,sum)/(sqrt(apply(u*u,1,sum))*sqrt(apply(v*v,1,sum))))
  pro1<-cbind(pro1,c)
}
##오른쪽 무릎 각도
for (i in aa) {
  u<-pro[,(i+51):(i+53)]-pro[,(i+54):(i+56)]
  v<-pro[,(i+60):(i+62)]-pro[,(i+54):(i+56)]
  d<-acos(apply(u*v,1,sum)/(sqrt(apply(u*u,1,sum))*sqrt(apply(v*v,1,sum))))
  pro1<-cbind(pro1,d)
}

#변수명 설정
n1<-paste(rep("left_elbow",30),1:30,sep="_")
n2<-paste(rep("right_elbow",30),1:30,sep="_")
n3<-paste(rep("left_knee",30),1:30,sep="_")
n4<-paste(rep("right_knee",30),1:30,sep="_")
names(pro1)[3:122]<-c(n1,n2,n3,n4)

#머리와 몸 y좌표 추출
pro2<-grep("Head_Y|SpineMid_Y",colnames(pro))
pro3<-cbind(pro1,pro[,pro2])


#데이터 불러오기 (테스트 데이터 넣으시면 됩니다! )
new<-read.csv("테스트 데이터.csv",header = T)

new$TYPE<-as.factor(new$TYPE)
new<-transform(new,gender = ifelse(gender =="M", 1, 0))


#프레임별로 각도를 열로 만들어 총 30개의 각도를 측정
aa<-seq(0,2240,75)

new1<-new[,1:2]
##왼쪽 팔꿈치 각도
for (i in aa) {
  u<-new[,(i+15):(i+17)]-new[,(i+18):(i+20)]
  v<-new[,(i+24):(i+26)]-new[,(i+18):(i+20)]
  a1<-acos(apply(u*v,1,sum)/(sqrt(apply(u*u,1,sum))*sqrt(apply(v*v,1,sum))))
  new1<-cbind(new1,a1)
}
##오른쪽 팔꿈치 각도
for (i in aa) {
  u<-new[,(i+27):(i+29)]-new[,(i+30):(i+32)]
  v<-new[,(i+36):(i+38)]-new[,(i+30):(i+32)]
  b1<-acos(apply(u*v,1,sum)/(sqrt(apply(u*u,1,sum))*sqrt(apply(v*v,1,sum))))
  new1<-cbind(new1,b1)
}
##왼쪽 무릎 각도
for (i in aa) {
  u<-new[,(i+39):(i+41)]-new[,(i+42):(i+44)]
  v<-new[,(i+48):(i+50)]-new[,(i+42):(i+44)]
  c1<-acos(apply(u*v,1,sum)/(sqrt(apply(u*u,1,sum))*sqrt(apply(v*v,1,sum))))
  new1<-cbind(new1,c1)
}
##오른쪽 무릎 각도
for (i in aa) {
  u<-new[,(i+51):(i+53)]-new[,(i+54):(i+56)]
  v<-new[,(i+60):(i+62)]-new[,(i+54):(i+56)]
  d1<-acos(apply(u*v,1,sum)/(sqrt(apply(u*u,1,sum))*sqrt(apply(v*v,1,sum))))
  new1<-cbind(new1,d1)
}

#변수명 설정
n1<-paste(rep("left_elbow",30),1:30,sep="_")
n2<-paste(rep("right_elbow",30),1:30,sep="_")
n3<-paste(rep("left_knee",30),1:30,sep="_")
n4<-paste(rep("right_knee",30),1:30,sep="_")
names(new1)[3:122]<-c(n1,n2,n3,n4)

#머리와 몸 y좌표 추출
new2<-grep("Head_Y|SpineMid_Y",colnames(new))
new3<-cbind(new1,new[,new2])


##랜덤 포레스트 모델 
rf.fit = randomForest(TYPE~., data=pro3, mtry = 11, ntree = 500, importance = T)

p<-predict(rf.fit,new3,type = "class")

confusionMatrix(p,as.factor(new3[,2]),mode = 'everything')




