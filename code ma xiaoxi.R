setwd("C:/Users/xiaoxi/Desktop/588 data mining/project/libras")

cc<-read.table("movement_libras.data.txt")
cc<-as.matrix(cc)
set.seed(2)
s=sample(1:nrow(cc), 240)

x.train<-cc[s,-91]
y.train<-as.character(cc[s,91])
train<-data.frame(x.train,y.train)

x.train<-data.frame(x.train)
x.train<-as.matrix(x.train)

x.test=cc[-s,-91]
y.test=(cc[-s,91])
test<-data.frame(x.test,y.test)

##############################################################

par(mfrow = c(2,5))
a=seq(from=1, to=nrow(as.matrix(x.train[1,])), by=2)
b=seq(from=2, to=nrow(as.matrix(x.train[1,])), by=2)


set.seed(1)
par(mfrow = c(2,5))
i=sample(c(1:240),10)
#[1] 126  76  67 187 166  39  16 176 144  40
for (k in 1:10){
  w=x.train[i[k],]
  new=as.matrix(cbind(w[a],w[b]),45,2)
  plot(new,type="b",col="blue",pch=16)
}

##############################################################

x.train1=subset(data.frame(x.train),y.train==1)
x.train2=subset(data.frame(x.train),y.train==2)
x.train3=subset(data.frame(x.train),y.train==3)
x.train4=subset(data.frame(x.train),y.train==4)
x.train5=subset(data.frame(x.train),y.train==5)
x.train6=subset(data.frame(x.train),y.train==6)
x.train7=subset(data.frame(x.train),y.train==7)
x.train8=subset(data.frame(x.train),y.train==8)
x.train9=subset(data.frame(x.train),y.train==9)
x.train10=subset(data.frame(x.train),y.train==10)
x.train11=subset(data.frame(x.train),y.train==11)
x.train12=subset(data.frame(x.train),y.train==12)
x.train13=subset(data.frame(x.train),y.train==13)
x.train14=subset(data.frame(x.train),y.train==14)
x.train15=subset(data.frame(x.train),y.train==15)

########################  Knn-1 #########################################
library(class)
k.vec <- c(1:15)
n.k <- length(k.vec)
test.error <- rep(NA, n.k)
train.error <- rep(NA, n.k)
for(i in 1:n.k){
  y.hat<- knn(x.train, rbind(x.train,x.test), y.train, k = k.vec[i], prob=FALSE)
  train.error[i] <- mean(y.hat[1:240]!=y.train)
  test.error[i] <- mean(y.hat[241:(240+120)]!=y.test)
}

train.error
# 0.0000000
test.error
# 0.2083333   
cbind(k.vec,test.error,train.error)
plot(k.vec,train.error,col="orange",,ylim=c(0,max(test.error)),type="o")
points(k.vec,test.error,col="blue",type="o")
legend("bottomright",lty=c(1,1),legend=c("train error","test error"),text.col=c("orange","blue"),col=c("orange","blue"))

plot(x=240/k.vec, test.error,col="orange",ylim=c(0,max(test.error)),type="o",xlab="Degress of freedom-N/K")
points(x=240/k.vec, train.error,col="blue",type="o")
legend("topright",lty=c(1,1),legend=c("test error","train error"),text.col=c("orange","blue"),col=c("orange","blue"))


########################  PCA #########################################
x.svd <- svd(x.train)
x.score <- as.matrix(x.train) %*% x.svd$v
x.svd$v[,1]
x.svd$v[,2]
plot(x.score[,1],x.score[,2],xlab="first principal component",ylab="second principal component")

# plot of PCA Component
pr.out=prcomp(x.train, scale=FALSE)
pr.out$rotation
biplot(pr.out, scale=0)
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)
pr.out$sdev
pr.var=pr.out$sdev^2
pr.var
pve=pr.var/sum(pr.var)
pve
plot(cumsum(pve),ylim=c(0,1),col="blue",pch=16,xlab="Component number",ylab=" Cumulative percentage",type="b")
abline(h=0.8,lty=3,col="red")
# fist fifth one come to 80%

########################  FDA #########################################
p <- 90
n1 <- length(x.train1)
n2 <- length(x.train2)
n3 <- length(x.train3)
n4 <- length(x.train4)
n5 <- length(x.train5)
n6 <- length(x.train6)
n7 <- length(x.train7)
n8 <- length(x.train8)
n9 <- length(x.train9)
n10 <- length(x.train10)
n11 <- length(x.train11)
n12 <- length(x.train12)
n13 <- length(x.train13)
n14 <- length(x.train14)
n15 <- length(x.train15)
n=n1+n2+n3+n4+n5+n6+n7+n8+n9+n10+n11+n12+n13+n14+n15

# the mean matrix
mu.hat <- apply(x.train,2,mean)
mu.1.hat <- apply(x.train1,2,mean)
mu.2.hat <- apply(x.train2,2,mean)
mu.3.hat <- apply(x.train3,2,mean)
mu.4.hat <- apply(x.train4,2,mean)
mu.5.hat <- apply(x.train5,2,mean)
mu.6.hat <- apply(x.train6,2,mean)
mu.7.hat <- apply(x.train7,2,mean)
mu.8.hat <- apply(x.train8,2,mean)
mu.9.hat <- apply(x.train9,2,mean)
mu.10.hat <- apply(x.train10,2,mean)
mu.11.hat <- apply(x.train11,2,mean)
mu.12.hat <- apply(x.train12,2,mean)
mu.13.hat <- apply(x.train13,2,mean)
mu.14.hat <- apply(x.train14,2,mean)
mu.15.hat <- apply(x.train15,2,mean)

# between class covariance
S.b <- (  (n1)*(mu.1.hat-mu.hat)%*%t(mu.1.hat-mu.hat)+
            (n2)*(mu.2.hat-mu.hat)%*%t(mu.2.hat-mu.hat)+
            (n3)*(mu.3.hat-mu.hat)%*%t(mu.3.hat-mu.hat)+
            (n4)*(mu.4.hat-mu.hat)%*%t(mu.4.hat-mu.hat)+
            (n5)*(mu.5.hat-mu.hat)%*%t(mu.5.hat-mu.hat)+
            (n6)*(mu.6.hat-mu.hat)%*%t(mu.6.hat-mu.hat)+
            (n7)*(mu.7.hat-mu.hat)%*%t(mu.7.hat-mu.hat)+
            (n8)*(mu.8.hat-mu.hat)%*%t(mu.8.hat-mu.hat)+
            (n9)*(mu.9.hat-mu.hat)%*%t(mu.9.hat-mu.hat)+
            (n10)*(mu.10.hat-mu.hat)%*%t(mu.10.hat-mu.hat)+
            (n11)*(mu.11.hat-mu.hat)%*%t(mu.11.hat-mu.hat)+
            (n12)*(mu.12.hat-mu.hat)%*%t(mu.12.hat-mu.hat)+
            (n13)*(mu.13.hat-mu.hat)%*%t(mu.13.hat-mu.hat)+
            (n14)*(mu.14.hat-mu.hat)%*%t(mu.14.hat-mu.hat)+
            (n15)*(mu.15.hat-mu.hat)%*%t(mu.15.hat-mu.hat)
)/(n-1)
# within class covariance
S.w <- ( as.matrix( t(x.train1 - rep(1,n1)%*% t(mu.1.hat))) %*% as.matrix((x.train1 - rep(1,n1)%*% t(mu.1.hat))) +
           as.matrix(t(x.train2 - rep(1,n1)%*% t(mu.2.hat))) %*% as.matrix((x.train2 - rep(1,n2)%*% t(mu.2.hat))) +
           as.matrix( t(x.train3 - rep(1,n1)%*% t(mu.3.hat))) %*% as.matrix((x.train3 - rep(1,n3)%*% t(mu.3.hat))) +
           as.matrix(t(x.train4 - rep(1,n1)%*% t(mu.4.hat))) %*%as.matrix( (x.train4 - rep(1,n4)%*% t(mu.4.hat))) +
           as.matrix(t(x.train5 - rep(1,n1)%*% t(mu.5.hat)))%*% as.matrix((x.train5 - rep(1,n5)%*% t(mu.5.hat)))+
           as.matrix(t(x.train6 - rep(1,n1)%*% t(mu.6.hat))) %*% as.matrix((x.train6 - rep(1,n6)%*% t(mu.6.hat)))+
           as.matrix(t(x.train7 - rep(1,n1)%*% t(mu.7.hat))) %*% as.matrix((x.train7 - rep(1,n7)%*% t(mu.7.hat))) +
           as.matrix( t(x.train8 - rep(1,n1)%*% t(mu.8.hat))) %*% as.matrix((x.train8 - rep(1,n8)%*% t(mu.8.hat))) +
           as.matrix( t(x.train9 - rep(1,n1)%*% t(mu.9.hat))) %*% as.matrix((x.train9 - rep(1,n9)%*% t(mu.9.hat))) +
           as.matrix( t(x.train10 - rep(1,n1)%*% t(mu.10.hat))) %*% as.matrix((x.train10 - rep(1,n10)%*% t(mu.10.hat))) +
           as.matrix(t(x.train11 - rep(1,n1)%*% t(mu.11.hat))) %*% as.matrix((x.train11 - rep(1,n11)%*% t(mu.11.hat))) +
           as.matrix(t(x.train12 - rep(1,n1)%*% t(mu.12.hat))) %*% as.matrix((x.train12 - rep(1,n12)%*% t(mu.12.hat))) +
           as.matrix( t(x.train13 - rep(1,n1)%*% t(mu.13.hat))) %*% as.matrix((x.train13 - rep(1,n13)%*% t(mu.13.hat))) +
           as.matrix( t(x.train14 - rep(1,n1)%*% t(mu.14.hat))) %*% as.matrix((x.train14 - rep(1,n14)%*% t(mu.14.hat))) +  
           as.matrix( t(x.train15 - rep(1,n1)%*% t(mu.15.hat))) %*% as.matrix((x.train15 - rep(1,n15)%*% t(mu.15.hat))) 
)/(n-15)

# total variance
S.t <- as.matrix(t(data.frame(x.train) - rep(1,n)%*% t(mu.hat))) %*% as.matrix((data.frame(x.train) - rep(1,n)%*% t(mu.hat)))
# relation
# S.t - S.b*(n-1) - S.w*(n-K) = 0
max(abs(S.t - S.b*(n-1) - S.w*(n-15)))

# define relative matrix
S <- solve(S.w) %*% S.b
# eigen decomp. of S
S.eig <- eigen(S)

# score 1 and score 2: 
x.scoref1=as.matrix(x.train)%*% Re(S.eig$vectors)[,1]
x.scoref2=as.matrix(x.train)%*% Re(S.eig$vectors)[,2]
x.scoref=as.matrix(x.train)%*% Re(S.eig$vectors)

# FDA projection

plot(as.matrix(x.train) %*% Re(S.eig$vectors)[,1], as.matrix(x.train)%*% Re(S.eig$vectors)[,2], col=rep(2:4,c(n1,n2,n3)),pch=rep(2:4,c(n1,n2,n3)))
dev.off()

##############################  OLS  ###################################
x.train <- as.matrix(x.train)
y.train<-as.matrix(y.train)
g <- y.train
y <- cbind(g==1,g==2,g==3,g==4,g==5,g==6,g==7,g==8,g==9,g==10,g==11,g==12,g==13,g==14,g==15)*1   # logic matrix to numeric matrix
# check all the row sums are 1
all(apply(y,1,sum)==1)


# or equivalently
lm1 <- lm(y[,1]~.,data=data.frame(x.train))   #use x.train instead of train which include y 
lm2 <- lm(y[,2]~.,data=data.frame(x.train))
lm3 <- lm(y[,3]~.,data=data.frame(x.train))
lm4 <- lm(y[,4]~.,data=data.frame(x.train))   
lm5 <- lm(y[,5]~.,data=data.frame(x.train))
lm6 <- lm(y[,6]~.,data=data.frame(x.train))
lm7 <- lm(y[,7]~.,data=data.frame(x.train))
lm8 <- lm(y[,8]~.,data=data.frame(x.train))
lm9 <- lm(y[,9]~.,data=data.frame(x.train))   
lm10 <- lm(y[,10]~.,data=data.frame(x.train))
lm11 <- lm(y[,11]~.,data=data.frame(x.train))
lm12<- lm(y[,12]~.,data=data.frame(x.train))
lm13 <- lm(y[,13]~.,data=data.frame(x.train))
lm14 <- lm(y[,14]~.,data=data.frame(x.train))   
lm15 <- lm(y[,15]~.,data=data.frame(x.train))


# check that B(1,1)+B(1,2)=1
# therefore sum_k \hat{y}_k = 1s
# double check
fitted(lm1)+fitted(lm2)+fitted(lm3)+fitted(lm4)+fitted(lm5)+fitted(lm6)+fitted(lm7)+fitted(lm8)+fitted(lm9)+fitted(lm10)+fitted(lm11)+fitted(lm12)+fitted(lm13)+fitted(lm14)+fitted(lm15)
fitted=cbind(fitted(lm1),fitted(lm2),fitted(lm3),fitted(lm4),fitted(lm5),fitted(lm6),fitted(lm7),fitted(lm8),fitted(lm9),fitted(lm10),fitted(lm11),fitted(lm12),fitted(lm13),fitted(lm14),fitted(lm15))
# prediction
g.hat=apply(fitted,1,which.max)
table(g.hat,g)
#     g
#h.hat   1    2    3
#    1  716   2    4
#    2   4   647  14
#    3   11   9  524
# error rate 
mean(g.hat!=g)
# train error  0.09583333

##########################################
x.test <- as.matrix(x.test)
y.test<-as.matrix(y.test)
g_t <- y.test
y_t <- cbind(g==1,g==2,g==3,g==4,g==5,g==6,g==7,g==8,g==9,g==10,g==11,g==12,g==13,g==14,g==15)*1 # logic matrix to numeric matrix
# check all the row sums are 1
all(apply(y_t,1,sum)==1)

# prediction
prep1=predict(lm1,data.frame(x.test))
prep2=predict(lm2,data.frame(x.test))
prep3=predict(lm3,data.frame(x.test))
prep4=predict(lm4,data.frame(x.test))
prep5=predict(lm5,data.frame(x.test))
prep6=predict(lm6,data.frame(x.test))
prep7=predict(lm7,data.frame(x.test))
prep8=predict(lm8,data.frame(x.test))
prep9=predict(lm9,data.frame(x.test))
prep10=predict(lm10,data.frame(x.test))
prep11=predict(lm11,data.frame(x.test))
prep12=predict(lm12,data.frame(x.test))
prep13=predict(lm13,data.frame(x.test))
prep14=predict(lm14,data.frame(x.test))
prep15=predict(lm15,data.frame(x.test))

pred<-cbind(prep1,prep2,prep3,prep4,prep5,prep6,prep7,prep8,prep9,prep10,prep11,prep12,prep13,prep14,prep15)

g.hat_t=apply(pred,1,which.max)
table(g.hat_t,g_t)
#           g_t
#g.hat_t   1   2   3
#     1   183   8   2
#     2    6 152  14
#     3    9   6 150
# error rate 
mean(g.hat_t!=g_t)

# test error  [1] 0.5166667

###################################### logist regression ##################################
library(glmnet)
# estimate parameter in logistic regression
# no regularization by lambda =0 

y.train<-(cc[s,91])

ans.logistic <- glmnet(x.train,y.train, family=c("multinomial"),standardize=TRUE)

############################################################

# Ridge Regularization of Generalized Linear Models
cv.out<-cv.glmnet(x.train,y.train,type.measure="class",nfolds=5,alpha=0,standardize=TRUE)

bestlam <- cv.out$lambda.min

set.seed(1)
k=5
p <- length(cv.out$lambda)
n=nrow(x.train)

set.seed(1)
folds=sample(1:k,n,replace=TRUE)
cv.errors=matrix(NA,k,p, dimnames=list(NULL, paste(1:p)))
for(j in 1:k){
  ridge.mod<-glmnet(x.train[folds!=j,],y.train[folds!=j],family=c("multinomial"),alpha=0,standardize=TRUE) 
  for(i in 1:p){
    pred=predict(ridge.mod,s=cv.out$lambda[i],newx=x.train[folds==j,],type = "class")
    cv.errors[j,i]=mean(pred!=y.train[folds==j])
  }
}
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
min(mean.cv.errors)
#[1] 0.245387
which.min(mean.cv.errors)
#99

cv.lambda<-cv.out$lambda[99]
#[1] 0.01333502
plot(cv.out$lambda,mean.cv.errors)

ridge.pred <- predict(ridge.mod,s=cv.lambda,newx=x.test,type = "class")
ridge.predt <- predict(ridge.mod,s=cv.lambda,newx=x.train,type = "class")
mean(ridge.predt!=y.train)
#  0.2791667
mean(ridge.pred!=y.test)
#0.4583333


############## Lasso Regularization of Generalized Linear Models #######################
cv.out<-cv.glmnet(x.train,y.train,type.measure="class",nfolds=5,alpha=1,standardize=TRUE)

set.seed(1)
folds=sample(1:k,n,replace=TRUE)
cv.errors=matrix(NA,k,p, dimnames=list(NULL, paste(1:p)))
for(j in 1:k){
  lasso.mod<-glmnet(x.train[folds!=j,],y.train[folds!=j],family=c("multinomial"),alpha=1,standardize=TRUE) 
  for(i in 1:p){
    pred=predict(lasso.mod,s=cv.out$lambda[i],newx=x.train[folds==j,],type = "class")
    cv.errors[j,i]=mean(pred!=y.train[folds==j])
  }
}
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
min(mean.cv.errors)
#[1] 0.2333333
which.min(mean.cv.errors)
#87

cv.lambda<-cv.out$lambda[87]
#[1] 0.0005639705
plot(cv.out$lambda,mean.cv.errors)

lasso.pred <- predict(lasso.mod,s=cv.lambda,newx=x.test,type = "class")
lasso.predt <- predict(lasso.mod,s=cv.lambda,newx=x.train,type = "class")
mean(lasso.predt!=y.train)
# 0.0625
mean(lasso.pred!=y.test)
#  0.2333333
############################################################

############################## LDA ###########################

# prepare
g <- y.train


# estimate parameters
pi1.hat <- mean(g==1)
pi2.hat <- mean(g==2)
pi3.hat <- mean(g==3)
pi4.hat <- mean(g==4)
pi5.hat <- mean(g==5)
pi6.hat <- mean(g==6)
pi7.hat <- mean(g==7)
pi8.hat <- mean(g==8)
pi9.hat <- mean(g==9)
pi10.hat <- mean(g==10)
pi11.hat <- mean(g==11)
pi12.hat <- mean(g==12)
pi13.hat <- mean(g==13)
pi14.hat <- mean(g==14)
pi15.hat <- mean(g==15)


# within class covariance
S.w

# decision boundary

# classification
b1=(x.train %*% solve(S.w)%*%mu.1.hat-as.vector(0.5*mu.1.hat%*% solve(S.w)%*%mu.1.hat)+ log(pi1.hat)) # The decision boundary
b2=(x.train %*% solve(S.w)%*%mu.2.hat -as.vector(0.5*mu.2.hat%*% solve(S.w)%*%mu.2.hat)+ log(pi2.hat)) 
b3=(x.train %*% solve(S.w)%*%mu.3.hat -as.vector(0.5*mu.3.hat%*% solve(S.w)%*%mu.3.hat) + log(pi3.hat)) 
b4=(x.train %*% solve(S.w)%*%mu.4.hat-as.vector(0.5*mu.4.hat%*% solve(S.w)%*%mu.4.hat)+ log(pi4.hat)) 
b5=(x.train %*% solve(S.w)%*%mu.5.hat -as.vector(0.5*mu.5.hat%*% solve(S.w)%*%mu.5.hat)+ log(pi5.hat)) 
b6=(x.train %*% solve(S.w)%*%mu.6.hat -as.vector(0.5*mu.6.hat%*% solve(S.w)%*%mu.6.hat) + log(pi6.hat))
b7=(x.train %*% solve(S.w)%*%mu.7.hat-as.vector(0.5*mu.7.hat%*% solve(S.w)%*%mu.7.hat)+ log(pi7.hat)) 
b8=(x.train %*% solve(S.w)%*%mu.8.hat -as.vector(0.5*mu.8.hat%*% solve(S.w)%*%mu.8.hat)+ log(pi8.hat)) 
b9=(x.train %*% solve(S.w)%*%mu.9.hat -as.vector(0.5*mu.9.hat%*% solve(S.w)%*%mu.9.hat) + log(pi9.hat))
b10=(x.train %*% solve(S.w)%*%mu.10.hat-as.vector(0.5*mu.10.hat%*% solve(S.w)%*%mu.10.hat)+ log(pi10.hat)) 
b11=(x.train %*% solve(S.w)%*%mu.11.hat -as.vector(0.5*mu.11.hat%*% solve(S.w)%*%mu.11.hat)+ log(pi11.hat)) 
b12=(x.train %*% solve(S.w)%*%mu.12.hat -as.vector(0.5*mu.12.hat%*% solve(S.w)%*%mu.12.hat) + log(pi12.hat))
b13=(x.train %*% solve(S.w)%*%mu.13.hat-as.vector(0.5*mu.13.hat%*% solve(S.w)%*%mu.13.hat)+ log(pi13.hat)) 
b14=(x.train %*% solve(S.w)%*%mu.14.hat -as.vector(0.5*mu.14.hat%*% solve(S.w)%*%mu.14.hat)+ log(pi14.hat)) 
b15=(x.train %*% solve(S.w)%*%mu.15.hat -as.vector(0.5*mu.15.hat%*% solve(S.w)%*%mu.15.hat) + log(pi15.hat))

x.trainb<-cbind(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15)
g.hatb=apply(x.trainb,1,which.max)
table(g.hatb,g)

mean(g.hatb!=g)
# training error 
# [1] 0.1208333

#####################
gg=y.test
b1t=(x.test %*% solve(S.w)%*%mu.1.hat-as.vector(0.5*mu.1.hat%*% solve(S.w)%*%mu.1.hat)+ log(pi1.hat)) # The decision boundary
b2t=(x.test %*% solve(S.w)%*%mu.2.hat -as.vector(0.5*mu.2.hat%*% solve(S.w)%*%mu.2.hat)+ log(pi2.hat)) 
b3t=(x.test %*% solve(S.w)%*%mu.3.hat -as.vector(0.5*mu.3.hat%*% solve(S.w)%*%mu.3.hat) + log(pi3.hat)) 
b4t=(x.test %*% solve(S.w)%*%mu.4.hat-as.vector(0.5*mu.4.hat%*% solve(S.w)%*%mu.4.hat)+ log(pi4.hat)) 
b5t=(x.test %*% solve(S.w)%*%mu.5.hat -as.vector(0.5*mu.5.hat%*% solve(S.w)%*%mu.5.hat)+ log(pi5.hat)) 
b6t=(x.test %*% solve(S.w)%*%mu.6.hat -as.vector(0.5*mu.6.hat%*% solve(S.w)%*%mu.6.hat) + log(pi6.hat))
b7t=(x.test %*% solve(S.w)%*%mu.7.hat-as.vector(0.5*mu.7.hat%*% solve(S.w)%*%mu.7.hat)+ log(pi7.hat)) 
b8t=(x.test %*% solve(S.w)%*%mu.8.hat -as.vector(0.5*mu.8.hat%*% solve(S.w)%*%mu.8.hat)+ log(pi8.hat)) 
b9t=(x.test %*% solve(S.w)%*%mu.9.hat -as.vector(0.5*mu.9.hat%*% solve(S.w)%*%mu.9.hat) + log(pi9.hat))
b10t=(x.test %*% solve(S.w)%*%mu.10.hat-as.vector(0.5*mu.10.hat%*% solve(S.w)%*%mu.10.hat)+ log(pi10.hat)) 
b11t=(x.test %*% solve(S.w)%*%mu.11.hat -as.vector(0.5*mu.11.hat%*% solve(S.w)%*%mu.11.hat)+ log(pi11.hat)) 
b12t=(x.test %*% solve(S.w)%*%mu.12.hat -as.vector(0.5*mu.12.hat%*% solve(S.w)%*%mu.12.hat) + log(pi12.hat))
b13t=(x.test %*% solve(S.w)%*%mu.13.hat-as.vector(0.5*mu.13.hat%*% solve(S.w)%*%mu.13.hat)+ log(pi13.hat)) 
b14t=(x.test %*% solve(S.w)%*%mu.14.hat -as.vector(0.5*mu.14.hat%*% solve(S.w)%*%mu.14.hat)+ log(pi14.hat)) 
b15t=(x.test %*% solve(S.w)%*%mu.15.hat -as.vector(0.5*mu.15.hat%*% solve(S.w)%*%mu.15.hat) + log(pi15.hat))  # The decision boundary
x.testb<-cbind(b1t,b2t,b3t,b4t,b5t,b6t,b7t,b8t,b9t,b10t,b11t,b12t,b13t,b14t,b15t)
g.hatbt=apply(x.testb,1,which.max)
table(g.hatbt,gg)
mean(g.hatbt!=gg)
#test error : 
# [1] 0.325


#####################################################
# Plot first and second score of PCA FDA 
g<-as.numeric(g)
par(mfrow = c(1,2))
plot(x.score[,1],x.score[,2],col= c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)[g],pch=c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)[g],main="PCA projection",xlab="First component",ylab="second component")
plot(x.scoref[,1],x.scoref[,2],col= c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)[g],pch=c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)[g],main="FDA projection",xlab="First component",ylab="second component")

############### by using the first fifth score of FDA ###############################################

x.pcf<-cbind(x.scoref[,1],x.scoref[,2],x.scoref[,3],x.scoref[,4],x.scoref[,5],x.scoref[,6])
x.pcf <- as.matrix(x.pcf)
y.train<-as.matrix(y.train)
g <- y.train
y <- cbind(g==1,g==2,g==3,g==4,g==5,g==6,g==7,g==8,g==9,g==10,g==11,g==12,g==13,g==14,g==15)*1   # logic matrix to numeric matrix

########## test ########### 

x.scoreft1=x.test %*% Re(S.eig$vectors)[,1]
x.scoreft2=x.test%*% Re(S.eig$vectors)[,2]
x.scoreft=x.test%*% Re(S.eig$vectors)
x.pctf<-cbind(x.scoreft[,1],x.scoreft[,2],x.scoreft[,3],x.scoreft[,4],x.scoreft[,5],x.scoreft[,6])
x.pctf <- as.matrix(x.pctf)
y.test<-as.matrix(y.test)
g_t <- y.test
y_t <- cbind(g_t==1,g_t==2,g_t==3,g_t==4,g_t==5,g_t==6,g_t==7,g_t==8,g_t==9,g_t==10,g_t==11,g_t==12,g_t==13,g_t==14,g_t==15)*1   # logic matrix to numeric matrix
# check all the row sums are 1
all(apply(y_t,1,sum)==1)


############### LDA with PCA projection ############################

set.seed(1)
nfolds=5
n.train=nrow(x.train)
s <- split(sample(n.train),rep(1:nfolds,length=n.train))
p <- ncol(x.train)
cv.errors.lda=matrix(NA,5,90)

for (j in seq(nfolds)){
  x.score.cv<-x.train[-s[[j]],]%*% x.svd$v
  y.train.cv=y.train[-s[[j]]]
  y.train.cv<-as.matrix(y.train.cv)
  g <- y.train.cv
  
  n1 <- sum(g==1)
  n2 <- sum(g==2)
  n3 <- sum(g==3)
  n4 <- sum(g==4)
  n5 <- sum(g==5)
  n6 <- sum(g==6)
  n7 <- sum(g==7)
  n8 <- sum(g==8)
  n9 <- sum(g==9)
  n10 <- sum(g==10)
  n11 <- sum(g==11)
  n12 <- sum(g==12)
  n13 <- sum(g==13)
  n14 <- sum(g==14)
  n15 <- sum(g==15)
  
  n <- n1+n2+n3+n4+n5+n6+n7+n8+n9+n10+n11+n12+n13+n14+n15   
  
  pi1.hat <- mean(g==1)
  pi2.hat <- mean(g==2)
  pi3.hat <- mean(g==3)
  pi4.hat <- mean(g==4)
  pi5.hat <- mean(g==5)
  pi6.hat <- mean(g==6)
  pi7.hat <- mean(g==7)
  pi8.hat <- mean(g==8)
  pi9.hat <- mean(g==9)
  pi10.hat <- mean(g==10)
  pi11.hat <- mean(g==11)
  pi12.hat <- mean(g==12)
  pi13.hat <- mean(g==13)
  pi14.hat <- mean(g==14)
  pi15.hat <- mean(g==15)
  
  test.cv=x.train[s[[j]],]%*% x.svd$v
  y.test.cv=y.train[s[[j]]]
  y.test.cv<-as.matrix(y.test.cv)
  gg=y.test.cv
  
  
  for(i in 2:90){
    
    x.cv.lda<-data.frame(x.score.cv[,1:i])
    x.cv.lda <- as.matrix(x.cv.lda)
    
    mu.1.hat <- apply(x.cv.lda[g==1,],2,mean)
    mu.2.hat <- apply(x.cv.lda[g==2,],2,mean)
    mu.3.hat <- apply(x.cv.lda[g==3,],2,mean)
    mu.4.hat <- apply(x.cv.lda[g==4,],2,mean)
    mu.5.hat <- apply(x.cv.lda[g==5,],2,mean)
    mu.6.hat <- apply(x.cv.lda[g==6,],2,mean)
    mu.7.hat <- apply(x.cv.lda[g==7,],2,mean)
    mu.8.hat <- apply(x.cv.lda[g==8,],2,mean)
    mu.9.hat <- apply(x.cv.lda[g==9,],2,mean)
    mu.10.hat <- apply(x.cv.lda[g==10,],2,mean)
    mu.11.hat <- apply(x.cv.lda[g==11,],2,mean)
    mu.12.hat <- apply(x.cv.lda[g==12,],2,mean)
    mu.13.hat <- apply(x.cv.lda[g==13,],2,mean)
    mu.14.hat <- apply(x.cv.lda[g==14,],2,mean)
    mu.15.hat <- apply(x.cv.lda[g==15,],2,mean)
    
    # within class covariance
    
    S.w <- (  t(x.cv.lda[g==1,] - rep(1,n1)%*% t(mu.1.hat)) %*% as.matrix((x.cv.lda[g==1,]- rep(1,n1)%*% t(mu.1.hat))) +
                t(x.cv.lda[g==2,] - rep(1,n2)%*% t(mu.2.hat)) %*% as.matrix((x.cv.lda[g==2,] - rep(1,n2)%*% t(mu.2.hat))) +
                t(x.cv.lda[g==3,] - rep(1,n3)%*% t(mu.3.hat)) %*% as.matrix((x.cv.lda[g==3,] - rep(1,n3)%*% t(mu.3.hat))) +
                t(x.cv.lda[g==4,] - rep(1,n4)%*% t(mu.4.hat)) %*%as.matrix( (x.cv.lda[g==4,] - rep(1,n4)%*% t(mu.4.hat))) +
                t(x.cv.lda[g==5,] - rep(1,n5)%*% t(mu.5.hat))%*% as.matrix((x.cv.lda[g==5,] - rep(1,n5)%*% t(mu.5.hat)))+
                t(x.cv.lda[g==6,] - rep(1,n6)%*% t(mu.6.hat)) %*% as.matrix((x.cv.lda[g==6,] - rep(1,n6)%*% t(mu.6.hat)))+
                t(x.cv.lda[g==7,] - rep(1,n7)%*% t(mu.7.hat)) %*% as.matrix((x.cv.lda[g==7,] - rep(1,n7)%*% t(mu.7.hat))) +
                t(x.cv.lda[g==8,] - rep(1,n8)%*% t(mu.8.hat)) %*% as.matrix((x.cv.lda[g==8,] - rep(1,n8)%*% t(mu.8.hat))) +
                t(x.cv.lda[g==9,] - rep(1,n9)%*% t(mu.9.hat)) %*% as.matrix((x.cv.lda[g==9,] - rep(1,n9)%*% t(mu.9.hat))) +
                t(x.cv.lda[g==10,] - rep(1,n10)%*% t(mu.10.hat)) %*% as.matrix((x.cv.lda[g==10,] - rep(1,n10)%*% t(mu.10.hat))) +
                t(x.cv.lda[g==11,] - rep(1,n11)%*% t(mu.11.hat)) %*% as.matrix((x.cv.lda[g==11,] - rep(1,n11)%*% t(mu.11.hat))) +
                t(x.cv.lda[g==12,] - rep(1,n12)%*% t(mu.12.hat)) %*% as.matrix((x.cv.lda[g==12,] - rep(1,n12)%*% t(mu.12.hat))) +
                t(x.cv.lda[g==13,] - rep(1,n13)%*% t(mu.13.hat)) %*% as.matrix((x.cv.lda[g==13,] - rep(1,n13)%*% t(mu.13.hat))) +
                t(x.cv.lda[g==14,] - rep(1,n14)%*% t(mu.14.hat)) %*% as.matrix((x.cv.lda[g==14,] - rep(1,n14)%*% t(mu.14.hat))) +  
                t(x.cv.lda[g==15,] - rep(1,n15)%*% t(mu.15.hat)) %*% as.matrix((x.cv.lda[g==15,] - rep(1,n15)%*% t(mu.15.hat))) 
    )/(n-15)
    # classification
    
    x.cv.lda.t<-data.frame(test.cv[,1:i])
    x.cv.lda.t <- as.matrix(x.cv.lda.t)
    
    b1t=(x.cv.lda.t %*% solve(S.w)%*%mu.1.hat-as.vector(0.5*mu.1.hat%*% solve(S.w)%*%mu.1.hat)+ log(pi1.hat)) # The decision boundary
    b2t=(x.cv.lda.t %*% solve(S.w)%*%mu.2.hat -as.vector(0.5*mu.2.hat%*% solve(S.w)%*%mu.2.hat)+ log(pi2.hat)) 
    b3t=(x.cv.lda.t %*% solve(S.w)%*%mu.3.hat -as.vector(0.5*mu.3.hat%*% solve(S.w)%*%mu.3.hat) + log(pi3.hat)) 
    b4t=(x.cv.lda.t %*% solve(S.w)%*%mu.4.hat-as.vector(0.5*mu.4.hat%*% solve(S.w)%*%mu.4.hat)+ log(pi4.hat)) 
    b5t=(x.cv.lda.t %*% solve(S.w)%*%mu.5.hat -as.vector(0.5*mu.5.hat%*% solve(S.w)%*%mu.5.hat)+ log(pi5.hat)) 
    b6t=(x.cv.lda.t %*% solve(S.w)%*%mu.6.hat -as.vector(0.5*mu.6.hat%*% solve(S.w)%*%mu.6.hat) + log(pi6.hat))
    b7t=(x.cv.lda.t %*% solve(S.w)%*%mu.7.hat-as.vector(0.5*mu.7.hat%*% solve(S.w)%*%mu.7.hat)+ log(pi7.hat)) 
    b8t=(x.cv.lda.t %*% solve(S.w)%*%mu.8.hat -as.vector(0.5*mu.8.hat%*% solve(S.w)%*%mu.8.hat)+ log(pi8.hat)) 
    b9t=(x.cv.lda.t %*% solve(S.w)%*%mu.9.hat -as.vector(0.5*mu.9.hat%*% solve(S.w)%*%mu.9.hat) + log(pi9.hat))
    b10t=(x.cv.lda.t %*% solve(S.w)%*%mu.10.hat-as.vector(0.5*mu.10.hat%*% solve(S.w)%*%mu.10.hat)+ log(pi10.hat)) 
    b11t=(x.cv.lda.t %*% solve(S.w)%*%mu.11.hat -as.vector(0.5*mu.11.hat%*% solve(S.w)%*%mu.11.hat)+ log(pi11.hat)) 
    b12t=(x.cv.lda.t %*% solve(S.w)%*%mu.12.hat -as.vector(0.5*mu.12.hat%*% solve(S.w)%*%mu.12.hat) + log(pi12.hat))
    b13t=(x.cv.lda.t %*% solve(S.w)%*%mu.13.hat-as.vector(0.5*mu.13.hat%*% solve(S.w)%*%mu.13.hat)+ log(pi13.hat)) 
    b14t=(x.cv.lda.t %*% solve(S.w)%*%mu.14.hat -as.vector(0.5*mu.14.hat%*% solve(S.w)%*%mu.14.hat)+ log(pi14.hat)) 
    b15t=(x.cv.lda.t %*% solve(S.w)%*%mu.15.hat -as.vector(0.5*mu.15.hat%*% solve(S.w)%*%mu.15.hat) + log(pi15.hat))  # The decision boundary
    x.testb<-cbind(b1t,b2t,b3t,b4t,b5t,b6t,b7t,b8t,b9t,b10t,b11t,b12t,b13t,b14t,b15t)
    
    g.hatbt=apply(x.testb,1,which.max)
    cv.errors.lda[j,i]=mean(g.hatbt!=gg)
    
  }
}

# when i=1

set.seed(1)
errors=matrix(NA,5,1)
n.train=nrow(x.train)
s <- split(sample(n.train),rep(1:nfolds,length=n.train))
p <- ncol(x.train)


for (j in seq(nfolds)){
  x.score.cv<-x.train[-s[[j]],]%*% x.svd$v
  y.train.cv=y.train[-s[[j]]]
  y.train.cv<-as.matrix(y.train.cv)
  g <- y.train.cv
  
  n1 <- sum(g==1)
  n2 <- sum(g==2)
  n3 <- sum(g==3)
  n4 <- sum(g==4)
  n5 <- sum(g==5)
  n6 <- sum(g==6)
  n7 <- sum(g==7)
  n8 <- sum(g==8)
  n9 <- sum(g==9)
  n10 <- sum(g==10)
  n11 <- sum(g==11)
  n12 <- sum(g==12)
  n13 <- sum(g==13)
  n14 <- sum(g==14)
  n15 <- sum(g==15)
  
  n <- n1+n2+n3+n4+n5+n6+n7+n8+n9+n10+n11+n12+n13+n14+n15   
  
  pi1.hat <- mean(g==1)
  pi2.hat <- mean(g==2)
  pi3.hat <- mean(g==3)
  pi4.hat <- mean(g==4)
  pi5.hat <- mean(g==5)
  pi6.hat <- mean(g==6)
  pi7.hat <- mean(g==7)
  pi8.hat <- mean(g==8)
  pi9.hat <- mean(g==9)
  pi10.hat <- mean(g==10)
  pi11.hat <- mean(g==11)
  pi12.hat <- mean(g==12)
  pi13.hat <- mean(g==13)
  pi14.hat <- mean(g==14)
  pi15.hat <- mean(g==15)
  
  
  test.cv=x.train[s[[j]],]%*% x.svd$v
  y.test.cv=y.train[s[[j]]]
  y.test.cv<-as.matrix(y.test.cv)
  gg=y.test.cv
  
  x.cv.lda<-data.frame(x.score.cv[,1])
  x.cv.lda <- as.matrix(x.cv.lda)
  
  x.cv.lda<-data.frame(x.score.cv[,1])
  x.cv.lda <- as.matrix(x.cv.lda)
  
  mu.1.hat <- apply(x.cv.lda,2,mean)
  mu.2.hat <- apply(x.cv.lda,2,mean)
  mu.3.hat <- apply(x.cv.lda,2,mean)
  mu.4.hat <- apply(x.cv.lda,2,mean)
  mu.5.hat <- apply(x.cv.lda,2,mean)
  mu.6.hat <- apply(x.cv.lda,2,mean)
  mu.7.hat <- apply(x.cv.lda,2,mean)
  mu.8.hat <- apply(x.cv.lda,2,mean)
  mu.9.hat <- apply(x.cv.lda,2,mean)
  mu.10.hat <- apply(x.cv.lda,2,mean)
  mu.11.hat <- apply(x.cv.lda,2,mean)
  mu.12.hat <- apply(x.cv.lda,2,mean)
  mu.13.hat <- apply(x.cv.lda,2,mean)
  mu.14.hat <- apply(x.cv.lda,2,mean)
  mu.15.hat <- apply(x.cv.lda,2,mean)
  
  # within class covariance
  
  S.w <- (  t(x.cv.lda[g==1,] - rep(1,n1)%*% t(mu.1.hat)) %*% as.matrix((x.cv.lda[g==1,]- rep(1,n1)%*% t(mu.1.hat))) +
              t(x.cv.lda[g==2,] - rep(1,n2)%*% t(mu.2.hat)) %*% as.matrix((x.cv.lda[g==2,] - rep(1,n2)%*% t(mu.2.hat))) +
              t(x.cv.lda[g==3,] - rep(1,n3)%*% t(mu.3.hat)) %*% as.matrix((x.cv.lda[g==3,] - rep(1,n3)%*% t(mu.3.hat))) +
              t(x.cv.lda[g==4,] - rep(1,n4)%*% t(mu.4.hat)) %*%as.matrix( (x.cv.lda[g==4,] - rep(1,n4)%*% t(mu.4.hat))) +
              t(x.cv.lda[g==5,] - rep(1,n5)%*% t(mu.5.hat))%*% as.matrix((x.cv.lda[g==5,] - rep(1,n5)%*% t(mu.5.hat)))+
              t(x.cv.lda[g==6,] - rep(1,n6)%*% t(mu.6.hat)) %*% as.matrix((x.cv.lda[g==6,] - rep(1,n6)%*% t(mu.6.hat)))+
              t(x.cv.lda[g==7,] - rep(1,n7)%*% t(mu.7.hat)) %*% as.matrix((x.cv.lda[g==7,] - rep(1,n7)%*% t(mu.7.hat))) +
              t(x.cv.lda[g==8,] - rep(1,n8)%*% t(mu.8.hat)) %*% as.matrix((x.cv.lda[g==8,] - rep(1,n8)%*% t(mu.8.hat))) +
              t(x.cv.lda[g==9,] - rep(1,n9)%*% t(mu.9.hat)) %*% as.matrix((x.cv.lda[g==9,] - rep(1,n9)%*% t(mu.9.hat))) +
              t(x.cv.lda[g==10,] - rep(1,n10)%*% t(mu.10.hat)) %*% as.matrix((x.cv.lda[g==10,] - rep(1,n10)%*% t(mu.10.hat))) +
              t(x.cv.lda[g==11,] - rep(1,n11)%*% t(mu.11.hat)) %*% as.matrix((x.cv.lda[g==11,] - rep(1,n11)%*% t(mu.11.hat))) +
              t(x.cv.lda[g==12,] - rep(1,n12)%*% t(mu.12.hat)) %*% as.matrix((x.cv.lda[g==12,] - rep(1,n12)%*% t(mu.12.hat))) +
              t(x.cv.lda[g==13,] - rep(1,n13)%*% t(mu.13.hat)) %*% as.matrix((x.cv.lda[g==13,] - rep(1,n13)%*% t(mu.13.hat))) +
              t(x.cv.lda[g==14,] - rep(1,n14)%*% t(mu.14.hat)) %*% as.matrix((x.cv.lda[g==14,] - rep(1,n14)%*% t(mu.14.hat))) +  
              t(x.cv.lda[g==15,] - rep(1,n15)%*% t(mu.15.hat)) %*% as.matrix((x.cv.lda[g==15,] - rep(1,n15)%*% t(mu.15.hat))) 
  )/(n-15)
  
  x.cv.lda.t<-data.frame(test.cv[,1])
  x.cv.lda.t <- as.matrix(x.cv.lda.t)
  colnames(x.cv.lda)="x1"
  colnames(x.cv.lda.t)="x1"  
  
  b1t=(x.cv.lda.t %*% solve(S.w)%*%mu.1.hat-as.vector(0.5*mu.1.hat%*% solve(S.w)%*%mu.1.hat)+ log(pi1.hat)) # The decision boundary
  b2t=(x.cv.lda.t %*% solve(S.w)%*%mu.2.hat -as.vector(0.5*mu.2.hat%*% solve(S.w)%*%mu.2.hat)+ log(pi2.hat)) 
  b3t=(x.cv.lda.t %*% solve(S.w)%*%mu.3.hat -as.vector(0.5*mu.3.hat%*% solve(S.w)%*%mu.3.hat) + log(pi3.hat)) 
  b4t=(x.cv.lda.t %*% solve(S.w)%*%mu.4.hat-as.vector(0.5*mu.4.hat%*% solve(S.w)%*%mu.4.hat)+ log(pi4.hat)) 
  b5t=(x.cv.lda.t %*% solve(S.w)%*%mu.5.hat -as.vector(0.5*mu.5.hat%*% solve(S.w)%*%mu.5.hat)+ log(pi5.hat)) 
  b6t=(x.cv.lda.t %*% solve(S.w)%*%mu.6.hat -as.vector(0.5*mu.6.hat%*% solve(S.w)%*%mu.6.hat) + log(pi6.hat))
  b7t=(x.cv.lda.t %*% solve(S.w)%*%mu.7.hat-as.vector(0.5*mu.7.hat%*% solve(S.w)%*%mu.7.hat)+ log(pi7.hat)) 
  b8t=(x.cv.lda.t %*% solve(S.w)%*%mu.8.hat -as.vector(0.5*mu.8.hat%*% solve(S.w)%*%mu.8.hat)+ log(pi8.hat)) 
  b9t=(x.cv.lda.t %*% solve(S.w)%*%mu.9.hat -as.vector(0.5*mu.9.hat%*% solve(S.w)%*%mu.9.hat) + log(pi9.hat))
  b10t=(x.cv.lda.t %*% solve(S.w)%*%mu.10.hat-as.vector(0.5*mu.10.hat%*% solve(S.w)%*%mu.10.hat)+ log(pi10.hat)) 
  b11t=(x.cv.lda.t %*% solve(S.w)%*%mu.11.hat -as.vector(0.5*mu.11.hat%*% solve(S.w)%*%mu.11.hat)+ log(pi11.hat)) 
  b12t=(x.cv.lda.t %*% solve(S.w)%*%mu.12.hat -as.vector(0.5*mu.12.hat%*% solve(S.w)%*%mu.12.hat) + log(pi12.hat))
  b13t=(x.cv.lda.t %*% solve(S.w)%*%mu.13.hat-as.vector(0.5*mu.13.hat%*% solve(S.w)%*%mu.13.hat)+ log(pi13.hat)) 
  b14t=(x.cv.lda.t %*% solve(S.w)%*%mu.14.hat -as.vector(0.5*mu.14.hat%*% solve(S.w)%*%mu.14.hat)+ log(pi14.hat)) 
  b15t=(x.cv.lda.t %*% solve(S.w)%*%mu.15.hat -as.vector(0.5*mu.15.hat%*% solve(S.w)%*%mu.15.hat) + log(pi15.hat))  # The decision boundary
  x.testb<-cbind(b1t,b2t,b3t,b4t,b5t,b6t,b7t,b8t,b9t,b10t,b11t,b12t,b13t,b14t,b15t)
  
  g.hatbt=apply(x.testb,1,which.max)
  errors[j,1]=mean(g.hatbt!=gg)
  
}
errors
mean.errors=mean(errors)
mean.errors

cv.errors.lda
mean.cv.errors.lda=apply(cv.errors.lda,2,mean)
mean.cv.errors.lda[1]=mean(errors)
min(mean.cv.errors.lda)
#[1] 0.2375
which.min(mean.cv.errors.lda)
# [1] 22

plot(1:p,mean.cv.errors.lda,type="b",col="blue")
points(which.min(mean.cv.errors.lda),min(mean.cv.errors.lda),col="red")

###########???train error

x.scoreft=x.test%*% Re(S.eig$vectors)

g=y.train
x.score <- as.matrix(x.train) %*% x.svd$v
x.pcf<-x.score[,1:22]
x.scoret<- as.matrix(x.test) %*% x.svd$v
x.pctf=x.scoret[,1:22]

x.train1=subset(data.frame(x.pcf),y.train==1)
x.train2=subset(data.frame(x.pcf),y.train==2)
x.train3=subset(data.frame(x.pcf),y.train==3)
x.train4=subset(data.frame(x.pcf),y.train==4)
x.train5=subset(data.frame(x.pcf),y.train==5)
x.train6=subset(data.frame(x.pcf),y.train==6)
x.train7=subset(data.frame(x.pcf),y.train==7)
x.train8=subset(data.frame(x.pcf),y.train==8)
x.train9=subset(data.frame(x.pcf),y.train==9)
x.train10=subset(data.frame(x.pcf),y.train==10)
x.train11=subset(data.frame(x.pcf),y.train==11)
x.train12=subset(data.frame(x.pcf),y.train==12)
x.train13=subset(data.frame(x.pcf),y.train==13)
x.train14=subset(data.frame(x.pcf),y.train==14)
x.train15=subset(data.frame(x.pcf),y.train==15)

mu.hat <- apply(x.pcf,2,mean)
mu.1.hat <- apply(x.train1,2,mean)
mu.2.hat <- apply(x.train2,2,mean)
mu.3.hat <- apply(x.train3,2,mean)
mu.4.hat <- apply(x.train4,2,mean)
mu.5.hat <- apply(x.train5,2,mean)
mu.6.hat <- apply(x.train6,2,mean)
mu.7.hat <- apply(x.train7,2,mean)
mu.8.hat <- apply(x.train8,2,mean)
mu.9.hat <- apply(x.train9,2,mean)
mu.10.hat <- apply(x.train10,2,mean)
mu.11.hat <- apply(x.train11,2,mean)
mu.12.hat <- apply(x.train12,2,mean)
mu.13.hat <- apply(x.train13,2,mean)
mu.14.hat <- apply(x.train14,2,mean)
mu.15.hat <- apply(x.train15,2,mean)


S.w <- ( as.matrix( t(x.train1 - rep(1,n1)%*% t(mu.1.hat))) %*% as.matrix((x.train1 - rep(1,n1)%*% t(mu.1.hat))) +
           as.matrix(t(x.train2 - rep(1,n1)%*% t(mu.2.hat))) %*% as.matrix((x.train2 - rep(1,n1)%*% t(mu.2.hat))) +
           as.matrix( t(x.train3 - rep(1,n1)%*% t(mu.3.hat))) %*% as.matrix((x.train3 - rep(1,n1)%*% t(mu.3.hat))) +
           as.matrix(t(x.train4 - rep(1,n1)%*% t(mu.4.hat))) %*%as.matrix( (x.train4 - rep(1,n1)%*% t(mu.4.hat))) +
           as.matrix(t(x.train5 - rep(1,n1)%*% t(mu.5.hat)))%*% as.matrix((x.train5 - rep(1,n1)%*% t(mu.5.hat)))+
           as.matrix(t(x.train6 - rep(1,n1)%*% t(mu.6.hat))) %*% as.matrix((x.train6 - rep(1,n1)%*% t(mu.6.hat)))+
           as.matrix(t(x.train7 - rep(1,n1)%*% t(mu.7.hat))) %*% as.matrix((x.train7 - rep(1,n1)%*% t(mu.7.hat))) +
           as.matrix( t(x.train8 - rep(1,n1)%*% t(mu.8.hat))) %*% as.matrix((x.train8 - rep(1,n1)%*% t(mu.8.hat))) +
           as.matrix( t(x.train9 - rep(1,n1)%*% t(mu.9.hat))) %*% as.matrix((x.train9 - rep(1,n1)%*% t(mu.9.hat))) +
           as.matrix( t(x.train10 - rep(1,n1)%*% t(mu.10.hat))) %*% as.matrix((x.train10 - rep(1,n1)%*% t(mu.10.hat))) +
           as.matrix(t(x.train11 - rep(1,n1)%*% t(mu.11.hat))) %*% as.matrix((x.train11 - rep(1,n1)%*% t(mu.11.hat))) +
           as.matrix(t(x.train12 - rep(1,n1)%*% t(mu.12.hat))) %*% as.matrix((x.train12 - rep(1,n1)%*% t(mu.12.hat))) +
           as.matrix( t(x.train13 - rep(1,n1)%*% t(mu.13.hat))) %*% as.matrix((x.train13 - rep(1,n1)%*% t(mu.13.hat))) +
           as.matrix( t(x.train14 - rep(1,n1)%*% t(mu.14.hat))) %*% as.matrix((x.train14 - rep(1,n1)%*% t(mu.14.hat))) +  
           as.matrix( t(x.train15 - rep(1,n1)%*% t(mu.15.hat))) %*% as.matrix((x.train15 - rep(1,n1)%*% t(mu.15.hat))) 
)/(n-15)

b1=(x.pcf %*% solve(S.w)%*%mu.1.hat-as.vector(0.5*mu.1.hat%*% solve(S.w)%*%mu.1.hat)+ log(pi1.hat)) # The decision boundary
b2=(x.pcf %*% solve(S.w)%*%mu.2.hat -as.vector(0.5*mu.2.hat%*% solve(S.w)%*%mu.2.hat)+ log(pi2.hat)) 
b3=(x.pcf %*% solve(S.w)%*%mu.3.hat -as.vector(0.5*mu.3.hat%*% solve(S.w)%*%mu.3.hat) + log(pi3.hat)) 
b4=(x.pcf %*% solve(S.w)%*%mu.4.hat-as.vector(0.5*mu.4.hat%*% solve(S.w)%*%mu.4.hat)+ log(pi4.hat)) 
b5=(x.pcf %*% solve(S.w)%*%mu.5.hat -as.vector(0.5*mu.5.hat%*% solve(S.w)%*%mu.5.hat)+ log(pi5.hat)) 
b6=(x.pcf %*% solve(S.w)%*%mu.6.hat -as.vector(0.5*mu.6.hat%*% solve(S.w)%*%mu.6.hat) + log(pi6.hat))
b7=(x.pcf %*% solve(S.w)%*%mu.7.hat-as.vector(0.5*mu.7.hat%*% solve(S.w)%*%mu.7.hat)+ log(pi7.hat)) 
b8=(x.pcf %*% solve(S.w)%*%mu.8.hat -as.vector(0.5*mu.8.hat%*% solve(S.w)%*%mu.8.hat)+ log(pi8.hat)) 
b9=(x.pcf %*% solve(S.w)%*%mu.9.hat -as.vector(0.5*mu.9.hat%*% solve(S.w)%*%mu.9.hat) + log(pi9.hat))
b10=(x.pcf %*% solve(S.w)%*%mu.10.hat-as.vector(0.5*mu.10.hat%*% solve(S.w)%*%mu.10.hat)+ log(pi10.hat)) 
b11=(x.pcf %*% solve(S.w)%*%mu.11.hat -as.vector(0.5*mu.11.hat%*% solve(S.w)%*%mu.11.hat)+ log(pi11.hat)) 
b12=(x.pcf %*% solve(S.w)%*%mu.12.hat -as.vector(0.5*mu.12.hat%*% solve(S.w)%*%mu.12.hat) + log(pi12.hat))
b13=(x.pcf %*% solve(S.w)%*%mu.13.hat-as.vector(0.5*mu.13.hat%*% solve(S.w)%*%mu.13.hat)+ log(pi13.hat)) 
b14=(x.pcf %*% solve(S.w)%*%mu.14.hat -as.vector(0.5*mu.14.hat%*% solve(S.w)%*%mu.14.hat)+ log(pi14.hat)) 
b15=(x.pcf %*% solve(S.w)%*%mu.15.hat -as.vector(0.5*mu.15.hat%*% solve(S.w)%*%mu.15.hat) + log(pi15.hat))


# test
gg=y.test
b1t=(x.pctf %*% solve(S.w)%*%mu.1.hat-as.vector(0.5*mu.1.hat%*% solve(S.w)%*%mu.1.hat)+ log(pi1.hat)) # The decision boundary
b2t=(x.pctf %*% solve(S.w)%*%mu.2.hat -as.vector(0.5*mu.2.hat%*% solve(S.w)%*%mu.2.hat)+ log(pi2.hat)) 
b3t=(x.pctf %*% solve(S.w)%*%mu.3.hat -as.vector(0.5*mu.3.hat%*% solve(S.w)%*%mu.3.hat) + log(pi3.hat)) 
b4t=(x.pctf %*% solve(S.w)%*%mu.4.hat-as.vector(0.5*mu.4.hat%*% solve(S.w)%*%mu.4.hat)+ log(pi4.hat)) 
b5t=(x.pctf %*% solve(S.w)%*%mu.5.hat -as.vector(0.5*mu.5.hat%*% solve(S.w)%*%mu.5.hat)+ log(pi5.hat)) 
b6t=(x.pctf %*% solve(S.w)%*%mu.6.hat -as.vector(0.5*mu.6.hat%*% solve(S.w)%*%mu.6.hat) + log(pi6.hat))
b7t=(x.pctf %*% solve(S.w)%*%mu.7.hat-as.vector(0.5*mu.7.hat%*% solve(S.w)%*%mu.7.hat)+ log(pi7.hat)) 
b8t=(x.pctf %*% solve(S.w)%*%mu.8.hat -as.vector(0.5*mu.8.hat%*% solve(S.w)%*%mu.8.hat)+ log(pi8.hat)) 
b9t=(x.pctf %*% solve(S.w)%*%mu.9.hat -as.vector(0.5*mu.9.hat%*% solve(S.w)%*%mu.9.hat) + log(pi9.hat))
b10t=(x.pctf %*% solve(S.w)%*%mu.10.hat-as.vector(0.5*mu.10.hat%*% solve(S.w)%*%mu.10.hat)+ log(pi10.hat)) 
b11t=(x.pctf %*% solve(S.w)%*%mu.11.hat -as.vector(0.5*mu.11.hat%*% solve(S.w)%*%mu.11.hat)+ log(pi11.hat)) 
b12t=(x.pctf %*% solve(S.w)%*%mu.12.hat -as.vector(0.5*mu.12.hat%*% solve(S.w)%*%mu.12.hat) + log(pi12.hat))
b13t=(x.pctf %*% solve(S.w)%*%mu.13.hat-as.vector(0.5*mu.13.hat%*% solve(S.w)%*%mu.13.hat)+ log(pi13.hat)) 
b14t=(x.pctf %*% solve(S.w)%*%mu.14.hat -as.vector(0.5*mu.14.hat%*% solve(S.w)%*%mu.14.hat)+ log(pi14.hat)) 
b15t=(x.pctf %*% solve(S.w)%*%mu.15.hat -as.vector(0.5*mu.15.hat%*% solve(S.w)%*%mu.15.hat) + log(pi15.hat))  # The decision boundary

x.testb<-cbind(b1t,b2t,b3t,b4t,b5t,b6t,b7t,b8t,b9t,b10t,b11t,b12t,b13t,b14t,b15t)

x.trainb<-cbind(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15)

g.hatb=apply(x.trainb,1,which.max)
g.hatbt=apply(x.testb,1,which.max)
#table(g.hatb,g)
train.error=mean(g.hatb!=g)
# 0.1833333
test.error=mean(g.hatbt!=gg)
#  0.325

###################### LDA with FDA projection ###################

x.scoref=as.matrix(x.train)%*% Re(S.eig$vectors)

set.seed(1)
nfolds=5
n.train=nrow(x.train)
s <- split(sample(n.train),rep(1:nfolds,length=n.train))
p <- ncol(x.train)
cv.errors.lda=matrix(NA,5,15)

for (j in seq(nfolds)){
  x.score.cv<-x.scoref[-s[[j]],]
  y.train.cv=y.train[-s[[j]]]
  y.train.cv<-as.matrix(y.train.cv)
  g <- y.train.cv
  
  n1 <- sum(g==1)
  n2 <- sum(g==2)
  n3 <- sum(g==3)
  n4 <- sum(g==4)
  n5 <- sum(g==5)
  n6 <- sum(g==6)
  n7 <- sum(g==7)
  n8 <- sum(g==8)
  n9 <- sum(g==9)
  n10 <- sum(g==10)
  n11 <- sum(g==11)
  n12 <- sum(g==12)
  n13 <- sum(g==13)
  n14 <- sum(g==14)
  n15 <- sum(g==15)
  
  n <- n1+n2+n3+n4+n5+n6+n7+n8+n9+n10+n11+n12+n13+n14+n15   
  
  pi1.hat <- mean(g==1)
  pi2.hat <- mean(g==2)
  pi3.hat <- mean(g==3)
  pi4.hat <- mean(g==4)
  pi5.hat <- mean(g==5)
  pi6.hat <- mean(g==6)
  pi7.hat <- mean(g==7)
  pi8.hat <- mean(g==8)
  pi9.hat <- mean(g==9)
  pi10.hat <- mean(g==10)
  pi11.hat <- mean(g==11)
  pi12.hat <- mean(g==12)
  pi13.hat <- mean(g==13)
  pi14.hat <- mean(g==14)
  pi15.hat <- mean(g==15)
  
  test.cv=x.scoref[s[[j]],]
  y.test.cv=y.train[s[[j]]]
  y.test.cv<-as.matrix(y.test.cv)
  gg=y.test.cv
  
  
  for(i in 2:15){
    
    x.cv.lda<-data.frame(x.score.cv[,1:i])
    x.cv.lda <- as.matrix(x.cv.lda)
    
    mu.1.hat <- apply(x.cv.lda[g==1,],2,mean)
    mu.2.hat <- apply(x.cv.lda[g==2,],2,mean)
    mu.3.hat <- apply(x.cv.lda[g==3,],2,mean)
    mu.4.hat <- apply(x.cv.lda[g==4,],2,mean)
    mu.5.hat <- apply(x.cv.lda[g==5,],2,mean)
    mu.6.hat <- apply(x.cv.lda[g==6,],2,mean)
    mu.7.hat <- apply(x.cv.lda[g==7,],2,mean)
    mu.8.hat <- apply(x.cv.lda[g==8,],2,mean)
    mu.9.hat <- apply(x.cv.lda[g==9,],2,mean)
    mu.10.hat <- apply(x.cv.lda[g==10,],2,mean)
    mu.11.hat <- apply(x.cv.lda[g==11,],2,mean)
    mu.12.hat <- apply(x.cv.lda[g==12,],2,mean)
    mu.13.hat <- apply(x.cv.lda[g==13,],2,mean)
    mu.14.hat <- apply(x.cv.lda[g==14,],2,mean)
    mu.15.hat <- apply(x.cv.lda[g==15,],2,mean)
    
    # within class covariance
    
    S.w <- (  t(x.cv.lda[g==1,] - rep(1,n1)%*% t(mu.1.hat)) %*% as.matrix((x.cv.lda[g==1,]- rep(1,n1)%*% t(mu.1.hat))) +
                t(x.cv.lda[g==2,] - rep(1,n2)%*% t(mu.2.hat)) %*% as.matrix((x.cv.lda[g==2,] - rep(1,n2)%*% t(mu.2.hat))) +
                t(x.cv.lda[g==3,] - rep(1,n3)%*% t(mu.3.hat)) %*% as.matrix((x.cv.lda[g==3,] - rep(1,n3)%*% t(mu.3.hat))) +
                t(x.cv.lda[g==4,] - rep(1,n4)%*% t(mu.4.hat)) %*%as.matrix( (x.cv.lda[g==4,] - rep(1,n4)%*% t(mu.4.hat))) +
                t(x.cv.lda[g==5,] - rep(1,n5)%*% t(mu.5.hat))%*% as.matrix((x.cv.lda[g==5,] - rep(1,n5)%*% t(mu.5.hat)))+
                t(x.cv.lda[g==6,] - rep(1,n6)%*% t(mu.6.hat)) %*% as.matrix((x.cv.lda[g==6,] - rep(1,n6)%*% t(mu.6.hat)))+
                t(x.cv.lda[g==7,] - rep(1,n7)%*% t(mu.7.hat)) %*% as.matrix((x.cv.lda[g==7,] - rep(1,n7)%*% t(mu.7.hat))) +
                t(x.cv.lda[g==8,] - rep(1,n8)%*% t(mu.8.hat)) %*% as.matrix((x.cv.lda[g==8,] - rep(1,n8)%*% t(mu.8.hat))) +
                t(x.cv.lda[g==9,] - rep(1,n9)%*% t(mu.9.hat)) %*% as.matrix((x.cv.lda[g==9,] - rep(1,n9)%*% t(mu.9.hat))) +
                t(x.cv.lda[g==10,] - rep(1,n10)%*% t(mu.10.hat)) %*% as.matrix((x.cv.lda[g==10,] - rep(1,n10)%*% t(mu.10.hat))) +
                t(x.cv.lda[g==11,] - rep(1,n11)%*% t(mu.11.hat)) %*% as.matrix((x.cv.lda[g==11,] - rep(1,n11)%*% t(mu.11.hat))) +
                t(x.cv.lda[g==12,] - rep(1,n12)%*% t(mu.12.hat)) %*% as.matrix((x.cv.lda[g==12,] - rep(1,n12)%*% t(mu.12.hat))) +
                t(x.cv.lda[g==13,] - rep(1,n13)%*% t(mu.13.hat)) %*% as.matrix((x.cv.lda[g==13,] - rep(1,n13)%*% t(mu.13.hat))) +
                t(x.cv.lda[g==14,] - rep(1,n14)%*% t(mu.14.hat)) %*% as.matrix((x.cv.lda[g==14,] - rep(1,n14)%*% t(mu.14.hat))) +  
                t(x.cv.lda[g==15,] - rep(1,n15)%*% t(mu.15.hat)) %*% as.matrix((x.cv.lda[g==15,] - rep(1,n15)%*% t(mu.15.hat))) 
    )/(n-15)
    # classification
    
    x.cv.lda.t<-data.frame(test.cv[,1:i])
    x.cv.lda.t <- as.matrix(x.cv.lda.t)
    
    b1t=(x.cv.lda.t %*% solve(S.w)%*%mu.1.hat-as.vector(0.5*mu.1.hat%*% solve(S.w)%*%mu.1.hat)+ log(pi1.hat)) # The decision boundary
    b2t=(x.cv.lda.t %*% solve(S.w)%*%mu.2.hat -as.vector(0.5*mu.2.hat%*% solve(S.w)%*%mu.2.hat)+ log(pi2.hat)) 
    b3t=(x.cv.lda.t %*% solve(S.w)%*%mu.3.hat -as.vector(0.5*mu.3.hat%*% solve(S.w)%*%mu.3.hat) + log(pi3.hat)) 
    b4t=(x.cv.lda.t %*% solve(S.w)%*%mu.4.hat-as.vector(0.5*mu.4.hat%*% solve(S.w)%*%mu.4.hat)+ log(pi4.hat)) 
    b5t=(x.cv.lda.t %*% solve(S.w)%*%mu.5.hat -as.vector(0.5*mu.5.hat%*% solve(S.w)%*%mu.5.hat)+ log(pi5.hat)) 
    b6t=(x.cv.lda.t %*% solve(S.w)%*%mu.6.hat -as.vector(0.5*mu.6.hat%*% solve(S.w)%*%mu.6.hat) + log(pi6.hat))
    b7t=(x.cv.lda.t %*% solve(S.w)%*%mu.7.hat-as.vector(0.5*mu.7.hat%*% solve(S.w)%*%mu.7.hat)+ log(pi7.hat)) 
    b8t=(x.cv.lda.t %*% solve(S.w)%*%mu.8.hat -as.vector(0.5*mu.8.hat%*% solve(S.w)%*%mu.8.hat)+ log(pi8.hat)) 
    b9t=(x.cv.lda.t %*% solve(S.w)%*%mu.9.hat -as.vector(0.5*mu.9.hat%*% solve(S.w)%*%mu.9.hat) + log(pi9.hat))
    b10t=(x.cv.lda.t %*% solve(S.w)%*%mu.10.hat-as.vector(0.5*mu.10.hat%*% solve(S.w)%*%mu.10.hat)+ log(pi10.hat)) 
    b11t=(x.cv.lda.t %*% solve(S.w)%*%mu.11.hat -as.vector(0.5*mu.11.hat%*% solve(S.w)%*%mu.11.hat)+ log(pi11.hat)) 
    b12t=(x.cv.lda.t %*% solve(S.w)%*%mu.12.hat -as.vector(0.5*mu.12.hat%*% solve(S.w)%*%mu.12.hat) + log(pi12.hat))
    b13t=(x.cv.lda.t %*% solve(S.w)%*%mu.13.hat-as.vector(0.5*mu.13.hat%*% solve(S.w)%*%mu.13.hat)+ log(pi13.hat)) 
    b14t=(x.cv.lda.t %*% solve(S.w)%*%mu.14.hat -as.vector(0.5*mu.14.hat%*% solve(S.w)%*%mu.14.hat)+ log(pi14.hat)) 
    b15t=(x.cv.lda.t %*% solve(S.w)%*%mu.15.hat -as.vector(0.5*mu.15.hat%*% solve(S.w)%*%mu.15.hat) + log(pi15.hat))  # The decision boundary
    x.testb<-cbind(b1t,b2t,b3t,b4t,b5t,b6t,b7t,b8t,b9t,b10t,b11t,b12t,b13t,b14t,b15t)
    
    g.hatbt=apply(x.testb,1,which.max)
    cv.errors.lda[j,i]=mean(g.hatbt!=gg)
    
  }
}

# when i=1

set.seed(1)
errors=matrix(NA,5,1)
n.train=nrow(x.train)
s <- split(sample(n.train),rep(1:nfolds,length=n.train))
p <- ncol(x.train)


for (j in seq(nfolds)){
  x.score.cv<-x.scoref[-s[[j]],]
  y.train.cv=y.train[-s[[j]]]
  y.train.cv<-as.matrix(y.train.cv)
  g <- y.train.cv
  
  n1 <- sum(g==1)
  n2 <- sum(g==2)
  n3 <- sum(g==3)
  n4 <- sum(g==4)
  n5 <- sum(g==5)
  n6 <- sum(g==6)
  n7 <- sum(g==7)
  n8 <- sum(g==8)
  n9 <- sum(g==9)
  n10 <- sum(g==10)
  n11 <- sum(g==11)
  n12 <- sum(g==12)
  n13 <- sum(g==13)
  n14 <- sum(g==14)
  n15 <- sum(g==15)
  
  n <- n1+n2+n3+n4+n5+n6+n7+n8+n9+n10+n11+n12+n13+n14+n15   
  
  pi1.hat <- mean(g==1)
  pi2.hat <- mean(g==2)
  pi3.hat <- mean(g==3)
  pi4.hat <- mean(g==4)
  pi5.hat <- mean(g==5)
  pi6.hat <- mean(g==6)
  pi7.hat <- mean(g==7)
  pi8.hat <- mean(g==8)
  pi9.hat <- mean(g==9)
  pi10.hat <- mean(g==10)
  pi11.hat <- mean(g==11)
  pi12.hat <- mean(g==12)
  pi13.hat <- mean(g==13)
  pi14.hat <- mean(g==14)
  pi15.hat <- mean(g==15)
  
  
  test.cv=x.scoref[s[[j]],]
  y.test.cv=y.train[s[[j]]]
  y.test.cv<-as.matrix(y.test.cv)
  gg=y.test.cv
  
  x.cv.lda<-data.frame(x.score.cv[,1])
  x.cv.lda <- as.matrix(x.cv.lda)
  
  x.cv.lda<-data.frame(x.score.cv[,1])
  x.cv.lda <- as.matrix(x.cv.lda)
  
  mu.1.hat <- apply(x.cv.lda,2,mean)
  mu.2.hat <- apply(x.cv.lda,2,mean)
  mu.3.hat <- apply(x.cv.lda,2,mean)
  mu.4.hat <- apply(x.cv.lda,2,mean)
  mu.5.hat <- apply(x.cv.lda,2,mean)
  mu.6.hat <- apply(x.cv.lda,2,mean)
  mu.7.hat <- apply(x.cv.lda,2,mean)
  mu.8.hat <- apply(x.cv.lda,2,mean)
  mu.9.hat <- apply(x.cv.lda,2,mean)
  mu.10.hat <- apply(x.cv.lda,2,mean)
  mu.11.hat <- apply(x.cv.lda,2,mean)
  mu.12.hat <- apply(x.cv.lda,2,mean)
  mu.13.hat <- apply(x.cv.lda,2,mean)
  mu.14.hat <- apply(x.cv.lda,2,mean)
  mu.15.hat <- apply(x.cv.lda,2,mean)
  
  # within class covariance
  
  S.w <- (  t(x.cv.lda[g==1,] - rep(1,n1)%*% t(mu.1.hat)) %*% as.matrix((x.cv.lda[g==1,]- rep(1,n1)%*% t(mu.1.hat))) +
              t(x.cv.lda[g==2,] - rep(1,n2)%*% t(mu.2.hat)) %*% as.matrix((x.cv.lda[g==2,] - rep(1,n2)%*% t(mu.2.hat))) +
              t(x.cv.lda[g==3,] - rep(1,n3)%*% t(mu.3.hat)) %*% as.matrix((x.cv.lda[g==3,] - rep(1,n3)%*% t(mu.3.hat))) +
              t(x.cv.lda[g==4,] - rep(1,n4)%*% t(mu.4.hat)) %*%as.matrix( (x.cv.lda[g==4,] - rep(1,n4)%*% t(mu.4.hat))) +
              t(x.cv.lda[g==5,] - rep(1,n5)%*% t(mu.5.hat))%*% as.matrix((x.cv.lda[g==5,] - rep(1,n5)%*% t(mu.5.hat)))+
              t(x.cv.lda[g==6,] - rep(1,n6)%*% t(mu.6.hat)) %*% as.matrix((x.cv.lda[g==6,] - rep(1,n6)%*% t(mu.6.hat)))+
              t(x.cv.lda[g==7,] - rep(1,n7)%*% t(mu.7.hat)) %*% as.matrix((x.cv.lda[g==7,] - rep(1,n7)%*% t(mu.7.hat))) +
              t(x.cv.lda[g==8,] - rep(1,n8)%*% t(mu.8.hat)) %*% as.matrix((x.cv.lda[g==8,] - rep(1,n8)%*% t(mu.8.hat))) +
              t(x.cv.lda[g==9,] - rep(1,n9)%*% t(mu.9.hat)) %*% as.matrix((x.cv.lda[g==9,] - rep(1,n9)%*% t(mu.9.hat))) +
              t(x.cv.lda[g==10,] - rep(1,n10)%*% t(mu.10.hat)) %*% as.matrix((x.cv.lda[g==10,] - rep(1,n10)%*% t(mu.10.hat))) +
              t(x.cv.lda[g==11,] - rep(1,n11)%*% t(mu.11.hat)) %*% as.matrix((x.cv.lda[g==11,] - rep(1,n11)%*% t(mu.11.hat))) +
              t(x.cv.lda[g==12,] - rep(1,n12)%*% t(mu.12.hat)) %*% as.matrix((x.cv.lda[g==12,] - rep(1,n12)%*% t(mu.12.hat))) +
              t(x.cv.lda[g==13,] - rep(1,n13)%*% t(mu.13.hat)) %*% as.matrix((x.cv.lda[g==13,] - rep(1,n13)%*% t(mu.13.hat))) +
              t(x.cv.lda[g==14,] - rep(1,n14)%*% t(mu.14.hat)) %*% as.matrix((x.cv.lda[g==14,] - rep(1,n14)%*% t(mu.14.hat))) +  
              t(x.cv.lda[g==15,] - rep(1,n15)%*% t(mu.15.hat)) %*% as.matrix((x.cv.lda[g==15,] - rep(1,n15)%*% t(mu.15.hat))) 
  )/(n-15)
  
  x.cv.lda.t<-data.frame(test.cv[,1])
  x.cv.lda.t <- as.matrix(x.cv.lda.t)
  colnames(x.cv.lda)="x1"
  colnames(x.cv.lda.t)="x1"  
  
  b1t=(x.cv.lda.t %*% solve(S.w)%*%mu.1.hat-as.vector(0.5*mu.1.hat%*% solve(S.w)%*%mu.1.hat)+ log(pi1.hat)) # The decision boundary
  b2t=(x.cv.lda.t %*% solve(S.w)%*%mu.2.hat -as.vector(0.5*mu.2.hat%*% solve(S.w)%*%mu.2.hat)+ log(pi2.hat)) 
  b3t=(x.cv.lda.t %*% solve(S.w)%*%mu.3.hat -as.vector(0.5*mu.3.hat%*% solve(S.w)%*%mu.3.hat) + log(pi3.hat)) 
  b4t=(x.cv.lda.t %*% solve(S.w)%*%mu.4.hat-as.vector(0.5*mu.4.hat%*% solve(S.w)%*%mu.4.hat)+ log(pi4.hat)) 
  b5t=(x.cv.lda.t %*% solve(S.w)%*%mu.5.hat -as.vector(0.5*mu.5.hat%*% solve(S.w)%*%mu.5.hat)+ log(pi5.hat)) 
  b6t=(x.cv.lda.t %*% solve(S.w)%*%mu.6.hat -as.vector(0.5*mu.6.hat%*% solve(S.w)%*%mu.6.hat) + log(pi6.hat))
  b7t=(x.cv.lda.t %*% solve(S.w)%*%mu.7.hat-as.vector(0.5*mu.7.hat%*% solve(S.w)%*%mu.7.hat)+ log(pi7.hat)) 
  b8t=(x.cv.lda.t %*% solve(S.w)%*%mu.8.hat -as.vector(0.5*mu.8.hat%*% solve(S.w)%*%mu.8.hat)+ log(pi8.hat)) 
  b9t=(x.cv.lda.t %*% solve(S.w)%*%mu.9.hat -as.vector(0.5*mu.9.hat%*% solve(S.w)%*%mu.9.hat) + log(pi9.hat))
  b10t=(x.cv.lda.t %*% solve(S.w)%*%mu.10.hat-as.vector(0.5*mu.10.hat%*% solve(S.w)%*%mu.10.hat)+ log(pi10.hat)) 
  b11t=(x.cv.lda.t %*% solve(S.w)%*%mu.11.hat -as.vector(0.5*mu.11.hat%*% solve(S.w)%*%mu.11.hat)+ log(pi11.hat)) 
  b12t=(x.cv.lda.t %*% solve(S.w)%*%mu.12.hat -as.vector(0.5*mu.12.hat%*% solve(S.w)%*%mu.12.hat) + log(pi12.hat))
  b13t=(x.cv.lda.t %*% solve(S.w)%*%mu.13.hat-as.vector(0.5*mu.13.hat%*% solve(S.w)%*%mu.13.hat)+ log(pi13.hat)) 
  b14t=(x.cv.lda.t %*% solve(S.w)%*%mu.14.hat -as.vector(0.5*mu.14.hat%*% solve(S.w)%*%mu.14.hat)+ log(pi14.hat)) 
  b15t=(x.cv.lda.t %*% solve(S.w)%*%mu.15.hat -as.vector(0.5*mu.15.hat%*% solve(S.w)%*%mu.15.hat) + log(pi15.hat))  # The decision boundary
  x.testb<-cbind(b1t,b2t,b3t,b4t,b5t,b6t,b7t,b8t,b9t,b10t,b11t,b12t,b13t,b14t,b15t)
  
  g.hatbt=apply(x.testb,1,which.max)
  errors[j,1]=mean(g.hatbt!=gg)
  
}
errors
mean.errors=mean(errors)
mean.errors

cv.errors.lda
mean.cv.errors.lda=apply(cv.errors.lda,2,mean)
mean.cv.errors.lda[1]=mean(errors)
min(mean.cv.errors.lda)
#[1] 0.1875
which.min(mean.cv.errors.lda)
# 10

# train error is 0.1666667
# test error is 0.1875


#################### svm #########################################
library(e1071)
svm.train <- svm(y.train~.,data = train,gamma=g,type="C-class",kernel="polynomial",cross=10)

table(predict(svm.train,x.test),y.test)
svm.train.err<- mean(predict(svm.train,x.train) != y.train)
# train error: 0
svm.test.err<-mean(predict(svm.train,x.test)!=as.character(y.test))
# test error: 0.1583333
svm.train$index
summary(svm.train)



##$$$%%^^&*&^%$#%()(*&^%#)
plot(train, col = 1:240 %in% svm.train$index + 1, xlim = c(-5,5), ylim=c(-5,5))
points(newdata, pch = "+", col = 2, cex = 5)

data1=data.frame(cbind(x.scoref[,1],x.scoref[,2]))
names(data1)

data2=data.frame(cbind(x.scoreft[,1],x.scoreft[,2]))
svm.train <- svm(y.train~.,data=data1,gamma=g,type="C-class",kernel="polynomial",cross=10)

table(predict(svm.train,data2),y.test)

svm.train.err<- mean(predict(svm.train,data1) != y.train)
svm.test.err<-mean(predict(svm.train,data2)!=as.character(y.test))
# test error: 0.1583333
#%^&*&^%$##$$%^&*(((*&&^%%%%)))


############################# tree ####################################################################

library(tree)
tree.libra=tree(y.train~.,train)

summary(tree.libra)
plot(tree.libra)
text(tree.libra,pretty=0)
# The argument pretty=0 instructs R to include the category names for any qualitative predictors, rather than simply displaying a letter for each category
tree.libra

set.seed(1)
tree.libra=tree(y.train~.,train)
# The predict() function can be used for this purpose. In the case of a classification tree, the argument type="class" instructs R to return the actual class prediction.
tree.pred=predict(tree.libra,data.frame(x.test),type="class")
mean(tree.pred!=y.test) 
tt=table(tree.pred,y.test)
#  train.error: 0.5416667


# The function cv.tree() performs cross-validation in order to determine the optimal level of tree compleinsurity; cost compleinsurity pruning is used in order to select a sequence of trees for consideration. We use the argument FUN=prune.misclass in order to indicate that we want the classification error rate to guide the cross-validation and pruning process, rather than the default for the cv.tree() function, which is deviance.
set.seed(3)
cv.libra=cv.tree(tree.libra,FUN=prune.misclass)
names(cv.libra)
cv.libra
# The cv.tree() function reports the number of terminal nodes of each tree considered (size) as well as the corresponding error rate and the value of the cost-compleinsurity parameter used (k=alpha)
# despite the name, dev corresponds to the cross-validation error rate in this instance.
par(mfrow=c(1,2))
plot(cv.libra$size,cv.libra$dev,type="b")
plot(cv.libra$k,cv.libra$dev,type="b")
# The tree with 9 terminal nodes results in the lowest cross-validation error rate, with 50 cross-validation errors
min(cv.libra$dev)
cv.libra$size
cv.libra$dev
# We now apply the prune.misclass() function in order to prune the tree to prune. obtain the nine-node tree 
prune.libra=prune.misclass(tree.libra,best=21)
plot(prune.libra)
text(prune.libra,pretty=0)
set.seed(1)
tree.pred1=predict(prune.libra,data.frame(x.test),type="class")
table(tree.pred1,y.test)
mean(tree.pred1!=y.test) 
# test error 0.5666667
tree.pred1=predict(prune.libra,data.frame(x.train),type="class")
table(tree.pred1,y.train)
mean(tree.pred1!=y.train) 
# train error [1] 0.2583333

# Here we apply bagging and random forests to the libra data, using the randomForest package in R.
library(MASS)
library(randomForest)
# Recall that bagging is simply a special case of a random forest with m = sqrt(p). Therefore, the randomForest() function can be used to perform both random forests and bagging

set.seed(1)
bag.libra=randomForest(y.train~.,data=train,mtry=10,ntree=500,importance=TRUE)
# The argument mtry=13 indicates that all 13 predictors should be considered for each split of the tree??in other words, that bagging should be done
bag.libra
# train error 0.2292

round(importance(bag.libra), 2)
varImpPlot(bag.libra)

yhat.bag = predict(bag.libra,data.frame(x.test))
table(yhat.bag,y.test)
mean(yhat.bag!=y.test) 
# 0.2333333


# Here we use the gbm package
# install.packages("gbm")
library(gbm)
set.seed(1)
# We run gbm() with the option distribution="gaussian" since this is a regression problem; if it were a binary classification problem, we would use distribution="bernoulli".

# The argument n.trees=5000 indicates that we want 5000 trees, and the option interaction.depth=4 limits the depth of each tree.
boost.libra=gbm(y.train~.,data=data.frame(x.train),distribution="multinomial",n.trees=500,interaction.depth=4)
# The summary() function produces a relative influence plot and also outputs the relative influence statistics.
summary(boost.libra)


# not good for mutliclass:

# We now use the boosted model to predict medv on the test set
yhat.boost=predict(boost.libra,data.frame(x.train),n.trees=500,type="response")
yhat.boost
yhat=apply(yhat.boost,1,which.max)

table(yhat,y.test)
mean(yhat!=y.test) 

################### maboost
install.packages("maboost")
library(maboost)
set.seed(1)
maboost.libra=maboost(y.train~.,data=data.frame(x.train))
#train.error= 0.025   
yhat.maboost=predict(maboost.libra, newdata = data.frame(x.test), type = c( "class"))
mean(yhat.maboost!=y.test) 
# test.error=0.2833333
table(yhat.maboost,y.test)
varplot.maboost(maboost.libra)
