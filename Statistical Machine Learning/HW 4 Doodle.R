
library(readr)

train_3<-read.csv('train_3.txt', header = FALSE)
train_8<-read.csv('train_8.txt', header = FALSE)
train_3$digit<--1
train_8$digit<-1

X<-rbind(train_3,train_8)
testind<-sample(seq(1,nrow(X)),nrow(X)*.2)
test<-X[testind,]
X<-X[-testind,]
y<-X$digit

adaclassify<-function(case, j_stars=j_stars, theta_stars=theta_stars, alphas=alphas){
  class_sum<-0
  for(g in 1:length(j_stars)){
    class_sum<-class_sum+(alphas[g]*ifelse(case[j_stars[g]]>theta_stars[g],1,-1))
  }
  ifelse(class_sum>0,1,-1)
}


theta_err<-function(X,theta,index,w,y){
  c<-rep(0,length(X[,index]))
  c<-ifelse(X[,index]>theta,1,-1)
  errs<-w*ifelse(y==c,0,1)
  sum(errs)
}

ada_errors<-function(ada,test){
  y_pred_train<-apply(X, 1, function(x) adaclassify(x, ada$j_stars, ada$theta_stars, ada$alphas))
  y_pred_test<-apply(test, 1, function(x) adaclassify(x, ada$j_stars, ada$theta_stars, ada$alphas))
  train_error<-mean(ifelse(y_pred_train==X$digit,0,1))
  test_error<-mean(ifelse(y_pred_test==test$digit,0,1))
  c(train_error,test_error)
  
}

adaboost_digits<-function(X, B){
  y<-X$digit
  weights<-rep(1/nrow(X), nrow(X))
  j_stars<-rep(0,B)
  theta_stars<-rep(0,B)
  alphas<-rep(0,B)
  
  for (b in 1:B){
    
    j_star<-which.min(sapply(seq(1,256),function(j) min(sapply(seq(-1,1,.1), function(x) theta_err(X,x,j,weights,y)))))
    theta_star<-seq(-1,1,.1)[which.min(sapply(seq(-1,1,.1), function(x) theta_err(X,x,j_star,weights,y)))]
    
    c<-ifelse(X[,j_star]>theta_star,1,-1)
    
    j_stars[b]<-j_star
    theta_stars[b]<-theta_star
    
    epsilon<-sum(weights*ifelse(y==c,0,1))/sum(weights)
    alphas[b]<-log((1-epsilon)/epsilon)
    weights<-weights*exp(alphas[b]*ifelse(y==c,0,1))
  }
  list(j_stars=j_stars,theta_stars=theta_stars,alphas=alphas)
}

crossvalerror<-function(X, B, k){ 
  X$fold<-ceiling(runif(nrow(X))*k)
  folderrors<-c()
  for (i in 1:k){
    curfold<-X[X$fold==i,]
    temptrain<-X[X$fold!=i,]
    tempada<-adaboost_digits(temptrain,B)
    folderrors<-append(folderrors, ada_errors(tempada, curfold)[2])
  }
  mean(folderrors)
}


train_errors<-c()
test_errors<-c()

## These had to be manually calculated using the above crossvalerror() function as they took several minutes each to run
crossval_errors<-c(0.12724,0.07879108, 0.06243371, 0.06541612, 0.05727944,
                   0.05675727,0.05950183, 0.04878585, 0.05821127, 0.05409029, 0.06067585)

bs<-c(1,seq(10,100,10))

for (i in c(1,seq(10,100,10))){
  temp<-ada_errors(adaboost_digits(X,i),test)
  train_errors<-append(train_errors,temp[1])
  test_errors<-append(test_errors,temp[2])
}



plot(bs, test_errors, type = "l", col = "blue", xlab = "Number of Learners", ylab = "Error", ylim = c(0,0.15))
lines(bs, train_errors, col = "red")
lines(bs, crossval_errors, col = "purple")
legend("topright", legend = c("Training Error", "Test Error", "Cross-Validation Error"), lty = c(1,1,1), col = c("red", "blue", "purple"))

