alfa.hat<-function(X,Y,beta.hat){
  X.mean<-mean(X);
  Y.mean<-mean(Y);
  alfa.h<-Y.mean-(beta.hat*X.mean);
  return(alfa.h);  
}