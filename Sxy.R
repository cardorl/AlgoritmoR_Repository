Sxy.f<-function(X,Y,n){
  sumxy<-0;
  X.mean<-mean(X);
  Y.mean<-mean(Y);  
  for(i in 1:n){    
    sumxy<-sumxy+((X[i]-X.mean)*(Y[i]-Y.mean));       
  }
  sumxy<-(1/n)*sumxy;
  return(sumxy);  
}