Sxx.f<-function(X,n){
  sumxx<-0;
  X.mean<-mean(X);
  for(i in 1:n){    
    sumxx<-sumxx+((X[i]-X.mean)^2);       
  }
  sumxx<-(1/n)*sumxx;
  return(sumxx);  
}