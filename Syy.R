Syy.f<-function(Y,n){
  sumyy<-0;
  Y.mean<-mean(Y);
  for(i in 1:n){    
    sumyy<-sumyy+((Y[i]-Y.mean)^2);       
  }
  sumyy<-(1/n)*sumyy;
  return(sumyy);  
}