R.hat.f<- function(n){
  r.hat=matrix(rep(-1/2,n),ncol=1);
  aux.matrix=matrix(data=rep(0,5*n*5),nrow=5*n,ncol=5);
  j<-1;
  k<-n;
  for(i in 1:5){    
    aux.matrix[j:k,i]=r.hat; 
    j<-j+n;
    k<-k+n;
  }  
  return(aux.matrix);  
}
