p.21.hat.f<-function(beta.hat,sigma2.x.hat,p.11.hat){
  result<-(beta.hat*sigma2.x.hat)/p.11.hat;
  return(result);  
}