teta.til.f<-function(beta.til,alfa.til,mu.x.til,sigma2.x.til,sigma2.u.til){
  result<-c(beta.til,alfa.til,mu.x.til,sigma2.x.til,sigma2.u.til);
  result<-matrix(data=result,ncol=1);
  return (result);
}