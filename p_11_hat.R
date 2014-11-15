p.11.hat.f<-function(beta.hat,sigma2.x.hat,sigma2.u.hat,lambda.e){
  result<-sqrt((beta.hat^2)*sigma2.x.hat+lambda.e*sigma2.u.hat);
  return(result);
}