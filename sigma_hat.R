sigma.hat.f<-function(beta.hat,sigma2.x.hat,sigma2.u.hat,lambda.e){
  resultcol1<-c(((beta.hat^2)*(sigma2.x.hat))+(lambda.e*sigma2.u.hat),(beta.hat*sigma2.x.hat))
  resultcol2<-c((beta.hat*sigma2.x.hat),(sigma2.x.hat+sigma2.u.hat));
  result<-cbind(resultcol1,resultcol2);
  return(result);
}