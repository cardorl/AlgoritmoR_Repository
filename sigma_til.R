sigma.til.f<-function(beta.til,sigma2.x.til,sigma2.u.til,lambda.e){
  resultcol1<-c(((beta.til^2)*(sigma2.x.til))+(lambda.e*sigma2.u.til),(beta.til*sigma2.x.til))
  resultcol2<-c((beta.til*sigma2.x.til),(sigma2.x.til+sigma2.u.til));
  result<-cbind(resultcol1,resultcol2);
  return(result);
  
}