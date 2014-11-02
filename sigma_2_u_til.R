sigma.2.u.til.f<-function(X.barra,Y.barra,Sxx,Sxy,Syy,lambda.e,alfa.til,beta.til){
  result<-Syy-(2*beta.til*Sxy)+((beta.til^2)*Sxx)+((X.barra*beta.til-Y.barra+alfa.til)^2)
  result<-result/((beta.til^2)+lambda.e);
  return(result);  
}