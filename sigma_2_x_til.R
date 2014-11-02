sigma.2.x.til.f<-function(X.barra,Y.barra,Sxx,Sxy,Syy,lambda.e,alfa.til,beta.til){
  result<-(Syy*(beta.til^2-lambda.e))+(4*beta.til*lambda.e*Sxy)-(Sxx*lambda.e*(beta.til^2-lambda.e));
  result<-result-(((X.barra*beta.til+alfa.til)^2)*lambda.e)+(Y.barra*lambda.e*(2*alfa.til-Y.barra+2*X.barra*beta.til));
  result<-result/((beta.til^2+lambda.e)^2);
  return(result);  
}