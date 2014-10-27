mu_x.til.f<-function(X.barra,Y.barra,beta.til,alfa.til,lambda.e){
  result<-((Y.barra*beta.til)+(lambda.e*X.barra)-(alfa.til*beta.til));
  result<-result/(beta.til^2)+lambda.e;
  return(result);
  
}
  
  