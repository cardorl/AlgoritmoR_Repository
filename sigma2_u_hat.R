sigma2.u.hat.f<-function(Sxx,Syy,Sxy,beta.hat,lambda.e){
  sig.aux=(Syy-(2*beta.hat*Sxy)+((beta.hat^2)*Sxx))/((beta.hat^2)+lambda.e);
  return(sig.aux);
  
}