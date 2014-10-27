beta.hat.f<-function(Sxx,Syy,Sxy,lambda.e){
  sqrt.aux<-sqrt(((Syy-(Sxx*lambda.e))^2)+((4*lambda.e)*(Sxy)^2))
  beta.h=(Syy-(Sxx*lambda.e)+sqrt.aux)/2*Sxy;
  return(beta.h);
}