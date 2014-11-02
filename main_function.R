main<-function(X,Y,n){

beta.0<-1;
lambda.e<-1;

#### Somas de quadrados usados para obtenção das estimativas
X.barra<-mean(X);
Y.barra<-mean(Y);
Sxx<-Sxx.f(X,n);
Syy<-Syy.f(Y,n);
Sxy<-Sxy.f(X,Y,n);

#### Estimativas de máxima verossimilhança (e.m.v.)
beta.hat<-beta.hat.f(Sxx,Syy,Sxy,lambda.e);
mu.x.hat<-mu.x.hat.f(X.barra);
alfa.hat<-alfa.hat.f(X,Y,beta.hat);
sigma2.x.hat<-sigma2.x.hat.f(Sxy,beta.hat);
sigma2.u.hat<-sigma2.u.hat.f(Sxx,Syy,Sxy,beta.hat,lambda.e);

#### Estimativas de máxima verossimilhança (e.m.v.) sob a hipótese nula
beta.til<-beta.0;
alfa.til<-alfa.til.f(X.barra,Y.barra,beta.til);
mu.x.til<-mu.x.til.f(X.barra,Y.barra,beta.til,alfa.til,lambda.e);
sigma.2.x.til<-sigma.2.x.til.f(X.barra,Y.barra,Sxx,Sxy,Syy,lambda.e,alfa.til,beta.til);
sigma.2.u.til<-sigma.2.u.til.f(X.barra,Y.barra,Sxx,Sxy,Syy,lambda.e,alfa.til,beta.til);

###Preenchimento do vetor de parâmetros restritos sob H0
teta.til<-teta.til.f(beta.til,alfa.til,mu.x.til,sigma.2.x.til,sigma.2.u.til);



#### Preenchimento do vetor mu estimado
mu.hat<-mu.hat.f(alfa.hat,beta.hat,mu.x.hat);
return(mu.hat);

}
  