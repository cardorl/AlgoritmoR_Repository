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
mu.x.hat<-X.barra
alfa.hat<-alfa.hat.f(X,Y,beta.hat);
sigma2.x.hat<-sigma2.x.hat.f(Sxy,beta.hat);
sigma2.u.hat<-sigma2.u.hat.f(Sxx,Syy,Sxy,beta.hat,lambda.e);

#### Estimativas de máxima verossimilhança (e.m.v.) sob a hipótese nula
beta.til<-beta.0;
alfa.til<-Y.barra-beta.til*X.barra;
mu_x.til<-mu_x.til.f(X.barra,Y.barra,beta.til,alfa.til,lambda.e);

  
  
  