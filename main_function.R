#### Universidade de São Paulo ###
#### Trabalho de Conclusão do Curso de Sistemas de Informação ###
#### Implementação de algoritmo de ajuste para a estatística da razão de verossimilhança no software R ###
#### Ricardo Lemos Cardoso NºUSP 5365160 ###



#### Função Principal que recebe dois vetores X e Y e o tamanho n desses vetores ###
main<-function(X,Y,n){

  
#### Parâmetros Pré-Estabelecidos
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
sigma2.x.til<-sigma.2.x.til.f(X.barra,Y.barra,Sxx,Sxy,Syy,lambda.e,alfa.til,beta.til);
sigma2.u.til<-sigma.2.u.til.f(X.barra,Y.barra,Sxx,Sxy,Syy,lambda.e,alfa.til,beta.til);

###Preenchimento do vetor de parâmetros restritos sob H0
teta.til<-teta.til.f(beta.til,alfa.til,mu.x.til,sigma2.x.til,sigma2.u.til);

#### Preenchimento do vetor mu estimado
mu.hat<-mu.hat.f(alfa.hat,beta.hat,mu.x.hat);

#### Preenchimento do vetor mu estimado sob a hipótese nula
mu.til<-mu.til.f(alfa.til,beta.til,mu.x.til);

#### Preenchimento da matriz Sigma estimada
sigma.hat<-sigma.hat.f(beta.hat,sigma2.x.hat,sigma2.u.hat,lambda.e);

#### Preenchimento da matriz Sigma estimada sob a hipótese nula
sigma.til<-sigma.til.f(beta.til,sigma2.x.til,sigma2.u.til,lambda.e);

#### Inversas das matrizes Sigma.hat e Sigma.til
inv.sigma.hat<-solve(sigma.hat);
inv.sigma.til<-solve(sigma.til);

#### Determinantes das matrizes sigma_hat e sigma_til
det.sigma.hat<-det(sigma.hat);
det.sigma.til<-det(sigma.til);

#### Primeiras derivadas: Vetor de médias estimado (mu)
mu.hat.teta1<-rbind(mu.x.hat,0);
mu.hat.teta2<-rbind(1,0);
mu.hat.teta3<-rbind(beta.hat,1);
mu.hat.teta4<-rbind(0,0);
mu.hat.teta5<-rbind(0,0);

#### Segundas derivadas: Vetor de médias estimado (mu)
mu.hat.teta1.teta1<-rbind(0,0);
mu.hat.teta1.teta2<-rbind(0,0);
mu.hat.teta1.teta3<-rbind(1,0);
mu.hat.teta1.teta4<-rbind(0,0);
mu.hat.teta1.teta5<-rbind(0,0);
mu.hat.teta2.teta1<-rbind(0,0);
mu.hat.teta2.teta2<-rbind(0,0);
mu.hat.teta2.teta3<-rbind(0,0);
mu.hat.teta2.teta4<-rbind(0,0);
mu.hat.teta2.teta5<-rbind(0,0);
mu.hat.teta3.teta1<-rbind(1,0);
mu.hat.teta3.teta2<-rbind(0,0);
mu.hat.teta3.teta3<-rbind(0,0);
mu.hat.teta3.teta4<-rbind(0,0);
mu.hat.teta3.teta5<-rbind(0,0);
mu.hat.teta4.teta1<-rbind(0,0);
mu.hat.teta4.teta2<-rbind(0,0);
mu.hat.teta4.teta3<-rbind(0,0);
mu.hat.teta4.teta4<-rbind(0,0);
mu.hat.teta4.teta5<-rbind(0,0);
mu.hat.teta5.teta1<-rbind(0,0);
mu.hat.teta5.teta2<-rbind(0,0);
mu.hat.teta5.teta3<-rbind(0,0);
mu.hat.teta5.teta4<-rbind(0,0);
mu.hat.teta5.teta5<-rbind(0,0);

#### Primeiras derivadas: Matriz de covariâncias estimada
sigma.hat.teta1<-matrix(data=cbind(2*beta.hat*sigma2.x.hat,sigma2.x.hat,sigma2.x.hat,0),ncol=2);
sigma.hat.teta2<-matrix(data=cbind(0,0,0,0),ncol=2);
sigma.hat.teta3<-matrix(data=cbind(0,0,0,0),ncol=2);
sigma.hat.teta4<-matrix(data=cbind(beta.hat^2,beta.hat,beta.hat,1),ncol=2);
sigma.hat.teta5<-matrix(data=cbind(lambda.e,0,0,1),ncol=2);

#### Segundas derivadas: Matriz de covariâncias estimada

##Array para guardar as matrizes de covariâncias estimada
sigma.hat.tetax.tetay<-vector("list",25)

sigma.hat.teta1.teta1<-matrix(data=cbind(2*sigma2.x.hat,0,0,0),ncol=2);
sigma.hat.teta1.teta2<-matrix(data=cbind(0,0,0,0),ncol=2);
sigma.hat.teta1.teta3<-matrix(data=cbind(0,0,0,0),ncol=2);
sigma.hat.teta1.teta4<-matrix(data=cbind(2*beta.hat,1,1,0),ncol=2);
sigma.hat.teta1.teta5<-matrix(data=cbind(0,0,0,0),ncol=2);

sigma.hat.teta2.teta1<-matrix(data=cbind(0,0,0,0),ncol=2);
sigma.hat.teta2.teta2<-matrix(data=cbind(0,0,0,0),ncol=2);
sigma.hat.teta2.teta3<-matrix(data=cbind(0,0,0,0),ncol=2);
sigma.hat.teta2.teta4<-matrix(data=cbind(0,0,0,0),ncol=2);
sigma.hat.teta2.teta5<-matrix(data=cbind(0,0,0,0),ncol=2);

sigma.hat.teta3.teta1<-matrix(data=cbind(0,0,0,0),ncol=2);
sigma.hat.teta3.teta2<-matrix(data=cbind(0,0,0,0),ncol=2);
sigma.hat.teta3.teta3<-matrix(data=cbind(0,0,0,0),ncol=2);
sigma.hat.teta3.teta4<-matrix(data=cbind(0,0,0,0),ncol=2);
sigma.hat.teta3.teta5<-matrix(data=cbind(0,0,0,0),ncol=2);

sigma.hat.teta4.teta1<-matrix(data=cbind(2*beta.hat,1,1,0),ncol=2);
sigma.hat.teta4.teta2<-matrix(data=cbind(0,0,0,0),ncol=2);
sigma.hat.teta4.teta3<-matrix(data=cbind(0,0,0,0),ncol=2);
sigma.hat.teta4.teta4<-matrix(data=cbind(0,0,0,0),ncol=2);
sigma.hat.teta4.teta5<-matrix(data=cbind(0,0,0,0),ncol=2);

sigma.hat.teta5.teta1<-matrix(data=cbind(0,0,0,0),ncol=2);
sigma.hat.teta5.teta2<-matrix(data=cbind(0,0,0,0),ncol=2);
sigma.hat.teta5.teta3<-matrix(data=cbind(0,0,0,0),ncol=2);
sigma.hat.teta5.teta4<-matrix(data=cbind(0,0,0,0),ncol=2);
sigma.hat.teta5.teta5<-matrix(data=cbind(0,0,0,0),ncol=2);



#### Primeiras derivadas: Inversa da matriz de covariâncias estimada
inv.sigma.hat.teta1<-(-(inv.sigma.hat))*sigma.hat.teta1*inv.sigma.hat;
inv.sigma.hat.teta2<-(-(inv.sigma.hat))*sigma.hat.teta2*inv.sigma.hat;
inv.sigma.hat.teta3<-(-(inv.sigma.hat))*sigma.hat.teta3*inv.sigma.hat;
inv.sigma.hat.teta4<-(-(inv.sigma.hat))*sigma.hat.teta4*inv.sigma.hat;
inv.sigma.hat.teta5<-(-(inv.sigma.hat))*sigma.hat.teta5*inv.sigma.hat;

####Segundas derivadas:inversa da matriz de covariâncias estimada
inv.Sigma.teta1.teta1.hat<--2*(inv.sigma.hat.teta1*sigma.hat.teta1*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta1.teta1*inv.sigma.hat);
inv.Sigma.teta1.teta2.hat<--2*(inv.sigma.hat.teta2*sigma.hat.teta1*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta1.teta2*inv.sigma.hat);
inv.Sigma.teta1.teta3.hat<--2*(inv.sigma.hat.teta3*sigma.hat.teta1*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta1.teta3*inv.sigma.hat);
inv.Sigma.teta1.teta4.hat<--2*(inv.sigma.hat.teta4*sigma.hat.teta1*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta1.teta4*inv.sigma.hat);
inv.Sigma.teta1.teta5.hat<--2*(inv.sigma.hat.teta5*sigma.hat.teta1*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta1.teta5*inv.sigma.hat);
inv.Sigma.teta2.teta1.hat<--2*(inv.sigma.hat.teta1*sigma.hat.teta2*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta2.teta1*inv.sigma.hat);
inv.Sigma.teta2.teta2.hat<--2*(inv.sigma.hat.teta2*sigma.hat.teta2*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta2.teta2*inv.sigma.hat);
inv.Sigma.teta2.teta3.hat<--2*(inv.sigma.hat.teta3*sigma.hat.teta2*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta2.teta3*inv.sigma.hat);
inv.Sigma.teta2.teta4.hat<--2*(inv.sigma.hat.teta4*sigma.hat.teta2*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta2.teta4*inv.sigma.hat);
inv.Sigma.teta2.teta5.hat<--2*(inv.sigma.hat.teta5*sigma.hat.teta2*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta2.teta5*inv.sigma.hat);
inv.Sigma.teta3.teta1.hat<--2*(inv.sigma.hat.teta1*sigma.hat.teta3*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta3.teta1*inv.sigma.hat);
inv.Sigma.teta3.teta2.hat<--2*(inv.sigma.hat.teta2*sigma.hat.teta3*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta3.teta2*inv.sigma.hat);
inv.Sigma.teta3.teta3.hat<--2*(inv.sigma.hat.teta3*sigma.hat.teta3*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta3.teta3*inv.sigma.hat);
inv.Sigma.teta3.teta4.hat<--2*(inv.sigma.hat.teta4*sigma.hat.teta3*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta3.teta4*inv.sigma.hat);
inv.Sigma.teta3.teta5.hat<--2*(inv.sigma.hat.teta5*sigma.hat.teta3*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta3.teta5*inv.sigma.hat);
inv.Sigma.teta4.teta1.hat<--2*(inv.sigma.hat.teta1*sigma.hat.teta4*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta4.teta1*inv.sigma.hat);
inv.Sigma.teta4.teta2.hat<--2*(inv.sigma.hat.teta2*sigma.hat.teta4*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta4.teta2*inv.sigma.hat);
inv.Sigma.teta4.teta3.hat<--2*(inv.sigma.hat.teta3*sigma.hat.teta4*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta4.teta3*inv.sigma.hat);
inv.Sigma.teta4.teta4.hat<--2*(inv.sigma.hat.teta4*sigma.hat.teta4*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta4.teta4*inv.sigma.hat);
inv.Sigma.teta4.teta5.hat<--2*(inv.sigma.hat.teta5*sigma.hat.teta4*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta4.teta5*inv.sigma.hat);
inv.Sigma.teta5.teta1.hat<--2*(inv.sigma.hat.teta1*sigma.hat.teta5*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta5.teta1*inv.sigma.hat);
inv.Sigma.teta5.teta2.hat<--2*(inv.sigma.hat.teta2*sigma.hat.teta5*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta5.teta2*inv.sigma.hat);
inv.Sigma.teta5.teta3.hat<--2*(inv.sigma.hat.teta3*sigma.hat.teta5*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta5.teta3*inv.sigma.hat);
inv.Sigma.teta5.teta4.hat<--2*(inv.sigma.hat.teta4*sigma.hat.teta5*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta5.teta4*inv.sigma.hat);
inv.Sigma.teta5.teta5.hat<--2*(inv.sigma.hat.teta5*sigma.hat.teta5*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta5.teta5*inv.sigma.hat);

#### Primeiras derivadas: Vetor de médias estimado (mu) restrito em H0
mu.til.teta1<-rbind(mu.x.til,0);
mu.til.teta2<-rbind(1,0);
mu.til.teta3<-rbind(beta.til,1);
mu.til.teta4<-rbind(0,0);
mu.til.teta5<-rbind(0,0);

#### Segundas derivadas: Vetor de médias estimado (mu) restrito em H0
mu.til.teta1.teta1<-rbind(0,0);
mu.til.teta1.teta2<-rbind(0,0);
mu.til.teta1.teta3<-rbind(1,0);
mu.til.teta1.teta4<-rbind(0,0);
mu.til.teta1.teta5<-rbind(0,0);
mu.til.teta2.teta1<-rbind(0,0);
mu.til.teta2.teta2<-rbind(0,0);
mu.til.teta2.teta3<-rbind(0,0);
mu.til.teta2.teta4<-rbind(0,0);
mu.til.teta2.teta5<-rbind(0,0);
mu.til.teta3.teta1<-rbind(1,0);
mu.til.teta3.teta2<-rbind(0,0);
mu.til.teta3.teta3<-rbind(0,0);
mu.til.teta3.teta4<-rbind(0,0);
mu.til.teta3.teta5<-rbind(0,0);
mu.til.teta4.teta1<-rbind(0,0);
mu.til.teta4.teta2<-rbind(0,0);
mu.til.teta4.teta3<-rbind(0,0);
mu.til.teta4.teta4<-rbind(0,0);
mu.til.teta4.teta5<-rbind(0,0);
mu.til.teta5.teta1<-rbind(0,0);
mu.til.teta5.teta2<-rbind(0,0);
mu.til.teta5.teta3<-rbind(0,0);
mu.til.teta5.teta4<-rbind(0,0);
mu.til.teta5.teta5<-rbind(0,0);

#### Primeiras derivadas: Matriz de covariâncias estimada sob H0
sigma.til.teta1<-matrix(data=cbind(2*beta.til*sigma2.x.til,sigma2.x.til,sigma2.x.til,0),ncol=2);
sigma.til.teta2<-matrix(data=cbind(0,0,0,0),ncol=2);
sigma.til.teta3<-matrix(data=cbind(0,0,0,0),ncol=2);
sigma.til.teta4<-matrix(data=cbind(beta.til^2,beta.til,beta.til,1),ncol=2);
sigma.til.teta5<-matrix(data=cbind(lambda.e,0,0,1),ncol=2);

#### Segundas derivadas: Matriz de covariâncias estimada sob H0
sigma.til.teta1.teta1<-matrix(data=cbind(2*sigma2.x.til,0,0,0),ncol=2);
sigma.til.teta1.teta2<-matrix(data=cbind(0,0,0,0),ncol=2);
sigma.til.teta1.teta3<-matrix(data=cbind(0,0,0,0),ncol=2);
sigma.til.teta1.teta4<-matrix(data=cbind(2*beta.til,1,1,0),ncol=2);
sigma.til.teta1.teta5<-matrix(data=cbind(0,0,0,0),ncol=2);

sigma.til.teta2.teta1<-matrix(data=cbind(0,0,0,0),ncol=2);
sigma.til.teta2.teta2<-matrix(data=cbind(0,0,0,0),ncol=2);
sigma.til.teta2.teta3<-matrix(data=cbind(0,0,0,0),ncol=2);
sigma.til.teta2.teta4<-matrix(data=cbind(0,0,0,0),ncol=2);
sigma.til.teta2.teta5<-matrix(data=cbind(0,0,0,0),ncol=2);

sigma.til.teta3.teta1<-matrix(data=cbind(0,0,0,0),ncol=2);
sigma.til.teta3.teta2<-matrix(data=cbind(0,0,0,0),ncol=2);
sigma.til.teta3.teta3<-matrix(data=cbind(0,0,0,0),ncol=2);
sigma.til.teta3.teta4<-matrix(data=cbind(0,0,0,0),ncol=2);
sigma.til.teta3.teta5<-matrix(data=cbind(0,0,0,0),ncol=2);

sigma.til.teta4.teta1<-matrix(data=cbind(2*beta.til,1,1,0),ncol=2);
sigma.til.teta4.teta2<-matrix(data=cbind(0,0,0,0),ncol=2);
sigma.til.teta4.teta3<-matrix(data=cbind(0,0,0,0),ncol=2);
sigma.til.teta4.teta4<-matrix(data=cbind(0,0,0,0),ncol=2);
sigma.til.teta4.teta5<-matrix(data=cbind(0,0,0,0),ncol=2);

sigma.til.teta5.teta1<-matrix(data=cbind(0,0,0,0),ncol=2);
sigma.til.teta5.teta2<-matrix(data=cbind(0,0,0,0),ncol=2);
sigma.til.teta5.teta3<-matrix(data=cbind(0,0,0,0),ncol=2);
sigma.til.teta5.teta4<-matrix(data=cbind(0,0,0,0),ncol=2);
sigma.til.teta5.teta5<-matrix(data=cbind(0,0,0,0),ncol=2);

#### Primeiras derivadas: Inversa da matriz de covariâncias estimada sob H0
inv.sigma.hat.teta1<-(-(inv.sigma.hat))*sigma.hat.teta1*inv.sigma.hat;
inv.sigma.hat.teta2<-(-(inv.sigma.hat))*sigma.hat.teta2*inv.sigma.hat;
inv.sigma.hat.teta3<-(-(inv.sigma.hat))*sigma.hat.teta3*inv.sigma.hat;
inv.sigma.hat.teta4<-(-(inv.sigma.hat))*sigma.hat.teta4*inv.sigma.hat;
inv.sigma.hat.teta5<-(-(inv.sigma.hat))*sigma.hat.teta5*inv.sigma.hat;

####Segundas derivadas:Inversa da matriz de covariâncias estimada sob H0
inv.Sigma.teta1.teta1.hat<--2*(inv.sigma.hat.teta1*sigma.hat.teta1*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta1.teta1*inv.sigma.hat);
inv.Sigma.teta1.teta2.hat<--2*(inv.sigma.hat.teta2*sigma.hat.teta1*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta1.teta2*inv.sigma.hat);
inv.Sigma.teta1.teta3.hat<--2*(inv.sigma.hat.teta3*sigma.hat.teta1*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta1.teta3*inv.sigma.hat);
inv.Sigma.teta1.teta4.hat<--2*(inv.sigma.hat.teta4*sigma.hat.teta1*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta1.teta4*inv.sigma.hat);
inv.Sigma.teta1.teta5.hat<--2*(inv.sigma.hat.teta5*sigma.hat.teta1*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta1.teta5*inv.sigma.hat);
inv.Sigma.teta2.teta1.hat<--2*(inv.sigma.hat.teta1*sigma.hat.teta2*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta2.teta1*inv.sigma.hat);
inv.Sigma.teta2.teta2.hat<--2*(inv.sigma.hat.teta2*sigma.hat.teta2*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta2.teta2*inv.sigma.hat);
inv.Sigma.teta2.teta3.hat<--2*(inv.sigma.hat.teta3*sigma.hat.teta2*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta2.teta3*inv.sigma.hat);
inv.Sigma.teta2.teta4.hat<--2*(inv.sigma.hat.teta4*sigma.hat.teta2*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta2.teta4*inv.sigma.hat);
inv.Sigma.teta2.teta5.hat<--2*(inv.sigma.hat.teta5*sigma.hat.teta2*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta2.teta5*inv.sigma.hat);
inv.Sigma.teta3.teta1.hat<--2*(inv.sigma.hat.teta1*sigma.hat.teta3*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta3.teta1*inv.sigma.hat);
inv.Sigma.teta3.teta2.hat<--2*(inv.sigma.hat.teta2*sigma.hat.teta3*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta3.teta2*inv.sigma.hat);
inv.Sigma.teta3.teta3.hat<--2*(inv.sigma.hat.teta3*sigma.hat.teta3*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta3.teta3*inv.sigma.hat);
inv.Sigma.teta3.teta4.hat<--2*(inv.sigma.hat.teta4*sigma.hat.teta3*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta3.teta4*inv.sigma.hat);
inv.Sigma.teta3.teta5.hat<--2*(inv.sigma.hat.teta5*sigma.hat.teta3*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta3.teta5*inv.sigma.hat);
inv.Sigma.teta4.teta1.hat<--2*(inv.sigma.hat.teta1*sigma.hat.teta4*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta4.teta1*inv.sigma.hat);
inv.Sigma.teta4.teta2.hat<--2*(inv.sigma.hat.teta2*sigma.hat.teta4*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta4.teta2*inv.sigma.hat);
inv.Sigma.teta4.teta3.hat<--2*(inv.sigma.hat.teta3*sigma.hat.teta4*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta4.teta3*inv.sigma.hat);
inv.Sigma.teta4.teta4.hat<--2*(inv.sigma.hat.teta4*sigma.hat.teta4*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta4.teta4*inv.sigma.hat);
inv.Sigma.teta4.teta5.hat<--2*(inv.sigma.hat.teta5*sigma.hat.teta4*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta4.teta5*inv.sigma.hat);
inv.Sigma.teta5.teta1.hat<--2*(inv.sigma.hat.teta1*sigma.hat.teta5*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta5.teta1*inv.sigma.hat);
inv.Sigma.teta5.teta2.hat<--2*(inv.sigma.hat.teta2*sigma.hat.teta5*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta5.teta2*inv.sigma.hat);
inv.Sigma.teta5.teta3.hat<--2*(inv.sigma.hat.teta3*sigma.hat.teta5*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta5.teta3*inv.sigma.hat);
inv.Sigma.teta5.teta4.hat<--2*(inv.sigma.hat.teta4*sigma.hat.teta5*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta5.teta4*inv.sigma.hat);
inv.Sigma.teta5.teta5.hat<--2*(inv.sigma.hat.teta5*sigma.hat.teta5*inv.sigma.hat)-(inv.sigma.hat*sigma.hat.teta5.teta5*inv.sigma.hat);

#### Decomposição de Choleski da matriz Sigma estimada e sua inversa
p.11.hat<-p.11.hat.f(beta.hat,sigma2.x.hat,sigma2.u.hat,lambda.e);
p.12.hat<-0
p.21.hat<-p.21.hat.f(beta.hat,sigma2.x.hat,p.11.hat);
p.22.hat<-p.22.hat.f(beta.hat,sigma2.x.hat,sigma2.u.hat,p.11.hat);

p.hat<-matrix(data=rbind(p.11.hat,p.12.hat,p.21.hat,p.22.hat),ncol=2);
inv.P.hat<-solve(p.hat);

#### Derivadas da matriz de decomposição de Choleski de Sigma estimada
p.hat.teta1<-p.hat.teta1.f(p.11.hat,p.21.hat,p.22.hat,sigma2.x.hat,sigma2.u.hat,lambda.e);
p.hat.teta2<-matrix(data=rbind(0,0,0,0),ncol=2);
p.hat.teta3<-matrix(data=rbind(0,0,0,0),ncol=2);
p.hat.teta4<-p.hat.teta4.f(beta.hat,p.11.hat,p.22.hat,sigma2.x.hat,sigma2.u.hat,lambda.e);
p.hat.teta5<-p.hat.teta5.f(beta.hat,p.11.hat,p.21.hat,p.22.hat,sigma2.x.hat,lambda.e);



#### Matrizes úteis na composição da matriz J.hat e l.hat.linha
#T.hat<-
#B.hat<-
#C.hat<-
#Q.hat<-
#M.hat<-
R.hat<-R.hat.f(n);
V.hat<-V.hat,f(n);
#W.hat<-
p.hat.teta1
p.hat.teta2
p.hat.teta3
p.hat.teta4
p.hat.teta5

mu.hat.teta1
mu.hat.teta2
mu.hat.teta3
mu.hat.teta4
mu.hat.teta5


inv.sigma.hat
p.hat
inv.P.hat
X e Y

  
#### Matrizes úteis na composição da matriz J.til e l.til.linha e U.linha.til
#T.til<-
#B.til<-
#C.til<-
#Q.til<-
#M.til<-
R.til<-R.til.f(n);
V.til<-V.til.f(n);
#W.til<-

inv.sigma.til



cbind(R.hat)
  

}
  