p.hat.teta1.f<-function(p.11.hat,p.21.hat,p.22.hat,sigma2.x.hat,sigma2.u.hat,lambda.e){
  
  p11<-p.21.hat;
  p12<-0;
  p21<-(lambda.e*sigma2.x.hat*sigma2.u.hat)/(p.11.hat^3);
  p22<-(-lambda.e*sigma2.x.hat*sigma2.u.hat*p.21.hat)/(p.11.hat^3)*(p.22.hat);
  result<-matrix(data=rbind(p11,p12,p21,p22),ncol=2);
  return(result);
  
}