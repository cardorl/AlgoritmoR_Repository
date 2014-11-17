p.hat.teta4.f<-function(beta.hat,p.11.hat,p.22.hat,sigma2.x.hat,sigma2.u.hat,lambda.e){
  p11<-(beta.hat^2)/2*p.11.hat;
  p12<-0;
  p21<-((beta.hat^3)*sigma2.x.hat+(2*beta.hat*lambda.e*sigma2.u.hat))/2*(p.11.hat^3);
  p22<-((lambda.e^2)*(sigma2.u.hat^2))/2*(p.11.hat^4)*p.22.hat;
  result<-matrix(data=rbind(p11,p12,p21,p22),ncol=2);
  return(result);
  
}