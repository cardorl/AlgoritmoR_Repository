p.22.hat.f<-function(beta.hat,sigma2.x.hat,sigma2.u.hat,p.11.hat){
  result<-sqrt((p.11.hat^2)*(sigma2.x.hat+sigma2.u.hat)-(beta.hat^2)*(sigma2.x.hat^2))/p.11.hat;
  return(result);  
}