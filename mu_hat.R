mu.hat.f<-function(alfa.hat,beta.hat,mu.x.hat){  
  result<-c(alfa.hat+(beta.hat*mu.x.hat),mu.x.hat);
  result<-matrix(data=result,ncol=1);
  return (result);  
}