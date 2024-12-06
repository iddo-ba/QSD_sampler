# Min QSD sampler 

N =3
p = matrix(0,nrow=N,ncol=N)
q = 0.6; s= 0.2
p[1,2]=0.6
p[2,3]=0.4
p[3,1]=0.3
p = p + diag(s,3)
 
QSD = eigen(t(p))$vectors[,1]
QSD=Re(QSD/sum(QSD))

psample = function (x=1,Nloops=100)
{
  path= c()
  nx=x
  times = c()
  T=0
  while (Nloops >0)
  {
   path=c(path,nx)
   pdie = 1-sum(p[nx,])
   pvec = c(pdie,p[nx,])
   nx = sample(0:N,1,prob=pvec)
   T=T+1
   path= c(path,nx)
   if (nx == 0 || nx ==x) { 
     Nloops = Nloops-1
     nx=x
     times = c(times,T)
     T=0
   }
  }
  return(times)
}

path=psample(x=3,Nloops=10000)
cf = 1/Re(eigen(p)$values[1])
mydata = sum(cf^path-1)/length(path)
QSDest = (cf-1)/mydata







