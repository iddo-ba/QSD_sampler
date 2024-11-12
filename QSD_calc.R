[# QSD Calc
library(rgl)
N = 100
probs=matrix(0,nrow=N+2,ncol=N+2,byrow=TRUE)
probs[N+1,1]=1
probs[N,1]=N^2/(N+1)
for (x in (N-2):0)
for (c in 0:(N-x-1))
  { 
   
      if (x+c+2<=N) probs[x+1,c+1]=probs[x+1,c+1] + probs[x+2,c+1]*(x+1)/N*(N-x-c)/(N-x-c)
      if (c>0) probs[x+1,c+1]=probs[x+1,c+1]+ probs[x+1,c]*(N-x)/N*(N-x-c+2)/(N-x-c+1)
}
probs=probs/sum(probs)
gd=which(probs>10^(-5),arr.ind = TRUE)
xx=gd[,1]
cc=gd[,2]
zz=probs[gd]
plot3d(xx,cc,zz,pch='.',col='blue')
rglwidget()]