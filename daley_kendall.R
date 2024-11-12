# Rumor on Complete Graph 
N = 300 # vertices 
# Initiation
i0= N-1  # Ignoranbts
sp0= 1 # Spreader 
c0= N - (i0+sp0) # Contained
sim = function (i=i0,sp=sp0,stopc=2)
{ c0= N - i0-sp0
 state = c(i0,sp0,c0)
 cvec = matrix (nrow = 3,ncol = 3)
 cvec[,1]= c(-1,1,0)
 cvec[,2]= c(0,-2,2)
 cvec[,3]= c(0,-1,1)
 t=0
  while (state[3]<stopc)
  {  
    i=state[1]
    sp=state[2]
    c=state[3]
    probvec = c(i*sp,sp*(sp-1)/2,sp*c) # prob of {i,sp}, {sp,sp},{sp,c}, {rest}
    if (sp==0) break
    state = c(i,sp,c) + cvec[,sample(c(1,2,3),size=1,prob=probvec)]
    t=t+1  
  }
 return(c(state,t))
}
T = 10^7
data= sapply (1:T,sim)
gr = table(data[1,])
plot(gr,xlim=c(0,N))
points(table(data[2,]),col='red')

I=which(data[4,]>80)
gr = table(data[1,I])
plot(gr,xlim=c(0,N))
points(table(data[2,I]),col='red')


