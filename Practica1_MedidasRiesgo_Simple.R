##EJEMPLOS DE CALCULO DE MEDIDAS DE RIESGO BAJO ESPECIFICACIONES SIMPLES
##DE LA VARIABLE ALEATORIA DE PERDIDAS L

q=.90#Nivel de confianza

#1. L~N(mu,sigma)=mu+sigma*Z
mu=1#La pérdida media
sigma=.5#La volatilidad de las pérdidas
(VaR_N=mu+sigma*qnorm(q))#Valor en Riesgo
(ES_N=mu+sigma*dnorm(qnorm(q))/(1-q))#Deficit Esperado

mRiesgoN<-function(q,mu,sigma){
  VaR_N=mu+sigma*qnorm(q)
  ES_N=mu+sigma*dnorm(qnorm(q))/(1-q)
  #return(list(VaR=VaR_N,ES=ES_N))
  return(c(VaR_N,ES_N))
}

mRiesgoN(.95,1,.5)#Ejecutamos la funcion con los parametros especificos

x=seq(.9,.9999,length=10000)
metricasN=NULL
for(i in 1:length(x)){
  metricasN=rbind(metricasN,mRiesgoN(x[i],mu,sigma))
}
metricas
plot(x,metricasN[,1],main="VaR vs ES: caso Normal",col="red",type="l",lty=1)
lines(x,metricasN[,2],col="blue",type="l",lty=2)
legend("topleft",c("VaR","ES"),col=c("red","blue"),lty=c(1,2))

plot(x,metricasN[,2]/metricasN[,1],main="ES/VaR: caso Normal",col="red",type="l",lty=1)

##TAREA: Calcular VaR y ES para el caso en que L tiene distribución t de Student
##      Con df=5 grados de libertad.
#2. L~mu+sigma*t(df)
mu=1#La pérdida media
sigma=.5#La volatilidad de las pérdidas
df=5#Grados de libertad
(VaR_T=mu+sigma*qt(q,df))#Valor en Riesgo
(ES_T=mu+sigma*dt(qt(q,df),df)*(df+qt(q,df)^2)/((1-q)*(df-1)))#Deficit Esperado

mRiesgoT<-function(q,mu,sigma,df){
  VaR_T=mu+sigma*qt(q,df)
  ES_T=mu+sigma*dt(qt(q,df),df)*(df+qt(q,df)^2)/((1-q)*(df-1))
  #return(list(VaR=VaR_N,ES=ES_N))
  return(c(VaR_T,ES_T))
}

mRiesgoT(.95,1,.5,df)#Ejecutamos la funcion con los parametros especificos

x=seq(.9,.9999,length=10000)
metricasT=NULL
for(i in 1:length(x)){
  metricasT=rbind(metricasT,mRiesgoT(x[i],mu,sigma,df))
}
metricasT
plot(x,metricasT[,1],main="VaR vs ES: caso T de Student",col="red",type="l",lty=1)
lines(x,metricasT[,2],col="blue",type="l",lty=2)
legend("topleft",c("VaR","ES"),col=c("red","blue"),lty=c(1,2))

plot(x,metricasT[,2]/metricasT[,1],main="ES/VaR: caso Normal",col="red",type="l",lty=1)

#COMPARACION DE METRICAS BAJO MODELO NORMAL VS T
ymin=min(c(metricasN[,1],metricasT[,1]))
ymax=max(c(metricasN[,1],metricasT[,1]))
plot(x,metricasN[,1],ylim=c(ymin,ymax),main="VaR Normal vs VaR T",col="red",type="l",lty=1)
lines(x,metricasT[,1],col="blue",type="l",lty=2)
legend("topleft",c("VaR Normal","VaR T"),col=c("red","blue"),lty=c(1,2))

ymin=min(c(metricasN[,2],metricasT[,2]))
ymax=max(c(metricasN[,2],metricasT[,2]))
plot(x,metricasN[,2],ylim=c(ymin,ymax),main="ES Normal vs VaR T",col="red",type="l",lty=1)
lines(x,metricasT[,2],col="blue",type="l",lty=2)
legend("topleft",c("ES Normal","ES T"),col=c("red","blue"),lty=c(1,2))

