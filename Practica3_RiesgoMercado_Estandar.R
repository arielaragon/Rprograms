##EJEMPLOS SOBRE LA IMPLEMENTACION DE MODELOS DE MEDICION DEL
##RIESGO DE MERCADO
library(fBasics)
library(its)
library(xts)
library(fGarch)

##EJEMPLO 1: PORTAFOLIO DE ACCIONES (SIGUIENDO NOTACION DE EMBRECHTS, 2005)
data=read.table("PreciosHistoricos.csv",header=TRUE,sep=",")
nH=500#Numero de escenarios de rendimientos historicos a considerar
pH=data[1:(nH+1),]#Precios historicos
ld=c(30,50)#Vector del número de títulos en cada acción
S=as.numeric(tail(pH,1))#Vector de precios actuales de cada acción
d=length(S)#Dimension del portafolio

Lfull<-function(x,ld,S){#Variable aleatoria de perdidas Lt+1 (Valuacion total)
  N=dim(x)[1]#x debe ser una matriz de dimension Nxd (escenarios de x)
  d=dim(x)[2]#Numero de factores de riesgo
  ld=matrix(ld,N,d,byrow=TRUE)
  S=matrix(S,N,d,byrow=TRUE)
  -rowSums(ld*S*(exp(x)-1))
}

Ldelta<-function(x,ld,S){#Aproximacion lineal de Lt+1
  V=sum(ld*S)#Valor portafolio
  w=ld*S/V#Vector de pesos del portafolio
  -V*sum(w*x)
}

q=.99#Nivel de confianza
V=sum(ld*S)#Valor actual del portafolio
rH=apply(log(pH),2,diff)#Rendimientos históricos de los factores de riesgo
mu=colMeans(rH)
Sigma=cov(rH)


#Método Paramétrico: x~Nd(mu,Sigma) y uso de aproximación lineal de Lt+1
c=0
b=matrix(ld*S,d,1)
VaR_P=-c-t(b)%*%mu+qnorm(q)*sqrt(t(b)%*%Sigma%*%b)#VaR portafolio
VaR_P/V

#Método MonteCarlo: x~Nd(mu,Sigma) y valuación de las pérdidas bajo escenarios simulados
library(mvtnorm)
N=100000#Numero de escenarios a simular
x=rmvnorm(N,mu,Sigma)#Escenarios simulados del vector x
L=Lfull(x,ld,S)#Escenarios de pérdidas bajo valuacion total
hist(L)
VaR_MC=quantile(L,q)#Cuantil empírico de L
VaR_MC/V

#Método Histórico: Valuar las pérdidas en escenarios observados de x
x=apply(log(pH),2,diff)#Escenarios históricos de los factores de riesgo
L=Lfull(x,ld,S)#Escenarios de pérdidas bajo valuacion total
hist(L)
VaR_H=quantile(L,q)#Cuantil empírico de L
VaR_H/V


##EJEMPLO 2: PORTAFOLIO DE ACCIONES TOMANDO EFECTOS DE PRECIOS Y TIPOS DE CAMBIO
#CARGA DE INFORMACION DE MERCADO
#FORMA SIMPLE:
data=read.table("EquityFX.csv",header=TRUE,sep=",")

#FORMA PARA SERIE DE TIEMPO xts
data=as.xts(readcsvIts("EquityFX.csv"))#ESPECIFICAR FORMATO DE FECHAS SI ES NECESARIO: informat="%d/%m/%Y",...

names(data)#ENCABEZADOS DE LAS VARIABLES EN LA BASE

factores=c("JAPDOWA","MXIPC35","S.PCOMP","JAPAYE.","MEXPES.")
x=apply(log(data[,factores]),2,diff)#RENDIMIENTOS LOGARITMICOS DIARIOS
#RENDIMIENTOS MENSUALES
xmensual=apply(log(data[endpoints(data,on="months"),factores]),2,diff)#DATOS SOBRE GRID MENSUAL

#DEFINICION DEL PORTAFOLIO
Inv=1000000#MONTO INVERTIDO EN USD
w=c(1/3,1/3,1/3)#COMPOSICION DEL PORTAFOLIO EN CADA INDICE
valActFactores=as.numeric(tail(data[,factores],1))#VALORES DE MERCADO ACTUALES
d=length(valActFactores)#NUMERO DE FACTORES DE RIESGO
PosOrigen=Inv*w*c(valActFactores[4:5],1)#POSICION EN CADA ACCION EN SU MONEDA DE ORIGEN
nTit=PosOrigen/valActFactores[1:3]#NUMERO DE TITULOS EN CADA ACCION

q=.99#NIVEL DE CONFIANZA DE LAS MEDIDAS DE RIESGO

##VAR HISTORICO: SUPONE QUE LOS RENDIMIENTOS OBSERVADOS HISTORICAMENTE
##PUEDEN OCURRIR EN EL FUTURO CON LA MISMA FRECUENCIA E INTENSIDAD
nEsH=500#NUMERO ESCENARIOS HISTORICOS
xH=tail(x,nEsH)#TOMAMOS LOS ULTIMOS nEsH RENDIMIENTOS OBERVADOS
valActM=matrix(rep(valActFactores,nEsH),nrow=nEsH,byrow=TRUE)#MATRIZ CON VALORES ACTUALES DE LOS FACTORES DE RIESGO
EsHFactores=valActM*exp(xH)#NIVELES POSIBLES SIMULADOS A PARTIR DE LOS RENDIMIENTOS HISTORICOS
nTitM=matrix(rep(nTit,nEsH),nrow=nEsH,byrow=TRUE)#MATRIZ DE NUMERO DE TITULOS PARA LOS ESCENARIOS
EsHPrecios=EsHFactores[,1:3]#ESCENARIOS PRECIOS SIMULADOS
EsHTipoCambio=cbind(EsHFactores[,4:5],rep(1,nEsH))#ESCENARIOS TIPO DE CAMBIO
valPortH=rowSums(nTitM*EsHPrecios/EsHTipoCambio)#VALUACION DEL PORTAFOLIO EN CADA ESCENARIO SIMULADO
valPortH=as.xts(valPortH)#COMO SERIE DE TIEMPO xts
hist(valPortH)
plot(valPortH)
PnL=Inv-valPortH#PERDIDAS (GANANCIAS) DEL PORTAFOLIO i.e. L_T+1
hist(PnL)
VaRH=quantile(PnL,q)
VaRH/Inv#COMO PORCENTAJE DEL VALOR DEL PORTAFOLIO
ESH=mean(PnL[PnL>VaRH])#TAMAÑO PROMEDIO DE LAS PERDIDAS QUE EXCEDEN A VaR
ESH/Inv

##VAR PARAMETRICO
vSen=matrix(c(w,-w[-length(w)]),d,1)#VECTOR DE SENSIBILIDADES
mu=matrix(colMeans(xH),d,1)
varcov=cov(xH)
VaRP=-t(vSen)%*%mu+qnorm(q)*sqrt(t(vSen)%*%varcov%*%vSen)
VaRP

##VAR MONTECARLO: REVALUAR EL PORTAFOLIO BAJO ESCENARIOS xMC SIMULADOS DE UNA DISTRIBUCION
##NORMAL d-DIMENSIONAL CON MEDIA mu Y MATRIZ DE VARIANZA COVARIANZA varcov.
##USAR n=10000 ESCENARIOS
library(mvtnorm)
N=100000#Numero de escenarios a simular
set.seed(10)
xMC=rmvnorm(N,mu,varcov)#Escenarios simulados del vector de cambios en los factores de riesgos x
valActM=matrix(rep(valActFactores,N),nrow=N,byrow=TRUE)#MATRIZ CON VALORES ACTUALES DE LOS FACTORES DE RIESGO
EsMCFactores=valActM*exp(xMC)#NIVELES POSIBLES SIMULADOS A PARTIR DE LOS RENDIMIENTOS HISTORICOS
nTitM=matrix(rep(nTit,N),nrow=N,byrow=TRUE)#MATRIZ DE NUMERO DE TITULOS PARA LOS ESCENARIOS
EsMCPrecios=EsMCFactores[,1:3]#ESCENARIOS PRECIOS SIMULADOS
EsMCTipoCambio=cbind(EsMCFactores[,4:5],rep(1,N))#ESCENARIOS TIPO DE CAMBIO
valPortMC=rowSums(nTitM*EsMCPrecios/EsMCTipoCambio)#VALUACION DEL PORTAFOLIO EN CADA ESCENARIO SIMULADO
PnLMC=Inv-valPortMC#PERDIDAS (GANANCIAS) DEL PORTAFOLIO i.e. L_T+1
hist(PnLMC)
VaRMC=quantile(PnLMC,q)
VaRMC/Inv#COMO PORCENTAJE DEL VALOR DEL PORTAFOLIO
ESMC=mean(PnLMC[PnLMC>VaRMC])#TAMAÑO PROMEDIO DE LAS PERDIDAS QUE EXCEDEN A VaR
ESMC/Inv
