##EJEMPLOS SOBRE EL CALCULO DE MEDIDAS DE RIESGO Y LA PROPIEDAD DE SUBADITIVIDAD

set.seed(123)
#EJEMPLO: L1, L2 v.a. PARETO INDEPENDIENTES CON f.d. F(x)=1-x^(-1/2),X>1.
n=10000#Número de escenarios
q=.99#Nivel de confianza
L1=(1-runif(n))^-2#Escenarios simulados de la v.a. L1
L2=(1-runif(n))^-2
L=L1+L2#Escenarios de pérdidas del portafolio
VaR1=quantile(L1,q)#Cuantil empírico de nivel q (Estimación empírica de VaR)
VaR2=quantile(L2,q)
VaRTot=quantile(L,q)#VaR del portafolio L1+L2 (riesgo fusionado)
VaRTot<=VaR1+VaR2#NO SE CUMPLE LA SUBADITIVIDAD
ES1=mean(L1[L1>VaR1])#Estimador empírico de CTE, el cual coincide con TVaR y ES (Embrechts)
ES2=mean(L2[L2>VaR2])
ESTot=mean(L[L>VaRTot])
ESTot<=ES1+ES2#ES siempre es subaditivo
ES1+ES2-ESTot#Efecto de diversificacion para ES
VaR1+VaR2-VaRTot

#EJEMPLO: UN PORTAFOLIO DE 100 BONOS CORPORATIVOS CON RIESGO DE INCUMPLIMIENTO,
#CADA UNO CON PRECIO ACTUAL DE $100 Y QUE AL VENCIMIENTO OTORGAN $105 AL TENEDOR
#CON PROBABILIDAD 0.98 Y $O EN OTRO CASO. LOS INCUMPLIMIENTOS SON INDEPENDIENTES.
n=10000
q=.95
L=matrix(0,n,100)
for(i in 1:100){
  L[,i]=105*rbinom(n,size=1,prob=.02)-5#Escenarios de pérdidas del bono i
}
LTot=rowSums(L)#PERDIDAS SIMULADAS DEL PORTAFOLIO TOTAL
VaRTot=quantile(LTot,q)#VaR DEL PORTAFOLIO L1+L2+...+L100
VaRSum=sum(apply(L,2,quantile,q))#SUMA DE VaRs INDIVIDUALES VaR1+...+VaR100
VaRTot<=VaRSum
ESTot=mean(LTot[LTot>VaRTot])
ES=rep(0,100)
for(i in 1:100){
  aux=L[,i]
  ES[i]=mean(aux[aux>quantile(aux,q)])
}
ESSum=sum(ES)
ESTot<=ESSum#ES siempre es subaditivo

