#FUNCIONES ADICIONALES
panel.hist <- function(x, celdas,...)#MANIPULA LOS HISTOGRAMAS DE CADA COLUMNA
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr=c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot=FALSE,breaks=celdas)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="grey90", ...)
  
  #lines(density(x),col='red')#DIBUJA EL AJUSTE DE UNA DENSIDAD KERNEL
  #xx=seq(min(x),max(x),length.out=1000)
  #lines(xx,dnorm(xx,mean=mean(x),sd=sd(x)),col="blue")#DIBUJA EL AJUSTE DE UNA DENSIDAD NORMAL
  
  
}

panel.cor <- function(x, y, digits=2, prefix="",cex.cor, ...)#MANIPULA LAS CORRELACIONES
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- (cor(x, y, use="complete"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt)
}
#######
