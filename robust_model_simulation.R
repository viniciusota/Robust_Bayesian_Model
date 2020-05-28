library("MASS")
b0=3
b1=5
e=rnorm(20,0,4)
x=seq(1,20)
y=b0+b1*x + e
#dados sem outlier
dados = as.data.frame(cbind(x,y))

#adicionando outlier
y_outlier = y[-20]
y_outlier[20]=400
dados_outlier = as.data.frame(cbind(x,y_outlier))


#modelo de regressão normal para dados sem outlier:

modelo_linear1=lm(dados$y ~ dados$x)

#modelo de regressão normal para dados com outlier

modelo_linear2=lm(dados_outlier$y_outlier ~ dados_outlier$x)

plot(dados_outlier,ylim=c(0,500),ylab)
points(20,400,pch=17,cex=1.5)
abline(modelo_linear1,col="black")
abline(modelo_linear2,col="red")
legend(1,500,legend=c("Reta com outlier","Reta sem Outlier"),
      lty=1,col = c("red","black"),box.col = "white")
#g1=ggplot(data = cbind(dados,dados_outlier),aes(dados_outlier$x,dados_outlier$y_outlier))+
#         geom_point()+
#          geom_smooth(aes(dados$x,dados$y),method ="lm",se=F,col="black")+ 
#          geom_smooth(data = dados_outlier,method = "lm",se=F,col="red")+theme_bw()+
#          xlab("X")+ylab("Y")
      

#Regressão Robusta Para Dados sem Outlier com M-Estimador
modelo_Mest1=rlm(dados$y ~ dados$x)

#Regressão Robusta Para Dados com Outlier com M-Estimador
modelo_Mest2=rlm(dados_outlier$y_outlier ~ dados_outlier$x)

plot(dados_outlier,xlab="X",ylab="Y",ylim=c(0,500),main="Modelo de Regressão robusta com M-Estimadores")
points(20,400,pch=17,cex=1.5)
abline(modelo_Mest1,col="black")
abline(modelo_Mest2,col="red")
legend(1,500,legend=c("Reta com outlier","Reta sem Outlier"),
       lty=1,col = c("red","black"),box.col = "white")
