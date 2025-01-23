black_hole = read.csv("C:\\Users\\ylana\\Documents\\EST\\blackhole\\data")
View(black_hole)
attach(black_hole)

summary(black_hole)


'''
Separamos o dataset original em cada tipo de galáxia, para fazer aplicar o
summary e ver as medidas-resumo.
'''

e = black_hole[black_hole$type == "E",]
s = black_hole[black_hole$type == "S",]
s0 = black_hole[black_hole$type == "S0",]

summary(e)
summary(s)
summary(s0)

'''
Agora vamos analisar as 4 colunas usando histogramas para observar a frequência
de cada valor (sem separar por tipo de galaxia)
'''
hist(logv, col = "aliceblue", xlim=c(min(logv) - 1, max(logv) + 1), right = FALSE)

hist(logm, col = "aliceblue", xlim=c(min(logm) - 1, max(logm) +1), right = FALSE)

## Histograma dos erros associados a medida 

hist(elogv, col = "aliceblue", xlim=c(min(elogv) , max(elogv)), right = FALSE)
hist(elogm, col = "aliceblue", xlim=c(min(elogm), max(elogm)), right = FALSE)


'''
Para ver algumas das características dispostas na função summary, separada pelos 
tipos de galaxia, usamos o boxplot para as duas variáveis 
'''

boxplot(logv ~ type, ylab = "logv", xlab = "Types")
boxplot(logm ~ type, ylab = "logm", xlab = "Types")

'''
Para vermos como os dados acumulam apesar das separações de tipo
'''

cdplot(as.factor(type) ~ logv, ylab = "Types")
cdplot(as.factor(type) ~ logm, ylab = "Types")

## Fazendo o mesmo para os erros, vemos que o tipo S0 tem medidas com menor
## erro quando comparada as outras

cdplot(as.factor(type) ~ elogv, ylab = "Types")
cdplot(as.factor(type) ~ elogm, ylab = "Types")


'''
Agora vamos analisar a relação do logv com o logm, para ver se vemos algum tipo
de correlação. Primeiro fazemos o gráfico de dispersão separando-os em tipo de 
galáxia. Em seguida juntamos os dados em um gráfico só, em que podemos observar
uma linearidade dos dados que independe do tipo de galáxia. Isso contendo as 
barras de erro equivalentes a cada medida.
'''

coplot(logm ~ logv | type,  panel = panel.smooth, pch = 10,
       lty = 2, col.smooth = "blue")

reg.ex1 = lm(logv ~ logm)

plot(logm, logv, pch = 16) #,col = c("blue", "green", "red")[as.factor(type)])
abline(reg.ex1, col = "red", lty = 1, lwd = 2)


arrows(logm - elogm, logv, logm + elogm, logv, angle = 90, code = 3, length = 0.1)#, col = c("blue", "green", "red")[as.factor(type)])
arrows(logm, logv - elogv, logm, logv + elogv, angle = 90, code = 3, length = 0.1)#, col = c("blue", "green", "red")[as.factor(type)])




'''
Inicio da lista 10.3
'''

coplot(logm ~ logv | type,  panel = panel.smooth, pch = 10,
       lty = 2, col.smooth = "blue")

'''
Fazemos a regressão linear, somente com a função do R 
'''
reg3.pot = lm(logv ~ logm); reg.ex1


plot(logm, logv, pch = 16) #,col = c("blue", "green", "red")[as.factor(type)])
abline(reg.ex1, col = "red", lty = 1, lwd = 2)


#arrows(logm - elogm, logv, logm + elogm, logv, angle = 90, code = 3, length = 0.1)#, col = c("blue", "green", "red")[as.factor(type)])
#arrows(logm, logv - elogv, logm, logv + elogv, angle = 90, code = 3, length = 0.1)#, col = c("blue", "green", "red")[as.factor(type)])
'''

'''
alpha = exp(reg3.pot$coef[1]); alpha
beta_1 = log(alpha)
beta = reg3.pot$coef[2]; beta
plot(function(x) beta_1 + beta*x, xlim = c(0, 15), col = "blue", lty = 2, lw = 2, add = TRUE)
