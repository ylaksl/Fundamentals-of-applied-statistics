# Ylana Karolina Santos Lopes
library(MASS)
data = data(survey)

attach(survey)

## Letra a)
'''
Aqui verificamos a contagem das variáveis qualitativas, e calculamos as estatísticas 
dos valores quantitativos
'''

summary(Sex)
summary(Wr.Hnd)
summary(NW.Hnd)
summary(W.Hnd)
summary(Pulse)
summary(Exer)
summary(Smoke)
summary(Height)
summary(Age)


'''
Inicialmente, vamos pegar as variáveis numéricas e olhar a distribuição de contagem dos dados.
'''


hist(Wr.Hnd, col = "aliceblue", right = FALSE)
hist(NW.Hnd, col = "aliceblue", right = FALSE)
hist(Pulse, col = "aliceblue", right = FALSE)
hist(Height, col = "aliceblue", right = FALSE)
hist(Age, col = "aliceblue", right = FALSE)

'''
Agora, vamos usar o boxplot para ver os dados associados às suas variáveis qualitativas.
Inicialmente, fiz o gráfico comparando o pulso de quem fuma muto, nunca, ocasionalmente,
ou regularmente. A partir dele, podemos ver que o a distribuição dos dados não muda muito,
mas a mediana varia consideravelmente, em que podemos observar um pulso elevado em quem é
Heavy Smoker e um pulso mais baixo em quem fuma regularmente (mas distribuição com limite
superior mais alto).E na questão da idade, que gente na casa dos 20 fuma mais.
'''
boxplot(Age ~ Smoke )

boxplot(Pulse ~ Smoke)


'''
Fazemos o mesmo comparando quem se exercita em frequências variadas, e quem não.Em que é 
possível ver que o exercício frequente realmente faz uma grande diferença. 
'''
boxplot(Pulse ~ Exer)


'''
Como fiz um boxplot comparando o pulso x exercício e pulso x fumante, acho justo ver a 
relação entre a coluna Smoke e a Exer. É possível ver que dos fumantes pesados e ocasionais
praticam mais exercícios frequentemente.
'''
spineplot(as.factor(Exer) ~ as.factor(Smoke), xlab = "Smoke",
          ylab = "Exercise")


'''
E aqui vemos a relação dos sexos com a frequência que se exercita, e com quem fuma ou não.
Aqui podemos ver que homens fumam mais, mas mulheres tendem a praticar mais exercício. 
Juntando com o cdplot, que mostra como os dados acumulam através do banco de dados.
'''
spineplot(as.factor(Sex) ~ as.factor(Smoke), xlab = "Smoke",
          ylab = "Sexo")
spineplot(as.factor(Sex) ~ as.factor(Exer), xlab = "Exercise",
          ylab = "Sexo")

cdplot(as.factor(Sex) ~ Pulse, ylab = "Types")


'''
Aqui fazemos o mesmo processo anterior, mas relacionando variáveis anatomica com o sexo.
No geral, vemos que homens são mais altos, mulheres tem o pulso ligeiramente mais alto,
e há uma relação linear entreo comprimento de ambas as mãos (que também é proporcional a
altura de cada um). Como também que o comprimento da mão não depende da idade, já que 
a idade mínima dos dados é 16 anos.  
'''
boxplot(Height ~ Sex)
boxplot(Pulse ~ Sex)

min(Age)

coplot(Wr.Hnd ~ NW.Hnd| Height,col = c("blue", "green")[as.factor(Sex)], panel = panel.smooth, pch = 20,
       lty = 2,)

coplot(Wr.Hnd ~ Height| Age,col = c("blue", "green")[as.factor(Sex)], panel = panel.smooth, pch = 20,
       lty = 2,)



'''
Por fim, fiz um painel em que podemos visualizar os gráficos de dispersão entre todas as 
variáveis numérica (separadas por cor indicando o sexo). Aqui podemosver que os valores 
são agrupados por sexo, indicando a relação entre NW.HNd, Wr.Hnd e Height.
'''

pairs(~ Wr.Hnd +  NW.Hnd + Pulse + Height, pch = 20,col = c("blue", "green")[as.factor(Sex)],  panel = panel.smooth, lty = 2)



## Letra b)
'''
Aqui fazemos o ajuste linear usando a função lm. Fazendo isso, encontramos o ajuste linear 
do gráfico de dispersão, e seus coeficientes em que o coeficiente angular é beta = 0.99277. 
A hipótese dada no enunciado, propõe que o comprimento da mão deve ser igual, ou seja, 
beta = 1. Quando plotamos a função com beta = 1, vemos o que já era esperado pela proximi-
dade dos valores. Há sim uma relação linear entre os comprimentos das mãos, em que podemos
dizer que são do mesmo tamanho.
'''
plot(Wr.Hnd ~ NW.Hnd)

reg1.lin = lm(NW.Hnd ~ Wr.Hnd); reg1.lin
abline(reg1.lin, lw = 2, col = "red")


beta = reg1.lin$coef[2]; beta
alpha = 1

plot(function(x) alpha*x, lw = 2, xlim = c(10, 30), col = "blue", add = TRUE)





