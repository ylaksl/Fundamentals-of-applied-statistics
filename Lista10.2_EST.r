install.packages("dplyr")
library(dplyr)

'''
Alternativas (a), (b) e (c) todas juntas: 
'''

data = read.csv("https://raw.githubusercontent.com/bliiir/hubble/master/galaxies.csv")
View(data)
attach(data)

# represe, v3k nta a velocidade da galáxia relativa a Terra (km/s)
# mpc a respectiva distância da Terra (em Mpc)

# lei de Hublle -> v = H0*d

summary(data)

'''
Inicialmente, vamos estimar a constante de Hubble (H0), usando a equação e as
respectivas colunas. E fazer a correção para segundos, e tirar a média das idades 
obtidas para obter a estimativa.
'''
H0 = 0
H0 = (v3k/dl_mpc) * 3.24076*10**-20
idade = 1/H0
mean(idade)

'''
Aqui fazemos o modelo de regressão linear e o plotamos junto do gráfico de dispersão
'''

reg.ex1 = lm( v3k ~ dl_mpc + 0)

plot( dl_mpc, v3k)
abline(reg.ex1, col = "red", lty = 2, lw = 2)

'''
Pudemos ver quer há dois outliners que tem a dl_mpc > 550 Mpc (e únicos), assim 
aplicamos um filtro para retirar essas duas galáxias e refazemos os cálculos. Em que 
vemos uma variação nos resultados.
'''

data_fil <- data %>% filter(dl_mpc < 550)

reg.ex1 = lm( data_fil$v3k ~ data_fil$dl_mpc)

plot( data_fil$dl_mpc, data_fil$v3k)
abline(reg.ex1, col = "red", lty = 2, lw = 2)


H0 = data_fil$v3k/data_fil$dl_mpc *  3.24076*10**-20
idade = 1 / H0
mean(idade)
