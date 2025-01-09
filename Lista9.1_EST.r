install.packages("readxl") 
library(readxl)
light = read_excel("C:\\Users\\ylana\\Downloads\\light\\light\\speed-light.xlsx")
View(light)

attach(light)

### primeira parte

fisico
table(fisico)

speed
table(speed)


eu = light[light$fisico == "Eu",]


michelson = light[light$fisico == "Michelson",]


hist(michelson$speed, main="Histogram of Speed", xlab="Speed",right = FALSE)


hist(eu$speed, col = "aliceblue", right = FALSE,  add = TRUE)

boxplot(speed ~ fisico, ylab = "Speed (km/s)")

### segunda parte

quantile(eu$speed, probs = c(0.25, 0.50, 0.75))
quantile(michelson$speed, probs = c(0.25, 0.50, 0.75))

names <- c("eu", "michelson")
mean <- c(mean(eu$speed), mean(michelson$speed))
median <- c(median(eu$speed), median(michelson$speed))
vint_cinc <- c(quantile(eu$speed, probs = c(0.25)), quantile(michelson$speed, probs = c(0.25)))
cinq <- c(quantile(eu$speed, probs = c(0.5)), quantile(michelson$speed, probs = c(0.5)))
sete <- c(quantile(eu$speed, probs = c(0.75)), quantile(michelson$speed, probs = c(0.75)))
                   

table <- data.frame(Name = names, Mean = mean, Median = median, vinte_e_cinco = vint_cinc, cinquenta = cinq, setenta = sete)
table

summary(light)