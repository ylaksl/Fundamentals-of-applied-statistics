iris = read.csv("C:\\Users\\ylana\\Downloads\\iris\\iris.csv")
View(iris)

attach(iris)

summary(iris)

setosa = iris[iris$especies == "setosa",]
versicolor = iris[iris$especies == "versicolor",]
virginica = iris[iris$especies == "virginica",]

summary(setosa)
summary(versicolor)
summary(virginica)

hist(versicolor$sepala_comp, col = "aliceblue", xlim=c(min(sepala_comp), max(sepala_comp)), right = FALSE)
hist(setosa$sepala_comp, main="Histogram of Speed", xlab="Comprimento da sépala (cm)",right = FALSE,add = TRUE)
hist(virginica$sepala_comp, col = "darksalmon", right = FALSE,  add = TRUE)


hist(versicolor$sepala_larg, col = "aliceblue", xlim=c(min(sepala_larg), max(sepala_larg)), right = FALSE)
hist(setosa$sepala_larg, main="Largura da Sépala", xlab="Largura da sépala (cm)",right = FALSE,add = TRUE)
hist(virginica$sepala_larg, col = "darksalmon", right = FALSE,  add = TRUE)

hist(versicolor$petala_comp, col = "aliceblue", xlim=c(min(petala_comp), max(petala_comp)), right = FALSE)
hist(setosa$petala_comp, main="Histogram of Speed", xlab="Comprimento da sépala (cm)",right = FALSE,add = TRUE)
hist(virginica$petala_comp, col = "darksalmon", right = FALSE,  add = TRUE)

hist(versicolor$petala_larg, col = "aliceblue", xlim=c(min(petala_larg), max(petala_larg)), right = FALSE)
hist(setosa$petala_larg, main="Histogram of Speed", xlab="Largura da sépala (cm)",right = FALSE,add = TRUE)
hist(virginica$petala_larg, col = "darksalmon", right = FALSE,  add = TRUE)


boxplot(sepala_comp ~ especies, ylab = "Comprimento da sépala (cm)", xlab = "Espécies")

cdplot(as.factor(especies) ~ sepala_comp, xlab = "Comprimento da sépala (cm)", ylab = "Espécies")


coplot(sepala_comp ~ sepala_larg | especies,  panel = panel.smooth, pch = 20,
       lty = 2,ylab = "Comprimento da sépala (cm)", xlab = "Largura da sépala (cm)", col.smooth = "blue")


coplot(petala_comp ~ petala_larg | especies, panel = panel.smooth, pch = 20,
       lty = 2,ylab = "Comprimento da sépala (cm)", xlab = "Largura da sépala (cm)", col.smooth = "blue")




pairs(~ sepala_comp +  sepala_larg + petala_comp + petala_larg,  col = c("blue", "green", "red")[as.factor(especies)], pch = 20, panel = panel.smooth, lty = 2)
legend("topright", 
       legend = c("Setosa", "versicolor", "virginica"), 
       col = c("blue", "green", "red"))


panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        r <- abs(cor(x, y))
        txt <- format(c(r, 0.123456789), digits = digits)[1]
        txt <- paste0(prefix, txt)
        if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
        text(0.5, 0.5, txt, cex = cex.cor * r)
}

panel.hist <- function(x, ...) {
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(usr[1:2], 0, 1.5) )
        h <- hist(x, plot = FALSE)
        breaks <- h$breaks; nB <- length(breaks)
        y <- h$counts; y <- y/max(y)
        rect(breaks[-nB], 0, breaks[-1], y, col = "grey", ...)
}


pairs(~ sepala_comp +  sepala_larg + petala_comp + petala_larg, pch = 20, lower.panel = panel.cor,
      upper.panel = panel.smooth, diag.panel = panel.hist,
      lty = 2)
