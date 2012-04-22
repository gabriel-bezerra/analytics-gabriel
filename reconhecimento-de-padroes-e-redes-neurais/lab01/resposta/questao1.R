require("lattice")

dados <- read.csv("iris.data.csv")

grafico <- splom(dados, group = dados$classe)

png(filename = "output-questao1.png", width = 1.5*480, height = 1.5*480)
print(grafico)
dev.off()
