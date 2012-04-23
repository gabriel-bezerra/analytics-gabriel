require("ggplot2")

dados <- read.csv(commandArgs(trailingOnly = TRUE))[, c("largura.petala", "comprimento.petala", "classe")]

grafico <- ggplot(dados, aes(x = largura.petala, y = comprimento.petala, colour = dados$classe)) +
               geom_point() +
               geom_abline(intercept = 2.5, slope = 0) +  # Reta que separa a iris setosa das demais
               geom_abline(intercept = 7.7, slope = -1.75, colour = "red") # Reta que separa a iris virginica e a
                                                                            # versicolor.

png(filename = "output-questao2.png")
print(grafico)
dev.off()

