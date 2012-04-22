usuarios = read.csv("askubuntu.csv")

# Função de Distribuição Acumulada

plot.fda <- function(data, col.name, ...)
    plot(ecdf(data[, col.name]),
         main = paste("Função de distribuição acumulada para a variável", col.name),
         las = 1,
         ...)

quartis = quantile(usuarios$views, c(0.25, 0.5, 0.75, 1))

usuarios$classe <- NA

# Comentados pela falta de existência de usuarios$views que casam com o critério. Descomentadas, estas linhas causam
# erro.
usuarios[                              usuarios$views <= quartis[1], ]$classe <- "Muito baixo"
#usuarios[usuarios$views > quartis[1] & usuarios$views <= quartis[2], ]$classe <- "Baixo"
#usuarios[usuarios$views > quartis[2] & usuarios$views <= quartis[3], ]$classe <- "Alto"
usuarios[usuarios$views > quartis[3] & usuarios$views <= quartis[4], ]$classe <- "Muito alto"

usuarios$classe <- factor(usuarios$classe,
                          levels = c("Muito baixo", "Baixo", "Alto", "Muito alto"),
                          ordered = TRUE)

png(filename = "output-questao2.png", width = 480, height = 960)
par(mfrow = c(2, 1))
    plot.fda(usuarios, "classe")
    plot(usuarios$classe)
dev.off()

