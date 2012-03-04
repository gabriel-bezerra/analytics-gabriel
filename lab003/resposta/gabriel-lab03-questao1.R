usuarios = read.csv("askubuntu.csv")

# Função de Massa de Probabilidade

plot.fmp <- function(data, col.name, ...)
    plot(prop.table(table(data[, col.name])),
         xlim = c(0, quantile(data[, col.name], 0.99)),
         main = paste("Função de massa de probabilidade para a variável", col.name),
         ylab = "Probabilidade",
         type = "h",
         lwd = 3,
         las = 1,
         ...)

png(filename = "output-questao1-fdp-fdm.png", width = 960, height = 960)
par(mfrow = c(2,2))
plot.fmp(usuarios, "reputation")
plot.fmp(usuarios, "views")
plot.fmp(usuarios, "up_votes")
plot.fmp(usuarios, "down_votes")
dev.off()


# Função de Distribuição Acumulada

plot.fda <- function(data, col.name, ...)
    plot(ecdf(data[, col.name]),
         xlim = c(0, quantile(data[, col.name], 0.99)),
         main = paste("Função de distribuição acumulada para a variável", col.name),
         las = 1,
         ...)


png(filename = "output-questao1-fda.png", width = 960, height = 960)
par(mfrow = c(2,2))
plot.fda(usuarios, "reputation")
plot.fda(usuarios, "views")
plot.fda(usuarios, "up_votes")
plot.fda(usuarios, "down_votes")
dev.off()
