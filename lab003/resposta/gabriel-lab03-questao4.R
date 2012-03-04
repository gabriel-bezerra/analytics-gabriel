args <- commandArgs(trailingOnly = TRUE)

# Coleta os argumentos
media <- as.numeric(args[1])
desvio.padrao <- as.numeric(args[2])

tamanho.da.amostra = 5000

# Cria as amostra normal e a log-normal
normal <- rnorm(n = tamanho.da.amostra, mean = media, sd = desvio.padrao)
log.normal <- exp(normal)

# Plota as FDPs
png(filename = "output-questao4-graficos-juntos.png", width = 960, height = 960)
plot(density(normal), col = "red", lwd = 2, las = 1,
     xlim = c(media - 4*desvio.padrao, media + 3*4*desvio.padrao))
lines(density(log.normal), col = "blue", lwd = 2, las = 1)
legend("topright", c("Normal", "Lognormal"), col = c("red", "blue"), lwd = 2)
dev.off()

png(filename = "output-questao4-graficos-separados.png", width = 960, height = 480)
par(mfrow = c(2,1))
plot(density(normal), col = "red", las = 1,
     main = paste("Normal com média", media, "e desvio padrão", desvio.padrao))
plot(density(log.normal), col = "blue", las = 1,
     main = paste("Lognormal gerada a partir da distribuição acima"))
dev.off()
