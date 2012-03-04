usuarios = read.csv("askubuntu.csv")

plot.fda <- function(data, col.name, ...)
    plot(ecdf(data[, col.name]),
         xlim = c(0, quantile(data[, col.name], 0.99)),
         main = paste("Função de distribuição acumulada para a variável", col.name),
         las = 1,
         ...)

# Implemente um script R que gera a distribuição de probabilidades empirica dos dados da coluna reputation.

Fn <- ecdf(usuarios$reputation)

# O script também deve extrair dessa distribuição uma amostra contendo a reputação de 25.000 usuários e salvá-la em um
# arquivo no formato *.txt, com cada usuário em uma linha do arquivo.

## Com probabilidade uniforme de escolher algum elemento da amostra principal que ocorre com o uso da função sample e
## seu parâmetro replace = TRUE, mantem-se, na nova amostra, as mesmas características da distribuição dos dados brutos.
nova.amostra = data.frame(name = 0:(25000 - 1),
                          reputation = sample(usuarios$reputation, 25000, replace = TRUE))

write.table(nova.amostra, file = "output-questao3-nova-amostra.txt")

# Para permitir a análise dos dados, o script também deve:
# (i) gerar uma figura em formato *.png contendo um único gráfico FDA com os dados brutos e a amostra gerada;
png(filename = "output-questao3-fda.png", width = 960, height = 960)
plot.fda(usuarios, col.name = "reputation", col = "blue")

lines(ecdf(nova.amostra$reputation), col = "red")

legend("bottomright", c("Dados brutos", "Amostra gerada"), col = c("blue", "red"), lwd = 1, pch = 19)
dev.off()

# (ii) imprimir os valores: min, max, média, mediana, 1o e 3o quartis.
print("Dados brutos")
print(summary(usuarios$reputation))

print("Nova amostra gerada")
print(summary(nova.amostra$reputation))

# Dica: estude com cuidado o help das funções sample, ecdf e hist no R. Essas funções podem tornar esse exercício mais
# fácil.

png(filename = "output-questao3-qqplot.png", width = 960, height = 960)
qqplot(usuarios$reputation, nova.amostra$reputation)
dev.off()
