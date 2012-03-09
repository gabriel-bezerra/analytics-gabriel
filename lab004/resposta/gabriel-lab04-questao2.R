library("ggplot2")

source("intervalos-de-confianca.R")


## 1. Implemente um script R que plota dois gráficos:
## Para gerar os gráficos acima, considere nível de significância de 5%.

dados <- read.csv(file = "dados-deputados.csv")

nivel.de.significancia = 0.25


## Um gráfico png com os intervalos de confiança das médias dos gastos
## anuais dos deputados de cada uma das cinco regiões do Brasil. Observe
## o gráfico gerado e compare os gastos dos deputados de cada região. É
## possível concluir algo interessante com base nos intervalos de confiança?
## Existem intervalos maiores que outros? A que você atribui isso?

# Intervalos de confiança
ic.gastos.totais.por.regiao <- aggregate(dados$gastos.total,
                                         list(regiao = dados$regiao),
                                         function(x) intervalo.de.confianca.para.a.media(x,
                                                                                         nivel.de.significancia))
names(ic.gastos.totais.por.regiao) <- c("regiao", "ic")

# Médias
media.gastos.totais.por.regiao <-
    aggregate(dados$gastos.total,
              list(regiao = dados$regiao),
              mean)
names(media.gastos.totais.por.regiao) <- c("regiao", "media")

# Quantidade
n.gastos.totais.por.regiao <-
    aggregate(dados$gastos.total,
              list(regiao = dados$regiao),
              length)
names(n.gastos.totais.por.regiao) <- c("regiao", "n")

# Desvio padrão
sd.gastos.totais.por.regiao <-
    aggregate(dados$gastos.total,
              list(regiao = dados$regiao),
              sd)
names(sd.gastos.totais.por.regiao) <- c("regiao", "desvio.padrao")

# Merge
gastos.totais.por.regiao <- merge(media.gastos.totais.por.regiao,
                                  ic.gastos.totais.por.regiao,
                                  by = c("regiao"))

gastos.totais.por.regiao <- merge(gastos.totais.por.regiao,
                                  n.gastos.totais.por.regiao,
                                  by = c("regiao"))

gastos.totais.por.regiao <- merge(gastos.totais.por.regiao,
                                  sd.gastos.totais.por.regiao,
                                  by = c("regiao"))

gastos.totais.por.regiao


# Gráfico
gastos.plot = ggplot(gastos.totais.por.regiao, aes(regiao, media, fill = regiao)) +
              geom_bar() +
              geom_errorbar(aes(ymin = ic[, 1], ymax = ic[, 2], width = 0.2)) +
              scale_x_discrete(name = "Região") +
              scale_y_continuous(name = "Média de gastos") +
              opts(legend.position = "none",
                   title = "Média de gastos dos deputados por região em 2011")

# Produz a figura
png(filename = "output-questao2-gastos-por-regiao.png")
print(gastos.plot)
dev.off()


## Um gráfico png com os intervalos de confiança das proporções de
## presenças dos deputados em sessões durante o ano, sabendo que o
## número total de sessões que ocorreram no ano foi 202 e considerando o
## número médio de presenças contabilizadas por região. Observe o gráfico
## gerado compare as assiduidades dos deputados de cada região. É
## possível identificar algo interessante com base nos intervalos de
## confiança? É possível identificar alguma relação entre gastos e
## assiduidade? Comente.

numero.de.sessoes = 202

# Proporção
prop.presenca.por.regiao <-
    aggregate(dados$presencas.total,
              list(regiao = dados$regiao),
              function(x) mean(x) / numero.de.sessoes)
names(prop.presenca.por.regiao) <- c("regiao", "prop.total")

# Intervalo de confiança
ic.presenca.por.regiao <-
    aggregate(dados$presencas.total,
              list(regiao = dados$regiao),
              function(x) intervalo.de.confianca.para.a.proporcao(sum(x),
                                                                  length(x) * numero.de.sessoes,
                                                                  nivel.de.significancia))
names(ic.presenca.por.regiao) <- c("regiao", "ic")

# Quantidade
n.presenca.por.regiao <-
    aggregate(dados$presencas.total,
              list(regiao = dados$regiao),
              length)
names(n.presenca.por.regiao) <- c("regiao", "n")

# Merge
presenca.por.regiao = merge(prop.presenca.por.regiao,
                            ic.presenca.por.regiao,
                            by = c("regiao"))

presenca.por.regiao = merge(presenca.por.regiao,
                            n.presenca.por.regiao,
                            by = c("regiao"))

presenca.por.regiao

# Gráfico
presenca.plot = ggplot(presenca.por.regiao, aes(regiao, prop.total, fill = regiao)) +
                       geom_bar() +
                       geom_errorbar(aes(ymin = ic[, 1], ymax = ic[, 2], width = 0.2)) +
                       scale_x_discrete(name = "Região") +
                       scale_y_continuous(name = "Proporção de presença") +
                       opts(legend.position = "none",
                            title = "Proporção de presença dos deputados por região em 2011")

png(filename = "output-questao2-presenca-por-regiao.png")
print(presenca.plot)
dev.off()
