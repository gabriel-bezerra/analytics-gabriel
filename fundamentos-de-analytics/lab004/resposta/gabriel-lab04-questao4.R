library("ggplot2")

source("intervalos-de-confianca.R")


## 4. Gere um gráfico com os intervalos de confiança da proporção de presenças
## dos deputados em sessões por estado. Você consegue identificar com 90% de
## confiança, um estado cujos deputados são, em média, mais assíduos que os
## deputados dos outros estados? Existem intervalos com tamanhos maiores que
## outros? Analise os dados para tentar descobrir porque a precisão de alguns
## intervalos está menor do que a de outros.

dados <- read.csv(file = "dados-deputados.csv")

nivel.de.significancia = 0.10

numero.de.sessoes = 202

# Intervalo de confiança
ic.presenca.por.estado <-
    aggregate(dados$presencas.total,
              list(estado = dados$estado),
              function(x) intervalo.de.confianca.para.a.proporcao(sum(x),
                                                                  length(x) * numero.de.sessoes,
                                                                  nivel.de.significancia))
names(ic.presenca.por.estado) <- c("estado", "ic")

# Proporção
prop.presenca.por.estado <-
    aggregate(dados$presencas.total,
              list(estado = dados$estado),
              function(x) mean(x) / numero.de.sessoes)
names(prop.presenca.por.estado) <- c("estado", "proporcao")

# Quantidade
n.presenca.por.estado <-
    aggregate(dados$presencas.total,
              list(estado = dados$estado),
              length)
names(n.presenca.por.estado) <- c("estado", "n")

# Desvio padrão
sd.presenca.por.estado <-
    aggregate(dados$presencas.total,
              list(estado = dados$estado),
              function(x) sd.proporcao(sum(x),
                                       length(x) * numero.de.sessoes))

names(sd.presenca.por.estado) <- c("estado", "desvio.padrao")

# Merge
presenca.por.estado = merge(prop.presenca.por.estado,
                            ic.presenca.por.estado,
                            by = c("estado"))

presenca.por.estado = merge(presenca.por.estado,
                            n.presenca.por.estado,
                            by = c("estado"))

presenca.por.estado = merge(presenca.por.estado,
                            sd.presenca.por.estado,
                            by = c("estado"))

presenca.por.estado


# Gráficos

# Ordenando os estados pela proporcao de presenca
presenca.por.estado$estado <- reorder(presenca.por.estado$estado,
                                      -presenca.por.estado$proporcao,
                                      order = TRUE)

presenca.plot = ggplot(presenca.por.estado, aes(estado, proporcao, fill = estado)) +
                       geom_bar() +
                       geom_errorbar(aes(ymin = ic[, 1], ymax = ic[, 2], width = 0.2)) +
                       scale_x_discrete(name = "Estado") +
                       scale_y_continuous(name = "Proporção de presença") +
                       opts(legend.position = "none",
                            title = "Proporção de presença dos deputados por estado em 2011")

png(filename = "output-questao4-presenca-por-estado.png", width = 960, height = 480)
print(presenca.plot)
dev.off()

# Ordenando os estados pela quantidade de deputados
presenca.por.estado$estado <- reorder(presenca.por.estado$estado,
                                       -presenca.por.estado$n,
                                           order = TRUE)

png(filename = "output-questao4-presenca-por-estado-n-deputados.png", width = 960, height = 480)
ggplot(presenca.por.estado, aes(estado, 0.001 * n, fill = estado)) + # Multiplicando por 0.001 apenas para melhorar
                                                                     # a visualização do gráfico
    geom_bar() +
    geom_errorbar(aes(ymin = 0, ymax = ic[, 2] - ic[, 1], width = 0.2)) +
    scale_x_discrete(name = "Estado") +
    scale_y_continuous(name = "n / 1000") +
    opts(legend.position = "none",
    title = "Quantidade de deputados e amplitude do intervalo de confiança")
dev.off()

# A influência do desvio padrão
png(filename = "output-questao4-presenca-por-estado-desvio-padrao.png", width = 960, height = 480)
ggplot(presenca.por.estado, aes(estado, desvio.padrao / 10, fill = estado)) + # Multiplicando por 0.1 apenas para
                                                                              # melhorar a visualização do gráfico
    geom_bar() +
    geom_errorbar(aes(ymin = 0, ymax = ic[, 2] - ic[, 1], width = 0.2)) +
    scale_x_discrete(name = "Estado") +
    scale_y_continuous(name = "Desvio padrão / 10") +
    opts(legend.position = "none",
    title = "Desvio padrão e amplitude do intervalo de confiança")
dev.off()

