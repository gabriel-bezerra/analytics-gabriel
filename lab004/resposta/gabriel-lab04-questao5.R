library("ggplot2")

source("intervalos-de-confianca.R")


## 5. Encontre o estado A do Brasil onde os deputados federais são, em média,
## mais assíduos. Encontre também o estado B do Brasil onde os deputados
## federais são, em média, menos assíduos. É possível afirmar com 95% de
## confiança que o deputado menos assíduo de A falta menos sessões que o
## deputado mais assíduo de B?

dados <- read.csv(file = "dados-deputados.csv")

nivel.de.significancia = 0.05

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

png(filename = "output-questao5-presenca-por-estado.png", width = 960, height = 480)
print(presenca.plot)
dev.off()


# TODO
# Análise dos deputados extremos do DF (mínimo) e do AP (máximo)
