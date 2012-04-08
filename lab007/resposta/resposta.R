require("ggplot2")

source("votacao-muzona.R")

governador.senador.coligacao.para <- function(votos.governadores.muzona,
                                              votos.senadores.muzona) {
    # Governador
    votos.governadores.por.partido <-
        aggregate(votos.governadores.muzona$total.votos,
                  list(nome.coligacao = votos.governadores.muzona$nome.coligacao),
                  sum)

    summary(votos.governadores.por.partido)
    ggplot(votos.governadores.por.partido, aes(x = nome.coligacao, y = x)) + geom_bar()


    # Senador
    votos.senadores.por.partido <-
        aggregate(votos.senadores.muzona$total.votos,
                  list(nome.coligacao = votos.senadores.muzona$nome.coligacao),
                  sum)

    summary(votos.senadores.por.partido)
    ggplot(votos.senadores.por.partido, aes(x = nome.coligacao, y = x)) + geom_bar()


    numero.de.votos.governador.senador.coligacao <- merge(votos.governadores.por.partido,
                                                          votos.senadores.por.partido,
                                                          by = "nome.coligacao",
                                                          all = FALSE)

    # Troca o nome #NULO# por SEM COLIGAÇÃO, para melhor visualização
    levels(numero.de.votos.governador.senador.coligacao$nome.coligacao)[levels(numero.de.votos.governador.senador.coligacao$nome.coligacao) == "#NULO#"] <- "SEM COLIGAÇÃO"

    names(numero.de.votos.governador.senador.coligacao) <-
        c("nome.coligacao", "votos.para.governador", "votos.para.senador")

    return(numero.de.votos.governador.senador.coligacao)
}

# Leitura de dados por estado
governador.senador.coligacao.para.estado <- function(estado) {
    votacao <- votacao.muzona.para(estado)

    governador.senador.coligacao <- governador.senador.coligacao.para(votacao$governadores, votacao$senadores)

    governador.senador.coligacao$estado <- estado

    return(governador.senador.coligacao)
}

# Plota gráficos
grafico.de.dispersao.para <- function(dados) {
    ggplot(dados,
           aes(x = votos.para.governador,
               y = votos.para.senador,
               label = nome.coligacao)) +
        xlab("Votos para governador") +
        ylab("Votos para senador") +
        opts(title = "Votos para governador e senador por coligação")
}

grafico.de.dispersao.estadual.para <- function(dados) {
    grafico.de.dispersao.para(dados) +
        geom_point() +
        geom_text(aes(size = 1)) +
        opts(legend.position = "none")
}

#grafico.de.dispersao.estadual.para(governador.senador.coligacao.para.estado("AC"))
#grafico.de.dispersao.estadual.para(governador.senador.coligacao.para.estado("AL"))
#grafico.de.dispersao.estadual.para(governador.senador.coligacao.para.estado("AM"))
#grafico.de.dispersao.estadual.para(governador.senador.coligacao.para.estado("AP"))
#grafico.de.dispersao.estadual.para(governador.senador.coligacao.para.estado("BA"))
#grafico.de.dispersao.estadual.para(governador.senador.coligacao.para.estado("CE"))
#grafico.de.dispersao.estadual.para(governador.senador.coligacao.para.estado("DF"))
#grafico.de.dispersao.estadual.para(governador.senador.coligacao.para.estado("ES"))
#grafico.de.dispersao.estadual.para(governador.senador.coligacao.para.estado("GO"))
#grafico.de.dispersao.estadual.para(governador.senador.coligacao.para.estado("MA"))
#grafico.de.dispersao.estadual.para(governador.senador.coligacao.para.estado("MG"))
#grafico.de.dispersao.estadual.para(governador.senador.coligacao.para.estado("MS"))
#grafico.de.dispersao.estadual.para(governador.senador.coligacao.para.estado("MT"))
#grafico.de.dispersao.estadual.para(governador.senador.coligacao.para.estado("PA"))
#grafico.de.dispersao.estadual.para(governador.senador.coligacao.para.estado("PB"))
#grafico.de.dispersao.estadual.para(governador.senador.coligacao.para.estado("PE"))
#grafico.de.dispersao.estadual.para(governador.senador.coligacao.para.estado("PI"))
#grafico.de.dispersao.estadual.para(governador.senador.coligacao.para.estado("PR"))
#grafico.de.dispersao.estadual.para(governador.senador.coligacao.para.estado("RJ"))
#grafico.de.dispersao.estadual.para(governador.senador.coligacao.para.estado("RN"))
#grafico.de.dispersao.estadual.para(governador.senador.coligacao.para.estado("RO"))
#grafico.de.dispersao.estadual.para(governador.senador.coligacao.para.estado("RR"))
#grafico.de.dispersao.estadual.para(governador.senador.coligacao.para.estado("RS"))
#grafico.de.dispersao.estadual.para(governador.senador.coligacao.para.estado("SC"))
#grafico.de.dispersao.estadual.para(governador.senador.coligacao.para.estado("SE"))
#grafico.de.dispersao.estadual.para(governador.senador.coligacao.para.estado("SP"))
#grafico.de.dispersao.estadual.para(governador.senador.coligacao.para.estado("TO"))


todos.os.estados <- rbind(
    governador.senador.coligacao.para.estado("AC"),
    governador.senador.coligacao.para.estado("AL"),
    governador.senador.coligacao.para.estado("AM"),
    governador.senador.coligacao.para.estado("AP"),
    governador.senador.coligacao.para.estado("BA"),
    governador.senador.coligacao.para.estado("CE"),
    governador.senador.coligacao.para.estado("DF"),
    governador.senador.coligacao.para.estado("ES"),
    governador.senador.coligacao.para.estado("GO"),
    governador.senador.coligacao.para.estado("MA"),
    governador.senador.coligacao.para.estado("MG"),
    governador.senador.coligacao.para.estado("MS"),
    governador.senador.coligacao.para.estado("MT"),
    governador.senador.coligacao.para.estado("PA"),
    governador.senador.coligacao.para.estado("PB"),
    governador.senador.coligacao.para.estado("PE"),
    governador.senador.coligacao.para.estado("PI"),
    governador.senador.coligacao.para.estado("PR"),
    governador.senador.coligacao.para.estado("RJ"),
    governador.senador.coligacao.para.estado("RN"),
    governador.senador.coligacao.para.estado("RO"),
    governador.senador.coligacao.para.estado("RR"),
    governador.senador.coligacao.para.estado("RS"),
    governador.senador.coligacao.para.estado("SC"),
    governador.senador.coligacao.para.estado("SE"),
    governador.senador.coligacao.para.estado("SP"),
    governador.senador.coligacao.para.estado("TO"))


# Análise de correlação por estado
correlacao.para.estado <- function(estado)
    cor(todos.os.estados[todos.os.estados$estado == estado, ]$votos.para.governador,
        todos.os.estados[todos.os.estados$estado == estado, ]$votos.para.senador,
        method = "spearman")


estados <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG",
             "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR",
             "RS", "SC", "SE", "SP", "TO")

print(data.frame(correlacao = Vectorize(correlacao.para.estado)(estados)))


# Gráfico nacional
grafico.de.dispersao.nacional.para <- function(dados) {
    grafico.de.dispersao.para(dados) +
        geom_point(aes(colour = estado)) +
        labs(colour = "Estado")
}

png(filename = "dispersao-governador-senador-coligacao-nacional.png")
    grafico.de.dispersao.nacional.para(todos.os.estados)
dev.off()


# Análise de correlação
# teste de normalidade das variáveis aleatórias analisadas
shapiro.test(todos.os.estados$votos.para.governador)
shapiro.test(todos.os.estados$votos.para.senador)

# caso os dados sejam normais
cor.test(todos.os.estados$votos.para.governador,
         todos.os.estados$votos.para.senador,
         alternative = "greater",
         method = "pearson")

# caso os dados não sejam normais
cor.test(todos.os.estados$votos.para.governador,
         todos.os.estados$votos.para.senador,
         alternative = "greater",
         method = "spearman")

cor.test(todos.os.estados$votos.para.governador,
         todos.os.estados$votos.para.senador,
         alternative = "greater",
         method = "kendall")



