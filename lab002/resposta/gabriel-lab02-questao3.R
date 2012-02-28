# Leitura do arquivo
data = read.table("atividade-maquinas-dsc.txt", header = TRUE)

summary.tempo.por.maquina <- function() {
    summary.tempo.ocupado.por.maquina = with(data[data$ociosa == FALSE, ],
                                             aggregate(data[data$ociosa == FALSE, ]$intervalo,
                                                       by = list(laboratorio, maquina),
                                                       sum))
    names(summary.tempo.ocupado.por.maquina) <- c("laboratorio", "maquina", "tempo.ocupada")

    summary.tempo.ocioso.por.maquina = with(data[data$ociosa == TRUE, ],
                                            aggregate(data[data$ociosa == TRUE, ]$intervalo,
                                                      by = list(laboratorio, maquina),
                                                      sum))
    names(summary.tempo.ocioso.por.maquina) <- c("laboratorio", "maquina", "tempo.ociosa")

    summary.tempo.por.maquina = merge(summary.tempo.ocupado.por.maquina,
                                      summary.tempo.ocioso.por.maquina)


    summary.tempo.por.maquina$prop.ocupada = summary.tempo.por.maquina$tempo.ocupada /
                                             (summary.tempo.por.maquina$tempo.ociosa +
                                              summary.tempo.por.maquina$tempo.ocupada)

    return(summary.tempo.por.maquina)
}
summary.tempo.por.maquina <- summary.tempo.por.maquina()

# Remove outlier do LSD, na resposta da questão 2, quando discuti sobre os outliers da proporção de tempo ocupado,
# explico por que considero esta máquina um outlier.
summary.filtrado = subset(summary.tempo.por.maquina, maquina != "barbado_1.lsd.ufcg.edu.br@xmpp.ourgrid.org")

summary.ordenado = summary.filtrado[order(summary.filtrado$prop.ocupada, decreasing = TRUE), ]

top10.maquinas.ocupadas = summary.ordenado[1:10, c("maquina", "laboratorio", "prop.ocupada")]

write.table(top10.maquinas.ocupadas, file = "output-questao3.txt")
