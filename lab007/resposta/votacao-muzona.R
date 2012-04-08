
votacao.muzona.para <- function(estado) {
    votacao.muzona <- read.csv2(file = paste("votacao_candidato_munzona_2010_", estado ,".txt.utf8", sep = ""),
                                col.names = c("data.geracao", "hora.geracao", "ano.eleicao", "num.turno",
                                              "descricao.eleicao", "sigla.uf", "sigla.ue", "codigo.municipio",
                                              "nome.municipio", "numero.zona", "codigo.cargo", "numero.cand",
                                              "sequencial.candidato", "nome.candidato", "nome.urna.candidato",
                                              "descricao.cargo", "cod.sit.cand.superior", "desc.sit.cand.superior",
                                              "codigo.sit.candidato", "desc.sit.candidato", "codigo.sit.cand.tot",
                                              "desc.sit.cand.tot", "numero.partido", "sigla.partido", "nome.partido",
                                              "sequencial.legenda", "nome.coligacao", "composicao.legenda",
                                              "total.votos"))

    list(deputados.estaduais = votacao.muzona[votacao.muzona$descricao.cargo == "DEPUTADO ESTADUAL", ],
         deputados.federais = votacao.muzona[votacao.muzona$descricao.cargo == "DEPUTADO FEDERAL", ],
         governadores = votacao.muzona[votacao.muzona$descricao.cargo == "GOVERNADOR", ],
         senadores = votacao.muzona[votacao.muzona$descricao.cargo == "SENADOR", ])
}

stopifnot(levels(factor(votacao.muzona.para("PB")$deputados.estaduais$descricao.cargo)) == "DEPUTADO ESTADUAL")
stopifnot(levels(factor(votacao.muzona.para("PB")$deputados.federais$descricao.cargo)) == "DEPUTADO FEDERAL")
stopifnot(levels(factor(votacao.muzona.para("PB")$governadores$descricao.cargo)) == "GOVERNADOR")
stopifnot(levels(factor(votacao.muzona.para("PB")$senadores$descricao.cargo)) == "SENADOR")

