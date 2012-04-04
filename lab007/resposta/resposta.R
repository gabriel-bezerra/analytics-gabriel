perfil.eleitorado <- read.table(file = "perfil_eleitorado_2010.txt", sep = ";",
                                col.names = c("ano", "uf", "municipio", "codigo.municipio", "zona", "sexo",
                                              "faixa.etaria", "escolaridade", "x"))

perfil.eleitorado.pb <- perfil.eleitorado[perfil.eleitorado$uf == "PB", ]

rm(perfil.eleitorado)
gc()


#summary(perfil.eleitorado.pb)

numero.de.eleitores.por.zona <-
    aggregate(perfil.eleitorado.pb$x,
              by = list(municipio = perfil.eleitorado.pb$municipio,
                        codigo.municipio = perfil.eleitorado.pb$codigo.municipio,
                        zona = perfil.eleitorado.pb$zona),
              sum)

hist(numero.de.eleitores.por.zona$x)

stopifnot(sum(perfil.eleitorado.pb$x) == sum(numero.de.eleitores.por.zona$x))


perfil.proporcional.por.zona <-
    aggregate(perfil.eleitorado.pb[, c("zona", "x")],
              by = list(municipio = perfil.eleitorado.pb$municipio,
                        codigo.municipio = perfil.eleitorado.pb$codigo.municipio,
                        zona = perfil.eleitorado.pb$zona,

                        sexo = perfil.eleitorado.pb$sexo,
                        faixa.etaria = perfil.eleitorado.pb$faixa.etaria,
                        escolaridade = perfil.eleitorado.pb$escolaridade),
              function(x) {
                  print(x)
                  #x$x / numero.de.eleitores.por.zona[numero.de.eleitores.por.zona == x$zona, ]
              })



summary(perfil.proporcional.por.zona)

#print(escolaridade.por.municipio)
#summary(escolaridade.por.municipio)
#hist(escolaridade.por.municipio)





#votacao.candidado <- read.table(file = "votacao_candidato_munzona_2010_PB.txt", sep = ";")
#summary(votacao.candidado)
