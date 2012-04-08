#!/bin/sh

# Baixa dados
wget http://agencia.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona/votacao_candidato_munzona_2010.zip

# Extrai dados baixados
unzip votacao_candidato_munzona_2010.zip

# Converte para UTF-8
for i in votacao_candidato_munzona_2010_*.txt; do iconv -f ISO_8859-1 -t UTF-8 $i > $i.utf8; done

