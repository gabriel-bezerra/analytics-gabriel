for i in votacao_candidato_munzona_2010_*.txt; do iconv -f ISO_8859-1 -t UTF-8 $i > $i.utf8; done
