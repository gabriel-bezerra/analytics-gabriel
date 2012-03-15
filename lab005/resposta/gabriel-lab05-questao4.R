library("nortest")

arquivo.de.dados <- commandArgs(trailingOnly = TRUE)[1]

dados <- read.table(file = arquivo.de.dados, header = TRUE)

## 4) Considerando os totais de tempo em que cada máquina permaneceu ociosa e os totais
## de tempo em que cada máquina permaneceu ocupada, implemente uma solução em R que
## permite utilizar os testes Anderson-Darling e Shapiro-Wilk para verificar se os tempos em que
## as máquinas permaneceram nesses estados seguem uma distribuição normal, antes e após
## uma transformação com sqrt e log. Escreva um relatório explicando sua solução, apresentando
## os resultados estatísticos que obteve, discutindo as conclusões que os resultados permitem
## tirar e comparando os resultados gerados pelos testes.

