library(mokken)
library(mirt)
# library(lavaan)
# library(tidyverse)
# library(psych)

# selecionando apenas os itens do instrumento
# HAAS <- QUADRIL_FINAL %>% select(
#   HAAS1, HAAS2, HAAS3, HAAS4)

HAAS <- readRDS("HAAS.rds")
# transformando em data.frame para analisar os dados
HAAS <- as.data.frame(HAAS)

# dataframe que vamos trabalhar
df <- HAAS

# encoding dicotômico
# n_itens = ncol(HAAS)
# n_categorias <- apply(HAAS, 2, function(col) length(unique(col)))
# nomes_colunas <- list()
# for (i in 1:n_itens) {
#   for(j in 1:n_categorias[i]){
#     col_name = sprintf("HAAS%d_C%d", i, j-1)
#     nomes_colunas <- append(nomes_colunas, col_name)
#   }
# }
# n_pacientes = nrow(HAAS)
# df <- data.frame(matrix(0, nrow = n_pacientes, ncol = length(nomes_colunas)))
# names(df) <- nomes_colunas
# for (i in 1:n_pacientes){
#   ic = 0
#   for(j in 1:n_itens){
#     x = HAAS[i,j]
#     for(k in 1:n_categorias[j]){
#       ic <- ic + 1
#       if(x >= k-1){
#         df[i, ic] <- 1
#       }
#     }
#   }
# }
# df
# cols_to_remove <- grep("_C0", names(df))
# df <- df[, -cols_to_remove]


# analisando o número de dimensões
# aisp(df, search = 'normal')
aisp1 <- aisp(df, search = 'normal', lowerbound=seq(.3,.8,by=.05))
aisp1
aisp2 <- aisp(df, search = 'ga', lowerbound=seq(.3,.8,by=.05))
aisp2
df <- df[,colnames(df)[which(aisp2[,"0.3"] == 1)]]
df

# analisando os coeficientes de escalonabilidade
H <- coefH(df)

# minsize = 15
minsize = 40

# checando monotonicidade - MHM
monotonicity_test <- check.monotonicity(df, minsize = minsize)
summary(monotonicity_test)
plot(monotonicity_test)

### Análise de Interseção

## Essas análises de interseção estão olhando para as ISRF
## segundo tutorial Sijtsma deve-se olhar par IRF no caso politomico

# r.restscore <- check.restscore(df, minsize = 40)
# summary(r.restscore)
# plot(r.restscore)
# # analise de insterseção com pmatrix
# r.pmatrix <- check.pmatrix(df)
# summary(r.pmatrix)
# plot(r.pmatrix)

## OII - MDM
# Either "MIIO" (default), "MSCPM", or "IT". 
check.iio.test <- check.iio(
  df, 
  method = "MIIO", 
  minsize = minsize, 
  verbose = FALSE, 
  item.selection = FALSE,
)
summary(check.iio.test)
par(mfrow=c(2,3))
plot(check.iio.test)

## Análise de independência local
IL <- check.ca(df, Windex=T, MINSIZE=minsize)
IL$InScale
summary(IL)

## Reliability
check.reliability(
  df,
  MS = TRUE,
  alpha = TRUE,
  lambda.2 = TRUE,
  LCRC = TRUE
)

# ==== Análise Paramétrica ==============
# https://github.com/ccs-amsterdam/r-course-material/blob/master/tutorials/R_test-theory_3_irt_graded.md
fitGraded <- mirt(df, 1, itemtype = "graded", SE = FALSE, verbose = TRUE)
fitGraded
fitRash <- mirt(df, 1, itemtype = "Rasch", SE = FALSE, verbose = TRUE)
fitRash
anova(fitRash, fitGraded)
theta <- fscores(fitGraded, full.scores.SE = TRUE)
empirical_rxx(theta)

summary(fitGraded)
M2(fitGraded, type = "C2", calcNULL = FALSE)
itemfit(fitGraded, p.adjust = 'fdr')
params <- coef(fitGraded, IRTpars = TRUE, simplify = TRUE)
round(params$items, 2) # g = c = guessing parameter

CCI_empirica <- lapply(
  1:ncol(df), 
  function(x) itemfit(fitGraded, empirical.plot = x)
)


plot(fitGraded, type = 'trace')
itemplot(fitGraded, item = 1, type = "trace")
plot(fitGraded, type = "infoSE")
plot(fitGraded, type = 'infotrace')


# # CTT
# fitCTT <- cfa("atividade =~ HAAS1 + HAAS2 + HAAS3 + HAAS4", data = df)
# # IRT solution
# summary(fitCTT)
# standardizedsolution(fitCTT)


# =======================================

citation('mokken')
citation('mirt')
# citation('tidyverse')
# citation('psych')