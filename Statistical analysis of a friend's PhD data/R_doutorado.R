#Ajuda para a minha amiga do doutorado
library("tidyverse")
library("ggplot2")
library("readtext")
library("reshape2")
library("qgraph")
library("ggfortify")
library("ltm")
library("pheatmap")
library("factoextra")
##
library("readxl")
library("compare")
library("dplyr")
library("raster")
library("rio")
library("leaflet")
library("spData")
library("spDataLarge")
##
##
library(factoextra)
library(ggpubr)
library(gridExtra)
##


dados = data.frame(
  read.table(
    "xxxxxx", 
    header = T, sep = ",", encoding = "ANSI"
    )
  )
colnames(dados)
dados$X <- NULL
dados$X.1 <- NULL
dados$X.2 <- NULL

video1 <- dados[8:13]
video2 <- dados[14:18]
video3 <- dados[19:24]
video4 <- dados[25:29]
videos <- dados[30:34]

names(video1) <- c("q1v1", "q2v1", "q3v1", "q4v1", "q5v1", "q6v1")
names(video2) <- c("q1v2", "q2v2", "q3v2", "q4v2", "q5v2")
names(video3) <- c("q1v3", "q2v3", "q3v3", "q4v3", "q5v3", "q6v3")
names(video4) <- c("q1v4", "q2v4", "q3v4", "q4v4", "q5v4")
names(videos) <- c("q1g", "q2g", "q3g", "q4g", "q5g")

n_meninos = sum(dados$Gênero == "Masculino")
n_meninas = sum(dados$Gênero == "Feminino")


ggplot(dados, aes(x = video3$q1v3, y = Gênero, color = Você.usa.aparelho.auditivo.ou.implante.coclear.)) +
  geom_count()

dados_rel <- as.data.frame(dados[, c(1, 9, 16, 19, 29, 11, 21, 26, 33, 20, 25, 12, 17, 23, 27, 31, 31, 13, 18, 24, 28, 30)])
names(dados_rel) <- c("Nome", "C1", "C2", "C3", "C4", "S1", "S3", "S4", "SG", "R3", "R4", "D1", "D2", "D3", "D4", "DGa", "DGb", "M1", "M2", "M3", "M4", "MG")

dados_rel_num <- dados_rel
Verd <- c("Sim", "Ótima")
Fals <- c("Não", "Horrível")
Talvez <- c("Talvez")
Ruim <- c("Ruim")
Média <-c("Média")
Boa <- c("Boa")
MB <- c("Muito boa")

#sum(is.na(dados_rel_num) == T)

for (i in seq_along(dados_rel_num)) {
  dados_rel_num[[i]][dados_rel_num[[i]] %in% Verd] <- as.numeric(1)
  dados_rel_num[[i]][dados_rel_num[[i]] %in% MB] <- as.numeric(0.8)
  dados_rel_num[[i]][dados_rel_num[[i]] %in% Boa] <- as.numeric(0.6)
  dados_rel_num[[i]][dados_rel_num[[i]] %in% Talvez] <- as.numeric(0.5)
  dados_rel_num[[i]][dados_rel_num[[i]] %in% Média] <- as.numeric(0.4)
  dados_rel_num[[i]][dados_rel_num[[i]] %in% Ruim] <- as.numeric(0.2)
  dados_rel_num[[i]][dados_rel_num[[i]] %in% Fals] <- as.numeric(0)
}

