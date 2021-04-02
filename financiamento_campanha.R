library(data.table)
library(tidyverse)
library(bit64)
library(ggplot2)
library(ggthemes)



# importar dados


colunas_uteis <- c("DT_PRESTACAO_CONTAS",
                    "SQ_PRESTADOR_CONTAS",
                    "NR_CNPJ_PRESTADOR_CONTA",
                    "SQ_CANDIDATO",
                    "DS_FONTE_RECEITA",
                    "DS_ORIGEM_RECEITA",
                    "DS_NATUREZA_RECEITA",
                    "DS_ESPECIE_RECEITA",
                    "NR_CPF_CNPJ_DOADOR",
                    "NM_DOADOR",
                    "NM_DOADOR_RFB",
                    "DS_ESFERA_PARTIDARIA_DOADOR",
                    "SQ_CANDIDATO_DOADOR", 
                    "DS_CARGO_CANDIDATO_DOADOR",
                    "SG_PARTIDO_DOADOR",
                    "NR_RECIBO_DOACAO",
                    "NR_DOCUMENTO_DOACAO",
                    "SQ_RECEITA",
                    "DT_RECEITA",
                    "DS_RECEITA",
                    "VR_RECEITA")


seleciona_col <- c("NR_TURNO", 
                   "DS_CARGO", 
                   "SQ_CANDIDATO", 
                   "NM_URNA_CANDIDATO", 
                   "NR_CPF_CANDIDATO", 
                   "SG_PARTIDO",
                   "VR_DESPESA_MAX_CAMPANHA",
                   "NM_UE")



receita_cand <- fread("C:\\Users\\mcaio\\Desktop\\Nova pasta\\receitas_candidatos_2020_SP.csv",
                      encoding = "Latin-1",
                      select = colunas_uteis)


candidatos_sp <- fread("C:\\Users\\mcaio\\Desktop\\Nova pasta\\consulta_cand_2020_SP.csv",
                       encoding = "Latin-1",
                       select = seleciona_col)



# base de dados pra são paulo

financiamento_candidatos_sp <- receita_cand %>%
  dplyr::mutate(VR_RECEITA = stringr::str_replace_all(VR_RECEITA,",", "."),
                VR_RECEITA = as.numeric(VR_RECEITA)) %>% 
  dplyr::filter(DS_ORIGEM_RECEITA == "Recursos próprios") %>% 
  dplyr::group_by(SQ_CANDIDATO) %>% 
  dplyr::summarise(valor = sum(VR_RECEITA)) %>% 
  dplyr::arrange(desc(valor)) %>% 
  dplyr::left_join(candidatos_sp, by = "SQ_CANDIDATO") %>% 
  dplyr::select(c("NR_TURNO",
           "DS_CARGO",
           "NM_UE",
           "NM_URNA_CANDIDATO",
           "SG_PARTIDO",
           "SQ_CANDIDATO",
           "valor"))




# gráfico vereadores que mais doaram

financiamento_candidatos_sp %>% 
  dplyr::filter(DS_CARGO == "VEREADOR") %>% 
  dplyr::slice_max(order_by = valor, n = 10) %>% 
  dplyr::rename(PARTIDOS = SG_PARTIDO) %>% 
  ggplot(aes(fct_reorder(NM_URNA_CANDIDATO, valor), valor, fill = PARTIDOS))+
  scale_y_continuous(labels = scales::number_format())+
  geom_col()+
  coord_flip()+
  xlab("")+
  ylab("")+
  ggtitle('Vereadores que mais doaram para própria campanha')+
  theme_hc()+
  theme(axis.text.x = element_text(face = 'bold', size = 11),
        axis.text.y = element_text(face = "bold", size = 10))




  