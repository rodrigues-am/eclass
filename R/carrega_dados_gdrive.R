####################################
### Carrega dados E-Class a partir do google drive
####################################

#### Carrega Bibliotecas ####
library(tidyverse) # manipulação de dados
library(googlesheets) # uso do google drive

# Evita ter que abrir o navegador
# Alerta: você deve copiar o link gerado e seguir as instruções do google drive para conseguir acesso
options(httr_oob_default=TRUE)

# Solicita autorização para acessar a conta do google drive
gs_auth() 
# Copiar o link no navegador

# Carrega a planilha Página 1 no data frame ec
# Caso a planilha mude de localização o aqruivo não será aberto corretamente
ec <- gs_title("Dados novos") %>% 
  gs_read()

# Visualiza o banco de dados ec
View(ec)








