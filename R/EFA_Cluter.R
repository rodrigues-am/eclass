###############################################
### Análise eclass 2016 (primeira aplicação)
### EFA e Cluster
###############################################

#### Carrega Bibliotecas ####
library(tidyr)    # organização de dados
library(dplyr)    # manipulação de daddos
library(ggplot2)  # produção de gráficos
library(ggthemes) # temas e cores para gráficos
library(descr)    # produção de tabela cruzada
library(Hmisc)    # diversos (correlação)
library(corrplot) # gráfico de correlações
library(ggdendro) # gráfico de cluster
library(psych)    # funções sobre EFA 

#### Carrega dados ####
eclass16 <- read.csv("/data/andre/R/eclass/dados/eclass_16.csv")

#### Define funções ####

# Define função flattenCorrMatrix 

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

#### Descritiva ####
ec.p1 <- eclass16 %>%
  select(NUSP, Q101A:Q130B)

# Criação do sumário
ec.desc <- ec.p1 %>%
  select(-NUSP) %>%
  gather(key=quest, value=resp) %>%
  group_by(quest) %>%
  summarise(med=mean(resp, na.rm = TRUE), dp=sd(resp, na.rm = TRUE),
            errop=dp/sqrt(n()),
            total=n()) %>%
  mutate(type=ifelse(grepl("A", quest), "P", "F")) %>%
  arrange(desc(quest))
  
  

# Gráfico descritivo 1

gd1 <- ggplot(ec.desc, aes(x=factor(quest), y=med)) +
  geom_point(aes(fill=type), shape=21, size=2) +
  geom_errorbar(aes(ymin=med-errop, ymax=med+errop),
                size=.3, width=.2, position=position_dodge(.9)) +
  theme_minimal() +
  scale_fill_wsj(name="Resposta", 
                 labels=c("Físico experimental", "Você mesmo")) +
  coord_flip()
 

gd1
  




# Gráfico descritivo selecionado
f.ec.desc <- filter(ec.desc, quest %in% c("Q116A", "Q116B", "Q128A", 
                                          "Q128B", "Q130A", "Q130B"))

gds <- ggplot(f.ec.desc, aes(x=factor(quest), y=med)) +
  geom_point(aes(fill=type), shape=21, size=3.5) +
  geom_errorbar(aes(ymin=med-errop, ymax=med+errop),
                size=.3, width=.2, position=position_dodge(.9)) +
  theme_wsj() +
  theme(panel.grid.major.x=element_line(linetype =3, colour="grey70"))+
  scale_fill_wsj(name="Resposta", 
                 labels=c("Físico experimental", "Pessoal")) +
  scale_x_discrete(labels=c("","16 - O objetivo principal de realizar \nexperimentos é confirmar resultados conhecidos",
                            
                            "","28 - Não espero que realizar um \nexperimento ajude a compreender a física",
                            
                            "","30 - Experimentos de física contribuem \npara a ampliação do conhecimento científico"))+
  coord_flip()


gds

ggsave("pdescr.png", plot=gds,height = 4, width = 12)

#### Correlação ####

# primeira parte


p1.cor <- ec.p1 %>%
  select(-NUSP) %>%
  cor() %>%
  as.matrix()%>%
  rcorr()
p1.corfl <- flattenCorrMatrix(p1.cor$r,p1.cor$P)

p1.corfl <- p1.corfl %>%
  arrange(desc(abs(cor)))


# Gráfico da matriz de correlação
corp1 <- ec.p1 %>%
  select(-NUSP) %>%
  cor() %>%
  corrplot(., method="circle", order="hclust")
  
corp1

test <- p1.corfl %>%
  filter(abs(cor)>0.70) %>%
  select(row)


corp1 <- ec.p1 %>%
  select(-NUSP) %>%
  select(Q124A,Q116A, Q115A, Q113A, Q111A, Q125B, Q104A, Q127B, Q117A, Q118A, Q108B,
         Q101A, Q126A, Q125B, Q119A, Q109A, Q103A, Q121A, Q110A, Q112B, Q112A) %>%
  cor() %>%
  corrplot.mixed()
corp1

corrplot.mixed(as.matrix(p1.cor))

write.csv(p1.corfl, file="cor1.csv")

head(filter(p1.corfl, row=="Q116A"), 20)

#### EFA ####

# parte 1
p1.pca <- ec.p1 %>%
  select(-NUSP) %>%
  na.omit() %>%
  princomp()

summary(p1.pca)
screeplot(p1.pca)
biplot(p1.pca)

p1.fac <- ec.p1 %>%
  tbl_df() %>%
  select(contains("A")) %>%
  fa(.,nfactors=5, rotate="varimax")
  

print(p1.fac, cut=0.27, sort=TRUE)
plot(p1.fac)
fa.diagram(p1.fac)
fa.graph(p1.fac)


#### Cluster ####

clust1 <- ec.p1 %>%
  select(-NUSP) %>%
  as.matrix()%>%
  dist() %>%
  hclust()

ggdendrogram(clust1, rotate=TRUE)


# Cortar cluster 

clust2 <- cutree(clust1, k=5)

ggdendrogram(clust1, rotate=TRUE) +
  geom_hline(aes(yintercept=17), linetype=2)
table(clust2)

t <- cbind(ec.p1, clust2)

test <- ggplot(ec.p1, aes(x=Q104A, y=Q111B, col=factor(clust2)))+
  geom_jitter(size=3, alpha=0.6)+
  theme_wsj()+
  labs(x="Pessoal", y="Físico Experimental" , 
       title="16 - O objetivo principal de realizar \nexperimentos é confirmar resultados conhecidos") +
  scale_color_wsj(name="Cluster \nGrupos")+
  theme(plot.title = element_text(size = rel(0.75)),
        legend.title=element_text(size = rel(0.5)))

test

ggsave("q16-clust.png", plot=test,height = 8.5, width = 8.5)

  


#### Cruzamento ####

test <- eclass16 %>%
  mutate(mr1=(Q126A+Q123A+Q106A+Q122A+ Q108A+
              Q101A+ Q114A+ Q102A+ Q129A)/9,
         mr2=(Q125A+Q128A+Q127A+Q103A-Q111A+Q112A+Q121A)/7,
         mr3=(Q109A+Q110A-Q107A+Q120A)/4,
         mr4=(Q117A+Q104A+Q124A+Q113A+Q105A)/5,
         mr5=(Q119A+Q130A+Q116A+Q118A+Q115A)/5) %>%
  select(mr1:mr5) %>%
  cbind(.,clust2) %>%
  gather(key=factor, value=med, -clust2)
  

ggplot(test, aes(x=factor(factor), y=med, fill=factor(clust2))) + 
  geom_bar(stat="identity", position = "dodge")+
  theme_wsj()+
  scale_fill_wsj(name="Grupos")+
  scale_x_discrete(label=c("F1", "F2", "F3", "F4", "F5"))+
  labs(y=NULL, x="Fatores")

  select(Q126A, Q123A, Q106A, Q122A, Q108A,
         Q101A, Q114A, Q102A, Q129A) %>%
  
View(mr1)





