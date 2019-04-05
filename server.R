set.seed(2018)
require(shiny)
require(shinythemes)
require(igraph)
require(TraMineR)
require(ggthemes)
require(RColorBrewer)
require(cluster)
require(purrr)
library(keras)                                                                  
require(shinyWidgets)
require(data.table)
require(plyr)
require(dplyr)
require(ggplot2)
require(tidyr)
require(readr)
require(stringr)
require(magrittr)
require(DT)
require(rsconnect)
require(pdftools)
require(yardstick)
require(forcats)
require(kableExtra)
require(plotly)
require(shinydashboard)
require(zoo)
 
#Impedir que o tensorflow use a GPU
#Sys.setenv("CUDA_VISIBLE_DEVICES" = -1)
server= function(input, output, session){
  prereq_lista <-  read_rds("data/PreReq_Lista.rds")
  Prereq <- prereq_lista %>% unnest()
  PreReq_Total <- prereq_lista %>% unnest()
  materias <- prereq_lista%>% select(Nome,Cod)
  names(materias) <- c("nm","cm")
  dados <- read_rds("data/dados_curso.rds")
  fluxograma1 <- read_rds("data/fluxograma2.rds")
  Materias_completas <- read_rds("data/Materias_completas.rds")
  dadosalunos <- read_rds("data/dadosalunos.rds")
  dados_curso <- read_rds("data/dados_curso.rds")
  dados_curso_modificado <-dados_curso %>% mutate(Título=as.factor(Título),Título=fct_recode(Título,"BACHARELADO EM" = "Bacharel", 
             "LICENCIATURA EM"="Licenciado"),grau_curso = paste(Título,Nome)) %>% select(`Nome-Código`,grau_curso)
  fluxograma1 <- left_join(fluxograma1,dados_curso_modificado ,by=c("Curso"="Nome-Código"))
  
  
  ##########  Fluxograma       ###########

  fluxocurso <- function(y){filter(fluxograma1,fluxograma1[,3]==y)}

  nomemateria <- function(x){fluxocurso(x)[,4]}
 
  codigomateria <- function(x){materias[which(materias[,2] %in% fluxocurso(x)[,4]),1]}
 
  
  numsemestres <- function(x){length(unique(x[,2])) }
  
  qtporsem <- function(y){as.vector(table(fluxocurso(y)[,2]))}


   
  
  
  curso_igraph <- function(x){
    curso <-   fluxograma1 %>% dplyr::filter(Curso %in% x)
    req <-dplyr::filter(PreReq_Total,is.na(PreReq) | PreReq %in% curso$Matéria )
    curso_1 <- left_join(curso,unique(req %>% select(PreReq,Cod)),by=c("Matéria"="Cod"))
    c_ig<- curso_1 %>% select(Nome,PreReq,Semestre) %>% mutate(PreReq=PreReq %>% as.character) %>%
      mutate(PreReq=NULL) %>% unique
    return(c_ig)
  }
  
  

  ig_fun <- function(x){
    curso <-   fluxograma1 %>% dplyr::filter(Curso %in% x)
    req <-dplyr::filter(Prereq,Cod %in% curso$Matéria )
    curso_1 <- left_join(curso,unique(req,by=c("Matéria"="Cod")))
    curso_igraph <- curso_1 %>% select(Nome,PreReq,Semestre) %>% mutate(PreReq=PreReq %>% as.character)
    curso_igraph$PreReq <- curso_igraph$PreReq %>% str_extract_all("[0-9]{6}") 
    curso_igraph <- curso_igraph %>% unnest()
    materias_ig <- materias %>% data.frame %>% dplyr::filter(cm %in% curso$Matéria )
    curso_ig <- left_join(curso_igraph,materias_ig,by=c("PreReq"="cm")) %>% select(nm,Nome,Semestre) %>% mutate(nm=as.character(nm))
    curso_ig[,1][is.na(curso_ig[,1])] <- curso_ig[,2][is.na(curso_ig[,1])]
    ig <- graph_from_data_frame(curso_ig)
    return(ig)}
  
  
  grid_fun <- function(ig,c_ig){
    construcao <- data.frame(Nome=V(ig)$name,numero=V(ig) %>% as.numeric)
    build <- left_join(construcao,c_ig,"Nome") %>% unique %>% arrange(numero,Semestre)
    qtd <- build %>% group_by(Semestre) %>% dplyr::summarise(n=n())
    max_semestre <- qtd$n %>% max
    vetor <- list()
    for(i in 1:nrow(qtd)){vetor[[i]] <- seq(1,qtd$n[i])+mean(1:max_semestre)-mean(1:qtd$n[i])}
    elementos <- unlist(vetor)
    grid <- matrix(c(1:nrow(build),build$Semestre),ncol=2) %>% data.frame %>% arrange(X2,X1)
    grid$X3 <- elementos
    grid_ig <- grid %>% arrange(X1) %>% select(X3,X2) %>% as.matrix
    return(grid_ig)}
  
  construir_tronco <- function(x){
    curso <-   fluxograma1 %>% dplyr::filter(Curso %in% x)
    req <-filter(PreReq_Total,is.na(PreReq) | PreReq %in% curso$Matéria )
    curso_1 <- left_join(curso,unique(req %>% select(PreReq,Cod)),by=c("Matéria"="Cod"))
    curso_igraph <- curso_1 %>% select(Nome,PreReq,Semestre) %>% mutate(PreReq=PreReq %>% as.character)
    materias_ig <- materias %>% data.frame %>% dplyr::filter(cm %in% curso$Matéria )
    curso_ig <- left_join(curso_igraph,materias_ig,by=c("PreReq"="cm")) %>% select(nm,Nome,Semestre) %>% mutate(nm=as.character(nm))
    return(curso_ig)}
  
 
  algoritmo <- function(y){ 
    fun <-    function(x){
      curso <-   fluxograma1 %>% dplyr::filter(Curso == x)
      req <-dplyr::filter(Prereq,Cod %in% curso$Matéria )
      curso_1 <- left_join(curso,unique(req,by=c("Matéria"="Cod")))
      curso_igraph <- curso_1 %>% select(Nome,PreReq,Semestre) %>% mutate(PreReq=PreReq %>% as.character)
      curso_igraph$PreReq <- curso_igraph$PreReq %>% str_extract_all("[0-9]{6}") 
      curso_igraph <- curso_igraph %>% unnest()
      materias_ig <- materias %>% data.frame %>% dplyr::filter(cm %in% curso$Matéria )
      curso_ig <- left_join(curso_igraph,materias_ig,by=c("PreReq"="cm")) %>% select(nm,Nome,Semestre) %>% mutate(nm=as.character(nm))
      return(curso_ig)}
    dados_curso <- y %>% fun()
    semestre_prereq <- left_join(dados_curso %>% select(nm),dados_curso %>% select(Nome,Semestre) %>% unique,by=c("nm"="Nome"))
    dados_curso <- dados_curso %>% mutate(Semestre_Prereq=semestre_prereq$Semestre) %>% select(Nome,Semestre,nm,Semestre_Prereq)
    melhor_prefreq <- dados_curso %>% group_by(nm,Semestre_Prereq) %>% dplyr::summarise(freq=n()) %>% dplyr::filter(!is.na(nm)) 
    melhor_prefreq_cond <- melhor_prefreq %>% arrange(desc(freq)) %>% ungroup %>% top_n(3)
    funplot5 <- function(x){
      ig <-ig_fun(x) 
      ig <- delete.vertices(simplify(ig), igraph::degree(ig)==0)
      return(ig)}
    teste <-  y %>% funplot5() 
    
    caminhos <- list()
    for(i in 1:length(melhor_prefreq_cond$nm) ) {
      caminhos[[i]] <- all_simple_paths(teste,  from=melhor_prefreq_cond$nm[i], mode = c("total"))}
    Caminhos_unlisted <- unlist(caminhos,recursive = F)
    aux_algoritmo <- lapply(Caminhos_unlisted,length) %>% as.numeric
    names_Caminhos_unlisted <- rep(paste("Caminho",1:length(Caminhos_unlisted)),aux_algoritmo)
    
    df_caminhos <-data.frame("Materia"= Caminhos_unlisted %>% unlist %>% names, "Caminho"=names_Caminhos_unlisted) 
    df_caminhos_join <- left_join(df_caminhos,fluxograma1 %>% filter(Curso ==y),by=c("Materia"="Nome")) %>% 
      select(Materia,Caminho,Semestre ) %>% filter(Semestre <5) %>% arrange(Semestre)
    condicao <- df_caminhos_join %>% group_by(Caminho) %>% dplyr::summarise(n=n()) %>% filter(n==4)
    condicionado <- df_caminhos_join %>% filter(Caminho %in% condicao$Caminho) %>%group_by(Caminho) %>%
      dplyr::summarise(cond = all(1:4 %in% Semestre) 
                       &all(Semestre %in% 1:4  ) ) %>% filter(cond==TRUE)
    options(scipen=10)
    final <- df_caminhos_join %>% filter(Caminho %in% condicionado$Caminho) %>% mutate(Caminho=as.factor(Caminho))
    disc <- final$Materia %>% unique
    curso <- fluxograma1$nome_hab[fluxograma1$Curso %in% y] %>% nth(1) %>% as.character()
    db_curso <- dadosalunos %>%  filter((Curso == curso | ABI==curso) , Materia %in% disc,Conceito %in% c("MM","MS","SS","Inconcluído")) 
    frequencias_mencoes <- db_curso %>% select(Materia,Conceito) %>% group_by(Materia,Conceito) %>% dplyr::summarise(n=n())  %>% 
      mutate(freq = n / sum(n)) %>% arrange(Conceito,freq %>% desc)
    frequencias_mencoes_2 <- left_join(final,frequencias_mencoes,by=c("Materia")) %>%group_by(Materia,Caminho,Conceito,Semestre) %>% dplyr::summarise(maior = max(freq)) %>%
      dplyr::filter(Conceito == "Inconcluído") %>% arrange(Caminho)
    escolhido <- frequencias_mencoes_2 %>% group_by(Caminho) %>% dplyr::summarise(soma=sum(maior)) %>% arrange(soma %>% desc) %>% filter(row_number()==1) %>% nth(1) %>% as.character()
    tronco <- final %>% dplyr::filter(Caminho == escolhido) %>%arrange(Semestre) %>%  select(Materia) %>% unlist %>% as.character()
    return(tronco)
  }
  

  
  # preparo_db <- function(a,b,c,d,e,qtdclusters=2){
  #   curso <- fluxograma1$nome_hab[fluxograma1$Curso %in% a] %>% nth(1) %>% as.character()
  #   grau_curso <- fluxograma1$grau_curso[fluxograma1$Curso %in% a] %>% nth(1) %>% as.character()
  #   db_curso <- dadosalunos %>%  dplyr::filter(Curso == curso| Curso ==grau_curso )  %>% group_by(CPFEncrypted) %>%  
  #     dplyr::filter(StatusFinal %in% c("NÃO FORMATURA","FORMATURA"),Conceito !="CC",!any(Posmateria<0),!any(TempoPermanencia>16)) %>% ungroup
  #   tronco <- c(b,c,d,e)
  #   aux <- list()
  #   for(i in 1:length(tronco))aux[[i]] <- paste0(tronco[i],c(" Inconcluído"," MM"," MS"," SS"))
  #   es <- aux %>% unlist
  #   estados <- c(es,"CADEIA FINALIZADA","NÃO FORMATURA","FORMATURA")
  #   tronco_curso = db_curso %>% dplyr::filter(Materia %in% tronco ) 
  #   filtro <- tronco_curso  %>% group_by(CPFEncrypted) %>%  
  #     dplyr::filter(StatusFinal %in% c("NÃO FORMATURA","FORMATURA"),Conceito !="CC",!any(Posmateria<0)) %>% ungroup
  #   
  #   curso_util <- filtro %>% select(CPFEncrypted,Materia,Conceito,Posmateria,TempoPermanencia,StatusFinal) %>% 
  #     mutate(Materia_Mencao = paste(Materia,Conceito),Materia = NULL , Conceito = NULL,Posmateria = factor(Posmateria),
  #            CPFEncrypted=factor(CPFEncrypted)) 
  #   
  #   
  #   curso_util$Materia_Mencao <- as.factor(curso_util$Materia_Mencao)
  #   curso_util$StatusFinal <- as.factor(curso_util$StatusFinal)
  #   
  #   curso_TraMiner = curso_util  %>% group_by(Posmateria,CPFEncrypted) %>%  mutate(i=1:n()) %>%  
  #     spread(key=Posmateria,value=Materia_Mencao) %>% select(-i)
  #   
  #   curso_TraMiner[as.character(c(1:20)[!1:20 %in%  names(curso_TraMiner)])]=NA
  #   
  #   
  #   curso_TraMiner <- curso_TraMiner %>%  select(CPFEncrypted,TempoPermanencia,StatusFinal,as.character(1:20))
  #   
  #   fator_1 <- function(x){
  #     if(is.factor(x)) return(factor(x, levels=estados))
  #     return(x)
  #   }
  #   curso_TraMiner[,4:23] <- as.data.frame(lapply(curso_TraMiner[4:23], fator_1),names=1:20 %>% as.character())
  #   Fim <- curso_TraMiner$TempoPermanencia
  #   for(i in 1:nrow(curso_TraMiner)){
  #     curso_TraMiner[i,as.character(Fim[i]:20)] = as.character(curso_TraMiner$StatusFinal[i])
  #   }
  #   Finalizado <- list()
  #   for(i in 1:nrow(curso_TraMiner)){
  #     Finalizado[[i]] <- which(curso_TraMiner[i,] == paste0(e," MM")|
  #                                curso_TraMiner[i,] == paste0(e," MS")|
  #                                curso_TraMiner[i,] == paste0(e," SS")
  #     )
  #   }
  #   Nao_formados <- (which(plyr::ldply(Finalizado,length)[[1]]==0))
  #   
  #   concluido <- NULL
  #   for(i in 1:length(Nao_formados)){
  #     Finalizado[[Nao_formados[i]]]=NA
  #   }
  #   
  #   cadeia_finalizada <- unlist(Finalizado)-2
  #   for(i in 1:nrow(curso_TraMiner)){
  #     if(!is.na(cadeia_finalizada)[i])
  #       curso_TraMiner[i,cadeia_finalizada[i]:(Fim[i]-1) %>% as.character()]="CADEIA FINALIZADA"}
  #   
  #   dados <- as.data.frame(lapply(curso_TraMiner[4:23],as.factor))
  #   
  #   
  #   dados <- as.data.frame(lapply(dados,fator_1))
  #   curso_TraMiner[,4:23] <- dados
  #   ############# Número de observações indeterminadas
  #   nas <- NULL
  #   for(i in 1:nrow(curso_TraMiner)){
  #     nas[i] <- any(!is.na(curso_TraMiner[,4:5][i,])) 
  #   }
  #   
  #   
  #   curso_TraMiner_limpo <- curso_TraMiner[nas,] %>% data.frame
  #   names(curso_TraMiner_limpo)[4:23] <- 1:20
  #   curso_TraMiner_limpo <- curso_TraMiner_limpo[-which(is.na(curso_TraMiner_limpo[,4])),]
  #   aux_2 <- list()
  #   for(i in 1:length(tronco))aux_2[[i]] <- paste0(tronco[i],c(" MM"," MS"," SS"))
  #   aux_2
  #   transformacao <- function(x){
  #     data_util=c(t(x))
  #     
  #     
  #     data_util[which(grepl(paste(aux_2[[1]],collapse = "|"),data_util) &                                                                                                           
  #                       is.na(data_util[which(grepl(paste(aux_2[[1]],collapse = "|"),data_util))+1]))+1] <- paste0(tronco[2]," Inconcluído")
  #     
  #     data_util[which(grepl(paste(aux_2[[2]],collapse = "|"),data_util) & 
  #                       is.na(data_util[which(grepl(paste(aux_2[[2]],collapse = "|"),data_util))+1]))+1] <- paste0(tronco[3]," Inconcluído")
  #     
  #     data_util[which(grepl(paste(aux_2[[3]],collapse = "|"),data_util) & 
  #                       is.na(data_util[which(grepl(paste(aux_2[[3]],collapse = "|"),data_util))+1]))+1] <- paste0(tronco[4]," Inconcluído")
  #     
  #     
  #     
  #     
  #     return(data_util)
  #   }
  #   
  #   for(i in 1:nrow(curso_TraMiner_limpo)){
  #     curso_TraMiner_limpo[i,] <-  transformacao(curso_TraMiner_limpo[i,])
  #   }
  #   for(i in 1:nrow(curso_TraMiner_limpo)){
  #     curso_TraMiner_limpo[i,] <-  na.locf(c(t(curso_TraMiner_limpo[i,])))
  #   }
  # 
  #   
  #   
  #   m1 <- brewer.pal(n = 8, name = "Blues")[3:6]
  #   m2 <- brewer.pal(n = 8, name = "Greens")[3:6]
  #   m3 <- brewer.pal(n = 8, name = "Oranges")[3:6]
  #   m4 <- brewer.pal(n = 8, name = "Purples")[3:6]
  #   #m5 <- c("yellow","yellow2","yellow3","yellow4")
  #   colors <- c(m1,m2,m3,m4,"navy","firebrick","lawngreen")
  #   curso_seq <- seqdef(curso_TraMiner_limpo[,4:23],cpal=colors,alphabet = estados)
  #   ########################## Matriz de custos para o agrupamento ##################
  #   
  #   
  #   custos_mat <- matriz_custos()
  #   
  #   dist<- seqdist(curso_seq, method="OM", indel=0.0001,sm= custos_mat) 
  #   clusterward1 <- agnes(dist, diss=TRUE, method="ward")
  #   clusters <- cutree(clusterward1, k=qtdclusters)
  #   clusters_fac <- factor(clusters, labels =  paste("Cluster" ,1:qtdclusters )) 
  #   
  #   require(data.table)
  #   dados_gg <- data.table(curso_TraMiner_limpo[,4:23],clusters_fac)
  #   dados_gather <-dados_gg %>%  gather(Semestre,Materia,-clusters_fac) %>% mutate(Semestre=Semestre %>% as.numeric)
  #   Frequência <- dados_gather %>% group_by(Semestre,Materia,clusters_fac) %>% dplyr::summarise(n=n())
  #   tamanho_clusters <- dados_gather %>% group_by(clusters_fac) %>% dplyr::summarise(nc=n()/20) 
  #   freq_merge <- left_join(Frequência,tamanho_clusters,by="clusters_fac") %>% mutate(Frequência=n/nc,n=NULL,nc=NULL) 
  #   freq_merge$Materia <- factor(freq_merge$Materia,levels=estados)
  #   
  #   
  #   
  #   freq_merge$Semestre <- factor(freq_merge$Semestre,levels=1:20)
  #   labels_clusters <- paste0("Cluster " ,1:qtdclusters ,"\n n=",tamanho_clusters$nc, "\n Porcentagem do total = ",100*round(tamanho_clusters$nc/sum(tamanho_clusters$nc),4),"%" )
  #   freq_merge$clusters_fac <- factor(freq_merge$clusters_fac,labels=labels_clusters)
  #   
  #   
  #   p <- ggplot(freq_merge,aes(Semestre,Frequência,fill=Materia))+geom_bar(stat="identity")+
  #     facet_wrap(~clusters_fac,scales="free")+ scale_fill_manual(values=colors,drop=F)+
  #     guides(fill=guide_legend(ncol=1))+
  #     xlab("Semestre")+ylab(paste0("Total =",sum(tamanho_clusters$nc)))+
  #     theme(axis.text.y=element_blank(),
  #           axis.ticks.y=element_blank(),
  #                axis.text.x=element_blank(),
  #                axis.ticks.x=element_blank(),
  #           panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  #           
  #           )
  #   resumos <-  curso_TraMiner_limpo %>% mutate(Grupo = clusters_fac)
  #  
  #   db_summary <- inner_join(resumos %>% select(CPFEncrypted,Grupo),db_curso,by="CPFEncrypted")
  #   
  #   return(list(plot=p,data=curso_TraMiner_limpo[,4:23],data_completo=curso_TraMiner_limpo,resposta=clusters_fac,Agrupamento=db_summary))
  # 
  # }

  matriz_custos <- function(t,peso_t,f,c_d,c_f, indel){
    # t é a quantidade de matérias do tronco, default = 4
    # peso_t é o peso da distância entre cada menção, default = 1/4
    # f é o peso da formatura, default = 10
    # cf_d é o peso do desligamento, dado que a cadeia foi finalizada
    # cf_f é o peso da formatura, dado que a cadeia foi finalizada
    # custo_d_f é o peso da diferença entre formatura e desligamento
    # custo_finalizado é o peso da diferença da cadeia finalizada
    t <- 4-0.25
    peso_t <- 1/4
    custo_d_f <- 6
    c_d <- 10
    c_f <- 10
    custo_finalizado <- 7.5  
    ## A primeira linha
    l1 <- seq(0,t,peso_t)
    ## O comprimento da primeira linha
    comp <- length(l1)
    custos_seq <- numeric(comp*comp)
    custos_seq[1:comp] <- l1
    for (i in 1:(comp-1)) {
      custos_seq[(comp*i+1):(comp*(i+1))] <- c(l1[(i+1):2],l1[-(comp:(comp-(i-1)))])
    }
    custos_mat <- matrix(custos_seq, ncol =  (t/peso_t+1), byrow = TRUE)
    
    ## Adicionar uma coluna para desligado e uma para formado
    ultima_coluna = rev(l1)
    ## Definir o custo entre cadeia finalizada e desligado ou formado
    custo_cf_desligado <- c_d
    custo_cf_formado <- c_f
    custo_des_form <- custo_d_f

    custos_mat <- cbind(custos_mat,ultima_coluna+custo_finalizado,rev(ultima_coluna+custo_cf_desligado)   ,
                        ultima_coluna+custo_cf_formado)
    
    ## Adicionar as linhas correspondentes
    custos_mat <- rbind(custos_mat,c(ultima_coluna+custo_finalizado,0,c_d+4,c_f-0.25), c(rev(ultima_coluna+custo_cf_desligado),(c_d+4), 0, custo_des_form),
                        c(ultima_coluna+custo_cf_formado,(c_f-0.25), custo_des_form, 0))
    return(custos_mat)
  }
  mudar_niveis <- function(x){
    if(is.factor(x)){return(factor(x, levels=c(0:(input$qtdclusters-1) )))}
    return(x)
  }
  apendice2 <- matriz_custos() %>% `colnames<-`(c("Matéria 1 \n Inconcluído","Matéria 1\n MM","Matéria 1\nMS","Matéria 4\n SS",
                                                  "Matéria 2 \n Inconcluído.","Matéria 2\n MM","Matéria 2\n MS","Matéria 2\n SS",
                                                  "Matéria 3 \n Inconcluído","Matéria 3\n MM","Matéria 3\n MS","Matéria 3\n SS",
                                                  "Matéria 4 \n Inconcluído","Matéria 4\n MM","Matéria 4\n MS","Matéria 4\n SS",
                                                  "Cadeia Finalizada","Não Fomatura","Formatura"
                                                  
                                                  ))%>% `rownames<-`(c("Matéria 1 \n Inconcluído","Matéria 1\n MM","Matéria 1\nMS","Matéria 4\n SS",
                                                                       "Matéria 2 \n Inconcluído.","Matéria 2\n MM","Matéria 2\n MS","Matéria 2\n SS",
                                                                       "Matéria 3 \n Inconcluído","Matéria 3\n MM","Matéria 3\n MS","Matéria 3\n SS",
                                                                       "Matéria 4 \n Inconcluído","Matéria 4\n MM","Matéria 4\n MS","Matéria 4\n SS",
                                                                       "Cadeia Finalizada","Não Fomatura","Formatura"
                                                                       
                                                  ))
  output$matriz_custos <- DT::renderDataTable(
    DT::datatable(apendice2,filter='top',options = list(pageLength = 134,paging = FALSE,searchHighlight = TRUE,
                                                    columnDefs = list(list(targets =c(1:6), searchable = TRUE)))))
  outputOptions(output, "matriz_custos", suspendWhenHidden = FALSE)

  ##########################################################################################################
  informacoes <- reactiveValues()
   fluxograma_plot <- reactive({
    
if(input$curso!=""){
  informacoes$curso = req(input$curso)
  #traj <- algoritmo(input$curso)
  traj <- readRDS("trajetoria_save.rds")
  informacoes$traj <- traj
  m1 <- brewer.pal(n = 8, name = "Blues")[3:6]
  m2 <- brewer.pal(n = 8, name = "Greens")[3:6]
  m3 <- brewer.pal(n = 8, name = "Oranges")[3:6]
  m4 <- brewer.pal(n = 8, name = "Purples")[3:6]
  
    if (input$m1==""& input$m2==""& input$m3==""& input$m4==""){
      funplot0 <- function(x){

        c_ig<-curso_igraph(x) 
        ig <-ig_fun(x) 
        ig <- delete.vertices(simplify(ig), igraph::degree(ig)==0)
        V(ig)$color <- adjustcolor("lightgrey")
        E(ig)$color<- NA
        E(ig)$weight <- 0
        grid <- grid_fun(ig,c_ig)
        label=V(ig)$name %>% str_wrap(22)
       V(ig)[traj]$color = NA
       
        plot.igraph(ig,layout=grid,vertex.label.cex = 1, edge.arrow.size = 0.25,vertex.frame.color=NA,
                    vertex.shape = "rectangle",edge.lty=1,
                    edge.curved=.5,
                    vertex.size=max((strwidth(label)* 275))*.8,
                    vertex.size2=0.025 *1000,asp=0,
                    edge.width=2,
                    mark.groups = sapply(V(ig)[traj] %>% as.numeric,list),mark.col = c(m1[2],m2[2],m3[2],m4[2]),mark.border = NA,
                    vertex.label=label,mark.shape = 1,mark.expand = max((strwidth(label)* 40))
  
        );legend("topleft",legend=c("Primeiro Semestre", "Segundo Semestre","Terceiro Semestre","Quarto Semestre"),
                 col=c(m1[2],m2[2],m3[2],m4[2]), pch = 19, cex=1.2,pt.cex=2,
                 title="Matérias Escolhidas Automaticamente", text.font=20, bg='white')}
      return(funplot0(input$curso))}
    
    if (input$m1!=""& input$m2==""& input$m3==""& input$m4==""){
      fun_cor1 <- function(x,y){
      V(x)[y]$color <-m1[2]
      E(x)[incident(x, y, mode = c('out'))]$color <- m1[2]
      V(x)[neighbors(x, y, mode = c('out'))]$color <- "red"
      return(x)}
      funplot1 <- function(x,y){
        c_ig<-curso_igraph(x) 
        ig <-ig_fun(x) 
        ig <- delete.vertices(simplify(ig), igraph::degree(ig)==0)
        V(ig)$color <- adjustcolor("lightgrey")
        E(ig)$color<- NA
        E(ig)$weight <- 0
        label=V(ig)$name %>% str_wrap(22)
        V(ig)[traj]$color = NA
        ig1 <- fun_cor1(ig,y)
        grid <- grid_fun(ig1,c_ig)
        plot.igraph(ig1,layout=grid,vertex.label.cex = 1, edge.arrow.size = 0.25,
                    vertex.shape = "rectangle",edge.lty=1,
                    edge.curved=.5,
                    vertex.size=max(strwidth(label)*275),
                    vertex.size2=0.025*1000,asp=0,
                    edge.width=2,
                    mark.groups = sapply(V(ig)[traj] %>% as.numeric,list),mark.col = c(m1[2],m2[2],m3[2],m4[2]),mark.border = NA,
                    vertex.label=label,mark.shape = 0,mark.expand = max((strwidth(label)* 40))
                    
        );legend("topleft",legend=c("Primeiro Semestre", "Segundo Semestre","Terceiro Semestre","Quarto Semestre"),
                 col=c(m1[2],m2[2],m3[2],m4[2]), pch = 19, cex=1.2,pt.cex=2,
                 title="Matérias Escolhidas Automaticamente", text.font=20, bg='white')}
      return(funplot1(input$curso,input$m1))
    }

  

    if(input$m1!=""& input$m2!=""& input$m3==""& input$m4==""){
      fun_cor2 <- function(x,y,z){
        V(x)[y]$color <- m1[2]
        V(x)[y]$frame.color = "red"
        E(x)[paste0(y,"|",z)]$color <- m1[2]
        V(x)[z]$color <- m2[2]
        V(x)[z]$frame.color = "red"
        E(x)[incident(x, z, mode = c('out'))]$color <- m2[2]
        V(x)[neighbors(x, z, mode = c('out'))]$color <- "red"

        return(x)}
      funplot2 <- function(x,y,z){
        c_ig<-  curso_igraph(x) 
        ig <-ig_fun(x) 
        ig <- delete.vertices(simplify(ig), igraph::degree(ig)==0)
        V(ig)$color <- adjustcolor("lightgrey")
        E(ig)$color<- adjustcolor("white")
        E(ig)$weight <- 0
        label=V(ig)$name %>% str_wrap(22)
        V(ig)[traj]$color = NA

        E(ig)$color = NA
        ig1 <- fun_cor2(ig,y,z)
        grid <- grid_fun(ig1,c_ig)
        plot.igraph(ig1,layout=grid,vertex.label.cex = 1, edge.arrow.size = 0.5,vertex.frame.color=NA,
                    vertex.shape = "rectangle",edge.lty=1,
                    edge.curved=.5,
                    vertex.size=max(strwidth(label)* 275),
                    vertex.size2=0.025 *1000,asp=0,
                    edge.width=2,
                    mark.groups = sapply(V(ig)[traj] %>% as.numeric,list),mark.col = c(m1[2],m2[2],m3[2],m4[2]),mark.border = NA,
                    vertex.label=label,mark.shape = 1,mark.expand = max((strwidth(label)* 40))
                    
        );legend("topleft",legend=c("Primeiro Semestre", "Segundo Semestre","Terceiro Semestre","Quarto Semestre"),
                 col=c(m1[2],m2[2],m3[2],m4[2]), pch = 19, cex=1.2,pt.cex=2,
                 title="Matérias Escolhidas Automaticamente", text.font=20, bg='white')}
      
      return(funplot2(input$curso,input$m1,input$m2))
      }
    
    if (input$m1!=""& input$m2!=""& input$m3!=""& input$m4==""){
      
      fun_cor3 <- function(x,y,z,a){
        V(x)[y]$color <- m1[2]
        E(x)[paste0(y,"|",z)]$color <-m1[2]
        V(x)[y]$frame.color = "red"
        V(x)[z]$frame.color = "red"
        V(x)[a]$frame.color = "red"
        V(x)[z]$color <-m2[2]
        
      if(construir_tronco(input$curso) %>% filter(Nome==a,nm==z) %>% nrow==0){
        E(x)[paste0(y,"|",a)]$color <-m2[2]
      }else E(x)[paste0(z,"|",a)]$color <-m2[2]

       
        V(x)[a]$color <- m3[2]
        E(x)[incident(x, a, mode = c('out'))]$color <-m3[2]
        V(x)[neighbors(x, a, mode = c('out'))]$color <- "red"
        return(x)}
      funplot3 <- function(x,y,z,a){
        c_ig<-  curso_igraph(x) 
        ig <-ig_fun(x) 
        ig <- delete.vertices(simplify(ig), igraph::degree(ig)==0)
        V(ig)$color <- adjustcolor("lightgrey")
        E(ig)$color<- adjustcolor("white")
        E(ig)$weight <- 0
        label=V(ig)$name %>% str_wrap(22)
        V(ig)[traj]$color = NA
        E(ig)$color = NA
        ig1 <- fun_cor3(ig,y,z,a)
        grid <- grid_fun(ig1,c_ig)
        plot.igraph(ig1,layout=grid,vertex.label.cex = 1, edge.arrow.size = 0.25,vertex.frame.color=NA,
                    vertex.shape = "rectangle",edge.lty=1,
                    edge.curved=.5,
                    vertex.size=max(strwidth(label)* 275),
                    vertex.size2=0.025 *1000,asp=0,
                    edge.width=2,
                    mark.groups = sapply(V(ig)[traj] %>% as.numeric,list),mark.col = c(m1[2],m2[2],m3[2],m4[2]),mark.border = NA,
                    vertex.label=label,mark.shape = 1,mark.expand = max((strwidth(label)* 40))
                    
        );legend("topleft",legend=c("Primeiro Semestre", "Segundo Semestre","Terceiro Semestre","Quarto Semestre"),
                 col=c(m1[2],m2[2],m3[2],m4[2]), pch = 19, cex=1.2,pt.cex=2,
                 title="Matérias Escolhidas Automaticamente", text.font=20, bg='white')}
      return(funplot3(input$curso,input$m1,input$m2,input$m3))
    }    
    
    
    
    if (input$m1!=""& input$m2!=""& input$m3!=""& input$m4!=""){
      fun_cor3 <- function(x,y,z,a,b){
        V(x)[y]$color <- m1[2]
        V(x)[y]$frame.color = "red"
        E(x)[paste0(y,"|",z)]$color <-m1[2]
        
        V(x)[z]$color <-m2[2]
        V(x)[y]$frame.color = "red"
        if(construir_tronco(input$curso) %>% filter(Nome==a,nm==z) %>% nrow!=0){
          E(x)[paste0(z,"|",a)]$color <-m2[2]
        }else  E(x)[paste0(b,"|",a)]$color <-m2[2]
        if(construir_tronco(input$curso) %>% filter(Nome==b,nm==a) %>% nrow!=0){
          E(x)[paste0(a,"|",b)]$color <-m2[2]
        }else if(construir_tronco(input$curso) %>% filter(Nome==b,nm==z) %>% nrow!=0){
          E(x)[paste0(z,"|",b)]$color<-m2[2]
        }else{E(x)[paste0(y,"|",b)]$color <- m3[2]}
        V(x)[a]$color <- m3[2]
        V(x)[z]$color <-m2[2]
       V(x)[b]$color <-m4[2]
       V(x)[a]$frame.color = "red"
       V(x)[z]$frame.color = "red"
       V(x)[b]$frame.color = "red"
       ################ Saídas Última matérias ##################### 
        #E(x)[incident(x, b, mode = c('out'))]$color <- m4[2]
        #V(x)[neighbors(x, b, mode = c('out'))]$color <- "red"
        
        
        return(x)}
      funplot4 <- function(x,y,z,a,b){
        c_ig<-  curso_igraph(x) 
        ig <-ig_fun(x) 
        ig <- delete.vertices(simplify(ig), igraph::degree(ig)==0)
        V(ig)$color <- adjustcolor("lightgrey")
        E(ig)$color<- adjustcolor("white")
        E(ig)$weight <- 3
        label=V(ig)$name %>% str_wrap(22)
        E(ig)$color = NA
        ig1 <- fun_cor3(ig,y,z,a,b)
        grid <- grid_fun(ig1,c_ig)
        plot.igraph(ig1,layout=grid,vertex.label.cex = 1, edge.arrow.size = 0.25,vertex.frame.color=NA,
                    vertex.shape = "rectangle",edge.lty=1,
                    edge.curved=.5,
                    vertex.size=max(strwidth(label)* 275),
                    vertex.size2=0.025 *1000,asp=0,
                    edge.width=2,
                    mark.groups = sapply(V(ig)[traj] %>% as.numeric,list),mark.col = c(m1[2],m2[2],m3[2],m4[2]),mark.border = NA,
                    vertex.label=label,mark.shape = 1,mark.expand = max((strwidth(label)* 40))
                    
        );legend("topleft",legend=c("Primeiro Semestre", "Segundo Semestre","Terceiro Semestre","Quarto Semestre"),
                 col=c(m1[2],m2[2],m3[2],m4[2]), pch = 19, cex=1.2,pt.cex=2,
                 title="Matérias Escolhidas Automaticamente", text.font=20, bg='white')}
      return(funplot4(input$curso,input$m1,input$m2,input$m3,input$m4))
    } 
    

    

}
  })


      
      output$fluxograma <- renderPlot(req({fluxograma_plot()}))
 
  
  # ##########  Primeira Matéria ##########
  # observe({
  #   if (input$curso!="") { 
  #     materia1 <-construir_tronco(input$curso) %>% dplyr::filter(Semestre ==1 | (Semestre==2 & is.na(nm)))  %>% select(Nome) %>% unlist %>% as.character()
  #     updateSelectizeInput(session, "m1",choices=materia1,        
  #                          selected="")}
  # 
  # })
  # 
  #     ##########  Segunda Matéria ##########
  # observe({
  #  
  #     materia2 <-  construir_tronco(input$curso) %>% dplyr::filter(nm %in% input$m1 & Semestre %in% 2|Semestre %in% 2) %>% select(Nome)  %>% unlist %>% as.character()
  #     updateSelectizeInput(session, "m2",
  #                          choices =materia2 ,selected="")})
  # 
  # ##########  Terceira Matéria ##########  
  # observe({
  #   materia3 <-  construir_tronco(input$curso) %>% dplyr::filter(nm %in% input$m2 & Semestre %in% 3 |Semestre %in% 3) %>% select(Nome)  %>% unlist %>% as.character()
  #   updateSelectizeInput(session, "m3",
  #                        choices =materia3  
  #                        ,selected="") 
  #   
  # })
  # 
  # ##########  Quarta Matéria ##########  
  # observe({
  #   materia4 <-  construir_tronco(input$curso) %>% dplyr::filter((nm %in% input$m3& Semestre %in% 4|Semestre %in% 4)|
  #                                                                  (nm %in% input$m1 & Semestre %in% 4)|
  #                                                                  (nm %in% input$m2 & Semestre %in% 4)
  #                                                                  ) %>% select(Nome)  %>% unlist %>% as.character()
  #   updateSelectizeInput(session, "m4",
  #                        choices =materia4 ,selected="") 
  #   
  # })
  # 
  # ##########  Quinta matéria ##########
  # observe({
  #   materia5 <-  construir_tronco(input$curso) %>% dplyr::filter(nm %in% input$m4& Semestre %in% 3:6) %>% select(Nome)  %>% unlist %>% as.character()
  #   updateSelectizeInput(session, "m5",
  #                        choices =materia5 ,selected="") 
  # })
  # 
      


################################################################## Clusters ###############################
     
      info <- reactiveValues()
      modelos <- reactiveValues()
       clusters_plot <- reactive({
      

        if(input$curso!="" & input$m1!=""& input$m2!=""& input$m3!=""& input$m4!=""& input$gerar_clusters=="Sim"){
          
          #dados_cluster = preparo_db(input$curso, input$m1, input$m2, input$m3, input$m4,qtdclusters = input$qtdclusters)

          # info$traj = c(input$m1, input$m2, input$m3, input$m4)
          # info$data=dados_cluster$data
          # info$resposta=dados_cluster$resposta
          # list(dados_cluster,
          #      plot=dados_cluster$plot)
          }else if(input$curso!=""& input$gerar_clusters=="Sim"& input$m4 ==""){
                 traj = req(informacoes$traj)
         # dados_cluster = preparo_db(input$curso,traj[1],traj[2],traj[3],traj[4],qtdclusters = input$qtdclusters)
                 dados_cluster=readRDS("preparodb_save.rds")
          info$traj =  req(traj)
          info$data=req(dados_cluster$data)
          info$resposta=req(dados_cluster$resposta)
          info$agrupamento = req(dados_cluster$Agrupamento)
          list(dados_cluster=req(dados_cluster),
               plot=req(dados_cluster$plot),traj=req(traj))
        }
    
      })
      
      output$clusters <- renderPlotly({clusters_plot()$plot %>% ggplotly %>% layout(margin=list(l=150,r=150,t=150,b=150))})
      outputOptions(output, "clusters", suspendWhenHidden = FALSE)
      output$clusters2 <- renderPlotly({clusters_plot()$plot %>% ggplotly %>% layout(margin=list(l=150,r=150,t=150,b=150))})
      outputOptions(output, "clusters2", suspendWhenHidden = FALSE)
observe({
      summary <-  req(info$agrupamento) %>% filter(!is.na(Grupo)) 
     
       bug <- req(info$agrupamento)%>% group_by(CPFEncrypted,StatusFinal) %>% dplyr::summarize(n=n()) %>% filter(n>1) %>% group_by(CPFEncrypted) %>% dplyr::summarize(n=n()) %>% filter(n>1) %>% 
        select(CPFEncrypted) %>% unlist %>% as.character()
       summary <-  req(info$agrupamento) %>% filter(!is.na(Grupo)) %>% filter(!(CPFEncrypted %in% bug))
      m1 <- summary %>% group_by(Grupo,CPFEncrypted) %>% select(Grupo,CPFEncrypted,TempoPermanencia) %>% unique %>% ungroup %>% group_by(Grupo) %>% dplyr::summarise(Tempo=mean(TempoPermanencia))
      m2 <- summary %>% group_by(Grupo) %>% dplyr::count(Conceito)  %>% group_by(Grupo) %>%
        mutate(Frequência = n / sum(n)) %>% select(Grupo,Conceito,Frequência) %>% spread(Conceito,Frequência) 
      m3 <- summary %>% group_by(Grupo,CPFEncrypted) %>% select(Grupo,CPFEncrypted,StatusFinal) %>% unique %>% ungroup %>% group_by(Grupo) %>% dplyr::count(StatusFinal) %>% select(Grupo,StatusFinal)
      estatisticas <- join_all(list(m1,m2,m3),by="Grupo") %>% select(Grupo,StatusFinal,Tempo,Inconcluído,MM,MS,SS) %>% `names<-`(NULL)
      info$estatisticas = estatisticas
      output$estatisticas <-DT::renderDataTable(
        DT::datatable(estatisticas,options = list(pageLength = 5,paging = FALSE,searchHighlight = TRUE,columnDefs = list(list(className = 'dt-center',target=1:8))),
                      colnames=c("Grupo","Estado Final","Tempo Médio até estado final (Em Semestres)","Percentual de Inconcluído","Percentual de MM","Percentual de MS",
                                                                                                                                "Percentual de SS")))})
      
      ################################              Classificador                          ###################
      NN <- reactive({
      
        if(input$treinar=="Sim"){
        
        tronco <- req(info$traj)

          dados_keras_cat <- req(info$data) %>%sapply(as.character) %>%  data.frame(stringsAsFactors = FALSE)

          resposta_keras <-req(info$resposta) %>% fct_recode("0"="Cluster 1","1"="Cluster 2","2"="Cluster 3","3"="Cluster 4") %>%
             as.character() %>% as.numeric()
           mtraj1 <- paste(tronco[1],c("Inconcluído","MM","MS","SS"))
           mtraj2 <- paste(tronco[2],c("Inconcluído","MM","MS","SS"))
           mtraj3 <- paste(tronco[3],c("Inconcluído","MM","MS","SS"))
           mtraj4 <- paste(tronco[4],c("Inconcluído","MM","MS","SS"))
           materias_traj <- c(mtraj1,mtraj2,mtraj3,mtraj4)
           pesos <- seq(0,4,0.25)
            for(i in 1:16){
              dados_keras_cat<- apply(dados_keras_cat,2,function(x){gsub(materias_traj[i],pesos[i],x)})}

              dados_keras_cat <- apply(dados_keras_cat,2,function(x){gsub("(NÃO\\sFORMATURA)","-10",x)})
                dados_keras_cat<- apply(dados_keras_cat,2,function(x){gsub("(FORMATURA)","10",x)})
                dados_keras_cat<- apply(dados_keras_cat,2,function(x){gsub("CADEIA FINALIZADA","7.5",x)})

           dados_keras <- dados_keras_cat %>% apply(2,as.numeric)%>%  as.matrix

           colnames(dados_keras) <- NULL

        primeiro_semestre <- dados_keras[,1] ;
        segundo_semestre <- dados_keras[,1:2] ;
        terceiro_semestre <- dados_keras[,1:3] ;
        quarto_semestre <- dados_keras[,1:4] ;
        quinto_semestre <- dados_keras[,1:5] ;
        sexto_semestre <- dados_keras[,1:6] ;
        setimo_semestre <- dados_keras[,1:7] ;
        oitavo_semestre <- dados_keras[,1:8] ;
      #
      #   set.seed(2018)
      #   ############################################# Primeiro semestre ################################################
      #   modelo_1 <-keras_model_sequential() %>% 
      #     layer_dense(units = 4, activation = 'sigmoid', input_shape = c(1)) %>% 
      #     layer_dense(units = 4, activation = 'tanh') %>%
      #     layer_dense(units = 4, activation = 'tanh') %>%
      #     layer_dense(units = input$qtdclusters, activation = 'softmax')%>% compile(loss = 'categorical_crossentropy',
      #                                                               optimizer = optimizer_rmsprop(lr = 0.01),
      #                                                               metrics = c('accuracy'))
      #   fit.modelo_1 <- modelo_1 %>% fit(
      #     primeiro_semestre  , to_categorical(resposta_keras,input$qtdclusters), 
      #     epochs = 100, batch_size = 32,validation_split=0.2)
      #   
      #   ############################################# Segundo semestre ################################################ 
      #   modelo_2 <-keras_model_sequential() %>% 
      #     layer_dense(units = 6, activation = 'sigmoid', input_shape = c(2)) %>% 
      #     layer_dense(units = 4, activation = 'tanh') %>%
      #     layer_dense(units = 4, activation = 'tanh') %>%
      #     layer_dense(units = input$qtdclusters, activation = 'softmax')%>% compile(loss = 'categorical_crossentropy',
      #                                                               optimizer = optimizer_rmsprop(lr = 0.01),
      #                                                               metrics = c('accuracy'))
      #   fit.modelo_2 <- modelo_2 %>% fit(
      #     segundo_semestre  , to_categorical(resposta_keras,input$qtdclusters), 
      #     epochs = 100, batch_size = 32,validation_split=0.2)
      #   
      #   ############################################# Terceiro semestre ################################################
      #   modelo_3 <-keras_model_sequential() %>% 
      #     layer_dense(units = 6, activation = 'sigmoid', input_shape = c(3)) %>% 
      #     layer_dense(units = 6, activation = 'tanh') %>%
      #     layer_dense(units = 4, activation = 'tanh') %>%
      #     layer_dense(units = input$qtdclusters, activation = 'softmax')%>% compile(loss = 'categorical_crossentropy',
      #                                                               optimizer = optimizer_rmsprop(lr = 0.01),
      #                                                               metrics = c('accuracy'))
      #   fit.modelo_3 <- modelo_3 %>% fit(
      #     terceiro_semestre  , to_categorical(resposta_keras,input$qtdclusters), 
      #     epochs = 100, batch_size = 32,validation_split=0.2)
      #   
      #   ############################################# Quarto semestre ################################################
      #   modelo_4 <-keras_model_sequential() %>% 
      #     layer_dense(units = 6, activation = 'sigmoid', input_shape = c(4)) %>% 
      #     layer_dense(units = 6, activation = 'tanh') %>%
      #     layer_dense(units = 6, activation = 'tanh') %>%
      #     layer_dense(units = input$qtdclusters, activation = 'softmax')%>% compile(loss = 'categorical_crossentropy',
      #                                                               optimizer = optimizer_rmsprop(lr = 0.01),
      #                                                               metrics = c('accuracy'))
      # 
      # fit.modelo_4 <- modelo_4 %>% fit(
      #   quarto_semestre  , to_categorical(resposta_keras,input$qtdclusters), 
      #   epochs = 100, batch_size = 32,validation_split=0.2)
      # 
      #   
      #   ############################################# Quinto semestre ################################################
      #   modelo_5 <-keras_model_sequential() %>% 
      #     layer_dense(units = 8, activation = 'sigmoid', input_shape = c(5)) %>% 
      #     layer_dense(units = 6, activation = 'tanh') %>%
      #   layer_dense(units = 6, activation = 'tanh') %>%
      #     layer_dense(units = input$qtdclusters, activation = 'softmax')%>% compile(loss = 'categorical_crossentropy',
      #                                                               optimizer = optimizer_rmsprop(lr = 0.01),
      #                                                               metrics = c('accuracy'))
      # fit.modelo_5 <- modelo_5 %>% fit(
      #   quinto_semestre  , to_categorical(resposta_keras,input$qtdclusters), 
      #   epochs = 100, batch_size = 32,validation_split=0.2)
      #   
      #   ############################################# Sexto semestre ################################################
      #   modelo_6 <-keras_model_sequential() %>% 
      #     layer_dense(units = 8, activation = 'sigmoid', input_shape = c(6)) %>% 
      #     layer_dense(units = 8, activation = 'tanh') %>%
      #   layer_dense(units = 6, activation = 'tanh') %>%
      #     layer_dense(units = input$qtdclusters, activation = 'softmax')%>% compile(loss = 'categorical_crossentropy',
      #                                                               optimizer = optimizer_rmsprop(lr = 0.01),
      #                                                               metrics = c('accuracy'))
      # fit.modelo_6 <- modelo_6 %>% fit(
      #   sexto_semestre  , to_categorical(resposta_keras,input$qtdclusters), 
      #   epochs = 100, batch_size = 32,validation_split=0.2)
      #   
      #   ############################################# Sétimo semestre ################################################
      #   modelo_7 <-keras_model_sequential() %>% 
      #     layer_dense(units = 8, activation = 'sigmoid', input_shape = c(7)) %>% 
      #     layer_dense(units = 8, activation = 'tanh') %>%
      #   layer_dense(units = 8, activation = 'tanh') %>%
      #     layer_dense(units = input$qtdclusters, activation = 'softmax')%>% compile(loss = 'categorical_crossentropy',
      #                                                               optimizer = optimizer_rmsprop(lr = 0.01),
      #                                                               metrics = c('accuracy'))
      # fit.modelo_7 <- modelo_7 %>% fit(
      #   setimo_semestre  , to_categorical(resposta_keras,input$qtdclusters), 
      #   epochs = 100, batch_size = 32,validation_split=0.2)
      #   ############################################# Oitavo semestre ################################################
      #   modelo_8 <-keras_model_sequential() %>% 
      #     layer_dense(units = 10, activation = 'sigmoid', input_shape = c(8)) %>% 
      #     layer_dense(units = 8, activation = 'tanh') %>%
      #   layer_dense(units = 8, activation = 'tanh') %>%
      #     layer_dense(units = input$qtdclusters, activation = 'softmax')%>% compile(loss = 'categorical_crossentropy',
      #                                                               optimizer = optimizer_rmsprop(lr = 0.01),
      #                                                               metrics = c('accuracy'))
      # fit.modelo_8 <- modelo_8 %>% fit(
      #   oitavo_semestre  , to_categorical(resposta_keras,input$qtdclusters), 
      #   epochs = 200, batch_size = 32,validation_split=0.2)
      
      modelos$modelo1 <- modelo_1 <- load_model_hdf5("modelo_1.h5")
      modelos$modelo2 <- modelo_2 <- load_model_hdf5("modelo_2.h5")
      modelos$modelo3 <- modelo_3 <- load_model_hdf5("modelo_3.h5")
      modelos$modelo4 <- modelo_4 <- load_model_hdf5("modelo_4.h5")
      modelos$modelo5 <- modelo_5 <- load_model_hdf5("modelo_5.h5")
      modelos$modelo6 <- modelo_6 <- load_model_hdf5("modelo_6.h5")
      modelos$modelo7 <- modelo_7 <- load_model_hdf5("modelo_7.h5")
      modelos$modelo8 <- modelo_8 <- load_model_hdf5("modelo_8.h5")
      

      fit.modelo_1<- readRDS("fit.modelo_1.rds")
      fit.modelo_2<- readRDS("fit.modelo_2.rds")
      fit.modelo_3<- readRDS("fit.modelo_3.rds")
      fit.modelo_4<- readRDS("fit.modelo_4.rds")
      fit.modelo_5<- readRDS("fit.modelo_5.rds")
      fit.modelo_6<- readRDS("fit.modelo_6.rds")
      fit.modelo_7<- readRDS("fit.modelo_7.rds")
      fit.modelo_8<- readRDS("fit.modelo_8.rds")

      
        acc1 <- fit.modelo_1$metrics$acc %>% tail(1) ;  val_acc1 <- fit.modelo_1$metrics$val_acc %>% tail(1)
        acc2 <- fit.modelo_2$metrics$acc %>% tail(1) ;  val_acc2 <- fit.modelo_2$metrics$val_acc %>% tail(1)
        acc3 <- fit.modelo_3$metrics$acc %>% tail(1) ;  val_acc3 <- fit.modelo_3$metrics$val_acc %>% tail(1)
        acc4 <- fit.modelo_4$metrics$acc %>% tail(1) ;  val_acc4 <- fit.modelo_4$metrics$val_acc %>% tail(1)
        acc5 <- fit.modelo_5$metrics$acc %>% tail(1) ;  val_acc5 <- fit.modelo_5$metrics$val_acc %>% tail(1)
        acc6 <- fit.modelo_6$metrics$acc %>% tail(1) ;  val_acc6 <- fit.modelo_6$metrics$val_acc %>% tail(1)
        acc7 <- fit.modelo_7$metrics$acc %>% tail(1) ;  val_acc7 <- fit.modelo_7$metrics$val_acc %>% tail(1)
        acc8 <- fit.modelo_8$metrics$acc %>% tail(1) ;  val_acc8 <- fit.modelo_8$metrics$val_acc %>% tail(1)
        
        tab <- data.frame("Semestre" = 1:8,"Precisão Treino"=c(acc1,acc2,acc3,acc4,acc5,acc6,acc7,acc8),
                          "Precisão Validação"=c(val_acc1,val_acc2,val_acc3,val_acc4,val_acc5,val_acc6,val_acc7,val_acc8))

        
          verdade    = resposta_keras %>% as.factor
          verdade %<>% fct_recode("Cluster 1"="0","Cluster 2"="1","Cluster 3"="2","Cluster 4"="3")
        
          matriz_confusao <-  function(modelo,dados){
            estimativa   = predict_classes(object = modelo, x = dados) %>% as.factor%>% mudar_niveis %>% 
              fct_recode("Cluster 1"="0","Cluster 2"="1","Cluster 3"="2","Cluster 4"="3")
            class_prob = predict_classes(object = modelo, x = dados)
            
            tabela_estimativas<- tibble(
              verdade,    
              estimativa,   
              class_prob  %>% list)
            
            # cm  <-  conf_mat(tabela_estimativas,verdade,estimativa) %>% `[[`(1) %>% 
            #   data.frame %>% spread(Truth,Freq) %>% `colnames<-`(c(" ",1:input$qtdclusters))%>% 
            #   `rownames<-`(c(1:input$qtdclusters)) %>% mutate(" "=1:input$qtdclusters)
            
            cm  <-  conf_mat(tabela_estimativas,verdade,estimativa) %>% `[[`(1) %>%
              data.frame %>% spread(Truth,Freq) %>% `colnames<-`(c(" ",1:2))%>%
              `rownames<-`(c(1:2)) %>% mutate(" "=1:2)
            return(cm)
          }

          
          grafico_plotly <- function(fit.modelo){
          modelo <- fit.modelo %>% data.frame %>% filter(metric=="acc") %>% mutate(data=data %>% fct_recode("Validação"="validation","Treinamento"="training"))
          f1 <- list(
            family = "Arial, sans-serif",
            size = 20
          )
          eixoy <- list(
            autotick = FALSE,
            ticks = "outside",
            tick0 = 0,
            dtick = 0.1,
            ticklen = 5,
            tickwidth = 2,
            range=c(0,1),
            nticks=10,
            title="Acurácia",
            titlefont=f1,
            tickcolor = toRGB("blue")
          )
          eixox <- list(
            ticks = "outside",
            tick0 = 0,
            ticklen = 5,
            tickwidth = 2,
            title="Ciclo de aprendizado",
            titlefont=f1,
            tickcolor = toRGB("blue")
          )
          legenda <- list(
            font = list(
              family = "sans-serif",
              size = 25),
            bgcolor = "#E2E2E2",
            bordercolor = "#FFFFFF",
            borderwidth = 2)
          
          p <- plot_ly(data=modelo,x=~epoch,y=~value,color=~data,type="scatter",sizes=100,mode="lines+markers",colors=c("red","blue")) %>%
            layout(yaxis=eixoy,legend=legenda,xaxis=eixox)
          return(p)
          }
          
          
          mc1 <- matriz_confusao(modelo_1,primeiro_semestre) 
          mc2 <- matriz_confusao(modelo_2,segundo_semestre)
          mc3 <- matriz_confusao(modelo_3,terceiro_semestre)
          mc4 <- matriz_confusao(modelo_4,quarto_semestre)
          mc5 <- matriz_confusao(modelo_5,quinto_semestre)
          mc6 <- matriz_confusao(modelo_6,sexto_semestre)
          mc7 <- matriz_confusao(modelo_7,setimo_semestre)
          mc8 <- matriz_confusao(modelo_8,oitavo_semestre)
        
        return(list(tab=tab,
                    confmat1=mc1,
                    confmat2=mc2,
                    confmat3=mc3,
                    confmat4=mc4,
                    confmat5=mc5,
                    confmat6=mc6,
                    confmat7=mc7,
                    confmat8=mc8,
                  p1 =grafico_plotly(fit.modelo_1)  ,
                  p2 =grafico_plotly(fit.modelo_2),
                  p3 =grafico_plotly(fit.modelo_3),
                  p4 =grafico_plotly(fit.modelo_4),
                  p5 =grafico_plotly(fit.modelo_5),
                  p6 =grafico_plotly(fit.modelo_6),
                  p7 =grafico_plotly(fit.modelo_7 ),
                  p8 =grafico_plotly(fit.modelo_8 )
                  
                    
                    ))
        }
        
      })
      output$rede <- renderTable({(NN()$tab)})
      
      output$cm1 <- function(){NN()$confmat1 %>% kable(aling="c",caption="Matriz de confusão para o primeiro semestre",format = "html") %>% 
          add_header_above(c(" ", "Cluster Observado"=input$qtdclusters)) %>% collapse_rows(columns = 1, valign = "top")%>%
          kable_styling("striped", full_width = F) %>% column_spec(1, bold = T)}
     
       output$cm2 <- function(){NN()$confmat2 %>% kable(aling="c",caption="Matriz de confusão para o segundo semestre",format = "html") %>% 
          add_header_above(c(" ", "Cluster Observado"=input$qtdclusters)) %>% collapse_rows(columns = 1, valign = "top")%>%
          kable_styling("striped", full_width = F) %>% column_spec(1, bold = T)}

       output$cm3 <- function(){NN()$confmat3 %>% kable(aling="c",caption="Matriz de confusão para o terceiro semestre",format = "html") %>% 
           add_header_above(c(" ", "Cluster Observado"=input$qtdclusters)) %>% collapse_rows(columns = 1, valign = "top")%>%
           kable_styling("striped", full_width = F) %>% column_spec(1, bold = T)}
       
       output$cm4 <- function(){NN()$confmat4 %>% kable(aling="c",caption="Matriz de confusão para o quarto semestre",format = "html") %>% 
           add_header_above(c(" ", "Cluster Observado"=input$qtdclusters)) %>% collapse_rows(columns = 1, valign = "top")%>%
           kable_styling("striped", full_width = F) %>% column_spec(1, bold = T)}
       
       output$cm5 <- function(){NN()$confmat5 %>% kable(aling="c",caption="Matriz de confusão para o quinto semestre",format = "html") %>% 
           add_header_above(c(" ", "Cluster Observado"=input$qtdclusters)) %>% collapse_rows(columns = 1, valign = "top")%>%
           kable_styling("striped", full_width = F) %>% column_spec(1, bold = T)}
       
       output$cm6 <- function(){NN()$confmat6 %>% kable(aling="c",caption="Matriz de confusão para o sexto semestre",format = "html") %>% 
           add_header_above(c(" ", "Cluster Observado"=input$qtdclusters)) %>% collapse_rows(columns = 1, valign = "top")%>%
           kable_styling("striped", full_width = F) %>% column_spec(1, bold = T)}
       
       output$cm7 <- function(){NN()$confmat7 %>% kable(aling="c",caption="Matriz de confusão para o sétimo semestre",format = "html") %>% 
           add_header_above(c(" ", "Cluster Observado"=input$qtdclusters)) %>% collapse_rows(columns = 1, valign = "top")%>%
           kable_styling("striped", full_width = F) %>% column_spec(1, bold = T)}
       
          output$cm8 <- function(){NN()$confmat8 %>% kable(aling="c",caption="Matriz de confusão para o outubro semestre",format = "html") %>% 
           add_header_above(c(" ", "Cluster Observado"=input$qtdclusters)) %>% collapse_rows(columns = 1, valign = "top")%>%
           kable_styling("striped", full_width = F) %>% column_spec(1, bold = T)}
          
          output$plot1 <- renderPlotly(NN()$p1)
          output$plot2 <- renderPlotly(NN()$p2)
          output$plot3 <- renderPlotly(NN()$p3)
          output$plot4 <- renderPlotly(NN()$p4)
          output$plot5 <- renderPlotly(NN()$p5)
          output$plot6 <- renderPlotly(NN()$p6)
          output$plot7 <- renderPlotly(NN()$p7)
          output$plot8 <- renderPlotly(NN()$p8)
  ########## Leitura do PDF ##########
          arquivos <- reactiveValues(state=FALSE)
          
          
           
          observe({
   

      require(pdftools)
    lerpdf <- function(x){ 
      
      pretext <- x %>%  pdftools::pdf_text() %>% unlist()
      
      text <-x%>%  pdftools::pdf_text() %>% unlist()  %>% strsplit( "\n|\r\n") %>% unlist()
      text <-  text[-(which(text %>% str_detect("(CONCURSO VESTIBULAR)")):length(text))]
      codigos_materias <-text %>%  str_extract("\\s[:digit:]{6}\\s")
      habilitacao <- pretext %>% str_extract("(?=Habilitação).*") 
      habilitacao <- gsub("Habilitação","",habilitacao)
      habilitacao <- habilitacao[!is.na(habilitacao)] %>% trimws
      habilitacao <- iconv(habilitacao,from="UTF-8",to="ASCII//TRANSLIT")
      habilitacao <-gsub("*`*\\'*~*\\^*", "",habilitacao[[1]]) %>% toupper() %>% str_extract("[A-Z]+")
      
      anoingresso <- pretext %>% str_extract("((?<=Ingresso na UnB).*?(?=\\r))|((?<=Ingresso na UnB).*?(?=\\n))")
      anoingresso <-anoingresso[!is.na(anoingresso)][[1]] %>% trimws %>% str_extract("[0-9]{4}.[0-9]")
      
      matricula <- text %>%  str_extract("[:digit:]{2}(\\/)[:digit:]{7}") 
      matricula <- matricula[!is.na(matricula)][[1]]
      matricula <-  gsub("\\/","",matricula)
      mencoes <- text %>% str_extract("(\\sSR\\s|\\sII\\s|\\sMI\\s|\\sMM\\s|\\sMS\\s|\\sSS\\s|\\sCC\\s|\\sTR\\s|\\sTJ\\s|\\sDP\\s)")
      periodos <- text %>% str_extract("\\s[:digit:]{4}/[:digit:]{1}")
      AnoIngresso <- anoingresso
      Mencoes <- pmin(periodos,mencoes,na.rm=T)%>% trimws()
      Mencoes <- Mencoes[!is.na(Mencoes)]
      Mencoes <- Mencoes[which(Mencoes != AnoIngresso)]
      Mencoes <- append(AnoIngresso,Mencoes)
      marcadores_semestre_1 <-   data.frame(Sem=Mencoes[str_detect(Mencoes,"[/]")],
                                            Pos=which(str_detect(Mencoes,"[/]")),Duplo=  duplicated(Mencoes[str_detect(Mencoes,"[/]")]))
      rep_1 <- marcadores_semestre_1 %>% filter(Duplo == T) %>% select(Pos) %>% pull()
      if( length(rep_1) != 0){Mencoes <- Mencoes[-rep_1]}
      
      Materias <- pmin(periodos,codigos_materias,na.rm=T) %>% trimws()
      Materias <- Materias[!is.na(Materias)]
      Materias <- Materias[which(Materias != AnoIngresso)]
      Materias <- append(AnoIngresso,Materias)
      marcadores_semestre_2 <-   data.frame(Sem=Materias[str_detect(Materias,"[/]")],
                                            Pos=which(str_detect(Materias,"[/]")),Duplo=  duplicated(Materias[str_detect(Materias,"[/]")]))
      rep_2 <- marcadores_semestre_2 %>% filter(Duplo == T) %>% select(Pos) %>% pull() %>% as.numeric
      if( length(rep_2) != 0){Materias <- Materias[-rep_2]}
      
      
      coisa <- which(grepl("[0-9]{4}/",Materias)==1)
      coisa[length(coisa)+1] <- length(Materias)+1
      semestres <- length(which(grepl("[0-9]{4}/",Materias)==1))
      trajetoria <- list()
      for(i in 1:semestres){
        trajetoria[[i]] <- data.frame(Materias[(coisa[i]+1):(coisa[i+1]-1)],Materias[coisa[i]],check.names = F)}
      cod <- ldply(trajetoria, data.frame)
      names(cod) <- c("Código Matéria","Semestre")
      coisa1 <- which(grepl("[0-9]{4}/",Mencoes)==1)
      coisa1[length(coisa1)+1] <- length(Mencoes)+1
      hist <- list()
      for(i in 1:semestres){
        hist[[i]] <- data.frame(Mencoes[(coisa1[i]+1):(coisa1[i+1]-1)],Mencoes[coisa1[i]],check.names = F)}
      mencao <- ldply(hist, data.frame)
      names(mencao) <- c("Menção","Semestre")
      cod$Menção <- mencao[,1]
      cod$Menção <- gsub(" ","",cod$Menção)
      cod[,1] <- gsub(" ","",cod[,1])
      Materias_Todas = PreReq_Total %>% select(Nome,Cod) %>% unique
      codaux <-  dplyr::filter(cod, cod[,1] %in% Materias_Todas$Cod)
      
      nomecod <- NULL
      for( i in 1:length(codaux[,1])){
        nomecod[i] <- Materias_Todas$Nome[which(codaux[i,1]==unique(Materias_Todas$Cod))][1]
      }    
      codaux$Disciplina <- nomecod
      codaux <- codaux[,c(1,4,3,2)]
      codaux$Matrícula <- matricula
      codaux$Ingresso <- anoingresso
      codaux$Habilitação <-  habilitacao
      Ingresso_data =gsub("(/1)|(/0)$","-01-01",anoingresso)
      Ingresso_data = gsub("(/2)|$","-07-01",Ingresso_data)%>% as.Date
      semestre <- codaux$Semestre
      semestre <- gsub("(/1)|(/0)$","-06-30",semestre)
      semestre <- gsub("(/2)|$","-12-31",semestre)%>% as.Date
      Posmateria <- plyr::round_any(as.double(difftime(semestre,Ingresso_data))/365*2,1)
      codaux$`Período` <- Posmateria
      codaux$Código = cod$`Código Matéria`
      codaux <- codaux %>% select(Matrícula,Código,Disciplina,Menção,Período)
      return(codaux)
      
    }
          lista_pdfs <- list()
          
          for(i in 1:length( req(input$histinput)[,1])){
            lista_pdfs[[i]]<-lerpdf( input$histinput[[i, 'datapath']])
          }

          arquivos$historico <- bind_rows(lista_pdfs)
          arquivos$merge <- bind_rows(arquivos$historico,arquivos$db)
 
            
          
  }) 
  
          

  
  
      output$apendice <- DT::renderDataTable(
      DT::datatable(dados_curso,filter='top',options = list(pageLength = 134,paging = FALSE,searchHighlight = TRUE,
                                          columnDefs = list(list(targets =c(1:6), searchable = TRUE))
                                          ))
      )
      
      
      
      observe({
        
        lista_arquivos <- list()
            
            for(i in 1:length(req(input$dbinput)[,1])){
              lista_arquivos[[i]]<-read.csv( input$dbinput[[i, 'datapath']],fileEncoding = "UTF-8",stringsAsFactors = F)
            }
            arquivos$db <- bind_rows(lista_arquivos)
              
      arquivos$merge <- bind_rows(arquivos$historico,arquivos$db)
            
 

        
      })
      output$dbrds <-DT::renderDataTable(
        DT::datatable(arquivos$merge,options = list(pageLength = 10),filter="top"))
      


   
 
      
      
      
      
      
      
      observe ({

  trajetorias <- req(informacoes$traj)
  req(arquivos$merge)
  data_util <-  arquivos$merge %>% mutate(Matrícula = as.factor(Matrícula))
  data_util$Disciplina <- iconv(data_util$Disciplina,from="UTF-8",to="ASCII//TRANSLIT")
  data_util$Disciplina <- gsub("*`*\\'*~*\\^*", "",data_util$Disciplina) %>% toupper() %>% trimws()
  data_util$Menção <-  data_util$Menção %>% trimws()
  data_util$Menção <- gsub("TR|TJ|DP|SR|II|MI","Inconcluído",  data_util$Menção)
  data_util$Período <- data_util$Período %>% trimws 
  semestre_corrente <- data_util %>%  group_by(Matrícula) %>% filter(Período<9) %>% dplyr::summarize(Semestre=max(Período))
  semestre_corrente <-semestre_corrente$Semestre  %>% unlist %>% as.numeric
        data_util <- data_util  %>%filter(Disciplina %in%  trajetorias,Período<9)%>%  mutate(Materia_Mencao= paste(Disciplina,Menção)) %>% 
          select(Matrícula,Materia_Mencao,Período) %>% group_by(Matrícula,Período) %>%  mutate(i=1:n()) %>%  
          spread(key=Período,value=Materia_Mencao) %>% select(-i)
        Missing = setdiff(1:8 %>% as.character,colnames(data_util))
        data_util[Missing] <-  NA
        data_util <- data_util %>% select(Matrícula,1:8 %>% as.character)
        aux_2 <- list()
        for(i in 1:length(trajetorias))aux_2[[i]] <- paste0(trajetorias[i],c(" MM"," MS"," SS"))
        transformacao <- function(x){
          data_util=c(t(x))
          data_util[which(grepl(paste(aux_2[[1]],collapse = "|"),data_util) &                                                                                                           
                            is.na(data_util[which(grepl(paste(aux_2[[1]],collapse = "|"),data_util))+1]))+1] <- paste0(trajetorias[2]," Inconcluído")
          
          data_util[which(grepl(paste(aux_2[[2]],collapse = "|"),data_util) & 
                            is.na(data_util[which(grepl(paste(aux_2[[2]],collapse = "|"),data_util))+1]))+1] <- paste0(trajetorias[3]," Inconcluído")
          
          data_util[which(grepl(paste(aux_2[[3]],collapse = "|"),data_util) & 
                            is.na(data_util[which(grepl(paste(aux_2[[3]],collapse = "|"),data_util))+1]))+1] <- paste0(trajetorias[4]," Inconcluído")
          data_util[which(grepl(paste(aux_2[[4]],collapse = "|"),data_util) & 
                            is.na(data_util[which(grepl(paste(aux_2[[4]],collapse = "|"),data_util))+1]))+1] <-"CADEIA FINALIZADA"
          data_util[which(grepl("CADEIA FINALIZADA",data_util) & 
                            is.na(data_util[which(grepl("CADEIA FINALIZADA",data_util))+1]))+1] <-"CADEIA FINALIZADA"
  
          
          return(data_util)
        }


for(i in 1:nrow(data_util)){
  data_util[i,1:(semestre_corrente[i]+1)] <- data_util[i,1:(semestre_corrente[i]+1)] %>% transformacao
  
}
        
        for(i in 1:nrow(data_util)){
          data_util[i,1:(semestre_corrente[i]+1)] <- na.locf(c(t(data_util[i,1:(semestre_corrente[i]+1)])))
          
        }
        
        dados_resultado <- data_util %>% data.frame
        dados_resultado$Semestre= semestre_corrente
     names(dados_resultado) <- c("Matrícula","Semestre 1","Semestre 2","Semestre 3","Semestre 4"
                                 ,"Semestre 5","Semestre 6","Semestre 7","Semestre 8","Semestre")

     output$hist <-         DT::renderDataTable(
       DT::datatable(dados_resultado,options = list(pageLength = 5,paging = FALSE,searchHighlight = TRUE)))
     
        mtraj1 <- paste(trajetorias[1],c("Inconcluído","MM","MS","SS"))
        mtraj2 <- paste(trajetorias[2],c("Inconcluído","MM","MS","SS"))
        mtraj3 <- paste(trajetorias[3],c("Inconcluído","MM","MS","SS"))
        mtraj4 <- paste(trajetorias[4],c("Inconcluído","MM","MS","SS"))
        materias_traj <- c(mtraj1,mtraj2,mtraj3,mtraj4,"CADEIA FINALIZADA")
        pesos <- c(seq(0,3.75,0.25),5.25)
        
        dados_resultados_avaliacao <-dados_resultado
        for(i in 2:9){
          dados_resultados_avaliacao[,i] <-  mapvalues(dados_resultados_avaliacao[,i] %>% as.vector ,materias_traj,pesos)}
        avaliar <- function(x){
          avaliacao <- data.frame(matrix(ncol=(1+input$qtdclusters)))
          for(i in 1:nrow(x)){
            if(req(x$Semestre)[i] == 1){
              avaliacao[i,] <- c(req(x$Matrícula)[i],predict_proba(req(modelos$modelo1),x[i,2]%>% as.matrix(nrow=1) %>% `colnames<-`(NULL)))
              }else if(req(x$Semestre)[i] == 2){
                avaliacao[i,] <-  c(req(x$Matrícula)[i],predict_proba(req(modelos$modelo2),x[i,2:3]%>% as.matrix(nrow=1) %>% `colnames<-`(NULL)))
                }else if(req(x$Semestre)[i] == 3){
                  avaliacao[i,] <-  c(req(x$Matrícula)[i],predict_proba(req(modelos$modelo3),x[i,2:4]%>% as.matrix(nrow=1) %>% `colnames<-`(NULL)))
                  }else if(req(x$Semestre)[i] == 4){
                    avaliacao[i,] <-  c(req(x$Matrícula)[i], predict_proba(req(modelos$modelo4),x[i,2:5]%>% as.matrix(nrow=1) %>% `colnames<-`(NULL)))
                    }else if(req(x$Semestre)[i] == 5){
                      avaliacao[i,] <-  c(req(x$Matrícula)[i],predict_proba(req(modelos$modelo5),x[i,2:6]%>% as.matrix(nrow=1) %>% `colnames<-`(NULL)))
                      }else if(req(x$Semestre)[i] == 6){
                        avaliacao[i,] <-  c(req(x$Matrícula)[i],predict_proba(req(modelos$modelo6),x[i,2:7]%>% as.matrix(nrow=1) %>% `colnames<-`(NULL)))
                        }else if(req(x$Semestre)[i] == 7){
                          avaliacao[i,] <- c(req(x$Matrícula)[i], predict_proba(req(modelos$modelo7),x[i,2:8]%>% as.matrix(nrow=1) %>% `colnames<-`(NULL)))
                          }else if(req(x$Semestre)[i] == 8){
                            avaliacao[i,] <- c(req(x$Matrícula)[i], predict_proba(req(modelos$modelo8),x[i,2:9]%>% as.matrix %>% `colnames<-`(NULL)))
                          }}
          
          estatisticas <- info$estatisticas

          colnames(avaliacao) <- c("Matrícula",paste0(paste0("Probabilidade de ",estatisticas[,2]),paste0(" (Cluster ",1:input$qtdclusters,")")))
          avaliacao$Matrícula=dados_resultados_avaliacao$Matrícula
          return(avaliacao)
        }
    
        
  
        
        
 
        resultado = dados_resultados_avaliacao %>% avaliar
        output$result <-DT::renderDataTable(
          DT::datatable(resultado,filter='top',options = list(pageLength = 10)))
        
        output$downloadData <- downloadHandler(
          filename = function() {
            paste("resultado", ".csv", sep = "")
          },
          content = function(file) {
            write.csv(datasetInput(), file, row.names = FALSE)
          }
        )
        
        ##### Gráfico novo #################
      
        
        
        
      })
      
      
      
      
      
      
      
      
      funplot_concluidos <- function(x,y){
        historico <- x %>%  filter(Disciplina %in%  
                                     ((fluxograma1 %>% dplyr::filter(Curso %in% y) %>% select(Nome) %>% pull()))
                                   & (Menção %in% c("MM","MS","SS"))
        )
        historico <-left_join(historico,Prereq,by=c("Disciplina"="Nome")) %>%dplyr::rename(Código=Cod)
        c_ig<-curso_igraph(y) 
        ig <-ig_fun(y) 
        ig <- delete.vertices(simplify(ig), igraph::degree(ig)==0)
        curso <-   fluxograma1 %>% dplyr::filter(Curso %in% y)
        req <-dplyr::filter(Prereq,Cod %in% curso$Matéria )
        curso_1 <- left_join(curso,unique(req,by=c("Matéria"="Cod")))
        curso_igraph_list <- curso_1 %>% select(Matéria,Nome,PreReq,Semestre) 
        curso_igraph <- curso_igraph_list
        curso_igraph$PreReq <- curso_igraph$PreReq %>% str_extract_all("[0-9]{6}")
        curso_igraph$PreReq[which(lapply(curso_igraph$PreReq,length)==0)]=NA
        curso_igraph <- curso_igraph %>% unnest()
        sem_prereq <- curso_igraph %>% filter(is.na(PreReq))
        
        Faltantes <- curso_igraph_list %>% filter(!(Nome %in% historico$Disciplina))
        if(nrow(Faltantes)>0){
          Id <- list()
          for(i in 1:nrow(Faltantes)){
            
            Id[[i]] <- all(Faltantes %>% select(PreReq) %>% pull() %>% nth(i) %in% historico$Código) 
            Possiveis <- bind_rows(sem_prereq %>% filter(!(Nome %in% historico$Disciplina)), Faltantes[unlist(Id),] %>% mutate(PreReq =as.character(PreReq)))
            Nomes_Possíveis <- Possiveis %>% select(Nome) %>% pull()
          }}else{Nomes_Possíveis <- NA}     
        
        
        V(ig)$color <- adjustcolor("lightgrey")
        E(ig)$color<- NA
        E(ig)$weight <- 0
        grid <- grid_fun(ig,c_ig)
        V(ig)[historico$Disciplina]$color="green"
        label=V(ig)$name %>% str_wrap(22)
        if(!is.na(Nomes_Possíveis)){V(ig)[Nomes_Possíveis]$color="lightblue"}
       

        
        plot.igraph(ig,layout=grid,vertex.label.cex = 1, edge.arrow.size = 0.25,vertex.frame.color=NA,
                    vertex.shape = "rectangle",edge.lty=1,
                    edge.curved=.5,
                    vertex.size=max((strwidth(label)* 275))*0.8,
                    vertex.size2=0.025 *1000,asp=0,
                    edge.width=2,vertex.label=label
                    
        )
        ;legend("topleft",legend=c("Disciplinas Concluídas","Disciplinas Possíveis"),
                col=c("green","lightblue"), pch = 15, cex=1.2,pt.cex=2,
                title="Fluxograma do Aluno", text.font=20, bg='white')
        
        }

  
      output$fluxograma_aluno <- renderPlot(funplot_concluidos(req(arquivos$merge),req(informacoes$curso)))
   
      
}


                     


