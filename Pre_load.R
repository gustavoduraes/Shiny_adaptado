########### Arquivos salvos
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


trajetoria_save <-  algoritmo("BACHARELADO EM ESTATISTICA-1716")

saveRDS(trajetoria_save,"trajetoria_save.rds")
trajetoria_save

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

preparo_db <- function(a,b,c,d,e,qtdclusters=2){
  curso <- fluxograma1$nome_hab[fluxograma1$Curso %in% a] %>% nth(1) %>% as.character()
  grau_curso <- fluxograma1$grau_curso[fluxograma1$Curso %in% a] %>% nth(1) %>% as.character()
  db_curso <- dadosalunos %>%  dplyr::filter(Curso == curso| Curso ==grau_curso )  %>% group_by(CPFEncrypted) %>%  
    dplyr::filter(StatusFinal %in% c("NÃO FORMATURA","FORMATURA"),Conceito !="CC",!any(Posmateria<0),!any(TempoPermanencia>16)) %>% ungroup
  tronco <- c(b,c,d,e)
  aux <- list()
  for(i in 1:length(tronco))aux[[i]] <- paste0(tronco[i],c(" Inconcluído"," MM"," MS"," SS"))
  es <- aux %>% unlist
  estados <- c(es,"CADEIA FINALIZADA","NÃO FORMATURA","FORMATURA")
  tronco_curso = db_curso %>% dplyr::filter(Materia %in% tronco ) 
  filtro <- tronco_curso  %>% group_by(CPFEncrypted) %>%  
    dplyr::filter(StatusFinal %in% c("NÃO FORMATURA","FORMATURA"),Conceito !="CC",!any(Posmateria<0)) %>% ungroup
  
  curso_util <- filtro %>% select(CPFEncrypted,Materia,Conceito,Posmateria,TempoPermanencia,StatusFinal) %>% 
    mutate(Materia_Mencao = paste(Materia,Conceito),Materia = NULL , Conceito = NULL,Posmateria = factor(Posmateria),
           CPFEncrypted=factor(CPFEncrypted)) 
  
  
  curso_util$Materia_Mencao <- as.factor(curso_util$Materia_Mencao)
  curso_util$StatusFinal <- as.factor(curso_util$StatusFinal)
  
  curso_TraMiner = curso_util  %>% group_by(Posmateria,CPFEncrypted) %>%  mutate(i=1:n()) %>%  
    spread(key=Posmateria,value=Materia_Mencao) %>% select(-i)
  
  curso_TraMiner[as.character(c(1:20)[!1:20 %in%  names(curso_TraMiner)])]=NA
  
  
  curso_TraMiner <- curso_TraMiner %>%  select(CPFEncrypted,TempoPermanencia,StatusFinal,as.character(1:20))
  
  fator_1 <- function(x){
    if(is.factor(x)) return(factor(x, levels=estados))
    return(x)
  }
  curso_TraMiner[,4:23] <- as.data.frame(lapply(curso_TraMiner[4:23], fator_1),names=1:20 %>% as.character())
  Fim <- curso_TraMiner$TempoPermanencia
  for(i in 1:nrow(curso_TraMiner)){
    curso_TraMiner[i,as.character(Fim[i]:20)] = as.character(curso_TraMiner$StatusFinal[i])
  }
  Finalizado <- list()
  for(i in 1:nrow(curso_TraMiner)){
    Finalizado[[i]] <- which(curso_TraMiner[i,] == paste0(e," MM")|
                               curso_TraMiner[i,] == paste0(e," MS")|
                               curso_TraMiner[i,] == paste0(e," SS")
    )
  }
  Nao_formados <- (which(plyr::ldply(Finalizado,length)[[1]]==0))
  
  concluido <- NULL
  for(i in 1:length(Nao_formados)){
    Finalizado[[Nao_formados[i]]]=NA
  }
  
  cadeia_finalizada <- unlist(Finalizado)-2
  for(i in 1:nrow(curso_TraMiner)){
    if(!is.na(cadeia_finalizada)[i])
      curso_TraMiner[i,cadeia_finalizada[i]:(Fim[i]-1) %>% as.character()]="CADEIA FINALIZADA"}
  
  dados <- as.data.frame(lapply(curso_TraMiner[4:23],as.factor))
  
  
  dados <- as.data.frame(lapply(dados,fator_1))
  curso_TraMiner[,4:23] <- dados
  ############# Número de observações indeterminadas
  nas <- NULL
  for(i in 1:nrow(curso_TraMiner)){
    nas[i] <- any(!is.na(curso_TraMiner[,4:5][i,])) 
  }
  
  
  curso_TraMiner_limpo <- curso_TraMiner[nas,] %>% data.frame
  names(curso_TraMiner_limpo)[4:23] <- 1:20
  curso_TraMiner_limpo <- curso_TraMiner_limpo[-which(is.na(curso_TraMiner_limpo[,4])),]
  aux_2 <- list()
  for(i in 1:length(tronco))aux_2[[i]] <- paste0(tronco[i],c(" MM"," MS"," SS"))
  aux_2
  transformacao <- function(x){
    data_util=c(t(x))
    
    
    data_util[which(grepl(paste(aux_2[[1]],collapse = "|"),data_util) &                                                                                                           
                      is.na(data_util[which(grepl(paste(aux_2[[1]],collapse = "|"),data_util))+1]))+1] <- paste0(tronco[2]," Inconcluído")
    
    data_util[which(grepl(paste(aux_2[[2]],collapse = "|"),data_util) & 
                      is.na(data_util[which(grepl(paste(aux_2[[2]],collapse = "|"),data_util))+1]))+1] <- paste0(tronco[3]," Inconcluído")
    
    data_util[which(grepl(paste(aux_2[[3]],collapse = "|"),data_util) & 
                      is.na(data_util[which(grepl(paste(aux_2[[3]],collapse = "|"),data_util))+1]))+1] <- paste0(tronco[4]," Inconcluído")
    
    
    
    
    return(data_util)
  }
  
  for(i in 1:nrow(curso_TraMiner_limpo)){
    curso_TraMiner_limpo[i,] <-  transformacao(curso_TraMiner_limpo[i,])
  }
  for(i in 1:nrow(curso_TraMiner_limpo)){
    curso_TraMiner_limpo[i,] <-  na.locf(c(t(curso_TraMiner_limpo[i,])))
  }
  
  
  
  m1 <- brewer.pal(n = 8, name = "Blues")[3:6]
  m2 <- brewer.pal(n = 8, name = "Greens")[3:6]
  m3 <- brewer.pal(n = 8, name = "Oranges")[3:6]
  m4 <- brewer.pal(n = 8, name = "Purples")[3:6]
  #m5 <- c("yellow","yellow2","yellow3","yellow4")
  colors <- c(m1,m2,m3,m4,"navy","firebrick","lawngreen")
  curso_seq <- seqdef(curso_TraMiner_limpo[,4:23],cpal=colors,alphabet = estados)
  ########################## Matriz de custos para o agrupamento ##################
  
  
  custos_mat <- matriz_custos()
  
  dist<- seqdist(curso_seq, method="OM", indel=0.0001,sm= custos_mat) 
  clusterward1 <- agnes(dist, diss=TRUE, method="ward")
  clusters <- cutree(clusterward1, k=qtdclusters)
  clusters_fac <- factor(clusters, labels =  paste("Cluster" ,1:qtdclusters )) 
  
  require(data.table)
  dados_gg <- data.table(curso_TraMiner_limpo[,4:23],clusters_fac)
  dados_gather <-dados_gg %>%  gather(Semestre,Materia,-clusters_fac) %>% mutate(Semestre=Semestre %>% as.numeric)
  Frequência <- dados_gather %>% group_by(Semestre,Materia,clusters_fac) %>% dplyr::summarise(n=n())
  tamanho_clusters <- dados_gather %>% group_by(clusters_fac) %>% dplyr::summarise(nc=n()/20) 
  freq_merge <- left_join(Frequência,tamanho_clusters,by="clusters_fac") %>% mutate(Frequência=n/nc,n=NULL,nc=NULL) 
  freq_merge$Materia <- factor(freq_merge$Materia,levels=estados)
  
  
  
  freq_merge$Semestre <- factor(freq_merge$Semestre,levels=1:20)
  labels_clusters <- paste0("Cluster " ,1:qtdclusters ,"\n n=",tamanho_clusters$nc, "\n Porcentagem do total = ",100*round(tamanho_clusters$nc/sum(tamanho_clusters$nc),4),"%" )
  freq_merge$clusters_fac <- factor(freq_merge$clusters_fac,labels=labels_clusters)
  
  
  p <- ggplot(freq_merge,aes(Semestre,Frequência,fill=Materia))+geom_bar(stat="identity")+
    facet_wrap(~clusters_fac,scales="free")+ scale_fill_manual(values=colors,drop=F)+
    guides(fill=guide_legend(ncol=1))+
    xlab("Semestre")+ylab(paste0("Total =",sum(tamanho_clusters$nc)))+
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank()
          
    )
  resumos <-  curso_TraMiner_limpo %>% mutate(Grupo = clusters_fac)
  
  db_summary <- inner_join(resumos %>% select(CPFEncrypted,Grupo),db_curso,by="CPFEncrypted")
  
  return(list(plot=p,data=curso_TraMiner_limpo[,4:23],data_completo=curso_TraMiner_limpo,resposta=clusters_fac,Agrupamento=db_summary))
  
}

preparodb_save <- preparo_db("BACHARELADO EM ESTATISTICA-1716",trajetoria_save[1],trajetoria_save[2],trajetoria_save[3],trajetoria_save[4],2)

saveRDS(preparodb_save,"preparodb_save.rds")


############################################################
#################### Classificador #########################
############################################################

tronco <- trajetoria_save

dados_cluster=readRDS("preparodb_save.rds")
names(dados_cluster)
info$traj =  traj
info$data=dados_cluster$data
dados_keras_cat <-dados_cluster$data %>%sapply(as.character) %>%  data.frame(stringsAsFactors = FALSE)

resposta_keras <-dados_cluster$resposta %>% fct_recode("0"="Cluster 1","1"="Cluster 2","2"="Cluster 3","3"="Cluster 4") %>%
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

set.seed(2018)
############################################# Primeiro semestre ################################################
modelo_1 <-keras_model_sequential() %>% 
  layer_dense(units = 4, activation = 'sigmoid', input_shape = c(1)) %>% 
  layer_dense(units = 4, activation = 'tanh') %>%
  layer_dense(units = 4, activation = 'tanh') %>%
  layer_dense(units = 2, activation = 'softmax')%>% compile(loss = 'categorical_crossentropy',
                                                                            optimizer = optimizer_rmsprop(lr = 0.01),
                                                                            metrics = c('accuracy'))
fit.modelo_1 <- modelo_1 %>% fit(
  primeiro_semestre  , to_categorical(resposta_keras,2), 
  epochs = 100, batch_size = 32,validation_split=0.2)

############################################# Segundo semestre ################################################ 
modelo_2 <-keras_model_sequential() %>% 
  layer_dense(units = 6, activation = 'sigmoid', input_shape = c(2)) %>% 
  layer_dense(units = 4, activation = 'tanh') %>%
  layer_dense(units = 4, activation = 'tanh') %>%
  layer_dense(units = 2, activation = 'softmax')%>% compile(loss = 'categorical_crossentropy',
                                                                            optimizer = optimizer_rmsprop(lr = 0.01),
                                                                            metrics = c('accuracy'))
fit.modelo_2 <- modelo_2 %>% fit(
  segundo_semestre  , to_categorical(resposta_keras,2), 
  epochs = 100, batch_size = 32,validation_split=0.2)

############################################# Terceiro semestre ################################################
modelo_3 <-keras_model_sequential() %>% 
  layer_dense(units = 6, activation = 'sigmoid', input_shape = c(3)) %>% 
  layer_dense(units = 6, activation = 'tanh') %>%
  layer_dense(units = 4, activation = 'tanh') %>%
  layer_dense(units = 2, activation = 'softmax')%>% compile(loss = 'categorical_crossentropy',
                                                                            optimizer = optimizer_rmsprop(lr = 0.01),
                                                                            metrics = c('accuracy'))
fit.modelo_3 <- modelo_3 %>% fit(
  terceiro_semestre  , to_categorical(resposta_keras,2), 
  epochs = 100, batch_size = 32,validation_split=0.2)

############################################# Quarto semestre ################################################
modelo_4 <-keras_model_sequential() %>% 
  layer_dense(units = 6, activation = 'sigmoid', input_shape = c(4)) %>% 
  layer_dense(units = 6, activation = 'tanh') %>%
  layer_dense(units = 6, activation = 'tanh') %>%
  layer_dense(units = 2, activation = 'softmax')%>% compile(loss = 'categorical_crossentropy',
                                                                            optimizer = optimizer_rmsprop(lr = 0.01),
                                                                            metrics = c('accuracy'))

fit.modelo_4 <- modelo_4 %>% fit(
  quarto_semestre  , to_categorical(resposta_keras,2), 
  epochs = 100, batch_size = 32,validation_split=0.2)


############################################# Quinto semestre ################################################
modelo_5 <-keras_model_sequential() %>% 
  layer_dense(units = 8, activation = 'sigmoid', input_shape = c(5)) %>% 
  layer_dense(units = 6, activation = 'tanh') %>%
  layer_dense(units = 6, activation = 'tanh') %>%
  layer_dense(units = 2, activation = 'softmax')%>% compile(loss = 'categorical_crossentropy',
                                                                            optimizer = optimizer_rmsprop(lr = 0.01),
                                                                            metrics = c('accuracy'))
fit.modelo_5 <- modelo_5 %>% fit(
  quinto_semestre  , to_categorical(resposta_keras,2), 
  epochs = 100, batch_size = 32,validation_split=0.2)

############################################# Sexto semestre ################################################
modelo_6 <-keras_model_sequential() %>% 
  layer_dense(units = 8, activation = 'sigmoid', input_shape = c(6)) %>% 
  layer_dense(units = 8, activation = 'tanh') %>%
  layer_dense(units = 6, activation = 'tanh') %>%
  layer_dense(units = 2, activation = 'softmax')%>% compile(loss = 'categorical_crossentropy',
                                                                            optimizer = optimizer_rmsprop(lr = 0.01),
                                                                            metrics = c('accuracy'))
fit.modelo_6 <- modelo_6 %>% fit(
  sexto_semestre  , to_categorical(resposta_keras,2), 
  epochs = 100, batch_size = 32,validation_split=0.2)

############################################# Sétimo semestre ################################################
modelo_7 <-keras_model_sequential() %>% 
  layer_dense(units = 8, activation = 'sigmoid', input_shape = c(7)) %>% 
  layer_dense(units = 8, activation = 'tanh') %>%
  layer_dense(units = 8, activation = 'tanh') %>%
  layer_dense(units = 2, activation = 'softmax')%>% compile(loss = 'categorical_crossentropy',
                                                                            optimizer = optimizer_rmsprop(lr = 0.01),
                                                                            metrics = c('accuracy'))
fit.modelo_7 <- modelo_7 %>% fit(
  setimo_semestre  , to_categorical(resposta_keras,2), 
  epochs = 100, batch_size = 32,validation_split=0.2)
############################################# Oitavo semestre ################################################
modelo_8 <-keras_model_sequential() %>% 
  layer_dense(units = 10, activation = 'sigmoid', input_shape = c(8)) %>% 
  layer_dense(units = 8, activation = 'tanh') %>%
  layer_dense(units = 8, activation = 'tanh') %>%
  layer_dense(units = 2, activation = 'softmax')%>% compile(loss = 'categorical_crossentropy',
                                                                            optimizer = optimizer_rmsprop(lr = 0.01),
                                                                            metrics = c('accuracy'))
fit.modelo_8 <- modelo_8 %>% fit(
  oitavo_semestre  , to_categorical(resposta_keras,2), 
  epochs = 200, batch_size = 32,validation_split=0.2)


saveRDS(resposta_keras,"resposta_keras.rds")
readRDS("resposta_keras.rds")
  modelo_1 %>%   save_model_hdf5("modelo_1.h5")
  modelo_2 %>%   save_model_hdf5("modelo_2.h5")
  modelo_3 %>%   save_model_hdf5("modelo_3.h5")
  modelo_4 %>%   save_model_hdf5("modelo_4.h5")
  modelo_5 %>%   save_model_hdf5("modelo_5.h5")
  modelo_6 %>%   save_model_hdf5("modelo_6.h5")
  modelo_7 %>%   save_model_hdf5("modelo_7.h5")
  modelo_8 %>%   save_model_hdf5("modelo_8.h5")
  
  
saveRDS(fit.modelo_1,"fit.modelo_1.rds")
saveRDS(fit.modelo_2,"fit.modelo_2.rds")
saveRDS(fit.modelo_3,"fit.modelo_3.rds")
saveRDS(fit.modelo_4,"fit.modelo_4.rds")
saveRDS(fit.modelo_5,"fit.modelo_5.rds")
saveRDS(fit.modelo_6,"fit.modelo_6.rds")
saveRDS(fit.modelo_7,"fit.modelo_7.rds")
saveRDS(fit.modelo_8,"fit.modelo_8.rds")

    
  
  fit.modelo_1 %>%   save_model_hdf5("fit.modelo_1.h5")
  fit.modelo_2 %>%   save_model_hdf5("fit.modelo_2.h5")
  fit.modelo_3 %>%   save_model_hdf5("fit.modelo_3.h5")
  fit.modelo_4 %>%   save_model_hdf5("fit.modelo_4.h5")
  fit.modelo_5 %>%   save_model_hdf5("fit.modelo_5.h5")
  fit.modelo_6 %>%   save_model_hdf5("fit.modelo_6.h5")
  fit.modelo_7 %>%   save_model_hdf5("fit.modelo_7.h5")
  fit.modelo_8 %>%   save_model_hdf5("fit.modelo_8.h5")
  
  
  getwd()
  require(profvis)
  profvis({ runApp("C:/Users/Gustavo/Documents/Novo_Shiny") }  
          , prof_output = "C:/Users/Gustavo/Documents/Novo_Shiny")
  p <- profvis(prof_input = 'C:/Users/Gustavo/Documents/Novo_Shiny/file2314604e76e4.Rprof')
  htmlwidgets::saveWidget(p, "C:/Users/Gustavo/Documents/Novo_Shiny/profile.html") 

  

