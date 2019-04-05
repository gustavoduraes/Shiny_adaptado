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
require(forcats)
require(rsconnect)
require(pdftools)
require(yardstick)
require(kableExtra)
require(plotly)
require(shinydashboard)
require(zoo)
getwd()

fluxograma1 <- read_rds("data/fluxograma2.rds")
ui = 
  
  
  navbarPage(position="fixed-top",title=div(img(src="unb.png" ,height = 30,
                     width = 50,style = "margin:-7px 0px")),windowTitle ="Universidade de Brasília",
             
             
             
             
             theme = shinytheme("cerulean"),

             tabPanel(icon=icon("home",lib = "glyphicon"),"",title=" Apresentação",theme="cerulean",
                      
                      fluidPage(
                        h3("Apresentação"),
                        div(column(4,style="text-align:justify","Essa plafatorma é resultado do Trabalho de Conclusão de Curso apresentado na Universidade de Brasília afim de  obter  o título de Bacharel em estatística
                        do aluno Gustavo Durães sob orientação do",
                        HTML("<a href=' http://lattes.cnpq.br/1899008966088424' onclick='detect_click(this)'>Prof. Dr. Donald Matthew Pianto. </a><br>"),
                        "O objetivo do aplicativo é a idenfiticação de discentes em situação
                        de risco através da probabilidade de formatura ou não formatura."))
                        
                      
                                   
                                   
                                   
                                   
                  )),
             tabPanel(icon=icon("alert",lib="glyphicon"),title = " Leia-me",theme="cerulean",
                      fluidPage(
                        h3("Instruções de uso"),
                        
                        div(column(4,
                                   "Este aplicativo tem como objetivo identificar alunos em situação de risco através da análise de 4 disciplinas chave dos 4 primeiros semestres de cada curso.
                                   A forma de utilização do aplicativo e uma breve descrição de suas funcionalidades estão descritas abaixo.
                                   ",
                                   HTML("<br>"),
"A aba 'Selecionar Curso' é dedicada à escolha 
                                   do curso a ser analisado, assim como a escolha das matérias principais do curso escolhido e a seleção do número de grupos
                                    que serão criadas a partir das escolhas anteriores.
É disponibilizado o fluxograma das matérias obrigatórias de cada curso para facilitar a eventual escolha das disciplinas chave e quando clicado na opção 'Gerar Grupos' são disponibilizados gráficos 
que descrevem cada grupo formado e uma tabela com estatísticas descritivas. É recomendada a utilização das matérias principais que estão pré-definidas.
",
                                   HTML("<br>"),
                                   "A aba 'Inserir Histórico Escolar' é utilizada para a  inserção de informações do(s) aluno(s)  através do arquivo em pdf 
                                   do histórico ou de um banco de dados em formato .csv. Podem ser inseridos múltiplos arquivos em cada um dos campos simultaneamente.
                                   "
                                 ,
HTML("<br>"),
"Na aba 'Resultados' são exibidas as probabilidades de cada aluno pertencer a algum dos grupos formados e é disponibilizado uma ferramenta para serem baixados os resultados.",
HTML("<br>"),
"Os apêndices ilustram a relação de cursos disponíveis na plataforma e a matriz de custos utilizada para definir os grupos, respectivamente."
,
   
                                   "
                                   É recomendado que os passos abaixo sejam sempre seguidos e que a página seja recarregada a cada nova consulta de curso afim de evitar erros.
                                   ",
                      
                                   HTML("<ul><li>Selecionar curso desejado</li>
                                        <li>Em seguida são selecionados 4 matéria automaticamente para compor o tronco principal, caso seja desejado 
pode ser formado um tronco customizado ao clicar na opção 'Escolher Manualmente'.

</li>
                                        <li>Clicar do botão 'Gerar Grupos' para serem gerados os grupos de acordo com a quantidade de grupos selecionados. A opção default é a geração
de dois grupos mas podem ser criados até 4 grupos caso seja desejado observar mais detalhamente as diferenças entre os alunos. É recomendada a utilização da opção default.</li>
<li> Em seguida a opção 'Treinar Classificador' deve ser selecionada para serem criados os modelos de classificação. Esta etapa demora alguns minutos. </li>  
<li> Na aba 'Insira suas informações' existem dois campos para inserção de dados, o primeiro deles é utilizado para analisar o histórico de um único aluno através de arquivo(s) do histórico em pdf e o segundo campo 
é utilizado para executar a análise de vários a a partir de um arquivo(s) em formato .csv
. </li>       
     <li>Na aba 'Resultados' estará disponível, após realizados os passos anteriores, uma tabela com as probabilidades de cada aluno pertencer a cada um dos grupo. É possível
                                        fazer o download da tabela ao clicar no botão 'Baixar Resultado'.</li>                                   </ul>"),style="text-align:justify"))
                        
                      )
                      
                      
                      
                      
                      
                      ),
        
             
             
             tabPanel(icon=icon("search",lib = "glyphicon"),"",title="Selecionar Curso",theme="cerulean",
                      
                      sidebarLayout(
                        sidebarPanel(  textOutput("teste"),
                                       selectizeInput('curso',"Nome do Curso",choices="BACHARELADO EM ESTATISTICA-1716",
                                                      selected ="BACHARELADO EM ESTATISTICA-1716"),#textOutput("nomecurso"),     
                                       conditionalPanel(
                                         condition = "input.curso !='' ",
                                         prettyCheckboxGroup("auto", "Escolher Manualmente", selected ="",
                                                             status = "default", shape = "square",
                                                             outline = FALSE, fill = TRUE, thick = TRUE, animation = "pulse",
                                                             icon = NULL, plain = FALSE, bigger = FALSE, inline = TRUE,
                                                             width = NULL, choiceNames ="" , choiceValues = "Sim")
                                         
                                         
                                         )
                                       ,
                                       conditionalPanel(
                                         condition = "input.auto =='Sim' ",
                                         selectInput('m1', 'Primeira matéria',""))
                                       ,
                                       conditionalPanel(
                                         condition = "input.m1 !='' & input.auto =='Sim'  ",
                                         selectInput('m2', 'Segunda matéria', "")
                                         
                                       ),
                                       
                                       conditionalPanel(
                                         condition ="input.m2 !=''&input.auto =='Sim' ",
                                         selectInput('m3','Terceira matéria',"")), 
                                       
                                       conditionalPanel(
                                         condition ="input.m3 !=''&input.auto =='Sim' ",
                                         selectInput('m4','Quarta matéria',"")),
                                       
                                       
                                       #conditionalPanel(
                                      #   condition ="input.m4 !=''",
                                       #  selectInput('m5','Quinta matéria',""))
                                      # ,
                                      
                                      conditionalPanel(condition ="input.m4 !='' | input.auto =='' & input.curso !='' ",
                                                       sliderInput("qtdclusters", "Quantidade de Grupos:",
                                                                   min = 2, max = 4,
                                                                   value = 2)),
                                       conditionalPanel(
                                         condition ="input.m4 !='' | input.auto =='' & input.curso !='' ",
                                         prettyCheckboxGroup("gerar_clusters", "Gerar Grupos", selected = "Sim",
                                                             status = "default", shape = "square",
                                                             outline = FALSE, fill = TRUE, thick = TRUE, animation = "pulse",
                                                             icon = NULL, plain = FALSE, bigger= FALSE, inline = TRUE,
                                                             width = NULL, choiceNames = "", choiceValues = "Sim"))
                                      ,   
                                      conditionalPanel(
                                        condition ="input.gerar_clusters=='Sim'  ",
                                        prettyCheckboxGroup("treinar", "Treinar classificador", selected = "",
                                                            status = "default", shape = "square",
                                                            outline = FALSE, fill = TRUE, thick = TRUE, animation = "pulse",
                                                            icon = NULL, plain = FALSE, bigger = FALSE, inline = TRUE,
                                                            width = NULL, choiceNames = "", choiceValues = "Sim")),
                                    
                                      
                                       
                                       width=2)
                                       
                        
                        
                        
                        ,mainPanel(       box(title = "Fluxograma do curso selecionado",plotOutput(outputId = "fluxograma",width="1400px",height="1000px"),
                                              width=12,align="center"),
                                          
                                           conditionalPanel(condition = "input.gerar_clusters=='Sim'& (input.qtdclusters=='2'|input.qtdclusters=='3') ",
                                           box(title="Grupos formados",plotlyOutput(outputId = "clusters",
                                                                                    width="1500",height="900px"),align="center")),
                                          
                                          conditionalPanel(condition = "input.gerar_clusters=='Sim'& input.qtdclusters =='4'",
                                                           box(title="Grupos formados",plotlyOutput(outputId = "clusters2",
                                                                                                    width="1500",height="1300px"),align="center",width=12)),
                                          
                                          conditionalPanel(condition = "input.gerar_clusters=='Sim'",
                                          box(title="Estatísticas Resumo dos Grupos",DT::dataTableOutput('estatisticas'),align="center",width = 12)),
                                          
                                          
                                          
                                           conditionalPanel(condition = "input.treinar=='Sim'",
                                                            column(12,box(title="Avalição da precisão",tableOutput("rede"),align="center")),
                                                           fluidRow( 
                                                            column(3,tableOutput("cm1"),tableOutput("cm5")),
                                                           column(3,tableOutput("cm2"),tableOutput("cm6")),
                                                           column(3,tableOutput("cm3"),tableOutput("cm7")),
                                                           column(3,tableOutput("cm4"),tableOutput("cm8"))),
                                                           fluidRow( 
                                    column(6,box(title="Ajuste para o modelo do primeiro semestre",plotlyOutput("plot1"),width=NULL),
                                           box(title="Ajuste para o modelo do terceiro semestre", plotlyOutput("plot3"),width=NULL),
                                           box(title="Ajuste para o modelo do quinto semestre", plotlyOutput("plot5"),width=NULL),
                                            box(title="Ajuste para o modelo do sétimo semestre",plotlyOutput("plot7"),width=NULL)),
                                    column(6,box(title="Ajuste para o modelo do segundo semestre",plotlyOutput("plot2"),width=NULL),
                                           box(title="Ajuste para o modelo do quarto semestre",plotlyOutput("plot4"),width=NULL),
                                           box(title="Ajuste para o modelo do sexto semestre", plotlyOutput("plot6"),width=NULL),
                                           box(title="Ajuste para o modelo do oitavo semestre",plotlyOutput("plot8"),width=NULL))

                                                             
                                                           
                                                             
                                                           
                                                           
                                                           
                                                           )

                                                            )
                                          
                                           
                                           
                                           
                                           
                                           
                                           
                                           
                                           
                        )
                      )
                      
             ),
             
             tabPanel(icon=icon("cloud-upload",lib = "glyphicon"),"",title="Inserir Histórico Escolar",theme="cerulean",
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          fileInput("histinput","Histórico ", multiple=TRUE,accept = ".pdf",
                                    buttonLabel = "Selecionar",placeholder = "Arquivo em formato em .pdf"),
                          
                          helpText( "O arquivo do histórico se encontra no",a("Matrícula web",    
                                                                              href="https://matriculaweb.unb.br/graduacao/sec/he.aspx")
                          ),
                          
                          
                          fileInput("dbinput","Banco de dados ", multiple=TRUE,accept = ".csv",
                                    buttonLabel = "Selecionar",placeholder = "Arquivo em  formato.csv"),
                          helpText( HTML("O arquivo deve conter 4 colunas, são elas: Matrícula, Disciplina, Menção e Período.
                                    As colunas do arquivo devem estar ordenadas e grafadas da mesma forma em que foram explicitadas. <br>
O Período deve ser um valor inteiro entre 1 e 8 representando em qual semestre a disciplina foi cursada pelo aluno.
                                    ")
                                    )
                          ,width=3
                          
                          
                          
                        )
                        ,mainPanel(
                          conditionalPanel(condition = "",
                          box(title="Dados Inseridos",DT::dataTableOutput('dbrds'),width=12)),
                          conditionalPanel(condition = "",
                                           box(title="Dados Transformados",DT::dataTableOutput('hist'),width=12)),
                          conditionalPanel(condition = "",
                          box(title = "Fluxograma do curso selecionado",plotOutput(outputId = "fluxograma_aluno",width="1400px",height="1000px"),
                              width=12,align="center"))
                         

                        ))),
             
             tabPanel(icon=icon("education",lib = "glyphicon"),"",title="Resultados",theme="cerulean",
                      
                      sidebarLayout(
                        
                        sidebarPanel(
                          conditionalPanel("input.treinar=='Sim'",
                                            h3("Baixar Resultados"),
                          downloadButton("downloadData", "Baixar Resultados"))
                          ,width=2
                        ),
                        mainPanel = (
                        box(Title="Probabilidades",mainPanel( DT::dataTableOutput('result')),width=6))
                        
                      )
                      
                   
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      ),
             
             tabPanel(icon=icon("table",lib = "font-awesome"),title = "Apêndice 1",theme="cerulean",
                      titlePanel("Relação de Cursos"),
                      mainPanel(
                        
                        DT::dataTableOutput('apendice')
                        
                      )
             ),
             
             tabPanel(icon=icon("table",lib = "font-awesome"),title = "Apêndice 2",theme="cerulean",
                      titlePanel("Matriz de Custos"),
                      mainPanel(
                        
                        DT::dataTableOutput('matriz_custos')
                        
                      )
             )




,tags$style(type="text/css","body{padding-top: 70px;}")

    )


  


  