 #carregando os pacotes...
 library('shiny')
 library('shinydashboard')
 library('shinydashboardPlus')
 library('shinyWidgets')
 #library('plotly')
 library('leaflet')
 library('leaflet.extras')
 #library('leaflet.extras2')
 #library('bs4Dash')

 library('dplyr')        #manipulação de dados - tydiverse
 library('stringr')      #funções de string  - tydiverse
 #desabilitado temporariamente
 #library('rgeos') #leitura de mapas
 #library('rgdal') #leitra de mapas
 #library('sf') #plot maps
 library('magrittr')     #para mudar nome de colunas
 library('reshape2')
 library('data.table')
 library('RColorBrewer')
 #library('scales')
 library('raster')
 #library('xts')
 #library('dygraphs')
 #library('highcharter')
 library('ggplot2')
 library('formattable') #tabelas personalizadas
 
 library('auth0')
 #a0_1 <- auth0::auth0_info()

 #carregand dados
 load('mapa_macro.RData')
 load('municipio_prog.RData')
 load('municipios_br.RData')
 load('pops_sc.RData')
 load('estab_saude.RData')
 load('cid10.RData')
 load('cid_evitavel.RData')
 load('lista_cidevitavel.RData')
 load('lista_escolas.RData')
 load('cbo_list.RData')
 load('categoria_falta.RData')
 load('regioes_saude.RData')
 load('tabs.RData')
 
 municipio_prog$cod6 <- floor(municipio_prog$codigo/10)
 
 source('./treating_data.R', local = T, encoding = 'UTF-8')
 #options(warn = -1)
 
source('./www/highcharts/generalhigh.R')
 source('./www/highcharts/plotlyjs.R')
 
 #carregando funçes dashboard
 #source('./funcoes_dashboard.R')
 
 conn <- function(){DBI::dbConnect(RPostgres::Postgres(), , host = '10.111.7.180', port = 5432, user = 'developer',
                       password = 'Q1w2e3r4$', dbname = 'desenvolvimento',
                       options="-c search_path=form_gentecatarina")} 
 
 #conn <- function(){DBI::dbConnect(RPostgres::Postgres(), host = Sys.getenv('BD_HOST'), port = vetor_a[[1]], user = Sys.getenv('BD_USER'),
  #                     password = Sys.getenv('BD_PASS'), dbname = Sys.getenv('BD_DB'),
   #                    options=vetor_a[[2]])}
   
 
 
 
