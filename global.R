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
 library('ggplot2')
 library('formattable') #tabelas personalizadas
 
 #library('auth0')
 #a0_1 <- auth0::auth0_info()

 #carregand dados
 load('mapa_macro.RData')
 load('municipios_br.RData')
 load('pops_sc.RData')
 load('estab_saude.RData')
 load('regioes_saude.RData')
 load('tabs.RData')
 
 
 source('./treating_data.R', local = T, encoding = 'UTF-8')
 #options(warn = -1)
 
source('./www/highcharts/generalhigh.R')
 source('./www/highcharts/plotlyjs.R')
 
 #carregando funçes dashboard
 #source('./funcoes_dashboard.R')
 
 conn <- function(){DBI::dbConnect(RPostgres::Postgres(),  host = '172.22.34.56', port = 5432, user = 'dimitri',
                     password = 'cievs666', dbname = 'rumores')} 
 
 