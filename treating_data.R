 #criando algumas variáveis para o uso no sistema
 
 #vetor de série / ano escolar
  educ_series <- c(paste0(1:9, 'º Ano do Fundamental'), c(paste0(1:4, 'º Ano do E. Médio')),
                 'Pré-escola', paste0('EJA (E.Fundamental) - Fase ',1:4))
 
 #vetor de caracterização da infrequência (refeito em 20-jul-22, 12:06h)
 categoria_falta$tipo <- factor(categoria_falta$tipo, levels = c('Causas internas', 'Causas externas'))
 escolhas_categoria <- lapply(split(categoria_falta, categoria_falta$tipo), function(x){
                              paste(x$codigo,'-', x$categorias)
                              })
 escolhas_categoria_vetor <- unlist(escolhas_categoria) %>% unname
 
 #função para geração de relatórios (19-maio-2022)
 
 func_relat <- function(saida, diretorio, rmd){
                    rmarkdown::render(
        if(input$relat_formulario == 1){
        paste0(tempReport,'/longevidade.Rmd')}else{
        if(input$relat_formulario == 2){
        paste0(tempReport,'/mortalidade_infantil_relat.Rmd')
        }else{
        if(input$relat_formulario == 3){
        paste0(tempReport,'/infrequencia_relat.Rmd')
        }
        }},#"markdown/evento_pdf.Rmd",
        output_format = 'html_fragment',
        output_file = 'relatorio.html',
        params = params_ ,
        envir = new.env(),
        intermediates_dir = diretorio_relatorio()
      )    
                        
                        }

 
