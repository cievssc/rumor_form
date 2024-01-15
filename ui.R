 #app Formulário Rumores (29-ago-2023, 15:25h)
 tags$html(
         tags$head(
         HTML('<script defer src="https://use.fontawesome.com/releases/v5.15.0/js/all.js"
          integrity="sha384-slN8GvtUJGnv6ca26v8EzVaR9DC58QEwsIk9q1QXdCU8Yu8ck/tL/5szYlBbqmS+"
           crossorigin="anonymous"></script>'),
         
         tags$meta(charset="utf-8"),
         tags$title("Rumores")#,
         #tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css"), # href="bootstrap.min.css")
         #tags$link( href="bootstrap.min.css", rel="stylesheet")
      ),  #head
      shinyjs::useShinyjs(),
      #shinyWidgets::useShinydashboard(),
     tags$body( 
    
  tags$div(id = 'form',
  htmlTemplate('index.html')
  
  ),
    shinyjs::hidden(    
         tags$div(id = 'teste',
   navbarPage(#theme = shinythemes::shinytheme('cerulean'),
               theme = bslib::bs_theme(version = 4, bootswatch = 'default'),
        id = "navbar",
        title = div(img(src = './images/cievs_nacional.png', style="
                               padding-right:10px;
                               padding-bottom:10px",
                        height = 62)),
                        
                      
        
         source('./ui/rumor_form.R', local = T, encoding = 'UTF-8')$value, #tabpanel rumor evento
         source('./ui/list_forms.R', local = T, encoding = 'UTF-8')$value,  #tabpanel lista eventos
         source('./ui/verifica_form.R', local = T, encoding = 'UTF-8')$value,  #tabpanel lista eventos        
         
        tabPanel('Sair') 
         
   ) #end navbarpage

   ) #enddivteste shinyjs
    ) #end shinyjs::hidden
  )#body
  ) #html
 #, info = a0_1) #autho_ui                       