#lista de função do Admlite (31-jul-2019, 14:43h)

#caixas de comparação em relaçao ao mês anterior              
   funcao_box <- function( x = NULL, y = NULL, z = NULL){
        # check some inputs
        #stopifnot(is.numeric(x))
        #stopifnot(is.numeric(y))
        #stopifnot(is.character(z)) 
       
       withTags(
         div(class = 'description-block border-right',
           if(x > 0){span(class = 'description-percentage text-green',
                          icon('caret-up'),
                          paste0(x,'%'))},
           if(x == 0 ){span(class = 'description-percentage text-yellow',
                          icon('caret-left'),
                          paste0(x,'%'))},
           if(x < 0){span(class = 'description-percentage text-red',
                          icon('caret-down'),
                          paste0(x,'%'))},
           h5(class = 'description-header', y),
           span(class = 'description-text', z)
           )#div
         ) #withtags
       }#end_function
       

 #caixas de comparação em relaçao ao mês anterior  (valor absolut)
   funcao_boxII <- function( x = NULL, y = NULL, z = NULL){
        # check some inputs
        #stopifnot(is.numeric(x))
        #stopifnot(is.numeric(y))
        #stopifnot(is.character(z)) 
       
       withTags(
         div(class = 'description-block border-right',
           if(x > 0){span(class = 'description-percentage text-green',
                          icon('caret-up'),
                          paste0(x))},
           if(x == 0 ){span(class = 'description-percentage text-yellow',
                          icon('caret-left'),
                          paste0(x))},
           if(x < 0){span(class = 'description-percentage text-red',
                          icon('caret-down'),
                          paste0(x))},
           h5(class = 'description-header', y),
           span(class = 'description-text', z)
           )#div
         ) #withtags
       }#end_function
        