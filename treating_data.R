 #criando algumas variáveis para o uso no sistema

 #vetor área técnica
 load('area_tecnica.RData')

 #arquivos mapa
 load('centroide.RData')

 #fonte notícia
 fonte <- c("EIOS","PROMED","Outbreak News", 'RENAVEH',
            "Rumor Verídico", "OPAS/OMS", "IHR", 'CIEVS Nacional',
            'Clipping Comunicação', 'RSSOWL','Twitter',"OUTROS")


 
 #equipes técnicas envolvidas (add 30-jan-24, 14:29h)

 equipes_monitora <- c('LACEN', 'Região de Saúde Extremo Oeste', 'Região de Saúde de Xanxerê', 'Região de Saúde do Oeste',
                        'Região de Saúde Alto Uruguai Catarinense', 'Região de Saúde do Meio Oeste', 'Região de Saúde do Alto Vale do Rio do peixe',
                        'Região de Saúde da Serra Catarinense', 'Região de Saúde da Foz do Rio Itajai', 'Região de Saúde do Alto Vale do Itajai',
                        'Região de Saúde do Médio Vale do Itajai', 'Região de Saúde de Florianópolis', 'Região de Saúde Carbonífera', 
                        'Região de Saúde de Laguna', 'Região de Saúde do Extremo Sul Catarinense', 'Região de Saúde Nordeste',
                        'Região de Saúde Vale do Itapocu', 'Região de Saúde do Planalto Norte' ,'CIEVS Chapecó' ,'CIEVS Dionísio Cerqueira' ,
                        'CIEVS DSEI-INTERIOR SUL' ,'CIEVS Florianópolis' ,'CIEVS Itajai','CIEVS Joinville' ,'CIEVS Navegantes' ,'VIGIDESASTRES' ,
                        'Defesa Civil' ,'SUV' ,'SUE' ,'SUH')
  
