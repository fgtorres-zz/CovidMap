
library(readxl)

setwd("/Users/fgtorres/Desktop/G215122020")

folder_path = "/Users/fgtorres/Desktop/"
  #df <- read.csv(inFile$datapath, header = TRUE,sep = input$separator)
  df <- read_excel("data.xls")
  
  #Filtering the header Excel File
  linhas <- c(1,2,3,4,5,6)
  df<- df[-linhas,]
  names(df) <- df[1,]
  df<- df[-1,]
  
  #Spliting in sub datasets
  i<-0
  gene<-1
  
  #DIAGNOSTICO DO KIT SEEGENE
  diagnostico = data.frame(ID = "",
                           Galeria = "",
                           Placa = "",
                           Termociclador = "",
                           Data = "",
                           Ct_gene1 = "",
                           Ct_gene2 = "",
                           Ct_gene3 = "",
                           Ct_gene4 = "",
                           diagnostico = "" )
  
  
  diagnostico<- diagnostico[-1,]
                           
  while(i< nrow(df)){
    i<-i+1
    amostra<- df[i,2]
    well<- df[i,1]
    diagcovid = ""
    
    if(gene==1){
      gene1<- df[i,7]
      gene <- gene + 1
    }else if (gene==2){
      gene2<- df[i,7]
      gene <- gene + 1
    }else if (gene==3){
      gene3<- df[i,7]
      gene <- gene + 1
    } else if(gene==4){
      gene4<- df[i,7]
      gene<-1
      
      # DIAGNOSTICANDO
      # Teste genes negativos
      testegene1 = 1
      testegene2 = 1
      testegene3 = 1
      testegene4 = 1
      
      if (gene1  == "Undetermined" || gene1 > 40){
        testegene1 = 0
      }
      
      if (gene2  == "Undetermined" || gene2 > 40){
        testegene2 = 0
      }
      
      if (gene3  == "Undetermined" || gene3 > 40){
        testegene3 = 0
      }
      
      if (gene4  == "Undetermined" || gene4 > 40){
        testegene4 = 0
      }
      
      #Diagnosticando....
      # Invalidos...
      if (testegene1==0 && testegene2==0 && testegene3==0 && testegene4==0){
        diagcovid = "Invalido"
      }
      
      #Nao detectados
      if (testegene1==1 && testegene2==0 && testegene3==0 && testegene4==0){
        diagcovid = "Nao detectado"
      }
      
      #Detectados
      if ((testegene2==1 && testegene3==1 && testegene4==1) || (testegene2==1 && testegene3==0 && testegene4==1) ||
          (testegene2==1 && testegene3==1 && testegene4==0) || (testegene2==0 && testegene3==1 && testegene4==1) ||
          (testegene2==0 && testegene3==0 && testegene4==1) || (testegene2==0 && testegene3==1 && testegene4==0)){
        diagcovid = "Detectado"
      }
      
      #Inconclusivo
      if (testegene2==1 && testegene3==0 && testegene4==0){
        diagcovid = "Inconclusivo"
      }
    
      amostra_data <- data.frame(ID = as.character(amostra),
                 Galeria = as.character(well),
                 Placa = as.character(""),
                 Termociclador = as.character(""),
                 Data = as.character(""),
                 Ct_gene1 = as.character(gene1),
                 Ct_gene2 = as.character(gene2),
                 Ct_gene3 = as.character(gene3),
                 Ct_gene4 = as.character(gene4),
                 diagnostico = diagcovid)
      
      diagnostico =  rbind(diagnostico, amostra_data)
    }
  }
  

  write.csv(diagnostico,
            paste(as.character(folder_path),
                  "/",
                  "diagnosticos_",
                  format(Sys.time(),
                         "%a %b %d %X %Y"),
                  ".csv",
                  sep = ""),
            sep = ",",
            quote = F)  
  
  