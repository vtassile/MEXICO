library(shiny)
library(shinydashboard)
library(FactoMineR)
library(lme4)
library(matrixcalc)
library(cluster)
library(tidyr)
library(dplyr)
library(networkD3)
library(DT)
library(d3Network)
library(RCurl)
library(leaflet)
library(highcharter)
library(dendextend)
library(circlize)
library(ggfortify)
library(d3heatmap)
library(ggplot2)
library(ggdendro)
library(plotly)
library(googleVis)
#library(htmltools)
#library(exportwidget)
#library(webshot)
#library(highcharter)
library(data.table)
library(lubridate)
#library(mxmaps)
library(scales) # needed for comma
library(viridis)

shinyServer(function(input, output,session) {

  output$localPlot <- renderPlotly({
    FIN_2<-dim(COMBO_MES)[1]
    COMBO_MES$MES2<-1:FIN_2
    NUEVO_2<-left_join(MAESTRO_1,COMBO_MES[,c(3:5)],fill = 0)
    NUEVO_2$EMPRESA<-as.factor(NUEVO_2$EMPRESA)
    # GENERA CODIGOS PARA PRODUCTOR LOS INCORPORA A LA BASE NUEVO_1
    EMPRESAS<-as.data.frame(unique(NUEVO_2$EMPRESA))
    names(EMPRESAS)<-"EMPRESA"
    LIMITE<-dim(EMPRESAS)[1]
    EMPRESAS$N_ENTIDAD<-1:LIMITE
    NUEVO_2<-left_join(NUEVO_2,EMPRESAS)
    #### DEFINE LAS VARIABLES
    TEMPO<-NUEVO_2
    RESPUESTA<-"MES"
    TEMPO$RESPUESTA<-RESPUESTA
    TEMPO$UNIDAD<-TEMPO$TEMPORADA
    ### CONSTRUYE LA VARIABLE PARA CRUZAR
    TEMPO<-unite_(TEMPO, "ESTRATO2", c("RESPUESTA","MES"))
    #### GENERA LOS GRUPOS PARA EL CRUCE
    #### CAMBIAR LA VARIABLE DENTRO DE mean PARA QUE COINCIDA CON RESPUESTA
    TEMPO2<-TEMPO %>% group_by(UNIDAD,ZONA_DESTINO,ESTRATO2) %>% summarise(mean(KILOS))
    names(TEMPO2)[4]<-"TOTAL"
    ###### REALIZA EL CRUCE
    TEMPO3<-TEMPO2 %>% spread(ESTRATO2,TOTAL,fill = 0)
    TEMPO3<-as.data.frame(TEMPO3)
    TEMPO3_1<-TEMPO3
    TEMPO3_1<-unite_(TEMPO3_1, "ESTRATO3", c("UNIDAD","ZONA_DESTINO"))
    rownames(TEMPO3_1) <-as.character(TEMPO3_1[1][,1])
    TEMPO3_2<-TEMPO3_1[,2:dim(TEMPO3_1)[2]]
    ##### identifica los clusters
    hc <- hclust(dist(TEMPO3_2), "ave")
    GRUPO=cutree(hc,k=5) 
    GRUPO<-as.data.frame(GRUPO)
    GRUPO$ESTRATO3<-rownames(GRUPO)
    GRUPO$ESTRATO3<-as.factor(GRUPO$ESTRATO3)
    TEMPO3_1$ESTRATO3<-as.factor(TEMPO3_1$ESTRATO3)
    TEMPO5<-left_join(GRUPO, TEMPO3_1)
    rownames(TEMPO5) <-as.character(TEMPO5[2][,1])
    TEMPO5<-TEMPO5[,c(1,3:dim(TEMPO5)[2])]
    V_CP1<-summary(prcomp(TEMPO5[,2:dim(TEMPO5)[2]], scale = TRUE))$importance[2]*100
    V_CP2<-summary(prcomp(TEMPO5[,2:dim(TEMPO5)[2]], scale = TRUE))$importance[5]*100
    Var_CP1<-paste("Primer Componente (",V_CP1,"%)",sep="")
    Var_CP2<-paste("Segunda Componente (",V_CP2,"%)",sep="")
    p<-autoplot(prcomp(TEMPO5[,2:dim(TEMPO5)[2]],scale=TRUE),
                label = TRUE, colour = TEMPO5$GRUPO, label.size = 3,
                loadings = TRUE, loadings.colour = 'blue',loadings.label = TRUE, 
                loadings.label.size = 4,shape = TRUE, frame = FALSE,
                xlab =Var_CP1,ylab=Var_CP2,main="Análisis de Componentes Principales")
    (gg <- ggplotly(p))
  })

  output$heatmap <- renderD3heatmap({
    FIN_2<-dim(COMBO_MES)[1]
    COMBO_MES$MES2<-1:FIN_2
    NUEVO_2<-left_join(MAESTRO_1,COMBO_MES[,c(3:5)],fill = 0)
    NUEVO_2$EMPRESA<-as.factor(NUEVO_2$EMPRESA)
    # GENERA CODIGOS PARA PRODUCTOR LOS INCORPORA A LA BASE NUEVO_1
    EMPRESAS<-as.data.frame(unique(NUEVO_2$EMPRESA))
    names(EMPRESAS)<-"EMPRESA"
    LIMITE<-dim(EMPRESAS)[1]
    EMPRESAS$N_ENTIDAD<-1:LIMITE
    NUEVO_2<-left_join(NUEVO_2,EMPRESAS)
    #### DEFINE LAS VARIABLES
    TEMPO<-NUEVO_2
    RESPUESTA<-"MES"
    TEMPO$RESPUESTA<-RESPUESTA
    TEMPO$UNIDAD<-TEMPO$TEMPORADA
    ### CONSTRUYE LA VARIABLE PARA CRUZAR
    TEMPO<-unite_(TEMPO, "ESTRATO2", c("RESPUESTA","MES"))
    #### GENERA LOS GRUPOS PARA EL CRUCE
    #### CAMBIAR LA VARIABLE DENTRO DE mean PARA QUE COINCIDA CON RESPUESTA
    TEMPO2<-TEMPO %>% group_by(UNIDAD,ZONA_DESTINO,ESTRATO2) %>% summarise(mean(KILOS))
    names(TEMPO2)[4]<-"TOTAL"
    ###### REALIZA EL CRUCE
    TEMPO3<-TEMPO2 %>% spread(ESTRATO2,TOTAL,fill = 0)
    TEMPO3<-as.data.frame(TEMPO3)
    TEMPO3_1<-TEMPO3
    TEMPO3_1<-unite_(TEMPO3_1, "ESTRATO3", c("UNIDAD","ZONA_DESTINO"))
    rownames(TEMPO3_1) <-as.character(TEMPO3_1[1][,1])
    TEMPO3_2<-TEMPO3_1[,2:dim(TEMPO3_1)[2]]
    d3heatmap(TEMPO3_2, scale="column", colors="Blues", 
              k_row = 5, k_col = 5,yaxis_width =100,
              xaxis_height = 100)
  }) 
 
  output$heatmap2_1 <- renderPlot({
    FIN_2<-dim(COMBO_MES)[1]
    COMBO_MES$MES2<-1:FIN_2
    if(input$MOMENTO==1){FILTRADO<-MAESTRO_1}
    if(input$MOMENTO==2){FILTRADO<-filter(MAESTRO_1,VENTA=="Cosecha")}
    if(input$MOMENTO==3){FILTRADO<-filter(MAESTRO_1,VENTA=="Poscosecha")}
    NUEVO_2<-left_join(FILTRADO,COMBO_MES[,c(3:5)],fill = 0)
    NUEVO_2$EMPRESA<-as.factor(NUEVO_2$EMPRESA)
    # GENERA CODIGOS PARA PRODUCTOR LOS INCORPORA A LA BASE NUEVO_1
    EMPRESAS<-as.data.frame(unique(NUEVO_2$EMPRESA))
    names(EMPRESAS)<-"EMPRESA"
    LIMITE<-dim(EMPRESAS)[1]
    EMPRESAS$N_ENTIDAD<-1:LIMITE
    NUEVO_2<-left_join(NUEVO_2,EMPRESAS)
    #### DEFINE LAS VARIABLES
    TEMPO<-NUEVO_2
    TEMPO$UNIDAD<-TEMPO$TEMPORADA
    if(input$ESTRATO=="MESES") {RESPUESTA<-"ZONA"}
    if(input$ESTRATO=="ZONAS") {RESPUESTA<-"MES"}  
    TEMPO$RESPUESTA<-RESPUESTA
    ### CONSTRUYE LA VARIABLE PARA CRUZAR
    if(input$ESTRATO=="MESES") {TEMPO<-unite_(TEMPO, "ESTRATO2", c("RESPUESTA","ZONA_DESTINO"))}
    if(input$ESTRATO=="ZONAS") {TEMPO<-unite_(TEMPO, "ESTRATO2", c("RESPUESTA","MES"))}  
    #### GENERA LOS GRUPOS PARA EL CRUCE
    if(input$ESTRATO=="MESES") {TEMPO2<-TEMPO %>% group_by(UNIDAD,MES,ESTRATO2) %>% summarise(mean(KILOS))}
    if(input$ESTRATO=="ZONAS") {TEMPO2<-TEMPO %>% group_by(UNIDAD,ZONA_DESTINO,ESTRATO2) %>% summarise(mean(KILOS))}  
    #### CAMBIAR LA VARIABLE DENTRO DE mean PARA QUE COINCIDA CON RESPUESTA
    COLA<-dim(TEMPO2)[2]
    names(TEMPO2)[4]<-"TOTAL"
    ###### REALIZA EL CRUCE
    TEMPO3<-TEMPO2 %>% spread(ESTRATO2,TOTAL,fill = 0)
    TEMPO3<-as.data.frame(TEMPO3)
    TEMPO3_1<-TEMPO3
    if(input$ESTRATO=="MESES") {TEMPO3_1<-unite_(TEMPO3_1, "ESTRATO3", c("UNIDAD","MES"))}
    if(input$ESTRATO=="ZONAS") {TEMPO3_1<-unite_(TEMPO3_1, "ESTRATO3", c("UNIDAD","ZONA_DESTINO"))}  
    rownames(TEMPO3_1) <-as.character(TEMPO3_1[1][,1])
    TEMPO3_2<-TEMPO3_1[,2:dim(TEMPO3_1)[2]]
    ################ APLICA LOS FILTROS  #################
    ######################################################
    TEMPORADA<-filter(TEMPO3,UNIDAD==input$E_MODELO1)
    rownames(TEMPORADA) <-as.character(TEMPORADA[2][,1])
    TEMPORADA<-TEMPORADA[,3:dim(TEMPORADA)[2]]
    TEMPORADA_1<-TEMPORADA
    TEMPORADA<-filter(TEMPO3,UNIDAD==input$E_MODELO2)
    rownames(TEMPORADA) <-as.character(TEMPORADA[2][,1])
    TEMPORADA<-TEMPORADA[,3:dim(TEMPORADA)[2]]
    TEMPORADA_2<-TEMPORADA
    hc1 <- hclust(dist(TEMPORADA_1), "ave")
    hc2 <- hclust(dist(TEMPORADA_2), "ave")
    GRAFICO<-tanglegram(hc1,hc2,k_labels = input$GRUPO_K, k_branches = input$GRUPO_K,
                        sort=TRUE,common_subtrees_color_branches = TRUE,intersecting=TRUE,main="Tanglegrama")
    print(entanglement(GRAFICO)) 
  })  
  
  plotInput <- reactive({
    FILTRA<-filter(MAESTRO_4,ANO<2016)
    FILTRA<-filter(FILTRA,ANO>2011)
    if(input$FRUTA_3==1){FILTRADO1<-FILTRA}
    if(input$FRUTA_3==2){FILTRADO1<-filter(FILTRA,ORIGEN=="I")}
    if(input$FRUTA_3==3){FILTRADO1<-filter(FILTRA,ORIGEN=="N")}
    if(input$MOMENTO2==1){FILTRADO<-FILTRADO1}
    if(input$MOMENTO2==2){FILTRADO<-filter(FILTRADO1,VENTAS=="Cosecha")}
    if(input$MOMENTO2==3){FILTRADO<-filter(FILTRADO1,VENTAS=="Postcosecha")}
    FIN_2<-dim(COMBO_MES)[1]
    COMBO_MES$MES2<-1:FIN_2
    NUEVO_2<-left_join(FILTRADO,COMBO_MES[,c(3:5)],fill = 0)
    #### DEFINE LAS VARIABLES
    TEMPO<-NUEVO_2
    TEMPO$UNIDAD<-TEMPO$ANO
    if(input$ESTRATO_LI=="MESES") {RESPUESTA<-"M"}
    if(input$ESTRATO_LI=="ZONAS") {RESPUESTA<-"R"}  
    TEMPO$RESPUESTA<-RESPUESTA
    ### CONSTRUYE LA VARIABLE PARA CRUZAR
    if(input$ESTRATO_LI=="MESES") {TEMPO<-unite_(TEMPO, "ESTRATO2", c("RESPUESTA","MES"))}
    if(input$ESTRATO_LI=="ZONAS") {TEMPO<-unite_(TEMPO, "ESTRATO2", c("RESPUESTA","REGION"))}  
    #### GENERA LOS GRUPOS PARA EL CRUCE
    if(input$ESTRATO_LI=="MESES") {TEMPO2<-TEMPO %>% group_by(UNIDAD,ORIGEN,REGION,ESTRATO2) %>% summarise(mean(KILOS))}
    if(input$ESTRATO_LI=="ZONAS") {TEMPO2<-TEMPO %>% group_by(UNIDAD,ORIGEN,MES,ESTRATO2) %>% summarise(mean(KILOS))}  
    #### CAMBIAR LA VARIABLE DENTRO DE mean PARA QUE COINCIDA CON RESPUESTA
    COLA<-dim(TEMPO2)[2]
    names(TEMPO2)[COLA]<-"TOTAL"
    ###### REALIZA EL CRUCE
    TEMPO3<-TEMPO2 %>% spread(ESTRATO2,TOTAL,fill = 0)
    TEMPO3<-as.data.frame(TEMPO3)
    TEMPO3_1<-TEMPO3
    if(input$ESTRATO_LI=="MESES") {TEMPO3_1<-unite_(TEMPO3_1, "ESTRATO3", c("UNIDAD","ORIGEN","REGION"))}
    if(input$ESTRATO_LI=="ZONAS") {TEMPO3_1<-unite_(TEMPO3_1, "ESTRATO3", c("UNIDAD","ORIGEN","MES"))}  
    rownames(TEMPO3_1) <-as.character(TEMPO3_1[1][,1])
    TEMPO3_2<-TEMPO3_1[,2:dim(TEMPO3_1)[2]]
    ################ APLICA LOS FILTROS  #################
    ######################################################
    TEMPORADA<-filter(TEMPO3,UNIDAD==input$E_MODELO1LI)
    if(input$ESTRATO_LI=="MESES") {TEMPORADA<-unite_(TEMPORADA, "ESTRATO3", c("ORIGEN","REGION"))}
    if(input$ESTRATO_LI=="ZONAS") {TEMPORADA<-unite_(TEMPORADA, "ESTRATO3", c("ORIGEN","MES"))}  
    rownames(TEMPORADA) <-as.character(TEMPORADA[2][,1])
    TEMPORADA<-TEMPORADA[,3:dim(TEMPORADA)[2]]
    TEMPORADA_1<-TEMPORADA
    TEMPORADA<-filter(TEMPO3,UNIDAD==input$E_MODELO2LI)
    if(input$ESTRATO_LI=="MESES") {TEMPORADA<-unite_(TEMPORADA, "ESTRATO3", c("ORIGEN","REGION"))}
    if(input$ESTRATO_LI=="ZONAS") {TEMPORADA<-unite_(TEMPORADA, "ESTRATO3", c("ORIGEN","MES"))}  
    rownames(TEMPORADA) <-as.character(TEMPORADA[2][,1])
    TEMPORADA<-TEMPORADA[,3:dim(TEMPORADA)[2]]
    TEMPORADA_2<-TEMPORADA
    hc1 <- hclust(dist(TEMPORADA_1), "ave")
    hc2 <- hclust(dist(TEMPORADA_2), "ave")
    p<-tanglegram(hc1,hc2,k_labels = input$GRUPO_K2, k_branches = input$GRUPO_K2,
                  sort=TRUE,common_subtrees_color_branches = TRUE,main="Tanglegrama")      
     

  })  

  plotInput2 <- function()({
    FILTRA<-filter(MAESTRO_4,ANO<2016)
    FILTRA<-filter(FILTRA,ANO>2011)
    if(input$FRUTA_3==1){FILTRADO1<-FILTRA}
    if(input$FRUTA_3==2){FILTRADO1<-filter(FILTRA,ORIGEN=="I")}
    if(input$FRUTA_3==3){FILTRADO1<-filter(FILTRA,ORIGEN=="N")}
    if(input$MOMENTO2==1){FILTRADO<-FILTRADO1}
    if(input$MOMENTO2==2){FILTRADO<-filter(FILTRADO1,VENTAS=="Cosecha")}
    if(input$MOMENTO2==3){FILTRADO<-filter(FILTRADO1,VENTAS=="Postcosecha")}
    FIN_2<-dim(COMBO_MES)[1]
    COMBO_MES$MES2<-1:FIN_2
    NUEVO_2<-left_join(FILTRADO,COMBO_MES[,c(3:5)],fill = 0)
    #### DEFINE LAS VARIABLES
    TEMPO<-NUEVO_2
    TEMPO$UNIDAD<-TEMPO$ANO
    if(input$ESTRATO_LI=="MESES") {RESPUESTA<-"M"}
    if(input$ESTRATO_LI=="ZONAS") {RESPUESTA<-"R"}  
    TEMPO$RESPUESTA<-RESPUESTA
    ### CONSTRUYE LA VARIABLE PARA CRUZAR
    if(input$ESTRATO_LI=="MESES") {TEMPO<-unite_(TEMPO, "ESTRATO2", c("RESPUESTA","MES"))}
    if(input$ESTRATO_LI=="ZONAS") {TEMPO<-unite_(TEMPO, "ESTRATO2", c("RESPUESTA","REGION"))}  
    #### GENERA LOS GRUPOS PARA EL CRUCE
    if(input$ESTRATO_LI=="MESES") {TEMPO2<-TEMPO %>% group_by(UNIDAD,ORIGEN,REGION,ESTRATO2) %>% summarise(mean(KILOS))}
    if(input$ESTRATO_LI=="ZONAS") {TEMPO2<-TEMPO %>% group_by(UNIDAD,ORIGEN,MES,ESTRATO2) %>% summarise(mean(KILOS))}  
    #### CAMBIAR LA VARIABLE DENTRO DE mean PARA QUE COINCIDA CON RESPUESTA
    COLA<-dim(TEMPO2)[2]
    names(TEMPO2)[COLA]<-"TOTAL"
    ###### REALIZA EL CRUCE
    TEMPO3<-TEMPO2 %>% spread(ESTRATO2,TOTAL,fill = 0)
    TEMPO3<-as.data.frame(TEMPO3)
    TEMPO3_1<-TEMPO3
    if(input$ESTRATO_LI=="MESES") {TEMPO3_1<-unite_(TEMPO3_1, "ESTRATO3", c("UNIDAD","ORIGEN","REGION"))}
    if(input$ESTRATO_LI=="ZONAS") {TEMPO3_1<-unite_(TEMPO3_1, "ESTRATO3", c("UNIDAD","ORIGEN","MES"))}  
    rownames(TEMPO3_1) <-as.character(TEMPO3_1[1][,1])
    TEMPO3_2<-TEMPO3_1[,2:dim(TEMPO3_1)[2]]
    ################ APLICA LOS FILTROS  #################
    ######################################################
    TEMPORADA<-filter(TEMPO3,UNIDAD==input$E_MODELO1LI)
    if(input$ESTRATO_LI=="MESES") {TEMPORADA<-unite_(TEMPORADA, "ESTRATO3", c("ORIGEN","REGION"))}
    if(input$ESTRATO_LI=="ZONAS") {TEMPORADA<-unite_(TEMPORADA, "ESTRATO3", c("ORIGEN","MES"))}  
    rownames(TEMPORADA) <-as.character(TEMPORADA[2][,1])
    TEMPORADA<-TEMPORADA[,3:dim(TEMPORADA)[2]]
    TEMPORADA_1<-TEMPORADA
    TEMPORADA<-filter(TEMPO3,UNIDAD==input$E_MODELO2LI)
    if(input$ESTRATO_LI=="MESES") {TEMPORADA<-unite_(TEMPORADA, "ESTRATO3", c("ORIGEN","REGION"))}
    if(input$ESTRATO_LI=="ZONAS") {TEMPORADA<-unite_(TEMPORADA, "ESTRATO3", c("ORIGEN","MES"))}  
    rownames(TEMPORADA) <-as.character(TEMPORADA[2][,1])
    TEMPORADA<-TEMPORADA[,3:dim(TEMPORADA)[2]]
    TEMPORADA_2<-TEMPORADA
    hc1 <- hclust(dist(TEMPORADA_1), "ave")
    hc2 <- hclust(dist(TEMPORADA_2), "ave")
    tanglegram(hc1,hc2,k_labels = input$GRUPO_K2, k_branches = input$GRUPO_K2,
               sort=TRUE,common_subtrees_color_branches = TRUE,main="Tanglegrama")      
  })  
  
  plotInput3 <- function() ({
    FIN_2<-dim(COMBO_MES)[1]
    COMBO_MES$MES2<-1:FIN_2
    if(input$MOMENTO==1){FILTRADO<-MAESTRO_1}
    if(input$MOMENTO==2){FILTRADO<-filter(MAESTRO_1,VENTA=="Cosecha")}
    if(input$MOMENTO==3){FILTRADO<-filter(MAESTRO_1,VENTA=="Poscosecha")}
    NUEVO_2<-left_join(FILTRADO,COMBO_MES[,c(3:5)],fill = 0)
    NUEVO_2$EMPRESA<-as.factor(NUEVO_2$EMPRESA)
    # GENERA CODIGOS PARA PRODUCTOR LOS INCORPORA A LA BASE NUEVO_1
    EMPRESAS<-as.data.frame(unique(NUEVO_2$EMPRESA))
    names(EMPRESAS)<-"EMPRESA"
    LIMITE<-dim(EMPRESAS)[1]
    EMPRESAS$N_ENTIDAD<-1:LIMITE
    NUEVO_2<-left_join(NUEVO_2,EMPRESAS)
    #### DEFINE LAS VARIABLES
    TEMPO<-NUEVO_2
    TEMPO$UNIDAD<-TEMPO$TEMPORADA
    if(input$ESTRATO=="MESES") {RESPUESTA<-"ZONA"}
    if(input$ESTRATO=="ZONAS") {RESPUESTA<-"MES"}  
    TEMPO$RESPUESTA<-RESPUESTA
    ### CONSTRUYE LA VARIABLE PARA CRUZAR
    if(input$ESTRATO=="MESES") {TEMPO<-unite_(TEMPO, "ESTRATO2", c("RESPUESTA","ZONA_DESTINO"))}
    if(input$ESTRATO=="ZONAS") {TEMPO<-unite_(TEMPO, "ESTRATO2", c("RESPUESTA","MES"))}  
    #### GENERA LOS GRUPOS PARA EL CRUCE
    if(input$ESTRATO=="MESES") {TEMPO2<-TEMPO %>% group_by(UNIDAD,MES,ESTRATO2) %>% summarise(mean(KILOS))}
    if(input$ESTRATO=="ZONAS") {TEMPO2<-TEMPO %>% group_by(UNIDAD,ZONA_DESTINO,ESTRATO2) %>% summarise(mean(KILOS))}  
    #### CAMBIAR LA VARIABLE DENTRO DE mean PARA QUE COINCIDA CON RESPUESTA
    COLA<-dim(TEMPO2)[2]
    names(TEMPO2)[4]<-"TOTAL"
    ###### REALIZA EL CRUCE
    TEMPO3<-TEMPO2 %>% spread(ESTRATO2,TOTAL,fill = 0)
    TEMPO3<-as.data.frame(TEMPO3)
    TEMPO3_1<-TEMPO3
    if(input$ESTRATO=="MESES") {TEMPO3_1<-unite_(TEMPO3_1, "ESTRATO3", c("UNIDAD","MES"))}
    if(input$ESTRATO=="ZONAS") {TEMPO3_1<-unite_(TEMPO3_1, "ESTRATO3", c("UNIDAD","ZONA_DESTINO"))}  
    rownames(TEMPO3_1) <-as.character(TEMPO3_1[1][,1])
    TEMPO3_2<-TEMPO3_1[,2:dim(TEMPO3_1)[2]]
    ################ APLICA LOS FILTROS  #################
    ######################################################
    TEMPORADA<-filter(TEMPO3,UNIDAD==input$E_MODELO1)
    rownames(TEMPORADA) <-as.character(TEMPORADA[2][,1])
    TEMPORADA<-TEMPORADA[,3:dim(TEMPORADA)[2]]
    TEMPORADA_1<-TEMPORADA
    TEMPORADA<-filter(TEMPO3,UNIDAD==input$E_MODELO2)
    rownames(TEMPORADA) <-as.character(TEMPORADA[2][,1])
    TEMPORADA<-TEMPORADA[,3:dim(TEMPORADA)[2]]
    TEMPORADA_2<-TEMPORADA
    hc1 <- hclust(dist(TEMPORADA_1), "ave")
    hc2 <- hclust(dist(TEMPORADA_2), "ave")
    GRAFICO<-tanglegram(hc1,hc2,k_labels = input$GRUPO_K, k_branches = input$GRUPO_K,sort=TRUE,common_subtrees_color_branches = TRUE,intersecting=TRUE)
    print(entanglement(GRAFICO)) 
  })  
  
  output$heatmap2LI <- renderPlot({
    print(plotInput())
    })
  
  output$heatmapLI <- renderD3heatmap({
    FILTRA<-filter(MAESTRO_4,ANO<2016)
    FILTRA<-filter(FILTRA,ANO>2011)
    if(input$FRUTA_2==1){FILTRADO1<-FILTRA}
    if(input$FRUTA_2==2){FILTRADO1<-filter(FILTRA,ORIGEN=="I")}
    if(input$FRUTA_2==3){FILTRADO1<-filter(FILTRA,ORIGEN=="N")}
    if(input$MOMENTO3==1){FILTRADO<-FILTRADO1}
    if(input$MOMENTO3==2){FILTRADO<-filter(FILTRADO1,VENTAS=="Cosecha")}
    if(input$MOMENTO3==3){FILTRADO<-filter(FILTRADO1,VENTAS=="Postcosecha")}
    FIN_2<-dim(COMBO_MES)[1]
    COMBO_MES$MES2<-1:FIN_2
    NUEVO_2<-left_join(FILTRADO,COMBO_MES[,c(3:5)],fill = 0)
    #### DEFINE LAS VARIABLES
    TEMPO<-NUEVO_2
    TEMPO$UNIDAD<-TEMPO$ANO
    if(input$ESTRATO_LI2=="MESES") {RESPUESTA<-"M"}
    if(input$ESTRATO_LI2=="ZONAS") {RESPUESTA<-"R"}  
    TEMPO$RESPUESTA<-RESPUESTA
    ### CONSTRUYE LA VARIABLE PARA CRUZAR
    if(input$ESTRATO_LI2=="MESES") {TEMPO<-unite_(TEMPO, "ESTRATO2", c("RESPUESTA","MES"))}
    if(input$ESTRATO_LI2=="ZONAS") {TEMPO<-unite_(TEMPO, "ESTRATO2", c("RESPUESTA","REGION"))}  
        #### GENERA LOS GRUPOS PARA EL CRUCE
    if(input$ESTRATO_LI2=="MESES") {TEMPO2<-TEMPO %>% group_by(UNIDAD,ORIGEN,REGION,ESTRATO2) %>% summarise(mean(KILOS))}
    if(input$ESTRATO_LI2=="ZONAS") {TEMPO2<-TEMPO %>% group_by(UNIDAD,ORIGEN,MES,ESTRATO2) %>% summarise(mean(KILOS))}  
    #### CAMBIAR LA VARIABLE DENTRO DE mean PARA QUE COINCIDA CON RESPUESTA
    COLA<-dim(TEMPO2)[2]
    names(TEMPO2)[COLA]<-"TOTAL"
    ###### REALIZA EL CRUCE
    TEMPO3<-TEMPO2 %>% spread(ESTRATO2,TOTAL,fill = 0)
    TEMPO3<-as.data.frame(TEMPO3)
    TEMPO3_1<-TEMPO3
    if(input$ESTRATO_LI2=="MESES") {TEMPO3_1<-unite_(TEMPO3_1, "ESTRATO3", c("UNIDAD","ORIGEN","REGION"))}
    if(input$ESTRATO_LI2=="ZONAS") {TEMPO3_1<-unite_(TEMPO3_1, "ESTRATO3", c("UNIDAD","ORIGEN","MES"))}  
    rownames(TEMPO3_1) <-as.character(TEMPO3_1[1][,1])
    TEMPO3_2<-TEMPO3_1[,2:dim(TEMPO3_1)[2]]
    ############## APLICA EL GRAFICO FINAL
    if(input$TRANSPUESTO) INGRESA<-t(TEMPO3_2) else INGRESA<-TEMPO3_2 
    d3heatmap(INGRESA, scale="column", colors="Blues", k_row = input$GRUPO_KF1, 
              k_col = input$GRUPO_KC1,yaxis_width =100,xaxis_height = 100)
      }) 
  
  output$localPlotLI <- renderPlotly({
    FILTRA<-filter(MAESTRO_4,ANO<2016)
    FILTRA<-filter(FILTRA,ANO>2011)
    if(input$FRUTA_1==1){FILTRADO1<-FILTRA}
    if(input$FRUTA_1==2){FILTRADO1<-filter(FILTRA,ORIGEN=="I")}
    if(input$FRUTA_1==3){FILTRADO1<-filter(FILTRA,ORIGEN=="N")}
    if(input$MOMENTO4==1){FILTRADO<-FILTRADO1}
    if(input$MOMENTO4==2){FILTRADO<-filter(FILTRADO1,VENTAS=="Cosecha")}
    if(input$MOMENTO4==3){FILTRADO<-filter(FILTRADO1,VENTAS=="Postcosecha")}
    FIN_2<-dim(COMBO_MES)[1]
    COMBO_MES$MES2<-1:FIN_2
    NUEVO_2<-left_join(FILTRADO,COMBO_MES[,c(3:5)],fill = 0)
    #### DEFINE LAS VARIABLES
    TEMPO<-NUEVO_2
    TEMPO$UNIDAD<-TEMPO$ANO
    if(input$ESTRATO_LI3=="MESES") {RESPUESTA<-"M"}
    if(input$ESTRATO_LI3=="ZONAS") {RESPUESTA<-"R"}  
    TEMPO$RESPUESTA<-RESPUESTA
    ### CONSTRUYE LA VARIABLE PARA CRUZAR
    if(input$ESTRATO_LI3=="MESES") {TEMPO<-unite_(TEMPO, "ESTRATO2", c("RESPUESTA","MES"))}
    if(input$ESTRATO_LI3=="ZONAS") {TEMPO<-unite_(TEMPO, "ESTRATO2", c("RESPUESTA","REGION"))}  
    #### GENERA LOS GRUPOS PARA EL CRUCE
    if(input$ESTRATO_LI3=="MESES") {TEMPO2<-TEMPO %>% group_by(UNIDAD,ORIGEN,REGION,ESTRATO2) %>% summarise(mean(KILOS))}
    if(input$ESTRATO_LI3=="ZONAS") {TEMPO2<-TEMPO %>% group_by(UNIDAD,ORIGEN,MES,ESTRATO2) %>% summarise(mean(KILOS))}  
    #### CAMBIAR LA VARIABLE DENTRO DE mean PARA QUE COINCIDA CON RESPUESTA
    COLA<-dim(TEMPO2)[2]
    names(TEMPO2)[COLA]<-"TOTAL"
    ###### REALIZA EL CRUCE
    TEMPO3<-TEMPO2 %>% spread(ESTRATO2,TOTAL,fill = 0)
    TEMPO3<-as.data.frame(TEMPO3)
    TEMPO3_1<-TEMPO3
    if(input$ESTRATO_LI3=="MESES") {TEMPO3_1<-unite_(TEMPO3_1, "ESTRATO3", c("UNIDAD","ORIGEN","REGION"))}
    if(input$ESTRATO_LI3=="ZONAS") {TEMPO3_1<-unite_(TEMPO3_1, "ESTRATO3", c("UNIDAD","ORIGEN","MES"))}  
    rownames(TEMPO3_1) <-as.character(TEMPO3_1[1][,1])
    TEMPO3_2<-TEMPO3_1[,2:dim(TEMPO3_1)[2]]
    ##### identifica los clusters
    hc <- hclust(dist(TEMPO3_2), "ave")
    GRUPO=cutree(hc,k=input$N_CLUSTER) 
    GRUPO<-as.data.frame(GRUPO)
    GRUPO$ESTRATO3<-rownames(GRUPO)
    GRUPO$ESTRATO3<-as.factor(GRUPO$ESTRATO3)
    TEMPO3_1$ESTRATO3<-as.factor(TEMPO3_1$ESTRATO3)
    TEMPO5<-left_join(GRUPO, TEMPO3_1)
    rownames(TEMPO5) <-as.character(TEMPO5[2][,1])
    TEMPO5<-TEMPO5[,c(1,3:dim(TEMPO5)[2])]
    V_CP1<-summary(prcomp(TEMPO5[,2:dim(TEMPO5)[2]], scale = TRUE))$importance[2]*100
    V_CP2<-summary(prcomp(TEMPO5[,2:dim(TEMPO5)[2]], scale = TRUE))$importance[5]*100
    Var_CP1<-paste("Primer Componente (",V_CP1,"%)",sep="")
    Var_CP2<-paste("Segunda Componente (",V_CP2,"%)",sep="")
    p<-autoplot(prcomp(TEMPO5[,2:dim(TEMPO5)[2]],scale=TRUE),
                label = TRUE, colour = TEMPO5$GRUPO, label.size = 3,
                loadings = TRUE, loadings.colour = 'blue',loadings.label = TRUE, 
                loadings.label.size = 4,shape = TRUE, frame = FALSE,
                xlab =Var_CP1,ylab=Var_CP2,main="Análisis de Componentes Principales")
    (gg <- ggplotly(p))
  })
  
  output$motionchart <- renderGvis({
    if (is.null(input$ORIGEN_SN)) return()
    MAESTRO_6$FECHA<-date(MAESTRO_6$FECHA)
    DATOS1<-filter(MAESTRO_6,COMPLETO=="SI")
    NEE_ANO<-input$ANO_S
    DATOS<-filter(DATOS1,ANO_L %in% NEE_ANO)
    NEE_REGION<-input$REGION_SN
    DATOS <- filter(DATOS, REGION %in% NEE_REGION)
    NEE_ORIGEN<-input$ORIGEN_SN
    DATOS <- filter(DATOS, ORIGEN %in% NEE_ORIGEN)
    TOTO<-DATOS1 %>%
      group_by(ORIGEN, REGION,MES) %>%
      select(KILOS, PRECIO) %>%
      summarise(
        KILOS = mean(KILOS, na.rm = TRUE),
        PRECIO = mean(PRECIO, na.rm = TRUE)) 
    TOTO$ANO_L<-"Promedio"
    TOTO <- filter(TOTO, REGION %in% NEE_REGION)
    TOTO <- filter(TOTO, ORIGEN %in% NEE_ORIGEN)
    TOTO_2<-full_join(DATOS,TOTO)
    TOTO_2$IDENT3<-paste(TOTO_2$ORIGEN,TOTO_2$REGION,TOTO_2$ANO_L,sep="-")
    TOTO_2$IDENT4<-paste(TOTO_2$ORIGEN,TOTO_2$REGION,TOTO_2$ANO_L,sep="-")
    TOTO_2$ORIGEN2<-if_else(TOTO_2$ORIGEN=="MEX",1,2)
    ANO1<-filter(DATOS1,ANO_L %in% NEE_ANO)
    ANO2<-unique(ANO1$ANO)
    TOTO_2$MES2<-as.Date(ISOdatetime(as.numeric(ANO2),month=TOTO_2$MES,min=0,hour=0,sec=0,day=1))
    return(gvisMotionChart(TOTO_2,sizevar = "ORIGEN2",
                          idvar="IDENT3",timevar="MES2",colorvar="IDENT4",
                           xvar = "KILOS", yvar = "PRECIO"))
  })  
  
  output$Guarda_1 <- downloadHandler(
    filename = function() {
      paste("grafico-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      png(file)
      print(plotInput2())
      dev.off()
    })      
  
  output$Guarda_2 <- downloadHandler(
    filename = function() {
      paste("grafico-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      png(file)
      print(plotInput3())
      dev.off()
    })      
  
  output$Guarda_3 <- downloadHandler(
    filename = function() {
      paste("grafico-", Sys.Date(), ".html", sep="")
    },
    content = function(file) {
        FILTRA<-filter(MAESTRO_4,ANO<2016)
        FILTRA<-filter(FILTRA,ANO>2011)
        if(input$FRUTA_2==1){FILTRADO1<-FILTRA}
        if(input$FRUTA_2==2){FILTRADO1<-filter(FILTRA,ORIGEN=="I")}
        if(input$FRUTA_2==3){FILTRADO1<-filter(FILTRA,ORIGEN=="N")}
        if(input$MOMENTO3==1){FILTRADO<-FILTRADO1}
        if(input$MOMENTO3==2){FILTRADO<-filter(FILTRADO1,VENTAS=="Cosecha")}
        if(input$MOMENTO3==3){FILTRADO<-filter(FILTRADO1,VENTAS=="Postcosecha")}
        FIN_2<-dim(COMBO_MES)[1]
        COMBO_MES$MES2<-1:FIN_2
        NUEVO_2<-left_join(FILTRADO,COMBO_MES[,c(3:5)],fill = 0)
        #### DEFINE LAS VARIABLES
        TEMPO<-NUEVO_2
        TEMPO$UNIDAD<-TEMPO$ANO
        if(input$ESTRATO_LI2=="MESES") {RESPUESTA<-"M"}
        if(input$ESTRATO_LI2=="ZONAS") {RESPUESTA<-"R"}  
        TEMPO$RESPUESTA<-RESPUESTA
        ### CONSTRUYE LA VARIABLE PARA CRUZAR
        if(input$ESTRATO_LI2=="MESES") {TEMPO<-unite_(TEMPO, "ESTRATO2", c("RESPUESTA","MES"))}
        if(input$ESTRATO_LI2=="ZONAS") {TEMPO<-unite_(TEMPO, "ESTRATO2", c("RESPUESTA","REGION"))}  
        #### GENERA LOS GRUPOS PARA EL CRUCE
        if(input$ESTRATO_LI2=="MESES") {TEMPO2<-TEMPO %>% group_by(UNIDAD,ORIGEN,REGION,ESTRATO2) %>% summarise(mean(KILOS))}
        if(input$ESTRATO_LI2=="ZONAS") {TEMPO2<-TEMPO %>% group_by(UNIDAD,ORIGEN,MES,ESTRATO2) %>% summarise(mean(KILOS))}  
        #### CAMBIAR LA VARIABLE DENTRO DE mean PARA QUE COINCIDA CON RESPUESTA
        COLA<-dim(TEMPO2)[2]
        names(TEMPO2)[COLA]<-"TOTAL"
        ###### REALIZA EL CRUCE
        TEMPO3<-TEMPO2 %>% spread(ESTRATO2,TOTAL,fill = 0)
        TEMPO3<-as.data.frame(TEMPO3)
        TEMPO3_1<-TEMPO3
        if(input$ESTRATO_LI2=="MESES") {TEMPO3_1<-unite_(TEMPO3_1, "ESTRATO3", c("UNIDAD","ORIGEN","REGION"))}
        if(input$ESTRATO_LI2=="ZONAS") {TEMPO3_1<-unite_(TEMPO3_1, "ESTRATO3", c("UNIDAD","ORIGEN","MES"))}  
        rownames(TEMPO3_1) <-as.character(TEMPO3_1[1][,1])
        TEMPO3_2<-TEMPO3_1[,2:dim(TEMPO3_1)[2]]
        ############## APLICA EL GRAFICO FINAL
        if(input$TRANSPUESTO) INGRESA<-t(TEMPO3_2) else INGRESA<-TEMPO3_2 
        AA<-  d3heatmap(INGRESA, scale="column", colors="Blues", 
                      k_row = input$GRUPO_KF1, k_col = input$GRUPO_KC1,yaxis_width =100,
                      xaxis_height = 100)
      saveWidget(AA,file)
  
    })
  
  output$Guarda_4 <- downloadHandler(
    filename = function() {
      paste("grafico-", Sys.Date(), ".html", sep="")
    },
    content = function(file) {
      FIN_2<-dim(COMBO_MES)[1]
      COMBO_MES$MES2<-1:FIN_2
      NUEVO_2<-left_join(MAESTRO_1,COMBO_MES[,c(3:5)],fill = 0)
      NUEVO_2$EMPRESA<-as.factor(NUEVO_2$EMPRESA)
      # GENERA CODIGOS PARA PRODUCTOR LOS INCORPORA A LA BASE NUEVO_1
      EMPRESAS<-as.data.frame(unique(NUEVO_2$EMPRESA))
      names(EMPRESAS)<-"EMPRESA"
      LIMITE<-dim(EMPRESAS)[1]
      EMPRESAS$N_ENTIDAD<-1:LIMITE
      NUEVO_2<-left_join(NUEVO_2,EMPRESAS)
      #### DEFINE LAS VARIABLES
      TEMPO<-NUEVO_2
      RESPUESTA<-"MES"
      TEMPO$RESPUESTA<-RESPUESTA
      TEMPO$UNIDAD<-TEMPO$TEMPORADA
      ### CONSTRUYE LA VARIABLE PARA CRUZAR
      TEMPO<-unite_(TEMPO, "ESTRATO2", c("RESPUESTA","MES"))
      #### GENERA LOS GRUPOS PARA EL CRUCE
      #### CAMBIAR LA VARIABLE DENTRO DE mean PARA QUE COINCIDA CON RESPUESTA
      TEMPO2<-TEMPO %>% group_by(UNIDAD,ZONA_DESTINO,ESTRATO2) %>% summarise(mean(KILOS))
      names(TEMPO2)[4]<-"TOTAL"
      ###### REALIZA EL CRUCE
      TEMPO3<-TEMPO2 %>% spread(ESTRATO2,TOTAL,fill = 0)
      TEMPO3<-as.data.frame(TEMPO3)
      TEMPO3_1<-TEMPO3
      TEMPO3_1<-unite_(TEMPO3_1, "ESTRATO3", c("UNIDAD","ZONA_DESTINO"))
      rownames(TEMPO3_1) <-as.character(TEMPO3_1[1][,1])
      TEMPO3_2<-TEMPO3_1[,2:dim(TEMPO3_1)[2]]
      AA<-d3heatmap(TEMPO3_2, scale="column", colors="Blues", k_row = 5, 
                    k_col = 5,yaxis_width =100,xaxis_height = 100)

      saveWidget(AA,file)
          })
      
  output$Serie1 <- renderHighchart({
    if (is.null(input$ORIGEN_SN2)) return()
    DATOS1<-filter(MAESTRO_6,COMPLETO=="SI")
    DATOS<-filter(DATOS1,Ano==levels(as.factor(input$ANO_S2)))
    NEE_REGION<-as.character(levels(as.factor(input$REGION_SN2)))
    DATOS <- filter(DATOS, Region==NEE_REGION)
    NEE_ORIGEN<-as.character(levels(as.factor(input$ORIGEN_SN)))
    DATOS <- filter(DATOS, Origen==NEE_ORIGEN)
    DATOS$Ano<-as.character(DATOS$Ano)
    TOTO<-DATOS1 %>%
      group_by(Origen, Region,Mes) %>%
      select(Kilos, Precio) %>%
      summarise(
        Kilos = mean(Kilos, na.rm = TRUE),
        Precio = mean(Precio, na.rm = TRUE)) 
    TOTO$Ano<-"Promedio"
    TOTO <- filter(TOTO, Region==NEE_REGION)
    TOTO <- filter(TOTO, Origen==NEE_ORIGEN)
    TOTO_2<-full_join(DATOS,TOTO)
    TOTO_2$Ident3<-paste(TOTO_2$Origen,TOTO_2$Region,TOTO_2$Ano,sep="-")
    TOTO_2$Ident4<-paste(TOTO_2$Origen,TOTO_2$Region,TOTO_2$Ano,sep="-")
    TOTO_2$Origen2<-if_else(TOTO_2$Origen=="MEX",1,2)
    TOTO_2$Mes2<-as.Date(ISOdatetime(as.numeric(input$ANO_S),month=TOTO_2$Mes,min=0,hour=0,sec=0,day=1))
    TOTO_2$Fecha2 <- months(TOTO_2$Fecha)
    highchart() %>%     
      hc_xAxis(categories = TOTO_2$Fecha2) %>% 
      hc_yAxis_multiples(
        list(top = "0%", height = "30%", lineWidth = 3),
        list(top = "30%", height = "70%", offset = 0,
             showFirstLabel = FALSE, showLastLabel = FALSE)) %>% 
      hc_add_series(name = "Precio", data = TOTO_2$Precio) %>% 
      hc_add_series(name = "Kilos", data = TOTO_2$Kilos, yAxis = 1)
  })    

#  output$Serie2 <- renderPlotly({
   output$Serie2 <- renderPlot({
    if (is.null(input$ORIGEN_SN2)) return()

    DATOS1<-filter(MAESTRO_6,COMPLETO=="SI")
    DATOS1$FECHA2 <- month(DATOS1$FECHA)
    DATOS1$MOMENTO<-ifelse(DATOS1$FECHA2<8,"Postcosecha","Cosecha") 
    NEE_ANO<-input$ANO_S2
    DATOS<-filter(DATOS1,ANO_L %in% NEE_ANO)
    NEE_REGION<-input$REGION_SN2
    DATOS <- filter(DATOS, REGION %in% NEE_REGION)
    NEE_ORIGEN<-input$ORIGEN_SN2
    DATOS <- filter(DATOS, ORIGEN %in% NEE_ORIGEN)
    DATOS$FECHA2 <- month(DATOS$FECHA)
    TOTO<-DATOS1 %>%
      group_by(ORIGEN, REGION,FECHA2,MOMENTO) %>%
      select(KILOS, PRECIO) %>%
      summarise(
        KILOS = mean(KILOS, na.rm = TRUE),
        PRECIO = mean(PRECIO, na.rm = TRUE)) 
    TOTO$ANO_L<-"Promedio"
    TOTO <- filter(TOTO, REGION %in% NEE_REGION)
    TOTO <- filter(TOTO, ORIGEN %in% NEE_ORIGEN)
    TOTO_2<-full_join(DATOS,TOTO)
    TOTO_2$IDENT3<-paste(TOTO_2$ORIGEN,TOTO_2$REGION,TOTO_2$ANO_L,sep="-")
    TOTO_2$IDENT4<-paste(TOTO_2$ORIGEN,TOTO_2$REGION,TOTO_2$ANO_L,sep="-")
    TOTO_2$ORIGEN2<-if_else(TOTO_2$ORIGEN=="MEX",1,2)
    LARGO<-melt(TOTO_2,id=c("ANO_L","FECHA2","ORIGEN","REGION","IDENT3","MOMENTO"),measure=c("KILOS","PRECIO"))
    p<-ggplot(aes(FECHA2,value),data=LARGO,group=IDENT3,xlab="Meses")+geom_point()+
    geom_smooth(se = FALSE,span = input$SPAN)+ scale_y_continuous(breaks = 1:12) + facet_grid(variable~MOMENTO,scales="free")+
      theme(strip.text.x = element_text(size=12, face="bold"),
            strip.text.y = element_text(size=12, face="bold"),
            strip.background = element_rect(colour="red", fill="#CCCCFF"))+
               aes(color=IDENT3)
    print(p)
#    ggplotly(p)
  })    

  output$REGION_S <- renderUI({
    if (is.null(input$ANO_S)) return("    ")
    DATOS<-filter(MAESTRO_6,COMPLETO=="SI")
    ANOS<-input$ANO_S
    DATOS<-filter(DATOS,ANO_L %in% ANOS)
    NEE_REGION<-levels(as.factor(DATOS$REGION))
    selectInput("REGION_SN",label = h5("Region:"),multiple = TRUE,
                choices=NEE_REGION,
                helpText("Elija"))    
  })
  
  output$ORIGEN_S <- renderUI({
    if (is.null(input$REGION_SN)) return("    ")
    DATOS<-filter(MAESTRO_6,COMPLETO=="SI")
    ANOS<-input$ANO_S
    DATOS<-filter(DATOS,ANO_L %in% ANOS)
    NEE_REGION<-input$REGION_SN
    DATOS <- filter(DATOS, REGION %in% NEE_REGION)
    NNE_ORIGEN<-levels(as.factor(DATOS$ORIGEN))
    selectInput("ORIGEN_SN",label = h5("Origen:"),
                choices = NNE_ORIGEN, 
                multiple = TRUE,
                helpText("Elija"))    
  })
  
  output$REGION_S2 <- renderUI({
    if (is.null(input$ANO_S2)) return("    ")
    DATOS<-filter(MAESTRO_6,COMPLETO=="SI")
    ANOS<-input$ANO_S2
    DATOS<-filter(DATOS,ANO_L %in% ANOS)
    NEE_REGION<-levels(as.factor(DATOS$REGION))
    selectInput("REGION_SN2",label = h5("Region:"),multiple = TRUE,
                choices=NEE_REGION,
                helpText("Elija"))    
  })
  
    output$ORIGEN_S2 <- renderUI({
    if (is.null(input$REGION_SN2)) return("    ")
    DATOS<-filter(MAESTRO_6,COMPLETO=="SI")
    ANOS<-input$ANO_S2
    DATOS<-filter(DATOS,ANO_L %in% ANOS)
    NEE_REGION<-input$REGION_SN2
    DATOS <- filter(DATOS, REGION %in% NEE_REGION)
    NNE_ORIGEN<-levels(as.factor(DATOS$ORIGEN))
    selectInput("ORIGEN_SN2",label = h5("Origen:"),
                choices = NNE_ORIGEN, 
                multiple = TRUE,
                helpText("Elija"))    
  })

  output$table <- DT::renderDataTable(DT::datatable({
    
    ANOS<-input$M_ANOS
    FILTRA<-filter(MAESTRO_4,ANO  %in% ANOS)
    ORIGENES<-input$M_ORIGENES
    FILTRA<-filter(FILTRA,ORIGEN  %in% ORIGENES)
    MESES<-input$M_MESES
    FILTRA<-filter(FILTRA,MES  %in% MESES)
    FILTRA$REGION<-as.factor(FILTRA$REGION)
    
    TOTO<-FILTRA %>%
      group_by(REGION) %>%
      select(KILOS) %>%
      summarise(
        value = mean(KILOS, na.rm = TRUE)) 
    TOTO1<-as.data.frame(TOTO)
    REGION_G1<-as.data.frame(REGION_G)
    TOTO1$Region_G<-as.character(TOTO1$REGION)
    REGION_G1$Region_G<-as.character(REGION_G1$Region_G)
    TOTO2<-left_join(REGION_G1,TOTO1,by="Region_G")
    TOTO3<-TOTO2[,c(3:5)]
    TOTO3$value[is.na(TOTO3$value)] <- 0
    MEXICO_0<-df_mxstate
    MEXICO<-full_join(MEXICO_0,TOTO3)
    MEXICO$value[is.na(MEXICO$value)] <- 0
    MEXICO
    
  },extensions = 'Scroller', options = list(deferRender = TRUE,dom = "frtiS",
                                           scrollY = 200,scrollX = TRUE,scrollCollapse = TRUE)))  

  output$mapa <- renderPlot({
#  output$map <- renderLeaflet({
    ANOS<-input$M_ANOS
    FILTRA<-filter(MAESTRO_4,ANO  %in% ANOS)
    ORIGENES<-input$M_ORIGENES
    FILTRA<-filter(FILTRA,ORIGEN  %in% ORIGENES)
    MESES<-input$M_MESES
    FILTRA<-filter(FILTRA,MES  %in% MESES)
    FILTRA$REGION<-as.factor(FILTRA$REGION)
    TOTO<-FILTRA %>%
      group_by(REGION) %>%
      select(KILOS) %>%
      summarise(
        value = mean(KILOS, na.rm = TRUE)) 
    TOTO1<-as.data.frame(TOTO)
    REGION_G1<-as.data.frame(REGION_G)
    TOTO1$Region_G<-as.character(TOTO1$REGION)
    REGION_G1$Region_G<-as.character(REGION_G1$Region_G)
    TOTO2<-left_join(REGION_G1,TOTO1,by="Region_G")
    TOTO3<-TOTO2[,c(3:5)]
    TOTO3$value[is.na(TOTO3$value)] <- 0
    MEXICO_0<-df_mxstate
    MEXICO<-full_join(MEXICO_0,TOTO3)
    MEXICO$value[is.na(MEXICO$value)] <- 0
    TITULO<-paste("Volumen por Region: Año: ",ANOS,"  -Origen: ",ORIGENES,"  -Mes: ",MESES,sep="")
    PP<-mxstate_choropleth(MEXICO, title = TITULO,num_colors = 7) 
    print(PP)
      })
  
  
  })



