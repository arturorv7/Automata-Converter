#Arturo Ramos Viedas - A01636133
#Paola Félix Torres - A00227869

library(shiny)
library(igraph)
library(stringr)

#Funcion principal
graficar <- function(partes){
  
  nodos <- c()
  ejes <- c()
  nodos.inicial <- c()
  
  for(i in 1:length(partes)){
    
    partes[i] <- gsub(" ", "", gsub("[()]", "", partes[i]))[[1]]
    linea <- str_split(partes[i], ",")[[1]]
    nodos <- c(nodos,linea[1],linea[2])
    ejes <- c(ejes,linea[3])
    nodos.inicial <- c(nodos.inicial,linea[1])
  }
  
  alfabeto <- sort(unique(subset(ejes, ejes != "-")))
  nodos.unique <- unique(nodos)
  nodos.length <- length(nodos.unique)
  
  nodo.categoria <- rep(1, nodos.length)
  ejes.categoria <- rep(1, length(ejes))#epsilon repetidos
  nombre.categoria <- rep(1,nodos.length)#faltantes
  
  nodo.categoria["S" == nodos.unique] <- 2
  nodo.categoria[grep("\\.", nodos.unique)] <- 3
  
  for(i in 1:nodos.length){
    ejes.nodo <- ejes[nodos.inicial == nodos.unique[i]]
    ejes.indice <- which(nodos.inicial == nodos.unique[i])
    ejes.categoria[ejes.indice[duplicated(ejes.nodo) | duplicated(ejes.nodo, fromLast = T)]] <- 2
    
    lo.que.tengo<-gsub("-","",unique(ejes.nodo))
    faltan <- !all(alfabeto %in% sort(lo.que.tengo))
    if(faltan){
      nombre.categoria[i]<-2
    }

  }
  ejes.categoria[ejes == "-"] <- 3
  
  nodos.categoria.colores <- c("white","green","red")#Normal, inicial y final
  ejes.categoria.colores <- c("gray","yellow","lightblue")#Normal, epsilon y duplicado
  nombre.categoria.colores <- c("black","purple")#no faltan, faltan
  
  
  nodo.colores <- nodos.categoria.colores[nodo.categoria]
  ejes.colores <- ejes.categoria.colores[ejes.categoria]
  nombres.colores <- nombre.categoria.colores[nombre.categoria]
  
  columnas <- list(nodos = nodos, ejes = ejes, nodo.colores = nodo.colores, ejes.colores = ejes.colores, nombres.colores = nombres.colores)
  return(columnas)
  
}

#Interfaz de usuario
interfaz <- fluidPage(
  
  title = "Convertidor (Opcional)",
  titlePanel("Determinista Checker"),
  sidebarLayout(
    sidebarPanel(
      textAreaInput(inputId = "inputtxt",
                    label = "Input: ",
                    placeholder = "Ingrese su gramatica",
                    height = "500px"
      )
    ),
    
    mainPanel(
      div(style = "display: flex;",
          div(style = "flex:1;",
              plotOutput("outputPlot")),
          div(style = "flex:1.2;",
              plotOutput("dfaPlot")
          )
      )
    )
  )
  
)

#Funciones de plot
servidor <- function(input, output){
  
  output$dfaPlot <- renderPlot({
    gram <- input$inputtxt
    partes <- strsplit(gram, "\n")[[1]]
    partes.length <- length(partes)
    
    #Si existen nodos
    if(partes.length > 0) 
    {
      
      grafica.datos <- graficar(partes)
      grafo.dirijido <- graph(grafica.datos$nodos, directed = TRUE)
      grafo.layout <- layout_with_fr(grafo.dirijido)
      curvas <- curve_multiple(grafo.dirijido)
      plot(grafo.dirijido, 
           edge.label = grafica.datos$ejes, 
           vertex.color = grafica.datos$nodo.colores, 
           vertex.frame.color = "black",
           edge.color = grafica.datos$ejes.colores,
           vertex.label.color = grafica.datos$nombres.colores, 
           edge.arrow.size = .65, 
           edge.curved = curvas, 
           layout = grafo.layout
      )
      
    } 
    #Si no hay nodos
    else 
    {
      
      emptygraph <- make_empty_graph()
      plot(emptygraph)
      
    }
  })
  
  output$outputPlot <- renderPlot({
    
    emptygraph <- make_empty_graph()
    plot(emptygraph)
    grafica.colores <- c("white","green","red", "yellow", "black", "purple")
    legend(x = -1.05, 
           y = 1.5,
           legend = c("Regular", "Inicial", "Final", "Epsilon", "No Faltan Ejes", "Faltan Ejes"),
           fill = grafica.colores,
           text.font = 2, 
           bg = "lightblue"
    )
    
  })
}

shinyApp(interfaz, servidor)