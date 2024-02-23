#Arturo Ramos Viedas - A01636133
#Paola Félix Torres - A00227869

library(shiny)
library(igraph)

#Funcion principal
graficar <- function(partes){
  
  nodos <- c()
  ejes <- c()
  
  for(i in 1:length(partes)){
    
    partes[i] <- gsub(" ", "", gsub("->", "", partes[i]))
    ejes <- c(ejes, substring(partes[i], 2, 2))
    
    if(nchar(partes[i]) == 3)
    {
      
      nodos <- c(nodos, substring(partes[i], 1, 1), substring(partes[i], 3, 3))
      
    }
    else
    {
      
      nodos <- c(nodos, substring(partes[i], 1, 1), "Z")
      
    }
  }
  
  nodos.unique <- unique(nodos)
  nodos.length <- length(nodos.unique)
  nodo.categoria <- rep(1, nodos.length)
  nodo.categoria["S" == nodos.unique] <- 2
  nodo.categoria["Z" == nodos.unique] <- 3
  grafica.colores <- c("gray","green","red")
  nodo.colores <- grafica.colores[nodo.categoria]
  columnas <- list(nodos = nodos, ejes = ejes, nodo.colores = nodo.colores)
  return(columnas)
  
}

#Interfaz de usuario
interfaz <- fluidPage(
  
  title = "Convertidor",
  titlePanel("Convertidor"),
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
           vertex.label.color = "black", 
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
    grafica.colores <- c("gray","green","red")
    legend(x = -1.05, 
           y = 1.5,
           legend = c("Regular", "Inicial", "Final"),
           fill = grafica.colores,
           text.font = 2, 
           bg = "lightblue"
    )
    
  })
}

shinyApp(interfaz, servidor)