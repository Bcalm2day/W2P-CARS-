#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# SE CARGA LA BASE DE DATOS ORIGINAL A LA CUAL SE HA AÑADIDO PREVIAMENTE LA COLUMNA #51 (RELACION PESO-POTENCIA) DESDE EXCEL Y SE CAMBIA A "Naturally Aspirated" LOS VALORES DE LA COLUMNA "BOOST TYPE" SIN TURBO.
suppressWarnings(library(readxl))
suppressWarnings(coches<-read_excel("C:/Users/dr0070/Desktop/MASTER/DATASETS/Car2DB_eng_cut.xlsx"))
# SE TRANSFORMA LA TABLA DE DATOS EN UN DATA FRAME
coches<-as.data.frame(coches)


# SE CARGA LA BASE DE DATOS ORIGINAL A LA CUAL SE HA AÑADIDO PREVIAMENTE LA COLUMNA #51 (RELACION PESO-POTENCIA) DESDE EXCEL Y SE CAMBIA A "Naturally Aspirated" LOS VALORES DE LA COLUMNA "BOOST TYPE" SIN TURBO.
suppressWarnings(library(readxl))
suppressWarnings(coches<-read_excel("C:/Users/dr0070/Desktop/MASTER/DATASETS/Car2DB_eng_cut.xlsx"))
# SE TRANSFORMA LA TABLA DE DATOS EN UN DATA FRAME
coches<-as.data.frame(coches)


# SE DETECTAN LOS DATOS AUSENTES
# sapply(coches,function(x) sum(is.na(x)))


# SE CAMBIA EL VALOR DE LOS DATOS AUSENTES POR CEROS
for(columna in 1:59)
  if(sum(is.na(coches[,columna])!=0)){
    coches[is.na(coches[,columna]),columna]<-0
  }
# SE VUELVE A COMPROBAR EL NÚMERO DE DATOS AUSENTES
# sapply(coches,function(x) sum(is.na(x)))

# SE HACE UN BUCLE PARA TRANSFORMAR LAS VARIABLES DE TIPO CARACTER EN FACTORES
for (columna in 1:59) {
  if(is.character(coches[,columna])){
    coches[,columna]<-as.factor(coches[,columna])
  }
}

# SE HACE UN BUCLE PARA TRANSFORMAR LOS DATOS NUMERICOS QUE DEBEN SER DE TIPO ENTERO (PESAN MENOS QUE LOS NUMERICOS DE DOBLE PRECISION)
for (columna in c(1:27,30:44,46:49,52)) {
  if(is.double(coches[,columna])){
    coches[,columna]<-as.integer(coches[,columna])
  }
}

# SE DEJAN CON DOS DECIMALES LOS VALORES DE LA VARIABLE PESO-POTENCIA
coches$`Weight to power ratio`<-round(coches$`Weight to power ratio`,digits = 2)

# SE LIMPIAN LOS DATOS QUE NO TIENEN PESO O POTENCIA
cochesfiltrados<-coches[coches$`Weight to power ratio`!=0,]

# SE LIMPIAN LOS DATOS QUE NO VELOCIDAD MAXIMA
cochesfiltrados2<-cochesfiltrados[cochesfiltrados$`Max speed [km/h]`!=0,]

#SE LIMPIAN LOS DATOS QUE NO TIENEN ACELERACION 0-100
cochesfiltrados3<-cochesfiltrados2[cochesfiltrados2$`Acceleration (0-100 km/h) [second]`!=0,]

# SE ORDENA EL DATA FRAME POR LA VARIABLE PESO-POTENCIA DE FORMA ASCENDENTE
consulta<-order(cochesfiltrados3$`Weight to power ratio`,decreasing = F)

# SE GENERA LA TABLA FINAL CON LOS RESULTADOS A MOSTRAR
tablafinal<-head(cochesfiltrados3[consulta,c(2,3,8,5,51,38,36,35,37,31,12,50,52,44,42,40,48)],100000)

library(shiny)



ui <- fluidPage(
    # Application title
    titlePanel("Best W2P cars"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          
          sliderInput("power","Select the engine power [BHP]",min=0, max=1000, value=c(100,200), step = 5),
          
          sliderInput("weight", "Select the weight [KGS]", min=500, max=2000, value=c(1200,1500), step = 10),
          
          sliderInput("weightpower", "Select the weight to power ratio", min=1, max=25, value=c(8,10),step = 0.5),
          
          sliderInput("year", "Select the production years", min=1970, max=2023, value=c(1990,2010)),
      
          checkboxGroupInput("transm","Select transmission", choices = c("Automatic", "Continuously variable transmission (CVT)", "Electronic", "Manual"), selected = c("Manual", "Automatic")),
          
          checkboxGroupInput("traction", "Select traction wheels", choices = c("All wheel drive (AWD)", "Four wheel drive (4WD)", "Front wheel drive","Rear wheel drive"), selected = c("Front wheel drive", "Rear wheel drive")),
          
          checkboxGroupInput("boost", "Select the air induction type", choices = c("Biturbo", "Compressor", "Naturally Aspirated", "Triple turbo",  "Turbine + compressor", "Turbo", "Twin-scroll"), selected = c("Naturally Aspirated", "Turbo")),
          
          checkboxGroupInput("fuel","Select the fuel type", choices = c("CNG", "Disel", "Electric", "Gasoline", "Gasoline, CNG", "Hybrid"), selected = c("Gasoline", "Disel")),
          
          numericInput("maxRPMbhp", "Select the max power RPM (from)", value = 4000),
          
          selectInput("x","Select the variable x for the graphic",choices = c("Weight to power ratio", "Engine power [bhp]", "Max Power [RPM]", "Max torque [Nm]", "Max Torque [RPM]", "Curb weight [kg]","Acceleration (0-100 km/h) [second]","Max speed [km/h]"),selected = "Engine power [bhp]"),
          
          selectInput("y","Select the variable y for the graphic",choices = c("Weight to power ratio", "Engine power [bhp]", "Max Power [RPM]", "Max torque [Nm]", "Max Torque [RPM]", "Curb weight [kg]","Acceleration (0-100 km/h) [second]","Max speed [km/h]"),selected = "Max speed [km/h]"),
          
                  width = 3),
        
        mainPanel(
          tableOutput("distTable"),
          plotOutput("distPlot", click = "plot_click"),
          tableOutput("data"),
          width = 9,
          tags$style(type="text/css",
                     ".shiny-output-error { visibility: hidden; }",
                     ".shiny-output-error:before { visibility: hidden; }"
          )
          )
    )
)

# Define server logic required to draw a table with the results a plot and a table with the click selection.

server <- function(input, output) {

    output$distTable<-renderTable({head(tablafinal[tablafinal$`Engine power [bhp]` > input$power[1] & 
                                        tablafinal$`Engine power [bhp]` < input$power[2] &
                                        tablafinal$`Curb weight [kg]` > input$weight[1] & 
                                        tablafinal$`Curb weight [kg]` < input$weight[2] &
                                        tablafinal$`Weight to power ratio` > input$weightpower[1] &
                                        tablafinal$`Weight to power ratio` < input$weightpower[2] &
                                        tablafinal$`Gearbox type` %in% input$transm &
                                        tablafinal$`Drive wheels` %in% input$traction &
                                        tablafinal$`Boost type` %in% input$boost & 
                                        tablafinal$`Engine type` %in% input$fuel &   
                                        tablafinal$`Max Power [RPM]` >= input$maxRPMbhp &
                                        tablafinal$`Year from (Generation)` > input$year[1] &
                                        tablafinal$`Year from (Generation)` < input$year[2]
                                        ,],20)}
                                        ,striped = T, spacing = "xs", bordered = T, align = "l")

    output$distPlot <- renderPlot({plot(head(tablafinal[tablafinal$`Engine power [bhp]` > input$power[1] & 
                                                        tablafinal$`Engine power [bhp]` < input$power[2] &
                                                        tablafinal$`Curb weight [kg]` > input$weight[1] & 
                                                        tablafinal$`Curb weight [kg]` < input$weight[2] &
                                                        tablafinal$`Weight to power ratio` > input$weightpower[1] &
                                                        tablafinal$`Weight to power ratio` < input$weightpower[2] &
                                                        tablafinal$`Gearbox type` %in% input$transm &
                                                        tablafinal$`Drive wheels` %in% input$traction &
                                                        tablafinal$`Boost type` %in% input$boost & 
                                                        tablafinal$`Engine type` %in% input$fuel &   
                                                        tablafinal$`Max Power [RPM]` >= input$maxRPMbhp &
                                                        tablafinal$`Year from (Generation)` > input$year[1] &
                                                        tablafinal$`Year from (Generation)` < input$year[2]
                                                        ,],20)[,input$x],
                                                        head(tablafinal[tablafinal$`Engine power [bhp]` > input$power[1] & 
                                                        tablafinal$`Engine power [bhp]` < input$power[2] &
                                                        tablafinal$`Curb weight [kg]` > input$weight[1] & 
                                                        tablafinal$`Curb weight [kg]` < input$weight[2] &
                                                        tablafinal$`Weight to power ratio` > input$weightpower[1] &
                                                        tablafinal$`Weight to power ratio` < input$weightpower[2] &
                                                        tablafinal$`Gearbox type` %in% input$transm &
                                                        tablafinal$`Drive wheels` %in% input$traction &
                                                        tablafinal$`Boost type` %in% input$boost & 
                                                        tablafinal$`Engine type` %in% input$fuel &   
                                                        tablafinal$`Max Power [RPM]` >= input$maxRPMbhp &
                                                        tablafinal$`Year from (Generation)` > input$year[1] &
                                                        tablafinal$`Year from (Generation)` < input$year[2]
                                                        ,],20)[,input$y],
                                                        col = ifelse(head(tablafinal[tablafinal$`Engine power [bhp]` > input$power[1] & 
                                                                             tablafinal$`Engine power [bhp]` < input$power[2] &
                                                                             tablafinal$`Curb weight [kg]` > input$weight[1] & 
                                                                             tablafinal$`Curb weight [kg]` < input$weight[2] &
                                                                             tablafinal$`Weight to power ratio` > input$weightpower[1] &
                                                                             tablafinal$`Weight to power ratio` < input$weightpower[2] &
                                                                             tablafinal$`Gearbox type` %in% input$transm &
                                                                             tablafinal$`Drive wheels` %in% input$traction &
                                                                             tablafinal$`Boost type` %in% input$boost & 
                                                                             tablafinal$`Engine type` %in% input$fuel &   
                                                                             tablafinal$`Max Power [RPM]` >= input$maxRPMbhp &
                                                                             tablafinal$`Year from (Generation)` > input$year[1] &
                                                                             tablafinal$`Year from (Generation)` < input$year[2]
                                                                           ,],20)$`Engine type`== "Gasoline", "red","blue"), 
                                                        pch = ifelse(head(tablafinal[tablafinal$`Engine power [bhp]` > input$power[1] & 
                                                                             tablafinal$`Engine power [bhp]` < input$power[2] &
                                                                             tablafinal$`Curb weight [kg]` > input$weight[1] & 
                                                                             tablafinal$`Curb weight [kg]` < input$weight[2] &
                                                                             tablafinal$`Weight to power ratio` > input$weightpower[1] &
                                                                             tablafinal$`Weight to power ratio` < input$weightpower[2] &
                                                                             tablafinal$`Gearbox type` %in% input$transm &
                                                                             tablafinal$`Drive wheels` %in% input$traction &
                                                                             tablafinal$`Boost type` %in% input$boost & 
                                                                             tablafinal$`Engine type` %in% input$fuel &   
                                                                             tablafinal$`Max Power [RPM]` >= input$maxRPMbhp &
                                                                             tablafinal$`Year from (Generation)` > input$year[1] &
                                                                             tablafinal$`Year from (Generation)` < input$year[2]
                                                                           ,],20)$`Gearbox type`== "Manual", 15 , 16),
                                                       cex = 2, 
                                                       xlab = input$x, 
                                                       ylab = input$y,
                                                       las = 1,
                                                      main = paste(input$x," and ", input$y, " plot chart comparison"),
                                        ) 
                                        grid(NULL, NULL, lwd = 2)
                                        mtext("* Red color is for gasoline engines, blue for the rest. Square shape is for manual transmission, circle is for the rest",
                                              side = 1,
                                              line = 2,
                                              col= "darkgreen",
                                              )
                                  }
                                  )

    output$data <- renderTable({nearPoints(head(tablafinal[tablafinal$`Engine power [bhp]` > input$power[1] & 
                                                          tablafinal$`Engine power [bhp]` < input$power[2] &
                                                          tablafinal$`Curb weight [kg]` > input$weight[1] & 
                                                          tablafinal$`Curb weight [kg]` < input$weight[2] &
                                                          tablafinal$`Weight to power ratio` > input$weightpower[1] &
                                                          tablafinal$`Weight to power ratio` < input$weightpower[2] &
                                                          tablafinal$`Gearbox type` %in% input$transm &
                                                          tablafinal$`Drive wheels` %in% input$traction &
                                                          tablafinal$`Boost type` %in% input$boost & 
                                                          tablafinal$`Engine type` %in% input$fuel &   
                                                          tablafinal$`Max Power [RPM]` >= input$maxRPMbhp &
                                                          tablafinal$`Year from (Generation)` > input$year[1] &
                                                          tablafinal$`Year from (Generation)` < input$year[2]
                                                          ,],20), input$plot_click, xvar = input$x, yvar = input$y)
                                },striped = T, spacing = "xs", bordered = T, align = "l")
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
