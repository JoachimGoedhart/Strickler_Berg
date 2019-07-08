##############################################################################
# Strickler_Berg: Shiny app for calculation of spectroscopic parameters
# Created by Joachim Goedhart (@joachimgoedhart), first version 2019
# Takes non-tidy, csv files as absorbance and emission spectra assuming first column is "wavelength"
# Calculates any of three parameters, EC/QY/tau, when two are known
##############################################################################

options(shiny.maxRequestSize=30*1024^2)

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(magrittr)
library(readxl)
library(caTools)  #Needed for trapz()


#Set the known spectroscopic parameters for the fluorophore
mTq2_EC = NA
mTq2_QY = 0.93
mTq2_tau = 4.00

Fluorescein_EC = 75500
Fluorescein_QY = 0.93
Fluorescein_tau = 4.1


#Set some physical parameters
#Refractive Index

Pi = 3.141593
c = 3e10 #speed of light in cm/s
Navogadro = 6.022e23

#Calculate constant
constant = 8*2303*Pi*c/Navogadro


#Read a text file (comma separated values)
df_mTq2 <- read.csv("mTurquoise2_spectra.csv", na.strings = "")
df_Fluo <- read.csv("Fluorescein_spectra.csv", na.strings = "")


ui <- fluidPage(
  titlePanel("Strickler-Berg calculator"),
  sidebarLayout(
    sidebarPanel(width=3,


                 
                 conditionalPanel(
                   condition = "input.tabs=='Result'",
                   h4("Calculate:"),
                   
                   radioButtons(
                     "parameter", NA,
                     choices = 
                       list("Extinction Coefficient" = "EC",
                            "Quantum Yield" = "QY",
                            "Fluorescence Lifetime" = "tau"
                       )
                     ,
                     selected =  "EC"),
                   hr(),
                   h4("Input parameters and spectral data:"),
                   
                   conditionalPanel(condition = "input.parameter=='QY' || input.parameter=='tau'" ,
                        numericInput("EC", "Extinction Coefficient (in M-1 cm-1): ",value = 38000, min=0, max=500000, step=100)
                    ),

                   
                   conditionalPanel(condition = "input.parameter=='EC' || input.parameter=='tau'" ,
                                    
                        numericInput("QY", "Quantum Yield (0-1): ",value = 0.93, min=0, max=1, step=0.01)
                   ),     
                   
                   
                   conditionalPanel(condition = "input.parameter=='EC' || input.parameter=='QY'" ,
                                    
                        numericInput("tau", "Fluorescence Lifetime (in ns): ",value = 4.1, min=0, max=20, step=0.01)
                   ),
                   
                   
                   radioButtons(
                     "RI", "Refractive index (n):",
                     choices = 
                       list("1.330 (H2O)" = 1,
                            "1.365 (in cells)" = 2,
                            "Other" = 3)
                     ,
                     selected =  1),
                   
                   conditionalPanel(condition = "input.RI=='3'",
                        numericInput("user_RI", NA,value = 1.330, min=1, max=4, step=0.01)
                    ),
                   

                   radioButtons(
                     "data_input", "Absorbance&Emission spectrum:",
                     choices = 
                       list("Fluorescein Abs/Em data" = 1,
                            "mTurquoise2 Abs/Em data" = 2,
                            "Upload file" = 3,
                            "Paste data" = 4)
                     ,
                     selected =  1),
                   
                   conditionalPanel(
                     condition = "input.data_input=='3'",
                     h5(""),
                     fileInput("upload", "Wavelength (in nm), Absorbance, Emission", multiple = FALSE),
                     selectInput("file_type", "Type of file:",
                                 list("text (csv)" = "text",
                                      "Excel" = "Excel"
                                 ),
                                 selected = "text")
                   ),
                   
                   conditionalPanel(
                     condition = "input.data_input=='4'",
                     h5("Paste data below (Wavelength in nm, Absorbance, Emission):"),
                     tags$textarea(id = "data_paste",
                                   placeholder = "Add data here",
                                   rows = 10,
                                   cols = 20, ""),
                     actionButton("submit_data_button", "Submit data"),
                     radioButtons(
                       "text_delim", "Delimiter",
                       choices = 
                         list("Tab (from Excel)" = "\t",
                              "Space" = " ",
                              "Comma" = ",",
                              "Semicolon" = ";"),
                       selected = "\t")),
                   

                   
                   
                   
                   hr(),
                   h4("Plot layout"),
                   
                   numericInput("plot_height", "Height (# pixels): ", value = 480),
                   numericInput("plot_width", "Width (# pixels):", value = 600),

                   
                   
                   checkboxInput(inputId = "no_grid",
                                 label = "Remove gridlines",
                                 value = FALSE),
                   
                   
                   checkboxInput("thicken", "The plot thickens", value = FALSE),
                   
                   checkboxInput(inputId = "change_scale",
                                 label = "Change scale",
                                 value = FALSE),                   
                   
                   
                   conditionalPanel(
                     condition = "input.change_scale == true",
                     textInput("range_x", "Range x-axis (min,max)", value = "")
                     
                   ),
                   conditionalPanel(
                     condition = "input.change_scale == true",
                     textInput("range_y", "Range y-axis (min,max)", value = "")
                     
                   ),
                   
                   h4("Labels"),
                   
                   checkboxInput(inputId = "add_title",
                                 label = "Add title",
                                 value = FALSE),
                   conditionalPanel(
                     condition = "input.add_title == true",
                     textInput("title", "Title:", value = "")
                   ),
                   
                   checkboxInput(inputId = "label_axes",
                                 label = "Change labels",
                                 value = FALSE),
                   conditionalPanel(
                     condition = "input.label_axes == true",
                     textInput("lab_x", "X-axis:", value = ""),
                     textInput("lab_y", "Y-axis:", value = "")
                     
                   ),
                   
                   checkboxInput(inputId = "adj_fnt_sz",
                                 label = "Change font size",
                                 value = FALSE),
                   conditionalPanel(
                     condition = "input.adj_fnt_sz == true",
                     numericInput("fnt_sz_ttl", "Size axis titles:", value = 24),
                     numericInput("fnt_sz_ax", "Size axis labels:", value = 18)
                     
                   ),
                   conditionalPanel(
                     condition = "input.color_data == true || input.color_stats == true",
                     checkboxInput(inputId = "add_legend",
                                   label = "Add legend",
                                   value = FALSE)),
                   
  
                   NULL  ####### End of heatmap UI#######
                 ),
 
                 conditionalPanel(
                   condition = "input.tabs=='About'",
                   h4("About")    
                 )
                 
    ),
    mainPanel(
      
      tabsetPanel(id="tabs",
                  tabPanel("Result", htmlOutput("LegendText"), plotOutput("coolplot")),

##### Uncomment for interactive graph panel
#                  tabPanel("Plot-interactive", plotlyOutput("plot_interact")
#                  ), 


                  tabPanel("About", includeHTML("about.html")
                  )
                  
      )
    )
  )         
)


server <- function(input, output, session) {

  ####################################################################
##################### Synchronize scales between tabs ##################  
  
# observeEvent(input$change_scale, {  
#   if (input$change_scale==TRUE)  {
#     updateCheckboxInput(session, "change_scale2", value = TRUE)
#   } else if (input$change_scale==FALSE)   {
#     updateCheckboxInput(session, "change_scale2", value = FALSE)
#   }
# })
#  
# observeEvent(input$change_scale2, {  
#   if (input$change_scale2==TRUE)  {
#     updateCheckboxInput(session, "change_scale", value = TRUE)
#   } else if (input$change_scale2==FALSE)   {
#     updateCheckboxInput(session, "change_scale", value = FALSE)
#   }
# })
#   
# 
# observeEvent(input$range_lineplot, {
#   updateTextInput(session, "range_x2", value = input$range_x)
#   updateTextInput(session, "range_y2", value = input$range_y)
#   
# })



####################################################################


    
    
  
  #####################################
  ###### READ IN / GET DATA ###########
  #####################################
  
df_upload <- reactive({
    if (input$data_input == 1) {
      df <- df_Fluo
      updateNumericInput(session, "tau", value=4.1)      
      updateNumericInput(session, "EC", value=75500)
      
    } else if (input$data_input == 2) {
      df <- df_mTq2
      updateNumericInput(session, "tau", value=4.0)
      updateNumericInput(session, "EC", value=38000)
      
      
    } else if (input$data_input == 3) {
      file_in <- input$upload
      # Avoid error message while file is not uploaded yet
      if (is.null(input$upload)) {
        return(data.frame(x = "Select your datafile", Wavelength=1,Abs=1, Em=1))
#      } else if (input$submit_datafile_button == 0) {
#        return(data.frame(x = "Press 'submit datafile' button"))
      } else {
        isolate({
          if (input$file_type == "text") {
            df <- read_delim(file_in$datapath,
                               delim = ",",
                               col_names = TRUE)
          } else if (input$file_type == "Excel") {
            df <- read_excel(file_in$datapath)
          } 
        })
      }
      
      
    } else if (input$data_input == 4) {
      if (input$data_paste == "" || input$submit_data_button == 0) {
        df <- data.frame(x = "Copy your data into the textbox,
                           select the appropriate delimiter, and
                           press 'Submit data'", Wavelength="1")

        
        } else {
          isolate({
            df <- read_delim(input$data_paste,
                               delim = input$text_delim,
                               col_names = TRUE)
            
            #Check that data has at least 3 columns (1st is Wavelength) and 2 rows (upper row is header)
            if (ncol(df)<3 || nrow(df)<1) {return(data.frame(x = "Number of columns and rows should be 3", Wavelength="1"))}
            #The first column is defined as Wavelength, id is added for compatibility            
          })
        }
      }
#  }
  
  names(df)[1] <- "Wavelength"
  names(df)[2] <- "Abs"
  names(df)[3] <- "Em"
  
  #Normalize Abs and Em data to maximum value and calculate wavenumbers [cm-1]
  df <- df %>% mutate(Abs=Abs/max(na.omit(Abs)), Em=Em/max(na.omit(Em)), Wv=1/(Wavelength*1e-7), Em3=Em/Wv^3)

  observe({ print(head(df)) })
  
  return(df)
})



#############################################################


df_param <- reactive({
  
  if (input$RI =="1") {
    n=1.330
  } else if (input$RI=="2") {
    n=1.365
  } else if (input$RI=="3") {
    n <- input$user_RI
  }
  
  koos <- df_upload()
  koos$Abs[is.na(koos$Abs)] <- 0
  koos$Em[is.na(koos$Em)] <- 0
  koos$Em3[is.na(koos$Em3)] <- 0
  
  #Calculate Integrals
  Int_I <-  trapz(koos$Wv,koos$Em)/trapz(koos$Wv,koos$Em3)
  Int_A <-  abs(trapz(koos$Wv,(koos$Abs/koos$Wv)))
  
  #Calculate the kr/EC
  k <- constant*(n)^2*Int_I*Int_A
  
#  observe({ print((k)) })

  if (input$parameter == "EC") {
    
    EC <-  input$QY * 1e9/k/input$tau
    QY <- input$QY
    tau <- input$tau
    
    observe({ print((EC)) })
    
    
  } else if (input$parameter == "QY") {
    
    EC <- input$EC
    QY <- input$EC*k*input$tau*1e-9
    tau <- input$tau    
    
    observe({ print((QY)) })
    
  } else if (input$parameter == "tau") {
    
    EC <- input$EC
    QY <- input$QY
    tau <- input$QY * 1e9/k/input$EC

    observe({ print((tau)) })
    
  }
  
  
  
  df <- data.frame(EC = EC, QY = QY, tau=tau, RI = input$RI)
  
  observe({ print((df)) })
  return(df)
  
})




 #################################################
############## Convert data  ############
df_upload_tidy <- reactive({
  
  koos <- df_upload()

    klaas <- gather(koos, Sample, Value, -Wavelength)

    klaas <- klaas %>% mutate (Wavelength = as.numeric(Wavelength), Value = as.numeric(Value))

    klaas 
    
    observe({ print(head(klaas)) })
    
    return(klaas)
})
 #################################################

####################################
##### Get the Variables ##############

# observe({ 
#   var_names  <- names(df_upload_tidy())
#   var_list <- c("none", var_names)
#   #        updateSelectInput(session, "colour_list", choices = var_list)
#   updateSelectInput(session, "y_var", choices = var_list, selected="Value")
#   updateSelectInput(session, "x_var", choices = var_list, selected="Wavelength")
#   updateSelectInput(session, "c_var", choices = var_list, selected="id")
#   updateSelectInput(session, "g_var", choices = var_list, selected="Sample")
# 
# })
################################### 







########################################################### 







##################################################
#### Caluclate Summary of the DATA for the MEAN ####

# df_summary_mean <- reactive({
#   koos <- df_binned() %>%
#     group_by(Wavelength, id) %>% 
#     summarise(n = n(),
#               mean = mean(Value, na.rm = TRUE),
#               median = median(Value, na.rm = TRUE),
#               sd = sd(Value, na.rm = TRUE)) %>%
#     mutate(sem = sd / sqrt(n - 1),
#            ci_lo = mean + qt((1-Confidence_level)/2, n - 1) * sem,
#            ci_hi = mean - qt((1-Confidence_level)/2, n - 1) * sem)
#   return(koos)
# })

#################################################



 ###########################################
######### DEFINE DOWNLOAD BUTTONS ###########

output$downloadPlotPDF <- downloadHandler(
  filename <- function() {
    paste("PlotTwist_", Sys.time(), ".pdf", sep = "")
  },
  content <- function(file) {
    pdf(file, width = input$myWidth/72, height = input$myHeight/72)
    plot(plot_data())
    dev.off()
  },
  contentType = "application/pdf" # MIME type of the image
)

output$downloadPlotPNG <- downloadHandler(
  filename <- function() {
    paste("PlotTwist", Sys.time(), ".png", sep = "")
  },
  content <- function(file) {
    png(file, width = input$plot_width*4, height = input$plot_height*4, res=300)
    plot(plot_data())
    dev.off()
  },
  contentType = "application/png" # MIME type of the image
)


 ###########################################  



 ###############################################
############## GENERATE PLOT LAYERS #############      
        

plot_data <- reactive({

    #Define how colors are used
    klaas <- df_upload_tidy()
    
    
    params <- df_param()

    #### Command to prepare the plot ####
 #   p <- ggplot(data=klaas, aes_string(x="Wavelength")) 
    p <- ggplot(data=df_upload(), aes_string(x="Wavelength")) 
    
         
      #### plot individual measurements ####
    
    if (input$thicken =="TRUE") {
      multiplier <- 10
    } else if (input$thicken =="FALSE"){
      multiplier <- 4
    }
    
#    p <- p+ geom_line(data=klaas, aes_string(x="Wavelength", y="Value", color="Sample"), size=0.5*multiplier, alpha=0.9)
    p <- p+ geom_line(data=df_upload(), aes_string(x="Wavelength", y="Abs"), color="darkblue",size=0.5*multiplier, alpha=0.9)
    p <- p+ geom_line(data=df_upload(), aes_string(x="Wavelength", y="Em"), color="darkgreen", size=0.5*multiplier, alpha=0.9)    

    ########### Do some formatting of the lay-out
    
    p <- p+ theme_light(base_size = 16)
    
   
    
    #Adjust scale if range for x (min,max) is specified
    if (input$range_x != "" &&  input$change_scale == TRUE) {
      rng_x <- as.numeric(strsplit(input$range_x,",")[[1]])
      observe({ print(rng_x) })
      #If min>max invert the axis
        if (rng_x[1]>rng_x[2]) {p <- p+ scale_x_reverse()}

      #Autoscale if rangeis NOT specified
    } else if (input$range_x == "" ||  input$change_scale == FALSE) {
      rng_x <- c(NULL,NULL)
 #     observe({ print(rng_x) })
    }


    #Adjust scale if range for y (min,max) is specified
    if (input$range_y != "" &&  input$change_scale == TRUE) {
      rng_y <- as.numeric(strsplit(input$range_y,",")[[1]])

      #If min>max invert the axis
      if (rng_y[1]>rng_y[2]) {p <- p+ scale_y_reverse()}

      #Autoscale if rangeis NOT specified
    } else if (input$range_y == "" ||  input$change_scale == FALSE) {
      rng_y <- c(NULL,NULL)
    }


#    observe({ print(rng_x) })
#    observe({ print(rng_y) })    


    # Define the axis limits    
    p <- p + coord_cartesian(xlim=c(rng_x[1],rng_x[2]),ylim=c(rng_y[1],rng_y[2]))    
    
    

    # if title specified
    if (input$add_title)
      p <- p + ggtitle(input$title)

    # # if labels specified
    if (input$label_axes)
      p <- p + labs(x = input$lab_x, y = input$lab_y)

    # # if font size is adjusted
    if (input$adj_fnt_sz) {
      p <- p + theme(axis.text = element_text(size=input$fnt_sz_ax))
      p <- p + theme(axis.title = element_text(size=input$fnt_sz_ttl))
    }
    
    #remove legend (if selected)
    if (input$add_legend == FALSE) {  
      p <- p + theme(legend.position="none")
    }
    
    #remove gridlines (if selected)
    if (input$no_grid == TRUE) {  
      p <- p+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())
    }


    return(p)
    
  }) #close output$coolplot

##### Uncomment for interactive graph panel
# output$plot_interact <- renderPlotly({
#   ggplotly(plot_data(), height=as.numeric(input$plot_height), width=as.numeric(input$plot_width))
# })

##### Set width and height of the plot area
width <- reactive ({ input$plot_width })
height <- reactive ({ input$plot_height })

output$coolplot <- renderPlot(width = width, height = height, {     
  plot(plot_data())
}) #close output$coolplot




###### Figure legend #########
output$LegendText <- renderText({

  if (input$RI =="1") {
    n <- c("1.330 (H2O)")
  } else if (input$RI=="2") {
    n <- c("1.365 (cells)")
  } else if (input$RI=="3") {
    n <- input$user_RI
  }
  
  Legend <- c('</br>')
  
  koos <- df_param()
  
  if (input$parameter == "EC") {
    Legend<-append(Legend, paste("<h4>The calculated <b>Extinction coefficient = ", round(koos$EC), " M-1 cm-1</b></h4>", sep=""))
    Legend<-append(Legend, paste("with the Quantum yield (", input$QY,") and Fluorescence lifetime (", round(input$tau,2), " ns) as input.", sep=""))
    
  } else if (input$parameter == "QY") {
    Legend<-append(Legend, paste("<h4>The calculated <b>Quantum Yield = ", round(koos$QY, 2), "</b></h4>", sep=""))
    Legend<-append(Legend, paste("with the Extinction coefficient (", round(koos$EC)," M-1 cm-1) and Fluorescence lifetime (", round(input$tau,2), " ns) as input.", sep=""))
    
  } else if (input$parameter == "tau"){
    Legend<-append(Legend, paste("<h4>The calculated <b>Fluorescence lifetime = ", round(koos$tau,2), " ns</b></h4>", sep=""))
    Legend<-append(Legend, paste("with the Quantum yield (", input$QY,") and Extinction coefficient (", round(koos$EC)," M-1 cm-1) as input.", sep=""))
  }
  

  Legend<-append(Legend, paste("</br>The refractive index was set to: ", n, ".", sep=""))
  Legend<-append(Legend, paste("</br>The absorbance and emission spectra that are used as input are shown below.", sep=""))  
  })

######## Figure Legend in HTML format #############

# output$LegendText <- renderText({
  
  # if (input$add_description == FALSE) {return(NULL)}
  
#   HTML_Legend <- c('</br></br><h4>Figure description</h4>')
#   
#   #The width of the legend (style as a paragraph between <p></p>) is adjusted to the width of the plot
#   
# #  HTML_Legend <-append(HTML_Legend, paste('<p style="width:',input$plot_width,'px;padding: 0px 15px 0px 40px">', sep=""))
#   
#   HTML_Legend <- append(HTML_Legend, Fig_legend())
#   
#   HTML_Legend <- append(HTML_Legend, paste("</p>"))
  
  
  
  
  # X <- Fig_legend()
  # return(X)
  
# })




    # End R-session when browser closed
    # session$onSessionEnded(stopApp)
################################################
  
  } #close server

shinyApp(ui = ui, server = server)