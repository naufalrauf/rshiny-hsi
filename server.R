#Server development for Hyperspectral Image Processing
#naufalrauf@yahoo.co.id
#Developed by Rauf 13 June 2017
#Updated by Rauf 22 November 2017


library (shiny)

#source("newhelpers.R")
shinyServer(function(input, output, session) {
  pltsy <- isolate(input$plotstyle)
  
  output$text1 <- renderText({paste (input$var)})
  
  output$text2 <- renderText({paste ("Chosen wavelength: ", input$wvl1)})
  
  observeEvent(input$dbload,
     isolate({
       
       dbl()
       output$text3 <- renderText({paste ("Loaded from ", filed)})
     }))
  
  observeEvent(input$browse, 
     isolate({
       
       wln <- as.integer(input$wvl1)
       fwl(wln)
       output$image1 <- renderImage({
          filepath <- normalizePath(file.path(paste('result', input$wvl1, '.jpeg', sep ='')))
          list(
            src = filepath,
            width = 609,
            alt = "Image is missing or not generated") 
        }, deleteFile = FALSE)
        
        }
         ))
  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
            " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }
    paste0(
      "Point: ", xy_str(input$plot_click),
      "Box: ", xy_range_str(input$plot_brush))
    
  })
  
  observe({
    updateNumericInput(session, "plotx1", value = input$plot_brush$xmin)
    updateNumericInput(session, "plotx2", value = input$plot_brush$xmax)
    updateNumericInput(session, "ploty1", value = input$plot_brush$ymin)
    updateNumericInput(session, "ploty2", value = input$plot_brush$ymax)
  })
  
  output$infoc <- renderText({
    xy_str2 <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    xy_range_str2 <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }
    paste0(
      "Point: ", xy_str2(input$plot_click2),
      "Box: ", xy_range_str2(input$plot_brush2))
  })
  observeEvent(input$plotss,
     isolate({
         
         #Since plot area for shiny is only 609 x 400
         #and y axis is left with empty area
         # 2.286 is compensation/constants to equalize the numbers of blocks difference

          #x1 <- input$plot_brush$xmin * 2.286
          #x2 <- input$plot_brush$xmax * 2.286
          #y1 <- input$plot_brush$ymin * 2.286
          #y2 <- input$plot_brush$ymax * 2.286
       
          
          x1 <- input$plotx1 * 2.286
          x2 <- input$plotx2 * 2.286
          y1 <- input$ploty1 * 2.286
          y2 <- input$ploty2 * 2.286
          
       
         cdnt <<- c(x1, y1, x2, y2) 
         
         withProgress(message = 'Processing data', value = 0, {
         for (i in m){
           
           gb <- mean(dbhsi[41, y1:y2, 260])
           k[i] <- mean(dbhsi[41, y1:y2, i])
           
           #n[i] <<- mean(dbhsi[x1:x2, y1:y2, i]) * (gb/k[i])
           n[i] <<- mean(dbhsi[x1:x2, y1:y2, i])
           #j[i] <- n[i] + (sd(dbhsi[x1:x2, y1:y2, i]) * (gb/k[i]))
           j[i] <<- n[i] + (sd(dbhsi[x1:x2, y1:y2, i]))
           
           incProgress(1/1040, detail = paste(i))
         } 
         })
         r760 <- n[624]
         r735 <- n[581]
         r710 <- n[537]
         r704 <- n[527]
         r701 <- n[522]
         r698 <- n[517]
         r670 <- n[468]
         r660 <- n[451]
         r605 <- n[355]
         r550 <- n[260]
         r534 <- n[233]
         r510 <- n[191]
         
         mcari <- ((r701-r670) - 0.2*(r701-r550))*(r701/r670)
         psri <- r670/(r550)
         #hindex <- ((r534-r698)/(r534+r698))-(r704/2)
         #nratio <- r550/mean(n[104:208])
         nratio <- r550/((n[174]-n[139])/(500-480))
         
         output$hsindex <- renderText({
           paste(
             "550: ", round(r550, 3),
             "| 670: ", round(r670, 3),
             "| 735: ", round(r735, 3),
             "| 735/670: ", round(r735/r670, 3),
             "| 670/550: ", round(psri, 3),
             "| 735/550: ", round(r735/r550, 3),
             "| 605/710: ", round(r605/r710, 3),
             "| NDVI: ", round((n[693]/((crn[1424*xa[693]/1040])/717) - n[468]/((crn[1424*xa[468]/1040])/717)) / (n[693]/((crn[1424*xa[693]/1040])/717) + n[468]/((crn[1424*xa[468]/1040])/717)), 3)
             )
         })
       
       
       
       output$ssplot <- renderPlot({
         plot(xa, n*100,  xlim = range(400:850), ylim = range(0:100), type = "o", xlab = "Wavelength (nm)", ylab = "Reflectance")
         par(new=TRUE)
         plot(xa, j*100, xlim = range(400:850), ylim = range(0:100), type = "l", xlab = "Wavelength (nm)", ylab = "Reflectance")
       })
       
       updateTabsetPanel(session, "inTabset", "panel2")
     }))
  
  #Image correction algorithm
  observeEvent(input$ccim,
        isolate({
          wln <- as.integer(input$wvl1)
          corrc(wln)
          output$image2 <- renderImage({
            filepath <- normalizePath(file.path(paste('corrected', input$wvl1, '.jpeg', sep ='')))
            list(
              src = filepath,
              width = 609,
              alt = "Image is missing or not generated") 
          }, deleteFile = FALSE)
          
          output$text4 <- renderText({paste ("Image has been adjusted")})
        })
        )
  
  observeEvent(input$ndvi.button, 
        isolate({
          genndvi()
          output$image2 <- renderImage({
            filepath <- 'correctedndvi.jpeg'
            list(
              src = filepath,
              width = 609,
              alt = "Image is missing or not generated") 
          }, deleteFile = FALSE)
          
          output$text4 <- renderText({paste ("Image has been adjusted")})   
               })
        )

  
  
})