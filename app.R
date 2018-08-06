#                          DynamicPlotWindowsApp
#
# Purpose   : Dysnamic creation of rows and colomns with plots
#
# Copyright : Vis Consultancy, the Netherlands
#             This program is made available in the hope that it will be useful,
#             but without any warranty; without even the implied warranty of
#             merchantability or fitness for a particular purpose.
#             See the GNU General Public License for more details
# History   :
#  Jul18 - Created
# ------------------------------------------------------------------------------
require (shiny)
require (ggplot2)

ui <- fluidPage (sidebarLayout (
    sidebarPanel (
        uiOutput ('uiPlotMgmnt')
    ),
    mainPanel (
        uiOutput ('uiPlots'),
        fluidRow (
            column (2, fluidRow (
                column (2, p ('rows')),
                column (2, actionButton ('uiRowsMin' , label='-')),
                column (2, actionButton ('uiRowsPlus', label='+'))
            )),
column (2, fluidRow (
                column (4, p ('columns'), offset=1 ),
                column (2, actionButton ('uiColsMin' , label='-')),
                column (2, actionButton ('uiColsPlus', label='+'))
            ), offset = 7)
        )
    )
))

server = function (input, output, session) {
    pe <- intitPlotEnvir()
    initPlots    (pe)
    setUiPlots   (input, output, pe)                        # and show empty plots
    
    observe ({
        n <- input$uiRowsMin
        if (!is.null(n) && n) processRowsMin (input, output, pe)
    })
    observe ({
        n <- input$uiRowsPlus
        if (!is.null(n) && n) processRowsPlus (input, output, pe)
    })
    observe ({
        n <- input$uiColsMin
        if (!is.null(n) && n) processColsMin (input, output, pe)
    })
    observe ({
        n <- input$uiColsPlus
        if (!is.null(n) && n) processColsPlus (input, output, pe)
    })
}

intitPlotEnvir <- function () {
    pe          <- new.env()
    pe$nProws   <- 2
    pe$nPcols   <- 2
    pe$nPW      <- pe$nProws * pe$nPcols
    pe$nPWui    <- 0
    pe$pwHeight <- 120                        # default plot height 
    pe$pw       <- list()
    pe$pr       <- list()
    pe
}

# ------------------------------------------------------------------------------
#                   init plot windows incl.  plot mgmnt panels
# ------------------------------------------------------------------------------
initPlots <- function (pe) {
    nRows <- plotNrc (pe, pe$nPW)$rowN
    for (rowN  in 1:nRows ) initPlotRow (pe, rowN)
    for (plotN in 1:pe$nPW) {
        initPlotWindow  (pe, plotN)                         # init plot and legend windows
        initPMpanel     (pe, plotN)                         # init Plot Management panels
        initPMapp       (pe, plotN)                         # init the application data (a user function)
    }
}

setUiPlots <- function (input, output, pe) {
    setUiPlotWindows (output, pe)                           # set user interface defintions for plot windows
    setUiPMpanels  (output, pe)                             # set user interface PM panels; create outputId's for indiviual panels
    if (pe$nPW > pe$nPWui) for (plotN in (pe$nPWui+1):pe$nPW) {
        setUiPMpanel  (output, pe, plotN)                   # set user interface for  a (collapsed) PW window
        setObservePMW (input, output, pe, plotN)            # make it observed
    }
    pe$nPWui <- pe$nPW
    showAllPlots  (output, pe)
}

# ------------------------------------------------------------------------------
#                           add / delete rows / columns
# ------------------------------------------------------------------------------

processRowsPlus <- function (input, output, pe) {
    nPWold              <- pe$nPW
    pe$nProws           <- pe$nProws + 1
    pe$nPW              <- pe$nProws * pe$nPcols
    initPlotRow       (pe, pe$nProws)
    for (plotN in (nPWold+1):pe$nPW ) {
        initPlotWindow (pe, plotN)                          # init new plot windows
        initPMpanel    (pe, plotN)                          # init new PM panels
        initPMapp      (pe, plotN)
    }
    setUiPlots (input, output, pe)
}

processColsPlus <- function (input, output, pe) {
    nPWold  <- pe$nPW
    if      (pe$nPcols <  4)    pe$nPcols <- pe$nPcols + 1
    else if (pe$nPcols == 4)    pe$nPcols <- 6              # 12 / nCols must be an integer
    else                        return()
    pe$nPW  <- pe$nProws * pe$nPcols
    for (plotN in (nPWold+1):pe$nPW ) {
        initPlotWindow (pe, plotN)                          # init new plot windows
        initPMpanel    (pe, plotN)                          # init new PM panels
        initPMapp      (pe, plotN)
    }
    setAllPMplotDescr  (pe)
    for (plotN in 1:pe$nPWui) {                             # the [r,c] mapping to plotN changed
        setUiPMpanel  (output, pe, plotN)      
    }
    setUiPlots (input, output, pe)
}

processRowsMin <- function (input, output, pe) {
    if      (pe$nProws == 1) return()
    nPWold              <- pe$nPW
    pe$pr[[pe$nProws]]  <- NULL
    pe$nProws           <- pe$nProws - 1
    pe$nPW              <- pe$nProws * pe$nPcols
    pe$nPWui            <- min (pe$nPW, pe$nPWui) 
    for (plotN in (pe$nPW+1):nPWold ) {
        pe$pw[[plotN]]  <- NULL
    }
    setUiPlots (input, output, pe)
}

processColsMin <- function (input, output, pe) {
    if      (pe$nPcols == 1) return()    
    else if (pe$nPcols == 6) pe$nPcols <- 4                 # 12 / nCols must be an integer
    else                     pe$nPcols <- pe$nPcols - 1   
    nPWold              <- pe$nPW
    pe$nPW              <- pe$nProws * pe$nPcols
    pe$nPWui            <- min (pe$nPW, pe$nPWui) 
    for (plotN in (pe$nPW+1):nPWold ) {
        pe$pw[[plotN]]  <- NULL
    }
    setAllPMplotDescr  (pe)
    for (plotN in 1:pe$nPWui) {                             # the [r,c] mapping to plotN changed
        setUiPMpanel  (output, pe, plotN)      
    }
    setUiPlots (input, output, pe)
}

# ------------------------------------------------------------------------------
#                                init plot windows
# ------------------------------------------------------------------------------

initPlotRow <- function (pe, rowN) {
    pe$pr[[rowN]]               <- list()
    pe$pr[[rowN]]$uiPWrow       <- paste0 ('uiPWrow', rowN)
    pe$pr[[rowN]]$pwHeight      <- pe$pwHeight
}

initPlotWindow <- function (pe, plotN) {
    # plot window
    pe$pw[[plotN]]              <- list()
    pe$pw[[plotN]]$uiPW         <- paste0 ('uiPW', plotN)   # ui PW id
    pe$pw[[plotN]]$p            <- p_empty (plotN)
}

p_empty <- function (plotN) {
    p <- ggplot()
    p <- p + annotate("text", label = plotN, x = .5, y = .5, size = 20, colour = "grey")
    p <- p + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x=element_blank())
    p <- p + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y=element_blank())
    p
}

# ------------------------------------------------------------------------------
#                               set UI plot windows 
# ------------------------------------------------------------------------------

setUiPlotWindows <- function (output, pe) {
    cat ("setUiPlotWindows:", "pe$nProws=", pe$nProws, '\n')
    nRows   <- pe$nProws
    rowList <- vector (mode='list', length = nRows)
    output$uiPlots <- renderUI ({
        cat ("setUiPlotWindows renderUI:", "pe$nProws=", pe$nProws, '\n')
        for (rowN in 1:nRows) {
            height          <- paste0 (pe$pr[[rowN]]$pwHeight, 'px')
            nCols           <- pe$nPcols
            colW            <- 12 / nCols
            plotN1          <- rcPlotN (pe, rowN, 1)
            if (nCols == 1) {
                pw              <- pe$pw[[plotN1]]
                rowList[[rowN]] <- plotOutput (pw$uiPW, height=height)
            } else if (nCols == 2) {
                plotNs          <- c(plotN1, plotN1+1)
                uiPWs           <- sapply (pe$pw[plotNs], function(X) X$uiPW)
                rowList[[rowN]] <- fluidRow (
                    column (colW, plotOutput (uiPWs[1], height=height)),
                    column (colW, plotOutput (uiPWs[2], height=height))
                )
            } else if (nCols == 3) {
                plotNs          <- c(plotN1, plotN1+1, plotN1+2)
                uiPWs           <- sapply (pe$pw[plotNs], function(X) X$uiPW)
                rowList[[rowN]] <- fluidRow (
                    column (colW, plotOutput (uiPWs[1], height=height)),
                    column (colW, plotOutput (uiPWs[2], height=height)),
                    column (colW, plotOutput (uiPWs[3], height=height))
                )
            } else if (nCols == 4) {
                plotNs          <- c(plotN1, plotN1+1, plotN1+2, plotN1+3)
                uiPWs           <- sapply (pe$pw[plotNs], function(X) X$uiPW)
                rowList[[rowN]] <- fluidRow (
                    column (colW, plotOutput (uiPWs[1], height=height)),
                    column (colW, plotOutput (uiPWs[2], height=height)),
                    column (colW, plotOutput (uiPWs[3], height=height)),
                    column (colW, plotOutput (uiPWs[4], height=height))
                )
            } else if (nCols == 6) {
                plotNs          <- c(plotN1, plotN1+1, plotN1+2, plotN1+3, plotN1+4, plotN1+5)
                uiPWs           <- sapply (pe$pw[plotNs], function(X) X$uiPW)
                rowList[[rowN]] <- fluidRow (
                    column (colW, plotOutput (uiPWs[1], height=height)),
                    column (colW, plotOutput (uiPWs[2], height=height)),
                    column (colW, plotOutput (uiPWs[3], height=height)),
                    column (colW, plotOutput (uiPWs[4], height=height)),
                    column (colW, plotOutput (uiPWs[5], height=height)),
                    column (colW, plotOutput (uiPWs[6], height=height))
                )
            }                    
        }
        tagList (rowList)
    })
}

setUiRowPlots  <- function (output, pe, rowN) {
    output[[pe$pr[[rowN]]$uiPWrow]] <- renderUI ({
        nCols       <- pe$nPcols
        pr          <- pe$pr[[rowN]]
        pwColList   <- vector (mode='list', length = nCols)
        for (colN in 1:nCols) {
            plotN               <- rcPlotN (pe, rowN, colN)
            pw                  <- pe$pw[[plotN]]
            height              <- paste0 (pr$pwHeight, 'px')
            width               <- 12 / nCols
            pwColList[[colN]]   <- plotOutput (pw$uiPW, height=height, width=width)
        }
        do.call(tagList, pwColList)  
    })
}

# ------------------------------------------------------------------------------
#                             plot managements panes
# ------------------------------------------------------------------------------
# a plot maanegemnt panel is a wellPanel (id uiPMwpXX) with :
# - a header with expand/collapse buttons (id uiPMplusXX, uiPMminXX), the plot designation, and an apply button (id uiPMapply)
# - user application idgets, id = uiPMappXX

initPMpanel <- function (pe, plotN) {
    pe$pw[[plotN]]$uiPMwp       <- paste0 ("uiPMwp"   , plotN)      # the wellpanel
    pe$pw[[plotN]]$uiPMplus     <- paste0 ('uiPMplus' , plotN)      # expand panel
    pe$pw[[plotN]]$uiPMmin      <- paste0 ('uiPMmin'  , plotN)      # collapse panel
    pe$pw[[plotN]]$uiPMdescr    <- paste0 ('uiPMdescr', plotN)      # plot description n or [r,c]
    pe$pw[[plotN]]$plotDescr    <- PMplotDescr (pe, plotN)
    pe$pw[[plotN]]$uiPMapply    <- paste0 ('uiPMapply', plotN)      # apply panel app settings
    pe$pw[[plotN]]$uiPMapp      <- paste0 ('uiPMapp'  , plotN)      # for panel app elements
    pe$pw[[plotN]]$plus         <- FALSE                            # start with a collapsed panel

}

setUiPMpanels <- function (output, pe) {
    output$uiPlotMgmnt <- renderUI ({
        nPW  <- pe$nPW
        outputList <- vector (mode='list', length = nPW)
        for (plotN in 1:nPW) { 
            outputList[[plotN]] <- wellPanel (uiOutput (pe$pw[[plotN]]$uiPMwp))
        }
        tagList (outputList)
    })
}

processPMWmin <- function (output, pe, plotN) {                     # min pressed
    setUiPMmin (output, pe, plotN)
}

processPMWplus <- function (output, pe, plotN) {                    # plus pressed
    setUiPMplus (output, pe, plotN)
}

setUiPMpanel <- function (output, pe, plotN) {
    cat ("setUiPMpanel invoked\n")
    pw                      <- pe$pw[[plotN]]
    if (pe$pw[[plotN]]$plus) {setUiPMplus (output, pe, plotN)}  
    else                     {setUiPMmin  (output, pe, plotN)}
}

setUiPMmin <- function (output, pe, plotN) {                        # set to min => set right pointing plus button
    pe$pw[[plotN]]$plus <- FALSE
    pw                  <- pe$pw[[plotN]]
    output[[pw$uiPMwp]] <- renderUI ({
        fluidRow (
            column (1, actionButton (pw$uiPMplus, label='\u25BA')), # \u25BA = right pointing => press: set to plus
            column (8, p (pw$plotDescr,  align = "center")),
            column (2, actionButton (pw$uiPMapply, label='apply' ))
        )
    })
}

setUiPMplus <- function (output, pe, plotN) {
    pw                  <- pe$pw[[plotN]]
    pe$pw[[plotN]]$plus <- TRUE
    output[[pw$uiPMwp]] <- renderUI ({ 
        pw                  <- pe$pw[[plotN]]
        tagList (
            fluidRow (
                column (1, actionButton (pw$uiPMmin  , label='\u25BC')),  # \u25BC = down  pointing => is plus => press: set to min
                column (8, p (pw$plotDescr,  align = "center")),
                column (2, actionButton (pw$uiPMapply, label='apply' ))
            ),
            uiOutput (pw$uiPMapp)                                   # for app data (content)
        )
    })
}

setAllPMplotDescr <- function (pe) {
    for (plotN in 1:pe$nPW) {
        pe$pw[[plotN]]$plotDescr <- PMplotDescr (pe, plotN)
    }
}

PMplotDescr <- function (pe, plotN) {
    if ( pe$nPcols == 1) {
        plotDescr <- paste0 ('plot ', plotN)
    } else {
        rc <- plotNrc (pe, plotN)
        plotDescr <- paste0 ('plot [', rc$rowN, ',', rc$colN, ']')
    }
    plotDescr
}

setObservePMW <- function (input, output, pe, plotN) {
    pw          <- pe$pw[[plotN]]
    uiPMplus    <- pw$uiPMplus
    observe ({
        n  <- input[[uiPMplus]]
        if (!is.null(n) ) {
            cat ("setObservePMW:", "expand button observed =", uiPMplus, 'plotN=', plotN, '\n')
            processPMWplus  (output, pe, plotN)
            setUiPMapp      (input, output, pe, plotN)
        }
    })
    uiPMmin     <- pw$uiPMmin
    observe ({
        n  <- input[[uiPMmin]]
        if (!is.null(n)) {
            cat ("setObservePMW:", "collapse button observed =", uiPMmin, 'plotN=', plotN, '\n')
            processPMWmin (output, pe, plotN)
        }
    })
    observe ({
        n  <- input[[pw$uiPMapply]]
        if (!is.null(n) && n) {
            processPMapply (input, output, pe, plotN)
        }
    })
}

plotNrc <- function (pe, plotN) {
    nCols   <- pe$nPcols
    rowN    <- (plotN-1) %/% nCols + 1
    colN    <- (plotN-1) %%  nCols + 1
    list (rowN=rowN, colN=colN)
}

rcPlotN <- function (pe, rowN, colN) {
    nCols   <- pe$nPcols
    (rowN-1)*nCols + colN
}

showAllPlots <- function (output, pe) {
    for (i in 1:pe$nPW) { local ({
        plotN               <- i                            # create a local copy
        pw                  <- pe$pw[[plotN]]
        output[[pw$uiPW]]   <- renderPlot ({pw$p})
    })}
}

initPMapp <- function (pe, plotN) {
    # user defined plot management initialisation function  
}

setUiPMapp <- function (input, output, pe, plotN) {
    cat ("setUiPMapp", "plotN=", plotN, '\n')
    pw <- pe$pw[[plotN]]
    # user defined plot management function 
    output[[pw$uiPMapp]] <- renderUI ({
        p (paste ('your plot', plotN, 'management widgets'), align='center')
    })
}

processPMapply <- function (input, output, pe, plotN) {
    # user defined plot management function, e.g. apply plot management settings    
}

shinyApp (ui,server)
