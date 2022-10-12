### Copyright (C) 2017  Alvaro Santamaria-Gomez, 12 May 2017
### alvaro.santamaria@get.omp.eu
### 
### This program is free software: you can redistribute it and/or modify
### it under the terms of the GNU General Public License as published by
### the Free Software Foundation, either version 3 of the License, or
### any later version.
### 
### This program is distributed in the hope that it will be useful,
### but WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### GNU General Public License for more details.
### 
### You should have received a copy of the GNU General Public License
### along with this program.  If not, see <https://www.gnu.org/licenses/>.

# Loading packages
suppressPackageStartupMessages(suppressMessages(suppressWarnings({
  library(data.table, verbose = F, quietly = T) #v1.14.0
  library(dlm, verbose = F, quietly = T) #v1.1-5
  library(fields, verbose = F, quietly = T) #v12.5
  library(lubridate, verbose = F, quietly = T) #v1.7.10
  library(magrittr, verbose = F, quietly = T) #v2.0.1
  library(markdown, verbose = F, quietly = T) #v1.1
  library(matrixStats, verbose = F, quietly = T) #v0.59.0
  library(mvcwt, verbose = F, quietly = T) #v1.3.1
  library(numDeriv, verbose = F, quietly = T) #v2016.8-1.1
  library(psych, verbose = F, quietly = T) #v2.1.6
  library(RColorBrewer, verbose = F, quietly = T) #v1.1-2
  library(shinyBS, verbose = F, quietly = T) #v0.61
  library(shinycssloaders, verbose = F, quietly = T) #v1.0.0
  library(shinyjs, verbose = F, quietly = T) #v2.0
  library(shinythemes, verbose = F, quietly = T) #v1.2.0
  library(shiny, verbose = F, quietly = T) #v1.6.0
  library(spectral, verbose = F, quietly = T) #v2.0
  library(strucchange, verbose = F, quietly = T) #v1.5-2
  library(tseries, verbose = F, quietly = T) #v0.10-48
  library(pracma, verbose = F, quietly = T) #v2.3.8
})))

# version ####
version <- "SARI octubre 2022"

# Some GUI functions

# Help popup (based on https://github.com/daattali/ddpcr/blob/master/inst/shiny/ui/helpers.R)
helpPopup <- function(content, title = NULL) {
  a(#href = "#",
    class = "popover-link",
    `data-toggle` = "popover",
    `data-title` = title,
    `data-content` = content,
    `data-html` = "true",
    `data-trigger` = "hover",
    `data-placement` = "auto right",
    `data-container` = "body",
    `data-animation` = "true",
    `data-delay` = "show: 100, hide: 500",
    icon("circle-question")
  )
}
# Working spinner (based on https://github.com/daattali/advanced-shiny/blob/master/busy-indicator/helpers.R)
withBusyIndicatorUI <- function(button) {
  id <- button[['attribs']][['id']]
  div(
    shinyjs::useShinyjs(),
    singleton(tags$head(
      tags$style(withBusyIndicatorCSS)
    )),
    `data-for-btn` = id,
    button,
    span(
      class = "btn-loading-container",
      shinyjs::hidden(
        img(src = "ajax-loader-bar.gif", class = "btn-loading-indicator"),
        icon("check", class = "btn-done-indicator")
      )
    ),
    shinyjs::hidden(
      div(class = "btn-err",
          div(icon("circle-exclamation"),
              tags$b("Error: "),
              span(class = "btn-err-msg")
          )
      )
    )
  )
}

# class for disabled tabs
withBusyIndicatorCSS <- "
  .btn-loading-container {
    margin-left: 10px;
    font-size: 1.2em;
  }
  .btn-done-indicator {
    color: green;
  }
  .btn-err {
    margin-top: 10px;
    color: red;
  }"

# Mobile detector (from https://g3rv4.com/2017/08/shiny-detect-mobile-browsers)
mobileDetect <- function(inputId, value = 0) {
  tagList(
    singleton(tags$head(tags$script(src = "mobile.js"))),
    tags$input(id = inputId,
               class = "mobile-element",
               type = "hidden")
  )
}

# Disable tab click (based on https://stackoverflow.com/questions/40741691/rshiny-disabling-tabs-adding-text-to-tabs)
css <- '
.disabled {
background: default !important;
cursor: not-allowed !important;
pointer-events: none; 
color: gray !important;
}'

# UI ####
ui <- fluidPage(theme = shinytheme("spacelab"),
                mobileDetect('isMobile'),
                useShinyjs(),
                
                # HTTP meta and style header tags
                # tags$head(includeScript("google-analytics.js")),
                # tags$head(includeHTML(("google-analytics.html"))),
                
                tags$style(css),
                tags$head(
                  tags$script(src = "ddpcr.js"),
                  tags$head(tags$link(rel = "shortcut icon", href = "favicon.png")),
                  tags$meta(name = "application-name", content = "SARI"),
                  tags$meta(name = "description", content = "Interactive & online GNSS position time series analysis tool"),
                  tags$meta(name = "keywords", content = "SARI,sari,GNSS,time,series,analysis"),
                  tags$meta(name = "author", content = "Alvaro Santamaría-Gómez"),
                  tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
                  tags$html(lang = "en"),
                  tags$style(HTML("
                      .popover {width: 15%; color: #ffffff; background-color: #446e9b; font-size: small; position: absolute; z-index: 9999;}
                      .arrow { border-left-color: #8447cf; }
                      .navbar-nav { width: 98%;}
                      .navbar-nav li:nth-child(6) { float: right }
                      .navbar-nav li:nth-child(7) { float: right }
                      .navbar-nav li:nth-child(2) { width: 17%; }
                      .navbar-nav li:nth-child(1) { width: 17%; }
                      .tabbable > .nav > li[class=active] > a { background-color: black; color:white; }
                      .shiny-notification { color: #ffffff; background-color: #446e9b; font-size: large; font-weight: bold; border: 3px solid #000000; padding: 10px 8px 10px 10px; margin: 2px; }
                      .shiny-notification-warning { color: #ffffff; }
                      .shiny-notification-error { color: #fbc317; }
                      .shiny-notification-message { color: #ffffff; background-color: #446e9b; font-size: large; font-weight: bold; border: 3px solid #000000; padding: 10px 8px 10px 10px; margin: 2px; position:fixed; top: 0; left: calc(28%); width: 71%}
                      .shiny-notification-close:hover { color: #ffffff; }
                      .fa-caret-down { float: right; }
                      # Fonts are obtained here https://fonts.google.com/
                      @import url('https://fonts.googleapis.com/css?family=Share+Tech+Mono
                       ")
                  ),
                  tags$style(type = "text/css", "#inline label{ display: table-cell; text-align: left; vertical-align: middle; padding: 0px 20px;} #inline .form-group { display: table-row; padding: 0px 20px;}"),
                  tags$style(type = 'text/css', 'form.well { height: 96vh; overflow-y: auto; width: 100%}'),
                  
                  # Getting user screen size (from https://stackoverflow.com/questions/36995142/get-the-size-of-the-window-in-shiny)
                  tags$script('
                                var size = [0, 0];
                                var tactile = 9;
                                $(document).on("shiny:connected", function(e) {
                                    size[0] = window.outerWidth;
                                    size[1] = window.outerHeight;
                                    Shiny.onInputChange("size", size);
                                });
                                $(window).resize(function(e) {
                                    size[0] = window.outerWidth;
                                    size[1] = window.outerHeight;
                                    Shiny.onInputChange("size", size);
                                });
                                $(document).on("shiny:connected", function(e) {
                                    tactile = navigator.maxTouchPoints;
                                    Shiny.onInputChange("tactile", tactile);
                                });
                            ')
                ),
                
                sidebarLayout(position = "left", fluid = T,
                              div( id = "menu_all", 
                                   
                                   # Sidebar menu ####
                                   sidebarPanel(
                                     width = 4,
                                     style = "position:fixed;width:inherit;",
                                     id = "side-panel",
                                     bsCollapse(id = "menu", open = c(1,2), multiple = T,
                                                
                                                # Expandable/collapsible blocks
                                                
                                                # * Input data and format ####
                                                bsCollapsePanel(value = 1,
                                                                tags$h4(style = "color:white", icon("database", class = NULL, lib = "font-awesome"), "Input data and format",  icon("caret-down", class = NULL, lib = "font-awesome")),
                                                                div(style = "padding: 0px 0px; margin-top:-2em",
                                                                    fluidRow(
                                                                      column(4,
                                                                             br(),
                                                                             div(style = "font-weight: bold", "Input series",
                                                                                 helpPopup("Select a column-based text file; comments must start with '#'")
                                                                             ),
                                                                             div(style = "margin-right: -1em", tags$a(href = "TLSE.neu", "Show file example", targe = "_blank"))
                                                                      ),
                                                                      column(8,
                                                                             fileInput(inputId = "series", label = "", multiple = F, buttonLabel = "Browse file ...", placeholder = "Empty")
                                                                      )
                                                                    )
                                                                ),
                                                                fluidRow(
                                                                  column(4,
                                                                         div(style = "font-weight: bold; margin-top:-1.75em",
                                                                             textInput(inputId = "ids", label = "Series ID", value = "")
                                                                         )
                                                                  ),
                                                                  column(8,
                                                                         fileInput(inputId = "loadSARI", label = NULL, multiple = F, accept = NULL, width = NULL, buttonLabel = "Load SARI model", placeholder = "Empty")
                                                                  )
                                                                ),
                                                                div(style = "padding: 0px 0px; margin-top:-20em",
                                                                    fluidRow(
                                                                      column(4,
                                                                             div(style = "font-weight: bold", "Series format", helpPopup("Select 1D if the other formats are unknown"))
                                                                      ),
                                                                      column(8,
                                                                             radioButtons(inputId = "format", label = NULL, choices = list("NEU/ENU" = 1, "PBO" = 2, "NGL" = 3, "1D" = 4), selected = 1, inline = T, width = "auto"),
                                                                      )
                                                                    )
                                                                ),
                                                                div(style = "padding: 0px 0px; margin-top:-0em",
                                                                    fluidRow(
                                                                      column(6,
                                                                             checkboxInput(inputId = "sigmas", label = "Use error bars", value = T),
                                                                             checkboxInput(inputId = "header", label = "Show series header", value = F),
                                                                             conditionalPanel(
                                                                               condition = "input.header == true",
                                                                               sliderInput(inputId = "lines", label = "Number of lines", min = 1, max = 50, value = 10))
                                                                      ),
                                                                      column(6,
                                                                             radioButtons(inputId = "tunits",
                                                                                          div("Time units",
                                                                                              helpPopup("These are the units of the time axis, not the series sampling. They are used to define periods of time in several options.")),
                                                                                          choices = list("Days" = 1, "Weeks" = 2, "Years" = 3), selected = 3, inline = F)
                                                                      )
                                                                    )
                                                                ),
                                                                conditionalPanel(
                                                                  condition = "input.format == 4",
                                                                  fluidRow(
                                                                    column(6,
                                                                           selectInput(inputId = "separator", label = "Column separation", choices = list("Blank/Tab" = 1, "Comma" = 2, "Semi-colon" = 3), selected = 1, multiple = F, selectize = T)
                                                                    ),
                                                                    column(6,
                                                                           div(style = "font-weight: bold", "Column selection",
                                                                               helpPopup("Select the column numbers for the epochs, data and the errorbars, respectively.")
                                                                           ),
                                                                           div(style = "margin-top:-1em",
                                                                               fluidRow(
                                                                                 column(4,
                                                                                        textInput(inputId = "epoch", label = "", value = "1")
                                                                                 ),
                                                                                 column(4, offset = -2,
                                                                                        textInput(inputId = "variable", label = "", value = "2")
                                                                                 ),
                                                                                 column(4, offset = -2,
                                                                                        textInput(inputId = "errorBar", label = "", value = "3")
                                                                                 )
                                                                               )
                                                                           )
                                                                    )
                                                                  )
                                                                ),
                                                                fluidRow(
                                                                  column(6, 
                                                                         checkboxInput(inputId = "average", 
                                                                                       div("Reduce sampling",
                                                                                           helpPopup("To compute the moving average of the series for a given non-overlapping window pediod between twice the time series sampling and half the time series length. The period must be given in the same units as the time axis in the series.")), 
                                                                                       value = F)
                                                                  ),
                                                                  column(6,
                                                                         div(style = "padding: 0px 0px; margin-top:1em",
                                                                             conditionalPanel(
                                                                               condition = "input.average == true",
                                                                               textInput(inputId = "step", label = "Averaging time period", value = "")
                                                                             )
                                                                         )
                                                                  )
                                                                ),
                                                                div(style = "padding: 0px 0px; margin-top: -1em",
                                                                    tags$hr(style = "border-color: black; border-top: 1px solid #000000;")
                                                                ),
                                                                div(style = "padding: 0px 0px; margin-top: -1em",
                                                                    conditionalPanel(
                                                                      condition = "output.data",
                                                                      htmlOutput("information")
                                                                    ),
                                                                    conditionalPanel(
                                                                      condition = "output.dataNone",
                                                                      htmlOutput("informationNone")
                                                                    )
                                                                ),
                                                                style = "primary"),
                                                
                                                # * Plot controls ####
                                                bsCollapsePanel(value = 2,
                                                                tags$h4(style = "color:white", icon("gamepad", class = "NULL", lib = "font-awesome"), "Plot controls", icon("caret-down", class = NULL, lib = "font-awesome")),
                                                                radioButtons(inputId = "symbol", label = NULL, choices = list("Points" = 0, "Lines" = 1, "Points & Lines" = 2), selected = 0, inline = T),
                                                                fluidRow(
                                                                  column(2, style = 'padding:0px 10px 0px 10px;', align = "left",
                                                                         actionButton(inputId = "plot", label = "Plot", icon = icon("eye", class = NULL, lib = "font-awesome"), style = "font-size: small")
                                                                  ),
                                                                  column(2, style = 'padding:0px 10px 0px 10px;', align = "left",
                                                                         actionButton(inputId = "reset", label = "Reset", icon = icon("eye-slash", class = NULL, lib = "font-awesome"), style = "font-size: small; color: #FF6700")
                                                                  ),
                                                                  column(4, style = 'padding:0px 0px 0px 0px;', align = "right",
                                                                         actionButton(inputId = "remove", label = "Toggle points", icon =  icon("ban", class = NULL, lib = "font-awesome"), style = "font-size: small")
                                                                  ),
                                                                  column(4, style = 'padding:0px 10px 0px 0px;', align = "right",
                                                                         actionButton(inputId = "delete_excluded", label = "Reset toggle", icon = icon("backward", class = NULL, lib = "font-awesome"), style = "font-size: small")
                                                                  )
                                                                ),
                                                                div(style = "padding: 0px 0px; margin-top:1em",
                                                                    fluidRow(
                                                                      column(4,
                                                                             textInput(inputId = "thresholdRes", 
                                                                                       div("Residual",
                                                                                           helpPopup("Threshold to delete points with larger absolute residual")),
                                                                                       value = NULL)
                                                                      ),
                                                                      column(4, style = "padding:0px 10px 0px 0px;", align = "left",
                                                                             textInput(inputId = "thresholdResN", 
                                                                                       div("Norm. residual",
                                                                                           helpPopup("Threshold to delete points with larger normalized absolute residual")),
                                                                                       value = NULL)
                                                                      ),
                                                                      column(4, style = "padding:0px 10px 0px 0px; margin-top:1.75em", align = "right",
                                                                             actionButton(inputId = "removeAuto", label = "Auto toggle", icon =  icon("car", class = NULL, lib = "font-awesome"), style = "font-size: small")#width = NULL)
                                                                      )
                                                                    )
                                                                ),
                                                                fluidRow(
                                                                  column(4,
                                                                         checkboxInput(inputId = "remove3D",
                                                                                       div("All components", align = "right",
                                                                                           helpPopup("To remove points from all components simultaneously")),
                                                                                       value = T)
                                                                  ),
                                                                  column(4,
                                                                         checkboxInput(inputId = "add_excluded",
                                                                                       div("Include in file", align = "right",
                                                                                           helpPopup("To include the removed points in the downloaded results file")),
                                                                                       value = F)
                                                                  ),
                                                                  column(4,
                                                                         checkboxInput(inputId = "overflow", 
                                                                                       div("Scrolling",
                                                                                           helpPopup("Enables or disables the vertical scrolling of the left panel. When the scrolling is deactivated, the user can take a screenshot of the full web page.")),
                                                                                       value = T),
                                                                  )
                                                                ),
                                                                style = "primary"),
                                                
                                                # * Ancillary information ####
                                                bsCollapsePanel(value = 3,
                                                                tags$h4(style = "color:white", icon("upload", class = NULL, lib = "font-awesome"), "Ancillary information", icon("caret-down", class = NULL, lib = "font-awesome")),
                                                                
                                                                # % sitelog ####
                                                                div(style = "padding: 0px 0px; margin-top:0em",
                                                                    fluidRow(
                                                                      column(6,
                                                                             div(style = "font-weight: bold", "Input log file",
                                                                                 helpPopup("IGS-like sitelog file")
                                                                             )
                                                                      ),
                                                                      column(6, align = "right",
                                                                             tags$a(href = "iraf00fra_20201021.log", "Show file example", target = "_blank")
                                                                      )
                                                                    ),
                                                                    fluidRow(
                                                                      column(8,
                                                                             fileInput(inputId = "log", label = NULL, multiple = F, buttonLabel = "Browse file ...", placeholder = "Empty")
                                                                      ),
                                                                      column(4,
                                                                             div(style = "padding: 0px 0px; margin-top:-1em",
                                                                                 conditionalPanel(
                                                                                   condition = "output.log",
                                                                                   checkboxInput(inputId = "traceLog", label = "Plot changes", value = F), 
                                                                                   checkboxInput(inputId = "printLog", label = "List changes", value = F)
                                                                                 )
                                                                             )
                                                                      )
                                                                    )
                                                                ),
                                                                
                                                                # % station.info ####
                                                                div(style = "padding: 0px 0px; margin-top:-1em",
                                                                    fluidRow(
                                                                      column(6,
                                                                             div(style = "font-weight: bold", "Input station.info file",
                                                                                 helpPopup("GAMIT/GLOBK-like station.info file")
                                                                             )
                                                                      ),
                                                                      column(6, align = "right",
                                                                             tags$a(href = "station.info", "Show file example", target = "_blank")
                                                                      )
                                                                    ),
                                                                    fluidRow(
                                                                      column(8,
                                                                             fileInput(inputId = "sinfo", label = NULL, multiple = F, buttonLabel = "Browse file ...", placeholder = "Empty")
                                                                      ),
                                                                      column(4,
                                                                             div(style = "padding: 0px 0px; margin-top:-1em",
                                                                                 conditionalPanel(
                                                                                   condition = "output.sinfo",
                                                                                   checkboxInput(inputId = "traceSinfo", label = "Plot changes", value = F),
                                                                                   checkboxInput(inputId = "printSinfo", label = "List changes", value = F)
                                                                                 )
                                                                             )
                                                                      )
                                                                    )
                                                                ),
                                                                
                                                                # % soln ####
                                                                div(style = "padding: 0px 0px; margin-top:-1em",
                                                                    fluidRow(
                                                                      column(6,
                                                                             div(style = "font-weight: bold", "Input soln file",
                                                                                 helpPopup("IGS-like soln file")
                                                                             )
                                                                      ),
                                                                      column(6, align = "right",
                                                                             tags$a(href = "soln.snx", "Show file example", target = "_blank")
                                                                      )
                                                                    ),
                                                                    fluidRow(
                                                                      column(8,
                                                                             fileInput(inputId = "soln", label = NULL, multiple = F, buttonLabel = "Browse file ...", placeholder = "Empty")
                                                                      ),
                                                                      column(4,
                                                                             div(style = "padding: 0px 0px; margin-top:-1em",
                                                                                 conditionalPanel(
                                                                                   condition = "output.soln",
                                                                                   checkboxInput(inputId = "traceSoln", label = "Plot changes", value = F),
                                                                                   checkboxInput(inputId = "printSoln", label = "List changes", value = F)
                                                                                 )
                                                                             )
                                                                      )
                                                                    )
                                                                ),
                                                                
                                                                # % Custom ####
                                                                div(style = "padding: 0px 0px; margin-top:-1em",
                                                                    fluidRow(
                                                                      column(6,
                                                                             div(style = "font-weight: bold", "Input custom offset file",
                                                                                 helpPopup("User-defined offset list to be shown")
                                                                             )
                                                                      ),
                                                                      column(6, align = "right",
                                                                             tags$a(href = "http://geodesy.unr.edu/NGLStationPages/steps.txt", "Show file example", target = "_blank")
                                                                      )
                                                                    ),
                                                                    fluidRow(
                                                                      column(8,
                                                                             fileInput(inputId = "custom", label = NULL, multiple = F, buttonLabel = "Browse file ...", placeholder = "Empty")
                                                                      ),
                                                                      column(4,
                                                                             div(style = "padding: 0px 0px; margin-top:-1em",
                                                                                 conditionalPanel(
                                                                                   condition = "output.custom",
                                                                                   checkboxInput(inputId = "traceCustom", label = "Plot changes", value = F),
                                                                                   checkboxInput(inputId = "printCustom", label = "List changes", value = F)
                                                                                 )
                                                                             )
                                                                      )
                                                                    )
                                                                ),
                                                                
                                                                # % Secondary series ####
                                                                div(style = "padding: 0px 0px; margin-top:-1em",
                                                                    fluidRow(
                                                                      column(8, 
                                                                             fileInput(inputId = "series2", 
                                                                                       div("Secondary series",
                                                                                           helpPopup("Secondary input series to show next to, subtract from or average with the primary series")),
                                                                                       multiple = F, buttonLabel = "Browse file ...", placeholder = "Empty")
                                                                      ),
                                                                      column(3, offset = 1,
                                                                             div(style = "padding: 0px 0px; margin-top:0em",
                                                                                 conditionalPanel(
                                                                                   condition = "output.series2",
                                                                                   radioButtons(inputId = "optionSecondary", label = NULL, choices = list("None" = 0, "Show" = 1, "Correct" = 2, "Average" = 3), selected = NULL, inline = F, width = NULL),
                                                                                 )
                                                                             )
                                                                      )
                                                                    ),
                                                                    conditionalPanel(
                                                                      condition = "output.series2",
                                                                      fluidRow(
                                                                        column(8,
                                                                               radioButtons(inputId = "typeSecondary", label = NULL, choices = list("Original" = 1, "Residual" = 2), selected = 1, inline = T),
                                                                               radioButtons(inputId = "format2", label = NULL, choices = list("NEU/ENU" = 1, "PBO" = 2, "NGL" = 3, "1D" = 4), selected = 1, inline = T, width = "auto")
                                                                        ),
                                                                        column(4,
                                                                               div(style = "padding: 0px 0px; margin-top:-1em",
                                                                                   checkboxInput(inputId = "sameScale",
                                                                                                 div("Equal scale",
                                                                                                     helpPopup("Force the y-axis of the secondary series on the right to have the same scale as the y-axis of the primary series on the left")),
                                                                                                 value = F),
                                                                                   checkboxInput(inputId = "same_axis",
                                                                                                 div("Same axis",
                                                                                                     helpPopup("Force the y-axis of the secondary series on the right to be the same as the y-axis of the primary series on the left")),
                                                                                                 value = F)
                                                                               )
                                                                        )
                                                                      )
                                                                    ),
                                                                    fluidRow(
                                                                      conditionalPanel(
                                                                        condition = "input.format2 == 4 && output.series2 == true",
                                                                        column(6,
                                                                               selectInput(inputId = "separator2", label = "Column separation", choices = list("Blank/Tab" = 1, "Comma" = 2, "Semi-colon" = 3), selected = 1, multiple = F, selectize = T)
                                                                        ),
                                                                        column(6,
                                                                               div(style = "font-weight: bold", "Column selection",
                                                                                   helpPopup("Select the column numbers for the epochs, data and the errorbars, respectively, of the secondary series.")
                                                                               ),
                                                                               div(style = "margin-top:-1em",
                                                                                   fluidRow(
                                                                                     column(4,
                                                                                            textInput(inputId = "epoch2", label = "", value = "1")
                                                                                     ),
                                                                                     column(4, offset = -2,
                                                                                            textInput(inputId = "variable2", label = "", value = "2")
                                                                                     ),
                                                                                     column(4, offset = -2,
                                                                                            textInput(inputId = "errorBar2", label = "", value = "3")
                                                                                     )
                                                                                   )
                                                                               )
                                                                        )
                                                                      )
                                                                    )
                                                                ),
                                                                
                                                                # % Euler ####
                                                                checkboxInput(inputId = "euler",
                                                                              div(style = "font-weight: bold", "Plate motion model",
                                                                                  helpPopup("Shows or removes a plate motion model at the series location given the parameters of an Euler pole")),
                                                                              value = F),
                                                                fluidRow(
                                                                  column(6,
                                                                         conditionalPanel(
                                                                           condition = "input.euler == true",
                                                                           div(style = "padding: 0px 0px; margin-top:0.75em",
                                                                               radioButtons(inputId = "eulerType", label = NULL, choices = list("None" = 0, "Show" = 1, "Remove" = 2), selected = 0, inline = T, width = NULL, choiceNames = NULL,  choiceValues = NULL)
                                                                           )
                                                                         )
                                                                  ),
                                                                  column(6,
                                                                         conditionalPanel(
                                                                           condition = "input.euler == true && input.format == 1",
                                                                           div(style = "padding: 0px 0px; margin-top:0.75em",
                                                                               radioButtons(inputId = "neuenu", label = NULL, choices = list("NEU" = 1, "ENU" = 2), selected = 1, inline = T, width = NULL, choiceNames = NULL, choiceValues = NULL)
                                                                           )
                                                                         )
                                                                  )
                                                                ),
                                                                conditionalPanel(
                                                                  condition = "input.euler == true",
                                                                  fluidRow(
                                                                    column(6,
                                                                           div("Station coordinates", helpPopup("Cartesian coordinates in the same units as the series. Geographic coordinates in decimal degrees"))
                                                                    ),
                                                                    column(6,
                                                                           radioButtons(inputId = "coordenadas_estacion", label = NULL, choices = list("Cartesian" = 1, "Geographic" = 2), selected = 1, inline = T, width = NULL, choiceNames = NULL,  choiceValues = NULL)
                                                                    )
                                                                  ),
                                                                  fluidRow(
                                                                    conditionalPanel(
                                                                      condition = "input.coordenadas_estacion == 1",
                                                                      column(4,
                                                                             textInput(inputId = "station_x", label = "Station X", value = "")
                                                                      ),
                                                                      column(4, offset = -2,
                                                                             textInput(inputId = "station_y", label = "Station Y", value = "")
                                                                      ),
                                                                      column(4, offset = -2,
                                                                             textInput(inputId = "station_z", label = "Station Z", value = "")
                                                                      )
                                                                    ),
                                                                    conditionalPanel(
                                                                      condition = "input.coordenadas_estacion == 2",
                                                                      column(4,
                                                                             textInput(inputId = "station_lat", label = "Station latitude", value = "")
                                                                      ),
                                                                      column(4, offset = -2,
                                                                             textInput(inputId = "station_lon", label = "Station longitude", value = "")
                                                                      )
                                                                    )
                                                                  ),
                                                                  fluidRow(
                                                                    column(6,
                                                                           div("Euler's pole", helpPopup("Cartesian rotation rates in decimal degrees/Ma. Geographic position in decimal degrees and rotation rate in decimal degrees/Ma"))
                                                                    ),
                                                                    column(6,
                                                                           radioButtons(inputId = "pole_coordinates", label = NULL, choices = list("Cartesian" = 1, "Geographic" = 2), selected = 1, inline = T, width = NULL, choiceNames = NULL,  choiceValues = NULL)
                                                                    )
                                                                  ),
                                                                  fluidRow(
                                                                    conditionalPanel(
                                                                      condition = "input.pole_coordinates == 1",
                                                                      column(4,
                                                                             textInput(inputId = "pole_x", label = "Pole rotation X", value = "")
                                                                      ),
                                                                      column(4, offset = -2,
                                                                             textInput(inputId = "pole_y", label = "Pole rotation Y", value = "")
                                                                      ),
                                                                      column(4, offset = -2,
                                                                             textInput(inputId = "pole_z", label = "Pole rotation Z", value = "")
                                                                      )
                                                                    ),
                                                                    conditionalPanel(
                                                                      condition = "input.pole_coordinates == 2",
                                                                      column(4,
                                                                             textInput(inputId = "pole_lat", label = "Pole latitude", value = "")
                                                                      ),
                                                                      column(4, offset = -2,
                                                                             textInput(inputId = "pole_lon", label = "Pole longitude", value = "")
                                                                      ),
                                                                      column(4, offset = -2,
                                                                             textInput(inputId = "pole_rot", label = "Pole rotation", value = "")
                                                                      )
                                                                    )
                                                                  ),
                                                                  fluidRow(
                                                                    column(6,
                                                                           div(style = "font-weight: bold", "Euler poles file",
                                                                               helpPopup("File with a list of stations and their associated Euler pole(s)")
                                                                           )
                                                                    ),
                                                                    column(6, align = "right",
                                                                           tags$a(href = "euler.txt", "Show file example", target = "_blank")
                                                                    )
                                                                  ),
                                                                  fileInput(inputId = "eulers", label = NULL, multiple = F, buttonLabel = "Browse file ...", placeholder = "Empty")
                                                                ),
                                                                style = "primary"),
                                                
                                                # * Fit controls ####
                                                bsCollapsePanel(value = 4,
                                                                tags$h4(style = "color:white", icon("wand-magic-sparkles", class = NULL, lib = "font-awesome"), "Fit controls",  icon("caret-down", class = NULL, lib = "font-awesome")),
                                                                div(style = "padding: 0px 0px; margin-top:0em",
                                                                    radioButtons(inputId = "fitType", label = NULL, choices = list("None" = 0, "LS" = 1, "KF" = 2), selected = 0, inline = T),
                                                                    conditionalPanel(
                                                                      condition = "input.fitType == 2",
                                                                      fluidRow(
                                                                        column(6,
                                                                               radioButtons(inputId = "kf", label = NULL, choices = list("EKF" = 1, "UKF" = 2), selected = 2, inline = T)
                                                                        ),
                                                                        column(width = 6, offset = 0, style = "margin-top:-2em; padding: 0px 40px 0px 0px", align = "right",
                                                                               withBusyIndicatorUI(
                                                                                 actionButton(inputId = "runKF", label = "Run KF", icon = icon("filter", class = NULL, lib = "font-awesome"), style = "font-size: small")
                                                                               )
                                                                        )
                                                                      ),
                                                                      fluidRow(
                                                                        column(7,
                                                                               checkboxInput(inputId = "errorm",
                                                                                             div("Compute measurement noise",
                                                                                                 helpPopup("To estimate the measurement noise within the provided bounds (WARNING: slower KF fit)")),
                                                                                             value = F)
                                                                        ),
                                                                        conditionalPanel(
                                                                          condition = "input.errorm == false",
                                                                          column(5,
                                                                                 textInput(inputId = "ObsError",
                                                                                           div("Measurement noise",
                                                                                               helpPopup("Measurement standard deviation in the same units as the observations. If empty, an approximate value will be used.")),
                                                                                           value = "")
                                                                          )
                                                                        )
                                                                      ),
                                                                      fluidRow(
                                                                        conditionalPanel(
                                                                          condition = "input.errorm == true",
                                                                          column(3, style = "padding: 0px 5px 0px 5px;", align = "left",
                                                                                 textInput(inputId = "min_optirange",
                                                                                           div("Min bound",
                                                                                               helpPopup("Lower & upper bounds of the measurement standard deviation in the same units as the observations")),
                                                                                           value = "")
                                                                          ),
                                                                          column(3,
                                                                                 textInput(inputId = "max_optirange", label = "Max bound", value = "")
                                                                          ),
                                                                          column(6,
                                                                                 htmlOutput("noise")
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                ),
                                                                conditionalPanel(
                                                                  condition = "input.fitType == 1 || input.fitType == 2",
                                                                  div(style = "padding: 0px 0px; margin-top:0.0em",
                                                                      tags$div(class = "header", checked = NA,
                                                                               tags$h4(icon("puzzle-piece", class = NULL, lib = "font-awesome"), "Select model components")
                                                                      )
                                                                  ),
                                                                  div(style = "padding: 0px 0px; margin-top:-1em",
                                                                      checkboxGroupInput(inputId = "model", label = "", choices = list("Linear","Polynomial","Sinusoidal","Offset","Exponential","Logarithmic"), selected = NULL, inline = T)
                                                                  ),
                                                                  
                                                                  # % Linear fit ####
                                                                  conditionalPanel(
                                                                    condition = "input.model.indexOf('Linear') != -1",
                                                                    div(style = "padding: 0px 0px; margin-top:0em",
                                                                        tags$hr(style = "border-color: black; border-top: 1px solid #000000;")
                                                                    ),
                                                                    fluidRow(
                                                                      column(6,
                                                                             conditionalPanel(
                                                                               condition = "input.fitType == 1",
                                                                               textInput(inputId = "trendRef",
                                                                                         div("Ref. epoch rate",
                                                                                             helpPopup("Reference epoch for the rate. If empty, the mean data epoch will be used.")),
                                                                                         value = "")
                                                                             )
                                                                      ),
                                                                      conditionalPanel(
                                                                        condition = "input.fitType == 2",
                                                                        column(6,
                                                                               textInput(inputId = "TrendDev", 
                                                                                         div("Rate process noise",
                                                                                             helpPopup("Rate variation (standard deviation) for each observation.")),
                                                                                         value = "0.0")
                                                                        )
                                                                      )
                                                                    ),
                                                                    conditionalPanel(
                                                                      condition = "input.fitType == 2",
                                                                      fluidRow(
                                                                        column(6,
                                                                               conditionalPanel(
                                                                                 condition = "input.fitType == 2",
                                                                                 textInput(inputId = "Intercept0", 
                                                                                           div("A priori intercept",
                                                                                               helpPopup("Initial state value for the intercept. If empty, an approximate value will be used.")),
                                                                                           value = "")
                                                                               )
                                                                        ),
                                                                        column(6,
                                                                               textInput(inputId = "eIntercept0", 
                                                                                         div("A priori intercept error",
                                                                                             helpPopup("Initial state uncertainty (standard deviation) for the intercept. If empty, an approximate value will be used.")),
                                                                                         value = "")
                                                                        )
                                                                      ),
                                                                      fluidRow(
                                                                        column(6,
                                                                               conditionalPanel(
                                                                                 condition = "input.fitType == 2",
                                                                                 textInput(inputId = "Trend0", 
                                                                                           div("A priori rate",
                                                                                               helpPopup("Initial state value for the rate. If empty, an approximate value will be used.")),
                                                                                           value = "")
                                                                               )
                                                                        ),
                                                                        column(6,
                                                                               textInput(inputId = "eTrend0", 
                                                                                         div("A priori rate error",
                                                                                             helpPopup("Initial state uncertainty (standard deviation) for the rate. If empty, an approximate value will be used.")),
                                                                                         value = "")
                                                                        )
                                                                      )
                                                                    )
                                                                  ),
                                                                  
                                                                  # % Sinusoidal fit ####
                                                                  conditionalPanel(
                                                                    condition = "input.model.indexOf('Sinusoidal') != -1",
                                                                    div(style = "padding: 0px 0px; margin-top:0em",
                                                                        tags$hr(style = "border-color: black; border-top: 1px solid #000000;")
                                                                    ),
                                                                    textInput(inputId = "period", 
                                                                              div("Sinusoidal periods",
                                                                                  helpPopup("Comma-separated list. Each period ended by<br/>d (for days)<br/>w (for weeks)<br/>y (for years)")), 
                                                                              value = "1y"),
                                                                    fluidRow(
                                                                      column(6,
                                                                             textInput(inputId = "periodRef", 
                                                                                       div("Ref. epoch periods",
                                                                                           helpPopup("Reference epoch for the phase of the periods. If empty, the mean data epoch will be used")), 
                                                                                       value = "")
                                                                      ),
                                                                      conditionalPanel(
                                                                        condition = "input.fitType == 2",
                                                                        column(6,
                                                                               textInput(inputId = "S0", 
                                                                                         div("A priori amplitude",
                                                                                             helpPopup("Initial state value for both sine & cosine amplitudes. If empty, an approximate value will be used.")), 
                                                                                         value = "")
                                                                        )
                                                                      )
                                                                    ),
                                                                    conditionalPanel(
                                                                      condition = "input.fitType == 2",
                                                                      fluidRow(
                                                                        column(6,
                                                                               textInput(inputId = "SinusoidalDev",
                                                                                         div("Amplitude process noise",
                                                                                             helpPopup("Sine/cosine amplitude variation (standard deviation) for each observation.")),
                                                                                         value = "0.0")                                              
                                                                        ),
                                                                        column(6,
                                                                               textInput(inputId = "eS0", 
                                                                                         div("A priori amplitude error",
                                                                                             helpPopup("Initial state uncertainty (standard deviation) for sine & cosine amplitudes. If empty, an approximate value will be used.")), 
                                                                                         value = "")
                                                                        )
                                                                      ),
                                                                      tags$div(id = "inline", 
                                                                               radioButtons(inputId = "SineCosine",
                                                                                            div("Amplitude process noise on",
                                                                                                helpPopup("Choose between varying the sine amplitude only or varying both the sine & cosine amplitudes independently.")),
                                                                                            choices = list("Sine" = 1, "Sine & Cosine" = 2), selected = 2, inline = T)
                                                                      )
                                                                    )
                                                                  ),
                                                                  
                                                                  # % Offset fit ####
                                                                  conditionalPanel(
                                                                    condition = "input.model.indexOf('Offset') != -1",
                                                                    div(style = "padding: 0px 0px; margin-top:0em",
                                                                        tags$hr(style = "border-color: black; border-top: 1px solid #000000;")
                                                                    ),
                                                                    textInput(inputId = "offsetEpoch", 
                                                                              div("Offset epochs",
                                                                                  helpPopup("Comma-separated list")), 
                                                                              value = ""),
                                                                    conditionalPanel(
                                                                      condition = "input.fitType == 2",
                                                                      fluidRow(
                                                                        column(6,
                                                                               textInput(inputId = "O0", 
                                                                                         div("A priori offset",
                                                                                             helpPopup("Initial state value for the offsets. If empty, an approximate value will be used.")), 
                                                                                         value = "")
                                                                        ),
                                                                        column(6,
                                                                               textInput(inputId = "eO0", 
                                                                                         div("A priori offset error",
                                                                                             helpPopup("Initial state uncertainty (standard deviation) for the offsets. If empty, an approximate value will be used.")), 
                                                                                         value = "")
                                                                        )
                                                                      )
                                                                    ),
                                                                    fluidRow(
                                                                      column(6,
                                                                             withBusyIndicatorUI(
                                                                               actionButton(inputId = "search", label = "Search discontinuities", icon = icon("magnifying-glass", class = NULL, lib = "font-awesome"), style = "font-size: small")
                                                                             )
                                                                      ),
                                                                      column(6,
                                                                             sliderInput("segmentLength", 
                                                                                         div("Minimum segment",
                                                                                             helpPopup("Minimum segment size given as % of the series length")),
                                                                                         min = 0.1, max = 50, value = 10, step = 1, round = 0, ticks = F, animate = F, width = NULL, sep = "", pre = NULL, post = NULL, timeFormat = NULL, timezone = NULL, dragRange = TRUE)
                                                                      )
                                                                    ),
                                                                    htmlOutput("offsetFound"),
                                                                    fluidRow(
                                                                      column(6,
                                                                             checkboxInput(inputId = "verif_offsets",
                                                                                           div("Offset verification",
                                                                                               helpPopup("To test if the estimated offsets may be due to random noise fluctuations.<br/>Roughly, if the estimated log-likelihood difference is < 6, offsets are likely generated by random noise."),
                                                                                               value = F))
                                                                      ),
                                                                      column(width = 6, offset = 0, style = "margin-top:0em; padding: 0px 0px 0px 0px", align = "left",
                                                                             conditionalPanel(
                                                                               condition = "input.verif_offsets == true",
                                                                               withBusyIndicatorUI(
                                                                                 actionButton(inputId = "runVerif", label = "Run verification", icon = icon("shuffle", class = NULL, lib = "font-awesome"), style = "font-size: small")
                                                                               )
                                                                             )
                                                                      )
                                                                    ),
                                                                    conditionalPanel(
                                                                      condition = "input.verif_offsets == true",
                                                                      fluidRow(
                                                                        column(4,
                                                                               textInput(inputId = "verif_white", 
                                                                                         div("White noise",
                                                                                             helpPopup("Standard deviation of the expected white noise in the series.")), 
                                                                                         value = "")
                                                                        ),
                                                                        column(4,
                                                                               textInput(inputId = "verif_pl", 
                                                                                         div("Power-law",
                                                                                             helpPopup("Stardard deviation of the expected power-law noise in the series.")), 
                                                                                         value = "")
                                                                        ),
                                                                        column(4,
                                                                               textInput(inputId = "verif_k", 
                                                                                         div("Spectral index",
                                                                                             helpPopup("Spectral index of the expected power-law noise in the series.")), 
                                                                                         value = "")
                                                                        )
                                                                      )
                                                                    ),
                                                                    conditionalPanel(
                                                                      condition = "output.verifhelp",
                                                                      fluidRow(
                                                                        div(style = "padding: 0px 30px;",
                                                                            htmlOutput("verif", inline = T)
                                                                        )
                                                                      )
                                                                    )
                                                                  ),
                                                                  
                                                                  # % Exponential fit ####
                                                                  conditionalPanel(
                                                                    condition = "input.model.indexOf('Exponential') != -1",
                                                                    div(style = "padding: 0px 0px; margin-top:0em",
                                                                        tags$hr(style = "border-color: black; border-top: 1px solid #000000;")
                                                                    ),
                                                                    textInput(inputId = "ExponenRef", 
                                                                              div("Ref. time exponential",
                                                                                  helpPopup("Comma-separated lisf of the starting time for the exponential decays.")), 
                                                                              value = ""),
                                                                    fluidRow(
                                                                      column(6,
                                                                             textInput(inputId = "E0", 
                                                                                       div("A priori constant",
                                                                                           helpPopup("Initial value for the asymptotic offsets. If empty, an approximate value will be used.")), 
                                                                                       value = "")
                                                                      ),
                                                                      column(6,
                                                                             textInput(inputId = "TE0", 
                                                                                       div("A priori decay rate",
                                                                                           helpPopup("Initial value for the exponential decay rates. If empty, an approximate value will be used.")), 
                                                                                       value = "")
                                                                      )
                                                                    ),
                                                                    conditionalPanel(
                                                                      condition = "input.fitType == 2",
                                                                      fluidRow(
                                                                        column(6,
                                                                               textInput(inputId = "eE0", 
                                                                                         div("A priori constant error",
                                                                                             helpPopup("Initial state uncertainty (standard deviation) for the asymptotic offsets. If empty, an approximate value will be used.")), 
                                                                                         value = "")
                                                                        ),
                                                                        column(6,
                                                                               textInput(inputId = "eTE0", 
                                                                                         div("A priori decay rate error",
                                                                                             helpPopup("Initial state uncertainty (standard deviation) for the exponential decay rates. If empty, an approximate value will be used.")), 
                                                                                         value = "")
                                                                        )
                                                                      )
                                                                    )
                                                                  ),
                                                                  
                                                                  # % Logarithmic fit ####
                                                                  conditionalPanel(
                                                                    condition = "input.model.indexOf('Logarithmic') != -1",
                                                                    div(style = "padding: 0px 0px; margin-top:0em",
                                                                        tags$hr(style = "border-color: black; border-top: 1px solid #000000;")
                                                                    ),
                                                                    textInput(inputId = "LogariRef", 
                                                                              div("Ref. time logarithmic",
                                                                                  helpPopup("Comma-separated lisf of the starting time for the logarithmic decays.")), 
                                                                              value = ""),
                                                                    fluidRow( 
                                                                      column(6,
                                                                             textInput(inputId = "L0", 
                                                                                       div("A priori constant",
                                                                                           helpPopup("Initial value for the asymptotic offsets. If empty, an approximate value will be used.")), 
                                                                                       value = "")
                                                                      ),
                                                                      column(6,
                                                                             textInput(inputId = "TL0", 
                                                                                       div("A priori decay rate",
                                                                                           helpPopup("Initial value for the logarithmic decay rates. If empty, an approximate value will be used.")), 
                                                                                       value = "")
                                                                      )
                                                                    ),
                                                                    conditionalPanel(
                                                                      condition = "input.fitType == 2",
                                                                      fluidRow(
                                                                        column(6,
                                                                               textInput(inputId = "eL0", 
                                                                                         div("A priori constant error",
                                                                                             helpPopup("Initial state uncertainty (standard deviation) for the asymptotic offsets. If empty, an approximate value will be used.")), 
                                                                                         value = "")
                                                                        ),
                                                                        column(6,
                                                                               textInput(inputId = "eTL0", 
                                                                                         div("A priori decay rate error",
                                                                                             helpPopup("Initial state uncertainty (standard deviation) for the logarithmic decay rates. If empty, an approximate value will be used.")), 
                                                                                         value = "")
                                                                        )
                                                                      )
                                                                    )
                                                                  ),
                                                                  
                                                                  # % Polynomial fit ####
                                                                  conditionalPanel(
                                                                    condition = "input.model.indexOf('Polynomial') != -1",
                                                                    div(style = "padding: 0px 0px; margin-top:0em",
                                                                        tags$hr(style = "border-color: black; border-top: 1px solid #000000;")
                                                                    ),
                                                                    fluidRow(
                                                                      column(6,
                                                                             textInput(inputId = "PolyRef",
                                                                                       div("Ref. epoch polynomial",
                                                                                           helpPopup("Reference epoch for the polynomial. If empty, the rate reference epoch or the mean data epoch will be used.")),
                                                                                       value = "")
                                                                      ),
                                                                      column(6,
                                                                             textInput(inputId = "PolyCoef",
                                                                                       div("Polynomial degree",
                                                                                           helpPopup("The degree must be > 1 & < 20. Degrees 0 and 1 are estimated within the linear component.")),
                                                                                       value = "")
                                                                      )
                                                                    ),
                                                                    conditionalPanel(
                                                                      condition = "input.fitType == 2",
                                                                      fluidRow(
                                                                        column(6,
                                                                               textInput(inputId = "P0", 
                                                                                         div("A priori polynomial",
                                                                                             helpPopup("Initial state value for the polynomial coefficients. If empty, an approximate value will be used.")),
                                                                                         value = "")
                                                                        ),
                                                                        column(6,
                                                                               textInput(inputId = "eP0", 
                                                                                         div("A priori polynomial error",
                                                                                             helpPopup("Initial state uncertainty (standard deviation) for the polynomial coefficients. If empty, an approximate value will be used.")),
                                                                                         value = "")
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                ),
                                                                style = "primary"),
                                                
                                                # * Additional fit ####
                                                bsCollapsePanel(value = 5,
                                                                tags$h4(style = "color:white", icon("magnifying-glass-plus", class = NULL, lib = "font-awesome"), "Additional fit", icon("caret-down", class = NULL, lib = "font-awesome")),
                                                                
                                                                # % Histogram ####
                                                                fluidRow(
                                                                  column(6,
                                                                         checkboxInput(inputId = "histogram", label = "Histogram", value = F)
                                                                  ),
                                                                  column(6,
                                                                         checkboxInput(inputId = "midas", 
                                                                                       div("MIDAS",
                                                                                           helpPopup("Median Interannual Difference Adjusted for Skewness")),
                                                                                       value = F)
                                                                  )
                                                                ),
                                                                conditionalPanel(
                                                                  condition = "input.histogram == true",
                                                                  radioButtons(inputId = "histogramType", label = NULL, choices = list("None" = 0, "Original" = 1, "Model" = 2, "Model res." = 3, "Filter" = 4, "Filter res." = 5), selected = 0, inline = T, width = NULL, choiceNames = NULL,  choiceValues = NULL),
                                                                  div(style = "padding: 0px 0px; margin-top:0em",
                                                                      tags$hr(style = "border-color: black; border-bottom: 1px solid #000000;")
                                                                  )
                                                                ),
                                                                
                                                                # % Waveform ####
                                                                fluidRow(
                                                                  column(6,
                                                                         checkboxInput(inputId = "waveform", 
                                                                                       div("Periodic waveform",
                                                                                           helpPopup("To fit a periodic waveform not having a sinusoidal shape")),
                                                                                       value = F)
                                                                  ),
                                                                  column(6,
                                                                         conditionalPanel(
                                                                           condition = "input.waveform == true",
                                                                           textInput(inputId = "waveformPeriod", 
                                                                                     div("Period",
                                                                                         helpPopup("The waveform period is given in the same units as the time unit of the series. It must be bigger than twice the average sampling period and smaller than half the total period of the series.")), 
                                                                                     value = "")
                                                                         )
                                                                  )
                                                                ),
                                                                conditionalPanel(
                                                                  condition = "input.waveform == true",
                                                                  fluidRow(
                                                                    column(6,
                                                                           div(style = "margin-top:-4em",
                                                                               checkboxInput(inputId = "correct_waveform", 
                                                                                             div("Remove from series",
                                                                                                 helpPopup("To remove the estimated periodic waveform from the original series before the model fit")),
                                                                                             value = F)
                                                                           )
                                                                    )
                                                                  ),
                                                                  div(style = "padding: 0px 0px; margin-top:-1em",
                                                                      tags$hr(style = "border-color: black; border-bottom: 1px solid #000000;")
                                                                  )
                                                                ),
                                                                
                                                                # % Periodogram ####
                                                                fluidRow(
                                                                  column(5,
                                                                         checkboxInput(inputId = "spectrum", 
                                                                                       div("Periodogram",
                                                                                           helpPopup("Lomb-Scargle amplitude/power spectrum")), 
                                                                                       value = F)
                                                                  ),
                                                                  column(6, offset = 1,
                                                                         div(style = "margin-top:0.6em;",
                                                                             conditionalPanel(
                                                                               condition = "input.spectrum == true",
                                                                               radioButtons(inputId = "spectrumType", label = NULL, choices = list("Amplitude" = 0, "Power" = 1), selected = 0, inline = T, width = NULL, choiceNames = NULL,  choiceValues = NULL)
                                                                             )
                                                                         )
                                                                  )
                                                                ),
                                                                conditionalPanel(
                                                                  condition = "input.spectrum == true",
                                                                  fluidRow(
                                                                    column(2,
                                                                           checkboxInput(inputId = "spectrumOriginal", label = "Original", value = F)
                                                                    ),
                                                                    column(2,
                                                                           checkboxInput(inputId = "spectrumModel", label = "Model", value = F)
                                                                    ),
                                                                    column(3,
                                                                           checkboxInput(inputId = "periodogram_residuals", label = "Model res.", value = F)
                                                                    ),
                                                                    column(2,
                                                                           checkboxInput(inputId = "spectrumFilter", label = "Filter", value = F)
                                                                    ),
                                                                    column(3,
                                                                           checkboxInput(inputId = "spectrumFilterRes", label = "Filter res.", value = F)
                                                                    )
                                                                  )
                                                                ),
                                                                conditionalPanel(
                                                                  condition = "input.spectrum == true",
                                                                  fluidRow(
                                                                    column(4,
                                                                           textInput(inputId = "ofac", label = "Oversampling", value = NULL)
                                                                    ),
                                                                    column(4,
                                                                           textInput(inputId = "long_period", label = "Longest period", value = NULL)
                                                                    ),
                                                                    column(4,
                                                                           textInput(inputId = "short_period", label = "Shortest period", value = NULL)
                                                                    )
                                                                  ),
                                                                  div(style = "padding: 0px 0px; margin-top:-0.5em",
                                                                      tags$hr(style = "border-color: black; border-bottom: 1px solid #000000;")
                                                                  )
                                                                ),
                                                                
                                                                # % Wavelet ####
                                                                div(style = "padding: 0px 0px; margin-top:0em",
                                                                    fluidRow(
                                                                      column(4,
                                                                             checkboxInput(inputId = "wavelet", 
                                                                                           div("Wavelets",
                                                                                               helpPopup("To plot the heatmap from the wavelet transform")),
                                                                                           value = F)
                                                                      ),
                                                                      column(4, offset = 4,
                                                                             conditionalPanel(
                                                                               condition = "input.wavelet == true",
                                                                               textInput(inputId = "loc_wavelet",
                                                                                         div("Sampling",
                                                                                             helpPopup("The temporal resolution or time separation between wavelets. The maximum valid is half the total observed period. The minimum valid is the sampling period of the series.")),
                                                                                         value = "")
                                                                             )
                                                                      )
                                                                    ),
                                                                    conditionalPanel(
                                                                      condition = "input.wavelet == true",
                                                                      fluidRow(
                                                                        column(4,
                                                                               textInput(inputId = "min_wavelet",
                                                                                         div("Min.",
                                                                                             helpPopup("The shortest period to compute the wavelet. The minimum valid is twice the median sampling period.")),
                                                                                         value = "")
                                                                        ),
                                                                        column(4,
                                                                               textInput(inputId = "max_wavelet",
                                                                                         div("Max.",
                                                                                             helpPopup("The longest period to compute the wavelet. The maximum valid is half the total observed period.")),
                                                                                         value = "")
                                                                        ),
                                                                        column(4,
                                                                               textInput(inputId = "res_wavelet",
                                                                                         div("Step",
                                                                                             helpPopup("The resolution or separation between the periods to compute the wavelet.")),
                                                                                         value = "")
                                                                        )
                                                                      )
                                                                    )
                                                                ),
                                                                conditionalPanel(
                                                                  condition = "input.wavelet == true",
                                                                  radioButtons(inputId = "waveletType", label = NULL, choices = list("None" = 0, "Original" = 1, "Model" = 2, "Model res." = 3, "Filter" = 4, "Filter res." = 5), selected = 0, inline = T, width = NULL, choiceNames = NULL,  choiceValues = NULL),
                                                                  div(style = "padding: 0px 0px; margin-top:-0.5em",
                                                                      tags$hr(style = "border-color: black; border-bottom: 1px solid #000000;")
                                                                  )
                                                                ),
                                                                
                                                                # % Vondrak ####
                                                                div(style = "padding: 0px 0px; margin-top:0em",
                                                                    fluidRow( 
                                                                      column(6, 
                                                                             checkboxInput(inputId = "filter", 
                                                                                           div("Band-pass smoother",
                                                                                               helpPopup("Vondrak smoother for the original or residual series.")), 
                                                                                           value = F)
                                                                      ),
                                                                      div(style = "padding: 0px 0px; margin-top:1em",
                                                                          column(6,
                                                                                 conditionalPanel(
                                                                                   condition = "input.filter == true",
                                                                                   div(style = "padding: 0px 0px; margin-top:-0.4em",
                                                                                       radioButtons(inputId = "series2filter", label = NULL, choices = list("Original" = 1, "Residual" = 2), selected = 1, inline = T)
                                                                                   )
                                                                                 )
                                                                          )
                                                                      )
                                                                    ),
                                                                    conditionalPanel(
                                                                      condition = "input.filter == true",
                                                                      fluidRow(
                                                                        column(6,
                                                                               textInput(inputId = "low", 
                                                                                         div("Low-pass period cutoff",
                                                                                             helpPopup("Maximum recommended = T/4 (T = series length)")), 
                                                                                         value = "")
                                                                        ),
                                                                        column(6,
                                                                               textInput(inputId = "high", 
                                                                                         div("High-pass period cutoff",
                                                                                             helpPopup("Maximum recommended = T/4 (T = series length)")), 
                                                                                         value = "")
                                                                        )
                                                                      ),
                                                                      div(style = "padding: 0px 0px; margin-top:-0.5em",
                                                                          tags$hr(style = "border-color: black; border-bottom: 1px solid #000000;")
                                                                      )
                                                                    )
                                                                ),
                                                                
                                                                # % Noise ####
                                                                div(style = "padding: 0px 0px; margin-top:0em",
                                                                    fluidRow(
                                                                      column(6,
                                                                             checkboxInput(inputId = "mle",
                                                                                           div("Noise analysis",
                                                                                               helpPopup("To estimate the parameters of a noise model for the covariance matrix of the residual series")),
                                                                                           value = F)
                                                                      ),
                                                                      column(width = 6, offset = 0, style = "margin-top:0em; padding: 0px 0px 0px 0px", align = "left",
                                                                             conditionalPanel(
                                                                               condition = "input.mle == true",
                                                                               withBusyIndicatorUI(
                                                                                 actionButton(inputId = "runmle", label = "Run MLE", icon = icon("hourglass", class = NULL, lib = "font-awesome"), style = "font-size: small")
                                                                               )
                                                                             )
                                                                      )
                                                                    ),
                                                                    fluidRow(
                                                                      conditionalPanel(
                                                                        condition = "input.mle == true",
                                                                        column(2, style = "padding: 0px 0px 0px 10px;",
                                                                               checkboxInput(inputId = "white", label = "White", value = F, width = '25%')
                                                                        ),
                                                                        column(2, style = "padding: 0px 0px 0px 0px;",
                                                                               checkboxInput(inputId = "flicker", label = "Flicker", value = F, width = '25%')
                                                                        ),
                                                                        column(4, style = "padding: 0px 0px 0px 0px;",
                                                                               checkboxInput(inputId = "randomw", label = "Random walk", value = F, width = '200%')
                                                                        ),
                                                                        column(4, style = "padding: 0px 10px 0px 10px;",
                                                                               checkboxInput(inputId = "powerl", label = "Power-law", value = F, width = '150%')
                                                                        )
                                                                      )
                                                                    ),
                                                                    fluidRow(
                                                                      column(4,
                                                                             conditionalPanel(
                                                                               condition = "input.white",
                                                                               textInput(inputId = "max_white", label = "max wn", value = "")
                                                                             )
                                                                      ),
                                                                      column(4,
                                                                             conditionalPanel(
                                                                               condition = "input.flicker",
                                                                               textInput(inputId = "max_fl", label = "max fn", value = "")
                                                                             ),
                                                                             conditionalPanel(
                                                                               condition = "input.powerl",
                                                                               textInput(inputId = "max_pl", label = "max pl", value = "")
                                                                             )
                                                                      ),
                                                                      column(4,
                                                                             conditionalPanel(
                                                                               condition = "input.randomw",
                                                                               textInput(inputId = "max_rw", label = "max rw", value = "")
                                                                             ),
                                                                             conditionalPanel(
                                                                               condition = "input.powerl",
                                                                               textInput(inputId = "max_k", label = "max k", value = "")
                                                                             )
                                                                      )
                                                                    ),
                                                                    fluidRow(
                                                                      column(4,
                                                                             conditionalPanel(
                                                                               condition = "input.white",
                                                                               textInput(inputId = "min_white", label = "min wn", value = "")
                                                                             )
                                                                      ),
                                                                      column(4,
                                                                             conditionalPanel(
                                                                               condition = "input.flicker",
                                                                               textInput(inputId = "min_fl", label = "min fn", value = "")
                                                                             ),
                                                                             conditionalPanel(
                                                                               condition = "input.powerl",
                                                                               textInput(inputId = "min_pl", label = "min pl", value = "")
                                                                             )
                                                                      ),
                                                                      column(4,
                                                                             conditionalPanel(
                                                                               condition = "input.randomw",
                                                                               textInput(inputId = "min_rw", label = "min rw", value = "")
                                                                             ),
                                                                             conditionalPanel(
                                                                               condition = "input.powerl",
                                                                               textInput(inputId = "min_k", label = "min k", value = "")
                                                                             )
                                                                      )
                                                                    ),
                                                                    fluidRow(
                                                                      column(4,
                                                                             conditionalPanel(
                                                                               condition = "input.white == true",
                                                                               htmlOutput("est.white")
                                                                             )
                                                                      ),
                                                                      conditionalPanel(
                                                                        condition = "input.powerl == true",
                                                                        column(4,
                                                                               htmlOutput("est.powerl")
                                                                        ),
                                                                        column(4,
                                                                               htmlOutput("est.index")
                                                                        )
                                                                      ),
                                                                      conditionalPanel(
                                                                        condition = "input.powerl != true",
                                                                        column(4,
                                                                               conditionalPanel(
                                                                                 condition = "input.flicker == true",
                                                                                 htmlOutput("est.flicker")
                                                                               )
                                                                        ),
                                                                        column(4,
                                                                               conditionalPanel(
                                                                                 condition = "input.randomw == true",
                                                                                 htmlOutput("est.randomw")
                                                                               )
                                                                        )
                                                                      )
                                                                    ),
                                                                    conditionalPanel(
                                                                      condition = "input.mle == true",
                                                                      htmlOutput("est.mle"),
                                                                      htmlOutput("est.unc")
                                                                    )
                                                                ),
                                                                style = "primary")
                                     ),
                                     
                                     # * Local download ####
                                     bsCollapse(id = "localDir", open = "", multiple = F,
                                                bsCollapsePanel(value = 6,
                                                                tags$h4(style = "color:white", icon("box-archive", class = NULL, lib = "font-awesome"), "Save results"),
                                                                fluidRow(
                                                                  column(2,
                                                                         downloadButton(outputId = "downloadAs", label = "Save as")
                                                                  ),
                                                                  column(10,
                                                                         verbatimTextOutput("localDirectory", placeholder = T)
                                                                  )
                                                                ),
                                                                div(style = "padding: 0px 0px; margin-top:1em",
                                                                    fluidRow(
                                                                      column(8,
                                                                             textInput(inputId = "directory", 
                                                                                       div("Select directory",
                                                                                           helpPopup("Copy & paste or type in the complete path to the desired download directory")), 
                                                                                       value = NULL)
                                                                      ),
                                                                      column(4,
                                                                             div(style = "padding: 0px 0px; margin-top:2em",
                                                                                 actionButton(inputId = "autoDownload", label = "Automatic save", icon = icon("floppy-disk", class = NULL, lib = "font-awesome"), style = "font-size: small")
                                                                             )
                                                                      )
                                                                    )
                                                                ),
                                                                actionButton(inputId = "browser", label = "Debug pit stop"),
                                                                style = "primary")
                                     )
                                   )
                              ),
                              
                              # Visualization panel ####
                              mainPanel(
                                # style = "position:fixed; right: 0px; height: 90vh; overflow-y: auto;",
                                id = "main-panel",
                                conditionalPanel(
                                  condition = "input.header == true",
                                  verbatimTextOutput("header", placeholder = F)
                                ),
                                fluidPage(
                                  tableOutput('debug')
                                ),
                                navbarPage(
                                  title = "", windowTitle = version, id = "tab", selected = 1, position = "fixed-top", header = NULL, footer = NULL, inverse = F, collapsible = T, fluid = T, theme = NULL,
                                  tabPanel(div(style = "display: inline-block; font-size: 40px; color: black", "SARI"), value = 0, id = "SARI"),
                                  tabPanel(div(style = "margin-top:-3.5em; font-size: 25px; display: inline-block;","Help"), value = 4, icon = icon("circle-info", class = "fas fa-2x"),
                                           withMathJax(includeMarkdown("www/about.md"))
                                  ),
                                  
                                  # * component 1 ####
                                  tabPanel(div(style = "font-size: 20px;",uiOutput("tabName")), value = 1,
                                           tags$style(type = "text/css", "body {padding-top: 60px;}"),
                                           withSpinner(
                                             plotOutput("plot1", click = "plot_1click", dblclick = "plot_2click", brush = brushOpts(id = "plot_brush", resetOnNew = T, fill = 'red', stroke = 'black', opacity = '0.5', clip = T)),
                                             type = getOption("spinner.type", default = 1),
                                             color = getOption("spinner.color", default = "#0080ff"),
                                             size = getOption("spinner.size", default = 2),
                                             color.background = getOption("spinner.color.background", default = "#ffffff"),
                                             custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", "plot1")) NULL else "400px"
                                           ),
                                           conditionalPanel(
                                             condition = "output.run",
                                             withSpinner(
                                               plotOutput("res1", click = "plot_1click", dblclick = "res_2click", brush = brushOpts(id = "res_brush", resetOnNew = T, fill = 'blue', stroke = 'black', opacity = '0.5', clip = T)),
                                               type = getOption("spinner.type", default = 1),
                                               color = getOption("spinner.color", default = "#0080ff"),
                                               size = getOption("spinner.size", default = 2),
                                               color.background = getOption("spinner.color.background", default = "#ffffff"),
                                               custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", "res1")) NULL else "400px"
                                             )
                                           ),
                                           verbatimTextOutput("plot1_info", placeholder = F),
                                           conditionalPanel(
                                             condition = "output.rate",
                                             withSpinner(
                                               plotOutput("rate1", click = "plot_1click", dblclick = "rate_2click", brush = brushOpts(id = "rate_brush", resetOnNew = T, fill = 'blue', stroke = 'black', opacity = '0.5', clip = T)),
                                               type = getOption("spinner.type", default = 1),
                                               color = getOption("spinner.color", default = "#0080ff"),
                                               size = getOption("spinner.size", default = 2),
                                               color.background = getOption("spinner.color.background", default = "#ffffff"),
                                               custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", "res1")) NULL else "400px"
                                             )
                                           ),
                                           conditionalPanel(
                                             condition = "input.filter == true && input.low !== input.high && output.residuals == false && input.series2filter == 1",
                                             withSpinner(
                                               plotOutput("vondrak1", click = "plot_1click", dblclick = "res_2click", brush = brushOpts(id = "vondrak_brush", resetOnNew = T, fill = 'blue', stroke = 'black', opacity = '0.5', clip = T)),
                                               type = getOption("spinner.type", default = 1),
                                               color = getOption("spinner.color", default = "#0080ff"),
                                               size = getOption("spinner.size", default = 2),
                                               color.background = getOption("spinner.color.background", default = "#ffffff"),
                                               custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", "res1")) NULL else "400px"
                                             )
                                           ),
                                           conditionalPanel(
                                             condition = "input.filter == true && input.low !== input.high && output.residuals == true && input.series2filter == 2",
                                             withSpinner(
                                               plotOutput("Vondrak1", click = "plot_1click", dblclick = "res_2click", brush = brushOpts(id = "vondrak_brush", resetOnNew = T, fill = 'blue', stroke = 'black', opacity = '0.5', clip = T)),
                                               type = getOption("spinner.type", default = 1),
                                               color = getOption("spinner.color", default = "#0080ff"),
                                               size = getOption("spinner.size", default = 2),
                                               color.background = getOption("spinner.color.background", default = "#ffffff"),
                                               custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", "res1")) NULL else "400px"
                                             )
                                           ),
                                           conditionalPanel(
                                             condition = "input.waveform == true && input.waveformPeriod.length > 0",
                                             withSpinner(
                                               plotOutput("waveform1", click = "plot_1click", dblclick = "waveform_2click", brush = brushOpts(id = "waveform_brush", resetOnNew = T, fill = 'blue', stroke = 'black', opacity = '0.5', clip = T)),
                                               type = getOption("spinner.type", default = 1),
                                               color = getOption("spinner.color", default = "#0080ff"),
                                               size = getOption("spinner.size", default = 2),
                                               color.background = getOption("spinner.color.background", default = "#ffffff"),
                                               custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", "res1")) NULL else "400px"
                                             )
                                           ),
                                           conditionalPanel(
                                             condition = "input.printLog == true",
                                             verbatimTextOutput("changes_ant1", placeholder = F),
                                             verbatimTextOutput("changes_rec1", placeholder = F)
                                           ),
                                           conditionalPanel(
                                             condition = "input.printSinfo == true",
                                             verbatimTextOutput("changes_ant1s", placeholder = F),
                                             verbatimTextOutput("changes_rec1s", placeholder = F)
                                           ),
                                           conditionalPanel(
                                             condition = "input.printSoln == true",
                                             verbatimTextOutput("changes_ant1so", placeholder = F),
                                             verbatimTextOutput("changes_rec1so", placeholder = F)
                                           ),
                                           conditionalPanel(
                                             condition = "input.printCustom == true",
                                             verbatimTextOutput("changes_ant1c", placeholder = F)
                                           ),
                                           conditionalPanel(
                                             condition = "input.model.length > 0 || input.midas == true || input.optionSecondary == 1",
                                             verbatimTextOutput("summary1", placeholder = F)
                                           ),
                                           conditionalPanel(
                                             condition = "input.histogram == true && input.histogramType > 0",
                                             plotOutput("hist1"),
                                             verbatimTextOutput("stats1")
                                           ),
                                           conditionalPanel(
                                             condition = "input.midas == true",
                                             plotOutput("midas_hist1")
                                           ),
                                           conditionalPanel(
                                             condition = "input.spectrumOriginal == true || input.spectrumModel == true || input.periodogram_residuals == true || input.spectrumFilter == true || input.spectrumFilterRes == true",
                                             withSpinner(
                                               plotOutput("res1_espectral", click = "lomb_1click", dblclick = "lomb_2click", brush = brushOpts(id = "lomb_brush", resetOnNew = T, fill = 'blue', stroke = 'black', opacity = '0.5', clip = T)),
                                               type = getOption("spinner.type", default = 1),
                                               color = getOption("spinner.color", default = "#0080ff"),
                                               size = getOption("spinner.size", default = 2),
                                               color.background = getOption("spinner.color.background", default = "#ffffff"),
                                               custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", "res1")) NULL else "400px"
                                             ),
                                             downloadLink('downloadSpectrum1', div(style = "margin-top:0em; font-size: 10px; text-align: right;","Get periodogram data")),
                                             verbatimTextOutput("lomb1_info", placeholder = F)
                                           ),
                                           conditionalPanel(
                                             condition = "input.wavelet == true && input.waveletType.length > 0",
                                             withSpinner(
                                               plotOutput("wavelet1", click = "wavelet_1click"),
                                               type = getOption("spinner.type", default = 1),
                                               color = getOption("spinner.color", default = "#0080ff"),
                                               size = getOption("spinner.size", default = 2),
                                               color.background = getOption("spinner.color.background", default = "#ffffff"),
                                               custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", "res1")) NULL else "400px"
                                             ),
                                             verbatimTextOutput("wavelet1_info", placeholder = F)
                                           )
                                  ),
                                  
                                  # * component 2 ####
                                  tabPanel(div(style = "font-size: 20px;","2nd component"), value = 2,
                                           tags$style(type = "text/css", "body {padding-top: 60px;}"),
                                           withSpinner(
                                             plotOutput("plot2", click = "plot_1click", dblclick = "plot_2click", brush = brushOpts(id = "plot_brush", resetOnNew = T, fill = 'red', stroke = 'black', opacity = '0.5', clip = T)),
                                             type = getOption("spinner.type", default = 1),
                                             color = getOption("spinner.color", default = "#0080ff"),
                                             size = getOption("spinner.size", default = 2),
                                             color.background = getOption("spinner.color.background", default = "#ffffff"),
                                             custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", "plot2")) NULL else "400px"
                                           ),
                                           conditionalPanel(
                                             condition = "output.run",
                                             withSpinner(
                                               plotOutput("res2", click = "plot_1click", dblclick = "res_2click", brush = brushOpts(id = "res_brush", resetOnNew = T, fill = 'blue', stroke = 'black', opacity = '0.5', clip = T)),
                                               type = getOption("spinner.type", default = 1),
                                               color = getOption("spinner.color", default = "#0080ff"),
                                               size = getOption("spinner.size", default = 2),
                                               color.background = getOption("spinner.color.background", default = "#ffffff"),
                                               custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", "res2")) NULL else "400px"
                                             )
                                           ),
                                           verbatimTextOutput("plot2_info", placeholder = F),
                                           conditionalPanel(
                                             condition = "output.rate",
                                             withSpinner(
                                               plotOutput("rate2", click = "plot_1click", dblclick = "rate_2click", brush = brushOpts(id = "rate_brush", resetOnNew = T, fill = 'blue', stroke = 'black', opacity = '0.5', clip = T)),
                                               type = getOption("spinner.type", default = 1),
                                               color = getOption("spinner.color", default = "#0080ff"),
                                               size = getOption("spinner.size", default = 2),
                                               color.background = getOption("spinner.color.background", default = "#ffffff"),
                                               custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", "res1")) NULL else "400px"
                                             )
                                           ),
                                           conditionalPanel(
                                             condition = "input.filter == true && input.low !== input.high && output.residuals == false && input.series2filter == 1",
                                             withSpinner(
                                               plotOutput("vondrak2", click = "plot_1click", dblclick = "res_2click", brush = brushOpts(id = "vondrak_brush", resetOnNew = T, fill = 'blue', stroke = 'black', opacity = '0.5', clip = T)),
                                               type = getOption("spinner.type", default = 1),
                                               color = getOption("spinner.color", default = "#0080ff"),
                                               size = getOption("spinner.size", default = 2),
                                               color.background = getOption("spinner.color.background", default = "#ffffff"),
                                               custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", "res2")) NULL else "400px"
                                             )
                                           ),
                                           conditionalPanel(
                                             condition = "input.filter == true && input.low !== input.high && output.residuals == true && input.series2filter == 2",
                                             withSpinner(
                                               plotOutput("Vondrak2", click = "plot_1click", dblclick = "res_2click", brush = brushOpts(id = "vondrak_brush", resetOnNew = T, fill = 'blue', stroke = 'black', opacity = '0.5', clip = T)),
                                               type = getOption("spinner.type", default = 1),
                                               color = getOption("spinner.color", default = "#0080ff"),
                                               size = getOption("spinner.size", default = 2),
                                               color.background = getOption("spinner.color.background", default = "#ffffff"),
                                               custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", "res2")) NULL else "400px"
                                             )
                                           ),
                                           conditionalPanel(
                                             condition = "input.waveform == true && input.waveformPeriod.length > 0",
                                             withSpinner(
                                               plotOutput("waveform2", click = "plot_1click", dblclick = "waveform_2click", brush = brushOpts(id = "waveform_brush", resetOnNew = T, fill = 'blue', stroke = 'black', opacity = '0.5', clip = T)),
                                               type = getOption("spinner.type", default = 1),
                                               color = getOption("spinner.color", default = "#0080ff"),
                                               size = getOption("spinner.size", default = 2),
                                               color.background = getOption("spinner.color.background", default = "#ffffff"),
                                               custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", "res2")) NULL else "400px"
                                             )
                                           ),
                                           conditionalPanel(
                                             condition = "input.printLog == true",
                                             verbatimTextOutput("changes_ant2", placeholder = F),
                                             verbatimTextOutput("changes_rec2", placeholder = F)
                                           ),
                                           conditionalPanel(
                                             condition = "input.printSinfo == true",
                                             verbatimTextOutput("changes_ant2s", placeholder = F),
                                             verbatimTextOutput("changes_rec2s", placeholder = F)
                                           ),
                                           conditionalPanel(
                                             condition = "input.printSoln == true",
                                             verbatimTextOutput("changes_ant2so", placeholder = F),
                                             verbatimTextOutput("changes_rec2so", placeholder = F)
                                           ),
                                           conditionalPanel(
                                             condition = "input.printCustom == true",
                                             verbatimTextOutput("changes_ant2c", placeholder = F)
                                           ),
                                           conditionalPanel(
                                             condition = "input.model.length > 0 || input.midas == true || input.optionSecondary == 1",
                                             verbatimTextOutput("summary2", placeholder = F)
                                           ),
                                           conditionalPanel(
                                             condition = "input.histogram == true && input.histogramType > 0",
                                             plotOutput("hist2"),
                                             verbatimTextOutput("stats2")
                                           ),
                                           conditionalPanel(
                                             condition = "input.midas == true",
                                             plotOutput("midas_hist2")
                                           ),
                                           conditionalPanel(
                                             condition = "input.spectrumOriginal == true || input.spectrumModel == true || input.periodogram_residuals == true || input.spectrumFilter == true || input.spectrumFilterRes == true",
                                             withSpinner(
                                               plotOutput("res2_espectral", click = "lomb_1click", dblclick = "lomb_2click", brush = brushOpts(id = "lomb_brush", resetOnNew = T, fill = 'blue', stroke = 'black', opacity = '0.5', clip = T)),
                                               type = getOption("spinner.type", default = 1),
                                               color = getOption("spinner.color", default = "#0080ff"),
                                               size = getOption("spinner.size", default = 2),
                                               color.background = getOption("spinner.color.background", default = "#ffffff"),
                                               custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", "res2")) NULL else "400px"
                                             ),
                                             downloadLink('downloadSpectrum2', div(style = "margin-top:0em; font-size: 10px; text-align: right;","Get periodogram data")),
                                             verbatimTextOutput("lomb2_info", placeholder = F)
                                           ),
                                           conditionalPanel(
                                             condition = "input.wavelet == true && input.waveletType.length > 0",
                                             # div(style = "background-color: OliveDrab;",
                                             withSpinner(
                                               plotOutput("wavelet2", click = "wavelet_1click"),
                                               type = getOption("spinner.type", default = 1),
                                               color = getOption("spinner.color", default = "#0080ff"),
                                               size = getOption("spinner.size", default = 2),
                                               color.background = getOption("spinner.color.background", default = "#ffffff"),
                                               custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", "res2")) NULL else "400px"
                                             ),
                                             verbatimTextOutput("wavelet2_info", placeholder = F)
                                             # )
                                           )
                                  ),
                                  
                                  # * component 3 ####
                                  tabPanel(div(style = "font-size: 20px;","3rd component"), value = 3,
                                           tags$style(type = "text/css", "body {padding-top: 60px;}"),
                                           withSpinner(
                                             plotOutput("plot3", click = "plot_1click", dblclick = "plot_2click", brush = brushOpts(id = "plot_brush", resetOnNew = T, fill = 'red', stroke = 'black', opacity = '0.5', clip = T)),
                                             type = getOption("spinner.type", default = 1),
                                             color = getOption("spinner.color", default = "#0080ff"),
                                             size = getOption("spinner.size", default = 2),
                                             color.background = getOption("spinner.color.background", default = "#ffffff"),
                                             custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", "plot3")) NULL else "400px"
                                           ),
                                           conditionalPanel(
                                             condition = "output.run",
                                             withSpinner(
                                               plotOutput("res3", click = "plot_1click", dblclick = "res_2click", brush = brushOpts(id = "res_brush", resetOnNew = T, fill = 'blue', stroke = 'black', opacity = '0.5', clip = T)),
                                               type = getOption("spinner.type", default = 1),
                                               color = getOption("spinner.color", default = "#0080ff"),
                                               size = getOption("spinner.size", default = 2),
                                               color.background = getOption("spinner.color.background", default = "#ffffff"),
                                               custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", "res3")) NULL else "400px"
                                             )
                                           ),
                                           verbatimTextOutput("plot3_info", placeholder = F),
                                           conditionalPanel(
                                             condition = "output.rate",
                                             withSpinner(
                                               plotOutput("rate3", click = "plot_1click", dblclick = "rate_2click", brush = brushOpts(id = "rate_brush", resetOnNew = T, fill = 'blue', stroke = 'black', opacity = '0.5', clip = T)),
                                               type = getOption("spinner.type", default = 1),
                                               color = getOption("spinner.color", default = "#0080ff"),
                                               size = getOption("spinner.size", default = 2),
                                               color.background = getOption("spinner.color.background", default = "#ffffff"),
                                               custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", "res1")) NULL else "400px"
                                             )
                                           ),
                                           conditionalPanel(
                                             condition = "input.filter == true && input.low !== input.high && output.residuals == false && input.series2filter == 1",
                                             withSpinner(
                                               plotOutput("vondrak3", click = "plot_1click", dblclick = "res_2click", brush = brushOpts(id = "vondrak_brush", resetOnNew = T, fill = 'blue', stroke = 'black', opacity = '0.5', clip = T)),
                                               type = getOption("spinner.type", default = 1),
                                               color = getOption("spinner.color", default = "#0080ff"),
                                               size = getOption("spinner.size", default = 2),
                                               color.background = getOption("spinner.color.background", default = "#ffffff"),
                                               custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", "res3")) NULL else "400px"
                                             )
                                           ),
                                           conditionalPanel(
                                             condition = "input.filter == true && input.low !== input.high && output.residuals == true && input.series2filter == 2",
                                             withSpinner(
                                               plotOutput("Vondrak3", click = "plot_1click", dblclick = "res_2click", brush = brushOpts(id = "vondrak_brush", resetOnNew = T, fill = 'blue', stroke = 'black', opacity = '0.5', clip = T)),
                                               type = getOption("spinner.type", default = 1),
                                               color = getOption("spinner.color", default = "#0080ff"),
                                               size = getOption("spinner.size", default = 2),
                                               color.background = getOption("spinner.color.background", default = "#ffffff"),
                                               custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", "res3")) NULL else "400px"
                                             )
                                           ),
                                           conditionalPanel(
                                             condition = "input.waveform == true && input.waveformPeriod.length > 0",
                                             withSpinner(
                                               plotOutput("waveform3", click = "plot_1click", dblclick = "waveform_2click", brush = brushOpts(id = "waveform_brush", resetOnNew = T, fill = 'blue', stroke = 'black', opacity = '0.5', clip = T)),
                                               type = getOption("spinner.type", default = 1),
                                               color = getOption("spinner.color", default = "#0080ff"),
                                               size = getOption("spinner.size", default = 2),
                                               color.background = getOption("spinner.color.background", default = "#ffffff"),
                                               custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", "res3")) NULL else "400px"
                                             )
                                           ),
                                           conditionalPanel(
                                             condition = "input.printLog == true",
                                             verbatimTextOutput("changes_ant3", placeholder = F),
                                             verbatimTextOutput("changes_rec3", placeholder = F)
                                           ),
                                           conditionalPanel(
                                             condition = "input.printSinfo == true",
                                             verbatimTextOutput("changes_ant3s", placeholder = F),
                                             verbatimTextOutput("changes_rec3s", placeholder = F)
                                           ),
                                           conditionalPanel(
                                             condition = "input.printSoln == true",
                                             verbatimTextOutput("changes_ant3so", placeholder = F),
                                             verbatimTextOutput("changes_rec3so", placeholder = F)
                                           ),
                                           conditionalPanel(
                                             condition = "input.printCustom == true",
                                             verbatimTextOutput("changes_ant3c", placeholder = F)
                                           ),
                                           conditionalPanel(
                                             condition = "input.model.length > 0 || input.midas == true || input.optionSecondary == 1",
                                             verbatimTextOutput("summary3", placeholder = F)
                                           ),
                                           conditionalPanel(
                                             condition = "input.histogram == true && input.histogramType > 0",
                                             plotOutput("hist3"),
                                             verbatimTextOutput("stats3")
                                           ),
                                           conditionalPanel(
                                             condition = "input.midas == true",
                                             plotOutput("midas_hist3")
                                           ),
                                           conditionalPanel(
                                             condition = "input.spectrumOriginal == true || input.spectrumModel == true || input.periodogram_residuals == true || input.spectrumFilter == true || input.spectrumFilterRes == true",
                                             withSpinner(
                                               plotOutput("res3_espectral", click = "lomb_1click", dblclick = "lomb_2click", brush = brushOpts(id = "lomb_brush", resetOnNew = T, fill = 'blue', stroke = 'black', opacity = '0.5', clip = T)),
                                               type = getOption("spinner.type", default = 1),
                                               color = getOption("spinner.color", default = "#0080ff"),
                                               size = getOption("spinner.size", default = 2),
                                               color.background = getOption("spinner.color.background", default = "#ffffff"),
                                               custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", "res3")) NULL else "400px"
                                             ),
                                             downloadLink('downloadSpectrum3', div(style = "margin-top:0em; font-size: 10px; text-align: right;","Get periodogram data")),
                                             verbatimTextOutput("lomb3_info", placeholder = F)
                                           ),
                                           conditionalPanel(
                                             condition = "input.wavelet == true && input.waveletType.length > 0",
                                             withSpinner(
                                               plotOutput("wavelet3", click = "wavelet_1click"),
                                               type = getOption("spinner.type", default = 1),
                                               color = getOption("spinner.color", default = "#0080ff"),
                                               size = getOption("spinner.size", default = 2),
                                               color.background = getOption("spinner.color.background", default = "#ffffff"),
                                               custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", "res3")) NULL else "400px"
                                             ),
                                             verbatimTextOutput("wavelet3_info", placeholder = F)
                                           )
                                  ),
                                  tabPanel(title = downloadLink('print_out', div(style = "margin-top:-3.5em; font-size: 25px; display: inline-block; font-family: sans-serif; font-weight: normal;","Print"), class = "fa fa-print", style = "font-size:30px; margin-top:-0.9em"), value = 6),
                                  tabPanel(title = downloadLink('download', div(style = "margin-top:-3.5em; font-size: 25px; display: inline-block; font-family: sans-serif; font-weight: normal;","Save"), class = "fa-solid fa-floppy-disk", style = "font-size:30px; margin-top:-0.9em"), value = 5)
                                )
                              )
                )
)




server <- function(input,output,session) {
  cat(file = stderr(), "\n", "\n", "START", "\n")
  
  # Debugging (from https://www.r-bloggers.com/2019/02/a-little-trick-for-debugging-shiny/?msclkid=3fafd7f3bc9911ec9c1253a868203435)
  observeEvent(input$browser,{
    browser()
  })
  
  # Initialize reactive variables of the global database
  
  # 1. input files.
  file <- reactiveValues(primary = NULL, secondary = NULL, id1 = NULL, id2 = NULL, sitelog = NULL, euler = NULL)
  
  # 2. series ranges:
  #   x1 = original time axis
  #   x2 = residual time axis
  #   x3 = periods
  #   y1 = primary series
  #   y12 = secondary series
  #   y2 = residual series
  #   y3 = amplitude/power
  #   y4 = instantaneous rate
  ranges <- reactiveValues(x1 = NULL, y1 = NULL, y12 = NULL, x2 = NULL, y2 = NULL, x3 = NULL, y3 = NULL, y4 = NULL)
  
  # 3. series info
  info <- reactiveValues(points = NULL, directory = NULL, log = NULL, sinfo = NULL, soln = NULL, custom = NULL, 
                         custom_warn = 0, tab = NULL, stop = NULL, noise = NULL, decimalsx = NULL, 
                         decimalsy = NULL, menu = c(1,2), sampling = NULL, rangex = NULL, step = 0, errorbars = T,
                         minx = NULL, maxx = NULL, miny = NULL, maxy = NULL, width = isolate(session$clientData$output_plot1_width),
                         run = F, regular = NULL, tunits = NULL, run_wavelet = T, run_filter = T, pixelratio = NULL, welcome = T)
  
  # 4. valid points
  values <- reactiveValues(used1 = NULL, excluded1 = NULL, used2 = NULL, excluded2 = NULL, 
                           used3 = NULL, excluded3 = NULL, used_all = NULL, excluded_all = NULL, used_all_kf = NULL)
  
  # 5. user input
  inputs <- reactiveValues(thresholdRes = NULL, thresholdResN = NULL, trendRef = NULL, period = NULL, 
                           periodRef = NULL, offsetEpoch = NULL, ExponenRef = NULL, E0 = NULL, TE0 = NULL, 
                           LogariRef = NULL, L0 = NULL, TL0 = NULL, PolyRef = NULL, PolyCoef = NULL, ofac = NULL, 
                           long_period = NULL, short_period = NULL, low = NULL, high = NULL)
  obs <- reactiveVal()
  
  # 6. computed values  
  trans <- reactiveValues(x0 = NULL, y0 = NULL, sy0 = NULL, x = NULL, y = NULL, sy = NULL, xe = NULL, ye = NULL, 
                          sye = NULL, z = NULL, sz = NULL, res = NULL, results = NULL, mod = NULL, filter = NULL, 
                          filterRes = NULL, kalman = NULL, equation = NULL, ordinate = NULL, midas_vel = NULL, 
                          midas_sig = NULL, midas_all = NULL, tol = NULL, midas_vel2 = NULL, midas_sig2 = NULL, 
                          mle = NULL, verif = NULL, pattern = NULL, unc = NULL, vondrak = NULL, wave = NULL,
                          noise = NULL, fs = NULL, names = NULL, LScoefs = NULL, fs = NULL, amp = NULL, psd = NULL, 
                          col = NULL, spectra = NULL, spectra_old = NULL, title = NULL, var = NULL, wavelet = NULL, 
                          model_old = NULL, plate = NULL, offsetEpochs = NULL, x0_kf = NULL)
  
  # 7. output
  OutPut <- reactiveValues(df = NULL)
  output_excluded <- reactiveValues(df = NULL)
  
  # Constants ####
  # Some initial values and constants
  options(shiny.maxRequestSize = 30*1024^2, width = 280, max.print = 50)
  daysInYear <- 365.2425 # Gregorian year
  degMa2radyr <- pi/180000000 # geologic to geodetic conversion
  local = Sys.getenv('SHINY_PORT') == "" # detect local connection
  debug <- F # saving the environment 
  messages <- 4 # print step by step messages on the console depending on the verbosity level (0, 1, 2, 3)
  toggleClass( # disabling clicking on SARI name (panic button)
    class = "disabled",
    selector = "#tab li a[data-value=0]"
  )
  
  # Welcome ####
  observe({
    req(input$size)
    if (length(input$isMobile) > 0 && input$isMobile) {
      cat(file = stderr(), "Mobil connection ", "\n")
      cat(file = stderr(), "Screen size ", input$size[1], "x", input$size[2], "\n")
      cat(file = stderr(), "Touchscreen ", input$tactile, "\n")
      shinyjs::hide(id = "menu")
      shinyjs::hide(selector = "#tab li a[data-value=1]")
      shinyjs::hide(selector = "#tab li a[data-value=2]")
      shinyjs::hide(selector = "#tab li a[data-value=3]")
      shinyjs::hide(selector = "#tab li a[data-value=6]")
      updateNavbarPage(session, "tab", selected = "4")
      shinyjs::hide(selector = "#tab li a[data-value=4]")
      showModal(modalDialog(
        title = tags$h3(style = "color: black; font-weight: bold; text-align: center;", "Dear user"),
        size = "m",
        easyClose = F,
        fade = F,
        tags$h3(style = "color: black; font-weight: bold; text-align: center;", "It is strongly discouraged to use SARI on small-screen devices."),
        tags$h3(style = "color: black; font-weight: bold; text-align: center;", "Please, consider using a desktop connection instead."),
        tags$h3(style = "color: blue; text-align: center;", "https://alvarosg.shinyapps.io/sari")
      ))
    } else {
      if (messages > 2) cat(file = stderr(), isolate(paste("Fixed width = ",info$width, " System width = ",session$clientData$output_plot1_width, " Pixel ratio = ",session$clientData$pixelratio)), "\n")
      if (!isTruthy(info$pixelratio)) info$pixelratio <- session$clientData$pixelratio
      if (info$pixelratio != session$clientData$pixelratio) {
        showNotification("The size and or resolution of the browser window has been modified. Please consider refreshing the web page.", action = NULL, duration = 10, closeButton = T, id = "kf_not_valid", type = "warning", session = getDefaultReactiveDomain())
      }
      if (local) {
        if (!is.null(dev.list())) dev.off()
        shinyjs::show("localDir")
      } else {
        if (messages > 2) cat(file = stderr(), "Screen size ", input$size[1], "x", input$size[2], "\n")
        if (messages > 2) cat(file = stderr(), "Pixel ratio ", info$pixelratio, "\n")
        if (messages > 2) cat(file = stderr(), "Touchscreen ", input$tactile, "\n")
        shinyjs::hide("localDir")
        if (isTRUE(info$welcome)) {
          showNotification("<<< It is strongly recommended to read the help content at least once to avoid mistakes and to make the most of this tool.", action = NULL, duration = 10, closeButton = T, id = "point_to_help", type = "message", session = getDefaultReactiveDomain())
          if (messages > 2) cat(file = stderr(), "Warning", "\n")
          if (isTruthy(input$tactile)) {
            if (input$tactile > 0) {
              if (messages > 1) cat(file = stderr(), "Touchscreen ", input$tactile, "\n")
              showModal(modalDialog(
                title = tags$h3(style = "color: black; font-weight: bold; text-align: center;", "Dear user"),
                size = "m",
                easyClose = T,
                fade = F,
                tags$h3(style = "color: black; font-weight: bold; text-align: center;", "It is strongly discouraged to use the touchscreen with SARI."),
                tags$h3(style = "color: black; font-weight: bold; text-align: center;", "Please, consider using the mouse instead.")
              ))
            }
          }
          info$welcome <- F
        }
      }
    }
  }, priority = 2000)
  
  # GUI reactive flags ####
  output$log <- reactive({
    return(!is.null(file$sitelog))
  })
  outputOptions(output, "log", suspendWhenHidden = F)
  
  output$sinfo <- reactive({
    return(!is.null(input$sinfo))
  })
  outputOptions(output, "sinfo", suspendWhenHidden = F)
  
  output$soln <- reactive({
    return(!is.null(input$soln))
  })
  outputOptions(output, "soln", suspendWhenHidden = F)
  
  output$custom <- reactive({
    return(!is.null(input$custom))
  })
  outputOptions(output, "custom", suspendWhenHidden = F)
  
  output$series2 <- reactive({
    return(!is.null(file$secondary))
  })
  outputOptions(output, "series2", suspendWhenHidden = F)
  
  output$run <- reactive({
    return(isTRUE(info$run))
  })
  outputOptions(output, "run", suspendWhenHidden = F)
  
  output$verifhelp <- reactive({
    return(isTruthy(trans$verif))
  })
  outputOptions(output, "verifhelp", suspendWhenHidden = F)
  
  output$data <- reactive({
    return(!is.null(info$rangex))
  })
  outputOptions(output, "data", suspendWhenHidden = F)
  
  output$dataNone <- reactive({
    return(is.null(info$rangex))
  })
  outputOptions(output, "dataNone", suspendWhenHidden = F)
  
  output$residuals <- reactive({
    return(!is.null(trans$res))
  })
  outputOptions(output, "residuals", suspendWhenHidden = F)
  
  output$rate <- reactive({
    return("Linear" %in% input$model && length(trans$kalman) > 0 && sd(trans$kalman[,2]) > .Machine$double.eps)
  })
  outputOptions(output, "rate", suspendWhenHidden = F)
  
  # Series summary ####
  output$information <- renderUI({
    line1 <- sprintf("Number of points = %d",info$points)
    line2 <- sprintf("Series length = %.1f time units",info$rangex)
    line3 <- sprintf("Series range = %f - %f",trans$x[1],trans$x[length(trans$x)])
    line4 <- sprintf("Series sampling = %f time units",info$sampling)
    line5 <- sprintf("Series completeness = %.1f %%",100*(info$points - 1)/(info$rangex/info$sampling))
    HTML(paste(line1, line2, line3, line4, line5, sep = "<br/>"))
  })
  output$informationNone <- renderUI({
    line1 <- "Number of points = 0"
    line2 <- "Series length = 0 time units"
    line3 <- "Series range = ? - ?"
    line4 <- "Series sampling = 0 time units"
    line5 <- "Series completeness = 0 %"
    HTML(paste(line1, line2, line3, line4, line5, sep = "<br/>"))
  })
  
  # Debouncers for reactive inputs ####
  reactive({
    inputs$ObsError <- suppressWarnings(as.numeric(trimws(input$ObsError, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)
  
  reactive({
    inputs$thresholdRes <- suppressWarnings(as.numeric(trimws(input$thresholdRes, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$thresholdResN <- suppressWarnings(as.numeric(trimws(input$thresholdResN, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$trendRef <- suppressWarnings(as.numeric(trimws(input$trendRef, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$period <- trimws(input$period, which = "both", whitespace = "[ \t\r\n]")
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$periodRef <- suppressWarnings(as.numeric(trimws(input$periodRef, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$offsetEpoch <- trimws(input$offsetEpoch, which = "both", whitespace = "[ \t\r\n]")
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$ExponenRef <- trimws(input$ExponenRef, which = "both", whitespace = "[ \t\r\n]")
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$E0 <- trimws(input$E0, which = "both", whitespace = "[ \t\r\n]")
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$eE0 <- trimws(input$eE0, which = "both", whitespace = "[ \t\r\n]")
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$TE0 <- trimws(input$TE0, which = "both", whitespace = "[ \t\r\n]")
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$eTE0 <- trimws(input$eTE0, which = "both", whitespace = "[ \t\r\n]")
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$LogariRef <- trimws(input$LogariRef, which = "both", whitespace = "[ \t\r\n]")
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$L0 <- trimws(input$L0, which = "both", whitespace = "[ \t\r\n]")
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$eL0 <- trimws(input$eL0, which = "both", whitespace = "[ \t\r\n]")
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$TL0 <- trimws(input$TL0, which = "both", whitespace = "[ \t\r\n]")
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$eTL0 <- trimws(input$eTL0, which = "both", whitespace = "[ \t\r\n]")
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$PolyRef <- suppressWarnings(as.numeric(trimws(input$PolyRef, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$PolyCoef <- suppressWarnings(as.numeric((trimws(input$PolyCoef, which = "both", whitespace = "[ \t\r\n]"))))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$ofac <- suppressWarnings(as.numeric(trimws(input$ofac, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$long_period <- suppressWarnings(as.numeric(trimws(input$long_period, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$short_period <- suppressWarnings(as.numeric(trimws(input$short_period, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$low <- suppressWarnings(as.numeric(trimws(input$low, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$high <- suppressWarnings(as.numeric(trimws(input$high, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$step <- suppressWarnings(as.numeric(trimws(input$step, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$min_wavelet <- suppressWarnings(as.numeric(trimws(input$min_wavelet, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$max_wavelet <- suppressWarnings(as.numeric(trimws(input$max_wavelet, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$res_wavelet <- suppressWarnings(as.numeric(trimws(input$res_wavelet, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$loc_wavelet <- suppressWarnings(as.numeric(trimws(input$loc_wavelet, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$verif_white <- suppressWarnings(as.numeric(trimws(input$verif_white, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$verif_pl <- suppressWarnings(as.numeric(trimws(input$verif_pl, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$verif_k <- suppressWarnings(as.numeric(trimws(input$verif_k, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$max_white <- suppressWarnings(as.numeric(trimws(input$max_white, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$min_white <- suppressWarnings(as.numeric(trimws(input$min_white, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$min_fl <- suppressWarnings(as.numeric(trimws(input$min_fl, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$min_pl <- suppressWarnings(as.numeric(trimws(input$min_pl, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$max_fl <- suppressWarnings(as.numeric(trimws(input$max_fl, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$max_pl <- suppressWarnings(as.numeric(trimws(input$max_pl, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$min_rw <-  suppressWarnings(as.numeric(trimws(input$min_rw, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$min_k <-  suppressWarnings(as.numeric(trimws(input$min_k, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$max_rw <-  suppressWarnings(as.numeric(trimws(input$max_rw, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$max_k <-  suppressWarnings(as.numeric(trimws(input$max_k, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$waveformPeriod <-  suppressWarnings(as.numeric(trimws(input$waveformPeriod, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$min_optirange <-  suppressWarnings(as.numeric(trimws(input$min_optirange, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$max_optirange <-  suppressWarnings(as.numeric(trimws(input$max_optirange, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$epoch <-  suppressWarnings(as.numeric(trimws(input$epoch, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$variable <-  suppressWarnings(as.numeric(trimws(input$variable, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$errorBar <-  suppressWarnings(as.numeric(trimws(input$errorBar, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$ids <-  trimws(input$ids, which = "both", whitespace = "[ \t\r\n]")
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$epoch2 <-  suppressWarnings(as.numeric(trimws(input$epoch2, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$variable2 <-  suppressWarnings(as.numeric(trimws(input$variable2, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$errorBar2 <-  suppressWarnings(as.numeric(trimws(input$errorBar2, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$station_x <- suppressWarnings(as.numeric(trimws(input$station_x, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$station_y <- suppressWarnings(as.numeric(trimws(input$station_y, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$station_z <- suppressWarnings(as.numeric(trimws(input$station_z, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$station_lat <- suppressWarnings(as.numeric(trimws(input$station_lat, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$station_lon <- suppressWarnings(as.numeric(trimws(input$station_lon, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$pole_x <- suppressWarnings(as.numeric(trimws(input$pole_x, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$pole_y <- suppressWarnings(as.numeric(trimws(input$pole_y, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$pole_z <- suppressWarnings(as.numeric(trimws(input$pole_z, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$pole_lat <- suppressWarnings(as.numeric(trimws(input$pole_lat, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$pole_lon <- suppressWarnings(as.numeric(trimws(input$pole_lon, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)

  reactive({
    inputs$pole_rot <- suppressWarnings(as.numeric(trimws(input$pole_rot, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(2000, priority = 1001)
  
  # Update data ####
  observeEvent(c(input$plot, input$sigmas, input$series2, input$tab, values$used1, values$used2,
                 values$used3, input$delete_excluded, input$format, input$tunits, input$optionSecondary,
                 inputs$step, inputs$epoch, inputs$variable, inputs$errorBar, input$separator, 
                 inputs$epoch2, inputs$variable2, inputs$errorBar2, input$separator2, input$format2, 
                 input$eulerType, input$neuenu), {
    req(obs(), values$used1)
    if (messages > 0) cat(file = stderr(), "Updating dataset", "\n")
    data <- obs()
    
    # data$y    = all points from input series
    # trans$y0  = all points from input series
    # trans$y   = points used
    # trans$sy  = sigmas used
    # trans$ye  = points excluded
    # trans$sye = sigmas excluded
    # trans$z   = points from secondary series (independent)
    # trans$sz  = sigmas from secondary series (independent)
    
    trans$x0 <- data$x
    if (isTruthy(input$remove3D)) {
      if ((input$tab == 1) || (input$format == 4)) {
        trans$y0 <- data$y1
        trans$sy0 <- data$sy1
        trans$y <- data$y1[!is.na(trans$y0)]
        trans$ye <- trans$y[values$excluded_all]
        trans$y <- trans$y[values$used_all]
        trans$sy <- data$sy1[!is.na(trans$y0)]
        trans$sye <- trans$sy[values$excluded_all]
        trans$sy <- trans$sy[values$used_all]
        trans$z <- data$z1
        trans$sz <- data$sz1
      } else if (input$tab == 2) {
        trans$y0 <- data$y2
        trans$sy0 <- data$sy2
        trans$y <- data$y2[!is.na(trans$y0)]
        trans$ye <- trans$y[values$excluded_all]
        trans$y <- trans$y[values$used_all]
        trans$sy <- data$sy2[!is.na(trans$y0)]
        trans$sye <- trans$sy[values$excluded_all]
        trans$sy <- trans$sy[values$used_all]
        trans$z <- data$z2
        trans$sz <- data$sz2
      } else if (input$tab == 3) {
        trans$y0 <- data$y3
        trans$sy0 <- data$sy3
        trans$y <- data$y3[!is.na(trans$y0)]
        trans$ye <- trans$y[values$excluded_all]
        trans$y <- trans$y[values$used_all]
        trans$sy <- data$sy3[!is.na(trans$y0)]
        trans$sye <- trans$sy[values$excluded_all]
        trans$sy <- trans$sy[values$used_all]
        trans$z <- data$z3
        trans$sz <- data$sz3
      }
      trans$x <- data$x[!is.na(trans$y0)]
      trans$xe <- trans$x[values$excluded_all]
      trans$x <- trans$x[values$used_all]
      if (input$fitType == 2 && length(trans$mod) > 0 && length(trans$res) > 0) {
        if (sum(values$used_all) < length(trans$mod0)) {
          trans$mod <- trans$mod0[values$used_all_kf]
          trans$res <- trans$res0[values$used_all_kf]
          trans$kalman <- trans$kalman0[values$used_all_kf]
          trans$kalman_unc <- trans$kalman_unc0[values$used_all_kf]
          if (length(trans$mod) != length(trans$mod0)) {
            showNotification("The KF fit is no longer valid. Consider running it again.", action = NULL, duration = 10, closeButton = T, id = "kf_not_valid", type = "warning", session = getDefaultReactiveDomain())
          }
        } else if (sum(values$used_all) == length(trans$mod0)) {
          trans$mod <- trans$mod0[values$used_all]
          trans$res <- trans$res0[values$used_all]
          trans$kalman <- trans$kalman0[values$used_all]
          trans$kalman_unc <- trans$kalman_unc0[values$used_all]
        } else {
          info$run <- F
          trans$mod <- trans$mod0 <- NULL
          trans$res <- trans$res0 <- NULL
          trans$kalman <- trans$kalman0 <- NULL
          trans$kalman_unc <- trans$kalman_unc0 <- NULL
        }
      }
    } else {
      if ((input$tab == 1) || (input$format == 4)) {
        trans$y0 <- data$y1
        trans$sy0 <- data$sy1
        trans$x <- data$x[!is.na(trans$y0)]
        trans$xe <- trans$x[values$excluded1]
        trans$x <- trans$x[values$used1]
        trans$y <- data$y1[!is.na(trans$y0)]
        trans$ye <- trans$y[values$excluded1]
        trans$y <- trans$y[values$used1]
        trans$sy <- data$sy1[!is.na(trans$y0)]
        trans$sye <- trans$sy[values$excluded1]
        trans$sy <- trans$sy[values$used1]
        trans$z <- data$z1
        trans$sz <- data$sz1
        if (input$fitType == 2 && length(trans$mod) > 0 && length(trans$res) > 0) {
          if (sum(values$used1) < length(trans$mod0)) {
            trans$mod <- trans$mod0[values$used_kf1]
            trans$res <- trans$res0[values$used_kf1]
            trans$kalman <- trans$kalman0[values$used_kf1]
            trans$kalman_unc <- trans$kalman_unc0[values$used_kf1]
            if (length(trans$mod) != length(trans$mod0)) {
              showNotification("The KF fit is no longer valid. Consider running it again.", action = NULL, duration = 10, closeButton = T, id = "kf_not_valid", type = "warning", session = getDefaultReactiveDomain())
            }
          } else if (sum(values$used1) == length(trans$mod0)) {
            trans$mod <- trans$mod0[values$used1]
            trans$res <- trans$res0[values$used1]
            trans$kalman <- trans$kalman0[values$used1]
            trans$kalman_unc <- trans$kalman_unc0[values$used1]
          } else {
            info$run <- F
            trans$mod <- trans$mod0 <- NULL
            trans$res <- trans$res0 <- NULL
            trans$kalman <- trans$kalman0 <- NULL
            trans$kalman_unc <- trans$kalman_unc0 <- NULL
          }
        }
      } else if (input$tab == 2) {
        trans$y0 <- data$y2
        trans$sy0 <- data$sy2
        trans$x <- data$x[!is.na(trans$y0)]
        trans$xe <- trans$x[values$excluded2]
        trans$x <- trans$x[values$used2]
        trans$y <- data$y2[!is.na(trans$y0)]
        trans$ye <- trans$y[values$excluded2]
        trans$y <- trans$y[values$used2]
        trans$sy <- data$sy2[!is.na(trans$y0)]
        trans$sye <- trans$sy[values$excluded2]
        trans$sy <- trans$sy[values$used2]
        trans$z <- data$z2
        trans$sz <- data$sz2
        if (input$fitType == 2 && length(trans$mod) > 0 && length(trans$res) > 0) {
          if (sum(values$used2) < length(trans$mod0)) {
            trans$mod <- trans$mod0[values$used_kf2]
            trans$res <- trans$res0[values$used_kf2]
            trans$kalman <- trans$kalman0[values$used_kf2]
            trans$kalman_unc <- trans$kalman_unc0[values$used_kf2]
            if (length(trans$mod) != length(trans$mod0)) {
              showNotification("The KF fit is no longer valid. Consider running it again.", action = NULL, duration = 10, closeButton = T, id = "kf_not_valid", type = "warning", session = getDefaultReactiveDomain())
            }
          } else if (sum(values$used2) == length(trans$mod0)) {
            trans$mod <- trans$mod0[values$used2]
            trans$res <- trans$res0[values$used2]
            trans$kalman <- trans$kalman0[values$used2]
            trans$kalman_unc <- trans$kalman_unc0[values$used2]
          } else {
            info$run <- F
            trans$mod <- trans$mod0 <- NULL
            trans$res <- trans$res0 <- NULL
            trans$kalman <- trans$kalman0 <- NULL
            trans$kalman_unc <- trans$kalman_unc0 <- NULL
          }
        }
      } else if (input$tab == 3) {
        trans$y0 <- data$y3
        trans$sy0 <- data$sy3
        trans$x <- data$x[!is.na(trans$y0)]
        trans$xe <- trans$x[values$excluded3]
        trans$x <- trans$x[values$used3]
        trans$y <- data$y3[!is.na(trans$y0)]
        trans$ye <- trans$y[values$excluded3]
        trans$y <- trans$y[values$used3]
        trans$sy <- data$sy3[!is.na(trans$y0)]
        trans$sye <- trans$sy[values$excluded3]
        trans$sy <- trans$sy[values$used3]
        trans$z <- data$z3
        trans$sz <- data$sz3
        if (input$fitType == 2 && length(trans$mod) > 0 && length(trans$res) > 0) {
          if (sum(values$used3) < length(trans$mod0)) {
            trans$mod <- trans$mod0[values$used_kf3]
            trans$res <- trans$res0[values$used_kf3]
            trans$kalman <- trans$kalman0[values$used_kf3]
            trans$kalman_unc <- trans$kalman_unc0[values$used_kf3]
            if (length(trans$mod) != length(trans$mod0)) {
              showNotification("The KF fit is no longer valid. Consider running it again.", action = NULL, duration = 10, closeButton = T, id = "kf_not_valid", type = "warning", session = getDefaultReactiveDomain())
            }
          } else if (sum(values$used3) == length(trans$mod0)) {
            trans$mod <- trans$mod0[values$used3]
            trans$res <- trans$res0[values$used3]
            trans$kalman <- trans$kalman0[values$used3]
            trans$kalman_unc <- trans$kalman_unc0[values$used3]
          } else {
            info$run <- F
            trans$mod <- trans$mod0 <- NULL
            trans$res <- trans$res0 <- NULL
            trans$kalman <- trans$kalman0 <- NULL
            trans$kalman_unc <- trans$kalman_unc0 <- NULL
          }
        }
      }
    }
    if (!isTruthy(input$sigmas)) {
      trans$sy <- rep(1, length(trans$sy))
      trans$sy0 <- rep(1, length(trans$sy0))
      trans$sye <- rep(1, length(trans$sye))
      trans$sz <- rep(1, length(trans$sz))
    }
    info$miny <- min(trans$y0, na.rm = T)
    info$maxy <- max(trans$y0, na.rm = T)
    ids <- trans$x0 >= ranges$x1[1] & trans$x0 <= ranges$x1[2]
    if (sum(ids) > 0) {
      ranges$y1 <- range(trans$y0[ids], na.rm = T)
      if (any(is.na(ranges$y1)) || any(is.infinite(ranges$y1))) {
        ranges$y1 <- c(info$miny, info$maxy)
      }
    } else {
      ranges$y1 <- c(info$miny, info$maxy)
    }
    if (length(file$secondary) > 1 && input$optionSecondary == 1 && any(!is.na(trans$z))) {
      ids <- trans$x0 >= ranges$x1[1] & trans$x0 <= ranges$x1[2]
      if (sum(ids) > 0) {
        ranges$y12 <- range(trans$z[ids], na.rm = T)
        if (any(is.na(ranges$y12)) || any(is.infinite(ranges$y12))) {
          ranges$y12 <- range(trans$z, na.rm = T)
        }
      } else {
        ranges$y12 <- range(trans$z, na.rm = T)
      }
    }
    info$decimalsx <- max(decimalplaces(trans$x))
    if (!isTruthy(info$decimalsx)) {
      info$decimalsx <- 4
    }
    if (info$decimalsx > 10) {
      info$decimalsx <- 10
    }
    info$decimalsy <- max(decimalplaces(trans$y))
    if (!isTruthy(info$decimalsy)) {
      info$decimalsy <- 4
    }
    if (info$decimalsy > 10) {
      info$decimalsy <- 10
    }
    info$points <- length(trans$x)
    info$sampling <- min(diff(trans$x,1))
    info$regular <- (median(diff(trans$x))/info$sampling < 1.25)
    info$rangex <- trans$x[length(trans$x)] - trans$x[1]
    if (input$tunits == 1) {
      info$tunits <- "days"
    } else if (input$tunits == 2) {
      info$tunits <- "weeks"
    } else if (input$tunits == 3) {
      info$tunits <- "years"
    }
    trans$ordinate <- median(trans$y)
    trans$tol <- min(diff(isolate(trans$x),1)) / 1.5
    info$noise <- (sd(head(trans$y, 30)) + sd(tail(trans$y, 30)))/2
    updateRadioButtons(session, inputId = "waveletType", label = NULL, 
                       choices = list("None" = 0, "Original" = 1, "Model" = 2, "Model res." = 3, "Filter" = 4, "Filter res." = 5), 
                       selected = 0, inline = T, choiceNames = NULL,  choiceValues = NULL)
    updateTextInput(session, "min_wavelet", value = "")
    updateTextInput(session, "max_wavelet", value = "")
    updateTextInput(session, "res_wavelet", value = "")
  }, priority = 3)
  
  # Load SARI file ####
  observeEvent(input$loadSARI, {
    req(file$primary, obs())
    comments <- grep("^#", readLines(con = input$loadSARI$datapath, n = 100, ok = T, warn = F, skipNul = T), ignore.case = F, perl = T, value = T, fixed = F, useBytes = F, invert = F)
    if (isTruthy(comments) && grepl("^# SARI ", comments[1], ignore.case = F, perl = T)) {
      if (messages > 0) cat(file = stderr(), "Loading SARI file", "\n")
      if (!grepl(version, comments[1], ignore.case = F, perl = T)) {
        showNotification("Warning: the SARI version used in the uploaded file is not the same as the current version", action = NULL, duration = 10, closeButton = T, id = "sari_version", type = "warning", session = getDefaultReactiveDomain())
      }
      if (sum(grepl("^# Model", comments, ignore.case = F, perl = T)) > 1) {
        showNotification("The format of the uploaded file is not compatible.", action = NULL, duration = 10, closeButton = T, id = "format_not_compatible", type = "error", session = getDefaultReactiveDomain())
      } else if (sum(grepl("^# Model", comments, ignore.case = F, perl = T)) < 1) {
        showNotification("No model found in the uploaded file.", action = NULL, duration = 10, closeButton = T, id = "no_model", type = "error", session = getDefaultReactiveDomain())
      } else {
        if (sum(grepl("^# Model .*KF", comments, ignore.case = F, perl = T)) > 0) {
          #This is a KF fit
          model <- grep("^# Model .*KF", comments, ignore.case = F, perl = T, value = T)
          if (nchar(model) > 18) {
            text <- strsplit(model, ")\\*|-|)|>|\\^")[[1]]
            aprioris <- grep("^# A priori: ", comments, ignore.case = F, perl = T, value = T)
            process_noises <- grep("^# Process noise: ", comments, ignore.case = F, perl = T, value = T)
            measurement_noise <- strsplit(grep("^# Measurement noise: ", comments, ignore.case = F, perl = T, value = T), ":")[[1]][2]
            components <- c()
            # Extracting Intercept info
            if (grepl("Intercept", model, ignore.case = F, perl = T)) {
              index <- grep(" Intercept", aprioris, ignore.case = F, perl = F, value = F, fixed = T, useBytes = F, invert = F)
              values <- strsplit(aprioris[index], "=|\\+\\/-")[[1]]
              updateTextInput(inputId = "Intercept0", value = values[2])
              updateTextInput(inputId = "eIntercept0", value = values[3])
            }
            # Extracting Rate info
            if (grepl(" \\+ Rate", model, ignore.case = F, perl = T)) {
              updateTextInput(session, "trendRef", value = text[2])
              index <- grep(" Rate", aprioris, ignore.case = F, perl = F, value = F, fixed = T, useBytes = F, invert = F)
              values <- strsplit(aprioris[index], "=|\\+\\/-")[[1]]
              updateTextInput(inputId = "Trend0", value = values[2])
              updateTextInput(inputId = "eTrend0", value = values[3])
              index <- grep(" Rate", process_noises, ignore.case = F, perl = F, value = F, fixed = T, useBytes = F, invert = F)
              values <- strsplit(process_noises[index], "=")[[1]]
              if (isTruthy(values)) {
                updateTextInput(inputId = "TrendDev", value = values[2])
              } else {
                updateTextInput(inputId = "TrendDev", value = "0.0")
              }
              components <- c(components, "Linear")
            }
            # Extracting Polynomial info
            if (grepl(" \\+ P", model, ignore.case = F, perl = T)) {
              index <- grep(" + P", text, ignore.case = F, perl = F, value = F, fixed = T, useBytes = F, invert = F)
              updateTextInput(session, "PolyRef", value = text[index[1] + 1])
              updateTextInput(session, "PolyCoef", value = text[index[length(index)] + 3])
              index <- grep(" A priori: P", aprioris, ignore.case = F, perl = F, value = F, fixed = T, useBytes = F, invert = F)
              values <- strsplit(aprioris[index], "=|\\+\\/-")
              updateTextInput(inputId = "P0", value = paste(sapply(values, "[[", 2), collapse = ", "))
              updateTextInput(inputId = "eP0", value = paste(sapply(values, "[[", 3), collapse = ", "))
              components <- c(components, "Polynomial")
            }
            # Extracting Sinusoidal info
            if (grepl(" \\+ S", model, ignore.case = F, perl = T)) {
              index <- grep(" + S", text, ignore.case = F, perl = F, value = F, fixed = T, useBytes = F, invert = F)
              updateTextInput(session, "periodRef", value = unique(text[index + 1]))
              if (input$tunits == 1) {
                units <- "d"
              } else if (input$tunits == 2) {
                units <- "w"
              } else if (input$tunits == 3) {
                units <- "y"
              }
              updateTextInput(session, "period", value = paste(paste0(1/as.numeric(text[index + 2]), units), collapse = ","))
              index <- grep(" A priori: S", aprioris, ignore.case = F, perl = F, value = F, fixed = T, useBytes = F, invert = F)
              values <- strsplit(aprioris[index], "=|\\+\\/-")
              updateTextInput(inputId = "S0", value = paste(sapply(values, "[[", 2), collapse = ", "))
              updateTextInput(inputId = "eS0", value = paste(sapply(values, "[[", 3), collapse = ", "))
              index <- grep(" Process noise: S", process_noises, ignore.case = F, perl = F, value = F, fixed = T, useBytes = F, invert = F)
              values <- strsplit(process_noises[index], "=")
              if (isTruthy(values)) {
                updateTextInput(inputId = "SinusoidalDev", value = paste(sapply(values, "[[", 2), collapse = ", "))
              } else {
                updateTextInput(inputId = "SinusoidalDev", value = paste(rep("0.0", length(index)), collapse = ","))
              }
              components <- c(components, "Sinusoidal")
            }
            # Extracting Offset info
            if (grepl(" \\+ O", model, ignore.case = F, perl = T)) {
              index <- grep("A priori: O", aprioris, ignore.case = F, perl = F, value = F, fixed = T, useBytes = F, invert = F)
              values <- strsplit(aprioris[index], "=|\\+\\/-")
              updateTextInput(inputId = "O0", value = paste(sapply(values, "[[", 2), collapse = ", "))
              updateTextInput(inputId = "eO0", value = paste(sapply(values, "[[", 3), collapse = ", "))
              index <- grep(" + O", text, ignore.case = F, perl = F, value = F, fixed = T, useBytes = F, invert = F)
              updateTextInput(session, "offsetEpoch", value = paste(text[index + 1], collapse = ", "))
              components <- c(components, "Offset")
            }
            # Extracting Exponential info
            if (grepl(" \\+ E", model, ignore.case = F, perl = T)) {
              index <- grep(" + E", text, ignore.case = F, perl = F, value = F, fixed = T, useBytes = F, invert = F)
              updateTextInput(session, "ExponenRef", value = paste(text[index + 1], collapse = ","))
              index <- grep(" A priori: E", aprioris, ignore.case = F, perl = F, value = F, fixed = T, useBytes = F, invert = F)
              values <- strsplit(aprioris[index], "=|\\+\\/-")
              updateTextInput(session, "E0", value = paste(sapply(values, "[[", 2), collapse = ", "))
              updateTextInput(session, "eE0", value = paste(sapply(values, "[[", 3), collapse = ", "))
              index <- grep(" A priori: TauE", aprioris, ignore.case = F, perl = F, value = F, fixed = T, useBytes = F, invert = F)
              values <- strsplit(aprioris[index], "=|\\+\\/-")
              updateTextInput(session, "TE0", value = paste(sapply(values, "[[", 2), collapse = ", "))
              updateTextInput(session, "eTE0", value = paste(sapply(values, "[[", 3), collapse = ", "))
              components <- c(components, "Exponential")
            }
            # Extracting Logarithmic info
            if (grepl(" \\+ L", model, ignore.case = F, perl = T)) {
              index <- grep(" + L", text, ignore.case = F, perl = F, value = F, fixed = T, useBytes = F, invert = F)
              updateTextInput(session, "LogariRef", value = paste(text[index + 1], collapse = ","))
              index <- grep(" A priori: L", aprioris, ignore.case = F, perl = F, value = F, fixed = T, useBytes = F, invert = F)
              values <- strsplit(aprioris[index], "=|\\+\\/-")
              updateTextInput(session, "L0", value = paste(sapply(values, "[[", 2), collapse = ", "))
              updateTextInput(session, "eL0", value = paste(sapply(values, "[[", 3), collapse = ", "))
              index <- grep(" A priori: TauL", aprioris, ignore.case = F, perl = F, value = F, fixed = T, useBytes = F, invert = F)
              values <- strsplit(aprioris[index], "=|\\+\\/-")
              updateTextInput(session, "TL0", value = paste(sapply(values, "[[", 2), collapse = ", "))
              updateTextInput(session, "eTL0", value = paste(sapply(values, "[[", 3), collapse = ", "))
              components <- c(components, "Logarithmic")
            }
            updateCheckboxGroupInput(session, inputId = "model", label = "", choices = list("Linear","Polynomial","Sinusoidal","Offset","Exponential","Logarithmic"), selected = components, inline = T)
            updateRadioButtons(session, inputId = "fitType", label = NULL, list("None" = 0, "LS" = 1, "KF" = 2), selected = 2, inline = T, choiceNames = NULL, choiceValues = NULL)
            if (grepl("^# Model EKF", model, ignore.case = F, perl = T)) {
              updateRadioButtons(session, inputId = "kf", label = NULL, choices = list("EKF" = 1, "UKF" = 2), selected = 1, inline = T)
            } else if (grepl("^# Model UKF", model, ignore.case = F, perl = T)) {
              updateRadioButtons(session, inputId = "kf", label = NULL, choices = list("EKF" = 1, "UKF" = 2), selected = 2, inline = T)
            }
            updateTextInput(session, inputId = "ObsError", value = measurement_noise)
            info$menu <- unique(c(info$menu, 4))
            updateCollapse(session, id = "menu", open = info$menu)
          }
        } else {
          #This is a LS fit
          model <- grep("^# Model LS:", comments, ignore.case = F, perl = T, value = T)
          if (nchar(model) > 18) {
            text <- strsplit(model, ")\\*|-|)|>|\\^")[[1]]
            parameters <- grep("^# Parameter: ", comments, ignore.case = F, perl = T, value = T)
            components <- c()
            # Extracting Rate info
            if (grepl(" \\+ Rate", model, ignore.case = F, perl = T)) {
              updateTextInput(session, "trendRef", value = text[2])
              components <- c(components, "Linear")
            }
            # Extracting Polynomial info
            if (grepl(" \\+ P", model, ignore.case = F, perl = T)) {
              index <- grep(" + P", text, ignore.case = F, perl = F, value = F, fixed = T, useBytes = F, invert = F)
              updateTextInput(session, "PolyRef", value = text[index[1] + 1])
              updateTextInput(session, "PolyCoef", value = text[index[length(index)] + 3])
              components <- c(components, "Polynomial")
            }
            # Extracting Sinusoidal info
            if (grepl(" \\+ S", model, ignore.case = F, perl = T)) {
              index <- grep(" + S", text, ignore.case = F, perl = F, value = F, fixed = T, useBytes = F, invert = F)
              updateTextInput(session, "periodRef", value = unique(text[index + 1]))
              if (input$tunits == 1) {
                units <- "d"
              } else if (input$tunits == 2) {
                units <- "w"
              } else if (input$tunits == 3) {
                units <- "y"
              }
              updateTextInput(session, "period", value = paste(paste0(1/as.numeric(text[index + 2]), units), collapse = ","))
              components <- c(components, "Sinusoidal")
            }
            # Extracting Offset info
            if (grepl(" \\+ O", model, ignore.case = F, perl = T)) {
              index <- grep(" + O", text, ignore.case = F, perl = F, value = F, fixed = T, useBytes = F, invert = F)
              updateTextInput(session, "offsetEpoch", value = paste(text[index + 1], collapse = ", "))
              components <- c(components, "Offset")
            }
            # Extracting Exponential info
            if (grepl(" \\+ E", model, ignore.case = F, perl = T)) {
              index <- grep(" + E", text, ignore.case = F, perl = F, value = F, fixed = T, useBytes = F, invert = F)
              updateTextInput(session, "ExponenRef", value = paste(text[index + 1], collapse = ","))
              index <- grep(" Parameter: E", parameters, ignore.case = F, perl = F, value = F, fixed = T, useBytes = F, invert = F)
              values <- strsplit(parameters[index], "=|\\+\\/-")
              updateTextInput(session, "E0", value = paste(sapply(values, "[[", 2), collapse = ", "))
              index <- grep(" Parameter: TauE", parameters, ignore.case = F, perl = F, value = F, fixed = T, useBytes = F, invert = F)
              values <- strsplit(parameters[index], "=|\\+\\/-")
              updateTextInput(session, "TE0", value = paste(sapply(values, "[[", 2), collapse = ", "))
              components <- c(components, "Exponential")
            }
            # Extracting Logarithmic info
            if (grepl(" \\+ L", model, ignore.case = F, perl = T)) {
              index <- grep(" + L", text, ignore.case = F, perl = F, value = F, fixed = T, useBytes = F, invert = F)
              updateTextInput(session, "LogariRef", value = paste(text[index + 1], collapse = ","))
              index <- grep(" Parameter: L", parameters, ignore.case = F, perl = F, value = F, fixed = T, useBytes = F, invert = F)
              values <- strsplit(parameters[index], "=|\\+\\/-")
              updateTextInput(session, "L0", value = paste(sapply(values, "[[", 2), collapse = ", "))
              index <- grep(" Parameter: TauL", parameters, ignore.case = F, perl = F, value = F, fixed = T, useBytes = F, invert = F)
              values <- strsplit(parameters[index], "=|\\+\\/-")
              updateTextInput(session, "TL0", value = paste(sapply(values, "[[", 2), collapse = ", "))
              components <- c(components, "Logarithmic")
            }
            updateCheckboxGroupInput(session, inputId = "model", label = "", choices = list("Linear","Polynomial","Sinusoidal","Offset","Exponential","Logarithmic"), selected = components, inline = T)
            updateRadioButtons(session, inputId = "fitType", label = NULL, list("None" = 0, "LS" = 1, "KF" = 2), selected = 1, inline = T, choiceNames = NULL, choiceValues = NULL)
            info$menu <- unique(c(info$menu, 4))
            updateCollapse(session, id = "menu", open = info$menu)
          }
        }
      }
    } else {
      showNotification("Unable to find the SARI version in the uploaded file. Is this a SARI file?", action = NULL, duration = 10, closeButton = T, id = "no_sari", type = "warning", session = getDefaultReactiveDomain())
    }
  })
  
  # Plot series ####
  output$plot1 <- output$plot2 <- output$plot3 <- renderPlot({
    req(obs(), trans$x, trans$y, trans$sy)
    if (messages > 0) cat(file = stderr(), "Plotting the series", "\n")
    title <- ""
    sigmas <- F
    if (isTruthy(input$sigmas) && ((input$format == 4 && isTruthy(inputs$errorBar)) || input$format != 4)) {
      sigmas <- T
    }
    if (length(isolate(file$secondary)) > 1 && input$optionSecondary == 1 && any(!is.na(trans$z))) {
      if (input$symbol == 0) {
        symbol <- 'p'
      } else if (input$symbol == 1) {
        symbol <- 'l'
      } else if (input$symbol == 2) {
        symbol <- 'o'
      }
      if (isTruthy(input$sameScale)) {
        half <- abs(ranges$y1[1] - (ranges$y1[1] + ranges$y1[2]) / 2)
        if (isTruthy(ranges$y12)[1]) {
          middle <- (ranges$y12[1] + ranges$y12[2]) / 2
        } else {
          middle <- median(trans$z, na.rm = T)
        }
        ranges$y12 <- c(middle - half, middle + half)
      } else if (isTruthy(input$same_axis)) {
        ranges$y12 <- ranges$y1
      } else {
        ranges$y12 <- NULL
      }
      plot(trans$x0[!is.na(trans$z)], trans$z[!is.na(trans$z)], type = symbol, pch = 20, col = "#59b300", axes = F, xlab = NA, ylab = NA, xlim = ranges$x1, ylim = ranges$y12)
      if (isTruthy(sigmas)) {
        color <- "#59b300"
        alfa <- 0.2
        shade <- adjustcolor(color, alpha.f = alfa)
        ba <- trans$z[!is.na(trans$z)] + trans$sz[!is.na(trans$z)]
        bb <- trans$z[!is.na(trans$z)] - trans$sz[!is.na(trans$z)]
        polygon(c(trans$x0[!is.na(trans$z)], rev(trans$x0[!is.na(trans$z)])), c(ba, rev(bb)), col = shade, border = NA)
      }
      axis(side = 4, at = NULL, labels = T, tick = T, line = NA, pos = NA, outer = F)
      par(new = T)
      plot_series(trans$x0[!is.na(trans$y0)],trans$y0[!is.na(trans$y0)],trans$sy0[!is.na(trans$y0)],ranges$x1,ranges$y1,sigmas,title,input$symbol)
    } else {
      plot_series(trans$x0,trans$y0,trans$sy0,ranges$x1,ranges$y1,sigmas,title,input$symbol)
    }
    points(isolate(trans$xe), isolate(trans$ye), type = "p", col = 'red', bg = 'red', pch = 21)
    if (input$eulerType == 1 && length(trans$plate[!is.na(trans$plate)]) == 3) {
      centerx <- which(abs(trans$x - median(trans$x)) == min(abs(trans$x - median(trans$x))))[1]
      centery <- which(abs(trans$y - median(trans$y)) == min(abs(trans$y - median(trans$y))))[1]
      if (input$tab == 1) {
        lines(c(trans$x[1],trans$x[length(trans$x)]),c(trans$y[centery] + trans$plate[1]*(trans$x[1] - trans$x[centerx]),trans$y[centery] + trans$plate[1]*(trans$x[length(trans$x)] - trans$x[centerx])), col = "green", lwd = 3)
      } else if (input$tab == 2) {
        lines(c(trans$x[1],trans$x[length(trans$x)]),c(trans$y[centery] + trans$plate[2]*(trans$x[1] - trans$x[centerx]),trans$y[centery] + trans$plate[2]*(trans$x[length(trans$x)] - trans$x[centerx])), col = "green", lwd = 3)
      } else if (input$tab == 3) {
        lines(c(trans$x[1],trans$x[length(trans$x)]),c(trans$y[centery] + trans$plate[3]*(trans$x[1] - trans$x[centerx]),trans$y[centery] + trans$plate[3]*(trans$x[length(trans$x)] - trans$x[centerx])), col = "green", lwd = 3)
      }
    }
    if (input$traceLog && length(info$log) > 0) {
      for (r in info$log[[2]]) {
        abline(v = r, col = "blue", lty = 2)
      }
      for (a in info$log[[1]]) {
        abline(v = a, col = "blue")
      }
    }
    if (input$traceSinfo && length(info$sinfo) > 0) {
      for (r in info$sinfo[[2]]) {
        abline(v = r, col = "blue", lty = 2)
      }
      for (a in info$sinfo[[1]]) {
        abline(v = a, col = "blue")
      }
    }
    if (input$traceSoln && length(info$soln) > 0) {
      for (a in info$soln) {
        abline(v = a, col = "orange")
      }
    }
    if (input$traceCustom && length(info$custom) > 0) {
      for (a in info$custom) {
        abline(v = a, col = "green")
      }
    }
    if (length(trans$mod) > 0) {
      lines(trans$x,trans$mod,col = "red", lwd = 3)
    }
    if (length(trans$filter) > 0 && input$filter == T && input$series2filter == 1) {
      lines(trans$x,trans$filter,col = "blue", lwd = 2)
    }
    output$plot1_info <- output$plot2_info <- output$plot3_info <- renderText({
      if (length(input$plot_1click$x) > 0) {
        paste("Plot coordinates = ", input$plot_1click$x, input$plot_1click$y, sep = "\t")
      }
    })
}, width = reactive(info$width), type = "cairo-png")
  
  # MIDAS ####
  observeEvent(c(input$midas, trans$y, trans$offsetEpochs), {
    req(trans$x, trans$y, trans$tol)
    if (isTruthy(input$midas)) {
      if (input$tunits == 1) {
        period <- 365.25
      } else if (input$tunits == 2) {
        period <- 365.25/7
      } else if (input$tunits == 3) {
        period <- 1
      }
      if ((info$rangex / period) < 1) {
        showNotification("Not enough data available to compute interannual differences.", action = NULL, duration = 10, closeButton = T, id = "no_interannual", type = "error", session = getDefaultReactiveDomain())
        updateCheckboxInput(session, inputId = "midas", label = NULL, value = F)
        req(info$stop)
      }
      if (messages > 0) cat(file = stderr(), "Computing MIDAS", "\n")
      if (length(trans$x) > 6) {
        vel <- sapply(1:length(trans$x), function(x) midas_vel(m = x, t = period, disc = 0))
        vel <- c(vel[1,],vel[2,])
        vel <- vel[vel > -999999]
        if (length(vel) > 9) {
          vel_sig <- 1.4826*mad(vel, na.rm = T)
          vel_lim <- c(median(vel) + 2*vel_sig, median(vel) - 2*vel_sig)
          vel_good <- vel[vel < vel_lim[1] & vel > vel_lim[2]]
          vel_mad <- mad(vel_good, na.rm = T)
          trans$midas_vel <- median(vel_good)
          trans$midas_sig <- 1.2533*1.4826*vel_mad/sqrt(length(vel_good)/4)
          trans$midas_all <- vel_good
        } else {
          showNotification("Not enough interannual differences to compute a reliable trend.", action = NULL, duration = 10, closeButton = T, id = "no_interannual", type = "error", session = getDefaultReactiveDomain())
          updateCheckboxInput(session, inputId = "midas", label = NULL, value = F)
        }
        if (length(trans$offsetEpochs) > 0 && "Offset" %in% isolate(input$model)) {
          vel <- sapply(1:length(trans$x), function(x) midas_vel(m = x, t = period, disc = 1))
          vel <- c(vel[1,],vel[2,])
          vel <- vel[vel > -999999]
          if (length(vel) > 9) {
            vel_sig <- 1.4826*mad(vel, na.rm = T)
            vel_lim <- c(median(vel) + 2*vel_sig, median(vel) - 2*vel_sig)
            vel_good <- vel[vel < vel_lim[1] & vel > vel_lim[2]]
            vel_mad <- mad(vel_good, na.rm = T)
            trans$midas_vel2 <- median(vel_good)
            trans$midas_sig2 <- 1.2533*1.4826*vel_mad/sqrt(length(vel_good)/4)
            trans$midas_all <- vel_good
          } else {
            showNotification("Not enough interannual differences to compute a reliable trend.", action = NULL, duration = 10, closeButton = T, id = "no_interannual", type = "error", session = getDefaultReactiveDomain())
            updateCheckboxInput(session, inputId = "midas", label = NULL, value = F)
          }
        }
      } else {
        showNotification("Not enough interannual differences to compute a reliable trend.", action = NULL, duration = 10, closeButton = T, id = "no_interannual", type = "error", session = getDefaultReactiveDomain())
        updateCheckboxInput(session, inputId = "midas", label = NULL, value = F)
      } 
    } else {
      trans$midas_vel <- NULL
    }
  })
  
  # Plot MIDAS histogram ####
  output$midas_hist1 <- output$midas_hist2 <- output$midas_hist3 <- renderPlot({
    req(obs(), input$midas, trans$midas_all)
    if (messages > 0) cat(file = stderr(), "MIDAS histogram", "\n")
    values <- trans$midas_all
    hist(values, breaks = "FD", freq = F, xlab = "Selected interannual velocities", ylab = "", main = "MIDAS velocity histogram", col = "lightpink")
    dnorm(values, mean = mean(values, na.rm = T), sd = sd(values), log = F)
    xfit <- seq(min(values),max(values),length = 40) 
    yfit <- dnorm(xfit,mean = mean(values, na.rm = T),sd = sd(values))
    lines(xfit, yfit, col = "red", lwd = 2)
  }, width = reactive(info$width), type = "cairo-png")
  
  # Offset verification ####
  observeEvent(input$runVerif, {
    req(input$verif_offsets, trans$res, trans$x, trans$offsetEpochs)
    if ((nchar(inputs$verif_white) > 0 && !is.na(inputs$verif_white) && inputs$verif_white > 0) || (nchar(input$verif_pl) > 0 && nchar(input$verif_k) > 0 && !is.na(inputs$verif_pl) && !is.na(inputs$verif_k) && inputs$verif_pl > 0 && inputs$verif_k <= 0)) {
      if (messages > 0) cat(file = stderr(), "Verifying offsets", "\n")
      n <- length(trans$res)
      C <- matrix(0,n,n)
      scaling <- 1000 # making things much bigger
      withBusyIndicatorServer("runVerif", {
        if (isTruthy(inputs$verif_white)) {
          C <- C + (inputs$verif_white*scaling)^2*diag(n)
        } 
        if (isTruthy(inputs$verif_pl) && isTruthy(inputs$verif_k) && inputs$verif_k < 0) {
          if (inputs$verif_k == -1) {
            Delta <- sapply(1:n, function(i) if (i < 150) {gamma(i - 1 + 0.5)/(factorial(i - 1)*gamma(0.5))} else {((i - 1)^-0.5)/gamma(0.5)})
            Z <- matrix(0,n,n)
            for (i in seq_len(n)) {
              for (j in i:n) {
                Z[j,i] <- Delta[j - i + 1]
              }
            }
            Zscaled <- sapply(1:ncol(Z), function(t,k) if (t < 2) {Z[,t]*(trans$x[t + 1] - trans$x[t])^(-k/4)} else {Z[,t]*(trans$x[t] - trans$x[t - 1])^(-k/4)}, k = -1)
            Cfl <- Zscaled %*% t(Zscaled)
            C <- C + (as.numeric(inputs$verif_pl)*scaling)^2*Cfl
          } else if (inputs$verif_k == -2) {
            Z <- lower.tri(matrix(1, n, n), diag = T)*1
            Zscaled <- sapply(1:ncol(Z), function(t,k) if (t < 2) {Z[,t]} else {Z[,t]*(trans$x[t] - trans$x[t - 1])^(-k/4)}, k = -2)
            Zscaled[,1] <- Zscaled[,2]
            Zscaled[1,1] <- Zscaled[2,2]
            Crw <- Zscaled %*% t(Zscaled)
            C <- C + (as.numeric(inputs$verif_pl)*scaling)^2*Crw
          } else {
            k <- inputs$verif_k
            Delta <- sapply(1:n, function(i) if (i < 150) {gamma(i - 1 - (k/2))/(factorial(i - 1)*gamma(-k/2))} else {((i - 1)^((-k/2) - 1))/gamma(-k/2)})
            Z <- matrix(0,n,n)
            for (i in seq_len(n)) {
              for (j in i:n) {
                Z[j,i] <- Delta[j - i + 1]
              }
            }
            Zscaled <- sapply(1:ncol(Z), function(t,k) if (t < 2) {Z[,t]*(trans$x[t + 1] - trans$x[t])^(-k/4)} else {Z[,t]*(trans$x[t] - trans$x[t - 1])^(-k/4)}, k = k)
            Cpl <- Zscaled %*% t(Zscaled)
            C <- C + (inputs$verif_pl*scaling)^2*Cpl
          }
        }
        if (!all(C == 0)) {
          y <- (trans$res*scaling - mean(trans$res*scaling))
          mle_after <- (-0.5*(length(n)*log(2*pi) + determinant(C)$modulus[[1]] + ( crossprod(y,solve(C)) %*% y) ))[1]
          offsets <- grep(pattern = "O", rownames(trans$LScoefs), ignore.case = F, perl = F, fixed = T)
          mle_before <- c()
          line <- c()
          for (i in seq_len(length(trans$offsetEpochs))) {
            y <- (trans$res*scaling - mean(trans$res*scaling))
            if (input$fitType == 1) {
              offsets <- grep(pattern = "O", rownames(trans$LScoefs), ignore.case = F, perl = F, fixed = T)
              y <- y + trans$LScoefs[offsets[i]]*scaling*I(trans$x > trans$offsetEpochs[i])
            } else if (input$fitType == 2) {
              offsets <- grep(pattern = "O", colnames(trans$kalmanman), ignore.case = F, perl = F, fixed = T)
              y <- y + colMeans(trans$kalmanman)[offsets[i]]*scaling*I(trans$x > trans$offsetEpochs[i])
            }
            mle_before <- c(mle_before, (-0.5*(length(n)*log(2*pi) + determinant(C)$modulus[[1]] + ( crossprod(y,solve(C)) %*% y) ))[1])
            line <- c(line, paste(paste0("O",i)," log-likelihood:", sprintf("%.4f",mle_after - mle_before[i]),"(", sprintf("%.4f",mle_after),"vs",sprintf("%.4f",mle_before[i]),")", sep = " "))
          }
          if (isTruthy(mle_after) && isTruthy(mle_before)) {
            trans$verif <- T
          } else {
            trans$verif <- F
          }
          output$verif <- renderUI({
            if (isTruthy(trans$verif)) {
              HTML(paste0(paste(line,"<br/>")))
            } else {
              NULL
            }
          })
        }
      })
    } else {
      trans$verif <- F
    }
  })
  
  # LS fit ####
  observeEvent(c(input$model, input$sigmas, inputs$LogariRef, inputs$L0, inputs$TL0, inputs$ExponenRef, inputs$E0, 
                 inputs$TE0, inputs$offsetEpoch, inputs$period, inputs$periodRef, inputs$trendRef, input$fitType, 
                 trans$y, input$tab, inputs$PolyRef, inputs$PolyCoef, input$P0, input$correct_waveform, inputs$step, 
                 input$tunits, trans$sy), {
    req(trans$x, trans$y, trans$sy, trans$ordinate)
    if (input$tab == 4) {
      req(info$stop)
    }
    output$offsetFound <- renderUI({
      NULL
    })
    if (length(input$model) > 0) {
      if (input$fitType == 1) {
        if (isTruthy(trans$model_old)) {
          if (length(trans$model_old) < length(input$model)) {
            changes <- setdiff(input$model,trans$model_old)
          } else {
            changes <- setdiff(trans$model_old,input$model)
          }
          if (isTruthy(changes) && (("Offset" %in% changes && !isTruthy(inputs$offsetEpoch)) || ("Exponential" %in% changes && !isTruthy(inputs$ExponenRef)) || ("Logarithmic" %in% changes && !isTruthy(inputs$LogariRef)) || ("Polynomial" %in% changes && !isTruthy(inputs$PolyCoef)))) {
            trans$model_old <- as.list(input$model)
            req(info$stop)
          }
        }
        trans$model_old <- input$model
        x <- trans$x
        y <- trans$y - trans$ordinate
        if (isTruthy(input$correct_waveform)) {
          if (length(trans$pattern) > 0) {
            y <- y - trans$pattern
          } else {
            updateCheckboxInput(session, inputId = "correct_waveform", label = NULL, value = F)
            updateCheckboxInput(session, inputId = "waveform", label = NULL, value = F)
          }
        }
        sy <- trans$sy
        if (any(sy <= 0) || any(is.na(sy))) {
          showNotification("Some errorbar values are not valid. No weighting applied.", action = NULL, duration = 10, closeButton = T, id = "bad_errorbar", type = "error", session = getDefaultReactiveDomain())
          sy <- rep(1, length(y))
          updateCheckboxInput(session, inputId = "sigmas", label = NULL, value = F)
        }
        weights <- 1/(sy^2)
        m <- model(x,y)
        if (isTruthy(info$run) && isTruthy(m)) {
          info$run <- F
          trans$mle <- F
          trans$verif <- NULL
          if (messages > 0) cat(file = stderr(), "LS fit", "\n")
          model <- m$model
          model_lm <- m$model_lm
          apriori <- m$apriori
          req(model, apriori)
          if (messages > 1) cat(file = stderr(), model, "\n")
            fit <- NULL
            fit <- try(nls(as.formula(model), model = T, start = apriori, trace = F, weights = weights, control = nls.control(minFactor = 1/8192, warnOnly = F, printEval = F)), silent = F)
            if (!inherits(fit,"try-error") && !is.null(fit)) {
              info$run <- T
              jacobian <- fit$m$gradient()/sqrt(weights)
              synthesis <- summary(fit,correlation = T, signif.stars = T)
              synthesis$coefficients[1] <- coef(synthesis)[1] + trans$ordinate
              trans$names <- names(coef(fit))
              synthesis$formula <- deparse(synthesis$formula)
              synthesis$formula <- gsub(" > ", ">", gsub(" - ", "-", gsub(" \\* ", "\\*", gsub("))", ")", gsub("I\\(cos", "cos", gsub("I\\(sin", "sin", gsub("^ *|(?<= ) | *$", "", Reduce(paste, synthesis$formula), perl = TRUE)))))))
              synthesis$parameters[1,1] <- synthesis$parameters[1,1] + trans$ordinate
              trans$unc <- synthesis$coefficients[2,2]
              mod <- predict(fit)
              if (length(mod) == 1) {
                mod <- rep(mod, length(trans$x))
              }
              mod <- mod + trans$ordinate
              if (isTruthy(input$correct_waveform) && length(trans$pattern) > 0) {
                mod <- mod + trans$pattern
              }
              res <- residuals(fit)
              if ("Sinusoidal" %in% input$model && isTruthy(synthesis$coefficients)) {
                ss <- 0
                info_out <- c()
                for (s in which(grepl(pattern = "S", row.names(synthesis$coefficients)))) {
                  ss <- ss + 1
                  sine <- synthesis$coefficients[s,1]
                  cosine <- synthesis$coefficients[s + 1,1]
                  sine_err <- synthesis$coefficients[s,2]
                  cosine_err <- synthesis$coefficients[s + 1,2]
                  if (sine == 0) {
                    a <- 1
                  } else {
                    a <- 1/( 1 + cosine^2/sine^2)
                  }
                  amp <- sqrt(sine^2 + cosine^2)
                  phase <- atan2(cosine,sine)
                  amp_err <- try(sqrt(sine^2*sine_err^2/amp^2 + cosine^2*cosine_err^2/amp^2 + 2*sine*cosine*synthesis$cov.unscaled[s,s + 1]/amp^2), silent = F)
                  phase_err <- try(sqrt(a^2*(cosine_err^2 + cosine^2*sine_err^2/sine^2 - 2*cosine*synthesis$cov.unscaled[s,s + 1]/sine)/sine^2), silent = F)
                  if (isTruthy(amp_err) && isTruthy(phase_err)) {
                    info_out <- c(info_out, amp, amp_err, phase, phase_err)
                  } else {
                    if (messages > 1) cat(file = stderr(), a, amp, phase, sine, sine_err, cosine, cosine_err, synthesis$cov.unscaled[s,s + 1], "\n")
                    showNotification(paste0("Unable to compute the amplitude and/or phase error from the sine and cosine factors of sinusoid ",ss,". Check the input sinusoidal parameters or the time series length and its time units."), action = NULL, duration = 10, closeButton = T, id = "bad_sinusoidal", type = "error", session = getDefaultReactiveDomain())
                    ss <- ss - 1
                  }
                }
                if (isTruthy(info_out)) {
                  synthesis$sinusoidales <- matrix(data = info_out, nrow = ss, ncol = 4, byrow = T)
                  dimnames(synthesis$sinusoidales) <- list(paste0("Sinusoidal ",1:ss), c("Amplitude","Amp. Error","Phase (rad)","Phase Error (rad)"))
                }
              }
              trans$equation <- sub("y ~","Model =",m$model)
              trans$results <- synthesis
              trans$LScoefs <- synthesis$coefficients
              trans$res <- res
              if (input$waveletType != 1) {
                updateRadioButtons(session, inputId = "waveletType", label = NULL, choices = list("None" = 0, "Original" = 1, "Model" = 2, "Model res." = 3, "Filter" = 4, "Filter res." = 5), selected = 0, inline = T, choiceNames = NULL,  choiceValues = NULL)
              }
              trans$moderror <- sqrt( diag(jacobian %*% synthesis$cov.unscaled %*% t(jacobian)) )
              if (isTruthy(synthesis$sigma)) {
                if (!any(1/weights < trans$moderror^2)) {
                  trans$reserror <- sqrt( 1/weights - trans$moderror^2 ) * synthesis$sigma
                }
                trans$moderror <- trans$moderror * synthesis$sigma
              }
              trans$mod <- mod
              if (isTruthy(inputs$waveformPeriod)) {
                save_value <- inputs$waveformPeriod
                updateTextInput(session, "waveformPeriod", value = "")
                updateTextInput(session, "waveformPeriod", value = save_value)
              }
            } else {
              trans$results <- NULL
              trans$unc <- NULL
              trans$res <- NULL
              trans$mod <- NULL
              showNotification("Unable to fit the LS model. Change the model components.", action = NULL, duration = 10, closeButton = T, id = "bad_LS", type = "error", session = getDefaultReactiveDomain())
            }
        } else {
          trans$results <- NULL
          trans$res <- NULL
          trans$mod <- NULL
        }
      }
    } else {
      trans$results <- NULL
      trans$res <- NULL
      trans$mod <- NULL
    }
    #
    if (isTruthy(debug)) {
      env <- environment()  # can use globalenv(), parent.frame(), etc
      output$debug <- renderTable({
        data.frame(
          object = ls(env),
          size = unlist(lapply(ls(env), function(x) {
            format(object.size(get(x, envir = env, inherits = F)), unit = 'Mb')
          }))
        )
      })
    }
  }, priority = 2) 
  
  # KF fit ####
  observeEvent(input$runKF, {
    req(input$model, trans$x, trans$y)
    output$offsetFound <- renderUI({
      NULL
    })
    if (input$fitType == 2) {
      trans$mle <- F
      trans$verif <- NULL
      withBusyIndicatorServer("runKF", {
        x <- trans$x
        y <- trans$y
        if (isTruthy(input$correct_waveform)) {
          if (length(trans$pattern) > 0) {
            y <- y - trans$pattern
          }
        }
        sy <- trans$sy
        if (any(sy <= 0) || any(is.na(sy))) {
          showNotification("Some errorbar values are not valid. No weighting applied.", action = NULL, duration = 10, closeButton = T, id = "no_weighting", type = "error", session = getDefaultReactiveDomain())
          sy <- rep(1, length(y))
          updateCheckboxInput(session, inputId = "sigmas", label = NULL, value = F)
        }
        trans$mod <- trans$mod0 <- NULL
        trans$res <- trans$res0 <- NULL
        trans$kalman <- trans$kalman0 <- NULL
        trans$kalman_unc <- trans$kalman_unc0 <- NULL
        trans$results <- NULL
        m <- model(x,y)
        req(m$model, m$apriori, m$nouns, m$processNoise, m$error)
        if (!isTruthy(m) && length(m$apriori) < 2) {
          showNotification("Not enough model components to run the KF. Check the input values.", action = NULL, duration = 10, closeButton = T, id = "bad_model", type = "error", session = getDefaultReactiveDomain())
          info$run <- F
          req(info$stop)
        }
        if (messages > 1) cat(file = stderr(), m$model, "\n")
        apriori <- as.numeric(m$apriori)
        unc_ini <- as.numeric(m$error)^2
        if (isTruthy(input$sigmas)) {
          if (isTruthy(input$ObsError)) {
            sigmaR <- as.numeric(input$ObsError) * sy / median(sy)
          } else {
            sigmaR <- info$noise/5
            updateTextInput(session, "ObsError", value = sigmaR)
            sigmaR <- sigmaR * sy / median(sy)
          }
        } else {
          if (isTruthy(input$ObsError)) {
            sigmaR <- rep(as.numeric(input$ObsError), length(trans$y))
          } else {
            sigmaR <- info$noise/5
            updateTextInput(session, "ObsError", value = sigmaR)
            sigmaR <- rep(sigmaR, length(trans$y))
          }
        }
        if (!isTruthy(info$regular)) {
          showNotification("The series is not evenly sampled. The KF process noise will not change after a data gap.", action = NULL, duration = 10, closeButton = T, id = "not_even", type = "warning", session = getDefaultReactiveDomain())
        }
        #Measurement function
        FFfunction <- function(x,k) {
          e <- matrix(0, nrow = k, ncol = length(x))
          e[k,] <- x
          x <- trans$x
          if (k == 1) {
            obs <- e[1,1]
          } else {
            obs <- eval(parse(text = model_kf)) 
          }
          c(obs)
        }
        #State transition
        if ("Linear" %in% input$model && !is.na(as.numeric(input$TrendDev)) && as.numeric(input$TrendDev) > 0) {
          model_kf <- m$model_kf_inst
          GGfunction <- function(x, k) {
            if (k != 1) {
              x[1] <- x[1] + x[2] * (trans$x[k] - trans$x[k - 1])
            }
            c(x)
          }
        } else {
          model_kf <- m$model_kf_mean
          GGfunction <- function(x, k) {
            c(x)
          }
        }
        start.time <- Sys.time()
        llikss <- function(x, data) {
          if (messages > 2) cat(file = stderr(), "This iteration = ", sqrt(exp(x[1])), "\n")
          mod <- NULL
          mod <- list(
            m0 = apriori,
            C0 = diag(unc_ini),
            V = exp(x[1]),
            W = diag(m$processNoise))
          UKF(y = data, mod = mod, FFfunction = FFfunction, GGfunction = GGfunction, simplify = T, logLik = T)$logLik
        }
        if (input$errorm) {
          if (inputs$min_optirange == "" || inputs$max_optirange == "") {
            min_optirange <- info$noise/10
            max_optirange <- info$noise*10
            updateTextInput(session, "min_optirange", value = min_optirange)
            updateTextInput(session, "max_optirange", value = max_optirange)
          } else {
            min_optirange <- inputs$min_optirange
            max_optirange <- inputs$max_optirange
          }
          if (nchar(min_optirange) > 0 && nchar(max_optirange) > 0 && min_optirange > 0 && !is.na(as.numeric(min_optirange)) && max_optirange > 0 && !is.na(as.numeric(max_optirange))) {
            if (messages > 0) cat(file = stderr(), "Optimizing measurement noise", "\n")
            mod <- optim(log(median(sigmaR)^2), llikss, lower = log(as.numeric(min_optirange)^2), upper = log(as.numeric(max_optirange)^2), method = "Brent", hessian = T, data = y, control = list(reltol = exp(as.numeric(min_optirange)/10)))
            if (mod$convergence == 0) {
              sigmaR <- sqrt(exp(mod$par))
              seParms <- sqrt(diag(solve(mod$hessian)))
              if (isTruthy(seParms)) {
                rangoR <- sqrt(exp(mod$par + qnorm(.05/2)*seParms %o% c(1,-1))) 
              } else {
                rangoR <- sqrt(exp(mod$par))
              }
            } else {
              rangoR <- c("?", "?")
            }
            output$noise <- renderUI({
              line1 <- "Estimated meas. noise (95% CI):"
              line2 <- paste0("[",paste(sprintf("%.4f",rangoR), collapse = " "),"]")
              HTML(paste(line1, line2, sep = '<br/>'))
            })
          } else {
            showNotification("The input measurement error bounds are not valid. Skipping optimization.", action = NULL, duration = 10, closeButton = T, id = "bad_measurement_error", type = "error", session = getDefaultReactiveDomain())
          }
        }
        ex1 <- list(m0 = apriori, C0 = diag(unc_ini), V = sigmaR^2, W = diag(m$processNoise))
        kfs <- NULL
        if (any(is.na(ex1$C0))) {
          showNotification("Missing information required on the a priori state to run the Kalman filter. Check the input values", action = NULL, duration = 10, closeButton = T, id = "bad_a_priori_state", type = "error", session = getDefaultReactiveDomain())
          info$run <- F
          req(info$stop)
        }
        # EKF
        if (input$kf == 1) {
          if (messages > 0) cat(file = stderr(), "EKF fit", "\n")
          kf <- NULL
          kf <- try(dlmExtFilter(y = y, mod = ex1, GGfunction = GGfunction, FFfunction = FFfunction), silent = F)
          if (!inherits(kf,"try-error") && !is.null(kf)) {
            kfs <- NULL
            kfs <- try(dlmExtSmooth(kf), silent = F)
            if (!inherits(kfs,"try-error") && !is.null(kfs)) {
              varcov_kfs <- dlmSvd2var(kfs$U.S, kfs$D.S)
              kfs_unc <- matrix(data = 0, nrow = nrow(kfs$s) - 1, ncol = ncol(kfs$s))
              for (component in seq_len(ncol(kfs$s))) {
                kfs_unc[,component] <- unlist(sapply(varcov_kfs[2:length(varcov_kfs)], function(x) diag(x)[component]))
              }
              if (any(kfs_unc < 0)) {
                kfs_unc[kfs_unc < 0] <- NA
                showNotification("Negative estimated state variances were found and changed to NA. Something went wrong with the EKF fit.", action = NULL, duration = 15, closeButton = T, id = "bad_variance", type = "warning", session = getDefaultReactiveDomain())
              }
            } else {
              trans$results <- NULL
              trans$res <- NULL
              trans$mod <- NULL
              info$run <- F
              showNotification("Unable to run the EKF smoother. The error covariances of the initial state may be zero or too large", action = NULL, duration = 10, closeButton = T, id = "bad_kf", type = "error", session = getDefaultReactiveDomain())
            }
          } else {
            trans$results <- NULL
            trans$res <- NULL
            trans$mod <- NULL
            info$run <- F
            showNotification("Unable to fit the EKF. Change the model parameters.", action = NULL, duration = 10, closeButton = T, id = "bad_kf", type = "error", session = getDefaultReactiveDomain())
          }
        # UKF
        } else if (input$kf == 2) {
          if (messages > 0) cat(file = stderr(), "UKF fit", "\n")
          kf <- NULL
          kf <- try(UKF(y = y, mod = ex1, sqrtMethod = "svd", GGfunction = GGfunction, FFfunction = FFfunction), silent = F)
          if (!inherits(kf,"try-error") && !is.null(kf)) {
            kfs <- NULL
            kfs <- try(UKFsmooth(kf, GGfunction = GGfunction), silent = F)
            if (!inherits(kfs,"try-error") && !is.null(kfs)) {
              kfs_unc <- matrix(data = 0, nrow = nrow(kfs$s) - 1, ncol = ncol(kfs$s))
              for (component in seq_len(ncol(kfs$s))) {
                kfs_unc[,component] <- unlist(sapply(kfs$S[2:length(kfs$S)], function(x) diag(x)[component]))
              }
              if (any(kfs_unc < 0)) {
                kfs_unc[kfs_unc < 0] <- NA
                showNotification("Negative estimated state variances were found and changed to NA. Something went wrong with the UKF fit.", action = NULL, duration = 15, closeButton = T, id = "bad_variance", type = "warning", session = getDefaultReactiveDomain())
              }
            } else {
              trans$results <- NULL
              trans$res <- NULL
              trans$mod <- NULL
              info$run <- F
              showNotification("Unable to run the UKF smoother. The error covariances of the initial state may be zero or too large.", action = NULL, duration = 10, closeButton = T, id = "bad_kf", type = "error", session = getDefaultReactiveDomain())
            }
          } else {
            trans$results <- NULL
            trans$res <- NULL
            trans$mod <- NULL
            info$run <- F
            showNotification("Unable to fit the UKF. Change the model parameters.", action = NULL, duration = 10, closeButton = T, id = "bad_kf", type = "error", session = getDefaultReactiveDomain())
          }
        }
        # Common EKF & UKF
        if (isTruthy(kfs$s)) {
          info$run <- T
          e <- kfs$s[2:nrow(kfs$s),]
          if ("Linear" %in% input$model && !is.na(as.numeric(input$TrendDev)) && as.numeric(input$TrendDev) > 0) { 
            trans$mod <- trans$mod0 <- sapply(1:length(x), function(k) if (k == 1) { e[1,1] } else { eval(parse(text = sub("+ e[k,2]*(x[k] - x[k-1])", "", model_kf, fixed = T))) })
          } else {
            trans$mod <- trans$mod0 <- sapply(1:length(x), function(k) eval(parse(text = model_kf)) )
          }
          trans$res <- trans$res0 <- y - trans$mod
          if (isTruthy(input$correct_waveform) && length(trans$pattern) > 0) {
            trans$mod <- trans$mod + trans$pattern
          }
          # Computing time-variable mean rate
          # if ("Linear" %in% input$model && !is.na(as.numeric(input$TrendDev)) && as.numeric(input$TrendDev) > 0) {
          #   mean_rate <- lapply(1:length(x), function(i) coefficients(summary(lm(e[1:i,2]~1,weights = 1/kfs_unc[1:i,1])))[1:2])
          #   mean_rate[[1]][2] <- kfs_unc[1,2]
          #   e <- cbind(e,sapply(mean_rate, "[", 1))
          #   colnames(e) <- c(m$nouns, "MeanRate")
          #   kfs_unc <- cbind(kfs_unc,sapply(mean_rate, "[", 2))
          #   colnames(kfs_unc) <- c(m$nouns, "MeanRate")
          # } else {
            colnames(e) <- m$nouns
            colnames(kfs_unc) <- m$nouns
          # }
          trans$kalman <- trans$kalman0 <- e
          trans$kalman_unc <- trans$kalman_unc0 <- sqrt(kfs_unc)
          trans$results <- print(psych::describe(trans$kalman, na.rm = F, interp = T, skew = F, ranges = T, trim = 0, type = 3, check = T, fast = F, quant = c(.05,.25,.75,.95), IQR = T), digits = 4)
          trans$kalman_info <- m
          trans$equation <- sub("y ~","Model =",m$model)
          trans$x0_kf <- x
          end.time <- Sys.time()
          time.taken <- end.time - start.time
          if (messages > 2) cat(file = stderr(), "Total time = ", time.taken, "\n")
          if (isTruthy(input$remove3D)) {
            values$used_all_kf <- rep(T, length(x))
          } else {
            if (input$tab == 1 || is.null(input$tab)) {
              values$used_kf1 <- rep(T, length(x))
            } else if (input$tab == 2) {
              values$used_kf2 <- rep(T, length(x))
            } else if (input$tab == 3) {
              values$used_kf3 <- rep(T, length(x))
            }
          }
          if (isTruthy(inputs$waveformPeriod)) {
            save_value <- inputs$waveformPeriod
            updateTextInput(session, "waveformPeriod", value = "")
            updateTextInput(session, "waveformPeriod", value = save_value)
          }
          # Plot instantaneous rate
          output$rate1 <- output$rate2 <- output$rate3 <- renderPlot({
            if ("Linear" %in% input$model && length(trans$kalman) > 0 && sd(trans$kalman[,2]) > .Machine$double.eps) {
              title <- "Instantaneous linear rate" 
              plot_series(x,trans$kalman[,2],trans$kalman_unc[,2],ranges$x2,ranges$y4,T,title,input$symbol)
            }
          }, width = reactive(info$width), type = "cairo-png")
        }
      })
    }
  })
  
  # Plot residuals ####
  output$res1 <- output$res2 <- output$res3 <- renderPlot({
    req(obs(), trans$res, trans$x, trans$sy, info$run)
    if (messages > 0) cat(file = stderr(), "Plotting residual series", "\n")
    if (!is.null(trans$filter)) {
      title <- "Obs-Model residuals & Filter-Model residuals (blue)"
    } else {
      title <- "Model residuals"
    }
    if (length(trans$reserror) > 0) {
      ey <- trans$reserror
    } else {
      ey <- trans$sy
    }
    sigmas <- F
    if (isTruthy(input$sigmas) && ((input$format == 4 && isTruthy(inputs$errorBar)) || input$format != 4)) {
      sigmas <- T
    }
    plot_series(trans$x,trans$res,ey,ranges$x2,ranges$y2,sigmas,title,input$symbol)
    abline(h = 0, col = "red", lwd = 2)
    if (input$traceLog && length(info$log) > 0) {
      for (r in info$log[[2]]) {
        abline(v = r, col = "blue", lty = 2)
      }
      for (a in info$log[[1]]) {
        abline(v = a, col = "blue")
      }
    }
    if (input$traceSinfo && length(info$sinfo) > 0) {
      for (r in info$sinfo[[2]]) {
        abline(v = r, col = "blue", lty = 2)
      }
      for (a in info$sinfo[[1]]) {
        abline(v = a, col = "blue")
      }
    }
    if (input$traceSoln && length(info$soln) > 0) {
      for (a in info$soln[[1]]) {
        abline(v = a, col = "orange")
      }
    }
    if (input$traceCustom && length(info$custom) > 0) {
      for (a in info$custom) {
        abline(v = a, col = "green")
      }
    }
    if ("Offset" %in% isolate(input$model)) {
      for (p in trans$offsetEpochs) {
        abline(v = p, col = "red")
      }
    }
    if (!is.null(trans$filter) && input$filter == T) {
      if (input$series2filter == 1) {
        lines(trans$x,trans$filter - trans$mod, col = "blue", lwd = 3)
      } else if (input$series2filter == 2) {
        lines(trans$x,trans$filter, col = "blue", lwd = 3)
      }
    }
  }, width = reactive(info$width), type = "cairo-png")
  
  # Plot histogram ####
  output$hist1 <- output$hist2 <- output$hist3 <- renderPlot({
    req(obs(), input$histogram)
    if (input$histogramType == 1) {
      values <- trans$y[trans$x >= ranges$x1[1] & trans$x <= ranges$x1[2]]
      label <- "Original series"
    } else if (input$histogramType == 2 && length(trans$mod) > 0) {
      values <- trans$mod[trans$x >= ranges$x1[1] & trans$x <= ranges$x1[2]]
      label <- "Model series"
    } else if (input$histogramType == 3 && length(trans$res) > 0) {
      values <- trans$res[trans$x >= ranges$x1[1] & trans$x <= ranges$x1[2]]
      label <- "Model residual series"
    } else if (input$histogramType == 4 && length(trans$filter) > 0) {
      values <- trans$filter[trans$x >= ranges$x1[1] & trans$x <= ranges$x1[2]]
      label <- "Filter series"
    } else if (input$histogramType == 5 && length(trans$filterRes) > 0) {
      values <- trans$filterRes[trans$x >= ranges$x1[1] & trans$x <= ranges$x1[2]]
      label <- "Filter residual series"
    } else {
      updateRadioButtons(session, inputId = "histogramType", label = NULL, choices = list("None" = 0, "Original" = 1, "Model" = 2, "Model res." = 3, "Filter" = 4, "Filter res." = 5), selected = 0, inline = T, choiceNames = NULL,  choiceValues = NULL)
      req(info$stop)
    }
    if (messages > 0) cat(file = stderr(), "Plotting histogram", "\n")
    if (sd(values) > 0) {
      hist(values, breaks = "FD", freq = F, xlab = label, ylab = "", main = "", col = "lightblue")
      dnorm(values, mean = mean(values, na.rm = T), sd = sd(values), log = F)
      xfit <- seq(min(values),max(values),length = 40) 
      yfit <- dnorm(xfit,mean = mean(values, na.rm = T),sd = sd(values))
      lines(xfit, yfit, col = "red", lwd = 2) 
    } else {
      showNotification("Unable to compute the histogram. Check the input series.", action = NULL, duration = 10, closeButton = T, id = "no_histogram", type = "error", session = getDefaultReactiveDomain())
      updateRadioButtons(session, inputId = "histogramType", label = NULL, choices = list("None" = 0, "Original" = 1, "Model" = 2, "Model res." = 3, "Filter" = 4, "Filter res." = 5), selected = 0, inline = T, choiceNames = NULL,  choiceValues = NULL)
    }
  }, width = reactive(info$width), type = "cairo-png")
  
  # Stats ####
  output$stats1 <- output$stats2 <- output$stats3 <- renderPrint({
    req(obs(), input$histogram)
    if (input$histogramType == 1) {
      values <- trans$y[trans$x >= ranges$x1[1] & trans$x <= ranges$x1[2]]
      label <- "original series"
    } else if (input$histogramType == 2 && length(trans$mod) > 0) {
      values <- trans$mod[trans$x >= ranges$x1[1] & trans$x <= ranges$x1[2]]
      label <- "model series"
    } else if (input$histogramType == 3 && length(trans$res) > 0) {
      values <- trans$res[trans$x >= ranges$x1[1] & trans$x <= ranges$x1[2]]
      label <- "model residual series"
    } else if (input$histogramType == 4 && length(trans$filter) > 0) {
      values <- trans$filter[trans$x >= ranges$x1[1] & trans$x <= ranges$x1[2]]
      label <- "filter series"
    } else if (input$histogramType == 5 && length(trans$filterRes) > 0) {
      values <- trans$filterRes[trans$x >= ranges$x1[1] & trans$x <= ranges$x1[2]]
      label <- "filter residual series"
    } else {
      req(info$stop)
    }
    if (messages > 0) cat(file = stderr(), "Computing statistics", "\n")
    adf <- suppressWarnings(adf.test(values, alternative = "stationary"))
    kpss <- suppressWarnings(kpss.test(values, null = "Level"))
    if (isTruthy(adf$p.value) && isTruthy(kpss$p.value)) {
      cat(paste0("Statistics for the period from ", ranges$x1[1], " to ", ranges$x1[2]), "\n\n")
      if (kpss$p.value <= 0.01 && adf$p.value >= 0.01) {
        cat(paste0("WARNING: the ",label," are most certainly NOT stationary (probability > 99%)."), "\n\n")
      } else if (kpss$p.value < 0.05 && adf$p.value > 0.05) {
        cat(paste0("WARNING: the ",label," are likely NOT stationary (probability > 95%)."), "\n\n")
      } else if (kpss$p.value < 0.1 && adf$p.value > 0.1) {
        cat(paste0("WARNING: the ",label," could be NOT stationary (probability > 90%)."), "\n\n")
      } else {
        cat(paste0("The ",label," may be stationary (probability of non stationarity < 90%)."), "\n\n")
      }
      stats <- psych::describe(matrix(values, ncol = 1, byrow = T), na.rm = F, interp = T, skew = T, ranges = T, trim = 0, type = 3, check = T, fast = F, quant = c(.05,.25,.75,.95), IQR = T)
      print(stats,digits = 4)
    } else {
      showNotification("Unable to assess stationarity. Check the input series.", action = NULL, duration = 10, closeButton = T, id = "no_stationarity", type = "error", session = getDefaultReactiveDomain())
    }
  }, width = 180)
  
  # Fit summary ####
  output$summary1 <- output$summary2 <- output$summary3 <- renderPrint({
    req(obs())
    options(scipen = 8)
    if (input$optionSecondary == 1 && isTruthy(trans$z)) {
      serie1 <- data.frame(x = trans$x0[!is.na(trans$y0)], y = trans$y0[!is.na(trans$y0)])
      serie2 <- data.frame(x = trans$x0[!is.na(trans$z)], y = trans$z[!is.na(trans$z)])
      common <- merge(serie1, serie2, by.x = "x", by.y = "x")
      if (length(common$x) > 30) {
        cat("Pearson's correlation = ", sprintf("%.3f",cor(common$y.x,common$y.y)), "from ", length(common$x)," points at common epochs\n\n")
      }
    }
    if (isTruthy(input$midas)) {
      cat("MIDAS rate estimate ")
      cat(trans$midas_vel," +/- ",trans$midas_sig,"\n\n")
      if (length(trans$offsetEpochs) > 0 && "Offset" %in% isolate(input$model)) {
        cat("MIDAS rate estimate (discontinuities skipped) ")
        cat(trans$midas_vel2," +/- ",trans$midas_sig2,"\n\n")
      }
    }
    if (isTruthy(info$run) && length(trans$results) > 0) {
      if (input$fitType == 2) {
        cat("KF estimate")
        cat(paste0("\n",gsub(" > ", ">", gsub(" - ", "-", gsub(" \\* ", "\\*", gsub("))", ")", gsub("I\\(cos", "cos", gsub("I\\(sin", "sin", gsub("^ *|(?<= ) | *$", "", trans$equation, perl = T)))))))), "\n\n")
      } else if (input$fitType == 1) {
        cat("LS estimate") 
        trans$results$formula <- sub("y ~","Model =",trans$results$formula)
      }
      options(max.print = 1000)
      if (isTruthy(trans$results$sinusoidales)) {
        print(trans$results)
        trans$results$sinusoidales
      } else {
        trans$results
      }
    }
  })
  
  # Periodic waveform ####
  observeEvent(c(input$waveformPeriod, inputs$waveformPeriod, input$waveform, inputs$low, inputs$high, input$fitType), {
    req(trans$x)
    if (isTruthy(input$correct_waveform)) {
      trans$pattern <- NULL
      trans$wave <- inputs$waveformPeriod
    } else {
      trans$wave <- NULL
      if (isTruthy(trans$pattern) && length(trans$pattern) > 0 && length(trans$filterRes) > 0) {
        trans$filter <- trans$filter - trans$pattern
        trans$filterRes <- trans$filterRes + trans$pattern
      }
      if (length(trans$x) > 0 && (length(trans$res) > 0 || length(trans$filterRes) > 0) && isTruthy(input$waveform) && isTruthy(inputs$waveformPeriod)) {
        if (nchar(inputs$waveformPeriod) > 0 && !is.na(as.numeric(inputs$waveformPeriod)) && as.numeric(inputs$waveformPeriod) > 2*info$sampling  && as.numeric(inputs$waveformPeriod) < abs(info$rangex)/2) {
          x <- trans$x %% as.numeric(inputs$waveformPeriod)
          if (length(trans$res) > 0) {
            serie <- data.frame(x0 = trans$x, x = x, y = trans$res, sy = trans$sy)[order(x),]  
          } else if (length(trans$filterRes) > 0) {
            serie <- data.frame(x0 = trans$x, x = x, y = trans$filterRes, sy = trans$sy)[order(x),]
          }
          serie$z <- floor(serie$x/info$sampling)
          table <- as.data.table(serie, keep.rownames = T)
          uniques <- sum(setDT(table)[, .N, z]$N ==  1)
          if (uniques > 0) {
            showNotification(paste0(uniques," data epochs are not repeated in the series. The waveform may be wrong at these epochs."), action = NULL, duration = 10, closeButton = T, id = "no_repeat", type = "warning", session = getDefaultReactiveDomain())          
          }
          average <- as.data.frame.matrix(table[,list(avg = weightedMedian(y,1/sy^2)), by = z])
          result <- merge(serie,average, by = "z")
          result <- result[order(result$x0),]
          trans$pattern <- result$avg
          if (messages > 0) cat(file = stderr(), "Computing periodic waveform", "\n")
          if (length(trans$res) > 0) {
            trans$mod <- trans$mod + trans$pattern
            trans$res <- trans$res - trans$pattern  
          } else if (length(trans$filterRes) > 0) {
            trans$filter <- trans$filter + trans$pattern
            trans$filterRes <- trans$filterRes - trans$pattern
          }
          toplot <- !duplicated(result[,"z"])
          output$waveform1 <- output$waveform2 <- output$waveform3 <- renderPlot({
            if (length(trans$res) > 0 || length(trans$filterRes) > 0) {
              if (length(trans$res) > 0) {
                title <- "Periodic waveform from model residuals" 
              } else if (length(trans$filterRes) > 0) {
                title <- "Periodic waveform from filter residuals"
              }
              if (input$symbol == 0) {
                symbol <- 'p'
              } else if (input$symbol == 1) {
                symbol <- 'l'
              } else if (input$symbol == 2) {
                symbol <- 'o'
              }
              waveform <- data.frame(x = result[toplot,]$x, y = result[toplot,]$avg)
              waveform <- waveform[order(waveform$x),]
              plot(waveform$x,waveform$y, type = symbol, pch = 20, xlab = "Epoch of period", ylab = "Average value", main = title)
            }
          }, width = reactive(info$width), type = "cairo-png")
        } else {
          showNotification("The period of the waveform is not valid. Check the input value.", action = NULL, duration = 10, closeButton = T, id = "bad_waveform_period", type = "error", session = getDefaultReactiveDomain())
          if (length(trans$pattern) > 0) {
            if (length(trans$res) > 0) {
              trans$mod <- trans$mod - trans$pattern
              trans$res <- trans$res + trans$pattern  
            }
            trans$pattern <- NULL
          }
        }
      } else {
        if (length(trans$pattern) > 0) {
          if (length(trans$res) > 0) {
            trans$mod <- trans$mod - trans$pattern
            trans$res <- trans$res + trans$pattern  
          }
          trans$pattern <- NULL
        }
      }
    }
  })
  
  # Computing spectrum ####
  observeEvent(c(input$spectrum, inputs$short_period, inputs$long_period, inputs$ofac, inputs$step), {
    req(obs(), input$spectrum)
    if (is.na(inputs$long_period) && input$long_period != "") {
      showNotification("The longest period is not a numeric value. Check the input value.", action = NULL, duration = 10, closeButton = T, id = "bad_long", type = "error", session = getDefaultReactiveDomain())
      req(info$stop)
    }
    if (is.na(inputs$short_period) && input$short_period != "") {
      showNotification("The shortest period is not a numeric value. Check the input value.", action = NULL, duration = 10, closeButton = T, id = "bad_short", type = "error", session = getDefaultReactiveDomain())
      req(info$stop)
    }
    if (is.na(inputs$ofac) && input$ofac != "") {
      showNotification("The oversampling value is not numeric. Check the input value.", action = NULL, duration = 10, closeButton = T, id = "bad_oversampling", type = "error", session = getDefaultReactiveDomain())
      req(info$stop)
    }
    if (messages > 0) cat(file = stderr(), "Setting periodogram limits", "\n")
    trans$fs <- NULL
    trans$title <- c("Lomb-Scargle periodogram:")
    max_period <- info$rangex
    intervals <- as.data.frame(table(diff(trans$x)))
    # min_period <- 2*gcd(trans$x[-1]*10^info$decimalsx-trans$x[1]*10^info$decimalsx)/10^info$decimalsx #following Eyer and Bartholdi 1999
    min_period <- 2*as.numeric(as.character(intervals$Var1[intervals$Freq/length(trans$x) >= 0.5][1])) #approximate the shortest period by twice the shortest interval repeating itself at least 50% of the time
    
    if (!isTruthy(min_period)) {
      min_period <- 2*min(abs(diff(trans$x)))
    }
    # Setting longest period
    if (isTruthy(inputs$long_period)) {
      if (inputs$long_period > max_period) {
        showNotification("The input longest period is out of bounds. Using the longest valid value instead.", action = NULL, duration = 10, closeButton = T, id = "bad_long", type = "warning", session = getDefaultReactiveDomain())
        updateTextInput(session, "long_period", value = max_period - 1/10^(info$decimalsx + 1))
        trans$fs <- NULL
        req(info$stop)
      } else {
        long_period <- inputs$long_period
      }
    } else {
      long_period <- max_period - 1/10^(info$decimalsx + 1)
      short_period <- min_period
      updateTextInput(session, "short_period", value = short_period)
      updateTextInput(session, "long_period", value = long_period)
      ranges$x3 <- NULL
      trans$fs <- NULL
      req(info$stop)
    }
    # Setting shortest period
    if (isTruthy(inputs$short_period)) {
      if (inputs$short_period < min_period) {
        showNotification("The input shortest period is smaller than the propossed value. The periodogram may be aliased into the Nyquist period range.", action = NULL, duration = 10, closeButton = T, id = "bad_short", type = "warning", session = getDefaultReactiveDomain())
      }
      short_period <- inputs$short_period
    } else {
      long_period <- max_period - 1/10^(info$decimalsx + 1)
      short_period <- min_period
      updateTextInput(session, "short_period", value = short_period)
      updateTextInput(session, "long_period", value = long_period)
      ranges$x3 <- NULL
      trans$fs <- NULL
      req(info$stop)
    }
    # Setting oversampling
    if (isTruthy(inputs$ofac)) {
      if (inputs$ofac < 0.01 || inputs$ofac > 100) {
        showNotification("The input oversampling value is out of bounds [0.01 - 100]. Using 1 instead.", action = NULL, duration = 10, closeButton = T, id = "bad_oversampling", type = "error", session = getDefaultReactiveDomain())
        trans$fs <- NULL
        updateTextInput(session, "ofac", value = 1)
        req(info$stop)
      } else {
        ofac <- inputs$ofac
        if (isTruthy(ranges$x3) && (ranges$x3[1] != short_period || ranges$x3[2] != long_period)) {
          short_period <- ranges$x3[1]
          long_period <- ranges$x3[2]
          ranges$x3 <- NULL
          ranges$y3 <- NULL
          trans$fs <- NULL
          updateTextInput(session, "long_period", value = long_period)
          updateTextInput(session, "short_period", value = short_period)
          req(info$stop)
        }
      }
    } else {
      trans$fs <- NULL
      updateTextInput(session, "ofac", value = 1)
      req(info$stop)
    }
    # Computing periodogram if all necessary values are good
    if (input$spectrum && short_period > 0 && long_period > 0 && 1/short_period > 1/long_period + 1/(max_period*ofac)) {
      if ((input$tab == 1) || (input$format == 4)) {
        shinyjs::show(id = "res1_espectral", anim = T, animType = "fade", time = 0.5, selector = NULL)
      } else if (input$tab == 2) {
        shinyjs::show(id = "res2_espectral", anim = T, animType = "fade", time = 0.5, selector = NULL)
      } else if (input$tab == 3) {
        shinyjs::show(id = "res3_espectral", anim = T, animType = "fade", time = 0.5, selector = NULL)
      }
      f <- seq(1/long_period,1/short_period,1/(max_period*ofac)) # cycles per time unit
      if (short_period == min_period) {
        f <- f[1:length(f) - 1] # drop the shortest period just in case the series are not evenly sampled
      }
      trans$fs <- f
      trans$amp <- matrix(NA, nrow = length(f), ncol = 5)
      trans$psd <- matrix(NA, nrow = length(f), ncol = 5)
      trans$col <- c(1,2,3,4,5)
      periodogram("all")
    } else {
      showNotification("Negative, null or invalid period bounds for the periodogram. Check the inputs.", action = NULL, duration = 10, closeButton = T, id = "bad_periods", type = "error", session = getDefaultReactiveDomain())
      trans$fs <- NULL
      if ((input$tab == 1) || (input$format == 4)) {
        shinyjs::hide(id = "res1_espectral", anim = T, animType = "fade", time = 0.5, selector = NULL)
      } else if (input$tab == 2) {
        shinyjs::hide(id = "res2_espectral", anim = T, animType = "fade", time = 0.5, selector = NULL)
      } else if (input$tab == 3) {
        shinyjs::hide(id = "res3_espectral", anim = T, animType = "fade", time = 0.5, selector = NULL)
      }
    }
  })
  observeEvent(c(input$spectrumOriginal), {
    req(obs(), input$spectrum)
    if (isTruthy(trans$spectra_old[1])) {
      trans$psd[,1] <- NA
      trans$amp[,1] <- NA
      trans$spectra_old[1] <- F
      trans$title[2] <- NA
    } else {
      periodogram("original")
    }
  })
  observeEvent(c(input$spectrumModel), {
    req(obs(), input$spectrum)
    if (isTruthy(trans$spectra_old[2])) {
      trans$psd[,2] <- NA
      trans$amp[,2] <- NA
      trans$spectra_old[2] <- F
      trans$title[3] <- NA
    } else {
      periodogram("model")
    }
  })
  observeEvent(c(input$periodogram_residuals), {
    req(obs(), input$spectrum)
    if (isTruthy(trans$spectra_old[3])) {
      trans$psd[,3] <- NA
      trans$amp[,3] <- NA
      trans$spectra_old[3] <- F
      trans$title[4] <- NA
    } else {
      periodogram("residuals")
    }
  })
  observeEvent(c(input$spectrumFilter), {
    req(obs(), input$spectrum)
    if (isTruthy(trans$spectra_old[4])) {
      trans$psd[,4] <- NA
      trans$amp[,4] <- NA
      trans$spectra_old[4] <- F
      trans$title[5] <- NA
    } else {
      periodogram("filter")
    }
  })
  observeEvent(c(input$spectrumFilterRes), {
    req(obs(), input$spectrum)
    if (isTruthy(trans$spectra_old[5])) {
      trans$psd[,5] <- NA
      trans$amp[,5] <- NA
      trans$spectra_old[5] <- F
      trans$title[6] <- NA
    } else {
      periodogram("filterRes")
    }
  })
  observeEvent(c(trans$y, trans$sy), {
    req(obs(), input$spectrum)
    if (input$spectrumOriginal) {
      periodogram("original")
    }
  })
  observeEvent(c(trans$res, trans$model), {
    req(obs(), input$spectrum)
    if (input$spectrumModel || input$periodogram_residuals) {
      periodogram(c("model","residuals"))
    }
  })
  observeEvent(c(trans$filter, trans$filterRes), {
    req(obs(), input$spectrum)
    if (input$spectrumFilter || input$spectrumFilterRes) {
      periodogram(c("filter","filterRes"))
    }
  })
  
  # Plot spectrum ####
  output$res1_espectral <- output$res2_espectral <- output$res3_espectral <- renderPlot({
    req(obs(), input$spectrum, trans$fs, trans$psd)
    if (length(trans$fs) > 0) {
      if (messages > 0) cat(file = stderr(), "Plotting periodogram", "\n")
      marks <- c(1 %o% 10^(-20:20))
      if (input$tunits == 1) {
        period <- "days"
      } else if (input$tunits == 2) {
        period <- "weeks"
      } else if (input$tunits == 3) {
        period <- "years"
      }
      if (input$spectrumType == 0) {
        spectrum_y <- trans$amp
        trans$spectra <- cbind(1/trans$fs, trans$amp[,!is.na(colSums(trans$amp))])
        ylab <- "Amplitude"
      } else if (input$spectrumType == 1) {
        spectrum_y <- trans$psd
        trans$spectra <- cbind(1/trans$fs, trans$psd[,!is.na(colSums(trans$psd))])
        ylab <- "Power"
      }
      title <- substring(paste(trans$title[!is.na(trans$title)],collapse = ""), 1, nchar(paste(trans$title[!is.na(trans$title)],collapse = "")) - 2)
      if (is.null(ranges$x3)) {
        matplot(x = 1/trans$fs, y = spectrum_y, type = "l", lty = 1, main = title, log = "xy", col = trans$col, xlab = paste0("Period (",period,")"), ylab = ylab, yaxt = 'n', xlim = rev(range(1/trans$fs)), ylim = ranges$y3)
      } else {
        matplot(x = 1/trans$fs, y = spectrum_y, type = "l", lty = 1, main = title, log = "xy", col = trans$col, xlab = paste0("Period (",period,")"), ylab = ylab, yaxt = 'n', xlim = rev(ranges$x3), ylim = ranges$y3)
      }
      axis(2,at = marks, labels = marks)
      if (input$spectrumType == 1) {
        if (input$mle && length(trans$noise) > 0 && isTruthy(trans$noise)) {
          if (input$tunits == 1) { #days
            f_scale <- 24*60*60
          } else if (input$tunits == 2) { #weeks
            f_scale <- 7*24*60*60
          } else if (input$tunits == 3) { #years
            f_scale <- 365.25*24*60*60
          }
          fs_hz <- 1/(info$sampling*f_scale)
          f_hz <- trans$fs/f_scale
          if (isTruthy(input$white)) {
            k <- 0
            b0 <- trans$noise[1]
          } else {
            b0 <- 0
            k <- 0
          }
          Dk <- 2*(2*pi)^k * f_scale^(k/2)
          wn <- b0^2 * Dk / (fs_hz^(1 + (k/2))) #from Williams 2003 (Eq. 10)
          pwn <- wn * f_hz^k
          if (isTruthy(input$flicker) && isTruthy(trans$noise[3])) {
            bk <- trans$noise[3]
            k <- -1
          } else if (isTruthy(input$randomw) && isTruthy(trans$noise[5])) {
            bk <- trans$noise[5]
            k <- -2
          } else if (isTruthy(input$powerl) && isTruthy(trans$noise[7])) {
            bk <- trans$noise[7]
            k <- trans$noise[9]
          } else {
            bk <- 0
            k <- 0
          }
          Dk <- 2*(2*pi)^k * (24*60*60*365.25)^(k/2)
          pl <- bk^2 * Dk / (fs_hz^(1 + k/2))
          pln <- pl * f_hz^k
          ps <- pmax(pwn, pln)
          ps <- ps*trans$var/sum(ps)
          ps_long <- head(ps, n = 1)
          ps_short <- tail(ps, n = 1)
          crossover <- 0
          if (isTruthy(b0) && isTruthy(bk)) {
            if (b0 > 0 && bk > 0) {
              crossover <- f_scale * ( (b0^2/bk^2) * 2*pi*sqrt(f_scale)/sqrt(fs_hz) )^(-1) #following Williams 2003
            } else if (b0 > 0) {
              crossover <- 1/(inputs$long_period + 1)
            } else if (bk > 0) {
              crossover <- 2/inputs$short_period
            }
          } else {
            showNotification("Unable to plot the noise power spectrum on top of the periodogram. Check the noise analysis results.", action = NULL, duration = 10, closeButton = T, id = "no_noise_psd", type = "error", session = getDefaultReactiveDomain())
          }
          if (crossover > 0) {
            if (1/crossover <= inputs$short_period) {
              lombx <- c(inputs$long_period,inputs$short_period)
              lomby <- c(ps_long,ps_long*(lombx[2]/inputs$long_period)^(-k))
            } else if (1/crossover >= inputs$long_period) {
              lombx <- c(inputs$long_period,inputs$short_period)
              lomby <- c(ps_short,ps_short)
            } else {
              lombx <- c(inputs$long_period,1/crossover,1/crossover,inputs$short_period)
              lomby <- c(ps_long,ps_short,ps_short,ps_short)
            }
            lines(lombx,lomby, col = "red", lty = 2, lwd = 2)
          }
        } else {
          c <- max(which(trans$spectra_old))
          p <- trans$psd[,c]
          slope <- lm(log10(p) ~ log10(1/trans$fs))
          slope$coef[2] <- -1*slope$coef[2]
          regression <- 10^(predict(slope, newdata = list(x = 1/trans$fs)))
          lines(1/trans$fs, regression, col = c)
          text(inputs$long_period/2,min(p),paste0("Slope = ",sprintf("%4.2f",slope$coef[2])," +- ",sprintf("%3.2f",summary(slope)$coefficients[2,2])), col = c)
          lombx <- c(inputs$long_period,inputs$short_period)
          bias <- 0.01
          lomby_flicker <- c(bias*(10^(slope$coef[1])*(inputs$long_period/inputs$short_period)),bias*10^(slope$coef[1]))
          lines(lombx,lomby_flicker, col = "hotpink", lty = 2, lwd = 2)
          text(inputs$long_period/10,min(p),"Slope = -1",col = "hotpink")
        }
      }
      grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dashed", lwd = 1, equilogs = T)
      output$lomb1_info <- output$lomb2_info <- output$lomb3_info <- renderText({
        if (length(input$lomb_1click$x) > 0) {
          if (inputs$ofac == 1) {
            if (input$spectrumType == 0) {
              amp <- as.matrix(trans$amp[,colSums(is.na(trans$amp)) < nrow(trans$amp)])[,ncol(as.matrix(trans$amp[,colSums(is.na(trans$amp)) < nrow(trans$amp)]))]
              paste("Periodogram coordinates = ", input$lomb_1click$x, input$lomb_1click$y, "\t", " Series scatter = ",sqrt(0.5*sum(amp[trans$fs > 1/input$lomb_1click$x]^2)), sep = "\t")  
            } else if (input$spectrumType == 1) {
              psd <- as.matrix(trans$psd[,colSums(is.na(trans$psd)) < nrow(trans$psd)])[,ncol(as.matrix(trans$psd[,colSums(is.na(trans$psd)) < nrow(trans$psd)]))]
              paste("Periodogram coordinates = ", input$lomb_1click$x, input$lomb_1click$y, "\t", " Series scatter = ",sqrt(sum(psd[trans$fs > 1/input$lomb_1click$x])), sep = "\t")
            }
          } else {
            paste("Periodogram coordinates = ", input$lomb_1click$x, input$lomb_1click$y, sep = "\t")
          }
        }
      })
    }
  }, width = reactive(info$width), type = "cairo-png")
  
  # Plot wavelet ####
  output$wavelet1 <- output$wavelet2 <- output$wavelet3 <- renderPlot({
    trans$wavelet <- NULL
    req(obs(), input$wavelet, input$waveletType)
    if (isTruthy(isolate(info$run_wavelet))) {
      if (input$tunits == 1) {
        period <- "days"
        t <- 1/365.25
      } else if (input$tunits == 2) {
        period <- "weeks"
        t <- 7/365.25
      } else if (input$tunits == 3) {
        period <- "years"
        t <- 1
      }
      if (nchar(inputs$min_wavelet) > 0 && !is.na(inputs$min_wavelet) && nchar(inputs$max_wavelet) > 0 && !is.na(inputs$max_wavelet) && nchar(inputs$res_wavelet) > 0 && !is.na(as.numeric(inputs$res_wavelet)) && nchar(inputs$loc_wavelet) > 0 && !is.na(as.numeric(inputs$loc_wavelet)) && as.numeric(inputs$loc_wavelet) > 0 && as.numeric(inputs$loc_wavelet) <= info$rangex/2 && inputs$max_wavelet > inputs$min_wavelet) {
        min_scale <- inputs$min_wavelet*t
        max_scale <- inputs$max_wavelet*t
        num_scale <- as.integer((max_scale - min_scale)/(as.numeric(inputs$res_wavelet)*t))
        locs <- info$rangex/as.numeric(inputs$loc_wavelet)
      } else {
        min_scale <- get.min.scale(trans$x)
        max_scale <- get.max.scale(trans$x)
        num_scale <- as.integer(get.nscales(trans$x))
        res <- (max_scale - min_scale)/num_scale
        loc <- info$rangex/500
        if (loc < info$sampling) {
          loc <- info$sampling
        }
        updateTextInput(session, "min_wavelet", value = min_scale)
        updateTextInput(session, "max_wavelet", value = max_scale)
        updateTextInput(session, "res_wavelet", value = res)
        updateTextInput(session, "loc_wavelet", value = loc)
        req(info$stop)
      }
      title <- "Wavelet transform:"
      if (input$waveletType == 1) {
        title <- paste0(title," original")
        y <- isolate(trans$y - mean(trans$y))
      } else if (input$waveletType == 2 && length(isolate(trans$mod)) > 0 && length(isolate(trans$mod)) > 0) {
        title <- paste0(title," model")
        req(trans$mod)
        y <- isolate(trans$mod - mean(trans$mod))
      } else if (input$waveletType == 3 && length(isolate(trans$res)) > 0) {
        title <- paste0(title," model residuals")
        req(trans$res)
        y <- isolate(trans$res - mean(trans$res))
      } else if (input$waveletType == 4 && length(isolate(trans$filter)) > 0) {
        title <- paste0(title," filter ")
        req(trans$filter)
        y <- isolate(trans$filter - mean(trans$filter))
      } else if (input$waveletType == 5 && length(isolate(trans$filterRes)) > 0) {
        title <- paste0(title," filter residuals")
        req(trans$filterRes)
        y <- isolate(trans$filterRes - mean(trans$filterRes))
      } else {
        updateRadioButtons(session, inputId = "waveletType", label = NULL, choices = list("None" = 0, "Original" = 1, "Model" = 2, "Model res." = 3, "Filter" = 4, "Filter res." = 5), selected = 0, inline = T, choiceNames = NULL,  choiceValues = NULL)
        req(info$stop)
      }
      if (min_scale < 0.999*get.min.scale(t*trans$x) || max_scale > 1.001*get.max.scale(t*trans$x) || num_scale > as.integer(get.nscales(t*trans$x) + 1) || num_scale < 2) {
        showNotification("The period bounds and/or resolution are not valid to compute the wavelet transform. Check the input values.", action = NULL, duration = 10, closeButton = T, id = "no_wavelet", type = "error", session = getDefaultReactiveDomain())
        updateRadioButtons(session, inputId = "waveletType", label = NULL, choices = list("None" = 0, "Original" = 1, "Model" = 2, "Model res." = 3, "Filter" = 4, "Filter res." = 5), selected = 0, inline = T, choiceNames = NULL,  choiceValues = NULL)
        req(info$stop)
      }
      if (messages > 0) cat(file = stderr(), "Computing wavelet", "\n")
      start.time <- Sys.time()
      suppressWarnings({
        trans$wavelet <- mvcwt(t*trans$x, y, scale.exp = 0.5, nscales = num_scale, min.scale = min_scale, max.scale = max_scale, loc = regularize(t*trans$x, nsteps = locs), wave.fun = "Morlet")
      })
      end.time <- Sys.time()
      time.taken <- difftime(end.time, start.time, units = "secs")
      if (messages > 2) cat(file = stderr(), start.time, end.time, "Total time = ", time.taken, " s\n")
      isolate({
        levels <- suppressWarnings(signif(sd(trans$wavelet$z), 4))
        z.fun <- match.fun("Mod")
        attr(trans$wavelet, 'class') <- 'list'
        # par(mfrow = c(1, 1), mar = c(0, 0, 3, 5), oma = rep(5, 4))
        pal = colorRampPalette(rev(brewer.pal(11, 'Spectral')))(1024)
        # par(mar = rep(1, 1))
        magin_in <- par("mar")
        magin_out <- par("oma")
        par(mar = magin_in + c(0, 3, 0, 3), oma = magin_out + c(1, 0, 0, 0))
        s <- info$sampling*365.25*t/7
        amplitude_approx <- s*array(t(t(as.matrix(z.fun(trans$wavelet$z[,,1]), ncols = length(trans$wavelet$y), nrows = length(trans$wavelet$x)))*(pi/2)/sqrt(trans$wavelet$y)), dim = c(length(trans$wavelet$x),length(trans$wavelet$y),1))
        trans$wavelet$x <- trans$wavelet$x/t
        trans$wavelet$y <- trans$wavelet$y/t
        if (num_scale > 10) {
          image(trans$wavelet$x, trans$wavelet$y, amplitude_approx[,,1], col = pal, xlab = "", ylab = "", log = "y")
          image.plot(zlim = range(amplitude_approx[,,1]), legend.only = T, col = pal, legend.args = list(text = "Amplitude", cex = 1, side = 2, las = 2, line = 1), legend.shrink = 0.5, legend.width = 0.5, legend.mar = 2, horizontal = T)
        } else {
          image(trans$wavelet$x, trans$wavelet$y, z.fun(trans$wavelet$z[,,1]), col = pal, xlab = "", ylab = "")
          image.plot(zlim = range(z.fun(trans$wavelet$z[,,1])), legend.only = T, col = pal, legend.args = list(text = "Amplitude", cex = 1, side = 2, las = 2, line = 1), legend.shrink = 0.5, legend.width = 0.5, legend.mar = 2, horizontal = T)
        }
        box()
        mtext(paste0("Period (",period,")"), side = 2, line = 3, outer = F)
        contour(trans$wavelet$x, trans$wavelet$y, z.fun(trans$wavelet$z[,,1]), levels = c(levels, levels*2, levels*3), add = T, labcex = 1.1, drawlabels = F)
        title(main = title)
        coord <- unlist(as.list(which(amplitude_approx == max(amplitude_approx), arr.ind = T)))
        points(trans$wavelet$x[coord[1]], trans$wavelet$y[coord[2]], pch = "*", cex = 3, col = "black")
        lines(min(trans$wavelet$x) + trans$wavelet$y, trans$wavelet$y, lty = 2, lwd = 2, col = "darkgrey")
        lines(max(trans$wavelet$x) - trans$wavelet$y, trans$wavelet$y, lty = 2, lwd = 2, col = "darkgrey")
      })
      #
      if (isTruthy(debug)) {
        env <- environment()
        output$debug <- renderTable({
          data.frame(
            object = ls(env),
            size = unlist(lapply(ls(env), function(x) {
              format(object.size(get(x, envir = env, inherits = F)), unit = 'Mb')
            }))
          )
        })
      }
      output$wavelet1_info <- output$wavelet2_info <- output$wavelet3_info <- renderText({
        if (length(input$wavelet_1click$x) > 0) {
          idx <- which.min(abs(trans$wavelet$x - input$wavelet_1click$x))
          idy <- which.min(abs(trans$wavelet$y - input$wavelet_1click$y))
          paste0("\tEpoch = ", input$wavelet_1click$x, "\tPeriod (", period, ") = ", input$wavelet_1click$y, "\t\tAmplitude = ", amplitude_approx[idx,idy,1])  
        }
      })
    } else {
      inputs$min_wavelet <- ""
      inputs$max_wavelet <- ""
      info$run_wavelet <- T
    }
  }, width = reactive(info$width), type = "cairo-png")
  
  # Compute smoother ####
  observeEvent(c(input$sigmas, inputs$low, inputs$high, input$filter, trans$y, input$series2filter, trans$res), {
    req(trans$x, trans$sy, input$series2filter, input$filter)
    if (isTruthy(info$run_filter)) {
      if (isTruthy(input$filter)) {
        if (inputs$high == "" || is.na(inputs$high)) {
          high <- 0
        } else {
          if (nchar(inputs$high) > 0 && !is.na(inputs$high)) {
            high <- inputs$high
          } else {
            high <- 0
          }
        }
        if (inputs$low == "" || is.na(inputs$low)) {
          low <- 0
        } else {
          if (nchar(inputs$low) > 0 && !is.na(inputs$low)) {
            low <- inputs$low
          } else {
            low <- 0
          }
        }
        filter_low <- NULL
        filter_high <- NULL
        if (low != high) {
          if (input$series2filter == 1) {
            ordinate <- mean(trans$y)
            y <- trans$y - ordinate
            sy <- trans$sy
          } else if (input$series2filter == 2 && length(trans$res) > 0) {
            ordinate <- mean(trans$res)
            y <- trans$res - ordinate
            if (input$fitType == 1) {
              sy <- trans$reserror
            } else if (input$fitType == 2) {
              sy <- trans$sy
            }
          } else {
            low <- high <- 0
          }
          if (low > 0) {
            if (messages > 0) cat(file = stderr(), "Vondrak low ", low, "\n")
            filter_low <- try(vondrak(trans$x, y, sy, as.numeric(low)), silent = F)
            if (!inherits(filter_low,"try-error") && !is.null(filter_low)) {
              trans$vondrak[1] <- low
            } else {
              showNotification("Unable to smooth the series. Change the filter parameters", action = NULL, duration = 10, closeButton = T, id = "no_smooth", type = "error", session = getDefaultReactiveDomain())
            }
          } else {
            trans$vondrak[1] <- NA
          }
          if (high > 0) {
            if (messages > 0) cat(file = stderr(), "Vondrak high ", high, "\n")
            filter_high <- try(vondrak(trans$x, y, sy, as.numeric(high)), silent = F)
            if (!inherits(filter_high,"try-error") && !is.null(filter_high)) {
              trans$vondrak[2] <- high
            } else {
              showNotification("Unable to smooth the series. Change the filter parameters", action = NULL, duration = 10, closeButton = T, id = "no_smooth", type = "error", session = getDefaultReactiveDomain())
            }
          } else {
            trans$vondrak[2] <- NA
          }
          if (length(filter_low) > 0 && length(filter_high) > 0) {
            if (high < low) {
              trans$filter <- y - (filter_high - filter_low) + ordinate
            } else if (high > low) {
              trans$filter <- filter_low - filter_high + ordinate
            }
            trans$filterRes <- y - trans$filter + ordinate
          } else if (length(filter_low) > 0) {
            trans$filter <- filter_low + ordinate
            trans$filterRes <- y - trans$filter + ordinate
          } else if (length(filter_high) > 0) {
            trans$filter <- y - filter_high + ordinate
            trans$filterRes <- y - trans$filter + ordinate
          } else {
            trans$filter <- NULL
            trans$filterRes <- NULL
          }
        } else if (low == 0) {
          trans$filter <- NULL
          trans$filterRes <- NULL
        } else {
          showNotification("Low-pass and high-pass periods are equal. Unable to smooth the series. Change the smoother parameters", action = NULL, duration = 10, closeButton = T, id = "same_periods", type = "error", session = getDefaultReactiveDomain())
          trans$filter <- NULL
          trans$filterRes <- NULL
        }
      } else {
        trans$filter <- NULL
        trans$filterRes <- NULL
      }
    } else {
      info$run_filter <- T
    }
  }, priority = 1)
  
  # Plot smoother ####
  output$vondrak1 <- output$vondrak2 <- output$vondrak3 <- output$Vondrak1 <- output$Vondrak2 <- output$Vondrak3 <- renderPlot({
    req(obs(),input$filter)
    if (length(trans$filterRes) > 0) {
      if (messages > 0) cat(file = stderr(), "Plotting Vondrak", "\n")
      if ((length(inputs$low) > 0 && inputs$low > 0 && !is.na(inputs$low)) || (length(inputs$high) > 0 && inputs$high > 0 && !is.na(inputs$high))) {
        title <- "Filter residuals"
        sigmas <- F
        if (isTruthy(input$sigmas) && ((input$format == 4 && isTruthy(inputs$errorBar)) || input$format != 4)) {
          sigmas <- T
        }
        if (input$series2filter == 1) {
          plot_series(trans$x,trans$filterRes,trans$sy,ranges$x2,ranges$y2,sigmas,title,input$symbol)  
        } else if (input$series2filter == 2 && length(trans$res) > 0) {
          if (input$fitType == 1) {
            plot_series(trans$x,trans$filterRes,trans$reserror,ranges$x2,ranges$y2,sigmas,title,input$symbol)
          } else if (input$fitType == 2) {
            plot_series(trans$x,trans$filterRes,trans$sy,ranges$x2,ranges$y2,sigmas,title,input$symbol) 
          }
        }
      }
    }
  }, width = reactive(info$width), type = "cairo-png")
  
  # Noise analysis ####
  observeEvent(input$runmle, {
    if (length(trans$res) > 0 || length(trans$filterRes) > 0) {
      trans$noise <- vector(length = 11)
      if (length(trans$res) > 0) {
        series <- trans$res
      } else if (length(trans$filterRes) > 0) {
        series <- trans$filterRes
      }
      if (sd(series) < 1) {
        scaling <- 10/sd(series)
      } else {
        scaling <- 1
      }
      new_series <- series * scaling
      n <- length(new_series)
      component <- 0
      variances <- c()
      bottom <- c()
      top <- c()
      start.time <- Sys.time()
      vari <- var(new_series)
      if (input$white) {
        component <- component + 1
        if (nchar(inputs$max_white) > 0 && !is.na(as.numeric(inputs$max_white)) && as.numeric(inputs$max_white) > 0 && nchar(inputs$min_white) > 0 && !is.na(as.numeric(inputs$min_white)) && as.numeric(inputs$min_white) > 0 && as.numeric(inputs$max_white) > as.numeric(inputs$min_white)) {
          top[component] <- (as.numeric(inputs$max_white)*scaling)
          bottom[component] <- (as.numeric(inputs$min_white)*scaling)
        } else {
          updateTextInput(session, "max_white", value = (sqrt(vari)/scaling)*10)
          updateTextInput(session, "min_white", value = (sqrt(vari)/scaling)/1000)
          click("runmle")
          req(info$stop)
        }
        variances[component] <- mean(c(top[component],bottom[component]))
        Cwh <- diag(n)
      } 
      if (input$flicker) {
        component <- component + 1
        if (nchar(inputs$max_fl) > 0 && !is.na(as.numeric(inputs$max_fl)) && as.numeric(inputs$max_fl) > 0 ) {
          top[component] <- (as.numeric(inputs$max_fl)*scaling)
        } else {
          updateTextInput(session, "max_fl", value = (sqrt(vari)/scaling)*10)
          click("runmle")
          req(info$stop)
        }
        if (nchar(inputs$min_fl) > 0 && !is.na(as.numeric(inputs$min_fl)) && as.numeric(inputs$min_fl) > 0) {
          bottom[component] <- (as.numeric(inputs$min_fl)*scaling)
        } else {
          updateTextInput(session, "min_fl", value = (sqrt(vari)/scaling)/1000)
          click("runmle")
          req(info$stop)
        }
        if (as.numeric(inputs$max_fl) > as.numeric(inputs$min_fl)) {
          variances[component] <- mean(c(top[component],bottom[component]))
          Delta <- sapply(1:n, function(i) if (i < 150) {gamma(i - 1 + 0.5)/(factorial(i - 1)*gamma(0.5))} else {((i - 1)^-0.5)/gamma(0.5)})
          Z <- matrix(0,n,n)
          for (i in seq_len(n)) {
            for (j in i:n) {
              Z[j,i] <- Delta[j - i + 1]
            }
          }
          Zscaled <- sapply(1:ncol(Z), function(t,k) if (t < 2) {Z[,t]*(trans$x[t + 1] - trans$x[t])^(-k/4)} else {Z[,t]*(trans$x[t] - trans$x[t - 1])^(-k/4)}, k = -1)
          Cfl <- Zscaled %*% t(Zscaled)
        } else {
          showNotification("Max flicker noise value is equal or smaller than min flicker noise value. Change the flicker noise bounds.", action = NULL, duration = 10, closeButton = T, id = "bad_flicker", type = "error", session = getDefaultReactiveDomain())
        }
      }
      if (input$randomw) {
        component <- component + 1
        if (nchar(inputs$max_rw) > 0 && !is.na(as.numeric(inputs$max_rw)) && as.numeric(inputs$max_rw) > 0) {
          top[component] <- (as.numeric(inputs$max_rw)*scaling)
        } else {
          updateTextInput(session, "max_rw", value = (sqrt(vari)/scaling)*10)
          click("runmle")
          req(info$stop)
        }
        if (nchar(inputs$min_rw) > 0 && !is.na(as.numeric(inputs$min_rw)) && as.numeric(inputs$min_rw) > 0) {
          bottom[component] <- (as.numeric(inputs$min_rw)*scaling)
        } else {
          updateTextInput(session, "min_rw", value = (sqrt(vari)/scaling)/1000)
          click("runmle")
          req(info$stop)
        }
        if (as.numeric(inputs$max_rw) > as.numeric(inputs$min_rw)) {
          variances[component] <- mean(c(top[component],bottom[component]))
          Z <- lower.tri(matrix(1, n, n), diag = T)*1
          Zscaled <- sapply(1:ncol(Z), function(t,k) if (t < 2) {Z[,t]} else {Z[,t]*(trans$x[t] - trans$x[t - 1])^(-k/4)}, k = -2)
          Zscaled[,1] <- Zscaled[,2]
          Zscaled[1,1] <- Zscaled[2,2]
          Crw <- Zscaled %*% t(Zscaled)
        } else {
          showNotification("Max random-walk noise value is equal or smaller than min random-walk noise value. Change the randow-walk noise bounds.", action = NULL, duration = 10, closeButton = T, id = "bad_rw", type = "error", session = getDefaultReactiveDomain())
        }
      }
      if (input$powerl) {
        component <- component + 1
        if (nchar(inputs$max_pl) > 0 && !is.na(as.numeric(inputs$max_pl)) && as.numeric(inputs$max_pl) > 0) {
          top[component] <- (as.numeric(inputs$max_pl)*scaling)
        } else {
          updateTextInput(session, "max_pl", value = (sqrt(vari)/scaling)*10)
          click("runmle")
          req(info$stop)
        }
        if (nchar(inputs$min_pl) > 0 && !is.na(as.numeric(inputs$min_pl)) && as.numeric(inputs$min_pl) > 0) {
          bottom[component] <- (as.numeric(inputs$min_pl)*scaling)
        } else {
          updateTextInput(session, "min_pl", value = (sqrt(vari)/scaling)/1000)
          click("runmle")
          req(info$stop)
        }
        if (as.numeric(inputs$max_pl) > as.numeric(inputs$min_pl)) {
          variances[component] <- mean(c(top[component],bottom[component]))
          component <- component + 1
          if (nchar(inputs$max_k) > 0 && !is.na(as.numeric(inputs$max_k)) && as.numeric(inputs$max_k) < 0 && nchar(inputs$min_k) > 0 && !is.na(as.numeric(inputs$min_k)) && as.numeric(inputs$min_k) < 0 && as.numeric(inputs$max_k) > as.numeric(inputs$min_k)) {
            top[component] <- -1*as.numeric(inputs$min_k) + 1
            bottom[component] <- -1*as.numeric(inputs$max_k) + 1
          } else {
            updateTextInput(session, "max_k", value = -0.01)
            updateTextInput(session, "min_k", value = -3)
            click("runmle")
            req(info$stop)
          }
          variances[component] <- 2
        } else {
          showNotification("Max power-law noise value is equal or smaller than min power-law noise value. Change the power-law noise bounds.", action = NULL, duration = 10, closeButton = T, id = "bad_pl", type = "error", session = getDefaultReactiveDomain())
        }
      }
      loglik <- function(x) {
        h <- 0
        C <- matrix(0,n,n)
        if (input$white) {
          h <- h + 1
          C <- C + exp(x[h])^2*Cwh
        }
        if (input$flicker) {
          h <- h + 1
          C <- C + exp(x[h])^2*Cfl
        }
        if (input$randomw) {
          h <- h + 1
          C <- C + exp(x[h])^2*Crw
        }
        if (input$powerl) {
          h <- h + 1
          pl <- exp(x[h])^2
          h <- h + 1
          k <- 1 - exp(x[h])
          Delta <- sapply(1:n, function(i) if (i < 150) {gamma(i - 1 - (k/2))/(factorial(i - 1)*gamma(-k/2))} else {((i - 1)^((-k/2) - 1))/gamma(-k/2)})
          Z <- matrix(0,n,n)
          for (i in seq_len(n)) {
            for (j in i:n) {
              Z[j,i] <- Delta[j - i + 1]
            }
          }
          Zscaled <- sapply(1:ncol(Z), function(t,k) if (t < 2) {Z[,t]*(trans$x[t + 1] - trans$x[t])^(-k/4)} else {Z[,t]*(trans$x[t] - trans$x[t - 1])^(-k/4)}, k = k)
          Cpl <- Zscaled %*% t(Zscaled)
          C <- C + pl*Cpl
        } else {
          k <- 0
        }
        y <- new_series - mean(new_series)
        ll <- -0.5*(length(n)*log(2*pi) + determinant(C)$modulus[[1]] + ( crossprod(y,solve(C)) %*% y) )
        if (messages > 2) cat(file = stderr(), "Std Dev noises = ", exp(x)/scaling, " Index = ", k, " loglik = ", sprintf("%f",ll), "\n")
        ll
      }
      if (messages > 0) cat(file = stderr(), "MLE fit start", "\n")
      start.time <- Sys.time()
      if (component > 0) {
        if (messages > 2) print(paste0("Inicio optimizacion ",Sys.time()))
        withBusyIndicatorServer("runmle", {
          modmle <- optim(par = log(variances),
                          fn = loglik,
                          lower = log(bottom),
                          upper = log(top),
                          method = "L-BFGS-B",
                          hessian = T,
                          control = c(fnscale = -1, factr = 1e11)
          )
          if (messages > 2) print(paste0("Fin optimizacion ",Sys.time()))
          end.time <- Sys.time()
          time.taken <- end.time - start.time
          if (messages > 2) cat(file = stderr(), start.time, end.time, "Total time = ", time.taken, " s\n")
          if (modmle$convergence == 0) {
            trans$mle <- 1
            sd_noises <- sqrt(diag(solve(-modmle$hessian)))
            zeroes <- NULL
            i <- 0
            sigmaFL <- NULL
            sigmaPL <- NULL
            sigmaRW <- NULL
            sigmaK <- NULL
            if (input$white) {
              i <- i + 1
              if (i %in% zeroes) {
                sigmaWH <- NA
                seParmsWH <- NA
              } else {
                sigmaWH <- exp(modmle$par[i])/scaling
                seParmsWH <- sd_noises[i]*sd(series)
              }
              trans$noise[1] <- sigmaWH
              trans$noise[2] <- seParmsWH
              output$est.white <- renderUI({
                if (isTruthy(trans$mle)) {
                  line1 <- "White noise:"
                  line2 <- sprintf("%f",sigmaWH)
                  line3 <- sprintf("+/- %f",seParmsWH)
                  HTML(paste(line1,'<br/>',line2,'<br/>',line3))
                } else {
                  NULL
                }
              })
            } else {
              trans$noise[1] <- NA
              trans$noise[2] <- NA
            }
            if (input$flicker) {
              i <- i + 1
              if (i %in% zeroes) {
                sigmaFL <- NA
                seParmsFL <- NA
              } else {
                sigmaFL <- exp(modmle$par[i])/scaling
                seParmsFL <- sd_noises[i]*sd(series)
              }
              trans$noise[3] <- sigmaFL
              trans$noise[4] <- seParmsFL
              output$est.flicker <- renderUI({
                if (isTruthy(trans$mle)) {
                  line1 <- "Flicker noise:"
                  line2 <- sprintf("%f",sigmaFL)
                  line3 <- sprintf("+/- %f",seParmsFL)
                  HTML(paste(line1,'<br/>',line2,'<br/>',line3))
                } else {
                  NULL
                }
              })
            } else {
              trans$noise[3] <- NA
              trans$noise[4] <- NA
            }
            if (input$randomw) {
              i <- i + 1
              if (i %in% zeroes) {
                sigmaRW <- NA
                seParmsRW <- NA
              } else {
                sigmaRW <- exp(modmle$par[i])/scaling
                seParmsRW <- sd_noises[i]*sd(series)
              }
              trans$noise[5] <- sigmaRW
              trans$noise[6] <- seParmsRW
              output$est.randomw <- renderUI({
                if (isTruthy(trans$mle)) {
                  line1 <- "Random walk:"
                  line2 <- sprintf("%f",sigmaRW)
                  line3 <- sprintf("+/- %f",seParmsRW)
                  HTML(paste(line1,'<br/>',line2,'<br/>',line3))
                } else {
                  NULL
                }
              })
            } else {
              trans$noise[5] <- NA
              trans$noise[6] <- NA
            }
            if (input$powerl) {
              i <- i + 1
              if (i %in% zeroes) {
                sigmaPL <- NA
                seParmsPL <- NA
              } else {
                sigmaPL <- exp(modmle$par[i])/scaling
                seParmsPL <- sd_noises[i]*sd(series)
              }
              trans$noise[7] <- sigmaPL
              trans$noise[8] <- seParmsPL
              output$est.powerl <- renderUI({
                if (isTruthy(trans$mle)) {
                  line1 <- "Power-law:"
                  line2 <- sprintf("%f",sigmaPL)
                  line3 <- sprintf("+/- %f",seParmsPL)
                  HTML(paste(line1,'<br/>',line2,'<br/>',line3))
                } else {
                  NULL
                }
              })
              i <- i + 1
              if (i %in% zeroes) {
                sigmaK <- NA
                seParmsK <- NA
              } else {
                sigmaK <- 1 - exp(modmle$par[i])
                seParmsK <- sd_noises[i]
              }
              trans$noise[9] <- sigmaK
              trans$noise[10] <- seParmsK
              output$est.index <- renderUI({
                if (isTruthy(trans$mle)) {
                  line1 <- "Spectral index:"
                  line2 <- sprintf("%f",sigmaK)
                  line3 <- sprintf("+/- %f",seParmsK)
                  HTML(paste(line1,'<br/>',line2,'<br/>',line3))
                } else {
                  NULL
                }
              })
            } else {
              trans$noise[7] <- NA
              trans$noise[8] <- NA
              trans$noise[9] <- NA
              trans$noise[10] <- NA
            }
            if (input$white || input$flicker || input$randomw || input$powerl) {
              if (length(modmle$value) > 0) {
                trans$noise[11] <- modmle$value
              } else {
                trans$noise[11] <- NA
              }
              output$est.mle <- renderUI({
                if (isTruthy(trans$mle)) {
                  line1 <- sprintf("<br/>log-Likelihood = %.4f",modmle$value)
                  HTML(line1)
                } else {
                  NULL
                }
              })
            }
            output$est.unc <- renderUI({
              if ("Linear" %in% input$model && input$fitType == 1) { 
                if (isTruthy(trans$mle)) {
                  num <- info$points
                  if (isTruthy(sigmaFL)) {
                    noise_amp <- sigmaFL
                    index <- -1
                  } else if (isTruthy(sigmaRW)) {
                    noise_amp <- sigmaRW
                    index <- -2
                  } else if (isTruthy(sigmaPL)) {
                    noise_amp <- sigmaPL
                    index <- sigmaK
                  } else {
                    noise_amp <- 0
                    index <- 0
                  }
                  noise_amp <- noise_amp * scaling
                  v <- -0.0237*index^9 - 0.3881*index^8 - 2.6610*index^7 - 9.8529*index^6 - 21.0922*index^5 - 25.1638*index^4 - 11.4275*index^3 + 10.7839*index^2 + 20.3377*index^1 + 11.9942*index^0
                  beta <- (-1*index)/2 - 2
                  if (index < -1.3) {
                    gamma <- -3 - index
                  } else {
                    gamma <- -3 + (-1*index) + 7.7435*exp(-10)*index^17 - (0.0144/(0.27*sqrt(2*pi)))*exp(-0.5*((index + 1.025)/0.27)^2)
                  }
                  unc <- sqrt( noise_amp^2 * v * info$sampling^beta * num^gamma ) / scaling
                  unc_white <- sqrt( 12 * sd(trans$res)^2 * (num - 1) / (num * info$rangex^2 * (num + 1)) )
                  if (isTruthy(trans$unc)) {
                    if (isTruthy(unc) && unc > 0) {
                      trans$LScoefs[2,2] <- sqrt(unc^2 + trans$unc^2)
                      trans$results$coefficients[2,2] <- sqrt(unc^2 + trans$unc^2)
                    } else {
                      unc <- trans$unc
                      trans$LScoefs[2,2] <- trans$unc
                    }
                    line1 <- sprintf("<br/>Colored/white rate error ratio = %.4f",unc/unc_white)
                    HTML(line1) 
                  } else {
                    NULL
                  }
                } else {
                  if (isTruthy(trans$unc)) {
                    if (isTruthy(trans$LScoefs[2,2])) {
                      trans$LScoefs[2,2] <- trans$unc
                    }
                    if (isTruthy(trans$results$coefficients[2,2])) {
                      trans$results$coefficients[2,2] <- trans$unc
                    }
                  }
                  NULL
                }
              }
            })
          } else {
            trans$mle <- NULL
            showNotification("MLE optimization did not converge. The model parameters are probably out of bounds.", action = NULL, duration = 10, closeButton = T, id = "no_mle", type = "error", session = getDefaultReactiveDomain())
          }
          if (messages > 0) cat(file = stderr(), "MLE fit end", "\n")
        })
      }
    }
  })
  
  # Search offsets ####
  observeEvent(input$search, {
    req(file$primary)
    if (length(trans$res) > 0) {
      if (messages > 0) cat(file = stderr(), "Searching offsets", "\n")
      output$offsetFound <- renderUI({
        NULL
      })
      if (length(trans$x) < 40) {
        piece <- length(trans$x)/2 - 1
      } else {
        piece <- as.integer(length(trans$x)/20)
      }
      if (length(trans$x) < 20) {
        extension <- 2
      } else {
        extension <- as.integer(length(trans$x)/10)
      }
      segment <- input$segmentLength/100
      if (segment*length(trans$x) < 3) {
        showNotification("The segment size is too small for the series sampling. Consider using a larger segment size.", action = NULL, duration = 10, closeButton = T, id = "bad_search", type = "error", session = getDefaultReactiveDomain())
        req(info$stop)
      }
      lag <- 1
      withBusyIndicatorServer("search", {
        center_ini <- mean(trans$res[1:piece])
        disp_ini <- sd(trans$res[1:piece])/2
        center_end <- mean(trans$res[(length(trans$res) - piece):length(trans$res)])
        disp_end <- sd(trans$res[(length(trans$res) - piece):length(trans$res)])/2
        extended_series <- ts(c(rnorm(extension,center_ini,disp_ini),trans$res, rnorm(extension,center_end,disp_end)))
        extended_series <- cbind(extended_series, lag(extended_series, k = -lag))
        colnames(extended_series) <- c("y", "ylag1")
        breaks <- breakpoints(y ~ ylag1, data = extended_series, h = segment)
        output$offsetFound <- renderUI({
          if (isTruthy(breaks$breakpoints)) {
            line <- paste(0.5*info$sampling + trans$x[(breaks$breakpoints + 1 - extension)[which((breaks$breakpoints + 1 - extension) > 0 & (breaks$breakpoints + 1 - extension) < length(trans$x))]],collapse = ", ")
          } else {
            line <- "None found"
          }
          HTML(line)
        })
      })
    } else {
      showNotification("Finding discontinuities is only possible from detrended series or, more generally, residual series.", action = NULL, duration = 10, closeButton = T, id = "no_search", type = "error", session = getDefaultReactiveDomain())
    }
  })
  
  # Download results ####
  output$localDirectory <- renderPrint({
    if (isTruthy(info$directory)) {
      cat(info$directory)
    }
  })
  observeEvent(input$autoDownload, {
    withBusyIndicatorServer("autoDownload", { 
      if (isTruthy(info$directory)) {
        if (input$format != 4) {
          file_out <- paste0(info$directory, "\\", file$primary$name, "_", input$tab, ".sari")
        } else {
          file_out <- paste0(info$directory, "\\", file$primary$name, ".sari")
        }
        collect(file_out)
      } else {
        showNotification("Download directory not found. File download skipped.", action = NULL, duration = 10, closeButton = T, id = "no_directory", type = "error", session = getDefaultReactiveDomain())
      }
    })
  })
  output$download <- output$downloadAs <- downloadHandler(
    filename = function() {
      if (input$format != 4) {
        paste0(file$primary$name, "_", input$tab, ".sari")
      } else {
        paste0(file$primary$name, ".sari")
      }
    },
    content = function(file) {
      collect(file)
    }
  )
  output$downloadSpectrum1 <- output$downloadSpectrum2 <- output$downloadSpectrum3 <- downloadHandler(
    filename = function() {
      if (input$format != 4) {
        paste0(file$primary$name, "_", input$tab, ".periodogram.sari")
      } else {
        paste0(file$primary$name, ".periodogram.sari")
      }
    },
    content = function(file) {
      collect_periodogram(file)
    }
  )
  #Based on https://stackoverflow.com/questions/40420450/how-to-download-a-pdf-file-in-a-shiny-app
  output$print_out <- downloadHandler(
    filename = paste0(gsub(" ", "_", version),".pdf"),
    content = function(file) {
      file.copy("www/about.pdf", file)
    }
  )
  
  # Control plots ####
  output$header <- renderText({
    req(input$header, file$primary)
    noquote(paste(readLines(con = file$primary$datapath, n = input$lines, ok = T, warn = T, skipNul = F, encoding = "UTF8"), collapse = "\n"))
  })
  observeEvent(input$remove3D, {
    req(file$primary)
    if (messages > 0) cat(file = stderr(), "Removing from all series is", input$remove3D, "\n")
    values$used1 <- values$used_all
    values$used2 <- values$used_all
    values$used3 <- values$used_all
    values$excluded1 <- values$excluded_all
    values$excluded2 <- values$excluded_all
    values$excluded3 <- values$excluded_all
  })
  observeEvent(input$plot_2click, {
    req(file$primary)
    brush <- input$plot_brush
    if (!is.null(brush)) {
      ranges$x1 <- c(brush$xmin, brush$xmax)
      ranges$y1 <- c(brush$ymin, brush$ymax)
      if (length(file$secondary) > 1 && input$optionSecondary == 1 && any(!is.na(trans$z))) {
        ids <- trans$x0 >= ranges$x1[1] & trans$x0 <= ranges$x1[2]
        if (sum(ids) > 0) {
          ranges$y12 <- range(trans$z[ids], na.rm = T)
          if (any(is.na(ranges$y12)) || any(is.infinite(ranges$y12))) {
            ranges$y12 <- range(trans$z, na.rm = T)
          }
        } else {
          ranges$y12 <- range(trans$z, na.rm = T)
        }
      }
      ranges$x2 <- ranges$x4 <- ranges$x1
      ranges$y2 <- NULL
    } else {
      ranges$x1 <- c(info$minx, info$maxx)
      ranges$y1 <- c(info$miny, info$maxy)
      ranges$y12 <- NULL
      ranges$x2 <- NULL
      ranges$y2 <- NULL
      ranges$x4 <- NULL
      ranges$y4 <- NULL
    }
  })
  observeEvent(input$res_2click, {
    req(file$primary)
    brush <- NULL
    if (isTruthy(input$res_brush)) {
      brush <- input$res_brush
    } else if (isTruthy(input$vondrak_brush)) {
      brush <- input$vondrak_brush
    }
    if (!is.null(brush)) {
      ranges$x2 <- c(brush$xmin, brush$xmax)
      ranges$y2 <- c(brush$ymin, brush$ymax)
      ranges$x1 <- ranges$x4 <- ranges$x2
      ids <- trans$x > ranges$x1[1] & trans$x < ranges$x1[2]
      ranges$y1 <- range(trans$y[ids])
      if (length(file$secondary) > 1 && input$optionSecondary == 1 && any(!is.na(trans$z))) {
        ids <- trans$x0 >= ranges$x1[1] & trans$x0 <= ranges$x1[2]
        if (sum(ids) > 0) {
          ranges$y12 <- range(trans$z[ids], na.rm = T)
          if (any(is.na(ranges$y12)) || any(is.infinite(ranges$y12))) {
            ranges$y12 <- range(trans$z, na.rm = T)
          }
        } else {
          ranges$y12 <- range(trans$z, na.rm = T)
        }
      }
    } else {
      ranges$x4 <- NULL
      ranges$y4 <- NULL
      ranges$x2 <- NULL
      ranges$y2 <- NULL
      ranges$x1 <- c(info$minx, info$maxx)
      ranges$y1 <- c(info$miny, info$maxy)
      ranges$y12 <- NULL
    }
  })
  observeEvent(input$lomb_2click, {
    req(file$primary)
    brush <- input$lomb_brush
    if (!is.null(brush)) {
      ranges$x3 <- c(brush$xmin, brush$xmax)
      ranges$y3 <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x3 <- NULL
      ranges$y3 <- NULL
    }
  })
  observeEvent(input$rate_2click, {
    req(file$primary)
    brush <- input$rate_brush
    if (!is.null(brush)) {
      ranges$x4 <- c(brush$xmin, brush$xmax)
      ranges$y4 <- c(brush$ymin, brush$ymax)
      ranges$x1 <- ranges$x2 <- ranges$x4
      ids <- trans$x > ranges$x1[1] & trans$x < ranges$x1[2]
      ranges$y1 <- range(trans$y[ids])
      if (length(file$secondary) > 1 && input$optionSecondary == 1 && any(!is.na(trans$z))) {
        ids <- trans$x0 >= ranges$x1[1] & trans$x0 <= ranges$x1[2]
        if (sum(ids) > 0) {
          ranges$y12 <- range(trans$z[ids], na.rm = T)
          if (any(is.na(ranges$y12)) || any(is.infinite(ranges$y12))) {
            ranges$y12 <- range(trans$z, na.rm = T)
          }
        } else {
          ranges$y12 <- range(trans$z, na.rm = T)
        }
      }
    } else {
      ranges$x4 <- NULL
      ranges$y4 <- NULL
      ranges$x2 <- NULL
      ranges$y2 <- NULL
      ranges$x1 <- c(info$minx, info$maxx)
      ranges$y1 <- c(info$miny, info$maxy)
      ranges$y12 <- NULL
    }
  })
  
  # Enable/disable options ####
  observe({
    if (input$tab == "4") {
      disable("fitType")
      disable("autoDownload")
      disable("white")
      disable("reset")
      disable("delete_excluded")
      disable("loadSARI")
      disable("correct_waveform")
      disable("custom")
      disable("downloadAs")
      disable("printCustom")
      disable("printLog")
      disable("printSinfo")
      disable("printSoln")
      disable("spectrum")
      disable("spectrumFilter")
      disable("spectrumFilterRes")
      disable("spectrumModel")
      disable("spectrumOriginal")
      disable("periodogram_residuals")
      disable("spectrumType")
      disable("est.mle")
      disable("remove")
      disable("remove3D")
      disable("removeAuto")
      disable("series")
      disable("filter")
      disable("flicker")
      disable("format")
      disable("format2")
      disable("histogram")
      disable("histogramType")
      disable("log")
      disable("average")
      disable("midas")
      disable("mle")
      disable("model")
      disable("wavelet")
      disable("waveletType")
      disable("optionSecondary")
      disable("waveform")
      disable("plot")
      disable("powerl")
      disable("randomw")
      disable("runVerif")
      disable("runmle")
      disable("series2")
      disable("separator")
      disable("separator2")
      disable("series2filter")
      disable("typeSecondary")
      disable("sigmas")
      disable("symbol")
      disable("sinfo")
      disable("soln")
      disable("header")
      disable("traceCustom")
      disable("traceLog")
      disable("traceSinfo")
      disable("traceSoln")
      disable("thresholdRes")
      disable("thresholdResN")
      disable("units")
      disable("verif_offsets")
      disable("euler")
    } else {
      if (length(file$primary) > 0) {
        enable("symbol")
        enable("header")
        enable("separator")
        enable("separator2")
        enable("format")
        if (input$format == 4) {
          updateRadioButtons(session, inputId = "format2", label = NULL, choices = list("NEU/ENU" = 1, "PBO" = 2, "NGL" = 3, "1D" = 4), selected = 4, inline = T)
          shinyjs::delay(100, disable("format2"))
        } else {
          enable("format2")
          enable("euler")
        }
        enable("units")
        enable("log")
        enable("sinfo")
        enable("soln")
        enable("custom")
        enable("printLog")
        enable("printSinfo")
        enable("printSoln")
        enable("printCustom")
        enable("series2")
        enable("optionSecondary")
        if (input$optionSecondary == 1) {
          if (isTruthy(input$sameScale)) {
            disable("same_axis")
          } else {
            enable("same_axis")
          }
          if (isTruthy(input$same_axis)) {
            disable("sameScale")
          } else {
            enable("sameScale")
          }
        } else {
          disable("sameScale")
          disable("same_axis")
        }
        if (!isTruthy(input$euler)) {
          updateRadioButtons(session, inputId = "eulerType", label = NULL, choices = list("None" = 0, "Show" = 1, "Remove" = 2), selected = 0, inline = T)
        }
        enable("plot")
        if (isTruthy(info$errorbars)) {
          enable("sigmas") 
        } else {
          updateCheckboxInput(session, inputId = "sigmas", value = F)
          disable("sigmas")
        }
        if (input$sigmas == T) {
          enable("errorBar")
          enable("errorBar2")
        } else {
          disable("errorBar")
          disable("errorBar2")
        }
        if (!isTruthy(input$average) && length(inputs$step) > 0) {
          updateTextInput(session, inputId = "step", value = "")
        }
        if (isTruthy(obs())) {
          disable("plot")
          disable("series")
          disable("format")
          enable("average")
          enable("loadSARI")
          enable("midas")
          enable("reset")
          if (length(input$plot_brush) > 0 || length(input$res_brush) > 0 || length(input$vondrak_brush) > 0) {
            enable("remove")  
          } else {
            disable("remove")
          }
          if (isTruthy(trans$xe)) {
            enable("delete_excluded")
          } else {
            disable("delete_excluded")
          }
          enable("traceLog")
          enable("traceSinfo")
          enable("traceSoln")
          enable("traceCustom")
          enable("histogram")
          enable("histogramType")
          enable("filter")
          enable("series2filter")
          enable("spectrum")
          enable("spectrumType")
          enable("spectrumOriginal")
          if (!isTruthy(input$spectrum)) {
            updateCheckboxInput(session, inputId = "spectrumOriginal", value = F)
            updateCheckboxInput(session, inputId = "periodogram_residuals", value = F)
            updateCheckboxInput(session, inputId = "spectrumModel", value = F)
            updateCheckboxInput(session, inputId = "spectrumFilter", value = F)
            updateCheckboxInput(session, inputId = "spectrumFilterRes", value = F)
            updateTextInput(session, inputId = "ofac", value = "")
            updateTextInput(session, inputId = "long_period", value = "")
            updateTextInput(session, inputId = "short_period", value = "")
          }
          enable("wavelet")
          enable("waveletType")
          if (sum(values$excluded1) == sum(values$excluded2) && sum(values$excluded1) == sum(values$excluded3)) {
            enable("remove3D")
          } else {
            disable("remove3D")
          }
          enable("fitType")
          enable("model")
          if (input$fitType == 1 || input$fitType == 2) {
            if (length(trans$mod) > 0 && length(trans$res) > 0) {
              enable("spectrumModel")
              enable("periodogram_residuals")
              shinyjs::show(id = "res", anim = T, animType = "fade", time = 0.5, selector = NULL)
              shinyjs::show(id = "res1", anim = T, animType = "fade", time = 0.5, selector = NULL)
              shinyjs::show(id = "res2", anim = T, animType = "fade", time = 0.5, selector = NULL)
              shinyjs::show(id = "res3", anim = T, animType = "fade", time = 0.5, selector = NULL)
              if (length(trans$offsetEpochs) > 0) {
                enable("verif_offsets")
                if (isTRUE(input$verif_offsets)) {
                  if (isTruthy(trans$mle)) {
                    if (nchar(inputs$verif_white) == 0 || nchar(inputs$verif_pl) == 0 || nchar(input$verif_k) == 0) {
                      if (isTruthy(trans$noise[1]) && isTruthy(trans$noise[2])) {
                        if (trans$noise[1] > trans$noise[2] * 3) {
                          updateTextInput(session, inputId = "verif_white", value = trans$noise[1])
                        } else {
                          updateTextInput(session, inputId = "verif_white", value = "0")
                        }
                      } else {
                        updateTextInput(session, inputId = "verif_white", value = "0")
                      }
                      if (isTruthy(trans$noise[3]) || isTruthy(trans$noise[5]) || isTruthy(trans$noise[7])) {
                        if (isTruthy(trans$noise[3]) && isTruthy(trans$noise[4])) {
                          if (trans$noise[3] > trans$noise[4] * 3) {
                            updateTextInput(session, inputId = "verif_pl", value = trans$noise[3])
                          } else {
                            updateTextInput(session, inputId = "verif_pl", value = "0")
                          }
                          updateTextInput(session, inputId = "verif_k", value = "-1")
                        }
                        if (isTruthy(trans$noise[5]) && isTruthy(trans$noise[6])) {
                          if (trans$noise[5] > trans$noise[6] * 3) {
                            updateTextInput(session, inputId = "verif_pl", value = trans$noise[5])
                          } else {
                            updateTextInput(session, inputId = "verif_pl", value = "0")
                          }
                          updateTextInput(session, inputId = "verif_k", value = "-2")
                        }
                        if (isTruthy(trans$noise[7]) && isTruthy(trans$noise[8]) && isTruthy(trans$noise[9]) && isTruthy(trans$noise[10])) {
                          if (trans$noise[7] > trans$noise[8] * 3) {
                            updateTextInput(session, inputId = "verif_pl", value = trans$noise[7])
                          } else {
                            updateTextInput(session, inputId = "verif_pl", value = "0")
                          }
                          if (abs(trans$noise[9]) > trans$noise[10] * 3) {
                            updateTextInput(session, inputId = "verif_k", value = trans$noise[9])
                          } else {
                            updateTextInput(session, inputId = "verif_k", value = "0")
                          }
                        }
                      } else {
                        updateTextInput(session, inputId = "verif_pl", value = "0")
                        updateTextInput(session, inputId = "verif_k", value = "0")
                      }
                    }
                  }
                  if ( (nchar(inputs$verif_white) > 0 && !is.na(as.numeric(inputs$verif_white)) && as.numeric(inputs$verif_white) > 0) || (nchar(input$verif_pl) > 0 && nchar(input$verif_k) > 0 && !is.na(as.numeric(inputs$verif_pl)) && !is.na(as.numeric(inputs$verif_k)) && as.numeric(inputs$verif_pl) > 0 && as.numeric(inputs$verif_k) <= 0) ) {
                    enable("runVerif")
                  } else {
                    disable("runVerif")
                  }
                }
              } else {
                updateCheckboxInput(session, inputId = "verif_offsets", label = NULL, value = F)
                updateTextInput(session, inputId = "verif_white", value = "")
                updateTextInput(session, inputId = "verif_pl", value = "")
                updateTextInput(session, inputId = "verif_k", value = "")
                disable("verif_offsets")
                disable("runVerif")
              }
            } else {
              disable("correct_waveform")
              shinyjs::hide(id = "res", anim = T, animType = "fade", time = 0.5, selector = NULL)
              shinyjs::hide(id = "res1", anim = T, animType = "fade", time = 0.5, selector = NULL)
              shinyjs::hide(id = "res2", anim = T, animType = "fade", time = 0.5, selector = NULL)
              shinyjs::hide(id = "res3", anim = T, animType = "fade", time = 0.5, selector = NULL)
              disable("mle")
              disable("spectrumModel")
              disable("periodogram_residuals")
            }
          } else {
            updateCheckboxGroupInput(session, inputId = "model", label = "", choices = list("Linear","Polynomial","Sinusoidal","Offset","Exponential","Logarithmic"), selected = NULL, inline = T)
            shinyjs::hide(id = "res", anim = T, animType = "fade", time = 0.5, selector = NULL)
            shinyjs::hide(id = "res1", anim = T, animType = "fade", time = 0.5, selector = NULL)
            shinyjs::hide(id = "res2", anim = T, animType = "fade", time = 0.5, selector = NULL)
            shinyjs::hide(id = "res3", anim = T, animType = "fade", time = 0.5, selector = NULL)
            disable("mle")
            info$run <- F
          }
          if (input$series2filter == 2 && input$fitType == 0) {
            updateRadioButtons(session, inputId = "series2filter", label = NULL, choices = list("Original" = 1, "Residual" = 2), selected = 1, inline = T)
          }
          if ((length(trans$mod) > 0 && length(trans$res) > 0) || (length(trans$filter) > 0)) {
            enable("mle")
            enable("waveform")
            if (isTruthy(input$waveform)) {
              if (isTruthy(input$correct_waveform) || length(trans$pattern) > 0 && length(trans$res) > 0) {
                enable("correct_waveform")
              } else {
                disable("correct_waveform")
              }
            } else {
              updateTextInput(session, inputId = "waveformPeriod", value = "")
              updateCheckboxInput(session, inputId = "correct_waveform", label = NULL, value = F)
            }
            if (isTruthy(input$mle)) {
              enable("white")
              enable("flicker")
              enable("randomw")
              enable("powerl")
              disable("runmle")
              enable("est.mle")
              if (input$flicker || input$randomw || input$white || input$powerl) {
                enable("runmle")
              } else {
                disable("runmle")
              }
              if (input$powerl) {
                updateCheckboxInput(session, inputId = "flicker", label = NULL, value = F)
                disable("flicker")
                disable("randomw")
              } else {
                enable("flicker")
                enable("randomw")
              }
              if (input$flicker || input$randomw) {
                disable("powerl")
              } else {
                enable("powerl")
              }
            } else {
              disable("white")
              disable("flicker")
              disable("randomw")
              disable("powerl")
              disable("runmle")
              disable("est.mle")
              trans$mle <- F
              updateTextInput(session, "max_white", value = "")
              updateTextInput(session, "min_white", value = "")
              updateTextInput(session, "max_flpl", value = "")
              updateTextInput(session, "min_flpl", value = "")
              updateTextInput(session, "max_rwk", value = "")
              updateTextInput(session, "min_rwk", value = "")
              updateCheckboxInput(session, inputId = "white", label = NULL, value = F)
              updateCheckboxInput(session, inputId = "flicker", label = NULL, value = F)
              updateCheckboxInput(session, inputId = "randomw", label = NULL, value = F)
              updateCheckboxInput(session, inputId = "powerl", label = NULL, value = F)
              trans$noise <- NULL
            }
            enable("downloadAs")
            if (isTruthy(info$directory)) {
              enable("autoDownload")
            } else {
              disable("autoDownload")
            }
          } else {
            disable("waveform")
            updateCheckboxInput(session, inputId = "waveform", label = NULL, value = F)
            disable("correct_waveform")
            disable("runmle")
            disable("downloadAs")
            disable("autoDownload")
          }
          if ((length(trans$mod) > 0 && length(trans$res) > 0) || (input$fitType == 0 && length(trans$filter) > 0)) {
            if (length(input$plot_brush) > 0 || length(input$res_brush) > 0 || length(input$vondrak_brush) > 0) {
              enable("remove")
            } else {
              disable("remove")
            }
            if (isTruthy(trans$xe)) {
              enable("delete_excluded")
            } else {
              disable("delete_excluded")
            }
            enable("thresholdRes")
            if (length(trans$mod) > 0 && length(trans$res) > 0) {
              if (input$sigmas == T) {
                enable("thresholdResN")
              } else {
                disable("thresholdResN")
              }
            } else {
              disable("thresholdResN")
            }
          } else if (length(trans$filter) > 0) {
            enable("thresholdRes")
            disable("thresholdResN")
          } else {
            disable("thresholdRes")
            disable("thresholdResN")
          }
          if (!is.na(inputs$thresholdRes) || !is.na(inputs$thresholdResN)) {
            enable("removeAuto")
          } else {
            disable("removeAuto")
          }
          if (!isTruthy(input$wavelet)) {
            updateTextInput(session, "corto_wavelet", value = "")
            updateTextInput(session, "largo_wavelet", value = "")
            updateRadioButtons(session, inputId = "waveletType", label = NULL, list("None" = 0, "Original" = 1, "Model" = 2, "Model res." = 3, "Filter" = 4, "Filter res." = 5), selected = 0, inline = T, choiceNames = NULL, choiceValues = NULL)
            shinyjs::delay(100, disable("waveletType"))
          }
        } else {
          updateRadioButtons(session, inputId = "fitType", label = NULL, list("None" = 0, "LS" = 1, "KF" = 2), selected = 0, inline = T, choiceNames = NULL, choiceValues = NULL)
          shinyjs::delay(100, disable("fitType"))
          disable("traceLog")
          disable("traceSinfo")
          disable("traceSoln")
          disable("traceCustom")
          disable("filter")
          disable("spectrum")
          disable("wavelet")
          updateRadioButtons(session, inputId = "waveletType", label = NULL, choices = list("None" = 0, "Original" = 1, "Model" = 2, "Model res." = 3, "Filter" = 4, "Filter res." = 5), selected = 0, inline = T, choiceNames = NULL,  choiceValues = NULL)
          shinyjs::delay(100, disable("waveletType"))
          updateTextInput(session, "corto_wavelet", value = "")
          updateTextInput(session, "largo_wavelet", value = "")
          disable("midas")
          disable("spectrumModel")
          disable("periodogram_residuals")
          disable("spectrumFilter")
          disable("spectrumFilterRes")
          enable("plot")
          enable("series")
          enable("format")
          disable("average")
        }
      } else {
        hideTab(inputId = "tab", target = "5", session = getDefaultReactiveDomain())
        updateRadioButtons(session, inputId = "waveletType", label = NULL, choices = list("None" = 0, "Original" = 1, "Model" = 2, "Model res." = 3, "Filter" = 4, "Filter res." = 5), selected = 0, inline = T, choiceNames = NULL,  choiceValues = NULL)
        shinyjs::delay(100, disable("waveletType"))
        updateTextInput(session, "corto_wavelet", value = "")
        updateTextInput(session, "largo_wavelet", value = "")
        enable("series")
        disable("fitType")
        disable("autoDownload")
        disable("white")
        disable("reset")
        disable("delete_excluded")
        disable("loadSARI")
        disable("correct_waveform")
        disable("custom")
        disable("downloadAs")
        disable("printCustom")
        disable("printLog")
        disable("printSinfo")
        disable("printSoln")
        disable("spectrum")
        disable("spectrumFilter")
        disable("spectrumFilterRes")
        disable("spectrumModel")
        disable("spectrumOriginal")
        disable("periodogram_residuals")
        disable("spectrumType")
        disable("est.mle")
        disable("remove")
        disable("remove3D")
        disable("removeAuto")
        disable("filter")
        disable("flicker")
        disable("format")
        disable("format2")
        disable("histogram")
        disable("histogramType")
        disable("log")
        disable("average")
        disable("midas")
        disable("mle")
        disable("model")
        disable("wavelet")
        disable("waveletType")
        disable("optionSecondary")
        disable("sameScale")
        disable("same_axis")
        disable("waveform")
        disable("plot")
        disable("powerl")
        disable("randomw")
        disable("runVerif")
        disable("runmle")
        disable("series2")
        disable("separator")
        disable("separator2")
        disable("euler")
        disable("series2filter")
        disable("typeSecondary")
        disable("sigmas")
        disable("symbol")
        disable("sinfo")
        disable("soln")
        disable("header")
        disable("traceCustom")
        disable("traceLog")
        disable("traceSinfo")
        disable("traceSoln")
        disable("thresholdRes")
        disable("thresholdResN")
        disable("units")
        disable("verif_offsets")
      }
    }
  }, priority = 100)
  
  # Observe Euler ####
  observeEvent(c(inputs$station_x, inputs$station_y, inputs$station_z, inputs$station_lat, inputs$station_lon, inputs$pole_x, inputs$pole_y, inputs$pole_z, inputs$pole_lat, inputs$pole_lon, inputs$pole_rot), {
    if (input$eulerType != 0) {
      updateRadioButtons(session, inputId = "eulerType", label = NULL, choices = list("None" = 0, "Show" = 1, "Remove" = 2), selected = 0, inline = T)
    }
  })
  observeEvent(c(input$eulers), {
    if (!is.null(file$id1)) {
      pattern <- paste0("^",file$id1)
      record <- grep(pattern, readLines(con = input$eulers$datapath, n = -1L, ok = T, warn = F, skipNul = T), ignore.case = F, perl = T, value = T)
      for (l in seq_len(length(record))) {
        elements <- unlist(strsplit(record[[l]], "\\s+", fixed = F, perl = T, useBytes = F))
        if (length(elements) == 7) { # Cartesian
          stationCartesian <- c(elements[2],elements[3],elements[4])
          updateRadioButtons(session, inputId = "coordenadas_estacion", choices = list("Cartesian" = 1, "Geographic" = 2), selected = 1, inline = T)
          updateTextInput(session, inputId = "station_x", value = stationCartesian[1])
          updateTextInput(session, inputId = "station_y", value = stationCartesian[2])
          updateTextInput(session, inputId = "station_z", value = stationCartesian[3])
          if (sqrt(as.numeric(elements[5])^2 + as.numeric(elements[6])^2 + as.numeric(elements[7])^2) > 2) { #Geographic
            polo_geo <- c(elements[5],elements[6],elements[7])
            updateRadioButtons(session, inputId = "pole_coordinates", choices = list("Cartesian" = 1, "Geographic" = 2), selected = 2, inline = T)
            updateTextInput(session, inputId = "pole_lat", value = polo_geo[1])
            updateTextInput(session, inputId = "pole_lon", value = polo_geo[2])
            updateTextInput(session, inputId = "pole_rot", value = polo_geo[3])
          } else { # Cartesian
            poleCartesian <- c(elements[5],elements[6],elements[7])
            updateRadioButtons(session, inputId = "pole_coordinates", choices = list("Cartesian" = 1, "Geographic" = 2), selected = 1, inline = T)
            updateTextInput(session, inputId = "pole_x", value = poleCartesian[1])
            updateTextInput(session, inputId = "pole_y", value = poleCartesian[2])
            updateTextInput(session, inputId = "pole_z", value = poleCartesian[3])
          }
        } else if (length(elements) == 6) { #Geographic
          stationGeo <- c(elements[2],elements[3])
          updateRadioButtons(session, inputId = "coordenadas_estacion", choices = list("Cartesian" = 1, "Geographic" = 2), selected = 2, inline = T)
          updateTextInput(session, inputId = "station_lat", value = stationGeo[1])
          updateTextInput(session, inputId = "station_lon", value = stationGeo[2])
          if (sqrt(as.numeric(elements[4])^2 + as.numeric(elements[5])^2 + as.numeric(elements[6])^2) > 2) { #Geographic
            polo_geo <- c(elements[4],elements[5],elements[6])
            updateRadioButtons(session, inputId = "pole_coordinates", choices = list("Cartesian" = 1, "Geographic" = 2), selected = 2, inline = T)
            updateTextInput(session, inputId = "pole_lat", value = polo_geo[1])
            updateTextInput(session, inputId = "pole_lon", value = polo_geo[2])
            updateTextInput(session, inputId = "pole_rot", value = polo_geo[3])
          } else { # Cartesian
            poleCartesian <- c(elements[4],elements[5],elements[6])
            updateRadioButtons(session, inputId = "pole_coordinates", choices = list("Cartesian" = 1, "Geographic" = 2), selected = 1, inline = T)
            updateTextInput(session, inputId = "pole_x", value = poleCartesian[1])
            updateTextInput(session, inputId = "pole_y", value = poleCartesian[2])
            updateTextInput(session, inputId = "pole_z", value = poleCartesian[3])
          }
        }
      }
    }
  })
  
  # Observe wavelet ####
  observeEvent(c(inputs$min_wavelet, inputs$max_wavelet, inputs$res_wavelet, inputs$loc_wavelet),{
    req(inputs$min_wavelet, inputs$max_wavelet, inputs$res_wavelet, inputs$loc_wavelet)
    if (isTruthy(inputs$max_wavelet) && isTruthy(inputs$min_wavelet) && isTruthy(as.numeric(inputs$res_wavelet)) && isTruthy(as.numeric(inputs$loc_wavelet)) && inputs$max_wavelet > 0 && inputs$min_wavelet > 0 && as.numeric(inputs$res_wavelet) > 0 && as.numeric(inputs$loc_wavelet) >= info$sampling && as.numeric(inputs$loc_wavelet) <= info$rangex/2) {
      num_scale <- as.integer((inputs$max_wavelet - inputs$min_wavelet)/as.numeric(inputs$res_wavelet))
      num_epochs <- info$rangex/as.numeric(inputs$loc_wavelet)
      time_needed <- ceiling(0.000588*num_scale*num_epochs/60)
      if (time_needed > 29) {
        showNotification(paste0("The time needed to compute the wavelet with the current parameters is around ",time_needed," min. WARNING: the server may kill the connection before the wavelet finishes!"), action = NULL, duration = 10, closeButton = T, id = "time_wavelet", type = "error", session = getDefaultReactiveDomain())
      } else {
        showNotification(paste0("The time needed to compute the wavelet with the current parameters is around ",time_needed," min."), action = NULL, duration = 10, closeButton = T, id = "time_wavelet", type = "warning", session = getDefaultReactiveDomain())
      }
    } else {
      showNotification(paste0("Invalid bounds to compute the wavelet. Check the input values."), action = NULL, duration = 10, closeButton = T, id = "bad_wavelet", type = "error", session = getDefaultReactiveDomain())
    }
  })
  
  # Observe time units ####
  observeEvent(input$tunits, {
    if (isTruthy(input$average) && nchar(input$step) > 0 && !is.na(inputs$step)) {
      showNotification(paste0("Changing the time units and resampling the series using an averaging period based on the previous time unit may produce unexpected results. Check the validity of the input averaging period."), action = NULL, duration = 10, closeButton = T, id = "new_units", type = "warning", session = getDefaultReactiveDomain())
    }
    if (input$fitType == 2 && length(trans$mod) > 0 && length(trans$res) > 0) {
      info$run <- F
      trans$mod <- trans$mod0 <- NULL
      trans$res <- trans$res0 <- NULL
      trans$kalman <- trans$kalman0 <- NULL
      trans$kalman_unc <- trans$kalman_unc0 <- NULL
    }
    if (isTruthy(input$wavelet)) {
      info$run_wavelet <- F
      updateCheckboxInput(session, inputId = "wavelet", value = F)
    }
    if (isTruthy(input$waveform)) {
      trans$pattern <- NULL
      updateCheckboxInput(session, inputId = "waveform", value = F)
    }
    if (isTruthy(input$mle)) {
      updateCheckboxInput(session, inputId = "mle", value = F)
    }
    if (isTruthy(input$spectrum)) {
      trans$fs <- NULL
      updateCheckboxInput(session, inputId = "spectrum", value = F)
    }
    if (isTruthy(input$filter)) {
      info$run_filter <- F
      trans$filter <- NULL
      updateCheckboxInput(session, inputId = "filter", value = F)
    }
  }, priority = 100)
  
  # Observe tab ####
  observeEvent(input$tab, {
    ranges$y1 <- NULL
    ranges$y2 <- NULL
    ranges$y12 <- NULL
    trans$midas_vel <- NULL
    trans$midas_all <- NULL
    trans$res <- trans$res0 <- NULL
    trans$reserror <- NULL
    trans$results <- NULL
    trans$mod <- trans$mod0 <- NULL
    trans$kalman <- trans$kalman0 <- NULL
    trans$kalman_unc <- trans$kalman_unc0 <- NULL
    trans$equation <- NULL
    trans$mle <- F
    info$run <- F
    trans$verif <- NULL
    trans$pattern <- NULL
    trans$names <- NULL
    trans$noise <- NULL
    updateTextInput(session, "ObsError", value = "")
    updateCheckboxInput(session, inputId = "white", label = NULL, value = F)
    updateCheckboxInput(session, inputId = "flicker", label = NULL, value = F)
    updateCheckboxInput(session, inputId = "randomw", label = NULL, value = F)
    updateCheckboxInput(session, inputId = "powerl", label = NULL, value = F)
  }, priority = 100)

  # Observe sitelog ####
  observeEvent(input$log, {
    req(file$primary)
    file$sitelog <- isolate(input$log)
  }, priority = 8)
  
  # Observe secondary series ####
  observeEvent(input$series2, {
    req(file$primary)
    file$secondary <- isolate(input$series2)
  }, priority = 8)
  
  # Observe series info ####
  observeEvent(c(input$tab, input$format, input$tunits, input$sigmas, input$series2, input$optionSecondary, input$log, input$sinfo, input$soln, input$custom, inputs$step), {
    if (input$tab == "4") {
      if (messages > 0) cat(file = stderr(), "Showing help file", "\n")
    } else {
      req(obs())
      info$tab <- input$tab
      if (messages > 0) cat(file = stderr(), "File : ", input$series$name,"   Format: ",input$format,"   Component: ", input$tab,
                            "   Units: ", input$tunits,"   Sigmas: ",input$sigmas,"   Average: ", inputs$step,"   Sitelog: ", 
                            file$sitelog$name, "   station.info: ", input$sinfo$name,"   soln: ", input$soln$name,"   custom: ", 
                            input$custom$name, "   Secondary: ", file$secondary$name,"   Option: ", input$optionSecondary, "\n")
    }
  }, priority = 7)
  
  # Observe primary series ####
  observeEvent(c(input$series), {
    file$primary <- isolate(input$series)
    info$run <- NULL
    trans$x <- NULL
    trans$y <- NULL
    trans$sy <- NULL
    trans$xe <- NULL
    trans$ye <- NULL
    trans$sye <- NULL
    trans$res <- NULL
    trans$reserror <- NULL
    trans$results <- NULL
    trans$mod <- NULL
    trans$filter <- NULL
    trans$filterRes <- NULL
    trans$kalman <- NULL
    trans$equation <- NULL
    trans$ordinate <- NULL
    trans$midas_vel <- NULL
    trans$midas_all <- NULL
    info$points <- NULL
    info$log <- NULL
    trans$mle <- F
    trans$verif <- NULL
    trans$pattern <- NULL
    updateTextInput(session, "ObsError", value = "")
    updateTextInput(session, "waveformPeriod", value = "")
    updateCheckboxInput(session, inputId = "correct_waveform", label = NULL, value = F)
  }, priority = 6)
  
  # Observe format 1D ####
  observeEvent(c(inputs$epoch, inputs$variable, inputs$errorBar, inputs$epoch2, inputs$variable2, inputs$errorBar2), {
    req(obs())
    obs(NULL)
    trans$x <- NULL
    trans$y <- NULL
    trans$sy <- NULL
    data <- digest()
    obs(data)
    if (!isTruthy(input$remove3D)) {
      info$points <- length(data$x)
      values$used_all <- values$used1 <- values$used2 <- values$used3 <- rep(T, info$points)
      values$excluded_all <- values$excluded1 <- values$excluded2 <- values$excluded3 <- rep(F, info$points)
    }
    if (input$fitType == 2) {
      trans$midas_vel <- NULL
      trans$midas_all <- NULL
      trans$res <- NULL
      trans$reserror <- NULL
      trans$results <- NULL
      trans$mod <- NULL
      trans$kalman <- NULL
      trans$equation <- NULL
      trans$mle <- F
      trans$verif <- NULL
      trans$pattern <- NULL
      trans$names <- NULL
      trans$noise <- NULL
      updateTextInput(session, "ObsError", value = "")
      updateTextInput(session, inputId = "Intercept0", value = "")
      updateTextInput(session, inputId = "eIntercept0", value = "")
      updateTextInput(session, inputId = "Trend0", value = "")
      updateTextInput(session, inputId = "eTrend0", value = "")
    }
    updateCheckboxInput(session, inputId = "white", label = NULL, value = F)
    updateCheckboxInput(session, inputId = "flicker", label = NULL, value = F)
    updateCheckboxInput(session, inputId = "randomw", label = NULL, value = F)
    updateCheckboxInput(session, inputId = "powerl", label = NULL, value = F)
  }, priority = 6)
  
  # Observe primary series format ####
  observeEvent(c(input$separator, input$format, input$tunits, input$eulerType, input$neuenu), {
    req(obs())
    obs(NULL)
    data <- digest()
    obs(data)
  }, priority = 6)
  
  # Observe secondary series format ####
  observeEvent(c(input$separator2, input$format2), {
    req(obs())
    if (input$optionSecondary > 0) {
      obs(NULL)
      data <- digest()
      obs(data)
    }
  }, priority = 6)
  
  # Observe averaging ####
  observeEvent(c(inputs$step), {
    req(file$primary)
    if (messages > 0) cat(file = stderr(), "Averaging series", "\n")
    if (input$fitType == 2) {
      info$run <- NULL
      trans$res <- NULL
      trans$reserror <- NULL
      trans$results <- NULL
      trans$mod <- NULL
      trans$kalman <- NULL
      trans$equation <- NULL
      trans$ordinate <- NULL
      trans$filter <- NULL
      trans$filterRes <- NULL
    }
    trans$midas_vel <- NULL
    trans$midas_all <- NULL
    info$points <- NULL
    info$log <- NULL
    trans$mle <- F
    trans$verif <- NULL
    trans$pattern <- NULL
    updateCheckboxInput(session, inputId = "correct_waveform", label = NULL, value = F)
    updateTextInput(session, "short_period", value = "")
    obs(NULL)
    data <- digest()
    obs(data)
    info$points <- length(data$x[!is.na(data$y1)])
    values$used1 <- rep(T, info$points)
    values$used2 <- rep(T, info$points)
    values$used3 <- rep(T, info$points)
    values$used_all <- rep(T, info$points)
    values$excluded1 <- rep(F, info$points)
    values$excluded2 <- rep(F, info$points)
    values$excluded3 <- rep(F, info$points)
    values$excluded_all <- rep(F, info$points)
    updateTextInput(session, "ObsError", value = "")
    if (isTruthy(inputs$step)) {
      info$step <- inputs$step
    } else {
      info$step <- 0
    }
  }, priority = 6)
  
  # Observe secondary series ####
  observeEvent(input$series2, {
    req(file$primary)
    if (input$optionSecondary > 0) {
      if (messages > 0) cat(file = stderr(), "Loading secondary series", "\n")
      data <- digest()
      obs(data)
    }
  }, priority = 6)
  observeEvent(input$optionSecondary, {
    req(file$primary)
    if (messages > 0) {
      if (input$optionSecondary == 0) {
        cat(file = stderr(), "Hidding secondary series", "\n")
      } else if (input$optionSecondary == 1) {
        cat(file = stderr(), "Showing secondary series", "\n")
      } else if (input$optionSecondary == 2) {
        cat(file = stderr(), "Subtracting secondary series", "\n")
      } else if (input$optionSecondary == 3) {
        cat(file = stderr(), "Averaging with secondary series", "\n")
      }
    }
    data <- digest()
    obs(data)
    info$points <- length(data$x[!is.na(data$y1)])
    values$used1 <- rep(T, info$points)
    values$used2 <- rep(T, info$points)
    values$used3 <- rep(T, info$points)
    values$used_all <- rep(T, info$points)
    values$excluded1 <- rep(F, info$points)
    values$excluded2 <- rep(F, info$points)
    values$excluded3 <- rep(F, info$points)
    values$excluded_all <- rep(F, info$points)
    updateTextInput(session, "ObsError", value = "")
    updateTextInput(session, "waveformPeriod", value = "")
    updateCheckboxInput(session, inputId = "correct_waveform", label = NULL, value = F)
  }, priority = 6)
  
  # Observe ids ####
  observeEvent(c(inputs$ids, input$optionSecondary), {
    req(file$primary)
    update <- 0
    file$id1 <- trim(strsplit(inputs$ids, "-|\\&|\\+")[[1]][1])
    if (!isTruthy(file$id1)) {
      file$id1 <- strsplit(input$series$name, "\\.|_|\\s|-|\\(")[[1]][1]
      update <- 1
    }
    file$id2 <- trim(strsplit(inputs$ids, "-|\\&|\\+")[[1]][2])
    if (isTruthy(input$series2$name) && !isTruthy(file$id2)) {
      file$id2 <- strsplit(input$series2$name, "\\.|_|\\s|-|\\(")[[1]][1]
      update <- 1
    }
    if (update > 0) {
      if (!is.null(file$id2)) {
        if (input$optionSecondary == 0) {
          ids_info <- file$id1
        } else if (input$optionSecondary == 1) {
          ids_info <- paste(file$id1,file$id2, sep = " & ")
        } else if (input$optionSecondary == 2) {
          ids_info <- paste(file$id1,file$id2, sep = " - ")
        } else if (input$optionSecondary == 3) {
          ids_info <- paste(file$id1,file$id2, sep = " + ")
        }
      } else {
        ids_info <- file$id1
      }
      updateTextInput(session, inputId = "ids", value = ids_info)
    }
  }, priority = 5)
  
  # Observe remove fit ####
  observeEvent(c(input$series, input$separator, input$format, input$format2, inputs$epoch, inputs$epoch2, input$separator2), {
    req(trans$res)
    if (messages > 0) cat(file = stderr(), "Deleting fit", "\n")
    trans$res <- NULL
    trans$reserror <- NULL
    trans$results <- NULL
    trans$mod <- NULL
    trans$kalman <- NULL
    trans$equation <- NULL
    trans$midas_vel <- NULL
    trans$midas_all <- NULL
    trans$filter <- NULL
    trans$filterRes <- NULL
    trans$mle <- F
    trans$verif <- NULL
    trans$pattern <- NULL
    trans$spectra <- NULL
    updateTextInput(session, "ObsError", value = "")
    updateTextInput(session, "waveformPeriod", value = "")
    updateCheckboxInput(session, inputId = "correct_waveform", label = NULL, value = F)
    updateCheckboxGroupInput(session, inputId = "model", label = "", choices = list("Linear","Polynomial","Sinusoidal","Offset","Exponential","Logarithmic"), selected = NULL, inline = T)
    updateRadioButtons(session, inputId = "fitType", label = NULL, list("None" = 0, "LS" = 1, "KF" = 2), selected = 0, inline = T, choiceNames = NULL, choiceValues = NULL)
    updateTextInput(session, "E0", value = "")
    updateTextInput(session, "L0", value = "")
    updateCheckboxInput(session, inputId = "white", label = NULL, value = F)
    updateCheckboxInput(session, inputId = "flicker", label = NULL, value = F)
    updateCheckboxInput(session, inputId = "randomw", label = NULL, value = F)
    updateCheckboxInput(session, inputId = "powerl", label = NULL, value = F)
  }, priority = 5)
  
  # Observe fit type ####
  observeEvent(input$fitType, {
    req(trans$res)
    if (messages > 0) cat(file = stderr(), "Deleting fit", "\n")
    trans$res <- NULL
    trans$reserror <- NULL
    trans$results <- NULL
    trans$mod <- NULL
    trans$kalman <- NULL
    trans$equation <- NULL
    trans$mle <- F
    trans$verif <- NULL
    trans$pattern <- NULL
    updateCheckboxInput(session, inputId = "correct_waveform", label = NULL, value = F)
    updateTextInput(session, "ObsError", value = "")
    updateCheckboxInput(session, inputId = "white", label = NULL, value = F)
    updateCheckboxInput(session, inputId = "flicker", label = NULL, value = F)
    updateCheckboxInput(session, inputId = "randomw", label = NULL, value = F)
    updateCheckboxInput(session, inputId = "powerl", label = NULL, value = F)
    updateTextInput(session, "E0", value = "")
    updateTextInput(session, "TE0", value = "")
    updateTextInput(session, "L0", value = "")
    updateTextInput(session, "TL0", value = "")
  }, priority = 5)
  
  # Observe delete model ####
  observeEvent(input$model, {
    req(trans$res)
    if (!isTruthy(input$model)) {
      if (messages > 0) cat(file = stderr(), "Deleting model", "\n")
      info$run <- F
      trans$res <- NULL
      trans$reserror <- NULL
      trans$results <- NULL
      trans$mod <- NULL
      trans$kalman <- NULL
      trans$equation <- NULL
      trans$midas_vel <- NULL
      trans$midas_all <- NULL
    }
    trans$mle <- F
    trans$verif <- NULL
    updateCheckboxInput(session, inputId = "white", label = NULL, value = F)
    updateCheckboxInput(session, inputId = "flicker", label = NULL, value = F)
    updateCheckboxInput(session, inputId = "randomw", label = NULL, value = F)
    updateCheckboxInput(session, inputId = "powerl", label = NULL, value = F)
  }, priority = 5)
  
  # Observe hide buttons ####
  observeEvent(input$format, {
    if (input$format == 4) {
      output$tabName = renderText({ "1D series" })
      hideTab(inputId = "tab", target = "2", session = getDefaultReactiveDomain())
      hideTab(inputId = "tab", target = "3", session = getDefaultReactiveDomain())
    } else {
      output$tabName = renderText({ "1st component" })
      showTab(inputId = "tab", target = "2", session = getDefaultReactiveDomain())
      showTab(inputId = "tab", target = "3", session = getDefaultReactiveDomain())
    }
  })
  
  # Observe plotting ####
  observeEvent(input$plot, {
    req(file$primary)
    if (messages > 0) cat(file = stderr(), "File : ", input$series$name,"   Format: ",input$format,"   Component: ", input$tab,
                          "   Units: ", input$tunits,"   Sigmas: ",input$sigmas,"   Average: ", inputs$step,"   Sitelog: ", 
                          file$sitelog$name, "   station.info: ", input$sinfo$name,"   soln: ", input$soln$name,"   custom: ", 
                          input$custom$name, "   Secondary: ", file$secondary$name,"   Option: ", input$optionSecondary, "\n")
    data <- digest()
    if (!is.null(data)) {
      obs(data)
      info$points <- length(data$x)
      values$used1 <- rep(T, info$points)
      values$used2 <- rep(T, info$points)
      values$used3 <- rep(T, info$points)
      values$used_all <- rep(T, info$points)
      values$excluded1 <- rep(F, info$points)
      values$excluded2 <- rep(F, info$points)
      values$excluded3 <- rep(F, info$points)
      values$excluded_all <- rep(F, info$points)
    }
  }, priority = 4)
  
  # Observe removing points manual ####
  observeEvent(input$remove, {
    req(file$primary)
    if (messages > 0) cat(file = stderr(), "Removing points, manually", "\n")
    brush1 <- input$plot_brush
    brush2 <- NULL
    excluding_plot <- excluding_plotres <- NULL
    if (isTruthy(input$res_brush)) {
      brush2 <- input$res_brush
    } else if (isTruthy(input$vondrak_brush)) {
      brush2 <- input$vondrak_brush
    }
    if (input$fitType == 2 && length(trans$mod) > 0 && length(trans$res) > 0) {
      if (isTruthy(brush1)) {
        showNotification("It is not possible to toggle points from the series plot after running a Kalman filter. Consider removing points from the residuals plot or restoring points by reseting all the toggled points.", action = NULL, duration = 10, closeButton = T, id = "no_toggle", type = "warning", session = getDefaultReactiveDomain())
        req(info$stop)
      } else {
        series_kf <- data.frame(x = trans$x0_kf, y = trans$res0)
      }
    }
    series <- data.frame(x = trans$x0[!is.na(trans$y0)], y = trans$y0[!is.na(trans$y0)])
    if (isTruthy(brush1) || isTruthy(brush2)) {
      if (length(brush1) > 0) {
        excluding_plot <- brushedPoints(series, brush1, xvar = "x", yvar = "y", allRows = T)
      }
      if (length(brush2) > 0) {
        if (isTruthy(input$res_brush) && length(trans$res) > 0) {
          residuals <- data.frame(x = trans$x, y = trans$res)  
        } else if (isTruthy(input$vondrak_brush) && length(trans$filterRes) > 0) {
          residuals <- data.frame(x = trans$x, y = trans$filterRes)
        } else {
          req(info$stop)
        }
        excluding_res <- brushedPoints(residuals, brush2, xvar = "x", yvar = "y", allRows = T)
        if (input$fitType == 2 && length(trans$mod) > 0 && length(trans$res) > 0) {
          excluding_plotres_kf <- merge(series_kf, excluding_res, by = "x", all.x = T)
          excluding_plotres_kf$selected_ <- sapply(1:length(excluding_plotres_kf$x), function(x) if (isTRUE(excluding_plotres_kf$selected_[x])) T else F)
        }
        excluding_plotres <- merge(series, excluding_res, by = "x", all.x = T)
        excluding_plotres$selected_ <- sapply(1:length(excluding_plotres$x), function(x) if (isTRUE(excluding_plotres$selected_[x])) T else F)
      }
      if ((isTruthy(excluding_plot$selected_) && sum(excluding_plot$selected_) > 0) || (isTruthy(excluding_plotres$selected_) && sum(excluding_plotres$selected_) > 0)) {
        if (isTruthy(input$remove3D)) {
          if (length(brush1) > 0) {
            values$used_all <- xor(values$used_all, excluding_plot$selected_)
            values$excluded_all <- (values$excluded_all + excluding_plot$selected_) == 1
          }
          if (length(brush2) > 0) {
            values$used_all <- xor(values$used_all, excluding_plotres$selected_)
            values$excluded_all <- (values$excluded_all + excluding_plotres$selected_) == 1
            if (input$fitType == 2 && length(trans$mod) > 0 && length(trans$res) > 0) {
              values$used_all_kf <- xor(values$used_all_kf, excluding_plotres_kf$selected_)
            }
          }
          values$used1 <- values$used2 <- values$used3 <- values$used_all
          values$excluded1 <- values$excluded2 <- values$excluded3 <- values$excluded_all
        } else {
          if (input$tab == 1 || is.null(input$tab)) {
            if (length(brush1) > 0) {
              values$used1 <- xor(values$used1, excluding_plot$selected_)
              values$excluded1 <- (values$excluded1 + excluding_plot$selected_) == 1
            }
            if (length(brush2) > 0) {
              values$used1 <- xor(values$used1, excluding_plotres$selected_)
              values$excluded1 <- (values$excluded1 + excluding_plotres$selected_) == 1
              if (input$fitType == 2 && length(trans$mod) > 0 && length(trans$res) > 0) {
                values$used_kf1 <- xor(values$used_kf1, excluding_plotres_kf$selected_)
              }
            }
          } else if (input$tab == 2) {
            if (length(brush1) > 0) {
              values$used2 <- xor(values$used2, excluding_plot$selected_)
              values$excluded2 <- (values$excluded2 + excluding_plot$selected_) == 1
            }
            if (length(brush2) > 0) {
              values$used2 <- xor(values$used2, excluding_plotres$selected_)
              values$excluded2 <- (values$excluded2 + excluding_plotres$selected_) == 1
              if (input$fitType == 2 && length(trans$mod) > 0 && length(trans$res) > 0) {
                values$used_kf2 <- xor(values$used_kf2, excluding_plotres_kf$selected_)
              }
            }
          } else if (input$tab == 3) {
            if (length(brush1) > 0) {
              values$used3 <- xor(values$used3, excluding_plot$selected_)
              values$excluded3 <- (values$excluded3 + excluding_plot$selected_) == 1
            }
            if (length(brush2) > 0) {
              values$used3 <- xor(values$used3, excluding_plotres$selected_)
              values$excluded3 <- (values$excluded3 + excluding_plotres$selected_) == 1
              if (input$fitType == 2 && length(trans$mod) > 0 && length(trans$res) > 0) {
                values$used_kf3 <- xor(values$used_kf3, excluding_plotres_kf$selected_)
              }
            }
          }
        } 
      } else {
        showNotification("No point was selected to be removed manually. Check the selected area.", action = NULL, duration = 10, closeButton = T, id = "no_point_manual", type = "warning", session = getDefaultReactiveDomain())
      }
    }
  }, priority = 4)
  
  # Observe removing points auto ####
  observeEvent(input$removeAuto, {
    req(file$primary)
    if (messages > 0) cat(file = stderr(), "Removing points, automatically", "\n")
    if (input$fitType == 2 && length(trans$mod) > 0 && length(trans$res) > 0) {
      series_kf <- data.frame(x = trans$x0_kf, y = trans$res0)
    }
    series <- data.frame(x = trans$x0[!is.na(trans$y0)], y = trans$y0[!is.na(trans$y0)])  
    if (length(trans$res) > 0) {
      if (isTruthy(input$sigmas)) {
        if (length(trans$reserror) > 0) {
          residuals <- data.frame(x = trans$x, res = trans$res, sy = trans$reserror)
        } else {
          residuals <- data.frame(x = trans$x, res = trans$res, sy = trans$sy)
        }
      } else {
        residuals <- data.frame(x = trans$x, res = trans$res)
      }
    } else if (length(trans$filterRes) > 0) {
      residuals <- data.frame(x = trans$x, res = trans$filterRes)
    } else {
      req(info$stop)
    }
    if (input$fitType == 2 && length(trans$mod) > 0 && length(trans$res) > 0) {
      excluding_kf <- rep(F,length(trans$res0))
      joint_kf <- merge(series_kf, residuals, by = "x", all.x = T)
      joint_kf$res <- sapply(1:length(joint_kf$x), function(x) if (is.na(joint_kf$res[x])) 0 else joint_kf$res[x])
      if (length(joint_kf$sy) > 0) {
        joint_kf$sy <- sapply(1:length(joint_kf$x), function(x) if (is.na(joint_kf$sy[x])) 1 else joint_kf$sy[x])
      }
    }
    excluding <- rep(F,length(series$x))
    joint <- merge(series, residuals, by = "x", all.x = T)
    joint$res <- sapply(1:length(joint$x), function(x) if (is.na(joint$res[x])) 0 else joint$res[x])
    if (length(joint$sy) > 0) {
      joint$sy <- sapply(1:length(series$x), function(x) if (is.na(joint$sy[x])) 1 else joint$sy[x])
    }
    if (nchar(input$thresholdResN) > 0 && length(joint$sy) > 0) {
      if (!is.na(inputs$thresholdResN)) {
        if (messages > 0) cat(file = stderr(), "Limit normalized residual ", inputs$thresholdResN, "\n")
        if (input$fitType == 2 && length(trans$mod) > 0 && length(trans$res) > 0) {
          excluding_plot_kf <- abs(joint_kf$res/joint_kf$sy) > abs(inputs$thresholdResN)
          excluding_kf <- excluding_plot_kf
        }
        excluding_plot <- abs(joint$res/joint$sy) > abs(inputs$thresholdResN)
        excluding <- excluding_plot
      } else {
        showNotification("The normalised residual threshold is not numeric. Check the input value.", action = NULL, duration = 10, closeButton = T, id = "bad_normalised_threshold", type = "warning", session = getDefaultReactiveDomain())
      }
    }
    if (nchar(input$thresholdRes) > 0) {
      if (!is.na(inputs$thresholdRes)) {
        if (abs(inputs$thresholdRes) < min(abs(residuals))) {
          showNotification("The residual threshold will remove all data from the residual series. Check the input value.", action = NULL, duration = 10, closeButton = T, id = "bad_threshold", type = "error", session = getDefaultReactiveDomain())	        
        } else {
          if (messages > 0) cat(file = stderr(), "Limit absolute residual ", inputs$thresholdRes, "\n")
          if (input$fitType == 2 && length(trans$mod) > 0 && length(trans$res) > 0) {
            excluding_res_kf <- abs(joint_kf$res) > abs(inputs$thresholdRes)
            excluding_kf <- excluding_kf + excluding_res_kf > 0
          }
          excluding_res <- abs(joint$res) > abs(inputs$thresholdRes)
          excluding <- excluding + excluding_res > 0
        }
      } else {
        showNotification("The residual threshold is not numeric. Check the input value.", action = NULL, duration = 10, closeButton = T, id = "bad_threshold", type = "error", session = getDefaultReactiveDomain())
      }
    }
    if (isTruthy(excluding) && sum(excluding) > 0) {
      if (isTruthy(input$remove3D)) {
        values$used_all <- (values$used_all - excluding) > 0
        values$excluded_all <- (values$excluded_all + excluding) > 0
        values$used1 <- values$used2 <- values$used3 <- values$used_all
        values$excluded1 <- values$excluded2 <- values$excluded3 <- values$excluded_all
        if (input$fitType == 2 && length(trans$mod) > 0 && length(trans$res) > 0) {
          values$used_all_kf <- (values$used_all_kf - excluding_kf) > 0
        }
      } else {
        if (input$tab == 1 || is.null(input$tab)) {
          values$used1 <- (values$used1 - excluding) > 0
          values$excluded1 <- (values$excluded1 + excluding) > 0
          if (input$fitType == 2 && length(trans$mod) > 0 && length(trans$res) > 0) {
            values$used_kf1 <- (values$used_kf1 - excluding_kf) > 0
          }
        } else if (input$tab == 2) {
          values$used2 <- (values$used2 - excluding) > 0
          values$excluded2 <- (values$excluded2 + excluding) > 0
          if (input$fitType == 2 && length(trans$mod) > 0 && length(trans$res) > 0) {
            values$used_kf2 <- (values$used_kf2 - excluding_kf) > 0
          }
        } else if (input$tab == 3) {
          values$used3 <- (values$used3 - excluding) > 0
          values$excluded3 <- (values$excluded3 + excluding) > 0
          if (input$fitType == 2 && length(trans$mod) > 0 && length(trans$res) > 0) {
            values$used_kf3 <- (values$used_kf3 - excluding_kf) > 0
          }
        }
      }
    } else {
      showNotification("No point was selected to be removed automatically. Check the input threshold.", action = NULL, duration = 10, closeButton = T, id = "no_point_auto", type = "warning", session = getDefaultReactiveDomain())
    }
  }, priority = 4)
  
  # Observe restore removed ####
  observeEvent(input$delete_excluded, {
    req(file$primary)
    if (messages > 0) cat(file = stderr(), "Restoring points", "\n")
    if (isTruthy(input$remove3D)) {
      values$used_all <- rep(T, length(trans$y0[!is.na(trans$y0)]))
      values$excluded_all <- rep(F, sum(values$used_all))
      values$used1 <- values$used2 <- values$used3 <- values$used_all
      values$excluded1 <- values$excluded2 <- values$excluded3 <- values$excluded_all
      if (input$fitType == 2 && length(trans$mod) > 0 && length(trans$res) > 0) {
        values$used_kf1 <- values$used_kf2 <- values$used_kf3 <- values$used_all_kf <- rep(T, length(trans$res0))
      }
    } else {
      if (input$tab == 1 || is.null(input$tab)) {
        values$used1 <- rep(T, length(trans$x0))
        values$excluded1 <- rep(F, length(trans$x0))
        if (input$fitType == 2 && length(trans$mod) > 0 && length(trans$res) > 0) {
          values$used_kf1 <- rep(T, length(trans$res0))
        }
      } else if (input$tab == 2) {
        values$used2 <- rep(T, length(trans$x0))
        values$excluded2 <- rep(F, length(trans$x0))
        if (input$fitType == 2 && length(trans$mod) > 0 && length(trans$res) > 0) {
          values$used_kf2 <- rep(T, length(trans$res0))
        }
      } else if (input$tab == 3) {
        values$used3 <- rep(T, length(trans$x0))
        values$excluded3 <- rep(F, length(trans$x0))
        if (input$fitType == 2 && length(trans$mod) > 0 && length(trans$res) > 0) {
          values$used_kf3 <- rep(T, length(trans$res0))
        }
      }
    }
    updateTextInput(session, "thresholdRes", value = "")
    updateTextInput(session, "thresholdResN", value = "")
  }, priority = 4)
  
  # Observe station.info ####
  observeEvent(c(input$sinfo, input$series, input$series2, input$optionSecondary, file$id1, file$id2), {
    req(input$sinfo,file$id1)
    if (messages > 0) cat(file = stderr(), "Reading station.info", "\n")
    id1 <- file$id1
    if (length(file$secondary) > 0) {
      if (input$optionSecondary < 2) {
        id2 <- NULL
      } else {
        id2 <- file$id2
      } 
    } else {
      id2 <- NULL
    }
    info$sinfo <- ReadInfo(id1,id2,input$sinfo)
  }, priority = 4)
  observeEvent(c(input$printSinfo),{
    req(file$primary, input$sinfo)
    output$changes_ant1s <- output$changes_ant2s <- output$changes_ant3s <- renderText({
      if (length(info$sinfo[[1]]) > 0) {
        sprintf("Antenna changes from station.info at\n%s",paste(unlist(info$sinfo[[1]]), collapse = ", "))
      } else {
        NULL
      }
    })
    output$changes_rec1s <- output$changes_rec2s <- output$changes_rec3s <- renderText({
      if (length(info$sinfo[[2]]) > 0) {
        sprintf("Receiver changes from station.info at\n%s",paste(unlist(info$sinfo[[2]]), collapse = ", "))
      } else {
        NULL
      }
    })
  })
  
  # Observe soln.snx ####
  observeEvent(c(input$soln, input$series, input$series2, input$optionSecondary, file$id1, file$id2), {
    req(input$soln,file$id1)
    if (messages > 0) cat(file = stderr(), "Reading soln", "\n")
    id1 <- file$id1
    if (length(file$secondary) > 0) {
      if (input$optionSecondary < 2) {
        id2 <- NULL
      } else {
        id2 <- file$id2
      } 
    } else {
      id2 <- NULL
    }
    info$soln <- ReadSoln(id1,id2,input$soln)
  }, priority = 4)
  observeEvent(c(input$printSoln),{
    req(file$primary, input$soln)
    output$changes_ant1so <- output$changes_ant2so <- output$changes_ant3so <- renderText({
      if (length(info$soln) > 0) {
        sprintf("Discontinuities from soln file at\n%s",paste(unlist(info$soln), collapse = ", "))
      } else {
        NULL
      }
    })
  })
  
  # Observe custom ####
  observeEvent(c(input$custom, input$series, input$series2, input$optionSecondary, file$id1, file$id2), {
    req(input$custom,file$id1)
    if (messages > 0) cat(file = stderr(), "Reading custom", "\n")
    id1 <- file$id1
    if (length(file$secondary) > 0) {
      if (input$optionSecondary < 2) {
        id2 <- NULL
      } else {
        id2 <- file$id2
      }
    } else {
      id2 <- NULL
    }
    info$custom <- ReadCustom(id1,id2,input$custom)
  }, priority = 4)
  observeEvent(c(input$printCustom),{
    req(file$primary, input$custom)
    output$changes_ant1c <- output$changes_ant2c <- output$changes_ant3c <- renderText({
      if (length(info$custom) > 0) {
        sprintf("Changes from custom file at\n%s",paste(unlist(info$custom), collapse = ", "))
      } else {
        NULL
      }
    })
  })
  
  # Observe sitelog ####
  observeEvent(file$sitelog, {
    if (messages > 0) cat(file = stderr(), "Reading sitelog", "\n")
    info$log <- ReadLog(file$sitelog)
  }, priority = 1)
  observeEvent(c(input$printLog),{
    req(file$primary, file$sitelog)
    output$changes_ant1 <- output$changes_ant2 <- output$changes_ant3 <- renderText({
      if (length(info$log[[1]]) > 0) {
        sprintf("Antenna changes from log file at\n%s",paste(unlist(info$log[[1]]), collapse = ", "))
      } else {
        NULL
      }
    })
    output$changes_rec1 <- output$changes_rec2 <- output$changes_rec3 <- renderText({
      if (length(info$log[[2]]) > 0) {
        sprintf("Receiver changes from log file at\n%s",paste(unlist(info$log[[2]]), collapse = ", "))
      } else {
        NULL
      }
    })
  })
  
  # Observe reset ####
  observeEvent(input$reset, {
    req(file$primary)
    if (messages > 0) cat(file = stderr(), "Reset all", "\n")
    reset("side-panel")
    reset("main-panel")
    obs(NULL)
    ranges$x1 <- NULL
    ranges$y1 <- NULL
    ranges$y12 <- NULL
    ranges$x2 <- NULL
    ranges$y2 <- NULL
    ranges$x3 <- NULL
    ranges$y3 <- NULL
    file$primary <- NULL
    file$secondary <- NULL
    file$id1 <- NULL
    file$id2 <- NULL
    file$sitelog <- NULL
    trans$x <- NULL
    trans$y <- NULL
    trans$sy <- NULL
    trans$xe <- NULL
    trans$ye <- NULL
    trans$sye <- NULL
    info$run <- NULL
    trans$res <- NULL
    trans$reserror <- NULL
    trans$results <- NULL
    trans$mod <- NULL
    trans$filter <- NULL
    trans$filterRes <- NULL
    trans$kalman <- NULL
    trans$equation <- NULL
    trans$ordinate <- NULL
    trans$offsetEpochs <- NULL
    trans$tol <- NULL
    trans$midas_vel <- NULL
    trans$midas_all <- NULL
    trans$mle <- F
    trans$verif <- NULL
    trans$pattern <- NULL
    trans$unc <- NULL
    values$used1 <- NULL
    values$excluded1 <- NULL
    values$used2 <- NULL
    values$excluded2 <- NULL
    values$used3 <- NULL
    values$excluded3 <- NULL
    values$used_all <- NULL
    values$excluded_all <- NULL
    info$points <- NULL
    info$log <- NULL
    info$rangex <- NULL
    info$sampling <- NULL
    info$errorbars <- T
    updateTextInput(session, "waveformPeriod", value = "")
    updateCheckboxInput(session, inputId = "waveform", label = NULL, value = F)
    updateCheckboxInput(session, inputId = "white", label = NULL, value = F)
    updateCheckboxInput(session, inputId = "flicker", label = NULL, value = F)
    updateCheckboxInput(session, inputId = "randomw", label = NULL, value = F)
    updateCheckboxInput(session, inputId = "powerl", label = NULL, value = F)
    updateTextInput(session, "ObsError", value = "")
    updateTabsetPanel(session, inputId = "tab", selected = "1")
    updateSliderInput(session, inputId = "segmentLength", value = 10)
    output$offsetFound <- renderUI({
      NULL
    })
  })
  
  # Observe hide buttons ####
  observeEvent(c(input$tab, trans$filter, trans$res, inputs$step, input$optionSecondary), {
    if (input$tab == 4) {
      showTab(inputId = "tab", target = "6", select = F, session = getDefaultReactiveDomain())
      hideTab(inputId = "tab", target = "5", session = getDefaultReactiveDomain())
    } else {
      hideTab(inputId = "tab", target = "6", session = getDefaultReactiveDomain())
      if (length(trans$filter) > 0 || length(trans$res) > 0 || (nchar(inputs$step) > 0 && !is.na(inputs$step) && inputs$step > 0) || input$optionSecondary > 1) {
        showTab(inputId = "tab", target = "5", select = F, session = getDefaultReactiveDomain())
      } else {
        hideTab(inputId = "tab", target = "5", session = getDefaultReactiveDomain())
      }
    }
  }, priority = 0)
  observeEvent(c(trans$filter, trans$res, input$model, input$filter), {
    if (length(trans$filter) > 0 || length(trans$res) > 0) {
      if (input$tab != 4) {
        showTab(inputId = "tab", target = "5", select = F, session = getDefaultReactiveDomain())
      }
    } else {
      hideTab(inputId = "tab", target = "5", session = getDefaultReactiveDomain())
    }
  }, priority = 0)
  
  # Observe directory ####
  observeEvent(input$directory, {
    if (isTruthy(input$directory)) {
      info$directory <- input$directory
    }
  })
  
  # Observe scrolling ####
  observeEvent(input$overflow, {
    if (isTruthy(input$overflow)) {
      runjs("document.getElementById('side-panel').style.overflowY='auto';")
      runjs("document.getElementById('side-panel').style.position='fixed';")
      runjs("document.getElementById('side-panel').style.width='inherit';")
      runjs("document.getElementById('side-panel').style.marginRight='0px';")
    } else {
      runjs("document.getElementById('side-panel').style.overflowY='visible';")
      runjs("document.getElementById('side-panel').style.position='static';")
      runjs("document.getElementById('side-panel').style.width='100%';")
      runjs("document.getElementById('side-panel').style.marginRight='0px';")
      runjs("window.scrollTo(0,0)")
    }
  })
  
  # Observe ??? ####
  observe({
    if (length(input$model) == 0) {
      updateCheckboxInput(session, inputId = "periodogram_residuals", label = NULL, value = F)
      updateCheckboxInput(session, inputId = "spectrumModel", label = NULL, value = F)
      shinyjs::delay(1000, disable("periodogram_residuals"))
      shinyjs::delay(1000, disable("spectrumModel"))
    }
    if (length(trans$filter) > 0) {
      enable("spectrumFilter")
      enable("spectrumFilterRes")
    } else {
      updateCheckboxInput(session, inputId = "spectrumFilter", label = NULL, value = F)
      updateCheckboxInput(session, inputId = "spectrumFilterRes", label = NULL, value = F)
      shinyjs::delay(1000, disable("spectrumFilter"))
      shinyjs::delay(1000, disable("spectrumFilterRes"))
    }
  }, priority = 0)
  
  
  
  # Functions ####
  digest <- function() {
    req(file$primary)
    if (messages > 0) cat(file = stderr(), "Reading input series", "\n")
    # Setting column separation
    if (input$separator == "1") {
      sep <- ""
    } else if (input$separator == "2") {
      sep <- ","
    } else if (input$separator == "3") {
      sep <- ";"
    }
    if (input$separator2 == "1") {
      sep2 <- ""
    } else if (input$separator2 == "2") {
      sep2 <- ","
    } else if (input$separator2 == "3") {
      sep2 <- ";"
    }
    
    if (input$format == 4) {
      updateTabsetPanel(session, inputId = "tab", selected = "1")
    }
    # Getting number of columns in file and setting station ID
    columns <- columns2 <- 0
    columns <- get_columns(input$series$datapath, sep, input$format)
    if (columns > 0) {
      if (!isTruthy(inputs$ids)) {
        file$id1 <- strsplit(input$series$name, "\\.|_|\\s|-|\\(")[[1]][1]
      }
      if (length(file$secondary) > 1 && input$optionSecondary > 0) {
        columns2 <- get_columns(file$secondary$datapath, sep2, input$format2)
        if (columns2 > 0) {
          if (!isTruthy(inputs$ids)) {
            file$id2 <- strsplit(input$series2$name, "\\.|_|\\s|-|\\(")[[1]][1]
          }
        } else {
          updateRadioButtons(session, inputId = "optionSecondary", label = NULL, choices = list("None" = 0, "Show" = 1, "Correct" = 2, "Average" = 3), selected = 0, inline = F)
        }
      }
      if (isTruthy(file$id1) && isTruthy(file$id2)) {
        if (input$optionSecondary == 0) {
          ids_info <- file$id1
        } else if (input$optionSecondary == 1) {
          ids_info <- paste(file$id1,file$id2, sep = " & ")
        } else if (input$optionSecondary == 2) {
          ids_info <- paste(file$id1,file$id2, sep = " - ")
        } else if (input$optionSecondary == 3) {
          ids_info <- paste(file$id1,file$id2, sep = " + ")
        }
      } else if (isTruthy(file$id1)) {
        ids_info <- file$id1
      } else {
        ids_info <- ""
        removeNotification(id = "ids_info", session = getDefaultReactiveDomain())
        showNotification("Problem extracting the series ID from the file name. No series ID will be used", action = NULL, duration = 10, closeButton = T, id = "ids_info", type = "warning", session = getDefaultReactiveDomain())
      }
      updateTextInput(session, inputId = "ids", value = ids_info)
      # Getting data series from input file
      table <- NULL
      table2 <- NULL
      table <- extract_table(input$series$datapath,sep,input$format,columns,as.numeric(inputs$epoch),as.numeric(inputs$variable),as.numeric(inputs$errorBar))
      if (length(file$secondary) > 1 && input$optionSecondary > 0 && columns2 > 0) {
        if (input$format < 4 && input$format != input$format2) {
          removeNotification(id = "formats", session = getDefaultReactiveDomain())
          showNotification("The primary and secondary series have different format. Verify the time units are the same.", action = NULL, duration = 10, closeButton = T, id = "different_formats", type = "warning", session = getDefaultReactiveDomain())
        }
        table2 <- extract_table(input$series2$datapath,sep2,input$format2,columns2,as.numeric(inputs$epoch2),as.numeric(inputs$variable2),as.numeric(inputs$errorBar2))
      }
      if (length(file$secondary) > 1 && !is.null(table) && !is.null(table2) && input$optionSecondary == 1 && columns2 > 0) {
        if (input$format == 4) {
          table <- data.frame(within(merge(table,table2,by = "x", all = T), {
            y1 <- y1.x 
            z1 <- y1.y
            sy1 <- sy1.x
            sz1 <- sy1.y
          })[,c("x","y1","sy1","z1","sz1")])
        } else {
          table <- data.frame(within(merge(table,table2,by = "x", all = T), {
            if (input$format2 == 4) {
              y1 <- y1.x 
              z1 <- y1.y
              y2 <- y2
              z2 <- y1.y
              y3 <- y3
              z3 <- y1.y
              sy1 <- sy1.x
              sz1 <- sy1.y
              sy2 <- sy2
              sz2 <- sy1.y
              sy3 <- sy3
              sz3 <- sy1.y
            } else {
              y1 <- y1.x 
              z1 <- y1.y
              y2 <- y2.x
              z2 <- y2.y
              y3 <- y3.x
              z3 <- y3.y
              sy1 <- sy1.x
              sz1 <- sy1.y
              sy2 <- sy2.x
              sz2 <- sy2.y
              sy3 <- sy3.x
              sz3 <- sy3.y
            }
          })[,c("x","y1","y2","y3","sy1","sy2","sy3","z1","z2","z3","sz1","sz2","sz3")])
        }
        info$sampling2 <- min(diff(table2$x,1))
      }
      if (length(file$secondary) > 1 && !is.null(table) && !is.null(table2) && input$optionSecondary == 2 && columns2 > 0) {
        if (input$format == 4) {
          table <- data.frame(within(merge(table,table2,by = "x"), {
            y1 <- y1.x - y1.y
            sy1 <- sqrt(sy1.x^2 + sy1.y^2)
          })[,c("x","y1","sy1")])
        } else {
          table <- data.frame(within(merge(table,table2,by = "x"), {
            if (input$format2 == 4) {
              y1 <- y1.x - y1.y
              y2 <- y2 - y1.y
              y3 <- y3 - y1.y
              sy1 <- sqrt(sy1.x^2 + sy1.y^2)
              sy2 <- sqrt(sy2^2 + sy1.y^2)
              sy3 <- sqrt(sy3^2 + sy1.y^2)
            } else {
              y1 <- y1.x - y1.y
              y2 <- y2.x - y2.y
              y3 <- y3.x - y3.y
              sy1 <- sqrt(sy1.x^2 + sy1.y^2)
              sy2 <- sqrt(sy2.x^2 + sy2.y^2)
              sy3 <- sqrt(sy3.x^2 + sy3.y^2)
            }
          })[,c("x","y1","y2","y3","sy1","sy2","sy3")])
        }
        showNotification(paste0("There are ",length(table$x)," epochs in common between the primary and secondary series"), action = NULL, duration = 10, closeButton = T, id = "in_common", type = "warning", session = getDefaultReactiveDomain())
      }
      if (length(file$secondary) > 1 && !is.null(table) && !is.null(table2) && input$optionSecondary == 3 && columns2 > 0) {
        if (input$format == 4) {
          table <- data.frame(within(merge(table,table2,by = "x"), {
            y1 <- (y1.x + y1.y) / 2
            sy1 = sd(c(sy1.x,sy1.y))/sqrt(2)
          })[,c("x","y1","sy1")])
        } else {
          table <- data.frame(within(merge(table,table2,by = "x"), {
            if (input$format2 == 4) {
              y1 <- (y1.x + y1.y) / 2
              y2 <- (y2 + y1.y) / 2
              y3 <- (y3 + y1.y) / 2
              sy1 <- sd(c(sy1.x,sy1.y))/sqrt(2)
              sy2 <- sd(c(sy2,sy1.y))/sqrt(2)
              sy3 <- sd(c(sy3,sy1.y))/sqrt(2)
            } else {
              y1 <- (y1.x + y1.y) / 2
              y2 <- (y2.x + y2.y) / 2
              y3 <- (y3.x + y3.y) / 2
              sy1 = sd(c(sy1.x,sy1.y))/sqrt(2)
              sy2 = sd(c(sy2.x,sy2.y))/sqrt(2)
              sy3 = sd(c(sy3.x,sy3.y))/sqrt(2)
            }
          })[,c("x","y1","y2","y3","sy1","sy2","sy3")])
        }
        showNotification(paste0("There are ",length(table$x)," epochs in common between the primary and secondary series"), action = NULL, duration = 10, closeButton = T, id = "in_common", type = "warning", session = getDefaultReactiveDomain())
      }
      # Checking series values and time order 
      if (!is.null(table)) {
        table <- table[order(table$x),]
        table <- table[!is.infinite(rowSums(table)),]
        if (anyNA(table) && is.null(table2)) {
          table <- na.omit(table)
          showNotification("The input file contains records with NA/NaN values. These records were removed", action = NULL, duration = 10, closeButton = T, id = "removing_NA", type = "warning", session = getDefaultReactiveDomain())
        }
        if (anyNA(table2)) {
          table2 <- na.omit(table2)
          showNotification("The secondary input file contains records with NA/NaN values. These records were removed", action = NULL, duration = 10, closeButton = T, id = "removing_NA_secondary", type = "warning", session = getDefaultReactiveDomain())
        }
        # Resampling the series
        if (input$average) {
          if (nchar(input$step) > 0 && is.na(inputs$step)) {
            showNotification("The re-sampling period is not numeric. Check input value.", action = NULL, duration = 10, closeButton = T, id = "bad_window", type = "error", session = getDefaultReactiveDomain())
          } else if (isTruthy(inputs$step)) {
            if (inputs$step >= 2*min(diff(table$x,1)) && inputs$step <= (max(table$x) - min(table$x))/2) {
              showNotification("Averaging the series. This may take a while ...", action = NULL, duration = NULL, closeButton = F, id = "averaging", type = "warning", session = getDefaultReactiveDomain())
              tolerance <- min(diff(table$x,1))/3
              if (input$format == 4) {
                if (info$step == inputs$step) {
                  averaged <- sapply(1:as.integer((max(table$x) - min(table$x))/inputs$step), function(p) average(p, x = table$x, y1 = table$y1, y2 = NULL, y3 = NULL, sy1 = table$sy1, sy2 = NULL, sy3 = NULL, tol = tolerance ), simplify = T)
                } else {
                  averaged <- sapply(1:as.integer((max(table$x) - min(table$x))/inputs$step), function(p) average(p, x = table$x[values$used1], y1 = table$y1[values$used1], y2 = NULL, y3 = NULL, sy1 = table$sy1[values$used1], sy2 = NULL, sy3 = NULL, tol = tolerance ), simplify = T) 
                }
                removeNotification(id = "averaging", session = getDefaultReactiveDomain())
                table <- data.frame(x = averaged[1,], y1 = averaged[2,], sy1 = averaged[3,]) 
              } else {
                if (info$step == inputs$step) {
                  averaged <- sapply(1:as.integer((max(table$x) - min(table$x))/inputs$step), function(p) average(p, x = table$x, y1 = table$y1, y2 = table$y2, y3 = table$y3, sy1 = table$sy1, sy2 = table$sy2, sy3 = table$sy3, tol = tolerance ), simplify = T)
                } else {
                  averaged <- sapply(1:as.integer((max(table$x) - min(table$x))/inputs$step), function(p) average(p, x = table$x[values$used1], y1 = table$y1[values$used1], y2 = table$y2[values$used2], y3 = table$y3[values$used3], sy1 = table$sy1[values$used1], sy2 = table$sy2[values$used2], sy3 = table$sy3[values$used3], tol = tolerance ), simplify = T)  
                }
                removeNotification(id = "averaging", session = getDefaultReactiveDomain())
                table <- data.frame(x = averaged[1,], y1 = averaged[2,], y2 = averaged[3,], y3 = averaged[4,], sy1 = averaged[5,], sy2 = averaged[6,], sy3 = averaged[7,])  
              }
              table <- na.omit(table)
            } else {
              showNotification("The re-sampling period is not valid. Check input value.", action = NULL, duration = 10, closeButton = T, id = "bad_window", type = "error", session = getDefaultReactiveDomain())
            }
          }
        }
        # Checking for simultaneous values and setting series limits
        if (nrow(table) > 0) {
          if (input$tab == 4) {
            NULL
          } else {
            if (any(diff(table$x) <= 0)) {
              showNotification("Negative or null increment in abscissa (probably 2 or more points at the same epoch).", action = NULL, duration = 10, closeButton = T, id = "bad_x", type = "error", session = getDefaultReactiveDomain())
              NULL
            } else {
              info$minx <- min(table$x, na.rm = T)
              info$maxx <- max(table$x, na.rm = T)
              ranges$x1 <- c(info$minx, info$maxx)
              table
            }
          }
        } else {
          showNotification("The input data file is empty or contains wrong data. Check if all columns contain the same amount of numeric values.", action = NULL, duration = 10, closeButton = T, id = "bad_series", type = "error", session = getDefaultReactiveDomain())
          NULL
        }
      } else {
        showNotification("The input data file is empty or it does not match the requested format.", action = NULL, duration = 10, closeButton = T, id = "bad_series", type = "error", session = getDefaultReactiveDomain())
        NULL
      }
    } else {
      showNotification("The input data file is empty or contains wrong data. Check all columns contain the same amount of numeric values.", action = NULL, duration = 10, closeButton = T, id = "bad_series", type = "error", session = getDefaultReactiveDomain())
      NULL
    }
  }
  get_columns <- function(file,sep,format) {
    if (any(grepl("RINEX VERSION / TYPE", readLines(file, n = 3)))) {
      showNotification("Hello my friend! It seems you uploaded a RINEX file. Please, consider uploading a time series instead ... everything will be funnier!", action = NULL, duration = 15, closeButton = T, id = "rinex_file", type = "error", session = getDefaultReactiveDomain())
      return(0)
    }
    if (format == 1) { #NEU/ENU
      skip <- 0
    } else if (format == 2) { #PBO
      skip <- try(which(grepl("YYYYMMDD HHMMSS JJJJJ.JJJJ", readLines(file, warn = F))), silent = F)
    } else if (format == 3) { #NGL
      skip <- try(which(grepl("site YYMMMDD", readLines(file, warn = F))), silent = F)
    } else if (format == 4) { #1D
      skip <- 0
    }
    if (!isTruthy(skip)) {
      showNotification("Unable to read the expeced header from PBO/NGL series. Check the requested series format", action = NULL, duration = 15, closeButton = T, id = "no_head", type = "error", session = getDefaultReactiveDomain())
      return(0)
    }
    columns <- try(range(count.fields(file, sep = sep, comment.char = "#", skip = skip)), silent = F)
    if (isTruthy(columns))  {
      if (format != 2 && columns[1] != columns[2]) {
        showNotification("The input file contains different number of columns per row. Check the requested input file format. It may contain uncommented text strings.", action = NULL, duration = 15, closeButton = T, id = "bad_columns", type = "error", session = getDefaultReactiveDomain())
        return(0)
      }
      columns <- columns[1]
      if (isTruthy(columns)) {
        if (format == 1) { #NEU/ENU
          if (isTruthy(input$sigmas)) {
            if (columns < 7) {
              showNotification("The number of columns in the input ENU/NEU file is less than 7. Check the series format.", action = NULL, duration = 10, closeButton = T, id = "bad_columns", type = "error", session = getDefaultReactiveDomain())
              return(0)
            }
          } else {
            if (columns < 4) {
              showNotification("The number of columns in the input ENU/NEU file is less than 4. Check the series format.", action = NULL, duration = 10, closeButton = T, id = "bad_columns", type = "error", session = getDefaultReactiveDomain())
              return(0)
            }
          }
        } else if (format == 2) { #PBO
          if (columns < 24) {
            showNotification("The number of columns in the input PBO file is less than 24. Check the series format.", action = NULL, duration = 10, closeButton = T, id = "bad_columns", type = "error", session = getDefaultReactiveDomain())
            return(0)
          }
        } else if (format == 3) { #NGL (on May 19, 2022: 3 columns were added to the tenv3 format; before it had only 20 columns)
          if (columns < 20) {
            showNotification("The number of columns in the input NGL file is less than 20. Check the series format.", action = NULL, duration = 10, closeButton = T, id = "bad_columns", type = "error", session = getDefaultReactiveDomain())
            return(0)
          }
        } else if (format == 4) { # 1D
          if (isTruthy(input$sigmas)) {
            if (columns < 3) {
              showNotification("The number of columns in the input file is less than 3. Check the series format.", action = NULL, duration = 10, closeButton = T, id = "bad_columns", type = "error", session = getDefaultReactiveDomain())
              return(0)
            }
          } else {
            if (columns < 2) {
              showNotification("The number of columns in the input file is less than 2. Check the series format.", action = NULL, duration = 10, closeButton = T, id = "bad_columns", type = "error", session = getDefaultReactiveDomain())
              return(0)
            }
          }
        } 
      }
    } else {
      showNotification("Impossible to read the columns from the input file. Check the requested input file format.", action = NULL, duration = 15, closeButton = T, id = "bad_columns", type = "error", session = getDefaultReactiveDomain())
      req(info$stop)
    }
    columns
  }
  extract_table <- function(file,sep,format,columns,epoch,variable,errorBar) {
    tableAll <- NULL
    extracted <- NULL
    if (format == 1) { #NEU/ENU
      skip <- 0
      tableAll <- try(read.table(file, comment.char = "#", sep = sep, skip = skip), silent = F)
      if (isTruthy(tableAll)) {
        extracted <- tableAll[,c(1,2,3,4)]
        names(extracted) <- c("x","y1","y2","y3")
        if (length(extracted) > 0) {
          if (columns > 4) {
            extracted$sy1 <- tableAll[,5]
            extracted$sy2 <- tableAll[,6]
            extracted$sy3 <- tableAll[,7]
          } else {
            extracted$sy1 <- extracted$sy2 <- extracted$sy3 <- rep(1,length(extracted$x))
            info$errorbars <- F
          }
          extracted <- suppressWarnings(extracted[apply(extracted, 1, function(r) !any(is.na(as.numeric(r)))) ,])
        }
      }
    } else if (format == 2) { #PBO
      skip <- which(grepl("YYYYMMDD HHMMSS JJJJJ.JJJJ", readLines(file, warn = F)))
      tableAll <- try(read.table(file, comment.char = "#", sep = sep, skip = skip), silent = F)
      if (isTruthy(tableAll)) {
        extracted <- tableAll[,c(16,17,18,19,20,21)]
        names(extracted) <- c("y1","y2","y3","sy1","sy2","sy3")
        if (input$tunits == 1) {
          extracted$x <- tableAll[,3]
        } else if (input$tunits == 2) {
          showNotification("There is no time units \"weeks\" in a the PBO format. Check the requested input file format.", action = NULL, duration = 15, closeButton = T, id = "no_weeks", type = "error", session = getDefaultReactiveDomain())
          updateRadioButtons(session, inputId = "units", label = "Time units", choices = list("Days" = 1, "Weeks" = 2, "Years" = 3), selected = 3, inline = F)
          req(info$stop)
        } else if (input$tunits == 3) {
          extracted$x <- decimal_date(strptime(paste(sprintf("%08d",tableAll[,1]),sprintf("%06d",tableAll[,2])),format = '%Y%m%d %H%M%S'))
        }
      }
    } else if (format == 3) { #NGL
      skip <- which(grepl("site YYMMMDD", readLines(file, warn = F)))
      tableAll <- try(read.table(file, comment.char = "#", sep = sep, skip = skip), silent = F)
      if (isTruthy(tableAll)) {
        if (input$tunits == 1) {
          extracted <- data.frame( x = tableAll[,4] )
        } else if (input$tunits == 2) {
          extracted <- data.frame( x = tableAll[,5] + tableAll[,6]/7 )
        } else if (input$tunits == 3) {
          extracted <- data.frame( x = tableAll[,3] )
        }
        extracted$y1 <- tableAll[,8] - tableAll[1,8] + tableAll[,9] #East (the coordinate integer portion seems to be constant, but just in case)
        extracted$y2 <- tableAll[,10] - tableAll[1,10] + tableAll[,11] #North
        extracted$y3 <- tableAll[,12] - tableAll[1,12] + tableAll[,13] #Up
        extracted$sy1 <- tableAll[,15]
        extracted$sy2 <- tableAll[,16]
        extracted$sy3 <- tableAll[,17]
      }
    } else if (format == 4) { #1D
      if (!is.na(epoch) && is.numeric(epoch) && epoch > 0 && epoch <= columns && !is.na(variable) && is.numeric(variable) && variable > 0 && variable <= columns && epoch != variable) {
        skip <- 0
        tableAll <- try(read.table(file, comment.char = "#", sep = sep, skip = skip), silent = F)
        if (isTruthy(tableAll)) {
          extracted <- data.frame( x = tableAll[[epoch]] )
          extracted$y1 <- tableAll[[variable]]
          if (columns > 2) {
            if (input$sigmas == T) {
              if (!is.na(errorBar) && is.numeric(errorBar) && errorBar > 0 && errorBar <= columns && errorBar != epoch && errorBar != variable) {
                extracted$sy1 <- tableAll[[errorBar]]
              } else {
                showNotification("Invalid column number for the series error bars. Provide a valid column number or uncheck the error bars option.", action = NULL, duration = 10, closeButton = T, id = "no_error_bars", type = "error", session = getDefaultReactiveDomain())
                req(info$stop)
              }
            } else {
              extracted$sy1 <- rep(1,length(extracted$x))
            }
          } else {
            extracted$sy1 <- rep(1,length(extracted$x))
            info$errorbars <- F
          }
          if (isTruthy(extracted)) {
            extracted <- suppressWarnings(extracted[apply(extracted, 1, function(r) !any(is.na(as.numeric(r)))) ,])
          }
        }
      }
    }
    if (input$euler && input$eulerType > 0) {
      stationCartesian <- c()
      stationGeo <- c()
      poleCartesian <- c()
      vup <- (mean(extracted$y3[-1*as.integer(length(extracted$x*0.1)):length(extracted$x)]) - mean(extracted$y3[1:as.integer(length(extracted$x*0.1))])) / (mean(extracted$x[-1*as.integer(length(extracted$x*0.1)):length(extracted$x)]) - mean(extracted$x[1:as.integer(length(extracted$x*0.1))]))
      if (abs(vup) > 0.05 && sd(extracted$y3 - vup*(extracted$x - mean(extracted$x))) > 0.05) {
        scaling <- 1000
      } else {
        scaling <- 1
      }
      if (input$coordenadas_estacion == 2) {
        stationGeo <- c(inputs$station_lat*pi/180,inputs$station_lon*pi/180)
        stationCartesian <- do.call(latlon2xyz,as.list(c(stationGeo,scaling)))
      } else {
        stationCartesian <- c(inputs$station_x,inputs$station_y,inputs$station_z)
        stationGeo <- do.call(xyz2llh,as.list(stationCartesian))
      }
      if (input$pole_coordinates == 2) {
        poleCartesian <- inputs$pole_rot*degMa2radyr * c(cos(inputs$pole_lat*pi/180)*cos(inputs$pole_lon*pi/180),cos(inputs$pole_lat*pi/180)*sin(inputs$pole_lon*pi/180),sin(inputs$pole_lat*pi/180))
      } else {
        poleCartesian <- c(inputs$pole_x,inputs$pole_y,inputs$pole_z)*degMa2radyr
      }
      if (length(stationCartesian[!is.na(stationCartesian)]) == 3 && length(stationGeo[!is.na(stationGeo)]) == 2 && length(poleCartesian[!is.na(poleCartesian)]) == 3) {
        if (stationGeo[1] < -90 || stationGeo[1] > 90 || stationGeo[2] > 360 || stationGeo[2] < -360) {
          showNotification("Station coordinates out of bounds. Check the input values.", action = NULL, duration = 15, closeButton = T, id = "bad_coordinates", type = "error", session = getDefaultReactiveDomain())
          updateRadioButtons(session, inputId = "eulerType", label = NULL, choices = list("None" = 0, "Show" = 1, "Remove" = 2), selected = 0, inline = T)
          req(info$stop)
        }
        if (sqrt(stationCartesian[1]^2 + stationCartesian[2]^2 + stationCartesian[3]^2) < 6355000*scaling || sqrt(stationCartesian[1]^2 + stationCartesian[2]^2 + stationCartesian[3]^2) > 6385000*scaling) {
          showNotification("Station coordinates out of bounds. Check the input values.", action = NULL, duration = 15, closeButton = T, id = "bad_coordinates", type = "error", session = getDefaultReactiveDomain())
          updateRadioButtons(session, inputId = "eulerType", label = NULL, choices = list("None" = 0, "Show" = 1, "Remove" = 2), selected = 0, inline = T)
          req(info$stop)
        }
        if (sqrt(poleCartesian[1]^2 + poleCartesian[2]^2 + poleCartesian[3]^2) > 2) {
          showNotification("Euler pole parameters out of bounds. Check the input values.", action = NULL, duration = 15, closeButton = T, id = "bad_pole", type = "error", session = getDefaultReactiveDomain())
          updateRadioButtons(session, inputId = "eulerType", label = NULL, choices = list("None" = 0, "Show" = 1, "Remove" = 2), selected = 0, inline = T)
          req(info$stop)
        }
        plateCartesian <- cross(poleCartesian,stationCartesian)
        rotation <- matrix(data = c(-1*sin(stationGeo[1])*cos(stationGeo[2]),-1*sin(stationGeo[2]),-1*cos(stationGeo[1])*cos(stationGeo[2]),-1*sin(stationGeo[1])*sin(stationGeo[2]),cos(stationGeo[2]),-1*cos(stationGeo[1])*sin(stationGeo[2]),cos(stationGeo[1]),0,-1*sin(stationGeo[1])), nrow = 3, ncol = 3)
        plate_neu <- c(rotation %*% plateCartesian)
        if ((format == 1 && input$neuenu == 1) || format == 2) { #NEU & PBO
          trans$plate <- plate_neu
        } else if ((format == 1 && input$neuenu == 2) || format == 3) { #ENU & NGL
          trans$plate <- c(plate_neu[2],plate_neu[1],plate_neu[3])
        }
        if (input$eulerType == 2) {
          extracted$y1 <- extracted$y1 - trans$plate[1]*(extracted$x - mean(extracted$x))
          extracted$y2 <- extracted$y2 - trans$plate[2]*(extracted$x - mean(extracted$x))
          extracted$y3 <- extracted$y3 - trans$plate[3]*(extracted$x - mean(extracted$x))
        }
      } else {
        showNotification("Problem reading the station coordinates and/or the Euler pole parameters. Check the input values.", action = NULL, duration = 15, closeButton = T, id = "no_rotation", type = "warning", session = getDefaultReactiveDomain())
        updateRadioButtons(session, inputId = "eulerType", label = NULL, choices = list("None" = 0, "Show" = 1, "Remove" = 2), selected = 0, inline = T)
      }
    }
    if (!is.null(extracted) && all(sapply(extracted, is.numeric))) {
      extracted
    } else {
      showNotification("Non numeric values extracted from the input file. Check the input file or the requested format.", action = NULL, duration = 15, closeButton = T, id = "no_values", type = "error", session = getDefaultReactiveDomain())
      req(info$stop)
    }
  }
  latlon2xyz <- function(lat,lon,scaling) {
    a <- 6378137
    b <- 6356752.314140347
    e2 <- (a^2 - b^2) / a^2
    N <- a / sqrt( 1 - e2 * sin(lat)^2)
    x <- N * cos(lat) * cos(lon)
    y <- N * cos(lat) * sin(lon)
    z <- N * (1 - e2) * sin(lat)
    c(x,y,z) * scaling
  }
  xyz2llh <- function(x,y,z) {
    a <- 6378137
    b <- 6356752.314140347
    e2 <- (a^2 - b^2) / a^2
    lon <- atan2(y,x)
    lat <- atan2(z*cos(lon),x*(1 - e2))
    if (lat > pi/2) {
      lat <- lat - pi
    } else if (lat < -1*pi/2) {
      lat <- lat + pi
    }
    c(lat,lon)
  }
  model <- function(x,y) {
    isolate({
      model <- model_lm <- "y ~"
      model_kf <- ""
      j <- 1
      apriori <- list()
      error <- list()
      nouns <- list()
      processNoise <- 0
      info$run <- F
      y_detrend <- NULL
      # * Linear model ####
      if ("Linear" %in% input$model) {
        if (!is.na(inputs$trendRef)) {
          reft <- inputs$trendRef
        } else {
          if (input$fitType == 1) {
            reft <- mean(x, na.rm = T)
          } else if (input$fitType == 2) {
            reft <- x[1]
          }
          updateTextInput(session, "trendRef", value = reft)
          if (input$fitType == 1) {
            req(info$stop)
          }
        }
        text_rate <- sprintf("%f",as.numeric(reft))
        if (input$fitType == 2) {
          if (nchar(input$TrendDev) > 0) {
            if (!is.na(suppressWarnings(as.numeric(input$TrendDev)))) {
              if (suppressWarnings(as.numeric(input$TrendDev)) > 0) {
                model <- paste(model, paste0("Intercept + Rate*dx"), sep = " ")
                noise <- as.numeric(input$TrendDev)
              } else if (suppressWarnings(as.numeric(input$TrendDev)) == 0) {
                noise <- 0
                model <- paste(model, paste0("Intercept + Rate*(x-",text_rate,")"), sep = " ")
              } else {
                showNotification("The process noise for the trend is not valid. Check the input value.", action = NULL, duration = 15, closeButton = T, id = "bad_rate_noise", type = "error", session = getDefaultReactiveDomain())
                return(NULL)                
              }
            } else {
              showNotification("The process noise for the trend is not valid. Check the input value.", action = NULL, duration = 15, closeButton = T, id = "bad_rate_noise", type = "error", session = getDefaultReactiveDomain())
              return(NULL)
            }
          } else {
            updateTextInput(session, "TrendDev", value = "0.0")
            noise <- 0
            model <- paste(model, paste0("Intercept + Rate*(x-",text_rate,")"), sep = " ")
            showNotification("The process noise value for the trend is missing. Using a value of zero.", action = NULL, duration = 10, closeButton = T, id = "missing_rate_noise", type = "warning", session = getDefaultReactiveDomain())
          }
        } else {
          model <- paste(model, paste0("Intercept + Rate*(x-",text_rate,")"), sep = " ")
        }
        model_lm <- paste(model_lm, "x", sep = " ")
        model_kf_inst <- paste(model_kf, paste0("e[k,",j,"] + e[k,",j + 1,"]*(x[k] - x[k-1])"), sep = " ")
        model_kf_mean <- paste(model_kf, paste0("e[k,",j,"] + e[k,",j + 1,"]*(x[k]-",text_rate,")"), sep = " ")
        j <- j + 2
        if (identical(input$Trend0,character(0)) || is.na(input$Trend0) || input$Trend0 == "" || input$Trend0 == " ") {
          tenth <- ceiling(length(y)/10)
          if (input$fitType == 1) {
            fastFit <- lm(y ~ x)
          } else {
            fastFit <- try(lm(y[1:tenth]~x[1:tenth]), silent = F)
          }
          if (isTruthy(fastFit)) {
            ap_rate <- summary(fastFit)$coefficients[2,1]
            sigma_rate <- summary(fastFit)$coefficients[2,2] * 3
          } else {
            ap_rate <- (mean(tail(y, n = tenth), na.rm = T) - mean(y[1:tenth], na.rm = T))/(mean(tail(x, n = tenth), na.rm = T) - mean(x[1:tenth], na.rm = T))
            sigma_rate <- ap_rate * 5
          }
          if (input$fitType == 2) {
            updateTextInput(session, "Trend0", value = ap_rate)
            updateTextInput(session, "eTrend0", value = sigma_rate)
          }
        } else {
          ap_rate <- as.numeric(input$Trend0)
          if (input$eTrend0 == 0) {
            showNotification("The a priori trend error is zero. Check the input value.", action = NULL, duration = 15, closeButton = T, id = "no_trend_error", type = "error", session = getDefaultReactiveDomain())
            req(info$stop)
          } else {
            sigma_rate <- as.numeric(input$eTrend0)  
          }
        }
        if (input$fitType == 1) {
          y_detrend <- y - (x - reft) * ap_rate
          ap_intercept <- mean(y_detrend, na.rm = T)
          sigma_intercept <- sd(y_detrend/sqrt(length(y)), na.rm = T)
          if (sigma_intercept <= 0) {
            sigma_intercept <- 1
          }
        } else if (input$fitType == 2) {
          if (identical(input$Intercept0,character(0)) || is.na(input$Intercept0) || input$Intercept0 == "" || input$Intercept0 == " ") {
            if (isTruthy(match("Intercept", trans$names))) {
              ap_intercept <- trans$LScoefs[match("Intercept", trans$names)]
              sigma_intercept <- abs(as.numeric(trans$LScoefs[match("Intercept", trans$names)*2]/sqrt(length(trans$x))))
            } else {
              ap_intercept <- y[1]
              sigma_intercept <- info$noise
            }
            updateTextInput(session, "Intercept0", value = ap_intercept)
            updateTextInput(session, "eIntercept0", value = sigma_intercept)
          } else {
            ap_intercept <- as.numeric(input$Intercept0)
            if (input$eIntercept0 == 0) {
              showNotification("The a priori intercept error is zero. Check the input value.", action = NULL, duration = 15, closeButton = T, id = "no_intercept_error", type = "error", session = getDefaultReactiveDomain())
              req(info$stop)
            } else {
              sigma_intercept <- as.numeric(input$eIntercept0)
            }
          }
          processNoise <- c(processNoise, as.numeric(noise)^2)
          error <- c(error, Intercept = as.numeric(sigma_intercept), Rate = as.numeric(sigma_rate))
          nouns <- c(nouns, "Intercept", "Rate")
        }
        apriori <- c(apriori, Intercept = as.numeric(ap_intercept), Rate = as.numeric(ap_rate))
        info$run <- T
      } else {
        model <- paste(model, "Intercept", sep = " ")
        model_lm <- paste(model_lm, "1", sep = " ")
        model_kf_inst <- model_kf_mean <- paste(model_kf, "e[k,",j,"]", sep = " ")
        j <- j + 1
        if (isTruthy(match("Intercept", trans$names))) {
          if (input$fitType == 1) {
            ap_intercept <- trans$LScoefs[match("Intercept", trans$names)] - trans$ordinate 
          } else if (input$fitType == 2) {
            ap_intercept <- trans$LScoefs[match("Intercept", trans$names)]
          }
          sigma_intercept <- abs(as.numeric(trans$LScoefs[match("Intercept", trans$names)*2]/sqrt(length(trans$x))))
        } else {
          ap_intercept <- mean(y, na.rm = T)
          sigma_intercept <- sd(y, na.rm = T)
        }
        apriori <- c(apriori, Intercept = as.numeric(ap_intercept))
        error <- c(error, Intercept = as.numeric(sigma_intercept))
        nouns <- c(nouns, "Intercept")
      }
      # * Sinusoidal model ####
      if ("Sinusoidal" %in% input$model) {
        periods <- unlist(strsplit(inputs$period, split = ","))
        S0 <- unlist(strsplit(input$S0, split = ","))
        eS0 <- unlist(strsplit(input$eS0, split = ","))
        sigamp <- unlist(strsplit(input$SinusoidalDev, split = ","))
        if (!is.na(inputs$periodRef)) {
          refs <- inputs$periodRef
        } else {
          if (input$fitType == 1) {
            refs <- mean(x, na.rm = T)
          } else if (input$fitType == 2) {
            refs <- x[1]
          }
          updateTextInput(session, "periodRef", value = refs)
          if (input$fitType == 1) {
            req(info$stop)
          }
        }
        if (length(periods) > 0) {
          i <- 0
          for (p in periods) {
            f <- NULL
            i <- i + 1
            if (grepl("d",p)) {
              f <- gsub("d", "", p)
              if (nchar(f) > 0 && !is.na(as.numeric(f))) {
                if (input$tunits == 1) {
                  f <- 1/as.numeric(f)
                } else if (input$tunits == 2) {
                  f <- 7/as.numeric(f)
                } else if (input$tunits == 3) {
                  f <- daysInYear/as.numeric(f)
                }
              } else {
                f <- NULL
              }
            } else if (grepl("w",p)) {
              f <- gsub("w", "", p)
              if (nchar(f) > 0 && !is.na(as.numeric(f))) {
                if (input$tunits == 1) {
                  f <- (1/as.numeric(f))*7
                } else if (input$tunits == 2) {
                  f <- (1/as.numeric(f))*1
                } else if (input$tunits == 3) {
                  f <- 1/as.numeric(f)*7/daysInYear
                }
              } else {
                f <- NULL
              }
            } else if (grepl("y",p)) {
              f <- gsub("y", "", p)
              if (nchar(f) > 0  && !is.na(as.numeric(f))) {
                if (input$tunits == 1) {
                  f <- (1/as.numeric(f))*1/daysInYear
                } else if (input$tunits == 2) {
                  f <- (1/as.numeric(f))*7/daysInYear
                } else if (input$tunits == 3) {
                  f <- 1/as.numeric(f)
                }
              } else {
                f <- NULL
              }
            }
            if (length(f) > 0 && f < 1/(2*info$sampling) && f > 1/(10*abs(info$rangex))) {
              if (f < 1/abs(info$rangex)) {
                showNotification(paste0("At least one of the input sinusoidal periods is larger than the series length (",format(info$rangex,nsmall = info$decimalsx, digits = info$decimalsx, trim = F,scientific = F)," ",info$tunits,"). Fitting results may be unreliable."), action = NULL, duration = 10, closeButton = T, id = "bad_sinusoidal_period", type = "warning", session = getDefaultReactiveDomain())
              }
              info$run <- T
              label_sin <- paste("S",i,sep = "")
              label_cos <- paste("C",i,sep = "")
              text_sin <- sprintf("I(sin(2*pi*(x-%f)*%f))",as.numeric(refs),f)
              text_cos <- sprintf("I(cos(2*pi*(x-%f)*%f))",as.numeric(refs),f)
              text_sin_kf <- sprintf("sin(2*pi*(x[k]-%f)*%f)",as.numeric(refs),f)
              text_cos_kf <- sprintf("cos(2*pi*(x[k]-%f)*%f)",as.numeric(refs),f)
              text_sin_lm <- sprintf("sin(2*pi*x*%f)",f)
              text_cos_lm <- sprintf("cos(2*pi*x*%f)",f)
              model <- paste(model, paste(label_sin,text_sin,sep = "*"), sep = " + ")
              model_lm <- paste(model_lm, text_sin_lm, text_cos_lm, sep = " + ")
              model_kf_inst <- paste(model_kf_inst, paste(paste0("e[k,",j,"]"),text_sin_kf,sep = "*"), sep = " + ")
              model_kf_mean <- paste(model_kf_mean, paste(paste0("e[k,",j,"]"),text_sin_kf,sep = "*"), sep = " + ")
              j <- j + 1
              model <- paste(model, paste(label_cos,text_cos,sep = "*"), sep = " + ")
              model_kf_inst <- paste(model_kf_inst, paste(paste0("e[k,",j,"]"),text_cos_kf,sep = "*"), sep = " + ")
              model_kf_mean <- paste(model_kf_mean, paste(paste0("e[k,",j,"]"),text_cos_kf,sep = "*"), sep = " + ")
              j <- j + 1
              if (length(y_detrend) > 0) {
                y_now <- y_detrend
              } else {
                y_now <- y - median(y)
              }
              if (identical(S0,character(0)) || is.na(S0[i]) || S0[i] == "" || S0[i] == " ") {
                S0[i] <- quantile(y_now, probs = 0.95)/(4*sqrt(2))
                eS0[i] <- as.numeric(S0[i])/2
                if (input$fitType == 2) {
                  if (isTruthy(match(paste0("S",i), trans$names))) {
                    s <- trans$LScoefs[match(paste0("S",i), trans$names)]
                    c <- trans$LScoefs[match(paste0("C",i), trans$names)]
                    S0[i] <- mean(c(as.numeric(s),as.numeric(c)))
                    eS0[i] <- abs(as.numeric(S0[i]))
                  }
                }
              }
              apriori[[label_sin]] <- as.numeric(S0[i])
              error[[label_sin]] <- as.numeric(eS0[i])
              nouns <- c(nouns, label_sin)
              apriori[[label_cos]] <- as.numeric(S0[i])
              if (eS0[i] == 0) {
                info$run <- F
                showNotification("At least one of the a priori sinusoidal amplitude errors is zero. Check the input value.", action = NULL, duration = 15, closeButton = T, id = "bad_amplitude_error", type = "error", session = getDefaultReactiveDomain())
                req(info$stop)
              } else {
                error[[label_cos]] <- as.numeric(eS0[i])
              }
              nouns <- c(nouns, label_cos)
              if (input$fitType == 2) {
                if (isTruthy(sigamp[i])) {
                  if (!is.na(suppressWarnings(as.numeric(sigamp[i]))) && suppressWarnings(as.numeric(sigamp[i]) >= 0)) {
                    if (input$SineCosine == 1) {
                      processNoise <- c(processNoise, as.numeric(sigamp[i])^2)
                      processNoise <- c(processNoise, 0)
                    } else if (input$SineCosine == 2) {
                      processNoise <- c(processNoise, as.numeric(sigamp[i])^2)
                      processNoise <- c(processNoise, as.numeric(sigamp[i])^2)
                    }
                  } else {
                    showNotification(paste("The process noise value for the sinusoid ",i," is not valid. Check the input values."), action = NULL, duration = 10, closeButton = T, id = "bad_sinusoidal_noise", type = "error", session = getDefaultReactiveDomain())
                    return(NULL)
                  }
                } else {
                  showNotification(paste("The process noise value for the sinusoid ",i," is missing. Using a value of zero."), action = NULL, duration = 10, closeButton = T, id = "missing_sinusoidal_noise", type = "warning", session = getDefaultReactiveDomain())
                  processNoise <- c(processNoise, 0)
                  processNoise <- c(processNoise, 0)
                }
              }
            } else {
              showNotification(paste("The period for sinusoid ",i," is way out of the data bounds and has been neglected."), action = NULL, duration = 10, closeButton = T, id = "bad_sinusoidal_period", type = "warning", session = getDefaultReactiveDomain())
            }
          }
          line_S0 <- paste(sprintf("%.7f",as.numeric(S0)), collapse = ", ")
          line_eS0 <- paste(sprintf("%.7f",as.numeric(eS0)), collapse = ", ")
          updateTextInput(session, "S0", value = line_S0)
          updateTextInput(session, "eS0", value = line_eS0)
        }
      }
      # * Offset model ####
      if ("Offset" %in% input$model) {
        offsetEpochs <- unlist(strsplit(inputs$offsetEpoch, split = ","))
        O0 <- unlist(strsplit(input$O0, split = ","))
        eO0 <- unlist(strsplit(input$eO0, split = ","))
        trans$offsetEpochs <- NULL
        i <- 0
        if (length(offsetEpochs) > 0) {
          for (p in offsetEpochs) {
            p <- as.numeric(trimmer(p))
            if (nchar(p) > 0 && !is.na(p)) {
              if (trans$x[1] < p && p < trans$x[length(trans$x)]) {
                trans$offsetEpochs <- c(trans$offsetEpochs, p)
                info$run <- T
                i <- i + 1
                label <- paste0("O",i)
                text <- sprintf("I(x>%s)",p)
                model <- paste(model, paste(label,text,sep = "*"), sep = " + ")
                model_lm <- paste(model_lm, text, sep = " + ")
                model_kf_inst <- paste(model_kf_inst, paste0("e[k,",j,"]*I(x[k]>",p,")"), sep = " + ")
                model_kf_mean <- paste(model_kf_mean, paste0("e[k,",j,"]*I(x[k]>",p,")"), sep = " + ")
                j <- j + 1
                if (identical(O0,character(0)) || is.na(O0[i]) || O0[i] == "" || O0[i] == " ") {
                  if (length(y_detrend) > 0) {
                    y_now <- y_detrend
                  } else {
                    y_now <- y
                  }
                  O0[i] <- y_now[which.max(x >= p)] - y_now[which.max(x >= p) - 1]
                  if (!isTruthy(O0[i])) {
                    O0[i] <- 0
                  }
                  if (input$tunits == 1) {
                    sample <- 1/12 * 365
                  } else if (input$tunits == 2) {
                    sample <- 1/12 * 365/7
                  } else if (input$tunits == 3) {
                    sample <- 1/12
                  }
                  eO0[i] <- ( sd(y_now[which.max(trans$x > p) - sample & which.min(trans$x < p)]) + sd(y_now[which.max(trans$x > p) & which.min(trans$x < p) + sample]) ) / 2
                  if (!isTruthy(eO0[i])) {
                    eO0[i] <- 1
                  }
                  if (input$fitType == 2) {
                    if (isTruthy(match(paste0("O",i), trans$names))) {
                      O0[i] <- trans$LScoefs[match(paste0("O",i), trans$names)]
                      eO0[i] <- abs(as.numeric(trans$LScoefs[match(paste0("O",i), trans$names) + length(trans$names)]/sqrt(length(trans$x))))
                    }
                    line_O0 <- paste(sprintf("%.7f",as.numeric(O0)), collapse = ", ")
                    line_eO0 <- paste(sprintf("%.7f",as.numeric(eO0)), collapse = ", ")
                    updateTextInput(session, "O0", value = line_O0)
                    updateTextInput(session, "eO0", value = line_eO0)
                  }
                }
                apriori[[label]] <- as.numeric(O0[i])
                error[[label]] <- as.numeric(eO0[i])
                nouns <- c(nouns, label)
                if (input$fitType == 2) {
                  processNoise <- c(processNoise, 0)
                }
              }
            } else {
              showNotification(paste0("The epoch given for offset #",i + 1," is not valid. Check the input values."), action = NULL, duration = 10, closeButton = T, id = "bad_offset_epoch", type = "error", session = getDefaultReactiveDomain())
              info$run <- F
              req(info$stop)
            }
          } 
        } else {
          trans$offsetEpochs <- NULL
        }
      } else {
        trans$offsetEpochs <- NULL
      }
      # * Exponential model ####
      if ("Exponential" %in% input$model) {
        expos <- unlist(strsplit(inputs$ExponenRef, split = ","))
        E0 <- unlist(strsplit(inputs$E0, split = ","))
        eE0 <- unlist(strsplit(input$eE0, split = ","))
        TE0 <- unlist(strsplit(inputs$TE0, split = ","))
        eTE0 <- unlist(strsplit(input$eTE0, split = ","))
        if (length(expos) > 0) {
          i <- 0
          update <- 0
          if (length(expos) < length(E0) || length(expos) < length(TE0)) {
            E0 <- ""
            TE0 <- ""
            update <- 1
          }
          for (refe in expos) {
            refe <- trimmer(refe)
            i <- i + 1
            if (nchar(refe) > 0 && !is.na(as.numeric(refe))) {
              if (identical(E0,character(0)) || identical(TE0,character(0)) || is.na(E0[i]) || is.na(TE0[i]) || E0[i] == "" || TE0[i] == "" || E0[i] == " " || TE0[i] == " ") {
                update <- 1
                if (isTruthy(match(paste0("E",i), trans$names))) {
                  E0[i] <- trans$LScoefs[match(paste0("E",i), trans$names)]
                  TE0[i] <- trans$LScoefs[match(paste0("TauE",i), trans$names)]
                  eE0[i] <- trans$LScoefs[match(paste0("E",i), trans$names) + length(trans$names)]/sqrt(length(trans$x))
                  eTE0[i] <- trans$LScoefs[match(paste0("TauE",i), trans$names) + length(trans$names)]/sqrt(length(trans$x))
                } else {
                  if (input$tunits == 1) {
                    span <- 3 * 365
                    forward <- 1 * 365
                    sample <- 1/12 * info$sampling*365.25
                  } else if (input$tunits == 2) {
                    span <- 3 * 365/7
                    forward <- 1 * 365/7
                    sample <- 1/12 * info$sampling*365.25/7
                  } else if (input$tunits == 3) {
                    span <- 3
                    forward <- 1
                    sample <- 1/12 * info$sampling*365.25
                  }
                  if (length(trans$x[trans$x > as.numeric(refe) + 3*sample & trans$x <= as.numeric(refe) + span]) > 0 && length(trans$x[trans$x > as.numeric(refe) & trans$x < as.numeric(refe) + 3*sample]) > 0) {
                    apriori_y_before <- trans$y[trans$x > as.numeric(refe) - span & trans$x < as.numeric(refe)]
                    apriori_y_after <- trans$y[trans$x > as.numeric(refe) & trans$x <= as.numeric(refe) + span]
                    apriori_x_before <- trans$x[trans$x > as.numeric(refe) - span & trans$x < as.numeric(refe)]
                    apriori_x_after <- trans$x[trans$x > as.numeric(refe) & trans$x <= as.numeric(refe) + span] - as.numeric(refe)
                    if (length(apriori_x_before) > 3) {
                      fitBefore <- lm(apriori_y_before ~ apriori_x_before)
                      trendBefore <- fitBefore$coef[2]
                    } else {
                      trendBefore <- 0
                    }
                    flat <- apriori_y_after - apriori_x_after * trendBefore
                    convex <- lm(flat ~ poly(apriori_x_after, 2))
                    if (convex$coef[3] > 0) {
                      coeff <- 1
                    } else {
                      coeff <- -1
                    }
                    flat <- coeff * flat - min(coeff * flat) + 0.0000000000001
                    forward <- apriori_x_after[which.min(abs(apriori_x_after - forward))]
                    if (length(apriori_x_after[apriori_x_after < sample]) > 3) {
                      x0 <- apriori_x_after[apriori_x_after < sample]
                      y0 <- flat[apriori_x_after < sample]
                    } else if (length(apriori_x_after[apriori_x_after < 2*sample]) > 3) {
                      x0 <- apriori_x_after[apriori_x_after < 2*sample]
                      y0 <- flat[apriori_x_after < 2*sample]
                    } else if (length(apriori_x_after[apriori_x_after < 3*sample]) > 3) {
                      x0 <- apriori_x_after[apriori_x_after < 3*sample]
                      y0 <- flat[apriori_x_after < 3*sample]
                    } else {
                      x0 <- 0
                      y0 <- 0
                      showNotification("Not enough data to obtain the a priori values of the exponential decay. The a priori values must be provided to continue.", action = NULL, duration = 10, closeButton = T, id = "no_exponential", type = "warning", session = getDefaultReactiveDomain())
                    }
                    x1 <- apriori_x_after[apriori_x_after >= forward - sample/2 & apriori_x_after < forward + sample/2]
                    y1 <- flat[apriori_x_after >= forward - sample/2 & apriori_x_after < forward + sample/2]
                    E0[i] <- mean(y0) * coeff
                    if (!isTruthy(E0[i])) {
                      E0[i] <- 0
                    }
                    TE0[i] <- (forward - mean(x0))/(log(mean(y0)) - log(mean(y1)))
                    if (!isTruthy(TE0[i]) || TE0[i] < 0) {
                      TE0[i] <- abs(as.numeric(TE0[i]))
                    }
                    if (input$fitType == 1) {
                      #NA
                    } else if (input$fitType == 2) {
                      if (isTruthy(match(paste0("O",i), trans$names))) {
                        E0[i] <- trans$LScoefs[match(paste0("O",i), trans$names)]
                        eO0[i] <- abs(as.numeric(trans$LScoefs[match(paste0("O",i), trans$names) + length(trans$names)]/sqrt(length(trans$x))))
                      } else {
                        eE0[i] <- sd(y0)
                        eTE0[i] <- sqrt( ( -1*sd(y0)/(mean(y0) * log(mean(y0)/mean(y1))^2) )^2 + ( 1*sd(y1)/(mean(y1) * log(mean(y0)/mean(y1))^2) )^2 )
                      }
                    }
                  } else {
                    showNotification("Not enough data to obtain the a priori values of the exponential decay. The a priori values must be provided to continue.", action = NULL, duration = 10, closeButton = T, id = "no_exponential", type = "warning", session = getDefaultReactiveDomain())
                  }
                }
              }
            } else {
              if (is.na(E0[i]) || trimmer(E0[i]) == "NA" || trimmer(E0[i]) == "") {
                E0[i] <- NA
                TE0[i] <- NA
              } else {
                E0[i] <- NA
                TE0[i] <- NA
                update <- 1
              }
            }
          }
          if (update > 0) {
            line_E0 <- paste(sprintf("%.7f",as.numeric(E0)), collapse = ", ")
            line_TE0 <- paste(sprintf("%.7f",as.numeric(TE0)), collapse = ", ")
            updateTextInput(session, "E0", value = line_E0)
            updateTextInput(session, "TE0", value = line_TE0)
            if (input$fitType == 1) {
              req(info$stop)
            } else if (input$fitType == 2) {
              line_eE0 <- paste(sprintf("%.7f",as.numeric(eE0)), collapse = ", ")
              line_eTE0 <- paste(sprintf("%.7f",as.numeric(eTE0)), collapse = ", ")
              updateTextInput(session, "eE0", value = line_eE0)
              updateTextInput(session, "eTE0", value = line_eTE0)
            }
          }
          for (i in seq_len(length(E0))) {
            if (!is.na(as.numeric(E0[i]))) {
              info$run <- T
              label1 <- paste0("E",i)
              label2 <- paste0("TauE",i)
              text_exp <- sprintf("%f",as.numeric(expos[i]))
              model <- paste(model, paste(label1,"*I(x>",text_exp,")*(exp((",text_exp,"-x)/",label2,"))"), sep = " + ")
              model_kf_inst <- paste(model_kf_inst, paste("e[k,",j,"]","*I(x[k]>",text_exp,")*(exp((",text_exp,"-x[k])/e[k,",j + 1,"]))"), sep = " + ")
              model_kf_mean <- paste(model_kf_mean, paste("e[k,",j,"]","*I(x[k]>",text_exp,")*(exp((",text_exp,"-x[k])/e[k,",j + 1,"]))"), sep = " + ")
              j <- j + 2
              apriori[[label1]] <- as.numeric(E0[i])
              error[[label1]] <- as.numeric(eE0[i])
              nouns <- c(nouns, label1)
              apriori[[label2]] <- as.numeric(TE0[i])
              error[[label2]] <- as.numeric(eTE0[i])
              nouns <- c(nouns, label2)
              if (input$fitType == 2) {
                processNoise <- c(processNoise, 0)
                processNoise <- c(processNoise, 0)
              }
            }
          }
        } else {
          updateTextInput(session, "E0", value = "")
          updateTextInput(session, "TE0", value = "")
          if (input$fitType == 2) {
            updateTextInput(session, "eE0", value = "")
            updateTextInput(session, "eTE0", value = "")
          }
        }
      }
      # * Logarithmic model ####
      if ("Logarithmic" %in% input$model) {
        logas <- unlist(strsplit(inputs$LogariRef, split = ","))
        L0 <- unlist(strsplit(inputs$L0, split = ","))
        TL0 <- unlist(strsplit(inputs$TL0, split = ","))
        eL0 <- unlist(strsplit(input$eL0, split = ","))
        eTL0 <- unlist(strsplit(input$eTL0, split = ","))
        if (length(logas) > 0) {
          i <- 0
          update <- 0
          if (length(logas) < length(L0) || length(logas) < length(TL0)) {
            L0 <- ""
            TL0 <- ""
            update <- 1
          }
          for (refl in logas) {
            refl <- trimmer(refl)
            i <- i + 1
            if (nchar(refl) > 0 && !is.na(as.numeric(refl))) {
              if (identical(L0,character(0)) || identical(TL0,character(0)) || is.na(L0[i]) || is.na(TL0[i]) || L0[i] == "" || TL0[i] == "" || L0[i] == " " || TL0[i] == " ") {
                update <- 1
                if (isTruthy(match(paste0("L",i), trans$names))) {
                  L0[i] <- trans$LScoefs[match(paste0("L",i), trans$names)]
                  TL0[i] <- trans$LScoefs[match(paste0("TauL",i), trans$names)]
                  eL0[i] <- abs(as.numeric(trans$LScoefs[match(paste0("L",i), trans$names) + length(trans$names)]/sqrt(length(trans$x))))
                  eTL0[i] <- abs(as.numeric(trans$LScoefs[match(paste0("TauL",i), trans$names) + length(trans$names)]/sqrt(length(trans$x))))
                } else {
                  if (input$tunits == 1) {
                    span <- 3 * 365
                    sample <- 1/12 * info$sampling*365.25
                    forward <- 1 * 365
                  } else if (input$tunits == 2) {
                    span <- 3 * 365/7
                    forward <- 1 * 365/7
                    sample <- 1/12 * info$sampling*365.25/7
                  } else if (input$tunits == 3) {
                    span <- 3
                    forward <- 1
                    sample <- 1/12 * info$sampling*365.25
                  }
                  if (length(trans$x[trans$x > as.numeric(refl) + 3*sample & trans$x <= as.numeric(refl) + span]) > 0 && length(trans$x[trans$x > as.numeric(refl) & trans$x < as.numeric(refl) + 3*sample]) > 0) {
                    apriori_x_before <- trans$x[trans$x < as.numeric(refl)]
                    apriori_y_before <- trans$y[trans$x < as.numeric(refl)]
                    apriori_y_after <- trans$y[trans$x > as.numeric(refl) & trans$x <= as.numeric(refl) + span]
                    apriori_x_after <- trans$x[trans$x > as.numeric(refl) & trans$x <= as.numeric(refl) + span] - as.numeric(refl)
                    if (length(apriori_x_before) > 3) {
                      fitBefore <- lm(apriori_y_before ~ apriori_x_before)
                      trendBefore <- fitBefore$coef[2]
                    } else {
                      trendBefore <- 0
                    }
                    flat <- apriori_y_after - apriori_x_after * trendBefore
                    convex <- lm(flat ~ poly(apriori_x_after, 2))
                    coeff <- 0
                    if (convex$coef[3] < 0) {
                      coeff <- -1
                    } else if (convex$coef[3] >= 0) {
                      coeff <- 1
                    }
                    flat <- coeff * flat - min(coeff * flat) + 0.0000000000001
                    forward <- apriori_x_after[which.min(abs(apriori_x_after - span))]
                    if (length(apriori_x_after[apriori_x_after < sample]) > 0) {
                      x0 <- apriori_x_after[apriori_x_after < sample]
                      y0 <- flat[apriori_x_after < sample]
                    } else if (length(apriori_x_after[apriori_x_after < 2*sample]) > 0) {
                      x0 <- apriori_x_after[apriori_x_after < 2*sample]
                      y0 <- flat[apriori_x_after < 2*sample]
                    } else if (length(apriori_x_after[apriori_x_after < 3*sample]) > 0) {
                      x0 <- apriori_x_after[apriori_x_after < 3*sample]
                      y0 <- flat[apriori_x_after < 3*sample]
                    } else {
                      x0 <- 0
                      y0 <- 0
                      showNotification("Not enough data to guess the a priori values of the logarithmic decay. The a priori values must be provided to continue.", action = NULL, duration = 10, closeButton = T, id = "no_logarithmic", type = "warning", session = getDefaultReactiveDomain())
                    }
                    x1 <- apriori_x_after[apriori_x_after > forward - sample & apriori_x_after <= forward]
                    y1 <- flat[apriori_x_after > forward - sample & apriori_x_after <= forward]
                    L0[i] <- coeff * (mean(y1) - mean(y0))
                    sampling_tauL <- function(x) {
                      if (length(apriori_x_after[apriori_x_after > (span/x) - sample & apriori_x_after < span/x]) > 3) {
                        mean(apriori_x_after[apriori_x_after > (span/x) - sample & apriori_x_after < span/x])/(exp(mean(flat[apriori_x_after > (span/x) - sample & apriori_x_after < span/x])/as.numeric(L0[i])) - 1) 
                      } else {
                        NA
                      }
                    }
                    tl <- sapply(3:8, function(x) sampling_tauL(x = x))
                    if (!isTruthy(tl)) {
                      tl <- 0
                    }
                    TL0[i] <- median(unlist(tl), na.rm = T) * coeff / 50
                    if (!isTruthy(TL0[i]) || TL0[i] < 0) {
                      TL0[i] <- abs(as.numeric(TL0[i]))
                    }
                    if (input$fitType == 1) {
                      #NA
                    } else if (input$fitType == 2) {
                      eL0[i] <- sd(y1)
                      eTL0[i] <- sd(unlist(tl))
                    }
                  } else {
                    showNotification("Not enough data to guess the a priori values of the logarithmic decay. The a priori values must be provided to continue.", action = NULL, duration = 10, closeButton = T, id = "no_logarithmic", type = "warning", session = getDefaultReactiveDomain())
                  }
                }
              }
            } else {
              if (is.na(L0[i]) || trimmer(L0[i]) == "NA" || trimmer(L0[i]) == "") {
                L0[i] <- NA
                TL0[i] <- NA
              } else {
                L0[i] <- NA
                TL0[i] <- NA
                update <- 1
              }
            }
          }
          if (update > 0) {
            line_L0 <- paste(sprintf("%.7f",as.numeric(L0)), collapse = ", ")
            line_TL0 <- paste(sprintf("%.7f",as.numeric(TL0)), collapse = ", ")
            updateTextInput(session, "L0", value = line_L0)
            updateTextInput(session, "TL0", value = line_TL0)
            if (input$fitType == 1) {
              req(info$stop)
            } else if (input$fitType == 2) {
              line_eL0 <- paste(sprintf("%.7f",as.numeric(eL0)), collapse = ", ")
              line_eTL0 <- paste(sprintf("%.7f",as.numeric(eTL0)), collapse = ", ")
              updateTextInput(session, "eL0", value = line_eL0)
              updateTextInput(session, "eTL0", value = line_eTL0)
            }
          }
          for (i in seq_len(length(L0))) {
            if (!is.na(as.numeric(L0[i]))) {
              info$run <- T
              label1 <- paste0("L",i)
              label2 <- paste0("TauL",i)
              text_log <- sprintf("%f",as.numeric(logas[i]))
              model <- paste(model, paste(label1,"*log1p(I(x>",text_log,")*(x-",text_log,")/",label2,")"), sep = " + ")
              model_kf_inst <- paste(model_kf_inst, paste0("e[k,",j,"]*log1p(I(x[k] > ",text_log,")*(x[k]-",text_log,")/e[k,",j + 1,"])"), sep = " + ")
              model_kf_mean <- paste(model_kf_mean, paste0("e[k,",j,"]*log1p(I(x[k] > ",text_log,")*(x[k]-",text_log,")/e[k,",j + 1,"])"), sep = " + ")
              j <- j + 2
              apriori[[label1]] <- as.numeric(L0[i])
              error[[label1]] <- as.numeric(eL0[i])
              nouns <- c(nouns, label1)
              apriori[[label2]] <- as.numeric(TL0[i])
              error[[label2]] <- as.numeric(eTL0[i])
              nouns <- c(nouns, label2)
              if (input$fitType == 2) {
                processNoise <- c(processNoise, 0)
                processNoise <- c(processNoise, 0)
              }
            }
          }
        } else {
          updateTextInput(session, "L0", value = "")
          updateTextInput(session, "TL0", value = "")
          if (input$fitType == 2) {
            updateTextInput(session, "eL0", value = "")
            updateTextInput(session, "eTL0", value = "")
          }
        }
      }
      # * Polynomial model ####
      if ("Polynomial" %in% input$model) {
        if (nchar(input$PolyCoef) > 0) {
          if (!is.na(inputs$PolyCoef) && inputs$PolyCoef > 1 && inputs$PolyCoef < 20) {
            P0 <- unlist(strsplit(input$P0, split = ","))
            eP0 <- unlist(strsplit(input$eP0, split = ","))
            if (!is.na(inputs$PolyRef)) {
              refp <- inputs$PolyRef
            } else {
              if ("Linear" %in% input$model) {
                refp <- reft
              } else {
                if (input$fitType == 1) {
                  refp <- mean(x, na.rm = T)
                } else if (input$fitType == 2) {
                  refp <- x[1]
                }
              }
              updateTextInput(session, "PolyRef", value = refp)
              if (input$fitType == 1) {
                req(info$stop)
              }
            }
            text_rate <- sprintf("%f",as.numeric(refp))
            i <- 0
            for (degree in 2:inputs$PolyCoef) {
              i <- i + 1
              if (identical(P0[i],character(0)) || is.na(P0[i])) {
                P0[i] <- 0
                if (input$fitType == 2) {
                  if (isTruthy(match(paste0("P",degree), trans$names))) {
                    P0[i] <- trans$LScoefs[match(paste0("P",degree), trans$names)]
                    eP0[i] <- trans$LScoefs[match(paste0("P",degree), trans$names) + length(trans$names)]/sqrt(length(trans$x))
                  } else {
                    eP0[i] <- 1
                  }
                  line_P0 <- paste(sprintf("%f",as.numeric(P0)), collapse = ", ")
                  line_eP0 <- paste(sprintf("%f",as.numeric(eP0)), collapse = ", ")
                  updateTextInput(session, "P0", value = line_P0)
                  updateTextInput(session, "eP0", value = line_eP0)
                }
              }
              info$run <- T
              label <- paste0("P",degree)
              model <- paste(model, paste0(label,"*(x-",text_rate,")^",degree), sep = " + ")
              model_lm <- paste(model_lm, paste0("x^",degree), sep = " + ")
              model_kf_inst <- paste(model_kf_inst, paste0("e[k,",j,"]*(x[k]-",text_rate,")^",degree), sep = " + ")
              model_kf_mean <- paste(model_kf_mean, paste0("e[k,",j,"]*(x[k]-",text_rate,")^",degree), sep = " + ")
              j <- j + 1
              apriori[[label]] <- as.numeric(P0[i])
              error[[label]] <- as.numeric(eP0[i])
              nouns <- c(nouns, label)
              processNoise <- c(processNoise, 0)
            }
          } else {
            showNotification("The requested degree of the polynomial is not valid. Check the input value.", action = NULL, duration = 10, closeButton = T, id = "bad_offset_epoch", type = "error", session = getDefaultReactiveDomain())
          }
        }
      }
      if (input$fitType == 1) {
        list(model = model, model_lm = model_lm, apriori = apriori) 
      } else if (input$fitType == 2) {
        list(model = model, model_kf_mean = model_kf_mean, model_kf_inst = model_kf_inst, apriori = apriori, nouns = nouns, processNoise = processNoise, error = error)
      }
    })
  }
  ReadLog <- function(x) {
    antrec <- grep("^[34].[x0-9]+ ",readLines(con = x$datapath, n = -1L, ok = T, warn = F, skipNul = T), ignore.case = F, perl = T, value = T)
    dates <- grep(" Date Removed ",readLines(con = x$datapath, n = -1L, ok = T, warn = F, skipNul = T), ignore.case = F, perl = T, value = T)
    if (length(antrec) > 0 && length(dates) > 0) {
      ante = c()
      rece = c()
      if (length(dates) > 0) {
        for (l in seq_len(length(dates))) {
          if (!(grepl('CCYY',dates[[l]]))) {
            f <- data.frame(strsplit(dates[[l]], " :"))[2,]
            t <- strptime(f, format = '%Y-%m-%dT%H:%M')
            if (is.na(t)) {
              t <- strptime(f, format = '%Y-%m-%d')
            }
            if (input$tunits == 1) {
              e <- time_length(ymd_hms("1858-11-17 00:00:00") %--% t, unit = "second")/86400  #mjd
            } else if (input$tunits == 2) {
              e <- time_length(ymd_hms("1980-01-06 00:00:00") %--% t, unit = "second")/604800 # GPS week
            } else if (input$tunits == 3) {
              e <- decimal_date(t)  
            }
            if (grepl('Antenna',antrec[[l]])) {
              ante <- c(ante,e)
            } else if (grepl('Receiver',antrec[[1]])) {
              rece <- c(rece,e)
            }
          }
        }
      }
      return(list(ante,rece))
    } else {
      showNotification("The input sitelog file is empty or has a wrong format.", action = NULL, duration = 10, closeButton = T, id = "bad_sitelog", type = "warning", session = getDefaultReactiveDomain())
      return(NULL)
    }
  }
  ReadInfo <- function(x,y,z) {
    antes = c()
    reces = c()
    if (!is.null(x)) {
      pattern <- paste0("^ ",x)
      record <- grep(pattern, readLines(con = z$datapath, n = -1L, ok = T, warn = F, skipNul = T), ignore.case = F, perl = T, value = T)
      if (length(record) > 1) {
        for (l in seq_len(length(record))) {
          elements1 <- unlist(strsplit(record[[l]], "\\s+", fixed = F, perl = T, useBytes = F))
          if (length(record) > l) {
            elements2 <- unlist(strsplit(record[[l + 1]], "\\s+", fixed = F, perl = T, useBytes = F))
            t <- strptime(substr(record[[l + 1]],26,43), format = '%Y %j %H %M %S')
            if (input$tunits == 1) {
              e <- time_length(ymd_hms("1858-11-17 00:00:00") %--% t, unit = "second")/86400  #mjd
            } else if (input$tunits == 2) {
              e <- time_length(ymd_hms("1980-01-06 00:00:00") %--% t, unit = "second")/604800 # GPS week
            } else if (input$tunits == 3) {
              e <- decimal_date(t)
            }
            if (substr(record[[l]],98,168) != substr(record[[l + 1]],98,168)) {
              reces <- c(reces,e)
            }
            if (substr(record[[l]],171,213) != substr(record[[l + 1]],171,213)) {
              antes <- c(antes,e)
            }
          }
        }
      } else {
        showNotification(paste("Station",x,"not found in the station.info file."), action = NULL, duration = 10, closeButton = T, id = "bad_stationinfo", type = "warning", session = getDefaultReactiveDomain())
      }
      reces <- unique(reces)
      antes <- unique(antes)
    }
    if (!is.null(y)) {
      pattern <- paste0("^ ",y)
      record <- grep(pattern,readLines(con = z$datapath, n = -1L, ok = T, warn = F, skipNul = T), ignore.case = F, perl = T, value = T)
      if (length(record) > 1) {
        for (l in seq_len(length(record))) {
          elements1 <- unlist(strsplit(record[[l]], "\\s+", fixed = F, perl = T, useBytes = F))
          if (length(record) > l) {
            elements2 <- unlist(strsplit(record[[l + 1]], "\\s+", fixed = F, perl = T, useBytes = F))
            t <- strptime(substr(record[[l + 1]],26,43), format = '%Y %j %H %M %S')
            if (input$tunits == 1) {
              e <- time_length(ymd_hms("1858-11-17 00:00:00") %--% t, unit = "second")/86400  #mjd
            } else if (input$tunits == 2) {
              e <- time_length(ymd_hms("1980-01-06 00:00:00") %--% t, unit = "second")/604800 # GPS week
            } else if (input$tunits == 3) {
              e <- decimal_date(t)
            }
            if (substr(record[[l]],98,168) != substr(record[[l + 1]],98,168)) {
              reces <- c(reces,e)
            }
            if (substr(record[[l]],171,213) != substr(record[[l + 1]],171,213)) {
              antes <- c(antes,e)
            }
          }
        }
      } else {
        showNotification(paste("Station",y,"not found in the station.info file."), action = NULL, duration = 10, closeButton = T, id = "bad_stationinfo", type = "warning", session = getDefaultReactiveDomain())
      }
    }
    return(list(antes,reces))
  }
  ReadSoln <- function(x,y,z) {
    req(x,z)
    changes <- c()
    site <- paste0(" ",x," ")
    extracted <- substring(grep(' P -',grep(site,readLines(z$datapath),value = T), value = T), 17, 28)
    if (!is.null(y)) {
      site2 <- paste0(" ",y," ")
      extracted2 <- substring(grep(' P -',grep(site2,readLines(z$datapath),value = T), value = T), 17, 28)
      extracted <- rbind(extracted,extracted2)
    }
    extracted <- extracted[-which(extracted == "00:000:00000")]
    if (length(extracted) > 0) {
      years <- as.numeric(substring(extracted, 1, 2)) + 1900
      days <- as.numeric(substring(extracted, 4, 6))
      segs <- as.numeric(substring(extracted, 8, 12))
      years[years < 1950] <- years[years < 1950] + 100
      changes <- decimal_date(as.Date(days - 1 + segs/86400, origin = paste0(years,"-01-01")))
    }
    changes <- na.omit(changes)
    return(changes)
  }
  ReadCustom <- function(x,y,z) {
    req(x,z)
    changes <- c()
    cols <- try(range(count.fields(z$datapath, comment.char = "#")), silent = F)
    if (!isTruthy(cols)) {
      showNotification("Unable to read the input custom discontinuity file.", action = NULL, duration = 15, closeButton = T, id = "bad_custom", type = "error", session = getDefaultReactiveDomain())
      req(info$stop)
    }
    if (!is.na(cols[1]) && !is.na(cols[2])) {
      if (cols[1] > 1) {
        col <- 2
      } else {
        col <- 1
      }
      table <- try(read.table(z$datapath, comment.char = "#", fill = T, col.names = c(1:cols[2]))[,1:col], silent = F)
      if (isTruthy(table)) {
        if (col == 2) {
          if (all(grepl("^\\d{2}\\w{3}\\d{2}$", table$X2, ignore.case = F, perl = T))) { #NGL steps file
            table$dyear <- decimal_date(as.Date(ymd(table$X2)))
            if (any(table$X1 == x)) {
              changes <- as.numeric(unlist(unique(table$dyear[table$X1 == x])))
            }
            if (!is.null(y)) {
              if (any(table$X1 == y)) {
                changes <- unique(c(changes, as.numeric(unlist(table$dyear[table$X1 == y]))))
              }
            }
          } else {
            if (cols[2] > 2 && info$custom_warn == 0) {
              info$custom_warn <- 1
              showNotification("The input custom discontinuity file contains more than 2 columns. Only the first 2 will be used.", action = NULL, duration = 15, closeButton = T, id = "bad_custom", type = "warning", session = getDefaultReactiveDomain())
            }
            if (length(table[table$X2 == x]) > 0) {
              changes <- as.numeric(unique(unlist(table$X1[table$X2 == x])))
            }
            if (!is.null(y)) {
              if (length(table[table$X2 == y]) > 0) {
                changes <- unique(c(changes, as.numeric(unlist(table$X1[table$X2 == y]))))
              }
            }
          }
        } else {
          changes <- as.numeric(unique(unlist(table)))
        }
        changes <- na.omit(changes)
      }
    }
    return(changes)
  }
  plot_series <- function(x,y,z,rangex,rangey,sigma,title,symbol) {
    options(digits = 10)
    if (symbol == 0) {
      s <- 'p'
    } else if (symbol == 1) {
      s <- 'l'
    } else if (symbol == 2) {
      s <- 'o'
    }
    mini <- min(y, na.rm = T)
    maxi <- max(y, na.rm = T)
    if ((abs(mini) > 999 || abs(maxi) > 999) && abs(maxi - mini) < 999) {
      if (mini < 0) {
        const <- maxi
        ylab <- paste(intToUtf8(8210),abs(mini))
      } else {
        const <- mini
        ylab <- paste("+",abs(mini))
      }
    } else {
      const <- 0
      ylab <- ""
    }
    plot(x,y, type = s, pch = 20, xlab = "", ylab = ylab, xlim = rangex, ylim = rangey, main = title, yaxt = "n")
    p <- par("usr")[3:4] # min/max Y-axis values
    pout <- pretty(p - const) # round new min/max Y-axis values
    pin <- pout + const
    axis(2, at = pin, labels = pout)
    if (sigma == T) {
      ba <- y + z
      bb <- y - z
      polygon(c(x, rev(x)), c(ba, rev(bb)), col = rgb(0,0,0,0.2), border = NA)
    }
  }
  periodogram <- function(serie) {
    req(trans$fs)
    if (messages > 0) cat(file = stderr(), "Computing periodogram ", serie, "\n")
    if (input$spectrumOriginal && any("all" %in% serie || "original" %in% serie)) {
      trans$title[2] <- "original (black), "
      lombscargle <- spec.lomb(y = trans$y, x = trans$x - trans$x[1], f = trans$fs, w = trans$sy, mode = "normal")
      trans$fs <- lombscargle$f
      trans$spectra <- 1/lombscargle$f
      trans$amp[,1] <- lombscargle$A
      trans$psd[,1] <- lombscargle$PSD*var(trans$y)
      trans$var <- var(trans$y)
    }
    if (input$spectrumModel && length(trans$mod) > 0 && length(trans$res) > 0 && any("all" %in% serie || "model" %in% serie)) {
      trans$title[3] <- "model (red), "
      ideal <- trans$mod
      lombscargle <- spec.lomb(y = ideal, x = trans$x - trans$x[1], f = trans$fs, mode = "normal")
      trans$fs <- lombscargle$f
      trans$spectra <- 1/lombscargle$f
      trans$amp[,2] <- lombscargle$A
      trans$psd[,2] <- lombscargle$PSD*var(ideal)
      trans$var <- var(ideal)
    } 
    if (input$periodogram_residuals && length(trans$res) > 0 && any("all" %in% serie || "residuals" %in% serie)) {
      trans$title[4] <- "model residuals (green), "
      if (length(trans$reserror) > 0) {
        sy <- trans$reserror
      } else {
        sy <- trans$sy
      }
      lombscargle <- spec.lomb(y = as.vector(trans$res), x = trans$x - trans$x[1], f = trans$fs, w = sy, mode = "normal")
      trans$fs <- lombscargle$f
      trans$spectra <- 1/lombscargle$f
      trans$amp[,3] <- lombscargle$A
      trans$psd[,3] <- lombscargle$PSD*var(as.vector(trans$res))
      trans$var <- var(as.vector(trans$res))
    }
    if (input$spectrumFilter && length(trans$filter > 0) && any("all" %in% serie || "filter" %in% serie)) {
      trans$title[5] <- "filter (blue), "
      lombscargle <- spec.lomb(y = as.vector(trans$filter), x = trans$x - trans$x[1], f = trans$fs, mode = "normal")
      trans$fs <- lombscargle$f
      trans$spectra <- 1/lombscargle$f
      trans$amp[,4] <- lombscargle$A
      trans$psd[,4] <- lombscargle$PSD*lombscargle$PSD*var(as.vector(trans$filter))
      trans$var <- var(as.vector(trans$filter))
    }
    if (input$spectrumFilterRes && length(trans$filterRes) > 0 && any("all" %in% serie || "filterRes" %in% serie)) {
      trans$title[6] <- "filter residuals (cyan), "
      lombscargle <- spec.lomb(y = as.vector(trans$filterRes), x = trans$x - trans$x[1], f = trans$fs, mode = "normal")
      trans$fs <- lombscargle$f
      trans$spectra <- 1/lombscargle$f
      trans$amp[,5] <- lombscargle$A
      trans$psd[,5] <- lombscargle$PSD*var(as.vector(trans$filterRes))
      trans$var <- var(as.vector(trans$filterRes))
    }
    trans$spectra_old <- c(input$spectrumOriginal,input$spectrumModel,input$periodogram_residuals,input$spectrumFilter,input$spectrumFilterRes)
  }
  vondrak <- function(x,y,yp,p) {
    #code adapted from Sylvain Loyer's Fortran code and from Vondrak's 1969 paper
    n <- length(x)
    p <- as.numeric(p)/(0.791020*log(n) - 2.339407)
    yp <- yp/sum(yp)
    xdim <- n - 3
    sr <- as.numeric(tail(x, n = 1) - x[1])
    if (p/sr >= 0.1) {
      showNotification("The input period of the Vondrak filter is larger than T/10. Results may be unreliable.", action = NULL, duration = 10, closeButton = T, id = "bad_vondrak_period", type = "warning", session = getDefaultReactiveDomain())
    }
    eps <- ((10^4.64)*p^-6)/xdim
    kmoy <- 10
    xpu <- matrix(0,n,1)
    for (i in seq_len(n)) {
      pm <- 0
      if (i < kmoy + 1) {
        ir <- 1
      } else if (i > n - kmoy) {
        ir <- n - 2*kmoy
      } else {
        ir <- i - kmoy
      }
      is <- ir + 2*kmoy
      pm <- sum(sapply(ir:is, function(x) yp[x]/(2*kmoy + 1)))
      xpu[i] <- yp[i]/pm
    }
    aa <- matrix(0,7,n)
    yl <- matrix(0,n,1)
    xpp <- matrix(0,4,4)
    nlim <- n - 2
    ls <- n - 3
    for (i in seq_len(n)) {
      delx <- x[i + 2] - x[i + 1]
      aux <- (6*sqrt(delx))/sqrt(sr)
      if (i > ls) {
        xpp[,4] <- 0
      } else {
        xpp[1,4] <- aux/((x[i] - x[i + 1])*(x[i] - x[i + 2])*(x[i] - x[i + 3]))
        xpp[2,4] <- aux/((x[i + 1] - x[i])*(x[i + 1] - x[i + 2])*(x[i + 1] - x[i + 3]))
        xpp[3,4] <- aux/((x[i + 2] - x[i + 1])*(x[i + 2] - x[i])*(x[i + 2] - x[i + 3]))
        xpp[4,4] <- aux/((x[i + 3] - x[i + 1])*(x[i + 3] - x[i + 2])*(x[i + 3] - x[i]))
      }
      aa[1,i] <- xpp[1,1]*xpp[4,1]
      aa[2,i] <- xpp[1,2]*xpp[3,2] + xpp[2,1]*xpp[4,1]
      aa[3,i] <- xpp[1,3]*xpp[2,3] + xpp[2,2]*xpp[3,2] + xpp[3,1]*xpp[4,1]
      aa[4,i] <- eps*yp[i] + xpp[1,4]*xpp[1,4] + xpp[2,3]*xpp[2,3] + xpp[3,2]*xpp[3,2] + xpp[4,1]*xpp[4,1]
      aa[5,i] <- xpp[1,4]*xpp[2,4] + xpp[2,3]*xpp[3,3] + xpp[3,2]*xpp[4,2]
      aa[6,i] <- xpp[1,4]*xpp[3,4] + xpp[2,3]*xpp[4,3]
      aa[7,i] <- xpp[1,4]*xpp[4,4]
      for (j in seq_len(3)) {
        for (l in seq_len(4)) {
          xpp[l,j] <- xpp[l,j + 1]
        }
      }
      yl[i] <- eps*yp[i]*y[i]
    }
    ndep <- 0
    repeat {
      i1 <- 4
      if (ndep == 0) {
        j1 <- 1
        ndep <- 1
      } else {
        j1 <- j1 + 1
      }
      if (j1 == nlim) {
        i1 <- 4
        j1 <- n - 2
        i2 <- 3
        j2 <- n - 1
        while (i2 != 1) {
          ls <- i2 + 2
          coef <- aa[i2,j2]/aa[i1,j1]
          il1 <- 3
          for (il in i2:ls) {
            il1 <- il1 + 1
            aa[il,j2] <- aa[il,j2] - aa[il1,j1]*coef
          }
          yl[j2] <- yl[j2] - yl[j1]*coef
          i2 <- i2 - 1
          j2 <- j2 + 1
        }
        j1 <- n - 1
        coef <- aa[3,n]/aa[4,j1]
        aa[4,n] <- aa[4,n] - aa[5,j1]*coef
        yl[n] <- yl[n] - yl[j1]*coef
        yl[n] <- yl[n]/aa[4,n]
        yl[j1] <- (yl[j1] - aa[5,j1]*yl[n])/aa[4,j1] 
        j2 <- n - 2
        yl[j2] <- (yl[j2] - aa[5,j2]*yl[j1] - aa[6,j2]*yl[n])/aa[4,j2]
        jl <- n - 3
        for (j in seq_len(jl)) {
          inn <- j2 - j
          yl[inn] <- (yl[inn] - aa[5,inn]*yl[inn + 1] - aa[6,inn]*yl[inn + 2] - aa[7,inn]*yl[inn + 3])/aa[4,inn]
        }
        break
      } else {
        i2 <- 3
        j2 <- j1 + 1
        while (i2 != 0) {
          coef <- aa[i2,j2]/aa[i1,j1]
          ls <- i2 + 3
          il1 <- 3
          for (il in i2:ls) {
            il1 <- il1 + 1
            aa[il,j2] <- aa[il,j2] - aa[il1,j1]*coef 
          }
          yl[j2] <- yl[j2] - yl[j1]*coef
          i2 <- i2 - 1
          j2 <- j2 + 1
        }
      }
    }
    return(yl)
  }
  trimmer <- function(x) gsub("^\\s+|\\s+$", "", x)
  collect <- function(file_out) {
    if (messages > 0) cat(file = stderr(), "Downloading results", "\n")
    now <- paste0(" run on ",Sys.time()," ",Sys.timezone())
    cat(paste0("# ",version,now), file = file_out, sep = "\n", fill = F, append = F)
    cat(paste0("# Original series: ",file$primary$name), file = file_out, sep = "\n", fill = F, append = T)
    if (input$format != 4) {
      cat(paste0("# Coordinate component: ",input$tab), file = file_out, sep = "\n", fill = F, append = T)
    } else {
      if (isTruthy(input$sigmas)) {
        cat(paste0("# Column numbers for data and errorbars: ",inputs$variable," ",inputs$errorBar), file = file_out, sep = "\n", fill = F, append = T)
      } else {
        cat(paste0("# Column number for data: ",inputs$variable), file = file_out, sep = "\n", fill = F, append = T)
      }
    }
    if (input$eulerType == 2 && length(trans$plate) > 0) {
      cat(sprintf('# Plate model rate removed: %f',trans$plate[as.numeric(input$tab)]), file = file_out, sep = "\n", fill = F, append = T)
    }
    if (input$fitType == 1 && length(trans$results) > 0) {
      cat(paste0("# Model LS: ",gsub(" > ", ">", gsub(" - ", "-", gsub(" \\* ", "\\*", gsub("))", ")", gsub("I\\(x>", "if(x>", gsub("I\\(cos", "cos", gsub("I\\(sin", "sin", gsub("^ *|(?<= ) | *$", "", Reduce(paste, trans$equation), perl = TRUE))))))))), file = file_out, sep = "\n", fill = F, append = T)
      for (i in seq_len(length(dimnames(trans$LScoefs)[[1]]))) {
        cat(sprintf('# Parameter: %s = %f +/- %f',dimnames(trans$LScoefs)[[1]][i],trans$LScoefs[i,1],trans$LScoefs[i,2]), file = file_out, sep = "\n", fill = F, append = T)
      }
    } else if (input$fitType == 2 && length(trans$kalman) > 0) {
      if (input$kf == 1) {
        cat(paste0("# Model EKF: ",gsub(" > ", ">", gsub(" - ", "-", gsub(" \\* ", "\\*", gsub("))", ")", gsub("I\\(x>", "if(x>", gsub("I\\(cos", "cos", gsub("I\\(sin", "sin", gsub("^ *|(?<= ) | *$", "", Reduce(paste, trans$equation), perl = TRUE))))))))), file = file_out, sep = "\n", fill = F, append = T)
      } else if (input$kf == 2) {
        cat(paste0("# Model UKF: ",gsub(" > ", ">", gsub(" - ", "-", gsub(" \\* ", "\\*", gsub("))", ")", gsub("I\\(x>", "if(x>", gsub("I\\(cos", "cos", gsub("I\\(sin", "sin", gsub("^ *|(?<= ) | *$", "", Reduce(paste, trans$equation), perl = TRUE))))))))), file = file_out, sep = "\n", fill = F, append = T)
      }
      cat(sprintf('# Parameter: %s = %f +/- %f', colnames(trans$kalman), colMeans(trans$kalman), colMeans(trans$kalman_unc)), file = file_out, sep = "\n", fill = F, append = T)
      cat(sprintf('# A priori: %s = %f +/- %f', trans$kalman_info$nouns, trans$kalman_info$apriori, trans$kalman_info$error), file = file_out, sep = "\n", fill = F, append = T)
      cat(sprintf('# Process noise: %s = %f', trans$kalman_info$nouns, as.list(sqrt(trans$kalman_info$processNoise))), file = file_out, sep = "\n", fill = F, append = T)
      cat(sprintf('# Measurement noise: %f', inputs$ObsError), file = file_out, sep = "\n", fill = F, append = T)
    }
    if (length(trans$offsetEpochs) > 0) {
      cat(paste0('# Discontinuities at: ',paste(trans$offsetEpochs, collapse = ", ")), file = file_out, sep = "\n", fill = F, append = T)
    }
    if (isTruthy(trans$midas_vel) && isTruthy(input$midas)) {
      if (isTruthy(trans$midas_vel2)) {
        cat(sprintf('# MIDAS: %f +/- %f #discontinuities included',trans$midas_vel,trans$midas_sig), file = file_out, sep = "\n", fill = F, append = T)
        cat(sprintf('# MIDAS: %f +/- %f #discontinuities skipped',trans$midas_vel2,trans$midas_sig2), file = file_out, sep = "\n", fill = F, append = T)
      } else {
        cat(sprintf('# MIDAS: %f +/- %f #discontinuities included',trans$midas_vel,trans$midas_sig), file = file_out, sep = "\n", fill = F, append = T)
      }
    }
    if (input$waveform && inputs$waveformPeriod > 0) {
      cat(sprintf('# Waveform: %f',as.numeric(inputs$waveformPeriod)), file = file_out, sep = "\n", fill = F, append = T)
    }
    if (isTruthy(input$filter)) {
      if (isTruthy(trans$vondrak) && (isTruthy(inputs$low) || isTruthy(inputs$high))) {
        if (input$series2filter == 1) {
          origen <- " from original series"
        } else if (input$series2filter == 2) {
          origen <- " from residual series"
        }
        if (isTruthy(trans$vondrak[1]) && isTruthy(trans$vondrak[2])) {
          cat(paste0(sprintf('# Vondrak: %f (low) %f (high)',as.numeric(trans$vondrak[1]),as.numeric(trans$vondrak[2])), origen), file = file_out, sep = "\n", fill = F, append = T)
        } else if (isTruthy(trans$vondrak[1])) {
          cat(paste0(sprintf('# Vondrak: %f (low)',as.numeric(trans$vondrak[1])), origen), file = file_out, sep = "\n", fill = F, append = T) 
        } else if (isTruthy(trans$vondrak[2])) {
          cat(sprintf('# Vondrak: %f (high)',as.numeric(trans$vondrak[2])), file = file_out, sep = "\n", fill = F, append = T) 
        }
      }
    }
    if (isTruthy(trans$noise) && (isTruthy(input$mle))) {
      if (isTruthy(trans$noise[1])) {
        cat(sprintf('# Noise: WH %f +/- %f ',as.numeric(trans$noise[1]),as.numeric(trans$noise[2])), file = file_out, sep = "\n", fill = F, append = T)
      }
      if (isTruthy(trans$noise[3])) {
        cat(sprintf('# Noise: FL %f +/- %f ',as.numeric(trans$noise[3]),as.numeric(trans$noise[4])), file = file_out, sep = "\n", fill = F, append = T)
      }
      if (isTruthy(trans$noise[5])) {
        cat(sprintf('# Noise: RW %f +/- %f ',as.numeric(trans$noise[5]),as.numeric(trans$noise[6])), file = file_out, sep = "\n", fill = F, append = T)
      }
      if (isTruthy(trans$noise[7])) {
        cat(sprintf('# Noise: PL %f +/- %f ',as.numeric(trans$noise[7]),as.numeric(trans$noise[8])), file = file_out, sep = "\n", fill = F, append = T)
      }
      if (isTruthy(trans$noise[9])) {
        cat(sprintf('# Noise: K  %f +/- %f ',as.numeric(trans$noise[9]),as.numeric(trans$noise[10])), file = file_out, sep = "\n", fill = F, append = T)
      }
      if (isTruthy(trans$noise[11])) {
        cat(sprintf('# Noise: MLE %f ',as.numeric(trans$noise[11])), file = file_out, sep = "\n", fill = F, append = T)
      }
    }
    if (isTruthy(inputs$step) && inputs$step > 0) {
      cat(sprintf('# Resampling: %f ',inputs$step), file = file_out, sep = "\n", fill = F, append = T)
    }
    if (input$optionSecondary == 2) {
      cat(sprintf('# Corrected with: %s ',input$series2$name), file = file_out, sep = "\n", fill = F, append = T)
    }
    if (input$optionSecondary == 3) {
      cat(sprintf('# Averaged with: %s ',input$series2$name), file = file_out, sep = "\n", fill = F, append = T)
    }
    if (isTruthy(input$sigmas)) {
      OutPut$df <- data.frame(x = trans$x, y = trans$y, sy = trans$sy)
      names(OutPut$df) <- c("# Epoch", "Data", "Sigma")
    } else {
      OutPut$df <- data.frame(x = trans$x, y = trans$y)
      names(OutPut$df) <- c("# Epoch", "Data")
    }
    req(OutPut$df)
    OutPut$df[,"# Epoch"] <- format(OutPut$df[,"# Epoch"],nsmall = info$decimalsx, digits = info$decimalsx, trim = F,scientific = F)
    OutPut$df[,"Data"] <- format(OutPut$df[,"Data"],nsmall = info$decimalsy, digits = info$decimalsy, trim = F,scientific = F)
    if (isTruthy(input$sigmas)) {
      OutPut$df[,"Sigma"] <- format(OutPut$df[,"Sigma"],nsmall = info$decimalsy, digits = info$decimalsy, trim = F,scientific = F)
    }
    if ((input$fitType == 1 || input$fitType == 2) && length(input$model) > 0) {
      if (length(trans$pattern) > 0 && input$waveform && inputs$waveformPeriod > 0) {
        OutPut$df$Model <- format(trans$mod - trans$pattern,nsmall = info$decimalsy, digits = info$decimalsy, trim = F,scientific = F)
        OutPut$df$Residuals <- format(trans$res + trans$pattern,nsmall = info$decimalsy, digits = 0, trim = F,scientific = F)
      } else {
        OutPut$df$Model <- format(trans$mod,nsmall = info$decimalsy, digits = info$decimalsy, trim = F,scientific = F)
        OutPut$df$Residuals <- format(trans$res,nsmall = info$decimalsy, digits = 0, trim = F,scientific = F)
      }
    }
    if (input$fitType == 1 && length(input$model) > 0) {
      if (length(trans$moderror) > 0) {
        OutPut$df$Sigma.Model <- format(trans$moderror,nsmall = info$decimalsy, digits = 1, trim = F,scientific = F)
      }
      if (length(trans$reserror) > 0) {
        OutPut$df$Sigma.Residuals <- format(trans$reserror,nsmall = info$decimalsy, digits = 1, trim = F,scientific = F)
      }
    }
    if (input$filter == T && (inputs$low != "" || inputs$high != "") && length(trans$filter) > 0) {
      if (length(trans$pattern) > 0 && input$waveform && inputs$waveformPeriod > 0 && length(trans$filterRes) > 0 && input$series2filter == 1) {
        OutPut$df$Smooth <- format(trans$filter - trans$pattern, nsmall = info$decimalsy, digits = info$decimalsy, trim = F, scientific = F)
        OutPut$df$Smooth.Residuals <- format(trans$filterRes + trans$pattern, nsmall = info$decimalsy, digits = info$decimalsy, trim = F, scientific = F)
      } else {
        OutPut$df$Smooth <- format(trans$filter, nsmall = info$decimalsy, digits = info$decimalsy, trim = F, scientific = F)
        OutPut$df$Smooth.Residuals <- format(trans$filterRes, nsmall = info$decimalsy, digits = info$decimalsy, trim = F, scientific = F)
      }
    }
    if (input$fitType == 2) {
      OutPut$df <- cbind(OutPut$df,format(trans$kalman,nsmall = info$decimalsy, digits = info$decimalsy, trim = F,scientific = F))
      colnames(trans$kalman_unc) <- paste0("sigma.",colnames(trans$kalman_unc))
      OutPut$df <- cbind(OutPut$df,format(trans$kalman_unc,nsmall = info$decimalsy, digits = info$decimalsy, trim = F,scientific = F))
    }
    if (length(trans$pattern) > 0 && input$waveform && inputs$waveformPeriod > 0) {
      OutPut$df$Waveform <- format(trans$pattern, nsmall = info$decimalsy, digits = info$decimalsy, trim = F, scientific = F)
    }
    if (isTruthy(input$add_excluded)) {
      if (isTruthy(input$sigmas)) {
        output_excluded$df <- data.frame(x = trans$xe, y = trans$ye, sy = trans$sye)
        names(output_excluded$df) <- c("# Epoch", "Data", "Sigma")
      } else {
        output_excluded$df <- data.frame(x = trans$xe, y = trans$ye)
        names(output_excluded$df) <- c("# Epoch", "Data")
      }
      output_excluded$df[,"# Epoch"] <- format(output_excluded$df[,"# Epoch"],nsmall = info$decimalsx, digits = info$decimalsx, trim = F,scientific = F)
      output_excluded$df[,"Data"] <- format(output_excluded$df[,"Data"],nsmall = info$decimalsy, digits = info$decimalsy, trim = F,scientific = F)
      if (isTruthy(input$sigmas)) {
        OutPut$df <- merge(OutPut$df,output_excluded$df,by = c("# Epoch", "Data", "Sigma"), all = T)
      } else {
        OutPut$df <- merge(OutPut$df,output_excluded$df,by = c("# Epoch", "Data"), all = T) 
      }
      excluded <- c(unique(which(is.na(OutPut$df), arr.ind = T)[,1]))
      for (i in excluded) {
        OutPut$df[i,"# Epoch"] <- paste0("#",OutPut$df[i,"# Epoch"])
      }
    }
    colnames(OutPut$df) <- sapply(1:length(colnames(OutPut$df)), function(x) paste(colnames(OutPut$df)[x],"[",x,"]", sep = ""))
    suppressWarnings(write.table(OutPut$df,file_out,append = T,quote = F,sep = " ",eol = "\n",na = "N/A",dec = ".",row.names = F,col.names = T))
  }
  collect_periodogram <- function(file_out) {
    if (messages > 0) cat(file = stderr(), "Downloading periodogram", "\n")
    now <- paste0(" run on ",Sys.time()," ",Sys.timezone())
    cat(paste0("# ",version,now), file = file_out, sep = "\n", fill = F, append = F)
    cat(paste0("# Original series: ",file$primary$name), file = file_out, sep = "\n", fill = F, append = T)
    if (input$format != 4) {
      cat(paste0("# Coordinate component: ",input$tab), file = file_out, sep = "\n", fill = F, append = T)
    } else {
      if (isTruthy(input$sigmas)) {
        cat(paste0("# Column numbers for data and errorbars: ",inputs$variable," ",inputs$errorBar), file = file_out, sep = "\n", fill = F, append = T)
      } else {
        cat(paste0("# Column number for data: ",inputs$variable), file = file_out, sep = "\n", fill = F, append = T)
      }
    }
    if (input$spectrumType == 0) {
      cat("# Amplitude spectrum ", file = file_out, sep = "\n", fill = F, append = T)
    } else if (input$spectrumType == 1) {
      cat("# Power spectrum ", file = file_out, sep = "\n", fill = F, append = T)
    }
    cat(paste0("# Longest period  = ",inputs$long_period), file = file_out, sep = "\n", fill = F, append = T)
    cat(paste0("# Shortest period = ",inputs$short_period), file = file_out, sep = "\n", fill = F, append = T)
    cat(paste0("# Oversampling = ",inputs$ofac), file = file_out, sep = "\n", fill = F, append = T)
    OutPut$df <- as.data.frame(trans$spectra)
    req(OutPut$df)
    names(OutPut$df)[1] <- "# period"
    column <- 2
    if (input$spectrumOriginal) {
      names(OutPut$df)[column] <- "original"
      column <- column + 1
    }
    if (input$spectrumModel && isolate(length(trans$mod) > 0) && isolate(length(trans$res) > 0)) {
      names(OutPut$df)[column] <- "model"
      column <- column + 1
    }
    if (input$periodogram_residuals && length(trans$res) > 0) {
      names(OutPut$df)[column] <- "modelResiduals"
      column <- column + 1
    }
    if (input$spectrumFilter && isolate(length(trans$filter) > 0)) {
      names(OutPut$df)[column] <- "filter"
      column <- column + 1
    }
    if (input$spectrumFilterRes && length(trans$filterRes) > 0) {
      names(OutPut$df)[column] <- "filterResiduals"
      column <- column + 1
    }
    colnames(OutPut$df) <- sapply(1:length(colnames(OutPut$df)), function(x) paste(colnames(OutPut$df)[x],"[",x,"]", sep = ""))
    suppressWarnings(write.table(OutPut$df,file_out,append = T,quote = F,sep = " ",eol = "\n",na = "N/A",dec = ".",row.names = F,col.names = T))
  }
  #Based on www.datall-analyse.nl/R/UKF.R
  UKF <- function(y, mod, GGfunction, FFfunction, kappa=0, sqrtMethod="Cholesky", logLik=FALSE, simplify=FALSE) {
    mod1 <- mod
    y <- as.matrix(y)
    ym <- ncol(y)
    yAttr <- attributes(y)
    p <- length(mod$m0)
    
    if (!is.null(mod$FF) | !is.null(mod$GG))
      warning("FF or GG matrix will not be used in the UKF")
    
    if (!is.null(mod$JFF) | !is.null(mod$JGG))
      warning("Time varying FF or GG matrix will not be used in the UKF")
    
    if (!is.null(mod$JW) | !is.null(mod$JV))
      warning("Time varying V or W matrix will not be used in the UKF")
    
    if (!(sqrtMethod == "Cholesky" | sqrtMethod == "svd"))
      stop("Name of sqrtMethod is incorrect")
    
    m <- rbind(mod$m0, matrix(0, nrow = nrow(y), ncol = length(mod$m0))) # a posteriori estimated state
    a <- matrix(0, nrow = nrow(y), ncol = length(mod$m0)) # a priori state in time update
    f <- matrix(0, nrow = nrow(y), ncol = ncol(y)) # predicted measurement from a priori state
    C <- vector(1 + nrow(y), mode = "list") # a posteriori state covariance
    R <- vector(nrow(y), mode = "list") # a priori state covariance in time update
    ll <- 0 # log-likelihood
    w <- as.vector(c(kappa/(p + kappa), rep(1/(2*(p + kappa)), 2*p))) # weights of sigma points
    
    C[[1]] <- mod$C0
    
    for (i in seq(length = nrow(y))) {
      
      ##time update
      
      ## Increase the process noise by a factor depending on the number of missing observations from the last one
      ## This implies the series must be sampled regularly with data gaps
      gapFactor <- 1
      if (i > 1 && info$regular == T) {
        gapFactor <- round((trans$x[i] - trans$x[i - 1]) / info$sampling, digits = 1)
      }
      #compute sigma points
      if (sqrtMethod == "Cholesky") {
        sigmaPlus <- t(chol((p + kappa)*C[[i]]))
      } else {
        tmpxs <- La.svd((p + kappa)*C[[i]], nu = 0)
        sigmaPlus <- t(sqrt(tmpxs$d)*tmpxs$vt)}
      sigmax <- t(m[i, ] + cbind(0, sigmaPlus, -sigmaPlus))
      #a priori state estimate
      tmpx <- matrix(sapply(1:nrow(sigmax), function(x) GGfunction(x = sigmax[x,], k = i)), nrow = p)
      a[i, ] <- tcrossprod(w, tmpx)
      #a priori error covariance
      # R[[i]] <- tcrossprod(crossprod(t(tmpx - a[i, ]), diag(w)), tmpx - a[i, ]) + mod$W
      R[[i]] <- tcrossprod(crossprod(t(tmpx - a[i, ]), diag(w)), tmpx - a[i, ]) + mod$W * gapFactor
      
      ##measurement update
      
      #compute sigma points
      if (sqrtMethod == "Cholesky") {
        sigmaPlus <- t(chol((p + kappa)*R[[i]]))}
      else {
        tmpys <- La.svd((p + kappa)*R[[i]], nu = 0)
        sigmaPlus <- t(sqrt(tmpys$d)*tmpys$vt)
      }
      sigmay <- t(a[i, ] + cbind(0, sigmaPlus, -sigmaPlus))
      #predicted measurement
      tmpy <- matrix(sapply(1:nrow(sigmay), function(x) FFfunction(x = sigmay[x,], k = i)), nrow = ym)
      f[i, ] <- tcrossprod(w, tmpy)
      #covariance of predicted measurement
      # Qy <- tcrossprod(crossprod(t(tmpy - f[i, ]), diag(w)), tmpy - f[i, ]) + mod$V
      Qy <- tcrossprod(crossprod(t(tmpy - f[i, ]), diag(w)), tmpy - f[i, ]) + mod$V[i]
      #cross covariance between a priori state estimate and predicted measurement
      Qxy <- tcrossprod(crossprod(t(t(sigmay) - a[i, ]), diag(w)), tmpy - f[i, ])
      
      ##a posteriori estimates
      
      #Kalman gain
      Kk <- crossprod(t(Qxy), solve(Qy, tol = 1e-30))
      #a posteriori state estimate
      m[i + 1, ] <- a[i, ] + crossprod(t(Kk), as.matrix(y[i, ] - f[i, ]))
      #a posteriori error covariance
      C[[i + 1]] <- R[[i]] - crossprod(t(Kk), tcrossprod(Qy, Kk))
      
      #compute log-likelihood
      if (logLik) {
        e <- as.matrix(y[i, ] - f[i,])
        ll <- ll + ym*log(2*pi) + sum(log(eigen(Qy)$values)) + crossprod(e, tcrossprod(solve(Qy, tol = 1e-30), t(e)))}
    }
    ans <- list(m = m, C = C, a = a, R = R, f = f)
    
    attributes(ans$f) <- yAttr
    
    if (logLik)
      ans <- c(ans, logLik = 0.5*ll)
    
    if (simplify) 
      ans <- c(mod = list(mod1), kappa = list(kappa), GGfunction = list(GGfunction), FFfunction = list(FFfunction), sqrtMethod = list(sqrtMethod), ans)
    else {
      attributes(y) <- yAttr
      ans <- c(y = list(y), mod = list(mod1), kappa = list(kappa), GGfunction = list(GGfunction), FFfunction = list(FFfunction), sqrtMethod = list(sqrtMethod), ans)
    }
    return(ans)
  }
  UKFsmooth <- function(filterData, GGfunction) {
    
    mod <- filterData
    mAttr <- attributes(mod$m)
    mod$m <- as.matrix(mod$m)
    mod$a <- as.matrix(mod$a)
    mod$W <- as.matrix(mod$mod$W)
    kappa <- mod$kappa
    sqrtMethod <- mod$sqrtMethod
    
    n <- length(mod$R)
    p <- ncol(mod$m)
    w <- as.vector(c(kappa/(p + kappa), rep(1/(2*(p + kappa)), 2*p)))
    s <- rbind(matrix(0, n, p), mod$m[n + 1, ])
    S <- vector("list", length = n + 1)
    
    S[[n + 1]] <- mod$C[[n + 1]]
    
    if (n > 0) 
      for (i in n:1) {
        
        #compute sigma points
        if (sqrtMethod == "Cholesky") {
          sigmaPlus <- t(chol((p + kappa)*mod$C[[i]]))}
        else {
          tmpxs <- La.svd((p + kappa)*mod$C[[i]], nu = 0)
          sigmaPlus <- t(sqrt(tmpxs$d)*tmpxs$vt)}
        sigmax <- t(mod$m[i, ] + cbind(0, sigmaPlus, -sigmaPlus))
        tmpx <- matrix(sapply(1:nrow(sigmax), function(x) GGfunction(x = sigmax[x,], k = i)), nrow = p)
        
        #cross covariance between a priori state estimate (at k+1) and posterior state estimate (at k)
        Qxy <- tcrossprod(crossprod(t(t(sigmax) - mod$m[i, ]), diag(w)), tmpx - mod$a[i, ])
        
        #smoother Kalman gain
        Kk <- crossprod(t(Qxy), solve(mod$R[[i]], tol = 1e-30))
        #smoothed state estimate
        s[i, ] <- mod$m[i, ] + crossprod(t(Kk), s[i + 1, ] - mod$a[i,])
        #smoothed error covariance
        S[[i]] <- mod$C[[i]] + tcrossprod(crossprod(t(Kk), S[[i + 1]] - mod$R[[i]]), Kk)
        
      }
    ans <- list(s = s, S = S)
    
    attributes(ans$s) <- mAttr
    return(ans)
  }
  #Based on http://www.datall-analyse.nl/EKF.R
  dlmExtFilter <- function(y, mod, GGfunction, FFfunction,
                            GGjacobian=NULL, FFjacobian=NULL,
                            logLik=FALSE, simplify=FALSE) {
    
    eps <- .Machine$double.eps^0.3
    mod1 <- mod
    y <- as.matrix(y)
    ym <- ncol(y)
    yAttr <- attributes(y)
    p <- length(mod$m0)
    
    if (!is.null(mod$FF) | !is.null(mod$GG))
      warning("FF or GG matrix will not be used in the EKF")
    
    if (!is.null(mod$JFF) | !is.null(mod$JGG))
      warning("Time varying FF or GG matrix will not be used in the EKF")
    
    if (!is.null(mod$JW) | !is.null(mod$JV))
      warning("Time varying V or W matrix will not be used in the EKF")
    
    m <- rbind(mod$m0, matrix(0, nrow = nrow(y), ncol = length(mod$m0)))
    a <- matrix(0, nrow = nrow(y), ncol = length(mod$m0))
    f <- matrix(0, nrow = nrow(y), ncol = ncol(y))
    dGG.dx <- vector(nrow(y), mode = "list")
    dFF.dx <- vector(nrow(y), mode = "list")
    U.C <- vector(1 + nrow(y), mode = "list")
    D.C <- matrix(0, 1 + nrow(y), length(mod$m0))
    U.R <- vector(nrow(y), mode = "list")
    D.R <- matrix(0, nrow(y), length(mod$m0))
    ll <- 0
    
    # this code is now run below for each observation
    # svdV <- La.svd(mod$V, nu = 0)
    # Uv <- t(svdV$vt)
    # Dv <- sqrt(svdV$d)
    # if (any(Dv < eps)) {
    #   Dv <- pmax(Dv, eps)
    #   warning("a numerically singular 'V' has been slightly perturbed to make it nonsingular")
    # }
    # Dv.inv <- 1/Dv
    # Dv.inv[abs(Dv.inv) == Inf] <- 0
    # sqrtVinv <- Dv.inv * svdV$vt

    # svdW <- La.svd(mod$W, nu = 0)
    # sqrtW <- sqrt(svdW$d) * svdW$vt
    
    tmp <- La.svd(mod$C0, nu = 0)
    U.C[[1]] <- t(tmp$vt)
    D.C[1, ] <- sqrt(tmp$d)
    
    for (i in seq(length = nrow(y))) {
      
      ## Increase the process noise by a factor depending on the number of missing observations from the last one
      ## This implies the series must be sampled regularly with data gaps
      svdV <- La.svd(mod$V[i], nu = 0)
      Uv <- t(svdV$vt)
      Dv <- sqrt(svdV$d)
      if (any(Dv < eps)) {
        Dv <- pmax(Dv, eps)
        warning("a numerically singular 'V' has been slightly perturbed to make it nonsingular")
      }
      Dv.inv <- 1/Dv
      Dv.inv[abs(Dv.inv) == Inf] <- 0
      sqrtVinv <- Dv.inv * svdV$vt
      gapFactor <- 1
      if (i > 1 && info$regular == T) {
        gapFactor <- round((trans$x[i] - trans$x[i - 1]) / info$sampling, digits = 1)
      }
      svdW <- La.svd(mod$W * gapFactor, nu = 0)
      sqrtW <- sqrt(svdW$d) * svdW$vt
      
      if (is.null(GGjacobian)) {
        dGG.dx[[i]] <- jacobian(GGfunction, x = m[i,], k = i)
      } else {
        dGG.dx[[i]] <- matrix(GGjacobian(x = m[i,], k = i), ncol = p, byrow = T)
      }
      
      if (!any(whereNA <- is.na(y[i, ]))) {
        
        a[i, ] <- GGfunction(x = m[i, ], k = i)
        if (is.null(FFjacobian)) {
          dFF.dx[[i]] <- jacobian(FFfunction, x = a[i,], k = i)
        } else {
          dFF.dx[[i]] <- matrix(FFjacobian(x = a[i,], k = i), ncol = p, byrow = T)
        }
        tmp <- La.svd(rbind(D.C[i, ] * t(dGG.dx[[i]] %*% U.C[[i]]), sqrtW), nu = 0)
        U.R[[i]] <- t(tmp$vt)
        D.R[i, ] <- tmp$d
        f[i, ] <- FFfunction(x = a[i, ], k = i)
        D.Rinv <- 1/D.R[i, ]
        D.Rinv[abs(D.Rinv) == Inf] <- 0
        tmp <- La.svd(rbind(sqrtVinv %*% dFF.dx[[i]] %*% U.R[[i]], diag(x = D.Rinv, nrow = length(D.Rinv))), nu = 0)
        U.C[[i + 1]] <- U.R[[i]] %*% t(tmp$vt)
        foo <- 1/tmp$d
        foo[abs(foo) == Inf] <- 0
        D.C[i + 1, ] <- foo
        tF.Vinv <- t(dFF.dx[[i]]) %*% crossprod(sqrtVinv)
        m[i + 1, ] <- a[i, ] + crossprod(D.C[i + 1, ] * t(U.C[[i + 1]])) %*% tF.Vinv %*% as.matrix(y[i, ] - f[i, ])
        if (logLik) {
          e <- as.matrix(y[i, ] - f[i,])
          Rt <- tcrossprod(rep(D.R[i, ], each = p) * U.R[[i]])
          # Qt <- mod$V + dFF.dx[[i]] %*% Rt %*% t(dFF.dx[[i]])
          Qt <- mod$V[i] + dFF.dx[[i]] %*% Rt %*% t(dFF.dx[[i]])
          ll <- ll + ym*log(2*pi) + sum(log(eigen(Qt)$values)) + t(e) %*% solve(Qt) %*% e
        }
      }
      else {
        if (all(whereNA)) {
          m[i + 1, ] <- a[i, ] <- GGfunction(x = m[i, ], k = i)
          tmp <- La.svd(rbind(D.C[i, ] * t(dGG.dx[[i]] %*% U.C[[i]]), sqrtW), nu = 0)
          U.C[[i + 1]] <- U.R[[i]] <- t(tmp$vt)
          D.C[i + 1, ] <- D.R[i, ] <- tmp$d
          if (is.null(FFjacobian)) dFF.dx[[i]] <- jacobian(FFfunction, x = a[i,], k = i)
          else dFF.dx[[i]] <- matrix(FFjacobian(x = a[i,], k = i), ncol = p, byrow = T)
          f[i, ] <- FFfunction(x = a[i, ], k = i)
        }
        
        else {
          good <- !whereNA
          
          a[i, ] <- GGfunction(x = m[i, ], k = i)
          tmp <- La.svd(rbind(D.C[i, ] * t(dGG.dx[[i]] %*% U.C[[i]]), sqrtW), nu = 0)
          U.R[[i]] <- t(tmp$vt)
          D.R[i, ] <- tmp$d
          if (is.null(FFjacobian)) dFF.dx[[i]] <- jacobian(FFfunction, x = a[i,], k = i)
          else dFF.dx[[i]] <- matrix(FFjacobian(x = a[i,], k = i), ncol = p, byrow = T)
          f[i, ] <- FFfunction(x = a[i, ], k = i)
          tmp <- La.svd(mod$V[good, good], nu = 0)
          Dv <- sqrt(tmp$d)
          Dv.inv <- 1/Dv
          Dv.inv[abs(Dv.inv) == Inf] <- 0
          sqrtVinvTMP <- Dv.inv * tmp$vt
          tF.VinvTMP <- t(dFF.dx[[i]][good, , drop = FALSE]) %*% crossprod(sqrtVinvTMP)
          D.Rinv <- 1/D.R[i, ]
          D.Rinv[abs(D.Rinv) == Inf] <- 0
          tmp <- La.svd(rbind(sqrtVinvTMP %*% dFF.dx[[i]][good, , drop = FALSE] %*% U.R[[i]], diag(x = D.Rinv, nrow = length(D.Rinv))), nu = 0)
          U.C[[i + 1]] <- U.R[[i]] %*% t(tmp$vt)
          foo <- 1/tmp$d
          foo[abs(foo) == Inf] <- 0
          D.C[i + 1, ] <- foo
          m[i + 1, ] <- a[i, ] + crossprod(D.C[i + 1, ] * t(U.C[[i + 1]])) %*% tF.VinvTMP %*% as.matrix(y[i, good] - f[i, good])
          if (logLik) {
            e <- as.matrix(y[i, good] - f[i, good])
            Rt <- tcrossprod(rep(D.R[i, ], each = p) * U.R[[i]])
            # Qt <- (mod$V + dFF.dx[[i]] %*% Rt %*% t(dFF.dx[[i]]))[good, good]
            Qt <- (mod$V[i] + dFF.dx[[i]] %*% Rt %*% t(dFF.dx[[i]]))[good, good]
            ll <- ll + sum(good)*log(2*pi) + sum(log(eigen(Qt)$values)) + t(e) %*% solve(Qt) %*% e
          }
        }
      }
    }
    ans <- list(m = m, U.C = U.C, D.C = D.C, a = a, U.R = U.R, D.R = D.R, f = f , dGG.dx = dGG.dx, dFF.dx = dFF.dx)
    
    attributes(ans$f) <- yAttr
    
    if (logLik)
      ans <- c(ans, logLik = 0.5*ll)
    
    if (simplify) 
      ans <- c(mod = list(mod1), GGfunction = list(GGfunction), FFfunction = list(FFfunction), GGjacobian = list(GGjacobian), FFjacobian = list(FFjacobian), ans)
    else {
      attributes(y) <- yAttr
      ans <- c(y = list(y), mod = list(mod1), GGfunction = list(GGfunction), FFfunction = list(FFfunction), GGjacobian = list(GGjacobian), FFjacobian = list(FFjacobian), ans)
    }
    class(ans) <- "dlmFiltered"
    return(ans)
  }
  dlmExtSmooth <- function(filterData) {
    
    big <- 1/sqrt(.Machine$double.eps)
    mod <- filterData
    mAttr <- attributes(mod$m)
    mod$m <- as.matrix(mod$m)
    mod$a <- as.matrix(mod$a)
    mod$W <- as.matrix(mod$mod$W)
    
    n <- length(mod$U.R)
    p <- NCOL(mod$m)
    s <- rbind(matrix(0, n, p), mod$m[n + 1, ])
    U.S <- vector("list", length = n + 1)
    U.S[[n + 1]] <- mod$U.C[[n + 1]]
    D.S <- rbind(matrix(0, n, p), mod$D.C[n + 1, ])
    
    # this code is now run below for each observation
    # svdW <- La.svd(mod$W, nu = 0)
    # Dw <- sqrt(svdW$d)
    # Dw.inv <- pmin(1/Dw, big)
    # sqrtWinv <- Dw.inv * svdW$vt
    
    if (n > 0) 
      for (i in n:1) {
        
        gapFactor <- 1
        if (i > 1 && info$regular == T) {
          gapFactor <- round((trans$x[i] - trans$x[i - 1]) / info$sampling, digits = 1)
        }
        svdW <- La.svd(mod$W * gapFactor, nu = 0)
        Dw <- sqrt(svdW$d)
        Dw.inv <- pmin(1/Dw, big)
        sqrtWinv <- Dw.inv * svdW$vt
        
        Dinv <- 1/mod$D.R[i, ]
        Dinv[abs(Dinv) == Inf] <- 0
        H <- crossprod(mod$D.C[i, ] * t(mod$U.C[[i]])) %*% t(mod$dGG.dx[[i]]) %*% crossprod(Dinv * t(mod$U.R[[i]]))
        Dinv <- 1/mod$D.C[i, ]
        Dinv[abs(Dinv) == Inf] <- 0
        tmp <- La.svd(rbind(sqrtWinv %*% mod$dGG.dx[[i]], Dinv * t(mod$U.C[[i]])), nu = 0)
        Dinv <- 1/tmp$d
        Dinv[abs(Dinv) == Inf] <- 0
        tmp <- La.svd(rbind(Dinv * tmp$vt, D.S[i + 1, ] * t(H %*% U.S[[i + 1]])))
        U.S[[i]] <- t(tmp$vt)
        D.S[i, ] <- tmp$d
        s[i, ] <- mod$m[i, ] + H %*% (s[i + 1, ] - mod$a[i, ])
      }
    ans <- list(s = s, U.S = U.S, D.S = D.S)
    
    attributes(ans$s) <- mAttr
    return(ans)
  }
  # Based on https://github.com/daattali/advanced-shiny/blob/master/busy-indicator/helpers.R
  withBusyIndicatorServer <- function(buttonId, expr) {
    loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
    doneEl <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
    errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
    shinyjs::disable(buttonId)
    shinyjs::show(selector = loadingEl)
    shinyjs::hide(selector = doneEl)
    shinyjs::hide(selector = errEl)
    on.exit({
      shinyjs::enable(buttonId)
      shinyjs::hide(selector = loadingEl)
    })
    tryCatch({
      value <- expr
      shinyjs::show(selector = doneEl)
      shinyjs::delay(4000, shinyjs::hide(selector = doneEl, anim = TRUE, animType = "fade", time = 5))
      value
    })
  }
  midas_vel <- function(m,t,disc) {
    vel_f <- -999999
    vel_b <- -999999
    index_f <- which.min(abs(trans$x - (trans$x[m] + t)))
    index_b <- which.min(abs(trans$x - (trans$x[m] - t)))
    # checking forward pair
    if (abs(trans$x[index_f] - trans$x[m] - t) < trans$tol) {
      if (disc == 1) {
        if (length(trans$offsetEpochs > 0)) {
          if (!any(trans$x[m] < as.numeric(trans$offsetEpochs) & trans$x[index_f] > as.numeric(trans$offsetEpochs))) {
            vel_f <- (trans$y[index_f] - trans$y[m]) / (trans$x[index_f] - trans$x[m])
          }
        }
      } else {
        vel_f <- (trans$y[index_f] - trans$y[m]) / (trans$x[index_f] - trans$x[m])
      }
    }
    # checking backward pair
    if (abs(trans$x[m] - trans$x[index_b] - t) < trans$tol) {
      if (disc == 1) {
        if (length(trans$offsetEpochs > 0)) {
          if (!any(trans$x[index_b] < as.numeric(trans$offsetEpochs) & trans$x[m] > as.numeric(trans$offsetEpochs))) {
            vel_b <- (trans$y[m] - trans$y[index_b]) / (trans$x[m] - trans$x[index_b])
          }
        }
      } else {
        vel_b <- (trans$y[m] - trans$y[index_b]) / (trans$x[m] - trans$x[index_b])
      }
    }
    return(c(vel_f,vel_b))
  }
  #Based on https://stackoverflow.com/questions/5173692/how-to-return-number-of-decimal-places-in-r
  decimalplaces <- function(x) {
    if (any(abs(x - round(x)) > .Machine$double.eps^0.5)) {
      max(na.omit(nchar(lapply(lapply(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE), `length<-`, 2), `[[`, 2))))
    } else {
      return(0)
    }
  }
  trim <- function(x) gsub("^\\s+|\\s+$", "", x)
  average <- function(p,x,y1,y2,y3,sy1,sy2,sy3,tol) {
    index <- x >= x[1] + (p - 1)*as.numeric(isolate(inputs$step)) - tol & x < x[1] + p*as.numeric(isolate(inputs$step)) - tol * 2/3
    if (length(x[index]) == 1) {
      x_ <- x[1] + (p - 0.5)*isolate(inputs$step)
      y1_ <- y1[index]
      y2_ <- y2[index]
      y3_ <- y3[index]
      sy1_ <- sy1[index]
      sy2_ <- sy2[index]
      sy3_ <- sy3[index]
    } else if (length(x[index]) > 1) {
      x_ <- x[1] + (p - 0.5)*isolate(inputs$step)
      y1_ <- weighted.mean(y1[index], 1/(sy1[index])^2)
      y2_ <- weighted.mean(y2[index], 1/(sy2[index])^2)
      y3_ <- weighted.mean(y3[index], 1/(sy3[index])^2)
      sy1_ <- sqrt(1/sum(1/sy1[index]^2))
      sy2_ <- sqrt(1/sum(1/sy2[index]^2))
      sy3_ <- sqrt(1/sum(1/sy3[index]^2))
    } else {
      x_ <- NA
      y1_ <- NA
      y2_ <- NA
      y3_ <- NA
      sy1_ <- NA
      sy2_ <- NA
      sy3_ <- NA
    }
    if (input$format == 4) {
      out <- c(x_,y1_,sy1_)
    } else {
      out <- c(x_,y1_,y2_,y3_,sy1_,sy2_,sy3_)
    }
    return(out)
  }
}

shinyApp(ui = ui, server = server)
