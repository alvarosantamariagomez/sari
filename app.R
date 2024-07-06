### Copyright (C) 2017-2024  Alvaro Santamaria-Gomez, 12 May 2017
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
### along with this program. If not, see <https://www.gnu.org/licenses/>.

# Loading packages ####
suppressPackageStartupMessages(suppressMessages(suppressWarnings({
  library(data.table, verbose = F, quietly = T)
  library(dlm, verbose = F, quietly = T)
  library(fields, verbose = F, quietly = T)
  library(lubridate, verbose = F, quietly = T)
  library(magrittr, verbose = F, quietly = T)
  library(markdown, verbose = F, quietly = T)
  library(matrixStats, verbose = F, quietly = T)
  library(mnormt, verbose = F, quietly = T)
  library(numDeriv, verbose = F, quietly = T)
  library(pracma, verbose = F, quietly = T)
  library(psych, verbose = F, quietly = T)
  library(RColorBrewer, verbose = F, quietly = T)
  library(RCurl, verbose = F, quietly = T)
  library(XML, verbose = F, quietly = T)
  library(jsonlite, verbose = F, quietly = T)
  library(shinyBS, verbose = F, quietly = T)
  library(shinycssloaders, verbose = F, quietly = T)
  library(shinyjs, verbose = F, quietly = T)
  library(shinythemes, verbose = F, quietly = T)
  library(shiny, verbose = F, quietly = T)
  library(spectral, verbose = F, quietly = T)
  library(strucchange, verbose = F, quietly = T)
  library(tseries, verbose = F, quietly = T)
})))

# Function to check and load packages if installed
check_load <- function(packages) {
  for (package in packages) {
    if (package %in% rownames(installed.packages())) {
      suppressPackageStartupMessages(suppressMessages(suppressWarnings(do.call('library', list(package = package, verbose = F, quietly = T)))))
    }
  } 
}

# Shinyapps & local version
# suppressPackageStartupMessages(suppressMessages(suppressWarnings({
#   library(mvcwt, verbose = F, quietly = T) #v1.3.1
#   library(leaflet, verbose = F, quietly = T) #v2.1.2
#   library(geojsonio, verbose = F, quietly = T) #v0.11.3
# })))
# GitHub version
optionalPackages <- c(
  "mvcwt",
  "leaflet",
  "geojsonio"
)
check_load(optionalPackages)

# GUI addons ####

# Help popups (based on https://github.com/daattali/ddpcr/blob/master/inst/shiny/ui/helpers.R)
helpPopup <- function(content, title = NULL, anchor = NULL) {
  if (is.null(anchor)) {
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
  } else {
    a(href = paste0("about.html#",anchor), target = "_blank",
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
}
helpPopupHeader <- function(content, title = NULL) {
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
    icon("circle-question", class = "headerIcon")
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

# CSS class for disabled tabs
withBusyIndicatorCSS <- "
  .btn-loading-container {
    margin-left: 10px;
    font-size: 1.2em;
  }
  .btn-done-indicator {
    color: #61D04F;
  }
  .btn-err {
    margin-top: 10px;
    color: #DF536B;
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
color: gray62 !important;
}'

# show & check plotAll popup
showPopup <- "shinyjs.showPopup = function(file) { overview = window.open('' + file + '', 'plotAll', 'popup=yes, width=1000, height=800, menubar=no, resizable=yes, status=no, titlebar=no, toolbar=no'); }"
checkPopup <- "shinyjs.checkPopup = function() { var status = 'FALSE'; if (typeof overview === 'object') { status = !overview.closed; } Shiny.onInputChange('overview', status);}"

# Update file names from URL/remote (based on https://stackoverflow.com/questions/62626901/r-shiny-change-text-fileinput-after-upload)
update_series <- "
Shiny.addCustomMessageHandler('filename', function(txt) {
  var target = $('#series').parent().parent().parent().find('input[type=text]');
  target.val(txt);
}); "
update_series2 <- "
Shiny.addCustomMessageHandler('filename2', function(txt) {
  var target = $('#series2').parent().parent().parent().find('input[type=text]');
  target.val(txt);
}); "
update_sitelog <- "
Shiny.addCustomMessageHandler('log', function(txt) {
  var target = $('#log').parent().parent().parent().find('input[type=text]');
  target.val(txt);
}); "
update_soln <- "
Shiny.addCustomMessageHandler('soln', function(txt) {
  var target = $('#soln').parent().parent().parent().find('input[type=text]');
  target.val(txt);
}); "
update_custom <- "
Shiny.addCustomMessageHandler('custom', function(txt) {
  var target = $('#custom').parent().parent().parent().find('input[type=text]');
  target.val(txt);
}); "
update_step <- "
Shiny.addCustomMessageHandler('step', function(txt) {
  var target = $('#step').parent().parent().parent().find('input[type=text]');
  target.val(txt);
}); "
update_step2 <- "
Shiny.addCustomMessageHandler('step2', function(txt) {
  var target = $('#step2').parent().parent().parent().find('input[type=text]');
  target.val(txt);
}); "
update_trendRef <- "
Shiny.addCustomMessageHandler('trendRef', function(txt) {
  var target = $('#trendRef').parent().parent().parent().find('input[type=text]');
  target.val(txt);
}); "

# Hide loading page splash, from https://stackoverflow.com/questions/35599470/shiny-dashboard-display-a-dedicated-loading-page-until-initial-loading-of
load_data <- function(seconds) {
  Sys.sleep(seconds)
  hide("loading_page")
  show("main_content")
}

# Setting the layout of the plots in the visualization panel
tabContents <- function(tabNum) {
  if (tabNum == 1) {
    tabName <- uiOutput("tabName1")
  } else if (tabNum == 2) {
    tabName <- uiOutput("tabName2")
  } else if (tabNum == 3) {
    tabName <- uiOutput("tabName3")
  }
  tabPanel(div(style = "font-size: 20px;",tabName), value = tabNum,
           tags$style(type = "text/css", "
                      body {padding-top: 60px;}
                      #side-panel,.navbar-nav {-webkit-user-select: none; -ms-user-select: none; user-select: none;}
                      .shiny-html-output {-webkit-user-select: text; -ms-user-select: text; user-select: text;}
                      "),
           hidden(div(id = paste0("zoomin",tabNum), style = "margin-bottom: -3em; margin-top: 2em; color: #DF536B; font-weight: bold; margin-right: 30px; font-size: 10px; text-align: right; position: relative; z-index: 1;", "Zoomed in")),
           withSpinner(
             plotOutput(paste0("plot",tabNum), click = "plot_1click", dblclick = "plot_2click", brush = brushOpts(id = "plot_brush", resetOnNew = T, fill = "#DF536B", stroke = "gray62", opacity = '0.5', clip = T)),
             type = getOption("spinner.type", default = 1),
             color = getOption("spinner.color", default = "#0080ff"),
             size = getOption("spinner.size", default = 2),
             color.background = getOption("spinner.color.background", default = "#ffffff"),
             custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", "plot1")) NULL else "400px"
           ),
           conditionalPanel(
             condition = "output.run",
             withSpinner(
               plotOutput(paste0("res",tabNum), click = "plot_1click", dblclick = "res_2click", brush = brushOpts(id = "res_brush", resetOnNew = T, fill = "#2297E6", stroke = "gray62", opacity = '0.5', clip = T)),
               type = getOption("spinner.type", default = 1),
               color = getOption("spinner.color", default = "#0080ff"),
               size = getOption("spinner.size", default = 2),
               color.background = getOption("spinner.color.background", default = "#ffffff"),
               custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", "res1")) NULL else "400px"
             )
           ),
           verbatimTextOutput(paste0("plot",tabNum,"_info"), placeholder = F),
           conditionalPanel(
             condition = "output.rate",
             withSpinner(
               plotOutput(paste0("rate",tabNum), click = "plot_1click", dblclick = "rate_2click", brush = brushOpts(id = "rate_brush", resetOnNew = T, fill = "#2297E6", stroke = "gray62", opacity = '0.5', clip = T)),
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
               plotOutput(paste0("vondrak",tabNum), click = "plot_1click", dblclick = "res_2click", brush = brushOpts(id = "vondrak_brush", resetOnNew = T, fill = "#2297E6", stroke = "gray62", opacity = '0.5', clip = T)),
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
               plotOutput(paste0("Vondrak",tabNum), click = "plot_1click", dblclick = "res_2click", brush = brushOpts(id = "vondrak_brush", resetOnNew = T, fill = "#2297E6", stroke = "gray62", opacity = '0.5', clip = T)),
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
               plotOutput(paste0("waveform",tabNum), click = "plot_1click", dblclick = "waveform_2click", brush = brushOpts(id = "waveform_brush", resetOnNew = T, fill = "#2297E6", stroke = "gray62", opacity = '0.5', clip = T)),
               type = getOption("spinner.type", default = 1),
               color = getOption("spinner.color", default = "#0080ff"),
               size = getOption("spinner.size", default = 2),
               color.background = getOption("spinner.color.background", default = "#ffffff"),
               custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", "res1")) NULL else "400px"
             )
           ),
           conditionalPanel(
             condition = "input.printLog == true",
             verbatimTextOutput(paste0("changes_ant",tabNum), placeholder = F),
             verbatimTextOutput(paste0("changes_rec",tabNum), placeholder = F)
           ),
           conditionalPanel(
             condition = "input.printSinfo == true",
             verbatimTextOutput(paste0("changes_ant",tabNum,"s"), placeholder = F),
             verbatimTextOutput(paste0("changes_rec",tabNum,"s"), placeholder = F)
           ),
           conditionalPanel(
             condition = "input.printSoln == true",
             verbatimTextOutput(paste0("changes_ant",tabNum,"so"), placeholder = F),
             verbatimTextOutput(paste0("changes_rec",tabNum,"so"), placeholder = F)
           ),
           conditionalPanel(
             condition = "input.printCustom == true",
             verbatimTextOutput(paste0("changes_ant",tabNum,"c"), placeholder = F)
           ),
           conditionalPanel(
             condition = "input.model.length > 0 || input.midas == true || input.entropy == true || input.optionSecondary == 1",
             verbatimTextOutput(paste0("summary",tabNum), placeholder = F)
           ),
           conditionalPanel(
             condition = "input.histogram == true && input.histogramType > 0",
             plotOutput(paste0("hist",tabNum)),
             verbatimTextOutput(paste0("stats",tabNum))
           ),
           conditionalPanel(
             condition = "input.midas == true",
             plotOutput(paste0("midas_hist",tabNum))
           ),
           div(id = paste0("lomb",tabNum), style = "margin-top: 1em",
               conditionalPanel(
                 condition = "input.spectrumOriginal == true || input.spectrumModel == true || input.spectrumResiduals == true || input.spectrumFilter == true || input.spectrumFilterRes == true",
                 withSpinner(
                   plotOutput(paste0("res",tabNum,"_espectral"), click = "lomb_1click", dblclick = "lomb_2click", brush = brushOpts(id = "lomb_brush", resetOnNew = T, fill = "#2297E6", stroke = "gray62", opacity = '0.5', clip = T)),
                   type = getOption("spinner.type", default = 1),
                   color = getOption("spinner.color", default = "#0080ff"),
                   size = getOption("spinner.size", default = 2),
                   color.background = getOption("spinner.color.background", default = "#ffffff"),
                   custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", "res1")) NULL else "400px"
                 ),
                 downloadLink(paste0("downloadSpectrum",tabNum), div(id = paste0("downloadlink",tabNum), style = "margin-top:0em; margin-bottom:2em; font-size: 10px; text-align: right;","Get periodogram data")),
                 verbatimTextOutput(paste0("lomb",tabNum,"_info"), placeholder = F)
               )
           ),
           div(id = paste0("wl",tabNum),
               conditionalPanel(
                 condition = "input.wavelet == true && input.waveletType.length > 0",
                 withSpinner(
                   plotOutput(paste0("wavelet",tabNum), click = "wavelet_1click"),
                   type = getOption("spinner.type", default = 1),
                   color = getOption("spinner.color", default = "#0080ff"),
                   size = getOption("spinner.size", default = 2),
                   color.background = getOption("spinner.color.background", default = "#ffffff"),
                   custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", "res1")) NULL else "400px"
                 ),
                 verbatimTextOutput(paste0("wavelet",tabNum,"_info"), placeholder = F)
               )
           )
  )
}

# Setting the layout of the model and the residual series for all the components simultaneously
tab3Contents <- function(series) {
  if (series == "3D") {
    tabNum <- 4
    tabName <- uiOutput("tabName4")
  } else if (series == "residuals") {
    tabNum <- 5
    tabName <- uiOutput("tabName5")
  }
  tabPanel(div(style = "font-size: 20px;",tabName), value = tabNum,
           tags$style(type = "text/css", "
                      body {padding-top: 60px;}
                      #side-panel,.navbar-nav {-webkit-user-select: none; -ms-user-select: none; user-select: none;}
                      .shiny-html-output {-webkit-user-select: text; -ms-user-select: text; user-select: text;}
                      "),
           hidden(div(id = paste0("zoomin",tabNum), style = "margin-bottom: -3em; margin-top: 2em; color: #DF536B; font-weight: bold; margin-right: 30px; font-size: 10px; text-align: right; position: relative; z-index: 1;", "Zoomed in")),
           hidden(div(id = paste0("component",tabNum,1), style = "margin: 2em 0em -4.5em 5em; font-weight: bold; font-size: 12px; text-align: left; position: relative; z-index: 1;", uiOutput(paste0("component",tabNum,"1")))),
           withSpinner(
             plotOutput(paste0("plot",tabNum,1), click = paste0("plot",tabNum,"1_1click"), dblclick = paste0("plot",tabNum,"1_2click"), brush = brushOpts(id = paste0("plot",tabNum,"1_brush"), resetOnNew = T, fill = "#DF536B", stroke = "gray62", opacity = '0.5', clip = T)),
             type = getOption("spinner.type", default = 1),
             color = getOption("spinner.color", default = "#0080ff"),
             size = getOption("spinner.size", default = 2),
             color.background = getOption("spinner.color.background", default = "#ffffff"),
             custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", "plot1")) NULL else "400px"
           ),
           div(style = "margin-top: 1em;",
               hidden(div(id = paste0("component",tabNum,2), style = "margin: 0em 0em -4.5em 5em; font-weight: bold; font-size: 12px; text-align: left; position: relative; z-index: 1;", uiOutput(paste0("component",tabNum,"2")))),
               withSpinner(
                 plotOutput(paste0("plot",tabNum,2), click = paste0("plot",tabNum,"2_1click"), dblclick = paste0("plot",tabNum,"2_2click"), brush = brushOpts(id = paste0("plot",tabNum,"2_brush"), resetOnNew = T, fill = "#DF536B", stroke = "gray62", opacity = '0.5', clip = T)),
                 type = getOption("spinner.type", default = 1),
                 color = getOption("spinner.color", default = "#0080ff"),
                 size = getOption("spinner.size", default = 2),
                 color.background = getOption("spinner.color.background", default = "#ffffff"),
                 custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", "plot1")) NULL else "400px"
               )
           ),
           div(style = "margin-top: 1em;",
               hidden(div(id = paste0("component",tabNum,3), style = "margin: 0em 0em -4.5em 5em; font-weight: bold; font-size: 12px; text-align: left; position: relative; z-index: 1;", uiOutput(paste0("component",tabNum,"3")))),
               withSpinner(
                 plotOutput(paste0("plot",tabNum,3), click = paste0("plot",tabNum,"3_1click"), dblclick = paste0("plot",tabNum,"3_2click"), brush = brushOpts(id = paste0("plot",tabNum,"3_brush"), resetOnNew = T, fill = "#DF536B", stroke = "gray62", opacity = '0.5', clip = T)),
                 type = getOption("spinner.type", default = 1),
                 color = getOption("spinner.color", default = "#0080ff"),
                 size = getOption("spinner.size", default = 2),
                 color.background = getOption("spinner.color.background", default = "#ffffff"),
                 custom.css = FALSE, proxy.height = if (grepl("height:\\s*\\d", "plot1")) NULL else "400px"
               )
           ),
           verbatimTextOutput(paste0("plot",tabNum,"_info"), placeholder = F)
  )
}

# Shiny/R general options
options(shiny.fullstacktrace = T, shiny.maxRequestSize = 60*1024^2, width = 280, max.print = 50)
# options(shiny.trace = T)
options(shiny.autoreload = T, shiny.autoreload.pattern = "app.R")
options(scipen = 4)
Sys.setlocale('LC_ALL','C')

# version ####
version <- "SARI julio 2024"

# UI ####
ui <- fluidPage(theme = shinytheme("spacelab"),
                mobileDetect('isMobile'),
                useShinyjs(),
                extendShinyjs(text = showPopup, functions = c("showPopup")),
                extendShinyjs(text = checkPopup, functions = c("checkPopup")),
                div( style = "text-align: center; display: flex; flex-direction: column; justify-content: space-between; margin: auto",
                  id = "loading_page",
                  h1(style = "text-align: center; color: #333333; font-weight: bold", "SARI session established."),
                  h1(style = "text-align: center; color: #333333; font-weight: bold", "Loading user interface ..."),
                  div( style = "margin: 0 auto",
                       img(src = "SARI_logo_animated.gif", width = "80%")
                  )
                ),
                
                # tracker analytics
                # tags$head(includeScript("matomo.js")),
                
                # bugfix for unsolicited scrolling to top of page when clicking on a fileInput button
                # from https://github.com/rstudio/shiny/issues/3327
                tags$script(
                  HTML(
                    'setTimeout(() => $(".shiny-bound-input[type=\'file\']").css("all","unset").css("display", "none"), 750);'
                  )
                ),
                
                # Getting the input elements that are solicited during execution
                tags$script(
                  "$(document).on('shiny:inputchanged', function(event) {
                      if (event.name !== 'changed') {
                        Shiny.setInputValue('changed', event.name);
                      }
                    });"
                ),
                
                # HTTP meta and style header tags
                tags$style(css),
                tags$head(
                  tags$script(src = "ddpcr.js"),
                  tags$head(tags$link(rel = "shortcut icon", href = "favicon.png")),
                  tags$meta(name = "application-name", content = "SARI"),
                  tags$meta(name = "description", content = "Interactive & online GNSS position time series analysis tool"),
                  tags$meta(name = "keywords", content = "SARI,sari,GNSS,gnss,GPS,gps,time,series,analysis"),
                  tags$meta(name = "author", content = "Alvaro Santamaria-Gomez"),
                  tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
                  tags$html(lang = "en"),
                  tags$style(HTML("
                      .help {color: #2297E6; font-weight: bold;}
                      .UIoption {color: #F5C710; font-weight: bold;}
                      .warning {color: #DF536B; font-weight: bold;}
                      .popover {min-width: 21%; color: #ffffff; background-color: #474949; font-size: medium; position: absolute; z-index: 9999;}
                      .arrow { border-left-color: #8447cf; }
                      .navbar-nav { width: 98%;}
                      .navbar-nav li:nth-child(9) { float: right }
                      .navbar-nav li:nth-child(8) { float: right }
                      .navbar-nav li:nth-child(2) { width: 17%; }
                      .navbar-nav li:nth-child(1) { width: 17%; }
                      .tabbable > .nav > li[class=active] > a { background-color: #333333; color:white; }
                      .shiny-notification { word-break: break-word; color: #ffffff; background-color: #446e9b; font-size: large; font-weight: bold; border: 3px solid #333333; padding: 10px 8px 10px 10px; margin: 2px; }
                      .shiny-notification-warning { color: #ffffff; }
                      .shiny-notification-error { color: #F5C710; }
                      .shiny-notification-message { color: #ffffff; background-color: #446e9b; font-size: large; font-weight: bold; border: 3px solid #333333; padding: 10px 8px 10px 10px; margin: 2px; position:fixed; top: 0; left: calc(28%); width: 71%}
                      .shiny-notification-close:hover { color: #ffffff; }
                      .fa-caret-down { float: right; }
                      .headerIcon { color: #ffffff;}
                      ")
                  ),
                  tags$style(type = "text/css", "#inline label{ display: table-cell; text-align: left; vertical-align: middle; padding: 0px 20px;} #inline .form-group { display: table-row; padding: 0px 20px;}"),
                  tags$style(type = 'text/css', 'form.well { height: 96vh; overflow-y: auto; width: 100%}'),
                  tags$style(".modal-body {padding: 10px}
                     .modal-content  {-webkit-border-radius: 6px !important;-moz-border-radius: 6px !important;border-radius: 6px !important;}
                     .modal-dialog { width: 90%; max-width: 600px; vertical-align: center;}
                     .modal { color: #333333; font-weight: bold; text-align: center; padding-right:10px; padding-top: 24px;}"),
                  
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
                            '),
                  
                  # update fileInput file names from URL
                  tags$script(HTML(update_series)),
                  tags$script(HTML(update_series2)),
                  tags$script(HTML(update_sitelog)),
                  tags$script(HTML(update_soln)),
                  tags$script(HTML(update_custom)),
                  tags$script(HTML(update_step)),
                  tags$script(HTML(update_step2)),
                  tags$script(HTML(update_trendRef)),
                  
                  # confirm click on refresh button
                  # uiOutput("refresh")
                ),
                
                hidden(
                  div(id = "main_content",
                      
                      sidebarLayout(position = "left", fluid = T,
                                    div( id = "menu_all",
                                         
                                         # Sidebar menu ####
                                         sidebarPanel(
                                           width = 4,
                                           style = "position:fixed;width:inherit;",
                                           id = "side-panel",
                                           bsCollapse(id = "menu", open = c(1), multiple = T,
                                                      
                                                      # Expandable/collapsible blocks
                                                      
                                                      # * Input data and format ####
                                                      bsCollapsePanel(value = 1,
                                                                      tags$h4(style = "color:white;", icon("database", class = "headerIcon", lib = "font-awesome"), div(style = "color: white; display: inline; text-decoration-line: inherit;", "Input data and format"),
                                                                              div(style = "float: right; margin-right: 10px;",
                                                                                  helpPopupHeader("This block allows uploading and setting the series format, if necessary, before plotting.<br><br>
                                                                                                  Load a series from a local file using the <span class='UIoption'>browse file</span> button or from a remote file using the <span class='UIoption'>server</span>, <span class='UIoption'>product</span>, and <span class='UIoption'>station</span> options.<br><br>
                                                                                                  When loading a local file, ensure that the <span class='UIoption'>series format</span>, <span class='UIoption'>time units</span>, and <span class='UIoption'>series units</span> are correct before plotting.<br><br>
                                                                                                  Check the series format before plotting with the <span class='UIoption'>show series header</span> option.<br><br>
                                                                                                  Activate or deactivate the series error bars with the <span class='UIoption'>use error bars</span> option.<br><br>
                                                                                                  Reduce the sampling of the series with the <span class='UIoption'>reduce sampling</span> option.<br><br>
                                                                                                  See more details in the </i><span class='help'>help</span> tab."))
                                                                      ),
                                                                      div(style = "padding: 0px 0px; margin-top:-2em",
                                                                          fluidRow(
                                                                            column(4,
                                                                                   br(),
                                                                                   div(style = "font-weight: bold", "Input series file"),
                                                                                   div(style = "margin-right: -1em", uiOutput("fileSeries1"))
                                                                            ),
                                                                            column(8,
                                                                                   fileInput(inputId = "series", label = "", multiple = F, buttonLabel = "Browse file ...", placeholder = "Empty")
                                                                            )
                                                                          )
                                                                      ),
                                                                      fluidRow(
                                                                        column(4,
                                                                               selectInput(inputId = "server1", label = "Input series server", choices = list("", "RENAG", "FORMATER", "SONEL", "IGS", "EUREF", "EPOS", "NGL", "JPL", "EARTHSCOPE", "SIRGAS", "EOSTLS", "PSMSL"), selected = "", multiple = F, selectize = T)
                                                                        ),
                                                                        column(4,
                                                                               selectizeInput(inputId = "product1", label = "Product", choices = list(""), selected = "", multiple = F, options = list(maxItems = 1))
                                                                        ),
                                                                        column(4,
                                                                               withBusyIndicatorUI(
                                                                                 uiOutput("station1")
                                                                               )
                                                                        )
                                                                      ),
                                                                      conditionalPanel(
                                                                        condition = "output.series1",
                                                                        div(style = "padding: 0px 0px",
                                                                            fluidRow(
                                                                              column(3,
                                                                                     div(style = "font-weight: bold", "Series ID",
                                                                                         helpPopup("This text field shows the GNSS station ID(s) extracted from the file name(s) of the primary and secondary series.<br>
                                                                                                   The GNSS station ID(s) are used for extracting the corresponding metadata.<br>
                                                                                                   Edit the station ID(s) if necessary.", anchor = "series-id"))
                                                                              ),
                                                                              column(6, 
                                                                                     textInput(inputId = "ids", label = NULL, value = "")
                                                                              )
                                                                            )
                                                                        ),
                                                                        div(style = "padding: 0px 0px; margin-top: 1em",
                                                                            fluidRow(
                                                                              column(4,
                                                                                     div(style = "font-weight: bold", "Series format",
                                                                                         helpPopup("This option sets the series file format. Select 1D if the other file formats are unknown.", anchor = "format"))
                                                                              ),
                                                                              column(8,
                                                                                     radioButtons(inputId = "format", label = NULL, choices = list("NEU/ENU" = 1, "PBO" = 2, "NGL" = 3, "1D" = 4), selected = 1, inline = T, width = "auto"),
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
                                                                                       helpPopup("Enter the column number for the epochs, data and error bars in the series file.", anchor = "format")
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
                                                                        div(style = "padding: 0px 0px; margin-top:-0em",
                                                                            fluidRow(
                                                                              column(4,
                                                                                     div(style = "margin-top:1em",
                                                                                         radioButtons(inputId = "tunits",
                                                                                                      div("Time units",
                                                                                                          helpPopup("This option sets the units of the time axis.<br>These units define the periods of time in several options.", anchor = "tunits")),
                                                                                                      choices = list("Days" = 1, "Weeks" = 2, "Years" = 3), selected = "", inline = F)
                                                                                     ),
                                                                                     div(
                                                                                       radioButtons(inputId = "sunits",
                                                                                                    div("Series units",
                                                                                                        helpPopup("This option sets the units of the variable in the time series.<br>They are used to define the units of the estimated parameters.", anchor = "tunits")),
                                                                                                    choices = list("?" = 0, "m" = 1, "mm" = 2), selected = 0, inline = T)
                                                                                     )
                                                                              ),
                                                                              column(6, offset = 2,
                                                                                     checkboxInput(inputId = "sigmas", label = "Use error bars", value = T),
                                                                                     checkboxInput(inputId = "header", 
                                                                                                   div("Show series header",
                                                                                                       helpPopup("Before plotting the series, this option shows the first lines of the series file.<br>After plotting the series, this option shows the first lines of the series data in the plot.", anchor = "header")),
                                                                                                   value = F),
                                                                                     conditionalPanel(
                                                                                       condition = "input.header == true",
                                                                                       sliderInput(inputId = "lines", label = "Number of lines", min = 1, max = 50, value = 10))
                                                                              )
                                                                            )
                                                                        ),
                                                                        fluidRow(
                                                                          column(6,
                                                                                 checkboxInput(inputId = "average",
                                                                                               div("Reduce sampling",
                                                                                                   helpPopup("This option computes the moving average of the series for a given non-overlapping time pediod.<br>
                                                                                                             The new sampling period must have a value between the time series sampling and half the time series length.<br>
                                                                                                             The new sampling period must be given in the same units as the time axis of the series.<br>
                                                                                                             Expressions are allowed starting by <span class='UIoption'>=</span>,<br>
                                                                                                             as in <span class='UIoption'>=7/365.25</span>, for a week period in units of years.", anchor = "average")),
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
                                                                        conditionalPanel(
                                                                          condition = "output.data",
                                                                          div(style = "padding: 0px 0px; margin-top: -1em",
                                                                              tags$hr(style = "border-color: #333333; border-top: 1px solid #333333;")
                                                                          ),
                                                                          conditionalPanel(
                                                                            condition = "output.location == true",
                                                                            fluidRow(
                                                                              column(6,
                                                                                     div(style = "padding: 0px 0px; margin-top: -1em", htmlOutput("information1"))
                                                                              ),
                                                                              column(6,
                                                                                     uiOutput("map")
                                                                              )
                                                                            )
                                                                          ),
                                                                          conditionalPanel(
                                                                            condition = "output.location == false",
                                                                            div(style = "padding: 0px 0px; margin-top: -1em", htmlOutput("information2"))
                                                                          )
                                                                        )
                                                                      ),
                                                                      style = "primary"),
                                                      
                                                      # * Plot controls ####
                                                      bsCollapsePanel(value = 2,
                                                                      tags$h4(style = "color:white;", icon("gamepad", class = "headerIcon", lib = "font-awesome"), div(style = "color: white; display: inline; text-decoration-line: inherit;", "Plot controls"),
                                                                         div(style = "float: right; margin-right: 10px;",
                                                                             helpPopupHeader("This block allows plotting/resetting the time series with the <span class='UIoption'>plot</span> and <span class='UIoption'>reset</span> buttons.<br><br>
                                                                                             The <span class='UIoption'>overview</span> button opens a new browser window containing a plot of the three coordinate components, if available.<br><br>
                                                                                             Outliers can be excluded manually or automatically with the <span class='UIoption'>toggle</span> and <span class='UIoption'>auto toggle</span> buttons.<br><br>
                                                                                             Removed outliers can be restored with the <span class='UIoption'>reset toggle</span> button.<br><br>
                                                                                             The <span class='UIoption'>truncate</span> option removes the beginning and/or end of the series.<br><br>
                                                                                             The <span class='UIoption'>all components</span> option removes the outliers from all the components (if more than one) simultaneously.<br><br>
                                                                                             The <span class='UIoption'>permanent</span> option permanently deletes (i.e., not possible to be restored) the next outliers flagged to be toggled or truncated.<br><br>
                                                                                             The <span class='UIoption'>include in file</span> option keeps the excluded outliers in the downloaded results file as commented lines.<br><br>
                                                                                             The <span class='UIoption'>scrolling</span> option enables/disables the vertical scrolling of the left panel.<br><br>
                                                                                             See more details in the <span class='help'>help</span> tab."))
                                                                      ),
                                                                      fluidRow(
                                                                        column(2, style = 'padding:0px 1px 0px 10px;', align = "left",
                                                                               actionButton(inputId = "plot", label = "Plot", icon = icon("eye", class = NULL, lib = "font-awesome"), style = "font-size: small")
                                                                        ),
                                                                        column(3, style = 'padding:0px 1px 0px 1px;', align = "left",
                                                                               actionButton(inputId = "plotAll", label = "Overview", icon = icon("bars", class = NULL, lib = "font-awesome"), style = "font-size: small")
                                                                        ),
                                                                        column(2, style = 'padding:0px 1px 0px 1px;', align = "right",
                                                                               actionButton(inputId = "reset", label = "Reset", icon = icon("trash", class = NULL, lib = "font-awesome"), style = "font-size: small; color: #F5C710; font-weight: bold")
                                                                        ),
                                                                        column(2, style = 'padding:0px 1px 0px 1px;', align = "right",
                                                                               actionButton(inputId = "remove", label = "Toggle", icon =  icon("ban", class = NULL, lib = "font-awesome"), style = "font-size: small")
                                                                        ),
                                                                        column(3, style = 'padding:0px 10px 0px 0px;', align = "right",
                                                                               actionButton(inputId = "delete_excluded", label = "Reset toggle", icon = icon("backward", class = NULL, lib = "font-awesome"), style = "font-size: small")
                                                                        )
                                                                      ),
                                                                      div(style = "padding: 0px 0px; margin-top:1em",
                                                                          fluidRow(
                                                                            column(4,
                                                                                   checkboxInput(inputId = "cut",
                                                                                                 div("Truncate", style = "font-weight: bold",
                                                                                                     helpPopup("This option reduces the time axis of the series by removing all points before and/or after the provided epochs.", anchor = "cut")),
                                                                                                 value = F)
                                                                            ),
                                                                            conditionalPanel(
                                                                              condition = "input.cut == true",
                                                                              column(4,
                                                                                     textInput(inputId = "cutStart", label = "Before", value = "")
                                                                              ),
                                                                              column(4,
                                                                                     textInput(inputId = "cutEnd", label = "After", value = "")
                                                                              )
                                                                            )
                                                                          )
                                                                      ),
                                                                      div(style = "padding: 0px 0px; margin-top:1em",
                                                                          fluidRow(
                                                                            column(4,
                                                                                   textInput(inputId = "thresholdRes",
                                                                                             div("Residual",
                                                                                                 helpPopup("Enter the threshold to delete all the points with larger absolute residual.", anchor = "threshold")),
                                                                                             value = NULL)
                                                                            ),
                                                                            column(4, style = "padding:0px 10px 0px 0px;", align = "left",
                                                                                   textInput(inputId = "thresholdResN",
                                                                                             div("Norm. residual",
                                                                                                 helpPopup("Enter the threshold to delete all the points with larger normalized absolute residual.<br>
                                                                                                           The residuals values are normalized by their error bars.", anchor = "threshold")),
                                                                                             value = NULL)
                                                                            ),
                                                                            column(4, style = "padding:0px 10px 0px 0px; margin-top:1.75em", align = "right",
                                                                                   actionButton(inputId = "removeAuto", label = "Auto toggle", icon =  icon("car", class = NULL, lib = "font-awesome"), style = "font-size: small")
                                                                            )
                                                                          )
                                                                      ),
                                                                      fluidRow(
                                                                        column(3,
                                                                               div(style = "font-weight: bold", "Plot type")
                                                                        ),
                                                                        column(8, 
                                                                               radioButtons(inputId = "symbol", label = NULL, choices = list("Points" = 0, "Lines" = 1, "Points & Lines" = 2), selected = 0, inline = T)
                                                                        )
                                                                      ),
                                                                      div(style = "font-weight: bold", "Plot options"),
                                                                      fluidRow(
                                                                        column(4,
                                                                               checkboxInput(inputId = "remove3D",
                                                                                             div("All components", align = "right",
                                                                                                 helpPopup("This option toggles or deletes the points from all the coordinate components at the same time.", anchor = "3d")),
                                                                                             value = T)
                                                                        ),
                                                                        column(4,
                                                                               checkboxInput(inputId = "permanent",
                                                                                             div("Permanent", align = "right",
                                                                                                 helpPopup("This option deletes the points from the series permanently.<br>
                                                                                                           Deleted points cannot be restored, unless the series are reset and reloaded.", anchor = "permanent")),
                                                                                             value = F)
                                                                        ),
                                                                        column(4,
                                                                               checkboxInput(inputId = "add_excluded",
                                                                                             div("Include in file", align = "right",
                                                                                                 helpPopup("This option includes the removed points in the downloaded file as commented lines.", anchor = "excluded")),
                                                                                             value = F)
                                                                        )
                                                                      ),
                                                                      fluidRow(
                                                                        column(4,
                                                                               checkboxInput(inputId = "overflow",
                                                                                             div("Scrolling",
                                                                                                 helpPopup("This options enables or disables the vertical scrolling of the left panel.<br>
                                                                                                           When the scrolling is disabled, the user can take a screenshot of the full web page.", anchor = "scrolling")),
                                                                                             value = T)
                                                                        )
                                                                      ),
                                                                      style = "primary"),
                                                      
                                                      # * Ancillary information ####
                                                      bsCollapsePanel(value = 3,
                                                                      tags$h4(style = "color:white;", icon("upload", class = "headerIcon", lib = "font-awesome"), div(style = "color: white; display: inline; text-decoration-line: inherit;", "Ancillary information"),
                                                                              div(style = "float: right; margin-right: 10px;",
                                                                                  helpPopupHeader("This block allows for the uploading of files containing complementary information or metadata related to the analysis of the series.<br><br>
                                                                                                  The user can upload any of the following possibilities:<br>
                                                                                                  a <span class='UIoption'>SARI</span> file,<br>
                                                                                                  a GNSS <span class='UIoption'>sitelog</span>,<br>
                                                                                                  a GAMIT-like <span class='UIoption'>station.info</span> file,<br>
                                                                                                  an IGS-like <span class='UIoption'>soln</span> file,<br>
                                                                                                  a <span class='UIoption'>custom</span> offset file,<br>
                                                                                                  a <span class='UIoption'>secondary</span> series.<br>
                                                                                                  The secondary series can be <span class='UIoption'>shown</span> next to the primary series or used to either <span class='UIoption'>correct</span> the primary series or to <span class='UIoption'>average</span> both the primary and secondary series.<br><br>
                                                                                                  Two model predictions can also be plotted or used to correct the primary series: a <span class='UIoption'>plate motion</span> model and a <span class='UIoption'>GIA</span> model.<br><br>
                                                                                                  See more details in the <span class='help'>help</span> tab."))
                                                                      ),
                                                                      
                                                                      ## % SARI model ####
                                                                      div(style = "padding: 0px 0px; margin-top:0em",
                                                                          fluidRow(
                                                                            column(6,
                                                                                   div(style = "font-weight: bold", "Load SARI model",
                                                                                       helpPopup("This option allows loading a file with the model fitted in a previous analysis with SARI.", anchor = "ancillary-information")
                                                                                   )
                                                                            )
                                                                          ),
                                                                          fluidRow(
                                                                            column(8,
                                                                                   fileInput(inputId = "loadSARI", label = NULL, multiple = F, buttonLabel = "Browse file ...", placeholder = "Empty")
                                                                            )
                                                                          )
                                                                      ),
                                                                      
                                                                      ## % sitelog ####
                                                                      div(style = "padding: 0px 0px; margin-top:0em",
                                                                          fluidRow(
                                                                            column(6,
                                                                                   div(style = "font-weight: bold", "Input log file",
                                                                                       helpPopup("This option allows loading an IGS-like sitelog file.", anchor = "notes-on-the-equipment-change-logs")
                                                                                   )
                                                                            ),
                                                                            column(6, align = "right",
                                                                                   uiOutput("sitelog")
                                                                            )
                                                                          ),
                                                                          fluidRow(
                                                                            column(8,
                                                                                   fileInput(inputId = "log", label = NULL, multiple = F, buttonLabel = "Browse file ...", placeholder = "Empty")
                                                                            ),
                                                                            column(4,
                                                                                   div(style = "padding: 0px 0px; margin-top:0em",
                                                                                       conditionalPanel(
                                                                                         condition = "output.log",
                                                                                         checkboxInput(inputId = "traceLog", label = "Plot changes", value = F),
                                                                                         checkboxInput(inputId = "printLog", label = "List changes", value = F)
                                                                                       )
                                                                                   )
                                                                            )
                                                                          )
                                                                      ),
                                                                      
                                                                      ## % station.info ####
                                                                      div(style = "padding: 0px 0px; margin-top:-1em",
                                                                          fluidRow(
                                                                            column(6,
                                                                                   div(style = "font-weight: bold", "Input station.info file",
                                                                                       helpPopup("This option allows loading a GAMIT/GLOBK station.info file.", anchor = "notes-on-the-equipment-change-logs")
                                                                                   )
                                                                            ),
                                                                            column(6, align = "right",
                                                                                   uiOutput("station.info")
                                                                            )
                                                                          ),
                                                                          fluidRow(
                                                                            column(8,
                                                                                   fileInput(inputId = "sinfo", label = NULL, multiple = F, buttonLabel = "Browse file ...", placeholder = "Empty")
                                                                            ),
                                                                            column(4,
                                                                                   div(style = "padding: 0px 0px; margin-top:0em",
                                                                                       conditionalPanel(
                                                                                         condition = "output.sinfo",
                                                                                         checkboxInput(inputId = "traceSinfo", label = "Plot changes", value = F),
                                                                                         checkboxInput(inputId = "printSinfo", label = "List changes", value = F)
                                                                                       )
                                                                                   )
                                                                            )
                                                                          )
                                                                      ),
                                                                      
                                                                      ## % soln ####
                                                                      div(style = "padding: 0px 0px; margin-top:-1em",
                                                                          fluidRow(
                                                                            column(6,
                                                                                   div(style = "font-weight: bold", "Input soln file",
                                                                                       helpPopup("This option allows loading an IGS-like soln file.", anchor = "notes-on-the-equipment-change-logs")
                                                                                   )
                                                                            ),
                                                                            column(6, align = "right",
                                                                                   uiOutput("solnFile")
                                                                            )
                                                                          ),
                                                                          fluidRow(
                                                                            column(8,
                                                                                   fileInput(inputId = "soln", label = NULL, multiple = F, buttonLabel = "Browse file ...", placeholder = "Empty")
                                                                            ),
                                                                            column(4,
                                                                                   div(style = "padding: 0px 0px; margin-top:0em",
                                                                                       conditionalPanel(
                                                                                         condition = "output.soln",
                                                                                         checkboxInput(inputId = "traceSoln", label = "Plot changes", value = F),
                                                                                         checkboxInput(inputId = "printSoln", label = "List changes", value = F)
                                                                                       )
                                                                                   )
                                                                            )
                                                                          )
                                                                      ),
                                                                      
                                                                      ## % Custom ####
                                                                      div(style = "padding: 0px 0px; margin-top:-1em",
                                                                          fluidRow(
                                                                            column(6,
                                                                                   div(style = "font-weight: bold", "Input custom offset file",
                                                                                       helpPopup("This option allows loading a user-defined offset list.", anchor = "notes-on-the-equipment-change-logs")
                                                                                   )
                                                                            ),
                                                                            column(6, align = "right",
                                                                                   uiOutput("customFile")
                                                                            )
                                                                          ),
                                                                          fluidRow(
                                                                            column(8,
                                                                                   fileInput(inputId = "custom", label = NULL, multiple = F, buttonLabel = "Browse file ...", placeholder = "Empty")
                                                                            ),
                                                                            column(4,
                                                                                   div(style = "padding: 0px 0px; margin-top:0em",
                                                                                       conditionalPanel(
                                                                                         condition = "output.custom",
                                                                                         checkboxInput(inputId = "traceCustom", label = "Plot changes", value = F),
                                                                                         checkboxInput(inputId = "printCustom", label = "List changes", value = F)
                                                                                       )
                                                                                   )
                                                                            )
                                                                          )
                                                                      ),
                                                                      
                                                                      ## % Secondary series ####
                                                                      div(style = "padding: 0px 0px; margin-top:-1em",
                                                                          fluidRow(
                                                                            column(7,
                                                                                   div(style = "margin-right:0; padding-right:0",
                                                                                       fileInput(inputId = "series2",
                                                                                                 div("Secondary series file",
                                                                                                     helpPopup("This option allows loading a secondary series to be shown next to, subtracted from or averaged with the primary series.", anchor = "notes-on-the-secondary-series")),
                                                                                                 multiple = T, buttonLabel = "Browse file ...", placeholder = "Empty")
                                                                                   )
                                                                            ),
                                                                            column(2, align = "right",
                                                                                   div(style = "padding: 0px 0px; margin-top:+1.9em",
                                                                                       actionButton(inputId = "swap", label = "Swap", icon = icon("refresh", class = NULL, lib = "font-awesome"), style = "font-size: small")
                                                                                   )
                                                                            ),
                                                                            column(3,
                                                                                   div(style = "padding: 0px 0px; margin-top:0em",
                                                                                       conditionalPanel(
                                                                                         condition = "output.series2",
                                                                                         radioButtons(inputId = "optionSecondary", label = NULL, choices = list("None" = 0, "Show" = 1, "Correct" = 2, "Average" = 3), selected = NULL, inline = F, width = NULL),
                                                                                       )
                                                                                   )
                                                                            )
                                                                          ),
                                                                          fluidRow(
                                                                            column(4,
                                                                                   selectInput(inputId = "server2", label = "Secondary series server", choices = list("", "RENAG", "FORMATER", "SONEL", "IGS", "EUREF", "EPOS", "NGL", "JPL", "EARTHSCOPE", "SIRGAS", "EOSTLS", "PSMSL"), selected = "", multiple = F, selectize = T)
                                                                            ),
                                                                            column(4,
                                                                                   selectizeInput(inputId = "product2", label = "Product", choices = list(""), selected = "", multiple = F, options = list(maxItems = 1))
                                                                            ),
                                                                            column(4,
                                                                                   withBusyIndicatorUI(
                                                                                     uiOutput("station2")
                                                                                   )
                                                                            )
                                                                          ),
                                                                          conditionalPanel(
                                                                            condition = "output.series2",
                                                                            fluidRow(
                                                                              column(8,
                                                                                     # radioButtons(inputId = "typeSecondary", label = NULL, choices = list("Original" = 1, "Residual" = 2), selected = 1, inline = T),
                                                                                     radioButtons(inputId = "format2", label = NULL, choices = list("NEU/ENU" = 1, "PBO" = 2, "NGL" = 3, "1D" = 4), selected = 1, inline = T, width = "auto"),
                                                                                     fluidRow(
                                                                                       column(6,
                                                                                              textInput(inputId = "scaleFactor",
                                                                                                        div("Scale factor",
                                                                                                            helpPopup("Enter the multiplicative coefficient to scale the y-axis of the secondary series.", anchor = "notes-on-the-secondary-series")),
                                                                                                        value = "1")
                                                                                       ),
                                                                                       column(6,
                                                                                              textInput(inputId = "step2",
                                                                                                        div("Averaging",
                                                                                                            helpPopup("This option computes the moving average of the secondary series for a given non-overlapping time pediod between the time series sampling and half the time series length.<br>
                                                                                                                      The period must be given in the same units as the time axis in the series.<br>
                                                                                                                      Expressions are allowed starting by <span class='UIoption'>=</span>, as in <span class='UIoption'>=7/365.25</span>.", anchor = "notes-on-the-secondary-series")),
                                                                                                        value = "")
                                                                                       )
                                                                                     ),
                                                                                     div(style = "margin-right: -1em", uiOutput("fileSeries2"))
                                                                              ),
                                                                              column(4,
                                                                                     div(style = "padding: 0px 0px; margin-top:1em",
                                                                                         checkboxInput(inputId = "fullSeries",
                                                                                                       div("Full series",
                                                                                                           helpPopup("This option shows the total length of the secondary series.<br>
                                                                                                                     By default, only the common time period with the primary series will be shown.", anchor = "notes-on-the-secondary-series")),
                                                                                                       value = F),
                                                                                         checkboxInput(inputId = "sameScale",
                                                                                                       div("Same scale",
                                                                                                           helpPopup("This option forces the y-axis of the secondary series on the right of the plot to have the same scale as the y-axis of the primary series on the left.", anchor = "notes-on-the-secondary-series")),
                                                                                                       value = F),
                                                                                         checkboxInput(inputId = "same_axis",
                                                                                                       div("Same axis",
                                                                                                           helpPopup("This option forces the y-axis of the secondary series on the right of the plot to be the same as the y-axis of the primary series on the left.", anchor = "notes-on-the-secondary-series")),
                                                                                                       value = F),
                                                                                         checkboxInput(inputId = "ne",
                                                                                                       div(HTML("N @ E"),
                                                                                                           helpPopup("This option swaps the columns of the North and East components of the secondary series to match those of the primary series.", anchor = "notes-on-the-secondary-series")),
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
                                                                                         helpPopup("Enter the column number for the epochs, data and the errorbars, respectively, of the secondary series.", anchor = NULL)
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
                                                                      
                                                                      div(style = "padding: 0px 0px; margin-top: -1em",
                                                                          tags$hr(style = "border-color: #333333; border-top: 1px solid #333333;")
                                                                      ),
                                                                      
                                                                      ## % Euler ####
                                                                      fluidRow(
                                                                        column(6,
                                                                               checkboxInput(inputId = "euler",
                                                                                             div(style = "font-weight: bold", "Plate motion model",
                                                                                                 helpPopup("This option shows or removes a plate motion model at the series location given the parameters of an Euler pole.", anchor = "notes-on-the-plate-motion-model")),
                                                                                             value = F)
                                                                               ),
                                                                        column(6,
                                                                               conditionalPanel(
                                                                                 condition = "input.euler == true",
                                                                                 div(style = "margin-top: 0.75em",
                                                                                     radioButtons(inputId = "eulerType", label = NULL, choices = list("None" = 0, "Show" = 1, "Remove" = 2), selected = 0, inline = T, width = NULL, choiceNames = NULL,  choiceValues = NULL)
                                                                                 )
                                                                               )
                                                                        )
                                                                      ),
                                                                      fluidRow(
                                                                        conditionalPanel(
                                                                          condition = "input.euler == true && input.format == 1",
                                                                          column(6,
                                                                                 div("Select NEU or ENU format:")
                                                                          ),
                                                                          column(6,
                                                                                 radioButtons(inputId = "neuenu", label = NULL, choices = list("ENU" = 1, "NEU" = 2), selected = 1, inline = T, width = NULL, choiceNames = NULL, choiceValues = NULL)
                                                                          )
                                                                        ),
                                                                        conditionalPanel(
                                                                          condition = "input.euler == true && input.format == 4",
                                                                          column(6,
                                                                                 div("Select component of 1D series:")
                                                                          ),
                                                                          column(6,
                                                                                 radioButtons(inputId = "neu1D", label = NULL, choices = list("North" = 1, "East" = 2, "Up" = 3), selected = 1, inline = T, width = NULL, choiceNames = NULL, choiceValues = NULL)
                                                                          )
                                                                        )
                                                                      ),
                                                                      conditionalPanel(
                                                                        condition = "input.euler == true",
                                                                        fluidRow(
                                                                          column(6,
                                                                                 selectInput(inputId = "plateModel", label = "Select a plate model", choices = list("", "ITRF2020", "NNR-MORVEL56", "NNR-GSRM"), selected = "", multiple = F, selectize = T),
                                                                                 div(style = "margin-top: -1em", uiOutput("pmm"))
                                                                          ),
                                                                          column(6,
                                                                                 selectizeInput(inputId = "plate", label = "Plate name", choices = list(""), selected = "", multiple = F, options = list(maxItems = 1))
                                                                          )
                                                                        ),
                                                                        fluidRow(
                                                                          div(style = "margin-top: 2em",
                                                                              column(8,
                                                                                     div(style = "font-weight: bold", "Upload a custom plate model",
                                                                                         helpPopup("This option allows loading a file with a list of station coordinates and their associated Euler poles.", anchor = "notes-on-the-plate-motion-model")
                                                                                     )
                                                                              ),
                                                                              column(4, align = "right",
                                                                                     tags$a(href = "euler.txt", "Show file example", target = "_blank")
                                                                              )
                                                                          )
                                                                        ),
                                                                        fileInput(inputId = "eulers", label = NULL, multiple = F, buttonLabel = "Browse file ...", placeholder = "Empty"),
                                                                        fluidRow(
                                                                          column(6,
                                                                                 div("Station coordinates", helpPopup("Option 1: Cartesian coordinates in the same units as the series.<br>
                                                                                                                      Option 2: geographic coordinates in decimal degrees.", anchor = "notes-on-the-plate-motion-model"))
                                                                          ),
                                                                          column(6, align = "right",
                                                                                 radioButtons(inputId = "station_coordinates", label = NULL, choices = list("Cartesian" = 1, "Geographic" = 2), selected = 1, inline = T, width = NULL, choiceNames = NULL,  choiceValues = NULL)
                                                                          )
                                                                        ),
                                                                        fluidRow(
                                                                          conditionalPanel(
                                                                            condition = "input.station_coordinates == 1",
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
                                                                            condition = "input.station_coordinates == 2",
                                                                            column(4,
                                                                                   textInput(inputId = "station_lat", label = "Station latitude", value = "")
                                                                            ),
                                                                            column(4, offset = -2,
                                                                                   textInput(inputId = "station_lon", label = "Station longitude", value = "")
                                                                            )
                                                                          )
                                                                        ),
                                                                        conditionalPanel(
                                                                          condition = "output.series2",
                                                                          fluidRow(
                                                                            column(6,
                                                                                   div("Secondary station coordinates", helpPopup("Option 1: Cartesian coordinates in the same units as the series.<br>
                                                                                                                                  Option 2: geographic coordinates in decimal degrees.", anchor = "notes-on-the-plate-motion-model"))
                                                                            ),
                                                                            column(6, align = "right",
                                                                                   radioButtons(inputId = "station_coordinates2", label = NULL, choices = list("Cartesian" = 1, "Geographic" = 2), selected = 1, inline = T, width = NULL, choiceNames = NULL,  choiceValues = NULL)
                                                                            )
                                                                          ),
                                                                          fluidRow(
                                                                            conditionalPanel(
                                                                              condition = "input.station_coordinates2 == 1",
                                                                              column(4,
                                                                                     textInput(inputId = "station_x2", label = "Station X", value = "")
                                                                              ),
                                                                              column(4, offset = -2,
                                                                                     textInput(inputId = "station_y2", label = "Station Y", value = "")
                                                                              ),
                                                                              column(4, offset = -2,
                                                                                     textInput(inputId = "station_z2", label = "Station Z", value = "")
                                                                              )
                                                                            ),
                                                                            conditionalPanel(
                                                                              condition = "input.station_coordinates2 == 2",
                                                                              column(4,
                                                                                     textInput(inputId = "station_lat2", label = "Station latitude", value = "")
                                                                              ),
                                                                              column(4, offset = -2,
                                                                                     textInput(inputId = "station_lon2", label = "Station longitude", value = "")
                                                                              )
                                                                            )
                                                                          )
                                                                        ),
                                                                        fluidRow(
                                                                          column(6,
                                                                                 div("Euler's pole", helpPopup("Option 1: Cartesian rotation rates in decimal degrees/Ma.<br>
                                                                                                               Option 2: geographic pole position in decimal degrees and rotation rate in decimal degrees/Ma.", anchor = "notes-on-the-plate-motion-model"))
                                                                          ),
                                                                          column(6, align = "right",
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
                                                                        )
                                                                      ),
                                                                      
                                                                      ## % GIA ####
                                                                      fluidRow(
                                                                        column(4,
                                                                               checkboxInput(inputId = "gia",
                                                                                             div(style = "font-weight: bold", "GIA",
                                                                                                 helpPopup("This option shows or removes the vertical land motion predicted by a Glacial Isostatic Adjustment model at the series location.", anchor = "notes-on-the-gia-model")),
                                                                                             value = F)
                                                                        )
                                                                      ),
                                                                      conditionalPanel(
                                                                        condition = "input.gia == true",
                                                                        fluidRow(
                                                                          column(6,
                                                                                 selectInput(inputId = "giaModel", label = "Select a GIA model", choices = list("", "Caron & Ivins", "ICE-6G-VM5a", "ICE-6G-ANU"), selected = "", multiple = F, selectize = T)
                                                                          ),
                                                                          column(6,
                                                                                 conditionalPanel(
                                                                                   condition = "input.gia == true",
                                                                                   div(style = "margin-top: 2em",
                                                                                       radioButtons(inputId = "giaType", label = NULL, choices = list("None" = 0, "Show" = 1, "Remove" = 2), selected = 0, inline = T, width = NULL, choiceNames = NULL,  choiceValues = NULL)
                                                                                   )
                                                                                 )
                                                                          )
                                                                        )
                                                                      ),
                                                                      conditionalPanel(
                                                                        condition = "input.gia == true",
                                                                        fluidRow(
                                                                          column(6,
                                                                                 textInput(inputId = "giaTrend", 
                                                                                           div(style = "font-weight: bold", "Vertical land motion trend",
                                                                                               helpPopup("This text field shows the vertical land motion trend in the same units as the series.<br>
                                                                                                         Modify the value of the VLM trend if necessary.", anchor = "notes-on-the-gia-model")),
                                                                                           value = "")
                                                                          ),
                                                                          conditionalPanel(
                                                                            condition = "output.series2",
                                                                            column(6,
                                                                                   textInput(inputId = "giaTrend2", "Secondary", value = "")
                                                                            )
                                                                          )
                                                                        )
                                                                      ),
                                                                      
                                                                      style = "primary"),
                                                      
                                                      # * Fit controls ####
                                                      bsCollapsePanel(value = 4,
                                                                      tags$h4(style = "color:white;", icon("wand-magic-sparkles", class = "headerIcon", lib = "font-awesome"), div(style = "color: white; display: inline; text-decoration-line: inherit;", "Fit controls"),
                                                                              div(style = "float: right; margin-right: 10px;",
                                                                                  helpPopupHeader("This block allows fitting a model to the time series using:<br>
                                                                                                  Weighted <span class='UIoption'>least squares</span> (LS)<br>
                                                                                                  <span class='UIoption'>Extended Kalman filter</span> (EKF)<br>
                                                                                                  <span class='UIoption'>Unscented Kalman filter</span> (UKF)<br><br>
                                                                                                  The fitted model may include any combination of <span class='UIoption'>linear</span> trend, higher-degree <span class='UIoption'>polynomial</span>, <span class='UIoption'>offsets</span>, <span class='UIoption'>sinusoidal</span> periodic signals, <span class='UIoption'>exponential</span> and <span class='UIoption'>logarithmic</span> decays.<br><br>
                                                                                                  The <span class='UIoption'>search discontinuities</span> button provides an automatic guesstimate of the location of probable discontinuities in the series.<br><span class='warning'>WARNING</span>: long computation time.<br><br>
                                                                                                  The <span class='UIoption'>check offsets</span> option checks the significance of the offset magnitudes with respect to colored noise.<br><br>
                                                                                                  See more details in the <span class='help'>help</span> tab."))
                                                                      ),
                                                                      div(style = "padding: 0px 0px; margin-top:0em",
                                                                          fluidRow(
                                                                            column(3,
                                                                                   div(style = "font-weight: bold", "Fit type")
                                                                            ),
                                                                            column(8, 
                                                                                   radioButtons(inputId = "fitType", label = NULL, choices = list("None" = 0, "LS" = 1, "KF" = 2), selected = 0, inline = T)
                                                                            )
                                                                          ),
                                                                          conditionalPanel(
                                                                            condition = "input.fitType == 2",
                                                                            fluidRow(
                                                                              column(3,
                                                                                     div(style = "font-weight: bold", "KF type")
                                                                                     ),
                                                                              column(5,
                                                                                     radioButtons(inputId = "kf", label = NULL, choices = list("EKF" = 1, "UKF" = 2), selected = 2, inline = T)
                                                                              ),
                                                                              column(width = 4, offset = 0, style = "margin-top:-2em; padding: 0px 40px 0px 0px", align = "right",
                                                                                     withBusyIndicatorUI(
                                                                                       actionButton(inputId = "runKF", label = " Run KF", icon = icon("filter", class = NULL, lib = "font-awesome"), style = "font-size: small")
                                                                                     )
                                                                              )
                                                                            ),
                                                                            checkboxInput(inputId = "errorm",
                                                                                          div("Optimize measurement noise",
                                                                                              helpPopup("This option estimates the measurement noise within the given bounds and with respect to the provided process noise.<br>
                                                                                                        <span class='warning'>WARNING</span>: several iterations of the KF fit.", anchor = "notes-on-the-kalman-filter")),
                                                                                          value = F),
                                                                            fluidRow(
                                                                              column(4,
                                                                                     textInput(inputId = "ObsError",
                                                                                               div("Measurement noise",
                                                                                                   helpPopup("Enter the measurement standard deviation in the same units as the series.<br>
                                                                                                             If left empty, an approximate value will be used.", anchor = "notes-on-the-kalman-filter")),
                                                                                               value = "")
                                                                              ),
                                                                              conditionalPanel(
                                                                                condition = "input.errorm == true",
                                                                                column(4, align = "left",
                                                                                       textInput(inputId = "min_optirange",
                                                                                                 div("Min bound",
                                                                                                     helpPopup("Lower & upper bounds of the measurement standard deviation in the same units as the series.", anchor = "notes-on-the-kalman-filter")),
                                                                                                 value = "")
                                                                                ),
                                                                                column(4,
                                                                                       textInput(inputId = "max_optirange", label = "Max bound", value = "")
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
                                                                        
                                                                        ## % Linear fit ####
                                                                        conditionalPanel(
                                                                          condition = "input.model.indexOf('Linear') != -1",
                                                                          div(style = "padding: 0px 0px; margin-top:0em",
                                                                              tags$hr(style = "border-color: #333333; border-top: 1px solid #333333;")
                                                                          ),
                                                                          fluidRow(
                                                                            column(6,
                                                                                   conditionalPanel(
                                                                                     condition = "input.fitType == 1",
                                                                                     textInput(inputId = "trendRef",
                                                                                               div("Ref. epoch rate",
                                                                                                   helpPopup("Enter the reference epoch for the rate. If empty, the mean data epoch will be used.", anchor = NULL)),
                                                                                               value = "")
                                                                                   )
                                                                            ),
                                                                            conditionalPanel(
                                                                              condition = "input.fitType == 2",
                                                                              column(6,
                                                                                     textInput(inputId = "TrendDev",
                                                                                               div("Rate process noise",
                                                                                                   helpPopup("Enter the rate variation (standard deviation) for each observation.<br>
                                                                                                             If a null value is used, a constant linear trend will be estimated.", anchor = "notes-on-the-kalman-filter")),
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
                                                                                                     helpPopup("Enter the initial state value for the intercept. If empty, an approximate value will be used.", anchor = "notes-on-the-kalman-filter")),
                                                                                                 value = "")
                                                                                     )
                                                                              ),
                                                                              column(6,
                                                                                     textInput(inputId = "eIntercept0",
                                                                                               div("A priori intercept error",
                                                                                                   helpPopup("Enter the initial state uncertainty (standard deviation) for the intercept.<br>
                                                                                                             If empty, an approximate value will be used.", anchor = "notes-on-the-kalman-filter")),
                                                                                               value = "")
                                                                              )
                                                                            ),
                                                                            fluidRow(
                                                                              column(6,
                                                                                     conditionalPanel(
                                                                                       condition = "input.fitType == 2",
                                                                                       textInput(inputId = "Trend0",
                                                                                                 div("A priori rate",
                                                                                                     helpPopup("Enter the initial state value for the rate. If empty, an approximate value will be used.", anchor = "notes-on-the-kalman-filter")),
                                                                                                 value = "")
                                                                                     )
                                                                              ),
                                                                              column(6,
                                                                                     textInput(inputId = "eTrend0",
                                                                                               div("A priori rate error",
                                                                                                   helpPopup("Enter the initial state uncertainty (standard deviation) for the rate.<br>
                                                                                                             If empty, an approximate value will be used.", anchor = "notes-on-the-kalman-filter")),
                                                                                               value = "")
                                                                              )
                                                                            )
                                                                          )
                                                                        ),
                                                                        
                                                                        ## % Sinusoidal fit ####
                                                                        conditionalPanel(
                                                                          condition = "input.model.indexOf('Sinusoidal') != -1",
                                                                          div(style = "padding: 0px 0px; margin-top:0em",
                                                                              tags$hr(style = "border-color: #333333; border-top: 1px solid #333333;")
                                                                          ),
                                                                          textInput(inputId = "period",
                                                                                    div("Sinusoidal periods",
                                                                                        helpPopup("Enter a comma-separated list of periods. Each period ended by<br/>
                                                                                                  d : for days<br/>
                                                                                                  w : for weeks<br/>
                                                                                                  y : for years.<br/>
                                                                                                  Add xN at the end to fit up to N higher harmonics, i.e., 1yx2 includes annual and semi-annual periods.", anchor = "notes-on-the-sinusoidal-fitting")),
                                                                                    value = "1y"),
                                                                          fluidRow(
                                                                            column(6,
                                                                                   textInput(inputId = "periodRef",
                                                                                             div("Ref. epoch periods",
                                                                                                 helpPopup("Enter the reference epoch for the phase of the periods. If empty, the mean data epoch will be used", anchor = "notes-on-the-sinusoidal-fitting")),
                                                                                             value = "")
                                                                            ),
                                                                            conditionalPanel(
                                                                              condition = "input.fitType == 2",
                                                                              column(6,
                                                                                     textInput(inputId = "S0",
                                                                                               div("A priori amplitude",
                                                                                                   helpPopup("Enter the initial state value for both sine & cosine amplitudes. If empty, an approximate value will be used.", anchor = "notes-on-the-kalman-filter")),
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
                                                                                                   helpPopup("Enter the sine/cosine amplitude variation (standard deviation) for each observation and for each period.<br>
                                                                                                             If a null value is used, a constant sinusoidal oscillation will be estimated.", anchor = "notes-on-the-kalman-filter")),
                                                                                               value = "0.0")
                                                                              ),
                                                                              column(6,
                                                                                     textInput(inputId = "eS0",
                                                                                               div("A priori amplitude error",
                                                                                                   helpPopup("Enter the initial state uncertainty (standard deviation) for sine & cosine amplitudes.<br>
                                                                                                             If empty, an approximate value will be used.", anchor = "notes-on-the-kalman-filter")),
                                                                                               value = "")
                                                                              )
                                                                            ),
                                                                            tags$div(id = "inline",
                                                                                     radioButtons(inputId = "SineCosine",
                                                                                                  div("Amplitude process noise on",
                                                                                                      helpPopup("This option allows choosing between varying the sine amplitude only or varying both the sine & cosine amplitudes independently.", anchor = "notes-on-the-kalman-filter")),
                                                                                                  choices = list("Sine" = 1, "Sine & Cosine" = 2), selected = 2, inline = T)
                                                                            )
                                                                          )
                                                                        ),
                                                                        
                                                                        ## % Offset fit ####
                                                                        conditionalPanel(
                                                                          condition = "input.model.indexOf('Offset') != -1",
                                                                          div(style = "padding: 0px 0px; margin-top:0em",
                                                                              tags$hr(style = "border-color: #333333; border-top: 1px solid #333333;")
                                                                          ),
                                                                          textInput(inputId = "offsetEpoch",
                                                                                    div("Offset epochs",
                                                                                        helpPopup("Enter a comma-separated list of offsets in the same time units as the series.", anchor = NULL)),
                                                                                    value = ""),
                                                                          conditionalPanel(
                                                                            condition = "input.fitType == 2",
                                                                            fluidRow(
                                                                              column(6,
                                                                                     textInput(inputId = "O0",
                                                                                               div("A priori offset",
                                                                                                   helpPopup("Enter the initial state value for the offsets. If empty, an approximate value will be used.", anchor = "notes-on-the-kalman-filter")),
                                                                                               value = "")
                                                                              ),
                                                                              column(6,
                                                                                     textInput(inputId = "eO0",
                                                                                               div("A priori offset error",
                                                                                                   helpPopup("Enter the initial state uncertainty (standard deviation) for the offsets. If empty, an approximate value will be used.", anchor = "notes-on-the-kalman-filter")),
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
                                                                                                   helpPopup("This option sets the minimum segment size given as % of the series length.", anchor = "notes-on-the-discontinuity-detection")),
                                                                                               min = 0.1, max = 50, value = 10, step = 1, round = 0, ticks = F, animate = F, width = NULL, sep = "", pre = NULL, post = NULL, timeFormat = NULL, timezone = NULL, dragRange = TRUE)
                                                                            )
                                                                          ),
                                                                          htmlOutput("offsetFound"),
                                                                          fluidRow(
                                                                            column(4,
                                                                                   checkboxInput(inputId = "verif_offsets",
                                                                                                 div("Check offsets",
                                                                                                     helpPopup("This option estimates the probability that the estimated offsets are not generated by random noise variations.<br/>
                                                                                                               If the probability is < 95 %, offsets may be generated by random noise variations.", anchor = "notes-on-the-offset-verification"),
                                                                                                     value = F))
                                                                            ),
                                                                            conditionalPanel(
                                                                              condition = "input.verif_offsets == true",
                                                                              column(4, style = "margin-top:1em;", align = "left",
                                                                                     radioButtons(inputId = "typeColor", NULL, choices = list("FL/RW" = 1, "PL" = 2), selected = 2, inline = F, width = "auto")
                                                                              ),
                                                                              column(width = 4, style = "margin-top:1em; padding: 0px 0px 0px 0px", align = "left",
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
                                                                                                   helpPopup("Enter the standard deviation of the expected white noise in the series.", anchor = "notes-on-the-offset-verification")),
                                                                                               value = "")
                                                                              ),
                                                                              conditionalPanel(
                                                                                condition = "input.typeColor == 2",
                                                                                column(4,
                                                                                       textInput(inputId = "verif_pl",
                                                                                                 div("Power-law",
                                                                                                     helpPopup("Enter the stardard deviation of the expected power-law noise in the series.", anchor = "notes-on-the-offset-verification")),
                                                                                                 value = "")
                                                                                ),
                                                                                column(4,
                                                                                       textInput(inputId = "verif_k",
                                                                                                 div("Spectral index",
                                                                                                     helpPopup("Enter the spectral index of the expected power-law noise in the series.", anchor = "notes-on-the-offset-verification")),
                                                                                                 value = "")
                                                                                )
                                                                              ),
                                                                              conditionalPanel(
                                                                                condition = "input.typeColor == 1",
                                                                                column(4,
                                                                                       textInput(inputId = "verif_fl",
                                                                                                 div("Flicker noise",
                                                                                                     helpPopup("Enter the stardard deviation of the expected flicker noise in the series.", anchor = "notes-on-the-offset-verification")),
                                                                                                 value = "")
                                                                                ),
                                                                                column(4,
                                                                                       textInput(inputId = "verif_rw",
                                                                                                 div("Random Walk",
                                                                                                     helpPopup("Enter the stardard deviation of the expected random walk noise in the series.", anchor = "notes-on-the-offset-verification")),
                                                                                                 value = "")
                                                                                )
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
                                                                        
                                                                        ## % Exponential fit ####
                                                                        conditionalPanel(
                                                                          condition = "input.model.indexOf('Exponential') != -1",
                                                                          div(style = "padding: 0px 0px; margin-top:0em",
                                                                              tags$hr(style = "border-color: #333333; border-top: 1px solid #333333;")
                                                                          ),
                                                                          textInput(inputId = "ExponenRef",
                                                                                    div("Ref. time exponential",
                                                                                        helpPopup("Enter a comma-separated lisf of the starting epoch of the exponential decays.", anchor = "notes-on-the-exponential/logarithmic-decay-fitting")),
                                                                                    value = ""),
                                                                          fluidRow(
                                                                            column(6,
                                                                                   textInput(inputId = "E0",
                                                                                             div("A priori constant",
                                                                                                 helpPopup("Enter the initial value for the asymptotic offset for each decay.<br>
                                                                                                           If empty, an approximate value will be used.", anchor = "notes-on-the-exponential/logarithmic-decay-fitting")),
                                                                                             value = "")
                                                                            ),
                                                                            column(6,
                                                                                   textInput(inputId = "TE0",
                                                                                             div("A priori decay rate",
                                                                                                 helpPopup("Enter the initial value for each exponential decay rate. If empty, an approximate value will be used.", anchor = "notes-on-the-exponential/logarithmic-decay-fitting")),
                                                                                             value = "")
                                                                            )
                                                                          ),
                                                                          conditionalPanel(
                                                                            condition = "input.fitType == 2",
                                                                            fluidRow(
                                                                              column(6,
                                                                                     textInput(inputId = "eE0",
                                                                                               div("A priori constant error",
                                                                                                   helpPopup("Enter the initial state uncertainty (standard deviation) for the asymptotic offsets.<br>
                                                                                                             If empty, an approximate value will be used.", anchor = "notes-on-the-exponential/logarithmic-decay-fitting")),
                                                                                               value = "")
                                                                              ),
                                                                              column(6,
                                                                                     textInput(inputId = "eTE0",
                                                                                               div("A priori decay rate error",
                                                                                                   helpPopup("Enter the initial state uncertainty (standard deviation) for the exponential decay rates.<br>
                                                                                                             If empty, an approximate value will be used.", anchor = "notes-on-the-exponential/logarithmic-decay-fitting")),
                                                                                               value = "")
                                                                              )
                                                                            )
                                                                          )
                                                                        ),
                                                                        
                                                                        ## % Logarithmic fit ####
                                                                        conditionalPanel(
                                                                          condition = "input.model.indexOf('Logarithmic') != -1",
                                                                          div(style = "padding: 0px 0px; margin-top:0em",
                                                                              tags$hr(style = "border-color: #333333; border-top: 1px solid #333333;")
                                                                          ),
                                                                          textInput(inputId = "LogariRef",
                                                                                    div("Ref. time logarithmic",
                                                                                        helpPopup("Enter a comma-separated lisf of the starting epoch for each logarithmic decay.", anchor = "notes-on-the-exponential/logarithmic-decay-fitting")),
                                                                                    value = ""),
                                                                          fluidRow(
                                                                            column(6,
                                                                                   textInput(inputId = "L0",
                                                                                             div("A priori constant",
                                                                                                 helpPopup("Enter the initial value for the asymptotic offset of each decay.<br>
                                                                                                           If empty, an approximate value will be used.", anchor = "notes-on-the-exponential/logarithmic-decay-fitting")),
                                                                                             value = "")
                                                                            ),
                                                                            column(6,
                                                                                   textInput(inputId = "TL0",
                                                                                             div("A priori decay rate",
                                                                                                 helpPopup("Enter the initial value for the logarithmic decay rates. If empty, an approximate value will be used.", anchor = "notes-on-the-exponential/logarithmic-decay-fitting")),
                                                                                             value = "")
                                                                            )
                                                                          ),
                                                                          conditionalPanel(
                                                                            condition = "input.fitType == 2",
                                                                            fluidRow(
                                                                              column(6,
                                                                                     textInput(inputId = "eL0",
                                                                                               div("A priori constant error",
                                                                                                   helpPopup("Enter the initial state uncertainty (standard deviation) for the asymptotic offsets.<br>
                                                                                                             If empty, an approximate value will be used.", anchor = "notes-on-the-exponential/logarithmic-decay-fitting")),
                                                                                               value = "")
                                                                              ),
                                                                              column(6,
                                                                                     textInput(inputId = "eTL0",
                                                                                               div("A priori decay rate error",
                                                                                                   helpPopup("Enter the initial state uncertainty (standard deviation) for the logarithmic decay rates.<br>
                                                                                                             If empty, an approximate value will be used.", anchor = "notes-on-the-exponential/logarithmic-decay-fitting")),
                                                                                               value = "")
                                                                              )
                                                                            )
                                                                          )
                                                                        ),
                                                                        
                                                                        ## % Polynomial fit ####
                                                                        conditionalPanel(
                                                                          condition = "input.model.indexOf('Polynomial') != -1",
                                                                          div(style = "padding: 0px 0px; margin-top:0em",
                                                                              tags$hr(style = "border-color: #333333; border-top: 1px solid #333333;")
                                                                          ),
                                                                          fluidRow(
                                                                            column(6,
                                                                                   textInput(inputId = "PolyRef",
                                                                                             div("Ref. epoch polynomial",
                                                                                                 helpPopup("Enter the reference epoch for the polynomial. If empty, the rate reference epoch or the mean data epoch will be used.", anchor = NULL)),
                                                                                             value = "")
                                                                            ),
                                                                            column(6,
                                                                                   textInput(inputId = "PolyCoef",
                                                                                             div("Polynomial degree",
                                                                                                 helpPopup("Enter the polynomial degree between 2 and 20.<br>
                                                                                                           The degrees 0 (intercept) and 1 (rate) are estimated with the linear component.", anchor = NULL)),
                                                                                             value = "")
                                                                            )
                                                                          ),
                                                                          conditionalPanel(
                                                                            condition = "input.fitType == 2",
                                                                            fluidRow(
                                                                              column(6,
                                                                                     textInput(inputId = "P0",
                                                                                               div("A priori polynomial",
                                                                                                   helpPopup("Enter the initial state value for each polynomial coefficient. If empty, an approximate value will be used.", anchor = NULL)),
                                                                                               value = "")
                                                                              ),
                                                                              column(6,
                                                                                     textInput(inputId = "eP0",
                                                                                               div("A priori polynomial error",
                                                                                                   helpPopup("Enter the initial state uncertainty (standard deviation) for each polynomial coefficient.<br>
                                                                                                             If empty, an approximate value will be used.", anchor = NULL)),
                                                                                               value = "")
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      ),
                                                                      style = "primary"),
                                                      
                                                      # * Additional fit ####
                                                      bsCollapsePanel(value = 5,
                                                                      tags$h4(style = "color:white;", icon("magnifying-glass-plus", class = "headerIcon", lib = "font-awesome"), div(style = "color: white; display: inline; text-decoration-line: inherit;", "Additional fit"),
                                                                              div(style = "float: right; margin-right: 10px;", 
                                                                                  helpPopupHeader("This block allows for additional time series fitting and analysis, including:<br><br>
                                                                                                  Two linear trend estimators using the <span class='UIoption'>MIDAS</span> and the <span class='UIoption'>minimum entropy</span> methods.<br><br>
                                                                                                  The <span class='UIoption'>histogram</span> of the original, model, residual or smoothed series, and a stationarity assessment.<br><br>
                                                                                                  The non-parametric <span class='UIoption'>periodic waveform</span> of any non-sinusoidal periodic variation.<br><br>
                                                                                                  The amplitude or power <span class='UIoption'>periodogram</span> of the original data, the fitted model, the model residuals, the smoothed values, or the smoother residuals.<br><br>
                                                                                                  The pseudo discrete <span class='UIoption'>wavelet</span> transform of the original series, the fitted model, the model residuals, the smoothed values, or the smoother residuals.<br><span class='warning'>WARNING</span>: long computation time.<br><br>
                                                                                                  The Vondr&#225;k <span class='UIoption'>band-pass smoother</span> of the original or residual series.<br><br>
                                                                                                  The MLE <span class='UIoption'>noise analysis</span> to estimate the temporal correlation of the model/filter residuals.<br><span class='warning'>WARNING</span>: long computation time.<br><br>
                                                                                                  See more details in the <span class='help'>help</span> tab."))
                                                                      ),
                                                                      
                                                                      ## % MIDAS ####
                                                                      checkboxInput(inputId = "midas",
                                                                                    div("MIDAS",
                                                                                        helpPopup("This option estimates the linear trend value with the Median Interannual Difference Adjusted for Skewness algorithm.", anchor = "notes-on-the-midas-trend-estimates")),
                                                                                    value = F),
                                                                      
                                                                      ## % Entropy ####
                                                                      checkboxInput(inputId = "entropy",
                                                                                    div("Minimum entropy",
                                                                                        helpPopup("This option estimates the linear rate value with the differential minimum Shannon entropy algorithm.", anchor = "notes-on-the-minimum-entropy")),
                                                                                    value = F),
                                                                      conditionalPanel(
                                                                        condition = "input.entropy == true",
                                                                        textInput(inputId = "offsetEpoch.entropy",
                                                                                  div("Offset epochs (entropy)",
                                                                                      helpPopup("Enter a comma-separated list of offset epochs.", anchor = "notes-on-the-minimum-entropy")),
                                                                                  value = "")
                                                                      ),
                                                                      
                                                                      ## % Histogram ####
                                                                      fluidRow(
                                                                        column(12,
                                                                               checkboxInput(inputId = "histogram", label = "Histogram", value = F)
                                                                        )
                                                                      ),
                                                                      conditionalPanel(
                                                                        condition = "input.histogram == true",
                                                                        radioButtons(inputId = "histogramType", label = NULL, choices = list("None" = 0, "Original" = 1, "Model" = 2, "Model res." = 3, "Filter" = 4, "Filter res." = 5), selected = 0, inline = T, width = NULL, choiceNames = NULL,  choiceValues = NULL),
                                                                        div(style = "padding: 0px 0px; margin-top:0em",
                                                                            tags$hr(style = "border-color: #333333; border-top: 1px solid #333333;")
                                                                        )
                                                                      ),
                                                                      
                                                                      ## % Waveform ####
                                                                      fluidRow(
                                                                        column(6,
                                                                               checkboxInput(inputId = "waveform",
                                                                                             div("Periodic waveform",
                                                                                                 helpPopup("This option estimates a periodic waveform that does not have a sinusoidal shape.", anchor = "notes-on-the-waveform")),
                                                                                             value = F)
                                                                        ),
                                                                        column(6,
                                                                               conditionalPanel(
                                                                                 condition = "input.waveform == true",
                                                                                 textInput(inputId = "waveformPeriod",
                                                                                           div("Period",
                                                                                               helpPopup("Enter the waveform period in the same units as the series.<br>
                                                                                                         The waveform period must be larger than twice the average sampling period and smaller than half the total period of the series.", anchor = "notes-on-the-waveform")),
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
                                                                                                       helpPopup("This option removes the estimated periodic waveform from the original series before the model fit.", anchor = "notes-on-the-waveform")),
                                                                                                   value = F)
                                                                                 )
                                                                          )
                                                                        ),
                                                                        div(style = "padding: 0px 0px; margin-top:-1em",
                                                                            tags$hr(style = "border-color: #333333; border-top: 1px solid #333333;")
                                                                        )
                                                                      ),
                                                                      
                                                                      ## % Periodogram ####
                                                                      fluidRow(
                                                                        column(5,
                                                                               checkboxInput(inputId = "spectrum",
                                                                                             div("Periodogram",
                                                                                                 helpPopup("This option estimates the Lomb-Scargle periodogram.", anchor = "notes-on-the-periodogram")),
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
                                                                                 checkboxInput(inputId = "spectrumResiduals", label = "Model res.", value = F)
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
                                                                            tags$hr(style = "border-color: #333333; border-top: 1px solid #333333;")
                                                                        )
                                                                      ),
                                                                      
                                                                      ## % Wavelet ####
                                                                      conditionalPanel(
                                                                        condition = "output.wavelet == true",
                                                                        div(style = "padding: 0px 0px; margin-top:0em",
                                                                            fluidRow(
                                                                              column(4,
                                                                                     checkboxInput(inputId = "wavelet",
                                                                                                   div("Wavelets",
                                                                                                       helpPopup("This option estimates the wavelet transform.", anchor = "notes-on-the-wavelet-transform")),
                                                                                                   value = F)
                                                                              ),
                                                                              column(4, offset = 4,
                                                                                     conditionalPanel(
                                                                                       condition = "input.wavelet == true",
                                                                                       textInput(inputId = "loc_wavelet",
                                                                                                 div("Sampling",
                                                                                                     helpPopup("Enter the temporal resolution or time separation between wavelets.<br>
                                                                                                               The maximum valid value is half the total observed period.<br>
                                                                                                               The minimum valid value is the sampling period of the series.", anchor = "notes-on-the-wavelet-transform")),
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
                                                                                                     helpPopup("Enter the shortest period to compute the transform.<br>The minimum valid value is twice the median sampling period.", anchor = "notes-on-the-wavelet-transform")),
                                                                                                 value = "")
                                                                                ),
                                                                                column(4,
                                                                                       textInput(inputId = "max_wavelet",
                                                                                                 div("Max.",
                                                                                                     helpPopup("Enter the longest period to compute the transform.<br>The maximum valid value is half the total observed period.", anchor = "notes-on-the-wavelet-transform")),
                                                                                                 value = "")
                                                                                ),
                                                                                column(4,
                                                                                       textInput(inputId = "res_wavelet",
                                                                                                 div("Step",
                                                                                                     helpPopup("Enter the time resolution or separation between the periods to compute the transform.", anchor = "notes-on-the-wavelet-transform")),
                                                                                                 value = "")
                                                                                )
                                                                              )
                                                                            )
                                                                        )
                                                                      ),
                                                                      conditionalPanel(
                                                                        condition = "input.wavelet == true",
                                                                        radioButtons(inputId = "waveletType", label = NULL, choices = list("None" = 0, "Original" = 1, "Model" = 2, "Model res." = 3, "Filter" = 4, "Filter res." = 5), selected = 0, inline = T, width = NULL, choiceNames = NULL,  choiceValues = NULL),
                                                                        div(style = "padding: 0px 0px; margin-top:-0.5em",
                                                                            tags$hr(style = "border-color: #333333; border-top: 1px solid #333333;")
                                                                        )
                                                                      ),
                                                                      
                                                                      ## % Vondrak ####
                                                                      div(style = "padding: 0px 0px; margin-top:0em",
                                                                          fluidRow(
                                                                            column(6,
                                                                                   checkboxInput(inputId = "filter",
                                                                                                 div("Band-pass smoother",
                                                                                                     helpPopup("This option computes the Vondrak smoother for the original or residual series.", anchor = "notes-on-the-vondrak-smoother")),
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
                                                                                                   helpPopup("Enter the low-pass period. The maximum recommended value is 1/4 of the series length.", anchor = "notes-on-the-vondrak-smoother")),
                                                                                               value = "")
                                                                              ),
                                                                              column(6,
                                                                                     textInput(inputId = "high",
                                                                                               div("High-pass period cutoff",
                                                                                                   helpPopup("Enter the high-pass period. The maximum recommended value is 1/4 of the series length.", anchor = "notes-on-the-vondrak-smoother")),
                                                                                               value = "")
                                                                              )
                                                                            ),
                                                                            div(style = "padding: 0px 0px; margin-top:-0.5em",
                                                                                tags$hr(style = "border-color: #333333; border-top: 1px solid #333333;")
                                                                            )
                                                                          )
                                                                      ),
                                                                      
                                                                      ## % Noise ####
                                                                      div(style = "padding: 0px 0px; margin-top:0em",
                                                                          fluidRow(
                                                                            column(4,
                                                                                   checkboxInput(inputId = "mle",
                                                                                                 div("Noise analysis",
                                                                                                     helpPopup("This option estimates the parameters of a covariance model of the residual series.", anchor = "notes-on-the-noise-analysis")),
                                                                                                 value = F)
                                                                            ),
                                                                            conditionalPanel(
                                                                              condition = "input.mle == true",
                                                                              column(width = 4, offset = 4, style = "margin-top:0em; padding: 0px 0px 0px 0px", align = "left",
                                                                                     conditionalPanel(
                                                                                       condition = "input.mle == true",
                                                                                       withBusyIndicatorUI(
                                                                                         actionButton(inputId = "runmle", label = "Run MLE", icon = icon("hourglass", class = NULL, lib = "font-awesome"), style = "font-size: small")
                                                                                       )
                                                                                     )
                                                                              )
                                                                            )
                                                                          ),
                                                                          conditionalPanel(
                                                                            condition = "input.mle == true",
                                                                            fluidRow(
                                                                              column(6,
                                                                                     checkboxInput(inputId = "noise_unc",
                                                                                                   div("Noise uncertainty",
                                                                                                       helpPopup("This option enables or disables the estimation of the formal uncertainties of the parameters of the estimated noise model.", anchor = "notes-on-the-noise-analysis")),
                                                                                                   value = T)
                                                                              ),
                                                                              column(6,
                                                                                     checkboxInput(inputId = "wiener",
                                                                                                   div("Noise separation",
                                                                                                       helpPopup("This option separates the residual series into the different estimated noise components.", anchor = "notes-on-the-noise-analysis")),
                                                                                                   value = F)
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
                                                                            htmlOutput("est.unc"),
                                                                            htmlOutput("crossover")
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
                                                                                                 helpPopup("Enter the full path of the download directory.", anchor = NULL)),
                                                                                             value = "")
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
                                    
                                    # Tab numbers in order from left to right:
                                    # 0: SARI logo
                                    # 6: help 
                                    # 4: 3D series
                                    # 1: 1st component
                                    # 2: 2nd component
                                    # 3: 3rd component
                                    # 5: residual series
                                    # 7: save
                                    # 8: PDF
                                    
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
                                        tabPanel(div(style = "display: inline-block; font-size: 40px; color: #333333", "SARI"), value = 0, id = "SARI"),
                                        tabPanel(div(style = "margin-top:-3.5em; font-size: 25px; display: inline-block;","Help"), value = 6, icon = icon("circle-info", class = "fas fa-2x"),
                                                 uiOutput("about_file")
                                        ),
                                        
                                        # * 3D series ####
                                        tab3Contents("3D"),
                                        
                                        # * component 1 ####
                                        tabContents(1),
                                        
                                        # * component 2 ####
                                        tabContents(2),
                                        
                                        # * component 3 ####
                                        tabContents(3),
                                        
                                        # * residual series ####
                                        tab3Contents("residuals"),
                                        
                                        tabPanel(title = downloadLink('print_out', div(style = "margin-top:-3.5em; font-size: 25px; display: inline-block; font-family: sans-serif; font-weight: normal;","PDF"), class = "fa-solid fa-file", style = "font-size:30px; margin-top:-0.9em"), value = 8),
                                        tabPanel(title = downloadLink('download', div(style = "margin-top:-3.5em; font-size: 25px; display: inline-block; font-family: sans-serif; font-weight: normal;","Save"), class = "fa-solid fa-floppy-disk", style = "font-size:30px; margin-top:-0.9em"), value = 7)
                                      )
                                    )
                      )
                  )
                )
)



server <- function(input,output,session) {
  
  toggleClass( # disabling clicking on the SARI name
    class = "disabled",
    selector = "#tab li a[data-value=0]"
  )
  
  # starting logging the user' session
  mySession <- NULL
  cat(file = stderr(), "\n", "\n", "START", "\n")
  
  # Catching a refreshed page, but trying to exclude the loading of a new instance
  shinyjs::runjs("
    var pageAccessedByReload = 'false';
    var startTime = 0;
    pageAccessedByReload = window.performance.getEntriesByType('navigation')[0].type.includes('reload');
    startTime = window.performance.getEntriesByType('navigation')[0].responseStart;
    if (pageAccessedByReload === true && startTime > 2500) {
      pageAccessedByReload = 'false';
    }
    Shiny.onInputChange('refreshed', pageAccessedByReload);
  ")

  # Debugging pit stop (from https://www.r-bloggers.com/2019/02/a-little-trick-for-debugging-shiny/?msclkid=3fafd7f3bc9911ec9c1253a868203435)
  observeEvent(input$browser,{
    browser()
  })

  # Initialize reactive variables of the global database
  database <- c("file", "ranges", "info", "db1", "db2", "inputs", "trans", "url")

  # 1. input files.
  file <- reactiveValues(primary = NULL, secondary = NULL, id1 = NULL, id2 = NULL, sitelog = NULL, euler = NULL, soln = NULL, custom = NULL)

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
  info <- reactiveValues(points = NULL, removed = NULL, directory = NULL, log = NULL, log_years = NULL, sinfo = NULL, sinfo_years = NULL, soln = NULL, soln_years = NULL, custom = NULL, custom_years = NULL,
                         custom_warn = 0, tab = NULL, stop = NULL, noise = NULL, menu = c(1,2),
                         decimalsx = NULL, decimalsy = NULL, scientific = F, nsmall = NULL, digits = NULL,
                         sampling = NULL, sampling0 = NULL, sampling_regular = NULL, rangex = NULL, errorbars = T,
                         step = NULL, step2 = NULL, stepUnit = NULL,
                         minx = NULL, maxx = NULL, miny = NULL, maxy = NULL, width = isolate(session$clientData$output_plot1_width),
                         run = F, tunits.label = NULL, tunits.known1 = F, tunits.known2 = F, tunits.last = NULL, run_wavelet = T, pixelratio = NULL, welcome = F,
                         last_optionSecondary = NULL, format = NULL, format2 = NULL, intro = T, KFiter = NULL, tol = NULL,
                         white = NULL, flicker = NULL, randomw = NULL, powerl = NULL, timeMLE = NULL, components = NULL, local = F,
                         product1 = NULL,
                         db1 = "stop", db2 = "stop",
                         trendRef = F, PolyRef = F, periodRef = F, noLS = F,
                         plateFile = NULL,
                         overview = F)
  
  # 4. database:
  #   1 = original
  #   2 = resampled
  #   3 = corrected (series are first resampled and then corrected)
  db1 <- reactiveValues(original = NULL)
  db2 <- reactiveValues(original = NULL)

  # 5. user input
  inputs <- reactiveValues(thresholdRes = NULL, thresholdResN = NULL, trendRef = NULL, period = NULL,
                           periodRef = NULL, offsetEpoch = NULL, ExponenRef = NULL, E0 = NULL, TE0 = NULL,
                           LogariRef = NULL, L0 = NULL, TL0 = NULL, PolyRef = NULL, PolyCoef = NULL, ofac = "",
                           long_period = "", short_period = "", low = NULL, high = NULL, scaleFactor = 1,
                           step = NULL, step2 = NULL,
                           giaTrend = NULL, giaTrend2 = NULL,
                           plot4_1click = NULL, plot5_1click = NULL)
  obs <- reactiveVal()

  # 6. computed values
  trans <- reactiveValues(x0 = NULL, y0 = NULL, sy0 = NULL, x = NULL, y = NULL, sy = NULL, xe = NULL, ye = NULL,
                          sye = NULL, y2 = NULL, sy2 = NULL, 
                          res = NULL, res1x = NULL, res1y = NULL, res1sy = NULL, res2x = NULL, res2y = NULL, res2sy = NULL, res3x = NULL, res3y = NULL, res3sy = NULL,
                          mod = NULL, mod1y = NULL, mod2y = NULL, mod3y = NULL,
                          results = NULL, filter = NULL,
                          filterRes = NULL, kalman = NULL, equation = NULL, ordinate = NULL, midas_vel = NULL,
                          midas_sig = NULL, midas_all = NULL, midas_vel2 = NULL, midas_sig2 = NULL,
                          mle = NULL, verif = NULL, pattern = NULL, unc = NULL, vondrak = NULL, wave = NULL,
                          noise = NULL, fs = NULL, names = NULL, KFnames = NULL, LScoefs = NULL, fs = NULL, amp = NULL, psd = NULL,
                          col = NULL, spectra = NULL, spectra_old = NULL, title = NULL, var = NULL, wavelet = NULL,
                          model_old = NULL, offsetEpochs = NULL, periods = NULL,
                          x_orig = NULL, gaps = NULL,
                          plate = NULL, plate2 = NULL, gia = NULL, gia2 = NULL,
                          entropy_vel = NULL, entropy_sig = NULL, offsetEpoch.entropy = NULL,
                          slope = NULL,
                          white = NULL, flicker = NULL, randomw = NULL, powerl = NULL, white_sig = NULL, flicker_sig = NULL, randomw_sig = NULL, powerl_sig = NULL)

  # 7. output
  OutPut <- reactiveValues(df = NULL)
  output_excluded <- reactiveValues(df = NULL)

  # 8. input parameters via URL
  url <- reactiveValues(station = NULL, server = NULL, file = NULL, station2 = NULL, server2 = NULL, file2 = NULL, logfile = NULL, logfile2 = NULL)

  # Resetting all parameters to default values each time the page loads (avoids problems when clicking back on the browser)
  reset("side-panel")
  reset("main-panel")
  
  # Constants ####
  SARIcolors <- c("black", "#DF536B", "#61D04F", "#2297E6", "#28E2E5", "#CD0BBC", "#F5C710", "gray62") # colorblind palette copied from the palette R4 for R versions < 4
  daysInYear <- 365.2425 # Gregorian year
  degMa2radyr <- pi/180000000 # geologic to geodetic units conversion
  debug <- F # saving the environment
  messages <- 6 # print step by step messages on the console depending on the verbosity level (0, 1, 2, 3, 4, 5, 6)
  info$components <- c("", "", "", "", "") # labels of the tab components at start up
  output$tabName1 <- renderText({ "Visualization panel" })
  output$tabName2 <- renderText({ info$components[2] })
  output$tabName3 <- renderText({ info$components[3] })
  output$tabName4 <- renderText({ info$components[4] })
  output$tabName5 <- renderText({ info$components[5] })

  # Welcome ####
  observe({
    inputChanged <- input$changed[lapply(input$changed, function(x) length(grep("clientdata|shinyjs-delay|shinyjs-resettable|undefined_", x, value = F))) == 0]
    if (length(inputChanged) > 0 && messages > 5) {
      cat(file = stderr(), mySession, paste("Latest input fired:", paste(input$changed, collapse = ", ")), "\n")
    }
    req(input$size, info$intro)
    info$local = Sys.getenv('SHINY_PORT') == "" || session$clientData$url_hostname == "127.0.0.1" # detect local connection
    if (length(input$isMobile) > 0 && input$isMobile) {
      cat(file = stderr(), mySession, "Mobile connection", "\n")
      cat(file = stderr(), mySession, "Screen size", input$size[1], "x", input$size[2], "\n")
      cat(file = stderr(), mySession, "Touchscreen", input$tactile, "\n")
      shinyjs::hide(id = "menu")
      shinyjs::hide(id = "localDir")
      shinyjs::hide(selector = "#tab li a[data-value=1]")
      shinyjs::hide(selector = "#tab li a[data-value=2]")
      shinyjs::hide(selector = "#tab li a[data-value=3]")
      shinyjs::hide(selector = "#tab li a[data-value=4]")
      shinyjs::hide(selector = "#tab li a[data-value=5]")
      shinyjs::hide(selector = "#tab li a[data-value=8]")
      updateNavbarPage(session, "tab", selected = "6")
      shinyjs::hide(selector = "#tab li a[data-value=6]")
      shinyjs::hideElement(id = "side-panel", anim = F)
      showModal(modalDialog(
        title = tags$h3("Dear SARI user"),
        HTML("It is strongly discouraged to use SARI on small-screen devices.<br>Please, consider using a desktop connection instead."),
        size = "m",
        easyClose = F,
        fade = F
      ))
      load_data(2)
    } else {
      if (info$local) { # local SARI session
        load_data(0)
        if (!is.null(dev.list())) dev.off()
        shinyjs::show("localDir")
      } else { # remote SARI session
        shinyjs::delay(100, {
          if (isTRUE(input$refreshed)) { # we do not like refreshed remote sessions
            if (messages > 0) cat(file = stderr(), mySession, "Page refreshed", "\n")
            showModal(modalDialog(
              title = tags$h3("Dear SARI user"),
              HTML("<div style='padding: 0px 50px'>The SARI webpage has been reloaded.<br><br>If this was due to an error, please consider giving feedback <a href='https://github.com/alvarosantamariagomez/sari' target='_blank'>here</a>.<br><br>Otherwise, if the session is still connected to the server, to start a new analysis, it is <span style='color: red; font-weight: bold;'>strongly recommended</span> to use the RESET button instead (left panel, plot controls section).</div>"),
              size = "m",
              easyClose = T,
              fade = T
            ))
          }
        })
        if (messages > 2) cat(file = stderr(), mySession, "Screen size", input$size[1], "x", input$size[2], "\n")
        if (messages > 2) cat(file = stderr(), mySession, "Pixel ratio", info$pixelratio, "\n")
        if (messages > 2) cat(file = stderr(), mySession, "Touchscreen", input$tactile, "\n")
        shinyjs::hide("localDir")
        # welcome message on screen (deprecated)
        if (isTRUE(info$welcome)) {
          showNotification("<<< It is strongly recommended to read the help content at least once to avoid mistakes and to make the most of this tool.", action = NULL, duration = 10, closeButton = T, id = "point_to_help", type = "message", session = getDefaultReactiveDomain())
          if (messages > 2) cat(file = stderr(), mySession, "Warning", "\n")
          if (isTruthy(input$tactile)) {
            if (input$tactile > 0) {
              if (messages > 2) cat(file = stderr(), mySession, "Touchscreen", input$tactile, "\n")
              showModal(modalDialog(
                title = tags$h3("Dear SARI user"),
                HTML("It is strongly discouraged to use the touchscreen with SARI.<br>Please, consider using the mouse instead."),
                size = "m",
                easyClose = T,
                fade = F
              ))
            }
          }
          info$welcome <- F
        }
        mySession <<- as.integer(runif(n = 1, min = 1, max = 999999)) # setting anonymous user' session id
        load_data(2)
      }
      # setting the IU options that are defined at the server side
      output$station1 <- renderUI({
        textInput(inputId = "station1", label = "Station", value = "")
      })
      output$station2 <- renderUI({
        textInput(inputId = "station2", label = "Station", value = "")
      })
      output$fileSeries1 <- renderUI({
        tags$a(href = "SPOTGINS_CRAL00FRA.enu", "Show file example", targe = "_blank")
      })
      output$fileSeries2 <- renderUI({
        NULL
      })
      output$sitelog <- renderUI({
        tags$a(href = "cral00fra_20231110.log", "Show file example", target = "_blank")
      })
      output$station.info <- renderUI({
        tags$a(href = "station.info", "Show file example", target = "_blank")
      })
      output$solnFile <- renderUI({
        tags$a(href = "soln.snx", "Show file example", target = "_blank")
      })
      output$customFile <- renderUI({
        tags$a(href = "steps.txt", "Show file example", target = "_blank")
      })
    }
    info$intro <- F
  }, priority = 2000)

  # UI output reactive flags ####
  output$print_out <- reactive({}) # bugfix to avoid hidden() breaking the downloadHandler()
  outputOptions(output, "print_out", suspendWhenHidden = F)
  output$download <- reactive({})
  outputOptions(output, "download", suspendWhenHidden = F)
  
  output$about_file <- renderUI({
    if (input$isMobile && length(input$isMobile) > 0) {
      file <- "www/about_mobile.md"
    } else {
      file <- "www/about.md"
    }
    withMathJax(includeMarkdown(file))
  })
  
  output$location <- reactive({
    return(exists("leaflet", mode = "function") && (isTruthy(input$station_lat) && isTruthy(input$station_lon)) || (isTruthy(input$station_lat2) && isTruthy(input$station_lon2)))
  })
  outputOptions(output, "location", suspendWhenHidden = F)
  
  output$wavelet <- reactive({
    return(exists("get.nscales", mode = "function"))
  })
  outputOptions(output, "wavelet", suspendWhenHidden = F)
  
  output$log <- reactive({
    return(!is.null(file$sitelog) || !is.null(file$primary$logfile) || !is.null(file$secondary$logfile))
  })
  outputOptions(output, "log", suspendWhenHidden = F)

  output$sinfo <- reactive({
    return(!is.null(input$sinfo))
  })
  outputOptions(output, "sinfo", suspendWhenHidden = F)

  output$soln <- reactive({
    return(!is.null(file$soln))
  })
  outputOptions(output, "soln", suspendWhenHidden = F)

  output$custom <- reactive({
    return(!is.null(file$custom))
  })
  outputOptions(output, "custom", suspendWhenHidden = F)

  output$series1 <- reactive({
    return(!is.null(file$primary))
  })
  outputOptions(output, "series1", suspendWhenHidden = F)

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

  output$residuals <- reactive({
    return(!is.null(trans$res))
  })
  outputOptions(output, "residuals", suspendWhenHidden = F)

  output$rate <- reactive({
    return("Linear" %in% input$model && length(trans$kalman) > 0 && trans$kalman_info$processNoise[2] > 0)
  })
  outputOptions(output, "rate", suspendWhenHidden = F)

  output$mobile <- reactive({
    return(length(input$isMobile) > 0 && input$isMobile)
  })
  outputOptions(output, "mobile", suspendWhenHidden = F)

  output$pmm <- renderUI({
    if (isTruthy(input$plateModel)) {
      if (input$plateModel == "ITRF2020") {
        model <- "ITRF2020-PMM.dat"
      } else if (input$plateModel == "NNR-MORVEL56") {
        model <- "NNR-MORVEL56.txt"
      } else if (input$plateModel == "NNR-GSRM") {
        model <- "NNR-GSRM_v2.1.txt"
      }
      link <- a("Show the selected plate model", href = model, target = "_blank")
      tagList(link)
    }
  })
  outputOptions(output, "pmm", suspendWhenHidden = F)

  # Series summary ####
  output$information1 <- output$information2 <- renderUI({
    req(db1[[info$db1]])
    if (input$tunits == 1) {
      units <- "days"
    } else if (input$tunits == 2) {
        units <- "weeks"
    } else if (input$tunits == 3) {
        units <- "years"
    }
    line1 <- sprintf("Number of points = %d",info$points)
    line2 <- sprintf("Number of points removed = %d",info$removed)
    line3 <- paste(sprintf("Series length = %.*f", info$decimalsx, info$rangex), units)
    line4 <- sprintf("Series range = %.*f - %.*f",info$decimalsx, trans$x[1],info$decimalsx,trans$x[length(trans$x)])
    line5 <- paste(sprintf("Series sampling = %.*f",info$decimalsx, info$sampling), units)
    line6 <- sprintf("Series completeness = %.1f %%",100*(info$points - 1)/(info$rangex/info$sampling))
    HTML(paste("", line1, line2, line3, line4, line5, line6, sep = "<br/>"))
  })

  # Debouncers & checks for user typed inputs ####
  reactive({
    inputs$ObsError <- suppressWarnings(as.numeric(trimws(input$ObsError, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(0, priority = 1000)

  reactive({
    inputs$thresholdRes <- suppressWarnings(as.numeric(trimws(input$thresholdRes, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(0, priority = 1000)

  reactive({
    inputs$thresholdResN <- suppressWarnings(as.numeric(trimws(input$thresholdResN, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(0, priority = 1000)

  trendRef_d <- reactive(input$trendRef) %>% debounce(1000, priority = 1000)
  observeEvent(trendRef_d(), {
    if (is.na(inputs$trendRef) || is.null(inputs$trendRef) || trendRef_d() != inputs$trendRef) {
      inputs$trendRef <- suppressWarnings(as.numeric(trimws(trendRef_d(), which = "both", whitespace = "[ \t\r\n]")))
    }
  }, priority = 1000)

  period_d <- reactive(input$period) %>% debounce(1000, priority = 1000)
  observeEvent(period_d(), {
    inputs$period <- trimws(period_d(), which = "both", whitespace = "[ \t\r\n]")
  }, priority = 1000)

  periodRef_d <- reactive(input$periodRef) %>% debounce(1000, priority = 1000)
  observeEvent(periodRef_d(), {
    if (is.na(inputs$periodRef) || is.null(inputs$periodRef) || periodRef_d() != inputs$periodRef) {
      inputs$periodRef <- suppressWarnings(as.numeric(trimws(periodRef_d(), which = "both", whitespace = "[ \t\r\n]")))
    }
  }, priority = 1000)

  offsetEpoch_d <- reactive(input$offsetEpoch) %>% debounce(1000, priority = 1000)
  observeEvent(offsetEpoch_d(), {
    inputs$offsetEpoch <- trimws(offsetEpoch_d(), which = "both", whitespace = "[ \t\r\n]")
  }, priority = 1000)
  
  offsetEpoch.entropy_d <- reactive(input$offsetEpoch.entropy) %>% debounce(1000, priority = 1000)
  observeEvent(offsetEpoch.entropy_d(), {
    inputs$offsetEpoch.entropy <- trimws(offsetEpoch.entropy_d(), which = "both", whitespace = "[ \t\r\n]")
  }, priority = 1000)

  ExponenRef_d <- reactive(input$ExponenRef) %>% debounce(1000, priority = 1000)
  observeEvent(ExponenRef_d(), {
    inputs$ExponenRef <- trimws(ExponenRef_d(), which = "both", whitespace = "[ \t\r\n]")
  }, priority = 1000)

  E0_d <- reactive(input$E0) %>% debounce(1000, priority = 1000)
  observeEvent(E0_d(), {
    if (!isTruthy(inputs$E0) || E0_d() != inputs$E0) {
      inputs$E0 <- trimws(E0_d(), which = "both", whitespace = "[ \t\r\n]")
    }
  }, priority = 1000)

  reactive({
    inputs$eE0 <- trimws(input$eE0, which = "both", whitespace = "[ \t\r\n]")
  }) %>% debounce(0, priority = 1000)

  TE0_d <- reactive(input$TE0) %>% debounce(1000, priority = 1000)
  observeEvent(TE0_d(), {
    if (!isTruthy(inputs$TE0) || TE0_d() != inputs$TE0) {
      inputs$TE0 <- trimws(TE0_d(), which = "both", whitespace = "[ \t\r\n]")
    }
  }, priority = 1000)

  reactive({
    inputs$eTE0 <- trimws(input$eTE0, which = "both", whitespace = "[ \t\r\n]")
  }) %>% debounce(0, priority = 1000)

  LogariRef_d <- reactive(input$LogariRef) %>% debounce(1000, priority = 1000)
  observeEvent(LogariRef_d(), {
    inputs$LogariRef <- trimws(LogariRef_d(), which = "both", whitespace = "[ \t\r\n]")
  }, priority = 1000)

  L0_d <- reactive(input$L0) %>% debounce(1000, priority = 1000)
  observeEvent(L0_d(), {
    if (!isTruthy(inputs$L0) || L0_d() != inputs$L0) {
      inputs$L0 <- trimws(L0_d(), which = "both", whitespace = "[ \t\r\n]")
    }
  }, priority = 1000)

  reactive({
    inputs$eL0 <- trimws(input$eL0, which = "both", whitespace = "[ \t\r\n]")
  }) %>% debounce(0, priority = 1000)

  TL0_d <- reactive(input$TL0) %>% debounce(1000, priority = 1000)
  observeEvent(TL0_d(), {
    if (!isTruthy(inputs$TL0) || TL0_d() != inputs$TL0) {
      inputs$TL0 <- trimws(TL0_d(), which = "both", whitespace = "[ \t\r\n]")
    }
  }, priority = 1000)

  reactive({
    inputs$eTL0 <- trimws(input$eTL0, which = "both", whitespace = "[ \t\r\n]")
  }) %>% debounce(0, priority = 1000)

  PolyRef_d <- reactive(input$PolyRef) %>% debounce(1000, priority = 1000)
  observeEvent(PolyRef_d(), {
    if (is.na(inputs$PolyRef) || is.null(inputs$PolyRef) || PolyRef_d() != inputs$PolyRef) {
      inputs$PolyRef <- suppressWarnings(as.numeric(trimws(PolyRef_d(), which = "both", whitespace = "[ \t\r\n]")))
    }
  }, priority = 1000)

  PolyCoef_d <- reactive(input$PolyCoef) %>% debounce(1000, priority = 1000)
  observeEvent(PolyCoef_d(), {
    inputs$PolyCoef <- suppressWarnings(as.numeric((trimws(PolyCoef_d(), which = "both", whitespace = "[ \t\r\n]"))))
  }, priority = 1000)

  ofac_d <- reactive(input$ofac) %>% debounce(1000, priority = 1000)
  observeEvent(ofac_d(), {
    if (is.na(inputs$ofac) || ofac_d() != inputs$ofac) {
      inputs$ofac <- suppressWarnings(as.numeric(trimws(ofac_d(), which = "both", whitespace = "[ \t\r\n]")))
    }
  }, priority = 1000)

  long_period_d <- reactive(input$long_period) %>% debounce(1000, priority = 1000)
  observeEvent(long_period_d(), {
    if (is.na(inputs$long_period) || long_period_d() != inputs$long_period) {
      inputs$long_period <- suppressWarnings(as.numeric(trimws(long_period_d(), which = "both", whitespace = "[ \t\r\n]")))
    }
  }, priority = 1000)

  short_period_d <- reactive(input$short_period) %>% debounce(1000, priority = 100)
  observeEvent(short_period_d(), {
    if (is.na(inputs$short_period) || short_period_d() != inputs$short_period) {
      inputs$short_period <- suppressWarnings(as.numeric(trimws(short_period_d(), which = "both", whitespace = "[ \t\r\n]")))
    }
  }, priority = 100)

  low_d <- reactive(input$low) %>% debounce(1000, priority = 1000)
  observeEvent(low_d(), {
    inputs$low <- suppressWarnings(as.numeric(trimws(low_d(), which = "both", whitespace = "[ \t\r\n]")))
  }, priority = 1000)

  high_d <- reactive(input$high) %>% debounce(1000, priority = 1000)
  observeEvent(high_d(), {
    inputs$high <- suppressWarnings(as.numeric(trimws(high_d(), which = "both", whitespace = "[ \t\r\n]")))
  }, priority = 1000)

  step_d <- reactive(input$step) %>% debounce(2000, priority = 1000)
  observeEvent(c(step_d()), {
    if (grepl("^=", trimws(step_d()), perl = T)) {
      step <- try(eval(parse(text = sub("=", "", trimws(step_d())))), silent = T)
      if (isTruthy(step) && !inherits(step,"try-error")) {
        updateTextInput(session, inputId = "step", value = step)
      }
      req(info$stop)
    } else {
      inputs$step <- suppressWarnings(as.numeric(trimws(step_d(), which = "both", whitespace = "[ \t\r\n]")))
    }
  }, priority = 1000)

  step2_d <- reactive(input$step2) %>% debounce(1000, priority = 1000)
  observeEvent(c(step2_d()), {
    if (grepl("^=", trimws(step2_d()), perl = T)) {
      step <- try(eval(parse(text = sub("=","",trimws(step2_d())))), silent = T)
      if (isTruthy(step) && !inherits(step,"try-error")) {
        updateTextInput(session, inputId = "step2", value = step)
      }
      req(info$stop)
    } else {
      inputs$step2 <- suppressWarnings(as.numeric(trimws(step2_d(), which = "both", whitespace = "[ \t\r\n]")))
    }
  }, priority = 1000)

  min_wavelet_d <- reactive(input$min_wavelet) %>% debounce(1000, priority = 1000)
  observeEvent(c(min_wavelet_d()), {
    inputs$min_wavelet <- suppressWarnings(as.numeric(trimws(min_wavelet_d(), which = "both", whitespace = "[ \t\r\n]")))
  }, priority = 1000)

  max_wavelet_d <- reactive(input$max_wavelet) %>% debounce(1000, priority = 1000)
  observeEvent(max_wavelet_d(), {
    inputs$max_wavelet <- suppressWarnings(as.numeric(trimws(max_wavelet_d(), which = "both", whitespace = "[ \t\r\n]")))
  }, priority = 1000)

  res_wavelet_d <- reactive(input$res_wavelet) %>% debounce(1000, priority = 1000)
  observeEvent(res_wavelet_d(), {
    inputs$res_wavelet <- suppressWarnings(as.numeric(trimws(res_wavelet_d(), which = "both", whitespace = "[ \t\r\n]")))
  }, priority = 1000)

  loc_wavelet_d <- reactive(input$loc_wavelet) %>% debounce(1000, priority = 1000)
  observeEvent(loc_wavelet_d(), {
    inputs$loc_wavelet <- suppressWarnings(as.numeric(trimws(loc_wavelet_d(), which = "both", whitespace = "[ \t\r\n]")))
  }, priority = 1000)

  reactive({
    inputs$verif_white <- suppressWarnings(as.numeric(trimws(input$verif_white, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(0, priority = 1000)

  reactive({
    inputs$verif_pl <- suppressWarnings(as.numeric(trimws(input$verif_pl, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(0, priority = 1000)

  reactive({
    inputs$verif_k <- suppressWarnings(as.numeric(trimws(input$verif_k, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(0, priority = 1000)
  
  reactive({
    inputs$verif_fl <- suppressWarnings(as.numeric(trimws(input$verif_fl, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(0, priority = 1000)
  
  reactive({
    inputs$verif_rw <- suppressWarnings(as.numeric(trimws(input$verif_rw, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(0, priority = 1000)

  waveformPeriod_d <- reactive(input$waveformPeriod) %>% debounce(1000, priority = 1000)
  observeEvent(waveformPeriod_d(), {
    inputs$waveformPeriod <-  suppressWarnings(as.numeric(trimws(waveformPeriod_d(), which = "both", whitespace = "[ \t\r\n]")))
  }, priority = 1000)

  reactive({
    inputs$min_optirange <-  suppressWarnings(as.numeric(trimws(input$min_optirange, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(0, priority = 1000)

  reactive({
    inputs$max_optirange <-  suppressWarnings(as.numeric(trimws(input$max_optirange, which = "both", whitespace = "[ \t\r\n]")))
  }) %>% debounce(0, priority = 1000)

  epoch_d <- reactive(input$epoch) %>% debounce(1000, priority = 1000)
  observeEvent(epoch_d(), {
    inputs$epoch <-  suppressWarnings(as.numeric(trimws(epoch_d(), which = "both", whitespace = "[ \t\r\n]")))
  }, priority = 1000)

  variable_d <- reactive(input$variable) %>% debounce(1000, priority = 1000)
  observeEvent(variable_d(), {
    inputs$variable <-  suppressWarnings(as.numeric(trimws(variable_d(), which = "both", whitespace = "[ \t\r\n]")))
  }, priority = 1000)

  errorBar_d <- reactive(input$errorBar) %>% debounce(1000, priority = 1000)
  observeEvent(errorBar_d(), {
    inputs$errorBar <-  suppressWarnings(as.numeric(trimws(errorBar_d(), which = "both", whitespace = "[ \t\r\n]")))
  }, priority = 1000)

  ids_d <- reactive(input$ids) %>% debounce(1000, priority = 1000)
  observeEvent(ids_d(), {
    inputs$ids <-  trimws(ids_d(), which = "both", whitespace = "[ \t\r\n]")
  }, priority = 1000)

  epoch2_d <- reactive(input$epoch2) %>% debounce(1000, priority = 1000)
  observeEvent(epoch2_d(), {
    inputs$epoch2 <-  suppressWarnings(as.numeric(trimws(epoch2_d(), which = "both", whitespace = "[ \t\r\n]")))
  }, priority = 1000)

  variable2_d <- reactive(input$variable2) %>% debounce(1000, priority = 1000)
  observeEvent(variable2_d(), {
    inputs$variable2 <-  suppressWarnings(as.numeric(trimws(variable2_d(), which = "both", whitespace = "[ \t\r\n]")))
  }, priority = 1000)

  errorBar2_d <- reactive(input$errorBar2) %>% debounce(1000, priority = 1000)
  observeEvent(errorBar2_d(), {
    inputs$errorBar2 <-  suppressWarnings(as.numeric(trimws(errorBar2_d(), which = "both", whitespace = "[ \t\r\n]")))
  }, priority = 1000)

  station_x_d <- reactive(input$station_x) %>% debounce(1000, priority = 1000)
  observeEvent(station_x_d(), {
    inputs$station_x <- suppressWarnings(as.numeric(trimws(station_x_d(), which = "both", whitespace = "[ \t\r\n]")))
  }, priority = 1000)
  
  station_x2_d <- reactive(input$station_x2) %>% debounce(1000, priority = 1000)
  observeEvent(station_x2_d(), {
    inputs$station_x2 <- suppressWarnings(as.numeric(trimws(station_x2_d(), which = "both", whitespace = "[ \t\r\n]")))
  }, priority = 1000)

  station_y_d <- reactive(input$station_y) %>% debounce(1000, priority = 1000)
  observeEvent(station_y_d(), {
    inputs$station_y <- suppressWarnings(as.numeric(trimws(station_y_d(), which = "both", whitespace = "[ \t\r\n]")))
  }, priority = 1000)
  
  station_y2_d <- reactive(input$station_y2) %>% debounce(1000, priority = 1000)
  observeEvent(station_y2_d(), {
    inputs$station_y2 <- suppressWarnings(as.numeric(trimws(station_y2_d(), which = "both", whitespace = "[ \t\r\n]")))
  }, priority = 1000)

  station_z_d <- reactive(input$station_z) %>% debounce(1000, priority = 1000)
  observeEvent(station_z_d(), {
    inputs$station_z <- suppressWarnings(as.numeric(trimws(station_z_d(), which = "both", whitespace = "[ \t\r\n]")))
  }, priority = 1000)
  
  station_z2_d <- reactive(input$station_z2) %>% debounce(1000, priority = 1000)
  observeEvent(station_z2_d(), {
    inputs$station_z2 <- suppressWarnings(as.numeric(trimws(station_z2_d(), which = "both", whitespace = "[ \t\r\n]")))
  }, priority = 1000)

  station_lat_d <- reactive(input$station_lat) %>% debounce(1000, priority = 1000)
  observeEvent(station_lat_d(), {
    inputs$station_lat <- suppressWarnings(as.numeric(trimws(station_lat_d(), which = "both", whitespace = "[ \t\r\n]")))
  }, priority = 1000)
  
  station_lat2_d <- reactive(input$station_lat2) %>% debounce(1000, priority = 1000)
  observeEvent(station_lat2_d(), {
    inputs$station_lat2 <- suppressWarnings(as.numeric(trimws(station_lat2_d(), which = "both", whitespace = "[ \t\r\n]")))
  }, priority = 1000)

  station_lon_d <- reactive(input$station_lon) %>% debounce(1000, priority = 1000)
  observeEvent(station_lon_d(), {
    inputs$station_lon <- suppressWarnings(as.numeric(trimws(station_lon_d(), which = "both", whitespace = "[ \t\r\n]")))
  }, priority = 1000)
  
  station_lon2_d <- reactive(input$station_lon2) %>% debounce(1000, priority = 1000)
  observeEvent(station_lon2_d(), {
    inputs$station_lon2 <- suppressWarnings(as.numeric(trimws(station_lon2_d(), which = "both", whitespace = "[ \t\r\n]")))
  }, priority = 1000)

  pole_x_d <- reactive(input$pole_x) %>% debounce(1000, priority = 1000)
  observeEvent(pole_x_d(), {
    inputs$pole_x <- suppressWarnings(as.numeric(trimws(pole_x_d(), which = "both", whitespace = "[ \t\r\n]")))
  }, priority = 1000)

  pole_y_d <- reactive(input$pole_y) %>% debounce(1000, priority = 1000)
  observeEvent(pole_y_d(), {
    inputs$pole_y <- suppressWarnings(as.numeric(trimws(pole_y_d(), which = "both", whitespace = "[ \t\r\n]")))
  }, priority = 1000)

  pole_z_d <- reactive(input$pole_z) %>% debounce(1000, priority = 1000)
  observeEvent(pole_z_d(), {
    inputs$pole_z <- suppressWarnings(as.numeric(trimws(pole_z_d(), which = "both", whitespace = "[ \t\r\n]")))
  }, priority = 1000)

  pole_lat_d <- reactive(input$pole_lat) %>% debounce(1000, priority = 1000)
  observeEvent(pole_lat_d(), {
    inputs$pole_lat <- suppressWarnings(as.numeric(trimws(pole_lat_d(), which = "both", whitespace = "[ \t\r\n]")))
  }, priority = 1000)

  pole_lon_d <- reactive(input$pole_lon) %>% debounce(1000, priority = 1000)
  observeEvent(pole_lon_d(), {
    inputs$pole_lon <- suppressWarnings(as.numeric(trimws(pole_lon_d(), which = "both", whitespace = "[ \t\r\n]")))
  }, priority = 1000)

  pole_rot_d <- reactive(input$pole_rot) %>% debounce(1000, priority = 1000)
  observeEvent(pole_rot_d(), {
    inputs$pole_rot <- suppressWarnings(as.numeric(trimws(pole_rot_d(), which = "both", whitespace = "[ \t\r\n]")))
  }, priority = 1000)

  scaleFactor_d <- reactive(input$scaleFactor) %>% debounce(1000, priority = 1000)
  observeEvent(scaleFactor_d(), {
    if (is.na(as.numeric(input$scaleFactor)) || trimws(input$scaleFactor) == "0") {
      updateTextInput(session, inputId = "scaleFactor", value = "1")
    } else {
      inputs$scaleFactor <- suppressWarnings(as.numeric(trimws(scaleFactor_d(), which = "both", whitespace = "[ \t\r\n]")))
    }
  }, priority = 1000)
  
  station1_d <- reactive(input$station1) %>% debounce(1000, priority = 1000)
  observeEvent(station1_d(), {
    inputs$station1 <- suppressWarnings(trimws(station1_d(), which = "both", whitespace = "[ \t\r\n]"))
  }, priority = 1000)
  
  station2_d <- reactive(input$station2) %>% debounce(1000, priority = 1000)
  observeEvent(station2_d(), {
    inputs$station2 <- suppressWarnings(trimws(station2_d(), which = "both", whitespace = "[ \t\r\n]"))
  }, priority = 1000)
  
  cutStart_d <- reactive(input$cutStart) %>% debounce(1000, priority = 1000)
  observeEvent(cutStart_d(), {
    inputs$cutStart <- suppressWarnings(as.numeric(trimws(cutStart_d(), which = "both", whitespace = "[ \t\r\n]")))
  }, priority = 1000)
  
  cutEnd_d <- reactive(input$cutEnd) %>% debounce(1000, priority = 1000)
  observeEvent(cutEnd_d(), {
    inputs$cutEnd <- suppressWarnings(as.numeric(trimws(cutEnd_d(), which = "both", whitespace = "[ \t\r\n]")))
  }, priority = 1000)
  
  giaTrend_d <- reactive(input$giaTrend) %>% debounce(1000, priority = 1000)
  observeEvent(giaTrend_d(), {
    inputs$giaTrend <- suppressWarnings(as.numeric(trimws(giaTrend_d(), which = "both", whitespace = "[ \t\r\n]")))
  }, priority = 1000)
  
  giaTrend2_d <- reactive(input$giaTrend2) %>% debounce(1000, priority = 1000)
  observeEvent(giaTrend2_d(), {
    inputs$giaTrend2 <- suppressWarnings(as.numeric(trimws(giaTrend2_d(), which = "both", whitespace = "[ \t\r\n]")))
  }, priority = 1000)

  # Update data ####
  observeEvent(c(input$plot, input$sigmas, input$tab, input$format, input$tunits,
                 inputs$step, inputs$epoch, inputs$variable, inputs$errorBar, input$separator,
                 inputs$epoch2, inputs$variable2, inputs$errorBar2, input$separator2, input$format2, input$ne, inputs$scaleFactor,
                 input$fullSeries, info$db1, info$db2,
                 input$eulerType, trans$plate, trans$plate2, input$giaType, trans$gia, trans$gia2,
                 db1[[info$db1]]$status1, db1[[info$db1]]$status2, db1[[info$db1]]$status3, db2[[info$db2]]), {
    req(db1[[info$db1]])
    if (input$tab > 3 || info$tab > 3) {
      req(info$stop)
    }
    removeNotification("kf_not_valid")
    removeNotification("regular")
    if (messages > 0) cat(file = stderr(), mySession, "Updating dataset", "\n")

    table1 <- db1[[info$db1]]
    table2 <- db2[[info$db2]]
    
    if (isTruthy(input$ne)) {
      table2y_tmp <- table2$y2
      table2sy_tmp <- table2$sy2
      table2$y2 <- table2$y1
      table2$sy2 <- table2$sy1
      table2$y1 <- table2y_tmp
      table2$sy1 <- table2sy_tmp
    }
    
    # trans$y0  = all points from the original input series (including deleted with status NA)
    # trans$y   = points with TRUE status
    # trans$sy  = sigmas with TRUE status
    # trans$ye  = points with FALSE status (excluded)
    # trans$sye = sigmas with FALSE status (excluded)
    # trans$y2  = points from secondary series (independent)
    # trans$sy2 = sigmas from secondary series (independent)

    # set time axis
    if (!isTruthy(input$tunits) || input$tunits == 3) {
      trans$x0 <- table1$x3
      trans$x2 <- table2$x3
      info$tunits.label <- "years"
    } else if (input$tunits == 1) {
      trans$x0 <- table1$x1
      trans$x2 <- table2$x1
      info$tunits.label <- "days"
    } else if (input$tunits == 2) {
      trans$x0 <- table1$x2
      trans$x2 <- table2$x2
      info$tunits.label <- "weeks"
    }
    
    # extract data for each component
    if ((input$tab == 1) || (input$format == 4)) {
      trans$y0 <- as.numeric(table1$y1)
      trans$sy0 <- as.numeric(table1$sy1)
      trans$y2 <- as.numeric(table2$y1) * inputs$scaleFactor
      trans$sy2 <- as.numeric(table2$sy1) * inputs$scaleFactor
      status <- table1$status1
      if (isTruthy(trans$plate) && input$eulerType == 2) {
        if (input$format == 4) {
          trans$y0 <- trans$y0 - trans$plate[as.numeric(input$neu1D)]*(trans$x0 - median(trans$x0[table1$status1], na.rm = T)) - median(trans$y0, na.rm = T)
        } else {
          trans$y0 <- trans$y0 - trans$plate[1]*(trans$x0 - median(trans$x0[table1$status1], na.rm = T)) - median(trans$y0, na.rm = T)
        }
      }
      if (input$format == 4 && isTruthy(trans$gia) && input$giaType == 2) {
        trans$y0 <- trans$y0 - trans$gia[3]*(trans$x0 - median(trans$x0[table1$status1], na.rm = T)) - median(trans$y0, na.rm = T)
      }
      if (isTruthy(trans$plate2) && input$eulerType == 2) {
        if (input$format2 == 4) {
          trans$y2 <- trans$y2 - trans$plate2[as.numeric(input$neu1D)]*(trans$x2 - median(trans$x2, na.rm = T)) - median(trans$y2, na.rm = T)
        } else {
          trans$y2 <- trans$y2 - trans$plate2[1]*(trans$x2 - median(trans$x2, na.rm = T)) - median(trans$y2, na.rm = T)
        }
      }
      if (input$format2 == 4 && isTruthy(trans$gia2) && input$giaType == 2) {
        trans$y2 <- trans$y2 - trans$gia2[3]*(trans$x2 - median(trans$x2, na.rm = T)) - median(trans$y2, na.rm = T)
      }
    } else if (input$tab == 2) {
      trans$y0 <- as.numeric(table1$y2)
      trans$sy0 <- as.numeric(table1$sy2)
      trans$y2 <- as.numeric(table2$y2) * inputs$scaleFactor
      trans$sy2 <- as.numeric(table2$sy2) * inputs$scaleFactor
      status <- table1$status2
      if (isTruthy(trans$plate) && input$eulerType == 2) {
        trans$y0 <- trans$y0 - trans$plate[2]*(trans$x0 - median(trans$x0[table1$status2], na.rm = T)) - median(trans$y0, na.rm = T)
      }
      if (isTruthy(trans$plate2) && input$eulerType == 2) {
        trans$y2 <- trans$y2 - trans$plate2[2]*(trans$x2 - median(trans$x2, na.rm = T)) - median(trans$y2, na.rm = T)
      }
    } else if (input$tab == 3) {
      trans$y0 <- as.numeric(table1$y3)
      trans$sy0 <- as.numeric(table1$sy3)
      trans$y2 <- as.numeric(table2$y3) * inputs$scaleFactor
      trans$sy2 <- as.numeric(table2$sy3) * inputs$scaleFactor
      status <- table1$status3
      if (isTruthy(trans$plate) && input$eulerType == 2) {
        trans$y0 <- trans$y0 - trans$plate[3]*(trans$x0 - median(trans$x0[table1$status3], na.rm = T)) - median(trans$y0, na.rm = T)
      }
      if (isTruthy(trans$gia) && input$giaType == 2) {
        trans$y0 <- trans$y0 - trans$gia[3]*(trans$x0 - median(trans$x0[table1$status1], na.rm = T)) - median(trans$y0, na.rm = T)
      }
      if (isTruthy(trans$plate2) && input$eulerType == 2) {
        trans$y2 <- trans$y2 - trans$plate2[3]*(trans$x2 - median(trans$x2, na.rm = T)) - median(trans$y2, na.rm = T)
      }
      if (isTruthy(trans$gia2) && input$giaType == 2) {
        trans$y2 <- trans$y2 - trans$gia2[3]*(trans$x2 - median(trans$x2, na.rm = T)) - median(trans$y2, na.rm = T)
      }
    }
    # getting data range including excluded points
    trans$x <- trans$xe <- trans$x0
    trans$x <- trans$x[status & !is.na(status)]
    trans$xe <- trans$xe[!status & !is.na(status)]
    info$removed <- length(trans$x[!status | is.na(status)])
    trans$y <- trans$y0[!is.na(status)]
    if (isTruthy(input$fullSeries) && input$optionSecondary < 2) {
      # show all points from primary & secondary series
      info$minx <- min(trans$x, trans$xe, trans$x2, na.rm = T)
      info$maxx <- max(trans$x, trans$xe, trans$x2, na.rm = T)
      ranges$x1 <- c(info$minx, info$maxx)
    } else {
      # show all points from primary series only
      info$minx <- min(trans$x, trans$xe, na.rm = T)
      info$maxx <- max(trans$x, trans$xe, na.rm = T)
      ranges$x1 <- c(info$minx, info$maxx)
    }
    info$miny <- min(trans$y, na.rm = T)
    info$maxy <- max(trans$y, na.rm = T)
    ids <- trans$x0[!is.na(status)] >= ranges$x1[1] & trans$x0[!is.na(status)] <= ranges$x1[2]
    if (sum(ids) > 0) {
      ranges$y1 <- range(trans$y[ids], na.rm = T)
      if (any(is.na(ranges$y1)) || any(is.infinite(ranges$y1))) {
        ranges$y1 <- c(info$miny, info$maxy)
      }
    } else {
      ranges$y1 <- c(info$miny, info$maxy)
    }
    # getting only valid points
    trans$y <- trans$y0[status & !is.na(status)]
    trans$ye <- trans$y0[!status & !is.na(status)]
    trans$sy <- trans$sy0[status & !is.na(status)]
    trans$sye <- trans$sy0[!status & !is.na(status)]
    if (length(file$secondary) > 0 && input$optionSecondary == 1 && any(!is.na(trans$y2))) {
      ids <- trans$x2[!is.na(trans$y2)] >= ranges$x1[1] & trans$x2[!is.na(trans$y2)] <= ranges$x1[2]
      if (sum(ids) > 0) {
        ranges$y12 <- range(trans$y2[ids], na.rm = T)
        if (any(is.na(ranges$y12)) || any(is.infinite(ranges$y12))) {
          ranges$y12 <- range(trans$y2, na.rm = T)
        }
      } else {
        ranges$y12 <- range(trans$y2, na.rm = T)
      }
    }
    # getting data sampling
    info$points <- length(trans$x)
    info$sampling <- min(diff(trans$x,1))
    if (!isTruthy(info$step)) {
      info$sampling0 <- info$sampling
    }
    info$sampling_regular <- median(diff(trans$x))
    info$tol <- ifelse(info$sampling_regular - info$sampling < info$sampling * 0.25, info$sampling * 0.25, info$sampling_regular - info$sampling)
    info$rangex <- trans$x[length(trans$x)] - trans$x[1]
    times <- round(diff(trans$x)/info$sampling)
    trans$gaps <- c(T, unlist(lapply(1:length(times), function(i) ifelse(times[i] == 1, T, list(unlist(list(rep(F, times[i] - 1),T)))))))
    # Getting significant decimals from the primary series
    # info$decimalsx <- decimalplaces(signif(info$sampling, digits = 2),"x")
    info$decimalsx <- decimalplaces(trans$x, "x")
    info$decimalsy <- decimalplaces(trans$y, "y")
    if (!isTruthy(info$decimalsy)) {
      info$decimalsy <- 4
      showNotification(HTML("It was not possible to extract the number of decimal places from the series.<br>Using 4 decimal places by default."), action = NULL, duration = 10, closeButton = T, id = "no_decimals", type = "warning", session = getDefaultReactiveDomain())
    }
    if (isTruthy(info$scientific)) {
      info$digits <- info$decimalsy + 1
      info$nsmall <- 0
    } else {
      info$digits <- 0
      info$nsmall <- info$decimalsy
    }
    trans$ordinate <- median(trans$y)
    info$noise <- (sd(head(trans$y, 30)) + sd(tail(trans$y, 30)))/2
    # dealing with the kalman filter series
    if (input$fitType == 2 && length(trans$mod) > 0 && length(trans$res) > 0) {
      if (sum(db1[[info$db1]][[paste0("status",input$tab)]], na.rm = T) < sum(db1[[info$db1]]$status.kf, na.rm = T)) {
        trans$mod <- trans$mod0[!is.na(db1[[info$db1]][[paste0("status",input$tab)]]) & db1[[info$db1]][[paste0("status",input$tab)]]]
        trans$res <- trans$res0[!is.na(db1[[info$db1]][[paste0("status",input$tab)]]) & db1[[info$db1]][[paste0("status",input$tab)]]]
        trans$kalman <- trans$kalman0[!is.na(db1[[info$db1]][[paste0("status",input$tab)]]) & db1[[info$db1]][[paste0("status",input$tab)]],]
        trans$kalman_unc <- trans$kalman_unc0[!is.na(db1[[info$db1]][[paste0("status",input$tab)]]) & db1[[info$db1]][[paste0("status",input$tab)]],]
        showNotification(HTML("At least one point used in the KF fit was removed. The KF fit results are no longer valid.<br>Consider running it again."), action = NULL, duration = 10, closeButton = T, id = "kf_not_valid", type = "warning", session = getDefaultReactiveDomain())
        updateButton(session, inputId = "runKF", label = " Run KF", icon = icon("filter", class = NULL, lib = "font-awesome"), style = "danger")
      } else if (sum(db1[[info$db1]][[paste0("status",input$tab)]], na.rm = T) > sum(db1[[info$db1]]$status.kf, na.rm = T)) {
        info$run <- F
        trans$mod <- trans$mod0 <- NULL
        trans$res <- trans$res0 <- NULL
        trans$kalman <- trans$kalman0 <- NULL
        trans$kalman_unc <- trans$kalman_unc0 <- NULL
        showNotification("At least one point previously not used in the KF fit has been restored in the series. The KF fit is no longer valid. Consider running it again.", action = NULL, duration = 10, closeButton = T, id = "kf_not_valid", type = "warning", session = getDefaultReactiveDomain())
      } else if (any(is.na(trans$mod0[!is.na(db1[[info$db1]][[paste0("status",input$tab)]]) & db1[[info$db1]][[paste0("status",input$tab)]]]))) {
        info$run <- F
        trans$mod <- trans$mod0 <- NULL
        trans$res <- trans$res0 <- NULL
        trans$kalman <- trans$kalman0 <- NULL
        trans$kalman_unc <- trans$kalman_unc0 <- NULL
        showNotification(HTML("At least one point previously not used in the KF fit has been restored in the series. The KF fit is no longer valid.<br>Consider running it again."), action = NULL, duration = 10, closeButton = T, id = "kf_not_valid", type = "warning", session = getDefaultReactiveDomain())
      } else {
        trans$mod <- trans$mod0[!is.na(db1[[info$db1]][[paste0("status",input$tab)]]) & db1[[info$db1]][[paste0("status",input$tab)]]]
        trans$res <- trans$res0[!is.na(db1[[info$db1]][[paste0("status",input$tab)]]) & db1[[info$db1]][[paste0("status",input$tab)]]]
        trans$kalman <- trans$kalman0[!is.na(db1[[info$db1]][[paste0("status",input$tab)]]) & db1[[info$db1]][[paste0("status",input$tab)]],]
        trans$kalman_unc <- trans$kalman_unc0[!is.na(db1[[info$db1]][[paste0("status",input$tab)]]) & db1[[info$db1]][[paste0("status",input$tab)]],]
        updateButton(session, inputId = "runKF", label = " Run KF", icon = icon("filter", class = NULL, lib = "font-awesome"), style = "default")
      }
    }
    # setting sigmas to one if needed
    if (!isTruthy(input$sigmas)) {
      trans$sy <- rep(1, length(trans$sy))
      trans$sy0 <- rep(1, length(trans$sy0))
      trans$sye <- rep(1, length(trans$sye))
      trans$sy2 <- rep(1, length(trans$sy2))
    }
    # removing wavelet data
    if (input$waveletType > 0) {
      updateRadioButtons(session, inputId = "waveletType", label = NULL,
                         choices = list("None" = 0, "Original" = 1, "Model" = 2, "Model res." = 3, "Filter" = 4, "Filter res." = 5),
                         selected = 0, inline = T, choiceNames = NULL,  choiceValues = NULL)
      updateTextInput(session, "min_wavelet", value = "")
      updateTextInput(session, "max_wavelet", value = "")
      updateTextInput(session, "res_wavelet", value = "")
    }
    #
    if (isTruthy(debug)) {
      debugMem()
    }
  }, priority = 3)

  # Load SARI file ####
  observeEvent(input$loadSARI, {
    req(db1[[info$db1]])
    removeNotification("sari_version")
    removeNotification("format_not_compatible")
    removeNotification("no_model")
    removeNotification("no_sari")
    comments <- grep("^#", readLines(con = input$loadSARI$datapath, n = 100, ok = T, warn = F, skipNul = T), ignore.case = F, perl = T, value = T, fixed = F, useBytes = F, invert = F)
    if (isTruthy(comments) && grepl("^# SARI ", comments[1], ignore.case = F, perl = T)) {
      if (messages > 0) cat(file = stderr(), mySession, "Loading SARI file", "\n")
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
            mn <- strsplit(grep("^# Measurement noise: ", comments, ignore.case = F, perl = T, value = T), ":")
            if (isTruthy(mn) && length(mn) > 0) {
              measurement_noise <- gsub("[a-zA-Z]", "", mn[[1]][2])
              updateTextInput(session, inputId = "ObsError", value = measurement_noise)
            }
            components <- c()
            # Extracting Intercept info
            if (grepl("Intercept", model, ignore.case = F, perl = T)) {
              index <- grep(" Intercept", aprioris, ignore.case = F, perl = F, value = F, fixed = T, useBytes = F, invert = F)
              values <- strsplit(aprioris[index], "=|\\+\\/-")
              if (isTruthy(values) && length(values) > 0) {
                values <- values[[1]]
                updateTextInput(inputId = "Intercept0", value = values[2])
                updateTextInput(inputId = "eIntercept0", value = values[3])
              }
            }
            # Extracting Rate info
            if (grepl(" \\+ Rate", model, ignore.case = F, perl = T)) {
              updateTextInput(session, "trendRef", value = text[2])
              index <- grep(" Rate", aprioris, ignore.case = F, perl = F, value = F, fixed = T, useBytes = F, invert = F)
              values <- strsplit(aprioris[index], "=|\\+\\/-")
              if (isTruthy(values) && length(values) > 0) {
                values <- values[[1]]
                updateTextInput(inputId = "Trend0", value = values[2])
                updateTextInput(inputId = "eTrend0", value = values[3])
              }
              index <- grep(" Rate", process_noises, ignore.case = F, perl = F, value = F, fixed = T, useBytes = F, invert = F)
              values <- strsplit(process_noises[index], "=")
              if (isTruthy(values) && length(values) > 0) {
                values <- values[[1]]
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
              info$trendRef <- T
              components <- c(components, "Linear")
            }
            # Extracting Polynomial info
            if (grepl(" \\+ P", model, ignore.case = F, perl = T)) {
              index <- grep(" + P", text, ignore.case = F, perl = F, value = F, fixed = T, useBytes = F, invert = F)
              updateTextInput(session, "PolyRef", value = text[index[1] + 1])
              updateTextInput(session, "PolyCoef", value = text[index[length(index)] + 3])
              info$PolyRef <- T
              components <- c(components, "Polynomial")
            }
            # Extracting Sinusoidal info
            if (grepl(" \\+ S", model, ignore.case = F, perl = T)) {
              index <- grep(" + S", text, ignore.case = F, perl = F, value = F, fixed = T, useBytes = F, invert = F)
              inputs$periodRef <- unique(text[index + 1])
              updateTextInput(session, "periodRef", value = inputs$periodRef)
              info$periodRef <- T
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
            info$menu <- unique(c(info$menu, 4))
            updateCollapse(session, id = "menu", open = info$menu)
            shinyjs::delay(1000, updateRadioButtons(session, inputId = "fitType", selected = 1))
            shinyjs::delay(1100, updateCheckboxGroupInput(session, inputId = "model", choices = list("Linear","Polynomial","Sinusoidal","Offset","Exponential","Logarithmic"), inline = T, selected = components))
          }
        }
      }
    } else {
      showNotification(HTML("Unable to find the SARI version in the uploaded file.<br><br>Is this a SARI file?"), action = NULL, duration = 10, closeButton = T, id = "no_sari", type = "warning", session = getDefaultReactiveDomain())
    }
  })

  # Plot series ####
  output$plot1 <- output$plot2 <- output$plot3 <- renderPlot({
    req(db1[[info$db1]], trans$x, trans$y, trans$sy)
    removeNotification("wrong_series")
    if (messages > 0) cat(file = stderr(), mySession, "Plotting the series", "\n")
    title <- ""
    sigmas <- F
    if (isTruthy(input$sigmas) && ((info$format == 4 && isTruthy(inputs$errorBar)) || input$format != 4)) {
      sigmas <- T
    }
    if (length(isolate(file$secondary)) > 0 && input$optionSecondary == 1 && any(!is.na(trans$y2))) {
      if (input$symbol == 0) {
        symbol <- 'p'
      } else if (input$symbol == 1) {
        symbol <- 'l'
      } else if (input$symbol == 2) {
        symbol <- 'o'
      }
      if (isTruthy(input$sameScale)) {
        pointsX1 <- trans$x[trans$x > ranges$x1[1] & trans$x < ranges$x1[2]]
        pointsX2 <- trans$x2[trans$x2 > ranges$x1[1] & trans$x2 < ranges$x1[2]]
        pointsY1 <- trans$y[trans$x > ranges$x1[1] & trans$x < ranges$x1[2]]
        pointsY2 <- trans$y2[trans$x2 > ranges$x1[1] & trans$x2 < ranges$x1[2]]
        half <- abs(ranges$y1[1] - mean(ranges$y1))
        middle <- ifelse(isTruthy(pointsY2), median(pointsY2), 0)
        ranges$y12 <- c(middle - half, middle + half)
        if (length(pointsX1) == 0 || length(pointsX2) == 0) {
          # NA
        } else if (pointsX2[1] > pointsX1[length(pointsX1)]) {
          # NA
        } else if (pointsX1[1] > pointsX2[length(pointsX2)]) {
          # NA
        } else {
          tie1 <- head(sort(sapply(pointsX1, function(x) min(abs(pointsX2 - x))), index.return = T)$ix, 100)
          tie2 <- head(sort(sapply(pointsX2, function(x) min(abs(pointsX1 - x))), index.return = T)$ix, 100)
          tie1 <- tie1[1:min(length(tie1),length(tie2))]
          tie2 <- tie2[1:min(length(tie1),length(tie2))]
          pointsBias <- median(pointsY1[tie1] - pointsY2[tie2])
          ranges$y12 <- isolate(ranges$y12 + (ranges$y1[1] - ranges$y12[1]) - pointsBias)
        }
      } else if (isTruthy(input$same_axis)) {
        ranges$y12 <- ranges$y1
      } else {
        ids <- trans$x2 >= ranges$x1[1] & trans$x2 <= ranges$x1[2]
        if (sum(ids) > 0) {
          ranges$y12 <- range(trans$y2[ids], na.rm = T) 
        } else {
          ranges$y12 <- range(trans$y2, na.rm = T)
        }
      }
      plot(trans$x2, trans$y2, type = symbol, lwd = 2, pch = 20, col = SARIcolors[3], axes = F, xlab = NA, ylab = NA, xlim = ranges$x1, ylim = ranges$y12)
      if (isTruthy(sigmas)) {
        color <- SARIcolors[3]
        alfa <- 0.2
        shade <- adjustcolor(color, alpha.f = alfa)
        ba <- trans$y2 + trans$sy2
        bb <- trans$y2 - trans$sy2
        polygon(c(trans$x2, rev(trans$x2)), c(ba, rev(bb)), col = shade, border = NA)
      }
      axis(side = 4, at = NULL, labels = T, tick = T, line = NA, pos = NA, outer = F)
      par(new = T)
    }
    plot_series(trans$x,trans$y,trans$sy,ranges$x1,ranges$y1,sigmas,title,input$symbol,T)
    points(trans$xe, trans$ye, type = "p", col = SARIcolors[2], bg = 2, pch = 21)
    xx <- median(trans$x[trans$x > ranges$x1[1] & trans$x < ranges$x1[2]], na.rm = T)
    yy <- median(trans$y[trans$x > ranges$x1[1] & trans$x < ranges$x1[2]], na.rm = T)
    centerx <- which(abs(trans$x - xx) == min(abs(trans$x - xx)))[1]
    centery <- which(abs(trans$y - yy) == min(abs(trans$y - yy)))[1]
    if (input$tab == 1 || input$tab == 2) {
      if (input$eulerType == 1 && length(trans$plate[!is.na(trans$plate)]) == 3) {
        rate <- trans$plate[as.numeric(input$tab)]
      }
      if (input$format == 4 && isTruthy(input$gia) && input$giaType == 1 && length(trans$gia[!is.na(trans$gia)]) == 3) {
        rate <- ifelse(exists("rate") && is.numeric(rate), yes = rate + trans$gia[3], no = trans$gia[3])
      }
    } else if (input$giaType == 1 && length(trans$gia[!is.na(trans$gia)]) == 3) {
      rate <- trans$gia[3]
    }
    if (exists("rate") && is.numeric(rate)) {
      lines(c(trans$x[1],trans$x[length(trans$x)]),c(trans$y[centery] + rate*(trans$x[1] - trans$x[centerx]),trans$y[centery] + rate*(trans$x[length(trans$x)] - trans$x[centerx])), col = SARIcolors[4], lwd = 3)
    }
    if (input$traceLog && length(info$log) > 0) {
      for (r in info$log[[2]]) {
        abline(v = r, col = SARIcolors[4], lty = 2)
      }
      for (a in info$log[[1]]) {
        abline(v = a, col = SARIcolors[4])
      }
    }
    if (input$traceSinfo && length(info$sinfo) > 0) {
      for (r in info$sinfo[[2]]) {
        abline(v = r, col = SARIcolors[6], lty = 2)
      }
      for (a in info$sinfo[[1]]) {
        abline(v = a, col = SARIcolors[6])
      }
    }
    if (input$traceSoln && length(info$soln) > 0) {
      for (a in info$soln) {
        abline(v = a, col = SARIcolors[8])
      }
    }
    if (input$traceCustom && length(info$custom) > 0) {
      for (a in info$custom) {
        abline(v = a, col = SARIcolors[5])
      }
    }
    if (length(trans$mod) > 0 && isTruthy(info$run)) {
      lines(trans$x,trans$mod, col = SARIcolors[2], lwd = 3)
    }
    if (length(trans$filter) > 0 && input$filter == T && input$series2filter == 1) {
      lines(trans$x,trans$filter, col = SARIcolors[7], lwd = 3)
    }
    if (ranges$x1[1] > info$minx || ranges$x1[2] < info$maxx) {
      shinyjs::show(paste0("zoomin",input$tab))
    } else {
      shinyjs::hide(paste0("zoomin",input$tab))
    }
    js$checkPopup()
    shinyjs::delay(100, {
      if (isTruthy(info$overview) && isTRUE(isolate(input$overview))) {
        shinyjs::click("plotAll")
      }
    })
    output$plot1_info <- output$plot2_info <- output$plot3_info <- renderText({
      if (length(input$plot_1click$x) > 0) {
        paste("Plot coordinates = ", input$plot_1click$x, input$plot_1click$y, sep = "\t")
      }
    })
}, width = reactive(info$width))
  output$plot41 <- renderPlot({
      plot3series(1)
  }, width = reactive(info$width))
  output$plot42 <- renderPlot({
      plot3series(2)
  }, width = reactive(info$width))
  output$plot43 <- renderPlot({
      plot3series(3)
  }, width = reactive(info$width))
  output$plot4_info <- renderText({
    x <- y <- NULL
    if (length(inputs$plot4_1click$x) > 0) {
      x <- inputs$plot4_1click$x
      y <- inputs$plot4_1click$y
      paste("Plot coordinates =", x, y, sep = "\t")
    }
  })

  # MIDAS ####
  observeEvent(c(input$midas, trans$y, trans$offsetEpochs, input$tunits), {
    req(trans$x, trans$y, info$tol)
    if (isTruthy(input$midas)) {
      if (input$tunits == 1) {
        period <- 365
      } else if (input$tunits == 2) {
        period <- 52
      } else if (input$tunits == 3) {
        period <- 1
      }
      if ((info$rangex / period) < 1) {
        showNotification("Not enough data available to compute interannual differences.", action = NULL, duration = 10, closeButton = T, id = "no_interannual", type = "error", session = getDefaultReactiveDomain())
        updateCheckboxInput(session, inputId = "midas", label = NULL, value = F)
        req(info$stop)
      }
      if (messages > 0) cat(file = stderr(), mySession, "Computing MIDAS", "\n")
      if (length(trans$x) > 6) {
        withProgress(message = 'Computing MIDAS trend.',
                     detail = 'This may take a while ...', value = 0, {
                       setProgress(0)
                       vel <- sapply(1:length(trans$x), function(x) midas_vel(m = x, t = period, disc = 0, trans$y))
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
                         setProgress(0)
                         vel <- sapply(1:length(trans$x), function(x) midas_vel(m = x, t = period, disc = 1, trans$y))
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
                       } else {
                         trans$midas_vel2 <- NULL
                       }
                     })
      } else {
        showNotification("Not enough interannual differences to compute a reliable trend.", action = NULL, duration = 10, closeButton = T, id = "no_interannual", type = "error", session = getDefaultReactiveDomain())
        updateCheckboxInput(session, inputId = "midas", label = NULL, value = F)
      }
    } else {
      trans$midas_vel <- NULL
    }
    #
    if (isTruthy(debug)) {
      debugMem()
    }
  })

  # Plot MIDAS histogram ####
  output$midas_hist1 <- output$midas_hist2 <- output$midas_hist3 <- renderPlot({
    req(db1[[info$db1]], input$midas, trans$midas_all)
    if (messages > 0) cat(file = stderr(), mySession, "MIDAS histogram", "\n")
    if (input$tunits == 1) {
      period <- "day"
    } else if (input$tunits == 2) {
      period <- "week"
    } else if (input$tunits == 3) {
      period <- "year"
    }
    if (input$sunits == 1) {
      units <- paste0("(m/",period,")")
    } else if (input$sunits == 2) {
      units <- paste0("(mm/",period,")")
    } else {
      units <- ""
    }
    values <- trans$midas_all
    hist(values, breaks = "FD", freq = F, xlab = paste("Selected interannual velocities", units), ylab = "", main = "MIDAS velocity histogram", col = SARIcolors[4])
    dnorm(values, mean = mean(values, na.rm = T), sd = sd(values), log = F)
    xfit <- seq(min(values),max(values),length = 40)
    yfit <- dnorm(xfit,mean = mean(values, na.rm = T),sd = sd(values))
    lines(xfit, yfit, col = SARIcolors[2], lwd = 3)
  }, width = reactive(info$width))
  
  # Minimum entropy ####
  observeEvent(c(input$entropy, trans$y, trans$offsetEpochs, inputs$offsetEpoch.entropy, info$rangex), {
    removeNotification("bad_entropy")
    removeNotification("different_offsets")
    removeNotification("bad_offset_epoch")
    req(trans$x, trans$y)
    if (isTruthy(input$entropy)) {
      # checking offset epochs provided for the entropy analysis
      offsetEpoch.entropy <- NULL
      if (isTruthy(inputs$offsetEpoch.entropy)) {
        offsetEpoch.entropy <- trimws(unlist(strsplit(inputs$offsetEpoch.entropy, split = ",")))
        offsetEpoch.entropy_all <- offsetEpoch.entropy
        # check for valid numeric values
        not_numeric <- suppressWarnings(which(is.na(as.numeric(offsetEpoch.entropy))))
        if (length(not_numeric) > 0) {
          offsetEpoch.entropy <- offsetEpoch.entropy[-not_numeric]
          showNotification(HTML(paste("The epoch given for offset(s)", paste0("#",not_numeric, collapse = " "), "is not numeric.<br>These offsets were skipped.")), action = NULL, duration = 10, closeButton = T, id = "not_numeric_offset", type = "warning", session = getDefaultReactiveDomain())
        }
        offsetEpoch.entropy <- as.numeric(offsetEpoch.entropy)
        if (length(offsetEpoch.entropy) > 0) {
          # check for soln without observations
          offsetEpoch.entropy_sorted <- suppressWarnings(sort(offsetEpoch.entropy, na.last = NA))
          if (length(offsetEpoch.entropy_sorted) > 1) {
            invalidSegment <- sapply(seq(length(offsetEpoch.entropy_sorted) - 1), function(x) length(trans$x[trans$x > offsetEpoch.entropy_sorted[x] & trans$x < offsetEpoch.entropy_sorted[x + 1]]) ) == 0
            for (soln in which(invalidSegment)) {
              uselessOffset_id <- which.min(abs(offsetEpoch.entropy - offsetEpoch.entropy_sorted[soln]))
              uselessOffset_id1 <- which.min(abs(suppressWarnings(as.numeric(offsetEpoch.entropy_all) - offsetEpoch.entropy_sorted[soln])))
              uselessOffset_id2 <- which.min(abs(suppressWarnings(as.numeric(offsetEpoch.entropy_all) - offsetEpoch.entropy_sorted[soln + 1])))
              offsetEpoch.entropy <- offsetEpoch.entropy[-uselessOffset_id]
              showNotification(HTML(paste0("There are no observations between offsets #", uselessOffset_id1, " and #", uselessOffset_id2,".<br>The first offset was skipped")), action = NULL, duration = 10, closeButton = T, id = "bad_offset_epoch", type = "warning", session = getDefaultReactiveDomain())
            }
          }
          # check for offsets outside data limits
          toremove <- 999999
          for (i in seq_len(length(offsetEpoch.entropy))) {
            if (offsetEpoch.entropy[i] > trans$x[length(trans$x)] || offsetEpoch.entropy[i] < trans$x[1]) {
              uselessOffset_id <- which.min(abs(suppressWarnings(as.numeric(offsetEpoch.entropy_all) - offsetEpoch.entropy[i])))
              toremove <- c(toremove, i)
              showNotification(HTML(paste0("There are no observations before or after offset #", uselessOffset_id,".<br>This offset was skipped.")), action = NULL, duration = 10, closeButton = T, id = "bad_offset_epoch", type = "warning", session = getDefaultReactiveDomain())
            }
          }
          offsetEpoch.entropy <- offsetEpoch.entropy[-toremove]
        }
      }
      trans$offsetEpoch.entropy <- offsetEpoch.entropy
      # updating the list of entropy offsets with the LS offsets
      if (isTruthy(input$model)) {
        if (length(setdiff(trans$offsetEpochs,trans$offsetEpoch.entropy)) > 0) {
          breaks <- paste(unique(sort(c(trans$offsetEpochs,offsetEpoch.entropy))), collapse = ", ")
          updateTextInput(session, inputId = "offsetEpoch.entropy", value = breaks)
          req(info$stop)
        }
        if (length(trans$offsetEpochs) < length(offsetEpoch.entropy)) {
          showNotification(HTML("Warning: there are more offset epochs used in the entropy estimate than in the LS estimate.<br>Check all the offset epochs are correct for the entropy estimate."), action = NULL, duration = 10, closeButton = T, id = "different_offsets", type = "warning", session = getDefaultReactiveDomain())
        }
      }
      if (messages > 0) cat(file = stderr(), mySession, "Computing entropy", "\n")
      # getting velocity samples to be tested
      ap <- try(unname(summary(lm(trans$y~trans$x))$coefficients)[2,1:2], silent = T)
      if (!isTruthy(ap) || inherits(ap,"try-error")) {
        ap <- c(0,1)
      }
      ap.1 <- ap[1] - ap[2]*100
      ap.2 <- ap[1] + ap[2]*100
      vel_samples <- 200
      incr <- (ap.2 - ap.1)/vel_samples
      aps <- seq(ap.1,ap.2,incr)
      # estimating the entropy for each velocity
      time <- ceiling(0.0009 * info$points) + 1
      withProgress(message = 'Optimizing min entropy.',
                   detail = paste("This should take about", time, "s"), value = 0, {
                     setProgress(0.3) # just for progress bar lovers
                     H <- sapply(aps, function(v) compute_entropy(v))
                     setProgress(0.7)
                     Sys.sleep(0.5)
                     setProgress(1)
                     Sys.sleep(0.5)
                   })
      # getting the velocity estimate from the minimum entropy
      minH <- which.min(H)
      if (minH < 10 || minH > vel_samples - 10) {
        showNotification(HTML("The minimim entropy value is not optimal.<br>The series may not be linear or some discontinuities may need to be removed."), action = NULL, duration = 10, closeButton = T, id = "bad_entropy", type = "warning", session = getDefaultReactiveDomain())
      }
      trans$entropy_vel <- aps[minH]
      # reducing the series length due to offsets
      n <- 0
      breaks <- unique(sort(c(trans$x[1],trans$offsetEpochs,offsetEpoch.entropy,trans$x[length(trans$x)])))
      for (i in seq_len(length(breaks) - 1)) {
        segment <- trans$x >= breaks[i] & trans$x < breaks[i + 1]
        n <- ifelse(sum(segment) > n, sum(segment), n)
      }
      l <- (n + 2*info$points) / (3*info$points) # longest segment counts 1/3 of total length
      # compute the velocity uncertainty
      trans$entropy_sig <- 2^(min(H) - 2.0471) / (info$rangex * l)
    } else {
      trans$entropy_vel <- trans$entropy_sig <- NULL
    }
    #
    if (isTruthy(debug)) {
      debugMem()
    }
  })
  
  # Offset verification ####
  observeEvent(c(input$runVerif), {
    req(trans$res, trans$x, trans$offsetEpochs)
    if (isTruthy(input$verif_offsets) &&
        ((nchar(inputs$verif_white) > 0 && !is.na(inputs$verif_white) && inputs$verif_white > 0) ||
        (nchar(input$verif_pl) > 0 && nchar(input$verif_k) > 0 && !is.na(inputs$verif_pl) && !is.na(inputs$verif_k) && inputs$verif_pl > 0 && inputs$verif_k <= 0) ||
        (nchar(inputs$verif_fl) > 0 && !is.na(inputs$verif_fl) && inputs$verif_fl > 0) ||
        (nchar(inputs$verif_rw) > 0 && !is.na(inputs$verif_rw) && inputs$verif_rw > 0))) {
      if (messages > 0) cat(file = stderr(), mySession, "Verifying offsets", "\n")
      n <- length(trans$res)
      n_all <- length(trans$gaps)
      C <- matrix(0,n,n)
      scaling <- 10^signifdecimal(sd(trans$res), T)
      if (isTruthy(inputs$verif_fl) || isTruthy(inputs$verif_rw) || isTruthy(inputs$verif_pl)) {
        estimatedTime <- as.integer(ceiling(1.3547*exp(0.0007*n)/60))
      } else {
        estimatedTime <- as.integer(ceiling(1.3988*exp(0.0006*n)/60))
      }
      withBusyIndicatorServer("runVerif", {
        withProgress(message = 'Verifying offset values.',
                     detail = paste("This should take about", estimatedTime, "min"), value = 0, {
                       start.time <- Sys.time()
                       if (isTruthy(inputs$verif_white) && inputs$verif_white > 0) {
                         C <- C + as.numeric(inputs$verif_white)^2 * diag(n) * scaling^2
                       }
                       if (isTruthy(inputs$verif_fl) && inputs$verif_fl > 0) {
                         Cfl <- cov_powerlaw(-1,n_all,F,trans$gaps,info$sampling)[[1]]
                         C <- C + as.numeric(inputs$verif_fl)^2 * Cfl * scaling^2
                       }
                       if (isTruthy(inputs$verif_rw) && inputs$verif_rw > 0) {
                         Crw <- cov_powerlaw(-2,n_all,F,trans$gaps,info$sampling)[[1]]
                         C <- C + as.numeric(inputs$verif_rw)^2 * Crw * scaling^2
                       }
                       if (isTruthy(inputs$verif_pl) && inputs$verif_pl > 0 && isTruthy(inputs$verif_k) && inputs$verif_k < 0) {
                         k <- as.numeric(inputs$verif_k)
                         Cpl <- cov_powerlaw(k,n_all,F,trans$gaps,info$sampling)[[1]]
                         C <- C + as.numeric(inputs$verif_pl)^2 * Cpl * scaling^2
                       } else {
                         updateTextInput(session, inputId = "verif_pl", value = "")
                         updateTextInput(session, inputId = "verif_k", value = "")
                       }
                       Sys.sleep(1)
                       setProgress(0.2)
                       if (!all(C == 0)) {
                         trans$verif <- F
                         y0 <- (trans$res - mean(trans$res)) * scaling
                         line <- c()
                         setProgress(0.9)
                         Sys.sleep(1)
                         for (i in seq_len(length(trans$offsetEpochs))) {
                           if (input$fitType == 1) {
                             offsets <- grep(pattern = "O", rownames(trans$LScoefs), ignore.case = F, perl = F, fixed = T)
                             ya <- y0 + trans$LScoefs[offsets[i]]*scaling*I(trans$x > trans$offsetEpochs[i])
                           } else if (input$fitType == 2) {
                             offsets <- grep(pattern = "O", colnames(trans$kalmanman), ignore.case = F, perl = F, fixed = T)
                             ya <- y0 + colMeans(trans$kalmanman)[offsets[i]]*scaling*I(trans$x > trans$offsetEpochs[i])
                           }
                           Tq <- crossprod(ya, solve(C, ya)) - crossprod(y0, solve(C, y0))
                           line <- c(line, paste("Offset",i,"significance:", sprintf("%.0f",pchisq(Tq, df = 1)*100),"%", sep = " "))
                         }
                         if (isTruthy(Tq)) {
                           trans$verif <- T
                         }
                       }
                     })
        end.time <- Sys.time()
        # print(end.time - start.time)
      })
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
    #
    if (isTruthy(debug)) {
      debugMem()
    }
  })

  # LS fit ####
  observeEvent(c(input$model, input$sigmas, inputs$LogariRef, inputs$L0, inputs$TL0, inputs$ExponenRef, inputs$E0,
                 inputs$TE0, inputs$offsetEpoch, inputs$period, inputs$periodRef, inputs$trendRef, input$fitType,
                 input$tab, inputs$PolyRef, inputs$PolyCoef, input$P0, input$correct_waveform, inputs$step, input$tunits,
                 trans$y, trans$sy), {
    if (input$tab == 4) {
      req(info$stop)
    }
    req(trans$x, trans$y, trans$sy, trans$ordinate)
    removeNotification("bad_errorbar")
    removeNotification("bad_sinusoidal")
    removeNotification("bad_LS")
    if (input$tab == 6) {
      req(info$stop)
    }
    output$offsetFound <- renderUI({ NULL })
    output$est.white <- renderUI({ NULL })
    output$est.flicker <- renderUI({ NULL })
    output$est.randomw <- renderUI({ NULL })
    output$est.powerl <- renderUI({ NULL })
    output$est.index <- renderUI({ NULL })
    output$est.mle <- renderUI({ NULL })
    output$est.unc <- renderUI({ NULL })
    trans$noise <- NULL
    trans$mle <- 0
    if (length(input$model) > 0) {
      if (input$fitType == 1) {
        if (messages > 0) cat(file = stderr(), mySession, "LS fit", "\n")
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
        x <- trans[[paste0("res",input$tab,"x")]] <- trans$x
        y <- trans$y - trans$ordinate
        if (isTruthy(input$correct_waveform)) {
          if (length(trans$pattern) > 0) {
            y <- y - trans$pattern
          } else {
            updateCheckboxInput(session, inputId = "correct_waveform", value = F)
            updateCheckboxInput(session, inputId = "waveform", label = NULL, value = F)
          }
        }
        sy <- trans$sy
        if (any(sy <= 0) || any(is.na(sy))) {
          showNotification(HTML("Some errorbar values are not valid.<br>No weighting applied."), action = NULL, duration = 10, closeButton = T, id = "bad_errorbar", type = "error", session = getDefaultReactiveDomain())
          sy <- rep(1, length(y))
          updateCheckboxInput(session, inputId = "sigmas", label = NULL, value = F)
        }
        weights <- 1/(sy^2)
        m <- model(x,y)
        if (isTruthy(info$run) && isTruthy(m)) {
          info$run <- F
          trans$mle <- F
          trans$verif <- NULL
          model <- m$model
          model_lm <- m$model_lm
          apriori <- m$apriori
          req(model, apriori)
          if (messages > 1) cat(file = stderr(), mySession, model, "\n")
          fit <- NULL
          fit <- try(nls(as.formula(model), model = T, start = apriori, trace = F, weights = weights, control = nls.control(minFactor = 1/8192, warnOnly = F, printEval = F)), silent = F)
          if (!inherits(fit,"try-error") && !is.null(fit)) {
            info$run <- T
            info$noLS <- F
            jacobian <- fit$m$gradient()/sqrt(weights)
            synthesis <- summary(fit,correlation = T, signif.stars = T)
            synthesis$coefficients[1] <- coef(synthesis)[1] + trans$ordinate
            synthesis$coefficients[1,3] <- abs(synthesis$coefficients[1,1]) / synthesis$coefficients[1,2]
            synthesis$coefficients[1,4] <- 2 * pt(abs(synthesis$coefficients[1,3]), synthesis$df , lower.tail = F)[2]
            trans$names <- names(coef(fit))
            synthesis$formula <- deparse(synthesis$formula)
            synthesis$formula <- gsub(" > ", ">", gsub(" - ", "-", gsub(" \\* ", "\\*", gsub("))", ")", gsub("I\\(cos", "cos", gsub("I\\(sin", "sin", gsub("^ *|(?<= ) | *$", "", Reduce(paste, synthesis$formula), perl = TRUE)))))))
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
            if (any(grepl(pattern = "S", row.names(synthesis$coefficients)))) {
              ss <- 0
              info_out <- list()
              for (s in which(grepl(pattern = "S", row.names(synthesis$coefficients)))) {
                ss <- ss + 1
                sine <- synthesis$coefficients[s,1]
                cosine <- synthesis$coefficients[s + 1,1]
                sine_err <- synthesis$coefficients[s,2]
                cosine_err <- synthesis$coefficients[s + 1,2]
                sine_cosine_cov <- synthesis$sigma^2 * synthesis$cov.unscaled[s,s + 1]
                amp <- sqrt(sine^2 + cosine^2)
                phase <- atan2(cosine,sine)
                amp_err <- try(sqrt((sine^2*sine_err^2 + cosine^2*cosine_err^2 + 2*sine*cosine*sine_cosine_cov)/amp^2), silent = F)
                phase_err <- try(sqrt((sine^2*cosine_err^2 + cosine^2*sine_err^2 - 2*sine*cosine*sine_cosine_cov)/amp^4), silent = F)
                if (isTruthy(amp_err) && isTruthy(phase_err) && !inherits(amp_err,"try-error") && !inherits(phase_err,"try-error")) {
                  for (i in list(noquote(trans$periods[ss]), amp, amp_err, phase, phase_err)) {
                    info_out[[length(info_out) + 1]] <- i
                  }
                } else {
                  if (messages > 1) cat(file = stderr(), mySession, a, amp, phase, sine, sine_err, cosine, cosine_err, synthesis$cov.unscaled[s,s + 1], "\n")
                  showNotification(HTML(paste0("Unable to compute the amplitude and/or phase error from the errors of the sine and cosine coefficients of sinusoid ",ss,".<br><br>Please contact the author to repport this problem.")), action = NULL, duration = 10, closeButton = T, id = "bad_sinusoidal", type = "error", session = getDefaultReactiveDomain())
                  ss <- ss - 1
                }
              }
              if (isTruthy(info_out)) {
                synthesis$sinusoidales <- matrix(data = info_out, nrow = ss, ncol = 5, byrow = T)
                dimnames(synthesis$sinusoidales) <- list(paste0("Sinusoidal ",1:ss), c("Period","Amplitude","Amp. Error","Phase (rad)","Ph. Error (rad)"))
              }
            }
            trans$equation <- sub("y ~","Model =",m$model)
            trans$results <- synthesis
            trans$LScoefs <- synthesis$coefficients
            trans$res <- trans[[paste0("res",input$tab,"y")]] <- res
            if (isTruthy(input$wavelet) && input$waveletType > 1) {
              updateRadioButtons(session, inputId = "waveletType", label = NULL, choices = list("None" = 0, "Original" = 1, "Model" = 2, "Model res." = 3, "Filter" = 4, "Filter res." = 5), selected = 0, inline = T, choiceNames = NULL,  choiceValues = NULL)
            }
            trans$moderror <- sqrt( diag(jacobian %*% synthesis$cov.unscaled %*% t(jacobian)) )
            if (isTruthy(synthesis$sigma)) {
              if (!any(1/weights < trans$moderror^2)) {
                trans$reserror <- trans[[paste0("res",input$tab,"sy")]] <- sqrt( 1/weights - trans$moderror^2 ) * synthesis$sigma
              }
              trans$moderror <- trans$moderror * synthesis$sigma
            }
            trans$mod <- trans[[paste0("mod",input$tab,"y")]] <- mod
            if (isTruthy(inputs$waveformPeriod)) {
              save_value <- inputs$waveformPeriod
              updateTextInput(session, "waveformPeriod", value = "")
              updateTextInput(session, "waveformPeriod", value = save_value)
            }
          } else {
            if (isTruthy(info$noLS) || "Logarithmic" %in% input$model || "Exponential" %in% input$model) {
              trans$results <- NULL
              trans$unc <- NULL
              trans$res <- NULL
              trans$mod <- NULL
              trans$LScoefs <- NULL
              trans$names <- NULL
              showNotification(HTML("Unable to fit the LS model.<br>Change the model components."), action = NULL, duration = 10, closeButton = T, id = "bad_LS", type = "error", session = getDefaultReactiveDomain())
            } else {
              info$noLS <- T
              if ("Sinusoidal" %in% input$model) {
                updateTextInput(session, "periodRef", value = inputs$periodRef*1.000001)
              } else if ("Linear" %in% input$model) {
                updateTextInput(session, "trendRef", value = inputs$trendRef*1.000001)
              } else if ("Polynomial" %in% input$model) {
                updateTextInput(session, "PolyRef", value = inputs$PolyRef*1.000001)
              }
            }
          }
        } else {
          trans$results <- NULL
          trans$res <- NULL
          trans$mod <- NULL
          trans$LScoefs <- NULL
          trans$names <- NULL
        }
      }
    } else {
      trans$results <- NULL
      trans$res <- NULL
      trans$mod <- NULL
      trans$LScoefs <- NULL
      trans$names <- NULL
    }
    #
    if (isTruthy(debug)) {
      debugMem()
    }
  }, priority = 2)

  # KF fit ####
  observeEvent(input$runKF, {
    req(input$model, trans$x, trans$y)
    removeNotification("no_weighting")
    removeNotification("bad_model")
    removeNotification("not_even")
    removeNotification("bad_measurement_error")
    removeNotification("bad_a_priori_state")
    removeNotification("bad_variance")
    removeNotification("bad_kf")
    removeNotification("kf_not_valid")
    removeNotification("bad_obserror")
    removeNotification("bad_sigmaPoints")
    updateButton(session, inputId = "runKF", label = " Run KF", icon = icon("filter", class = NULL, lib = "font-awesome"), style = "default")
    output$offsetFound <- renderUI({ NULL })
    output$est.white <- renderUI({ NULL })
    output$est.flicker <- renderUI({ NULL })
    output$est.randomw <- renderUI({ NULL })
    output$est.powerl <- renderUI({ NULL })
    output$est.index <- renderUI({ NULL })
    output$est.mle <- renderUI({ NULL })
    output$est.unc <- renderUI({ NULL })
    trans$noise <- NULL
    trans$mle <- 0
    if (input$fitType == 2) {
      trans$mle <- F
      trans$verif <- NULL
      withBusyIndicatorServer("runKF", {
        withProgress(message = 'Running Kalman Filter.',
                     detail = 'This may take a while ...', value = 0, {
                       x <- trans$x
                       y <- trans$y
                       if (isTruthy(input$correct_waveform)) {
                         if (length(trans$pattern) > 0) {
                           y <- y - trans$pattern
                         }
                       }
                       sy <- trans$sy
                       if (any(sy <= 0) || any(is.na(sy))) {
                         showNotification(HTML("Some errorbar values are not valid.<br>No weighting applied."), action = NULL, duration = 10, closeButton = T, id = "no_weighting", type = "error", session = getDefaultReactiveDomain())
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
                       trans$KFnames <- unlist(m$nouns)
                       if (!isTruthy(m) && length(m$apriori) < 2) {
                         showNotification(HTML("Not enough model components to run the KF.<br>Check the input values."), action = NULL, duration = 10, closeButton = T, id = "bad_model", type = "error", session = getDefaultReactiveDomain())
                         info$run <- F
                         req(info$stop)
                       }
                       if (messages > 1) cat(file = stderr(), mySession, m$model, "\n")
                       apriori <- as.numeric(m$apriori)
                       unc_ini <- as.numeric(m$error)^2
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
                         if (messages > 2) cat(file = stderr(), mySession, "This iteration =", sqrt(exp(x[1])), "\n")
                         info$KFiter <- info$KFiter + 1
                         showNotification(paste0("Running KF iteration ", info$KFiter), action = NULL, duration = NULL, closeButton = T, id = "KF_iter", type = "warning", session = getDefaultReactiveDomain())
                         mod <- NULL
                         mod <- list(
                           m0 = apriori,
                           C0 = diag(unc_ini),
                           V = exp(x[1]),
                           W = diag(m$processNoise))
                         UKF(y = data, mod = mod, FFfunction = FFfunction, GGfunction = GGfunction, simplify = T, logLik = T)$logLik
                       }
                       sigmaR <- NULL
                       if (isTruthy(input$ObsError)) {
                         if (isTruthy(inputs$ObsError) && inputs$ObsError > 0) {
                           sigmaR <- inputs$ObsError
                         } else {
                           sigmaR <- info$noise/5
                           max_decimals <- signifdecimal(sigmaR, F) + 2
                           updateTextInput(session, "ObsError", value = sprintf("%.*f", max_decimals, sigmaR))
                           showNotification("The input measurement error is not valid.", action = NULL, duration = 10, closeButton = T, id = "bad_obserror", type = "error", session = getDefaultReactiveDomain())
                         }
                       } else {
                         sigmaR <- info$noise/5
                         max_decimals <- signifdecimal(sigmaR, F) + 2
                         updateTextInput(session, "ObsError", value = sprintf("%.*f", max_decimals, sigmaR))
                       }
                       if (isTruthy(input$errorm)) {
                         if (isTruthy(inputs$min_optirange)) {
                           min_optirange <- inputs$min_optirange
                         } else {
                           min_optirange <- info$noise/10
                           max_decimals <- signifdecimal(min_optirange, F) + 2
                           updateTextInput(session, "min_optirange", value = sprintf("%.*f", max_decimals, min_optirange))
                         }
                         if (isTruthy(inputs$max_optirange)) {
                           max_optirange <- inputs$max_optirange
                         } else {
                           max_optirange <- info$noise*10
                           max_decimals <- signifdecimal(max_optirange, F) + 2
                           updateTextInput(session, "max_optirange", value = sprintf("%.*f", max_decimals, max_optirange))
                         }
                         if (min_optirange > 0 && max_optirange > 0 && max_optirange > min_optirange) {
                           if (messages > 0) cat(file = stderr(), mySession, "Optimizing measurement noise", "\n")
                           info$KFiter <- 0
                           mod <- optim(log(sigmaR^2), llikss, lower = log(as.numeric(min_optirange)^2), upper = log(as.numeric(max_optirange)^2), method = "Brent", hessian = T, data = y, control = list(reltol = exp(as.numeric(min_optirange)/10)))
                           removeNotification("KF_iter")
                           if (mod$convergence == 0) {
                             sigmaR <- sqrt(exp(mod$par))
                             seParms <- sqrt(diag(solve(mod$hessian)))
                             max_decimals <- signifdecimal(sigmaR, F) + 2
                             updateTextInput(session, "ObsError", value = sprintf("%.*f", max_decimals, sigmaR))
                             if (isTruthy(seParms)) {
                               rangoR <- sqrt(exp(mod$par + qnorm(.05/2)*seParms %o% c(1,-1)))
                               max_decimals <- max(signifdecimal(rangoR, F)) + 2
                               updateTextInput(session, "min_optirange", value = sprintf("%.*f", max_decimals, rangoR[1]))
                               updateTextInput(session, "max_optirange", value = sprintf("%.*f", max_decimals, rangoR[2]))
                               updateCheckboxInput(inputId = "errorm", value = F)
                             }
                           }
                         } else {
                           showNotification(HTML("The input measurement error bounds are not valid.<br>Skipping optimization."), action = NULL, duration = 10, closeButton = T, id = "bad_measurement_error", type = "error", session = getDefaultReactiveDomain())
                         }
                       }
                       if (!isTruthy(sigmaR)) {
                         sigmaR <- info$noise/5
                         max_decimals <- signifdecimal(rangoR, F) + 2
                         updateTextInput(session, "ObsError", value = sprintf("%.*f", max_decimals, sigmaR))
                       }
                       if (isTruthy(input$sigmas)) {
                         sigmaR <- sigmaR * sy / median(sy)
                       } else {
                         sigmaR <- rep(sigmaR, length(trans$y))
                       }
                       ex1 <- list(m0 = apriori, C0 = diag(unc_ini), V = sigmaR^2, W = diag(m$processNoise))
                       kfs <- NULL
                       if (any(is.na(ex1$C0))) {
                         showNotification(HTML("Missing information required on the a priori state to run the Kalman filter.<br>Check the input values"), action = NULL, duration = 10, closeButton = T, id = "bad_a_priori_state", type = "error", session = getDefaultReactiveDomain())
                         info$run <- F
                         req(info$stop)
                       }
                       # EKF
                       if (input$kf == 1) {
                         if (messages > 0) cat(file = stderr(), mySession, "EKF fit", "\n")
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
                               showNotification(HTML("Negative estimated state variances were found and changed to NA.<br>Something went wrong with the EKF fit."), action = NULL, duration = 15, closeButton = T, id = "bad_variance", type = "warning", session = getDefaultReactiveDomain())
                             }
                           } else {
                             trans$results <- NULL
                             trans$res <- NULL
                             trans$mod <- NULL
                             info$run <- F
                             showNotification(HTML("Unable to run the EKF smoother.<br>The error covariances of the initial state may be zero or too large"), action = NULL, duration = 10, closeButton = T, id = "bad_kf", type = "error", session = getDefaultReactiveDomain())
                           }
                         } else {
                           trans$results <- NULL
                           trans$res <- NULL
                           trans$mod <- NULL
                           info$run <- F
                           showNotification(HTML("Unable to fit the EKF.<br>Change the model parameters."), action = NULL, duration = 10, closeButton = T, id = "bad_kf", type = "error", session = getDefaultReactiveDomain())
                         }
                         # UKF
                       } else if (input$kf == 2) {
                         if (messages > 0) cat(file = stderr(), mySession, "UKF fit", "\n")
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
                               showNotification(HTML("Negative estimated state variances were found and changed to NA.<br>Something went wrong with the UKF fit."), action = NULL, duration = 15, closeButton = T, id = "bad_variance", type = "warning", session = getDefaultReactiveDomain())
                             }
                           } else {
                             trans$results <- NULL
                             trans$res <- NULL
                             trans$mod <- NULL
                             info$run <- F
                             showNotification(HTML("Unable to run the UKF smoother.<br>The error covariances of the initial state may be zero or too large."), action = NULL, duration = 10, closeButton = T, id = "bad_kf", type = "error", session = getDefaultReactiveDomain())
                           }
                         } else {
                           trans$results <- NULL
                           trans$res <- NULL
                           trans$mod <- NULL
                           info$run <- F
                           showNotification(HTML("Unable to fit the UKF.<br>Change the model parameters."), action = NULL, duration = 10, closeButton = T, id = "bad_kf", type = "error", session = getDefaultReactiveDomain())
                         }
                       }
                       # Common EKF & UKF
                       if (isTruthy(kfs$s)) {
                         info$run <- T
                         e <- kfs$s[2:nrow(kfs$s),]
                         if ("Linear" %in% input$model && !is.na(as.numeric(input$TrendDev)) && as.numeric(input$TrendDev) > 0) {
                           trans$mod <- sapply(1:length(x), function(k) if (k == 1) { e[1,1] } else { eval(parse(text = sub("+ e[k,2]*(x[k] - x[k-1])", "", model_kf, fixed = T))) })
                         } else {
                           trans$mod <- sapply(1:length(x), function(k) eval(parse(text = model_kf)) )
                         }
                         trans$mod0 <- db1[[info$db1]][[paste0("status",input$tab)]]
                         trans$mod0[which(trans$mod0)] <- trans$mod
                         trans$mod0[!db1[[info$db1]][[paste0("status",input$tab)]]] <- NA
                         trans$res <- y - trans$mod
                         trans$res0 <- db1[[info$db1]][[paste0("status",input$tab)]]
                         trans$res0[which(trans$res0)] <- trans$res
                         trans$mod0[!db1[[info$db1]][[paste0("status",input$tab)]]] <- NA
                         if (isTruthy(input$correct_waveform) && length(trans$pattern) > 0) {
                           trans$mod <- trans$mod0 <- trans$mod + trans$pattern
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
                         trans$kalman <- e
                         trans$kalman0 <- matrix(db1[[info$db1]][[paste0("status", input$tab)]], nrow = length(db1[[info$db1]][[paste0("status", input$tab)]]), ncol = ncol(trans$kalman))
                         trans$kalman0[which(trans$kalman0)] <- trans$kalman
                         trans$kalman0[!db1[[info$db1]][[paste0("status",input$tab)]]] <- NA
                         colnames(trans$kalman0) <- colnames(trans$kalman)
                         trans$kalman_unc <- sqrt(kfs_unc)
                         trans$kalman_unc0 <- matrix(db1[[info$db1]][[paste0("status", input$tab)]], nrow = length(db1[[info$db1]][[paste0("status", input$tab)]]), ncol = ncol(trans$kalman_unc))
                         trans$kalman_unc0[which(trans$kalman_unc0)] <- trans$kalman_unc
                         trans$kalman_unc0[!db1[[info$db1]][[paste0("status",input$tab)]]] <- NA
                         colnames(trans$kalman_unc0) <- colnames(trans$kalman_unc)
                         trans$results <- formatting(psych::describe(trans$kalman, na.rm = F, interp = T, skew = F, ranges = T, trim = 0, type = 3, check = T, fast = F, quant = c(.05,.25,.75,.95), IQR = T),1)
                         trans$kalman_info <- m
                         trans$equation <- sub("y ~","Model =",m$model)
                         end.time <- Sys.time()
                         time.taken <- end.time - start.time
                         if (messages > 2) cat(file = stderr(), mySession, "Total time =", time.taken, "\n")
                         db1[[info$db1]]$status.kf <- db1[[info$db1]][[paste0("status",input$tab)]]
                         if (isTruthy(inputs$waveformPeriod)) {
                           save_value <- inputs$waveformPeriod
                           updateTextInput(session, "waveformPeriod", value = "")
                           updateTextInput(session, "waveformPeriod", value = save_value)
                         }
                         # Plot instantaneous rate
                         output$rate1 <- output$rate2 <- output$rate3 <- renderPlot({
                           if ("Linear" %in% input$model && length(trans$kalman) > 0 && trans$kalman_info$processNoise[2] > 0) {
                             if (input$tunits == 1) {
                               period <- "day"
                             } else if (input$tunits == 2) {
                               period <- "week"
                             } else if (input$tunits == 3) {
                               period <- "year"
                             }
                             if (input$sunits == 1) {
                               units <- paste0("(m/",period,")")
                             } else if (input$sunits == 2) {
                               units <- paste0("(mm/",period,")")
                             } else {
                               units <- ""
                             }
                             title <- "Instantaneous linear rate"
                             plot_series(trans$x,trans$kalman[,2],trans$kalman_unc[,2],ranges$x2,ranges$y4,T,"",input$symbol,F)
                             title(ylab = units)
                             title(title, line = 3)
                           }
                         }, width = reactive(info$width))
                         trans$model_old <- input$model
                         updateButton(session, inputId = "runKF", label = " Run KF", icon = icon("filter", class = NULL, lib = "font-awesome"), style = "default")
                       }
                     })
      })
    }
    #
    if (isTruthy(debug)) {
      debugMem()
    }
  })

  # Plot residuals ####
  output$res1 <- output$res2 <- output$res3 <- renderPlot({
    req(trans$res, trans$x, trans$sy, info$run)
    if (messages > 0) cat(file = stderr(), mySession, "Plotting residual series", "\n")
    if (input$sunits == 1) {
      units <- "(m)"
    } else if (input$sunits == 2) {
      units <- "(mm)"
    } else {
      units <- ""
    }
    title <- "Model residuals"
    if (!is.null(trans$filter)) {
      title <- paste(title, "(black) & Filter-Model residuals (yellow)")
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
    plot_series(trans$x,trans$res,ey,ranges$x2,ranges$y2,sigmas,"",input$symbol,T)
    title(title, line = 3)
    abline(h = 0, col = SARIcolors[2], lwd = 3)
    if (input$traceLog && length(info$log) > 0) {
      for (r in info$log[[2]]) {
        abline(v = r, col = SARIcolors[4], lty = 2)
      }
      for (a in info$log[[1]]) {
        abline(v = a, col = SARIcolors[4])
      }
    }
    if (input$traceSinfo && length(info$sinfo) > 0) {
      for (r in info$sinfo[[2]]) {
        abline(v = r, col = SARIcolors[6], lty = 2)
      }
      for (a in info$sinfo[[1]]) {
        abline(v = a, col = SARIcolors[6])
      }
    }
    if (input$traceSoln && length(info$soln) > 0) {
      for (a in info$soln[[1]]) {
        abline(v = a, col = SARIcolors[8])
      }
    }
    if (input$traceCustom && length(info$custom) > 0) {
      for (a in info$custom) {
        abline(v = a, col = SARIcolors[5])
      }
    }
    if ("Offset" %in% isolate(input$model)) {
      for (p in trans$offsetEpochs) {
        abline(v = p, col = SARIcolors[2], lwd = 2)
      }
    }
    if (!is.null(trans$filter) && input$filter == T) {
      if (input$series2filter == 1) {
        lines(trans$x,trans$filter - trans$mod, col = SARIcolors[7], lwd = 3)
      } else if (input$series2filter == 2) {
        lines(trans$x,trans$filter, col = SARIcolors[7], lwd = 3)
      }
    }
  }, width = reactive(info$width))

  # Compute stats & histogram ####
  observeEvent(c(input$histogramType, trans$y, trans$res, trans$filter, ranges$x1, input$tab, inputs$epoch, inputs$variable, inputs$errorBar, input$sunits), {
    req(db1[[info$db1]], input$histogram)
    if (input$sunits == 1) {
      units <- "(m)"
    } else if (input$sunits == 2) {
      units <- "(mm)"
    } else {
      units <- ""
    }
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
      updateRadioButtons(session, inputId = "histogramType", label = NULL, choices = list("None" = 0, "Original" = 1, "Model" = 2, "Model res." = 3, "Filter" = 4, "Filter res." = 5), selected = 0, inline = T, choiceNames = NULL,  choiceValues = NULL)
      req(info$stop)
    }
    removeNotification("no_histogram")
    removeNotification("no_histogram")
    if (isTruthy(values) && isTruthy(sd(values)) && length(values) > 1 && sd(values) > 0) {
      if (messages > 0) cat(file = stderr(), mySession, "Plotting histogram", "\n")
      output$hist1 <- output$hist2 <- output$hist3 <- renderPlot({
        title <- paste("Histogram of the", label, units)
        h <- hist(values, breaks = "FD", plot = F)
        h$density <- 100*h$density/sum(h$density)
        plot(h, col = SARIcolors[5], freq = F, xlab = paste("Values",units), ylab = "Frequency (%)", main = title)
        xfit <- seq(min(values),max(values),length = 3*length(h$mids))
        yfit <- dnorm(xfit, mean = mean(values, na.rm = T), sd = sd(values, na.rm = T))
        lines(xfit, yfit*3*100/sum(yfit), col = SARIcolors[2], lwd = 3)
      }, width = reactive(info$width))
      if (messages > 0) cat(file = stderr(), mySession, "Computing statistics", "\n")
      adf <- try(suppressWarnings(adf.test(values, alternative = "stationary")), silent = T)
      kpss <- suppressWarnings(kpss.test(values, null = "Level"))
      stats <- psych::describe(matrix(values, ncol = 1, byrow = T), na.rm = T, interp = F, skew = T, ranges = T, trim = 0, type = 3, check = T, fast = F, quant = c(.05,.25,.75,.95), IQR = T)
      output$stats1 <- output$stats2 <- output$stats3 <- renderPrint({
        if (!inherits(adf,"try-error") && !is.null(adf) && isTruthy(adf$p.value) && isTruthy(kpss$p.value)) {
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
        } else {
          showNotification(HTML("Unable to assess stationarity.<br>Check the input series."), action = NULL, duration = 10, closeButton = T, id = "no_stationarity", type = "error", session = getDefaultReactiveDomain())
        }
        if (input$sunits == 1) {
          cat("Series units: m", "\n\n")
        } else if (input$sunits == 2) {
          cat("Series units: mm", "\n\n")
        }
        print(stats,digits = info$decimalsy)
      }, width = 180)
    } else {
      showNotification(HTML("Unable to compute the histogram.<br>Check the input series."), action = NULL, duration = 10, closeButton = T, id = "no_histogram", type = "error", session = getDefaultReactiveDomain())
      updateRadioButtons(session, inputId = "histogramType", label = NULL, choices = list("None" = 0, "Original" = 1, "Model" = 2, "Model res." = 3, "Filter" = 4, "Filter res." = 5), selected = 0, inline = T, choiceNames = NULL,  choiceValues = NULL)
    }
  })

  # Fit summary ####
  output$summary1 <- output$summary2 <- output$summary3 <- renderPrint({
    req(db1[[info$db1]])
    if (input$optionSecondary == 1 && isTruthy(trans$y2)) {
      serie1 <- data.frame(x = trans$x, y = trans$y)
      serie2 <- data.frame(x = trans$x2, y = trans$y2)
      common <- merge(serie1, serie2, by.x = "x", by.y = "x")
      if (length(common$x) > 30) {
        cat("Pearson's correlation =", sprintf("%.3f",cor(common$y.x,common$y.y)), "from", length(common$x),"points at common epochs\n\n")
      }
    }
    if (input$tunits == 1) {
      period <- "day"
    } else if (input$tunits == 2) {
      period <- "week"
    } else if (input$tunits == 3) {
      period <- "year"
    }
    if (input$sunits == 1) {
      unit <- "m"
      units <- paste0("m/",period)
    } else if (input$sunits == 2) {
      unit <- "mm"
      units <- paste0("mm/",period)
    } else {
      unit <- ""
      units <- ""
    }
    if (input$sunits > 0 && isTruthy(info$run) && length(trans$results) > 0) {
      cat("Parameter units:", unit, "&", units, "\n\n")
    }
    if (isTruthy(input$midas)) {
      cat("MIDAS rate estimate","\n")
      cat(trans$midas_vel, "+/-", trans$midas_sig, units, "\n\n")
      if (length(trans$offsetEpochs) > 0 && "Offset" %in% isolate(input$model)) {
        cat("MIDAS rate estimate (discontinuities skipped)","\n")
        cat(trans$midas_vel2, "+/-", trans$midas_sig2, units, "\n\n")
      }
    }
    if (isTruthy(input$entropy) && isTruthy(trans$entropy_vel) && isTruthy(trans$entropy_sig)) {
      cat("Minimum entropy rate estimate","\n")
      cat(trans$entropy_vel, "+/-", trans$entropy_sig, units, "\n\n")
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
    removeNotification("no_repeat")
    removeNotification("bad_waveform_period")
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
          average <- as.data.frame.matrix(table[,list(avg = weightedMedian(y,1/sy^2), std = sd(y)), by = z])
          result <- merge(serie,average, by = "z")
          if (uniques > 0) {
            result$std[is.na(result$std)] <- result$sy[is.na(result$std)]
            showNotification(HTML(paste0(uniques," data epochs are not repeated in the series.<br>The waveform may be wrong at these epochs.")), action = NULL, duration = 10, closeButton = T, id = "no_repeat", type = "warning", session = getDefaultReactiveDomain())
          }
          result <- result[order(result$x0),]
          trans$pattern <- result$avg
          if (messages > 0) cat(file = stderr(), mySession, "Computing periodic waveform", "\n")
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
              waveform <- data.frame(x = result[toplot,]$x, y = result[toplot,]$avg, sy = result[toplot,]$std)
              waveform <- waveform[order(waveform$x),]
              plot(waveform$x,waveform$y, type = symbol, pch = 20, xlab = "Epoch of period", ylab = "Average value", main = title)
              ba <- waveform$y + waveform$sy
              bb <- waveform$y - waveform$sy
              polygon(c(waveform$x, rev(waveform$x)), c(ba, rev(bb)), col = rgb(0,0,0,0.2), border = NA)
            }
          }, width = reactive(info$width))
        } else {
          showNotification(HTML("The period of the waveform is not valid.<br>Check the input value."), action = NULL, duration = 10, closeButton = T, id = "bad_waveform_period", type = "error", session = getDefaultReactiveDomain())
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
    #
    if (isTruthy(debug)) {
      debugMem()
    }
  })

  # Computing spectrum ####
  observeEvent(c(input$spectrum, inputs$short_period, inputs$long_period, inputs$ofac, inputs$step), {
    req(db1[[info$db1]], input$spectrum)
    removeNotification("bad_periods")
    if (is.na(inputs$long_period) && input$long_period != "") {
      showNotification(HTML("The longest period is not a numeric value.<br>Check the input value."), action = NULL, duration = 10, closeButton = T, id = "bad_long", type = "error", session = getDefaultReactiveDomain())
      req(info$stop)
    }
    if (is.na(inputs$short_period) && input$short_period != "") {
      showNotification(HTML("The shortest period is not a numeric value.<br>Check the input value."), action = NULL, duration = 10, closeButton = T, id = "bad_short", type = "error", session = getDefaultReactiveDomain())
      req(info$stop)
    }
    if (is.na(inputs$ofac) && input$ofac != "") {
      showNotification(HTML("The oversampling value is not numeric.<br>Check the input value."), action = NULL, duration = 10, closeButton = T, id = "bad_oversampling", type = "error", session = getDefaultReactiveDomain())
      req(info$stop)
    }
    if (messages > 0) cat(file = stderr(), mySession, "Setting periodogram limits", "\n")
    trans$fs <- NULL
    trans$title <- c("Lomb-Scargle periodogram: ")
    # max_period <- as.numeric(sprintf("%.*f", info$decimalsx, info$rangex)) # full range
    max_period <- trunc(info$rangex * 10^info$decimalsx) / 10^info$decimalsx # truncated range
    intervals <- as.data.frame(table(diff(trans$x)), stringsAsFactors = F)
    # min_period <- 2*gcd(trans$x[-1]*10^info$decimalsx-trans$x[1]*10^info$decimalsx)/10^info$decimalsx #following Eyer and Bartholdi 1999
    min_period <- 2*as.numeric(sort(intervals$Var1[intervals$Freq/(length(trans$x) - 1) >= 0.5], decreasing = T)[1]) #approximate the shortest period by twice the shortest interval repeating itself at least 50% of the time
    if (isTruthy(min_period)) {
      min_period <- as.numeric(sprintf("%.*f", info$decimalsx, min_period))
    } else {
      min_period <- as.numeric(sprintf("%.*f", info$decimalsx, 2*info$sampling_regular))
    }
    # Setting longest period
    if (isTruthy(inputs$long_period)) {
      if (inputs$long_period > max_period) {
        showNotification(HTML("The input longest period is out of bounds.<br>Using the longest valid value instead."), action = NULL, duration = 10, closeButton = T, id = "bad_long", type = "warning", session = getDefaultReactiveDomain())
        long_period <- max_period
        inputs$long_period <- long_period
        updateTextInput(session, "long_period", value = sprintf("%.*f", info$decimalsx, long_period))
        trans$fs <- NULL
        req(info$stop)
      } else {
        long_period <- inputs$long_period
      }
    } else {
      long_period <- max_period
      short_period <- min_period
      inputs$long_period <- long_period
      inputs$short_period <- short_period
      updateTextInput(session, "short_period", value = sprintf("%.*f", info$decimalsx, short_period))
      updateTextInput(session, "long_period", value = sprintf("%.*f", info$decimalsx, long_period))
      ranges$x3 <- NULL
      trans$fs <- NULL
      req(info$stop)
    }
    # Setting shortest period
    if (isTruthy(inputs$short_period)) {
      if (inputs$short_period < min_period) {
        showNotification(HTML("The input shortest period is smaller than the propossed value.<br>The periodogram may be aliased into the Nyquist period range."), action = NULL, duration = 10, closeButton = T, id = "bad_short", type = "warning", session = getDefaultReactiveDomain())
      }
      short_period <- inputs$short_period
    } else {
      long_period <- max_period
      short_period <- min_period
      inputs$long_period <- long_period
      inputs$short_period <- short_period
      updateTextInput(session, "short_period", value = sprintf("%.*f", info$decimalsx, short_period))
      updateTextInput(session, "long_period", value = sprintf("%.*f", info$decimalsx, long_period))
      ranges$x3 <- NULL
      trans$fs <- NULL
      req(info$stop)
    }
    # Setting oversampling
    if (isTruthy(inputs$ofac)) {
      if (inputs$ofac < 0.01 || inputs$ofac > 100) {
        showNotification(HTML("The input oversampling value is out of bounds [0.01 - 100].<br>Using 1 instead."), action = NULL, duration = 10, closeButton = T, id = "bad_oversampling", type = "error", session = getDefaultReactiveDomain())
        trans$fs <- NULL
        updateTextInput(session, "ofac", value = 1)
        req(info$stop)
      } else {
        ofac <- inputs$ofac
        if (isTruthy(ranges$x3) && (ranges$x3[1] != short_period || ranges$x3[2] != long_period)) {
          short_period <- ranges$x3[1]
          long_period <- ranges$x3[2]
          inputs$long_period <- long_period
          inputs$short_period <- short_period
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
      ofac <- 1
      inputs$ofac <- ofac
      updateTextInput(session, "ofac", value = ofac)
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
      if (any(c(input$spectrumOriginal, input$spectrumModel, input$spectrumResiduals, input$spectrumFilter, input$spectrumFilterRes))) {
        periodogram("all")
      }
    } else {
      showNotification(HTML("Negative, null or invalid period bounds for the periodogram.<br>Check the input values."), action = NULL, duration = 10, closeButton = T, id = "bad_periods", type = "error", session = getDefaultReactiveDomain())
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
    req(db1[[info$db1]])
    if (isTruthy(trans$spectra_old[1])) {
      trans$psd[,1] <- NA
      trans$amp[,1] <- NA
      trans$spectra_old[1] <- F
      trans$title[2] <- NA
    } else {
      req(input$spectrum, input$spectrumOriginal)
      periodogram("original")
      if (input$tab == 1 || input$format == 4) {
        runjs("window.scrollTo(0,document.getElementById('lomb1').offsetTop);")
      } else if (input$tab == 2) {
        runjs("window.scrollTo(0,document.getElementById('lomb2').offsetTop);")
      } else if (input$tab == 3) {
        runjs("window.scrollTo(0,document.getElementById('lomb3').offsetTop);")
      }
    }
  })
  observeEvent(c(input$spectrumModel), {
    req(db1[[info$db1]])
    if (isTruthy(trans$spectra_old[2])) {
      trans$psd[,2] <- NA
      trans$amp[,2] <- NA
      trans$spectra_old[2] <- F
      trans$title[3] <- NA
    } else {
      req(input$spectrum, input$spectrumModel)
      periodogram("model")
      if (input$tab == 1 || input$format == 4) {
        runjs("window.scrollTo(0,document.getElementById('lomb1').offsetTop);")
      } else if (input$tab == 2) {
        runjs("window.scrollTo(0,document.getElementById('lomb2').offsetTop);")
      } else if (input$tab == 3) {
        runjs("window.scrollTo(0,document.getElementById('lomb3').offsetTop);")
      }
    }
  })
  observeEvent(c(input$spectrumResiduals), {
    req(db1[[info$db1]])
    if (isTruthy(trans$spectra_old[3])) {
      trans$psd[,3] <- NA
      trans$amp[,3] <- NA
      trans$spectra_old[3] <- F
      trans$title[4] <- NA
    } else {
      req(input$spectrum, input$spectrumResiduals)
      periodogram("residuals")
      if (input$tab == 1 || input$format == 4) {
        runjs("window.scrollTo(0,document.getElementById('lomb1').offsetTop);")
      } else if (input$tab == 2) {
        runjs("window.scrollTo(0,document.getElementById('lomb2').offsetTop);")
      } else if (input$tab == 3) {
        runjs("window.scrollTo(0,document.getElementById('lomb3').offsetTop);")
      }
    }
  })
  observeEvent(c(input$spectrumFilter), {
    req(db1[[info$db1]])
    if (isTruthy(trans$spectra_old[4])) {
      trans$psd[,4] <- NA
      trans$amp[,4] <- NA
      trans$spectra_old[4] <- F
      trans$title[5] <- NA
    } else {
      req(input$spectrum, input$spectrumFilter)
      periodogram("filter")
      if (input$tab == 1 || input$format == 4) {
        runjs("window.scrollTo(0,document.getElementById('lomb1').offsetTop);")
      } else if (input$tab == 2) {
        runjs("window.scrollTo(0,document.getElementById('lomb2').offsetTop);")
      } else if (input$tab == 3) {
        runjs("window.scrollTo(0,document.getElementById('lomb3').offsetTop);")
      }
    }
  })
  observeEvent(c(input$spectrumFilterRes), {
    req(db1[[info$db1]])
    if (isTruthy(trans$spectra_old[5])) {
      trans$psd[,5] <- NA
      trans$amp[,5] <- NA
      trans$spectra_old[5] <- F
      trans$title[6] <- NA
    } else {
      req(input$spectrum, input$spectrumFilterRes)
      periodogram("filterRes")
      if (input$tab == 1 || input$format == 4) {
        runjs("window.scrollTo(0,document.getElementById('lomb1').offsetTop);")
      } else if (input$tab == 2) {
        runjs("window.scrollTo(0,document.getElementById('lomb2').offsetTop);")
      } else if (input$tab == 3) {
        runjs("window.scrollTo(0,document.getElementById('lomb3').offsetTop);")
      }
    }
  })
  observeEvent(c(trans$y, trans$sy), {
    req(db1[[info$db1]], input$spectrum)
    if (input$spectrumOriginal) {
      periodogram("original")
    }
  })
  observeEvent(c(trans$res, trans$model), {
    req(db1[[info$db1]], input$spectrum)
    if (input$spectrumModel || input$spectrumResiduals) {
      periodogram(c("model","residuals"))
    }
  })
  observeEvent(c(trans$filter, trans$filterRes), {
    req(db1[[info$db1]], input$spectrum)
    if (input$spectrumFilter || input$spectrumFilterRes) {
      periodogram(c("filter","filterRes"))
    }
  })

  # Plot spectrum ####
  output$res1_espectral <- output$res2_espectral <- output$res3_espectral <- renderPlot({
    trans$slope <- NULL
    req(db1[[info$db1]], input$spectrum, trans$fs, trans$psd)
    removeNotification("no_noise_psd")
    if (length(trans$fs) > 0) {
      if (messages > 0) cat(file = stderr(), mySession, "Plotting periodogram", "\n")
      marks <- c(1 %o% 10^(-20:20))
      if (input$tunits == 1) {
        period <- "days"
      } else if (input$tunits == 2) {
        period <- "weeks"
      } else if (input$tunits == 3) {
        period <- "years"
      }
      if (input$sunits == 1) {
        units <- "(m)"
      } else if (input$sunits == 2) {
        units <- "(mm)"
      } else {
        units <- ""
      }
      if (input$spectrumType == 0) {
        spectrum_y <- trans$amp
        trans$spectra <- cbind(1/trans$fs, trans$amp[,!is.na(colSums(trans$amp))])
        ylab <- paste("Amplitude", units)
      } else if (input$spectrumType == 1) {
        spectrum_y <- trans$psd
        trans$spectra <- cbind(1/trans$fs, trans$psd[,!is.na(colSums(trans$psd))])
        ylab <- "Power"
      }
      title <- substring(paste(trans$title[!is.na(trans$title)],collapse = ""), 1, nchar(paste(trans$title[!is.na(trans$title)],collapse = "")) - 2)
      par(mar = c(5.1,4.1,6.1,2.1))
      if (is.null(ranges$x3)) {
        matplot(x = 1/trans$fs, y = spectrum_y, type = "l", lty = 1, lwd = 2, log = "xy", col = SARIcolors[trans$col], xlab = paste0("Period (",period,")"), ylab = ylab, yaxt = 'n', xlim = rev(range(1/trans$fs)), ylim = ranges$y3)
      } else {
        matplot(x = 1/trans$fs, y = spectrum_y, type = "l", lty = 1, lwd = 2, log = "xy", col = SARIcolors[trans$col], xlab = paste0("Period (",period,")"), ylab = ylab, yaxt = 'n', xlim = rev(ranges$x3), ylim = ranges$y3)
      }
      title(title, line = 5)
      axis(2, at = marks, labels = marks)
      if (input$tunits == 1) {
        newPeriods <- sprintf('%.*f', 3, axTicks(1)/daysInYear)
        lab = "Period (years)"
      } else if (input$tunits == 2) {
        newPeriods <- sprintf('%.*f', 1, axTicks(1)*7)
        lab = "Period (days)"
      } else if (input$tunits == 3) {
        newPeriods <- sprintf('%.*f', 1, axTicks(1)*daysInYear)
        lab = "Period (days)"
      }
      axis(3, at = axTicks(1), labels = newPeriods)
      mtext(lab, side = 3, line = 3)
      if (input$spectrumType == 1) {
        if (input$mle && length(trans$noise) > 0 && isTruthy(trans$noise) && (isTruthy(input$spectrumResiduals) || isTruthy(input$spectrumFilterRes))) {
          if (input$tunits == 1) { #days
            f_scale <- 24*60*60
          } else if (input$tunits == 2) { #weeks
            f_scale <- 7*24*60*60
          } else if (input$tunits == 3) { #years
            f_scale <- daysInYear*24*60*60
          }
          f_hz <- trans$fs/f_scale
          if (isTruthy(info$white)) {
            wn <- noise_var(trans$noise[1],0)
          } else {
            wn <- 0
          }
          pwn <- wn * f_hz^0
          psd <- pwn
          crossover <- NULL
          type_crossover <- NULL
          if (isTruthy(info$flicker) && isTruthy(trans$noise[3])) {
            fl <- noise_var(trans$noise[3],-1)
            pfl <- fl * f_hz^-1
            psd <- psd + pfl
            if (wn > 0) {
              crossover <- suppressWarnings(min(1/trans$fs[pfl > pwn]))
              type_crossover <- "Flicker / White"
            }
            if (isTruthy(info$randomw) && isTruthy(trans$noise[5])) {
              rw <- noise_var(trans$noise[5],-2)
              prw <- rw * f_hz^-2
              psd <- psd + prw
              crossover <- c(crossover, suppressWarnings(min(1/trans$fs[prw > pfl])))
              type_crossover <- c(type_crossover, "Random walk / Flicker")
            }
          } else if (isTruthy(info$randomw) && isTruthy(trans$noise[5])) {
            rw <- noise_var(trans$noise[5],-2)
            prw <- rw * f_hz^-2
            psd <- psd + prw
            if (wn > 0) {
              crossover <- suppressWarnings(min(1/trans$fs[prw > pwn]))
              type_crossover <- "Random walk / White"
            }
          } else if (isTruthy(info$powerl) && isTruthy(trans$noise[7])) {
            pl <- noise_var(trans$noise[7],trans$noise[9])
            ppl <- pl * f_hz^trans$noise[9]
            psd <- psd + ppl
            if (wn > 0) {
              crossover <- suppressWarnings(min(1/trans$fs[ppl > pwn]))
              type_crossover <- "Power-law / White"
            }
          }
          var <- NULL
          if (isTruthy(input$spectrumResiduals) && isTruthy(trans$res)) {
            var <- var(trans$res)
          } else if (isTruthy(input$spectrumFilterRes) && isTruthy(trans$filterRes)) {
            var <- var(trans$filterRes)
          }
          psd <- psd*var/sum(psd)
          lines(1/trans$fs,psd, col = SARIcolors[6], lty = 2, lwd = 3)
          output$crossover <- renderUI({
            if (!is.null(trans$noise) && length(crossover) > 0 && var > 0) {
              line <- sprintf("<br/>Crossover period %s = %.2f %s\n", type_crossover, crossover, period)
              HTML(line)
            } else {
              NULL
            }
          })
        } else {
          c <- max(which(trans$spectra_old))
          p <- trans$psd[,c]
          slope <- try(lm(log10(p) ~ log10(1/trans$fs)), silent = T)
          if (isTruthy(slope) && !inherits(slope,"try-error")) {
            slope$coef[2] <- -1*slope$coef[2]
            trans$slope <- slope$coef[2]
            regression <- 10^(predict(slope, newdata = list(x = 1/trans$fs)))
            lines(1/trans$fs, regression, col = SARIcolors[c], lwd = 3)
            text(inputs$long_period/2,min(p),paste0("Slope = ",sprintf("%4.2f",slope$coef[2])," +- ",sprintf("%3.2f",summary(slope)$coefficients[2,2])), col = SARIcolors[c])
          }
          lombx <- c(inputs$long_period,inputs$short_period)
          longest <- ifelse(length(p) > 200, as.integer(length(p)/100), 10)
          start <- median(head(p, n = longest))
          lomby_flicker <- c(start,start/(inputs$long_period/inputs$short_period))
          lines(lombx,lomby_flicker, col = SARIcolors[6], lty = 2, lwd = 3)
          text(inputs$long_period/10,min(p),"Slope = -1",col = SARIcolors[6])
        }
      }
      grid(nx = NULL, ny = NULL, col = SARIcolors[8], lty = "dashed", lwd = 1, equilogs = T)
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
      #
      if (isTruthy(debug)) {
        debugMem()
      }
    }
  }, width = reactive(info$width))

  # Plot wavelet ####
  output$wavelet1 <- output$wavelet2 <- output$wavelet3 <- renderPlot({
    trans$wavelet <- NULL
    req(db1[[info$db1]], input$wavelet, input$waveletType)
    removeNotification("no_wavelet")
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
      if (input$sunits == 1) {
        units <- "(m)"
      } else if (input$sunits == 2) {
        units <- "(mm)"
      } else {
        units <- ""
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
        if (info$points < 500) {
          num_epochs <- info$points
        } else {
          num_epochs <- 500
        }
        loc <- info$rangex/num_epochs
        if (loc < info$sampling) {
          loc <- info$sampling
        }
        updateTextInput(session, "min_wavelet", value = sprintf("%.*f", info$decimalsx, min_scale))
        updateTextInput(session, "max_wavelet", value = sprintf("%.*f", info$decimalsx, max_scale))
        updateTextInput(session, "res_wavelet", value = res)
        updateTextInput(session, "loc_wavelet", value = sprintf("%.*f", info$decimalsx, loc))
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
        showNotification(HTML("The period bounds and/or resolution are not valid to compute the wavelet transform.<br>Check the input values."), action = NULL, duration = 10, closeButton = T, id = "no_wavelet", type = "error", session = getDefaultReactiveDomain())
        updateRadioButtons(session, inputId = "waveletType", label = NULL, choices = list("None" = 0, "Original" = 1, "Model" = 2, "Model res." = 3, "Filter" = 4, "Filter res." = 5), selected = 0, inline = T, choiceNames = NULL,  choiceValues = NULL)
        req(info$stop)
      }
      if (messages > 0) cat(file = stderr(), mySession, "Computing wavelet", "\n")
      if (input$waveletType > 0) {
        start.time <- Sys.time()
        suppressWarnings({
          withProgress(message = 'Computing wavelet transform.',
                       detail = 'This may take a while ...', value = 0.1, {
                         trans$wavelet <- mvcwt(t*trans$x, y, scale.exp = 0.5, nscales = num_scale, min.scale = min_scale, max.scale = max_scale, loc = regularize(t*trans$x, nsteps = locs), wave.fun = "Morlet")
                         setProgress(0.7) # just for progress bar lovers
                         Sys.sleep(1)
                         setProgress(1)
                         Sys.sleep(1)
                       })
        })
        end.time <- Sys.time()
        time.taken <- difftime(end.time, start.time, units = "secs")
        if (messages > 2) cat(file = stderr(), mySession, start.time, end.time, "Total time =", time.taken, "s\n")
      }
      isolate({
        levels <- suppressWarnings(signif(sd(trans$wavelet$z), 4))
        z.fun <- match.fun("Mod")
        attr(trans$wavelet, 'class') <- 'list'
        pal = colorRampPalette(rev(brewer.pal(11, "RdYlBu")))(1024)
        magin_in <- par("mar")
        magin_out <- par("oma")
        par(mar = magin_in + c(0, 3, 0, 3), oma = magin_out + c(1, 0, 0, 0))
        s <- info$sampling*365.25*t/7
        amplitude_approx <- s*array(t(t(as.matrix(z.fun(trans$wavelet$z[,,1]), ncols = length(trans$wavelet$y), nrows = length(trans$wavelet$x)))*(pi/2)/sqrt(trans$wavelet$y)), dim = c(length(trans$wavelet$x),length(trans$wavelet$y),1))
        trans$wavelet$x <- trans$wavelet$x/t
        trans$wavelet$y <- trans$wavelet$y/t
        if (num_scale > 10) {
          image(trans$wavelet$x, trans$wavelet$y, amplitude_approx[,,1], col = pal, xlab = "", ylab = "", log = "y")
          image.plot(zlim = range(amplitude_approx[,,1]), legend.only = T, col = pal, legend.args = list(text = paste("Amplitude", units), cex = 1, side = 2, las = 2, line = 1), legend.shrink = 0.5, legend.width = 0.5, legend.mar = 2, horizontal = T)
        } else {
          image(trans$wavelet$x, trans$wavelet$y, z.fun(trans$wavelet$z[,,1]), col = pal, xlab = "", ylab = "")
          image.plot(zlim = range(z.fun(trans$wavelet$z[,,1])), legend.only = T, col = pal, legend.args = list(text = paste("Amplitude", units), cex = 1, side = 2, las = 2, line = 1), legend.shrink = 0.5, legend.width = 0.5, legend.mar = 2, horizontal = T)
        }
        box()
        mtext(paste0("Period (",period,")"), side = 2, line = 3, outer = F)
        contour(trans$wavelet$x, trans$wavelet$y, z.fun(trans$wavelet$z[,,1]), levels = c(levels, levels*2, levels*3), add = T, labcex = 1.1, drawlabels = F)
        title(main = title)
        coord <- unlist(as.list(which(amplitude_approx == max(amplitude_approx), arr.ind = T)))
        points(trans$wavelet$x[coord[1]], trans$wavelet$y[coord[2]], pch = "*", cex = 3, col = SARIcolors[1])
        lines(min(trans$wavelet$x) + trans$wavelet$y, trans$wavelet$y, lty = 2, lwd = 3, col = SARIcolors[1])
        lines(max(trans$wavelet$x) - trans$wavelet$y, trans$wavelet$y, lty = 2, lwd = 3, col = SARIcolors[1])
      })
      if (input$tab == 1 || input$format == 4) {
        runjs("window.scrollTo(0,document.getElementById('wl1').offsetTop);")
      } else if (input$tab == 2) {
        runjs("window.scrollTo(0,document.getElementById('wl2').offsetTop);")
      } else if (input$tab == 3) {
        runjs("window.scrollTo(0,document.getElementById('wl3').offsetTop);")
      }
      #
      if (isTruthy(debug)) {
        debugMem()
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
    #
    if (isTruthy(debug)) {
      debugMem()
    }
  }, width = reactive(info$width))

  # Compute smoother ####
  observeEvent(c(input$sigmas, inputs$low, inputs$high, input$filter, trans$y, input$series2filter, trans$res), {
    req(trans$x, trans$y, trans$sy, input$series2filter)
    removeNotification("no_smooth")
    removeNotification("same_periods")
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
          if (messages > 0) cat(file = stderr(), mySession, "Vondrak low", low, "\n")
          filter_low <- try(vondrak(trans$x, y, sy, as.numeric(low)), silent = F)
          if (!inherits(filter_low,"try-error") && !is.null(filter_low)) {
            trans$vondrak[1] <- low
          } else {
            showNotification(HTML("Unable to smooth the series.<br>Change the filter parameters"), action = NULL, duration = 10, closeButton = T, id = "no_smooth", type = "error", session = getDefaultReactiveDomain())
          }
        } else {
          trans$vondrak[1] <- NA
        }
        if (high > 0) {
          if (messages > 0) cat(file = stderr(), mySession, "Vondrak high", high, "\n")
          filter_high <- try(vondrak(trans$x, y, sy, as.numeric(high)), silent = F)
          if (!inherits(filter_high,"try-error") && !is.null(filter_high)) {
            trans$vondrak[2] <- high
          } else {
            showNotification(HTML("Unable to smooth the series.<br>Change the filter parameters"), action = NULL, duration = 10, closeButton = T, id = "no_smooth", type = "error", session = getDefaultReactiveDomain())
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
        showNotification(HTML("Low-pass and high-pass periods are equal.<br>Unable to smooth the series.<br>Change the smoother parameters"), action = NULL, duration = 10, closeButton = T, id = "same_periods", type = "error", session = getDefaultReactiveDomain())
        trans$filter <- NULL
        trans$filterRes <- NULL
      }
    } else {
      trans$filter <- NULL
      trans$filterRes <- NULL
    }
    #
    if (isTruthy(debug)) {
      debugMem()
    }
  }, priority = 1)

  # Plot smoother ####
  output$vondrak1 <- output$vondrak2 <- output$vondrak3 <- output$Vondrak1 <- output$Vondrak2 <- output$Vondrak3 <- renderPlot({
    req(db1[[info$db1]],input$filter)
    if (length(trans$filterRes) > 0) {
      if (messages > 0) cat(file = stderr(), mySession, "Plotting Vondrak", "\n")
      if ((length(inputs$low) > 0 && inputs$low > 0 && !is.na(inputs$low)) || (length(inputs$high) > 0 && inputs$high > 0 && !is.na(inputs$high))) {
        title <- "Filter residuals"
        sigmas <- F
        if (isTruthy(input$sigmas) && ((input$format == 4 && isTruthy(inputs$errorBar)) || input$format != 4)) {
          sigmas <- T
        }
        if (input$series2filter == 1) {
          plot_series(trans$x,trans$filterRes,trans$sy,ranges$x2,ranges$y2,sigmas,"",input$symbol,T)
        } else if (input$series2filter == 2 && length(trans$res) > 0) {
          if (input$fitType == 1) {
            plot_series(trans$x,trans$filterRes,trans$reserror,ranges$x2,ranges$y2,sigmas,"",input$symbol,T)
          } else if (input$fitType == 2) {
            plot_series(trans$x,trans$filterRes,trans$sy,ranges$x2,ranges$y2,sigmas,"",input$symbol,T)
          }
        }
        title(title, line = 3)
      }
    }
  }, width = reactive(info$width))

  # Noise analysis ####
  observeEvent(input$runmle, {
    removeNotification("no_mle")
    removeNotification("no_optim")
    removeNotification("too_long")
    removeNotification("timeWillTake")
    if (length(trans$res) > 0 || length(trans$filterRes) > 0) {
      trans$noise <- vector(length = 11)
      if (length(trans$res) > 0) {
        res <- trans$res
        resError <- trans$reserror
      } else if (length(trans$filterRes) > 0) {
        res <- unlist(as.list(trans$filterRes))
        resError <- trans$sy
      }
      req(res)
      vari <- var(res)
      n <- length(res)
      n_all <- length(trans$gaps)
      component <- 0
      convergence <- 1
      withBusyIndicatorServer("runmle", {
        cat(file = stderr(), mySession, "MLE fit", "\n")
        start.time <- Sys.time()
        if (messages > 2) cat(file = stderr(), mySession, "MLE fit start:", format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), "\n")
        message <- ""
        if (isTruthy(info$timeMLE)) {
          if (info$timeMLE < 60) {
            message <- paste("This should take less than 1 min")
          } else {
            message <- paste("This may take about", ceiling(info$timeMLE/60), "min")
          }
        }
        withProgress(message = 'Fitting the noise model.',
                     detail = message, value = 0, {
                       # build known covariance matrices
                       if (input$white) {
                         component <- component + 1
                         info$white <- T
                         Cwh <- diag(n)
                       } else {
                         info$white <- F
                       }
                       if (input$flicker) {
                         component <- component + 1
                         info$flicker <- T
                         Cfl <- cov_powerlaw(-1,n_all,F,trans$gaps,info$sampling)[[1]]
                       } else {
                         info$flicker <- F
                       }
                       if (input$randomw) {
                         component <- component + 1
                         info$randomw <- T
                         Crw <- cov_powerlaw(-2,n_all,F,trans$gaps,info$sampling)[[1]]
                       } else {
                         info$randomw <- F
                       }
                       if (input$powerl) {
                         component <- component + 2
                         info$powerl <- T
                         Cfl <- cov_powerlaw(-1,n_all,F,trans$gaps,info$sampling)[[1]]
                       } else {
                         info$powerl <- F
                       }
                       
                       # Noise optimization
                       CPL <- NULL # this and next two are updated inside loglik_global and used inside grad_global
                       Qinv <- NULL
                       QinvR <- NULL
                       fitmle <- NULL
                       apriori <- NULL
                       cl <- NULL
                       scaling <- 1/sd(res)
                       resS <- res*scaling
                       if (isTruthy(input$noise_unc)) {
                         hessian <- T
                       } else {
                         hessian <- F
                       }
                       
                       loglik_global <- function(x) {
                         h <- 0
                         k <- 0
                         CPL <<- NULL
                         Qinv <<- NULL
                         QinvR <<- NULL
                         if (white) {
                           h <- 1
                           wh <- exp(x[h])
                           Q <- wh * Cwh
                         } else {
                           Q <- matrix(0, ncol = n, nrow = n)
                         }
                         if (flicker) {
                           h <- h + 1
                           k <- -1
                           fl <- exp(x[h])
                           Q <- Q + fl * Cfl
                         }
                         if (randomw) {
                           h <- h + 1
                           k <- -2
                           rw <- exp(x[h])
                           Q <- Q + rw * Crw
                         }
                         if (powerl) {
                           h <- h + 1
                           pl <- exp(x[h])
                           h <- h + 1
                           k <- x[h] + 3
                           x <- head(x, -1)
                           CPL <<- cov_powerlaw(k,n_all,T,gaps,sampling)
                           Cpl <- CPL[[1]]
                           k_deriv <- CPL[[2]]
                           Q <- Q + pl * Cpl
                         }
                         ll_out <- loglikelihood(resS,Q,0)
                         ll <- ll_out[[1]]
                         Qinv <<- ll_out[[2]]
                         QinvR <<- ll_out[[3]]
                         if (method == "NLM") {
                           attr(ll, "gradient") <- grad_global(x)  
                         }
                         if (messages > 2) cat(file = stderr(), mySession, "Std Dev noises =", sqrt(exp(x))/scaling, " Index =", k, " loglik =", sprintf("%f",ll), "\n")
                         ll/-1
                       }
                       grad_global <- function(x) {
                         grad <- NULL
                         h <- 0
                         trQinv <- t(resS) %*% Qinv
                         if (white) {
                           h <- h + 1
                           wh <- exp(x[h])
                           grad <- -0.5*(tr(Qinv) - sum(dot(trQinv, QinvR)))[1]*wh
                         }
                         if (flicker) {
                           h <- h + 1
                           fl <- exp(x[h])
                           grad <- c(grad, -0.5*(sum(dot(Qinv,Cfl)) - sum(dot(trQinv, Cfl %*% QinvR)))[1]*fl)
                         }
                         if (randomw) {
                           h <- h + 1
                           rw <- exp(x[h])
                           grad <- c(grad, -0.5*(sum(dot(Qinv,Crw)) - sum(dot(trQinv, Crw %*% QinvR)))[1]*rw)
                         }
                         if (powerl) {
                           h <- h + 1
                           pl <- exp(x[h])
                           h <- h + 1
                           k <- x[h] + 3
                           x <- head(x, -1)
                           Cpl <- CPL[[1]]
                           k_deriv <- pl * CPL[[2]]
                           grad <- c(grad, -0.5*(sum(dot(Qinv,Cpl)) - sum(dot(trQinv, Cpl %*% QinvR)))[1]*pl)
                           grad <- c(grad, -0.5*(sum(dot(Qinv,k_deriv)) - sum(dot(trQinv, k_deriv %*% QinvR)))[1])
                         }
                         if (messages > 2) cat(file = stderr(), mySession, "Grads =", grad/-1, "\n")
                         grad/-1
                       }
                       
                       ##* one noise variance with fixed spectral index, easy peasy ####
                       if (component == 1) {
                         if (isTruthy(info$white)) {
                           variance <- var(resS)
                           VtPV <- variance*n
                           fitmle$value <- 0.5*(n*log(2*pi*variance) + n)
                         } else {
                           if (isTruthy(info$flicker)) {
                             C <- Cfl
                           }
                           if (isTruthy(info$randomw)) {
                             C <- Crw
                           }
                           setProgress(0.25)
                           VtPV <- crossprod(resS, solve(C, resS))
                           setProgress(0.75)
                           variance <- VtPV/n
                           fitmle$value <- 0.5*(n*log(2*pi*variance) + determinant(C)$modulus[[1]] + n)
                         }
                         fitmle$par <- log(variance)
                         fitmle$hessian <- (-n/(2*variance^2) + VtPV/variance^3) * variance
                         convergence <- 0
                       }
                       #* more than one noise variance ####
                       else if (component > 1) {
                         # computing a priori noise variances. RW is minimized to avoid having to much contribution in the final result (gradient of RW is usually very flat)
                         if (info$white) {
                           wh <- var(res)/sqrt(component)
                           apriori <- wh
                           typsize <- 3 # scale applied to the step size defined in the optimization run (which should be 1)
                           if (isTruthy(info$flicker)) {
                             fl <- (crossprod(res, solve(Cfl, res))/n)[1]/sqrt(component)
                             apriori <- c(apriori, fl)
                             typsize <- c(typsize, 3)
                             if (isTruthy(info$randomw)) {
                               rw <- (crossprod(res, solve(Crw, res))/n)[1]/sqrt(component)/1e3
                               apriori <- c(apriori, rw)
                               typsize <- c(typsize, 20)
                             }
                           } else if (isTruthy(info$randomw)) {
                             rw <- (crossprod(res, solve(Crw, res))/n)[1]/sqrt(component)/1e3
                             apriori <- c(apriori, rw)
                             typsize <- c(typsize, 20)
                           } else if (isTruthy(info$powerl)) {
                             pl <- (crossprod(res, solve(Cfl, res))/n)[1]/sqrt(component)
                             apriori <- c(apriori, pl)
                             typsize <- c(typsize, 3)
                           }
                         } else if (info$flicker) {
                           fl <- (crossprod(res, solve(Cfl, res))/n)[1]/sqrt(component)
                           apriori <- c(apriori, fl)
                           typsize <- 3
                           rw <- (crossprod(res, solve(Crw, res))/n)[1]/sqrt(component)/1e3
                           apriori <- c(apriori, rw)
                           typsize <- c(typsize, 20)
                         } else if (isTruthy(info$powerl)) {
                           pl <- (crossprod(res, solve(Cfl, res))/n)[1]/sqrt(component)
                           apriori <- c(apriori, pl)
                           typsize <- 3
                         }
                         setProgress(0.25)
                         apriori <- log(apriori*scaling^2)
                         upper <- apriori + log(3)
                         lower <- apriori - log(10)
                         if (isTruthy(info$powerl)) {
                           if (isTruthy(trans$slope) && trans$slope < 0 && trans$slope > -4) {
                             slope <- trans$slope - 3
                           } else {
                             slope <- -4
                           }
                           apriori <- c(apriori, slope) # a priori spectral index (= k - 3)
                           typsize <- c(typsize, 3)
                           upper <- c(upper, -3) # max expected spectral index (= k - 3)
                           lower <- c(lower, -7) # min expected spectral index (= k - 3)
                         }
                         # creating non reactive variables for running on a cluster
                         white <- info$white
                         flicker <- info$flicker
                         randomw <- info$randomw
                         powerl <- info$powerl
                         gaps <- trans$gaps
                         sampling <- info$sampling
                         tryOptim <- try({
                           # setting cluster if run is local and long
                           # if (info$local && info$timeMLE > 60) {
                           #   cl <- makeCluster(detectCores() - 1, type = "FORK")
                           #   setDefaultCluster(cl = cl)
                           # }
                           if (isTruthy(cl)) {
                             # BFGS quasi-Newton method with box (actually upper only) constraints, Byrd et. al. (1995)
                             method <- "L-BFGS-B"
                             fitmle <- optimParallel(par = apriori,
                                                     fn = loglik_global,
                                                     gr = grad_global,
                                                     lower = lower,
                                                     upper = upper,
                                                     method = "L-BFGS-B",
                                                     hessian = hessian,
                                                     control = list(fnscale = 1, pgtol = 1e1, factr = 1e11)
                             )
                             setDefaultCluster(cl = NULL)
                             stopCluster(cl)
                           } else { # standard 1 proc run
                             # BFGS quasi-Newton method with box (actually upper only) constraints, Byrd et. al. (1995)
                             # method <- "L-BFGS-B"
                             # fitmle <- optim(par = apriori,
                             #                 fn = loglik_global,
                             #                 gr = grad_global,
                             #                 lower = lower,
                             #                 upper = upper,
                             #                 method = "L-BFGS-B",
                             #                 hessian = hessian,
                             #                 control = list(fnscale = 1, pgtol = 1e1, factr = 1e13)
                             # )
                             
                             # if (isTruthy(info$powerl)) { # does not converge for < -2 slopes, using the NLM method for now
                             # Nelder and Mead (1965) method: provides better likelihoods, but can be slow as duck if the a priori values are not good!
                             # method <- "Nelder & Mead"
                             # fitmle <- optim(par = apriori,
                             #                 fn = loglik_global,
                             #                 hessian = hessian,
                             #                 control = list(fnscale = 1, reltol = 1e-2)
                             # )
                             
                             # } else {
                             # Quasi-Newton method as in optim, but seems to run faster
                             method <- "NLM"
                             fitmle <- nlm(loglik_global, apriori, hessian = hessian, typsize = typsize,
                                           fscale = 1, print.level = 0, ndigit = 1, gradtol = 1e-2,
                                           stepmax = 1e0, steptol = 1e-2, iterlim = 100, check.analyticals = F
                             )
                             setProgress(0.75)
                             if (fitmle$code < 4) { # transforming nlm results into optim format
                               fitmle$convergence <- 0
                               fitmle$value <- fitmle$minimum
                               fitmle$par <- fitmle$estimate
                             }
                             # }
                             
                             if (fitmle$convergence == 0) {
                               convergence <- 0
                             }
                           }
                         }, silent = T)
                         if (inherits(tryOptim,"try-error") || tryOptim == "") {
                           showNotification(HTML("Something went wrong with the MLE optimization.<br>The tested model parameters are probably out of bounds."), action = NULL, duration = 10, closeButton = T, id = "no_optim", type = "error", session = getDefaultReactiveDomain())
                           req(info$stop)
                         }
                       }
                       Sys.sleep(1)
                       setProgress(1)
                     })
        
        ##* convergence ####
        if (!is.na(convergence)) {
          if (convergence == 0) {
            if (input$wiener) {
              kk <- loglik_global(fitmle$estimate) # updating noise covariance matrix from best noise estimates
            }
            trans$mle <- 1
            sd_noises <- NA
            line3 <- NULL
            if (input$noise_unc) {
              sd_noises <- suppressWarnings(sqrt(diag(solve(fitmle$hessian))))
            }
            i <- 0
            sigmaFL <- NULL
            sigmaPL <- NULL
            sigmaRW <- NULL
            sigmaK <- NULL
            if (input$sunits == 1) {
              unit <- "m"
            } else if (input$sunits == 2) {
              unit <- "mm"
            } else {
              unit <- ""
            }
            if (isTruthy(info$white)) {
              i <- i + 1
              sigmaWH <- sqrt(exp(fitmle$par[i]))/scaling
              trans$noise[1] <- sigmaWH
              if (input$noise_unc) {
                seParmsWH <- sd_noises[i]/scaling
                trans$noise[2] <- seParmsWH
                if (isTruthy(sigmaWH) && isTruthy(seParmsWH) && sigmaWH > seParmsWH * 3) {
                  updateTextInput(session, inputId = "verif_white", value = sigmaWH)
                } else {
                  updateTextInput(session, inputId = "verif_white", value = "0")
                }
              } else {
                updateTextInput(session, inputId = "verif_white", value = sigmaWH)
              }
              if (input$wiener) {
                qQ <- (exp(fitmle$par[i])*Cwh) %*% Qinv
                trans$white <- unlist(as.list(res %*% qQ))
                if (input$sigmas && length(resError) > 0) {
                  trans$white_sig <- unlist(as.list(sqrt(diag(qQ %*% diag(resError^2) %*% t(qQ)))))
                }
              }
              output$est.white <- renderUI({
                line1 <- "White noise:"
                line2 <- formatting(sigmaWH,1)
                if (input$noise_unc) {
                  line3 <- paste("+/-", formatting(seParmsWH,1))
                }
                HTML(paste(line1,'<br/>',line2,unit,'<br/>',line3))
              })
            } else {
              updateTextInput(session, inputId = "verif_white", value = "0")
              trans$noise[1] <- NA
              trans$noise[2] <- NA
            }
            if (isTruthy(info$flicker)) {
              i <- i + 1
              sigmaFL <- sqrt(exp(fitmle$par[i]))/scaling
              trans$noise[3] <- sigmaFL
              if (input$noise_unc) {
                seParmsFL <- sd_noises[i]/scaling
                trans$noise[4] <- seParmsFL
                if (isTruthy(sigmaFL) && isTruthy(seParmsFL) && sigmaFL > seParmsFL * 3) {
                  updateTextInput(session, inputId = "verif_fl", value = sigmaFL)
                } else {
                  updateTextInput(session, inputId = "verif_fl", value = "0")
                }
              } else {
                updateTextInput(session, inputId = "verif_fl", value = sigmaFL)
              }
              if (input$wiener) {
                qQ <- (exp(fitmle$par[i])*Cfl) %*% Qinv
                trans$flicker <- unlist(as.list(res %*% qQ))
                if (input$sigmas && length(resError) > 0) {
                  trans$flicker_sig <- unlist(as.list(sqrt(diag(qQ %*% diag(resError^2) %*% t(qQ)))))
                }
              }
              output$est.flicker <- renderUI({
                line1 <- "Flicker noise:"
                line2 <- formatting(sigmaFL,1)
                if (input$noise_unc) {
                  line3 <- paste("+/-", formatting(seParmsFL,1))
                }
                HTML(paste(line1,'<br/>',line2, unit, '<br/>',line3))
              })
              updateRadioButtons(session, inputId = "typeColor", selected = 1)
            } else {
              updateTextInput(session, inputId = "verif_fl", value = "0")
              trans$noise[3] <- NA
              trans$noise[4] <- NA
            }
            if (isTruthy(info$randomw)) {
              i <- i + 1
              sigmaRW <- sqrt(exp(fitmle$par[i]))/scaling
              trans$noise[5] <- sigmaRW
              if (input$noise_unc) {
                seParmsRW <- sd_noises[i]/scaling
                trans$noise[6] <- seParmsRW
                if (isTruthy(sigmaRW) && isTruthy(seParmsRW) && sigmaRW > seParmsRW * 3) {
                  updateTextInput(session, inputId = "verif_rw", value = sigmaRW)
                } else {
                  updateTextInput(session, inputId = "verif_rw", value = "0")
                }
              } else {
                updateTextInput(session, inputId = "verif_rw", value = sigmaRW)
              }
              if (input$wiener) {
                qQ <- (exp(fitmle$par[i])*Crw) %*% Qinv
                trans$randomw <- unlist(as.list(res %*% qQ))
                if (input$sigmas && length(resError) > 0) {
                  trans$randomw_sig <- unlist(as.list(sqrt(diag(qQ %*% diag(resError^2) %*% t(qQ)))))
                }
              }
              output$est.randomw <- renderUI({
                line1 <- "Random walk:"
                line2 <- formatting(sigmaRW,1)
                if (input$noise_unc) {
                  line3 <- paste("+/-", formatting(seParmsRW,1))
                }
                HTML(paste(line1,'<br/>',line2, unit, '<br/>',line3))
              })
              updateRadioButtons(session, inputId = "typeColor", selected = 1)
            } else {
              updateTextInput(session, inputId = "verif_rw", value = "0")
              trans$noise[5] <- NA
              trans$noise[6] <- NA
            }
            if (isTruthy(info$powerl)) {
              i <- i + 1
              sigmaPL <- sqrt(exp(fitmle$par[i]))/scaling
              trans$noise[7] <- sigmaPL
              if (input$noise_unc) {
                seParmsPL <- sd_noises[i]/scaling
                trans$noise[8] <- seParmsPL
                if (isTruthy(sigmaPL) && isTruthy(seParmsPL) && sigmaPL > seParmsPL * 3) {
                  updateTextInput(session, inputId = "verif_pl", value = sigmaPL)
                } else {
                  updateTextInput(session, inputId = "verif_pl", value = "0")
                }
              } else {
                updateTextInput(session, inputId = "verif_pl", value = sigmaPL)
              }
              if (input$wiener) {
                qQ <- (exp(fitmle$par[i])*CPL[[1]]) %*% Qinv
                trans$powerl <- unlist(as.list(res %*% qQ))
                if (input$sigmas && length(resError) > 0) {
                  trans$powerl_sig <- unlist(as.list(sqrt(diag(qQ %*% diag(resError^2) %*% t(qQ)))))
                }
              }
              output$est.powerl <- renderUI({
                line1 <- "Power-law:"
                line2 <- formatting(sigmaPL,1)
                if (input$noise_unc) {
                  line3 <- paste("+/-", formatting(seParmsPL,1))
                }
                HTML(paste(line1,'<br/>',line2, unit, '<br/>',line3))
              })
              updateRadioButtons(session, inputId = "typeColor", selected = 2)
              i <- i + 1
              sigmaK <- fitmle$par[i] + 3
              trans$noise[9] <- sigmaK
              if (input$noise_unc) {
                seParmsK <- sd_noises[i]
                trans$noise[10] <- seParmsK
                if (isTruthy(sigmaK) && isTruthy(seParmsK) && abs(sigmaK) > seParmsK * 3) {
                  updateTextInput(session, inputId = "verif_k", value = sigmaK)
                } else {
                  updateTextInput(session, inputId = "verif_k", value = "0")
                }
              } else {
                updateTextInput(session, inputId = "verif_k", value = sigmaK)
              }
              output$est.index <- renderUI({
                line1 <- "Spectral index:"
                line2 <- format(sigmaK, nsmall = 3, digits = 0, trim = F, scientific = F)
                if (input$noise_unc) {
                  if (isTruthy(seParmsK) && seParmsK <= 0.0005) {
                    seParmsK <- 0.001
                  }
                  line3 <- paste("+/-", format(seParmsK, nsmall = 3, digits = 0, trim = F, scientific = F))
                }
                HTML(paste(line1,'<br/>',line2, '<br/>',line3))
              })
            } else {
              updateTextInput(session, inputId = "verif_pl", value = "0")
              updateTextInput(session, inputId = "verif_k", value = "0")
              trans$noise[7] <- NA
              trans$noise[8] <- NA
              trans$noise[9] <- NA
              trans$noise[10] <- NA
            }
            end.time <- Sys.time()
            if (messages > 2) cat(file = stderr(), mySession, "MLE fit end:", format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), "\n")
            time.taken <- end.time - start.time
            if (messages > 2) cat(file = stderr(), mySession, "Total time =", time.taken, units(time.taken), "\n")
            if (isTruthy(info$white) || isTruthy(info$flicker) || isTruthy(info$randomw) || isTruthy(info$powerl)) {
              if (length(fitmle$value) > 0) {
                trans$noise[11] <- fitmle$value
              } else {
                trans$noise[11] <- NA
              }
              output$est.mle <- renderUI({
                if (isTruthy(trans$mle)) {
                  line1 <- sprintf("<br/>log-Likelihood = %.4f",fitmle$value/-1)
                  HTML(line1)
                } else {
                  NULL
                }
              })
            }
            output$est.unc <- renderUI({
              if ("Linear" %in% input$model && input$fitType == 1) {
                if (isTruthy(trans$mle)) {
                  unc_pl <- 0
                  if (isTruthy(sigmaFL)) {
                    unc_pl <- pl_trend_unc(sigmaFL,-1,info$sampling) # general power-law trend uncertainty
                    # unc_pl <- sqrt( 1.78 * sigmaFL^2 * info$sampling^0.22 / ((info$points - 1) * info$sampling)^2 ) # from Mao et al. (1999)
                    # unc_pl <- sqrt( 9 * sigmaFL^2*samplingScale^(-1/4) / (16 * (info$sampling)^2 * (info$points^2 - 1)) ) # from Zhang et al. (1997)
                  }
                  if (isTruthy(sigmaRW)) {
                    unc <- pl_trend_unc(sigmaRW,-2,info$sampling) # general power-law trend uncertainty
                    # unc <- sqrt( sigmaRW^2*samplingScale^(-2/4) / (info$points - 1) ) # from Zhang et al. (1997)
                    unc_pl <- sqrt(unc_pl^2 + unc^2)
                  }
                  if (isTruthy(sigmaPL)) {
                    # unc <- pl_trend_unc(sigmaPL*samplingScale^(sigmaK/4),sigmaK,info$sampling) # general power-law trend uncertainty
                    unc <- pl_trend_unc(sigmaPL,sigmaK,info$sampling) # general power-law trend uncertainty
                    unc_pl <- sqrt(unc_pl^2 + unc^2)
                  }
                  unc_white <- sqrt( 12 * sd(res)^2 / (info$points * ((info$points - 1) * info$sampling)^2) )
                  if (isTruthy(trans$unc)) {
                    if (isTruthy(unc_pl) && unc_pl > 0) {
                      trans$LScoefs[2,2] <- sqrt(unc_pl^2 + trans$unc^2)
                      trans$results$coefficients[2,2] <- sqrt(unc_pl^2 + trans$unc^2)
                      trans$results$coefficients[2,3] <- abs(trans$results$coefficients[2,1]) / trans$results$coefficients[2,2]
                      trans$results$coefficients[2,4] <- 2 * pt(abs(trans$results$coefficients[2,3]), trans$results$df , lower.tail = F)[2]
                      line1 <- sprintf("<br/>Colored/white rate error ratio = %.2f", unc_pl/unc_white)
                      HTML(line1)
                    } else {
                      NULL
                    }
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
                      trans$results$coefficients[2,3] <- abs(trans$results$coefficients[2,1]) / trans$results$coefficients[2,2]
                      trans$results$coefficients[2,4] <- 2 * pt(abs(trans$results$coefficients[2,3]), trans$results$df , lower.tail = F)[2]
                    }
                  }
                  NULL
                }
              }
            })
          } else {
            trans$mle <- NULL
            showNotification(HTML("The MLE optimization did not converge.<br>The tested model parameters are probably out of bounds."), action = NULL, duration = 10, closeButton = T, id = "no_mle", type = "error", session = getDefaultReactiveDomain())
          }
        }
      })
    }
    #
    if (isTruthy(debug)) {
      debugMem()
    }
  })
  
  # Search offsets ####
  observeEvent(input$search, {
    req(db1$original)
    removeNotification("bad_search")
    removeNotification("no_search")
    if (length(trans$res) > 0) {
      if (messages > 0) cat(file = stderr(), mySession, "Searching offsets", "\n")
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
        showNotification(HTML("The segment size is too small for the series sampling.<br>Consider using a larger segment size."), action = NULL, duration = 10, closeButton = T, id = "bad_search", type = "error", session = getDefaultReactiveDomain())
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
        withProgress(message = 'Searching discontinuities.',
                     detail = 'This may take a while ...', value = 0.1, {
                       breaks <- breakpoints(y ~ ylag1, data = extended_series, h = segment)
                       setProgress(0.7) # just for progress bar lovers
                       Sys.sleep(1)
                       setProgress(1)
                       Sys.sleep(1)
                     })
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
    #
    if (isTruthy(debug)) {
      debugMem()
    }
  })

  # Download results ####
  ## Local directory ####
  output$localDirectory <- renderPrint({
    if (isTruthy(info$directory)) {
      cat(info$directory)
    }
  })
  observeEvent(input$autoDownload, {
    removeNotification("no_directory")
    withBusyIndicatorServer("autoDownload", {
      if (isTruthy(info$directory)) {
        if (input$format != 4) {
          if (any(grepl("East", info$components))) {
            file_out <- paste0(info$directory, "\\", file$primary$name, "_", strsplit(info$components[as.numeric(input$tab)], " ")[[1]][1], ".sari")
          } else {
            file_out <- paste0(info$directory, "\\", file$primary$name, "_", input$tab, ".sari") 
          }
        } else {
          file_out <- paste0(info$directory, "\\", file$primary$name, ".sari")
        }
        collect(file_out)
      } else {
        showNotification(HTML("Download directory not found.<br>File download skipped."), action = NULL, duration = 10, closeButton = T, id = "no_directory", type = "error", session = getDefaultReactiveDomain())
      }
    })
  })
  ## Save series ####
  output$download <- output$downloadAs <- downloadHandler(
    filename = function() {
      if (input$format != 4) {
        if (any(grepl("East", info$components))) {
          paste0(file$primary$name, "_", strsplit(info$components[as.numeric(input$tab)], " ")[[1]][1], ".sari")
        } else {
          paste0(file$primary$name, "_", input$tab, ".sari")
        }
      } else {
        paste0(file$primary$name, ".sari")
      }
    },
    content = function(file) {
      collect(file)
    }
  )
  ## Save spectrum ####
  output$downloadSpectrum1 <- output$downloadSpectrum2 <- output$downloadSpectrum3 <- downloadHandler(
    filename = function() {
      if (input$format != 4) {
        if (any(grepl("East", info$components))) {
          paste0(file$primary$name, "_", strsplit(info$components[as.numeric(input$tab)], " ")[[1]][1], ".periodogram.sari")
        } else {
          paste0(file$primary$name, "_", input$tab, ".periodogram.sari")
        }
      } else {
        paste0(file$primary$name, ".periodogram.sari")
      }
    },
    content = function(file) {
      collect_periodogram(file)
    }
  )
  ## Save help file ####
  #Based on https://stackoverflow.com/questions/40420450/how-to-download-a-pdf-file-in-a-shiny-app
  output$print_out <- downloadHandler(
    filename = function() {
      paste0(gsub(" ", "_", version),".pdf")
    },
    content = function(file) {
      if (messages > 0) cat(file = stderr(), mySession, "Downloading help file", "\n")
      file.copy("www/about.pdf", file)
    }
  )

  # Control plots ####
  output$header <- renderText({
    req(input$header, file$primary)
    if (length(trans$x) > 0 && input$format < 4) {
      if (input$sigmas) {
        header <- data.frame(x = sprintf("%.*f", info$decimalsx, trans$x), y = sprintf("%.*f", info$decimalsy, trans$y), sy = sprintf("%.*f", info$decimalsy, trans$sy))[1:input$lines,]
      } else {
        header <- data.frame(x = sprintf("%.*f", info$decimalsx, trans$x), y = sprintf("%.*f", info$decimalsy, trans$y))[1:input$lines,]
      }
      tmpfile <- tempfile()
      write.table(header, file = tmpfile, append = F, sep = "\t", quote = F, na = "NA", row.names = F, col.names = F)
      noquote(paste(readLines(con = tmpfile, ok = T, warn = F, skipNul = F, encoding = "UTF8"), collapse = "\n"))
    } else {
      noquote(paste(readLines(con = file$primary$datapath, n = input$lines, ok = T, warn = F, skipNul = F, encoding = "UTF8"), collapse = "\n"))
    }
  })
  observeEvent(input$remove3D, {
    req(db1$original)
    if (messages > 0) cat(file = stderr(), mySession, "Removing from all series is", input$remove3D, "\n")
  })
  observeEvent(input$permanent, {
    req(input$permanent)
    if (messages > 0) cat(file = stderr(), mySession, "Deleting next points\n")
  })
  observeEvent(input$plot_2click, {
    req(db1[[info$db1]])
    if (input$tab == 1) {
      values_now <- db1[[info$db1]]$status1
    } else if (input$tab == 2) {
      values_now <- db1[[info$db1]]$status2
    } else if (input$tab == 3) {
      values_now <- db1[[info$db1]]$status3
    }
    brush <- input$plot_brush
    if (!is.null(brush) && isTruthy(trans$y0[trans$x0 >= brush$xmin & trans$x0 <= brush$xmax]) && !all(is.na(values_now[trans$x0[!is.na(trans$y0)] > brush$xmin & trans$x0[!is.na(trans$y0)] < brush$xmax]))) {
      ranges$x1 <- c(brush$xmin, brush$xmax)
      ids <- trans$x >= ranges$x1[1] & trans$x <= ranges$x1[2]
      if (length(trans$y[ids & trans$y >= brush$ymin & trans$y <= brush$ymax]) > 0) {
        ranges$y1 <- c(brush$ymin, brush$ymax) 
      } else {
        ranges$y1 <- range(trans$y[ids], na.rm = T)
        if (any(is.na(ranges$y1)) || any(is.infinite(ranges$y1))) {
          ranges$y1 <- range(trans$y, na.rm = T)
        }
      }
      if (length(file$secondary) > 0 && input$optionSecondary == 1 && any(!is.na(trans$y2))) {
        ids <- trans$x2 >= ranges$x1[1] & trans$x2 <= ranges$x1[2]
        if (sum(ids) > 0) {
          ranges$y12 <- range(trans$y2[ids], na.rm = T)
          if (any(is.na(ranges$y12)) || any(is.infinite(ranges$y12))) {
            ranges$y12 <- range(trans$y2, na.rm = T)
          }
        } else {
          ranges$y12 <- range(trans$y2, na.rm = T)
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
  observeEvent(c(input$plot41_2click, input$plot42_2click, input$plot43_2click), {
    brush <- NULL
    if (length(input$plot41_brush) > 0) {
      brush <- input$plot41_brush
    } else if (length(input$plot42_brush) > 0) {
      brush <- input$plot42_brush
    } else if (length(input$plot43_brush) > 0) {
      brush <- input$plot43_brush
    }
    values_now <- db1[[info$db1]]$status1
    if (!is.null(brush) && !all(is.na(values_now[trans$x0 > brush$xmin & trans$x0 < brush$xmax]))) {
      ranges$x1 <- ranges$x2 <- ranges$x4 <- c(brush$xmin, brush$xmax)
      ranges$y1 <- ranges$y2 <- NULL
    } else {
      ranges$x1 <- c(info$minx, info$maxx)
      ranges$y1 <- NULL
      ranges$y12 <- NULL
      ranges$x2 <- NULL
      ranges$y2 <- NULL
      ranges$x4 <- NULL
      ranges$y4 <- NULL
    }
  })
  observeEvent(input$res_2click, {
    req(db1[[info$db1]])
    brush <- NULL
    if (isTruthy(input$res_brush)) {
      brush <- input$res_brush
      res <- trans$res
    } else if (isTruthy(input$vondrak_brush)) {
      brush <- input$vondrak_brush
      res <- trans$filterRes
    }
    if (input$tab == 1) {
      values_now <- db1[[info$db1]]$status1
    } else if (input$tab == 2) {
      values_now <- db1[[info$db1]]$status2
    } else if (input$tab == 3) {
      values_now <- db1[[info$db1]]$status3
    }
    if (!is.null(brush) && isTruthy(trans$y0[trans$x0 > brush$xmin & trans$x0 < brush$xmax]) && !all(is.na(values_now[trans$x0[!is.na(trans$y0)] > brush$xmin & trans$x0[!is.na(trans$y0)] < brush$xmax]))) {
      ranges$x2 <- c(brush$xmin, brush$xmax)
      ids <- trans$x >= ranges$x2[1] & trans$x <= ranges$x2[2]
      if (sum(res[ids & res >= brush$ymin & res <= brush$ymax]) > 0) {
        ranges$y2 <- c(brush$ymin, brush$ymax) 
      } else {
        ranges$y2 <- range(res[ids], na.rm = T)
        if (any(is.na(ranges$y2)) || any(is.infinite(ranges$y2))) {
          ranges$y2 <- range(res, na.rm = T)
        }
      }
      ranges$x1 <- ranges$x4 <- ranges$x2
      ids <- trans$x0 > ranges$x1[1] & trans$x0 < ranges$x1[2]
      ranges$y1 <- range(trans$y0[ids], na.rm = T)
      if (length(file$secondary) > 0 && input$optionSecondary == 1 && any(!is.na(trans$y2))) {
        ids <- trans$x2 >= ranges$x1[1] & trans$x2 <= ranges$x1[2]
        if (sum(ids) > 0) {
          ranges$y12 <- range(trans$y2[ids], na.rm = T)
          if (any(is.na(ranges$y12)) || any(is.infinite(ranges$y12))) {
            ranges$y12 <- range(trans$y2, na.rm = T)
          }
        } else {
          ranges$y12 <- range(trans$y2, na.rm = T)
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
    req(db1[[info$db1]])
    brush <- input$lomb_brush
    if (!is.null(brush) && length(1/trans$fs[1/trans$fs >= brush$xmin & 1/trans$fs <= brush$xmax]) > 0) {
      ranges$x3 <- c(brush$xmin, brush$xmax)
      ranges$y3 <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x3 <- NULL
      ranges$y3 <- NULL
    }
  })
  observeEvent(input$rate_2click, {
    req(db1[[info$db1]])
    brush <- input$rate_brush
    if (!is.null(brush)) {
      ranges$x4 <- c(brush$xmin, brush$xmax)
      ranges$y4 <- c(brush$ymin, brush$ymax)
      ranges$x1 <- ranges$x2 <- ranges$x4
      ids <- trans$x > ranges$x1[1] & trans$x < ranges$x1[2]
      ranges$y1 <- range(trans$y[ids])
      if (length(file$secondary) > 0 && input$optionSecondary == 1 && any(!is.na(trans$y2))) {
        ids <- trans$x0 >= ranges$x1[1] & trans$x0 <= ranges$x1[2]
        if (sum(ids) > 0) {
          ranges$y12 <- range(trans$y2[ids], na.rm = T)
          if (any(is.na(ranges$y12)) || any(is.infinite(ranges$y12))) {
            ranges$y12 <- range(trans$y2, na.rm = T)
          }
        } else {
          ranges$y12 <- range(trans$y2, na.rm = T)
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
    if (input$tab == "6") {
      disable("fitType")
      disable("autoDownload")
      disable("white")
      disable("reset")
      disable("cut")
      disable("delete_excluded")
      disable("loadSARI")
      disable("ids")
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
      disable("spectrumResiduals")
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
      disable("tunits")
      disable("sunits")
      disable("histogram")
      disable("histogramType")
      disable("log")
      disable("average")
      disable("midas")
      disable("entropy")
      disable("mle")
      disable("wiener")
      disable("model")
      disable("wavelet")
      disable("waveletType")
      disable("optionSecondary")
      disable("waveform")
      disable("plot")
      disable("plotAll")
      disable("overflow")
      disable("add_excluded")
      disable("permanent")
      disable("cut")
      disable("powerl")
      disable("randomw")
      disable("runVerif")
      disable("runmle")
      disable("noise_unc")
      disable("series2")
      disable("separator")
      disable("separator2")
      disable("ne")
      disable("series2filter")
      disable("typeSecondary")
      disable("fullSeries")
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
      disable("gia")
      disable("server1")
      disable("station1")
      disable("product1")
      disable("server2")
      disable("station2")
      disable("product2")
      disable("swap")
      disable("step")
      disable("step2")
      disable("scaleFactor")
    } else {
      if (input$tab == 4) {
        runjs("document.getElementsByClassName('panel-primary')[3].classList.add('hidden');")
        runjs("document.getElementsByClassName('panel-primary')[4].classList.add('hidden');")
      } else {
        runjs("document.getElementsByClassName('panel-primary')[3].classList.remove('hidden');")
        runjs("document.getElementsByClassName('panel-primary')[4].classList.remove('hidden');")
      }
      if (length(file$primary) > 0) {
        info$menu <- unique(c(info$menu, 2))
        updateCollapse(session, id = "menu", open = info$menu)
        disable("server1")
        disable("station1")
        disable("product1")
        enable("reset")
        enable("ids")
        enable("symbol")
        enable("header")
        enable("separator")
        enable("format")
        enable("tunits")
        enable("sunits")
        enable("units")
        enable("plot")
        if (isTruthy(info$errorbars)) {
          enable("sigmas")
        } else {
          updateCheckboxInput(session, inputId = "sigmas", value = F)
          disable("sigmas")
        }
        if (input$sigmas == T) {
          enable("errorBar")
        } else {
          disable("errorBar")
        }
        if (isTruthy(db1[[info$db1]])) {
          disable("plot")
          disable("series")
          disable("format")
          disable("separator")
          enable("overflow")
          enable("cut")
          enable("log")
          enable("sinfo")
          enable("soln")
          enable("custom")
          enable("printLog")
          enable("printSinfo")
          enable("printSoln")
          enable("printCustom")
          enable("traceLog")
          enable("traceSinfo")
          enable("traceSoln")
          enable("traceCustom")
          enable("permanent")
          enable("cut")
          enable("loadSARI")
          enable("midas")
          enable("entropy")
          enable("reset")
          enable("average")
          if (!isTruthy(input$average) && length(inputs$step) > 0) {
            updateTextInput(session, inputId = "step", value = "")
          }
          enable("gia")
          if (!isTruthy(input$gia)) {
            updateRadioButtons(session, inputId = "giaType", selected = 0)
            updateTextInput(session, inputId = "giaTrend", value = "")
            updateSelectInput(session, inputId = "giaModel", selected = "")
            disable("giaType")
          } else {
            if (!is.na(inputs$giaTrend)) {
              enable("giaType")
            } else {
              disable("giaType")
            }
          }
          enable("euler")
          if (!isTruthy(input$euler)) {
            enable("neuenu")
            updateRadioButtons(session, inputId = "eulerType", selected = 0)
            updateTextInput(inputId = "plate", value = "")
          } else {
            disable("neuenu")
            if ((((isTruthy(inputs$station_x) && isTruthy(inputs$station_y) && isTruthy(inputs$station_z)) || (isTruthy(inputs$station_lat) && isTruthy(inputs$station_lon))) ||
                 ((isTruthy(inputs$station_x2) && isTruthy(inputs$station_y2) && isTruthy(inputs$station_z2)) || (isTruthy(inputs$station_lat2) && isTruthy(inputs$station_lon2)))) &&
                ((isTruthy(inputs$pole_x) && isTruthy(inputs$pole_y) && isTruthy(inputs$pole_z)) || (isTruthy(inputs$pole_lat) && isTruthy(inputs$pole_lon) && isTruthy(inputs$pole_rot)))) {
              enable("eulerType")
            } else {
              disable("eulerType")
            }
          }
          if (input$format == 4) {
            disable("plotAll")
          } else {
            enable("plotAll")
          }
          if (length(input$plot_brush) > 0 || length(input$res_brush) > 0 || length(input$vondrak_brush) > 0 ||
              length(input$plot41_brush) > 0 || length(input$plot42_brush) > 0 || length(input$plot43_brush) > 0) {
            enable("remove")
          } else {
            disable("remove")
          }
          if (any(c(db1[[info$db1]]$status1, db1[[info$db1]]$status2, db1[[info$db1]]$status3) == F)) {
            enable("add_excluded")
            enable("delete_excluded")
          } else {
            disable("add_excluded")
            disable("delete_excluded")
          }
          if (sum(db1[[info$db1]]$status1, na.rm = T) == sum(db1[[info$db1]]$status2, na.rm = T) && sum(db1[[info$db1]]$status1, na.rm = T) == sum(db1[[info$db1]]$status3, na.rm = T)) {
            enable("remove3D")
          } else {
            disable("remove3D")
          }
          enable("histogram")
          enable("histogramType")
          enable("filter")
          enable("series2filter")
          enable("spectrum")
          enable("spectrumType")
          enable("spectrumOriginal")
          if (!isTruthy(input$spectrum)) {
            updateCheckboxInput(session, inputId = "spectrumOriginal", value = F)
            updateCheckboxInput(session, inputId = "spectrumResiduals", value = F)
            updateCheckboxInput(session, inputId = "spectrumModel", value = F)
            updateCheckboxInput(session, inputId = "spectrumFilter", value = F)
            updateCheckboxInput(session, inputId = "spectrumFilterRes", value = F)
            updateTextInput(session, inputId = "ofac", value = "")
            updateTextInput(session, inputId = "long_period", value = "")
            updateTextInput(session, inputId = "short_period", value = "")
          }
          enable("wavelet")
          enable("waveletType")
          enable("series2")
          enable("server2")
          if (isTruthy(input$server2)) {
            enable("product2")
            if (isTruthy(input$product2)) {
              enable("station2")
            } else {
              file$secondary <- NULL
              updateRadioButtons(inputId = "optionSecondary", selected = 0)
              output$station2 <- renderUI({
                textInput(inputId = "station2", label = "Station", value = "")
              })
              disable("station2")
            }
          } else {
            disable("station2")
            disable("product2")
          }
          if (length(file$secondary) > 0) {
            enable("format2")
            if (input$format == 4) {
              updateRadioButtons(session, inputId = "format2", label = NULL, choices = list("NEU/ENU" = 1, "PBO" = 2, "NGL" = 3, "1D" = 4), selected = 4, inline = T)
              disable("server2")
              shinyjs::delay(100, disable("format2"))
              disable("ne")
            } else {
              enable("format2")
              enable("server2")
              enable("ne")
            }
            enable("scaleFactor")
            enable("step2")
            enable("optionSecondary")
            enable("separator2")
            enable("epoch2")
            enable("variable2")
            if (input$sigmas == T) {
              enable("errorBar2")
            } else {
              disable("errorBar2")
            }
            if (input$optionSecondary > 0 && input$format < 4) {
              enable("ne")
            } else {
              disable("ne")
              updateCheckboxInput(session, inputId = "fullSeries", value = F)
              updateCheckboxInput(session, inputId = "sameScale", value = F)
              updateCheckboxInput(session, inputId = "same_axis", value = F)
              updateCheckboxInput(session, inputId = "ne", value = F)
              # setting new axis limits
              if (input$tab == 4) {
                info$minx <- min(db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]]$status1)],
                                 db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]]$status2)],
                                 db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]]$status3)])
                info$maxx <- max(db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]]$status1)],
                                 db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]]$status2)],
                                 db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]]$status3)])
              } else {
                req(db2[[info$db2]])
                info$minx <- min(db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]][[paste0("status", input$tab)]])])
                info$maxx <- max(db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]][[paste0("status", input$tab)]])])
              }
              ranges$x1 <- c(info$minx, info$maxx)
            }
            if (input$optionSecondary == 1) {
              enable("fullSeries")
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
              enable("swap")
            } else {
              disable("fullSeries")
              disable("sameScale")
              disable("same_axis")
              disable("swap")
            }
          } else {
            disable("optionSecondary")
            disable("ne")
            disable("format2")
            disable("scaleFactor")
            disable("step2")
            disable("sameScale")
            disable("same_axis")
            disable("separator2")
            disable("fullSeries")
            disable("epoch2")
            disable("variable2")
            disable("errorBar2")
            disable("swap")
          }
          enable("fitType")
          enable("model")
          if (input$fitType == 1 || input$fitType == 2) {
            if (length(trans$mod) > 0 && length(trans$res) > 0) {
              if (length(trans$res1x) > 0 && length(trans$res2x) > 0 && length(trans$res3x) > 0) {
                showTab(inputId = "tab", target = "5", session = getDefaultReactiveDomain())
              } else {
                hideTab(inputId = "tab", target = "5", session = getDefaultReactiveDomain())
              }
              enable("spectrumModel")
              enable("spectrumResiduals")
              shinyjs::show(id = "res", anim = T, animType = "fade", time = 0.5, selector = NULL)
              shinyjs::show(id = "res1", anim = T, animType = "fade", time = 0.5, selector = NULL)
              shinyjs::show(id = "res2", anim = T, animType = "fade", time = 0.5, selector = NULL)
              shinyjs::show(id = "res3", anim = T, animType = "fade", time = 0.5, selector = NULL)
              if (length(trans$offsetEpochs) > 0) {
                enable("verif_offsets")
                if (isTRUE(input$verif_offsets)) {
                  if ( (nchar(inputs$verif_white) > 0 && !is.na(as.numeric(inputs$verif_white)) && as.numeric(inputs$verif_white) > 0) || 
                       (nchar(input$verif_pl) > 0 && nchar(input$verif_k) > 0 && !is.na(as.numeric(inputs$verif_pl)) && !is.na(as.numeric(inputs$verif_k)) && as.numeric(inputs$verif_pl) > 0 && as.numeric(inputs$verif_k) <= 0) ||
                       (nchar(input$verif_fl) > 0 && !is.na(as.numeric(inputs$verif_fl)) && as.numeric(inputs$verif_fl) > 0) ||
                       (nchar(input$verif_rw) > 0 && !is.na(as.numeric(inputs$verif_rw)) && as.numeric(inputs$verif_rw) > 0) ) {
                    enable("runVerif")
                  } else {
                    disable("runVerif")
                  }
                }
              } else {
                updateCheckboxInput(session, inputId = "verif_offsets", label = NULL, value = F)
                updateTextInput(session, inputId = "verif_white", value = "")
                updateTextInput(session, inputId = "verif_fl", value = "")
                updateTextInput(session, inputId = "verif_rw", value = "")
                updateTextInput(session, inputId = "verif_pl", value = "")
                updateTextInput(session, inputId = "verif_k", value = "")
                disable("verif_offsets")
                disable("runVerif")
              }
            } else {
              hideTab(inputId = "tab", target = "5", session = getDefaultReactiveDomain())
              disable("correct_waveform")
              shinyjs::hide(id = "res", anim = T, animType = "fade", time = 0.5, selector = NULL)
              shinyjs::hide(id = "res1", anim = T, animType = "fade", time = 0.5, selector = NULL)
              shinyjs::hide(id = "res2", anim = T, animType = "fade", time = 0.5, selector = NULL)
              shinyjs::hide(id = "res3", anim = T, animType = "fade", time = 0.5, selector = NULL)
              updateCheckboxInput(session, inputId = "spectrumResiduals", value = F)
              updateCheckboxInput(session, inputId = "spectrumModel", value = F)
              info$run <- F
              disable("mle")
              disable("spectrumModel")
              disable("spectrumResiduals")
            }
          } else {
            if (isTruthy(input$model)) {
              updateCheckboxGroupInput(session, inputId = "model", label = "", choices = list("Linear","Polynomial","Sinusoidal","Offset","Exponential","Logarithmic"), selected = NULL, inline = T)
            }
            shinyjs::hide(id = "res", anim = T, animType = "fade", time = 0.5, selector = NULL)
            shinyjs::hide(id = "res1", anim = T, animType = "fade", time = 0.5, selector = NULL)
            shinyjs::hide(id = "res2", anim = T, animType = "fade", time = 0.5, selector = NULL)
            shinyjs::hide(id = "res3", anim = T, animType = "fade", time = 0.5, selector = NULL)
            updateCheckboxInput(session, inputId = "spectrumResiduals", value = F)
            updateCheckboxInput(session, inputId = "spectrumModel", value = F)
            info$run <- F
            disable("mle")
            disable("spectrumModel")
            disable("spectrumResiduals")
          }
          if (length(trans$filter) > 0) {
            enable("spectrumFilter")
            enable("spectrumFilterRes")
          } else {
            updateCheckboxInput(session, inputId = "spectrumFilter", value = F)
            updateCheckboxInput(session, inputId = "spectrumFilterRes", value = F)
            disable("spectrumFilter")
            disable("spectrumFilterRes")
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
              if (isTruthy(input$correct_waveform)) {
                updateCheckboxInput(session, inputId = "correct_waveform", value = F)
              }
            }
            if (isTruthy(input$mle)) {
              enable("white")
              enable("flicker")
              enable("randomw")
              enable("powerl")
              disable("runmle")
              disable("noise_unc")
              enable("est.mle")
              if (sum(input$white,input$flicker,input$randomw,input$powerl) > 1) {
                enable("wiener")
              } else {
                if (input$wiener) {
                  updateCheckboxInput(session, inputId = "wiener", value = F)
                }
                disable("wiener")
              }
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
              if ((input$white && !input$flicker && !input$randomw && !input$powerl) ||
                  (!input$white && input$flicker && !input$randomw && !input$powerl) ||
                  (!input$white && !input$flicker && input$randomw && !input$powerl)) {
                disable("noise_unc")
                updateCheckboxInput(session, inputId = "noise_unc", value = T)
              } else {
                enable("noise_unc")
              }
            } else {
              disable("white")
              disable("flicker")
              disable("randomw")
              disable("powerl")
              disable("runmle")
              disable("noise_unc")
              disable("est.mle")
              trans$mle <- F
              if (isTruthy(input$white)) {
                updateCheckboxInput(session, inputId = "white", label = NULL, value = F)
              }
              if (isTruthy(input$flicker)) {
                updateCheckboxInput(session, inputId = "flicker", label = NULL, value = F)
              }
              if (isTruthy(input$randomw)) {
                updateCheckboxInput(session, inputId = "randomw", label = NULL, value = F)
              }
              if (isTruthy(input$powerl)) {
                updateCheckboxInput(session, inputId = "powerl", label = NULL, value = F)
              }
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
            if (isTruthy(input$waveform)) {
              updateCheckboxInput(session, inputId = "waveform", label = NULL, value = F)
            }
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
            if (length(trans$filter) > 0) {
              disable("thresholdResN")
            } else {
              if (input$sigmas == T) {
                enable("thresholdResN")
              } else {
                disable("thresholdResN")
              }
            }
          } else {
            disable("thresholdRes")
            disable("thresholdResN")
          }
          if (!is.na(inputs$thresholdRes) || !is.na(inputs$thresholdResN)) {
            enable("removeAuto")
          } else {
            disable("removeAuto")
          }
          if (!isTruthy(input$wavelet) && input$waveletType > 0) {
            updateTextInput(session, "corto_wavelet", value = "")
            updateTextInput(session, "largo_wavelet", value = "")
            updateRadioButtons(session, inputId = "waveletType", label = NULL, list("None" = 0, "Original" = 1, "Model" = 2, "Model res." = 3, "Filter" = 4, "Filter res." = 5), selected = 0, inline = T, choiceNames = NULL, choiceValues = NULL)
            shinyjs::delay(100, disable("waveletType"))
          }
        } else {
          if (input$fitType > 0) {
            updateRadioButtons(session, inputId = "fitType", label = NULL, list("None" = 0, "LS" = 1, "KF" = 2), selected = 0, inline = T, choiceNames = NULL, choiceValues = NULL)
          }
          shinyjs::delay(100, disable("fitType"))
          disable("traceLog")
          disable("traceSinfo")
          disable("traceSoln")
          disable("traceCustom")
          disable("filter")
          disable("spectrum")
          disable("wavelet")
          # updateRadioButtons(session, inputId = "waveletType", label = NULL, choices = list("None" = 0, "Original" = 1, "Model" = 2, "Model res." = 3, "Filter" = 4, "Filter res." = 5), selected = 0, inline = T, choiceNames = NULL,  choiceValues = NULL)
          shinyjs::delay(100, disable("waveletType"))
          updateTextInput(session, "corto_wavelet", value = "")
          updateTextInput(session, "largo_wavelet", value = "")
          disable("midas")
          disable("entropy")
          disable("spectrumModel")
          disable("spectrumResiduals")
          disable("spectrumFilter")
          disable("spectrumFilterRes")
          enable("plot")
          disable("cut")
          disable("plotAll")
          enable("series")
          disable("series2")
          disable("separator2")
          enable("format")
          disable("average")
          disable("server2")
          disable("swap")
        }
      } else {
        hideTab(inputId = "tab", target = "7", session = getDefaultReactiveDomain())
        # updateRadioButtons(session, inputId = "waveletType", label = NULL, choices = list("None" = 0, "Original" = 1, "Model" = 2, "Model res." = 3, "Filter" = 4, "Filter res." = 5), selected = 0, inline = T, choiceNames = NULL,  choiceValues = NULL)
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
        disable("spectrumResiduals")
        disable("spectrumType")
        disable("est.mle")
        disable("remove")
        disable("remove3D")
        disable("add_excluded")
        disable("cut")
        disable("permanent")
        disable("removeAuto")
        disable("filter")
        disable("flicker")
        disable("format")
        disable("format2")
        disable("tunits")
        disable("sunits")
        disable("ids")
        disable("histogram")
        disable("histogramType")
        disable("log")
        disable("average")
        disable("midas")
        disable("entropy")
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
        disable("noise_unc")
        disable("series2")
        disable("separator")
        disable("separator2")
        disable("euler")
        disable("gia")
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
        if (isTruthy(input$server1)) {
          enable("station1")
          enable("product1")
        } else {
          disable("station1")
          disable("product1")
        }
        disable("station2")
        disable("server2")
        disable("product2")
        disable("swap")
      }
    }
  }, priority = 100)

  # Observe screen ####
  observeEvent(c(session$clientData$pixelratio, session$clientData$output_plot1_width), {
    if (messages > 2) cat(file = stderr(), mySession, "Screen size change", "\n")
    info$pixelratio <- session$clientData$pixelratio
    info$width <- session$clientData$output_plot1_width
  }, priority = 2000)

  # Observe URL ####
  observeEvent(c(session$clientData$url_search), {
    if (length(input$isMobile) > 0 && !isTruthy(input$isMobile)) {
      if (!isTruthy(url$station)) {
        query <- parseQueryString(session$clientData$url_search)
        if (length(query) > 0) {
          removeNotification("parsing_url1")
          removeNotification("parsing_url2")
          removeNotification("no_local")
          if (messages > 0) cat(file = stderr(), mySession, "Analyzing URL from", toupper(query[['server']]), "&", toupper(query[['product']]), "\n")
          info$local = Sys.getenv('SHINY_PORT') == "" || session$clientData$url_hostname == "127.0.0.1" # info$local needs to be set here too
          if (!is.null(query[['server']]) && !is.null(query[['station']]) && !is.null(query[['product']])) {
            removeNotification("bad_url")
            if (!isTruthy(info$local) && tolower(query[['server']]) == "local") {
              showNotification(paste0("Local server is not available on remote connections."), action = NULL, duration = 10, closeButton = T, id = "no_local", type = "warning", session = getDefaultReactiveDomain())
              url$station <- NULL
              url$file <- NULL
              req(info$stop)
            }
            url_info <- unlist(get_URL_info(query[['server']],query[['station']],query[['product']],1))
            if (isTruthy(url_info)) {
              url$station <- url_info[1]
              url$file <- url_info[2]
              url$server <- toupper(query[['server']])
              file$primary$name <- url_info[3]
              info$format <- url_info[4]
              url$logfile <- url_info[5]
              info$product1 <- toupper(query[['product']])
              if (tolower(query[['server']]) == "local") {
                if (!isTruthy(file.exists(url$file))) {
                  showNotification(paste0("Local file ",url$file," not found."), action = NULL, duration = 10, closeButton = T, id = "bad_url", type = "error", session = getDefaultReactiveDomain())
                  url$station <- NULL
                  url$file <- NULL
                  req(info$stop)
                }
                showNotification(paste0("Uploading series file ",file$primary$name," from ",toupper(query[['server']]),"."), action = NULL, duration = 30, closeButton = T, id = "parsing_url1", type = "warning", session = getDefaultReactiveDomain())
                down <- 0
                file$primary$datapath <- url$file
              } else {
                showNotification(paste0("Downloading series file ",file$primary$name," from ",toupper(query[['server']]),"."), action = NULL, duration = 30, closeButton = T, id = "parsing_url1", type = "warning", session = getDefaultReactiveDomain())
                file$primary$datapath <- paste0("www/tempFiles/",file$primary$name)
                down <- download(url$server, url$file, file$primary$datapath)
                if (file.exists(file$primary$datapath)) {
                  downloaded <- readLines(file$primary$datapath, n = 2, warn = F)
                  if (grepl("DOCTYPE", downloaded[1], ignore.case = F) ||
                      length(downloaded) < 2) {
                    down <- 1
                  }
                }
              }
              if (isTruthy(down) && down == 0) {
                # if (messages > 0) cat(file = stderr(), mySession, "Primary series downloaded in", file$primary$datapath, "\n")
                # update format for primary series
                shinyjs::delay(100, updateRadioButtons(session, inputId = "format", selected = info$format))
                # download associated logfile
                if (isTruthy(url$logfile)) {
                  showNotification(paste0("Downloading logfile for ",toupper(url$station),"."), action = NULL, duration = 5, closeButton = T, id = "parsing_log1", type = "warning", session = getDefaultReactiveDomain())
                  file$primary$logfile <- paste0("www/tempFiles/",basename(url$logfile))
                  down <- download("", url$logfile, file$primary$logfile)
                  if (down == 0) {
                    file$sitelog <- NULL
                    session$sendCustomMessage("log", basename(url$logfile))
                    shinyjs::delay(100, updateCheckboxInput(inputId = "traceLog", value = T))
                  } else {
                    showNotification(HTML(paste0("Logfile not found in ", url$server,".<br>No file was downloaded.")), action = NULL, duration = 10, closeButton = T, id = "bad_url", type = "error", session = getDefaultReactiveDomain())
                    file$primary$logfile <- NULL
                    url$logfile <- NULL
                  }
                }
                # processing secondary series
                if (!is.null(query[['server2']]) && !is.null(query[['station2']]) && !is.null(query[['product2']])) {
                  if (messages > 0) cat(file = stderr(), mySession, "Analyzing secondary URL from", toupper(query[['server2']]), "&", toupper(query[['product2']]), "\n")
                  url_info <- unlist(get_URL_info(query[['server2']],query[['station2']],query[['product2']],2))
                  if (isTruthy(url_info)) {
                    url$station2 <- url_info[1]
                    url$file2 <- url_info[2]
                    url$server2 <- toupper(query[['server2']])
                    file$secondary$name <- url_info[3]
                    info$format2 <- url_info[4]
                    url$logfile2 <- url_info[5]
                    shinyjs::delay(100, updateRadioButtons(session, inputId = "format2", label = NULL, selected = info$format2))
                    if (tolower(query[['server2']]) == "local") {
                      if (!isTruthy(file.exists(url$file2))) {
                        showNotification(paste0("Local file ",url$file2," not found."), action = NULL, duration = 10, closeButton = T, id = "bad_url", type = "error", session = getDefaultReactiveDomain())
                        url$station2 <- NULL
                        url$file2 <- NULL
                        req(info$stop)
                      }
                      showNotification(paste0("Uploading series file ",file$secondary$name," from ",toupper(query[['server']]),"."), action = NULL, duration = 30, closeButton = T, id = "parsing_url2", type = "warning", session = getDefaultReactiveDomain())
                      down <- 0
                      file$secondary$datapath <- url$file2
                    } else {
                      showNotification(paste0("Downloading secondary series file ",file$secondary$name," from ",toupper(query[['server2']]),"."), action = NULL, duration = 30, closeButton = T, id = "parsing_url2", type = "warning", session = getDefaultReactiveDomain())
                      file$secondary$datapath <- tempfile()
                      down <- download(url$server2, url$file2, file$secondary$datapath)
                      if (file.exists(file$secondary$datapath)) {
                        downloaded <- readLines(file$secondary$datapath, warn = F)
                        if (grepl("DOCTYPE", downloaded[1], ignore.case = F) ||
                            length(downloaded) < 2) {
                          down <- 1
                        }
                      }
                    }
                    if (isTruthy(down) && down == 0) {
                      # if (messages > 0) cat(file = stderr(), mySession, "Secondary series downloaded in", file$secondary$datapath, "\n")
                      info$menu <- unique(c(info$menu, 3))
                      updateCollapse(session, id = "menu", open = info$menu)
                      shinyjs::delay(100, updateRadioButtons(session, inputId = "optionSecondary", label = NULL, selected = 1))
                      if (url$server2 == "LOCAL") {
                        filename2 <- basename(url$file2)
                      } else {
                        filename2 <- file$secondary$name
                      }
                      session$sendCustomMessage("filename2", filename2)
                      if (isTruthy(url$logfile2) && !isTruthy(url$logfile)) {
                        showNotification(paste0("Downloading logfile for ",toupper(url$station2),"."), action = NULL, duration = 5, closeButton = T, id = "parsing_log2", type = "warning", session = getDefaultReactiveDomain())
                        file$secondary$logfile <- paste0("www/tempFiles/",basename(url$logfile2))
                        down <- download("", url$logfile2, file$secondary$logfile)
                        if (down == 0) {
                          file$sitelog <- NULL
                          session$sendCustomMessage("log", basename(url$logfile2))
                          shinyjs::delay(100, updateCheckboxInput(inputId = "traceLog", value = T))
                        } else {
                          showNotification(HTML(paste0("Logfile not found in ", url$server2,".<br>No file was downloaded.")), action = NULL, duration = 10, closeButton = T, id = "bad_url", type = "error", session = getDefaultReactiveDomain())
                          file$secondary$logfile <- NULL
                        }
                      }
                    } else {
                      removeNotification("parsing_url2")
                      showNotification(HTML(paste0("File ",file$secondary$name," not found in ",toupper(query[['server2']]),".<br>No file was downloaded.")), action = NULL, duration = 10, closeButton = T, id = "bad_url", type = "error", session = getDefaultReactiveDomain())
                    }
                  }
                }
                shinyjs::delay(1500, {
                  if (messages > 4) cat(file = stderr(), mySession, "From: observe url\n")
                  digest(1)
                  if (isTruthy(url$station2) && isTruthy(url$file2) && isTruthy(url$server2)) {
                    digest(2)
                  }
                  if (url$server == "LOCAL") {
                    filename <- basename(url$file)
                  } else {
                    filename <- file$primary$name
                  }
                  session$sendCustomMessage("filename", filename)
                })
              } else {
                removeNotification("parsing_url1")
                showNotification(HTML(paste0("File ",file$primary$name," not found in ",toupper(query[['server']]),".<br>No file was downloaded.")), action = NULL, duration = 10, closeButton = T, id = "bad_url", type = "error", session = getDefaultReactiveDomain())
                url$station <- NULL
                url$file <- NULL
              }
            }
          } else {
            showNotification(HTML(paste0("At least one missing argument in the URL (station, server and product).<br>No file was downloaded.")), action = NULL, duration = 10, closeButton = T, id = "bad_url", type = "error", session = getDefaultReactiveDomain())
            url$station <- NULL
            url$file <- NULL
          }
        } else {
          url$station <- NULL
          url$file <- NULL
        }
      }
    }
  })
  
  # Observe remote series ####
  observeEvent(input$server1, {
    if (input$server1 == "RENAG") {
      updateSelectizeInput(session, inputId = "product1", choices = list("UGA"), selected = "UGA")
    } else if (input$server1 == "FORMATER") {
      updateSelectizeInput(session, inputId = "product1", choices = list("SPOTGINS_POS", "UGA_POS"), selected = "")
    } else if (input$server1 == "IGS") {
      updateSelectizeInput(session, inputId = "product1", choices = list("IGS20"), selected = "IGS20")
    } else if (input$server1 == "EUREF") {
      updateSelectizeInput(session, inputId = "product1", choices = list("IGB14"), selected = "IGB14")
    } else if (input$server1 == "EPOS") {
      updateSelectizeInput(session, inputId = "product1", choices = list("INGV", "ROB-EUREF", "SGO-EPND", "UGA-CNRS"), selected = "")
    } else if (input$server1 == "NGL") {
      updateSelectizeInput(session, inputId = "product1", choices = list("FINAL", "RAPID"), selected = "")
    } else if (input$server1 == "JPL") {
      updateSelectizeInput(session, inputId = "product1", choices = list("REPRO2018A"), selected = "REPRO2018A")
    } else if (input$server1 == "EOSTLS") {
      updateSelectizeInput(session, inputId = "product1", choices = list("ATMIB", "ATMIB(d)", "ATMMO(o)", "ECCO(o)", "ECCO2", "ERA5IB", "ERA5IB(d)", "ERA5TUGO", "ERA5TUGO(d)", "ERA5HYD", "ERA5HYD(d)", "GRACE", "GLDAS2", "GLDAS2(d)", "GLORYS(o)", "MERRA2ATM", "MERRA2ATM(d)", "MERRA2HYD", "MERRA2HYD(d)"), selected = "")
    } else if (input$server1 == "SONEL") {
      updateSelectizeInput(session, inputId = "product1", choices = list("ULR7A"), selected = "ULR7A")
    } else if (input$server1 == "SIRGAS") {
      updateSelectizeInput(session, inputId = "product1", choices = list("IGB14"), selected = "IGB14")
    } else if (input$server1 == "EARTHSCOPE") {
      updateSelectizeInput(session, inputId = "product1", choices = list("CWU", "PBO", "NMT"), selected = "")
    } else if (input$server1 == "PSMSL") {
      updateSelectizeInput(session, inputId = "product1", choices = list("RLR"), selected = "RLR")
    }
    output$station1 <- renderUI({
      textInput(inputId = "station1", label = "Station", value = "")
    })
  })
  observeEvent(input$server2, {
    req(db1[[info$db1]])
    updateRadioButtons(inputId = "optionSecondary", selected = 0)
    if (input$server2 == "RENAG") {
      updateSelectizeInput(session, inputId = "product2", choices = list("UGA"), selected = "UGA")
    } else if (input$server2 == "FORMATER") {
      updateSelectizeInput(session, inputId = "product2", choices = list("SPOTGINS_POS", "UGA_POS"), selected = "")
    } else if (input$server2 == "IGS") {
      updateSelectizeInput(session, inputId = "product2", choices = list("IGS20"), selected = "IGS20")
    } else if (input$server2 == "EUREF") {
      updateSelectizeInput(session, inputId = "product2", choices = list("IGB14"), selected = "IGB14")
    } else if (input$server2 == "EPOS") {
      updateSelectizeInput(session, inputId = "product2", choices = list("INGV", "ROB-EUREF", "SGO-EPND", "UGA-CNRS"), selected = "")
    } else if (input$server2 == "NGL") {
      updateSelectizeInput(session, inputId = "product2", choices = list("FINAL", "RAPID"), selected = "")
    } else if (input$server2 == "JPL") {
      updateSelectizeInput(session, inputId = "product2", choices = list("REPRO2018A"), selected = "REPRO2018A")
    } else if (input$server2 == "EOSTLS") {
      updateSelectizeInput(session, inputId = "product2", choices = list("ATMIB", "ATMIB(d)", "ATMMO(o)", "ECCO(o)", "ECCO2", "ERA5IB", "ERA5IB(d)", "ERA5TUGO", "ERA5TUGO(d)", "ERA5HYD", "ERA5HYD(d)", "GRACE", "GLDAS2", "GLDAS2(d)", "GLORYS(o)", "MERRA2ATM", "MERRA2ATM(d)", "MERRA2HYD", "MERRA2HYD(d)"), selected = "", options = list(maxItems = 12))
    } else if (input$server2 == "SONEL") {
      updateSelectizeInput(session, inputId = "product2", choices = list("ULR7A"), selected = "ULR7A")
    } else if (input$server2 == "SIRGAS") {
      updateSelectizeInput(session, inputId = "product2", choices = list("IGB14"), selected = "IGB14")
    } else if (input$server2 == "EARTHSCOPE") {
      updateSelectizeInput(session, inputId = "product2", choices = list("CWU", "PBO", "NMT"), selected = "")
    } else if (input$server2 == "PSMSL") {
      updateSelectizeInput(session, inputId = "product2", choices = list("RLR"), selected = "RLR")
    }
    output$station2 <- renderUI({
      textInput(inputId = "station2", label = "Station", value = "")
    })
    if (input$server2 != "") {
      file$secondary <- NULL
    }
  })
  observeEvent(c(input$product1), {
    req(input$server1,input$product1)
    removeNotification("bad_remote")
    removeNotification("bad_url")
    removeNotification("parsing_url1")
    removeNotification("no_answer")
    info$product1 <- input$product1
    get_URL_info(input$server1,NULL,input$product1,1)
  })
  ## station1 ####
  observeEvent(c(inputs$station1), {
    if (isTruthy(inputs$station1) && isTruthy(input$server1) && isTruthy(input$product1)) {
      removeNotification("bad_remote")
      removeNotification("bad_url")
      removeNotification("parsing_url1")
      url_info <- unlist(get_URL_info(input$server1,inputs$station1,input$product1,1))
      if (isTruthy(url_info)) {
        url$station <- url_info[1]
        url$file <- url_info[2]
        url$server <- input$server1
        file$primary$name <- url_info[3]
        info$format <- url_info[4]
        url$logfile <- url_info[5]
        showNotification(paste0("Downloading series file ",file$primary$name," from ",toupper(input$server1),"."), action = NULL, duration = 30, closeButton = T, id = "parsing_url1", type = "warning", session = getDefaultReactiveDomain())
        if (messages > 0) cat(file = stderr(), mySession, "Downloading series from", toupper(input$server1), "&", toupper(input$product1), "\n")
        file$primary$datapath <- paste0("www/tempFiles/",file$primary$name)
        down <- download(url$server, url$file, file$primary$datapath)
        if (file.exists(file$primary$datapath)) {
          downloaded <- readLines(file$primary$datapath, n = 2, warn = F)
          if (grepl("DOCTYPE", downloaded[1], ignore.case = F) ||
              length(downloaded) < 2) {
            down <- 1
          }
        }
        if (isTruthy(down) && down == 0) {
          shinyjs::delay(1500, {
            # if (messages > 0) cat(file = stderr(), mySession, "Primary series downloaded in", file$primary$datapath, "\n")
            if (messages > 4) cat(file = stderr(), mySession, "From: observe remote series (primary)\n")
            updateRadioButtons(session, inputId = "format", label = NULL, selected = info$format)
            shinyjs::delay(1500, {
              digest(1)
              filename <- file$primary$name
              session$sendCustomMessage("filename", filename)
              if (isTruthy(url$logfile)) {
                showNotification(paste0("Downloading logfile for ",toupper(input$station1),"."), action = NULL, duration = 5, closeButton = T, id = "parsing_log1", type = "warning", session = getDefaultReactiveDomain())
                file$primary$logfile <- paste0("www/tempFiles/",basename(url$logfile))
                down <- download("", url$logfile, file$primary$logfile)
                if (down == 0) {
                  file$sitelog <- NULL
                  session$sendCustomMessage("log", basename(url$logfile))
                  updateCheckboxInput(inputId = "traceLog", value = T)
                } else {
                  showNotification(HTML(paste0("Logfile not found in ",input$server,".<br>No file was downloaded.")), action = NULL, duration = 10, closeButton = T, id = "bad_url", type = "error", session = getDefaultReactiveDomain())
                  file$primary$logfile <- NULL
                  url$logfile <- NULL
                }
              }
            })
          })
        } else {
          removeNotification("parsing_url1")
          showNotification(HTML(paste0("File ",file$primary$name," not found in ",toupper(input$server1),".<br>No file was downloaded.")), action = NULL, duration = 10, closeButton = T, id = "bad_url", type = "error", session = getDefaultReactiveDomain())
          updateSelectInput(session, inputId = "station1", selected = "")
          file$primary <- NULL
        }
      } else {
        showNotification(paste0("Product ", input$product1, "for station ", inputs$station1," not found in ",toupper(input$server1)," server"), action = NULL, duration = 10, closeButton = T, id = "bad_remote", type = "error", session = getDefaultReactiveDomain())
        updateSelectInput(session, inputId = "station1", selected = "")
      }
    }
  })
  observeEvent(c(input$product2), {
    req(input$server2)
    removeNotification("bad_remote")
    removeNotification("bad_url")
    removeNotification("no_answer")
    file$secondary <- NULL
    updateRadioButtons(inputId = "optionSecondary", selected = 0)
    req(input$product2)
    get_URL_info(input$server2,NULL,input$product2,2)
  })
  ## station2 ####
  observeEvent(c(inputs$station2), {
    req(db1[[info$db1]])
    if (input$station2 != "") {
      file$secondary <- NULL
    }
    if (isTruthy(inputs$station2) && isTruthy(input$server2) && isTruthy(input$product2)) {
      removeNotification("bad_remote")
      removeNotification("bad_url")
      url_info <- get_URL_info(input$server2,inputs$station2,input$product2,2)
      if (isTruthy(url_info)) {
        url$station2 <- url_info[[1]]
        url$file2 <- url_info[[2]]
        url$server2 <- input$server2
        file$secondary$name <- url_info[[3]]
        info$format2 <- url_info[[4]]
        url$logfile2 <- url_info[[5]]
        file$secondary$datapath <- c()
        secondary_files <- 0
        for (f in 1:length(url$file2)) {
          if (length(url$file2) > 1) {
            file$secondary$datapath <- c(file$secondary$datapath, tempfile())
          } else {
            file$secondary$datapath <- paste0("www/tempFiles/",file$secondary$name)
          }
          showNotification(paste0("Downloading secondary series file ",file$secondary$name[f]," from ",toupper(input$server2),"."), action = NULL, duration = NULL, closeButton = T, id = paste0("parsing_url2_",f), type = "warning", session = getDefaultReactiveDomain())
          if (messages > 0) cat(file = stderr(), mySession, "Downloading secondary series from", toupper(input$server2), "&", toupper(input$product2), "\n")
          down <- download(url$server2, url$file2[f], file$secondary$datapath[f])
          if (file.exists(file$secondary$datapath[f])) {
            downloaded <- readLines(file$secondary$datapath[f], n = 2, warn = F)
            if (grepl("DOCTYPE", downloaded[1], ignore.case = F) ||
                length(downloaded) < 2) {
              down <- 1
            }
          }
          if (isTruthy(down) && down == 0) {
            secondary_files <- secondary_files + 1
            # if (messages > 0) cat(file = stderr(), mySession, "Secondary series downloaded in", file$secondary$datapath[f], "\n")
          } else {
            file$secondary$datapath <- file$secondary$datapath[-length(file$secondary$datapath)]
            removeNotification(paste0("parsing_url2_",f))
            showNotification(HTML(paste0("File ",file$secondary$name[f]," not found in ",input$server2,".<br>No file was downloaded.")), action = NULL, duration = 10, closeButton = T, id = "bad_url", type = "error", session = getDefaultReactiveDomain())
          }
        }
        if (secondary_files > 0) {
          shinyjs::delay(1000, updateRadioButtons(session, inputId = "optionSecondary", label = NULL, selected = 1))
          if (length(file$secondary$name) > 1) {
            filename2 <- paste0(paste(input$station2, paste(input$product2, collapse = "_"), sep = "_"), ".enu")
            file$secondary$datapath <- as.matrix(file$secondary$datapath)
          } else {
            filename2 <- file$secondary$name
            if (isTruthy(url$logfile2) && !isTruthy(url$logfile)) {
              showNotification(paste0("Downloading logfile for ",toupper(input$station2),"."), action = NULL, duration = 5, closeButton = T, id = "parsing_log2", type = "warning", session = getDefaultReactiveDomain())
              file$secondary$logfile <- paste0("www/tempFiles/",basename(url$logfile2))
              down <- download("", url$logfile2, file$secondary$logfile)
              if (down == 0) {
                file$sitelog <- NULL
                session$sendCustomMessage("log", basename(url$logfile2))
                updateCheckboxInput(inputId = "traceLog", value = T)
              } else {
                showNotification(HTML(paste0("Logfile not found in ",input$server2,".<br>No file was downloaded.")), action = NULL, duration = 10, closeButton = T, id = "bad_url", type = "error", session = getDefaultReactiveDomain())
                file$secondary$logfile <- NULL
              }
            }
          }
          file$secondary$newname <- filename2
          file$secondary$newpath <- paste0("www/tempFiles/",file$secondary$newname)
          session$sendCustomMessage("filename2", filename2)
          updateRadioButtons(session, inputId = "format2", selected = info$format2)
          shinyjs::delay(1500, {
            if (messages > 4) cat(file = stderr(), mySession, "From: observe remote series (secondary)\n")
            digest(2)
          })
        } else {
          updateSelectInput(session, inputId = "station2", selected = "")
          file$secondary <- NULL
        }
      } else {
        showNotification(paste("Product", input$product2, "for station", inputs$station2, "not found in", input$server2, "server."), action = NULL, duration = 10, closeButton = T, id = "bad_remote", type = "error", session = getDefaultReactiveDomain())
        updateSelectInput(session, inputId = "station2", selected = "")
      }
    } else if (isTruthy(input$server2) && isTruthy(input$product2)) {
      url$station2 <- url$file2 <- url$server2 <- file$secondary$name <- info$format2 <- file$secondary$datapath <- NULL
      updateRadioButtons(session, inputId = "optionSecondary", label = NULL, selected = 0)
    }
  })

  # Observe Euler ####
  observeEvent(c(input$plateModel), {
    req(db1[[info$db1]], input$euler)
    if (messages > 0) cat(file = stderr(), mySession, "Plate model:", input$plateModel, "\n")
    if (input$plateModel == "ITRF2020") {
      info$plateFile <- "www/ITRF2020-PMM.dat"
      updateSelectizeInput(session, inputId = "plate", choices = read.table(file = info$plateFile, header = F, skip = 5)$V5, selected = "")
    } else if (input$plateModel == "NNR-MORVEL56") {
      info$plateFile <- "www/NNR-MORVEL56.txt"
      updateSelectizeInput(session, inputId = "plate", choices = read.table(file = info$plateFile, header = F, skip = 5, sep = "\t", comment.char = "#")$V2, selected = "")
    } else if (input$plateModel == "NNR-GSRM") {
      info$plateFile <- "www/NNR-GSRM_v2.1.txt"
      updateSelectizeInput(session, inputId = "plate", choices = read.table(file = info$plateFile, header = F, skip = 2)$V1, selected = "")
    } else {
      info$plateFile <- NULL
      updateSelectizeInput(session, inputId = "plate", choices = list(), selected = "")
    }
  })
  observeEvent(c(input$plate), {
    req(db1[[info$db1]], input$euler, info$plateFile)
    if (messages > 0) cat(file = stderr(), mySession, "Plate:", input$plate, "\n")
    removeNotification("bad_plate")
    if (isTruthy(input$plate)) {
      record <- grep(input$plate, grep("^#", readLines(con = info$plateFile, n = -1L, ok = T, warn = F, skipNul = T), perl = T, value = T, invert = T), fixed = F, ignore.case = T, perl = F, value = T)
      if (length(record) == 1) {
        elements <- NULL
        if ((((isTruthy(inputs$station_x) && isTruthy(inputs$station_y) && isTruthy(inputs$station_z)) || (isTruthy(inputs$station_lat) && isTruthy(inputs$station_lon))) ||
             ((isTruthy(inputs$station_x2) && isTruthy(inputs$station_y2) && isTruthy(inputs$station_z2)) || (isTruthy(inputs$station_lat2) && isTruthy(inputs$station_lon2))))
        ) {
          updateRadioButtons(session, inputId = "eulerType", selected = 1)
        }
        if (input$plateModel == "ITRF2020") {
          elements <- unlist(strsplit(record, "\\s+", fixed = F, perl = T, useBytes = F))[c(3,4,5,2)]
          updateRadioButtons(session, inputId = "pole_coordinates", selected = 1)
          inputs$pole_x <- inputs$pole_y <- inputs$pole_z <- inputs$pole_lat <- inputs$pole_lon <- inputs$pole_rot <- NULL
          updateTextInput(session, inputId = "pole_x", value = elements[1])
          updateTextInput(session, inputId = "pole_y", value = elements[2])
          updateTextInput(session, inputId = "pole_z", value = elements[3])
          updateTextInput(session, inputId = "pole_lat", value = "")
          updateTextInput(session, inputId = "pole_lon", value = "")
          updateTextInput(session, inputId = "pole_rot", value = "")
        } else if (input$plateModel == "NNR-MORVEL56") {
          elements <- unlist(strsplit(record, "\\t+", fixed = F, perl = T, useBytes = F))[c(4,5,6,2)]
          updateRadioButtons(session, inputId = "pole_coordinates", selected = 2)
          inputs$pole_x <- inputs$pole_y <- inputs$pole_z <- inputs$pole_lat <- inputs$pole_lon <- inputs$pole_rot <- NULL
          updateTextInput(session, inputId = "pole_lat", value = elements[1])
          updateTextInput(session, inputId = "pole_lon", value = elements[2])
          updateTextInput(session, inputId = "pole_rot", value = elements[3])
          updateTextInput(session, inputId = "pole_x", value = "")
          updateTextInput(session, inputId = "pole_y", value = "")
          updateTextInput(session, inputId = "pole_z", value = "")
        } else if (input$plateModel == "NNR-GSRM") {
          elements <- unlist(strsplit(record, "\\s+", fixed = F, perl = T, useBytes = F))[c(6,7,8,1)]
          updateRadioButtons(session, inputId = "pole_coordinates", selected = 2)
          inputs$pole_x <- inputs$pole_y <- inputs$pole_z <- inputs$pole_lat <- inputs$pole_lon <- inputs$pole_rot <- NULL
          updateTextInput(session, inputId = "pole_lat", value = elements[1])
          updateTextInput(session, inputId = "pole_lon", value = elements[2])
          updateTextInput(session, inputId = "pole_rot", value = elements[3])
          updateTextInput(session, inputId = "pole_x", value = "")
          updateTextInput(session, inputId = "pole_y", value = "")
          updateTextInput(session, inputId = "pole_z", value = "")
        }
      } else {
        trans$plate <- NULL
        trans$plate2 <- NULL
        showNotification(paste("Plate", input$plate, "not found in the", plateModel, "plate model file"), action = NULL, duration = 10, closeButton = T, id = "bad_plate", type = "error", session = getDefaultReactiveDomain())
      }
    }
  })
  observeEvent(c(inputs$station_lat, inputs$station_lon), {
    coordinates <- NULL
    if (isTruthy(inputs$station_lat) && isTruthy(inputs$station_lon)) {
      if (input$sunits == 1) {
        coordinates <- latlon2xyz(inputs$station_lat*pi/180,inputs$station_lon*pi/180,1)
      } else if (input$sunits == 2) {
        coordinates <- latlon2xyz(inputs$station_lat*pi/180,inputs$station_lon*pi/180,1000)
      }
      if (isTruthy(coordinates) && length(coordinates) == 3) {

        if (!isTruthy(inputs$station_x) || !isTruthy(inputs$station_y) || !isTruthy(inputs$station_z)) {
          updateTextInput(session, inputId = "station_x", value = coordinates[1])
          updateTextInput(session, inputId = "station_y", value = coordinates[2])
          updateTextInput(session, inputId = "station_z", value = coordinates[3])
        }

      }
    }
  }, priority = 5)
  observeEvent(c(inputs$station_x, inputs$station_y, inputs$station_z), {
    if (isTruthy(inputs$station_x) && isTruthy(inputs$station_y) && isTruthy(inputs$station_z) && (!isTruthy(inputs$station_lat) || !isTruthy(inputs$station_lon))) {
      stationGeo <- do.call(xyz2llh,as.list(as.numeric(c(inputs$station_x,inputs$station_y,inputs$station_z))))
      coordinates <- c(stationGeo[1] * 180/pi, stationGeo[2] * 180/pi)
      if (length(coordinates) == 2 && (!isTruthy(inputs$station_lat) || !isTruthy(inputs$station_lon))) {
        updateTextInput(session, inputId = "station_lat", value = coordinates[1])
        updateTextInput(session, inputId = "station_lon", value = coordinates[2])
      }
    }
  }, priority = 5)
  observeEvent(c(inputs$station_lat2, inputs$station_lon2), {
    coordinates <- NULL
    if (isTruthy(inputs$station_lat2) && isTruthy(inputs$station_lon2)) {
      if (input$sunits == 1) {
        coordinates <- latlon2xyz(inputs$station_lat2*pi/180,inputs$station_lon2*pi/180,1)
      } else if (input$sunits == 2) {
        coordinates <- latlon2xyz(inputs$station_lat2*pi/180,inputs$station_lon2*pi/180,1000)
      }
      if (length(coordinates) == 3) {
        updateTextInput(session, inputId = "station_x2", value = coordinates[1])
        updateTextInput(session, inputId = "station_y2", value = coordinates[2])
        updateTextInput(session, inputId = "station_z2", value = coordinates[3])
      }
    }
  }, priority = 5)
  observeEvent(c(inputs$station_x2, inputs$station_y2, inputs$station_z2), {
    if (isTruthy(inputs$station_x2) && isTruthy(inputs$station_y2) && isTruthy(inputs$station_z2)) {
      stationGeo <- do.call(xyz2llh,as.list(as.numeric(c(inputs$station_x2,inputs$station_y2,inputs$station_z2))))
      coordinates <- c(stationGeo[1] * 180/pi, stationGeo[2] * 180/pi)
      if (length(coordinates) == 2) {
        updateTextInput(session, inputId = "station_lat2", value = coordinates[1])
        updateTextInput(session, inputId = "station_lon2", value = coordinates[2])
      }
    }
  }, priority = 5)
  observeEvent(c(inputs$station_lat, inputs$station_lon, inputs$station_lat2, inputs$station_lon2), {
    if (isTruthy(inputs$station_lat) && isTruthy(inputs$station_lon)) {
      # Mapping the station positions
      # Plate polygons and boundaries come from Hugo Ahlenius, Nordpil and Peter Bird (https://github.com/fraxen/tectonicplates)
      if (exists("leaflet", mode = "function") && file.exists("www/PB2002_boundaries.json")) {
        if (messages > 0) cat(file = stderr(), mySession, "Location map", "\n")
        lat <- inputs$station_lat
        lon <- inputs$station_lon
        lon <- ifelse(lon > 180, lon - 360, lon)
        lon <- ifelse(lon < -180, lon + 360, lon)
        if (exists("geojson_read", mode = "function") && file.exists("www/PB2002_plates.json")) {
          plates <- geojsonio::geojson_read("www/PB2002_plates.json", what = "sp")
          map <- leaflet(plates, options = leafletOptions(dragging = F, zoomControl = F, doubleClickZoom = F, scrollWheelZoom = "center", minZoom = 1)) %>%
            addTiles() %>%
            addPolygons(
              fillColor = "white",
              weight = 1,
              opacity = 1,
              color = "black",
              dashArray = "1",
              fillOpacity = 0,
              highlightOptions = highlightOptions(
                weight = 3,
                color = "#DF536B",
                dashArray = "",
                fillOpacity = 0,
                bringToFront = TRUE),
              label = plates$PlateName,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")
            ) %>%
            setView(lng = lon, lat = lat, zoom = 10) %>%
            addMarkers(icon = list(iconUrl = "www/GNSS_marker.png", iconSize = c(50,50)), lng = lon, lat = lat, label = file$id1)
        } else {
          boundaries <- readLines("www/PB2002_boundaries.json") %>% paste(collapse = "\n")
          map <- leaflet(options = leafletOptions(dragging = F, zoomControl = F, scrollWheelZoom = "center")) %>%
            addTiles() %>%
            addGeoJSON(boundaries, weight = 3, color = "#DF536B", fill = FALSE) %>%
            setView(lng = lon, lat = lat, zoom = 10) %>%
            addMarkers(icon = list(iconUrl = "www/GNSS_marker.png", iconSize = c(50,50)), lng = lon, lat = lat, label = file$id1)
        }
        if (isTruthy(inputs$station_lat2) && isTruthy(inputs$station_lon2)) {
          lat2 <- inputs$station_lat2
          lon2 <- inputs$station_lon2
          lon2 <- ifelse(lon2 > 180, lon2 - 360, lon2)
          lon2 <- ifelse(lon2 < -180, lon2 + 360, lon2)
          map <- addMarkers(map = map, icon = list(iconUrl = "www/GNSS_marker.png", iconSize = c(25,25)), lng = lon2, lat = lat2, label = file$id2)
        }
        output$myMap <- renderLeaflet(map)
        output$map <- renderUI({
          suppressWarnings(leafletOutput(outputId = "myMap", width = "100%", height = "18vh"))
        })
        shinyjs::delay(500, {runjs("document.getElementsByClassName('leaflet-control-attribution')[0].style.visibility = 'hidden';")})
      }
    }
  }, priority = 3)
  observeEvent(c(input$neuenu, input$tunits, input$sunits, inputs$pole_x, inputs$pole_y, inputs$pole_z, inputs$pole_lat, inputs$pole_lon, inputs$pole_rot, inputs$station_lon2), {
    req(db1[[info$db1]], input$euler)
    if (((isTruthy(inputs$pole_x) && isTruthy(inputs$pole_y) && isTruthy(inputs$pole_z)) || (isTruthy(inputs$pole_lat) && isTruthy(inputs$pole_lon) && isTruthy(inputs$pole_rot))) &&
        (((isTruthy(inputs$station_x) && isTruthy(inputs$station_y) && isTruthy(inputs$station_z)) || (isTruthy(inputs$station_lat) && isTruthy(inputs$station_lon))) ||
         ((isTruthy(inputs$station_x2) && isTruthy(inputs$station_y2) && isTruthy(inputs$station_z2)) || (isTruthy(inputs$station_lat2) && isTruthy(inputs$station_lon2))))
    ) {
      stationCartesian <- c()
      stationCartesian2 <- c()
      stationGeo <- c()
      stationGeo2 <- c()
      poleCartesian <- c()
      if (messages > 0) cat(file = stderr(), mySession, paste0("Compute plate rotation (", input$eulerType, ")"), "\n")
      if (input$sunits == 1) {
        scaling <- 1
      } else if (input$sunits == 2) {
        scaling <- 1000
      } else { #guessing the series units
        if (input$tunits == 1) {
          period <- 365.25
        } else if (input$tunits == 2) {
          period <- 365.25/7
        } else if (input$tunits == 3) {
          period <- 1
        }
        if (input$format == 4) { 
          selected <- db1[[info$db1]]$y1 # current series
        } else {
          selected <- db1[[info$db1]]$y3 # up series
        }
        if (diff(range(db1[[info$db1]]$x3))/period < 1 || length(db1[[info$db1]]$x3) < 6) {
          rate <- (mean(selected[-1*as.integer(length(db1[[info$db1]]$x3*0.1)):length(db1[[info$db1]]$x3)]) - mean(selected[1:as.integer(length(db1[[info$db1]]$x3*0.1))])) / (mean(db1[[info$db1]]$x3[-1*as.integer(length(db1[[info$db1]]$x3*0.1)):length(db1[[info$db1]]$x3)]) - mean(db1[[info$db1]]$x3[1:as.integer(length(db1[[info$db1]]$x3*0.1))]))
        } else {
          withProgress(message = 'Series units not defined.',
                       detail = 'Trying to guess the units ...', value = 0, {
                         setProgress(0)
                         vel <- sapply(1:length(db1[[info$db1]]$x3), function(x) midas_vel(m = x, t = period, disc = 0, selected))
                         vel <- c(vel[1,],vel[2,])
                         vel <- vel[vel > -999999]
                         vel_sig <- 1.4826*mad(vel, na.rm = T)
                         vel_lim <- c(median(vel) + 2*vel_sig, median(vel) - 2*vel_sig)
                         rate <- vel[vel < vel_lim[1] & vel > vel_lim[2]]
                       })
        }
        if (abs(rate) > 0.05 && sd(selected - rate*(db1[[info$db1]]$x3 - mean(db1[[info$db1]]$x3))) > 0.05) {
          scaling <- 1000 # series units are mm most likely
          updateRadioButtons(session, inputId = "sunits", selected = 2)
        } else {
          scaling <- 1 # series units are m most likely
          updateRadioButtons(session, inputId = "sunits", selected = 1)
        }
      }
      stationCartesian <- c(inputs$station_x,inputs$station_y,inputs$station_z)
      stationGeo <- c(inputs$station_lat*pi/180,inputs$station_lon*pi/180)
      if (input$pole_coordinates == 2) {
        poleCartesian <- inputs$pole_rot*degMa2radyr * c(cos(inputs$pole_lat*pi/180)*cos(inputs$pole_lon*pi/180),cos(inputs$pole_lat*pi/180)*sin(inputs$pole_lon*pi/180),sin(inputs$pole_lat*pi/180))
      } else {
        poleCartesian <- c(inputs$pole_x,inputs$pole_y,inputs$pole_z)*degMa2radyr
      }
      if (length(stationCartesian[!is.na(stationCartesian)]) == 3 && length(stationGeo[!is.na(stationGeo)]) == 2 && length(poleCartesian[!is.na(poleCartesian)]) == 3) {
        if (stationGeo[1] < -pi/2 || stationGeo[1] > pi/2 || stationGeo[2] > 2*pi || stationGeo[2] < -2*pi) {
          showNotification(HTML("Station coordinates are missing or out of bounds.<br>Check the input values."), action = NULL, duration = 15, closeButton = T, id = "bad_coordinates", type = "error", session = getDefaultReactiveDomain())
          updateRadioButtons(session, inputId = "eulerType", selected = 0)
          req(info$stop)
        }
        if (sqrt(stationCartesian[1]^2 + stationCartesian[2]^2 + stationCartesian[3]^2) < 6355000*scaling || sqrt(stationCartesian[1]^2 + stationCartesian[2]^2 + stationCartesian[3]^2) > 6385000*scaling) {
          showNotification(HTML("Station coordinates are missing or out of bounds.<br>Check the input values."), action = NULL, duration = 15, closeButton = T, id = "bad_coordinates", type = "error", session = getDefaultReactiveDomain())
          updateRadioButtons(session, inputId = "eulerType", selected = 0)
          req(info$stop)
        }
        if (sqrt(poleCartesian[1]^2 + poleCartesian[2]^2 + poleCartesian[3]^2) > 2) {
          showNotification(HTML("Euler pole parameters missing or out of bounds.<br>Check the input values."), action = NULL, duration = 15, closeButton = T, id = "bad_pole", type = "error", session = getDefaultReactiveDomain())
          updateRadioButtons(session, inputId = "eulerType", selected = 0)
          req(info$stop)
        }
        plateCartesian <- cross(poleCartesian,stationCartesian)
        # Applying the ORB correction
        if (input$plateModel == "ITRF2020") {
          plateCartesian <- plateCartesian + c(0.37, 0.35, 0.74)*scaling/1000
        }
        # rotation <- matrix(data = c(-1*sin(stationGeo[1])*cos(stationGeo[2]),-1*sin(stationGeo[2]),-1*cos(stationGeo[1])*cos(stationGeo[2]),-1*sin(stationGeo[1])*sin(stationGeo[2]),cos(stationGeo[2]),-1*cos(stationGeo[1])*sin(stationGeo[2]),cos(stationGeo[1]),0,-1*sin(stationGeo[1])), nrow = 3, ncol = 3) #NEU
        rotation <- matrix(data = c(-1*sin(stationGeo[2]),-1*sin(stationGeo[1])*cos(stationGeo[2]),-1*cos(stationGeo[1])*cos(stationGeo[2]),cos(stationGeo[2]),-1*sin(stationGeo[1])*sin(stationGeo[2]),-1*cos(stationGeo[1])*sin(stationGeo[2]),0,cos(stationGeo[1]),-1*sin(stationGeo[1])), nrow = 3, ncol = 3) #ENU
        plate_neu <- c(rotation %*% plateCartesian)
        if ((input$format == 1 && input$neuenu == 1) || input$format == 2 || input$format == 3 || input$format == 4) { #ENU
          trans$plate <- plate_neu
        } else if ((input$format == 1 && input$neuenu == 2)) { #NEU
          trans$plate <- c(plate_neu[2],plate_neu[1],plate_neu[3])
        }
        # Forcing the Up velocity to zero, especially if an ORB was corrected
        trans$plate[3] <- 0
        if (input$tunits == 1) {
          trans$plate <- trans$plate/daysInYear
        } else if (input$tunits == 2) {
          trans$plate <- trans$plate*7/daysInYear
        }
        if (isTruthy(inputs$station_x2) && isTruthy(inputs$station_y2) && isTruthy(inputs$station_z2) && isTruthy(inputs$station_lat2) && isTruthy(inputs$station_lon2)) {
          stationCartesian2 <- c(inputs$station_x2,inputs$station_y2,inputs$station_z2)
          stationGeo2 <- c(inputs$station_lat2*pi/180,inputs$station_lon2*pi/180)
          plateCartesian2 <- cross(poleCartesian,stationCartesian2)
          if (input$plateModel == "ITRF2020") {
            plateCartesian2 <- plateCartesian2 + c(0.37, 0.35, 0.74)*scaling/1000
          }
          rotation2 <- matrix(data = c(-1*sin(stationGeo2[2]),-1*sin(stationGeo2[1])*cos(stationGeo2[2]),-1*cos(stationGeo2[1])*cos(stationGeo2[2]),cos(stationGeo2[2]),-1*sin(stationGeo2[1])*sin(stationGeo2[2]),-1*cos(stationGeo2[1])*sin(stationGeo2[2]),0,cos(stationGeo2[1]),-1*sin(stationGeo2[1])), nrow = 3, ncol = 3) #ENU
          plate_neu2 <- c(rotation2 %*% plateCartesian2)
          if ((input$format2 == 1 && input$neuenu == 1) || input$format2 == 2 || input$format2 == 3 || input$format2 == 4) { #ENU
            trans$plate2 <- plate_neu2
          } else if ((input$format2 == 1 && input$neuenu == 2)) { #NEU
            trans$plate2 <- c(plate_neu2[2],plate_neu2[1],plate_neu2[3])
          }
          if (isTruthy(input$ne)) {
            trans$plate2 <- c(plate_neu2[2],plate_neu2[1],plate_neu2[3])
          }
          trans$plate2[3] <- 0
          if (input$tunits == 1) {
            trans$plate2 <- trans$plate2/daysInYear
          } else if (input$tunits == 2) {
            trans$plate2 <- trans$plate2*7/daysInYear
          }
        }
      } else {
        showNotification(HTML("Problem reading the station coordinates and/or the Euler pole parameters.<br>Check the input values."), action = NULL, duration = 15, closeButton = T, id = "no_rotation", type = "warning", session = getDefaultReactiveDomain())
        updateRadioButtons(session, inputId = "eulerType", selected = 0)
        trans$plate <- NULL
        trans$plate2 <- NULL
      }
    } else {
      trans$plate <- NULL
      trans$plate2 <- NULL
    }
  }, priority = 4)
  observeEvent(c(input$eulers), {
    if (!is.null(file$id1)) {
      pattern <- paste0("^",file$id1)
      record <- grep(pattern, readLines(con = input$eulers$datapath, n = -1L, ok = T, warn = F, skipNul = T), ignore.case = F, perl = T, value = T)
      l <- length(record)
      if (l > 0) {
        elements <- unlist(strsplit(record[[l]], "\\s+", fixed = F, perl = T, useBytes = F))
        if (length(elements) == 7) { # Cartesian
          if (!isTruthy(inputs$station_x) || !isTruthy(inputs$station_y) || !isTruthy(inputs$station_z)) {
            stationCartesian <- c(elements[2],elements[3],elements[4])
            updateRadioButtons(session, inputId = "station_coordinates", selected = 1)
            updateTextInput(session, inputId = "station_x", value = stationCartesian[1])
            updateTextInput(session, inputId = "station_y", value = stationCartesian[2])
            updateTextInput(session, inputId = "station_z", value = stationCartesian[3])
          }
          if (sqrt(as.numeric(elements[5])^2 + as.numeric(elements[6])^2 + as.numeric(elements[7])^2) > 2) { #Geographic
            polo_geo <- c(elements[5],elements[6],elements[7])
            updateRadioButtons(session, inputId = "pole_coordinates", selected = 2)
            inputs$pole_x <- inputs$pole_y <- inputs$pole_z <- NULL
            updateTextInput(session, inputId = "pole_lat", value = polo_geo[1])
            updateTextInput(session, inputId = "pole_lon", value = polo_geo[2])
            updateTextInput(session, inputId = "pole_rot", value = polo_geo[3])
          } else { # Cartesian
            poleCartesian <- c(elements[5],elements[6],elements[7])
            updateRadioButtons(session, inputId = "pole_coordinates", selected = 1)
            inputs$pole_lat <- inputs$pole_lon <- inputs$pole_rot <- NULL
            updateTextInput(session, inputId = "pole_x", value = poleCartesian[1])
            updateTextInput(session, inputId = "pole_y", value = poleCartesian[2])
            updateTextInput(session, inputId = "pole_z", value = poleCartesian[3])
          }
        } else if (length(elements) == 6) { #Geographic
          if (!isTruthy(inputs$station_lat) || !isTruthy(inputs$station_lon)) {
            stationGeo <- c(elements[2],elements[3])
            updateRadioButtons(session, inputId = "station_coordinates", selected = 2)
            updateTextInput(session, inputId = "station_lat", value = stationGeo[1])
            updateTextInput(session, inputId = "station_lon", value = stationGeo[2])
          }
          if (sqrt(as.numeric(elements[4])^2 + as.numeric(elements[5])^2 + as.numeric(elements[6])^2) > 2) { #Geographic
            polo_geo <- c(elements[4],elements[5],elements[6])
            updateRadioButtons(session, inputId = "pole_coordinates", selected = 2)
            inputs$pole_x <- inputs$pole_y <- inputs$pole_z <- NULL
            updateTextInput(session, inputId = "pole_lat", value = polo_geo[1])
            updateTextInput(session, inputId = "pole_lon", value = polo_geo[2])
            updateTextInput(session, inputId = "pole_rot", value = polo_geo[3])
          } else { # Cartesian
            poleCartesian <- c(elements[4],elements[5],elements[6])
            updateRadioButtons(session, inputId = "pole_coordinates", selected = 1)
            inputs$pole_lat <- inputs$pole_lon <- inputs$pole_rot <- NULL
            updateTextInput(session, inputId = "pole_x", value = poleCartesian[1])
            updateTextInput(session, inputId = "pole_y", value = poleCartesian[2])
            updateTextInput(session, inputId = "pole_z", value = poleCartesian[3])
          }
        }
        updateSelectInput(inputId = "plateModel", selected = "")
        updateRadioButtons(session, inputId = "eulerType", selected = 1)
      }
    }
  }, priority = 200)
  
  # Observe GIA ####
  observeEvent(c(input$giaModel, inputs$station_lon, inputs$station_lat, inputs$station_lat2, inputs$station_lon2), {
    req(db1[[info$db1]])
    if (isTruthy(input$gia) || isTruthy(trans$gia)) {
      if (messages > 0) cat(file = stderr(), mySession, "GIA model:", input$giaModel, "\n")
      removeNotification("bad_coordinates")
      z1 <- z2 <- NULL
      if (input$tunits == 1) {
        scaling <- 1/daysInYear
      } else if (input$tunits == 2) {
        scaling <- 7/daysInYear
      } else {
        scaling <- 1
      }
      if (input$sunits == 1) {
        scaling <- scaling/1000
      } else if (input$sunits == 2) {
        scaling <- scaling
      } else { #guessing the series units
        if (input$tunits == 1) {
          period <- 365.25
        } else if (input$tunits == 2) {
          period <- 365.25/7
        } else if (input$tunits == 3) {
          period <- 1
        }
        if (input$format == 4) { 
          selected <- db1[[info$db1]]$y1 # current series
        } else {
          selected <- db1[[info$db1]]$y3 # up series
        }
        if (diff(range(db1[[info$db1]]$x3))/period < 1 || length(db1[[info$db1]]$x3) < 6) {
          rate <- (mean(selected[-1*as.integer(length(db1[[info$db1]]$x3*0.1)):length(db1[[info$db1]]$x3)]) - mean(selected[1:as.integer(length(db1[[info$db1]]$x3*0.1))])) / (mean(db1[[info$db1]]$x3[-1*as.integer(length(db1[[info$db1]]$x3*0.1)):length(db1[[info$db1]]$x3)]) - mean(db1[[info$db1]]$x3[1:as.integer(length(db1[[info$db1]]$x3*0.1))]))
        } else {
          withProgress(message = 'Series units not defined.',
                       detail = 'Trying to guess the units ...', value = 0, {
                         setProgress(0)
                         vel <- sapply(1:length(db1[[info$db1]]$x3), function(x) midas_vel(m = x, t = period, disc = 0, selected))
                         vel <- c(vel[1,],vel[2,])
                         vel <- vel[vel > -999999]
                         vel_sig <- 1.4826*mad(vel, na.rm = T)
                         vel_lim <- c(median(vel) + 2*vel_sig, median(vel) - 2*vel_sig)
                         rate <- vel[vel < vel_lim[1] & vel > vel_lim[2]]
                       })
        }
        if (abs(rate) > 0.05 && sd(selected - rate*(db1[[info$db1]]$x3 - mean(db1[[info$db1]]$x3))) > 0.05) {
          scaling <- 1000 # series units are mm most likely
          updateRadioButtons(session, inputId = "sunits", selected = 2)
        } else {
          scaling <- 1 # series units are m most likely
          updateRadioButtons(session, inputId = "sunits", selected = 1)
        }
      }
      withBusyIndicatorServer("giaModel", {
        if (isTruthy(inputs$station_lat) && isTruthy(inputs$station_lon)) {
          if (inputs$station_lon < 0) {
            x1 <- inputs$station_lon + 360 
          } else {
            x1 <- inputs$station_lon
          }
          y1 <- inputs$station_lat
          z1 <- interpolateGIA(x1,y1,1)
          if (isTruthy(z1)) {
            z1 <- z1 * scaling
          }
        }
        if (isTruthy(z1)) {
          updateTextInput(session, inputId = "giaTrend", value = sprintf('%.6f', z1))
        }
        if (isTruthy(inputs$station_lat2) && isTruthy(inputs$station_lon2)) {
          if (inputs$station_lon2 < 0) {
            x2 <- inputs$station_lon2 + 360 
          } else {
            x2 <- inputs$station_lon2
          }
          y2 <- inputs$station_lat2
          z2 <- interpolateGIA(x2,y2,2)
          if (isTruthy(z2)) {
            z2 <- z2 * scaling * inputs$scaleFactor
          }
        }
        if (isTruthy(z2)) {
          updateTextInput(session, inputId = "giaTrend2", value = sprintf('%.6f', z2))
        }
      })
    }
  })
  observeEvent(inputs$giaTrend, {
    req(db1[[info$db1]])
    if (isTruthy(inputs$giaTrend)) {
      trans$gia <- c(0,0,inputs$giaTrend)
      updateRadioButtons(session, inputId = "giaType", selected = 1)
    } else {
      trans$gia <- NULL
    }
  })
  observeEvent(inputs$giaTrend2, {
    req(db2[[info$db2]])
    if (isTruthy(inputs$giaTrend2)) {
      trans$gia2 <- c(0,0,inputs$giaTrend2)
    } else {
      trans$gia2 <- NULL
    }
  })

  # Observe wavelet ####
  observeEvent(c(inputs$min_wavelet, inputs$max_wavelet, inputs$res_wavelet, inputs$loc_wavelet),{
    req(inputs$min_wavelet, inputs$max_wavelet, inputs$res_wavelet, inputs$loc_wavelet)
    removeNotification("bad_wavelet")
    if (isTruthy(inputs$max_wavelet) && isTruthy(inputs$min_wavelet) && isTruthy(as.numeric(inputs$res_wavelet)) && isTruthy(as.numeric(inputs$loc_wavelet)) && inputs$max_wavelet > 0 && inputs$min_wavelet > 0 && as.numeric(inputs$res_wavelet) > 0 && as.numeric(inputs$loc_wavelet) >= info$sampling && as.numeric(inputs$loc_wavelet) <= info$rangex/2) {
      removeNotification("time_wavelet")
      num_scale <- as.integer((inputs$max_wavelet - inputs$min_wavelet)/as.numeric(inputs$res_wavelet))
      num_epochs <- info$rangex/as.numeric(inputs$loc_wavelet)
      time_needed <- ceiling(0.000588*num_scale*num_epochs/60)
      if (time_needed > 29) {
        if (isTruthy(info$local)) {
          shinyjs::delay(500, showNotification(paste0("The time needed to compute the wavelet with the current parameters is around ",time_needed," min."), action = NULL, duration = 10, closeButton = T, id = "time_wavelet", type = "warning", session = getDefaultReactiveDomain()))
        } else {
          shinyjs::delay(500, showNotification(HTML(paste0("The time needed to compute the wavelet with the current parameters is around ",time_needed," min.<br><br>WARNING: the server may kill the connection before the wavelet finishes!")), action = NULL, duration = 10, closeButton = T, id = "time_wavelet", type = "error", session = getDefaultReactiveDomain()))
        }
      } else {
        shinyjs::delay(500, showNotification(paste0("The time needed to compute the wavelet with the current parameters is around ",time_needed," min."), action = NULL, duration = 10, closeButton = T, id = "time_wavelet", type = "warning", session = getDefaultReactiveDomain()))
      }
    } else {
      showNotification(HTML(paste0("Invalid bounds to compute the wavelet.<br>Check the input values.")), action = NULL, duration = 10, closeButton = T, id = "bad_wavelet", type = "error", session = getDefaultReactiveDomain())
    }
  })

  # Observe time units ####
  observeEvent(input$tunits, {
    req(db1[[info$db1]], input$tunits != info$tunits.last)
    if (input$tunits == 1) {
      x1 <- db1[[info$db1]]$x1
      x2 <- db2[[info$db2]]$x1
      if (info$tunits.last == 2) {
        fun <- "week2mjd"
        scale <- 7
      } else if (info$tunits.last == 3) {
        fun <- "year2mjd"
        scale <- daysInYear
      }
    } else if (input$tunits == 2) {
      x1 <- db1[[info$db1]]$x2
      x2 <- db2[[info$db2]]$x2
      if (info$tunits.last == 1) {
        fun <- "mjd2week"
        scale <- 1/7
      } else if (info$tunits.last == 3) {
        fun <- "year2week"
        scale <- daysInYear/7
      }
    } else if (input$tunits == 3) {
      x1 <- db1[[info$db1]]$x3
      x2 <- db2[[info$db2]]$x3
      if (info$tunits.last == 1) {
        fun <- "mjd2year"
        scale <- 1/daysInYear
      } else if (info$tunits.last == 2) {
        fun <- "week2year"
        scale <- 7/daysInYear
      }
    }
    if (isTruthy(inputs$step)) {
      session$sendCustomMessage("step", min(diff(x1,1)))
    }
    if (isTruthy(inputs$step2)) {
      session$sendCustomMessage("step2", min(diff(x2,1)))
    }
    if (isTruthy(inputs$trendRef)) {
      if (info$tunits.last == 1) {
        if (input$tunits == 2) {
          new <- mjd2week(inputs$trendRef)
        } else if (input$tunits == 3) {
          new <- mjd2year(inputs$trendRef)
        }
      } else if (info$tunits.last == 2) {
        if (input$tunits == 1) {
          new <- week2mjd(inputs$trendRef)
        } else if (input$tunits == 3) {
          new <- week2year(inputs$trendRef)
        }
      } else if (info$tunits.last == 3) {
        if (input$tunits == 1) {
          new <- year2mjd(inputs$trendRef)
        } else if (input$tunits == 2) {
          new <- year2week(inputs$trendRef)
        }
      }
      if (isTruthy(new)) {
        # session$sendCustomMessage("trendRef", new)
      }
    }
    # Setting plot limits
    if (isTruthy(input$remove3D)) {
      if (isTruthy(input$fullSeries)) {
        # show all points from primary & secondary series
        info$minx <- min(x1[!is.na(db1[[info$db1]]$status1)], x2, na.rm = T)
        info$maxx <- max(x1[!is.na(db1[[info$db1]]$status1)], x2, na.rm = T)
      } else {
        # show all points from primary series only
        info$minx <- min(x1[!is.na(db1[[info$db1]]$status1)], na.rm = T)
        info$maxx <- max(x1[!is.na(db1[[info$db1]]$status1)], na.rm = T)
      }
      ranges$x1 <- c(info$minx, info$maxx)
    }
    if (input$fitType == 2 && length(trans$mod) > 0 && length(trans$res) > 0) {
      info$run <- F
      trans$mod <- trans$mod0 <- NULL
      trans$res <- trans$res0 <- NULL
      trans$kalman <- trans$kalman0 <- NULL
      trans$kalman_unc <- trans$kalman_unc0 <- NULL
    }
    if (isTruthy(inputs$trendRef)) {
      info$trendRef <- F
    }
    if (isTruthy(inputs$PolyRef)) {
      info$PolyRef <- F
    }
    if (isTruthy(inputs$periodRef)) {
      info$periodRef <- F
    }
    if (info$sampling0 > 0) {
      info$sampling0 <- info$sampling0*scale
    }
    if (length(trans$offsetEpochs) > 0) {
      inputs$offsetEpoch <- paste(sapply(trans$offsetEpochs, fun), collapse = ", ")
      updateTextInput(inputId = "offsetEpoch", value = inputs$offsetEpoch)
    }
    if (length(inputs$ExponenRef) > 0) {
      inputs$ExponenRef <- paste(sapply(as.numeric(unlist(strsplit(inputs$ExponenRef,","))), fun), collapse = ", ")
      inputs$TE0 <- paste(as.numeric(unlist(strsplit(inputs$TE0,",")))*scale, collapse = ", ")
      updateTextInput(inputId = "ExponenRef", value = inputs$ExponenRef)
      updateTextInput(inputId = "TE0", value = inputs$TE0)
    }
    if (length(inputs$LogariRef) > 0) {
      inputs$LogariRef <- paste(sapply(as.numeric(unlist(strsplit(inputs$LogariRef,","))), fun), collapse = ", ")
      inputs$TL0 <- paste(as.numeric(unlist(strsplit(inputs$TL0,",")))*scale, collapse = ", ")
      updateTextInput(inputId = "LogariRef", value = inputs$LogariRef)
      updateTextInput(inputId = "TL0", value = inputs$TL0)
    }
    if (isTruthy(input$wavelet)) {
      updateRadioButtons(session, inputId = "waveletType", selected = 0)
      min_wavelet <- as.numeric(inputs$min_wavelet)*scale
      min_scale <- get.min.scale(x1)
      if (min_scale > min_wavelet) {
        min_wavelet <- min_scale
      }
      max_wavelet <- as.numeric(inputs$max_wavelet)*scale
      max_scale <- get.max.scale(x1)
      if (max_scale > max_wavelet) {
        max_wavelet <- max_scale
      }
      res_wavelet <- as.numeric(inputs$res_wavelet)*scale
      loc_wavelet <- as.numeric(inputs$loc_wavelet)*scale
      shinyjs::delay(500,{
        updateTextInput(session, "min_wavelet", value = sprintf("%.*f", info$decimalsx, min_wavelet))
        updateTextInput(session, "max_wavelet", value = sprintf("%.*f", info$decimalsx, max_wavelet))
        updateTextInput(session, "res_wavelet", value = res_wavelet)
        updateTextInput(inputId = "loc_wavelet", value = sprintf("%.*f", info$decimalsx, loc_wavelet))
      })
    }
    if (isTruthy(inputs$waveformPeriod)) {
      inputs$waveformPeriod <- as.numeric(inputs$waveformPeriod)*scale
      updateTextInput(inputId = "waveformPeriod", value = "")
      shinyjs::delay(100, updateTextInput(inputId = "waveformPeriod", value = inputs$waveformPeriod))
    }
    if (isTruthy(input$mle)) {
      updateCheckboxInput(session, inputId = "mle", value = F)
    }
    if (isTruthy(input$spectrum)) {
      trans$fs <- NULL
      inputs$long_period <- as.numeric(inputs$long_period)*scale
      inputs$short_period <- as.numeric(inputs$short_period)*scale
      updateTextInput(inputId = "long_period", value = inputs$long_period)
      updateTextInput(inputId = "short_period", value = inputs$short_period)
    }
    if (isTruthy(input$filter)) {
      trans$filter <- NULL
      inputs$low <- as.numeric(inputs$low)*scale
      inputs$high <- as.numeric(inputs$high)*scale
      updateTextInput(inputId = "low", value = inputs$low)
      updateTextInput(inputId = "high", value = inputs$high)
    }
    if (isTruthy(info$custom) && length(info$custom) > 0) {
      if (input$tunits == 1) {
        info$custom <- year2mjd(info$custom_years)
      } else if (input$tunits == 2) {
        info$custom <- year2week(info$custom_years)
      } else if (input$tunits == 3) {
        info$custom <- info$custom_years
      }
    }
    if (isTruthy(info$soln) && length(info$soln) > 0) {
      if (input$tunits == 1) {
        info$soln <- year2mjd(info$soln_years)
      } else if (input$tunits == 2) {
        info$soln <- year2week(info$soln_years)
      } else if (input$tunits == 3) {
        info$soln <- info$soln_years
      }
    }
    if (length(info$log) > 0) {
      tmp_log <- info$log
      for (d in 1:length(info$log_years)) {
        if (isTruthy(info$log_years[[d]])) {
          if (input$tunits == 1) {
            tmp_log[[d]] <- year2mjd(info$log_years[[d]])
          } else if (input$tunits == 2) {
            tmp_log[[d]] <- year2week(info$log_years[[d]])
          } else if (input$tunits == 3) {
            tmp_log[[d]] <- info$log_years[[d]]
          } 
        }
      }
      info$log <- tmp_log
    }
    if (length(info$sinfo) > 0) {
      tmp_sinfo <- info$sinfo
      for (d in 1:length(info$sinfo_years)) {
        if (isTruthy(info$sinfo_years[[d]])) {
          if (input$tunits == 1) {
            tmp_sinfo[[d]] <- year2mjd(info$sinfo_years[[d]])
          } else if (input$tunits == 2) {
            tmp_sinfo[[d]] <- year2week(info$sinfo_years[[d]])
          } else if (input$tunits == 3) {
            tmp_sinfo[[d]] <- info$sinfo_years[[d]]
          }
        }
      }
      info$sinfo <- tmp_sinfo
    }
    info$tunits.last <- input$tunits
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
    inputs$min_wavelet <- ""
    if (nchar(input$ObsError) > 0) {
      updateTextInput(session, "ObsError", value = "")
    }
    if (isTruthy(input$white)) {
      updateCheckboxInput(session, inputId = "white", label = NULL, value = F)
    }
    if (isTruthy(input$flicker)) {
      updateCheckboxInput(session, inputId = "flicker", label = NULL, value = F)
    }
    if (isTruthy(input$randomw)) {
      updateCheckboxInput(session, inputId = "randomw", label = NULL, value = F)
    }
    if (isTruthy(input$powerl)) {
      updateCheckboxInput(session, inputId = "powerl", label = NULL, value = F)
    }
    if (input$waveletType > 0) {
      updateRadioButtons(session, inputId = "waveletType", label = NULL, selected = 0)
    }
    if (input$tab == 4) {
      info$minx <- min(db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]]$status1)],
                       db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]]$status2)],
                       db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]]$status3)])
      info$maxx <- max(db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]]$status1)],
                       db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]]$status2)],
                       db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]]$status3)])
      ranges$x1 <- c(info$minx, info$maxx)
    }
  }, priority = 100)

  # Observe ancillary files ####
  observeEvent(input$log, {
    req(db1$original)
    file$sitelog <- isolate(input$log)
    file$sitelog$datapath <- paste0("www/tempFiles/",input$log$name)
    file.copy(input$log$datapath, file$sitelog$datapath, overwrite = T, recursive = F, copy.mode = T, copy.date = T)
  }, priority = 8)
  observeEvent(input$sinfo, {
    req(db1$original)
    file$sinfo <- isolate(input$sinfo)
    file$sinfo$datapath <- paste0("www/tempFiles/",input$sinfo$name)
    file.copy(input$sinfo$datapath, file$sinfo$datapath, overwrite = T, recursive = F, copy.mode = T, copy.date = T)
  }, priority = 8)
  observeEvent(input$soln, {
    req(db1$original)
    file$soln <- isolate(input$soln)
    file.copy(file$soln$datapath, paste0("www/tempFiles/",input$soln$name), overwrite = T, recursive = F, copy.mode = T, copy.date = T)
    file$soln$datapath <- paste0("www/tempFiles/",input$soln$name)
  }, priority = 8)
  observeEvent(input$custom, {
    req(db1$original)
    file$custom <- isolate(input$custom)
    file.copy(file$custom$datapath, paste0("www/tempFiles/",input$custom$name), overwrite = T, recursive = F, copy.mode = T, copy.date = T)
    file$custom$datapath <- paste0("www/tempFiles/",input$custom$name)
  }, priority = 8)

  # Observe series info ####
  observeEvent(c(input$tab, input$format, input$format2), {
    if (input$tab == "6") {
      if (messages > 0) cat(file = stderr(), mySession, "Showing help file", "\n")
    } else {
      req(db1[[info$db1]])
      info$tab <- input$tab
      info$format <- input$format
      info$format2 <- input$format2
      printInfo("CHANGE")
    }
  }, priority = 7)
  observeEvent(c(input$tunits, input$sunits, input$sigmas, file$secondary, input$optionSecondary, input$log, input$sinfo, file$soln, file$custom, inputs$step, input$separator, inputs$epoch, inputs$variable, inputs$errorBar, inputs$scaleFactor, inputs$step2, input$separator2, inputs$epoch2, inputs$variable2, inputs$errorBar2, input$fullSeries, input$sameScale, input$same_axis, input$ne), {
    if (input$tab == "6") {
      if (messages > 0) cat(file = stderr(), mySession, "Showing help file", "\n")
    } else {
      req(db1[[info$db1]])
      printInfo("CHANGE")
    }
  }, priority = 7)

  # Observe primary file ####
  observeEvent(c(input$series), {
    file$primary <- isolate(input$series)
    header <- readLines(input$series$datapath, n = 1)
    if (grepl(".tenv3$", file$primary$name, perl = T) && grepl("site YYMMMDD ", header, fixed = T)) {
      updateRadioButtons(inputId = "format", selected = 3)
      updateRadioButtons(inputId = "sunits", selected = 1)
      updateRadioButtons(inputId = "tunits", selected = 3)
    } else if (grepl(".pos$", file$primary$name, perl = T) && grepl("PBO Station Position Time Series", header, fixed = T)) {
      updateRadioButtons(inputId = "format", selected = 2)
      updateRadioButtons(inputId = "sunits", selected = 1)
      updateRadioButtons(inputId = "tunits", selected = 3)
    } else if (grepl(".pos$", file$primary$name, perl = T) && grepl("GeoCSV", header, fixed = T)) {
      updateRadioButtons(inputId = "format", selected = 1)
      updateRadioButtons(inputId = "sunits", selected = 1)
      updateRadioButtons(inputId = "tunits", selected = 3)
      url$server <- "EARTHSCOPE"
      url$file <- file$primary
    } else if (grepl(".enu$", file$primary$name, perl = T) && grepl("SPOTGINS SOLUTION [POSITION]", header, fixed = T)) {
      updateRadioButtons(inputId = "format", selected = 1)
      updateRadioButtons(inputId = "sunits", selected = 1)
      updateRadioButtons(inputId = "tunits", selected = 3)
      info$product1 <- "SPOTGINS_POS"
    } else if (grepl(".PLH$", file$primary$name, perl = T) && grepl("^DGFI-TUM:", header, perl = T)) {
      updateRadioButtons(inputId = "format", selected = 1)
      updateRadioButtons(inputId = "sunits", selected = 1)
      updateRadioButtons(inputId = "tunits", selected = 3)
      url$server <- "SIRGAS"
      url$file <- file$primary
    } else if (grepl(".series$", file$primary$name, perl = T) && grepl(pattern = "\\.00\\s{2}\\d{4}\\s{1,2}\\d{1,2}\\s{1,2}\\d{1,2}\\s{1,2}\\d{1,2}\\s{1,2}\\d{1,2}\\s{1,2}\\d{1,2}$", header, perl = T)) {
      updateRadioButtons(inputId = "format", selected = 1)
      updateRadioButtons(inputId = "sunits", selected = 1)
      updateRadioButtons(inputId = "tunits", selected = 3)
      url$server <- "JPL"
      url$file <- file$primary
    } else {
      updateRadioButtons(inputId = "format", selected = 1)
      updateRadioButtons(inputId = "sunits", selected = 0)
    }
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
    updateTabsetPanel(session, inputId = "tab", selected = "1")
    updateTextInput(session, "ObsError", value = "")
    updateTextInput(session, "waveformPeriod", value = "")
    if (isTruthy(input$correct_waveform)) {
      updateCheckboxInput(session, inputId = "correct_waveform", value = F)
    }
  }, priority = 6)

  # Observe format 1D ####
  observeEvent(c(inputs$epoch, inputs$variable, inputs$errorBar, input$separator), {
    req(db1[[info$db1]])
    trans$x <- NULL
    trans$y <- NULL
    trans$sy <- NULL
    if (isTruthy(inputs$long_period)) {
      trans$fs <- NULL
      updateTextInput(session, "long_period", value = "")
    }
    if (messages > 4) cat(file = stderr(), mySession, "From: observe format 1D\n")
    digest(1)
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
    if (isTruthy(input$white)) {
      updateCheckboxInput(session, inputId = "white", label = NULL, value = F)
    }
    if (isTruthy(input$flicker)) {
      updateCheckboxInput(session, inputId = "flicker", label = NULL, value = F)
    }
    if (isTruthy(input$randomw)) {
      updateCheckboxInput(session, inputId = "randomw", label = NULL, value = F)
    }
    if (isTruthy(input$powerl)) {
      updateCheckboxInput(session, inputId = "powerl", label = NULL, value = F)
    }
  }, priority = 6)
  observeEvent(c(inputs$epoch2, inputs$variable2, inputs$errorBar2, input$separator2), {
    req(db2[[info$db2]])
    if (messages > 4) cat(file = stderr(), mySession, "From: observe format 1D secondary\n")
    digest(2)
  }, priority = 6)

  # Observe averaging ####
  observeEvent(c(inputs$step), {
    req(db1$original)
    removeNotification("bad_window")
    if (messages > 0) cat(file = stderr(), mySession, "Averaging primary series", "\n")
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
    updateCheckboxInput(session, inputId = "mle", value = F)
    if (isTruthy(input$correct_waveform)) {
      updateCheckboxInput(session, inputId = "correct_waveform", value = F)
    }
    updateTextInput(session, "short_period", value = "")
    updateTextInput(session, "ObsError", value = "")
    if (nchar(input$step) > 0 && is.na(inputs$step)) {
      info$step <- NULL
      showNotification(HTML("The resampling period is not numeric.<br>Check input value."), action = NULL, duration = 10, closeButton = T, id = "bad_window", type = "error", session = getDefaultReactiveDomain())
    } else if (isTruthy(inputs$step)) {
      if (input$tunits == 1) {
        x <- db1$original$x1
      } else if (input$tunits == 2) {
        x <- db1$original$x2
      } else if (input$tunits == 3) {
        x <- db1$original$x3
      }
      db1$resampled <- NULL
      if (inputs$step > info$sampling0 && inputs$step <= (max(x) - min(x))/2) {
        tolerance <- min(diff(x,1))/3
        info$step <- inputs$step
        info$stepUnit <- input$tunits
        withProgress(message = 'Averaging the series.',
                     detail = 'This may take a while ...', value = 0, {
                       w <- as.integer((max(x) - min(x))/inputs$step)
                       if (info$format == 4) {
                         if (input$sigmas) {
                           averaged <- sapply(1:w, function(p) average(p, x = x, y1 = db1$original$y1, y2 = NULL, y3 = NULL, sy1 = db1$original$sy1, sy2 = NULL, sy3 = NULL, tol = tolerance, w = w, s = inputs$step, second = F, sigmas = T), simplify = T)
                           db1$resampled <- na.omit(data.frame(x1 = averaged[1,], y1 = averaged[2,], sy1 = averaged[3,]))
                         } else {
                           averaged <- sapply(1:w, function(p) average(p, x = x, y1 = db1$original$y1, y2 = NULL, y3 = NULL, sy1 = NULL, sy2 = NULL, sy3 = NULL, tol = tolerance, w = w, s = inputs$step, second = F, sigmas = F), simplify = T)
                           db1$resampled <- na.omit(data.frame(x1 = averaged[1,], y1 = averaged[2,], sy1 = rep(1, length(averaged[1,]))))
                         }
                       } else {
                         if (input$sigmas) {
                           averaged <- sapply(1:w, function(p) average(p, x = x, y1 = db1$original$y1, y2 = db1$original$y2, y3 = db1$original$y3, sy1 = db1$original$sy1, sy2 = db1$original$sy2, sy3 = db1$original$sy3, tol = tolerance, w = w, s = inputs$step, second = F, sigmas = T), simplify = T)
                           db1$resampled <- na.omit(data.frame(x1 = averaged[1,], y1 = averaged[2,], y2 = averaged[3,], y3 = averaged[4,], sy1 = averaged[5,], sy2 = averaged[6,], sy3 = averaged[7,]))
                         } else {
                           averaged <- sapply(1:w, function(p) average(p, x = x, y1 = db1$original$y1, y2 = db1$original$y2, y3 = db1$original$y3, sy1 = NULL, sy2 = NULL, sy3 = NULL, tol = tolerance, w = w, s = inputs$step, second = F, sigmas = F), simplify = T)
                           db1$resampled <- na.omit(data.frame(x1 = averaged[1,], y1 = averaged[2,], y2 = averaged[3,], y3 = averaged[4,], sy1 = rep(1, length(averaged[1,])), sy2 = rep(1, length(averaged[1,])), sy3 = rep(1, length(averaged[1,]))))
                         }
                         
                       }
                     })
        if (input$tunits == 1) {
          db1$resampled$x2 <- mjd2week(db1$resampled$x1)
          db1$resampled$x3 <- mjd2year(db1$resampled$x1)
        } else if (input$tunits == 2) {
          db1$resampled$x2 <- db1$resampled$x1
          db1$resampled$x3 <- week2year(db1$resampled$x1)
          db1$resampled$x1 <- week2mjd(db1$resampled$x1)
        } else if (input$tunits == 3) {
          db1$resampled$x3 <- db1$resampled$x1
          db1$resampled$x2 <- year2week(db1$resampled$x1)
          db1$resampled$x1 <- year2mjd(db1$resampled$x1)
        }
        info$db1 <- "resampled"
        db1$resampled$status1 <- db1$resampled$status2 <- db1$resampled$status3 <- rep(T, length(db1$resampled$x1))
      } else {
        info$db1 <- "original"
        updateTextInput(session, inputId = "step", value = "")
        info$step <- NULL
        info$stepUnit <- NULL
        showNotification(HTML("The resampling period is not valid.<br>Check input value."), action = NULL, duration = 10, closeButton = T, id = "bad_window", type = "error", session = getDefaultReactiveDomain())
      }
    } else {
      info$db1 <- "original"
      updateTextInput(session, inputId = "step", value = "")
      info$step <- NULL
    }
  }, priority = 6)
  
  observeEvent(c(inputs$step2), {
    req(db2$original)
    removeNotification("bad_window")
    if (messages > 0) cat(file = stderr(), mySession, "Averaging secondary series", "\n")
    if (input$optionSecondary > 1) {
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
      if (isTruthy(input$correct_waveform)) {
        updateCheckboxInput(session, inputId = "correct_waveform", value = F)
      }
      updateTextInput(session, "short_period", value = "")
      updateTextInput(session, "ObsError", value = "")
    }
    if (nchar(input$step2) > 0 && is.na(inputs$step2)) {
      info$step <- NULL
      showNotification(HTML("The resampling period of the secondary series is not numeric.<br>Check input value."), action = NULL, duration = 10, closeButton = T, id = "bad_window", type = "error", session = getDefaultReactiveDomain())
    } else if (isTruthy(inputs$step2)) {
      if (input$tunits == 1) {
        x <- db2$original$x1
      } else if (input$tunits == 2) {
        x <- db2$original$x2
      } else if (input$tunits == 3) {
        x <- db2$original$x3
      }
      db2$resampled <- NULL
      if (inputs$step2 > min(diff(x,1)) && inputs$step2 <= (max(x) - min(x))/2) {
        tolerance <- min(diff(x,1))/3
        info$step2 <- inputs$step2
        withProgress(message = 'Averaging the secondary series.',
                     detail = 'This may take a while ...', value = 0, {
                       w <- as.integer((max(x) - min(x))/inputs$step2)
                       if (info$format == 4) {
                         if (input$sigmas) {
                           averaged <- sapply(1:w, function(p) average(p, x = x, y1 = db2$original$y1, y2 = NULL, y3 = NULL, sy1 = db2$original$sy1, sy2 = NULL, sy3 = NULL, tol = tolerance, w = w, s = inputs$step2, second = T, sigmas = T), simplify = T)
                           db2$resampled <- na.omit(data.frame(x1 = averaged[1,], y1 = averaged[2,], sy1 = averaged[3,]))
                         } else {
                           averaged <- sapply(1:w, function(p) average(p, x = x, y1 = db2$original$y1, y2 = NULL, y3 = NULL, sy1 = NULL, sy2 = NULL, sy3 = NULL, tol = tolerance, w = w, s = inputs$step2, second = T, sigmas = F), simplify = T)
                           db2$resampled <- na.omit(data.frame(x1 = averaged[1,], y1 = averaged[2,], sy1 = rep(1, length(averaged[1,]))))
                         }
                       } else {
                         if (input$sigmas) {
                           averaged <- sapply(1:w, function(p) average(p, x = x, y1 = db2$original$y1, y2 = db2$original$y2, y3 = db2$original$y3, sy1 = db2$original$sy1, sy2 = db2$original$sy2, sy3 = db2$original$sy3, tol = tolerance, w = w, s = inputs$step2, second = T, sigmas = T), simplify = T)
                           db2$resampled <- na.omit(data.frame(x1 = averaged[1,], y1 = averaged[2,], y2 = averaged[3,], y3 = averaged[4,], sy1 = averaged[5,], sy2 = averaged[6,], sy3 = averaged[7,]))
                         } else {
                           averaged <- sapply(1:w, function(p) average(p, x = x, y1 = db2$original$y1, y2 = db2$original$y2, y3 = db2$original$y3, sy1 = NULL, sy2 = NULL, sy3 = NULL, tol = tolerance, w = w, s = inputs$step2, second = T, sigmas = F), simplify = T)
                           db2$resampled <- na.omit(data.frame(x1 = averaged[1,], y1 = averaged[2,], y2 = averaged[3,], y3 = averaged[4,], sy1 = rep(1, length(averaged[1,])), sy2 = rep(1, length(averaged[1,])), sy3 = rep(1, length(averaged[1,]))))
                         }
                         
                       }
                     })
        if (input$tunits == 1) {
          db2$resampled$x2 <- mjd2week(db2$resampled$x1)
          db2$resampled$x3 <- mjd2year(db2$resampled$x1)
        } else if (input$tunits == 2) {
          db2$resampled$x2 <- db2$resampled$x1
          db2$resampled$x3 <- week2year(db2$resampled$x1)
          db2$resampled$x1 <- week2mjd(db2$resampled$x1)
        } else if (input$tunits == 3) {
          db2$resampled$x3 <- db2$resampled$x1
          db2$resampled$x2 <- year2week(db2$resampled$x1)
          db2$resampled$x1 <- year2mjd(db2$resampled$x1)
        }
        info$db2 <- "resampled"
      } else {
        info$db2 <- "original"
        info$step2 <- NULL
        updateTextInput(session, inputId = "step2", value = "")
        showNotification(HTML("The resampling period of the secondary series is not valid.<br>Check input value."), action = NULL, duration = 10, closeButton = T, id = "bad_window", type = "error", session = getDefaultReactiveDomain())
      }
    } else {
      info$db2 <- "original"
      updateTextInput(session, inputId = "step2", value = "")
      info$step <- NULL
    }
  }, priority = 6)

  # Observe secondary series ####
  observeEvent(input$series2, {
    req(db1$original)
    file$secondary <- isolate(input$series2)
    url$file2 <- url$station2 <- NULL
    url$server2 <- ""
    if (isTruthy(url$logfile2)) {
      url$logfile2 <- info$log <- NULL
      session$sendCustomMessage("log", "")
    }
    updateRadioButtons(inputId = "optionSecondary", selected = 0)
    updateSelectInput(inputId = "server2", selected = "")
    updateSelectInput(inputId = "product2", selected = "")
    if (length(input$series2$datapath) == 1) {
      header <- readLines(input$series2$datapath, n = 1)
      if (grepl(".tenv3$", file$secondary$name, perl = T) && grepl("site YYMMMDD ", header, fixed = T)) {
        updateRadioButtons(inputId = "format2", selected = 3)
        info$format2 <- 3
      } else if (grepl(".pos$", file$secondary$name, perl = T) && grepl("PBO Station Position Time Series", header, fixed = T)) {
        updateRadioButtons(inputId = "format2", selected = 2)
        info$format2 <- 2
      } else if (grepl(".pos$", file$secondary$name, perl = T) && grepl("GeoCSV", header, fixed = T)) {
        updateRadioButtons(inputId = "format2", selected = 1)
        info$format2 <- 1
        url$server2 <- "EARTHSCOPE"
        url$file2 <- file$secondary
      } else if (grepl(".enu$", file$secondary$name, perl = T) && grepl("SPOTGINS SOLUTION [POSITION]", header, fixed = T)) {
        updateRadioButtons(inputId = "format2", selected = 1)
        info$format2 <- 1
        info$product2 <- "SPOTGINS_POS"
      } else if (grepl(".PLH$", file$secondary$name, perl = T) && grepl("^DGFI-TUM:", header, perl = T)) {
        updateRadioButtons(inputId = "format2", selected = 1)
        info$format2 <- 1
        url$server2 <- "SIRGAS"
        url$file2 <- file$secondary
      } else if (grepl(".series$", file$secondary$name, perl = T) && grepl(pattern = "\\.00\\s{2}\\d{4}\\s{1,2}\\d{1,2}\\s{1,2}\\d{1,2}\\s{1,2}\\d{1,2}\\s{1,2}\\d{1,2}\\s{1,2}\\d{1,2}$", header, perl = T)) {
        info$format2 <- 1
        url$server2 <- "JPL"
        url$file2 <- file$secondary
      } else if (as.numeric(input$format) < 4) {
        updateRadioButtons(inputId = "format2", selected = 1)
        info$format2 <- 1
      } else {
        updateRadioButtons(inputId = "format2", selected = 4)
        info$format2 <- 4
        disable("format2")
      }
    } else {
      if (as.numeric(input$format) < 4) {
        updateRadioButtons(inputId = "format2", selected = 1)
        info$format2 <- 1
        url$server2 <- "EOSTLS"
      } else {
        updateRadioButtons(inputId = "format2", selected = 4)
        info$format2 <- 4
        disable("format2")
      }
    }
    req(input$optionSecondary > 0)
    if (messages > 4) cat(file = stderr(), mySession, "From: observe secondary file\n")
    digest(2)
  }, priority = 8)
  
  observeEvent(c(input$format2), {
    req(db2[[info$db2]])
    if (info$format2 != input$format2) {
      updateRadioButtons(session, inputId = "optionSecondary", selected = 0)
    }
  }, priority = 6)
  
  observeEvent(c(input$separator2), {
    req(db2[[info$db2]])
    if (messages > 4) cat(file = stderr(), mySession, "From: observe secondary series (2)\n")
    digest(2)
  }, priority = 6)
  
  observeEvent(input$optionSecondary, {
    req(db1[[info$db1]])
    if (!isTruthy(db2[[info$db2]])) {
      if (messages > 4) cat(file = stderr(), mySession, "From: observe secondary option (2)\n")
      digest(2)
    }
    req(db2[[info$db2]])
    removeNotification("in_common")
    if (messages > 0) {
      if (input$optionSecondary == 0) {
        cat(file = stderr(), mySession, "Hidding secondary series", "\n")
        ids_info <- file$id1
      } else if (input$optionSecondary == 1) {
        cat(file = stderr(), mySession, "Showing secondary series", "\n")
        ids_info <- paste(file$id1,file$id2, sep = " & ")
      } else if (input$optionSecondary == 2) {
        cat(file = stderr(), mySession, "Subtracting secondary series", "\n")
        ids_info <- paste(file$id1,file$id2, sep = " - ")
      } else if (input$optionSecondary == 3) {
        cat(file = stderr(), mySession, "Averaging with secondary series", "\n")
        ids_info <- paste(file$id1,file$id2, sep = " + ")
      }
    }
    # merging primary and secondary series
    if (input$optionSecondary > 1) {
      table1 <- db1[[info$db1]]
      table2 <- db2[[info$db2]]
      if (isTruthy(input$ne)) {
        table2y_tmp <- table2$y2
        table2sy_tmp <- table2$sy2
        table2$y2 <- table2$y1
        table2$sy2 <- table2$sy1
        table2$y1 <- table2y_tmp
        table2$sy1 <- table2sy_tmp
      }
      if (min(diff(table1$x1)) <= 1 && min(diff(table2$x1)) <= 1) {
        delta <- as.numeric(names(sort(table(table1$x1 - floor(table1$x1))))) - as.numeric(names(sort(table(table2$x1 - floor(table2$x1)))))
        if (length(delta) == 1 && isTruthy(is.numeric(delta))) {
          if (delta != 0) {
            table2$x1 <- table2$x1 + delta
            showNotification(paste0("The time axis of the secondary series has been shifted by a constant ",delta," days"), action = NULL, duration = 10, closeButton = T, id = "time_shift", type = "warning", session = getDefaultReactiveDomain())
          }
        } else {
          # if (min(diff(table1$x1)) < min(diff(table2$x1))) {
          #   showNotification(HTML("The sampling of the primary series is not regular.<br>Consider using the \"Reduce sampling\" option to average the series to a constant sampling."), action = NULL, duration = 10, closeButton = T, id = "bad_time_shift", type = "error", session = getDefaultReactiveDomain())
          # } else {
          #   showNotification(HTML("The sampling of the secondary series is not regular.<br>It is not possible to correct the secondary series."), action = NULL, duration = 10, closeButton = T, id = "bad_time_shift", type = "error", session = getDefaultReactiveDomain())
          # }
          showNotification(HTML("The sampling of the primary series, or secodary series, or both, is not regular.<br>Consider using the \"Reduce sampling\" option to average the series to a constant sampling."), action = NULL, duration = 10, closeButton = T, id = "bad_time_shift", type = "error", session = getDefaultReactiveDomain())
        }
      } else {
        if (info$format != 4) {
          showNotification(HTML("The sampling of the primary and/or secondary series is larger than one day.<br>It is not possible to shift the secondary series."), action = NULL, duration = 10, closeButton = T, id = "bad_time_shift", type = "warning", session = getDefaultReactiveDomain())
        }
      }
      if (input$optionSecondary == 2) {
        if (info$format == 4) {
          table_common <- data.frame(within(merge(table1,table2,by = "x1"), {
            x2 <- x2.x
            x3 <- x3.x
            y1 <- y1.x - y1.y * inputs$scaleFactor
            sy1 <- sqrt(sy1.x^2 + (sy1.y * inputs$scaleFactor)^2)
            status1 <- status1
          })[,c("x1","x2","x3","y1","sy1","status1")])
        } else {
          table_common <- data.frame(within(merge(table1,table2,by = "x1"), {
            if (info$format2 == 4) {
              x2 <- x2.x
              x3 <- x3.x
              y1 <- y1.x - y1.y * inputs$scaleFactor
              y2 <- y2 - y1.y * inputs$scaleFactor
              y3 <- y3 - y1.y * inputs$scaleFactor
              sy1 <- sqrt(sy1.x^2 + (sy1.y * inputs$scaleFactor)^2)
              sy2 <- sqrt(sy2^2 + (sy1.y * inputs$scaleFactor)^2)
              sy3 <- sqrt(sy3^2 + (sy1.y * inputs$scaleFactor)^2)
              status1 <- status1
              status2 <- status2
              status3 <- status3
            } else {
              x2 <- x2.x
              x3 <- x3.x
              y1 <- y1.x - y1.y * inputs$scaleFactor
              y2 <- y2.x - y2.y * inputs$scaleFactor
              y3 <- y3.x - y3.y * inputs$scaleFactor
              sy1 <- sqrt(sy1.x^2 + (sy1.y * inputs$scaleFactor)^2)
              sy2 <- sqrt(sy2.x^2 + (sy2.y * inputs$scaleFactor)^2)
              sy3 <- sqrt(sy3.x^2 + (sy3.y * inputs$scaleFactor)^2)
              status1 <- status1
              status2 <- status2
              status3 <- status3
            }
          })[,c("x1","x2","x3","y1","y2","y3","sy1","sy2","sy3","status1","status2","status3")])
        }
      } else if (input$optionSecondary == 3) {
        if (info$format == 4) {
          table_common <- data.frame(within(merge(table1,table2,by = "x1"), {
            x2 <- x2.x
            x3 <- x3.x
            y1 <- (y1.x + y1.y * inputs$scaleFactor) / 2
            sy1 <- abs(sy1.x - sy1.y * inputs$scaleFactor)/2
            status1 <- status1
          })[,c("x1","x2","x3","y1","sy1")])
        } else {
          table_common <- data.frame(within(merge(table1,table2,by = "x1"), {
            if (info$format2 == 4) {
              x2 <- x2.x
              x3 <- x3.x
              y1 <- (y1.x + y1.y * inputs$scaleFactor) / 2
              y2 <- (y2 + y1.y * inputs$scaleFactor) / 2
              y3 <- (y3 + y1.y * inputs$scaleFactor) / 2
              sy1 <- sqrt(sy1.x^2 + (sy1.y * inputs$scaleFactor)^2)
              sy2 <- sqrt(sy2^2 + (sy1.y * inputs$scaleFactor)^2)
              sy3 <- sqrt(sy3^2 + (sy1.y * inputs$scaleFactor)^2)
              status1 <- status1
              status2 <- status2
              status3 <- status3
            } else {
              x2 <- x2.x
              x3 <- x3.x
              y1 <- (y1.x + y1.y * inputs$scaleFactor) / 2
              y2 <- (y2.x + y2.y * inputs$scaleFactor) / 2
              y3 <- (y3.x + y3.y * inputs$scaleFactor) / 2
              sy1 <- sqrt(sy1.x^2 + (sy1.y * inputs$scaleFactor)^2)
              sy2 <- sqrt(sy2.x^2 + (sy2.y * inputs$scaleFactor)^2)
              sy3 <- sqrt(sy3.x^2 + (sy3.y * inputs$scaleFactor)^2)
              status1 <- status1
              status2 <- status2
              status3 <- status3
            }
          })[,c("x1","x2","x3","y1","y2","y3","sy1","sy2","sy3","status1","status2","status3")])
        }
      }
      showNotification(paste0("There are ",length(table_common$x1)," epochs in common between the primary and secondary series (before excluding removed points)"), action = NULL, duration = 10, closeButton = T, id = "in_common", type = "warning", session = getDefaultReactiveDomain())
      if (nrow(table_common) > 0) {
        if (isTruthy(db1$merged$status1) && length(db1$merged$status1) == length(table_common$status1)) {
          table_common$status1 <- table_common$status1 + db1$merged$status1 > 1
        }
        if (isTruthy(db1$merged$status2) && length(db1$merged$status2) == length(table_common$status2)) {
          table_common$status2 <- table_common$status2 + db1$merged$status2 > 1
        }
        if (isTruthy(db1$merged$status3) && length(db1$merged$status3) == length(table_common$status3)) {
          table_common$status3 <- table_common$status3 + db1$merged$status3 > 1
        }
        db1$merged <- table_common
        info$db1 <- "merged"
        rm(table_common,table1,table2)
        if (input$eulerType > 1) {
          updateRadioButtons(session, inputId = "eulerType", selected = 0)
        }
      } else {
        updateRadioButtons(session, inputId = "optionSecondary", selected = 1)
      }
    } else if (isTruthy(info$last_optionSecondary) && info$last_optionSecondary > 1) {
      if (isTruthy(inputs$step) && isTruthy(db1$resampled$x1)) {
        info$db1 <- "resampled"  
      } else {
        info$db1 <- "original"
      }
    }
    # Updating plot ranges
    if (input$optionSecondary > 1 || (isTruthy(info$last_optionSecondary) && info$last_optionSecondary > 1)) {
      # setting new axis limits
      if (input$tab == 4) {
        info$minx <- min(db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]]$status1)],
                         db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]]$status2)],
                         db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]]$status3)])
        info$maxx <- max(db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]]$status1)],
                         db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]]$status2)],
                         db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]]$status3)])
      } else {
        info$minx <- min(db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]][[paste0("status", input$tab)]])])
        info$maxx <- max(db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]][[paste0("status", input$tab)]])])
      }
      ranges$x1 <- c(info$minx, info$maxx)
    }
    # Updating station IDs
    updateTextInput(session, inputId = "ids", value = ids_info)
    if (input$fitType == 2 && length(trans$mod) > 0 && length(trans$res) > 0) {
      info$run <- F
      trans$mod <- trans$mod0 <- NULL
      trans$res <- trans$res0 <- NULL
      trans$kalman <- trans$kalman0 <- NULL
      trans$kalman_unc <- trans$kalman_unc0 <- NULL
    }
    updateTextInput(session, "ObsError", value = "")
    updateTextInput(session, "waveformPeriod", value = "")
    if (isTruthy(input$correct_waveform)) {
      updateCheckboxInput(session, inputId = "correct_waveform", value = F)
    }
    info$last_optionSecondary <- input$optionSecondary
    # providing remote secondary series
    output$fileSeries2 <- renderUI({
      if (input$optionSecondary == 1 && isTruthy(url$station2) && isTruthy(file$secondary$newname)) {
        tags$a(href = sub(pattern = "www/", replacement = "", x = file$secondary$newpath), "Show secondary series file", title = "Open the file of the secondary series in a new tab", target = "_blank", download = file$secondary$newname)
      } else {
        NULL
      }
    })
  }, priority = 6)
  
  # Observe swap ####
  observeEvent(input$swap, {
    req(db1[[info$db1]], db2[[info$db2]])
    if (messages > 0) cat(file = stderr(), mySession, "Swapping primary and secondary series", "\n")
    # data base
    info$db1 <- info$db2 <- NULL
    info$db1 <- info$db2 <- "original"
    db1_tmp <- db1[[info$db1]]
    db2_tmp <- db2[[info$db2]]
    db1[[info$db1]] <- db2_tmp
    db2[[info$db1]] <- db1_tmp
    if (!isTruthy(db1[[info$db1]]$status1)) {
      db1[[info$db1]]$status1 <- rep(T, length(db1[[info$db1]]$x1))
      db1[[info$db1]]$status2 <- rep(T, length(db1[[info$db1]]$x1))
      db1[[info$db1]]$status3 <- rep(T, length(db1[[info$db1]]$x1))
    }
    db2[[info$db1]]$status1 <- NULL
    db2[[info$db1]]$status2 <- NULL
    db2[[info$db1]]$status3 <- NULL
    ranges$x1 <- range(db1[[info$db1]][[paste0("x",input$tunits)]], na.rm = T)
    if (isTruthy(inputs$scaleFactor)) {
      updateTextInput(session, inputId = "scaleFactor", value = 1/inputs$scaleFactor)
    }
    # file names
    file1 <- file$primary
    file2 <- file$secondary
    file$primary <- file2
    file$secondary <- file1
    session$sendCustomMessage("filename", file$primary$name)
    session$sendCustomMessage("filename2", file$secondary$name)
    log1 <- url$logfile
    log2 <- url$logfile2
    url$logfile <- log2
    url$logfile2 <- log1
    # station coordinates
    if ((((isTruthy(inputs$station_x) && isTruthy(inputs$station_y) && isTruthy(inputs$station_z)) && (isTruthy(inputs$station_lat) && isTruthy(inputs$station_lon))) &&
         ((isTruthy(inputs$station_x2) && isTruthy(inputs$station_y2) && isTruthy(inputs$station_z2)) && (isTruthy(inputs$station_lat2) && isTruthy(inputs$station_lon2))))
    ) {
      x1 <- inputs$station_x
      y1 <- inputs$station_y
      z1 <- inputs$station_z
      lat1 <- inputs$station_lat
      lon1 <- inputs$station_lon
      x2 <- inputs$station_x2
      y2 <- inputs$station_y2
      z2 <- inputs$station_z2
      lat2 <- inputs$station_lat2
      lon2 <- inputs$station_lon2
      updateTextInput(session, inputId = "station_x", value = x2)
      updateTextInput(session, inputId = "station_y", value = y2)
      updateTextInput(session, inputId = "station_z", value = z2)
      updateTextInput(session, inputId = "station_lat", value = lat2)
      updateTextInput(session, inputId = "station_lon", value = lon2)
      updateTextInput(session, inputId = "station_x2", value = x1)
      updateTextInput(session, inputId = "station_y2", value = y1)
      updateTextInput(session, inputId = "station_z2", value = z1)
      updateTextInput(session, inputId = "station_lat2", value = lat1)
      updateTextInput(session, inputId = "station_lon2", value = lon1)
    }
    # station IDs
    if (isTruthy(file$id1) && isTruthy(file$id2)) {
      id1 <- file$id1
      id2 <- file$id2
      ids_info <- paste(id2, id1, sep = " & ")
      updateTextInput(session, inputId = "ids", value = ids_info)
    }
  })

  # Observe ids ####
  observeEvent(c(inputs$ids, input$optionSecondary), {
    req(db1$original)
    update <- 0
    file$id1 <- toupper(trim(strsplit(as.character(inputs$ids), "-|\\&|\\+")[[1]][1]))
    if (!isTruthy(file$id1)) {
      if (isTruthy(url$station)) {
        if (url$server == "LOCAL") {
          file$id1 <- toupper(strsplit(as.character(url$station), "\\.|_|\\s|-|\\(")[[1]][1])
        } else {
          file$id1 <- toupper(url$station)
        }
      } else {
        file$id1 <- toupper(strsplit(as.character(input$series$name), "\\.|_|\\s|-|\\(")[[1]][1])
      }
      update <- 1
    }
    if (length(file$secondary) > 0) {
      file$id2 <- toupper(trim(strsplit(as.character(inputs$ids), "-|\\&|\\+")[[1]][2]))
      if (!isTruthy(file$id2)) {
        if (isTruthy(url$station2)) {
          if (url$server2 == "LOCAL") {
            file$id2 <- toupper(strsplit(as.character(url$station2), "\\.|_|\\s|-|\\(")[[1]][1])
          } else {
            file$id2 <- toupper(url$station2)
          }
        } else {
          if (isTruthy(input$series2$name)) {
            file$id2 <- toupper(strsplit(as.character(input$series2$name), "\\.|_|\\s|-|\\(")[[1]][1])
          }
        }
        update <- 1
      }
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
    if (messages > 0) cat(file = stderr(), mySession, "Deleting fit", "\n")
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
    if (isTruthy(input$correct_waveform)) {
      updateCheckboxInput(session, inputId = "correct_waveform", value = F)
    }
    if (isTruthy(input$model)) {
      updateCheckboxGroupInput(session, inputId = "model", label = "", choices = list("Linear","Polynomial","Sinusoidal","Offset","Exponential","Logarithmic"), selected = NULL, inline = T)
    }
    updateRadioButtons(session, inputId = "fitType", label = NULL, list("None" = 0, "LS" = 1, "KF" = 2), selected = 0, inline = T, choiceNames = NULL, choiceValues = NULL)
    updateTextInput(session, "E0", value = "")
    updateTextInput(session, "L0", value = "")
    if (isTruthy(input$white)) {
      updateCheckboxInput(session, inputId = "white", label = NULL, value = F)
    }
    if (isTruthy(input$flicker)) {
      updateCheckboxInput(session, inputId = "flicker", label = NULL, value = F)
    }
    if (isTruthy(input$randomw)) {
      updateCheckboxInput(session, inputId = "randomw", label = NULL, value = F)
    }
    if (isTruthy(input$powerl)) {
      updateCheckboxInput(session, inputId = "powerl", label = NULL, value = F)
    }
  }, priority = 5)

  # Observe fit type ####
  observeEvent(input$fitType, {
    req(trans$res)
    if (messages > 0) cat(file = stderr(), mySession, "Deleting fit", "\n")
    trans$res <- NULL
    trans$reserror <- NULL
    trans$results <- NULL
    trans$mod <- NULL
    trans$kalman <- NULL
    trans$equation <- NULL
    trans$mle <- F
    trans$verif <- NULL
    trans$pattern <- NULL
    if (isTruthy(input$correct_waveform)) {
      updateCheckboxInput(session, inputId = "correct_waveform", value = F)
    }
    updateTextInput(session, "ObsError", value = "")
    if (isTruthy(input$white)) {
      updateCheckboxInput(session, inputId = "white", label = NULL, value = F)
    }
    if (isTruthy(input$flicker)) {
      updateCheckboxInput(session, inputId = "flicker", label = NULL, value = F)
    }
    if (isTruthy(input$randomw)) {
      updateCheckboxInput(session, inputId = "randomw", label = NULL, value = F)
    }
    if (isTruthy(input$powerl)) {
      updateCheckboxInput(session, inputId = "powerl", label = NULL, value = F)
    }
    updateTextInput(session, "E0", value = "")
    updateTextInput(session, "TE0", value = "")
    updateTextInput(session, "L0", value = "")
    updateTextInput(session, "TL0", value = "")
  }, priority = 5)

  # Observe delete model ####
  observeEvent(input$model, {
    req(trans$res)
    if (!isTruthy(input$model)) {
      if (messages > 0) cat(file = stderr(), mySession, "Deleting model", "\n")
      info$run <- F
      trans$res <- NULL
      trans$reserror <- NULL
      trans$results <- NULL
      trans$mod <- NULL
      trans$kalman <- NULL
      trans$equation <- NULL
      trans$midas_vel <- NULL
      trans$midas_all <- NULL
    } else if (input$fitType == 2 && length(trans$kalman) > 0)  {
      if (setequal(input$model,trans$model_old)) {
        updateButton(session, inputId = "runKF", label = " Run KF", icon = icon("filter", class = NULL, lib = "font-awesome"), style = "default")
      } else {
        updateButton(session, inputId = "runKF", label = " Run KF", icon = icon("filter", class = NULL, lib = "font-awesome"), style = "danger")
      }
    }
    trans$mle <- F
    trans$verif <- NULL
    if (isTruthy(input$white)) {
      updateCheckboxInput(session, inputId = "white", label = NULL, value = F)
    }
    if (isTruthy(input$flicker)) {
      updateCheckboxInput(session, inputId = "flicker", label = NULL, value = F)
    }
    if (isTruthy(input$randomw)) {
      updateCheckboxInput(session, inputId = "randomw", label = NULL, value = F)
    }
    if (isTruthy(input$powerl)) {
      updateCheckboxInput(session, inputId = "powerl", label = NULL, value = F)
    }
    output$est.white <- renderUI({ NULL })
    output$est.flicker <- renderUI({ NULL })
    output$est.randomw <- renderUI({ NULL })
    output$est.powerl <- renderUI({ NULL })
    output$est.index <- renderUI({ NULL })
    output$est.mle <- renderUI({ NULL })
    output$est.unc <- renderUI({ NULL })
    trans$noise <- NULL
    trans$mle <- 0
  }, priority = 5)

  # Observe hide tabs ####
  observeEvent(input$format, {
    if (input$format == 4) { #1D
      output$tabName1 <- renderText({ "1D series" })
      hideTab(inputId = "tab", target = "2", session = getDefaultReactiveDomain())
      hideTab(inputId = "tab", target = "3", session = getDefaultReactiveDomain())
      hideTab(inputId = "tab", target = "4", session = getDefaultReactiveDomain())
      hideTab(inputId = "tab", target = "5", session = getDefaultReactiveDomain())
    } else {
      showTab(inputId = "tab", target = "2", session = getDefaultReactiveDomain())
      showTab(inputId = "tab", target = "3", session = getDefaultReactiveDomain())
      showTab(inputId = "tab", target = "4", session = getDefaultReactiveDomain())
      # showTab(inputId = "tab", target = "5", session = getDefaultReactiveDomain())
    }
  }, priority = 10)

  # Observe plotting ####
  observeEvent(input$plot, {
    req(file$primary)
    removeNotification("no_component")
    if (input$tab < 1) {
      if (messages > 0) cat(file = stderr(), mySession, "WARNING: tab number is", input$tab, "\n")
      showNotification("Please click on any component tab before plotting a coordiante series.", action = NULL, duration = 10, closeButton = T, id = "no_component", type = "error", session = getDefaultReactiveDomain())
      req(info$stop)
    }
    info$format <- input$format
    info$width <- isolate(session$clientData$output_plot1_width)
    if (messages > 4) cat(file = stderr(), mySession, "From: observe plotting\n")
    digest(1)
  }, priority = 4)
  
  # Observe overview ####
  observeEvent(input$plotAll, {
    req(db1[[info$db1]])
    if (input$format < 4) {
      if (messages > 0) cat(file = stderr(), mySession, "Overview plot", "\n")
      if (input$sunits == 1) {
        unit <- "(m)"
      } else if (input$sunits == 2) {
        unit <- "(mm)"
      } else {
        unit <- ""
      }
      if (input$symbol == 0) {
        symbol <- 'p'
      } else if (input$symbol == 1) {
        symbol <- 'l'
      } else if (input$symbol == 2) {
        symbol <- 'o'
      }
      if (input$tunits == 1) {
        x <- db1[[info$db1]]$x1
        x2 <- db2[[info$db2]]$x1
      } else if (input$tunits == 2) {
        x <- db1[[info$db1]]$x2
        x2 <- db2[[info$db2]]$x2
      } else if (input$tunits == 3) {
        x <- db1[[info$db1]]$x3
        x2 <- db2[[info$db2]]$x3
      }
      if (isTruthy(trans$plate) && input$eulerType == 2 && isTruthy(inputs$station_x) && isTruthy(inputs$station_y) && isTruthy(inputs$station_z)) {
        y1 <- db1[[info$db1]]$y1 - trans$plate[1]*(x - median(x, na.rm = T)) - median(db1[[info$db1]]$y1, na.rm = T)
        y2 <- db1[[info$db1]]$y2 - trans$plate[2]*(x - median(x, na.rm = T)) - median(db1[[info$db1]]$y2, na.rm = T)
        y3 <- db1[[info$db1]]$y3 - trans$plate[3]*(x - median(x, na.rm = T)) - median(db1[[info$db1]]$y3, na.rm = T)
      } else {
        y1 <- db1[[info$db1]]$y1
        y2 <- db1[[info$db1]]$y2
        y3 <- db1[[info$db1]]$y3
      }
      if (isTruthy(trans$gia) && input$giaType == 2) {
        y1 <- y1 - trans$gia[1]*(x - median(x, na.rm = T)) - median(y1, na.rm = T)
        y2 <- y2 - trans$gia[2]*(x - median(x, na.rm = T)) - median(y2, na.rm = T)
        y3 <- y3 - trans$gia[3]*(x - median(x, na.rm = T)) - median(y3, na.rm = T)
      }
      if (isTruthy(trans$plate2) && input$eulerType == 2 && isTruthy(inputs$station_x2) && isTruthy(inputs$station_y2) && isTruthy(inputs$station_z2)) {
        y12 <- db2[[info$db2]]$y1 - trans$plate2[1]*(x2 - median(x2, na.rm = T)) - median(db2[[info$db2]]$y1, na.rm = T)
        y22 <- db2[[info$db2]]$y2 - trans$plate2[2]*(x2 - median(x2, na.rm = T)) - median(db2[[info$db2]]$y2, na.rm = T)
        y32 <- db2[[info$db2]]$y3 - trans$plate2[3]*(x2 - median(x2, na.rm = T)) - median(db2[[info$db2]]$y3, na.rm = T)
      } else {
        y12 <- db2[[info$db2]]$y1
        y22 <- db2[[info$db2]]$y2
        y32 <- db2[[info$db2]]$y3
      }
      if (isTruthy(trans$gia2) && input$giaType == 2) {
        y12 <- y12 - trans$gia2[1]*(x2 - median(x2, na.rm = T)) - median(db2[[info$db2]]$y1, na.rm = T)
        y22 <- y22 - trans$gia2[2]*(x2 - median(x2, na.rm = T)) - median(db2[[info$db2]]$y2, na.rm = T)
        y32 <- y32 - trans$gia2[3]*(x2 - median(x2, na.rm = T)) - median(db2[[info$db2]]$y3, na.rm = T)
      }
      y12 <- y12 * inputs$scaleFactor
      y22 <- y22 * inputs$scaleFactor
      y32 <- y32 * inputs$scaleFactor
      if (all(db1[[info$db1]]$sy1 == 1)) {
        sy1 <- rep(0, length(db1[[info$db1]]$sy1))
        sy2 <- rep(0, length(db1[[info$db1]]$sy2))
        sy3 <- rep(0, length(db1[[info$db1]]$sy3))
      } else {
        sy1 <- db1[[info$db1]]$sy1
        sy2 <- db1[[info$db1]]$sy2
        sy3 <- db1[[info$db1]]$sy3
      }
      if (all(db2[[info$db2]]$sy1 == 1)) {
        sy12 <- rep(0, length(db2[[info$db2]]$sy1))
        sy22 <- rep(0, length(db2[[info$db2]]$sy2))
        sy32 <- rep(0, length(db2[[info$db2]]$sy3))
      } else {
        sy12 <- db2[[info$db2]]$sy1
        sy22 <- db2[[info$db2]]$sy2
        sy32 <- db2[[info$db2]]$sy3 
      }
      valid1 <- db1[[info$db1]]$status1 & !is.na(db1[[info$db1]]$status1)
      valid2 <- db1[[info$db1]]$status2 & !is.na(db1[[info$db1]]$status2)
      valid3 <- db1[[info$db1]]$status3 & !is.na(db1[[info$db1]]$status3)
      fileout <- paste0("www/tempFiles/",file$primary$name,".png")
      if (ranges$x1[1] > info$minx || ranges$x1[2] < info$maxx) {
        x.range <- ranges$x1
      } else {
        x.range <- c(min(x[valid1], x[valid2], x[valid3], na.rm = T), max(x[valid1], x[valid2], x[valid3], na.rm = T))
      }
      ragg::agg_png(filename = fileout, width = info$width, height = 800, pointsize = 25)
      par(mai = c(1, 2, 1, 1))
      layout(mat = matrix(data = c(1,2,3), nrow = 3, ncol = 1))
      ## East ####
      par(mai = c(0.3, 1.2, 0.3, 0.6))
      y.range <- range(y1[valid1][x[valid1] >= x.range[1] & x[valid1] <= x.range[2]], na.rm = T)
      if (isTruthy(db2[[info$db2]]) && input$optionSecondary == 1) {
        if (isTruthy(input$sameScale)) {
          x2.common <- x2[x2 > x.range[1] & x2 < x.range[2]]
          y2.common <- y12[x2 > x.range[1] & x2 < x.range[2]]
          half <- abs(y.range[1] - mean(y.range))
          middle <- ifelse(isTruthy(y2.common), median(y2.common), 0)
          y2.range <- c(middle - half, middle + half)
          if (length(x) == 0 || length(x2.common) == 0) {
            # NA
          } else if (x2.common[1] > x.range[2]) {
            # NA
          } else if (x.range[1] > x2.common[length(x2.common)]) {
            # NA
          } else {
            tie1 <- head(sort(sapply(x, function(i) min(abs(x2.common - i))), index.return = T)$ix, 100)
            tie2 <- head(sort(sapply(x2.common, function(i) min(abs(x - i))), index.return = T)$ix, 100)
            tie1 <- tie1[1:min(length(tie1),length(tie2))]
            tie2 <- tie2[1:min(length(tie1),length(tie2))]
            pointsBias <- median(y1[valid1][tie1] - y2.common[tie2], na.rm = T)
            y2.range <- y2.range + (y.range[1] - y2.range[1]) - pointsBias
          }
        } else if (isTruthy(input$same_axis)) {
          y2.range <- y.range
        } else {
          y2.range <- range(y12[x2 >= x.range[1] & x2 <= x.range[2]])
        }
        plot(x2, y12, type = symbol, pch = 20, col = SARIcolors[3], xlab = NA, yaxt = "n", xaxt = "n", ylab = NA, xlim = x.range, ylim = y2.range)
        if (isTruthy(input$sigmas)) {
          color <- SARIcolors[3]
          alfa <- 0.5
          shade <- adjustcolor(color, alpha.f = alfa)
          ba <- y12 + sy12
          bb <- y12 - sy12
          polygon(c(x2, rev(x2)), c(ba, rev(bb)), col = shade, border = NA)
        }
        axis(side = 1, labels = F, tick = F)
        axis(side = 2, labels = F, tick = F)
        axis(side = 4, at = NULL, labels = T, tick = T, outer = F)
        par(new = T)
      }
      mini <- min(y1[valid1], na.rm = T)
      maxi <- max(y1[valid1], na.rm = T)
      if ((abs(mini) > 99 || abs(maxi) > 99) && abs(maxi - mini) < 99) {
        if (mini < 0) {
          const <- as.integer(round(maxi))
          ylab <- paste(gsub("component","",info$components[1]),intToUtf8(8210),abs(const),unit)
        } else {
          const <- as.integer(round(mini))
          ylab <- paste(gsub("component","",info$components[1]),"+",abs(const),unit)
        }
      } else {
        const <- 0
        ylab <- gsub("component","",paste(info$components[1], unit))
      }
      plot(x[valid1], y1[valid1], type = symbol, pch = 20, xlab = NA, xaxt = "n", yaxt = "n", ylab = ylab, xlim = x.range, ylim = y.range)
      p <- par("usr")[3:4]
      pout <- base::pretty(p - const)
      pin <- pout + const
      axis(2, at = pin, labels = pout)
      if (isTruthy(input$sigmas)) {
        color <- SARIcolors[1]
        alfa <- 0.2
        shade <- adjustcolor(color, alpha.f = alfa)
        ba <- y1 + sy1
        bb <- y1 - sy1
        polygon(c(x[valid1], rev(x[valid1])), c(ba[valid1], rev(bb[valid1])), col = shade, border = NA)
      }
      axis(side = 1, labels = F, tick = T)
      if (input$eulerType == 1 && length(trans$plate[!is.na(trans$plate)]) == 3) {
        xx <- median(x[valid1][x[valid1] > x.range[1] & x[valid1] < x.range[2]], na.rm = T)
        yy <- median(y1[valid1][x[valid1] > x.range[1] & x[valid1] < x.range[2]], na.rm = T)
        centerx <- which(abs(x[valid1] - xx) == min(abs(x[valid1] - xx)))[1]
        centery <- which(abs(y1[valid1] - yy) == min(abs(y1[valid1] - yy)))[1]
        lines(c(x[valid1][1],x[valid1][length(x[valid1])]),c(y1[valid1][centery] + trans$plate[1]*(x[valid1][1] - x[valid1][centerx]), y1[valid1][centery] + trans$plate[1]*(x[valid1][length(x[valid1])] - x[valid1][centerx])), col = SARIcolors[4], lwd = 3)
      }
      # North ####
      par(mai = c(0.3, 1.2, 0.1, 0.6))
      y.range <- range(y2[valid2][x[valid2] >= x.range[1] & x[valid2] <= x.range[2]], na.rm = T)
      if (isTruthy(db2[[info$db2]]) && input$optionSecondary == 1) {
        if (isTruthy(input$sameScale)) {
          x2.common <- x2[x2 > x.range[1] & x2 < x.range[2]]
          y2.common <- y22[x2 > x.range[1] & x2 < x.range[2]]
          half <- abs(y.range[1] - mean(y.range))
          middle <- ifelse(isTruthy(y2.common), median(y2.common), 0)
          y2.range <- c(middle - half, middle + half)
          if (length(x) == 0 || length(x2.common) == 0) {
            # NA
          } else if (x2.common[1] > x.range[2]) {
            # NA
          } else if (x.range[1] > x2.common[length(x2.common)]) {
            # NA
          } else {
            tie1 <- head(sort(sapply(x, function(i) min(abs(x2.common - i))), index.return = T)$ix, 100)
            tie2 <- head(sort(sapply(x2.common, function(i) min(abs(x - i))), index.return = T)$ix, 100)
            tie1 <- tie1[1:min(length(tie1),length(tie2))]
            tie2 <- tie2[1:min(length(tie1),length(tie2))]
            pointsBias <- median(y2[valid2][tie1] - y2.common[tie2])
            y2.range <- y2.range + (y.range[1] - y2.range[1]) - pointsBias
          }
        } else if (isTruthy(input$same_axis)) {
          y2.range <- y.range
        } else {
          y2.range <- range(y22[x2 >= x.range[1] & x2 <= x.range[2]])
        }
        plot(x2, y22, type = symbol, pch = 20, col = SARIcolors[3], xlab = NA, yaxt = "n", xaxt = "n", ylab = NA, xlim = x.range, ylim = y2.range)
        if (isTruthy(input$sigmas)) {
          color <- SARIcolors[3]
          alfa <- 0.5
          shade <- adjustcolor(color, alpha.f = alfa)
          ba <- y22 + sy22
          bb <- y22 - sy22
          polygon(c(x2, rev(x2)), c(ba, rev(bb)), col = shade, border = NA)
        }
        axis(side = 1, labels = F, tick = F)
        axis(side = 2, labels = F, tick = F)
        axis(side = 4, at = NULL, labels = T, tick = T, outer = F)
        par(new = T)
      }
      mini <- min(y2[valid2], na.rm = T)
      maxi <- max(y2[valid2], na.rm = T)
      if ((abs(mini) > 99 || abs(maxi) > 99) && abs(maxi - mini) < 99) {
        if (mini < 0) {
          const <- as.integer(round(maxi))
          ylab <- paste(gsub("component","",info$components[2]),intToUtf8(8210),abs(const),unit)
        } else {
          const <- as.integer(round(mini))
          ylab <- paste(gsub("component","",info$components[2]),"+",abs(const),unit)
        }
      } else {
        const <- 0
        ylab <- gsub("component","",paste(info$components[2], unit))
      }
      plot(x[valid2], y2[valid2], type = symbol, pch = 20, xlab = "", xaxt = "n", yaxt = "n", ylab = ylab, xlim = x.range, ylim = y.range)
      p <- par("usr")[3:4]
      pout <- base::pretty(p - const)
      pin <- pout + const
      axis(2, at = pin, labels = pout)
      if (isTruthy(input$sigmas)) {
        color <- SARIcolors[1]
        alfa <- 0.2
        shade <- adjustcolor(color, alpha.f = alfa)
        ba <- y2 + sy2
        bb <- y2 - sy2
        polygon(c(x[valid1], rev(x[valid1])), c(ba[valid1], rev(bb[valid1])), col = shade, border = NA)
      }
      axis(side = 1, labels = F, tick = T)
      if (input$eulerType == 1 && length(trans$plate[!is.na(trans$plate)]) == 3) {
        xx <- median(x[valid2][x[valid2] > x.range[1] & x[valid2] < x.range[2]], na.rm = T)
        yy <- median(y2[valid2][x[valid2] > x.range[1] & x[valid2] < x.range[2]], na.rm = T)
        centerx <- which(abs(x[valid2] - xx) == min(abs(x[valid2] - xx)))[1]
        centery <- which(abs(y2[valid2] - yy) == min(abs(y2[valid2] - yy)))[1]
        lines(c(x[valid2][1],x[valid2][length(x[valid2])]),c(y2[valid2][centery] + trans$plate[2]*(x[valid2][1] - x[valid2][centerx]), y2[valid2][centery] + trans$plate[2]*(x[valid2][length(x[valid2])] - x[valid2][centerx])), col = SARIcolors[4], lwd = 3)
      }
      # Up ####
      par(mai = c(1.2, 1.2, 0.1, 0.6))
      y.range <- range(y3[valid3][x[valid3] >= x.range[1] & x[valid3] <= x.range[2]], na.rm = T)
      if (isTruthy(db2[[info$db2]]) && input$optionSecondary == 1) {
        if (isTruthy(input$sameScale)) {
          x2.common <- x2[x2 > x.range[1] & x2 < x.range[2]]
          y2.common <- y32[x2 > x.range[1] & x2 < x.range[2]]
          half <- abs(y.range[1] - mean(y.range))
          middle <- ifelse(isTruthy(y2.common), median(y2.common), 0)
          y2.range <- c(middle - half, middle + half)
          if (length(x) == 0 || length(x2.common) == 0) {
            # NA
          } else if (x2.common[1] > x.range[2]) {
            # NA
          } else if (x.range[1] > x2.common[length(x2.common)]) {
            # NA
          } else {
            tie1 <- head(sort(sapply(x, function(i) min(abs(x2.common - i))), index.return = T)$ix, 100)
            tie2 <- head(sort(sapply(x2.common, function(i) min(abs(x - i))), index.return = T)$ix, 100)
            tie1 <- tie1[1:min(length(tie1),length(tie2))]
            tie2 <- tie2[1:min(length(tie1),length(tie2))]
            pointsBias <- median(y3[valid3][tie1] - y2.common[tie2])
            y2.range <- y2.range + (y.range[1] - y2.range[1]) - pointsBias
          }
        } else if (isTruthy(input$same_axis)) {
          y2.range <- y.range
        } else {
          y2.range <- range(y32[x2 >= x.range[1] & x2 <= x.range[2]])
        }
        plot(x2, y32, type = symbol, pch = 20, col = SARIcolors[3], xlab = NA, yaxt = "n", xaxt = "n", ylab = NA, xlim = x.range, ylim = y2.range)
        if (isTruthy(input$sigmas)) {
          color <- SARIcolors[3]
          alfa <- 0.5
          shade <- adjustcolor(color, alpha.f = alfa)
          ba <- y32 + sy32
          bb <- y32 - sy32
          polygon(c(x2, rev(x2)), c(ba, rev(bb)), col = shade, border = NA)
        }
        axis(side = 1, labels = F, tick = F)
        axis(side = 2, labels = F, tick = F)
        axis(side = 4, at = NULL, labels = T, tick = T, outer = F)
        par(new = T)
      }
      mini <- min(y3[valid3], na.rm = T)
      maxi <- max(y3[valid3], na.rm = T)
      if ((abs(mini) > 99 || abs(maxi) > 99) && abs(maxi - mini) < 99) {
        if (mini < 0) {
          const <- as.integer(round(maxi))
          ylab <- paste(gsub("component","",info$components[3]),intToUtf8(8210),abs(const),unit)
        } else {
          const <- as.integer(round(mini))
          ylab <- paste(gsub("component","",info$components[3]),"+",abs(const),unit)
        }
      } else {
        const <- 0
        ylab <- gsub("component","",paste(info$components[3], unit))
      }
      plot(x[valid3], y3[valid3], type = symbol, pch = 20, yaxt = "n", xlab = info$tunits.label, ylab = ylab, xlim = x.range, ylim = y.range)
      p <- par("usr")[3:4]
      pout <- base::pretty(p - const)
      pin <- pout + const
      axis(2, at = pin, labels = pout)
      if (isTruthy(input$sigmas)) {
        color <- SARIcolors[1]
        alfa <- 0.2
        shade <- adjustcolor(color, alpha.f = alfa)
        ba <- y3 + sy3
        bb <- y3 - sy3
        polygon(c(x[valid1], rev(x[valid1])), c(ba[valid1], rev(bb[valid1])), col = shade, border = NA)
      }
      if (input$giaType == 1 && length(trans$gia[!is.na(trans$gia)]) == 3) {
        xx <- median(x[valid3][x[valid3] > x.range[1] & x[valid3] < x.range[2]], na.rm = T)
        yy <- median(y3[valid3][x[valid3] > x.range[1] & x[valid3] < x.range[2]], na.rm = T)
        centerx <- which(abs(x[valid3] - xx) == min(abs(x[valid3] - xx)))[1]
        centery <- which(abs(y3[valid3] - yy) == min(abs(y3[valid3] - yy)))[1]
        lines(c(x[valid3][1],x[valid3][length(x[valid3])]),c(y3[valid3][centery] + trans$gia[3]*(x[valid3][1] - x[valid3][centerx]), y3[valid3][centery] + trans$gia[3]*(x[valid3][length(x[valid3])] - x[valid3][centerx])), col = SARIcolors[4], lwd = 3)
      }
      dev.off()
      info$overview <- T
      js$showPopup(sub(pattern = "www/", replacement = "", x = fileout))
    }
  })

  # Observe removing points manual ####
  observeEvent(input$remove, {
    req(db1[[info$db1]])
    removeNotification("no_toggle")
    removeNotification("no_point_manual")
    if (messages > 0) cat(file = stderr(), mySession, "Removing points, manually", "\n")
    excluding_plot <- excluding_plotres <- NULL
    if (isTruthy(input$plot_brush)) {
      brush1 <- input$plot_brush
      series <- data.frame(x = trans$x0[!is.na(trans$y0)], y = trans$y0[!is.na(trans$y0)])
    } else if (isTruthy(input$plot41_brush)) {
      brush1 <- input$plot41_brush
      x <- db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]]$status1)]
      y <- db1[[info$db1]]$y1[!is.na(db1[[info$db1]]$status1)]
      series <- data.frame(x = x, y = y)
    } else if (isTruthy(input$plot42_brush)) {
      brush1 <- input$plot42_brush
      x <- db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]]$status2)]
      y <- db1[[info$db1]]$y2[!is.na(db1[[info$db1]]$status2)]
      series <- data.frame(x = x, y = y)
    } else if (isTruthy(input$plot43_brush)) {
      brush1 <- input$plot43_brush
      x <- db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]]$status3)]
      y <- db1[[info$db1]]$y3[!is.na(db1[[info$db1]]$status3)]
      series <- data.frame(x = x, y = y)
    }
    brush2 <- NULL
    if (isTruthy(input$res_brush) && length(trans$res) > 0) {
      residuals <- data.frame(x = trans$x, y = trans$res)
      brush2 <- input$res_brush
    } else if (isTruthy(input$vondrak_brush) && length(trans$filterRes) > 0) {
      residuals <- data.frame(x = trans$x, y = trans$filterRes)
      brush2 <- input$vondrak_brush
    }
    if (isTruthy(brush1) || isTruthy(brush2)) {
      if (length(brush1) > 0) {
        excluding_plot <- brushedPoints(series, brush1, xvar = "x", yvar = "y", allRows = T)
      }
      if (length(brush2) > 0) {
        excluding_res <- brushedPoints(residuals, brush2, xvar = "x", yvar = "y", allRows = T)
        if (input$fitType == 2 && length(trans$mod) > 0 && length(trans$res) > 0) {
          series_kf <- data.frame(x = trans$x0[db1[[info$db1]]$status.kf], y = trans$res[db1[[info$db1]]$status.kf])
          excluding_plotres_kf <- merge(series_kf, excluding_res, by = "x", all.x = T)
          excluding_plotres_kf$selected_ <- sapply(1:length(excluding_plotres_kf$x), function(x) if (isTRUE(excluding_plotres_kf$selected_[x])) T else F)
        }
        excluding_plotres <- merge(series, excluding_res, by = "x", all.x = T)
        excluding_plotres$selected_ <- sapply(1:length(excluding_plotres$x), function(x) if (isTRUE(excluding_plotres$selected_[x])) T else F)
      }
      if ((isTruthy(excluding_plot$selected_) && sum(excluding_plot$selected_) > 0) || (isTruthy(excluding_plotres$selected_) && sum(excluding_plotres$selected_) > 0)) {
        if (isTruthy(input$remove3D)) {
          if (length(brush1) > 0) {
            if (isTruthy(input$permanent)) {
              db1[[info$db1]]$status1[excluding_plot$selected_] <- NA
              # setting new axis limits
              if (isTruthy(input$fullSeries) && input$optionSecondary < 2) {
                # show all points from primary & secondary series
                info$minx <- min(db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]]$status1)], db2[[info$db2]][[paste0("x",input$tunits)]])
                info$maxx <- max(db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]]$status1)], db2[[info$db2]][[paste0("x",input$tunits)]])
              } else {
                # show all points from primary series only
                info$minx <- min(db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]]$status1)])
                info$maxx <- max(db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]]$status1)])
              }
              ranges$x1 <- c(info$minx, info$maxx)
              updateCheckboxInput(session, inputId = "permanent", value = F)
            } else {
              db1[[info$db1]]$status1 <- xor(db1[[info$db1]]$status1, excluding_plot$selected_)
            }
          }
          if (length(brush2) > 0) {
            if (isTruthy(input$permanent)) {
              db1[[info$db1]]$status1[excluding_plotres$selected_] <- NA
              # setting new axis limits
              if (isTruthy(input$fullSeries) && input$optionSecondary < 2) {
                # show all points from primary & secondary series
                info$minx <- min(db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]]$status1)], db2[[info$db2]][[paste0("x",input$tunits)]])
                info$maxx <- max(db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]]$status1)], db2[[info$db2]][[paste0("x",input$tunits)]])
              } else {
                # show all points from primary series only
                info$minx <- min(db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]]$status1)])
                info$maxx <- max(db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]]$status1)])
              }
              ranges$x1 <- c(info$minx, info$maxx)
              updateCheckboxInput(session, inputId = "permanent", value = F)
            } else {
              db1[[info$db1]]$status1 <- xor(db1[[info$db1]]$status1, excluding_plotres$selected_)
            }
          }
          db1[[info$db1]]$status2 <- db1[[info$db1]]$status3 <- db1[[info$db1]]$status1
        } else {
          if (input$tab == 1 || isTruthy(input$plot41_brush) || is.null(input$tab)) {
            if (length(brush1) > 0) {
              if (isTruthy(input$permanent)) {
                db1[[info$db1]]$status1[excluding_plot$selected_] <- NA
                updateCheckboxInput(session, inputId = "permanent", value = F)
              } else {
                db1[[info$db1]]$status1 <- xor(db1[[info$db1]]$status1, excluding_plot$selected_)
              }
            }
            if (length(brush2) > 0) {
              if (isTruthy(input$permanent)) {
                db1[[info$db1]]$status1[excluding_plotres$selected_] <- NA
                updateCheckboxInput(session, inputId = "permanent", value = F)
              } else {
                db1[[info$db1]]$status1 <- xor(db1[[info$db1]]$status1, excluding_plotres$selected_)
              }
            }
          } else if (input$tab == 2 || isTruthy(input$plot42_brush)) {
            if (length(brush1) > 0) {
              if (isTruthy(input$permanent)) {
                db1[[info$db1]]$status2[excluding_plot$selected_] <- NA
                updateCheckboxInput(session, inputId = "permanent", value = F)
              } else {
                db1[[info$db1]]$status2 <- xor(db1[[info$db1]]$status2, excluding_plot$selected_)
              }
            }
            if (length(brush2) > 0) {
              if (isTruthy(input$permanent)) {
                db1[[info$db1]]$status2[excluding_plotres$selected_] <- NA
                updateCheckboxInput(session, inputId = "permanent", value = F)
              } else {
                db1[[info$db1]]$status2 <- xor(db1[[info$db1]]$status2, excluding_plotres$selected_)
              }
            }
          } else if (input$tab == 3 || isTruthy(input$plot43_brush)) {
            if (length(brush1) > 0) {
              if (isTruthy(input$permanent)) {
                db1[[info$db1]]$status3[excluding_plot$selected_] <- NA
                updateCheckboxInput(session, inputId = "permanent", value = F)
              } else {
                db1[[info$db1]]$status3 <- xor(db1[[info$db1]]$status3, excluding_plot$selected_)
              }
            }
            if (length(brush2) > 0) {
              if (isTruthy(input$permanent)) {
                db1[[info$db1]]$status3[excluding_plotres$selected_] <- NA
                updateCheckboxInput(session, inputId = "permanent", value = F)
              } else {
                db1[[info$db1]]$status3 <- xor(db1[[info$db1]]$status3, excluding_plotres$selected_)
              }
            }
          }
        }
      } else {
        showNotification(HTML("No point was selected to be removed manually.<br>Check the selected area."), action = NULL, duration = 10, closeButton = T, id = "no_point_manual", type = "warning", session = getDefaultReactiveDomain())
      }
    }
  }, priority = 4)

  # Observe removing points auto ####
  observeEvent(input$removeAuto, {
    req(db1[[info$db1]])
    removeNotification("bad_normalised_threshold")
    removeNotification("bad_threshold")
    removeNotification("no_point_auto")
    if (messages > 0) cat(file = stderr(), mySession, "Removing points, automatically", "\n")
    if (input$fitType == 2 && length(trans$mod) > 0 && length(trans$res) > 0) {
      series_kf <- data.frame(x = trans$x0[db1[[info$db1]]$status.kf], y = trans$res[db1[[info$db1]]$status.kf])
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
        if (messages > 0) cat(file = stderr(), mySession, "Limit normalized residual", inputs$thresholdResN, "\n")
        if (input$fitType == 2 && length(trans$mod) > 0 && length(trans$res) > 0) {
          excluding_plot_kf <- abs(joint_kf$res/joint_kf$sy) > abs(inputs$thresholdResN)
          excluding_kf <- excluding_plot_kf
        }
        excluding_plot <- abs(joint$res/joint$sy) > abs(inputs$thresholdResN)
        excluding <- excluding_plot
      } else {
        showNotification(HTML("The normalised residual threshold is not numeric.<br>Check the input value."), action = NULL, duration = 10, closeButton = T, id = "bad_normalised_threshold", type = "warning", session = getDefaultReactiveDomain())
      }
    }
    if (nchar(input$thresholdRes) > 0) {
      if (!is.na(inputs$thresholdRes)) {
        if (abs(inputs$thresholdRes) < min(abs(residuals))) {
          showNotification(HTML("The residual threshold will remove all data from the residual series.<br>Check the input value."), action = NULL, duration = 10, closeButton = T, id = "bad_threshold", type = "error", session = getDefaultReactiveDomain())
        } else {
          if (messages > 0) cat(file = stderr(), mySession, "Limit absolute residual", inputs$thresholdRes, "\n")
          if (input$fitType == 2 && length(trans$mod) > 0 && length(trans$res) > 0) {
            excluding_res_kf <- abs(joint_kf$res) > abs(inputs$thresholdRes)
            excluding_kf <- excluding_kf + excluding_res_kf > 0
          }
          excluding_res <- abs(joint$res) > abs(inputs$thresholdRes)
          excluding <- excluding + excluding_res > 0
        }
      } else {
        showNotification(HTML("The residual threshold is not numeric.<br>Check the input value."), action = NULL, duration = 10, closeButton = T, id = "bad_threshold", type = "error", session = getDefaultReactiveDomain())
      }
    }
    if (isTruthy(excluding) && sum(excluding) > 0) {
      if (min(sum(db1[[info$db1]]$status1, na.rm = T), sum(db1[[info$db1]]$status2, na.rm = T), sum(db1[[info$db1]]$status3, na.rm = T), na.rm = T) - sum(excluding, na.rm = T) < 2) {
        showNotification(HTML("The residual threshold will remove all data from the residual series.<br>Check the input value."), action = NULL, duration = 10, closeButton = T, id = "bad_threshold", type = "error", session = getDefaultReactiveDomain())
      } else {
        if (isTruthy(input$remove3D)) {
          db1[[info$db1]]$status1 <- xor(db1[[info$db1]]$status1, excluding)
          if (isTruthy(input$permanent)) {
            db1[[info$db1]]$status1[excluding] <- NA
            # setting new axis limits
            if (isTruthy(input$fullSeries) && input$optionSecondary < 2) {
              # show all points from primary & secondary series
              info$minx <- min(db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]]$status1)], db2[[info$db2]][[paste0("x",input$tunits)]])
              info$maxx <- max(db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]]$status1)], db2[[info$db2]][[paste0("x",input$tunits)]])
            } else {
              # show all points from primary series only
              info$minx <- min(db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]]$status1)])
              info$maxx <- max(db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]]$status1)])
            }
            ranges$x1 <- c(info$minx, info$maxx)
            updateCheckboxInput(session, inputId = "permanent", value = F)
          }
          db1[[info$db1]]$status2 <- db1[[info$db1]]$status3 <- db1[[info$db1]]$status1
        } else {
          if (input$tab == 1 || is.null(input$tab)) {
            db1[[info$db1]]$status1 <- xor(db1[[info$db1]]$status1, excluding)
            if (isTruthy(input$permanent)) {
              db1[[info$db1]]$status1[excluding] <- NA
              updateCheckboxInput(session, inputId = "permanent", value = F)
            }
          } else if (input$tab == 2) {
            db1[[info$db1]]$status2 <- xor(db1[[info$db1]]$status2, excluding)
            if (isTruthy(input$permanent)) {
              db1[[info$db1]]$status2[excluding] <- NA
              updateCheckboxInput(session, inputId = "permanent", value = F)
            }
          } else if (input$tab == 3) {
            db1[[info$db1]]$status3 <- xor(db1[[info$db1]]$status3, excluding)
            if (isTruthy(input$permanent)) {
              db1[[info$db1]]$status3[excluding] <- NA
              updateCheckboxInput(session, inputId = "permanent", value = F)
            }
          }
        }
      }
    } else {
      showNotification(HTML("No point was selected to be removed automatically.<br>Check the input threshold."), action = NULL, duration = 10, closeButton = T, id = "no_point_auto", type = "warning", session = getDefaultReactiveDomain())
    }
  }, priority = 4)
  
  # Observe truncate ####
  observeEvent(c(inputs$cutStart, inputs$cutEnd), {
    req(db1[[info$db1]], input$cut)
    removeNotification("bad_cut")
    if (isTruthy(inputs$cutStart) || isTruthy(inputs$cutEnd)) {
      if (messages > 0) cat(file = stderr(), mySession, "Cutting series:", inputs$cutStart, "-", inputs$cutEnd, "\n")
      start <- ifelse(isTruthy(inputs$cutStart), inputs$cutStart, trans$x[1])
      end <- ifelse(isTruthy(inputs$cutEnd), inputs$cutEnd, trans$x[length(trans$x)])
      start <- ifelse(start < trans$x[1], trans$x[1], start)
      end <- ifelse(end > trans$x[length(trans$x)], trans$x[length(trans$x)], end)
      if (end <= start) {
        shinyjs::delay(500, showNotification(HTML("The End epoch or the end of the series is equal or smaller than the Start epoch or the start of the series.<br>Check the truncation values."), action = NULL, duration = 10, closeButton = T, id = "bad_cut", type = "error", session = getDefaultReactiveDomain()))
        req(info$stop)
      }
      if (isTruthy(input$permanent)) {
        if (isTruthy(inputs$cutStart) && isTruthy(inputs$cutEnd)) {
          # NA
        } else if (isTruthy(inputs$cutStart)) {
          end <- trans$x0[length(trans$x0)]
        } else if (isTruthy(inputs$cutEnd)) {
          start <- trans$x0
        }
        if (isTruthy(input$remove3D)) {
          db1[[info$db1]]$status1[trans$x0 < start | trans$x0 > end] <- NA
          db1[[info$db1]]$status2[trans$x0 < start | trans$x0 > end] <- NA
          db1[[info$db1]]$status3[trans$x0 < start | trans$x0 > end] <- NA
        } else {
          if (input$tab == 1) {
            db1[[info$db1]]$status1[trans$x0 < start | trans$x0 > end] <- NA
          } else if (input$tab == 2) {
            db1[[info$db1]]$status2[trans$x0 < start | trans$x0 > end] <- NA
          } else if (input$tab == 3) {
            db1[[info$db1]]$status3[trans$x0 < start | trans$x0 > end] <- NA
          }
        }
        # setting new axis limits
        if (isTruthy(input$fullSeries) && input$optionSecondary < 2) {
          # show all points from primary & secondary series
          info$minx <- min(db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]][[paste0("status", input$tab)]])], db2[[info$db2]][[paste0("x",input$tunits)]])
          info$maxx <- max(db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]][[paste0("status", input$tab)]])], db2[[info$db2]][[paste0("x",input$tunits)]])
        } else {
          # show all points from primary series only
          info$minx <- min(db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]][[paste0("status", input$tab)]])])
          info$maxx <- max(db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]][[paste0("status", input$tab)]])])
        }
        ranges$x1 <- c(info$minx, info$maxx)
        updateCheckboxInput(session, inputId = "permanent", value = F)
      } else {
        if (isTruthy(input$remove3D)) {
          db1[[info$db1]]$status1[(trans$x0 < start | trans$x0 > end) & !is.na(db1[[info$db1]]$status1)] <- F
          db1[[info$db1]]$status2[(trans$x0 < start | trans$x0 > end) & !is.na(db1[[info$db1]]$status2)] <- F
          db1[[info$db1]]$status3[(trans$x0 < start | trans$x0 > end) & !is.na(db1[[info$db1]]$status3)] <- F
        } else {
          if (input$tab == 1) {
            db1[[info$db1]]$status1[(trans$x0 < start | trans$x0 > end) & !is.na(db1[[info$db1]]$status1)] <- F
          } else if (input$tab == 2) {
            db1[[info$db1]]$status2[(trans$x0 < start | trans$x0 > end) & !is.na(db1[[info$db1]]$status2)] <- F
          } else if (input$tab == 3) {
            db1[[info$db1]]$status3[(trans$x0 < start | trans$x0 > end) & !is.na(db1[[info$db1]]$status3)] <- F
          }
        }
      }
      updateTextInput(session, inputId = "cutStart", value = "")
      updateTextInput(session, inputId = "cutEnd", value = "")
    }
  })

  # Observe restore removed ####
  observeEvent(input$delete_excluded, {
    req(db1$original)
    if (messages > 0) cat(file = stderr(), mySession, "Restoring points", "\n")
    if (isTruthy(input$remove3D)) {
      db1[[info$db1]]$status1[!db1[[info$db1]]$status1 & !is.na(db1[[info$db1]]$status1)] <- T
      db1[[info$db1]]$status2 <- db1[[info$db1]]$status3 <- db1[[info$db1]]$status1
    } else {
      if (input$tab == 1 || is.null(input$tab)) {
        db1[[info$db1]]$status1[!db1[[info$db1]]$status1 & !is.na(db1[[info$db1]]$status1)] <- T
      } else if (input$tab == 2) {
        db1[[info$db1]]$status2[!db1[[info$db1]]$status2 & !is.na(db1[[info$db1]]$status2)] <- T
      } else if (input$tab == 3) {
        db1[[info$db1]]$status3[!db1[[info$db1]]$status3 & !is.na(db1[[info$db1]]$status3)] <- T
      }
    }
    updateTextInput(session, "thresholdRes", value = "")
    updateTextInput(session, "thresholdResN", value = "")
    updateTextInput(session, "cutStart", value = "")
    updateTextInput(session, "cutEnd", value = "")
  }, priority = 4)

  # Observe station.info ####
  observeEvent(c(file$sinfo, input$series, input$series2, input$optionSecondary, file$id1, file$id2), {
    req(file$sinfo,file$id1)
    if (messages > 0) cat(file = stderr(), mySession, "Reading station.info", "\n")
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
    info$sinfo_years <- info$sinfo <- ReadInfo(id1,id2,file$sinfo)
    output$station.info <- renderUI({
      tags$a(href = basename(file$sinfo$datapath), "Show station.info", title = "Open the station.info file in a new tab", target = "_blank")
    })
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
  observeEvent(c(file$soln, input$series, input$series2, input$optionSecondary, file$id1, file$id2), {
    req(file$soln,file$id1)
    if (messages > 0) cat(file = stderr(), mySession, "Reading soln", "\n")
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
    info$soln_years <- info$soln <- ReadSoln(id1,id2,file$soln)
    output$solnFile <- renderUI({
      tags$a(href = basename(file$soln$datapath), "Show soln file", title = "Open the soln file in a new tab", target = "_blank")
    })
  }, priority = 4)
  observeEvent(c(input$printSoln),{
    req(file$primary, file$soln)
    output$changes_ant1so <- output$changes_ant2so <- output$changes_ant3so <- renderText({
      if (length(info$soln) > 0) {
        sprintf("Discontinuities from soln file at\n%s",paste(unlist(info$soln), collapse = ", "))
      } else {
        NULL
      }
    })
  })

  # Observe custom ####
  observeEvent(c(file$custom, input$series, input$series2, input$optionSecondary, file$id1, file$id2), {
    req(file$custom,file$id1)
    if (messages > 0) cat(file = stderr(), mySession, "Reading custom", "\n")
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
    info$custom_years <- info$custom <- ReadCustom(id1,id2,file$custom)
    output$customFile <- renderUI({
      tags$a(href = basename(file$custom$datapath), "Show custom file", title = "Open the custom file in a new tab", target = "_blank")
    })
  }, priority = 4)
  observeEvent(c(input$printCustom),{
    req(file$primary, file$custom)
    output$changes_ant1c <- output$changes_ant2c <- output$changes_ant3c <- renderText({
      if (length(info$custom) > 0) {
        sprintf("Changes from custom file at\n%s",paste(unlist(info$custom), collapse = ", "))
      } else {
        NULL
      }
    })
  })

  # Observe sitelog ####
  observeEvent(c(file$sitelog, file$primary$logfile, file$secondary$logfile), {
    if (messages > 0) cat(file = stderr(), mySession, "Reading sitelog", "\n")
    if (isTruthy(file$sitelog)) {
      info$log <- ReadLog(file$sitelog$datapath)
      datapath <- file$sitelog$datapath
    } else if (isTruthy(file$primary$logfile)) {
      info$log <- ReadLog(file$primary$logfile)
      datapath <- file$primary$logfile
    } else if (isTruthy(file$secondary$logfile)) {
      info$log <- ReadLog(file$secondary$logfile)
      datapath <- file$secondary$logfile
    }
    if (isTruthy(datapath)) {
      output$sitelog <- renderUI({
        tags$a(href = sub(pattern = "www/", replacement = "", x = datapath), "Show log file", title = "Open the log file in a new tab", target = "_blank")
      })
    }
    if (any(!sapply(info$log, is.null))) {
      info$log_years <- info$log 
    } else {
      info$log <- NULL
    }
  }, priority = 1)
  observeEvent(c(input$printLog),{
    req(db1$original)
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
    if (messages > 0) cat(file = stderr(), mySession, "Reset all", "\n")
    if (session$clientData$url_search != "") {
      if (session$clientData$url_pathname == "/") {
        path <- ""
      } else {
        path <- session$clientData$url_pathname
      }
      if (session$clientData$url_port == "") {
        port <- ""
      } else {
        port <- paste0(":",session$clientData$url_port)
      }
      jscode <- paste0("window.location.href = '",session$clientData$url_protocol,"//",session$clientData$url_hostname,path,port,"';")
      runjs(jscode)
      req(info$stop)
    }
    reset("side-panel")
    reset("main-panel")
    updateCollapse(session, id = "menu", open = 1, close = c(2,3,4,5,6))
    db1[[info$db1]] <- NULL
    db2[[info$db2]] <- NULL
    for (i in names(db1)) {
      db1[[i]] <- NULL
      db2[[i]] <- NULL
    }
    updateRadioButtons(session, inputId = "tunits", selected = character(0))
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
    trans$model_old <- NULL
    info$tol <- NULL
    trans$midas_vel <- NULL
    trans$midas_all <- NULL
    trans$mle <- F
    trans$verif <- NULL
    trans$pattern <- NULL
    trans$unc <- NULL
    trans$spectra_old <- NULL
    trans$plate <- NULL
    trans$gia <- NULL
    info$points <- NULL
    info$log <- NULL
    info$rangex <- NULL
    info$sampling <- NULL
    info$errorbars <- T
    info$last_optionSecondary <- NULL
    info$trendRef <- NULL
    info$PolyRef <- NULL
    info$periodRef <- NULL
    info$noLS <- F
    info$tunits.known1 <- F
    info$tunits.known2 <- F
    url$file <- NULL
    url$file2 <- NULL
    url$logfile <- NULL
    url$logfile2 <- NULL
    url$server <- NULL
    url$server2 <- NULL
    url$station <- NULL
    url$station2 <- NULL
    updateTextInput(session, "waveformPeriod", value = "")
    updateCheckboxInput(session, inputId = "waveform", label = NULL, value = F)
    if (isTruthy(input$white)) {
      updateCheckboxInput(session, inputId = "white", label = NULL, value = F)
    }
    if (isTruthy(input$flicker)) {
      updateCheckboxInput(session, inputId = "flicker", label = NULL, value = F)
    }
    if (isTruthy(input$randomw)) {
      updateCheckboxInput(session, inputId = "randomw", label = NULL, value = F)
    }
    if (isTruthy(input$powerl)) {
      updateCheckboxInput(session, inputId = "powerl", label = NULL, value = F)
    }
    updateTextInput(session, "ObsError", value = "")
    updateTabsetPanel(session, inputId = "tab", selected = "1")
    updateSliderInput(session, inputId = "segmentLength", value = 10)
    output$offsetFound <- renderUI({
      NULL
    })
    enable("neuenu")
    enable("server1")
    info$components <- c("", "", "", "", "")
    output$tabName1 <<- renderText({ "Visualization panel" })
    output$tabName2 <<- renderText({ info$components[2] })
    output$tabName3 <<- renderText({ info$components[3] })
    output$tabName4 <<- renderText({ info$components[4] })
    output$tabName5 <<- renderText({ info$components[5] })
    output$fileSeries1 <- renderUI({
      tags$a(href = "SPOTGINS_CRAL00FRA.enu", "Show file example", targe = "_blank")
    })
    output$fileSeries2 <- renderUI({
      NULL
    })
    output$sitelog <- renderUI({
      tags$a(href = "cral00fra_20231110.log", "Show file example", target = "_blank")
    })
    output$station.info <- renderUI({
      tags$a(href = "station.info", "Show file example", target = "_blank")
    })
    output$customFile <- renderUI({
      tags$a(href = "steps.txt", "Show file example", target = "_blank")
    })
    output$solnFile <- renderUI({
      tags$a(href = "soln.snx", "Show file example", target = "_blank")
    })
    shinyjs::hide("zoomin1")
    shinyjs::hide("zoomin2")
    shinyjs::hide("zoomin3")
    updateButton(session, inputId = "runKF", label = " Run KF", icon = icon("filter", class = NULL, lib = "font-awesome"), style = "default")
    runjs('overview.close();')
  })

  # Observe hide buttons ####
  observeEvent(c(input$tab, trans$filter, trans$res, trans$y, inputs$step, input$optionSecondary), {
    if (input$tab == 6) {
      showTab(inputId = "tab", target = "8", select = F, session = getDefaultReactiveDomain())
      hideTab(inputId = "tab", target = "7", session = getDefaultReactiveDomain())
    } else {
      hideTab(inputId = "tab", target = "8", session = getDefaultReactiveDomain())
      if (length(trans$y) > 0 && 
          (  length(trans$filter) > 0 || 
             length(trans$res) > 0 || 
             (nchar(inputs$step) > 0 && !is.na(inputs$step) && inputs$step > 0) || 
             input$optionSecondary > 1 ||
             (input$eulerType == 2 && length(trans$plate) > 0)  )
      ) {
        showTab(inputId = "tab", target = "7", select = F, session = getDefaultReactiveDomain())
      } else {
        hideTab(inputId = "tab", target = "7", session = getDefaultReactiveDomain())
      }
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
  
  # Observe type of noise color ####
  observeEvent(input$typeColor, {
    if (input$typeColor == 1) {
      updateTextInput(session, inputId = "verif_pl", value = "")
      updateTextInput(session, inputId = "verif_k", value = "")  
    } else if (input$typeColor == 2) {
      updateTextInput(session, inputId = "verif_fl", value = "")
      updateTextInput(session, inputId = "verif_rw", value = "")  
    }
  })

  # Observe noise model ####
  observeEvent(c(input$white, input$flicker, input$randomw, input$powerl, input$noise_unc, info$points), {
    removeNotification("no_mle")
    removeNotification("too_long")
    removeNotification("warning_no_slope")
    trans$noise <- NULL
    trans$mle <- 0
    output$est.white <- renderUI({ NULL })
    output$est.flicker <- renderUI({ NULL })
    output$est.randomw <- renderUI({ NULL })
    output$est.powerl <- renderUI({ NULL })
    output$est.index <- renderUI({ NULL })
    output$est.mle <- renderUI({ NULL })
    output$est.unc <- renderUI({ NULL })
    # updating LS trend error
    if (isTruthy(trans$unc)) {
      if (isTruthy(trans$LScoefs[2,2])) {
        trans$LScoefs[2,2] <- trans$unc
      }
      if (isTruthy(trans$results$coefficients[2,2])) {
        trans$results$coefficients[2,2] <- trans$unc
        trans$results$coefficients[2,3] <- abs(trans$results$coefficients[2,1]) / trans$results$coefficients[2,2]
        trans$results$coefficients[2,4] <- 2 * pt(abs(trans$results$coefficients[2,3]), trans$results$df , lower.tail = F)[2]
      }
    }
    # estimating MLE duration
    if (isTruthy(input$mle)) {
      info$timeMLE <- 0
      if (isTruthy(info$points) && info$points > 0) {
        if (input$white) {
          if (input$flicker) {
            if (input$randomw) { # WH + FN + RW
              if (isTruthy(input$noise_unc)) {
                info$timeMLE <- ceiling(0.0000000212*info$points^2.7310170576)
              } else {
                info$timeMLE <- ceiling(0.0000000163*info$points^2.6969406699)
              }
            } else {  # WH + FN
              if (isTruthy(input$noise_unc)) {
                info$timeMLE <- ceiling(0.0000000190*info$points^2.7218809680)
              } else {
                info$timeMLE <- ceiling(0.0000000194*info$points^2.6777146385)
              }
            }
          } else if (input$randomw) { # WH + RW
            if (isTruthy(input$noise_unc)) {
              info$timeMLE <- ceiling(0.0000000193*info$points^2.7102027977)
            } else {
              info$timeMLE <- ceiling(0.0000000153*info$points^2.6965310604)
            }
          } else if (input$powerl) { # WH + PL
            if (!isTruthy(trans$slope)) {
              showNotification("It is highly recommended to estimate the power spectrum of the residual series before fitting a PL noise model.", action = NULL, duration = 10, closeButton = T, id = "warning_no_slope", type = "warning", session = getDefaultReactiveDomain())
            }
            if (isTruthy(trans$slope) && trans$slope < 0 && trans$slope > -4) {
              if (isTruthy(input$noise_unc)) {
                info$timeMLE <- ceiling(0.0000000283*info$points^2.8574872668)
              } else {
                info$timeMLE <- ceiling(0.0000000182*info$points^2.8093811786)
              }
            } else {
              if (isTruthy(input$noise_unc)) {
                info$timeMLE <- ceiling(0.0000000029*info$points^3.1797797014)
              } else {
                info$timeMLE <- ceiling(0.0000000006*info$points^3.3238998899)
              }
            }
            # info$timeMLE <- info$timeMLE * 3 # if using the Nelder & Mead method
          } else { # WH
            info$timeMLE <- 2
          }
        } else if (input$flicker) {
          if (input$randomw) { # FN + RW
            if (isTruthy(input$noise_unc)) {
              info$timeMLE <- ceiling(0.0000000041*info$points^2.9122849861)
            } else {
              info$timeMLE <- ceiling(0.0000000022*info$points^2.9418404761)
            }
          } else { # FN
            info$timeMLE <- ceiling(0.0000000335*info$points^2.3767196923)
          }
        } else if (input$randomw) { # RW
          info$timeMLE <- ceiling(0.0000000502*info$points^2.3299799835)
        } else if (input$powerl) { # PL
          if (!isTruthy(trans$slope)) {
            showNotification("It is highly recommended to estimate the power spectrum of the residual series before fitting a PL noise model.", action = NULL, duration = 10, closeButton = T, id = "warning_no_slope", type = "warning", session = getDefaultReactiveDomain())
          }
          if (isTruthy(trans$slope) && trans$slope < 0 && trans$slope > -4) {
            if (isTruthy(input$noise_unc)) {
              info$timeMLE <- ceiling(0.0000000137*info$points^2.8892566790)
            } else {
              info$timeMLE <- ceiling(0.0000000084*info$points^2.8777970916)
            }
          } else {
            if (isTruthy(input$noise_unc)) {
              info$timeMLE <- ceiling(0.0000000209*info$points^2.8807048341)
            } else {
              info$timeMLE <- ceiling(0.0000000145*info$points^2.8835059302)
            }
            # info$timeMLE <- info$timeMLE * 3 # thanks to the Nelder & Mead method
          }
        }
        if (info$timeMLE < 0) {
          info$timeMLE <- 10
        }
      }
      if (info$timeMLE > 0) {
        if (info$timeMLE < 60) {
          showNotification(paste0("The noise model fit may take about ", ceiling(info$timeMLE), " sec"), action = NULL, duration = 10, closeButton = T, id = "timeWillTake", type = "warning", session = getDefaultReactiveDomain())
        } else {
          showNotification(paste0("The noise model fit may take about ", ceiling(info$timeMLE/60), " min"), action = NULL, duration = 10, closeButton = T, id = "timeWillTake", type = "warning", session = getDefaultReactiveDomain())
        }
        if (info$timeMLE > 14*60 && !isTruthy(info$local)) {
          showNotification(HTML("The noise model analysis will likely not finish before the server closes the session for lack of user activity.<br>Consider averaging the series for a faster analysis."), action = NULL, duration = 10, closeButton = T, id = "too_long", type = "error", session = getDefaultReactiveDomain())
        }
      }
    }
  })
  
  # Observe clicks on multiple plots ####
  observeEvent(input$plot41_1click, {
    inputs$plot4_1click$x <- input$plot41_1click$x
    inputs$plot4_1click$y <- input$plot41_1click$y
  })
  observeEvent(input$plot42_1click, {
    inputs$plot4_1click$x <- input$plot42_1click$x
    inputs$plot4_1click$y <- input$plot42_1click$y
  })
  observeEvent(input$plot43_1click, {
    inputs$plot4_1click$x <- input$plot43_1click$x
    inputs$plot4_1click$y <- input$plot43_1click$y
  })


  # Functions ####
  digest <- function(series) {
    removeNotification("bad_series")
    removeNotification("removing_NA")
    removeNotification("bad_x")
    removeNotification("bad_window")
    removeNotification("unknown_components")
    removeNotification("time_shift")
    removeNotification("parsing_url1")
    lat <- lon <- lat2 <- lon2 <- NULL
    ## primary series ####
    if (series == 1) {
      if (messages > 0) cat(file = stderr(), mySession, "Reading primary series file", "\n")
      if (isTruthy(url$file) && isTruthy(url$station)) {
        fileName <- file$primary$name
        output$fileSeries1 <- renderUI({
          tags$a(href = sub(pattern = "www/", replacement = "", x = file$primary$datapath), "Show series file", title = "Open the file of the primary series in a new tab", target = "_blank", download = fileName)
        })
      } else {
        fileName <- input$series$name
        output$fileSeries1 <- renderUI({
          NULL
        })
      }
      printInfo("PLOT  ")
      # Setting column separation
      if (input$separator == "1") {
        sep <- ""
      } else if (input$separator == "2") {
        sep <- ","
      } else if (input$separator == "3") {
        sep <- ";"
      }
      if (isTruthy(info$format)) {
        if (info$format == 4) {
          updateTabsetPanel(session, inputId = "tab", selected = "1")
        } else {
          updateTabsetPanel(session, inputId = "tab", selected = "4")
          info$tab <- 4
        }
      }
      # Getting primary series
      table <- NULL
      if (isTruthy(url$file)) {
        filein <- file$primary$datapath
        table <- extract_table(filein,sep,info$format,as.numeric(inputs$epoch),as.numeric(inputs$variable),as.numeric(inputs$errorBar),F,url$server,1)
      } else {
        filein <- input$series$datapath
        table <- extract_table(filein,sep,info$format,as.numeric(inputs$epoch),as.numeric(inputs$variable),as.numeric(inputs$errorBar),F,"",1)
      }
      # Checking series values and time order
      if (!is.null(table)) {
        table <- table[order(table$x1),]
        if (any(diff(table$x1) <= 0)) {
          bad_x <- which(diff(table$x1) <= 0)
          showNotification(HTML(paste("Negative or null increment in abscissa (probably 2 or more points at the same day epoch).<br>Check points", paste(bad_x, collapse = " "))), action = NULL, duration = 10, closeButton = T, id = "bad_x", type = "error", session = getDefaultReactiveDomain())
          req(info$stop)
        }
        table <- table[order(table$x2),]
        if (any(diff(table$x2) <= 0)) {
          bad_x <- which(diff(table$x2) <= 0)
          showNotification(HTML(paste("Negative or null increment in abscissa (probably 2 or more points at the same week epoch).<br>Check points", paste(bad_x, collapse = " "))), action = NULL, duration = 10, closeButton = T, id = "bad_x", type = "error", session = getDefaultReactiveDomain())
          req(info$stop)
        }
        table <- table[order(table$x3),]
        if (any(diff(table$x3) <= 0)) {
          bad_x <- which(diff(table$x3) <= 0)
          showNotification(HTML(paste("Negative or null increment in abscissa (probably 2 or more points at the same year epoch).<br>Check points", paste(bad_x, collapse = " "))), action = NULL, duration = 10, closeButton = T, id = "bad_x", type = "error", session = getDefaultReactiveDomain())
          req(info$stop)
        }
        table <- table[!is.infinite(rowSums(table)),]
        if (anyNA(table)) {
          table <- na.omit(table)
          showNotification(HTML("The primary series contains records with NA/NaN values.<br>These records were removed"), action = NULL, duration = 10, closeButton = T, id = "removing_NA", type = "warning", session = getDefaultReactiveDomain())
        }
      } else {
        shinyjs::delay(500, {
          showNotification("The input series is empty or has wrong format.", action = NULL, duration = 10, closeButton = T, id = "bad_series", type = "error", session = getDefaultReactiveDomain())
          req(info$stop)
        })
      }
      if (is.null(table)) {
        showNotification(HTML("All records in the series were removed.<br> Check the series format."), action = NULL, duration = 10, closeButton = T, id = "bad_series", type = "error", session = getDefaultReactiveDomain())
        req(info$stop)
      }
      # Extracting station coordinates
      coordinates <- as.numeric(extract_coordinates(filein,info$format,url$server,info$product1,url$station,skip,sep))
      lat <- coordinates[4]
      lon <- coordinates[5]
      shinyjs::delay(100, updateTextInput(session, inputId = "station_x", value = coordinates[1]))
      shinyjs::delay(100, updateTextInput(session, inputId = "station_y", value = coordinates[2]))
      shinyjs::delay(100, updateTextInput(session, inputId = "station_z", value = coordinates[3]))
      shinyjs::delay(100, updateTextInput(session, inputId = "station_lat", value = coordinates[4]))
      shinyjs::delay(100, updateTextInput(session, inputId = "station_lon", value = coordinates[5]))
      # Fixing NEU/ENU if known
      if (isTruthy(url$server)) {
        disable("neuenu")
      }
      # Setting series units if known
      if (isTruthy(url$server)) {
        if (url$server == "EPOS" || url$server == "EOSTLS" || url$server == "PSMSL") {
          updateRadioButtons(session, inputId = "sunits", selected = 2)
        } else {
          updateRadioButtons(session, inputId = "sunits", selected = 1)
        }
      } else if (info$format == 2 || info$format == 3) {
        updateRadioButtons(session, inputId = "sunits", selected = 1)
      } else {
        if (input$sunits == 0) {
          showNotification(HTML("Unknown units of the series.<br>If necessary, the series units can be set on the left panel."), action = NULL, duration = 10, closeButton = T, id = NULL, type = "warning", session = getDefaultReactiveDomain())
        }
      }
      # Setting plot limits
      if (!isTruthy(input$tunits) && isTruthy(info$tunits.known1)) {
        x <- table$x3
        info$tunits.last <- 3
      } else {
        if (input$tunits == 1) {
          x <- table$x1
        } else if (input$tunits == 2) {
          x <- table$x2
        } else if (input$tunits == 3) {
          x <- table$x3
        }
        info$tunits.last <- input$tunits
      }
      info$minx <- min(x, na.rm = T)
      info$maxx <- max(x, na.rm = T)
      ranges$x1 <- c(info$minx, info$maxx)
      # Setting new tab names if necessary
      if (info$format == 1) { #NEU/ENU
        if (isTruthy(url$server) && url$server != "LOCAL") {
          info$components <- c("East component", "North component", "Up component", "3D", "Residuals")
          output$tabName1 <<- renderText({ info$components[1] })
          output$tabName2 <<- renderText({ info$components[2] })
          output$tabName3 <<- renderText({ info$components[3] })
          output$tabName4 <<- renderText({ info$components[4] })
          output$tabName5 <<- renderText({ info$components[5] })
        } else {
          extension <- tolower(rev(strsplit(as.character(file$primary$name), ".", fixed = T)[[1]])[1])
          if (isTruthy(extension) && (extension == "neu" || extension == "enu")) {
            info$components <- c("East component", "North component", "Up component", "3D", "Residuals")
            output$tabName1 <<- renderText({ info$components[1] })
            output$tabName2 <<- renderText({ info$components[2] })
            output$tabName3 <<- renderText({ info$components[3] })
            output$tabName4 <<- renderText({ info$components[4] })
            output$tabName5 <<- renderText({ info$components[5] })
          }
        }
        hideTab(inputId = "tab", target = "5", session = getDefaultReactiveDomain())
      } else if (info$format == 4) { #1D
        output$tabName1 <- renderText({ "Series" })
      } else { #PBO & NGL
        info$components <- c("East component", "North component", "Up component", "3D", "Residuals")
        output$tabName1 <<- renderText({ info$components[1] })
        output$tabName2 <<- renderText({ info$components[2] })
        output$tabName3 <<- renderText({ info$components[3] })
        output$tabName4 <<- renderText({ info$components[4] })
        output$tabName5 <<- renderText({ info$components[5] })
        hideTab(inputId = "tab", target = "5", session = getDefaultReactiveDomain())
      }
      if (info$components[1] != "East component" && info$format != 4) {
        info$components <- c("1st component", "2nd component", "3rd component", "3D", "Residuals")
        showNotification(HTML("Unknown coordinate components in the primary series.<br>Assuming a ENU column format."), action = NULL, duration = 10, closeButton = T, id = "unknown_components", type = "warning", session = getDefaultReactiveDomain())
      }
      # all good
      info$db1 <- "original"
      db1$original <- as.data.frame(table)
      db1$original$status1 <- db1$original$status2 <- db1$original$status3 <- rep(T, length(table$x1))
    ## secondary series ####
    } else if (series == 2) {
      if (messages > 0) cat(file = stderr(), mySession, "Reading secondary series file", "\n")
      # Setting column separation
      if (input$separator2 == "1") {
        sep2 <- ""
      } else if (input$separator2 == "2") {
        sep2 <- ","
      } else if (input$separator2 == "3") {
        sep2 <- ";"
      }
      # Getting secondary series
      table2 <- NULL
      if (isTruthy(url$file2) && isTruthy(url$station2)) {
        files <- file$secondary
      } else {
        files <- input$series2
      }
      table_stack <- NULL
      num <- dim(as.matrix(files$datapath))[1]
      if (url$server2 == "EOSTLS" && num > 1) {
        showNotification(HTML("Stacking the secondary series into one series."), action = NULL, duration = NULL, closeButton = T, id = "stacking", type = "warning", session = getDefaultReactiveDomain())
        if (any(grepl("ECCO", input$product2)) && any(grepl("TUGO", input$product2))) {
          showNotification(HTML("WARNING: the TUGO and ECCO models have similar forcing, which will be considered twice in the secondary series."), action = NULL, duration = 15, closeButton = T, id = NULL, type = "warning", session = getDefaultReactiveDomain())
        }
      }
      for (i in 1:num) {
        table2 <- extract_table(files$datapath[i],sep2,info$format2,as.numeric(inputs$epoch2),as.numeric(inputs$variable2),as.numeric(inputs$errorBar2),input$ne,url$server2,2)
        removeNotification(paste0("parsing_url2_",i))
        removeNotification("parsing_url2")
        # starting EOSTLS series at epoch .0, except for daily series
        if (url$server2 == "EOSTLS" && num > 1 && any(unique(table2$x1 %% 1) == 0)) {
          while (table2$x1[1] %% 1 > 0) {
            table2 <- table2[-1,]
          }
        }
        # Resampling the secondary series if there are more than one
        if (dim(as.matrix(files$datapath))[1] > 1) {
          if (nchar(input$step2) > 0 && is.na(inputs$step2)) {
            if (is.na(as.numeric(input$step2))) {
              info$step2 <- NULL
              showNotification(HTML("The resampling period of the secondary series is not numeric.<br>Check input value."), action = NULL, duration = 10, closeButton = T, id = "bad_window", type = "error", session = getDefaultReactiveDomain())
            }
          } else if (isTruthy(inputs$step2)) {
            if (input$tunits == 1) {
              x <- table2$x1
            } else if (input$tunits == 2) {
              x <- table2$x2
            } else if (input$tunits == 3) {
              x <- table2$x3
            }
            if (inputs$step2 >= 2*min(diff(x,1)) && inputs$step2 <= (max(x) - min(x))/2) {
              tolerance <- min(diff(x,1))/3
              info$step2 <- inputs$step2
              withProgress(message = paste('Averaging the', files$name[i], 'series.'),
                           detail = 'This may take a while ...', value = 0, {
                             w <- as.integer((max(x) - min(x))/inputs$step2)
                             if (info$format2 == 4) {
                               averaged <- sapply(1:w, function(p) average(p, x = x, y1 = table2$y1, y2 = NULL, y3 = NULL, sy1 = table2$sy1, sy2 = NULL, sy3 = NULL, tol = tolerance, w = w, s = inputs$step2, second = T, sigmas = F), simplify = T)
                               table2 <- data.frame(x1 = averaged[1,], y1 = averaged[2,], sy1 = rep(1, length(table2$x)))
                             } else {
                               if (url$server2 == "EOSTLS" || url$server2 == "EPOS") {
                                 averaged <- sapply(1:w, function(p) average(p, x = x, y1 = table2$y1, y2 = table2$y2, y3 = table2$y3, sy1 = table2$sy1, sy2 = table2$sy2, sy3 = table2$sy3, tol = tolerance, w = w, s = inputs$step2, second = T, sigmas = F), simplify = T)
                                 table2 <- data.frame(x1 = averaged[1,], y1 = averaged[2,], y2 = averaged[3,], y3 = averaged[4,], sy1 = rep(1, length(averaged[1,])), sy2 = rep(1, length(averaged[1,])), sy3 = rep(1, length(averaged[1,])))
                               } else {
                                 averaged <- sapply(1:w, function(p) average(p, x = x, y1 = table2$y1, y2 = table2$y2, y3 = table2$y3, sy1 = table2$sy1, sy2 = table2$sy2, sy3 = table2$sy3, tol = tolerance, w = w, s = inputs$step2, second = T, sigmas = T), simplify = T)
                                 table2 <- data.frame(x1 = averaged[1,], y1 = averaged[2,], y2 = averaged[3,], y3 = averaged[4,], sy1 = averaged[5,], sy2 = averaged[6,], sy3 = averaged[7,])
                               }
                             }
                           })
              table2 <- na.omit(table2)
              if (input$tunits == 1) {
                table2$x2 <- mjd2week(table2$x1)
                table2$x3 <- mjd2year(table2$x1)
              } else if (input$tunits == 2) {
                table2$x2 <- table2$x1
                table2$x3 <- week2year(table2$x1)
                table2$x1 <- week2mjd(table2$x1)
              } else if (input$tunits == 3) {
                table2$x3 <- table2$x1
                table2$x2 <- year2week(table2$x1)
                table2$x1 <- year2mjd(table2$x1)
              }
            } else {
              info$step2 <- NULL
            }
          } else {
            info$step2 <- NULL
          }
        }
        # computing the sum of secondary series
        if (!is.null(table2)) {
          if (!is.null(table_stack)) {
            # shifting the next secondary series if necessary
            delta <- as.numeric(names(sort(table(table_stack$x1 - floor(table_stack$x1))))) - as.numeric(names(sort(table(table2$x1 - floor(table2$x1)))))
            if (length(delta) == 1 && isTruthy(is.numeric(delta))) {
              if (delta != 0) {
                table2$x1 <- table2$x1 + delta
                table2$x2 <- mjd2week(table2$x1)
                table2$x3 <- mjd2year(table2$x1)
                showNotification(paste("The time axis of the", files$name[i], "series has been shifted by a constant",delta,info$tunits.label), action = NULL, duration = 10, closeButton = T, id = "time_shift", type = "warning", session = getDefaultReactiveDomain())
              }
            } else {
              if (info$sampling < info$sampling_regular) {
                showNotification(HTML("The sampling of the primary series is not regular.<br>Consier using the \"Reduce sampling\" option to average the series to a constant sampling."), action = NULL, duration = 10, closeButton = T, id = "bad_time_shift", type = "error", session = getDefaultReactiveDomain())
              } else {
                showNotification(HTML(paste("The sampling of the", files$name[i], "series is not regular.<br>It is not possible to correct the secondary series.")), action = NULL, duration = 10, closeButton = T, id = "bad_time_shift", type = "error", session = getDefaultReactiveDomain())
              }
            }
            if (url$server2 == "EOSTLS" || url$server2 == "EPOS") {
              table_stack_tmp <- data.frame(within(merge(table_stack,table2, by = "x1", all = T), {
                x2 <- ifelse(is.na(x2.y),x2.x,x2.y)
                x3 <- ifelse(is.na(x3.y),x3.x,x3.y)
                y1 <- rowSums(cbind(y1.x, y1.y), na.rm = T)
                y2 <- rowSums(cbind(y2.x, y2.y), na.rm = T)
                y3 <- rowSums(cbind(y3.x, y3.y), na.rm = T)
                sy1 <- sy2 <- sy3 <- 1e-9
              })[,c("x1","x2","x3","y1","y2","y3","sy1","sy2","sy3")])
            } else {
              table_stack_tmp <- data.frame(within(merge(table_stack,table2, by = "x1", all = T), {
                x2 <- ifelse(is.na(x2.y),x2.x,x2.y)
                x3 <- ifelse(is.na(x3.y),x3.x,x3.y)
                y1 <- rowSums(cbind(y1.x, y1.y), na.rm = T)
                y2 <- rowSums(cbind(y2.x, y2.y), na.rm = T)
                y3 <- rowSums(cbind(y3.x, y3.y), na.rm = T)
                sy1 <- sqrt(rowSums(cbind(sy1.x^2, sy1.y^2), na.rm = T))
                sy2 <- sqrt(rowSums(cbind(sy2.x^2, sy2.y^2), na.rm = T))
                sy3 <- sqrt(rowSums(cbind(sy3.x^2, sy3.y^2), na.rm = T))
              })[,c("x1","x2","x3","y1","y2","y3","sy1","sy2","sy3")])
            }
            table_stack <- na.omit(table_stack_tmp)
            rm(table_stack_tmp)
          } else {
            table_stack <- table2
          }
        } else {
          showNotification(HTML(paste0("Wrong series format in ",files$name[i],".<br>Check the input file or the requested format.")), action = NULL, duration = 10, closeButton = T, id = NULL, type = "error", session = getDefaultReactiveDomain())
        }
      }
      removeNotification("stacking")
      # create secondary series merged file
      if (!is.null(table_stack)) {
        if (anyNA(table_stack)) {
          table_stack <- na.omit(table_stack)
          showNotification(HTML("The secondary series contains records with NA/NaN values.<br>These records were removed"), action = NULL, duration = 10, closeButton = T, id = "removing_NA", type = "warning", session = getDefaultReactiveDomain())
        }
        table2 <- table_stack
        if (!is.null(table2)) {
          # writing the merged series into a file, unless the series were uploaded from local files
          if (isTruthy(url$station2) && dim(as.matrix(files$datapath))[1] > 1) {
            table_stack <- table_stack[,c("x1","y1","y2","y3")]
            names(table_stack) <- c("# MJD", "East", "North", "Up")
            suppressWarnings(write.table(x = format(table_stack, justify = "right", nsmall = 2, digits = 0, scientific = F), file = file$secondary$newpath, append = F, quote = F, sep = "\t", eol = "\n", na = "N/A", dec = ".", row.names = F, col.names = T))
          } else {
            # Extracting station coordinates
            coordinates <- extract_coordinates(files$datapath[i],info$format2,url$server2,info$product2,url$station2,skip,sep2)
            lat2 <- as.numeric(coordinates[4])
            lon2 <- as.numeric(coordinates[5])
            shinyjs::delay(100, updateTextInput(session, inputId = "station_x2", value = coordinates[1]))
            shinyjs::delay(100, updateTextInput(session, inputId = "station_y2", value = coordinates[2]))
            shinyjs::delay(100, updateTextInput(session, inputId = "station_z2", value = coordinates[3]))
            shinyjs::delay(100, updateTextInput(session, inputId = "station_lat2", value = coordinates[4]))
            shinyjs::delay(100, updateTextInput(session, inputId = "station_lon2", value = coordinates[5]))
          }
          rm(table_stack)
          info$db2 <- "original"
          db2$original <- as.data.frame(table2)
          output$fileSeries2 <- renderUI({
            if (input$optionSecondary == 1 && isTruthy(url$station2) && isTruthy(file$secondary$newpath)) {
              tags$a(href = sub(pattern = "www/", replacement = "", x = file$secondary$newpath), "Show secondary series file", title = "Open the file of the secondary series in a new tab", target = "_blank", download = file$secondary$newname)
            } else {
              NULL
            }
          })
        } else {
          showNotification("The secondary series is empty or it does not match the requested format.", action = NULL, duration = 10, closeButton = T, id = "bad_series", type = "error", session = getDefaultReactiveDomain())
        }
      } else {
        showNotification("The secondary series is empty or it does not match the requested format.", action = NULL, duration = 10, closeButton = T, id = "bad_series", type = "error", session = getDefaultReactiveDomain())
      }
    }
    # Setting station IDs
    removes <- "^SPOTGINS_|^UGA_"
    if (isTruthy(url$station)) {
      file$id1 <- toupper(url$station)
    } else {
      file$id1 <- toupper(strsplit(gsub(pattern = removes, replacement = "", x = input$series$name, ignore.case = T, perl = T, fixed = F), "\\.|_|\\s|-|\\(")[[1]][1])
    }
    if (length(file$secondary) > 0) {
      if (isTruthy(url$station2)) {
        file$id2 <- toupper(url$station2)
      } else {
        file$id2 <- try(toupper(strsplit(gsub(pattern = removes, replacement = "", x = input$series2$name, ignore.case = T, perl = T, fixed = F), "\\.|_|\\s|-|\\(")[[1]][1]), silent = T)
        if (inherits(file$id2,"try-error") || file$id2 == "") {
          file$id2 <- NULL
          showNotification(HTML("Problem extracting the series ID from the secondary file name.<br>No series ID will be used"), action = NULL, duration = 10, closeButton = T, id = "ids_info", type = "warning", session = getDefaultReactiveDomain())
        }
      }
    }
    # Updating station IDs
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
      showNotification(HTML("Problem extracting the series ID from the file name.<br>No series ID will be used"), action = NULL, duration = 10, closeButton = T, id = "ids_info", type = "warning", session = getDefaultReactiveDomain())
    }
    shinyjs::delay(100, updateTextInput(session, inputId = "ids", value = ids_info))
  }
  #
  extract_table <- function(file,sep,format,epoch,variable,errorBar,swap,server,series) {
    tableAll <- NULL
    extracted <- NULL
    if (input$optionSecondary > 1 && isTruthy(db1$merged)) {
      updateRadioButtons(session, inputId = "optionSecondary", selected = 1)
    }
    removeNotification("no_weeks")
    removeNotification("no_error_bars")
    removeNotification("bad_coordinates")
    removeNotification("bad_pole")
    removeNotification("no_rotation")
    removeNotification("no_values")
    removeNotification("no_epos")
    removeNotification("bad_sigmas")
    removeNotification("no_tunits")
    # NEU/ENU ####
    if (format == 1) { #NEU/ENU
      skip <- 0
      a <- 6378137
      b <- 6356752.314140347
      e2 <- (a^2 - b^2) / a^2
      tunitsKnown <- F
      spotgins <- grepl("# SPOTGINS SOLUTION [POSITION]", readLines(file, n = 1, warn = F), fixed = T)
      # extracting series from SIRGAS format and transforming lat lon into ENU format
      if (server == "SIRGAS") {
        sirgas_new <- grep(" IGb14 ", readLines(file, warn = F), ignore.case = F, value = T, fixed = T)
        tableAll <- try(read.table(text = sirgas_new)[,c("V3", "V7", "V8", "V9", "V10", "V11", "V12")], silent = T)
        N <- a / sqrt( 1 - e2 * sin(tableAll[,2]*pi/180)^2) + tableAll[,3]
        dn <- N * (tableAll[,2] - tableAll[1,2]) * pi/180
        sdn <- N * tableAll[,5] * pi/180
        de <- N * (tableAll[,3] - tableAll[1,3]) * pi/180 * cos(tableAll[,2]*pi/180)
        sde <- N * tableAll[,6] * pi/180 * cos(tableAll[,2]*pi/180)
        tableAll <- cbind(de, dn, sde, sdn, tableAll)[,c(5,1,2,8,3,4,11)]
      } else if (server == "EARTHSCOPE") { # extracting ENU format from UNAVCO series
        unavco_new <- grep("^Datetime,", grep("^#", readLines(file, warn = F), ignore.case = F, value = T, perl = T, invert = T), ignore.case = F, value = T, perl = T, invert = T)
        tableAll <- try(read.table(text = unavco_new, sep = ",")[,c("V1", "V15", "V14", "V16", "V17", "V18", "V19")], silent = T)
      } else {
        tableAll <- try(read.table(text = trimws(readLines(file, warn = F)), comment.char = "#", sep = sep, skip = skip), silent = T)
      }
      # extracting series from EPOS format into ENU format
      if (server == "EPOS") {
        if (isTruthy(tableAll)) {
          tableAll$new <- paste(tableAll$V1, tableAll$V2)
          tableAll <- tableAll[, c("new", "V3", "V4", "V5")]
        } else {
          showNotification(HTML("The EPOS server is not accessible.<br>Try again a bit later, maybe?"), action = NULL, duration = 10, closeButton = T, id = "no_epos", type = "error", session = getDefaultReactiveDomain())
          req(info$stop)
        }
      }
      # extracting series from SONEL format into ENU format
      if (server == "SONEL") {
        tableAll <- tableAll[, c("V1", "V3", "V2", "V4", "V6", "V5", "V7")]
      }
      # extracting series from EOSTLS format into ENU format
      if (server == "EOSTLS") {
        tableAll <- tableAll[, c("V1", "V3", "V2", "V4")]
      }
      # transforming series from IGS lat lon into ENU format
      if (server == "IGS") {
        N <- a / sqrt( 1 - e2 * sin(tableAll[,5]*pi/180)^2) + tableAll[,7]
        dn <- N * (tableAll[,5] - tableAll[1,5]) * pi/180
        sdn <- N * tableAll[,8] * pi/180
        de <- N * (tableAll[,6] - tableAll[1,6]) * pi/180 * cos(tableAll[,5]*pi/180)
        sde <- N * tableAll[,9] * pi/180 * cos(tableAll[,5]*pi/180)
        tableAll <- cbind(de, dn, sde, sdn, tableAll)[,c(7,1,2,11,3,4,14,5,6)]
      }
      # extracting data in columns
      if (isTruthy(tableAll)) {
        columns <- dim(tableAll)[2]
        if (columns > 3) {
          extension <- tolower(rev(strsplit(as.character(file), ".", fixed = T)[[1]])[1])
          if (server == "" && isTruthy(extension) && extension == "neu") {
            swap <- T
          }
          if (isTruthy(swap)) {
            extracted <- tableAll[,c(3,2,4)]
          } else {
            extracted <- tableAll[,c(2,3,4)]
          }
          names(extracted) <- c("y1","y2","y3") 
          if (columns > 6) {
            if (isTruthy(swap)) {
              extracted$sy1 <- tableAll[,6]
              extracted$sy2 <- tableAll[,5]
              extracted$sy3 <- tableAll[,7]
            } else {
              extracted$sy1 <- tableAll[,5]
              extracted$sy2 <- tableAll[,6]
              extracted$sy3 <- tableAll[,7]
            }
          } else {
            extracted$sy1 <- extracted$sy2 <- extracted$sy3 <- rep(1e-9,length(extracted$y1))
            if (server != "EOSTLS" || series != 2) {
              info$errorbars <- F
            }
          }
          # get different time units
          if (isTruthy(extracted) && all(sapply(extracted, is.numeric))) {
            if (server == "JPL") {
              if (series == 1) {
                info$tunits.known1 <- T
              } else if (series == 2) {
                info$tunits.known2 <- T
              }
              extracted$x1 <- as.numeric(sprintf("%.*f", 5, difftime(strptime(paste(sprintf("%4d",tableAll[,12]),sprintf("%02d",tableAll[,13]),sprintf("%02d",tableAll[,14]),sprintf("%02d",tableAll[,15]),sprintf("%02d",tableAll[,16]),sprintf("%02d",tableAll[,17])), format = '%Y %m %d %H %M %S', tz = "GMT"), strptime(paste(sprintf("%08d",18581117),sprintf("%06d",000000)),format = '%Y%m%d %H%M%S', tz = "GMT"), units = "days")))
              extracted$x2 <- mjd2week(extracted$x1)
              extracted$x3 <- mjd2year(extracted$x1)
            } else if (server == "SONEL") {
              if (series == 1) {
                info$tunits.known1 <- T
              } else if (series == 2) {
                info$tunits.known2 <- T
              }
              extracted$x1 <- year2mjd(tableAll[,1])
              extracted$x2 <- year2week(tableAll[,1])
              extracted$x3 <- tableAll[,1]
            } else if (server == "SIRGAS") {
              if (series == 1) {
                info$tunits.known1 <- T
              } else if (series == 2) {
                info$tunits.known2 <- T
              }
              extracted$x1 <- as.numeric(sprintf("%.*f", 0, difftime(as.Date("1980-01-06") + tableAll[,1] * 7 + 3, strptime(paste(sprintf("%08d",18581117),sprintf("%06d",000000)), format = '%Y%m%d %H%M%S', tz = "GMT"), units = "days")))
              extracted$x2 <- mjd2week(extracted$x1)
              extracted$x3 <- mjd2year(extracted$x1)
            } else if (server == "IGS") {
              if (series == 1) {
                info$tunits.known1 <- T
              } else if (series == 2) {
                info$tunits.known2 <- T
              }
              extracted$x1 <- as.numeric(sprintf("%.*f", 1, tableAll[,1]))
              extracted$x2 <- as.numeric(sprintf("%.*f", 2, tableAll[,8] + tableAll[,9]/7))
              extracted$x3 <- mjd2year(extracted$x1)
            } else if (server == "FORMATER" || isTruthy(spotgins)) { # SPOTGINS series
              if (series == 1) {
                info$tunits.known1 <- T
              } else if (series == 2) {
                info$tunits.known2 <- T
              }
              extracted$x1 <- as.numeric(sprintf("%.*f", 1, tableAll[,1]))
              extracted$x2 <- as.numeric(sprintf("%.*f", 2, difftime(strptime(tableAll[,8], format = '%Y%m%d', tz = "GMT"), strptime(paste(sprintf("%08d",19800106),sprintf("%06d",000000)),format = '%Y%m%d %H%M%S', tz = "GMT"), units = "weeks")))
              extracted$x3 <- as.numeric(sprintf("%.*f", 3, tableAll[,9]))
            } else if (server == "EARTHSCOPE") {
              if (series == 1) {
                info$tunits.known1 <- T
              } else if (series == 2) {
                info$tunits.known2 <- T
              }
              extracted$x1 <- as.numeric(difftime(as.Date(tableAll[,1]), strptime(paste(sprintf("%08d",18581117),sprintf("%06d",000000)),format = '%Y%m%d %H%M%S', tz = "GMT"), units = "days"))
              extracted$x2 <- mjd2week(extracted$x1)
              extracted$x3 <- mjd2year(extracted$x1)
            } else if (server == "EOSTLS") {
              if (series == 1) {
                info$tunits.known1 <- T
              } else if (series == 2) {
                info$tunits.known2 <- T
              }
              extracted$x1 <- tableAll[,1]
              extracted$x2 <- mjd2week(extracted$x1)
              extracted$x3 <- mjd2year(extracted$x1)
            } else if (server == "EPOS") {
              if (series == 1) { 
                info$tunits.known1 <- T
              } else if (series == 2) {
                info$tunits.known2 <- T
              }
              extracted$x1 <- as.numeric(sprintf("%.*f", 0, difftime(as.Date(tableAll[,1]), strptime(paste(sprintf("%08d",18581117),sprintf("%06d",000000)),format = '%Y%m%d %H%M%S', tz = "GMT"), units = "days")))
              extracted$x2 <- mjd2week(extracted$x1)
              extracted$x3 <- mjd2year(extracted$x1)
            } else { #plain ENU series
              # ISO 8601 dates
              if (all(isTruthy(suppressWarnings(parse_date_time(tableAll[,1], c("%Y-%m-%dT%H:%M:%S%z", "%Y-%m-%dT%H:%M:%S"), exact = T))))) {
                if (series == 1) {
                  info$tunits.known1 <- T
                } else if (series == 2) {
                  info$tunits.known2 <- T
                }
                updateRadioButtons(session, inputId = "tunits", selected = 1)
                extracted$x1 <- as.numeric(difftime(ymd_hms(tableAll[,1]), strptime(paste(sprintf("%08d",18581117),sprintf("%06d",000000)),format = '%Y%m%d %H%M%S', tz = "GMT"), units = "days"))
                extracted$x2 <- mjd2week(extracted$x1)
                extracted$x3 <- mjd2year(extracted$x1)
              } else {
                if (!isTruthy(input$tunits)) {
                  showNotification("The time units of the series must be set before plotting.", action = NULL, duration = 10, closeButton = T, id = "no_tunits", type = "error", session = getDefaultReactiveDomain())
                  return(NULL)
                }
                # assuming the time units set by the user are good
                if (input$tunits == 1) {
                  extracted$x1 <- tableAll[,1]
                  extracted$x2 <- mjd2week(extracted$x1)
                  extracted$x3 <- mjd2year(extracted$x1)
                } else if (input$tunits == 2) {
                  extracted$x2 <- tableAll[,1]
                  extracted$x1 <- week2mjd(extracted$x2)
                  extracted$x3 <- week2year(extracted$x2)
                } else if (input$tunits == 3) {
                  extracted$x3 <- tableAll[,1]
                  extracted$x1 <- year2mjd(extracted$x3)
                  extracted$x2 <- year2week(extracted$x3)
                }
              }
            }
            extracted <- suppressWarnings(extracted[apply(extracted, 1, function(r) !any(is.na(as.numeric(r)))) ,])
          }
        }
      } else {
        errorInfo <- gsub("\n", "", gsub("(.|\\\\s)*?  ", "", tableAll[1]))
        if (isTruthy(errorInfo)) {
          errorInfo <- paste(errorInfo, "(skipping comment lines)")
        }
        showNotification(HTML(paste("Format error when reading the input NEU/ENU file.<br>", errorInfo)), action = NULL, duration = 10, closeButton = T, id = "no_values", type = "error", session = getDefaultReactiveDomain())
        return(NULL)
      }
    # PBO ####
    } else if (format == 2) { #PBO
      skip <- which(grepl("YYYYMMDD HHMMSS JJJJJ.JJJJ", readLines(file, warn = F)))
      tableAll <- try(read.table(file, comment.char = "#", sep = sep, skip = skip), silent = T)
      if (isTruthy(tableAll) && !inherits(tableAll,"try-error")) {
        if (series == 1) {
          info$tunits.known1 <- T
        } else if (series == 2) {
          info$tunits.known2 <- T
        }
        if (isTruthy(swap)) {
          extracted <- tableAll[,c(16,17,18,19,20,21)]
        } else {
          extracted <- tableAll[,c(17,16,18,20,19,21)]
        }
        names(extracted) <- c("y1","y2","y3","sy1","sy2","sy3")
        extracted$x1 <- tableAll[,3]
        extracted$x2 <- mjd2week(extracted$x1)
        extracted$x3 <- mjd2year(extracted$x1)
      }
    ## NGL ####
    } else if (format == 3) { #NGL
      skip <- which(grepl("site YYMMMDD", readLines(file, warn = F)))
      tableAll <- try(read.table(file, comment.char = "#", sep = sep, skip = skip), silent = T)
      if (isTruthy(tableAll) && !inherits(tableAll,"try-error")) {
        if (series == 1) {
          info$tunits.known1 <- T
        } else if (series == 2) {
          info$tunits.known2 <- T
        }
        extracted <- data.frame(x1 = tableAll[,4])
        extracted$x2 <- as.numeric(sprintf("%.*f", 2, tableAll[,5] + tableAll[,6]/7))
        extracted$x3 <- mjd2year(extracted$x1)
        if (isTruthy(swap)) {
          extracted$y2 <- tableAll[,8] - tableAll[1,8] + tableAll[,9] #East
          extracted$y1 <- tableAll[,10] - tableAll[1,10] + tableAll[,11] #North
          extracted$sy2 <- tableAll[,15]
          extracted$sy1 <- tableAll[,16]
        } else {
          extracted$y1 <- tableAll[,8] - tableAll[1,8] + tableAll[,9] #East (the coordinate integer portion seems to be constant, but just in case)
          extracted$y2 <- tableAll[,10] - tableAll[1,10] + tableAll[,11] #North
          extracted$sy1 <- tableAll[,15]
          extracted$sy2 <- tableAll[,16]
        }
        extracted$y3 <- tableAll[,12] - tableAll[1,12] + tableAll[,13] #Up
        extracted$sy3 <- tableAll[,17]
      }
      ## 1D ####
    } else if (format == 4) { #1D
      if (!is.na(epoch) && is.numeric(epoch) && epoch > 0 && !is.na(variable) && is.numeric(variable) && variable > 0 && epoch != variable) {
        if (server == "PSMSL") {
          tableAll <- try(read.table(file, sep = ";"), silent = T)
          tableAll <- tableAll[tableAll$V2 != -99999,]
          columns <- 2
        } else {
          skip <- 0
          tableAll <- try(read.table(text = trimws(readLines(file, warn = F)), comment.char = "#", sep = sep, skip = skip), silent = T)
          columns <- dim(tableAll)[2]
        }
        if (isTruthy(tableAll) && !inherits(tableAll,"try-error")) {
          if (epoch <= columns && variable <= columns) {
            if (all(sapply(tableAll[[variable]], is.numeric))) {
              extracted <- data.frame(y1 = tableAll[[variable]])
              # ISO 8601 dates
              if (all(isTruthy(suppressWarnings(parse_date_time(tableAll[[epoch]], c("%Y-%m-%dT%H:%M:%S%z", "%Y-%m-%dT%H:%M:%S"), exact = T))))) {
                if (series == 1) {
                  info$tunits.known1 <- T
                } else if (series == 2) {
                  info$tunits.known2 <- T
                }
                updateRadioButtons(session, inputId = "tunits", selected = 1)
                extracted$x1 <- as.numeric(difftime(ymd_hms(tableAll[[epoch]]), strptime(paste(sprintf("%08d",18581117),sprintf("%06d",000000)),format = '%Y%m%d %H%M%S', tz = "GMT"), units = "days"))
                extracted$x2 <- mjd2week(extracted$x1)
                extracted$x3 <- mjd2year(extracted$x1)
              } else if (server == "PSMSL") {
                if (series == 1) {
                  info$tunits.known1 <- T
                } else if (series == 2) {
                  info$tunits.known2 <- T
                }
                extracted$x1 <- year2mjd(tableAll[,1])
                extracted$x2 <- year2week(tableAll[,1])
                extracted$x3 <- tableAll[,1]
              } else {
                if (!isTruthy(input$tunits)) {
                  showNotification("The time units of the series must be set before plotting.", action = NULL, duration = 10, closeButton = T, id = "no_tunits", type = "error", session = getDefaultReactiveDomain())
                  return(NULL)
                }
                # assuming the time units set by the user are good
                if (all(sapply(tableAll[[epoch]], is.numeric))) {
                  extracted$x1 <- tableAll[[epoch]]
                  if (input$tunits == 1) {
                    extracted$x2 <- mjd2week(extracted$x1)
                    extracted$x3 <- mjd2year(extracted$x1)
                  } else if (input$tunits == 2) {
                    extracted$x2 <- extracted$x1
                    extracted$x1 <- week2mjd(extracted$x2)
                    extracted$x3 <- week2year(extracted$x2)
                  } else if (input$tunits == 3) {
                    extracted$x3 <- extracted$x1
                    extracted$x1 <- year2mjd(extracted$x3)
                    extracted$x2 <- year2week(extracted$x3)
                  }
                } else {
                  showNotification(HTML("Non numeric values extracted from the input series.<br>Check the input file or the requested format."), action = NULL, duration = 10, closeButton = T, id = "no_values", type = "error", session = getDefaultReactiveDomain())
                  return(NULL)
                }
              }
              if (columns > 2) {
                if (input$sigmas == T) {
                  if (!is.na(errorBar) && is.numeric(errorBar) && errorBar > 0 && errorBar <= columns && errorBar != epoch && errorBar != variable) {
                    extracted$sy1 <- tableAll[[errorBar]]
                  } else {
                    showNotification(HTML("Invalid column number for the series error bars.<br>Provide a valid column number or uncheck the error bars option."), action = NULL, duration = 10, closeButton = T, id = "no_error_bars", type = "error", session = getDefaultReactiveDomain())
                    req(info$stop)
                  }
                } else {
                  extracted$sy1 <- rep(1,length(extracted$y1))
                }
              } else {
                extracted$sy1 <- rep(1,length(extracted$y1))
                info$errorbars <- F
              }
              if (isTruthy(extracted)) {
                extracted <- suppressWarnings(extracted[apply(extracted, 1, function(r) !any(is.na(as.numeric(r)))) ,])
              }
            } else {
              extracted <- NULL
            }
          }
        }
      }
    }
    if (!isTruthy(input$tunits)) {
      if (isTruthy(info$tunits.known1)) {
        updateRadioButtons(session, inputId = "tunits", selected = 3)
      } else {
        showNotification("The time units of the series must be set before plotting.", action = NULL, duration = 10, closeButton = T, id = "no_tunits", type = "error", session = getDefaultReactiveDomain())
        return(NULL)
      }
    }
    if (!is.null(extracted) && all(sapply(extracted, is.numeric))) {
      # checking the error bars
      if (any(extracted[,grepl("sy", names(extracted))] <= 0)) {
        extracted[,grepl("sy", names(extracted))] <- rep(1, length(extracted$y1))
        info$errorbars <- F
        showNotification(HTML("Negative or null error bars extracted from the input series.<br>The use of the error bars has been deactivated."), action = NULL, duration = 10, closeButton = T, id = "bad_sigmas", type = "error", session = getDefaultReactiveDomain())
      }
      extracted
    } else {
      showNotification(HTML("Non numeric values extracted from the input series.<br>Check the input file or the requested format."), action = NULL, duration = 10, closeButton = T, id = "no_values", type = "error", session = getDefaultReactiveDomain())
      NULL
    }
  }
  #
  extract_coordinates <- function(filein,format,server,product,station,skip,sep) {
    if (format == 1) {
      if (isTruthy(server)) {
        if (server == "FORMATER") {
          coordinates <- unlist(strsplit(grep("_pos ", readLines(filein, warn = F), ignore.case = F, value = T, perl = T), "\\s+", fixed = F, perl = T, useBytes = F))[c(4,8,12)]
          shinyjs::delay(100, updateRadioButtons(session, inputId = "station_coordinates", selected = 1))
          stationGeo <- do.call(xyz2llh,as.list(as.numeric(c(coordinates[1],coordinates[2],coordinates[3]))))
          lat <- stationGeo[1] * 180/pi
          lon <- stationGeo[2] * 180/pi
          coordinates <- c(coordinates,lat,lon)
        } else if (server == "SONEL") {
          coordinates <- unlist(strsplit(grep("^# X : |^# Y : |^# Z : ", readLines(filein, warn = F), ignore.case = F, value = T, perl = T), "\\s+", fixed = F, perl = T, useBytes = F))[c(4,17,30)]
          shinyjs::delay(100, updateRadioButtons(session, inputId = "station_coordinates", selected = 1))
          stationGeo <- do.call(xyz2llh,as.list(as.numeric(c(coordinates[1],coordinates[2],coordinates[3]))))
          lat <- stationGeo[1] * 180/pi
          lon <- stationGeo[2] * 180/pi
          coordinates <- c(coordinates,lat,lon)
        } else if (server == "IGS") {
          tableAll <- try(read.table(text = trimws(readLines(filein, warn = F)[1]), comment.char = "#"), silent = T)
          shinyjs::delay(100, updateRadioButtons(inputId = "station_coordinates", selected = 2))
          lat <- tableAll[1,5]
          lon <- tableAll[1,6]
          coordinates <- latlon2xyz(lat*pi/180,lon*pi/180,1)
          coordinates <- c(coordinates,lat,lon)
        } else if (server == "JPL") {
          if (isTruthy(station)) {
            tableAll <- read.table("www/JPL_database.txt")
            coordinates <- tableAll[tableAll$V1 == station, c(2,3,4)]
            shinyjs::delay(100, updateRadioButtons(session, inputId = "station_coordinates", selected = 1))
            stationGeo <- do.call(xyz2llh,as.list(as.numeric(c(coordinates[1],coordinates[2],coordinates[3]))))
            lat <- stationGeo[1] * 180/pi
            lon <- stationGeo[2] * 180/pi
            coordinates <- c(coordinates,lat,lon)
          }
        } else if (server == "SIRGAS") {
          tableAll <- try(read.table(text = grep(" IGb14 ", readLines(filein, warn = F), value = T, fixed = T)[1], comment.char = "#"), silent = T)
          shinyjs::delay(100, updateRadioButtons(inputId = "station_coordinates", selected = 2))
          lat <- tableAll[1,7]
          lon <- tableAll[1,8]
          coordinates <- latlon2xyz(lat*pi/180,lon*pi/180,1)
          coordinates <- c(coordinates,lat,lon)
        } else if (server == "EPOS") {
          stationsFromEPOS <- try(read.table(file = "www/EPOS_database.txt", header = T), silent = T)
          tableAll <- stationsFromEPOS[grepl(station, stationsFromEPOS$id), c(2,3)]
          shinyjs::delay(100, updateRadioButtons(inputId = "station_coordinates", selected = 2))
          lat <- tableAll[1,1]
          lon <- tableAll[1,2]
          coordinates <- latlon2xyz(lat*pi/180,lon*pi/180,1000)
          coordinates <- c(coordinates,lat,lon)
        } else if (server == "EARTHSCOPE") {
          tableAll <- try(read.table(text = grep("# XYZ Reference Coordinate", readLines(filein, warn = F, n = 10), ignore.case = F, value = T, fixed = T), comment.char = ""), silent = T)
          shinyjs::delay(100, updateRadioButtons(session, inputId = "station_coordinates", selected = 1))
          stationGeo <- do.call(xyz2llh,as.list(as.numeric(c(tableAll[6],tableAll[8],tableAll[10]))))
          lat <- stationGeo[1] * 180/pi
          lon <- stationGeo[2] * 180/pi
          coordinates <- c(as.numeric(tableAll[6]),as.numeric(tableAll[8]),as.numeric(tableAll[10]),lat,lon)
        }
      } else if (isTruthy(product) && product == "SPOTGINS_POS") {
        coordinates <- unlist(strsplit(trim(grep("_pos ", readLines(filein, warn = F), ignore.case = F, value = T, perl = T)), "\\s+", fixed = F, perl = T, useBytes = F))[c(4,8,12)]
        if (!any(is.na(suppressWarnings(as.numeric(coordinates))))) {
          shinyjs::delay(100, updateRadioButtons(session, inputId = "station_coordinates", selected = 1))
          stationGeo <- do.call(xyz2llh,as.list(as.numeric(c(coordinates[1],coordinates[2],coordinates[3]))))
          lat <- stationGeo[1] * 180/pi
          lon <- stationGeo[2] * 180/pi
          coordinates <- c(coordinates,lat,lon)
        } else {
          coordinates <- NULL
        }
      }
    } else if (format == 2) {
      ref_pos <- grep("^XYZ Reference position",readLines(filein, n = 10, ok = T, warn = F, skipNul = T), ignore.case = F, perl = T, value = T)
      if (length(ref_pos) > 0) {
        shinyjs::delay(100, updateRadioButtons(session, inputId = "station_coordinates", choices = list("Cartesian" = 1, "Geographic" = 2), selected = 1, inline = T))
        x <- unlist(strsplit(ref_pos, split = " +"))[5]
        y <- unlist(strsplit(ref_pos, split = " +"))[6]
        z <- unlist(strsplit(ref_pos, split = " +"))[7]
        stationGeo <- do.call(xyz2llh,as.list(as.numeric(c(x,y,z))))
        lat <- stationGeo[1] * 180/pi
        lon <- stationGeo[2] * 180/pi
        coordinates <- c(x,y,z,lat,lon)
      }
    } else if (format == 3) {
      skip <- which(grepl("site YYMMMDD", readLines(filein, warn = F)))
      tableAll <- try(read.table(filein, comment.char = "#", sep = sep, skip = skip)[1,], silent = T)
      shinyjs::delay(100, updateRadioButtons(session, inputId = "station_coordinates", choices = list("Cartesian" = 1, "Geographic" = 2), selected = 2, inline = T))
      lat <- tableAll[1,21]
      lon <- ifelse(tableAll[1,22] < -180, yes = tableAll[1,22] + 360, no = tableAll[1,22])
      coordinates <- latlon2xyz(lat*pi/180,lon*pi/180,1)
      coordinates <- c(coordinates,lat,lon)
    } else if (format == 4) {
      if (isTruthy(server)) {
        if (server == "PSMSL") {
          stationsFromPSMSL <- try(read.table(file = "www/PSMSL_database.txt", sep = ";"), silent = T)
          tableAll <- stationsFromPSMSL[grepl(station, stationsFromPSMSL$V2), c(3,4)]
          shinyjs::delay(100, updateRadioButtons(inputId = "station_coordinates", selected = 2))
          lat <- as.numeric(tableAll[1,1])
          lon <- as.numeric(tableAll[1,2])
          coordinates <- latlon2xyz(lat*pi/180,lon*pi/180,1)
          coordinates <- c(coordinates,lat,lon)
        }
      }
    }
  }
  #
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
  #
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
  #
  model <- function(x,y) {
    removeNotification("bad_rate_noise")
    removeNotification("missing_rate_noise")
    removeNotification("no_trend_error")
    removeNotification("no_intercept_error")
    removeNotification("bad_sinusoidal_period")
    removeNotification("bad_amplitude_error")
    removeNotification("bad_sinusoidal_noise")
    removeNotification("missing_sinusoidal_noise")
    removeNotification("repeated_offset_epoch")
    removeNotification("useless_offset_epoch")
    removeNotification("outside_offset_epoch")
    removeNotification("bad_offset_epoch")
    removeNotification("not_numeric_offset")
    removeNotification("no_exponential")
    removeNotification("no_logarithmic")
    removeNotification("bad_degree")
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
        if (isTruthy(info$trendRef) && isTruthy(inputs$trendRef)) {
          reft <- inputs$trendRef
        } else {
          if (input$fitType == 1) {
            reft <- sprintf("%.*f", info$decimalsx, mean(x, na.rm = T))
            reft <- as.numeric(reft)
          } else if (input$fitType == 2) {
            reft <- x[1]
          }
          inputs$trendRef <- reft
          updateTextInput(session, "trendRef", value = sprintf("%.*f", info$decimalsx, reft))
          info$trendRef <- T
          if (input$fitType == 1) {
            req(info$stop)
          }
        }
        text_rate <- reft
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
                showNotification(HTML("The process noise for the trend is not valid.<br>Check the input value."), action = NULL, duration = 15, closeButton = T, id = "bad_rate_noise", type = "error", session = getDefaultReactiveDomain())
                return(NULL)
              }
            } else {
              showNotification(HTML("The process noise for the trend is not valid.<br>Check the input value."), action = NULL, duration = 15, closeButton = T, id = "bad_rate_noise", type = "error", session = getDefaultReactiveDomain())
              return(NULL)
            }
          } else {
            updateTextInput(session, "TrendDev", value = "0.0")
            noise <- 0
            model <- paste(model, paste0("Intercept + Rate*(x-",text_rate,")"), sep = " ")
            showNotification(HTML("The process noise value for the trend is missing.<br>Using a value of zero."), action = NULL, duration = 10, closeButton = T, id = "missing_rate_noise", type = "warning", session = getDefaultReactiveDomain())
          }
        } else {
          model <- paste(model, paste0("Intercept + Rate*(x-",text_rate,")"), sep = " ")
        }
        model_lm <- paste(model_lm, "x", sep = " ")
        model_kf_inst <- paste(model_kf, paste0("e[k,",j,"] + e[k,",j + 1,"]*(x[k] - x[k-1])"), sep = " ")
        model_kf_mean <- paste(model_kf, paste0("e[k,",j,"] + e[k,",j + 1,"]*(x[k]-",text_rate,")"), sep = " ")
        j <- j + 2
        if (identical(input$Trend0,character(0)) || is.na(input$Trend0) || input$Trend0 == "" || input$Trend0 == " ") {
          if (input$fitType == 1) {
            ap_rate <- sigma_rate <- 0
          } else {
            if (length(y) > 30) {
              tenth <- ceiling(length(y)/10)  
            } else {
              tenth <- length(y)
            }
            fastFit <- try(lm(y[1:tenth]~x[1:tenth]), silent = F)
            if (isTruthy(fastFit)) {
              ap_rate <- summary(fastFit)$coefficients[2,1]
              sigma_rate <- summary(fastFit)$coefficients[2,2] * 3
            } else {
              ap_rate <- (mean(tail(y, n = tenth), na.rm = T) - mean(y[1:tenth], na.rm = T))/(mean(tail(x, n = tenth), na.rm = T) - mean(x[1:tenth], na.rm = T))
              sigma_rate <- ap_rate * 5
            }
          }
          if (input$fitType == 2) {
            max_decimals <- signifdecimal(ap_rate, F) + 2
            updateTextInput(session, "Trend0", value = sprintf("%.*f", max_decimals, ap_rate))
            max_decimals <- signifdecimal(sigma_rate, F) + 2
            updateTextInput(session, "eTrend0", value = sprintf("%.*f", max_decimals, sigma_rate))
          }
        } else {
          ap_rate <- as.numeric(input$Trend0)
          if (input$eTrend0 == 0) {
            showNotification(HTML("The a priori trend error is zero.<br>Check the input value."), action = NULL, duration = 15, closeButton = T, id = "no_trend_error", type = "error", session = getDefaultReactiveDomain())
            req(info$stop)
          } else {
            sigma_rate <- as.numeric(input$eTrend0)
          }
        }
        if (input$fitType == 1) {
          y_detrend <- y - (x - reft) * ap_rate
          ap_intercept <- mean(y_detrend, na.rm = T)
          sigma_intercept <- sd(y_detrend/sqrt(length(y)), na.rm = T)
          if (!isTruthy(sigma_intercept) || sigma_intercept <= 0) {
            sigma_intercept <- 1
          }
        } else if (input$fitType == 2) {
          if (identical(input$Intercept0,character(0)) || is.na(input$Intercept0) || input$Intercept0 == "" || input$Intercept0 == " ") {
            if (isTruthy(match("Intercept", trans$names))) {
              if (isTruthy(match("Rate", trans$names))) {
                ap_intercept <- trans$LScoefs[match("Intercept", trans$names),1] + (trans$x[1] - reft) * trans$LScoefs[match("Rate", trans$names),1]
              } else {
                ap_intercept <- trans$LScoefs[match("Intercept", trans$names),1] + (trans$x[1] - reft) * ap_rate
              }
              sigma_intercept <- abs(as.numeric(trans$LScoefs[match("Intercept", trans$names),2] * sqrt(length(trans$x))))
            } else {
              ap_intercept <- y[1]
              sigma_intercept <- info$noise
            }
            max_decimals <- signifdecimal(ap_intercept, F) + 2
            updateTextInput(session, "Intercept0", value = sprintf("%.*f", max_decimals, ap_intercept))
            max_decimals <- signifdecimal(sigma_intercept, F) + 2
            updateTextInput(session, "eIntercept0", value = sprintf("%.*f", max_decimals, sigma_intercept))
          } else {
            ap_intercept <- as.numeric(input$Intercept0)
            if (input$eIntercept0 == 0) {
              showNotification(HTML("The a priori intercept error is zero.<br>Check the input value."), action = NULL, duration = 15, closeButton = T, id = "no_intercept_error", type = "error", session = getDefaultReactiveDomain())
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
            ap_intercept <- trans$LScoefs[match("Intercept", trans$names),1] - trans$ordinate
          } else if (input$fitType == 2) {
            ap_intercept <- trans$LScoefs[match("Intercept", trans$names),1]
          }
          sigma_intercept <- abs(as.numeric(trans$LScoefs[match("Intercept", trans$names),2] * sqrt(length(trans$x))))
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
        periods2 <- NULL
        trans$periods <- NULL
        S0 <- unlist(strsplit(input$S0, split = ","))
        eS0 <- unlist(strsplit(input$eS0, split = ","))
        sigamp <- unlist(strsplit(input$SinusoidalDev, split = ","))
        if (isTruthy(info$periodRef)) {
          refs <- inputs$periodRef
        } else {
          if (input$fitType == 1) {
            refs <- sprintf("%.*f", info$decimalsx, mean(x, na.rm = T))
            refs <- as.numeric(refs)
          } else if (input$fitType == 2) {
            refs <- x[1]
          }
          inputs$periodRef <- refs
          updateTextInput(session, "periodRef", value = refs)
          info$periodRef <- T
          if (input$fitType == 1) {
            req(info$stop)
          }
        }
        if (length(periods) > 0) {
          i <- 0
          for (p in periods) {
            f <- NULL
            i <- i + 1
            h <- 0
            if (grepl("x",p)) {
              harmonics <- unlist(strsplit(p, split = "x"))
              p <- harmonics[1]
              if (isTruthy(as.numeric(harmonics[2]))) {
                h <- as.integer(harmonics[2])
              }
            }
            if (grepl("d",p)) {
              f <- gsub("d", "", p)
              if (h > 1) {
                periods2 <- c(periods2, paste(as.numeric(f)/seq(h)[-1],"d",sep = ""))
              }
              if (nchar(f) > 0 && !is.na(as.numeric(f))) {
                trans$periods <- c(trans$periods, trim(paste0(f,"d")))
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
              if (h > 1) {
                periods2 <- c(periods2, paste(as.numeric(f)/seq(h)[-1],"w",sep = ""))
              }
              if (nchar(f) > 0 && !is.na(as.numeric(f))) {
                trans$periods <- c(trans$periods, trim(paste0(f,"w")))
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
              if (h > 1) {
                periods2 <- c(periods2, paste(as.numeric(f)/seq(h)[-1],"y",sep = ""))
              }
              if (nchar(f) > 0  && !is.na(as.numeric(f))) {
                trans$periods <- c(trans$periods, trim(paste0(f,"y")))
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
                showNotification(HTML(paste0("At least one of the input sinusoidal periods is larger than the series length (",format(info$rangex, nsmall = info$decimalsx, digits = 0, scientific = F, trim = F)," ",info$tunits.label,").<br>The fitting results may be unreliable.")), action = NULL, duration = 10, closeButton = T, id = "bad_sinusoidal_period", type = "warning", session = getDefaultReactiveDomain())
              }
              info$run <- T
              label_sin <- paste0("S",i)
              label_cos <- paste0("C",i)
              text_sin <- paste0("I(sin(2*pi*(x-", refs, ")*", f, "))")
              text_cos <- paste0("I(cos(2*pi*(x-", refs, ")*", f, "))")
              text_sin_kf <- paste0("sin(2*pi*(x[k]-", refs, ")*", f, ")")
              text_cos_kf <- paste0("cos(2*pi*(x[k]-", refs, ")*", f, ")")
              text_sin_lm <- paste0("sin(2*pi*x*", f, ")")
              text_cos_lm <- paste0("cos(2*pi*x*", f, ")")
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
                max_decimals <- signifdecimal(as.numeric(S0[i]), F) + 2
                S0[i] <- sprintf("%.*f", max_decimals, as.numeric(S0[i]))
                eS0[i] <- as.numeric(S0[i])/2
                max_decimals <- signifdecimal(as.numeric(eS0[i]), F) + 2
                eS0[i] <- sprintf("%.*f", max_decimals, as.numeric(eS0[i]))
                if (input$fitType == 2) {
                  if (isTruthy(match(paste0("S",i), trans$names))) {
                    s <- trans$LScoefs[match(paste0("S",i), trans$names),1]
                    c <- trans$LScoefs[match(paste0("C",i), trans$names),1]
                    S0[i] <- mean(c(as.numeric(s),as.numeric(c)))
                    max_decimals <- signifdecimal(as.numeric(S0[i]), F) + 2
                    S0[i] <- sprintf("%.*f", max_decimals, as.numeric(S0[i]))
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
                showNotification(HTML("At least one of the a priori sinusoidal amplitude errors is zero.<br>Check the input value."), action = NULL, duration = 15, closeButton = T, id = "bad_amplitude_error", type = "error", session = getDefaultReactiveDomain())
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
                    showNotification(HTML(paste("The process noise value for the sinusoid ",i," is not valid.<br>Check the input values.")), action = NULL, duration = 10, closeButton = T, id = "bad_sinusoidal_noise", type = "error", session = getDefaultReactiveDomain())
                    return(NULL)
                  }
                } else {
                  showNotification(HTML(paste("The process noise value for the sinusoid ",i," is missing.<br>Using a value of zero.")), action = NULL, duration = 10, closeButton = T, id = "missing_sinusoidal_noise", type = "warning", session = getDefaultReactiveDomain())
                  processNoise <- c(processNoise, 0)
                  processNoise <- c(processNoise, 0)
                }
              }
            } else {
              if (isTruthy(f)) {
                if (info$sampling == f) {
                  showNotification(paste("The period asked for sinusoid ",i," is equal to the series sampling and has been rejected"), action = NULL, duration = 10, closeButton = T, id = "bad_sinusoidal_period", type = "warning", session = getDefaultReactiveDomain())
                } else {
                  showNotification(paste("The period asked for sinusoid ",i," is way out of the data bounds and has been rejected"), action = NULL, duration = 10, closeButton = T, id = "bad_sinusoidal_period", type = "warning", session = getDefaultReactiveDomain())
                }
              } else {
                showNotification(paste("The period asked for sinusoid ",i," has wrong format and has been rejected"), action = NULL, duration = 10, closeButton = T, id = "bad_sinusoidal_period", type = "warning", session = getDefaultReactiveDomain())
              }
            }
          }
          for (p in periods2) {
            f <- NULL
            i <- i + 1
            if (grepl("d",p)) {
              f <- gsub("d", "", p)
              if (nchar(f) > 0 && !is.na(as.numeric(f))) {
                trans$periods <- c(trans$periods, trim(paste0(f,"d")))
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
                trans$periods <- c(trans$periods, trim(paste0(f,"w")))
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
                trans$periods <- c(trans$periods, trim(paste0(f,"y")))
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
                showNotification(HTML(paste0("At least one of the input sinusoidal periods is larger than the series length (",format(info$rangex, nsmall = info$decimalsx, digits = 0, scientific = F, trim = F)," ",info$tunits.label,").<br>The fitting results may be unreliable.")), action = NULL, duration = 10, closeButton = T, id = "bad_sinusoidal_period", type = "warning", session = getDefaultReactiveDomain())
              }
              info$run <- T
              label_sin <- paste0("S", i)
              label_cos <- paste0("C", i)
              text_sin <- paste0("I(sin(2*pi*(x-", refs, ")*", f, "))")
              text_cos <- paste0("I(cos(2*pi*(x-", refs, ")*", f, "))")
              text_sin_kf <- paste0("sin(2*pi*(x[k]-", refs, ")*", f, ")")
              text_cos_kf <- paste0("cos(2*pi*(x[k]-", refs, ")*", f, ")")
              text_sin_lm <- paste0("sin(2*pi*x*", f, ")")
              text_cos_lm <- paste0("cos(2*pi*x*", f, ")")
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
                max_decimals <- signifdecimal(as.numeric(S0[i]), F) + 2
                S0[i] <- sprintf("%.*f", max_decimals, as.numeric(S0[i]))
                eS0[i] <- as.numeric(S0[i])/2
                max_decimals <- signifdecimal(as.numeric(eS0[i]), F) + 2
                eS0[i] <- sprintf("%.*f", max_decimals, as.numeric(eS0[i]))
                if (input$fitType == 2) {
                  if (isTruthy(match(paste0("S",i), trans$names))) {
                    s <- trans$LScoefs[match(paste0("S",i), trans$names), 1]
                    c <- trans$LScoefs[match(paste0("C",i), trans$names), 1]
                    S0[i] <- mean(c(as.numeric(s),as.numeric(c)))
                    max_decimals <- signifdecimal(as.numeric(S0[i]), F) + 2
                    S0[i] <- sprintf("%.*f", max_decimals, as.numeric(S0[i]))
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
                showNotification(HTML("At least one of the a priori sinusoidal amplitude errors is zero.<br>Check the input value."), action = NULL, duration = 15, closeButton = T, id = "bad_amplitude_error", type = "error", session = getDefaultReactiveDomain())
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
                    showNotification(HTML(paste("The process noise value for the sinusoid ",i," is not valid.<br>Check the input values.")), action = NULL, duration = 10, closeButton = T, id = "bad_sinusoidal_noise", type = "error", session = getDefaultReactiveDomain())
                    return(NULL)
                  }
                } else {
                  showNotification(HTML(paste("The process noise value for the sinusoid ",i," is missing.<br>Using a value of zero.")), action = NULL, duration = 10, closeButton = T, id = "missing_sinusoidal_noise", type = "warning", session = getDefaultReactiveDomain())
                  processNoise <- c(processNoise, 0)
                  processNoise <- c(processNoise, 0)
                }
              }
            } else {
              showNotification(paste("The period for sinusoid ",i," is way out of the data bounds and has been neglected."), action = NULL, duration = 10, closeButton = T, id = "bad_sinusoidal_period", type = "warning", session = getDefaultReactiveDomain())
            }
          }
          line_S0 <- paste(S0, collapse = ", ")
          line_eS0 <- paste(eS0, collapse = ", ")
          updateTextInput(session, "S0", value = line_S0)
          updateTextInput(session, "eS0", value = line_eS0)
        }
      }
      # * Offset model ####
      if ("Offset" %in% input$model) {
        if (isTruthy(inputs$offsetEpoch)) {
          offsetEpochs <- trimws(unlist(strsplit(inputs$offsetEpoch, split = ",")))
          offsetEpochs_all <- offsetEpochs
          O0 <- unlist(strsplit(input$O0, split = ","))
          eO0 <- unlist(strsplit(input$eO0, split = ","))
          trans$offsetEpochs <- NULL
          # check for valid numeric values
          not_numeric <- suppressWarnings(which(is.na(as.numeric(offsetEpochs))))
          if (length(not_numeric) > 0) {
            offsetEpochs <- offsetEpochs[-not_numeric]
            showNotification(HTML(paste("The epoch given for offset(s)", paste0("#",not_numeric, collapse = " "), "is not numeric.<br>These offsets were skipped.")), action = NULL, duration = 10, closeButton = T, id = "not_numeric_offset", type = "warning", session = getDefaultReactiveDomain())
          }
          offsetEpochs <- as.numeric(offsetEpochs)
          if (length(offsetEpochs) > 0) {
            # check for duplicated offset epochs
            while (anyDuplicated(offsetEpochs) > 0) {
              offset_duplicated <- anyDuplicated(offsetEpochs)
              uselessOffset_id <- which.min(abs(suppressWarnings(as.numeric(offsetEpochs_all) - offsetEpochs[offset_duplicated])))
              showNotification(HTML(paste0("The epoch given for offset #", uselessOffset_id, " is duplicated.<br>This offset was skipped.")), action = NULL, duration = 10, closeButton = T, id = NULL, type = "warning", session = getDefaultReactiveDomain())
              offsetEpochs <- offsetEpochs[-offset_duplicated]
            }
            # check for soln without observations
            offsetEpochs_sorted <- suppressWarnings(sort(offsetEpochs, na.last = NA))
            if (length(offsetEpochs_sorted) > 1) {
              invalidSegment <- sapply(seq(length(offsetEpochs_sorted) - 1), function(x) length(trans$x[trans$x > offsetEpochs_sorted[x] & trans$x < offsetEpochs_sorted[x + 1]]) ) == 0
              for (soln in which(invalidSegment)) {
                uselessOffset_id <- which.min(abs(offsetEpochs - offsetEpochs_sorted[soln]))
                uselessOffset_id1 <- which.min(abs(suppressWarnings(as.numeric(offsetEpochs_all) - offsetEpochs_sorted[soln])))
                uselessOffset_id2 <- which.min(abs(suppressWarnings(as.numeric(offsetEpochs_all) - offsetEpochs_sorted[soln + 1])))
                offsetEpochs <- offsetEpochs[-uselessOffset_id]
                showNotification(HTML(paste0("There are no observations between offsets #", uselessOffset_id1, " and #", uselessOffset_id2,".<br>The first offset was skipped")), action = NULL, duration = 10, closeButton = T, id = NULL, type = "warning", session = getDefaultReactiveDomain())
              }
            }
            # check for offsets outside data limits
            toremove <- 999999
            for (i in seq_len(length(offsetEpochs))) {
              if (offsetEpochs[i] > trans$x[length(trans$x)] || offsetEpochs[i] < trans$x[1]) {
                uselessOffset_id <- which.min(abs(suppressWarnings(as.numeric(offsetEpochs_all) - offsetEpochs[i])))
                toremove <- c(toremove, i)
                showNotification(HTML(paste0("There are no observations before or after offset #", uselessOffset_id,".<br>This offset was skipped.")), action = NULL, duration = 10, closeButton = T, id = NULL, type = "warning", session = getDefaultReactiveDomain())
              }
            }
            offsetEpochs <- offsetEpochs[-toremove]
            i <- 0
            for (p in offsetEpochs) {
              p <- as.numeric(trim(p))
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
                    } else {
                      max_decimals <- signifdecimal(as.numeric(O0[i]), F) + 2
                      O0[i] <- sprintf("%.*f", max_decimals, as.numeric(O0[i]))
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
                    } else {
                      max_decimals <- signifdecimal(as.numeric(eO0[i]), F) + 2
                      eO0[i] <- sprintf("%.*f", max_decimals, as.numeric(eO0[i]))
                    }
                    if (input$fitType == 2) {
                      if (isTruthy(match(paste0("O",i), trans$names))) {
                        O0[i] <- trans$LScoefs[match(paste0("O",i), trans$names),1]
                        max_decimals <- signifdecimal(as.numeric(O0[i]), F) + 2
                        O0[i] <- sprintf("%.*f", max_decimals, as.numeric(O0[i]))
                        eO0[i] <- abs(as.numeric(trans$LScoefs[match(paste0("O",i), trans$names),2]))
                        max_decimals <- signifdecimal(as.numeric(eO0[i]), F) + 2
                        eO0[i] <- sprintf("%.*f", max_decimals, as.numeric(eO0[i]))
                      }
                      line_O0 <- paste(O0, collapse = ", ")
                      line_eO0 <- paste(eO0, collapse = ", ")
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
                showNotification(HTML(paste0("The epoch given for offset #",i + 1," is not valid.<br>Check the input values.")), action = NULL, duration = 10, closeButton = T, id = "bad_offset_epoch", type = "error", session = getDefaultReactiveDomain())
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
            refe <- trim(refe)
            i <- i + 1
            if (nchar(refe) > 0 && !is.na(as.numeric(refe))) {
              if (identical(E0,character(0)) || identical(TE0,character(0)) || is.na(E0[i]) || is.na(TE0[i]) || E0[i] == "" || TE0[i] == "" || E0[i] == " " || TE0[i] == " ") {
                update <- 1
                if (isTruthy(trans$LScoefs) && isTruthy(match(paste0("E",i), trans$names))) {
                  E0[i] <- trans$LScoefs[match(paste0("E",i), trans$names),1]
                  TE0[i] <- trans$LScoefs[match(paste0("TauE",i), trans$names),1]
                  eE0[i] <- trans$LScoefs[match(paste0("E",i), trans$names),2]
                  eTE0[i] <- trans$LScoefs[match(paste0("TauE",i), trans$names),2]
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
                      showNotification(HTML("Not enough data to obtain the a priori values of the exponential decay.<br>The a priori values must be provided to continue."), action = NULL, duration = 10, closeButton = T, id = "no_exponential", type = "warning", session = getDefaultReactiveDomain())
                    }
                    x1 <- apriori_x_after[apriori_x_after >= forward - sample/2 & apriori_x_after < forward + sample/2]
                    y1 <- flat[apriori_x_after >= forward - sample/2 & apriori_x_after < forward + sample/2]
                    E0[i] <- mean(y0) * coeff
                    if (!isTruthy(as.numeric(E0[i]))) {
                      E0[i] <- 0
                    }
                    TE0[i] <- (forward - mean(x0))/(log(mean(y0)) - log(mean(y1)))
                    if (!isTruthy(as.numeric(TE0[i]))) {
                      TE0[i] <- forward
                    }
                    if (input$fitType == 2) {
                      eE0[i] <- sd(y0)
                      eTE0[i] <- sqrt( ( -1*sd(y0)/(mean(y0) * log(mean(y0)/mean(y1))^2) )^2 + ( 1*sd(y1)/(mean(y1) * log(mean(y0)/mean(y1))^2) )^2 )
                    }
                  } else {
                    showNotification(HTML("Not enough data to obtain the a priori values of the exponential decay.<br>The a priori values must be provided to continue."), action = NULL, duration = 10, closeButton = T, id = "no_exponential", type = "warning", session = getDefaultReactiveDomain())
                  }
                }
              }
              if (isTruthy(as.numeric(E0[i])) && isTruthy(as.numeric(TE0[i]))) {
                max_decimals <- signifdecimal(as.numeric(E0[i]), F) + 2
                E0[i] <- sprintf("%.*f", max_decimals, as.numeric(E0[i]))
                max_decimals <- signifdecimal(as.numeric(TE0[i]), F) + 2
                TE0[i] <- sprintf("%.*f", max_decimals, as.numeric(TE0[i]))
              }
              if (input$fitType == 2 && isTruthy(as.numeric(eE0[i])) && isTruthy(as.numeric(eTE0[i]))) {
                max_decimals <- signifdecimal(as.numeric(eE0[i]), F) + 2
                eE0[i] <- sprintf("%.*f", max_decimals, as.numeric(eE0[i]))
                max_decimals <- signifdecimal(as.numeric(eTE0[i]), F) + 2
                eTE0[i] <- sprintf("%.*f", max_decimals, as.numeric(eTE0[i]))
              }
            } else {
              if (is.na(E0[i]) || trim(E0[i]) == "NA" || trim(E0[i]) == "") {
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
            line_E0 <- paste(E0, collapse = ", ")
            line_TE0 <- paste(TE0, collapse = ", ")
            inputs$E0 <- line_E0
            inputs$TE0 <- line_TE0
            updateTextInput(session, "E0", value = line_E0)
            updateTextInput(session, "TE0", value = line_TE0)
            if (input$fitType == 1) {
              req(info$stop)
            } else if (input$fitType == 2) {
              line_eE0 <- paste(eE0, collapse = ", ")
              line_eTE0 <- paste(eTE0, collapse = ", ")
              updateTextInput(session, "eE0", value = line_eE0)
              updateTextInput(session, "eTE0", value = line_eTE0)
            }
          }
          for (i in seq_len(length(E0))) {
            if (!is.na(as.numeric(E0[i]))) {
              info$run <- T
              label1 <- paste0("E",i)
              label2 <- paste0("TauE",i)
              text_exp <- expos[i]
              model <- paste(model, paste(label1,"*I(x>",text_exp,")*(exp((",text_exp,"-x)/",label2,") - 1)"), sep = " + ")
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
          inputs$E0 <- ""
          inputs$TE0 <- ""
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
            refl <- trim(refl)
            i <- i + 1
            if (nchar(refl) > 0 && !is.na(as.numeric(refl))) {
              if (identical(L0,character(0)) || identical(TL0,character(0)) || is.na(L0[i]) || is.na(TL0[i]) || L0[i] == "" || TL0[i] == "" || L0[i] == " " || TL0[i] == " ") {
                update <- 1
                if (isTruthy(trans$LScoefs) && isTruthy(match(paste0("L",i), trans$names))) {
                  L0[i] <- trans$LScoefs[match(paste0("L",i), trans$names),1]
                  TL0[i] <- trans$LScoefs[match(paste0("TauL",i), trans$names),1]
                  eL0[i] <- abs(as.numeric(trans$LScoefs[match(paste0("L",i), trans$names),2]))
                  eTL0[i] <- abs(as.numeric(trans$LScoefs[match(paste0("TauL",i), trans$names),2]))
                  TL0[i] <- ifelse(as.numeric(TL0[i]) - 10*as.numeric(eTL0[i]) <= 0, as.numeric(TL0[i])*10, TL0[i])
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
                      showNotification(HTML("Not enough data to guess the a priori values of the logarithmic decay.<br>The a priori values must be provided to continue."), action = NULL, duration = 10, closeButton = T, id = "no_logarithmic", type = "warning", session = getDefaultReactiveDomain())
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
                    if (!isTruthy(TL0[i]) || as.numeric(TL0[i]) < 0) {
                      TL0[i] <- abs(as.numeric(TL0[i]))
                    }
                    if (input$fitType == 2) {
                      eL0[i] <- sd(y1)
                      eTL0[i] <- sd(unlist(tl), na.rm = T)
                    }
                  } else {
                    showNotification(HTML("Not enough data to guess the a priori values of the logarithmic decay.<br>The a priori values must be provided to continue."), action = NULL, duration = 10, closeButton = T, id = "no_logarithmic", type = "warning", session = getDefaultReactiveDomain())
                  }
                }
              }
              if (isTruthy(as.numeric(L0[i])) && isTruthy(as.numeric(TL0[i]))) {
                max_decimals <- signifdecimal(as.numeric(L0[i]), F) + 2
                L0[i] <- sprintf("%.*f", max_decimals, as.numeric(L0[i]))
                max_decimals <- signifdecimal(as.numeric(TL0[i]), F) + 2
                TL0[i] <- sprintf("%.*f", max_decimals, as.numeric(TL0[i]))
              }
              if (input$fitType == 2 && isTruthy(as.numeric(eL0[i])) && isTruthy(as.numeric(eTL0[i]))) {
                max_decimals <- signifdecimal(as.numeric(eL0[i]), F) + 2
                eL0[i] <- sprintf("%.*f", max_decimals, as.numeric(eL0[i]))
                max_decimals <- signifdecimal(as.numeric(eTL0[i]), F) + 2
                eTL0[i] <- sprintf("%.*f", max_decimals, as.numeric(eTL0[i]))
              }
            } else {
              if (is.na(L0[i]) || trim(L0[i]) == "NA" || trim(L0[i]) == "") {
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
            line_L0 <- paste(L0, collapse = ", ")
            line_TL0 <- paste(TL0, collapse = ", ")
            inputs$L0 <- line_L0
            inputs$TL0 <- line_TL0
            updateTextInput(session, "L0", value = line_L0)
            updateTextInput(session, "TL0", value = line_TL0)
            if (input$fitType == 1) {
              req(info$stop)
            } else if (input$fitType == 2) {
              line_eL0 <- paste(eL0, collapse = ", ")
              line_eTL0 <- paste(eTL0, collapse = ", ")
              updateTextInput(session, "eL0", value = line_eL0)
              updateTextInput(session, "eTL0", value = line_eTL0)
            }
          }
          for (i in seq_len(length(L0))) {
            if (!is.na(as.numeric(L0[i]))) {
              info$run <- T
              label1 <- paste0("L",i)
              label2 <- paste0("TauL",i)
              text_log <- logas[i]
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
          inputs$L0 <- ""
          inputs$TL0 <- ""
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
            if (isTruthy(info$PolyRef)) {
              refp <- inputs$PolyRef
            } else {
              if ("Linear" %in% input$model) {
                refp <- as.numeric(reft)
              } else {
                if (input$fitType == 1) {
                  refp <- sprintf("%.*f", info$decimalsx, mean(x, na.rm = T))
                  refp <- as.numeric(refp)
                } else if (input$fitType == 2) {
                  refp <- x[1]
                }
                inputs$PolyRef <- refp
              }
              info$PolyRef <- T
              inputs$PolyRef <- refp
              updateTextInput(session, "PolyRef", value = refp)
              if (input$fitType == 1) {
                req(info$stop)
              }
            }
            text_rate <- refp
            i <- 0
            for (degree in 2:inputs$PolyCoef) {
              i <- i + 1
              if (identical(P0[i],character(0)) || is.na(P0[i])) {
                P0[i] <- 0
                if (input$fitType == 2) {
                  if (isTruthy(match(paste0("P",degree), trans$names))) {
                    P0[i] <- trans$LScoefs[match(paste0("P",degree), trans$names),1]
                    max_decimals <- signifdecimal(as.numeric(P0[i]), F) + 2
                    P0[i] <- sprintf("%.*f", max_decimals, as.numeric(P0[i]))
                    eP0[i] <- trans$LScoefs[match(paste0("P",degree), trans$names),2]
                    max_decimals <- signifdecimal(as.numeric(eP0[i]), F) + 2
                    eP0[i] <- sprintf("%.*f", max_decimals, as.numeric(eP0[i]))
                  } else {
                    eP0[i] <- 1
                  }
                  line_P0 <- paste(P0, collapse = ", ")
                  line_eP0 <- paste(eP0, collapse = ", ")
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
            showNotification(HTML("The requested degree of the polynomial is not valid.<br>Check the input value."), action = NULL, duration = 10, closeButton = T, id = "bad_degree", type = "error", session = getDefaultReactiveDomain())
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
  #
  ReadLog <- function(x) {
    removeNotification("bad_sitelog")
    antrec <- grep("^[34].[x0-9]+ ",readLines(con = x, n = -1L, ok = T, warn = F, skipNul = T), ignore.case = F, perl = T, value = T)
    dates <- grep(" Date Removed ",readLines(con = x, n = -1L, ok = T, warn = F, skipNul = T), ignore.case = F, perl = T, value = T)
    if (length(antrec) > 0 && length(dates) > 0) {
      ante = c()
      rece = c()
      for (l in seq_len(length(dates))) {
        if (!(grepl('CCYY',dates[[l]]))) {
          f <- data.frame(strsplit(dates[[l]], " :"))[2,]
          t <- strptime(f, format = '%Y-%m-%dT%H:%M', tz = "GMT")
          if (is.na(t)) {
            t <- strptime(f, format = '%Y-%m-%d', tz = "GMT")
          }
          if (!isTruthy(input$tunits) || input$tunits == 3) {
            e <- decimal_date(t)
          } else if (input$tunits == 1) {
            e <- time_length(ymd_hms("1858-11-17 00:00:00") %--% t, unit = "second")/86400  #mjd
          } else if (input$tunits == 2) {
            e <- time_length(ymd_hms("1980-01-06 00:00:00") %--% t, unit = "second")/604800 # GPS week
          }
          if (grepl('Antenna',antrec[[l]])) {
            ante <- c(ante,e)
          } else if (grepl('Receiver',antrec[[1]])) {
            rece <- c(rece,e)
          }
        }
      }
      return(list(ante,rece))
    } else {
      showNotification("The input sitelog file is empty or has a wrong format.", action = NULL, duration = 10, closeButton = T, id = "bad_sitelog", type = "warning", session = getDefaultReactiveDomain())
      return(NULL)
    }
  }
  #
  ReadInfo <- function(x,y,z) {
    antes = c()
    reces = c()
    removeNotification("bad_stationinfo")
    if (!is.null(x)) {
      pattern <- paste0("^ ",x)
      record <- grep(pattern, readLines(con = z$datapath, n = -1L, ok = T, warn = F, skipNul = T), ignore.case = F, perl = T, value = T)
      if (length(record) > 1) {
        for (l in seq_len(length(record))) {
          elements1 <- unlist(strsplit(record[[l]], "\\s+", fixed = F, perl = T, useBytes = F))
          if (length(record) > l) {
            elements2 <- unlist(strsplit(record[[l + 1]], "\\s+", fixed = F, perl = T, useBytes = F))
            t <- strptime(substr(record[[l + 1]],26,43), format = '%Y %j %H %M %S', tz = "GMT")
            if (!isTruthy(input$tunits) || input$tunits == 3) {
              e <- decimal_date(t)
            } else if (input$tunits == 1) {
              e <- time_length(ymd_hms("1858-11-17 00:00:00") %--% t, unit = "second")/86400  #mjd
            } else if (input$tunits == 2) {
              e <- time_length(ymd_hms("1980-01-06 00:00:00") %--% t, unit = "second")/604800 # GPS week
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
            t <- strptime(substr(record[[l + 1]],26,43), format = '%Y %j %H %M %S', tz = "GMT")
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
  #
  ReadSoln <- function(x,y,z) {
    req(x,z)
    changes <- c()
    site <- paste0(" ",x," ")
    extracted <- substring(grep(' P -',grep(site,readLines(z$datapath, warn = F),value = T), value = T), 17, 28)
    if (!is.null(y)) {
      site2 <- paste0(" ",y," ")
      extracted2 <- substring(grep(' P -',grep(site2,readLines(z$datapath, warn = F),value = T), value = T), 17, 28)
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
  #
  ReadCustom <- function(x,y,z) {
    req(x,z)
    removeNotification("bad_custom")
    changes <- c()
    cols <- try(range(count.fields(z$datapath, comment.char = "#")), silent = F)
    if (!isTruthy(cols) || inherits(cols,"try-error")) {
      showNotification("Unable to read the input custom discontinuity file.", action = NULL, duration = 15, closeButton = T, id = "bad_custom", type = "error", session = getDefaultReactiveDomain())
      req(info$stop)
    }
    if (!is.na(cols[1]) && !is.na(cols[2]) && cols[2] < 100) {
      if (cols[1] > 1) {
        col <- 2
      } else {
        col <- 1
      }
      table <- try(read.table(z$datapath, comment.char = "#", fill = T, col.names = c(1:cols[2]))[,1:col], silent = F)
      if (isTruthy(table) && !inherits(table,"try-error")) {
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
          } else if (grepl(pattern = "# Offset file", x = readLines(z$datapath, n = 1), ignore.case = F, perl = F, fixed = T)) { #FORMATER offset file
            table$dyear <- decimal_date(strptime("18581117", format = '%Y%m%d', tz = "GMT") + table$X2*86400)
              changes <- as.numeric(unlist(unique(table$dyear[table$X1 == x])))
              changes <- unique(c(changes, as.numeric(unlist(unique(table$dyear[table$X1 == y])))))
          } else {
            if (cols[2] > 2 && info$custom_warn == 0) {
              info$custom_warn <- 1
              showNotification(HTML("The input custom discontinuity file contains more than 2 columns.<br>Only the first 2 will be used."), action = NULL, duration = 15, closeButton = T, id = "bad_custom", type = "warning", session = getDefaultReactiveDomain())
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
        changes <- sort(na.omit(changes), decreasing = F)
      }
    }
    return(changes)
  }
  #
  plot_series <- function(x,y,z,rangex,rangey,sigma,title,symbol,unit) {
    if (symbol == 0) {
      s <- 'p'
    } else if (symbol == 1) {
      s <- 'l'
    } else if (symbol == 2) {
      s <- 'o'
    }
    units <- ""
    if (unit) {
      if (input$sunits == 1) {
        units <- "(m)"
      } else if (input$sunits == 2) {
        units <- "(mm)"
      }
    }
    mini <- min(y, na.rm = T)
    maxi <- max(y, na.rm = T)
    if ((abs(mini) > 999 || abs(maxi) > 999) && abs(maxi - mini) < 999) {
      if (mini < 0) {
        const <- maxi
        ylab <- paste(intToUtf8(8210),abs(mini),units)
      } else {
        const <- mini
        ylab <- paste("+",abs(mini),units)
      }
    } else {
      const <- 0
      ylab <- units
    }
    plot(x, y, type = s, pch = 20, lwd = 2, xlab = "", ylab = ylab, xlim = rangex, ylim = rangey, main = title, yaxt = "n")
    p <- par("usr")[3:4] # min/max Y-axis values
    pout <- base::pretty(p - const) # round new min/max Y-axis values
    pin <- pout + const
    axis(2, at = pin, labels = pout)
    if (isTruthy(input$tunits)) {
      if (input$tunits == 1) {
        offset <- 0
        if (all(x < 35000)) {
          offset <- 33282
        }
        if (isTruthy(rangex)) {
          ticks <- base::pretty(x[x > rangex[1] & x < rangex[2]])
        } else {
          ticks <- base::pretty(x)
        }
        labels_dyear <- sprintf("%.2f", decimal_date(as.Date("1858-11-17") + ticks + offset))
        axis(3, at = ticks, labels = labels_dyear)
      } else if (input$tunits == 2) {
        if (isTruthy(rangex)) {
          ticks <- base::pretty(x[x > rangex[1] & x < rangex[2]])
        } else {
          ticks <- base::pretty(x)
        }
        labels_dyear <- sprintf("%.2f", decimal_date(as.Date("1980-01-06") + ticks*7))
        axis(3, at = ticks, labels = labels_dyear)
      }
    }
    if (sigma == T) {
      if (length(y) == length(z)) {
        ba <- y + z
        bb <- y - z
        polygon(c(x, rev(x)), c(ba, rev(bb)), col = rgb(0,0,0,0.2), border = NA)
      } else {
        if (messages > 0) cat(file = stderr(), mySession, paste0("Different length of the series values (", length(y), ") and errorbars (", length(z), "\n"))
        showNotification(HTML("Something is wrong with the series plot and the errorbars could not be plotted.<br>Please contact the author to provide feedback"), action = NULL, duration = 10, closeButton = T, id = "wrong_series", type = "error", session = getDefaultReactiveDomain())
      }
    }
  }
  #
  plot3series <- function(component) {
        removeNotification("wrong_series")
        if (messages > 0) cat(file = stderr(), mySession, "Plotting component", component, "\n")
        x0 <- db1[[info$db1]][[paste0("x",input$tunits)]][!is.na(db1[[info$db1]][[paste0("status",component)]])]
        x1 <- db1[[info$db1]][[paste0("x",input$tunits)]][db1[[info$db1]][[paste0("status",component)]] %in% T]
        xe <- db1[[info$db1]][[paste0("x",input$tunits)]][db1[[info$db1]][[paste0("status",component)]] %in% F]
        y0 <- db1[[info$db1]][[paste0("y",component)]][!is.na(db1[[info$db1]][[paste0("status",component)]])]
        y1 <- db1[[info$db1]][[paste0("y",component)]][db1[[info$db1]][[paste0("status",component)]] %in% T]
        ye <- db1[[info$db1]][[paste0("y",component)]][db1[[info$db1]][[paste0("status",component)]] %in% F]
        if (isTruthy(trans$plate) && input$eulerType == 2 && isTruthy(inputs$station_x) && isTruthy(inputs$station_y) && isTruthy(inputs$station_z)) {
          y1 <- y1 - trans$plate[component]*(x1 - median(x1, na.rm = T)) - median(y1, na.rm = T)
        }
        if (isTruthy(trans$gia) && input$giaType == 2) {
          y1 <- y1 - trans$gia[component]*(x1 - median(x1, na.rm = T)) - median(y1, na.rm = T)
        }
        sy1 <- db1[[info$db1]][[paste0("sy",component)]][db1[[info$db1]][[paste0("status",component)]] %in% T]
        title <- ""
        sigmas <- F
        if (isTruthy(input$sigmas) && ((info$format == 4 && isTruthy(inputs$errorBar)) || input$format != 4)) {
          sigmas <- T
        }
        if (length(ranges$x1) > 0 && !isTruthy(ranges$y1)) {
          rangeY <- range(y0[x0 > ranges$x1[1] & x0 < ranges$x1[2]])
        } else {
          rangeY <- ranges$y1
        }
        if (input$optionSecondary == 1 && sum(abs(db2[[info$db2]][[paste0("y",component)]]), na.rm = T) > 0) {
          x2 <- db2[[info$db2]][[paste0("x",input$tunits)]]
          y2 <- db2[[info$db2]][[paste0("y",component)]]
          if (isTruthy(trans$plate2) && input$eulerType == 2 && isTruthy(inputs$station_x2) && isTruthy(inputs$station_y2) && isTruthy(inputs$station_z2)) {
            y2 <- y2 - trans$plate2[component]*(x2 - median(x2, na.rm = T)) - median(y2, na.rm = T)
          }
          if (isTruthy(trans$gia2) && input$giaType == 2) {
            y2 <- y2 - trans$gia2[component]*(x2 - median(x2, na.rm = T)) - median(y2, na.rm = T)
          }
          y2 <- y2 * inputs$scaleFactor
          sy2 <- db2[[info$db2]][[paste0("sy",component)]]
          if (input$symbol == 0) {
            symbol <- 'p'
          } else if (input$symbol == 1) {
            symbol <- 'l'
          } else if (input$symbol == 2) {
            symbol <- 'o'
          }
          if (isTruthy(input$sameScale)) {
            pointsX1 <- x1[x1 > ranges$x1[1] & x1 < ranges$x1[2]]
            pointsX2 <- x2[x2 > ranges$x1[1] & x2 < ranges$x1[2]]
            pointsY1 <- y1[x1 > ranges$x1[1] & x1 < ranges$x1[2]]
            pointsY2 <- y2[x2 > ranges$x1[1] & x2 < ranges$x1[2]]
            half <- abs(rangeY[1] - mean(rangeY))
            middle <- ifelse(isTruthy(pointsY2), median(pointsY2), 0)
            rangeY2 <- c(middle - half, middle + half)
            if (length(pointsX1) == 0 || length(pointsX2) == 0) {
              # NA
            } else if (pointsX2[1] > pointsX1[length(pointsX1)]) {
              # NA
            } else if (pointsX1[1] > pointsX2[length(pointsX2)]) {
              # NA
            } else {
              tie1 <- head(sort(sapply(pointsX1, function(x) min(abs(pointsX2 - x))), index.return = T)$ix, 100)
              tie2 <- head(sort(sapply(pointsX2, function(x) min(abs(pointsX1 - x))), index.return = T)$ix, 100)
              tie1 <- tie1[1:min(length(tie1),length(tie2))]
              tie2 <- tie2[1:min(length(tie1),length(tie2))]
              pointsBias <- median(pointsY1[tie1] - pointsY2[tie2])
              rangeY2 <- rangeY2 + (rangeY[1] - rangeY2[1]) - pointsBias
            }
          } else if (isTruthy(input$same_axis)) {
            rangeY2 <- rangeY
          } else {
            ids <- x2 >= ranges$x1[1] & x2 <= ranges$x1[2]
            if (sum(ids) > 0) {
              rangeY2 <- range(y2[ids], na.rm = T) 
            } else {
              rangeY2 <- range(y2, na.rm = T)
            }
          }
          plot(x2, y2, type = symbol, lwd = 2, pch = 20, col = SARIcolors[3], axes = F, xlab = NA, ylab = NA, xlim = ranges$x1, ylim = rangeY2)
          if (isTruthy(sigmas)) {
            color <- SARIcolors[3]
            alfa <- 0.2
            shade <- adjustcolor(color, alpha.f = alfa)
            ba <- y2 + sy2
            bb <- y2 - sy2
            polygon(c(x2, rev(x2)), c(ba, rev(bb)), col = shade, border = NA)
          }
          axis(side = 4, at = NULL, labels = T, tick = T, line = NA, pos = NA, outer = F)
          par(new = T)
        }
        plot_series(x1,y1,sy1,ranges$x1,rangeY,sigmas,title,input$symbol,T)
        points(xe, ye, type = "p", col = SARIcolors[2], bg = 2, pch = 21)
        output[[paste0("component4",component)]] <- renderText(sub(" component", "", info$components[component]))
        shinyjs::show(paste0("component4",component))
        xx <- median(x1[x1 > ranges$x1[1] & x1 < ranges$x1[2]], na.rm = T)
        yy <- median(y1[x1 > ranges$x1[1] & x1 < ranges$x1[2]], na.rm = T)
        centerx <- which(abs(x1 - xx) == min(abs(x1 - xx)))[1]
        centery <- which(abs(y1 - yy) == min(abs(y1 - yy)))[1]
        if (input$eulerType == 1 && length(trans$plate[!is.na(trans$plate)]) == 3) {
            rate <- trans$plate[component]
        }
        if (input$giaType == 1 && length(trans$gia[!is.na(trans$gia)]) == 3) {
          if (exists("rate") && is.numeric(rate)) {
            rate <- rate + trans$gia[component]
          } else {
            rate <- trans$gia[component]
          }
        }
        if (exists("rate") && is.numeric(rate)) {
          lines(c(x1[1],x1[length(x1)]),c(y1[centery] + rate*(x1[1] - x1[centerx]),y1[centery] + rate*(x1[length(x1)] - x1[centerx])), col = SARIcolors[4], lwd = 3)
        }
        if (input$traceLog && length(info$log) > 0) {
          for (r in info$log[[2]]) {
            abline(v = r, col = SARIcolors[4], lty = 2)
          }
          for (a in info$log[[1]]) {
            abline(v = a, col = SARIcolors[4])
          }
        }
        if (input$traceSinfo && length(info$sinfo) > 0) {
          for (r in info$sinfo[[2]]) {
            abline(v = r, col = SARIcolors[6], lty = 2)
          }
          for (a in info$sinfo[[1]]) {
            abline(v = a, col = SARIcolors[6])
          }
        }
        if (input$traceSoln && length(info$soln) > 0) {
          for (a in info$soln) {
            abline(v = a, col = SARIcolors[8])
          }
        }
        if (input$traceCustom && length(info$custom) > 0) {
          for (a in info$custom) {
            abline(v = a, col = SARIcolors[5])
          }
        }
        if (length(trans[[paste0("mod",component,"y")]]) > 0) {
          lines(trans[[paste0("res",component,"x")]],trans[[paste0("mod",component,"y")]], col = SARIcolors[2], lwd = 3)
        }
        if (length(trans$filter) > 0 && input$filter == T && input$series2filter == 1) {
          lines(x1,trans$filter, col = SARIcolors[7], lwd = 3)
        }
        if (ranges$x1[1] > info$minx || ranges$x1[2] < info$maxx) {
          shinyjs::show(paste0("zoomin",input$tab))
        } else {
          shinyjs::hide(paste0("zoomin",input$tab))
        }
  }
  #
  periodogram <- function(serie) {
    req(trans$fs)
    if (messages > 0) cat(file = stderr(), mySession, "Computing periodogram", serie, "\n")
    withProgress(message = 'Computing  periodogram.',
                 detail = 'This may take a while ...', value = 0, {
                   incProgress(0.5)
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
                   if (input$spectrumResiduals && length(trans$res) > 0 && any("all" %in% serie || "residuals" %in% serie)) {
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
                   trans$spectra_old <- c(input$spectrumOriginal,input$spectrumModel,input$spectrumResiduals,input$spectrumFilter,input$spectrumFilterRes)
                 })
  }
  #
  vondrak <- function(x,y,yp,p) {
    #code adapted from Sylvain Loyer's Fortran code and from Vondrak's 1969 paper
    removeNotification("bad_vondrak_period")
    n <- length(x)
    p <- as.numeric(p)/(0.791020*log(n) - 2.339407)
    yp <- yp/sum(yp)
    xdim <- n - 3
    sr <- as.numeric(tail(x, n = 1) - x[1])
    if (p/sr >= 0.1) {
      showNotification(HTML("The input period of the Vondrak filter is larger than T/10.<br>Results may be unreliable."), action = NULL, duration = 10, closeButton = T, id = "bad_vondrak_period", type = "warning", session = getDefaultReactiveDomain())
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
  #
  trim    <- function(x) { gsub("^\\s+|\\s+$", "", x) }
  #
  collect <- function(file_out) {
    if (messages > 0) cat(file = stderr(), mySession, "Downloading results", "\n")
    id <- showNotification("Preparing file to download ...", action = NULL, duration = NULL, closeButton = T, id = NULL, type = "warning", session = getDefaultReactiveDomain())
    now <- paste0(" run on ",Sys.time()," ",Sys.timezone())
    cat(paste0("# ",version,now), file = file_out, sep = "\n", fill = F, append = F)
    cat(paste0("# Original series: ",file$primary$name), file = file_out, sep = "\n", fill = F, append = T)
    if (input$format != 4) {
      cat(paste0("# Coordinate component: ", info$components[as.numeric(input$tab)]), file = file_out, sep = "\n", fill = F, append = T)
    } else {
      if (isTruthy(input$sigmas)) {
        cat(paste0("# Column numbers for data and errorbars: ",inputs$variable," ",inputs$errorBar), file = file_out, sep = "\n", fill = F, append = T)
      } else {
        cat(paste0("# Column number for data: ",inputs$variable), file = file_out, sep = "\n", fill = F, append = T)
      }
    }
    if (input$tunits == 1) {
      period <- "day"
      periods <- "days"
    } else if (input$tunits == 2) {
      period <- "week"
      periods <- "weeks"
    } else if (input$tunits == 3) {
      period <- "year"
      periods <- "years"
    }
    if (input$sunits == 1) {
      unit <- "m"
      units <- paste0("m/",period)
    } else if (input$sunits == 2) {
      unit <- "mm"
      units <- paste0("mm/",period)
    } else {
      unit <- ""
      units <- ""
    }
    cat(paste('# Series units:', unit, periods, units), file = file_out, sep = "\n", fill = F, append = T)
    if (isTruthy(inputs$step) && inputs$step > 0) {
      if (info$stepUnit == 1) {
        stepUnit <- "days"
      } else if (info$stepUnit == 2) {
        stepUnit <- "weeks"
      } else if (info$stepUnit == 3) {
        stepUnit <- "years"
      }
      cat(paste('# Resampling:', inputs$step, stepUnit), file = file_out, sep = "\n", fill = F, append = T)
    }
    if (input$optionSecondary == 2) {
      if (length(file$secondary$name) > 1) {
        cat(paste('# Corrected with:', paste(input$product2, collapse = " & ")), file = file_out, sep = "\n", fill = F, append = T)
      } else {
        cat(sprintf('# Corrected with: %s',file$secondary$name), file = file_out, sep = "\n", fill = F, append = T)
      }
    }
    if (input$optionSecondary == 3) {
      if (length(file$secondary$name) > 1) {
        cat(paste('# Averaged with:', paste(input$product2, collapse = " & ")), file = file_out, sep = "\n", fill = F, append = T)
      } else {
        cat(sprintf('# Averaged with: %s',file$secondary$name), file = file_out, sep = "\n", fill = F, append = T)
      }
    }
    if (input$eulerType == 2 && length(trans$plate) > 0) {
      cat(paste(sprintf('# Plate model rate removed: %f', trans$plate[as.numeric(input$tab)]), units, "from model", input$plateModel, "and plate", input$plate), file = file_out, sep = "\n", fill = F, append = T)
    }
    if (input$fitType == 1 && length(trans$results) > 0) {
      cat(paste0("# Model LS: ",gsub(" > ", ">", gsub(" - ", "-", gsub(" \\* ", "\\*", gsub("))", ")", gsub("I\\(x>", "if(x>", gsub("I\\(cos", "cos", gsub("I\\(sin", "sin", gsub("^ *|(?<= ) | *$", "", Reduce(paste, trans$equation), perl = TRUE))))))))), file = file_out, sep = "\n", fill = F, append = T)
      for (i in seq_len(length(dimnames(trans$LScoefs)[[1]]))) {
        max_decimals <- signifdecimal(trans$LScoefs[i,2], F) + 2
        cat(paste('# Parameter:', dimnames(trans$LScoefs)[[1]][i], '=', formatting(trans$LScoefs[i,1],1), '+/-', formatting(trans$LScoefs[i,2],1)), file = file_out, sep = "\n", fill = F, append = T)
      }
      if (isTruthy(trans$results$sinusoidales)) {
        for (i in 1:dim(trans$results$sinusoidales)[1]) {
          cat(paste('# Sinusoidal period', sprintf('%*s', max(nchar(trans$results$sinusoidales[,1])), trans$results$sinusoidales[i,1]), ':   Amplitude', formatting(trans$results$sinusoidales[i,2],1), '+/-', formatting(trans$results$sinusoidales[i,3],1), unit, '   Phase ', formatting(trans$results$sinusoidales[i,4],2), '+/-', formatting(trans$results$sinusoidales[i,5],2), 'rad'), file = file_out, sep = "\n", fill = F, append = T)
        } 
      }
    } else if (input$fitType == 2 && length(trans$kalman) > 0) {
      if (input$kf == 1) {
        cat(paste0("# Model EKF: ",gsub(" > ", ">", gsub(" - ", "-", gsub(" \\* ", "\\*", gsub("))", ")", gsub("I\\(x>", "if(x>", gsub("I\\(cos", "cos", gsub("I\\(sin", "sin", gsub("^ *|(?<= ) | *$", "", Reduce(paste, trans$equation), perl = TRUE))))))))), file = file_out, sep = "\n", fill = F, append = T)
      } else if (input$kf == 2) {
        cat(paste0("# Model UKF: ",gsub(" > ", ">", gsub(" - ", "-", gsub(" \\* ", "\\*", gsub("))", ")", gsub("I\\(x>", "if(x>", gsub("I\\(cos", "cos", gsub("I\\(sin", "sin", gsub("^ *|(?<= ) | *$", "", Reduce(paste, trans$equation), perl = TRUE))))))))), file = file_out, sep = "\n", fill = F, append = T)
      }
      cat(paste('# Parameter:', colnames(trans$kalman), '=', formatting(colMeans(trans$kalman),1), '+/-', formatting(colMeans(trans$kalman_unc),1)), file = file_out, sep = "\n", fill = F, append = T)
      cat(paste('# A priori:', trans$kalman_info$nouns, '=', formatting(trans$kalman_info$apriori,1), '+/-', formatting(trans$kalman_info$error,1)), file = file_out, sep = "\n", fill = F, append = T)
      cat(paste('# Process noise:', trans$kalman_info$nouns, '=', as.list(formatting(sqrt(trans$kalman_info$processNoise),1))), file = file_out, sep = "\n", fill = F, append = T)
      cat(paste('# Measurement noise:', formatting(inputs$ObsError,1), unit), file = file_out, sep = "\n", fill = F, append = T)
    }
    if (length(trans$offsetEpochs) > 0) {
      cat(paste0('# Discontinuities at: ',paste(trans$offsetEpochs, collapse = ", ")), file = file_out, sep = "\n", fill = F, append = T)
    }
    if (isTruthy(trans$midas_vel) && isTruthy(input$midas)) {
      if (isTruthy(trans$midas_vel2)) {
        cat(paste('# MIDAS:', formatting(trans$midas_vel,1), '+/-', formatting(trans$midas_sig,1), units, '#discontinuities included'), file = file_out, sep = "\n", fill = F, append = T)
        cat(paste('# MIDAS:', formatting(trans$midas_vel2,1), '+/-', formatting(trans$midas_sig2,1), units, '#discontinuities skipped'), file = file_out, sep = "\n", fill = F, append = T)
      } else {
        cat(paste('# MIDAS:', formatting(trans$midas_vel,1), '+/-', formatting(trans$midas_sig,1), units), file = file_out, sep = "\n", fill = F, append = T)
      }
    }
    if (isTruthy(trans$entropy_vel) && isTruthy(input$entropy)) {
      cat(paste('# Minimum entropy rate:', formatting(trans$entropy_vel,1), '+/-', formatting(trans$entropy_sig,1), units), file = file_out, sep = "\n", fill = F, append = T)
    }
    if (input$waveform && inputs$waveformPeriod > 0) {
      cat(paste('# Waveform:', as.numeric(inputs$waveformPeriod), periods), file = file_out, sep = "\n", fill = F, append = T)
    }
    if (isTruthy(input$filter)) {
      if (isTruthy(trans$vondrak) && (isTruthy(inputs$low) || isTruthy(inputs$high))) {
        if (input$series2filter == 1) {
          origen <- " from original series"
        } else if (input$series2filter == 2) {
          origen <- " from residual series"
        }
        if (isTruthy(trans$vondrak[1]) && isTruthy(trans$vondrak[2])) {
          cat(paste('# Vondrak:', trans$vondrak[1], periods, '(low)', trans$vondrak[2], periods, '(high)', origen), file = file_out, sep = "\n", fill = F, append = T)
        } else if (isTruthy(trans$vondrak[1])) {
          cat(paste('# Vondrak:', trans$vondrak[1], periods, '(low)', origen), file = file_out, sep = "\n", fill = F, append = T)
        } else if (isTruthy(trans$vondrak[2])) {
          cat(paste('# Vondrak:', trans$vondrak[2], periods, '(high)'), file = file_out, sep = "\n", fill = F, append = T)
        }
      }
    }
    if (isTruthy(trans$noise) && (isTruthy(input$mle))) {
      if (isTruthy(trans$noise[1])) {
        cat(paste('# Noise: WH', formatting(trans$noise[1],1), '+/-', formatting(trans$noise[2],1), unit), file = file_out, sep = "\n", fill = F, append = T)
      }
      if (isTruthy(trans$noise[3])) {
        cat(paste('# Noise: FL', formatting(trans$noise[3],1), '+/-', formatting(trans$noise[4],1), unit, paste0(period,"^(-1/4)")), file = file_out, sep = "\n", fill = F, append = T)
      }
      if (isTruthy(trans$noise[5])) {
        cat(paste('# Noise: RW', formatting(trans$noise[5],1), '+/-', formatting(trans$noise[6],1), unit, paste0(period,"^(-1/2)")), file = file_out, sep = "\n", fill = F, append = T)
      }
      if (isTruthy(trans$noise[7])) {
        cat(paste('# Noise: PL', formatting(trans$noise[7],1), '+/-', formatting(trans$noise[8],1), unit, paste0(period,"^(K/4)")), file = file_out, sep = "\n", fill = F, append = T)
      }
      if (isTruthy(trans$noise[9])) {
        cat(paste('# Noise: K', format(trans$noise[9], nsmall = 3, digits = 0, scientific = F, trim = F), '+/-', format(trans$noise[10], nsmall = 3, digits = 0, scientific = F, trim = F)), file = file_out, sep = "\n", fill = F, append = T)
      }
      if (isTruthy(trans$noise[11])) {
        cat(sprintf('# Noise: MLE %s', format(trans$noise[11]/-1, nsmall = 2, digits = 0, scientific = F, trim = F)), file = file_out, sep = "\n", fill = F, append = T)
      }
    }
    if (isTruthy(input$sigmas)) {
      OutPut$df <- data.frame(x = trans$x, y = trans$y, sy = trans$sy)
      names(OutPut$df) <- c("# Epoch", "Data", "Sigma")
    } else {
      OutPut$df <- data.frame(x = trans$x, y = trans$y)
      names(OutPut$df) <- c("# Epoch", "Data")
    }
    req(OutPut$df)
    if (isTruthy(info$scientific)) {
      digits <- info$decimalsy
    } else {
      digits <- 0
    }
    OutPut$df[,"# Epoch"] <- format(OutPut$df[,"# Epoch"], nsmall = info$decimalsx, digits = 0, trim = F, scientific = F, width = info$decimalsx)
    OutPut$df[,"Data"] <- formatting(OutPut$df[,"Data"],0)
    if (isTruthy(input$sigmas)) {
      OutPut$df[,"Sigma"] <- formatting(OutPut$df[,"Sigma"],0)
    }
    
    if ((input$fitType == 1 || input$fitType == 2) && length(trans$res) > 0) {
      if (length(trans$pattern) > 0 && input$waveform && inputs$waveformPeriod > 0) {
        OutPut$df$Model <- formatting(trans$mod - trans$pattern,1)
        OutPut$df$Residuals <- formatting(trans$res + trans$pattern,1)
      } else {
        OutPut$df$Model <- formatting(trans$mod,1)
        OutPut$df$Residuals <- formatting(trans$res,1)
      }
    }
    if (input$fitType == 1 && length(input$model) > 0) {
      if (length(trans$moderror) > 0) {
        OutPut$df$Sigma.Model <- formatting(trans$moderror,1)
      }
      if (length(trans$reserror) > 0) {
        OutPut$df$Sigma.Residuals <- formatting(trans$reserror,1)
      }
    }
    if (input$filter == T && (inputs$low != "" || inputs$high != "") && length(trans$filter) > 0) {
      if (length(trans$pattern) > 0 && input$waveform && inputs$waveformPeriod > 0 && length(trans$filterRes) > 0 && input$series2filter == 1) {
        OutPut$df$Smooth <- formatting(trans$filter - trans$pattern,1)
        OutPut$df$Smooth.Residuals <- formatting(trans$filterRes + trans$pattern,1)
      } else {
        OutPut$df$Smooth <- formatting(trans$filter,1)
        OutPut$df$Smooth.Residuals <- formatting(trans$filterRes,1)
      }
    }
    if (input$wiener && input$mle) {
      if (input$white && length(trans$white) > 0) {
        OutPut$df$White <- formatting(trans$white,1)
        if (input$sigmas && length(trans$white_sig) > 0) {
          OutPut$df$Sigma.White <- formatting(trans$white_sig,1)
        }
      }
      if (input$flicker && length(trans$flicker) > 0) {
        OutPut$df$Flicker <- formatting(trans$flicker,1)
        if (input$sigmas && length(trans$flicker) > 0) {
          OutPut$df$Sigma.Flicker <- formatting(trans$flicker_sig,1)
        }
      }
      if (input$randomw && length(trans$randomw) > 0) {
        OutPut$df$RandomWalk <- formatting(trans$randomw,1)
        if (input$sigmas && length(trans$randomw_sig) > 0) {
          OutPut$df$Sigma.RandomWalk <- formatting(trans$randomw_sig,1)
        }
      }
      if (input$powerl && length(trans$powerl) > 0) {
        OutPut$df$PowerLaw <- formatting(trans$powerl,1)
        if (input$sigmas && length(trans$powerl_sig) > 0) {
          OutPut$df$Sigma.PowerLaw <- formatting(trans$powerl_sig,1)
        }
      }
    }
    if (input$fitType == 2 && length(trans$res) > 0) {
      OutPut$df <- cbind(OutPut$df,formatting(trans$kalman,1))
      colnames(trans$kalman_unc) <- paste0("sigma.",colnames(trans$kalman_unc))
      OutPut$df <- cbind(OutPut$df,formatting(trans$kalman_unc,1))
    }
    if (length(trans$pattern) > 0 && input$waveform && inputs$waveformPeriod > 0) {
      OutPut$df$Waveform <- formatting(trans$pattern,1)
    }
    if (isTruthy(input$add_excluded)) {
      if (isTruthy(input$sigmas)) {
        output_excluded$df <- data.frame(x = trans$xe, y = trans$ye, sy = trans$sye)
        names(output_excluded$df) <- c("# Epoch", "Data", "Sigma")
      } else {
        output_excluded$df <- data.frame(x = trans$xe, y = trans$ye)
        names(output_excluded$df) <- c("# Epoch", "Data")
      }
      output_excluded$df[,"# Epoch"] <- format(output_excluded$df[,"# Epoch"], nsmall = info$decimalsx, digits = 0, trim = F,scientific = F)
      output_excluded$df[,"Data"] <- formatting(output_excluded$df[,"Data"],0)
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
    shinyjs::delay(2000, removeNotification(id = id, session = getDefaultReactiveDomain()))
  }
  #
  collect_periodogram <- function(file_out) {
    removeNotification("bad_output")
    if (messages > 0) cat(file = stderr(), mySession, "Downloading periodogram", "\n")
    now <- paste0(" run on ",Sys.time()," ",Sys.timezone())
    cat(paste0("# ",version,now), file = file_out, sep = "\n", fill = F, append = F)
    cat(paste0("# Original series: ",file$primary$name), file = file_out, sep = "\n", fill = F, append = T)
    if (input$format != 4) {
      cat(paste0("# Coordinate component: ", info$components[as.numeric(input$tab)]), file = file_out, sep = "\n", fill = F, append = T)
    } else {
      if (isTruthy(input$sigmas)) {
        cat(paste0("# Column numbers for data and errorbars: ",inputs$variable," ",inputs$errorBar), file = file_out, sep = "\n", fill = F, append = T)
      } else {
        cat(paste0("# Column number for data: ",inputs$variable), file = file_out, sep = "\n", fill = F, append = T)
      }
    }
    if (input$tunits == 1) {
      period <- "days"
    } else if (input$tunits == 2) {
      period <- "weeks"
    } else if (input$tunits == 3) {
      period <- "years"
    }
    if (input$sunits == 1) {
      units <- "(m)"
    } else if (input$sunits == 2) {
      units <- "(mm)"
    } else {
      units <- ""
    }
    if (input$spectrumType == 0) {
      cat(paste("# Amplitude spectrum", units), file = file_out, sep = "\n", fill = F, append = T)
    } else if (input$spectrumType == 1) {
      cat("# Power spectrum", file = file_out, sep = "\n", fill = F, append = T)
    }
    cat(paste("# Longest period  =",inputs$long_period, period), file = file_out, sep = "\n", fill = F, append = T)
    cat(paste("# Shortest period =",inputs$short_period, period), file = file_out, sep = "\n", fill = F, append = T)
    cat(paste("# Oversampling =",inputs$ofac), file = file_out, sep = "\n", fill = F, append = T)
    OutPut$df <- as.data.frame(trans$spectra)
    if (isTruthy(OutPut$df) && ncol(OutPut$df) > 1) {
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
      if (input$spectrumResiduals && length(trans$res) > 0) {
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
      suppressWarnings(write.table(format(OutPut$df, digits = 6, nsmall = 6, trim = F, scientific = F),file_out,append = T,quote = F,sep = "  ",eol = "\n",na = "N/A",dec = ".",row.names = F,col.names = T))
    } else {
      showNotification("Problem when writing the periodogram data to a file.", action = NULL, duration = 10, closeButton = T, id = "bad_output", type = "error", session = getDefaultReactiveDomain())
    }
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

      setProgress(round(i/info$points, digits = 1))

      ##time update

      ## Increase the process noise by a factor depending on the number of missing observations from the last one
      ## This is approximate for series that are irregularly sampled (excluding gaps)
      gapFactor <- 1
      if (i > 1) {
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
      taulog <- which(grepl("TauL", trans$KFnames))
      if (any(sigmay[,taulog] <= 0)) {
        showNotification(HTML("Negative values in some sigma points of the logarithmic decay rate.<br>The logarithmic decay must always be positive."), action = NULL, duration = 10, closeButton = T, id = "bad_sigmaPoints", type = "error", session = getDefaultReactiveDomain())
      }
      #predicted measurement
      tmpy <- matrix(sapply(1:nrow(sigmay), function(x) FFfunction(x = sigmay[x,], k = i)), nrow = ym)
      f[i, ] <- tcrossprod(w, tmpy)
      #covariance of predicted measurement
      # Qy <- tcrossprod(crossprod(t(tmpy - f[i, ]), diag(w)), tmpy - f[i, ]) + mod$V
      if (!isTruthy(mod$V[i])) {
        mod$V[i] <- mod$V[1] # ugly but necessary for running the measurement noise optimization only
      }
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
  #
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

        # setProgress(0.5*i/info$points + 0.5)

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

      setProgress(round(i/info$points, digits = 1))

      ## Increase the process noise by a factor depending on the number of missing observations from the last one
      ## This implies the series must be sampled regularly with data gaps
      if (!isTruthy(mod$V[i])) {
        mod$V[i] <- mod$V[1] # ugly but necessary for running the measurement noise optimization only
      }
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
      if (i > 1) {
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
          ll <- ll + ym*log(2*pi) + sum(log(eigen(Qt)$values)) + crossprod(e, solve(Qt)) %*% e
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
            Qt <- (mod$V[i] + dFF.dx[[i]] %*% Rt %*% t(dFF.dx[[i]]))[good, good]
            ll <- ll + sum(good)*log(2*pi) + sum(log(eigen(Qt)$values)) + crossprod(e, solve(Qt)) %*% e
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
  #
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
        if (i > 1) {
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
  #
  midas_vel <- function(m,t,disc,series) {
    setProgress(round(m/info$points, digits = 1))
    vel_f <- -999999
    vel_b <- -999999
    index_f <- which.min(abs(trans$x - (trans$x[m] + t)))
    index_b <- which.min(abs(trans$x - (trans$x[m] - t)))
    # checking forward pair
    if (abs(trans$x[index_f] - trans$x[m] - t) < info$tol) {
      if (disc == 1) {
        if (length(trans$offsetEpochs > 0)) {
          if (!any(trans$x[m] < as.numeric(trans$offsetEpochs) & trans$x[index_f] > as.numeric(trans$offsetEpochs))) {
            vel_f <- (series[index_f] - series[m]) / (trans$x[index_f] - trans$x[m])
          }
        }
      } else {
        vel_f <- (series[index_f] - series[m]) / (trans$x[index_f] - trans$x[m])
      }
    }
    # checking backward pair
    if (abs(trans$x[m] - trans$x[index_b] - t) < info$tol) {
      if (disc == 1) {
        if (length(trans$offsetEpochs > 0)) {
          if (!any(trans$x[index_b] < as.numeric(trans$offsetEpochs) & trans$x[m] > as.numeric(trans$offsetEpochs))) {
            vel_b <- (series[m] - series[index_b]) / (trans$x[m] - trans$x[index_b])
          }
        }
      } else {
        vel_b <- (series[m] - series[index_b]) / (trans$x[m] - trans$x[index_b])
      }
    }
    return(c(vel_f,vel_b))
  }
  #Based on https://stackoverflow.com/questions/5173692/how-to-return-number-of-decimal-places-in-r
  decimalplaces <- function(x,axis) {
    if (any(abs(x - round(x)) > .Machine$double.eps)) {
      d <- quantile(na.omit(nchar(lapply(lapply(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE), `length<-`, 2), `[[`, 2))), probs = 0.95)
      if (isTruthy(d) && d > 0) {
        if (axis == "x") {
          return(d)
        } else if (d < 7) {
          info$scientific <- F
          return(d)
        } else {
          info$scientific <- T
          d <- quantile(na.omit(nchar(sub(pattern = "^.*\\.0*([1-9])", replacement = "\\1", x = x))), probs = 0.05)
          if (isTruthy(d) && d > 0) {
            return(d)
          } else {
            return(0)
          }
        }
      } else {
        return(0)
      }
    } else {
      return(0)
    }
  }
  #
  signifdecimal <- function(x, rounded) {
    if (abs(x) >= 1) {
      return(0)
    } else {
      decimal <- nchar(format(signif(x,1), scientific = F)) - 3
      if (rounded) {
        if (round(x, decimal) > 0) {
          return(decimal)
        } else {
          return(decimal + 1)
        }
      } else {
        return(decimal)
      }
    }
  }
  #
  trim <- function(x) { gsub("^\\s+|\\s+$", "", x) }
  #
  average <- function(p,x,y1,y2,y3,sy1,sy2,sy3,tol,w,s,second,sigmas) {
    index <- x >= x[1] + (p - 1)*s - tol & x < x[1] + p*s - tol * 2/3
    x_ <- y1_ <- y2_ <- y3_ <- NULL
    if (sigmas) sy1_ <- sy2_ <- sy3_ <- NULL
    if (length(x[index]) == 1) {
      x_ <- x[1] + (p - 0.5)*s
      if (isTruthy(second)) {
        y1_ <- y1[index]
        if (isTruthy(y2)) y2_ <- y2[index]
        if (isTruthy(y3)) y3_ <- y3[index]
        if (sigmas) {
          sy1_ <- sy1[index]
          if (isTruthy(sy2)) sy2_ <- sy2[index]
          if (isTruthy(sy3)) sy3_ <- sy3[index]
        }
      } else {
        y1_ <- y1[index & db1$original$status1]
        if (isTruthy(y2)) y2_ <- y2[index & db1$original$status2]
        if (isTruthy(y3)) y3_ <- y3[index & db1$original$status3]
        if (sigmas) {
          sy1_ <- sy1[index & db1$original$status1]
          if (isTruthy(sy2)) sy2_ <- sy2[index & db1$original$status2]
          if (isTruthy(sy3)) sy3_ <- sy3[index & db1$original$status3]
        }
      }
    } else if (length(x[index]) > 1) {
      x_ <- x[1] + (p - 0.5)*s
      if (isTruthy(second)) {
        if (sigmas) {
          y1_ <- weighted.mean(y1[index], 1/(sy1[index])^2, na.rm = T)
          if (isTruthy(y2)) y2_ <- weighted.mean(y2[index], 1/(sy2[index])^2, na.rm = T)
          if (isTruthy(y3)) y3_ <- weighted.mean(y3[index], 1/(sy3[index])^2, na.rm = T)
          sy1_ <- sqrt(1/sum(1/sy1[index]^2, na.rm = T))
          if (isTruthy(sy2)) sy2_ <- sqrt(1/sum(1/sy2[index]^2, na.rm = T))
          if (isTruthy(sy3)) sy3_ <- sqrt(1/sum(1/sy3[index]^2, na.rm = T))
        } else {
          y1_ <- mean(y1[index], na.rm = T)
          if (isTruthy(y2)) y2_ <- mean(y2[index], na.rm = T)
          if (isTruthy(y3)) y3_ <- mean(y3[index], na.rm = T)
        }
      } else {
        if (sigmas) {
          y1_ <- weighted.mean(y1[index & db1$original$status1], 1/(sy1[index & db1$original$status1])^2, na.rm = T)
          if (isTruthy(y2)) y2_ <- weighted.mean(y2[index & db1$original$status2], 1/(sy2[index & db1$original$status2])^2, na.rm = T)
          if (isTruthy(y3)) y3_ <- weighted.mean(y3[index & db1$original$status3], 1/(sy3[index & db1$original$status3])^2, na.rm = T)
          sy1_ <- sqrt(1/sum(1/sy1[index & db1$original$status1]^2, na.rm = T))
          if (isTruthy(sy2)) sy2_ <- sqrt(1/sum(1/sy2[index & db1$original$status2]^2, na.rm = T))
          if (isTruthy(sy3)) sy3_ <- sqrt(1/sum(1/sy3[index & db1$original$status3]^2, na.rm = T))
        } else {
          y1_ <- mean(y1[index & db1$original$status1], na.rm = T)
          if (isTruthy(y2)) y2_ <- mean(y2[index & db1$original$status2], na.rm = T)
          if (isTruthy(y3)) y3_ <- mean(y3[index & db1$original$status3], na.rm = T)
        }
      }
    }
    if (input$format == 4) {
      if (sigmas) {
        if (isTruthy(x_) && isTruthy(y1_) && isTruthy(sy1_)) {
          out <- c(x_,y1_,sy1_)
        } else {
          out <- c(NA,NA,NA)
        }
      } else {
        if (isTruthy(x_) && isTruthy(y1_)) {
          out <- c(x_,y1_)
        } else {
          out <- c(NA,NA)
        }
      }
    } else {
      if (sigmas) {
        if (isTruthy(x_) && isTruthy(y1_) && isTruthy(sy1_) && isTruthy(y2_) && isTruthy(sy2_) && isTruthy(y3_) && isTruthy(sy3_)) {
          out <- c(x_,y1_,y2_,y3_,sy1_,sy2_,sy3_)
        } else {
          out <- c(NA,NA,NA,NA,NA,NA,NA)
        }
      } else {
        if (isTruthy(x_) && isTruthy(y1_) && isTruthy(y2_) && isTruthy(y3_)) {
          out <- c(x_,y1_,y2_,y3_)
        } else {
          out <- c(NA,NA,NA,NA)
        }
      }
    }
    setProgress(round(p/w, digits = 1))
    return(out)
  }
  #
  get_URL_info <- function(server,station,product,series) {
    if (isTruthy(series)) {
      if (series == 1) {
        variable <- "station1"
      } else if (series == 2) {
        variable <- "station2"
      }
    }
    logfile <- NULL
    server <- toupper(server)
    product <- toupper(product)
    ## NGL ####
    if (server == "NGL") {
      format <- 3
      if (product == "FINAL") {
        url <- "http://geodesy.unr.edu/gps_timeseries/tenv3/IGS14/"
      } else if (product == "RAPID") {
        url <- "http://geodesy.unr.edu/gps_timeseries/rapids/tenv3/"
      }
      if (product == "FINAL" || product == "RAPID") {
        pattern <- ".tenv3"
        if (isTruthy(station)) {
          name <- paste0(toupper(station),pattern)
          filepath <- paste0(url,name)
          if (file.exists("www/steps.txt") && series == 1) {
            file$custom$name <- "steps.txt"
            file$custom$datapath <- "www/steps.txt"
            session$sendCustomMessage("custom", "steps.txt")
            shinyjs::delay(100, updateCheckboxInput(inputId = "traceCustom", value = T))
          }
        } else {
          withBusyIndicatorServer(variable, {
            if (file.exists("www/NGL_database.txt")) {
              stations_available <- readLines("www/NGL_database.txt", warn = F)
            } else {
              dir_contents <- try(XML::readHTMLTable(url, skip.rows = 1:2, trim = T)[[1]]$Name, silent = T)
              if (isTruthy(dir_contents) && !inherits(dir_contents,"try-error")) {
                stations_available <- sub(pattern, "", grep(pattern, dir_contents, fixed = T, value = T))
                writeLines(stations_available, "www/NGL_database.txt", sep = "\n")
              } else {
                showNotification(HTML(paste("Server", server, "seems to be unreachable.<br>It is not possible to get the list of available stations.")), action = NULL, duration = 10, closeButton = T, id = "no_answer", type = "warning", session = getDefaultReactiveDomain())
                return(NULL)
              }
            }
            if (series == 1) {
              output$station1 <- renderUI({
                suppressWarnings(selectInput(inputId = "station1", label = "Station", choices = c("Available stations" = "", stations_available), selected = "", selectize = T))
              })  
            } else if (series == 2) {
              output$station2 <- renderUI({
                suppressWarnings(selectInput(inputId = "station2", label = "Station", choices = c("Available stations" = "", stations_available), selected = "", selectize = T))
              })
              if (input$sunits == 2) {
                updateTextInput(session, inputId = "scaleFactor", value = "1000")
              }
            }
            return(NULL)
          })
        }
      } else {
        showNotification(HTML(paste0("Unknown product ",product,".<br>No file was downloaded.")), action = NULL, duration = 10, closeButton = T, id = "bad_url", type = "error", session = getDefaultReactiveDomain())
        return(NULL)
      }
    ## RENAG ####
    } else if (server == "RENAG") {
      format <- 2
      if (product == "UGA") {
        url <- "ftp://webrenag.unice.fr/products/position-timeseries/"
        pattern <- "_raw.pos_UGA_ITRF14.pos"
        if (isTruthy(station)) {
          name <- paste0(toupper(station),pattern)
          filepath <- paste0(url,name)
          url_log <- "ftp://webrenag.unice.fr/sitelogs/"
          logfile <- paste0(url_log,toupper(station),"00FRA.log")
        } else {
          withBusyIndicatorServer(variable, {
            dir_contents <- try(getURL(url, ftp.use.epsv = FALSE, ftplistonly = TRUE, crlf = TRUE), silent = T)
            if (isTruthy(dir_contents) && !inherits(dir_contents,"try-error")) {
              stations_available <- sapply(strsplit(grep(pattern, strsplit(dir_contents, "\r*\n")[[1]], perl = F, value = T, fixed = T), split = pattern, fixed = T), "[[", 1)
              if (series == 1) {
                output$station1 <- renderUI({
                  suppressWarnings(selectInput(inputId = "station1", label = "Station", choices = c("Available stations" = "", stations_available), selected = "", selectize = T))
                })  
              } else if (series == 2) {
                output$station2 <- renderUI({
                  suppressWarnings(selectInput(inputId = "station2", label = "Station", choices = c("Available stations" = "", stations_available), selected = "", selectize = T))
                })
                if (input$sunits == 2) {
                  updateTextInput(session, inputId = "scaleFactor", value = "1000")
                }
              }
            } else {
              showNotification(HTML(paste("Server", server, "seems to be unreachable.<br>It is not possible to get the list of available stations.")), action = NULL, duration = 10, closeButton = T, id = "no_answer", type = "warning", session = getDefaultReactiveDomain())
            }
            return(NULL)
          })
        }
      } else {
        showNotification(HTML(paste0("Unknown product ",product,".<br>No file was downloaded.")), action = NULL, duration = 10, closeButton = T, id = "bad_url", type = "error", session = getDefaultReactiveDomain())
        return(NULL)
      }
    ## JPL ####
    } else if (server == "JPL") {
      format <- 1
      if (product == "REPRO2018A") {
        url <- "https://sideshow.jpl.nasa.gov/pub/JPL_GPS_Timeseries/repro2018a/post/point/"
        pattern <- ".series"
        if (isTruthy(station)) {
          name <- paste0(toupper(station),pattern)
          filepath <- paste0(url,name)
        } else {
          withBusyIndicatorServer(variable, {
            if (!file.exists("www/JPL_database.txt")) {
              fullFile <- try(suppressWarnings(read.table("https://sideshow.jpl.nasa.gov/post/tables/table1.html", skip = 6, fill = T)), silent = T)
              if (isTruthy(fullFile) && !inherits(fullFile,"try-error")) {
                listStations <- fullFile[fullFile$V2 == "POS", c(1,3,4,5)]
                listStations$V3 <- as.numeric(listStations$V3) / 1000
                listStations$V4 <- as.numeric(listStations$V4) / 1000
                listStations$V5 <- as.numeric(listStations$V5) / 1000
                write.table(listStations, "www/JPL_database.txt", append = F, quote = F, sep = " ", row.names = F, col.names = F)
              } else {
                showNotification(HTML(paste("Server", server, "seems to be unreachable.<br>It is not possible to get the list of available stations.")), action = NULL, duration = 10, closeButton = T, id = "no_answer", type = "warning", session = getDefaultReactiveDomain())
                return(NULL)
              }
            }
            stations_available <- read.table("www/JPL_database.txt")$V1
            if (series == 1) {
              output$station1 <- renderUI({
                suppressWarnings(selectInput(inputId = "station1", label = "Station", choices = c("Available stations" = "", stations_available), selected = "", selectize = T))
              })  
            } else if (series == 2) {
              output$station2 <- renderUI({
                suppressWarnings(selectInput(inputId = "station2", label = "Station", choices = c("Available stations" = "", stations_available), selected = "", selectize = T))
              })
              if (input$sunits == 2) {
                updateTextInput(session, inputId = "scaleFactor", value = "1000")
              }
            }
            return(NULL)
          })
        }
      } else {
        showNotification(HTML(paste0("Unknown product ",product,".<br>No file was downloaded.")), action = NULL, duration = 10, closeButton = T, id = "bad_url", type = "error", session = getDefaultReactiveDomain())
        return(NULL)
      }
    ## IGS ####
    } else if (server == "IGS") {
      format <- 1
      if (product == "IGS20") {
        url <- "ftp://igs-rf.ign.fr/pub/crd/"
        pattern <- "_igs.plh"
        if (isTruthy(station)) {
          name <- paste0(toupper(station),pattern)
          filepath <- paste0(url,name)
          if (file.exists("www/soln.snx") && series == 1) {
            file$soln$name <- "soln.snx"
            file$soln$datapath <- "www/soln.snx"
            session$sendCustomMessage("soln", "soln.snx")
            shinyjs::delay(100, updateCheckboxInput(inputId = "traceSoln", value = T))
          }
        } else {
          withBusyIndicatorServer(variable, {
            dir_contents <- try(getURL(url, ftp.use.epsv = FALSE, ftplistonly = TRUE, crlf = TRUE), silent = T)
            if (isTruthy(dir_contents) && !inherits(dir_contents,"try-error")) {
              stations_available <- sapply(strsplit(grep(pattern, strsplit(dir_contents, "\r*\n")[[1]], perl = F, value = T, fixed = T), split = pattern, fixed = T), "[[", 1)
              if (series == 1) {
                output$station1 <- renderUI({
                  suppressWarnings(selectInput(inputId = "station1", label = "Station", choices = c("Available stations" = "", stations_available), selected = "", selectize = T))
                })  
              } else if (series == 2) {
                output$station2 <- renderUI({
                  suppressWarnings(selectInput(inputId = "station2", label = "Station", choices = c("Available stations" = "", stations_available), selected = "", selectize = T))
                })
                if (input$sunits == 2) {
                  updateTextInput(session, inputId = "scaleFactor", value = "1000")
                }
              }
            } else {
              showNotification(HTML(paste("Server", server, "seems to be unreachable.<br>It is not possible to get the list of available stations.")), action = NULL, duration = 10, closeButton = T, id = "no_answer", type = "warning", session = getDefaultReactiveDomain())
            }
            return(NULL)
          })
        }
      } else {
        showNotification(HTML(paste0("Unknown product ",product,".<br>No file was downloaded.")), action = NULL, duration = 10, closeButton = T, id = "bad_url", type = "error", session = getDefaultReactiveDomain())
        return(NULL)
      }
    ## SONEL ####
    } else if (server == "SONEL") {
      format <- 1
      if (product == "ULR7A") {
        if (isTruthy(station)) {
          name <- paste0(toupper(station), ".neu")
          filepath <- paste0("https://api.sonel.org/v1/products/vlm/gnss/timeseries?solution=ULR7A&acro=",station,"&format=neu&sampling=daily")
          url_log <- "ftp://ftp.sonel.org/meta/gpslog/"
          dir_contents <- try(getURL(url_log, ftp.use.epsv = FALSE, ftplistonly = TRUE, crlf = TRUE), silent = T)
          if (isTruthy(dir_contents) && !inherits(dir_contents,"try-error")) {
            found <- grep(paste0("^",tolower(station)), strsplit(dir_contents, "\r*\n")[[1]], perl = T, value = T, fixed = F)
            if (isTruthy(found)) {
              logfile <- paste0(url_log,found)
            }
          }
        } else {
          url <- "https://api.sonel.org/v1/products/vlm/gnss/meta?mode=solution"
          withBusyIndicatorServer(variable, {
            dir_contents <- try(fromJSON(txt = url), silent = T)
            if (isTruthy(dir_contents) && !inherits(dir_contents,"try-error")) {
              code <- which(dir_contents$code == product)
              if (code > 0) {
                stations_available <- dir_contents$stations[[code]]
                if (series == 1) {
                  output$station1 <- renderUI({
                    suppressWarnings(selectInput(inputId = "station1", label = "Station", choices = c("Available stations" = "", stations_available), selected = "", selectize = T))
                  })  
                } else if (series == 2) {
                  output$station2 <- renderUI({
                    suppressWarnings(selectInput(inputId = "station2", label = "Station", choices = c("Available stations" = "", stations_available), selected = "", selectize = T))
                  })
                  if (input$sunits == 2) {
                    updateTextInput(session, inputId = "scaleFactor", value = "1000")
                  }
                }
              }
            } else {
              showNotification(HTML(paste("Server", server, "seems to be unreachable.<br>It is not possible to get the list of available stations.")), action = NULL, duration = 10, closeButton = T, id = "no_answer", type = "warning", session = getDefaultReactiveDomain())
            }
            return(NULL)
          })
        }
      } else {
        showNotification(HTML(paste0("Unknown product ",product,".<br>No file was downloaded.")), action = NULL, duration = 10, closeButton = T, id = "bad_url", type = "error", session = getDefaultReactiveDomain())
        return(NULL)
      }
    ## UNAVCO ####
    } else if (server == "EARTHSCOPE") {
      format <- 1
      if (isTruthy(product)) {
        if (isTruthy(station)) {
          if (product == "CWU" || product == "PBO" || product == "NMT") {
            name <- paste0(toupper(station),".",tolower(product),".igs14.pos")
            filepath <- paste0("https://web-services.unavco.org/gps/data/position/", toupper(station), "/v3?analysisCenter=", tolower(product), "&referenceFrame=igs14&starttime=&endtime=&report=long&dataPostProcessing=Uncleaned&refCoordOption=from_analysis_center")
          } else {
            return(NULL)
          }
        } else {
          withBusyIndicatorServer(variable, {
            if (file.exists("www/UNAVCO_database.txt")) {
              stations_available <- readLines("www/UNAVCO_database.txt", warn = F)
            } else {
              tmp_unavco <- tempfile()
              download.file("https://web-services.unavco.org/gps/metadata/sites/v1?minlatitude=-90&maxlatitude=90&minlongitude=-180&maxlongitude=180&starttime=&endtime=&summary=false", destfile = tmp_unavco, method = "curl", extra = "-X GET", headers = c(accept = "text/csv"), quiet = T, cacheOK = F)
              stations_available <- sort(unique(read.csv(tmp_unavco, comment.char = "#")[,1]))
              writeLines(stations_available, "www/UNAVCO_database.txt", sep = "\n")
            }
            if (series == 1) {
              output$station1 <- renderUI({
                suppressWarnings(selectInput(inputId = "station1", label = "Station", choices = c("Available stations" = "", stations_available), selected = "", selectize = T))
              })  
            } else if (series == 2) {
              output$station2 <- renderUI({
                suppressWarnings(selectInput(inputId = "station2", label = "Station", choices = c("Available stations" = "", stations_available), selected = "", selectize = T))
              })
              if (input$sunits == 2) {
                updateTextInput(session, inputId = "scaleFactor", value = "1000")
              }
            }
            return(NULL)
          })
        }
      } else {
        return(NULL)
      }
    ## EUREF ####
    } else if (server == "EUREF") {
      format <- 2
      if (product == "IGB14") {
        url <- "https://epncb.eu/ftp/product/cumulative/C2235/pbo/"
        pattern <- ".pos"
        if (isTruthy(station)) {
          name <- paste0(toupper(station),pattern)
          filepath <- paste0(url,name)
          url_log <- "https://gnss-metadata.eu/data/station/log/"
          found <- grep(paste0(tolower(station),""), readHTMLTable(readLines(url_log), header = F)$list$V1, perl = F, value = T, fixed = T)
          if (isTruthy(found)) {
            logfile <- paste0(url_log,found)
          }
        } else {
          withBusyIndicatorServer(variable, {
            dir_contents <- try(readHTMLTable(getURL(url, crlf = TRUE), skip.rows = 1:2, trim = T)[[1]]$Name, silent = T)
            if (isTruthy(dir_contents) && !inherits(dir_contents,"try-error")) {
              stations_available <- sub(pattern, "", grep(pattern, dir_contents, ignore.case = F, value = T))
              if (series == 1) {
                output$station1 <- renderUI({
                  suppressWarnings(selectInput(inputId = "station1", label = "Station", choices = c("Available stations" = "", stations_available), selected = "", selectize = T))
                })  
              } else if (series == 2) {
                output$station2 <- renderUI({
                  suppressWarnings(selectInput(inputId = "station2", label = "Station", choices = c("Available stations" = "", stations_available), selected = "", selectize = T))
                })
                if (input$sunits == 2) {
                  updateTextInput(session, inputId = "scaleFactor", value = "1000")
                }
              }
            } else {
              showNotification(HTML(paste("Server", server, "seems to be unreachable.<br>It is not possible to get the list of available stations.")), action = NULL, duration = 10, closeButton = T, id = "no_answer", type = "warning", session = getDefaultReactiveDomain())
            }
            return(NULL)
          })
        }
      } else {
        showNotification(paste0("Unknown product ",product,". No file was downloaded."), action = NULL, duration = 10, closeButton = T, id = "bad_url", type = "error", session = getDefaultReactiveDomain())
        return(NULL)
      }
    ## FORMATER ####
    } else if (server == "FORMATER") {
      if (product == "SPOTGINS_POS") {
        format <- 1
        pattern1 <- "SPOTGINS_"
        pattern2 <- ".enu"
        name <- paste0(pattern1,toupper(station),pattern2)
      } else if (product == "UGA_POS") {
        format <- 2
        pattern1 <- "UGA_"
        pattern2 <- ".pos"
        name <- paste0(pattern1,toupper(station),pattern2)
      }
      url <- "https://geodesy-plotter.ipgp.fr/"
      if (product == "SPOTGINS_POS" || product == "UGA_POS") {
        if (isTruthy(station)) {
          filepath <- paste0(url,"data/",toupper(station),"/",name)
          if (series == 1) {
            if (product == "SPOTGINS_POS") {
              if (file.exists("www/formater_offset.dat")) {
                file$custom$name <- "formater_offset.dat"
                file$custom$datapath <- "www/formater_offset.dat"
                session$sendCustomMessage("custom", "formater_offset.dat")
                shinyjs::delay(100, updateCheckboxInput(inputId = "traceCustom", value = T))
              }
            } else if (product == "UGA_POS") {
              url_log <- "https://gnss-metadata.eu/data/station/log/"
              found <- grep(paste0(tolower(station),""), readHTMLTable(readLines(url_log), header = F)$list$V1, perl = F, value = T, fixed = T)
              if (isTruthy(found)) {
                logfile <- paste0(url_log,found)
              }
            }
          }
        } else {
          withBusyIndicatorServer(variable, {
            url <- paste0(url,"api/1.0/products/?output=csv")
            dir_contents <- try(read.csv(url, skip = 2, header = T), silent = T)
            if (isTruthy(dir_contents) && !inherits(dir_contents,"try-error")) {
              stations_available <- sub(pattern1, "", sub(pattern2, "", grep(pattern1, dir_contents$NAME, fixed = T, value = T), ignore.case = T), ignore.case = T)
              if (length(stations_available) > 0) {
                if (series == 1) {
                  output$station1 <- renderUI({
                    suppressWarnings(selectInput(inputId = "station1", label = "Station", choices = c("Available stations" = "", stations_available), selected = "", selectize = T))
                  })  
                } else if (series == 2) {
                  output$station2 <- renderUI({
                    suppressWarnings(selectInput(inputId = "station2", label = "Station", choices = c("Available stations" = "", stations_available), selected = "", selectize = T))
                  })
                  if (input$sunits == 2) {
                    updateTextInput(session, inputId = "scaleFactor", value = "1000")
                  }
                }
              } else {
                showNotification(HTML(paste("Server", server, "seems to be unreachable.<br>It is not possible to get the list of available stations.")), action = NULL, duration = 10, closeButton = T, id = "no_answer", type = "warning", session = getDefaultReactiveDomain())
              }
            } else {
              showNotification(HTML(paste("Server", server, "seems to be unreachable.<br>It is not possible to get the list of available stations.")), action = NULL, duration = 10, closeButton = T, id = "no_answer", type = "warning", session = getDefaultReactiveDomain())
            }
            return(NULL)
          })
        }
      } else {
        showNotification(paste0("Unknown product ",product,". No file was downloaded."), action = NULL, duration = 10, closeButton = T, id = "bad_url", type = "error", session = getDefaultReactiveDomain())
        return(NULL)
      }
    ## EPOS ####
    } else if (server == "EPOS") {
      format <- 1
      if (product == "INGV" || product == "SGO-EPND" || product == "UGA-CNRS" || product == "ROB-EUREF") {
        if (isTruthy(station)) {
          name <- paste0(station,"_",product,".enu")
          station <- toupper(strtrim(station, 4))
          if (product == "INGV") {
            filepath <- paste0("https://gnssproducts.epos.ubi.pt/GlassFramework/webresources/products/timeseries/", station, "/INGV/daily/enu/json/?epoch_start=1990-01-01&epoch_end=2099-12-01")
          } else if (product == "SGO-EPND") {
            filepath <- paste0("https://gnssproducts.epos.ubi.pt/GlassFramework/webresources/products/timeseries/", station, "/SGO-EPND/weekly/enu/json/?epoch_start=1990-01-01&epoch_end=2099-12-01")
          } else if (product == "UGA-CNRS") {
            filepath <- paste0("https://gnssproducts.epos.ubi.pt/GlassFramework/webresources/products/timeseries/", station, "/UGA-CNRS/daily/enu/json/?epoch_start=1990-01-01&epoch_end=2099-12-01")
          } else if (product == "ROB-EUREF") {
            filepath <- paste0("https://gnssproducts.epos.ubi.pt/GlassFramework/webresources/products/timeseries/", station, "/ROB-EUREF/daily/enu/json/?epoch_start=1990-01-01&epoch_end=2099-12-01")
          }
          url_log <- "https://gnss-metadata.eu/data/station/log/"
          found <- grep(paste0(tolower(station),""), readHTMLTable(readLines(url_log), header = F)$list$V1, perl = F, value = T, fixed = T)
          if (isTruthy(found)) {
            logfile <- paste0(url_log,found)
          }
        } else {
          withBusyIndicatorServer(variable, {
            if (file.exists("www/EPOS_database.txt")) {
              stationsFromEPOS <- read.table(file = "www/EPOS_database.txt", header = T)
              stations_available <- stationsFromEPOS[grepl(product, stationsFromEPOS$provider), 1]
            } else {
              url <- "https://gnssproducts.epos.ubi.pt/GlassFramework/webresources/stations/v2/station/bbox/-25.664/35.60371874069731/27.07/68.0075?with=2"
              stationsFromEPOS.json <- jsonlite::fromJSON(readLines(url, ok = T, warn = F))
              if (isTruthy(stationsFromEPOS.json)) {
                stationsFromEPOS <- data.frame(id = stationsFromEPOS.json$features$id, lat = stationsFromEPOS.json$features$properties$Latitude, lon = stationsFromEPOS.json$features$properties$Longitude, provider = stationsFromEPOS.json$features$properties$`TimeSeries Data Providers`)
                stationsFromEPOS$provider <- gsub(" ", "|", stationsFromEPOS$provider)
                stations_available <- stationsFromEPOS[grepl(product, stationsFromEPOS$provider), 1]
                write.table(stationsFromEPOS, file = "www/EPOS_database.txt", append = F, quote = F, row.names = F)
              } else {
                showNotification(HTML(paste("Server", server, "seems to be unreachable.<br>It is not possible to get the list of available stations.")), action = NULL, duration = 10, closeButton = T, id = "no_answer", type = "warning", session = getDefaultReactiveDomain())
                return(NULL)
              }
            }
            if (series == 1) {
              output$station1 <- renderUI({
                suppressWarnings(selectInput(inputId = "station1", label = "Station", choices = c("Available stations" = "", stations_available), selected = "", selectize = T))
              })  
            } else if (series == 2) {
              output$station2 <- renderUI({
                suppressWarnings(selectInput(inputId = "station2", label = "Station", choices = c("Available stations" = "", stations_available), selected = "", selectize = T))
              })
              if (input$sunits == 1) {
                updateTextInput(session, inputId = "scaleFactor", value = "0.001")
              }
            }
            return(NULL)
          })
        }
      } else {
        showNotification(paste0("Unknown product ",product,". No file was downloaded."), action = NULL, duration = 10, closeButton = T, id = "bad_url", type = "error", session = getDefaultReactiveDomain())
        return(NULL)
      }
    ## EOSTLS ####
    } else if (server == "EOSTLS") {
      format <- 1
      pattern <- "_NEU."
      patternd <- "_NEU_daily."
      if (isTruthy(station)) {
        filepath <- c()
        name <- c()
        for (p in product) {
          naming <- paste0(toupper(station),pattern,tolower(p))
          url <- "http://loading.u-strasbg.fr/ITRF/CF/"
          if (tolower(p) == "atmib") {
            url <- paste0(url, "ATMIB/")
          } else if (tolower(p) == "atmib(d)") {
            url <- paste0(url, "ATMIB_daily/")
            naming <- paste0(toupper(station),patternd,"atmib")
          } else if (tolower(p) == "atmmo") {
            url <- paste0(url, "ATMMO/")
          } else if (tolower(p) == "ecco") {
            url <- paste0(url, "ECCO/")
          } else if (tolower(p) == "ecco2") {
            url <- paste0(url, "ECCO2/")
          } else if (tolower(p) == "era5ib") {
            naming <- paste0(toupper(station),pattern,"era5")
            url <- paste0(url, "ERA5_IB/")
          } else if (tolower(p) == "era5ib(d)") {
            naming <- paste0(toupper(station),patternd,"era5")
            url <- paste0(url, "ERA5_IB_daily/")
          } else if (tolower(p) == "era5tugo") {
            naming <- paste0(toupper(station),pattern,"era5")
            url <- paste0(url, "ERA5_TUGO/")
          } else if (tolower(p) == "era5tugo(d)") {
            naming <- paste0(toupper(station),patternd,"era5")
            url <- paste0(url, "ERA5_TUGO_daily/")
          } else if (tolower(p) == "era5hyd") {
            naming <- paste0(toupper(station),pattern,"era5")
            url <- paste0(url, "ERA5_hydro/")
          } else if (tolower(p) == "era5hyd(d)") {
            naming <- paste0(toupper(station),patternd,"era5")
            url <- paste0(url, "ERA5_hydro_daily/")
          } else if (tolower(p) == "gldas2") {
            url <- paste0(url, "GLDAS2/")
          } else if (tolower(p) == "gldas2(d)") {
            naming <- paste0(toupper(station),patternd,"gldas2")
            url <- paste0(url, "GLDAS2_daily/")
          } else if (tolower(p) == "glorys") {
            url <- paste0(url, "GLORYS/")
          } else if (tolower(p) == "grace") {
            url <- paste0(url, "GRACE/")
          } else if (tolower(p) == "merra2atm") {
            naming <- paste0(toupper(station),"_NEU_ib.merra2")
            url <- paste0(url, "MERRA2_atm/")
          } else if (tolower(p) == "merra2atm(d)") {
            naming <- paste0(toupper(station),"_NEU_daily_ib.merra2")
            url <- paste0(url, "MERRA2_atm_daily/")
          } else if (tolower(p) == "merra2hyd") {
            naming <- paste0(toupper(station),pattern,"merra2")
            url <- paste0(url, "MERRA2_hyd/")
          } else if (tolower(p) == "merra2hyd(d)") {
            naming <- paste0(toupper(station),patternd,"merra2")
            url <- paste0(url, "MERRA2_hyd_daily/")
          } else {
            showNotification(paste0("Unknown product ",p,". No file was downloaded."), action = NULL, duration = 10, closeButton = T, id = "bad_url", type = "error", session = getDefaultReactiveDomain())
            return(NULL)
          }
          name <- c(name, naming)
          filepath <- c(filepath, paste0(url,naming))
        }
      } else {
        withBusyIndicatorServer(variable, {
          if (file.exists("www/EOSTLS_database.txt")) {
            stations_available <- readLines("www/EOSTLS_database.txt", warn = F)
            if (series == 1) {
              output$station1 <- renderUI({
                suppressWarnings(selectInput(inputId = "station1", label = "Station", choices = c("Available stations" = "", stations_available), selected = "", selectize = T))
              })  
            } else if (series == 2) {
              output$station2 <- renderUI({
                suppressWarnings(selectInput(inputId = "station2", label = "Station", choices = c("Available stations" = "", stations_available), selected = "", selectize = T))
              })
              if (input$tunits == 1) {
                step <- 1
              } else if (input$tunits == 2) {
                step <- 1/7
              } else if (input$tunits == 3) {
                step <- 1/daysInYear
              }
              updateTextInput(session, inputId = "step2", value = step)
              if (input$sunits == 1) {
                updateTextInput(session, inputId = "scaleFactor", value = "0.001")
              }
            }
          } else {
            showNotification(HTML("The list of EOSTLS stations is not found.<br>It is not possible to get the list of available stations."), action = NULL, duration = 10, closeButton = T, id = "no_answer", type = "warning", session = getDefaultReactiveDomain())
          }
          return(NULL)
        })
      }
    ## SIRGAS ####
    } else if (server == "SIRGAS") {
      if (product == "IGB14") {
        format <- 1
        pattern <- ".PLH"
        if (isTruthy(station)) {
          url <- "https://www.sirgas.org/fileadmin/docs/SIRGAS_CRD/"
          name <- paste0(toupper(station),".PLH")
          filepath <- paste0(url,name)
          url_log <- "ftp://ftp.sirgas.org/pub/gps/DGF/station/log/"
          dir_contents <- try(getURL(url_log, ftp.use.epsv = FALSE, ftplistonly = TRUE, crlf = TRUE), silent = T)
          if (isTruthy(dir_contents) && !inherits(dir_contents,"try-error")) {
            found <- grep(tolower(station), strsplit(dir_contents, "\r*\n")[[1]], perl = F, value = T, fixed = T)
            if (isTruthy(found)) {
              logfile <- paste0(url_log,found)
            }
          }
        } else {
          url <- "https://www.sirgas.org/en/stations/station-list/"
          url2 <- "https://sirgas.ipgh.org/maps/stations/stations-list.php"
          withBusyIndicatorServer(variable, {
            dir_contents <- try(httr::GET(url), silent = T)
            if (isTruthy(dir_contents) && !inherits(dir_contents,"try-error")) {
              stations_available <- strtrim(readHTMLTable(rawToChar(dir_contents$content))[[1]]$ID, 4)
              if (series == 1) {
                output$station1 <- renderUI({
                  suppressWarnings(selectInput(inputId = "station1", label = "Station", choices = c("Available stations" = "", stations_available), selected = "", selectize = T))
                })  
              } else if (series == 2) {
                output$station2 <- renderUI({
                  suppressWarnings(selectInput(inputId = "station2", label = "Station", choices = c("Available stations" = "", stations_available), selected = "", selectize = T))
                })
                if (input$sunits == 2) {
                  updateTextInput(session, inputId = "scaleFactor", value = "1000")
                }
              }
            } else {
              dir_contents <- try(httr::GET(url2), silent = T)
              if (isTruthy(dir_contents) && !inherits(dir_contents,"try-error")) {
                stations_available <- strtrim(readHTMLTable(rawToChar(dir_contents$content))[[1]]$ID, 4)
                if (series == 1) {
                  output$station1 <- renderUI({
                    suppressWarnings(selectInput(inputId = "station1", label = "Station", choices = c("Available stations" = "", stations_available), selected = "", selectize = T))
                  })  
                } else if (series == 2) {
                  output$station2 <- renderUI({
                    suppressWarnings(selectInput(inputId = "station2", label = "Station", choices = c("Available stations" = "", stations_available), selected = "", selectize = T))
                  })
                }
              } else {
                showNotification(HTML(paste("Server", server, "seems to be unreachable.<br>It is not possible to get the list of available stations.")), action = NULL, duration = 10, closeButton = T, id = "no_answer", type = "warning", session = getDefaultReactiveDomain())
              }
            }
            return(NULL)
          })
        }
      } else {
        showNotification(paste0("Unknown product ",product,". No file was downloaded."), action = NULL, duration = 10, closeButton = T, id = "bad_url", type = "error", session = getDefaultReactiveDomain())
        return(NULL)
      }
    ## PSMSL ####
    } else if (server == "PSMSL") {
      format <- 4
      pattern <- ".rlrdata"
      if (isTruthy(station)) {
        url <- "https://psmsl.org/data/obtaining/rlr.monthly.data/"
        if (grepl(":", station, fixed = T)) {
          stationId <- trimws(unlist(strsplit(station, split = ":")))[1]
          station <- trimws(unlist(strsplit(station, split = ":")))[2] 
        } else {
          stationId <- station
          stationsFromPSMSL <- try(read.table(file = "www/PSMSL_database.txt", sep = ";"), silent = T)
          station <- stationsFromPSMSL[stationsFromPSMSL$V1 == station,2]
        }
        name <- paste0(stationId,pattern)
        filepath <- paste0(url,name)
        updateCheckboxInput(session, inputId = "sigmas", value = F)
        disable("sigmas")
        if (series == 1) {
          updateSelectInput(session, inputId = "separator", selected = 3)
        } else if (series == 2) {
          updateSelectInput(session, inputId = "separator2", selected = 3)
        }
      } else {
        withBusyIndicatorServer(variable, {
          if (file.exists("www/PSMSL_database.txt")) {
            stations_available <- do.call(paste, c(read.table("www/PSMSL_database.txt", sep = ";", quote = "@")[,c(1,2)], sep = ": "))
            if (series == 1) {
              output$station1 <- renderUI({
                suppressWarnings(selectInput(inputId = "station1", label = "Station", choices = c("Available stations" = "", stations_available), selected = "", selectize = T))
              })  
            } else if (series == 2) {
              output$station2 <- renderUI({
                suppressWarnings(selectInput(inputId = "station2", label = "Station", choices = c("Available stations" = "", stations_available), selected = "", selectize = T))
              })
            }
          } else {
            showNotification(HTML("The list of PSMSL stations is not found.<br>It is not possible to get the list of available stations."), action = NULL, duration = 10, closeButton = T, id = "no_answer", type = "warning", session = getDefaultReactiveDomain())
          }
          return(NULL)
        })
      }
    ## LOCAL ####
    } else if (server == "LOCAL") {
      if (product == "NEU" || product == "ENU") {
        format <- 1
      } else if (product == "PBO") {
        format <- 2
      } else if (product == "NGL") {
        format <- 3
      } else if (product == "1D") {
        format <- 4
      } else {
        showNotification(paste0("Unknown product ",product,". No file was downloaded."), action = NULL, duration = 10, closeButton = T, id = "bad_url", type = "error", session = getDefaultReactiveDomain())
        return(NULL)
      }
      name <- basename(station)
      filepath <- station
      removes <- "^SPOTGINS_|^UGA_"
      station <- strsplit(gsub(pattern = removes, replacement = "", x = name, ignore.case = T, perl = T, fixed = F), "\\.|_|\\s|-|\\(")[[1]][1]
      #
    } else {
      showNotification(paste0("Unknown server ",server,". No file was downloaded."), action = NULL, duration = 10, closeButton = T, id = "bad_url", type = "error", session = getDefaultReactiveDomain())
      return(NULL)
    }
    return(list(station,filepath,name,format,logfile))
  }
  #
  deg2m <- function(lat,lon,h,dlat,dlon) {
    #lat, lon, dlat, dlon in radians; h in m
    a <- 6378137
    b <- 6356752.314140347
    e2 <- (a^2 - b^2) / a^2
    N <- a / sqrt( 1 - e2 * sin(lat)^2) + h
    n <- N * dlat
    e <- N * dlon * cos(lat)
    return(n,e)
  }
  #
  cov_powerlaw <- function(k,n,deriv,gaps,sampling) {
    Delta <- 1 # Delta[1]
    a <- 0 # a[1]
    for (i in 2:n) {
      a[i] <- (-k/2 + i - 1 - 1)/(i - 1)
      Delta[i] <- Delta[i - 1] * a[i]
    }
    Z <- toeplitz(Delta[gaps])
    Z[upper.tri(Z)] <- 0
    # Z <- Z * sampling^(-k/2) # variance scaling from Gobron 2020
    Z <- Z * sampling^(-k/4) # Variance scaling from Williams 2003
    if (deriv) {
      u <- 0 # u[1]
      for (i in 2:n) {
        u[i] <- -1/(2*(i - 1))*Delta[i - 1] + a[i]*u[i - 1]
        # u[i] <- -1/(2*i)*Delta[i - 1] + a[i]*u[i - 1]
      }
      U <- toeplitz(u[gaps])
      U[upper.tri(U)] <- 0
      derivZ <- sampling^(-k/2)*(-0.5*log(sampling)*tcrossprod(Z) + tcrossprod(U,Z) + tcrossprod(Z,U)) # from Gobron 2020
      derivZ <- derivZ * sampling^(-k/4) # Variance scaling from Williams 2003
      return(list(tcrossprod(Z), derivZ))
    } else {
      return(list(tcrossprod(Z)))
    }
  }
  #
  loglikelihood <- function(series,M,r) {
    if (r > 0) {
      return(-0.5*(length(series)*log(2*pi*r) + determinant(M)$modulus[[1]] + length(series)))
    } else {
      Qinv <- pd.solve(M, log.det = T)
      logDet <- attr(Qinv, "log.det")
      QinvR <- Qinv %*% series
      # return((-0.5*(length(series)*log(2*pi) + determinant(M)$modulus[[1]] + crossprod(series, solve(M, series))))[1])
      ll <- (-0.5*(length(series)*log(2*pi) + logDet + crossprod(series, QinvR)))[1]
      return(list(ll,Qinv,QinvR))
    }
  }
  #
  pl_trend_unc <- function(amp,index,sampling) {
    v <- -0.0237*index^9 - 0.3881*index^8 - 2.6610*index^7 - 9.8529*index^6 - 21.0922*index^5 - 25.1638*index^4 - 11.4275*index^3 + 10.7839*index^2 + 20.3377*index^1 + 11.9942*index^0
    beta <- (-1*index)/2 - 2
    if (index > -1.5 && index < -0.5) {
      gamma <- -3 - index + 7.7435*exp(-10)*index^17 - (0.0144/(0.27*sqrt(2*pi)))*exp(-0.5*((index + 1.025)/0.27)^2)
    } else if (index < -2.5) {
      gamma <- -3 - index - 0.52 * index^2 - 2.67 * index - 3.43
    } else {
      gamma <- -3 - index
    }
    return(sqrt(amp^2 * v * sampling^beta * info$points^gamma))
  }
  #
  noise_var <- function(std,k) {
    if (input$tunits == 1) { #days
      f_scale <- 24*60*60
    } else if (input$tunits == 2) { #weeks
      f_scale <- 7*24*60*60
    } else if (input$tunits == 3) { #years
      f_scale <- 365.25*24*60*60
    }
    fs_hz <- 1/(info$sampling*f_scale)
    
    Dk <- 2*(2*pi)^k * f_scale^(k/2)
    return(std^2 * Dk / (fs_hz^(1 + (k/2)))) #from Williams 2003 (Eq. 10)
  }
  #
  download <- function(server,remote,local) {
    removeNotification("no_cmd")
    # stream JSON file
    if (server == "EPOS") {
      con <- url(remote)
      json <- suppressWarnings(try(jsonlite::stream_in(con, verbose = F), silent = T))
      if (isTruthy(json) && !inherits(json,"try-error")) {
        down <- 0
        json <- json[,c("epoch","e","n","u")]
        write.table(json, file = local, append = F, quote = F, row.names = F, col.names = F)
      } else {
        down <- 1
        close(con)
      }
    } else {
      # download series using curl or wget
      if (isTruthy(Sys.which("curl"))) {
        method <- "curl"
        if (server == "FORMATER") {
          extras <- "-u SARI:bwPgzhe4Zu"
        } else {
          extras <- ""
        }
      } else if (isTruthy(Sys.which("wget"))) {
        method <- "wget"
        if (server == "FORMATER") {
          extras <- "--user SARI --password bwPgzhe4Zu --auth-no-challenge"
        } else {
          extras <- ""
        }
      } else {
        showNotification("Neither curl nor wget are available on the system.", action = NULL, duration = 10, closeButton = T, id = "no_cmd", type = "error", session = getDefaultReactiveDomain())
        return(1)
      }
      down <- suppressWarnings(try(download.file(remote, destfile = local, method = method, extra = extras, quiet = T, mode = "w", cacheOK = T), silent = T))
    }
    return(down)
  }
  #
  mjd2week <- function(x) {
    offset <- 0
    if (all(x < 35000)) {
      offset <- 33282
    }
    decimals <- decimalplaces(x, "x") + 2
    decimals <- ifelse(decimals > 0, decimals, 0)
    return(as.numeric(sprintf("%.*f", decimals, (x + offset - 44244)/7)))
  }
  #
  week2mjd <- function(x) {
    decimals <- decimalplaces(x, "x") - 2
    decimals <- ifelse(decimals > 0, decimals, 0)
    return(as.numeric(sprintf("%.*f", decimals, x * 7 + 44244)))
  }
  #
  mjd2year <- function(x) {
    offset <- 0
    if (all(x < 35000)) {
      offset <- 33282
    }
    decimals <- decimalplaces(x, "x") + 3
    decimals <- ifelse(decimals > 0, decimals, 0)
    return(as.numeric(sprintf("%.*f", decimals, decimal_date(as.Date(x + offset, origin = as.Date("1858-11-17"))))))
  }
  #
  year2mjd <- function(x) {
    decimals <- decimalplaces(x, "x") - 3
    decimals <- ifelse(decimals > 0, decimals, 0)
    return(as.numeric(sprintf("%.*f", decimals, difftime(date_decimal(x), strptime(paste(sprintf("%08d",18581117),sprintf("%06d",000000)),format = '%Y%m%d %H%M%S', tz = "GMT"), units = "days"))))
  }
  #
  week2year <- function(x) {
    decimals <- decimalplaces(x, "x") + 1
    decimals <- ifelse(decimals > 0, decimals, 0)
    return(as.numeric(sprintf("%.*f", decimals, decimal_date(as.Date("1980-01-06") + x * 7))))
  }
  #
  year2week <- function(x) {
    decimals <- decimalplaces(x, "x") - 1
    decimals <- ifelse(decimals > 0, decimals, 0)
    return(as.numeric(sprintf("%.*f", decimals, difftime(date_decimal(x), strptime(paste(sprintf("%08d",19800106),sprintf("%06d",000000)),format = '%Y%m%d %H%M%S', tz = "GMT"), units = "weeks"))))
  }
  #
  printInfo <- function(label) {
    if (isTruthy(file$sitelog) || isTruthy(file$primary$logfile) || isTruthy(file$secondary$logfile)) {
      sitelog <- T
    } else {
      sitelog <- F
    }
    if (isTruthy(file$soln)) {
      soln <- T
    } else {
      soln <- F
    }
    if (isTruthy(file$sinfo)) {
      sinfo <- T
    } else {
      sinfo <- F
    }
    if (isTruthy(file$custom)) {
      custom <- T
    } else {
      custom <- F
    }
    if (isTruthy(file$secondary$name)) {
      secondary <- T
    } else {
      secondary <- F
    }
    if (messages > 0) cat(file = stderr(), mySession, label,
                          "   Format:",info$format,
                          "   Component:", input$tab,
                          "   T.units:", input$tunits,
                          "   S.units:", input$sunits,
                          "   Sigmas:",input$sigmas,
                          "   Average1:", inputs$step,
                          "   Separator:", input$separator,
                          "   Epoch:", input$epoch,
                          "   Variable:", input$variable,
                          "   ErrorBar:", input$errorBar,
                          "   Sitelog:", sitelog,
                          "   station.info:", sinfo,
                          "   soln:", soln,
                          "   custom:", custom,
                          "   Secondary:", secondary,
                          "   Format2:", input$format2,
                          "   Option:", input$optionSecondary,
                          "   Scale:", inputs$scaleFactor,
                          "   Average2:", inputs$step2,
                          "   Separator2:", input$separator2,
                          "   Full series:", input$fullSeries,
                          "   Same scale:", input$sameScale,
                          "   Same axis:", input$same_axis,
                          "   N/E:", input$ne,
                          "   Epoch2:", input$epoch2,
                          "   Variable2:", input$variable2,
                          "   ErrorBar2:", input$errorBar2,
                          "\n")
  }
  #
  compute_entropy <- function(vel) {
    # based on the entropy.estimate function from the vsgoftest package but in log2
    H <- 0
    breaks <- unique(sort(c(trans$x[1],trans$offsetEpochs,trans$offsetEpoch.entropy,trans$x[length(trans$x)])))
    detrended <- trans$y - trans$x * vel
    ntot <- length(unique(detrended))
    for (i in seq_len(length(breaks) - 1)) {
      segment <- trans$x >= breaks[i] & trans$x < breaks[i + 1]
      series <- unique(detrended[segment])
      w <- max(table(series)) + 1
      n <- length(series)
      p <- n/ntot
      xord <- sort(series)
      S <- numeric(n)
      S[1:w] <- log2(n*(xord[1:w + w] - xord[1])/(2*w))
      S[(w + 1):(n - w - 1)] <- log2(n*(xord[(w + 1):(n - w - 1) + w] - xord[(w + 1):(n - w - 1) - w])/(2*w))
      S[(n - w):n] <- log2(n*(xord[n] - xord[(n - w):n - w])/(2*w))
      h <- p * mean(S)
      H <- H + h
    }
    return(H)
  }
  #
  interpolateGIA <- function(x,y,series) {
    z <- NULL
    if (series == 1) {
      variable <- "giaTrend"
      secondary <- ""
    } else if (series == 2) {
      variable <- "giaTrend2"
      secondary <- "secondary"
    } else {
      req(info$stop)
    }
    if (!(x >= 0 & x <= 360) || !(y >= -90 & y <= 90)) {
      showNotification(HTML(paste("The", secondary, "station coordinates are out of bounds.<br>Check the input values.")), action = NULL, duration = 15, closeButton = T, id = "bad_coordinates", type = "error", session = getDefaultReactiveDomain())
      req(info$stop)
    }
    withProgress(message = 'Interpolating GIA grid',
                 detail = paste("from", input$giaModel), value = 0, {
                   withBusyIndicatorServer(variable, {
                     if (input$giaModel == "ICE-6G-VM5a") {
                       gia <- read.table("www/drad.12mgrid_512.txt", comment.char = "#")
                     } else if (input$giaModel == "ICE-6G-ANU") {
                       gia <- read.table("www/ICE6G_ANU.txt")
                       names(gia) <- c("V2","V1","V3")
                     } else if (input$giaModel == "Caron & Ivins") {
                       gia <- read.table("www/GIA_maps_Caron_Ivins_2019.txt", comment.char = "%")[,c(1,2,3)]
                       y <- 90 - y
                     } else {
                       return(z)
                     }
                     setProgress(0.9)
                     lat <- unique(gia$V1)
                     lon <- unique(gia$V2)
                     xs <- lon[abs(lon - x) %in% sort(abs(lon - x), partial = 1:2)[1:2]]
                     ys <- lat[abs(lat - y) %in% sort(abs(lat - y), partial = 1:2)[1:2]]
                     q11 <- gia$V3[gia$V1 == ys[1] & gia$V2 == xs[1]]
                     q12 <- gia$V3[gia$V1 == ys[2] & gia$V2 == xs[1]]
                     q21 <- gia$V3[gia$V1 == ys[1] & gia$V2 == xs[2]]
                     q22 <- gia$V3[gia$V1 == ys[2] & gia$V2 == xs[2]]
                     if ( abs(diff(xs)) > 0 && abs(diff(ys)) > 0 ) {
                       z <- (1/((xs[2] - xs[1])*(ys[2] - ys[1]))) * matrix(c(xs[2] - x, x - xs[1]), nrow = 1, ncol = 2) %*% matrix(c(q11,q21,q12,q22), nrow = 2, ncol = 2) %*% matrix(c(ys[2] - y, y - ys[1]), nrow = 2, ncol = 1)
                     } else if ( abs(diff(ys)) > 0 ) {
                       z <- q11*(ys[2] - y)/(ys[2] - ys[1]) + q12*(y - ys[1])/(ys[2] - ys[1])
                     } else if ( abs(diff(xs)) > 0 ) {
                       z <- q11*(xs[2] - x)/(xs[2] - xs[1]) + q21*(x - xs[1])/(xs[2] - xs[1])
                     } else {
                       z <- gia$V3[gia$V1 == ys[1] & gia$V2 == xs[1]]
                     }
                   })
                 })
    return(z)
  }
  #
  formatting <- function(x, y) {
    # width <- nchar(format(x, nsmall = info$nsmall, digits = info$digits, scientific = info$scientific, trim = F))
    # width <- max(width + 1 - 1*grepl("^-", x, perl = T, ))
    width <- NULL
    if (info$scientific) {
      extra_dec <- 0
    } else {
      extra_dec <- y
    }
    return(format(x, nsmall = info$nsmall + extra_dec, digits = info$digits, scientific = info$scientific, trim = F, width = width))
  }
  #
  debugMem <- function() {
    output$debug <- renderTable({
      data.frame(
        object = database,
        size = unlist(lapply(database, function(x) {
          total <- 0
          for (i in 1:length(names(get(x)))) {
            name <- names(get(x))[i]
            total <- total + object.size(get(x)[[name]])
          }
          format(total, unit = 'Mb')
        }))
      )
    })
  }
}

shinyApp(ui = ui, server = server)
