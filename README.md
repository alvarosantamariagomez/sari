![twitter](https://img.shields.io/twitter/follow/timeserious?style=social)
![license](https://img.shields.io/github/license/alvarosantamariagomez/sari)
![version](https://img.shields.io/badge/version-marzo%202023-blue)
![github](https://img.shields.io/github/languages/code-size/alvarosantamariagomez/sari?color=g)
![docker](https://img.shields.io/docker/image-size/alvarosg/sari?color=g)

# ![SARI logo](/www/favicon.png) sari 

SARI is an R/Shiny app that allows you to visualise discrete time series data, analyse them individually, fit unidimensional models interactively and save the results using a web interface.  
The app focuses mostly on GNSS position time series, but any other type of time series can be used as long as the series format is simple, consistent and does not have uncommented alphabetic characters.

![SARI screenshot](/www/screenshot.png)


SARI has been developed in the R programming language, under the interactive framework of the Shiny R package.  
Currently tested in R version 4.1.0.


SARI can be run:  
- remotelly from the shinyapps server at this link https://alvarosg.shinyapps.io/sari
- locally on your machine using a prebuilt Docker image available at https://hub.docker.com/r/alvarosg/sari  
- locally on your machine after installing R, RStudio and all the R dependencies given in the [INSTALL](INSTALL) file. Once all this is set up, then just clone, open and run the [source code](app.R) in RStudio or simply execute `library(shiny)` and then `runGitHub("sari","alvarosantamariagomez",launch.browser=T)`  

The files containing the GNSS series (or any other type of series) can be uploaded from your local machine or from specific remote servers via some parameters included in the URL. The latter option allows adding links on websites that open specific GNSS series with SARI, like for example [this one.](https://alvarosg.shinyapps.io/sari/?station=TLSE&server=NGL&product=FINAL)  

The script [sari.sh](/scripts/sari.sh) can be used to launch SARI on the web browser from a Linux terminal. To add the script to the PATH environment variable of your system use  
`sudo ln -s $(realpath scripts/startSARI.sh) /usr/local/bin/.`    

The [about.pdf](/www/about.pdf) file contains detailed information on how to run SARI, including the description of all the implemented functionalities. This file is also accessible from the main web interface of SARI by clicking the `Help` button.  
Further details and some examples can be found in:

Santamaría-Gómez, A. (2019) SARI: interactive GNSS position time series analysis software. GPS solutions, 23:52. DOI: [10.1007/s10291-019-0846-y](https://link.springer.com/article/10.1007/s10291-019-0846-y)

A video tutorial is also available here: https://youtu.be/Zt61jzehhoc  
This video tutorial was made with a previous version of SARI and does not include the latest additions and corrections, but it is still valid to learn the general usage of the app.
