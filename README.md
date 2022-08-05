# ![SARI logo](/www/favicon.png) sari 

SARI is an R/Shiny app that allows you to visualise discrete time series data, analyse them individually, fit unidimensional models interactively and save the results using a web interface.  
The app focuses mostly on GNSS position time series, but any other type of time series can be used as long as the series format is simple, consistent and does not have uncommented alphabetic characters.

![SARI screenshot](/www/screenshot.png)


SARI has been developed in the R programming language, under the interactive framework of the Shiny R package.  
Currently tested in R version 4.1.0.


SARI can be run:  
- remotelly from the shinyapps server at this link https://alvarosg.shinyapps.io/sari
- locally on your machine using a prebuilt Docker image available at https://hub.docker.com/r/alvarosg/sari  
- locally on your machine using the [source code](app.R) after installing R, RStudio and all the R dependencies given in the [INSTALL](INSTALL) file. Once all this is set up, then just clone, open and run the source code or simply execute `runGitHub("sari","alvarosantamariagomez")`  

The [about.pdf](/www/about.pdf) file contains all the details to run SARI including the description of all the implemented functionalities.  
This file is accessed on the main web interface with the `Help` button or locally in the /www directory.  
Further details and some examples can be found in:

Santamaría-Gómez, A. (2019) SARI: interactive GNSS position time series analysis software. GPS solutions, 23:52. DOI: [10.1007/s10291-019-0846-y](https://link.springer.com/article/10.1007/s10291-019-0846-y)

A video tutorial is also available here: https://youtu.be/Zt61jzehhoc  
This video tutorial was made with a previous version of SARI and does not include the latest additions, but it is still valid to learn the general usage of the app.
