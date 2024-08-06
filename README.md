![twitter](https://img.shields.io/twitter/follow/timeserious?style=social)
![license](https://img.shields.io/github/license/alvarosantamariagomez/sari)
![version](https://img.shields.io/badge/version-agosto%202024-blue)
![github](https://img.shields.io/github/languages/code-size/alvarosantamariagomez/sari?color=g)
![docker](https://img.shields.io/docker/image-size/alvarosg/sari?color=g)

# ![SARI logo](/www/favicon.png) sari 

SARI is an R/Shiny app that allows you to visualise discrete time series data, fit unidimensional models, analyse the results interactively and save the results using a web interface.  
The app focuses mostly on GNSS position time series, but any other type of time series can be used as long as the series format is simple, consistent and does not have uncommented alphabetic characters.

![SARI screenshot](/www/screen_capture.gif)

The app includes a large list of analysis options: fitting polynomial, sinusoidal, exponential, logarithmic models (with least-squares or Kalman filter), power spectrum, noise analysis, Vondrak smoother, wavelet, waveform, automatic offset detection, automatic outlier removal, etc.

Some examples of use can be found on the [SARI Official Website](https://sari-gnss.github.io)

## How to run and/or install

SARI can be run:  
- remotely on the shinyapps server at this link https://alvarosg.shinyapps.io/sari
- locally on your machine using a prebuilt Docker image available at https://hub.docker.com/r/alvarosg/sari  
- locally on your machine after installing R (optionally RStudio) and all the package dependencies given in the [INSTALL](INSTALL) file. Once everything is set up, then just clone, open and run the [source code](app.R) on RStudio or simply execute `library(shiny)` and then `runGitHub("sari","alvarosantamariagomez",launch.browser=T)`  

The [SARI shell script](/scripts/sari.sh) can be used to launch SARI from the command line on Unix-like desktop environments and to create desktop shortcuts on any system. To add the script to the PATH environment variable of your system use this from the SARI Git directory:  
`sudo ln -s $(realpath scripts/sari.sh) /usr/local/bin/.`

## Where to find help

The [help file](/www/about.pdf) contains detailed information on how to use SARI, including the description of all the implemented functionalities. This file is also accessible from the main web interface of SARI by clicking on the `Help` button.  

A video tutorial is also available [here](https://youtu.be/Zt61jzehhoc).  
This video tutorial was made with a previous version of SARI and does not include the latest additions and corrections, but it is still valid to learn the general usage of the app.

Further details and references can be found in:  
Santamaría-Gómez, A. (2019) SARI: interactive GNSS position time series analysis software. GPS solutions, 23:52. DOI: [10.1007/s10291-019-0846-y](https://link.springer.com/article/10.1007/s10291-019-0846-y)

## How to contribute

Contributions to the SARI app are welcome!  
Any type of contribution is accepted: from changing complex functions in the code to suggesting a better style of the interface.

For changes in the code, plase fork this repository, then clone it, make the modification and finally send a pull request. For a more detailed step-by-step guide, you can check [this repo](https://github.com/firstcontributions/first-contributions).

For bug reports, suggestions or questions, please create an issue using a template.
