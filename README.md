# iSTATS
## Intro
A package developed in R using shiny dashbord, iSTATS (interactive STAtisTical Spectroscopy) have a GUI platform to when user can analize NMR data. From the construction of the matrix, going through a pre-treatment of the data to the application of STOCSY methods, which are used in the study of metabolomics, and among other areas whose characterization and relationship between the molecules are the object of study in NMR technology.

## Install iSTATS
There are 2 ways to install iSTATS, fisrt one is on CRAN (The Comprehensive R Archive Network) and another is from here github.

### Install iSTATS from CRAN 
You can install by R with command `install.packages('iSTATS')` or you can use RStudio tool as in the figure below:


![Alt ou título da imagem](https://github.com/vitor-mendes-iq/iSTATS/blob/master/rstudio_install.jpg)


### Install iSTATS from Github
First install devtools package with the command `install.packages("devtools")`.
After install `library(devtools)`, you can install iSTATS with the command `install_github("vitor-mendes-iq/iSTATS")`.

## Run iSTATS
After install iSTATS, you must load and run the package with the commands  `library(iSTATS)` and `iSTATS()`, so it will open in the web browser.

## How to use iSTATS step by step
iSTATS open on browser in the menu **Home** where have gif that ilustrate the step to how using iSTATS. On the top it have five menus: **Data, Plot, Select, STOCSY and Home**. 

### first Step: import data
The first menu that we should use is **DATA**, where are four sections, one data example and three of then for each kind of file extension: `.csv`, `.ascii` (Bruker-Topspin export) and `.Rdata`. 

#### Import CSV file
In this section we need first define the range of spectra that we need to analize (default is 0 - 10 ppm) them on the botton of **Choose CSV Files**  we can click in browse and choose the CSV file that need to have ppm separete by virgula to intenssity signal, like show in **figure 2**

![Alt ou título da imagem](https://github.com/vitor-mendes-iq/iSTATS/blob/master/rstudio_install.jpg)
