# tempFGN
Code, data and results of the Temperature Fluctuations paper

This repository contains all the material related to the paper: [How does temperature vary over time? Evidence on the stationary and fractal nature of temperature](https://rss.onlinelibrary.wiley.com/doi/full/10.1111/rssa.12557) by John Dagsvik, Mariachiara Fortuna, Sigmund H. Moen. A wider description of the project is available in a [dedicated webpage](https://www.vanlog.it/temperature/)

All the materials are stored in the form of an R package. You can download or clone the repository and install the package to get the functions available for usage. The same repository contains the online resources published as paper's supplementary materials. These resources are available as .pdf files as well as .RMarkdown files, to allow the results reproduction.


### tempFGN functions

The functions developed for the Temperature Fluctuations paper are available installing the tempFGN package. As usual for R packages, the functions are stored in the /R folder. They are divided into 5 groups:

* Preprocessing: contains functions for manipulating the time series
* Plots: contains functions for producting the temperature plots 
* Estimators: contains functions for the parameters estimation
* Autocorrelation: contains functions for the estimating the autocorrelation and correcting its bias
* Test: contains functions for the inferential tests


### Data

All the data are store into the /inst/data folder. A wide description of the data and their organization can be found here.


### Online resources

All the online resources and supplementary materials published with the paper are stored into the /inst folder. Each of these files is available as .pdf file and as .Rmd file, for a full reproducibility of all the analysis. 

The folder contains the following materials:

* [Paper results](/temperature/paper-results)
* [Appendix B](/temperature/appendix-b): Summary data information
* [Appendix C](/temperature/appendix-c): Estimation and test results
* [Appendix D](/temperature/appendix-d): Property of the estimators
* [Appendix E](/temperature/appendix-e): Summary infographic for each weather station
* [Appendix F](/temperature/appendix-f): Selection process and cleaning procedure
* Functions code, written as plain text. This file was required as supplementary material for the paper publication.


================================================


#### TO REPRODUCE PAPER/APPENDIX RESULTS 


1. Clone or download the tempFGN package

2. Open the tempFGN.Rproj file from RStudio

3. Build the tempFGN package (you can use the shortcut Ctrl+Shift+B)  

4. Open the .Rmd file what you want to reproduce 

5. Execute the code

================================================ 

For questions or inquiries write an email to mariachiara.fortuna [at] vanlog.it
