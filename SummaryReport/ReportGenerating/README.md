## Purpose

The following documents the Machine Summary Reporting code base. 

## About

The current iteration compares real harvester data, gathered from customer fields, to simulated harvester data. The summary report focuses on specific metrics visualized in a tabular and graphical manner. These areas are:

* Harvesting Performance Scores
* Harvesting Performance Engineering Units
* Power Summaries
* Agronomic & Output Summaries


## Generating the Report

The following section will document the processess to generate the Machine Summary Report.

### Rmd, R, and Support files

To generate reports, the `machineSummaryReport.Rmd` file will need to be within the same folder as the `src/` folder and the `header.tex` latex file. Furthermore, please verify that within the `src/` folder it contains:

* `plotUtil.R`
* `tableUtil.R`
* `units.R`
* `versatility.R`

### Input File Preparations

Before any report summary can be generated, the input `.csv` files need to be prepared. Please refer to the Simulation Toolchain Visio Diagram located on Intelligent Combine server for the input `.csv` process.

### Yaml

The only significant change needed to be made to the `machineSummaryReport.Rmd` file is the Yaml header. The Yaml header is as follows:

```yaml
---
output:
  pdf_document:
    includes:
      in_header: header.tex
    fig_width: 25
    fig_height: 25
classoption: landscape
geometry: margin=2cm
params:
  pc: 0.8
  dpc: 0.98
  file1: 
  file2: 
  file3: //iso19prod/IntelligentCombine/HA/ISE/reports/data/sfcVarMapping.csv
  tz: 
---
```
Input the As Harvested `.csv` absolute file path to `file1:` and the Simulated `.csv` absolute file path to `file2:`. The `sfcVarMapping.csv` file can be found in the displayed `file3:` link. `sfcVarMapping.csv` is used to provide header names to the Simulation data. Please verify the location of this file. 

Additionally, `tz` specifies the timezone of the reporting field. Please follow proper timezone code format of `posixtc` class objects. The following link is where you can find timezone codes: https://en.wikipedia.org/wiki/List_of_tz_database_time_zones#List

### Enviroment Verifications

Please verify that you are using RStudio to edit and run the summary reporting code. If you do not have RStudio installed, please visit **JDSRS** or https://www.rstudio.com/ to download Rstudio. 

After you have verified the installation of RStudio, please verify that you have `MikTex` insalled. `MikTex` is a `laTex` compiler which generates `pdf` files. To install `MikTex` please visit https://miktex.org/.

Additionally, the sumamry report uses various packages. Please verify that you have installed the following packages:

* knitr
* tidyverse
* dbplyr
* tinytex
* colorRamps
* lknitr
* kableExtra
* gridExtra
* grid
* ggplot2
* ggmap
* ggpubr
* scales
* data.table
* pander

If you do not have one of these packages installed, please enter `install.packages("name")` into the RStudio console. Note that you will need to enter the name of the package within `""`.

### Generating the Report
To generate the report, click on the `knit` button (looks like a ball of blue yarn) loacated in RStudio. This will prompt RStudio to start generating the report. After RStudio finishes, the Machine Summary Report can be found at the same location where the `.Rmd` file is located in. As by convention, the `pdf` will share the same file name as the `.Rmd` file. It is good practice to always rename the `.Rmd` file to your desired `pdf` file name before generating. 

---
