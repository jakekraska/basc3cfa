# Confirmatory Factor Analyses of the Behavior Assessment System for Children – Third Edition Among an Australian Sample

This repository includes the data and code required for analysis in the **Confirmatory Factor Analyses of the Behavior Assessment System for Children – Third Edition Among an Australian Sample** manuscript. This manuscript is currently under peer review.

## Abstract

It has been estimated that 10-20% of children and adolescents worldwide experience emotional and behavioural disorders (EBD; Kieling et al., 2011). When EBD in children and adolescents are neither identified nor treated, their problems may have significant adverse impacts on both the individual and society (Ogundele, 2018). The Behavior Assessment System for Children, Third Edition (BASC-3) is a behaviour rating scale for screening EBD in children and adolescents commonly utilised in Australia but relies on a US normative sample. This study aimed to assess the cross-cultural validity of the BASC-3 in Australian populations by assessing the original factor structures of the BASC-3 Teacher Rating Scale Children (TRS-C) and Parent Rating Scale Children (PRS-C) among an Australian sample. The hypothesis for this study was that the factor structure of the BASC-3 TRS-C and PRS-C would yield acceptable or good fit indices among the Australian sample. Analyses were conducted on parent and teacher ratings of 716 children (298 children for the TRS-C and 418 children for the PRS-C) with a mean age of 8.63 (SD = 1.54) from a psychology training clinic in Melbourne, Australia. Single factor congeneric modelling and second-order confirmatory factor analyses were conducted to examine how well the BASC-3 TRS-C and PRS-C factor structure fit the Australian data. Good and acceptable model fit, and significant factor loadings were found in the single factor congeneric modelling and second-order factor modelling. The results provide supporting evidence of the cross-cultural validity of the BASC-3 among Australian children. Future research can examine the factor structure of the BASC-3 SRP-C among an Australian sample.

## Authors and Contributions

Authors: Anggun Triana Sari Tan, Jake Kraska, Karen Bell, Shane Costello

The authors acknowledge the Krongold Clinic, Monash University for their support data collection.

All authors and contributors are affiliated with the Faculty of Education, Monash University, Australia. Jake Kraska is also affiliated with the Krongold Clinic, Monash University.

## Correspondence

All correspondence regarding this analysis and manuscript can be directed to [Anggun Triana Sari Tan](mailto:angguntriana.128@gmail.com)

## Installation and Running

* Clone this repo to your local machine using https://github.com/jakekraska/basc3cfa
* Open analysis.R in your preferred R IDE
* Set the working directory to the source file within the IDE or using `setwd(dirname(rstudioapi::getActiveDocumentContext($path`
* Install the necessary packages using the `install.packages("PACKAGENAME")` command
    * plyr version 1.8.6
    * ggplot2 version 3.3.2
    * psych version 2.0.7
    * knitr version 1.29
    * lavaan version 0.6-7
    * dplyr version 1.0.1
    * tidyr version 1.1.1
    * latticeExtra version 0.6-29
    * lubridate version 1.7.9

* The code is commented and minimal SEM knowledge should allow understanding of the output. Minimal R knowledge is required to understand the code.