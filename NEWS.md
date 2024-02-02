# IntakeInspectR (dev)

* breaking change: 'replace: minor increase' 
* changed to 'keep: minor increase' to be consistent with the minor decrease classed as a 'keep'


# IntakeInspectR 2.1.2

* Added download button to the Uploads page, so that an .rds or .csv of the uploaded, filtered and collated data can be downloaded as the 'raw' version of data

* Added R code to final log that is a full reproducible copy of the analysis, incorporating user selections throughout shiny app.



# IntakeInspectR 2.1.1

* Fixed Rd to HTML so that it works when package is installed by user, not cloned repos. Problem was with where help .Rd files are stored in different scenarios.

* Added Hex logo

* Minor UI fixes

# IntakeInspectR 2.1.0

* 'By Cow' cleaning now uses biologically relevant thresholds of rate of intakes for each feeding event. The original statistical approach can also be applied if desired.

* Added more user inputs to 'By Cow' that allow users to interact with the new outlier detection logic.

* Changed column names for user upload csv/txt files to match our facility's data pipelines.

* Added 'custom column names' feature that lets users define the names used in their original file to be renamed for use in the Shiny app. However, column names can already be defined when using the package functions in R independently of Shiny.

* Change all 'cow' references to 'animal' to signify that this package will work with any animal data, not just dairy cows.

* Modified the layout of the cleaning pages of the shiny app (2a and 3a ) with the Log, Data Structure and a View Table page now separated into a navbar.

* Included download options for the 'By Bin' cleaning page, similar to what was available on the 'By Animal' page.

* Users can now bypass the 'By Bin' cleaning if they want to only check for outliers 'By Animal'

* The 'By Animal' cleaning now asks users to select which cleaning they want to use from the previous step. It still lumps all 'end_weight' corrections together. If a user wanted a more detailed selection of say only 1 'replace' type, then this can be done manually in R outside of the Shiny app.


# IntakeInspectR v1.0.0

* Initial version published in 2023 ADSA conference abstract, Ottawa.


