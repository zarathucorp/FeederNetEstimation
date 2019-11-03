# FeederNetEstimation

[![Travis build status](https://travis-ci.org/zarathucorp/FeederNetEstimation.svg?branch=master)](https://travis-ci.org/zarathucorp/FeederNetEstimation)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/zarathucorp/FeederNetEstimation?branch=master&svg=true)](https://ci.appveyor.com/project/zarathucorp/FeederNetEstimation)

Show FeederNet estimation result with shiny


## Make installer for Windows - [RInno](https://github.com/ficonsulting/RInno)

```r
create_app(app_name = "FeederNetEstimation", 
           app_repo_url = "https://github.com/zarathucorp/FeederNetEstimation",
           pkgs = c("ggplot2", "DT", "data.table", "jsmodule", "shinycustomloader", "shinyhttr", "shinyWidgets", "zip", "htmltools", "devEMF")
           )

compile_iss()           
```