library(shiny);library(DT);library(shinycustomloader);library(shinyhttr);library(shinyWidgets);library(data.table);library(jsmodule);library(jstable);library(ggplot2);library(survival)
options(shiny.sanitize.errors = F)
options(shiny.maxRequestSize=50*1024^2)

navbarPage("CDM: estimation",
           tabPanel("Data",
                    sidebarLayout(
                      sidebarPanel(
                        fileInput("file", "Upload compressed file"),
                        helpText(a("Example data", href="https://consultdata.s3.ap-northeast-2.amazonaws.com/example_CDMestimation")),
                        uiOutput("selectdata")
                      ),
                      mainPanel(
                        tabsetPanel(type = "pills",
                                    tabPanel("Original", 
                                             withLoader(DTOutput("data_original"), type="html", loader="loader6"),
                                             uiOutput("downoriginal"),
                                             withLoader(imageOutput("flow_original"), type="html", loader="loader6")),
                                    tabPanel("Matching", 
                                             withLoader(DTOutput("data_ps", width = "100%"), type="html", loader="loader6"),
                                             uiOutput("downps"),
                                             withLoader(imageOutput("flow_ps"), type="html", loader="loader6"))
                        )
                      )
                    )
           ),
           tabPanel("Data description",
                    sidebarLayout(
                      sidebarPanel(
                        tb1moduleUI("desc")
                      ),
                      mainPanel(
                        tabsetPanel(type = "pills",
                                    tabPanel("Original", 
                                             withLoader(DTOutput("desc_original"), type="html", loader="loader6")
                                    ),
                                    tabPanel("Matching", 
                                             withLoader(DTOutput("desc_ps", width = "100%"), type="html", loader="loader6")
                                    )
                        )
                      )
                    )
           ),
           tabPanel("Matching info",
                    sidebarLayout(
                      sidebarPanel(
                        sliderInput("dec_bal", "Digits", value = 1, min = 1, max = 3)
                        
                      ),
                      mainPanel(
                        tabsetPanel(type = "pills",
                                    tabPanel("Balance Table", 
                                             withLoader(DTOutput("data_balance"), type="html", loader="loader6"),
                                             uiOutput("downbalance")
                                    ),
                                    tabPanel("Balance Image",
                                             radioButtons("bal_img", "Image select", choices = c("Top 20", "Scatterplot"), selected = "Top 20", inline = T),
                                             withLoader(imageOutput("image_balance", width = "100%"), type="html", loader="loader6")
                                    ),
                                    tabPanel("PS distribution", 
                                             radioButtons("psdist_img", "Image select", choices = c("Before", "After"), selected = "Before", inline = T),
                                             withLoader(imageOutput("image_psdist", width = "100%"), type="html", loader="loader6")
                                    )
                        )
                      )
                    )
           ),
           tabPanel("Model outcome",
                    sidebarLayout(
                      sidebarPanel(
                        uiOutput("typeOutcome")
                      ),
                      mainPanel(
                        withLoader(uiOutput("modelOutcome"), type="html", loader="loader6")
                      )
                    )
           ),
           tabPanel("Kaplan-meier",
                    sidebarLayout(
                      sidebarPanel(
                        uiOutput("indepkm"),
                        checkboxInput("cumhazkm", "Show cumulative hazard", F),
                        checkboxInput("pvalkm", "Show p-value(log-rank test)", T),
                        checkboxInput("tablekm", "Show table", T),
                        checkboxInput("cikm", "Show 95% CI", F),
                        uiOutput("ranges"),
                        checkboxInput("cox_marginal", "Marginal cox model", value = F),
                        checkboxInput("cox_strata", "Stratified cox model", value = T)
                        
                      ),
                      mainPanel(
                        shinyWidgets::dropdownButton(
                          uiOutput("option_kaplan"),
                          circle = TRUE, status = "danger", icon = icon("gear"), width = "300px",
                          tooltip = shinyWidgets::tooltipOptions(title = "Click to see other options !")
                        ),
                        tabsetPanel(type = "pills",
                                    tabPanel("Original", 
                                             withLoader(plotOutput("km_original"), type="html", loader="loader6"),
                                             verbatimTextOutput("cox_original"),
                                             h3("Download options"),
                                             wellPanel(
                                               uiOutput("downloadControls_km_original"),
                                               downloadButton("downloadButton_km_original", label = "Download the plot")
                                             ),
                                             h4("Survival rate table"),
                                             verbatimTextOutput("surv_original")
                                    ),
                                    tabPanel("Matching", 
                                             withLoader(plotOutput("km_ps"), type="html", loader="loader6"),
                                             verbatimTextOutput("cox_ps"),
                                             h3("Download options"),
                                             wellPanel(
                                               uiOutput("downloadControls_km_ps"),
                                               downloadButton("downloadButton_km_ps", label = "Download the plot")
                                             ),
                                             h4("Survival rate table"),
                                             verbatimTextOutput("surv_ps")
                                    ),
                                    wellPanel(
                                      h4("Marginal cox model considers repeated measure."),
                                      h4("Stratified cox model uses matching pairs as strata.")
                                    )
                        )
                      )
                    )
           )
)