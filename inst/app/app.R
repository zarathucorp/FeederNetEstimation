library(shiny);library(DT);library(shinycustomloader);library(shinyhttr);library(shinyWidgets);library(data.table);library(jsmodule);library(jstable);library(ggplot2);library(survival)
options(shiny.sanitize.errors = F)
options(shiny.maxRequestSize=50*1024^2)

#credentials <- data.frame(
#     user = c("", ""),
#     password = c("", ""),
#     admin = c(T, F),
#     stringsAsFactors = FALSE)

#create_db(credentials_data = credentials, sqlite_path = "database.sqlite")

ui <- navbarPage("CDM: estimation",
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



server <- function(input, output, session) {
  
  
  userFile <- eventReactive(input$file, {
    # If no file is selected, don't do anything
    #validate(need(input$file, message = FALSE))
    input$file
  })
  
  data <- eventReactive(input$file, {
    #validate(need((grepl("zip", userFile()$name) == T), message = "Please upload zip file"))
    #validate(need((grepl("csv", userFile()$name) == T), message = "Please upload csv/xlsx/sav/sas7bdat file"))
    #files <- unzip(userFile()$datapath, exdir = "ex")
    tmp <- tempfile()
    zip::unzip(userFile()$datapath, exdir = tmp)
    ref <- readRDS(paste0(tmp, "/result/outcomeModelReference.rds"))
    fig.att <- grep("attritionDiagram", list.files(paste0(tmp, "/result")), value = T)
    fig.attstrata <- grep("Strata", fig.att, value = T)
    fig.att <- list(setdiff(fig.att, fig.attstrata), fig.attstrata)
    
    fig.covbal <- grep("covBal", list.files(paste0(tmp, "/result")), value = T)
    fig.covalscatter <- grep("Scatter", fig.covbal, value = T)
    fig.covbal <-list(setdiff(fig.covbal, fig.covalscatter), fig.covalscatter)
    
    fig.psplot <- grep("propensityScorePlot", list.files(paste0(tmp, "/result")), value = T) 
    fig.psstrata <- grep("Strata", fig.psplot, value = T)
    fig.psplot <- list(setdiff(fig.psplot, fig.psstrata), fig.psstrata)
    
    fig.km <-  grep("kaplanMeier", list.files(paste0(tmp, "/result")), value = T) 
    
    return(list(dir = tmp, ref = ref, fig.att = fig.att, fig.covbal = fig.covbal, fig.psplot = fig.psplot, fig.km = fig.km))
  })
  
  
  observeEvent(data(), {
    output$selectdata <- renderUI({
      tagList(
        h4("Select study"),
        selectInput("tID", "targetId", choices = unique(data()$ref$targetId)),
        selectInput("cID", "comparatorId", choices = unique(data()$ref$comparatorId)),
        selectInput("oID", "outcomeId", choices = unique(data()$ref$outcomeId))
        
      )
    })
    
    output$typeOutcome <-renderUI({
      if (length(data()$fig.km) == 0){
        radioButtons("type_outcome", "Result select", choices = c("Hazard ratio",  "psModelCoef", "attrition", "Event", "Others"), selected = "Hazard ratio", inline = T)
      } else{
        radioButtons("type_outcome", "Result select", choices = c("kaplanMeier", "Hazard ratio", "psModelCoef",  "attrition", "Event", "Others"), selected = "kaplanMeier", inline = T)
      }
      
    })
  })
  
  dlist <- reactive({
    req(input$tID)
    req(input$cID)
    req(input$oID)
    info <- subset(data()$ref, targetId == input$tID & comparatorId == input$cID & outcomeId == input$oID)
    return(info)
  })
  
  
  observeEvent(data() ,{
    output$downoriginal <- renderUI({
      downloadButton("downloadoriginal", "Download")
    })
    
    output$downps <- renderUI({
      downloadButton("downloadps", "Download")
    })
    
    output$downbalance <- renderUI({
      downloadButton("downloadbalance", "Download")
    })
    
  })
  
  dts <- reactive({
    req(dlist())
    org <- readRDS(paste0(data()$dir, dlist()$psFile))
    ps <- readRDS(paste0(data()$dir, dlist()$strataFile))
    org$treatment <- factor(org$treatment)
    ps$treatment <- factor(ps$treatment)
    org$outcomeCount <- factor(org$outcomeCount)
    ps$outcomeCount <- factor(ps$outcomeCount)
    list(original = org, ps = ps, label = mk.lev(org))
  })
  
  x = reactiveValues(df = NULL, dfmat = NULL, bal = NULL)
  observeEvent(dts(), {
    df <- dts()$original
    x$df <- df
    dfmat <- dts()$ps
    x$dfmat <- dfmat
  })
  
  
  output$data_original <- renderDT({
    datatable(x$df, rownames = F, filter = "top", caption = "Original cohort", 
              options = c(jstable::opt.data("data_original"), list(scrollX = TRUE)), editable = T)
    
  })
  
  proxy = dataTableProxy('data_original')
  
  observeEvent(input$data_original_cell_edit, {
    info = input$data_original_cell_edit
    str(info)
    i = info$row
    j = info$col + 1
    v = info$value
    x$df[i, j] <<- DT::coerceValue(v, x$df[i, j])
    replaceData(proxy, x$df, resetPaging = FALSE)  # important
  })
  
  
  output$data_ps <- renderDT({
    datatable(x$dfmat, rownames = F, filter = "top", caption = "PS matching cohort", 
              options = c(jstable::opt.data("data_ps"), list(scrollX = TRUE)), editable = T)
    
  })
  
  proxymat = dataTableProxy('data_ps')
  
  observeEvent(input$data_ps_cell_edit, {
    info = input$data_ps_cell_edit
    str(info)
    i = info$row
    j = info$col + 1
    v = info$value
    x$dfmat[i, j] <<- DT::coerceValue(v, x$dfmat[i, j])
    replaceData(proxymat, x$dfmat, resetPaging = FALSE)  # important
  })
  
  
  output$downloadoriginal <- downloadHandler(
    filename = function() {
      paste("data_original", ".csv", sep = "")
    },
    content = function(file) {
      data.table::fwrite(x$df, file)
    }
  )
  
  output$downloadps <- downloadHandler(
    filename = function() {
      paste("data_ps", ".csv", sep = "")
    },
    content = function(file) {
      data.table::fwrite(x$dfmat, file)
    }
  )
  
  output$flow_original <- renderImage({
    req(input$tID)
    req(input$cID)
    req(input$oID)
    name.img <- grep(paste0("t", input$tID, "_c", input$cID, "_o", input$oID) , data()$fig.att[[1]], value = T)
    filename <- normalizePath(file.path(paste0(data()$dir, "/result/"), name.img))
    #filename <- paste("data", input$file_figure, sep="")
    list(src = filename,
         width = "100%",
         alt = "This is alternate text")
  }, deleteFile = F)
  
  output$flow_ps <- renderImage({
    req(input$tID)
    req(input$cID)
    req(input$oID)
    name.img <- grep(paste0("t", input$tID, "_c", input$cID, "_o", input$oID) , data()$fig.att[[2]], value = T)
    filename <- normalizePath(file.path(paste0(data()$dir, "/result/"), name.img))
    #filename <- paste("data", input$file_figure, sep="")
    list(src = filename,
         width = "100%",
         alt = "This is alternate text")
  }, deleteFile = F)
  
  
  desc.original <- callModule(tb1module2, "desc", data = reactive(dts()$original[,  c(3, 5:ncol(dts()$original))]), data_label = reactive(dts()$label))
  
  output$desc_original <- renderDT({
    tb = desc.original()$table
    cap = desc.original()$caption
    out.tb1 = datatable(tb, rownames = T, extensions= "Buttons", caption = cap,
                        options = c(opt.tb1("tb1"),
                                    list(columnDefs = list(list(visible=FALSE, targets= which(colnames(tb) %in% c("test","sig"))))
                                    ),
                                    list(scrollX = TRUE)
                        )
    )
    if ("sig" %in% colnames(tb)){
      out.tb1 = out.tb1 %>% formatStyle("sig", target = 'row' ,backgroundColor = styleEqual("**", 'yellow'))
    }
    return(out.tb1)
  })
  
  desc.ps <- callModule(tb1module2, "desc", data = reactive(dts()$ps[,  c(3, 5:(ncol(dts()$ps) -1))]), data_label = reactive(dts()$label))
  
  output$desc_ps <- renderDT({
    tb = desc.ps()$table
    cap = desc.ps()$caption
    out.tb1 = datatable(tb, rownames = T, extensions= "Buttons", caption = cap,
                        options = c(opt.tb1("tb1"),
                                    list(columnDefs = list(list(visible=FALSE, targets= which(colnames(tb) %in% c("test","sig"))))
                                    ),
                                    list(scrollX = TRUE)
                        )
    )
    if ("sig" %in% colnames(tb)){
      out.tb1 = out.tb1 %>% formatStyle("sig", target = 'row' ,backgroundColor = styleEqual("**", 'yellow'))
    }
    return(out.tb1)
  })
  
  
  out.balance <- reactive({
    out.bal <- readRDS(paste0(data()$dir, dlist()$covariateBalanceFile))
    
    dd <- data.table(out.bal)
    for (v in names(dd)[c(2:5, 7:10)]){
      dd[[v]] <- ifelse(is.na(dd[[v]]), 0, dd[[v]])
    }
    
    splitcov <- strsplit(as.character(dd$covariateName), ": ")
    
    n1 <- sapply(splitcov, `[`, 1)
    n2 <- sapply(splitcov, `[`, 2)
    
    lb <- unique(n1)[order(unique(n1))]
    
    
    kk <- lapply(lb, function(x){
      if (grepl("gender = ", x) == F){
        rowss <- data.table(t(c(rep(NA, 11), x, rep(NA, 4))))
        names(rowss) <- names(dd)
        out <- dd[grep(x, n1), ]
        out$covariateName <- gsub(paste0(x, ": "), "", out$covariateName)
        out <- rbind(rowss,  out[order(covariateName)])
      } else{
        out <- dd[grep(x, n1), ]
        out$covariateName <- gsub(paste0("gender = "), "", out$covariateName)
      }
      return(out)
    })
    
    out.bal <- rbindlist(kk)
    varconti <- names(out.bal)[c(2:11, 15:16)]
    out.bal[, (varconti) := lapply(.SD, as.numeric), .SDcols = varconti]
    out.bal <- data.frame(out.bal)
    
    
    it <- 2
    ic <- 3
    bal.before <- sapply(c(it, ic), function(x){paste0(out.bal[, x + 2], " (", round(100 * out.bal[, x], input$dec_bal), "%)")})
    bal.after <- sapply(c(it, ic), function(x){paste0(out.bal[, x + 7], " (", round(100 * out.bal[, x + 5], input$dec_bal), "%)")})
    
    out.tb <- data.frame(out.bal[, 12], bal.before, round(out.bal[, 15], input$dec_bal + 2), bal.after, round(out.bal[, 16], input$dec_bal + 2), out.bal[, c(1, 13, 14)])
    out.tb[out.tb == "NA (NA%)"] <- NA
    names(out.tb)[1:7] <- c("Covariate", "Treated_before", "Comparator_before", "SMD_before", "Treated_after", "Comparator_after", "SMD_after")
    
    return(out.tb)
  })
  
  output$data_balance <- renderDT({
    
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          class = 'dt-center',
          th(rowspan = 2, 'Covariate'),
          th(colspan = 3, class = "dt-center", 'Before'),
          th(colspan = 3, class = "dt-center", 'After')
        ),
        tr(
          lapply(rep(c('Treated', 'Comparator', 'SMD'), 2), th)
        )
      )
    ))
    
    observeEvent(out.balance(), {
      bal <- out.balance()
      x$bal <- bal
    })
    
    
    
    datatable(x$bal[, 1:7], rownames = F, filter = "top", caption = "Balance table for PS matching", container = sketch, class = "stripe nowrap compact",
              options = c(list(dom = "<lf<rt>Bip>", lengthMenu = list(c(10, 
                                                                        25, 50, 100, -1), c("10", "25", "50", "100", "All")), pageLength = 25, ordering = T, 
                               buttons = list("copy", "print", list(extend = "collection", 
                                                                    buttons = list(list(extend = "csv", filename = "data_balance"), 
                                                                                   list(extend = "excel", filename = "data_balance"), list(extend = "pdf", 
                                                                                                                                           filename = "data_balance")), text = "Download"))), 
                          list(scrollX = TRUE), list(pageLength = 25),
                          list(columnDefs = list(list(className = 'dt-right', targets = 0), list(className = 'dt-center', targets = c(1:2, 4:5))))), editable = T
    ) %>% formatStyle("Treated_before", target = 'row', fontWeight = styleEqual(NA, 'bold'))
    
  })
  
  proxybal = dataTableProxy('data_balance')
  
  observeEvent(input$data_balance_cell_edit, {
    info = input$data_balance_cell_edit
    str(info)
    i = info$row
    j = info$col + 1
    v = info$value
    x$bal[i, j] <<- DT::coerceValue(v, x$bal[i, j])
    replaceData(proxybal, x$bal, resetPaging = FALSE)  # important
  })
  
  
  
  
  output$downloadbalance <- downloadHandler(
    filename = function() {
      paste("balance", ".csv", sep = "")
    },
    content = function(file) {
      data.table::fwrite(x$bal, file)
    }
  )
  
  
  output$image_balance <- renderImage({
    req(input$tID)
    req(input$cID)
    req(input$oID)
    
    nn <- ifelse(input$bal_img == "Top 20", data()$fig.covbal[[1]], data()$fig.covbal[[2]])
    name.img <- grep(paste0("t", input$tID, "_c", input$cID, "_o", input$oID) , nn, value = T)
    filename <- normalizePath(file.path(paste0(data()$dir, "/result/"), name.img))
    #filename <- paste("data", input$file_figure, sep="")
    list(src = filename,
         width = "100%",
         alt = "This is alternate text")
  }, deleteFile = F)
  
  
  
  
  output$image_psdist <- renderImage({
    req(input$tID)
    req(input$cID)
    req(input$oID)
    nn <- ifelse(input$psdist_img == "Before", data()$fig.psplot[[1]], data()$fig.psplot[[2]])
    name.img <- grep(paste0("t", input$tID, "_c", input$cID, "_o", input$oID) , nn, value = T)
    filename <- normalizePath(file.path(paste0(data()$dir, "/result/"), name.img))
    #filename <- paste("data", input$file_figure, sep="")
    list(src = filename,
         width = "100%",
         alt = "This is alternate text")
  }, deleteFile = F)
  
  
  
  
  observeEvent(input$type_outcome, {
    if (input$type_outcome == "kaplanMeier"){
      output$modelOutcome <- renderUI(imageOutput("model_outcome1"))
      output$model_outcome1 <- renderImage({
        name.img <- "kaplanMeier.png"
        filename <- normalizePath(file.path(paste0(data()$dir, "/result/"), name.img))
        #filename <- paste("data", input$file_figure, sep="")
        list(src = filename,
             width = "100%",
             alt = "This is alternate text")
      }, deleteFile = F)
    } else{
      output$modelOutcome <- renderUI(DTOutput("model_outcome"))
      output$model_outcome <- renderDT({
        #userFile()
        res <- readRDS(paste0(data()$dir, dlist()$outcomeModelFile))
        if (input$type_outcome == "psModelCoef"){
          #out.df <- data.frame(Coefficients = res[[input$type_outcome]])
          
          datatable(data.frame(Covariates = names(res[[input$type_outcome]]), Coef = res[[input$type_outcome]]),  rownames = F, filter = "top", extensions= "Buttons", caption = "PS model coefficients", 
                    options = c(jstable::opt.data(input$type_outcome), list(scrollX = TRUE))) %>% 
            formatRound(2, digits = 3)
        } else if (input$type_outcome == "Hazard ratio"){
          hr <- res$outcomeModelTreatmentEstimate
          pv <- 2 * (1 - pnorm(abs(hr$logRr/hr$seLogRr)))
          pv <- ifelse(pv < 0.001, "< 0.001", round(pv, 3))
          tb.hr <- data.table(`HR (95% CI)` = paste0(round(exp(hr$logRr), 3), " (", round(exp(hr$logLb95), 3), "-", round(exp(hr$logUb95), 3), ")"), `P value` = pv)
          datatable(tb.hr,  rownames = F, extensions= "Buttons", caption = "Hazard ratio", 
                    options = c(jstable::opt.tbreg("Hazard ratio"), list(scrollX = TRUE))) 
        } else if (input$type_outcome == "Others"){
          name.tb <- setdiff(names(res), c("targetId", "comparatorId", "attrition", "psModelCoef", "outcomeModelTreatmentEstimate", "populationCounts", "outcomeCounts", "timeAtRisk"))
          datatable(t(sapply(name.tb, function(x){res[[x]]})), rownames = F,  extensions= "Buttons", caption = input$type_outcome, 
                    options = c(jstable::opt.data(input$type_outcome), list(scrollX = TRUE))) 
        } else if (input$type_outcome == "Event"){
          tb.oth <- t(data.frame(as.integer(res$populationCounts[, 2:3]), as.integer(res$timeAtRisk), as.integer(res$outcomeCounts[5:6])))
          colnames(tb.oth) <- c("Treated", "Comparator")
          rownames(tb.oth) <- c("Population", "Time at Risk", "Event")
          datatable(tb.oth,  rownames = T, extensions= "Buttons", caption = input$type_outcome, 
                    options = c(jstable::opt.data("Event"), list(scrollX = TRUE))) 
        } else {
          datatable(res[[input$type_outcome]],  rownames = F, extensions= "Buttons", caption = input$type_outcome, 
                    options = c(jstable::opt.data(input$type_outcome), list(scrollX = TRUE))) 
        }
        
      }) 
    }
  })
  
  
  observeEvent(dts(),{
    output$indepkm <- renderUI({
      selectInput("indep_km", "Independent variables",
                  choices = c("None", "treatment"), multiple = F,
                  selected = "treatment")
    })
  })
  
  form.km <- reactive({
    validate(
      need(!is.null(input$indep_km), "Please select at least 1 independent variable.")
    )
    
    if (input$indep_km == "None"){
      return(as.formula(paste("survival::Surv(", "survivalTime", ",", "event", ") ~ ", "1", sep="")))
    } else {
      return(as.formula(paste("survival::Surv(", "survivalTime", ",", "event", ") ~ ", input$indep_km, sep="")))
    }
  })
  
  kmList <- reactive({
    req(input$indep_km)
    data.org <- dts()$original
    data.org$event <- as.integer(as.character(data.org$outcomeCount) != 0)
    data.org$survivalTime <- data.org$survivalTime / 365
    cc <- substitute(survival::survfit(.form, data= data.org), list(.form= form.km()))
    res.org <- eval(cc)
    
    data.ps <- dts()$ps
    data.ps$event <- as.integer(as.character(data.ps$outcomeCount) != 0)
    data.ps$survivalTime <- data.ps$survivalTime / 365
    cc2 <- substitute(survival::survfit(.form, data= data.ps), list(.form= form.km()))
    res.ps <- eval(cc2)
    
    
    surv.org <- summary(res.org, time = 0:floor(max(summary(res.org)$time)))
    surv.ps <- summary(res.ps, time = 0:floor(max(summary(res.ps)$time)))
    cox.org <- cox.ps <- NULL
    if (input$indep_km != "None"){
      form.corg <- form.km()
      if (input$cox_marginal){
        form.corg <- as.formula(paste0(deparse(form.km()), " + cluster(subjectId)"))
      }
      corg <- substitute(survival::coxph(.form, data= data.org), list(.form= form.corg)) 
      resorg <- summary(eval(corg))$coefficients
      cox.org <- data.table(`HR (95% CI)` = paste0(round(resorg[, 2], 3), " (", round(exp(resorg[, 1] - qnorm(0.975) * resorg[, 3]), 3), "-", round(exp(resorg[, 1] + qnorm(0.975) * resorg[, 3]), 3), ")"),
                            `P value` = ifelse(resorg[, "Pr(>|z|)"] < 0.001, "< 0.001", round(resorg[, "Pr(>|z|)"], 3)))
      
      form.cps <- form.corg
      if (input$cox_strata){
        form.cps <- as.formula(paste0(deparse(form.corg), " + strata(stratumId)"))
      }
      cps <- substitute(survival::coxph(.form, data= data.ps), list(.form= form.cps))
      resps <- summary(eval(cps))$coefficients
      cox.ps <- data.table(`HR (95% CI)` = paste0(round(resps[, 2], 3), " (", round(exp(resps[, 1] - qnorm(0.975) * resps[, 3]), 3), "-", round(exp(resps[, 1] + qnorm(0.975) * resps[, 3]), 3), ")"),
                           `P value` = ifelse(resps[, "Pr(>|z|)"] < 0.001, "< 0.001", round(resps[, "Pr(>|z|)"], 3)))
    }
    
    
    
    
    if (input$indep_km == "None"){
      yst.name <- ""
      yst.lab <- "All"
    } else {
      yst.name <- ""
      yst.lab <- c("Comparator", "Treatment")
      if (!is.null(input$groupc)){
        yst.lab <- c(input$groupc, input$groupt)
      }
    } 
    ylab <- ifelse(input$cumhazkm, "Cumulative hazard", "Survival")
    return(list(res.org = res.org, res.ps = res.ps, ylab = ylab, yst.name = yst.name, yst.lab = yst.lab, data.org = data.org, data.ps = data.ps, 
                cox.org = cox.org, cox.ps = cox.ps, surv.org = surv.org, surv.ps = surv.ps))
  })
  
  observeEvent(kmList(), {
    output$ranges = renderUI({
      res.km <- kmList()$res.ps
      xmax <- round(max(res.km$time))
      
      value.timeby <- signif(xmax/7, 1)
      #if (!is.null(timeby)){
      #  value.timeby <- timeby
      #}
      
      range.x <- c(0, xmax)
      #if(is.null(range.x)){
      #  range.x <- c(0, xmax)
      #}
      range.y <- c(0, 1)
      #if(is.null(range.y)){
      #range.y <- c(0, 1)
      #}
      
      xstep.default <- ifelse(xmax <= 365, 1, 5)
      
      tagList(
        sliderInput("timeby", "Time by",
                    min = 1, max = xmax, value = value.timeby, step = xstep.default),
        
        sliderInput("xlims", "X axis range(time)",
                    min = 0, max = xmax, value = range.x, step = xstep.default),
        sliderInput("ylims", "Y axis range(probability)",
                    min = 0, max = 1, value = range.y , step = 0.02)
      )
    })
  })
  
  kmInput.original <- reactive({
    req(kmList())
    req(input$timeby)
    req(input$xlims)
    req(input$ylims)
    res.km <- kmList()$res.org
    ylab <- kmList()$ylab
    yst.name <- kmList()$yst.name
    yst.lab <- kmList()$yst.lab
    data.km <- kmList()$data.org
    
    if(is.null(input$legendx)){
      legend.p <- c(0.85, 0.8)
    } else{
      legend.p <-  c(input$legendx, input$legendy)
    }
    
    if(is.null(input$pvalx)){
      pval.coord <- c(as.integer(input$xlims[1]+ input$xlims[2]/5), 0.1 + input$ylims[1])
    } else{
      pval.coord <-  c(input$pvalx, input$pvaly)
    }
    
    linecols = "Set1"
    if (!is.null(input$linetype)){
      linecols = ifelse(input$linetype, "black", "Set1")
    }
    
    return(jskm::jskm(res.km, pval = input$pvalkm, mark=F, table= input$tablekm, ylab= ylab, ystrataname = yst.name, ystratalabs = yst.lab, ci= input$cikm, timeby = input$timeby, xlims = input$xlims, ylims = input$ylims,
                      cumhaz= input$cumhazkm, cluster.option = "None", cluster.var = NULL, data = data.km, pval.coord = pval.coord, legendposition = legend.p, xlabs = "Time in years", surv.scale = "percent", linecols = linecols)
           
    )
  })
  
  kmInput.ps <- reactive({
    req(kmList())
    req(input$timeby)
    req(input$xlims)
    req(input$ylims)
    res.km <- kmList()$res.ps
    ylab <- kmList()$ylab
    yst.name <- kmList()$yst.name
    yst.lab <- kmList()$yst.lab
    data.km <- kmList()$data.ps
    
    if(is.null(input$legendx)){
      legend.p <- c(0.85, 0.8)
    } else{
      legend.p <-  c(input$legendx, input$legendy)
    }
    
    if(is.null(input$pvalx)){
      pval.coord <- c(as.integer(input$xlims[1]+ input$xlims[2]/5), 0.1 + input$ylims[1])
    } else{
      pval.coord <-  c(input$pvalx, input$pvaly)
    }
    
    linecols = "Set1"
    if (!is.null(input$linetype)){
      linecols = ifelse(input$linetype, "black", "Set1")
    }
    return(jskm::jskm(res.km, pval = input$pvalkm, mark=F, table= input$tablekm, ylab= ylab, ystrataname = yst.name, ystratalabs = yst.lab, ci= input$cikm, timeby = input$timeby, xlims = input$xlims, ylims = input$ylims,
                      cumhaz= input$cumhazkm, cluster.option = "None", cluster.var = NULL, data = data.km, pval.coord = pval.coord, legendposition = legend.p, xlabs = "Time in years", surv.scale = "percent", linecols = linecols)
           
    )
  })
  
  output$km_original <- renderPlot({
    print(kmInput.original())
  })
  
  output$km_ps <- renderPlot({
    print(kmInput.ps())
  })
  
  
  
  output$downloadControls_km_original <- renderUI({
    tagList(
      column(4,
             selectizeInput("file_ext_km_original", "File extension (dpi = 300)",
                            choices = c("jpg","pdf", "tiff", "svg", "emf"), multiple = F,
                            selected = "jpg"
             )
      ),
      column(4,
             sliderInput("fig_width_km_original", "Width (in):",
                         min = 5, max = 15, value = 8
             )
      ),
      column(4,
             sliderInput("fig_height_km_original", "Height (in):",
                         min = 5, max = 15, value = 6
             )
      )
    )
  })
  
  output$downloadButton_km_original <- downloadHandler(
    filename =  function() {
      paste(input$indep_km,"_kaplan_meier_original.",input$file_ext_km_original ,sep="")
      
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      withProgress(message = 'Download in progress',
                   detail = 'This may take a while...', value = 0, {
                     for (i in 1:15) {
                       incProgress(1/15)
                       Sys.sleep(0.01)
                     }
                     
                     if (input$file_ext_km_original == "emf"){
                       devEMF::emf(file, width = input$fig_width_km_original, height = input$fig_height_km_original, coordDPI = 300, emfPlus = F)
                       graphics::plot(kmInput.original())
                       grDevices::dev.off()
                       
                     } else{
                       ggsave(file, kmInput.original(), dpi = 300, units = "in", width = input$fig_width_km_original, height =input$fig_height_km_original)
                     }
                   })
      
    }
  )
  
  
  output$downloadControls_km_ps <- renderUI({
    tagList(
      column(4,
             selectizeInput("file_ext_km_ps", "File extension (dpi = 300)",
                            choices = c("jpg","pdf", "tiff", "svg", "emf"), multiple = F,
                            selected = "jpg"
             )
      ),
      column(4,
             sliderInput("fig_width_km_ps", "Width (in):",
                         min = 5, max = 15, value = 8
             )
      ),
      column(4,
             sliderInput("fig_height_km_ps", "Height (in):",
                         min = 5, max = 15, value = 6
             )
      )
    )
  })
  
  output$downloadButton_km_ps <- downloadHandler(
    filename =  function() {
      paste(input$indep_km,"_kaplan_meier_ps.",input$file_ext_km_ps ,sep="")
      
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      withProgress(message = 'Download in progress',
                   detail = 'This may take a while...', value = 0, {
                     for (i in 1:15) {
                       incProgress(1/15)
                       Sys.sleep(0.01)
                     }
                     
                     if (input$file_ext_km_ps == "emf"){
                       devEMF::emf(file, width = input$fig_width_km_ps, height = input$fig_height_km_ps, coordDPI = 300, emfPlus = F)
                       graphics::plot(kmInput.ps())
                       grDevices::dev.off()
                       
                     } else{
                       ggsave(file, kmInput.ps(), dpi = 300, units = "in", width = input$fig_width_km_ps, height =input$fig_height_km_ps)
                     }
                   })
      
    }
  )
  
  
  
  output$option_kaplan <- renderUI({
    if (input$indep_km == "None"){
      tagList(
        h3("Legend position"),
        sliderInput(session$ns("legendx"), "x-axis (proportion)",
                    min = 0, max = 1, value = 0.85),
        sliderInput(session$ns("legendy"), "y-axis",
                    min = 0, max = 1, value = 0.8)
      )
    } else{
      tagList(
        h3("Legend position"),
        sliderInput(session$ns("legendx"), "x-axis (proportion)",
                    min = 0, max = 1, value = 0.85),
        sliderInput(session$ns("legendy"), "y-axis",
                    min = 0, max = 1, value = 0.8),
        
        h3("P-value position"),
        sliderInput(session$ns("pvalx"), "x-axis (time)",
                    min = 0, max = input$xlims[2], value = as.integer(input$xlims[1]+ input$xlims[2]/5)),
        sliderInput(session$ns("pvaly"), "y-axis",
                    min = 0, max = 1, value = 0.1 + input$ylims[1]),
        h3("Group label"),
        textInput(session$ns("groupc"), "Comparator", value = "Comparator"),
        textInput(session$ns("groupt"), "Treatment", value = "Treatment"),
        h3("Line setting"),
        checkboxInput(session$ns("linetype"), "Black with dashed line", value = F)
        
      )
    }
  })
  
  output$cox_original <- renderPrint({
    req(input$indep_km)
    if(input$indep_km != "None"){
      kmList()$cox.org 
    }
  })
  
  output$cox_ps <- renderPrint({
    req(input$indep_km)
    if(input$indep_km != "None"){
      kmList()$cox.ps
    }
  })
  
  output$surv_original <- renderPrint({
    req(input$indep_km)
    kmList()$surv.org
  })
  
  output$surv_ps <- renderPrint({
    req(input$indep_km)
    kmList()$surv.ps
  })
  
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
}




shinyApp(ui, server)