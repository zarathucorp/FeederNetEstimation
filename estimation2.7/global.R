source("DataPulls.R")
source("PlotsAndTables.R")

# shinySettings <- list(dataFolder = "c:/temp/Gowtham/data", blind = TRUE)
#dataFolder <- "data"
blind <- F
connection <- NULL
positiveControlOutcome <- NULL


## csv to rds
prepareForEvidenceExplorer <- function(resultsZipFile, dataFolder) {
  # resultsZipFile <- "c:/temp/ResultsMDCD.zip"
  # dataFolder <- "c:/temp/shinyData"
  if (!file.exists(dataFolder)) {
    dir.create(dataFolder, recursive = TRUE)
  }
  tempFolder <- paste(tempdir(), "unzip")
  on.exit(unlink(tempFolder, recursive = TRUE))
  zip::unzip(resultsZipFile, exdir = tempFolder)
  databaseFileName <- file.path(tempFolder, "database.csv")
  if (!file.exists(databaseFileName)) {
    stop("Cannot find file database.csv in zip file")
  }
  databaseId <- read.csv(databaseFileName, stringsAsFactors = FALSE)$database_id
  splittableTables <- c("covariate_balance", "preference_score_dist", "kaplan_meier_dist")
  
  processSubet <- function(subset, tableName) {
    targetId <- subset$target_id[1]
    comparatorId <- subset$comparator_id[1]
    fileName <- sprintf("%s_t%s_c%s_%s.rds", tableName, targetId, comparatorId, databaseId)
    saveRDS(subset, file.path(dataFolder, fileName))
  }
  
  processFile <- function(file) {
    tableName <- gsub(".csv$", "", file)
    table <- read.csv(file.path(tempFolder, file))
    if (tableName %in% splittableTables) {
      subsets <- split(table, list(table$target_id, table$comparator_id))
      plyr::l_ply(subsets, processSubet, tableName = tableName)
    } else {
      saveRDS(table, file.path(dataFolder, sprintf("%s_%s.rds", tableName, databaseId)))  
    }
  }
  
  files <- list.files(tempFolder, ".*.csv")
  plyr::l_ply(files, processFile, .progress = "text")
}