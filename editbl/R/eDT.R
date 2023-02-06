#' UI of the eDT module
#' @param id character
#' @param ... arguments passed to  DT::DTOutput
#' @importFrom DT DTOutput
#' @importFrom shiny actionButton tagList HTML tags fluidPage
#' @importFrom shinyjs disabled useShinyjs
#' @return HTML
#' 
#' @author Jasper Schelfhout
#' @export
eDTOutput <- function(id,...) {
  ns <- NS(id)
  
  # input$current_id to detect clicked row.
  js <- HTML(
      sprintf("function get_id(clicked_id, ns) {
              Shiny.setInputValue(ns+\"current_id\", clicked_id, {priority: \"event\"});
              };
              document.addEventListener(\"keydown\", (event) => {
              if (event.ctrlKey && event.key === \"z\") {
              Shiny.setInputValue(\"%1$s\",  Math.random(), {priority: \"event\"});
              }});
              document.addEventListener(\"keydown\", (event) => {
              if (event.ctrlKey && event.key === \"y\") {
              Shiny.setInputValue(\"%2$s\",  Math.random(), {priority: \"event\"});
              }});",
          ns("undo"), ns("redo")
      )
  )
  
  fluidPage(
      shinyjs::useShinyjs(),
      tags$script(js),
      tags$head(tags$style(HTML(disableDoubleClickButtonCss))),
      actionButton(ns("addRow"), label = "add", icon = icon("plus")),
      shinyjs::disabled(actionButton(ns("undo"), label = "undo", icon = icon("rotate-left"))),
      shinyjs::disabled(actionButton(ns("redo"), label = "redo", icon = icon("rotate-right"))),
      shinyjs::disabled(actionButton(ns("commit"), label = "save", icon = icon("floppy-disk"))),
      DT::DTOutput(outputId = ns("DT"), ...)
  )
}

#' Server of eDT module
#' 
#' @details Can also be used as standalone app when not ran in reactive context.
#' @details All arguments can be normal objects or reactive objects.
#' 
#' @param id character module id
#' @param data `tbl`. The function will automatically cast to tbl if needed.
#' @inheritParams DT::datatable
#' @param keys character. Defaults to all columns under the assumption that at least every row is unique.
#' @param format function accepting and returning a \code{\link[DT]{datatable}}
#' @param in_place boolean.
#' @param foreignTbls `list`. List of objects created by \code{\link{foreignTbl}}
#' @param statusColor named character. Colors to indicate status of the row.
#' @param inputUI function. UI function of a shiny module with at least arguments `id` `data` and `...`.
#' elements with inputIds identical to one of the column names are used to update the data.
#' 
#' @return reactive modified version of \code{data}
#' 
#' @examples 
#' \dontrun{
#' 
#' # tibble support
#' modifiedData <- editbl::eDT(tibble::as_tibble(mtcars))
#' 
#' # data.table support
#' modifiedData <- editbl::eDT(dtplyr::lazy_dt(data.table::data.table(mtcars)))
#' 
#' # database support
#' conn <- editbl:::connectDB()
#' modifiedData <- editbl::eDT(dplyr::tbl(conn, "Artists"), in_place = TRUE)
#' DBI::dbDisconnect(conn)
#' 
#' }
#' 
#' @author Jasper Schelfhout
#' @export
eDT <- function(
    data,
    options = list(
        keys = TRUE,
        ordering = FALSE,
        autoFill = list(update = FALSE, focus = 'focus')),
    class = "display",
    callback = NULL,
    rownames = FALSE,
    colnames = NULL,
    container,
    caption = NULL,
    filter = c("none", "bottom", "top"),
    escape = TRUE,
    style = "auto",
    width = NULL,
    height = NULL,
    elementId = NULL,
    fillContainer = getOption("DT.fillContainer", NULL),
    autoHideNavigation = getOption("DT.autoHideNavigation", NULL),
    selection = "none",
    extensions = c('KeyTable', 'AutoFill'),
    plugins = NULL,
    editable = list(target = "cell"),
    id,
    keys = NULL,
    in_place = FALSE,
    format = function(x){x},
    foreignTbls = list(),
    statusColor = c("insert"="#e6e6e6", "update"="#32a6d3", "delete"="#e52323"),
    inputUI = editbl::inputUI
) {
  args <- as.list(environment())
  
  # if not in reactive context start standalone app
  if(is.null(shiny::getDefaultReactiveDomain())){
    if(missing(id)){
      args$id <- "nevergonnaletyoudown"
    }
    # FIXME probably a better way to deal with missing arguments
    if(missing(container)){
      args$container <- NULL
    }
    result <- do.call(eDT_app, args)
  } else {
    if(missing(id)){
      stop("Please specify an id")
    }
    result <- do.call(eDTServer, args)
  }
  result
}


#' @inheritParams eDT
#' @importFrom shiny moduleServer observe reactiveValues reactive 
#'  observeEvent actionButton icon renderPrint showNotification req
#'  isolate is.reactive modalDialog modalButton renderUI uiOutput showModal
#' @importFrom DT dataTableProxy renderDT formatStyle styleEqual hideCols
#' @importFrom dplyr collect %>% relocate rows_update rows_insert rows_delete is.tbl
#' @importFrom utils str tail
#' @importFrom uuid UUIDgenerate
#' @importFrom shinyjs disable enable
#' @author Jasper Schelfhout
eDTServer <- function(
    id,
    data,
    options = list(
        keys = TRUE,
        ordering = FALSE,
        autoFill = list(update = FALSE, focus = 'focus')),
    class = "display",
    callback = NULL,
    rownames = FALSE,
    colnames = NULL,
    container,
    caption = NULL,
    filter = c("none", "bottom", "top"),
    escape = TRUE,
    style = "auto",
    width = NULL,
    height = NULL,
    elementId = NULL,
    fillContainer = getOption("DT.fillContainer", NULL),
    autoHideNavigation = getOption("DT.autoHideNavigation", NULL),
    selection = "none",
    extensions = c('KeyTable', 'AutoFill'),
    plugins = NULL,
    editable = list(target = "cell"),
    keys = NULL,
    in_place = FALSE,
    format = function(x){x},
    foreignTbls = list(),
    statusColor = c("insert"="#e6e6e6", "update"="#32a6d3", "delete"="#e52323"),
    inputUI = editbl::inputUI
) {
  moduleServer(
      id,
      function(input, output, session) {
        ns <- session$ns
        rv <- reactiveValues(
            changelog = list(),
            changeLogTracker = 0,
            fullTableRefresh = 0,
        )
        
        # Make arguments reactive
        # Need to be explicit about environement. Otherwhise they overwrite themselves.
        # This way users can pass on both reactive an non reactive arguments
        argEnv <- parent.frame(3)
        args <- c(as.list(argEnv))
        args$id <- NULL
        
        for(arg in names(args)){
          if(!shiny::is.reactive(args[[arg]])){
            eval(parse(text = sprintf("assign(arg, shiny::reactive(%s, env = argEnv))", arg)))
          }
        }
        
        # Default for keys is all columns
        if(is.null(argEnv$keys)){
          keys <- reactive({
                as.character(dplyr::tbl_vars(data()))
              })
        }
        
        # The colnames argument can have different formats
        # Unify format here so it can be re-used in edit modals as well
        charColnames <- reactive({
              standardizeColnamesArgument(colnames(),  data())
            }
        )
        
        deductedColnames <- reactive({
              getNonNaturalKeyCols(foreignTbls())
            })
        
        # When source data changes, reset module
        # rv$committedData equals all changes
        # rv$checkPointData equals the committed data with additional utility columns
        # rv$modifiedData keeps track of the current modified/displayed status.
        observe({
              rv$fullTableRefresh
              
              data <- data()
              
              if(!dplyr::is.tbl(data)){
                warning("Data is not of class `tbl`. Converting automatically.")
                data <- castToTbl(data)
              }
              
              rv$committedData <- data
              
              for(foreignTbl in foreignTbls()){
                data <- joinForeignTbl(data,foreignTbl)
              }
              
              # FIXME: support these column names
              utilityColumns <- c("buttons", "i", "status", "deleted")
              if(any(utilityColumns %in% dplyr::tbl_vars(data))){
                stop(sprintf("Due to current code design, `editbl` does not support any of following column names: %s. \n Will be fixed on a next release.",
                        paste(utilityColumns, collapse = ", ")))
              }
              
              data <- dplyr::collect(data)
              data <- as.data.frame(data)
              
              data <- initData(data, ns = ns)
              rv$checkPointData <- data
              rv$modifiedData <- data
              rv$changelog <- list()
            })
        
        # Update server side and client side data
        # rv$newState gets assigned by various actions in the app.
        observe({
              rv$triggerNewState
              req(rv$newState)
              castCols <- base::colnames(isolate(data()))
              
              data <- rv$newState
              data <- relocate(data,  "buttons")     
              rv$modifiedData <- data
              
              data <- castForDisplay(data, cols = castCols)
              DT::replaceData(
                  proxy = proxyDT,
                  data = data,
                  resetPaging = FALSE,
                  rownames = rownames()
              )
            })
        
        output$DT <- DT::renderDT({           
              data()
              rv$fullTableRefresh
              
              # Reactive arguments that need slight modifications to work
              # with extra utitily columns
              options <- options()
              colnames <- charColnames()
              rownames <- rownames()
              escape <- escape()
              editable <- editable()
              
              data <- isolate(rv$modifiedData)
              
              baseCols <- setdiff(base::colnames(data), c("buttons", "i", "status", "deleted"))
              baseColsI <- which(base::colnames(data) %in% baseCols)
              buttonCol <- which(base::colnames(data) == "buttons") -1 + rownames
              deductedCols <- which(base::colnames(data) %in% deductedColnames()) -1 + rownames
              
              data <- castForDisplay(data)
              
              bcol <- "buttons"
              names(bcol) <- ""
              colnames <- c(bcol,colnames)
              
              # Hide completely hidden utility columns
              options$columnDefs <- c(
                  options$columnDefs, # Not sure about datatable internals, but this gives priority to user specified columnDefs.
                  list(list(
                          visible = FALSE,
                          targets = which(base::colnames(data) %in% 
                                  c("status", "i", "deleted")) - !rownames)
                  ))
              
              if(escape == TRUE){
                escape <- -buttonCol
              }
              else if(escape == FALSE){
                escape <- FALSE
              }
              else if(is.numeric(escape)){
                escape <- c(escape, -buttonCol)
              } else if(is.character(escape)){
                escape <- which(base::colnames(data) %in% escape) + rownames
                escape <- c(escape, -buttonCol)
              }
              
              # Make sure utility columns are not editable
              options$autoFill$columns <- c(options$autoFill$columns, baseColsI - 1 + rownames)
              
              if(!"disable" %in% names(editable)){
                editable <- c(editable, list("disable" = list("columns" = c(buttonCol, deductedCols))))
              } else {
                editable$disable <- list("columns" = unique(c(editable$disable$columns,
                            buttonCol,
                            deductedCols)))
              }
              
              
              # Deal with the fact that 'container' can be a missing argument
              # Which is why put arguments in a list and use do.call instead of passing on directly.
              # FIXME: there should be a better approach              
              internalArgs <- list(
                  data = data,
                  options = options,
                  class = class(),
                  callback = DT::JS(c(keyTableJS,autoFillJs, callback())),
                  rownames = rownames,
                  colnames = colnames,
                  caption = caption(),
                  filter = filter(),
                  escape = escape,
                  style = style(),
                  width = width(),
                  height = height(),
                  elementId = elementId(),
                  fillContainer = fillContainer(),
                  autoHideNavigation = autoHideNavigation(),
                  selection = selection(),
                  extensions = extensions(),
                  plugins = plugins(),
                  editable = editable
              )
              containerIsMissing <- tryCatch({container(); FALSE}, error = function(e){TRUE})
              if(!containerIsMissing){
                internalArgs <- c(internalArgs, list(container = container))
              }
              
              do.call(DT::datatable, internalArgs) %>%
                  formatStyle("status", target='row',
                      backgroundColor = styleEqual('inserted',statusColor()["insert"]))%>%
                  formatStyle("status", target='row',
                      backgroundColor = styleEqual('edited',statusColor()["update"]))%>%
                  formatStyle("deleted", target='row',
                      backgroundColor = styleEqual(TRUE,statusColor()["delete"])) %>%
                  format()()
            })
        
        proxyDT <- DT::dataTableProxy("DT")
        
        output$inputUI <- renderUI({
              data <- rv$modifiedData
              # Columns that should not be edited
              status <- c("i", "buttons", "status", "deleted")
              deducted <- deductedColnames() # Therefore non-editable
              invisible <- unlist(lapply(options()$columnDefs, function(x){
                    if(!is.null(x$visible)){
                      if(!x$visible){
                        x$targets
                      }
                    }
                  }))
              
              data <- data[,setdiff(base::colnames(data), unique(c(
                              status,
                              deducted,
                              invisible
                              )))]
                            
              data <- castToFactor(data, foreignTbls())
              
              inputUI()(
                  id = ns("modalinput"),
                  data = data[clickedRow(),],
                  colnames = charColnames())
            })
        
        observeEvent(input$edit,{
              showModal(
                  modalDialog(
                      renderUI(uiOutput(ns("inputUI"))),
                      footer = tagList(
                          actionButton(ns("confirmEdit"), "Ok"),
                          modalButton("cancel")
                      )
                  )
              )
            })
        
        clickedRow <- reactive({
              as.numeric(sub("^.*_","",input$current_id))
            })
        
        effectiveChanges <- reactive({
              data <- do.call(rbind, rv$changelog[seq_len(rv$changeLogTracker)])
              
              # get last state of the row per id
              data <- do.call(rbind,(lapply(unique(data$i), 
                            function(i){tail(data[data$i == i,],1)})))
              data
            })
        
        observe({
              req(rv$changelog)
              rv$changeLogTracker <- length(rv$changelog)
            })
        
        observeEvent(input$undo,{
              i <- rv$changeLogTracker
              req(i > 0)
              data <- rv$modifiedData
              data$buttons <- NULL
              undoChanges <- rv$changelog[[rv$changeLogTracker]]
              
              for(row in seq_len(nrow(undoChanges))){
                undoChange <- undoChanges[row,]
                
                lastLogState <- do.call(rbind,rv$changelog[seq_len(max(0,i-1))])
                lastLogState <- tail(lastLogState[lastLogState$i == undoChange$i,],1)
                
                lastCheckPointState <- rv$checkPointData[rv$checkPointData$i == undoChange$i,]
                lastCheckPointState$buttons <- NULL
                
                if(!is.null(lastLogState) && nrow(lastLogState)){
                  stateBeforeChange <- lastLogState
                } else {
                  stateBeforeChange <- lastCheckPointState
                }
                
                if(undoChange$status == "inserted" && nrow(stateBeforeChange) == 0){ # delete if row did not exist before
                  data <- data[data$i != undoChange$i,]
                } else if (undoChange$status == "deleted" && !undoChange$i %in% data$i){ # re-insert if row is now deleted
                  data <- cbind(undoChange, data)
                } else { # set row to previous state
                  data[data$i == undoChange$i,] <- stateBeforeChange
                }
              }
              
              rv$changeLogTracker <- max(0, rv$changeLogTracker - 1)
              data <- addButtons(data, "buttons", ns)
              rv$newState <- data
            })
        
        observeEvent(input$redo,{
              i <- rv$changeLogTracker
              nChanges <- length(rv$changelog)
              req(i < nChanges)
              
              data <- rv$modifiedData
              data$buttons <- NULL
              
              redoChanges <- rv$changelog[[i + 1]]
              
              for (iRedoChange in seq_len(nrow(redoChanges))){
                redoChange <- redoChanges[iRedoChange,]
                if(redoChange$status == "inserted" && !redoChange$i %in% data$i){
                  data <- rbind(redoChange,data)
                } else {
                  data[data$i == redoChange$i,] <- redoChange
                }
              }
              
              rv$changeLogTracker <- i + 1
              data <- addButtons(data, "buttons", ns)
              rv$newState <- data
              
            })
        
        observeEvent(input$confirmEdit, {
              i <- clickedRow()
              data <- rv$modifiedData
              
              data[i,] <-  fillDeductedColumns(
                  inputServer("modalinput", data[i,])(),
                  foreignTbls())
              
              currentStatus <- data[i,"status"]
              if(currentStatus == "unmodified"){
                data[i,"status"] <- "edited"
              }
              
              newChange <- data[i,]
              newChange$buttons <- NULL
              changelog <- rv$changelog[seq_len(rv$changeLogTracker)]
              changelog[[rv$changeLogTracker +1]] <- newChange
              rv$changelog <- changelog
              
              rv$newState <- data
              shiny::removeModal()
            })
        
        observe({              
              rv$edits <- input$DT_cells_filled
            })
        
        observe({
              rv$edits <- input$DT_cell_edit
            })
        
        observeEvent(rv$edits, {
              req(rv$edits)
              edits <- unique(rv$edits)
              rv$edits <- NULL
                    
              data <- rv$modifiedData
              tryCatch({                    
                    newRows <- list()
                    
                    for(i in sort(unique(edits$row))){
                      changes <- edits[edits$row == i,]
                      currentRow <- data[i,]
                      newRow <- currentRow
                      
                      hasChanged <- FALSE
                      for(iChange in seq_len(nrow(changes))){
                        change <- changes[iChange,]
                        j <- change$col + 1 + rownames()
                        currentValue <- currentRow[,j]
                        newValue <- coerceValue(
                            change$value,
                            data[i, j])
                        
                        if(!identical(currentValue, newValue)){
                          hasChanged <- TRUE
                          newRow[,j] <- newValue
                          currentStatus <- newRow[,"status"]
                          if(currentStatus == "unmodified"){
                            newRow[,"status"] <- "edited"
                          }
                        }
                      }
                      
                      if(!hasChanged){
                        next()
                      }
                      
                      newRow <- fillDeductedColumns(newRow, foreignTbls())
                      newRows[[as.character(i)]] <- newRow
                      data[i,] <- newRow
                      
                    }    
                    newChanges <- do.call(rbind, newRows)
                    
                    newChanges$buttons <- NULL
                    changelog <- rv$changelog[seq_len(rv$changeLogTracker)]
                    changelog[[rv$changeLogTracker +1]] <- newChanges
                    rv$changelog <- changelog
                    
                    rv$newState <- data
                  },
                  error = function(e){
                    rv$resultMessage <- "The change you just made is not allowed. Reverting."
                    rv$showResultMessage <- Sys.time()
                    rv$newState <- data
                    rv$triggerNewState <- Sys.time()
                  }
              )
            })
        
        observeEvent(input$delete,{
              rowNumber <- clickedRow()
              data <- rv$modifiedData
              row <- data[rowNumber,]
              row$deleted <- !row$deleted
              
              newChange <- row
              newChange$buttons <- NULL
              changelog <- rv$changelog[seq_len(rv$changeLogTracker)]
              changelog[[rv$changeLogTracker +1]] <- newChange
              rv$changelog <- changelog
              
              data[rowNumber,] <- row
              rv$newState <- data
            })
        
        observeEvent(input$addRow,{
              data <- rv$modifiedData
              data$buttons <- NULL
              
              # create new row
              newRow <- data %>%
                  dplyr::filter(FALSE)
              newRow <- newRow[1,]
              newRow <- fixInteger64(newRow) # https://github.com/Rdatatable/data.table/issues/4561
              
              newRow$status <- "inserted"
              newRow$deleted <- FALSE
              newRow$i <- uuid::UUIDgenerate()
              
              # save to changelog
              newChange <- newRow
              changelog <- rv$changelog[seq_len(rv$changeLogTracker)]
              changelog[[rv$changeLogTracker +1]] <- newChange
              rv$changelog <- changelog
              
              data <- rbind(
                  newRow,
                  data
              )
              
              data <- addButtons(data, "buttons", ns = ns)
              
              rv$newState <- data
            })
        
        effectiveInserted <- reactive({
              modified <- effectiveChanges()         
              modified[
                  modified$status == "inserted" &
                      modified$deleted == FALSE,]
            })
        
        effectiveEdited <- reactive({
              modified <- effectiveChanges()
              modified[
                  modified$status == "edited" &
                      modified$deleted == FALSE,]
            })
        
        effectiveDeleted <- reactive({
              modified <- effectiveChanges()
              modified[
                  modified$status != "inserted" &
                      modified$deleted == TRUE,]
            })
        
        observeEvent(input$commit,{
              shiny::showModal(
                  modalDialog(
                      title = "Do you really want to make the changes?",
                      sprintf("inserted: %s
                              edited: %s
                              deleted: %s
                              ",
                          nrow(effectiveInserted()),
                          nrow(effectiveEdited()),
                          nrow(effectiveDeleted())
                      ),
                      footer = tagList(
                          actionButton(ns("confirmCommit"), label = "Ok"),
                          modalButton("Cancel")
                      )
                  )
              
              )
            })
        
        observe({
              if(is.null(effectiveChanges())){
                shinyjs::disable("commit")
              } else {
                shinyjs::enable("commit")             
              }
            })
        
        observe({
              if(rv$changeLogTracker == 0){
                shinyjs::disable("undo")
              } else {
                shinyjs::enable("undo")             
              }
            })
        
        observe({
              if(length(rv$changelog) <= rv$changeLogTracker){
                shinyjs::disable("redo")
              } else {
                shinyjs::enable("redo")             
              }
            })
        
        observeEvent(input$confirmCommit, {
              
              req(effectiveChanges())
              modified <- effectiveChanges()
              cols <- as.character(dplyr::tbl_vars(data()))
              checkPoint <- rv$checkPointData
              tryCatch({
                    # Keep order. Delete > update > insert.
                    result <- rv$committedData
                    beginTransaction(result)
                    
                    # deletes
                    deleted <- merge(
                        modified[modified$deleted == TRUE,"i",drop = FALSE],
                        checkPoint,
                        by = "i")[,keys(),drop = FALSE]
                    
                    # edits
                    edited <- effectiveEdited()
                    if(!checkForeignTbls(edited, foreignTbls())){
                      stop("You made invalid edits to a row.")
                    }
                    edited <- edited[, c(cols, "i")]
                    match_x <-  merge(
                        edited[,"i", drop = FALSE],
                        checkPoint,
                        by = "i")
                    match_x <- match_x[order(match_x$i), keys(), drop = FALSE]
                    match_y <- edited[order(edited$i), keys(), drop = FALSE]
                    match <- list(
                        x = match_x,
                        y = match_y
                    )
                    edited <- edited[cols]
                    
                    # inserts
                    inserted <- effectiveInserted()
                    if(!checkForeignTbls(inserted, foreignTbls())){
                      stop("You made invalid edits to a row.")
                    }
                    inserted <- inserted[,cols]
                    
                    if(nrow(deleted)){
                      result <- rows_delete(
                          x = result,
                          y = deleted,
                          by = keys(),
                          in_place = in_place())
                    }
                    
                    result <- rows_update(
                        x = result,
                        y = edited,
                        match = match,
                        by = keys(),
                        in_place = in_place())
                    
                    result <- rows_insert(
                        x = result,
                        y = inserted,
                        by = keys(),
                        in_place = in_place())
                    
                    rv$committedData <- result
                    
                    # Set modified and rendered to comitted version
                    # re-read data in case of in_place modification
                    # This is because certain backends might modify the row further with defaults etc.
                    if(in_place()){
                      rv$fullTableRefresh <- UUIDgenerate()
                    } else {
                      checkPointState <- rv$modifiedData
                      checkPointState <- checkPointState[checkPointState$deleted != TRUE,]
                      checkPointState$status <- 'unmodified'
                      checkPointState$deleted <- FALSE
                      checkPointState$buttons <- NULL
                      rv$changelog <- list()
                      checkPointState <- addButtons(checkPointState, "buttons", ns)
                      
                      rv$checkPointData <- checkPointState
                      rv$newState <- checkPointState
                    }
                    
                    commitTransaction(result)
                    rv$resultMessage <- "success."
                  }, error = function(cond){
                    rollbackTransaction(result)
                    rv$resultMessage <- sprintf(
                        "Failure: %s",
                        cond$message
                    )
                  })
              rv$showResultMessage <- Sys.time()
              shiny::removeModal()
            })
        
        observeEvent(rv$showResultMessage,{
              shiny::showNotification(
                  rv$resultMessage
              )   
            })
        
        result <- reactive({
              inputData <- isolate(data())
              result <- rv$committedData
              tryCatch({
                    result <- castFromTbl(tbl = result, template = inputData)
                  }, error = function(e){
                    warning(sprintf("%s. Returning 'tbl'.", e$message))
                  })
              result
            })
        
        return(result)
      }
  )
}


#' Add some extra columns to data to allow for / keep track of modifications
#' @param data data.frame
#' @param ns namespace function
#' @param buttonCol character name of column with buttons
#' @param statusCol character name of column with general status (e.g. modified or not).
#' @param deleteCol character name of the column with deletion status.
#' @param iCol name of column with i
#' @return data with extra columns buttons, status, i.
#' @importFrom dplyr relocate
#' @importFrom uuid UUIDgenerate
#' @author Jasper Schelfhout
initData <- function(
    data,
    ns,
    buttonCol = "buttons",
    statusCol = "status",
    deleteCol = "deleted",
    iCol = "i"
){
  data[statusCol] <- rep("unmodified", nrow(data))
  data[deleteCol] <- rep(FALSE, nrow(data))
  
  if(nrow(data) > 0){
    data[iCol] <- unlist(lapply(seq_len(nrow(data)), uuid::UUIDgenerate))
  } else {
    data[iCol] <- character(0)
  }
  data <- addButtons(
      df = data,
      columnName = buttonCol,
      ns = ns)
  data <- relocate(data, buttonCol)
  data
}


#' Add modification buttons as a column
#' @param df data.frame
#' @param columnName character
#' @param ns namespace function
#' @return df with extra column containing buttons
#' 
#' @author Jasper Schelfhout
addButtons <- function(df, columnName, ns){
  if(!nrow(df)){
    df[columnName] <- character(0)
    return(df)
  }
  
  df[columnName] <- lapply(seq_len(nrow(df)), function(i){
        createButtons(i, ns)
      }) %>% unlist()
  df     
}

#' Helper function to write HTML
#' @details generate HTML as character once and reuse.
#' Since buttons have to be generated a lot, this otherwhise slows down the app.
#' @param suffix character sprintf placeholer for suffix
#' @param ns character sprintf placeholder for ns
#' @importFrom shiny div actionButton icon
#' @return character HTML to be filled in with \code{sprintf}
createButtonsHTML <- function(suffix = "%1$s", ns = "%2$s"){
  as.character(
      div(class = "btn-group",
          actionButton(
              inputId =  paste0(ns, "delete_row_", suffix),
              label = "",
              icon = icon("trash"),
              style = "color: red;background-color: white",
              onclick =  HTML(sprintf("get_id(this.id, '%1$s');
                          Shiny.setInputValue(\"%1$sdelete\", Math.random(), {priority: \"event\"});",
                      ns))
          ),
          actionButton(
              inputId =  paste0(ns, "edit_row_", suffix),
              label = "",
              icon = icon("pen-to-square"),
              style = "background-color: white",
              onclick =  HTML(sprintf("get_id(this.id, '%1$s');
                          Shiny.setInputValue(\"%1$sedit\", Math.random(), {priority: \"event\"});",
                      ns))
          )
      )
  )
}

buttonsHTML <- createButtonsHTML()

#' Create buttons to modify the row. See \code{\link{character}}
#' @details buttons used per row in the app.
#' @param suffix character
#' @param ns namespace
#' @return character HTML
createButtons <- function(suffix, ns){
  sprintf(
      # Can be generated with createButtonsHTML
      buttonsHTML,
      suffix,
      ns("")
  )
}

# https://stackoverflow.com/questions/60406027/how-to-disable-double-click-reactivity-for-specific-columns-in-r-datatable
disableDoubleClickButtonCss <-  "
    table tbody td:nth-child(1) {pointer-events: none;}
    table tbody td:nth-child(1)>div {pointer-events: auto;}
    "

keyTableJS <- c(
    # Trigger doubleclick by enter
    "table.on('key', function(e, datatable, key, cell, originalEvent){",
    "  var targetName = originalEvent.target.localName;",
    "  var col = cell.index().column;",
    "  if(key == 13 && targetName == 'body' && col){", # do not modify first column
    "    $(cell.node()).trigger('dblclick.dt');",
    "  }",
    "});",
    # Blur old cell when moving
    "table.on('keydown', function(e){",
    "  var keys = [9,13,37,38,39,40];",
    "  if(e.target.localName == 'input' && keys.indexOf(e.keyCode) > -1){",
    "    $(e.target).trigger('blur');",
    "  }",
    "});",
    # Click new cell when moving
    "table.on('key-focus', function(e, datatable, cell, originalEvent){",
    "  var targetName = originalEvent.target.localName;",
    "  var type = originalEvent.type;",
    "  var col = cell.index().column;", 
    "  if(type == 'keydown' && targetName == 'input' && col){",  # do not modify first column
    "    if([9,37,38,39,40].indexOf(originalEvent.keyCode) > -1){",
    "      $(cell.node()).trigger('dblclick.dt');",
    "    }",
    "  }",
    "});"
)

# get autoFill edits https://laustep.github.io/stlahblog/posts/DTcallbacks.html
autoFillJs <- c(
    "var tbl = $(table.table().node());",
    "var id = tbl.closest('.datatables').attr('id');",
    "table.on('autoFill', function(e, datatable, cells){",
    "  var out = [];",
    "  for(var i = 0; i < cells.length; ++i){",
    "    var cells_i = cells[i];",
    "    for(var j = 0; j < cells_i.length; ++j){",
    "      var c = cells_i[j];",
    "      var value = c.set === null ? '' : c.set;", # null => problem in R
    "      out.push({",
    "        row: c.index.row + 1,",
    "        col: c.index.column,",
    "        value: value",
    "      });",
    "    }",
    "  }",
    "  Shiny.setInputValue(id + '_cells_filled:DT.cellInfo', out);",
    "  table.rows().invalidate();", # this updates the column type
    "});"
)
