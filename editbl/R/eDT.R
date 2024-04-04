#' UI part of \code{\link{eDT}}
#' 
#' @details Works exactly like \code{\link[DT]{DTOutput}} apart from the fact that instead of the `outputId`
#' argument, `id` is requested. Reason being that this function is a UI to a shiny module.
#' This means that the datatable can be found under the id \code{'{namespace}-{id}-DT'} instead of \code{'{namespace}-{outputId}'}.
#' 
#' Also some minor CSS and javascript is executed for functional puposes.
#' 
#' @param id `character(1)`
#' @param ... arguments passed to \code{\link[DT]{DTOutput}} 
#' @importFrom DT DTOutput
#' @importFrom shiny actionButton tagList HTML tags fluidPage tags
#' @importFrom shinyjs disabled useShinyjs hidden
#' @inherit eDT examples
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
      # Hack that ensures fontawesome is properly loaded
      shinyjs::hidden(actionButton(ns("activate_shiny_css"), label = "hidden", icon = icon("plus"))),
      
      tags$style(HTML(disableDoubleClickButtonCss(ns("DT")))),   
      DT::DTOutput(outputId = ns("DT"), ...)
  )
}

#' Create a modifieable datatable.
#' 
#' @details Works the same as \code{\link[DT]{datatable}}.
#' This function is however a shiny module and comes with additional arguments and different defaults.
#' Instead of having `output$id = renderDT(DT::datatable(iris))`, `eDT(id = 'id', data = iris)` should be used on the server side.
#' On the UI side \code{\link{eDTOutput}} should be used instead of \code{\link[DT]{DTOutput}}.
#' 
#' @details Can also be used as standalone app when not ran in reactive context.
#' @details All arguments except 'id' and 'env' can be normal objects or reactive objects.
#' 
#' @param id `character(1)` module id
#' @param data `tbl`. The function will automatically cast to tbl if needed.
#' @inheritParams DT::datatable
#' @param keys `character`. Defaults to all columns under the assumption that at least every row is unique.
#' @param format function accepting and returning a \code{\link[DT]{datatable}}
#' @param in_place `logical`. Whether to modify the data object in place or to return a modified copy.
#' @param foreignTbls `list`. List of objects created by \code{\link{foreignTbl}}
#' @param statusColor named `character`. Colors to indicate status of the row.
#' @param inputUI `function`. UI function of a shiny module with at least arguments `id` `data` and `...`.
#' #'   elements with inputIds identical to one of the column names are used to update the data.
#' @param defaults expression that evaluates to a `tibble` with (a subset of) columns of the data.
#'   It will be evaluated for each new row in the environment defined by 'env'.
#'   This allows for defaults like Sys.time() or uuid::UUIDgenerate() as well as dynamic inputs.
#' @param env `environment` in which the server function is running. Should normally not be modified.
#' 
#' @return list
#' - result `reactive` modified version of `data` (saved)
#' - state `reactive` current state of the `data` (unsaved)
#' - selected `reactive` selected rows of the `data` (unsaved)
#' 
#' @examples 
#' ## Only run this example in interactive R sessions
#' if(interactive()){
#'   # tibble support
#'   modifiedData <- editbl::eDT(tibble::as_tibble(mtcars))
#' 
#'   # data.table support
#'   modifiedData <- editbl::eDT(dtplyr::lazy_dt(data.table::data.table(mtcars)))
#' 
#'   # database support
#'   tmpFile <- tempfile(fileext = ".sqlite")
#'   file.copy(system.file("extdata", "chinook.sqlite", package = 'editbl'), tmpFile)
#' 
#'   conn <- editbl::connectDB(dbname = tmpFile)
#'   modifiedData <- editbl::eDT(dplyr::tbl(conn, "Artist"), in_place = TRUE)
#'   DBI::dbDisconnect(conn)
#' 
#'   unlink(tmpFile)
#' 
#'   # Within shiny
#'   library(shiny)
#'   library(editbl)
#'   shinyApp(
#'     ui = fluidPage(fluidRow(column(12, eDTOutput('tbl')))),
#'     server = function(input, output) {
#'       eDT('tbl',iris,)
#'     }
#'   )
#' 
#'   # Custom inputUI
#'   editbl::eDT(mtcars, inputUI = function(id, data){
#'     ns <- NS(id)
#'     textInput(
#'     ns("mpg"),
#'     label = "mpg",
#'     value = data$mpg)})
#' }
#' 
#' @author Jasper Schelfhout
#' @export
eDT <- function(
    data,
    options = list(
        dom = 'Bfrtlip',
        keys = TRUE,
        ordering = FALSE,
        autoFill = list(update = FALSE, focus = 'focus'),
        buttons = list("add", "undo", "redo", "save")
    ),
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
    extensions = c('KeyTable', 'AutoFill', "Buttons"),
    plugins = NULL,
    editable = list(target = "cell"),
    id,
    keys = NULL,
    in_place = FALSE,
    format = function(x){x},
    foreignTbls = list(),
    statusColor = c("insert"="#e6e6e6", "update"="#32a6d3", "delete"="#e52323"),
    inputUI = editbl::inputUI,
    defaults = tibble(),
    env = environment()
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
#'  freezeReactiveValue isTruthy
#' @importFrom DT dataTableProxy renderDT formatStyle styleEqual hideCols
#' @importFrom dplyr collect %>% relocate is.tbl all_of tibble
#' @importFrom utils str tail
#' @importFrom uuid UUIDgenerate
#' @importFrom shinyjs disable enable
#' @author Jasper Schelfhout
eDTServer <- function(
    id,
    data,
    options = list(
        dom = 'Bfrtlip',
        keys = TRUE,
        ordering = FALSE,
        autoFill = list(update = FALSE, focus = 'focus'),
        buttons = list("add", "undo", "redo", "save")
    ),
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
    extensions = c('KeyTable', 'AutoFill', "Buttons"),
    plugins = NULL,
    editable = list(target = "cell"),
    keys = NULL,
    in_place = FALSE,
    format = function(x){x},
    foreignTbls = list(),
    statusColor = c("insert"="#e6e6e6", "update"="#32a6d3", "delete"="#e52323"),
    inputUI = editbl::inputUI,
    defaults = tibble(),
    env = environment()
) {
  missingContainer <- missing(container)
  moduleServer(
      id,
      function(input, output, session) {
        ns <- session$ns
                
        rv <- reactiveValues(
            changelog = list(),
            changeLogTracker = 0,
            fullTableRefresh = 0,
            edits_react = 0, # to force refreshing even when reactive value stays the same
            changelog_react = 0 # # to force refreshing even when reactive value stays the same
        )
        
        # Make arguments reactive / set defaults
        # This way users can pass on both reactive an non reactive arguments
        # Need to be explicit about environement. Otherwhise they overwrite themselves.
        argEnv <- parent.frame(3)
        
        if(!shiny::is.reactive(data)){
          data <- shiny::reactive(data, env = argEnv)
        }
        
        if(!shiny::is.reactive(options)){
          options <- shiny::reactive(options, env = argEnv)
        }
        
        if(!shiny::is.reactive(class)){
          class <- shiny::reactive(class, env = argEnv)
        }
        
        if(!shiny::is.reactive(callback)){
          callback <- shiny::reactive(callback, env = argEnv)
        }
        
        if(!shiny::is.reactive(rownames)){
          rownames <- shiny::reactive(rownames, env = argEnv)
        }
        
        if(!shiny::is.reactive(colnames)){
          colnames <- shiny::reactive(colnames, env = argEnv)
        }
        
        if(!missingContainer && !shiny::is.reactive(container)){
          container <- shiny::reactive(container, env = argEnv)
        }
        
        if(!shiny::is.reactive(caption)){
          caption <- shiny::reactive(caption, env = argEnv)
        }
        
        if(!shiny::is.reactive(filter)){
          filter <- shiny::reactive(filter, env = argEnv)
        }
        
        if(!shiny::is.reactive(escape)){
          escape <- shiny::reactive(escape, env = argEnv)
        }
        
        if(!shiny::is.reactive(style)){
          style <- shiny::reactive(style, env = argEnv)
        }
        
        if(!shiny::is.reactive(width)){
          width <- shiny::reactive(width, env = argEnv)
        }
        
        if(!shiny::is.reactive(height)){
          height <- shiny::reactive(height, env = argEnv)
        }
        
        if(!shiny::is.reactive(elementId)){
          elementId <- shiny::reactive(elementId, env = argEnv)
        }
        
        if(!shiny::is.reactive(fillContainer)){
          fillContainer <- shiny::reactive(fillContainer, env = argEnv)
        }
        
        if(!shiny::is.reactive(autoHideNavigation)){
          autoHideNavigation <- shiny::reactive(autoHideNavigation, env = argEnv)
        }
        
        if(!shiny::is.reactive(selection)){
          selection <- shiny::reactive(selection, env = argEnv)
        }
        
        if(!shiny::is.reactive(extensions)){
          extensions <- shiny::reactive(extensions, env = argEnv)
        }
        
        if(!shiny::is.reactive(plugins)){
          plugins <- shiny::reactive(plugins, env = argEnv)
        }
        
        if(!shiny::is.reactive(editable)){
          editable <- shiny::reactive(editable, env = argEnv)
        }
        
        if(is.null(keys)){
          keys <- reactive({
                as.character(dplyr::tbl_vars(data()))
              })
        } else if (!shiny::is.reactive(keys)){
          keys <- shiny::reactive(keys, env = argEnv)
        }
        
        if(!shiny::is.reactive(in_place)){
          in_place <- shiny::reactive(in_place, env = argEnv)
        }
        
        if(!shiny::is.reactive(format)){
          format <- shiny::reactive(format, env = argEnv)
        }
        
        if(!shiny::is.reactive(foreignTbls)){
          foreignTbls <- shiny::reactive(foreignTbls, env = argEnv)
        }
        
        if(!shiny::is.reactive(statusColor)){
          statusColor <- shiny::reactive(statusColor, env = argEnv)
        }
        
        if(!shiny::is.reactive(inputUI)){
          inputUI <- shiny::reactive(inputUI, env = argEnv)
        }

        if(!shiny::is.reactive(defaults)){  
          defaults <- shiny::reactive({
                eval(substitute(defaults, env))
              },
              env = argEnv)
        }
        
        # Force re-evaluting reactive for values like Sys.time(), uuid::UUIDgenerate()
        defaultsAddBound <- defaults %>% shiny::bindEvent(input$add)
        
        # Some arguments can have various formats.
        # Standardize first to the most expressive format to make
        # it easier to work with in downstream code.
        colnames_std <- reactive({
              standardizeArgument_colnames(colnames(),  data())
            }
        )
        
        editable_std <- reactive({
              standardizeArgument_editable(editable(), data())
            })
        
        # Columns that are used as additional information but 
        # are not part of the key, nor the original table.
        deductedColnames <- reactive({
              getNonNaturalKeyCols(foreignTbls())
            })
        
        # When source data changes, reset module
        # rv$committedData equals all changes
        # rv$checkPointData equals the committed data with additional utility columns
        # rv$modifiedData keeps track of the current modified/displayed status.
        observe(priority = 2, label = "Reset module with new data",{
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
              
              DT::selectRows(proxyDT, NA)
              freezeReactiveValue(input, "DT_rows_selected")
            })
        
        # Update server side and client side data
        # rv$newState gets assigned by various actions in the app.
        observe(label = "Replace front-end data",{
              rv$triggerNewState
              req(!is.null(rv$newState) && isTruthy(rv$newState))
              castCols <- base::colnames(isolate(data()))
              data <- rv$newState
              data <- relocate(data,  dplyr::all_of("buttons"))     
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
              colnames <- colnames_std()
              rownames <- rownames()
              escape <- escape()
              editable <- editable_std()
              
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
              
              if(!inherits(editable, "logical")){
                if(!"disable" %in% names(editable)){
                  editable <- c(editable, list("disable" = list("columns" = c(buttonCol, deductedCols))))
                } else {
                  editable$disable <- list("columns" = unique(c(editable$disable$columns,
                              buttonCol,
                              deductedCols)))
                }
              }
              
              # For backwards compatibility
              # Maybe remove at some point? E.g. just require to be explicit about options?
              if(is.null(options$dom)){
                options$dom <- "Bfrtip"
              }
              if(is.null(options$buttons)){
                options$buttons <- list("add", "undo", "redo", "save")
              }
              
              options$buttons <- lapply(options$buttons, function(x){
                    if(is.character(x) && x %in% c("add", "undo", "redo", "save")){
                      icon = switch(x,
                          "add" = icon("plus"),
                          "undo" = icon("rotate-left"),
                          "redo" = icon("rotate-right"),
                          "save" = icon("floppy-disk"),
                          ""
                          )   
                      disabled = switch(x,
                              "add" = FALSE,
                              "undo" = TRUE,
                              "redo" =  TRUE,
                              "save" = TRUE,
                              TRUE
                          )       
                          
                      customButton(ns(x), label = x, icon, disabled = disabled)
                    } else {
                      x
                    }
                  })
                            
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
              if(!missingContainer){
                internalArgs <- c(internalArgs, list(container = container()))
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
        
        notInModalColumns <- reactive({              
              # Columns that should not be edited through the modal
              status <- c("i", "buttons", "status", "deleted")
              deducted <- deductedColnames() # Therefore non-editable
              invisible <- unlist(lapply(options()$columnDefs, function(x){
                        if(!is.null(x$visible)){
                          if(!x$visible){
                            x$targets
                          }
                        }
                      }))
              notEditable <- names(rv$modifiedData)[editable_std()$disable$columns + 1]
              unique(c(
                      status,
                      deducted,
                      invisible,
                      notEditable
                  ))
            })
        
        # Use different id each time to prevent conflicts / flashing re-rendering due to renderUI
        editModalId <- reactive({
              input$edit
              gsub("-", "_", uuid::UUIDgenerate())
            })
        
        observeEvent(input$edit, {
              rv$modalData <- inputServer(
                  editModalId(),
                  data = rv$modifiedData[clickedRow(),],
                  notEditable = notInModalColumns,
                  colnames = colnames_std,
                  foreignTbls = foreignTbls)
            })
        
        observeEvent(input$edit, {
              showModal(
                  modalDialog(
                      inputUI()(id = ns(editModalId()), data = rv$modifiedData[clickedRow(),]),
                      footer = tagList(
                          actionButton(ns("confirmEdit"), "Ok"),
                          modalButton("cancel")
                      ),
                      easyClose = TRUE
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
              req(!is.null(rv$changelog) && isTruthy(rv$changelog))
              rv$changelog_react
              
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
              data[i,] <-  fillDeductedColumns(rv$modalData(), foreignTbls())
              
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
        
        observeEvent(input$DT_cells_filled, {
              req(!is.null(input$DT_cells_filled) && isTruthy(input$DT_cells_filled))
              edits <- input$DT_cells_filled
              edits$row <- input$DT_rows_current[edits$row]           
              rv$edits <- edits
              rv$edits_react <-  rv$edits_react + 1
            })
        
        observeEvent(input$DT_cell_edit, {
              rv$edits <- input$DT_cell_edit
              rv$edits_react <-  rv$edits_react + 1
            })
         
        observeEvent(rv$edits_react, {
              req(!is.null(rv$edits) && isTruthy(rv$edits))
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
                    rv$changelog_react <- rv$changelog_react + 1
                    
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
        
        observeEvent(input$add,{
              data <- rv$modifiedData
              data$buttons <- NULL
                            
              # create new row
              newRow <- data %>%
                  dplyr::filter(FALSE)
              newRow <- newRow[1,]
              newRow <- fixInteger64(newRow) # https://github.com/Rdatatable/data.table/issues/4561
              
              defaults <- defaultsAddBound()
              for(col in base::colnames(defaults)){
                currentClass <- base::class(data[[col]])
                defaultClass <- base::class(defaults[[col]])
                
                if(!col %in% base::colnames(newRow)){
                  stop(sprintf("Column %s not available. Not adding default.", col))
                } else if (! identical(currentClass, defaultClass)){
                  stop(sprintf("Default set for %s is of type %s instead of %s", col, defaultClass, currentClass))
                } else {
                  newRow[[col]] <- defaults[[col]]
                }
              }
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
        
        observeEvent(input$save,{
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
                      ),
                      easyClose = TRUE
                  )
              
              )
            })
        
        observe({
              if(is.null(effectiveChanges())){
                shinyjs::disable("save")
              } else {
                shinyjs::enable("save")             
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
              req(!is.null(effectiveChanges()) && isTruthy(effectiveChanges()))
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
                      if(inherits(result, 'tbl_dbi') & in_place()){
                        # dbplyr:::rows_delete.tbl_dbi requires y to be in the same source and will start a transaction for this if not the case.
                        # Most backends do not support nested transactions. Therefore manually copy the data first.
                        # See also: https://github.com/tidyverse/dbplyr/issues/1298
                        temp_name = paste0("editbl_", gsub("-", "", UUIDgenerate()))
                        deleted <- dplyr::copy_to(
                            dest = dbplyr::remote_src(result),
                            df = deleted,
                            name = temp_name,
                            temporary = TRUE,
                            in_transaction = FALSE,
                            types = dbplyr::db_col_types(
                                dbplyr::remote_con(result),
                                dbplyr::remote_table(result))[base::colnames(deleted)])
                      }
                      
                      result <- rows_delete(
                          x = result,
                          y = deleted,
                          by = keys(),
                          in_place = in_place(),
                          unmatched = 'ignore')
                      
                      if(inherits(result, 'tbl_dbi')){
                        DBI::dbRemoveTable(dbplyr::remote_con(result), temp_name)
                      }
                    }
                    if(nrow(edited)){
                      result <- e_rows_update(
                          x = result,
                          y = edited,
                          match = match,
                          by = keys(),
                          in_place = in_place())
                    }
                    
                    if(nrow(inserted)){
# Needed code should there be a switch to dbplyr::rows_insert
#                      if(inherits(result, 'tbl_dbi')){
#                        # https://github.com/openanalytics/editbl/issues/1
#                        # dbplyr:::rows_insert.tbl_dbi requires y to be in the same source and will start a transaction for this if not the case.
#                        # Most backends do not support nested transactions. Therefore manually copy the data first.
#                        temp_name = paste0("editbl_", gsub("-", "", UUIDgenerate()))
#                        inserted <- dplyr::copy_to(
#                            dest = result$src,
#                            df = inserted,
#                            name = temp_name,
#                            temporary = TRUE,
#                            in_transaction = FALSE)
#                      }
                      
                      result <- e_rows_insert(
                          x = result,
                          y = inserted,
                          by = keys(),
                          in_place = in_place(),
                          conflict = "ignore")
                      
# Needed code should there be a switch to dbplyr::rows_insert                     
#                      if(inherits(result, 'tbl_dbi')){
#                        DBI::dbRemoveTable(result$src$con, temp_name)
#                      }       
                    }
                    
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
                        conditionMessage(cond)
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
        
        # Ensure selection holds while deleting / adding rows
        observe(priority = 1,{
              req(!is.null(rv$modifiedData) && isTruthy(rv$modifiedData))
              req(!is.null(isolate(rv$selected)) && isTruthy(isolate(rv$selected)))

              currentSelection <- isolate(rv$selected)$i  
              newIndexes <- which(rv$modifiedData$i %in% currentSelection)
              if(length(newIndexes)){
                DT::selectRows(proxyDT, newIndexes)
              }
            })
        
        observe({
               rv$selected <- selected() # To force evaluation
            })
        
        selected <- reactive({
              rows <-input$DT_rows_selected
              data <- isolate(rv$modifiedData)[rows,]
              data
            })
        
        dataVars <- reactive({
              dplyr::tbl_vars(data())
            })
                
        return(list(
                result = result,
                state = reactive({castToTemplate(rv$modifiedData[,dataVars()], data())}),
                selected = reactive({castToTemplate(selected()[,dataVars()], data())})
                ))
      }
  )
}

#' Add some extra columns to data to allow for / keep track of modifications
#' @param data `data.frame`
#' @param ns namespace function
#' @param buttonCol `character(1)` name of column with buttons
#' @param statusCol `character(1)` name of column with general status (e.g. modified or not).
#' @param deleteCol `character(1)` name of the column with deletion status.
#' @param iCol `character(1)` name of column containing a unique identifier.
#' @return data with extra columns buttons, status, i.
#' @importFrom dplyr relocate all_of
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
  data <- relocate(data, all_of(buttonCol))
  data
}

#' Add modification buttons as a column
#' @param df `data.frame`
#' @param columnName `character(1)`
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
#' @param suffix `character(1)` sprintf placeholer for suffix
#' @param ns `character(1)` sprintf placeholder for ns
#' @importFrom shiny div actionButton icon
#' @return `character(1)` HTML to be filled in with \code{sprintf}
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

#' Create buttons to modify the row. See \code{\link{createButtonsHTML}}
#' @details buttons used per row in the app.
#' @param suffix `character(1)`
#' @param ns `character(1)` namespace
#' @return `character` HTML
createButtons <- function(suffix, ns){
  sprintf(
      # Can be generated with createButtonsHTML
      buttonsHTML,
      suffix,
      ns("")
  )
}

#' Function to generate CSS to disable clicking events on a column
#' @param id `character(1)` namespaced id of the datatable
#' @details <https://stackoverflow.com/questions/60406027/how-to-disable-double-click-reactivity-for-specific-columns-in-r-datatable>
#' @details <https://stackoverflow.com/questions/75406546/apply-css-styling-to-a-single-dt-datatable>
#' @return `character` CSS
disableDoubleClickButtonCss <- function(id){
  sprintf("
      #%1$s > .dataTables_wrapper > table tbody td:nth-child(1) {pointer-events: none;}
      #%1$s > .dataTables_wrapper > table tbody td:nth-child(1)>div {pointer-events: auto;}
      ",id)
} 

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
    "table.on('preAutoFill', function(e, datatable, cells){",
    "  var out = [];",
    "  for(var i = 0; i < cells.length; ++i){",
    "    var cells_i = cells[i];",
    "    for(var j = 0; j < cells_i.length; ++j){",
    "      var c = cells_i[j];",
    "      var value = (c.set === null || (typeof c.set === 'number' && isNaN(c.set)))? '' : c.set;", # null => problem in R
    "      out.push({",
    "        row: c.index.row + 1,",
    "        col: c.index.column,",
    "        value: value",
    "      });",
    "    }",
    "  }",
    "  Shiny.setInputValue(id + '_cells_filled:DT.cellInfo', out,  {priority: \"event\"});",
    "  table.rows().invalidate();", # this updates the column type
    "});"
)
