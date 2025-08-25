#' Server logic for the rapadm app
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#'
#' @return A shiny app server object
#' @export

app_server <- function(input, output, session) {
  rapbase::logShinyInputChanges(input)

  user <- rapbase::navbarWidgetServer2(
    "rapadm-widget",
    orgName = "RapAdm",
    caller = packageName()
  )

  # Environment
  output$user <- shiny::renderText({
    paste("rapbase::getUserName(session):",
          user$name())
  })
  output$group <- shiny::renderText({
    paste("rapbase::getUserGroups(session):",
          user$group())
  })
  output$resh_id <- shiny::renderText({
    paste("rapbase::getUserReshId(session):",
          user$org())
  })
  output$role <- shiny::renderText({
    paste("rapbase::getUserRole(session):",
          user$role())
  })
  output$email <- shiny::renderText({
    paste("rapbase::getUserEmail(session):",
          user$name())
  })
  output$full_name <- shiny::renderText({
    paste("rapbase::getUserFullName(session):",
          user$name())
  })
  output$phone <- shiny::renderText({
    paste("rapbase::getUserPhone(session):",
          user$phone())
  })
  output$instance <- shiny::renderText({
    Sys.getenv("R_RAP_INSTANCE")
  })
  output$config_path <- shiny::renderText({
    Sys.getenv("R_RAP_CONFIG_PATH")
  })
  output$sp_usergroups <- shiny::renderText({
    paste("Sys.getenv('SHINYPROXY_USERGROUPS'):",
          Sys.getenv("SHINYPROXY_USERGROUPS"))
  })
  output$locale <- shiny::renderText({
    Sys.getlocale()
  })


  # Configuration"
  output$rapbase_config <- shiny::renderText({
    f <- file.path(Sys.getenv("R_RAP_CONFIG_PATH"), "rapbaseConfig.yml")
    if (file.exists(f)) {
      yaml::as.yaml(yaml::read_yaml(f))
    } else {
      "Not found!"
    }
  })


  # Usestats
  log <- shiny::reactive({
    rapbase:::readLog(type = input$type, name = "") %>%
      rapbase::logFormat()
    # NULL
  })

  output$download <- shiny::downloadHandler(
    filename = function() {
      basename(
        tempfile(
          pattern = paste0(input$type, "_usestats_"),
          fileext = ".csv"
        )
      )
    },
    content = function(file) {
      if (input$downloadFormat == "xlsx-csv") {
        readr::write_excel_csv2(log(), file)
      } else {
        readr::write_csv2(log(), file)
      }
    }
  )


  output$usestats_data <- shiny::renderUI({
    shiny::req(log())
    if (input$viewtype == "table") {
      DT::renderDT(
        log()
      )
    } else {
      rpivotTable::rpivotTable(
        log(),
        rows = c("group"),
        cols = c("year", "month"),
        rendererName = "Heatmap"
      )
    }
  })

  # Autoreport

  # Toggle between raw and formatted table
  output$whichAutoReportTable <- shiny::renderUI({
    bslib::input_switch(
      "rawTable", "Raw table"
    )
  })

  # Static list of all autoreports, used to populate filter dropdowns
  staticAutoReport <- rapbase::readAutoReportData() %>%
    dplyr::select(-"runDayOfYear")

  # Reactive autoreport data, updated when an autoreport is deleted
  reactiveAutoReport <- shiny::reactive({
    rapbase::readAutoReportData() %>%
      dplyr::select(-"runDayOfYear")
  }) %>%
    shiny::bindEvent(input$del_button, ignoreNULL = FALSE, ignoreInit = FALSE)

  # Filtered autoreport data, updated when filter inputs change
  filteredAutoReport <- shiny::reactive({
    shiny::req(input$fpackage, input$ftype, input$fowner, input$forganization)
    message("Filtering autoreport data")
    far <- reactiveAutoReport()
    if (input$fpackage != "no filter") {
      far <- rapbase::filterAutoRep(
        far,
        "package",
        input$fpackage
      )
    }
    if (input$ftype != "no filter") {
      far <- rapbase::filterAutoRep(far, "type", input$ftype)
    }
    if (input$fowner != "no filter") {
      far <- rapbase::filterAutoRep(far, "owner", input$fowner)
    }
    if (input$forganization != "no filter") {
      far <- rapbase::filterAutoRep(
        far,
        "organization",
        input$forganization
      )
    }
    far
  })

  output$fpackage <- shiny::renderUI({
    shiny::selectInput(
      "fpackage",
      "- registry:",
      choices = c("no filter", unique_autoreport(staticAutoReport, "package"))
    )
  })
  output$ftype <- shiny::renderUI({
    shiny::selectInput(
      "ftype",
      "- type:",
      choices = c("no filter", unique_autoreport(staticAutoReport, "type"))
    )
  })
  output$fowner <- shiny::renderUI({
    shiny::selectInput(
      "fowner",
      "- owner:",
      choices = c("no filter", unique_autoreport(staticAutoReport, "owner"))
    )
  })
  output$forganization <- shiny::renderUI({
    shiny::selectInput(
      "forganization",
      "- organization:",
      choices = c(
        "no filter",
        unique_autoreport(staticAutoReport, "organization")
      )
    )
  })

  output$autoreport_data <- DT::renderDT({
    filteredAutoReport()
  })

  shiny::observeEvent(input$del_button, {
    repId <- strsplit(input$del_button, "__")[[1]][2]
    print(repId)
    rapbase::deleteAutoReport(repId)
  })

  output$autoReportTable <- shiny::renderUI({
    if (input$rawTable) {
      DT::DTOutput("autoreport_data")
    } else {
      DT::renderDataTable(
        autoReportTab(),
        server = FALSE, escape = FALSE, selection = "none",
        rownames = FALSE,
        options = list(
          pageLength = 25
        )
      )
    }
  })

  autoReportTab <- shiny::reactive({
    autoRep <- filteredAutoReport()
    message("Rendering autoreport table")
    print(dim(autoRep))
    message(names(autoRep))

    dateFormat <- "%A %e. %B %Y"

    if (length(autoRep$id) == 0) {
      return(as.matrix(autoRep))
    }

    l <- list()
    for (i in seq_len(nrow(autoRep))) {
      nextDate <- rapbase::findNextRunDate(
        runDayOfYear = NULL,
        startDate = autoRep[i, ]$startDate,
        terminateDate = autoRep[i, ]$terminateDate,
        interval = autoRep[i, ]$interval,
        returnFormat = dateFormat
      )
      if (as.Date(nextDate, format = dateFormat) > autoRep[i, ]$terminateDate) {
        nextDate <- "Utl\u00F8pt"
      }
      r <- list(
        "id" = autoRep[i, ]$id,
        "Ansvarlig" = autoRep[i, ]$ownerName,
        "Rapport" = autoRep[i, ]$synopsis,
        "Datakilde" = autoRep[i, ]$organization,
        "Mottaker" = autoRep[i, ]$email,
        "Periode" = autoRep[i, ]$intervalName,
        "Slutt" = strftime(
          as.Date(
            autoRep[i, ]$terminateDate
          ),
          format = dateFormat
        ),
        "Neste" = nextDate,
        "Slett" = as.character(
          shiny::actionButton(
            inputId = paste0("del__", autoRep[i, ]$id),
            label = "",
            icon = shiny::icon("trash"),
            onclick = sprintf(
              "Shiny.onInputChange('%s', this.id)",
              "del_button"
            )
          )
        )
      )
      l <- rbind(l, r)
    }
    return(as.matrix(l))
  })

  output$download_autoreport_data <- shiny::downloadHandler(
    filename = function() {
      paste0("autoreport-", Sys.Date(), ".csv")
    },
    content = function(con) {
      write.csv2(far(), con, row.names = FALSE, na = "")
    }
  )
}
