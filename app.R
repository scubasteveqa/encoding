library(shiny)
library(bslib)

ui <- page_sidebar(
  title = "Locale & Encoding Test",
  sidebar = sidebar(
    h4("About"),
    p("This app tests locale and encoding settings from various sources."),
    p("Primary test: it_IT / ISO-8859-1"),
    hr(),
    selectInput("setLocale", "Try setting locale to:", 
                choices = c("Don't change", "it_IT", "fr_FR", "de_DE", "es_ES"),
                selected = "Don't change"),
    selectInput("setEncoding", "Try setting encoding to:", 
                choices = c("Don't change", "ISO-8859-1", "UTF-8", "latin1"),
                selected = "Don't change"),
    actionButton("applySettings", "Apply Settings"),
    hr(),
    downloadButton("downloadReport", "Download Results")
  ),
  
  card(
    card_header("Configuration Information"),
    card_body(
      h4("Configuration Sources:"),
      verbatimTextOutput("configSources")
    )
  ),
  
  card(
    card_header("System Locale Information"),
    card_body(
      h4("Current Locale Settings:"),
      verbatimTextOutput("localeInfo"),
      hr(),
      h4("Locale Test Results:"),
      verbatimTextOutput("testResults")
    )
  ),
  
  card(
    card_header("Troubleshooting"),
    card_body(
      h4("Available Locales:"),
      p("First 20 locales available on this system:"),
      verbatimTextOutput("availableLocales")
    )
  )
)

server <- function(input, output, session) {
  
  # Try to read manifest.json if it exists
  manifest_locale <- NULL
  manifest_encoding <- NULL
  
  tryCatch({
    if(file.exists("manifest.json")) {
      manifest_content <- jsonlite::fromJSON("manifest.json")
      manifest_locale <- manifest_content$locale
      manifest_encoding <- manifest_content$encoding
    }
  }, error = function(e) {
    # Silently continue if manifest can't be read
  })
  
  # Configuration sources with priorities
  output$configSources <- renderPrint({
    cat("Environment Variables:\n")
    cat("  R_LOCALE=", Sys.getenv("R_LOCALE", "(not set)"), "\n")
    cat("  LOCALE=", Sys.getenv("LOCALE", "(not set)"), "\n")
    cat("  R_ENCODING=", Sys.getenv("R_ENCODING", "(not set)"), "\n")
    cat("  ENCODING=", Sys.getenv("ENCODING", "(not set)"), "\n\n")
    
    cat("Manifest File:\n")
    if(is.null(manifest_locale) && is.null(manifest_encoding)) {
      cat("  (No manifest.json found or couldn't be read)\n\n")
    } else {
      cat("  locale=", ifelse(is.null(manifest_locale), "(not set)", manifest_locale), "\n")
      cat("  encoding=", ifelse(is.null(manifest_encoding), "(not set)", manifest_encoding), "\n\n")
    }
    
    cat("R Session Info:\n")
    cat("  R version: ", R.version.string, "\n")
    cat("  Platform: ", R.version$platform, "\n\n")
  })
  
  # Reactive value to track locale changes
  locale_status <- reactiveValues(
    current_locale = Sys.getlocale(),
    attempted_change = FALSE,
    change_result = ""
  )
  
  # Function to get expected values with priority order:
  # 1. Environment variables
  # 2. Manifest file
  # 3. Default values
  get_expected_values <- reactive({
    expected_locale <- Sys.getenv("R_LOCALE", 
                     Sys.getenv("LOCALE", 
                              ifelse(is.null(manifest_locale), "it_IT", manifest_locale)))
    
    expected_encoding <- Sys.getenv("R_ENCODING", 
                       Sys.getenv("ENCODING", 
                                ifelse(is.null(manifest_encoding), "ISO-8859-1", manifest_encoding)))
    
    list(locale = expected_locale, encoding = expected_encoding)
  })
  
  # Apply locale settings when button is clicked
  observeEvent(input$applySettings, {
    if(input$setLocale != "Don't change" || input$setEncoding != "Don't change") {
      locale_status$attempted_change <- TRUE
      
      # Build locale string
      new_locale <- NA
      
      if(input$setLocale != "Don't change" && input$setEncoding != "Don't change") {
        # Both specified
        locale_string <- paste0(input$setLocale, ".", input$setEncoding)
      } else if(input$setLocale != "Don't change") {
        # Only locale specified
        locale_string <- input$setLocale
      } else {
        # Only encoding specified
        locale_string <- paste0("en_US.", input$setEncoding)
      }
      
      # Try to set the locale
      tryCatch({
        new_locale <- Sys.setlocale(locale = locale_string)
        locale_status$change_result <- paste("Successfully changed locale to:", new_locale)
      }, error = function(e) {
        locale_status$change_result <- paste("Failed to set locale:", e$message)
      }, warning = function(w) {
        locale_status$change_result <- paste("Warning when setting locale:", w$message)
      })
      
      # Update current locale
      locale_status$current_locale <- Sys.getlocale()
    }
  })
  
  # Display locale information
  output$localeInfo <- renderPrint({
    # Get latest locale info
    current_locale <- Sys.getlocale()
    locale_status$current_locale <- current_locale
    
    cat("Full locale string:\n", current_locale, "\n\n")
    
    if(locale_status$attempted_change) {
      cat("Locale change attempt result:\n", locale_status$change_result, "\n\n")
    }
    
    cat("Individual categories:\n")
    
    # Get all locale categories
    categories <- c("LC_COLLATE", "LC_CTYPE", "LC_MONETARY", 
                    "LC_NUMERIC", "LC_TIME", "LC_MESSAGES")
    
    for (cat_name in categories) {
      if (exists(cat_name)) {
        cat_val <- tryCatch({
          Sys.getlocale(category = get(cat_name))
        }, error = function(e) {
          "Not available"
        })
        cat(cat_name, ": ", cat_val, "\n", sep = "")
      }
    }
  })
  
  # Display test results
  output$testResults <- renderPrint({
    # Get current locale and expected values
    current_locale <- locale_status$current_locale
    expected <- get_expected_values()
    
    # Test if locale matches expected value
    locale_test <- grepl(expected$locale, current_locale, fixed = TRUE)
    
    # Test if encoding is as expected
    encoding_test <- grepl(expected$encoding, current_locale, fixed = TRUE)
    
    cat("Expected locale: ", expected$locale, "\n")
    cat("Expected encoding: ", expected$encoding, "\n\n")
    
    cat("Locale test (", expected$locale, "): ", 
        if(locale_test) "PASSED" else "FAILED", "\n", sep = "")
    cat("Encoding test (", expected$encoding, "): ", 
        if(encoding_test) "PASSED" else "FAILED", "\n", sep = "")
    
    if (!locale_test || !encoding_test) {
      cat("\nNOTE: If tests failed, it could be because:\n")
      cat("1. The specified locale isn't available on this system\n")
      cat("2. The locale/encoding settings weren't applied correctly\n")
      cat("3. The locale was overridden by environment variables\n")
      cat("\nTry using the controls in the sidebar to manually set locale/encoding\n")
    }
  })
  
  # List available locales
  output$availableLocales <- renderPrint({
    all_locales <- tryCatch({
      system("locale -a", intern = TRUE)
    }, error = function(e) {
      "Could not determine available locales"
    })
    
    if(is.character(all_locales) && length(all_locales) > 1) {
      # Only show first 20 to avoid overloading the display
      if(length(all_locales) > 20) {
        cat(paste(all_locales[1:20], collapse = "\n"))
        cat("\n... and ", length(all_locales) - 20, " more")
      } else {
        cat(paste(all_locales, collapse = "\n"))
      }
    } else {
      cat("Could not retrieve list of available locales on this system.\n")
      cat("On Windows, this information may not be easily accessible.\n")
      cat("On Linux, the 'locale -a' command typically shows available locales.")
    }
  })
  
  # Download report
  output$downloadReport <- downloadHandler(
    filename = function() {
      "locale_test_results.txt"
    },
    content = function(file) {
      # Get latest info
      current_locale <- Sys.getlocale()
      expected <- get_expected_values()
      
      # Test if locale matches expected value
      locale_test <- grepl(expected$locale, current_locale, fixed = TRUE)
      
      # Test if encoding is as expected
      encoding_test <- grepl(expected$encoding, current_locale, fixed = TRUE)
      
      writeLines(
        c(
          "Locale Test Results",
          "===================",
          "",
          paste("Full locale string:", current_locale),
          "",
          paste("Environment Variables:"),
          paste("  R_LOCALE=", Sys.getenv("R_LOCALE", "(not set)")),
          paste("  LOCALE=", Sys.getenv("LOCALE", "(not set)")),
          paste("  R_ENCODING=", Sys.getenv("R_ENCODING", "(not set)")),
          paste("  ENCODING=", Sys.getenv("ENCODING", "(not set)")),
          "",
          paste("Manifest Settings:"),
          paste("  locale=", ifelse(is.null(manifest_locale), "(not set)", manifest_locale)),
          paste("  encoding=", ifelse(is.null(manifest_encoding), "(not set)", manifest_encoding)),
          "",
          paste("Expected locale: ", expected$locale),
          paste("Expected encoding: ", expected$encoding),
          "",
          paste("Locale test (", expected$locale, "): ", if(locale_test) "PASSED" else "FAILED", sep=""),
          paste("Encoding test (", expected$encoding, "): ", if(encoding_test) "PASSED" else "FAILED", sep=""),
          "",
          "System Information:",
          paste("  R version:", R.version.string),
          paste("  Platform:", R.version$platform)
        ),
        file
      )
    }
  )
}

shinyApp(ui, server)
