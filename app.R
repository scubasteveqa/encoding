library(shiny)
library(bslib)
library(jsonlite)  # Add jsonlite to read the manifest directly

ui <- page_sidebar(
  title = "Locale & Encoding Test",
  sidebar = sidebar(
    h4("About"),
    p("This app tests if the locale and encoding settings in manifest.json are correctly applied."),
    p("Expected locale: it_IT"),
    p("Expected encoding: ISO-8859-1"),
    hr(),
    actionButton("trySetLocale", "Try Setting Locale Directly"),
    hr(),
    downloadButton("downloadReport", "Download Results")
  ),
  
  card(
    card_header("Manifest.json Analysis"),
    card_body(
      h4("Manifest File Contents:"),
      verbatimTextOutput("manifestInfo")
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
    card_header("Environment Variables"),
    card_body(
      verbatimTextOutput("envVars")
    )
  )
)

server <- function(input, output, session) {
  
  # Read manifest.json if it exists
  manifest_data <- reactive({
    tryCatch({
      if(file.exists("manifest.json")) {
        fromJSON("manifest.json")
      } else {
        NULL
      }
    }, error = function(e) {
      NULL
    })
  })
  
  # Get manifest values
  manifest_locale <- reactive({
    manifest <- manifest_data()
    if(!is.null(manifest) && !is.null(manifest$locale)) manifest$locale else "it_IT"
  })
  
  manifest_encoding <- reactive({
    manifest <- manifest_data()
    if(!is.null(manifest) && !is.null(manifest$encoding)) manifest$encoding else "ISO-8859-1"
  })
  
  # Display manifest information
  output$manifestInfo <- renderPrint({
    manifest <- manifest_data()
    if(is.null(manifest)) {
      cat("Manifest.json not found or couldn't be read.\n")
      cat("Using default expected values instead.\n")
    } else {
      cat("Manifest.json contents:\n")
      cat("Version:", manifest$version, "\n")
      cat("Locale setting:", manifest$locale, "\n")
      cat("Encoding setting:", manifest$encoding, "\n")
      
      cat("\nRuntime information:\n")
      if(!is.null(manifest$runtime)) {
        for(name in names(manifest$runtime)) {
          cat("  ", name, ": ", manifest$runtime[[name]], "\n", sep="")
        }
      } else {
        cat("  No runtime information found\n")
      }
    }
  })
  
  # Get current locale
  current_locale <- reactive({
    # This will be re-evaluated if locale is changed through button click
    Sys.getlocale()
  })
  
  # Try to set locale directly when button is clicked
  observeEvent(input$trySetLocale, {
    locale_string <- paste0(manifest_locale(), ".", manifest_encoding())
    
    result <- tryCatch({
      new_locale <- Sys.setlocale(locale = locale_string)
      list(success = TRUE, message = paste("Successfully set locale to:", new_locale))
    }, error = function(e) {
      list(success = FALSE, message = paste("Error setting locale:", e$message))
    }, warning = function(w) {
      list(success = FALSE, message = paste("Warning setting locale:", w$message))
    })
    
    # Show result notification
    if(result$success) {
      showNotification(result$message, type = "message")
    } else {
      showNotification(result$message, type = "error")
    }
  })
  
  # Display locale information
  output$localeInfo <- renderPrint({
    locale <- current_locale()
    
    cat("Full locale string:\n", locale, "\n\n")
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
    locale <- current_locale()
    expected_locale <- manifest_locale()
    expected_encoding <- manifest_encoding()
    
    # Test if locale matches expected value
    locale_test <- grepl(expected_locale, locale, fixed = TRUE)
    
    # Test if encoding is as expected
    encoding_test <- grepl(expected_encoding, locale, fixed = TRUE)
    
    cat("Expected locale from manifest: ", expected_locale, "\n")
    cat("Expected encoding from manifest: ", expected_encoding, "\n\n")
    
    cat("Locale test (", expected_locale, "): ", 
        if(locale_test) "PASSED" else "FAILED", "\n", sep = "")
    cat("Encoding test (", expected_encoding, "): ", 
        if(encoding_test) "PASSED" else "FAILED", "\n", sep = "")
    
    if (!locale_test || !encoding_test) {
      cat("\nNOTE: If tests failed, it could be because:\n")
      cat("1. The specified locale isn't available on this system\n")
      cat("2. The manifest.json settings weren't applied correctly\n")
      cat("3. The locale was overridden by environment variables\n")
      
      cat("\nDebug information:\n")
      cat("  Current locale: ", locale, "\n")
      cat("  Expected locale pattern: ", expected_locale, "\n")
      cat("  Expected encoding pattern: ", expected_encoding, "\n")
      cat("  Try clicking the 'Try Setting Locale Directly' button to attempt manual setting\n")
    }
  })
  
  # Display environment variables
  output$envVars <- renderPrint({
    cat("R Environment Variables related to locale:\n\n")
    
    # List common locale-related environment variables
    loc_vars <- c("LC_ALL", "LC_COLLATE", "LC_CTYPE", "LC_MONETARY", 
                  "LC_NUMERIC", "LC_TIME", "LC_MESSAGES", "LANG", 
                  "LANGUAGE", "R_LOCALE", "R_ENCODING")
    
    for(var in loc_vars) {
      val <- Sys.getenv(var, NA)
      if(!is.na(val) && val != "") {
        cat(var, ": ", val, "\n", sep="")
      } else {
        cat(var, ": (not set)\n", sep="")
      }
    }
    
    # Also show R options that might affect locale
    cat("\nR Options related to locale:\n")
    cat("R_DEFAULT_LOCALE: ", getOption("R_DEFAULT_LOCALE", "(not set)"), "\n")
  })
  
  # Download report
  output$downloadReport <- downloadHandler(
    filename = function() {
      "locale_test_results.txt"
    },
    content = function(file) {
      locale <- current_locale()
      expected_locale <- manifest_locale()
      expected_encoding <- manifest_encoding()
      
      # Test if locale matches expected value
      locale_test <- grepl(expected_locale, locale, fixed = TRUE)
      
      # Test if encoding is as expected
      encoding_test <- grepl(expected_encoding, locale, fixed = TRUE)
      
      # Get manifest for report
      manifest <- manifest_data()
      manifest_section <- if(is.null(manifest)) {
        c("Manifest.json not found or couldn't be read.",
          "Using default expected values instead.")
      } else {
        c(
          "Manifest.json contents:",
          paste("Version:", manifest$version),
          paste("Locale setting:", manifest$locale),
          paste("Encoding setting:", manifest$encoding)
        )
      }
      
      # Generate report
      writeLines(
        c(
          "Locale Test Results",
          "===================",
          "",
          manifest_section,
          "",
          paste("Full locale string:", locale),
          "",
          paste("Expected locale: ", expected_locale),
          paste("Expected encoding: ", expected_encoding),
          "",
          paste("Locale test (", expected_locale, "): ", if(locale_test) "PASSED" else "FAILED"),
          paste("Encoding test (", expected_encoding, "): ", if(encoding_test) "PASSED" else "FAILED"),
          "",
          "Environment Variables:",
          paste("LC_ALL: ", Sys.getenv("LC_ALL", "(not set)")),
          paste("LANG: ", Sys.getenv("LANG", "(not set)")),
          paste("LANGUAGE: ", Sys.getenv("LANGUAGE", "(not set)")),
          paste("R_LOCALE: ", Sys.getenv("R_LOCALE", "(not set)")),
          paste("R_ENCODING: ", Sys.getenv("R_ENCODING", "(not set)")),
          "",
          "System Information:",
          paste("R Version:", R.version.string),
          paste("Platform:", R.version$platform)
        ),
        file
      )
    }
  )
}

shinyApp(ui, server)
