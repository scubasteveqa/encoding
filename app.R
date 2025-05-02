library(shiny)
library(bslib)

ui <- page_sidebar(
  title = "Locale & Encoding Test",
  sidebar = sidebar(
    h4("About"),
    p("This app tests if the locale and encoding settings in manifest.json are correctly applied."),
    p("Expected locale: it_IT"),
    p("Expected encoding: ISO-8859-1"),
    hr(),
    downloadButton("downloadReport", "Download Results")
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
  )
)

server <- function(input, output, session) {
  
  # Get current locale information
  current_locale <- Sys.getlocale()
  
  # Expected values based on manifest.json
  expected_locale <- "it_IT"
  expected_encoding <- "ISO-8859-1"
  
  # Test if locale matches expected value
  locale_test <- grepl(expected_locale, current_locale, fixed = TRUE)
  
  # Test if encoding is as expected
  encoding_test <- grepl(expected_encoding, current_locale, fixed = TRUE)
  
  # Display locale information
  output$localeInfo <- renderPrint({
    cat("Full locale string:\n", current_locale, "\n\n")
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
    cat("Locale test (it_IT): ", if(locale_test) "PASSED" else "FAILED", "\n", sep = "")
    cat("Encoding test (ISO-8859-1): ", if(encoding_test) "PASSED" else "FAILED", "\n", sep = "")
    
    if (!locale_test || !encoding_test) {
      cat("\nNOTE: If tests failed, it could be because:\n")
      cat("1. The specified locale isn't available on this system\n")
      cat("2. The manifest.json settings weren't applied correctly\n")
      cat("3. The locale was overridden by environment variables\n")
    }
  })
  
  # Download report
  output$downloadReport <- downloadHandler(
    filename = function() {
      "locale_test_results.txt"
    },
    content = function(file) {
      writeLines(
        c(
          "Locale Test Results",
          "===================",
          "",
          paste("Full locale string:", current_locale),
          "",
          paste("Expected locale: ", expected_locale),
          paste("Expected encoding: ", expected_encoding),
          "",
          paste("Locale test (it_IT): ", if(locale_test) "PASSED" else "FAILED"),
          paste("Encoding test (ISO-8859-1): ", if(encoding_test) "PASSED" else "FAILED")
        ),
        file
      )
    }
  )
}

shinyApp(ui, server)
