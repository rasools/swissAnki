library(shiny)
library(reticulate)
library(spacyr)
library(DT)
library(httr)
library(jsonlite)
library(base64enc)
library(magick)
library(shinythemes)
library(shinyjs)

# Setting up Python environment
Sys.setenv(RETICULATE_PYTHON = "/Users/rasools/miniconda3/envs/svenski4serve/bin/python")
spacy_initialize(model = "de_core_news_md")

server <- function(input, output, session) {
  values <- reactiveValues(
    apiKey = if (file.exists("/Users/rasools/Desktop/svenski_api.txt")) {
      readLines("/Users/rasools/Desktop/svenski_api.txt", warn = FALSE)[1]
    } else {
      NULL
    },
    results = NULL,
    image = NULL,
    audio = list(),
    pic_url = NULL,
    gptModel = "gpt-4o",
    dalleModel = "dall-e-3",
    ttsModel = "tts-1",
    deckName = "Nya ord",
    gptTokens = 0,
    dalleRequests = 0,
    ttsRequests = 0,
    gptCost = 0,
    dalleCost = 0,
    ttsCost = 0,
    appVersion = "germanAnki.v.1.0"
  )
  
  # Function to query ChatGPT API
  chatGPT <- function(prompt, modelName, temperature = 1, apiKey) {
    tryCatch({
      response <- POST(
        url = "https://api.openai.com/v1/chat/completions",
        add_headers(Authorization = paste("Bearer", apiKey)),
        content_type_json(),
        encode = "json",
        body = list(
          model = modelName,
          temperature = temperature,
          messages = list(list(role = "user", content = prompt))
        )
      )
      stop_for_status(response)
      result <- content(response)
      values$gptTokens <- values$gptTokens + result$usage$total_tokens
      input_tokens <- result$usage$prompt_tokens
      output_tokens <- result$usage$completion_tokens
      # Updated pricing for ChatGPT
      values$gptCost <- values$gptCost + (input_tokens / 1000) * 0.0005 + (output_tokens / 1000) * 0.0015
      result$choices[[1]]$message$content
    }, error = function(e) {
      print(paste("Error in chatGPT:", e$message))
      showNotification("Failed to get response from ChatGPT. Please check the prompt and try again.", type = "error")
      NULL
    })
  }
  
  
  # Function to query DALL-E API
  dallE <- function(prompt, apiKey) {
    response <- POST(
      url = "https://api.openai.com/v1/images/generations",
      add_headers(Authorization = paste("Bearer", apiKey)),
      content_type_json(),
      encode = "json",
      body = list(
        model = values$dalleModel,
        prompt = prompt,
        n = 1,
        size = "1024x1024"
      )
    )
    if (http_status(response)$category != "Success") {
      print(content(response, "text"))
      stop_for_status(response)
    }
    result <- content(response)
    values$dalleRequests <- values$dalleRequests + 1
    # Updated pricing for DALL-E
    values$dalleCost <- values$dalleCost + 0.02
    result$data[[1]]$url
  }
  
  
  # Function to call OpenAI TTS API and save the response as an MP3 file
  openai_tts <- function(text, model, api_key) {
    url <- "https://api.openai.com/v1/audio/speech"
    
    # Define a list of allowed voices for shuffling
    allowed_voices <- c("nova", "shimmer", "onyx", "fable", "ash", "coral")
    selected_voice <- sample(allowed_voices, 1)  # Randomly select a voice
    print(paste(selected_voice))
    
    headers <- add_headers(
      `Authorization` = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    )
    
    body <- list(
      model = model,
      voice = selected_voice,
      input = text
    )
    
    response <- POST(url, headers, body = toJSON(body, auto_unbox = TRUE))
    
    if (status_code(response) != 200) {
      print(content(response, as = "text"))
      stop("Request failed: ", content(response, as = "text"))
    }
    
    values$ttsRequests <- values$ttsRequests + 1
    # Calculate duration of audio (in minutes) and updated pricing for Whisper TTS
    audio_duration_minutes <- nchar(text) / 180 # Assuming average speaking rate of 180 characters per minute
    values$ttsCost <- values$ttsCost + audio_duration_minutes * 0.006
    content(response, as = "raw")
  }
  
  
  observeEvent(input$submit, {
    req(input$text)
    if (is.null(values$apiKey) || values$apiKey == "") {
      showModal(modalDialog(
        title = "API Key Required",
        passwordInput("apiKeyInput", "Enter API Key:", value = ""),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("save", "Save"),
          tags$p("Your API key will not be saved by the app and will only be used to query the OpenAI API.", style = "font-size: 10px; text-align: left;")
        )
      ))
    } else {
      performAnalysis(input$text, values$gptModel, values$apiKey)
    }
  })
  
  observeEvent(input$save, {
    req(input$apiKeyInput)
    values$apiKey <- input$apiKeyInput
    removeModal()
    performAnalysis(input$text, values$gptModel, values$apiKey)
  })
  
  observeEvent(input$generateImage, {
    req(values$results)

    original_prompt <- values$results$Details[4]
    original_part_of_speech <- values$results$Details[3]
    
    # Ask ChatGPT to make a better DALL-E prompt
    chat_prompt <- paste0(
      "Erstelle eine kurze visuelle Beschreibung für die Bildgenerierung basierend auf diesem deutschen Satz oder Wort: ",
      original_prompt, " - welches ein deutsches ", original_part_of_speech,
      " ist. Konzentriere dich auf das Hauptnomen oder die Handlung und beschreibe kurz eine Szene ohne Übersetzung oder abstrakte Konzepte."
    )
    print(paste("Prompt for ChatGPT:", chat_prompt))
    
    enhanced_prompt <- chatGPT(chat_prompt, modelName = values$gptModel, apiKey = values$apiKey)
    
    # Fallback to original if needed
    if (is.null(enhanced_prompt) || nchar(enhanced_prompt) < 10) {
      showNotification("ChatGPT failed to create a prompt. Using original sentence instead.", type = "warning")
      enhanced_prompt <- original_prompt
    }
    
    print(paste("Prompt for DALL-E:", enhanced_prompt))
    
    image_url <- tryCatch({
      dallE(enhanced_prompt, values$apiKey)
    }, error = function(e) {
      showNotification("Failed to generate image. Please check the prompt.", type = "error")
      return(NULL)
    })
    
    if (!is.null(image_url)) {
      # Download and process the image
      temp_image_path <- tempfile(fileext = ".jpg")
      download.file(image_url, temp_image_path, mode = "wb")
      
      img <- image_read(temp_image_path)
      img <- image_scale(img, "512x512")
      img <- image_convert(img, format = "jpeg")
      image_write(img, path = temp_image_path, quality = 70)
      
      # Encode the image in base64
      image_data <- base64enc::base64encode(temp_image_path)
      image_filename <- basename(temp_image_path)
      
      values$image <- list(data = image_data, filename = image_filename)
    }
  })
  
  observeEvent(input$removePicture, {
    values$image <- NULL
    showNotification("Picture removed from card.", type = "message")
  })
  
  observeEvent(input$readExamples, {
  req(values$results)
  
  audio_files <- lapply(seq_len(nrow(values$results)), function(i) {
    if (grepl("example", values$results$Property[i], ignore.case = TRUE)) {
      text_to_read <- sub("\\s*\\(.*?\\)\\s*$", "", values$results$Details[i])
      print(paste("Text to read:", text_to_read))
      openai_tts(text_to_read, values$ttsModel, api_key = values$apiKey)
    } else {
      NULL
    }
  })
  
  values$audio <- audio_files
  
  values$results$Audio <- sapply(seq_len(length(audio_files)), function(i) {
    if (!is.null(audio_files[[i]])) {
      audio_content <- audio_files[[i]]
      audio_src <- base64enc::base64encode(audio_content)
      as.character(tags$div(
        tags$audio(src = paste0("data:audio/mp3;base64,", audio_src), type = "audio/mp3", controls = TRUE)
      ))
    } else {
      ""
    }
  })
  
  showNotification("Audio files for rows with 'example' have been generated.", type = "message")
})
  
output$resultsTable <- renderDataTable({
  req(values$results)
  
  df <- values$results
  
  # Add a Delete button for each row
  df$Delete <- sprintf(
    '<button class="btn btn-danger btn-sm delete-btn" data-row="%s">Delete</button>',
    seq_len(nrow(df))
  )
  
  datatable(
    df,
    escape = FALSE,
    extensions = 'Buttons',
    editable = list(
      target = 'cell',
      disable = list(columns = which(colnames(df) == "Delete"))
    ),
    options = list(
      dom = 't',
      paging = FALSE,
      info = FALSE,
      ordering = FALSE,
      columnDefs = list(
        list(targets = '_all', className = 'dt-center')
      )
    ),
    callback = JS("
    table.on('click', '.delete-btn', function() {
      var row = $(this).data('row');
      Shiny.setInputValue('delete_row', row, {priority: 'event'});
    });
  ")
  )
})

observeEvent(input$delete_row, {
  row_index <- as.numeric(input$delete_row)
  if (!is.null(row_index) && row_index >= 1 && row_index <= nrow(values$results)) {
    values$results <- values$results[-row_index, ]
  }
})
  
observeEvent(input$resultsTable_cell_edit, {
  info <- input$resultsTable_cell_edit
  str(info)  # Optional: view edit info in R console
  i <- info$row
  j <- info$col
  v <- info$value
  
  values$results[i, j] <<- isolate(coerceValue(v, values$results[i, j]))
})
  
performAnalysis <- function(txt, modelName, apiKey) {
  values$image <- NULL
  values$audio <- list()
  values$pic_url <- NULL
  consolidatedtxt <- entity_consolidate(spacy_parse(txt, lemma = FALSE, entity = TRUE, nounphrase = TRUE))
  
  # Split the input text into words
  words <- unlist(strsplit(txt, "\\s+"))
  
  # Logic for two-word inputs
  if (length(words) == 2) {
    pos_tags <- consolidatedtxt$pos[1:2]
    if ("VERB" %in% pos_tags) {
      pos <- "VERB"
    } else {
      pos <- "phrase"
    }
    prompt <- createPrompt(txt, pos, consolidatedtxt)
    
  } else if (length(words) > 2) {
    # Treat multi-word inputs as a phrase
    pos <- "phrase"
    prompt <- createPrompt(txt, pos, consolidatedtxt)
    
  } else {
    # Original logic for single-word inputs
    prompt <- createPrompt(consolidatedtxt$token[1], consolidatedtxt$pos[1], consolidatedtxt)
  }
  
  # Make API request
  response <- chatGPT(prompt, modelName = modelName, apiKey = apiKey)
  if (is.null(response)) {
    showNotification("Failed to get analysis results. Please try again.", type = "error")
    return()
  }
  values$results <- parseResults(response)
  
  # Additional checks for single-word inputs
  if (length(words) == 1) {
    res <- parseResults(response)
    pos_type <- consolidatedtxt$pos[1]
    
    if (pos_type == "VERB" || pos_type == "AUX" || pos_type == "NOUN" || pos_type == "ADJ" || pos_type == "PRON") {
      prompt <- createPrompt(res$Details[4], pos_type, consolidatedtxt)
      response <- chatGPT(prompt, modelName = modelName, apiKey = apiKey)
      if (is.null(response)) {
        showNotification("Failed to get analysis results. Please try again.", type = "error")
        return()
      }
      values$results <- parseResults(response)
    }
  }
  
  output$result <- NULL
  print(prompt)
}

  
  createPrompt <- function(word, pos, consolidatedtxt) {
    if (tolower(pos) == "aux") {
      pos <- "verb"
    }
    path_template <- sprintf("temp_tables/%s.txt", tolower(pos))
    
    if (file.exists(path_template)) {
      lines <- readLines(path_template)
      table_string <- paste(
        lines[which(grepl("Category | Details", lines)):which(lines == "End |")],
        collapse = "\n"
      )
      paste(
        word, sprintf("ist ein deutsches %s. Bitte fülle die Lücken im folgenden Tabellenformat aus:", tolower(pos)),
        table_string, 
        "Die Tabelle sollte strikt dem Format 'Kategorie | Details' folgen.",
        "Für Beispielsätze bitte vollständige Sätze angeben, die einfach und nützlich für die tägliche Kommunikation sind (Anfänger/Mittelstufe).",
        "Jeder Beispielsatz sollte mindestens 15 Wörter enthalten, und die Bedeutung auf Englisch sollte am Ende des Satzes in Klammern stehen.",
        "Format für Beispiele: 'Präsens Beispielsatz | Satz auf Deutsch (Bedeutung auf Englisch)'.",
        sep = " "
      )
    } else {
      paste("No template found for", pos)
    }
  }
  
parseResults <- function(response) {
  response <- trimws(response)
  lines <- strsplit(response, "\n")[[1]]
  lines <- lines[-1]
  lines <- lines[-length(lines)]
  if (lines[1] == "--- | ---") {
    lines <- lines[-1]
  }
  keys <- sub("\\|.*", "", lines)
  values <- sub(".*\\| ", "", lines)
  
  df <- data.frame(Property = keys, Details = values, stringsAsFactors = FALSE)
  
  # Find first "example" row (case insensitive)
  example_index <- which(grepl("example", df$Property, ignore.case = TRUE))[1]
  insert_at <- if (!is.na(example_index)) example_index else (nrow(df) + 1)
  
  # Insert "My Notes" row
  note_row <- data.frame(Property = "My Note", Details = "", stringsAsFactors = FALSE)
  df <- rbind(df[1:(insert_at - 1), ], note_row, df[insert_at:nrow(df), ])
  
  rownames(df) <- NULL
  return(df)
}
  
  
  output$image <- renderUI({
    req(values$image)
    tagList(
      tags$div(style = "margin-top: 20px;", 
               tags$img(src = paste0("data:image/jpeg;base64,", values$image$data), height = "512px", width = "512px")
      ),
      tags$div(style = "margin-top: 20px;",
               actionButton("removePicture", "Remove Picture")
      )
    )
  })
  
  observe({
    req(values$results)
    output$generateButtons <- renderUI({
      tagList(
        actionButton("readExamples", "Generate Audio", style = "margin-bottom: 10px; margin-top: 20px;"),
        
        div(class = "card", style = "margin-top: 20px; padding: 20px; box-shadow: 0px 0px 10px rgba(0, 0, 0, 0.1); border-radius: 10px;",
            h3("Generate Image"),
            actionButton("generateImage", "Generate Picture"),
            uiOutput("image")
        ),
        
        tags$div(
          class = "card",
          style = "margin-top: 20px; padding: 15px; background-color: #ffffff; border-radius: 10px; box-shadow: 0 0 10px rgba(0,0,0,0.1);",
          h4("Kartenrichtung"),
          tags$p("Wähle, wie die Karte erstellt werden soll."),
                  selectInput("cardDirection", NULL,
                    choices = c("Deutsch zu Englisch", "Englisch zu Deutsch"),
                    selected = "Englisch zu Deutsch")
        ),
        
        actionButton("createAnkiCard", "Anki-Karte erstellen", class = "btn btn-primary btn-lg", style = "width: 100%; margin-top: 20px; font-weight: bold; font-size: 1.5em;")
      )
    })
  })
  
  observeEvent(input$createAnkiCard, {
    showModal(modalDialog(
      title = "Choose Anki Deck",
      selectInput("deckNameInput", "Choose Deck Name:", choices = values$deckNames, selected = values$deckName),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("saveDeckName", "Save and Create Card")
      )
    ))
  })
  
  observeEvent(input$saveDeckName, {
    values$deckName <- input$deckNameInput
    removeModal()
    createAnkiCard(input$text, values$results, values$image, values$deckName)
  })
  
  createAnkiCard <- function(word, results, image, deckName) {
    direction <- input$cardDirection
    # Extract part of speech
    property_clean <- trimws(tolower(results$Property))
    pos_row <- which(property_clean == "part of speech")
    
    pos_tag <- if (length(pos_row) > 0) {
      trimws(results$Details[pos_row])
    } else {
      "unknown"
    }
    
    # Check if the audio column exists and contains data
    has_audio <- "Audio" %in% names(results) && any(results$Audio != "")
    
    if (input$cardDirection == "Deutsch zu Englisch") {
      front_content <- results$Details[4]  # German example sentence
    } else {
      front_content <- results$Details[1]  # English meaning
    }
    
    if (input$cardDirection == "Deutsch zu Englisch") {
      back_content <- paste0(
        if (!is.null(image)) {
          sprintf("<img src='data:image/jpeg;base64,%s' /><br>", image$data)
        } else {
          ""
        },
        paste(
          apply(results, 1, function(row) {
            row_content <- paste(
              sprintf("<span style='color: lightblue; font-style: italic;'>%s</span>", row[1]),
              row[2],
              sep = "<br>"
            )
            if (length(row) >= 3 && !is.na(row[3]) && nchar(row[3]) > 0) {
              row_content <- paste(row_content, "<br>", row[3], sep = "<br>")
            }
            row_content
          }),
          collapse = "<br><br>"
        )
      )
    } else {
      # Englisch zu Deutsch — exclude the English meaning line (row 2)
      back_content <- paste0(
        if (!is.null(image)) {
          sprintf("<img src='data:image/jpeg;base64,%s' /><br>", image$data)
        } else {
          ""
        },
        paste(
          apply(results[-2, ], 1, function(row) {
            row_content <- paste(
              sprintf("<span style='color: lightblue; font-style: italic;'>%s</span>", row[1]),
              row[2],
              sep = "<br>"
            )
            if (length(row) >= 3 && !is.na(row[3]) && nchar(row[3]) > 0) {
              row_content <- paste(row_content, "<br>", row[3], sep = "<br>")
            }
            row_content
          }),
          collapse = "<br><br>"
        )
      )
    }
    
    
    fields <- list(
      Front = front_content,
      Back = back_content
    )
    
    direction_tag <- if (direction == "Deutsch zu Englisch") {
      "de2en"
    } else {
      "en2de"
    }
    
    tags <- list(
      pos_tag,
      values$appVersion,
      values$gptModel,
      direction_tag
    )
    
    # include the DALL-E tag if an image was generated
    if (!is.null(image)) {
      tags <- c(tags, values$dalleModel)
    }
    
    # include TTS tag if audio was generated for any row
    if ("Audio" %in% names(results) && any(results$Audio != "")) {
      tags <- c(tags, values$ttsModel)
    }
    
    note <- list(
      deckName = deckName,
      modelName = "Basic",
      fields = fields,
      tags = tags
    )
    
    response <- httr::POST(
      url = "http://localhost:8765",
      body = toJSON(list(action = "addNote", version = 6, params = list(note = note)), auto_unbox = TRUE),
      encode = "json"
    )
    
    result <- content(response)
    if (is.null(result$error)) {
      showNotification("Anki card created successfully.", type = "message")
    } else {
      showNotification(paste("Failed to create Anki card:", result$error), type = "error")
    }
  }
  
  
  # Get the Anki server URL from environment variables or use default for local development
  anki_server_url <- Sys.getenv("ANKI_SERVER_URL", unset = "http://localhost:8765")
  
  # Function to send a request to the Anki server
  send_anki_request <- function(url, body) {
    response <- httr::POST(
      url = url,
      body = toJSON(body, auto_unbox = TRUE),
      encode = "json"
    )
    return(content(response))
  }
  
  # Add a new note to Anki
  observeEvent(input$addNote, {
    note <- list(
      deckName = input$deckName,
      modelName = "Basic",
      fields = list(
        Front = input$front,
        Back = input$back
      ),
      tags = list("auto")
    )
    
    result <- send_anki_request(
      url = anki_server_url,
      body = list(action = "addNote", version = 6, params = list(note = note))
    )
    
    if (is.null(result$error)) {
      showNotification("Anki card created successfully.", type = "message")
    } else {
      showNotification(paste("Failed to create Anki card:", result$error), type = "error")
    }
  })
  
  # Retrieve the list of Anki decks
  observe({
    result <- send_anki_request(
      url = anki_server_url,
      body = list(action = "deckNames", version = 6)
    )
    
    if (is.null(result$error)) {
      values$deckNames <- result$result
    } else {
      showNotification(paste("Failed to retrieve Anki decks:", result$error), type = "error")
    }
  })
  
  
  observeEvent(input$saveSettings, {
    values$apiKey <- input$apiKeyInput
    values$gptModel <- input$gptModelInput
    values$dalleModel <- input$dalleModelInput
    values$ttsModel <- input$ttsModelInput
    removeModal()
  })
  
  # Translate API call
  translate <- function(text, source, target) {
    url <- "https://api.mymemory.translated.net/get"
    response <- GET(url, query = list(q = text, langpair = paste(source, "|", target, sep = "")))
    stop_for_status(response)
    content(response, "parsed")$responseData$translatedText
  }
  
  observeEvent(input$translate, {
    req(input$textToTranslate, input$sourceLang, input$targetLang)
    translation <- translate(input$textToTranslate, input$sourceLang, input$targetLang)
    output$translationResult <- renderText({ translation })
  })
  
  output$usageTable <- renderTable({
    total_cost <- values$gptCost + values$dalleCost + values$ttsCost
    data.frame(
      Service = c("ChatGPT", "DALL-E", "TTS", "Total"),
      Tokens_Requests = c(values$gptTokens, values$dalleRequests, values$ttsRequests, values$gptTokens + values$dalleRequests + values$ttsRequests),
      Cost = c(values$gptCost, values$dalleCost, values$ttsCost, total_cost)
    )
  })
  
  # Visualization for usage and cost
  output$usagePlot <- renderPlot({
    total_cost <- values$gptCost + values$dalleCost + values$ttsCost
    costs <- c(values$gptCost, values$dalleCost, values$ttsCost, total_cost)
    barplot(
      height = costs,
      names.arg = c("ChatGPT", "DALL-E", "TTS", "Total"),
      col = c("darkblue", "darkgreen", "darkred", "black"),
      main = "Usage Cost",
      ylab = "Cost (USD)"
    )
  })
  
}

ui <- fluidPage(
  theme = shinytheme("flatly"),
  useShinyjs(),
  tags$head(tags$style(HTML("
    .shiny-input-container {
      margin-bottom: 15px;
    }
    .btn {
      margin-right: 10px;
    }
    .dataTables_wrapper .dataTables_paginate .paginate_button {
      padding: 0;
    }
    .main-panel {
      background-color: #f8f9fa;
      padding: 20px;
      border-radius: 10px;
      box-shadow: 0px 0px 10px rgba(0, 0, 0, 0.1);
    }
    .sidebar-panel {
      background-color: #343a40;
      color: #fff;
      padding: 20px;
      border-radius: 10px;
      box-shadow: 0px 0px 10px rgba(0, 0, 0, 0.1);
    }
    .card {
      background-color: #ffffff;
      border-radius: 10px;
      box-shadow: 0px 0px 10px rgba(0, 0, 0, 0.1);
      padding: 20px;
      margin-top: 20px;
    }
  "))),
  titlePanel("GermanAnki: Dein Anki-Assistent für Deutsch!"),
  sidebarLayout(
    sidebarPanel(
      class = "sidebar-panel",
      textInput("textToTranslate", "Zuerst Übersetzung prüfen (optional):", value = ""),
      selectInput("sourceLang", "Ausgangssprache:", choices = c("de", "en"), selected = "de"),
      selectInput("targetLang", "Zielsprache:", choices = c("de", "en"), selected = "en"),
      actionButton("translate", "Übersetzen"),
      textOutput("translationResult"),
      tags$hr(),
      textInput("text", "Deutsches Wort / Phrase eingeben:", value = ""),
      actionButton("submit", "Analysieren"),
      tags$div(
        style = "text-align: right; margin-top: 15px;",
        tags$a(
          icon("cog", class = "fa-2x", style = "color: white;"),
          href = "#",
          id = "settings"
        )
      )
    ),
    mainPanel(
      class = "main-panel",
      tabsetPanel(
        tabPanel("Results",
                 DTOutput("resultsTable"),
                 uiOutput("generateButtons")
        ),
        tabPanel("Usage and Cost",
                 plotOutput("usagePlot"),
                 tableOutput("usageTable")
        )
      )
    )
  ),
  tags$script(HTML("
    $(document).on('click', '#settings', function() {
      Shiny.setInputValue('settings', Math.random());
    });
  "))
)

shinyApp(ui = ui, server = server)
