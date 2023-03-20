library(shiny)
library(shinydashboard)
library(DT)
library(tidyr)
library(dplyr)
library(shinycssloaders)
library(openai)

sparql_endpoint <- "https://ldf.fi/semparl/sparql"
ua <- httr::user_agent("https://github.com/tts/puhe")

sparql <- function(query, endpoint = sparql_endpoint, ...){
  enc_query <- gsub("\\+", "%2B", URLencode(query, reserved = TRUE))
  res <- httr::GET(
    paste(endpoint, "?query=", enc_query, sep = ""),
    httr::add_headers("Accept" = "application/sparql-results+json"),
    ua,
    ...
  )
  res
}

process_json <- function(res) {
  res <- jsonlite::parse_json(res, simplifyVector = TRUE)$results$bindings
}

options(spinner.type  = 7,
        spinner.size  = 0.5,
        spinner.color = "#ff6502")

ui <- function(request) {
  
  sidebar <- dashboardSidebar(
    width = 300,
    sidebarMenu(
      textInput(inputId = "search",
                label = "Hakusana, katkaisu=*",
                placeholder = "esim. olkiluo*"),
      actionButton("do", "Hae"),
      menuItem(HTML("<p>1. Palauttaa 5 uusinta puheenvuoroa</p>
                    <p>2. Valitse taulukosta jokin rivi</p>
                    <p>3. Siirry sivun alalaitaan</p>
                    <p>4. Hae valitun rivin tekstille avainsanat (OpenAI)</p>
                    <p>5. Tee avainsanoista kuva (OpenAI)</p>")
      ))
  )
  
  body <- dashboardBody(
    fluidRow(
      style = "font-size: 75%",
      column(width = 12,
             withSpinner(DTOutput("table")))
    ),
    fluidRow(),
    fluidRow(
      column(width = 3,
             uiOutput("dokws")),
      column(width = 9,
             withSpinner(verbatimTextOutput("kws"))) 
    ),
    fluidRow(),
    fluidRow(
      column(width = 3,
             uiOutput("dopic")),
      column(width = 9,
             withSpinner(uiOutput("pic")))
      )
  )
  
  dashboardPage(
    dashboardHeader(
      title = "Eduskunnan täysistuntojen puheenvuoroja 1907-2022", titleWidth = "800",
      dropdownMenu(type = "messages",
                   messageItem(from = "Lisätietoja:",
                               message = "Lähde",
                               href = "https://parlamenttisampo.fi/fi"),
                   messageItem(from = "Lisätietoja:",
                               message = "OpenAI",
                               href = "https://platform.openai.com/"),
                   messageItem(from = "Lisätietoja:", 
                               message = "Tämän sovelluksen koodi",
                               href = "https://github.com/tts/puhe"))),
    sidebar,
    body,
    skin = "black"
  )
  
}

server <- function(input, output, session) {
  
  # Escaping in the query needs 4 backslashes
  # https://github.com/eclipse/rdf4j/issues/1105#issuecomment-652204116
  
  result <- eventReactive(
    input$do, {
      q <- paste0('
  PREFIX dct: <http://purl.org/dc/terms/>
  PREFIX semparls: <http://ldf.fi/schema/semparl/>
  PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
  PREFIX text: <http://jena.apache.org/text#>
  PREFIX facets: <http://ldf.fi/schema/parliamentsampo-portal/>

  SELECT *
  WHERE {
    {
      SELECT DISTINCT ?id ?score ?literal {
        ?id text:query ( semparls:content ','"',input$search,'"',' 10000000  ) .
        VALUES ?facetClass { semparls:Speech }
        ?id a ?facetClass .
        OPTIONAL { ?id dct:date ?orderBy }
      }
      ORDER BY (!BOUND(?orderBy)) desc(?orderBy) # ttso: changed to desc
      LIMIT 5 OFFSET 0 # ttso: changed to 5
    }
    FILTER(BOUND(?id))
    # score and literal are used only for Jena full text index, but may slow down the query performance
    ( ?id ?score ?literal ) text:query ( semparls:content ','"',input$search,'"',' 10000000 "highlight:s:<b> | e:</b> | z:150" ) .


  ?id skos:prefLabel ?prefLabel__id .
  BIND(?prefLabel__id as ?prefLabel__prefLabel)
  BIND(CONCAT("/speeches/page/", REPLACE(STR(?id), "^.*\\\\/(.+)", "$1")) AS ?prefLabel__dataProviderUrl)
  BIND(?id as ?uri__id)
  BIND(?id as ?uri__dataProviderUrl)
  BIND(?id as ?uri__prefLabel)
  {
    ?id semparls:speaker ?speaker__id .
    ?speaker__id skos:prefLabel ?speaker__prefLabel .
    BIND(CONCAT("/people/page/", REPLACE(STR(?speaker__id), "^.*\\\\/(.+)", "$1")) AS ?speaker__dataProviderUrl)
  }
  UNION
  {
    ?id semparls:party ?party__id .
    ?party__id skos:prefLabel ?party__prefLabel .
    FILTER(LANG(?party__prefLabel) = "fi")
    BIND(CONCAT("/groups/page/", REPLACE(STR(?party__id), "^.*\\\\/(.+)", "$1")) AS ?party__dataProviderUrl)
  }
  UNION
  {
    ?id semparls:speechType ?speechType__id .
    ?speechType__id skos:prefLabel ?speechType__prefLabel .
  }
  UNION
  {
    ?id dct:language ?language_ .
    BIND(REPLACE(STR(?language_), "http://id.loc.gov/vocabulary/iso639-2/", "") as ?language)
  }
  UNION
  {
    ?id dct:date ?date_ .
    BIND(CONCAT(STR(DAY(?date_)),
                     ".",
                     STR(MONTH(?date_)),
                     ".",
                    STR(YEAR(?date_))) as ?date)
  }
  UNION
  {
    ?id semparls:item ?item__id .
    ?item__id skos:prefLabel ?item__prefLabel .
    BIND(CONCAT("/items/page/", REPLACE(STR(?item__id), "^.*\\\\/(.+)", "$1")) AS ?item__dataProviderUrl) .
  }
  UNION
  {
    ?id semparls:content ?content .
  }

  }')
      res <- process_json(sparql(q))
      
      validate(need(length(res)>0, message = "Ei löytynyt mitään!"))
      
      res_df <- do.call(data.frame, res) %>% 
        select(id.value, content.value)
      
      s <- sub("\\*", "", input$search)

      df_cleaned <- res_df %>% 
        group_by(id.value, .drop = FALSE) %>% 
        fill(content.value, .direction = "downup") %>% 
        ungroup() %>% 
        rename(id = id.value,
               puhe = content.value) %>% 
        mutate(id = sub("http://ldf.fi/semparl/speeches/", "https://parlamenttisampo.fi/speeches/page/", id),
               id = paste0('<a href="',id,'" target="_blank">', id, '</a>'),
               puhe = gsub(paste0("(.*?)(",s,")(.*?)"), "\\1<b>\\2</b>\\3", puhe, ignore.case = T))

      df_distinct <- distinct(df_cleaned, id, .keep_all = TRUE)
    })
  
  output$table <- renderDT(
    datatable(result(), 
              escape = c(TRUE, FALSE),
              selection = "single")
  )
  
  # Construct the prompt: ignore the opening sentence and possible remarks in square brackets, 
  # and take a substring of 1000 chars
  speech <- eventReactive(
    input$table_rows_selected, {
      r <- input$table_rows_selected
      if (length(r)) { t <- toString(result()[r, "puhe"]) }
      t_clean <- gsub("^[^!]*! ", "", t)
      t_clean2 <- gsub("\\[.*?\\]", "", t_clean)
      t1000 <- substr(t_clean2, 1, 1000) 
    })
  
  # When a row is clicked, render an action button 'dokws' for starting the keyword making process
  observeEvent(input$table_rows_selected, {
    output$dokws <- renderUI({
      actionButton("dokws", label = "Hae avainsanat", icon = icon("key"))
    })
  })
  
  # When the action button 'dokws' is clicked, create the keywords...
  kws <- eventReactive(
    input$dokws, {
      kwords <- create_completion(
        model = "text-davinci-003",
        max_tokens = 30,
        prompt = paste0("Extract keywords from this text: ", speech()),
        openai_api_key = "[your key]"
      )
    })
  
  # ... and print them
  output$kws <- renderPrint({ 
    kws()$choices$text
  })
  
  # When we have keywords, render an action button 'dopic' for starting the pic making process
  observeEvent(kws(), {
    output$dopic <- renderUI({
      actionButton("dopic", label = "Tee kuva", icon = icon("image"))
    })
  })
  
  # When the action button 'dopic' is clicked, create the pic...
  picresult <- eventReactive(
    input$dopic, {
      res <- create_image(
        prompt = kws()$choices$text,
        size = "512x512",
        openai_api_key = "[your key]"
      )
      tags$img(src = res$data$url)       
    }
  )
  
  # ... and render it
  output$pic <- renderUI({
    picresult()
  })
  
}


shinyApp(ui = ui, server = server)

