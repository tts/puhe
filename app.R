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
        spinner.color = "orange")

ui <- function(request) {
  
  sidebar <- dashboardSidebar(
    tags$head(
      tags$style(HTML('#do{background-color:orange}')),
      tags$style(HTML('#info{margin-left:20px}'))
    ),
    width = 300,
    sidebarMenu(
      textInput(inputId = "search",
                label = "Syötä hakusana",
                placeholder = "esim. olkiluo*"),
      actionButton("do", "Hae", width = "100px"),
      hr(),
      HTML("<p id='info'>Haku palauttaa viisi uusinta puheenvuoroa<br/>
      1. Valitse jokin rivi</br>
      2. Siirry sivulle <b>Kuva</b></br>
      3. Hae tekstille asiasanoja</br>
      4. Muokkaa niistä virike (prompt)</br>
      5. Tee virikkeestä kuva</br></p>"),
      hr(),
      menuItem("Puheet", tabName = "main"),
      menuItem("Kuva", tabName = "kuva")
    ))
  
  body <- dashboardBody(
    tabItems(
      tabItem(tabName = "main",
              fluidRow(
                style = "font-size: 80%",
                column(width = 12,
                       withSpinner(DTOutput("table")))
              )
      ),
      tabItem(tabName = "kuva",
              fluidRow(
                column(width = 12,
                       box(title = "Valitun puheen alku", width = "100%", height = "230px",
                           textOutput("speech")))
                ),
              fluidRow(
                column(width = 4,
                       actionButton("do_kws", "Hae asiasanoja", width = "100%")),
                column(width = 8,
                       withSpinner(uiOutput("kws")))
              ),
              fluidRow(
                column(width = 4,
                       actionButton("do_pic", "Tee kuva", width = "100%")),
                column(width = 8,
                       withSpinner(uiOutput("pic")))
              )
      )
    )
  )
  
  dashboardPage(
    dashboardHeader(
      title = "Eduskuntapuheenvuorosta kuva (OpenAI)", titleWidth = "800",
      dropdownMenu(type = "messages",
                   messageItem(from = "Linkki",
                               message = "Puheiden lähde",
                               href = "https://parlamenttisampo.fi/fi"),
                   messageItem(from = "Linkki",
                               message = "OpenAI",
                               href = "https://platform.openai.com/"),
                   messageItem(from = "Linkki", 
                               message = "Tämän sovelluksen koodi",
                               href = "https://github.com/tts/puhe"))),
    sidebar,
    body,
    skin = "black"
  )
  
}

server <- function(input, output, session) {
  
  # The SPARQL query is copied from the Yasqui editor linked from 
  # https://parlamenttisampo.fi/fi/speeches/faceted-search/table?page=0
  # Changes to the original code are commented with "ttso:" below.
  #
  # Note that escaping in the query needs 4 backslashes
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
      ORDER BY (!BOUND(?orderBy)) desc(?orderBy) # ttso: changed asc to desc
      LIMIT 5 OFFSET 0 # ttso: changed 10 to 5
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
      
      validate(need(length(res)>0, message = "Ei löytynyt yhtään puhetta. Kokeile muuta hakusanaa tai yritä katkaisumerkkiä (*)"))
      
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
  
  # Start the prompt building process when a row is clicked. First, edit the speech:
  # ignore the first greeting sentence and possible remarks in square brackets, 
  # remove newlines, trim, and take a substring of 1000 chars (OpenAI API max)
  speech <- eventReactive(
    input$table_rows_selected, {
      r <- input$table_rows_selected
      if (length(r)) { rs <- toString(result()[r, "puhe"]) }
      s <- gsub("</?b>", "", rs)
      s_nopre <- gsub("^[^!]*! ", "", s)
      s_nobrack <- gsub("\\[.*?\\]", "", s_nopre)
      s_1000raw <- substr(s_nobrack, 1, 1000) 
      s_1000raw_norn <- gsub("[\r\n]", "", s_1000raw)
      s_1000 <- trimws(s_1000raw_norn, which = "both")
      })
  
  output$speech <- renderText(speech())
  
  # FIXME: when a new row from the table is selected and the speech() is changed,
  # clear or remove output$kws and output$pic
  
  # Then, when 'dokws' is clicked, create the keywords from the 1000 char string
  keywords <- eventReactive(
    input$do_kws, {
      kwords <- create_completion(
        model = "text-davinci-003",
        max_tokens = 60,
        temperature = 0.5,
        top_p = 1,
        frequency_penalty = 0.8,
        presence_penalty = 0,
        prompt = paste0("Extract keywords from this text:", speech()),
        openai_api_key = "[your key]"
      )
    })
  
  # and render them for the user to check and edit
  output$kws <- renderUI({
    tabItem(tabName = "kuva",
            textAreaInput(inputId = "kws",
                          label = "Muokkaa näistä virike (prompt)",
                          value = trimws(keywords()$choices$text, which = "both"),
                          resize = "both", width = "80%", height = "100px"))
  })

  
  # When 'dopic' is clicked, create the pic from the prompt 
  picresult <- eventReactive(
    input$do_pic, {
      res <- create_image(
        prompt = input$kws,
        size = "512x512",
        openai_api_key = "[your key]"
      )
      tags$img(src = res$data$url)       
    }
  )
  
  # and render it
  output$pic <- renderUI({
    tabItem(tabName = "kuva",
            picresult())
  })
  
}


shinyApp(ui = ui, server = server)

