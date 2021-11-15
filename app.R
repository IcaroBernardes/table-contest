# 0. Loads packages and defines some general objects
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(readr)
library(reactable)
library(crosstalk)
library(gfonts)
library(shinyjs)
library(tippy)
library(rlang)
library(plotly)

## Defines nicknames for each variable
nickname <- list(
  `Name` = "name", ### Name of game
  `Year Published` = "year", ### Year of publication
  `Min Players` = "minplayers", ### Min players
  `Max Players` = "maxplayers", ### Max players
  `Play Time` = "time", ### Play time
  `Min Age` = "age", ### Min Age
  `Users Rated` = "votes", ### Users Rated
  `Rating Average` = "rating", ### Rating Average
  `BGG Rank` = "rank", ### BGG Rank
  `Complexity Average` = "complexity", ### Complexity Average
  `Owned Users` = "owners", ### Owned Users
  `Domains` = "domains" ### Domains
)

# 1. Crafts UI
ui <- fluidPage(
  
  ## Enables the use of shinyjs
  shinyjs::useShinyjs(),
  
  ## Enables the use of a google font
  gfonts::use_font("open-sans", "www/css/open-sans.css"),
  
  ## Establishes a link to the css style file
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/styles.css")
  ),
  
  ## Constructs the "skeleton" of the app
  ### Header
  titlePanel(uiOutput("app-header"), windowTitle = "BGG Explorer"),
  
  ### Control panel layout
  sidebarLayout(
    sidebarPanel(
      uiOutput("panel-header"),
      div(id = "panel",
          uiOutput("UIname"), 
          
          purrr::map(nickname[2:11], function(nick) {
            tagList(
              uiOutput(paste0("LABEL",nick)),
              uiOutput(paste0("UInum",nick)),
              shinyjs::hidden(uiOutput(paste0("UIcat",nick)))
            )
          }),
          
          uiOutput("UIdomains")
      )
    ),
    
    ### Main part
    tagList(
      #### List of games
      div(
        id = "table-container",
        uiOutput("table-header"),
        reactableOutput("table")
      ), 
      
      #### Compare games
      div(
        id = "versus-container",
        uiOutput("versus-header"),
        plotlyOutput("versus", height = "645px")
      )
    )
    
  )
)

# 2. Outlines server logic
server <- function(input, output, session) {
  
  ## Loads database
  original <- readr::read_rds("database.rds")
  
  ## Gets the pairing Name and ID for all games
  thumb <- original %>% 
    dplyr::select(ID, Name)
  
  ## Lists the type inputs
  list_type <- reactive({
    
    aux <- list(
      `Year Published` = input$type_year,
      `Min Players` = input$type_minplayers,
      `Max Players` = input$type_maxplayers,
      `Play Time` = input$type_time,
      `Min Age` = input$type_age,
      `Users Rated` = input$type_votes,
      `Rating Average` = input$type_rating,
      `BGG Rank` = input$type_rank,
      `Complexity Average` = input$type_complexity,
      `Owned Users` = input$type_owners
    )
    
    ### Converts NULL type inputs to FALSE (numerical variables as default)
    purrr::map(aux, ~ifelse(is.null(.), FALSE, .))
    
  })
  
  ## Lists the reset inputs
  list_reset <- reactive({
    
    aux <- list(
      `Year Published` = input$reset_year,
      `Min Players` = input$reset_minplayers,
      `Max Players` = input$reset_maxplayers,
      `Play Time` = input$reset_time,
      `Min Age` = input$reset_age,
      `Users Rated` = input$reset_votes,
      `Rating Average` = input$reset_rating,
      `BGG Rank` = input$reset_rank,
      `Complexity Average` = input$reset_complexity,
      `Owned Users` = input$reset_owners
    )
    
  })
  
  ## Gets the original data, select the chosen type and filters it
  df <- reactive({
    
    ### Gets the original data
    datum <- original
    
    ### Selects the chosen variables types
    #### Gets the list of type inputs
    typedef <- list_type()
    
    #### Adds the CAT prefix to the variable name of type inputs that are TRUE
    #### and makes these names as the contents of the list
    type <- purrr::imap(typedef, ~ifelse(., paste("CAT", .y), .y))
    selected <- unlist(type)
    names(selected) <- NULL
    
    #### Selects the chosen types of variables and renames them so
    #### all names are equal to the original
    datum <- datum %>%
      dplyr::select(ID, Name, all_of(selected), Domains) %>% 
      dplyr::rename(!!!type)
    
    ### Filters data according to the filter inputs
    #### Lists the type inputs (numerical and categorical)
    limits <- list(
      `Year Published` = list(num = input$NUMyear, cat = input$CATyear),
      `Min Players` = list(num = input$NUMminplayers, cat = input$CATminplayers),
      `Max Players` = list(num = input$NUMmaxplayers, cat = input$CATmaxplayers),
      `Play Time` = list(num = input$NUMtime, cat = input$CATtime),
      `Min Age` = list(num = input$NUMage, cat = input$CATage),
      `Users Rated` = list(num = input$NUMvotes, cat = input$CATvotes),
      `Rating Average` = list(num = input$NUMrating, cat = input$CATrating),
      `BGG Rank` = list(num = input$NUMrank, cat = input$CATrank),
      `Complexity Average` = list(num = input$NUMcomplexity, cat = input$CATcomplexity),
      `Owned Users` = list(num = input$NUMowners, cat = input$CATowners)
    )
    
    #### Verifies if the filter is active. Gets the ID's of games
    #### according to the type of filter
    limits <- purrr::pmap(list(limits, names(limits), typedef), function(lim, var, nature) {
      
      ##### Non-initialized inputs do not filter the data
      if (is.null(lim[["num"]])) {
        datum %>% dplyr::pull(ID)
      } else {
        
        ##### Turns the string that holds the name of the variable into a symbol
        var = rlang::sym(var)
        
        ##### Gets the ID and the variable to filter
        aux = datum %>% dplyr::select(ID, !!var)
        
        ##### Gets the type of filter and applies it
        if (nature) {
          aux %>%
            dplyr::filter(!!var %in% lim[["cat"]]) %>% 
            dplyr::pull(ID)
        } else {
          aux %>%
            dplyr::filter(between(!!var, lim[["num"]][1], lim[["num"]][2])) %>% 
            dplyr::pull(ID)
        }
        
      }
    })
    
    #### Finds the ID's that are present in all sets
    limits <- purrr::reduce(limits, function(x, y) {
      intersect(x, y)
    })
    
    #### Keeps only the filtered ID's
    datum <- datum %>% 
      dplyr::filter(ID %in% limits)
    
    
    #### Verifies the Name and Domains inputs and filters those which are active
    if (!is.null(input$name) && stringr::str_length(input$name) > 0) {
      datum <- datum %>% 
        dplyr::filter(stringr::str_detect(Name, input$name))
    } else {
      datum
    }
    
    if (!is.null(input$filter_domains) && input$filter_domains) {
      datum <- datum %>%
        dplyr::filter(stringr::str_detect(Domains, paste(input$domains, sep = "|")))
    } else {
      datum
    }
    
    ### Eliminates th ID variable
    datum <- datum %>% dplyr::select(-ID)
    
  })
  
  ## Constructs the UI's for filtering
  ### Name of game
  output$UIname <- renderUI({
    
    label <- tagList(
      "Name of game: ",
      span(id = "name-tip", icon("info-circle", "fa-1x")),
      tippy::tippy_this("<div class='tips'>The input is case-sensitive</div>",
                        allowHTML = TRUE, theme = "light", placement = "right",
                        arrow = TRUE, elementId = "name-tip")
    )
    
    searchInput(
      inputId = "name",
      label = label, 
      placeholder = "Click search icon to update or hit 'Enter'",
      btnSearch = icon("search"),
      btnReset = icon("times"),
      width = "100%"
    )
    
  })
  
  ### Constructs UI's that are similar by using list structures
  #### List of construction parameters
  list_nick <- nickname[2:11]
  names(list_nick) <- NULL
  list_title <- list("Year of publication: ","Min of players: ",
                     "Max of players: ", "Play time: ", "Min. age: ",
                     "# of user votes: ", "Rating average: ", "BGG Rank: ",
                     "Complexity Average: ", "# of game owners: ")
  list_var <- list("Year Published", "Min Players", "Max Players",
                   "Play Time", "Min Age", "Users Rated",
                   "Rating Average", "BGG Rank",
                   "Complexity Average", "Owned Users")
  
  #### Places the lists inside another list and names them
  #### to simplify access inside the pmap function
  constructor <-list(
    nick = list_nick,
    title = list_title,
    var = list_var
  )
  
  #### Constructs the labels
  purrr::pmap(constructor, function(nick, title, var) {
    
    output[[paste0("LABEL",nick)]] = renderUI({
      
      label <- tagList(
        title,
        ##### Creates the reset button
        span(
          actionBttn(
            inputId = paste0("reset_", nick),
            label = "Reset",
            style = "fill", 
            color = "warning",
            size = "xs"
          )),
        ##### Creates the type button
        span(
          switchInput(
            inputId = paste0("type_", nick),
            label = "Type",
            onLabel = "Categorical",
            offLabel = "Numerical",
            onStatus = "success", 
            offStatus = "danger",
            size = "mini",
            inline = TRUE,
            labelWidth = "45px",
            handleWidth = "90px"
          ))
      )
      
    })
    
  })
  
  #### Constructs the UI's (numerical version)
  purrr::pmap(constructor, function(nick, title, var) {
    
    output[[paste0("UInum",nick)]] = renderUI({
      
      ##### Gets the variable name and makes it a symbol
      var = rlang::sym(var)
      
      ##### Obtains the possible instances of the filter
      choices <- original %>% 
        dplyr::distinct(!!var) %>%
        na.exclude() %>% 
        dplyr::arrange(!!var) %>% 
        dplyr::pull()
      
      ##### Gets the selected values
      selected <- original %>% 
        dplyr::summarise(min = min(!!var, na.rm = TRUE),
                         max = max(!!var, na.rm = TRUE))
      selected  <- c(selected$min,selected$max)
      
      ##### Creates the numeric input selector
      sliderTextInput(
        inputId = paste0("NUM",nick),
        label = NULL, 
        choices = choices,
        selected = selected
      )
      
    })
    
  })
  
  #### Constructs the UI's (categorical version)
  purrr::pmap(constructor, function(nick, title, var) {
    
    output[[paste0("UIcat",nick)]] = renderUI({
      
      ##### Gets the variable name and makes it a symbol
      var = rlang::sym(paste0("CAT ", var))
      
      ##### Obtains the possible instances of the filter
      choices <- original %>% 
        dplyr::summarise(!!var := levels(!!var)) %>% 
        dplyr::mutate(!!var := as.character(!!var)) %>% 
        dplyr::pull()
      
      ##### Gets the selected values
      selected <- choices
      
      ##### Creates the numeric input selector
      checkboxGroupButtons(
        inputId = paste0("CAT",nick),
        label = NULL, 
        choices = choices,
        selected = selected,
        status = "danger",
        checkIcon = list(
          yes = icon("check"),
          no = icon("times")
        )
      )
      
    })
    
  })
  
  ### Domains
  output$UIdomains <- renderUI({
    
    label <- tagList(
      "Domains: ",
      span(
        actionBttn(
          inputId = "reset_domains",
          label = "Reset",
          style = "fill", 
          color = "warning",
          size = "xs"
        )),
      span(
        switchInput(
          inputId = "filter_domains",
          onStatus = "success", 
          offStatus = "danger",
          label = "Filtering",
          size = "mini",
          inline = TRUE,
          labelWidth = "60px",
          handleWidth = "40px"
        ))
    )
    
    choices <- original %>% 
      dplyr::select(Domains) %>%
      na.exclude() %>% 
      dplyr::summarise(Domains = paste0(Domains, collapse = ", ")) %>% 
      dplyr::pull()
    choices <- tibble(
      Domains = unlist(str_split(choices, ", "))
    )
    choices <- choices %>% 
      dplyr::distinct(Domains) %>% 
      dplyr::arrange(Domains) %>% 
      dplyr::pull()
    
    multiInput(
      inputId = "domains",
      label = label, 
      choices = choices,
      selected = choices,
      options = list(
        selected_header = "Click to remove options in the filter:",
        non_selected_header = "Click to insert options in the filter:"
      )
    )
    
  })
  
  ## Updates input UI's to the type selection
  purrr::pmap(constructor, function(nick, title, var) {
    observeEvent(list_type()[[var]], {
      shinyjs::toggle(paste0("UInum",nick), condition = !(list_type()[[var]]))
      shinyjs::toggle(paste0("UIcat",nick), condition = list_type()[[var]])
    })
  })
  
  ## Resets UI's if a specific "reset" button or the "reset all" are pressed
  ### Name of game
  observeEvent(req(input$reset_all), {
    
    updateSearchInput(
      session = session,
      inputId = "name",
      value = "",
      trigger = TRUE
    )
    
  })
  
  ### Uses list to update similar structures
  purrr::pmap(constructor, function(nick, title, var) {
    observeEvent(req(isTruthy(input$reset_all | list_reset()[[var]])), {
      
      #### Resets the numerical input selector
      ##### Gets the variable name and makes it a symbol
      varNUM = rlang::sym(var)
      
      ##### Obtains the possible instances of the filter
      choicesNUM <- original %>% 
        dplyr::distinct(!!varNUM) %>%
        na.exclude() %>% 
        dplyr::arrange(!!varNUM) %>% 
        dplyr::pull()
      
      ##### Gets the selected values
      selectedNUM <- original %>% 
        dplyr::summarise(min = min(!!varNUM, na.rm = TRUE),
                         max = max(!!varNUM, na.rm = TRUE))
      selectedNUM  <- c(selectedNUM$min,selectedNUM$max)
      
      ##### Creates the numeric input selector
      updateSliderTextInput(
        session = session,
        inputId = paste0("NUM",nick),
        choices = choicesNUM,
        selected = selectedNUM
      )
      
      #### Resets the categorical input selector
      ##### Gets the variable name and makes it a symbol
      varCAT = rlang::sym(paste0("CAT ", var))
      
      ##### Obtains the possible instances of the filter
      choicesCAT <- original %>% 
        dplyr::summarise(!!varCAT := levels(!!varCAT)) %>% 
        dplyr::mutate(!!varCAT := as.character(!!varCAT)) %>% 
        dplyr::pull()
      
      ##### Gets the selected values
      selectedCAT <- choicesCAT
      
      ##### Creates the numeric input selector
      updateCheckboxGroupButtons(
        session = session,
        inputId = paste0("CAT",nick),
        choices = choicesCAT,
        selected = selectedCAT,
        status = "danger",
        checkIcon = list(
          yes = icon("check"),
          no = icon("times")
        )
      )
      
    })
  })  
  
  ### Domains
  observeEvent(req(isTruthy(input$reset_domains | input$reset_all)), {
    
    #### Resets the "filtering" condition to off
    updateSwitchInput(
      session = session,
      inputId = "filter_domains",
      value = FALSE
    )
    
    choices <- original %>% 
      dplyr::select(Domains) %>%
      na.exclude() %>% 
      dplyr::summarise(Domains = paste0(Domains, collapse = ", ")) %>% 
      dplyr::pull()
    choices <- tibble(
      Domains = unlist(str_split(choices, ", "))
    )
    choices <- choices %>% 
      dplyr::distinct(Domains) %>% 
      dplyr::arrange(Domains) %>% 
      dplyr::pull()
    
    updateMultiInput(
      session = session,
      inputId = "domains",
      choices = choices,
      selected = choices
    )
    
  })
  
  ## Saves the filtered data in an object readable by reactable
  data <- crosstalk::SharedData$new(df)
  
  ## Creates the logic for the reactable
  output$table <- renderReactable({
    
    reactable(data,
              minRows = 10,
              borderless = TRUE,
              striped = TRUE,
              compact = TRUE,
              highlight = TRUE,
              showSortIcon = TRUE,
              selection = "multiple",
              columns = list(
                `Name` = colDef(name = "Name of Game"),
                `Year Published` = colDef(name = "Year of Publication"),
                `Users Rated` = colDef(name = "# of user votes"),
                `Owned Users` = colDef(name = "# of game owners")
              ),
              theme = reactableTheme(
                rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
              ),
              style = list(maxHeight = 800, height = 800, padding = 10, borderRadius = 12))
    
  })
  
  ## Clears the selections on the table when data is changed by filters
  observeEvent(df(), {
    updateReactable("table", selected = NA)
  })
  
  ## Generates the scatter plot
  output$versus <- renderPlotly({
    
    ### Avoids processing when the data is not defined
    req(df())
    
    ### Gets the row numbers 
    selected <- getReactableState("table", "selected")
    
    ### Highlights selected points  
    if (is.null(selected)) {
      dataplot <- df() %>% 
        dplyr::mutate(color = "blue") %>% 
        dplyr::mutate(alpha = 0.4)
    } else {
      dataplot <- df() %>% 
        rowid_to_column() %>% 
        dplyr::mutate(color = ifelse(rowid %in% selected, "red", "blue")) %>% 
        dplyr::mutate(alpha = ifelse(rowid %in% selected, 0.8, 0.4))
    }
    
    ### 
    if (is.numeric(dataplot$`Rating Average`)) {
      if (is.numeric(dataplot$`Complexity Average`)) {
        pos <- position_identity()
      } else {
        pos <- position_jitter(height = 0.1, seed = 42)
      }
    } else {
      pos <- position_jitter(width = 0.1, seed = 42)
    }
    
    ### Starts the plot
    plot <- dataplot %>%
      ggplot(aes(
        x = `Rating Average`, y = `Complexity Average`,
        text = paste(
          "Name: ", Name,
          "<br>Players: ", `Min Players`, "-", `Max Players`,
          "<br>Rating: ", `Rating Average`,
          "<br>Complexity: ", `Complexity Average`
        )
      )) +
      geom_point(aes(color = color, alpha = alpha), position = pos) +
      scale_color_identity() +
      scale_alpha_identity() +
      theme_bw() +
      theme(
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.margin = unit(rep(1.3, 4), "cm")
      ) +
      theme(legend.position = "none")
    
    ggplotly(plot, tooltip = "text", height = 630)
    
  })
  
  ## Constructs the headers
  ### App header
  output$`app-header` <- renderUI({
    
    tagList(
      div(
        span(id = "app-icon", icon("dice-d20", class = "fa-3x")),
        span(id = "app-title", "BGG Explorer"),
        span(class = "app-icaro", "by Ícaro Bernardes"),
        a(class = "app-icaro", href = "https://twitter.com/IcaroBSC", "(@IcaroBSC)"),
        br(),
        span(id="app-subtitle",
             'Data from: Dilini Samarasinghe, July 5, 2021, "BoardGameGeek Dataset on Board Games", IEEE Dataport, doi: https://dx.doi.org/10.21227/9g61-bs59.')
      )
    )
    
  })
  
  ### Panel header
  output$`panel-header` <- renderUI({
    
    tagList(
      div(
        id="panel-title",
        icon("cog", "fa-1x"),
        "Control panel of the list",
        span(
          actionBttn(
            inputId = "reset_all",
            label = "Reset all",
            style = "fill",
            color = "warning",
            size = "xs"
          )),
        span(id = "panel-icon", icon(signs$panel, "fa-1x"))
      )
    )
    
  })
  
  ### Table header
  output$`table-header` <- renderUI({
    
    tagList(
      div(
        id="table-title",
        icon("list", "fa-1x"),
        "List of games",
        span(id = "table-icon", icon(signs$table, "fa-1x"))
      )
    )
    
  })
  
  ### Versus header
  output$`versus-header` <- renderUI({
    
    tagList(
      div(
        id="versus-title",
        icon("balance-scale", "fa-1x"),
        "Compare games",
        span(id = "versus-tip", icon("info-circle", "fa-0x")),
        tippy::tippy_this("<div class='tips'>Select some games in the list<br>above to highlight them</div>",
                          allowHTML = TRUE, theme = "light",
                          arrow = TRUE, elementId = "versus-tip"),
        span(id = "versus-icon", icon(signs$versus, "fa-1x"))
      )
    )
    
  })
  
  ## Show/hide some elements on click of the plus/minus sign
  ### Sets the holder of the signs
  signs <- reactiveValues()
  signs$panel <- "minus-circle"
  signs$panel_count <- 1
  signs$table <- "minus-circle"
  signs$table_count <- 1
  signs$versus <- "minus-circle"
  signs$versus_count <- 1
  
  ### Control panel
  shinyjs::onclick(
    "panel-icon", {
      shinyjs::toggle("panel", anim = TRUE)
      
      signs$panel_count <- signs$panel_count+1
      if (signs$panel_count %% 2 == 0) {
        signs$panel <- "plus-circle"
      } else {
        signs$panel <- "minus-circle"
      }
    }
  )
  
  ### Table
  shinyjs::onclick(
    "table-icon", {
      shinyjs::toggle("table", anim = TRUE)
      
      signs$table_count <- signs$table_count+1
      if (signs$table_count %% 2 == 0) {
        signs$table <- "plus-circle"
      } else {
        signs$table <- "minus-circle"
      }
    }
  )
  
  ### Versus
  shinyjs::onclick(
    "versus-icon", {
      shinyjs::toggle("versus", anim = TRUE)
      
      signs$versus_count <- signs$versus_count+1
      if (signs$versus_count %% 2 == 0) {
        signs$versus <- "plus-circle"
      } else {
        signs$versus <- "minus-circle"
      }
    }
  )
  
}

shinyApp(ui, server)