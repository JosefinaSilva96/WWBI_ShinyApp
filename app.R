# Shiny Dashboard for Worldwide Bureaucracy Indicators

### Libraries

library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(labelled)
library(data.table)
library(ggplot2)
library(shiny)
library(shinythemes)
library(DT)
library(maps)
library(mapdata)
library(leaflet)
library(rnaturalearth)
library(sf)
library(plotly)
library(officer)
library(viridis)
library(here)
library(glue)
library(colourpicker)
library(htmlwidgets)
library(bs4Dash)
library(countrycode)
library(bslib)
library(ggthemes)
library(shinyBS)
library(tibble)

### Load data sets ----

data_path <- file.path(getwd())

if (basename(getwd()) == "Code") {
  data_path <- dirname(getwd())
} else {
  data_path <- getwd()
}

print(paste("Using data path:", data_path))

data_wwbi         <- read_dta(file.path(data_path, "Data/data_wwbi.dta"))
data_gdp          <- read_dta(file.path(data_path, "Data/data_gdp.dta"))
gdp_2015          <- read_dta(file.path(data_path, "Data/gdp_2015.dta"))
world_spdf        <- st_read(file.path(data_path, "Data/world_spatial.gpkg"))
selected_data_long <- read_dta(file.path(data_path, "Data/selected_data_long.dta"))
data_wwbi_long    <- read_dta(file.path(data_path, "Data/data_wwbi_long.dta"))

# ── KEY CHANGE: split into time-series vs latest-obs versions ──────────────────
data_wwbi_long_ts     <- data_wwbi_long %>%
  filter(is_latest == FALSE | is.na(is_latest))   # year-by-year rows (for trend charts)

data_wwbi_long_latest <- data_wwbi_long %>%
  filter(is_latest == TRUE)                        # latest-obs rows (for cross-country comparisons)
# ──────────────────────────────────────────────────────────────────────────────

wage_bill_publicexp   <- read_dta(file.path(data_path, "Data/wage_bill_publicexp.dta")) %>%
  filter(is_latest == FALSE | is.na(is_latest))   # time series only

wage_bill_gdp         <- read_dta(file.path(data_path, "Data/wage_bill_gdp.dta")) %>%
  filter(is_latest == FALSE | is.na(is_latest))   # time series only

public_sector_emp_temp <- readRDS(file.path(data_path, "Data", "public_sector_emp_temp.rds")) %>%
  mutate(across(where(~inherits(.x, "haven_labelled")), as_factor))

public_sector_emp <- readRDS(file.path(data_path, "Data", "public_sector_emp.rds"))

public_sector_emp_temp_last <- readRDS(file.path(data_path, "Data", "public_sector_emp_temp_last.rds")) %>%
  mutate(across(where(~inherits(.x, "haven_labelled")), as_factor))

public_sector_workforce_clean <- read_dta(file.path(data_path, "Data/public_sector_workforce_clean.dta"))
public_sector_workforce       <- read_dta(file.path(data_path, "Data/public_sector_workforce.dta"))
public_sector_workforce_first_last <- read_dta(file.path(data_path, "Data/public_sector_workforce_first_last.dta"))
gender_workforce      <- read_dta(file.path(data_path, "Data/gender_workforce.dta"))
data_indicator_wb     <- read_dta(file.path(data_path, "Data/data_indicator_wb.dta"))
merged_data           <- read_dta(file.path(data_path, "Data/merged_data.dta"))
tertiary_education    <- read_dta(file.path(data_path, "Data/tertiary_education.dta"))
public_wage_premium   <- read_dta(file.path(data_path, "Data/public_wage_premium.dta"))
public_wage_premium_educ <- read_dta(file.path(data_path, "Data/public_wage_premium_educ.dta"))
gender_wage_premium   <- readRDS(file.path(data_path, "Data", "gender_wage_premium.rds"))
gender_wage_premium_last <- readRDS(file.path(data_path, "Data", "gender_wage_premium_last.rds"))
gender_leadership     <- readRDS(file.path(data_path, "Data", "gender_leadership.rds"))
gender_wage_premiumpublic <- readRDS(file.path(data_path, "Data", "gender_wage_premiumpublic.rds"))
pay_compression       <- readRDS(file.path(data_path, "Data", "pay_compression.rds"))
pay_compression_wide  <- readRDS(file.path(data_path, "Data", "pay_compression_wide.rds"))

# ---------------------------
# UI
# ---------------------------

library(shiny)
library(bslib)

`%||%` <- function(x, y) if (is.null(x)) y else x
sanitize_vec <- function(x) {
  x <- x %||% character(0)
  x <- as.character(x)
  unique(x[!is.na(x) & nzchar(x)])
}

ui <- bootstrapPage(
  theme = bs_theme(version = 5, bootswatch = "sandstone"),
  tags$head(
    tags$style(HTML("
      :root {
        --wb-navy: #002244;
        --wb-blue: #003366;
        --bg: #F4F6F8;
        --card: #FFFFFF;
        --soft: #E6F0F7;
        --text: #1F2A33;
        --accent: #003366;
        --border: #E0E6ED;
      }
      html, body { height: 100%; }
      body, .container-fluid, .main-container, .content-wrapper, .flex-grow-1 {
        background-color: var(--bg) !important;
        color: var(--text) !important;
      }
      h1, h2, h3, h4, h5, h6 { color: var(--wb-navy) !important; font-weight: 600; }
      p, li, span, label { color: var(--text) !important; }
      a { color: var(--wb-blue) !important; text-decoration: none; }
      a:hover { color: #0072CE !important; text-decoration: underline; }
      #sidebar {
        height: 100vh; width: 290px; min-width: 290px;
        background: var(--wb-navy) !important;
        padding: 18px 16px; color: #ffffff;
        overflow-y: auto; border-right: 1px solid rgba(0,0,0,0.08);
        box-shadow: none; position: sticky; top: 0;
      }
      #sidebar::-webkit-scrollbar { width: 8px; }
      #sidebar::-webkit-scrollbar-thumb { background: rgba(255,255,255,.25); border-radius: 8px; }
      #sidebar::-webkit-scrollbar-track { background: transparent; }
      .sidebar-brand { color: #fff; }
      .sidebar-brand .brand-dot { background: var(--soft); }
      .nav-section {
        display: flex; align-items: center; justify-content: space-between;
        font-size: 16px; font-weight: 700;
        padding: 10px 10px; margin: 12px 6px 4px;
        color: #ffffff; border-radius: 8px;
        transition: background .2s, color .2s; cursor: pointer;
      }
      .nav-section:hover { background: rgba(255,255,255,.10); }
      .nav-section::after { content: '▾'; font-size: 14px; opacity: .8; margin-left: 8px; }
      .section-open::after { transform: rotate(180deg); }
      .nav-item a, .nav-sub-item a { color: inherit !important; text-decoration: none; }
      .nav-item {
        display: flex; align-items: center; gap: 10px;
        margin: 6px 6px; padding: 10px 12px;
        font-size: 16px; font-weight: 600; color: #eef5ff;
        border-radius: 10px; transition: transform .08s, background .2s;
      }
      .nav-item:hover { background: rgba(255,255,255,.10); transform: translateX(2px); }
      .nav-item.active {
        background: rgba(230,240,247,.18);
        box-shadow: inset 0 0 0 1px rgba(230,240,247,.35); position: relative;
      }
      .nav-item.active::before {
        content: ''; position: absolute; left: -6px; top: 10px; bottom: 10px;
        width: 4px; border-radius: 4px; background: var(--soft);
      }
      #macro_section, #public_sector_section, #public_sector_workforce_section,
      #public_sector_wages_section, #equity_public_sector_section {
        padding: 4px 6px 6px 12px; display: none;
        border-left: 1px dashed rgba(255,255,255,.25); margin-left: 10px;
      }
      .nav-sub-item {
        display: flex; align-items: center; gap: 8px;
        margin: 4px 0; padding: 8px 10px;
        font-size: 15px; color: #eaf3ff; border-radius: 8px;
        transition: background .2s, transform .08s;
      }
      .nav-sub-item:hover { background: rgba(255,255,255,.10); transform: translateX(2px); }
      .nav-sub-item.active { background: rgba(230,240,247,.18); }
      .well, .card, .panel, .box, .custom-info-box, .info-box {
        background-color: var(--card) !important; color: var(--text) !important;
        border: 1px solid var(--border) !important; border-radius: 10px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.04);
      }
      .accordion-item {
        background-color: var(--card) !important; border: 1px solid var(--border) !important;
        border-radius: 12px !important; margin-bottom: 14px; overflow: hidden; color: var(--text) !important;
      }
      .accordion-button {
        background-color: var(--card) !important; color: var(--wb-navy) !important;
        box-shadow: none !important; font-size: 18px; padding: 16px 20px;
      }
      .accordion-button:not(.collapsed) { background-color: var(--soft) !important; color: var(--wb-navy) !important; }
      .accordion-button:focus { box-shadow: none !important; }
      .accordion-body {
        background-color: var(--card) !important; color: var(--text) !important;
        padding: 18px 22px; border-top: 1px solid var(--border);
      }
      .logos-row { display:flex; align-items:center; justify-content:space-between; gap:24px; flex-wrap:nowrap; }
      .logo-wrap { flex: 1 1 0; height: 90px; display:flex; align-items:center; justify-content:center; }
      .logos-row img { height: 70px !important; width: auto !important; max-width: 100%; object-fit: contain; display:block; }
      .logos-row img.bl-logo { height: 130px !important; }
      .btn, .btn-primary, .dl-btn {
        background-color: var(--wb-blue) !important; border: none !important;
        color: #fff !important; border-radius: 10px;
      }
      .btn:hover, .btn-primary:hover, .dl-btn:hover { background-color: var(--wb-navy) !important; color:#fff !important; }
      li i.fa, li i.fas, li i.fa-solid { margin: 0 6px; color: #fff; }
      .plot-container { background-color: var(--card) !important; border-radius: 10px; padding: 10px; border: 1px solid var(--border); }
      #graph_choice .form-check { margin-bottom: .3rem; }
    ")),
    tags$script(HTML("
      function toggleSection(id){
        var section = document.getElementById(id);
        section.style.display = (section.style.display === 'none' || section.style.display === '') ? 'block' : 'none';
        var header = document.querySelector('[onclick=\"toggleSection(\\''+id+'\\')\"]');
        if(header){ header.classList.toggle('section-open'); }
      }
      document.addEventListener('click', function(e){
        if(e.target.closest('.nav-item')){
          document.querySelectorAll('#sidebar .nav-item').forEach(n=>n.classList.remove('active'));
          e.target.closest('.nav-item').classList.add('active');
        }
        if(e.target.closest('.nav-sub-item')){
          document.querySelectorAll('#sidebar .nav-sub-item').forEach(n=>n.classList.remove('active'));
          e.target.closest('.nav-sub-item').classList.add('active');
        }
      }, true);
    "))
  ),
  div(
    class = "d-flex",
    div(
      id = "sidebar",
      div(class = "nav-item", actionLink("nav_dashboard",   "Overview")),
      div(class = "nav-item", actionLink("nav_instructions","Instructions")),
      div(class = "nav-item", actionLink("nav_metadata",    "Metadata")),
      div(class = "nav-section", onclick = "toggleSection('macro_section')", "Macro Fundamentals of the Public Sector"),
      div(id = "macro_section",
          div(class = "nav-sub-item", actionLink("nav_wagebill",     "Wage Bill Graphs")),
          div(class = "nav-sub-item", actionLink("nav_wagebill_gdp", "Wage Bill & GDP Graphs"))),
      div(class = "nav-section", onclick = "toggleSection('public_sector_section')", "Size and Characteristics of the Public Sector Employment"),
      div(id = "public_sector_section",
          div(class = "nav-sub-item", actionLink("nav_public_graphs",    "Public Employment")),
          div(class = "nav-sub-item", actionLink("nav_public_workforce", "Employment Distribution")),
          div(class = "nav-sub-item", actionLink("nav_education",        "Tertiary Education"))),
      div(class = "nav-section", onclick = "toggleSection('public_sector_wages_section')", "Competitiveness of Public Sector Wages"),
      div(id = "public_sector_wages_section",
          div(class = "nav-sub-item", actionLink("nav_wagepremium",   "Wage Premium")),
          div(class = "nav-sub-item", actionLink("nav_public_educ",   "Wage Premium by Education")),
          div(class = "nav-sub-item", actionLink("nav_pay_compression","Pay Compression"))),
      div(class = "nav-section", onclick = "toggleSection('equity_public_sector_section')", "Equity in Public Sector Employment"),
      div(id = "equity_public_sector_section",
          div(class = "nav-sub-item", actionLink("nav_gender_workforce",   "Female Employment")),
          div(class = "nav-sub-item", actionLink("nav_female_leadership",  "Female Leadership")),
          div(class = "nav-sub-item", actionLink("nav_wagepremium_gender", "Wage Premium by Gender")),
          div(class = "nav-sub-item", actionLink("nav_gender_wage_premium","Gender Wage Premium by Industry"))),
      div(class = "nav-item", actionLink("nav_download_all", "📥 Download All Graphs"))
    ),
    div(
      class = "flex-grow-1 p-4",
      h2("Worldwide Bureaucracy Indicators"),
      uiOutput("main_content")
    )
  )
)

# SERVER
server <- function(input, output, session) {
  
  active_tab <- reactiveVal("dashboard")
  
  safe_acc_open  <- function(id, panels) {
    if (exists("accordion_open",  envir = asNamespace("bslib"), inherits = FALSE)) bslib::accordion_open(id, panels)
  }
  safe_acc_close <- function(id, panels) {
    if (exists("accordion_close", envir = asNamespace("bslib"), inherits = FALSE)) bslib::accordion_close(id, panels)
  }
  
  observeEvent(input$nav_dashboard,          { active_tab("dashboard") })
  observeEvent(input$nav_instructions,       { active_tab("instructions") })
  observeEvent(input$nav_metadata,           { active_tab("metadata") })
  observeEvent(input$nav_publications,       { active_tab("publications") })
  observeEvent(input$nav_wagebill,           { active_tab("wagebill") })
  observeEvent(input$nav_wagebill_gdp,       { active_tab("wagebill_gdp") })
  observeEvent(input$nav_public_workforce,   { active_tab("public_workforce") })
  observeEvent(input$nav_gender_workforce,   { active_tab("gender_workforce") })
  observeEvent(input$nav_education,          { active_tab("education") })
  observeEvent(input$nav_public_educ,        { active_tab("public_educ") })
  observeEvent(input$nav_public_graphs,      { active_tab("public_graphs") })
  observeEvent(input$nav_wagepremium_gender, { active_tab("wagepremium_gender") })
  observeEvent(input$nav_female_leadership,  { active_tab("female_leadership") })
  observeEvent(input$nav_wagepremium,        { active_tab("wagepremium") })
  observeEvent(input$nav_gender_wage_premium,{ active_tab("gender_wage_premium") })
  observeEvent(input$nav_pay_compression,    { active_tab("pay_compression") })
  observeEvent(input$nav_download_all,       { active_tab("download_all") })
  
  # ── Country dropdowns: use full data_wwbi_long for complete country list ──────
  all_country_choices <- sort(unique(data_wwbi_long$country_name))
  
  output$main_content <- renderUI({
    tab <- active_tab()
    
    if (tab == "dashboard") {
      tagList(
        fluidRow(class = "mb-3 logos-row",
                 column(4, div(class="logo-wrap", tags$img(src="logos/WBG-Institutions-Horizontal-CMYK-01.jpeg", class="wb-logo wb-logo--right", alt="WBG Institutions"))),
                 column(4, div(class="logo-wrap", tags$img(src="logos/bl_logo.png",                              class="bl-logo",              alt="Bureaucracy Lab"))),
                 column(4, div(class="logo-wrap", tags$img(src="logos/WB-DEC-Impact-horizontal-RGB-high.png",    class="wb-logo wb-logo--dec", alt="WBG DEC")))
        ),
        h3("Overview"),
        accordion(
          id = "ov_acc", multiple = TRUE, open = character(0),
          accordion_panel("About the WWBI",
                          tagList(
                            p("The Worldwide Bureaucracy Indicators (WWBI) database is a unique cross-national dataset on public sector employment and wages that aims to fill an information gap, thereby helping researchers, development practitioners, and policymakers gain a better understanding of the personnel dimensions of state capability, the footprint of the public sector within the overall labor market, and the fiscal implications of the public sector wage bill."),
                            p("Key indicators include:"),
                            tags$ul(
                              tags$li("Size of the public and private sector workforce"),
                              tags$li("Demographics of public and private sector employment"),
                              tags$li("Gender equity in public and private sectors"),
                              tags$li("Public sector wage premiums"),
                              tags$li("Pay compression ratios in public and private sectors")
                            )
                          )
          ),
          accordion_panel("Contact Information",
                          tags$p(
                            "Flavia Sacco – ", tags$a(href="mailto:fsaccocapurro@worldbank.org","fsaccocapurro@worldbank.org"), br(),
                            "Josefina Silva – ", tags$a(href="mailto:jsilvafuentealba@worldbank.org","jsilvafuentealba@worldbank.org")
                          )
          ),
          accordion_panel("Citation",
                          p("We kindly ask all users of the dashboard to cite it as follows: Source: Worldwide Bureaucracy Indicators (WWBI) Dashboard – World Bank.")
          ),
          accordion_panel("Disclaimer",
                          p("The findings, interpretations, and conclusions presented in this dashboard are those of the World Bank staff and do not necessarily reflect the views of the World Bank.")
          )
        ),
        fluidRow(column(10, h3("📄 Publications"),
                        wellPanel(
                          h4("Download Team Publications:"),
                          tags$ul(
                            tags$li(downloadLink("pub1", "Innovating Bureaucracy for a More Capable Government"), br(), tags$small("Report")),
                            tags$li(downloadLink("pub2", "Introducing the Worldwide Bureaucracy Indicators"), br(), tags$small("Baig et al.")),
                            tags$li(downloadLink("pub3", "Public Sector Employment and Compensation: An Assessment Framework"), br(), tags$small("Report")),
                            tags$li(downloadLink("pub4", "Worldwide Bureaucracy Indicators"), br(), tags$small("Report"))
                          )
                        )
        ))
      )
      
    } else if (tab == "instructions") {
      tagList(
        h3("📘 Instruction Manual"),
        accordion(id = "inst_acc", multiple = TRUE, open = "About this dashboard",
                  accordion_panel("About this dashboard",
                                  p("This Dashboard is a product of the Bureaucracy Lab, a joint initiative between the Governance Global Practice and the Development Impact Evaluation (DIME) Department of the Research Group at the World Bank.")
                  ),
                  accordion_panel("How to use the dashboard",
                                  tags$ol(
                                    tags$li("In each tab, select a country of interest and choose comparator countries, regions, or income groups."),
                                    tags$li("To check indicator availability, go to 'Metadata'."),
                                    tags$li("The first selected country appears first in graphs and acts as the benchmark."),
                                    tags$li("Each tab provides downloadable graphs via the camera icon."),
                                    tags$li("Use 'Download All Graphs' to export a comprehensive Word report.")
                                  )
                  ),
                  accordion_panel("Resources & links",
                                  tags$p("GitHub:", tags$a(href="https://github.com/worldbank/Worldwide-Bureaucracy-Indicators","https://github.com/worldbank/Worldwide-Bureaucracy-Indicators", target="_blank")),
                                  tags$p("Data Catalog:", tags$a(href="https://datacatalog.worldbank.org/int/home","https://datacatalog.worldbank.org/int/home", target="_blank")),
                                  div(style="margin-top:8px;", downloadButton("download_pdf","📥 Download Codebook", class="btn btn-primary"))
                  )
        )
      )
      
    } else if (tab == "metadata") {
      tagList(
        h3("Metadata"),
        fluidRow(
          column(4, infoBox("Indicators",  302,                                   icon=icon("list"))),
          column(4, infoBox("Economies",   length(unique(data_wwbi$country_name)), icon=icon("globe"))),
          column(4, infoBox("Coverage",    "2000-2022",                           icon=icon("calendar")))
        ),
        fluidRow(column(6, selectInput("indicatorSelect","Select Indicator", choices=unique(data_wwbi$indicator_name)))),
        fluidRow(textOutput("countryCount")),
        fluidRow(leafletOutput("worldMap", height="600px"))
      )
      
    } else if (tab == "wagebill") {
      tagList(
        h3("Wage Bill Graphs"),
        fluidRow(div(style="background-color:rgba(255,255,255,0.05);border:1px solid white;border-radius:10px;padding:20px;",
                     "This visualization explores the wage bill over time for selected countries.")),
        fluidRow(
          # ── CHANGE: use all_country_choices so regions/income groups appear ──
          column(7, selectInput("countries","Select country(ies)/region(s)/income group(s) – Your first selection will be treated as the reference point",
                                choices=all_country_choices, multiple=TRUE, width="100%")),
          column(5, radioButtons("graph_choice", label=tags$span(class="rb-title","Choose wage-bill measure:"),
                                 choices=c("Wage Bill as % of Public Expenditure"="Public","Wage Bill as % of GDP"="GDP"),
                                 selected="Public", inline=FALSE))
        ),
        fluidRow(plotlyOutput("plotwagebill", height="500px")),
        fluidRow(div(style="background-color:rgba(255,255,255,0.05);border:1px solid white;border-radius:10px;padding:20px;", textOutput("note_wagebill"))),
        fluidRow(column(12, downloadButton("downloadWord","Download Report in Word", class="dl-btn w-100"))),
        fluidRow(column(4,  downloadButton("dl_csv_wagebill","Download data (CSV)", class="dl-btn w-100")))
      )
      
    } else if (tab == "wagebill_gdp") {
      tagList(
        h3("Wage Bill & GDP Graphs"),
        fluidRow(div(style="background-color:rgba(255,255,255,0.05);border:1px solid white;border-radius:10px;padding:20px;",
                     "This graph shows the relationship between the size of the wage bill and GDP per capita.")),
        fluidRow(
          column(7,
                 selectInput("countries_gdp","Select country(ies)/region(s)/income group(s)",
                             choices=all_country_choices, multiple=TRUE, width="100%"),
                 br(),
                 downloadButton("downloadGDPDoc","Download GDP Analysis Report", class="dl-btn w-100")
          ),
          column(5,
                 tags$label(class="form-label fw-semibold","Choose label type"),
                 radioButtons("label_type", label=NULL, choices=c("Country","Region"), selected="Country")
          )
        ),
        fluidRow(column(12, plotlyOutput("dot_plot_gdp", height="500px"))),
        fluidRow(column(12, div(style="background-color:rgba(255,255,255,0.05);border:1px solid white;border-radius:10px;padding:20px;", textOutput("note_dotplot_gdp")))),
        fluidRow(column(12, div(class="text-end mt-3", downloadButton("dl_csv_gdp","Download data (CSV)", class="dl-btn"))))
      )
      
    } else if (tab == "public_workforce") {
      tagList(
        h3("Distribution of Public Sector Employment"),
        fluidRow(div(style="background-color:rgba(255,255,255,0.05);border:1px solid white;border-radius:10px;padding:20px;",
                     "This visualization shows the distribution of the public sector workforce across the three main industries.")),
        fluidRow(
          column(7, selectInput("countries_workforce","Select country(ies)/region(s)/income group(s)",
                                choices=all_country_choices, multiple=TRUE, width="100%")),
          column(5,
                 checkboxGroupInput("selected_graphs_public","Select Graphs to Download",
                                    choices=c("Multi-Country Graph"="firstGraph","Single-Country Graph"="secondGraph"),
                                    selected=c("firstGraph","secondGraph")),
                 downloadButton("downloadGraphsemploymentdist","Download Selected Graphs in Word", class="dl-btn w-100")
          )
        ),
        fluidRow(plotlyOutput("stackedBarGraph", height="600px")),
        fluidRow(div(style="background-color:rgba(255,255,255,0.05);border:1px solid white;border-radius:10px;padding:20px;", textOutput("note_stackedBarGraph"))),
        fluidRow(column(12, selectInput("selected_country","Select country/region/income group",
                                        choices=all_country_choices, multiple=FALSE, width="100%"))),
        fluidRow(plotlyOutput("horizontalStackedBar", height="600px")),
        fluidRow(div(style="background-color:rgba(255,255,255,0.05);border:1px solid white;border-radius:10px;padding:20px;", textOutput("note_horizontalStackedBar"))),
        fluidRow(column(4, downloadButton("dl_csv_public_workforce","Download data (CSV)", class="dl-btn w-100")))
      )
      
    } else if (tab == "education") {
      tagList(
        h3("Workers with Tertiary Education"),
        fluidRow(div(style="background-color:rgba(255,255,255,0.05);border:1px solid white;border-radius:10px;padding:20px;",
                     "This visualization shows the proportion of workers with tertiary education in the public and private sectors.")),
        fluidRow(
          column(7,
                 selectInput("selected_countries","Select country(ies)/region(s)/income group(s)",
                             choices=all_country_choices, multiple=TRUE, width="100%"),
                 br(),
                 downloadButton("downloadGraphsWordEducation","Download Tertiary Education Report", class="dl-btn w-100")
          ),
          column(5,
                 tags$label(class="form-label fw-semibold","Choose label type"),
                 radioButtons("label_type_edu", label=NULL, choices=c("Country","Region"), selected="Country", inline=TRUE)
          )
        ),
        fluidRow(plotlyOutput("barPlot", height="600px")),
        fluidRow(div(style="background-color:rgba(255,255,255,0.05);border:1px solid white;border-radius:10px;padding:20px;", textOutput("note_tertiaryEducation"))),
        fluidRow(column(4, downloadButton("dl_csv_tertiary_edu","Download data (CSV)", class="dl-btn w-100")))
      )
      
    } else if (tab == "wagepremium") {
      tagList(
        h3("Public Sector Wage Premium"),
        fluidRow(
          column(7,
                 selectInput("countries_wage_premium","Select country(ies)/region(s)/income group(s)",
                             choices=all_country_choices, multiple=TRUE, width="100%"),
                 br(),
                 downloadButton("downloadWagePremiumReport","Download Wage Premium Report", class="dl-btn w-100")
          )
        ),
        fluidRow(plotlyOutput("dotPlot", height="500px")),
        fluidRow(div(style="background-color:rgba(255,255,255,0.05);border:1px solid white;border-radius:10px;padding:20px;", textOutput("note_wage_premium"))),
        fluidRow(column(4, downloadButton("dl_csv_wage_premium","Download data (CSV)", class="dl-btn w-100")))
      )
      
    } else if (tab == "public_educ") {
      tagList(
        h3("Public Sector Wage Premium by Education Level"),
        fluidRow(div(style="background-color:rgba(255,255,255,0.05);border:1px solid white;border-radius:10px;padding:20px;",
                     "This visualization explores the public sector wage premium by education level.")),
        fluidRow(
          column(7,
                 selectInput("selected_country","Select country/region/income group",
                             choices=all_country_choices, multiple=FALSE, width="100%"),
                 br(),
                 downloadButton("downloadEducationWagePremium","Download Education Wage Premium Report", class="dl-btn w-100")
          )
        ),
        fluidRow(plotlyOutput("education_wage_premium_plot", height="600px")),
        fluidRow(div(style="background-color:rgba(255,255,255,0.05);border:1px solid white;border-radius:10px;padding:20px;", textOutput("note_education_wage_premium"))),
        fluidRow(column(4, downloadButton("dl_csv_wagepremium_educ","Download data (CSV)", class="dl-btn w-100")))
      )
      
    } else if (tab == "public_graphs") {
      tagList(
        h3("Public Sector Employment Graphs"),
        fluidRow(column(7, selectInput("countries_first","Select country(ies)/region(s)/income group(s)",
                                       choices=all_country_choices, multiple=TRUE, width="100%"))),
        fluidRow(plotlyOutput("firstGraphpublic", height="600px")),
        fluidRow(div(style="background-color:rgba(255,255,255,0.05);border:1px solid white;border-radius:10px;padding:20px;", textOutput("note_firstGraphpublic"))),
        fluidRow(column(12, selectInput("country_second","Select country/region/income group",
                                        choices=all_country_choices, multiple=FALSE, width="100%"))),
        fluidRow(plotlyOutput("secondGraphpublic", height="600px")),
        fluidRow(div(style="background-color:rgba(255,255,255,0.05);border:1px solid white;border-radius:10px;padding:20px;", textOutput("note_secondGraphpublic"))),
        fluidRow(column(12, downloadButton("downloadGraphsWord","Download Graphs as Word File", class="dl-btn w-100"))),
        fluidRow(column(12, div(class="text-end", downloadButton("dl_public_emp_data","Download data CSV", class="dl-btn"))))
      )
      
    } else if (tab == "gender_workforce") {
      tagList(
        h3("Female share of employment"),
        fluidRow(div(style="background-color:rgba(255,255,255,0.05);border:1px solid white;border-radius:10px;padding:20px;", textOutput("note_firstGraphGenderWorkforce"))),
        fluidRow(
          column(7,
                 selectInput("countries_gender","Select country(ies)/region(s)/income group(s)",
                             choices=all_country_choices, multiple=TRUE, width="100%"),
                 br(),
                 downloadButton("downloadGraphsWordGender","Download Female Share of Employment Report", class="dl-btn w-100")
          )
        ),
        fluidRow(plotlyOutput("firstGraphGenderWorkforce", height="600px")),
        fluidRow(div(style="background-color:rgba(255,255,255,0.05);border:1px solid white;border-radius:10px;padding:20px;", textOutput("note_secondGraphGenderWorkforce"))),
        fluidRow(column(12, selectInput("country_gender","Select country/region/income group",
                                        choices=all_country_choices, multiple=FALSE, width="100%"))),
        fluidRow(plotlyOutput("secondGraphGenderWorkforce", height="600px")),
        fluidRow(column(4, downloadButton("dl_gender_workforce_xlsx","Download data (CSV)", class="dl-btn w-100")))
      )
      
    } else if (tab == "female_leadership") {
      tagList(
        h3("Female Leadership: Occupations and Sector"),
        fluidRow(column(7,
                        selectInput("selected_countries","Select country(ies)/region(s)/income group(s)",
                                    choices=all_country_choices, multiple=TRUE, width="100%"),
                        br(),
                        downloadButton("downloadGraphsWordfemale","Download Female Leadership Report", class="dl-btn w-100")
        )),
        fluidRow(column(12, plotlyOutput("barPlotwomen", height="600px"))),
        fluidRow(column(12, div(style="background-color:rgba(255,255,255,0.05);border:1px solid white;border-radius:10px;padding:20px;", textOutput("note_barPlotwomen")))),
        fluidRow(column(4, downloadButton("dl_csv_female_leadership","Download data (CSV)", class="dl-btn w-100")))
      )
      
    } else if (tab == "wagepremium_gender") {
      tagList(
        h3("Public Sector Wage Premium by Gender"),
        fluidRow(column(7,
                        selectInput("countries_first","Select country(ies)/region(s)/income group(s)",
                                    choices=all_country_choices, multiple=TRUE, width="100%"),
                        br(),
                        downloadButton("downloadGraphswagepremiumbygender","Download Wage Premium by Gender Report", class="dl-btn w-100")
        )),
        fluidRow(column(12, plotlyOutput("firstGraphGenderWagePremium", height="600px"))),
        fluidRow(column(12, div(style="background-color:rgba(255,255,255,0.05);border:1px solid white;border-radius:10px;padding:20px;", textOutput("note_firstGraphGenderWagePremium")))),
        fluidRow(column(7, selectInput("country_second","Select country/region/income group",
                                       choices=all_country_choices, multiple=FALSE, width="100%"))),
        fluidRow(column(12, plotlyOutput("secondGraphGenderWagePremium", height="600px"))),
        fluidRow(column(12, div(style="background-color:rgba(255,255,255,0.05);border:1px solid white;border-radius:10px;padding:20px;", textOutput("note_secondGraphGenderWagePremium")))),
        fluidRow(column(4, downloadButton("dl_gender_wageprem_xlsx","Download data (CSV)", class="dl-btn w-100")))
      )
      
    } else if (tab == "gender_wage_premium") {
      tagList(
        h3("Gender Wage Premium in Public Sector by Industry"),
        fluidRow(column(7,
                        selectInput("selected_countries","Select country(ies)/region(s)/income group(s)",
                                    choices=all_country_choices, multiple=TRUE, width="100%"),
                        br(),
                        downloadButton("downloadGenderWagePremium","Download Gender Wage Premium Report", class="dl-btn w-100")
        )),
        fluidRow(column(12, plotOutput("gender_wage_barplot", height="600px"))),
        fluidRow(column(12, div(style="background-color:rgba(255,255,255,0.05);border:1px solid white;border-radius:10px;padding:20px;", textOutput("note_gender_wage_barplot")))),
        fluidRow(column(4, downloadButton("dl_csv_gender_wage_industry","Download data (CSV)", class="dl-btn w-100")))
      )
      
    } else if (tab == "pay_compression") {
      tagList(
        h3("Pay Compression Ratios"),
        fluidRow(div(style="background-color:rgba(255,255,255,0.05);border:1px solid white;border-radius:10px;padding:20px;",
                     "This visualization explores pay compression in the public and private sectors across selected countries.")),
        fluidRow(column(7,
                        selectInput("countries_first","Select country(ies)/region(s)/income group(s)",
                                    choices=all_country_choices, multiple=TRUE, width="100%"),
                        br(),
                        downloadButton("downloadPayCompressionDoc","Download Pay Compression Report", class="dl-btn w-100")
        )),
        fluidRow(plotlyOutput("paycompression_plot", height="600px")),
        fluidRow(div(style="background-color:rgba(255,255,255,0.05);border:1px solid white;border-radius:10px;padding:20px;", textOutput("note_dotplot_pay"))),
        fluidRow(column(4, downloadButton("dl_csv_pay_compression","Download data (CSV)", class="dl-btn w-100")))
      )
      
    } else if (tab == "download_all") {
      tagList(
        h3("Download Graph Reports"),
        fluidRow(div(style="background-color:rgba(255,255,255,0.05);border:1px solid white;border-radius:10px;padding:20px;",
                     p("Download a comprehensive report with all graphs or select specific graphs.", style="font-size:16px;color:#333;"))),
        br(),
        fluidRow(column(6, align="center",
                        selectInput("download_report_countries","Select countries/regions/income group(s) for the report:",
                                    choices=all_country_choices, selected=c("Chile"), multiple=TRUE, selectize=TRUE)
        )),
        h4("Download a Custom Report"),
        checkboxGroupInput("selected_graphs","Select Graphs to Include:",
                           choices=list(
                             "Wage Bill"="wagebill","Wage Bill as % of GDP"="wagebill_gdp",
                             "Tertiary Education"="tertiaryeducation","Wage Premium by Education"="wagepremiumeducation",
                             "Public Employment"="public_employment","Wage Premium by Gender"="wagepremiumgender",
                             "Public Sector Workforce"="public_workforce","Female Employment"="gender_workforce",
                             "Female Leadership"="female_leadership","Wage Premium"="wagepremium",
                             "Gender Wage Premium by Industry"="gender_wage_premium","Pay Compression"="pay_compression"
                           ),
                           selected=c("wagebill","public_employment")
        ),
        br(),
        fluidRow(
          column(6, align="center", downloadButton("downloadAllGraphsDoc","\U1F4C4 Download Full Word Report", style="padding:10px 20px;font-size:16px;margin-top:10px;")),
          column(6, align="center", downloadButton("downloadSelectedGraphsDoc","\U1F4C4 Download Custom Word Report", style="padding:10px 20px;font-size:16px;margin-top:10px;")),
          column(6, align="center", downloadButton("downloadSelectedGraphsPPT","\U1F4CA Download PowerPoint Slides", style="padding:10px 20px;font-size:16px;margin-top:10px;"))
        )
      )
    }
  })
  
  # ── WAGE BILL ────────────────────────────────────────────────────────────────
  # KEY CHANGE: wage_bill_publicexp and wage_bill_gdp are already filtered to
  # time-series rows (is_latest == FALSE) at load time above, so charts show
  # the full trend rather than a single point.
  
  selected_data <- reactive({
    req(input$countries)
    if (input$graph_choice == "GDP") {
      wage_bill_gdp %>% filter(country_name %in% input$countries)
    } else {
      wage_bill_publicexp %>% filter(country_name %in% input$countries)
    }
  })
  
  output$plotwagebill <- renderPlotly({
    d <- selected_data()
    if (nrow(d) == 0) return(plotly_empty(type="scatter",mode="markers") %>% layout(title="No data available"))
    title_text <- ifelse(input$graph_choice=="GDP","Wage Bill as % of GDP Over Time","Wage Bill as % of Public Expenditure Over Time")
    y_label    <- ifelse(input$graph_choice=="GDP","Wage Bill (% of GDP)","Wage Bill (% of Public Expenditure)")
    plot_ly(data=d, x=~year, y=~value, color=~country_name, type="scatter", mode="lines+markers", marker=list(size=8)) %>%
      layout(title=title_text, xaxis=list(title="Year",dtick=2), yaxis=list(title=y_label), legend=list(title=list(text="Country")))
  })
  
  output$note_wagebill <- renderText({
    if (input$graph_choice=="GDP") "Note: Wage bill as a percentage of GDP." else "Note: Wage bill as a percentage of public expenditure."
  })
  
  output$dl_csv_wagebill <- downloadHandler(
    filename = function() paste0("wagebill_",ifelse(input$graph_choice=="GDP","gdp","publicexp"),"_",Sys.Date(),".csv"),
    content  = function(file) { d <- selected_data(); validate(need(nrow(d)>0,"No data")); readr::write_csv(d,file) }
  )
  
  # ── GDP DOT PLOT ─────────────────────────────────────────────────────────────
  # KEY CHANGE: merged_data uses the latest obs per country, which is correct
  # for a cross-country scatter — no change needed here.
  
  output$dot_plot_gdp <- renderPlotly({
    req(input$countries_gdp)
    d <- merged_data %>% dplyr::filter(country_name %in% input$countries_gdp)
    if (nrow(d)==0) return(plotly_empty(type="scatter",mode="markers") %>% layout(title="No data available"))
    first_sel  <- input$countries_gdp[1]
    d <- d %>% mutate(color=ifelse(country_name==first_sel,"#B3242B","#003366"))
    region_col <- intersect(c("region","region_name","Region"),names(d))[1]
    label_vec  <- if (!is.na(region_col) && identical(input$label_type,"Region")) d[[region_col]] else d$country_name
    m    <- lm(indicator_value~log_gdp, data=d)
    pred <- predict(m, newdata=d)
    plot_ly(d, x=~log_gdp, y=~indicator_value, type="scatter", mode="markers+text",
            text=label_vec, textposition="top center",
            marker=list(size=10,color=~color,opacity=0.7)) %>%
      add_trace(x=d$log_gdp, y=pred, inherit=FALSE, type="scatter", mode="lines",
                line=list(color="gray",dash="dash"), showlegend=FALSE) %>%
      layout(title="Wage Bill vs. Log(GDP per Capita)", xaxis=list(title="Log(GDP per Capita, 2015)"),
             yaxis=list(title="Wage Bill"), showlegend=FALSE, plot_bgcolor="white", paper_bgcolor="white")
  })
  
  output$note_dotplot_gdp <- renderText({
    "Note: Relationship between wage bill (share of total expenditure) and income level. Shows last year available per country."
  })
  
  dot_data_gdp <- reactive({
    req(input$countries_gdp)
    merged_data %>% dplyr::filter(country_name %in% input$countries_gdp) %>%
      dplyr::mutate(label=country_name) %>%
      dplyr::select(dplyr::any_of(c("country_name","label","year","indicator_value","log_gdp")))
  })
  
  output$dl_csv_gdp <- downloadHandler(
    filename = function() paste0("wagebill_vs_gdp_",Sys.Date(),".csv"),
    content  = function(file) { d <- dot_data_gdp(); req(nrow(d)>0); utils::write.csv(d,file,row.names=FALSE) }
  )
  
  # ── WAGE PREMIUM (cross-country dot plot) ────────────────────────────────────
  # KEY CHANGE: public_wage_premium already has latest obs per country from the
  # processing script — this cross-country chart is correct as-is.
  
  output$dotPlot <- renderPlotly({
    req(input$countries_wage_premium)
    filtered_data <- public_wage_premium %>%
      dplyr::filter(country_name %in% input$countries_wage_premium) %>%
      dplyr::select(country_name, value_percentage, year) %>%
      tidyr::drop_na(value_percentage) %>%
      dplyr::mutate(color=ifelse(country_name==input$countries_wage_premium[1],"#B3242B","#003366"))
    if (nrow(filtered_data)==0) return(plotly_empty(type="scatter") %>% layout(title="No data available"))
    plot_ly(data=filtered_data, x=~country_name, y=~value_percentage, type="scatter", mode="markers",
            marker=list(size=10,opacity=0.8,color=~color),
            text=~paste("Country:",country_name,"<br>Value:",round(value_percentage,1),"%","<br>Year:",year)) %>%
      layout(title="Public Sector Wage Premium by Country", xaxis=list(title="Country"),
             yaxis=list(title="Wage Premium (%)"), showlegend=FALSE)
  })
  
  output$note_wage_premium <- renderText({
    "Note: Estimated public sector wage premium compared to private sector, controlling for gender, education, tenure, and location. Shows last year available."
  })
  
  output$dl_csv_wage_premium <- downloadHandler(
    filename = function() paste0("public_sector_wage_premium_",Sys.Date(),".csv"),
    content  = function(file) {
      req(input$countries_wage_premium)
      df <- public_wage_premium %>% dplyr::filter(country_name %in% input$countries_wage_premium) %>%
        dplyr::select(country_name,year,value_percentage) %>% tidyr::drop_na(value_percentage)
      utils::write.csv(df,file,row.names=FALSE,na="")
    }
  )
  
  # ── EMPLOYMENT DISTRIBUTION ──────────────────────────────────────────────────
  filtered_workforce_data <- reactive({
    req(input$countries_workforce)
    public_sector_workforce_clean %>%
      group_by(country_name,indicator_name) %>% slice_max(order_by=year,n=1) %>% ungroup()
  })
  
  output$stackedBarGraph <- renderPlotly({
    req(input$countries_workforce)
    data_to_plot <- filtered_workforce_data() %>% filter(country_name %in% input$countries_workforce)
    if (nrow(data_to_plot)==0) return(plotly_empty(type="bar") %>% layout(title="No data available"))
    color_blind_palette <- c("Public Administration"="#E69F00","Education"="#56B4E9","Health"="#009E73","Other"="#F0E442")
    plot_ly(data=data_to_plot, x=~country_name, y=~value_percentage, color=~indicator_name, type="bar",
            text=~paste("Country:",country_name,"Indicator:",indicator_name,"Value:",round(value_percentage,1),"%"),
            textposition="auto", colors=color_blind_palette) %>%
      layout(barmode="stack", title="Public Workforce Distribution by Country",
             xaxis=list(title="Country"), yaxis=list(title="Workforce Distribution (%)",range=c(0,100)))
  })
  
  output$note_stackedBarGraph <- renderText({
    "Note: Distribution of public sector employment across industries as a percentage of paid public employment. Shows last year available."
  })
  
  output$horizontalStackedBar <- renderPlotly({
    req(input$selected_country)
    filtered_data <- public_sector_workforce %>% filter(country_name==input$selected_country)
    if (nrow(filtered_data)==0) return(NULL)
    first_year <- min(filtered_data$year,na.rm=TRUE); last_year <- max(filtered_data$year,na.rm=TRUE)
    if (is.infinite(first_year)||is.infinite(last_year)) return(NULL)
    data_to_plot <- filtered_data %>% filter(year %in% c(first_year,last_year)) %>%
      group_by(year,indicator_name) %>% summarise(value_percentage=mean(value_percentage,na.rm=TRUE),.groups="drop")
    color_blind_palette <- c("Public Administration"="#E69F00","Education"="#56B4E9","Health"="#009E73","Other"="#F0E442")
    plot_ly(data=data_to_plot, x=~value_percentage, y=~factor(year,levels=c(last_year,first_year)),
            color=~indicator_name, type="bar", orientation="h",
            text=~paste0(round(value_percentage,1),"%"), textposition="inside", colors=color_blind_palette) %>%
      layout(barmode="stack",
             title=paste("Sectoral Distribution of Public Sector Workforce in",input$selected_country,"(",first_year,"&",last_year,")"),
             xaxis=list(title="Percentage (%)"), yaxis=list(title="Year"))
  })
  
  output$note_horizontalStackedBar <- renderText({
    paste0("Note: Distribution of the public sector workforce in ",input$selected_country," for earliest and latest available years.")
  })
  
  output$dl_csv_public_workforce <- downloadHandler(
    filename = function() paste0("public_workforce_data_",Sys.Date(),".csv"),
    content  = function(file) {
      req(input$countries_workforce)
      df <- public_sector_workforce %>% dplyr::filter(country_name %in% input$countries_workforce) %>%
        dplyr::select(country_name,year,indicator_name,value_percentage) %>% tidyr::drop_na(value_percentage)
      utils::write.csv(df,file,row.names=FALSE,na="")
    }
  )
  outputOptions(output,"dl_csv_public_workforce",suspendWhenHidden=FALSE)
  
  # ── TERTIARY EDUCATION ───────────────────────────────────────────────────────
  output$barPlot <- renderPlotly({
    req(input$selected_countries)
    filtered_data <- tertiary_education %>% filter(country_name %in% input$selected_countries)
    if (nrow(filtered_data)==0) return(plotly_empty(type="bar") %>% layout(title="No data available"))
    custom_colors <- c("as a share of private paid employees"="#0072B2","as a share of public paid employees"="#D55E00")
    filtered_data %>%
      plot_ly(x=~country_name, y=~value_percentage, color=~indicator_name, colors=custom_colors,
              type="bar", barmode="group",
              text=~paste("Country:",country_name,"Indicator:",indicator_name,"Value:",round(value_percentage,1),"%","Year:",year),
              textposition="auto") %>%
      layout(title="Workers with Tertiary Education by Sector and Country",
             xaxis=list(title="Country"), yaxis=list(title="Tertiary Education (%)"))
  })
  
  output$note_tertiaryEducation <- renderText({
    "Note: Proportion of individuals with tertiary education in public and private sectors. Shows last year available."
  })
  
  output$dl_csv_tertiary_edu <- downloadHandler(
    filename = function() paste0("tertiary_education_",Sys.Date(),".csv"),
    content  = function(file) {
      req(input$selected_countries)
      out <- tertiary_education %>% dplyr::filter(country_name %in% input$selected_countries) %>%
        dplyr::select(country_name,year,indicator_name,value_percentage) %>% dplyr::arrange(country_name,indicator_name,year)
      utils::write.csv(out,file,row.names=FALSE,na="")
    }
  )
  
  # ── EDUCATION WAGE PREMIUM ───────────────────────────────────────────────────
  output$education_wage_premium_plot <- renderPlotly({
    req(input$selected_country)
    filtered_data <- public_wage_premium_educ %>% filter(country_name==input$selected_country) %>% drop_na(value_percentage)
    if (nrow(filtered_data)==0) return(plotly_empty(type="bar") %>% layout(title="No data available"))
    education_colors <- c("No Education"="#E69F00","Primary Education"="#56B4E9","Secondary Education"="#009E73","Tertiary Education"="#D55E00")
    p <- ggplot(filtered_data, aes(x=indicator_name, y=value_percentage, fill=indicator_name)) +
      geom_bar(stat="identity") + scale_fill_manual(values=education_colors) +
      labs(title="Public Sector Wage Premium by Education Level", x="Education Level", y="Wage Premium (%)") + theme_minimal()
    ggplotly(p)
  })
  
  output$note_education_wage_premium <- renderText({
    "Note: Public sector wage premium across education levels compared to private formal workers. Shows last year available."
  })
  
  output$dl_csv_wagepremium_educ <- downloadHandler(
    filename = function() paste0("public_wage_premium_by_education_",Sys.Date(),".csv"),
    content  = function(file) {
      req(input$selected_country)
      df <- public_wage_premium_educ %>% dplyr::filter(country_name==input$selected_country) %>% tidyr::drop_na(value_percentage)
      utils::write.csv(df,file,row.names=FALSE,na="")
    }
  )
  
  # ── PUBLIC SECTOR EMPLOYMENT ─────────────────────────────────────────────────
  output$firstGraphpublic <- renderPlotly({
    filtered_data <- public_sector_emp_temp_last %>% filter(country_name %in% input$countries_first)
    if (nrow(filtered_data)==0) return(plotly_empty(type="scatter") %>% layout(title="No data available"))
    ggplotly(
      ggplot(filtered_data, aes(x=country_name, y=value_percentage, color=indicator_label)) +
        geom_point(size=4) +
        scale_color_manual(values=c("as a share of formal employment"="#E69F00","as a share of paid employment"="#56B4E9","as a share of total employment"="#009E73")) +
        labs(title="Public Sector Employment (Last Year Available)", x="Country", y="Value", color="Indicator") +
        theme_minimal() + theme(axis.text.x=element_text(angle=45,hjust=1))
    ) %>% layout(legend=list(title=list(text="Indicator")))
  })
  
  output$note_firstGraphpublic <- renderText({
    "Note: Relative size of public sector employment in the labor market for the latest available year."
  })
  
  # KEY CHANGE: secondGraphpublic uses public_sector_emp_temp which is the full
  # time series (not filtered to latest) — this correctly shows trend over time.
  output$secondGraphpublic <- renderPlotly({
    filtered_data <- public_sector_emp_temp %>% filter(country_name==input$country_second)
    if (nrow(filtered_data)==0) return(plotly_empty(type="scatter") %>% layout(title="No data available"))
    ggplotly(
      ggplot(filtered_data, aes(x=year, y=value_percentage, color=indicator_label)) +
        geom_line(size=1.2) + geom_point(size=3) +
        scale_color_manual(values=c("as a share of formal employment"="#E69F00","as a share of paid employment"="#56B4E9","as a share of total employment"="#009E73")) +
        labs(title="Public Sector Employment Over Time", x="Year", y="Value", color="Indicator") + theme_minimal()
    ) %>% layout(legend=list(title=list(text="Indicator")))
  })
  
  output$note_secondGraphpublic <- renderText({
    "Note: Evolution of public sector employment in the labor market over time."
  })
  
  output$dl_public_emp_data <- downloadHandler(
    filename = function() paste0("public_sector_employment_data_",Sys.Date(),".xlsx"),
    content  = function(file) {
      d1 <- public_sector_emp_temp_last %>% dplyr::filter(country_name %in% input$countries_first) %>%
        dplyr::transmute(country_name, indicator=indicator_label, year=as.numeric(year), value_percentage=as.numeric(value_percentage))
      d2 <- public_sector_emp_temp %>% dplyr::filter(country_name==input$country_second) %>%
        dplyr::transmute(country_name, indicator=indicator_label, year=as.numeric(year), value_percentage=as.numeric(value_percentage))
      writexl::write_xlsx(list("Graph1_MultiCountry_LastYear"=d1,"Graph2_SingleCountry_OverTime"=d2), path=file)
    }
  )
  
  # ── GENDER WORKFORCE ─────────────────────────────────────────────────────────
  output$firstGraphGenderWorkforce <- renderPlotly({
    req(input$countries_gender)
    d <- gender_workforce %>% dplyr::filter(country_name %in% input$countries_gender)
    if (nrow(d)==0) return(plotly_empty(type="bar") %>% layout(title="No data available"))
    d_last <- d %>% dplyr::group_by(country_name,indicator_name) %>%
      dplyr::arrange(year,.by_group=TRUE) %>% dplyr::slice_tail(n=1) %>% dplyr::ungroup()
    d_last$text <- paste0("Country: ",d_last$country_name,"<br>Sector: ",d_last$indicator_name,
                          "<br>Employment: ",round(d_last$value_percentage,1),"%<br>Year: ",d_last$year)
    plotly::ggplotly(
      ggplot(d_last, aes(x=country_name, y=value_percentage, fill=indicator_name, text=text)) +
        geom_col(position=position_dodge(width=0.8),width=0.7) +
        scale_fill_manual(values=c("as a share of private paid employees"="#E69F00","as a share of public paid employees"="#56B4E9")) +
        labs(title="Female Employment by Sector (Last Year Available)", x="Country", y="Employment (%)", fill="Sector") +
        theme_minimal(), tooltip="text"
    )
  })
  
  output$note_firstGraphGenderWorkforce <- renderText({
    "Note: Share of females employed in public and private sectors."
  })
  
  # KEY CHANGE: secondGraphGenderWorkforce uses full gender_workforce time series
  output$secondGraphGenderWorkforce <- renderPlotly({
    filtered_data <- gender_workforce %>% filter(country_name==input$country_gender)
    if (nrow(filtered_data)==0) return(plotly_empty(type="scatter") %>% layout(title="No data available"))
    ggplotly(
      ggplot(filtered_data, aes(x=year, y=value_percentage, color=indicator_name)) +
        geom_line(size=1.2) + geom_point(size=3) +
        scale_color_manual(values=c("as a share of private paid employees"="#E69F00","as a share of public paid employees"="#56B4E9")) +
        labs(title=paste("Female Employment by Sector Over Time in",input$country_gender), x="Year", y="Female Employment (%)", color="Sector") +
        theme_minimal()
    )
  })
  
  output$note_secondGraphGenderWorkforce <- renderText({
    "Note: Female employment in public and private sectors over time for the selected country."
  })
  
  output$dl_gender_workforce_xlsx <- downloadHandler(
    filename = function() paste0("female_employment_by_sector_",Sys.Date(),".xlsx"),
    content  = function(file) {
      d1 <- gender_workforce %>% dplyr::filter(country_name %in% input$countries_gender) %>%
        dplyr::group_by(country_name,indicator_name) %>% dplyr::slice_tail(n=1) %>% dplyr::ungroup() %>%
        dplyr::transmute(country_name, sector=indicator_name, year=as.numeric(year), female_share_pct=as.numeric(value_percentage))
      d2 <- gender_workforce %>% dplyr::filter(country_name==input$country_gender) %>%
        dplyr::transmute(country_name, sector=indicator_name, year=as.numeric(year), female_share_pct=as.numeric(value_percentage))
      writexl::write_xlsx(list("Graph1_MultiCountry_LastYear"=d1,"Graph2_SingleCountry_OverTime"=d2), path=file)
    }
  )
  
  # ── GENDER WAGE PREMIUM ───────────────────────────────────────────────────────
  output$firstGraphGenderWagePremium <- renderPlotly({
    filtered_data <- gender_wage_premium_last %>% filter(country_name %in% input$countries_first)
    if (nrow(filtered_data)==0) return(plotly_empty(type="scatter") %>% layout(title="No data available"))
    ggplotly(
      ggplot(filtered_data, aes(x=country_name, y=value_percentage, color=indicator_label)) +
        geom_point(size=4) +
        scale_color_manual(values=c("Male"="#E69F00","Female"="#56B4E9")) +
        labs(title="Public Sector Wage Premium by Gender (Last Year Available)", x="Country", y="Wage Premium (%)", color="Gender") +
        theme_minimal()
    )
  })
  
  output$note_firstGraphGenderWagePremium <- renderText({
    "Note: Public sector wage premium for the latest available year. Positive = higher wages in public sector."
  })
  
  # KEY CHANGE: secondGraphGenderWagePremium uses gender_wage_premium (full time series)
  output$secondGraphGenderWagePremium <- renderPlotly({
    filtered_data <- gender_wage_premium %>% filter(country_name==input$country_second)
    if (nrow(filtered_data)==0) return(plotly_empty(type="scatter") %>% layout(title="No data available"))
    ggplotly(
      ggplot(filtered_data, aes(x=year, y=value_percentage, color=indicator_label)) +
        geom_line(size=1.2) + geom_point(size=3) +
        scale_color_manual(values=c("Male"="#E69F00","Female"="#56B4E9")) +
        labs(title="Public Sector Wage Premium by Gender Over Time", x="Year", y="Wage Premium (%)", color="Gender") +
        theme_minimal()
    )
  })
  
  output$note_secondGraphGenderWagePremium <- renderText({
    "Note: Public sector wage premium for men and women over time compared to private sector."
  })
  
  output$dl_gender_wageprem_xlsx <- downloadHandler(
    filename = function() paste0("wage_premium_by_gender_",Sys.Date(),".xlsx"),
    content  = function(file) {
      d1 <- gender_wage_premium_last %>% dplyr::filter(country_name %in% input$countries_first) %>%
        dplyr::transmute(country_name, gender=indicator_label, year=as.numeric(year), wage_premium_pct=as.numeric(value_percentage))
      d2 <- gender_wage_premium %>% dplyr::filter(country_name==input$country_second) %>%
        dplyr::transmute(country_name, gender=indicator_label, year=as.numeric(year), wage_premium_pct=as.numeric(value_percentage))
      writexl::write_xlsx(list("Graph1_MultiCountry_LastYear"=d1,"Graph2_SingleCountry_OverTime"=d2), path=file)
    }
  )
  
  # ── FEMALE LEADERSHIP ────────────────────────────────────────────────────────
  output$barPlotwomen <- renderPlotly({
    if (is.null(input$selected_countries)||length(input$selected_countries)==0)
      return(plotly_empty(type="bar") %>% layout(title="No country selected"))
    filtered_data <- gender_leadership %>% dplyr::filter(country_name %in% input$selected_countries) %>%
      dplyr::mutate(indicator_label=factor(indicator_label,levels=c("Clerks-Private","Clerks-Public","Managers-Private","Managers-Public")))
    if (nrow(filtered_data)==0) return(plotly_empty(type="bar") %>% layout(title="No data available"))
    cols <- c("Clerks-Private"="#9ECAE1","Clerks-Public"="#08519C","Managers-Private"="#FDAE6B","Managers-Public"="#E6550D")
    plot_ly(data=filtered_data, x=~country_name, y=~value_percentage, color=~indicator_label, colors=cols,
            type="bar", barmode="group",
            text=~paste0("Country: ",country_name,"<br>Group: ",indicator_label,"<br>Female Share: ",round(value_percentage,1),"%"),
            hoverinfo="text") %>%
      layout(title="Females by Occupational Group and Sector", xaxis=list(title="Country"), yaxis=list(title="Female Share (%)"))
  })
  
  output$note_barPlotwomen <- renderText({
    "Note: Share of females in Managers/Clerks in public and private sectors. Shows last year available."
  })
  
  output$dl_csv_female_leadership <- downloadHandler(
    filename = function() paste0("female_leadership_occupation_sector_",Sys.Date(),".csv"),
    content  = function(file) {
      if (is.null(input$selected_countries)||!length(input$selected_countries)) {
        utils::write.csv(data.frame(), file, row.names=FALSE); return()
      }
      d <- gender_leadership %>% dplyr::filter(country_name %in% input$selected_countries) %>%
        dplyr::group_by(country_name,indicator_label) %>% dplyr::slice_max(order_by=year,n=1,with_ties=FALSE) %>% dplyr::ungroup() %>%
        dplyr::transmute(country_name=as.character(country_name), group_sector=as.character(indicator_label), year=as.integer(year), female_share_pct=as.numeric(value_percentage))
      utils::write.csv(d,file,row.names=FALSE,na="")
    }
  )
  
  # ── GENDER WAGE PREMIUM BY INDUSTRY ──────────────────────────────────────────
  output$gender_wage_barplot <- renderPlot({
    filtered_data <- gender_wage_premiumpublic %>%
      filter(country_name %in% input$selected_countries, indicator_label %in% c("Public Administration","Education","Health","Other"))
    if (nrow(filtered_data)==0) return(ggplot()+theme_void()+annotate("text",x=0.5,y=0.5,label="No data available",size=6,color="grey"))
    ggplot(filtered_data, aes(x=country_name, y=value_percentage, fill=indicator_label)) +
      geom_bar(stat="identity",position="dodge") + scale_fill_viridis_d(name="Indicator",option="D") +
      labs(title="Gender Wage Premium in Public Sector by Industry", x="Country", y="Wage Premium (%)") + theme_minimal()
  })
  
  output$note_gender_wage_barplot <- renderText({
    "Note: Gender wage premium in the public sector across industries, comparing female to male employees."
  })
  
  output$dl_csv_gender_wage_industry <- downloadHandler(
    filename = function() paste0("gender_wage_premium_public_by_industry_",Sys.Date(),".csv"),
    content  = function(file) {
      if (is.null(input$selected_countries)||!length(input$selected_countries)) {
        utils::write.csv(data.frame(),file,row.names=FALSE,na=""); return()
      }
      d <- gender_wage_premiumpublic %>% dplyr::filter(country_name %in% input$selected_countries, indicator_label %in% c("Public Administration","Education","Health","Other")) %>%
        dplyr::group_by(country_name,indicator_label) %>% dplyr::slice_max(order_by=year,n=1,with_ties=FALSE) %>% dplyr::ungroup() %>%
        dplyr::transmute(country_name, industry=indicator_label, year=as.integer(year), wage_premium_pct=as.numeric(value_percentage))
      utils::write.csv(d,file,row.names=FALSE,na="")
    }
  )
  
  # ── PAY COMPRESSION ───────────────────────────────────────────────────────────
  output$paycompression_plot <- renderPlotly({
    req(input$countries_first)
    filtered_data_df <- pay_compression_wide %>% dplyr::filter(country_name %in% input$countries_first) %>%
      dplyr::mutate(Public_Sector=as.numeric(unlist(Public_Sector)), Private_Sector=as.numeric(unlist(Private_Sector))) %>%
      tidyr::drop_na(Public_Sector,Private_Sector)
    if (nrow(filtered_data_df)==0) return(plotly_empty(type="scatter") %>% layout(title="No data available"))
    filtered_data_df <- filtered_data_df %>% dplyr::mutate(color=ifelse(country_name==input$countries_first[1],"#B3242B","#003366"))
    p <- plot_ly() %>% add_trace(data=filtered_data_df, x=~Private_Sector, y=~Public_Sector,
                                 type="scatter", mode="markers+text", text=~country_name, textposition="top center",
                                 marker=list(size=10,color=~color,opacity=0.7), name="Country")
    if (nrow(filtered_data_df)>=2 && sd(filtered_data_df$Private_Sector,na.rm=TRUE)>0) {
      m <- lm(Public_Sector~Private_Sector, data=filtered_data_df)
      x_seq <- seq(min(filtered_data_df$Private_Sector,na.rm=TRUE), max(filtered_data_df$Private_Sector,na.rm=TRUE), length.out=50)
      p <- p %>% add_trace(x=x_seq, y=predict(m,newdata=data.frame(Private_Sector=x_seq)), type="scatter", mode="lines",
                           line=list(color="gray",dash="dash"), name="Trendline")
    }
    p %>% layout(title="Pay Compression: Public vs. Private Sector (Latest Year)",
                 xaxis=list(title="Private Sector Pay Compression"), yaxis=list(title="Public Sector Pay Compression"),
                 plot_bgcolor="white", paper_bgcolor="white")
  })
  
  output$note_dotplot_pay <- renderText({
    "Note: Pay compression ratios (P90/P10). Higher values = wider wage dispersion. Shows last year available."
  })
  
  output$dl_csv_pay_compression <- downloadHandler(
    filename = function() paste0("pay_compression_public_vs_private_",Sys.Date(),".csv"),
    content  = function(file) {
      if (is.null(input$countries_first)||!length(input$countries_first)) {
        utils::write.csv(data.frame(),file,row.names=FALSE,na=""); return()
      }
      d <- pay_compression_wide %>% dplyr::filter(country_name %in% input$countries_first) %>%
        dplyr::transmute(country_name, private_sector=as.numeric(Private_Sector), public_sector=as.numeric(Public_Sector))
      utils::write.csv(d,file,row.names=FALSE,na="")
    }
  )
  
  # ── METADATA MAP ─────────────────────────────────────────────────────────────
  output$worldMap <- renderLeaflet({
    leaflet(world_spdf) %>% addTiles() %>% setView(lng=0,lat=20,zoom=2) %>%
      addLegend(position="bottomright", colors=c("gray","#6DA96F"), labels=c("No Data","Reported"),
                title="Indicator Availability", opacity=1)
  })
  
  filtered_data_for_map <- reactive({
    req(input$indicatorSelect)
    data_wwbi %>% filter(indicator_name==input$indicatorSelect) %>%
      mutate(any_data=apply(select(.,starts_with("year_")),1,function(x) any(!is.na(x)))) %>%
      filter(any_data) %>% transmute(country_name,indicator_name,has_data=1)
  })
  
  observe({
    req(input$indicatorSelect)
    reported_countries <- filtered_data_for_map()
    if (nrow(reported_countries)==0) return()
    world_data_merged <- world_spdf %>% left_join(reported_countries,by=c("name_long"="country_name"))
    color_pal <- colorFactor(palette=c("gray","#6DA96F"),domain=c(0,1))
    leafletProxy("worldMap") %>% clearShapes() %>%
      addPolygons(data=world_data_merged,
                  fillColor=~color_pal(ifelse(is.na(has_data),0,has_data)), fillOpacity=0.7,
                  color="white", weight=1,
                  highlightOptions=highlightOptions(color="#FFD700",weight=2,fillOpacity=0.9),
                  label=~paste0("Country: ",name_long," - ",ifelse(!is.na(has_data),"Reported","No Data")))
    output$countryCount <- renderText({ paste("Total Countries with Data:",nrow(reported_countries)) })
  })
  
  # ── PUBLICATIONS & DOWNLOADS ──────────────────────────────────────────────────
  output$download_pdf <- downloadHandler(filename="Codebook and Explanatory Note.pdf",
                                         content=function(file) file.copy(file.path(data_path,"Files","WWBI Codebook v3.1.pdf"),file))
  output$pub1 <- downloadHandler(filename=function() "Innovating-Bureaucracy-for-a-More-Capable-Government.pdf",
                                 content=function(file) file.copy(file.path(data_path,"Files","Innovating-Bureaucracy-for-a-More-Capable-Government.pdf"),file))
  output$pub2 <- downloadHandler(filename=function() "WWBI-Introduction.pdf",
                                 content=function(file) file.copy(file.path(data_path,"Files","Public Administration Review - 2021 - Baig - Introducing the Worldwide Bureaucracy Indicators  A New Global Dataset on.pdf"),file))
  output$pub3 <- downloadHandler(filename=function() "Public-Sector-Employment-and-Compensation-An-Assessment-Framework.pdf",
                                 content=function(file) file.copy(file.path(data_path,"Files","Public-Sector-Employment-and-Compensation-An-Assessment-Framework.pdf"),file))
  output$pub4 <- downloadHandler(filename=function() "Worldwide-Bureaucracy-Indicators-Methodology-Insights-and-Applications.pdf",
                                 content=function(file) file.copy(file.path(data_path,"Files","Worldwide-Bureaucracy-Indicators-Methodology-Insights-and-Applications.pdf"),file))
  
  # ── DOWNLOAD HANDLERS (Word reports) — kept from original code ───────────────
  # These pass through to the generate_* helper functions defined below.
  
  output$downloadWord <- downloadHandler(
    filename=function() paste0("Wage_Bill_Analysis_",Sys.Date(),".docx"),
    content=function(file) {
      req(input$countries)
      first_country <- if (!is.null(input$countries)&&length(input$countries)>0) input$countries[1] else "Unknown"
      doc <- officer::read_docx()
      title_style <- fp_text(color="#722F37",font.size=16,bold=TRUE)
      doc <- doc %>% body_add_fpar(fpar(ftext(paste("Wage Bill Analysis Report -",first_country),prop=title_style)))
      graph_data <- if (input$graph_choice=="GDP") wage_bill_gdp else wage_bill_publicexp
      graph_data <- graph_data %>% filter(country_name %in% input$countries)
      if (nrow(graph_data)>0) {
        g <- ggplot(graph_data, aes(x=year,y=value,color=country_name)) + geom_line(size=1.2) + geom_point(size=3) +
          labs(title=ifelse(input$graph_choice=="GDP","Wage Bill as % of GDP","Wage Bill as % of Public Expenditure"),
               x="Year", y="Value") + theme_minimal()
        doc <- doc %>% body_add_gg(value=g,style="centered")
      }
      print(doc,target=file)
    }
  )
  
  output$downloadGDPDoc <- downloadHandler(
    filename=function() paste0("Wage_Bill_vs_GDP_Report_",Sys.Date(),".docx"),
    content=function(file) {
      req(input$countries_gdp)
      d <- merged_data %>% dplyr::filter(country_name %in% input$countries_gdp)
      req(nrow(d)>0)
      doc <- officer::read_docx() %>% body_add_par("Wage Bill vs. GDP Analysis",style="heading 1")
      p <- ggplot(d,aes(x=log_gdp,y=indicator_value,color=country_name)) + geom_point(size=3) +
        geom_smooth(method="lm",se=FALSE,color="gray50",linetype="dashed") +
        labs(title="Wage Bill vs. Log(GDP per Capita)", x="Log(GDP per Capita, 2015)", y="Wage Bill (%)") + theme_minimal()
      doc <- doc %>% body_add_gg(value=p,width=6.5,height=4.5)
      print(doc,target=file)
    }
  )
  
  output$downloadWagePremiumReport <- downloadHandler(
    filename=function() paste0("Public_Sector_Wage_Premium_Report_",Sys.Date(),".docx"),
    content=function(file) {
      req(input$countries_wage_premium)
      doc <- officer::read_docx() %>% body_add_par("Public Sector Wage Premium Analysis",style="heading 1")
      d <- public_wage_premium %>% dplyr::filter(country_name %in% input$countries_wage_premium) %>% tidyr::drop_na(value_percentage)
      if (nrow(d)>0) {
        p <- ggplot(d,aes(x=country_name,y=value_percentage,color=country_name)) + geom_point(size=4) +
          labs(title="Public Sector Wage Premium by Country", x="Country", y="Wage Premium (%)") +
          theme_minimal() + theme(axis.text.x=element_text(angle=45,hjust=1))
        doc <- doc %>% body_add_gg(value=p,width=6.5,height=4.5)
      }
      print(doc,target=file)
    }
  )
  
  output$downloadEducationWagePremium <- downloadHandler(
    filename=function() paste0("Public_Sector_Wage_Premium_Education_Level_",Sys.Date(),".docx"),
    content=function(file) {
      req(input$selected_country)
      d <- public_wage_premium_educ %>% dplyr::filter(country_name==input$selected_country) %>% tidyr::drop_na(value_percentage)
      req(nrow(d)>0)
      doc <- officer::read_docx() %>% body_add_par(paste0("Wage Premium by Education – ",input$selected_country),style="heading 1")
      p <- ggplot(d,aes(x=indicator_name,y=value_percentage,fill=indicator_name)) + geom_col(width=0.7) +
        scale_fill_manual(values=c("No Education"="#E69F00","Primary Education"="#56B4E9","Secondary Education"="#009E73","Tertiary Education"="#D55E00")) +
        labs(title="Wage Premium by Education Level", x=NULL, y="Wage Premium (%)") + theme_minimal()
      doc <- doc %>% body_add_gg(value=p,width=6.5,height=4.5)
      print(doc,target=file)
    }
  )
  
  output$downloadGraphsWord <- downloadHandler(
    filename=function() paste0("Public_Sector_Employment_",Sys.Date(),".docx"),
    content=function(file) {
      req(input$countries_first,input$country_second)
      doc <- officer::read_docx() %>% body_add_par("Public Sector Employment Analysis",style="heading 1")
      d1 <- public_sector_emp_temp_last %>% dplyr::filter(country_name %in% input$countries_first)
      d2 <- public_sector_emp_temp     %>% dplyr::filter(country_name==input$country_second)
      if (nrow(d1)>0) {
        p1 <- ggplot(d1,aes(x=country_name,y=value_percentage,color=indicator_label)) + geom_point(size=4) +
          labs(title="Public Sector Employment (Last Year)", x="Country", y="Value (%)") + theme_minimal()
        doc <- doc %>% body_add_gg(value=p1,width=6.5,height=4.5)
      }
      if (nrow(d2)>0) {
        p2 <- ggplot(d2,aes(x=year,y=value_percentage,color=indicator_label,group=indicator_label)) +
          geom_line(size=1.1) + geom_point(size=2.8) +
          labs(title=paste0("Public Sector Employment Over Time — ",input$country_second), x="Year", y="Value (%)") + theme_minimal()
        doc <- doc %>% body_add_gg(value=p2,width=6.5,height=4.5)
      }
      print(doc,target=file)
    }
  )
  
  output$downloadGraphsWordGender <- downloadHandler(
    filename=function() paste0("Female_Share_of_Employment_",Sys.Date(),".docx"),
    content=function(file) {
      req(input$countries_gender,input$country_gender)
      doc <- officer::read_docx() %>% body_add_par("Female Share of Employment",style="heading 1")
      d1 <- gender_workforce %>% dplyr::filter(country_name %in% input$countries_gender) %>%
        dplyr::group_by(country_name,indicator_name) %>% dplyr::slice_tail(n=1) %>% dplyr::ungroup()
      d2 <- gender_workforce %>% dplyr::filter(country_name==input$country_gender)
      if (nrow(d1)>0) {
        p1 <- ggplot(d1,aes(x=country_name,y=value_percentage,fill=indicator_name)) + geom_col(position=position_dodge(0.8),width=0.7) +
          scale_fill_manual(values=c("as a share of private paid employees"="#E69F00","as a share of public paid employees"="#56B4E9")) +
          labs(title="Female Employment by Sector (Last Year)", x="Country", y="Employment (%)") + theme_minimal()
        doc <- doc %>% body_add_gg(value=p1,width=6.5,height=4.5)
      }
      if (nrow(d2)>0) {
        p2 <- ggplot(d2,aes(x=year,y=value_percentage,color=indicator_name,group=indicator_name)) +
          geom_line(size=1.1) + geom_point(size=2.8) +
          scale_color_manual(values=c("as a share of private paid employees"="#E69F00","as a share of public paid employees"="#56B4E9")) +
          labs(title=paste0("Female Employment Over Time — ",input$country_gender), x="Year", y="Employment (%)") + theme_minimal()
        doc <- doc %>% body_add_gg(value=p2,width=6.5,height=4.5)
      }
      print(doc,target=file)
    }
  )
  
  output$downloadGraphswagepremiumbygender <- downloadHandler(
    filename=function() paste0("Wage_Premium_Gender_Graphs_",Sys.Date(),".docx"),
    content=function(file) {
      doc <- officer::read_docx() %>% body_add_par("Public Sector Wage Premium by Gender",style="heading 1")
      if (!is.null(input$countries_first)&&length(input$countries_first)>0) {
        d1 <- gender_wage_premium_last %>% dplyr::filter(country_name %in% input$countries_first)
        if (nrow(d1)>0) {
          p1 <- ggplot(d1,aes(x=country_name,y=value_percentage,color=indicator_label)) + geom_point(size=4) +
            scale_color_manual(values=c("Male"="#E69F00","Female"="#56B4E9")) +
            labs(title="Wage Premium by Gender (Last Year)", x="Country", y="Wage Premium (%)") + theme_minimal()
          doc <- doc %>% body_add_gg(value=p1,width=6.5,height=4.5)
        }
      }
      if (isTruthy(input$country_second)) {
        d2 <- gender_wage_premium %>% dplyr::filter(country_name==input$country_second)
        if (nrow(d2)>0) {
          p2 <- ggplot(d2,aes(x=year,y=value_percentage,color=indicator_label,group=indicator_label)) +
            geom_line(size=1) + geom_point(size=2.8) +
            scale_color_manual(values=c("Male"="#E69F00","Female"="#56B4E9")) +
            labs(title=paste0("Wage Premium Over Time — ",input$country_second), x="Year", y="Wage Premium (%)") + theme_minimal()
          doc <- doc %>% body_add_gg(value=p2,width=6.5,height=4.5)
        }
      }
      print(doc,target=file)
    }
  )
  
  output$downloadGraphsWordfemale <- downloadHandler(
    filename=function() paste0("Females_Occupation_Groups_Analysis_",Sys.Date(),".docx"),
    content=function(file) {
      req(input$selected_countries)
      d <- gender_leadership %>% dplyr::filter(country_name %in% input$selected_countries)
      req(nrow(d)>0)
      cols <- c("Clerks-Private"="#9ECAE1","Clerks-Public"="#08519C","Managers-Private"="#FDAE6B","Managers-Public"="#E6550D")
      p <- ggplot(d,aes(x=country_name,y=value_percentage,fill=indicator_label)) +
        geom_col(position=position_dodge(0.8),width=0.7) + scale_fill_manual(values=cols) +
        labs(title="Females by Occupational Group and Sector", x="Country", y="Female Share (%)") +
        theme_minimal() + theme(axis.text.x=element_text(angle=45,hjust=1))
      doc <- officer::read_docx() %>% body_add_par("Females by Occupational Group and Sector",style="heading 1") %>% body_add_gg(value=p,width=6.5,height=4.5)
      print(doc,target=file)
    }
  )
  
  output$downloadGenderWagePremium <- downloadHandler(
    filename=function() paste0("Gender_Wage_Premium_Report_",Sys.Date(),".docx"),
    content=function(file) {
      req(input$selected_countries)
      d <- gender_wage_premiumpublic %>% dplyr::filter(country_name %in% input$selected_countries, indicator_label %in% c("Public Administration","Education","Health"))
      req(nrow(d)>0)
      p <- ggplot(d,aes(x=country_name,y=value_percentage,fill=indicator_label)) +
        geom_col(position=position_dodge(0.8),width=0.7) + scale_fill_viridis_d(option="D") +
        labs(title="Gender Wage Premium in Public Sector by Industry", x="Country", y="Wage Premium (%)") +
        theme_minimal() + theme(axis.text.x=element_text(angle=45,hjust=1))
      doc <- officer::read_docx() %>% body_add_par("Gender Wage Premium by Industry",style="heading 1") %>% body_add_gg(value=p,width=6.5,height=4.5)
      print(doc,target=file)
    }
  )
  
  output$downloadPayCompressionDoc <- downloadHandler(
    filename=function() paste0("Pay_Compression_Ratios_Report_",Sys.Date(),".docx"),
    content=function(file) {
      req(input$countries_first)
      d <- pay_compression_wide %>% dplyr::filter(country_name %in% input$countries_first) %>%
        dplyr::mutate(Public_Sector=as.numeric(unlist(Public_Sector)),Private_Sector=as.numeric(unlist(Private_Sector))) %>%
        tidyr::drop_na(Public_Sector,Private_Sector)
      req(nrow(d)>0)
      p <- ggplot(d,aes(x=Private_Sector,y=Public_Sector,label=country_name)) + geom_point(size=3) +
        ggrepel::geom_text_repel(size=3,color="black") +
        geom_smooth(method="lm",se=FALSE,color="gray50",linetype="dashed") +
        labs(title="Pay Compression: Public vs. Private Sector", x="Private Sector (P90/P10)", y="Public Sector (P90/P10)") + theme_minimal()
      doc <- officer::read_docx() %>% body_add_par("Pay Compression Ratios",style="heading 1") %>% body_add_gg(value=p,width=6.5,height=4.5)
      print(doc,target=file)
    }
  )
  
  output$downloadGraphsemploymentdist <- downloadHandler(
    filename=function() paste0("Employment_Distribution_Analysis_",Sys.Date(),".docx"),
    content=function(file) {
      doc <- officer::read_docx() %>% body_add_par("Employment Distribution Analysis",style="heading 1")
      fgd <- filtered_workforce_data() %>% dplyr::filter(country_name %in% input$countries_workforce)
      if (nrow(fgd)>0) {
        p1 <- ggplot(fgd,aes(x=country_name,y=value_percentage,fill=indicator_name)) + geom_bar(stat="identity",position="stack") +
          scale_fill_viridis_d(option="D") + labs(title="Employment distribution by country", x="Country", y="Employment distribution (%)") + theme_minimal()
        doc <- doc %>% body_add_gg(value=p1,width=6,height=4)
      }
      print(doc,target=file)
    }
  )
  
  # ── COMBINED WORD & PPT REPORTS ───────────────────────────────────────────────
  # Helper: generate intro
  generate_intro_section <- function(doc, selected_countries) {
    first_country <- if (!is.null(selected_countries)&&length(selected_countries)>0) selected_countries[1] else "Unknown Country"
    first_region  <- tryCatch(countrycode(first_country,origin="country.name",destination="region"), error=function(e) "its region")
    if (is.na(first_region)) first_region <- "its region"
    title_style    <- fp_text(color="#722F37",font.size=20,bold=TRUE)
    subtitle_style <- fp_text(color="black",font.size=16,bold=TRUE)
    doc <- doc %>%
      body_add_fpar(fpar(ftext(first_country,prop=title_style))) %>%
      body_add_fpar(fpar(ftext("Wage Bill and Public Employment Analysis",prop=subtitle_style))) %>%
      body_add_par(paste0("This note presents evidence on public sector employment and compensation practices in ",first_country,
                          " using the Worldwide Bureaucracy Indicators (WWBI). For international comparisons, peer countries from ",
                          first_region," are included."), style="Normal")
    return(doc)
  }
  
  output$downloadAllGraphsDoc <- downloadHandler(
    filename=function() paste0("Wage_bill_and_public_employment_analysis_",Sys.Date(),".docx"),
    content=function(file) {
      selected_countries <- sanitize_vec(input$download_report_countries)
      doc <- officer::read_docx()
      title_style   <- officer::fp_text(color="#722F37",font.size=20,bold=TRUE)
      section_style <- officer::fp_text(color="#003366",font.size=14,bold=TRUE)
      doc <- doc %>% officer::body_add_fpar(officer::fpar(officer::ftext("Worldwide Bureaucracy Indicators Analysis",prop=title_style)))
      doc <- generate_intro_section(doc,selected_countries)
      # Add each section with simple graphs
      sections <- list(
        list(title="Macro-Fundamentals of the Public Sector", data=wage_bill_publicexp %>% filter(country_name %in% selected_countries),
             aes_x="year", aes_y="value", color="country_name", type="line", plot_title="Wage Bill as % of Public Expenditure"),
        list(title="Public Sector Employment", data=public_sector_emp_temp_last %>% filter(country_name %in% selected_countries),
             aes_x="country_name", aes_y="value_percentage", color="indicator_label", type="point", plot_title="Public Sector Employment (Last Year)"),
        list(title="Tertiary Education", data=tertiary_education %>% filter(country_name %in% selected_countries),
             aes_x="country_name", aes_y="value_percentage", color="indicator_name", type="bar", plot_title="Workers with Tertiary Education"),
        list(title="Wage Premium", data=public_wage_premium %>% filter(country_name %in% selected_countries),
             aes_x="country_name", aes_y="value_percentage", color=NULL, type="point", plot_title="Public Sector Wage Premium"),
        list(title="Female Employment", data=gender_workforce %>% filter(country_name %in% selected_countries) %>% group_by(country_name,indicator_name) %>% slice_tail(n=1) %>% ungroup(),
             aes_x="country_name", aes_y="value_percentage", color="indicator_name", type="bar", plot_title="Female Employment by Sector")
      )
      for (s in sections) {
        doc <- doc %>% officer::body_add_fpar(officer::fpar(officer::ftext(s$title,prop=section_style)))
        if (!is.null(s$data)&&nrow(s$data)>0) {
          p <- if (s$type=="line") {
            ggplot(s$data,aes_string(x=s$aes_x,y=s$aes_y,color=s$color))+geom_line(size=1)+geom_point(size=2)
          } else if (s$type=="bar") {
            ggplot(s$data,aes_string(x=s$aes_x,y=s$aes_y,fill=s$color))+geom_col(position=position_dodge(0.8),width=0.7)
          } else {
            ggplot(s$data,aes_string(x=s$aes_x,y=s$aes_y,color=s$color))+geom_point(size=3)
          }
          p <- p + labs(title=s$plot_title)+theme_minimal()+theme(axis.text.x=element_text(angle=45,hjust=1))
          img <- tempfile(fileext=".png"); ggsave(img,plot=p,width=7,height=4.5,dpi=300)
          doc <- doc %>% officer::body_add_img(src=img,width=6.5,height=4)
        }
      }
      print(doc,target=file)
    }
  )
  
  output$downloadSelectedGraphsDoc <- downloadHandler(
    filename=function() paste0("Wage_bill_and_public_employment_analysis_Selected_Report_",Sys.Date(),".docx"),
    content=function(file) {
      selected_countries <- input$download_report_countries
      selected_sections  <- input$selected_graphs
      doc <- officer::read_docx()
      title_style <- fp_text(color="#722F37",font.size=20,bold=TRUE)
      doc <- doc %>% body_add_fpar(fpar(ftext("Wage bill and public employment analysis",prop=title_style)))
      doc <- generate_intro_section(doc,selected_countries)
      if (is.null(selected_sections)||length(selected_sections)==0) {
        doc <- doc %>% body_add_par("No sections selected for download.",style="Normal")
      } else {
        add_simple_graph <- function(doc, d, title, x, y, color=NULL, type="point") {
          doc <- doc %>% body_add_par(title,style="heading 2")
          if (!is.null(d)&&nrow(d)>0) {
            p <- if (type=="line") ggplot(d,aes_string(x=x,y=y,color=color))+geom_line(size=1)+geom_point(size=2)
            else if (type=="bar") ggplot(d,aes_string(x=x,y=y,fill=color))+geom_col(position=position_dodge(0.8),width=0.7)
            else ggplot(d,aes_string(x=x,y=y,color=color))+geom_point(size=3)
            p <- p+labs(title=title)+theme_minimal()+theme(axis.text.x=element_text(angle=45,hjust=1))
            img <- tempfile(fileext=".png"); ggsave(img,plot=p,width=7,height=4.5,dpi=300)
            doc <- doc %>% body_add_img(src=img,width=6.5,height=4)
          }
          return(doc)
        }
        if ("wagebill"            %in% selected_sections) doc <- add_simple_graph(doc, wage_bill_publicexp %>% filter(country_name %in% selected_countries), "Wage Bill as % of Public Expenditure","year","value","country_name","line")
        if ("wagebill_gdp"        %in% selected_sections) doc <- add_simple_graph(doc, merged_data %>% filter(country_name %in% selected_countries), "Wage Bill vs GDP","log_gdp","indicator_value","country_name","point")
        if ("tertiaryeducation"   %in% selected_sections) doc <- add_simple_graph(doc, tertiary_education %>% filter(country_name %in% selected_countries), "Tertiary Education","country_name","value_percentage","indicator_name","bar")
        if ("public_employment"   %in% selected_sections) doc <- add_simple_graph(doc, public_sector_emp_temp_last %>% filter(country_name %in% selected_countries), "Public Sector Employment","country_name","value_percentage","indicator_label","point")
        if ("wagepremium"         %in% selected_sections) doc <- add_simple_graph(doc, public_wage_premium %>% filter(country_name %in% selected_countries), "Wage Premium","country_name","value_percentage",NULL,"point")
        if ("gender_workforce"    %in% selected_sections) doc <- add_simple_graph(doc, gender_workforce %>% filter(country_name %in% selected_countries) %>% group_by(country_name,indicator_name) %>% slice_tail(n=1) %>% ungroup(), "Female Employment by Sector","country_name","value_percentage","indicator_name","bar")
        if ("gender_wage_premium" %in% selected_sections) doc <- add_simple_graph(doc, gender_wage_premiumpublic %>% filter(country_name %in% selected_countries,indicator_label %in% c("Public Administration","Education","Health")), "Gender Wage Premium by Industry","country_name","value_percentage","indicator_label","bar")
        if ("pay_compression"     %in% selected_sections) doc <- add_simple_graph(doc, pay_compression_wide %>% filter(country_name %in% selected_countries) %>% mutate(Public_Sector=as.numeric(unlist(Public_Sector)),Private_Sector=as.numeric(unlist(Private_Sector))), "Pay Compression","Private_Sector","Public_Sector","country_name","point")
      }
      print(doc,target=file)
    }
  )
  
  output$downloadSelectedGraphsPPT <- downloadHandler(
    filename=function() paste0("Wage_bill_and_public_employment_analysis_Selected_Presentation_",Sys.Date(),".pptx"),
    content=function(file) {
      selected_countries <- input$download_report_countries
      selected_sections  <- input$selected_graphs
      ppt <- officer::read_pptx() %>%
        officer::add_slide(layout="Title Slide",master="Office Theme") %>%
        officer::ph_with("Worldwide Bureaucracy Indicators",location=officer::ph_location_type(type="ctrTitle")) %>%
        officer::ph_with(paste("Generated on",Sys.Date()),location=officer::ph_location_type(type="subTitle"))
      add_slide_graph <- function(ppt, d, title, x, y, color=NULL, type="point") {
        if (is.null(d)||nrow(d)==0) return(ppt)
        p <- if (type=="line") ggplot(d,aes_string(x=x,y=y,color=color))+geom_line(size=1)+geom_point(size=2)
        else if (type=="bar") ggplot(d,aes_string(x=x,y=y,fill=color))+geom_col(position=position_dodge(0.8),width=0.7)
        else ggplot(d,aes_string(x=x,y=y,color=color))+geom_point(size=3)
        p <- p+labs(title=title)+theme_minimal()+theme(axis.text.x=element_text(angle=45,hjust=1))
        img <- tempfile(fileext=".png"); ggsave(img,plot=p,width=8,height=5,dpi=300)
        ppt %>% officer::add_slide(layout="Title and Content",master="Office Theme") %>%
          officer::ph_with(officer::external_img(img,height=5,width=7),location=officer::ph_location_type(type="body"))
      }
      if (!is.null(selected_sections)&&length(selected_sections)>0) {
        if ("wagebill"            %in% selected_sections) ppt <- add_slide_graph(ppt, wage_bill_publicexp %>% filter(country_name %in% selected_countries), "Wage Bill as % of Public Expenditure","year","value","country_name","line")
        if ("wagebill_gdp"        %in% selected_sections) ppt <- add_slide_graph(ppt, merged_data %>% filter(country_name %in% selected_countries), "Wage Bill vs GDP","log_gdp","indicator_value","country_name","point")
        if ("tertiaryeducation"   %in% selected_sections) ppt <- add_slide_graph(ppt, tertiary_education %>% filter(country_name %in% selected_countries), "Tertiary Education","country_name","value_percentage","indicator_name","bar")
        if ("public_employment"   %in% selected_sections) ppt <- add_slide_graph(ppt, public_sector_emp_temp_last %>% filter(country_name %in% selected_countries), "Public Sector Employment","country_name","value_percentage","indicator_label","point")
        if ("wagepremium"         %in% selected_sections) ppt <- add_slide_graph(ppt, public_wage_premium %>% filter(country_name %in% selected_countries), "Wage Premium","country_name","value_percentage",NULL,"point")
        if ("gender_workforce"    %in% selected_sections) ppt <- add_slide_graph(ppt, gender_workforce %>% filter(country_name %in% selected_countries) %>% group_by(country_name,indicator_name) %>% slice_tail(n=1) %>% ungroup(), "Female Employment","country_name","value_percentage","indicator_name","bar")
        if ("gender_wage_premium" %in% selected_sections) ppt <- add_slide_graph(ppt, gender_wage_premiumpublic %>% filter(country_name %in% selected_countries,indicator_label %in% c("Public Administration","Education","Health")), "Gender Wage Premium by Industry","country_name","value_percentage","indicator_label","bar")
        if ("pay_compression"     %in% selected_sections) ppt <- add_slide_graph(ppt, pay_compression_wide %>% filter(country_name %in% selected_countries) %>% mutate(Public_Sector=as.numeric(unlist(Public_Sector)),Private_Sector=as.numeric(unlist(Private_Sector))), "Pay Compression","Private_Sector","Public_Sector","country_name","point")
      }
      print(ppt,target=file)
    }
  )
  
}

shinyApp(ui=ui, server=server)
