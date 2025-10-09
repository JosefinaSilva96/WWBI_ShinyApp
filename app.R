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

### Load data sets ----

# Load the data sets

data_path <- file.path(getwd()) 

# Automatically detect the correct root folder (without "Code/")
if (basename(getwd()) == "Code") {
  data_path <- dirname(getwd())  # Move one level up if inside "Code/"
} else {
  data_path <- getwd()  # Use current directory if already correct
}

# Debugging: Print the detected data path to check correctness
print(paste("Using data path:", data_path))



data_wwbi     <- read_dta(file.path(data_path, "Data/data_wwbi.dta"))


data_gdp     <- read_dta(file.path(data_path, "Data/data_gdp.dta"))

gdp_2015     <- read_dta(file.path(data_path, "Data/gdp_2015.dta"))


world_spdf <- st_read(file.path(data_path, "Data/world_spatial.gpkg"))


selected_data_long     <- read_dta(file.path(data_path, "Data/selected_data_long.dta"))


data_wwbi_long     <- read_dta(file.path(data_path, "Data/data_wwbi_long.dta"))


wage_bill_publicexp     <- read_dta(file.path(data_path, "Data/wage_bill_publicexp.dta"))


wage_bill_gdp     <- read_dta(file.path(data_path, "Data/wage_bill_gdp.dta"))

public_sector_emp_temp <- readRDS(file.path(data_path, "Data", "public_sector_emp_temp.rds"))

public_sector_emp_temp <- readRDS(file.path(data_path, "Data", "public_sector_emp_temp.rds")) %>%
  mutate(
    across(where(~inherits(.x, "haven_labelled")), as_factor)  # Convert haven-labelled to factors
  )

public_sector_emp <- readRDS(file.path(data_path, "Data", "public_sector_emp.rds"))

public_sector_emp_temp_last <- readRDS(file.path(data_path, "Data", "public_sector_emp_temp_last.rds")) %>%
  mutate(
    across(where(~inherits(.x, "haven_labelled")), as_factor)  # Convert haven-labelled to factors
  )

public_sector_workforce_clean     <- read_dta(file.path(data_path, "Data/public_sector_workforce_clean.dta"))


public_sector_workforce     <- read_dta(file.path(data_path, "Data/public_sector_workforce.dta"))

public_sector_workforce_first_last     <- read_dta(file.path(data_path, "Data/public_sector_workforce_first_last.dta"))

gender_workforce     <- read_dta(file.path(data_path, "Data/gender_workforce.dta"))


data_indicator_wb     <- read_dta(file.path(data_path, "Data/data_indicator_wb.dta"))

merged_data     <- read_dta(file.path(data_path, "Data/merged_data.dta"))

tertiary_education     <- read_dta(file.path(data_path, "Data/tertiary_education.dta"))

public_wage_premium     <- read_dta(file.path(data_path, "Data/public_wage_premium.dta"))

public_wage_premium_educ     <- read_dta(file.path(data_path, "Data/public_wage_premium_educ.dta"))


gender_wage_premium <- readRDS(file.path(data_path, "Data", "gender_wage_premium.rds"))


gender_wage_premium_last <- readRDS(file.path(data_path, "Data", "gender_wage_premium_last.rds"))


gender_leadership <- readRDS(file.path(data_path, "Data", "gender_leadership.rds"))


gender_wage_premiumpublic <- readRDS(file.path(data_path, "Data", "gender_wage_premiumpublic.rds"))


pay_compression <- readRDS(file.path(data_path, "Data", "pay_compression.rds"))

pay_compression_wide <- readRDS(file.path(data_path, "Data", "pay_compression_wide.rds"))

# ---------------------------
# UI
# ---------------------------


library(shiny)
library(bslib)

ui <- bootstrapPage(
  theme = bs_theme(version = 5, bootswatch = "sandstone"),
  
  # Put global CSS/JS in <head>
  tags$head(
    # ------- Your styles (unchanged) -------
    tags$style(HTML("
      :root { --accent: #6fa8dc; }

      /* General page background and text color */
      html, body { height: 100%; }
      body, .container-fluid, .main-container, .content-wrapper, .flex-grow-1 {
        background-color: #002244 !important;
        color: #ffffff !important;
      }

      /* Typography and panels */
      h1, h2, h3, h4, h5, h6, p, .well, .card, .panel, .info-box, .custom-info-box, .box {
        color: #ffffff !important;
        background-color: transparent !important;
        border: none !important;
      }

      /* Panels and wells with a border */
      .well, .panel {
        background-color: #002244 !important;
        border: 1px solid var(--accent) !important;
        border-radius: 8px;
      }

      /* Buttons */
      .btn, .btn-primary {
        background-color: var(--accent) !important;
        border: none !important;
      }
      .btn:hover { background-color: #4a90c2 !important; }

      /* Link styling */
      a { color: #ffffff !important; text-decoration: none; }
      a:hover { text-decoration: underline; }

      /* --- Sidebar container --- */
      #sidebar {
        height: 100vh;
        width: 290px; min-width: 290px;
        background: linear-gradient(180deg, #2b4c66 0%, #253f57 100%);
        padding: 18px 16px;
        color: #e8f0fb;
        overflow-y: auto;
        border-right: 1px solid rgba(255,255,255,0.08);
        box-shadow: inset 0 0 12px rgba(0,0,0,.25);
        position: sticky; top: 0;
      }

      /* subtle custom scrollbar */
      #sidebar::-webkit-scrollbar { width: 8px; }
      #sidebar::-webkit-scrollbar-thumb {
        background: rgba(255,255,255,.25);
        border-radius: 8px;
      }
      #sidebar::-webkit-scrollbar-track { background: transparent; }

      /* brand/title area (optional) */
      .sidebar-brand {
        display: flex; align-items: center; gap: 10px;
        margin: 2px 6px 14px;
        font-weight: 700; letter-spacing: .3px;
        color: #fff;
      }
      .sidebar-brand .brand-dot {
        width: 10px; height: 10px; border-radius: 50%;
        background: var(--accent); display: inline-block;
      }

      /* section headings (click to expand) */
      .nav-section {
        display: flex; align-items: center; justify-content: space-between;
        font-size: 16px; font-weight: 700;
        padding: 10px 10px; margin: 12px 6px 4px;
        color: #dbe7ff; border-radius: 8px;
        transition: background .2s, color .2s;
        cursor: pointer;
      }
      .nav-section:hover { background: rgba(255,255,255,.06); }
      .nav-section::after {
        content: '▾'; font-size: 14px; opacity: .8; margin-left: 8px;
      }
      .section-open::after { transform: rotate(180deg); }

      /* links */
      .nav-item a, .nav-sub-item a { color: inherit; text-decoration: none; }

      /* top-level items */
      .nav-item {
        display: flex; align-items: center; gap: 10px;
        margin: 6px 6px; padding: 10px 12px;
        font-size: 16px; font-weight: 600; color: #eef5ff;
        border-radius: 10px; transition: transform .08s, background .2s;
      }
      .nav-item:hover { background: rgba(255,255,255,.08); transform: translateX(2px); }

      /* active item with accent bar */
      .nav-item.active {
        background: rgba(111,168,220,.25);
        box-shadow: inset 0 0 0 1px rgba(111,168,220,.5);
        position: relative;
      }
      .nav-item.active::before {
        content: ''; position: absolute; left: -6px; top: 10px; bottom: 10px;
        width: 4px; border-radius: 4px; background: var(--accent);
      }

      /* sub-items (include all expandable sections) */
      #macro_section,
      #public_sector_section,
      #public_sector_workforce_section,
      #public_sector_wages_section,
      #equity_public_sector_section {
        padding: 4px 6px 6px 12px; display: none;
        border-left: 1px dashed rgba(255,255,255,.15);
        margin-left: 10px;
      }

      .nav-sub-item {
        display: flex; align-items: center; gap: 8px;
        margin: 4px 0; padding: 8px 10px;
        font-size: 15px; color: #eaf3ff; border-radius: 8px;
        transition: background .2s, transform .08s;
      }
      .nav-sub-item:hover { background: rgba(255,255,255,.06); transform: translateX(2px); }
      .nav-sub-item.active { background: rgba(111,168,220,.22); }

      /* =========================
         Accordion (Bootstrap 5)
         ========================= */
      .accordion-item{
        background-color:#2b4c66;
        border:1px solid #6fa8dc;
        border-radius:12px !important;
        margin-bottom:14px;
        overflow:hidden;
        color:#fff;
      }
      .accordion-button{
        background-color:#2b4c66;
        color:#fff;
        box-shadow:none !important;
        font-size:18px;
        padding:16px 20px;
      }
      .accordion-button:not(.collapsed){
        background-color:#356088;
        color:#fff;
      }
      .accordion-button:focus{
        box-shadow:none !important;
      }
      .accordion-body{
        background-color:#356088;
        color:#fff;
        padding:18px 22px;
        border-top:1px solid #6fa8dc;
      }
      .accordion-button::after{ filter: invert(1); }  /* white chevron */

      /* Keep all three logos on one line and evenly spaced */
      .logos-row { display:flex; align-items:center; justify-content:space-between; gap:12px; flex-wrap:nowrap; }
      .logo-wrap { flex:1 1 0; display:flex; justify-content:center; }
      .logos-row img { max-width:100%; height:auto; object-fit:contain; display:inline-block; vertical-align:middle; }
      img.bl-logo, img.wb-logo { max-height:64px; }
      @media (min-width: 992px) { img.wb-logo.wb-logo--right { max-height:80px; } }
      .wb-logo.wb-logo--right.padfix { transform: scale(1.12); transform-origin: center; }
img.wb-logo.wb-logo--dec { max-height: 60px !important; }
      /* Info boxes (optional) */
      .custom-infobox .info-box-icon{
        flex: 0 0 var(--tile);
        height: var(--tile);
        border-radius: 12px;
        background-color: #00BFE5 !important;
        color: #fff !important;
        display:flex; align-items:center; justify-content:center;
        float: none !important;
      }
      .custom-infobox .info-box-content{ margin:0 !important; padding:0; }
      .custom-infobox .info-box-text{ font-size:15px !important; line-height:1.2; letter-spacing:.2px; margin:0; }
      .custom-infobox .info-box-number{ font-size:22px !important; font-weight:600; line-height:1.1; margin-top:2px; }

      @media (max-width: 992px){
        .custom-infobox .info-box{ --tile:64px; }
        .custom-infobox .info-box-icon i{ font-size:22px !important; }
        .custom-infobox .info-box-text{ font-size:14px !important; }
        .custom-infobox .info-box-number{ font-size:20px !important; }
      }
      @media (max-width: 768px){
        .custom-infobox .info-box{ --tile:56px; --gap:10px; padding:6px 4px; }
        .custom-infobox .info-box-icon i{ font-size:20px !important; }
        .custom-infobox .info-box-text{ font-size:13px !important; }
        .custom-infobox .info-box-number{ font-size:18px !important; }
      }

      #graph_choice .form-check { margin-bottom: .25rem; }
    ")),
    
    # ------- Accordion styles to match your palette -------
    tags$style(HTML("
      .accordion-item{
        background-color:#2b4c66;
        border:1px solid #6fa8dc;
        border-radius:12px !important;
        margin-bottom:14px;
        overflow:hidden;
        color:#fff;
      }
      .accordion-button{
        background-color:#2b4c66;
        color:#fff;
        box-shadow:none !important;
        font-size:18px;
        padding:16px 20px;
      }
      .accordion-button:not(.collapsed){
        background-color:#356088;
        color:#fff;
      }
      .accordion-button:focus{ box-shadow:none !important; }
      .accordion-body{
        background-color:#356088;
        color:#fff;
        padding:18px 22px;
        border-top:1px solid #6fa8dc;
      }
      .accordion-button::after{ filter: invert(1); }

      /* Logo size (base) */
      .wb-logo{
        max-height:60px;
        width:auto;
        height:auto;
        display:inline-block;
      }

      /* Download buttons */
      .dl-btn {
        font-size: 18px;
        padding: 14px 22px;
        border-radius: 12px;
        background-color: #76A9D6;
        border-color: #76A9D6;
        color: #fff;
      }
      .dl-btn:hover {
        background-color: #669bd0;
        border-color: #669bd0;
        color:#fff;
      }

      /* Only shrink the DEC logo */
      img.wb-logo.wb-logo--dec { max-height: 50px !important; }

      li i.fa, li i.fas, li i.fa-solid { margin: 0 6px; color: #fff; }

      /* Keep the group left-aligned */
      #graph_choice { text-align: left; }

      /* Title aligned with the radio dots (fixed small typo: removed stray space in -1.9rem) */
      #graph_choice .rb-title{
        display: block;
        font-weight: 700;
        font-size: 1.1rem;
        line-height: 1.2;
        margin: 0 0 .25rem 0;
        padding-left: 0;
        margin-left: -1.9rem;
      }

      @media (max-width: 768px){
        #graph_choice .rb-title{
          font-size: 1.0rem;
          padding-left: 1.9rem;
        }
      }

      #graph_choice .form-check { margin-bottom: .3rem; }
    ")),
    
    # ------- JS to toggle sidebar submenus (yours) -------
    tags$script(HTML("
      function toggleSection(id){
        var section = document.getElementById(id);
        section.style.display = (section.style.display === 'none' || section.style.display === '') ? 'block' : 'none';
        var header = document.querySelector('[onclick=\"toggleSection(\\''+id+'\\')\"]');
        if(header){ header.classList.toggle('section-open'); }
      }
      // highlight clicked items
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
  
  # ------- Layout -------
  div(class = "d-flex",
      # --- Sidebar ---
      div(
        id = "sidebar",
        div(class = "nav-item", actionLink("nav_dashboard", "Overview")),
        div(class = "nav-item", actionLink("nav_instructions", "Instructions")),
        div(class = "nav-item", actionLink("nav_metadata", "Metadata")),
        
        div(class = "nav-section", onclick = "toggleSection('macro_section')",
            "Macro Fundamentals of the Public Sector"),
        div(id = "macro_section",
            div(class = "nav-sub-item", actionLink("nav_wagebill", "Wage Bill Graphs")),
            div(class = "nav-sub-item", actionLink("nav_wagebill_gdp", "Wage Bill & GDP Graphs"))
        ),
        
        div(class = "nav-section", onclick = "toggleSection('public_sector_section')",
            "Size and Characteristics of the Public Sector Employment"),
        div(id = "public_sector_section",
            div(class = "nav-sub-item", actionLink("nav_public_graphs", "Public Employment")),
            div(class = "nav-sub-item", actionLink("nav_public_workforce", "Employment Distribution")),
            div(class = "nav-sub-item", actionLink("nav_education", "Tertiary Education"))
        ),
        
        div(class = "nav-section", onclick = "toggleSection('public_sector_wages_section')",
            "Competitiveness of Public Sector Wages"),
        div(id = "public_sector_wages_section",
            div(class = "nav-sub-item", actionLink("nav_wagepremium", "Wage Premium")),
            div(class = "nav-sub-item", actionLink("nav_public_educ", "Wage Premium by Education")),
            div(class = "nav-sub-item", actionLink("nav_pay_compression", "Pay Compression"))
        ),
        
        div(class = "nav-section", onclick = "toggleSection('equity_public_sector_section')",
            "Equity in Public Sector Employment"),
        div(id = "equity_public_sector_section",
            div(class = "nav-sub-item", actionLink("nav_gender_workforce", "Female Employment")),
            div(class = "nav-sub-item", actionLink("nav_female_leadership", "Female Leadership")),
            div(class = "nav-sub-item", actionLink("nav_wagepremium_gender", "Wage Premium by Gender")),
            div(class = "nav-sub-item", actionLink("nav_gender_wage_premium", "Gender Wage Premium by Industry"))
        ),
        
        div(class = "nav-item", actionLink("nav_download_all", "📥 Download All Graphs"))
      ),
      
      # --- Main content with collapsible tabs (accordion) ---
      div(class = "flex-grow-1 p-4",
          h2("Worldwide Bureaucracy Indicators"),
          uiOutput("main_content")
      )
  )
)

# SERVER

server <- function(input, output, session) {
  
  # Track active page
  # 1. Track the active tab via a reactive value  
  active_tab <- reactiveVal("dashboard")
  
  # put near the top of server()
  safe_acc_open  <- function(id, panels) {
    if (exists("accordion_open", envir=asNamespace("bslib"), inherits=FALSE)) {
      bslib::accordion_open(id, panels)
    }
  }
  safe_acc_close <- function(id, panels) {
    if (exists("accordion_close", envir=asNamespace("bslib"), inherits=FALSE)) {
      bslib::accordion_close(id, panels)
    }
  }
  
  # Update active_tab when each sidebar link is clicked
  observeEvent(input$nav_dashboard,         { active_tab("dashboard") })
  observeEvent(input$nav_instructions,         { active_tab("instructions") })
  observeEvent(input$nav_metadata,          { active_tab("metadata") })
  observeEvent(input$nav_publications,          { active_tab("publications") })
  observeEvent(input$nav_wagebill,          { active_tab("wagebill") })
  observeEvent(input$nav_wagebill_gdp,      { active_tab("wagebill_gdp") })
  observeEvent(input$nav_public_workforce,  { active_tab("public_workforce") })
  observeEvent(input$nav_gender_workforce,  { active_tab("gender_workforce") })
  observeEvent(input$nav_education,         { active_tab("education") })
  observeEvent(input$nav_public_educ,       { active_tab("public_educ") })
  observeEvent(input$nav_public_graphs,     { active_tab("public_graphs") })
  observeEvent(input$nav_wagepremium_gender,{ active_tab("wagepremium_gender") })
  observeEvent(input$nav_female_leadership, { active_tab("female_leadership") })
  observeEvent(input$nav_wagepremium,       { active_tab("wagepremium") })
  observeEvent(input$nav_gender_wage_premium, { active_tab("gender_wage_premium") })
  observeEvent(input$nav_pay_compression, { active_tab("pay_compression") })
  observeEvent(input$nav_download_all,      { active_tab("download_all") })
  observeEvent(input$acc_open, {
    if (active_tab() == "dashboard")
      safe_acc_open("ov_acc",
                    c("About the WWBI","Contact Information","Citation","Disclaimer"))
  })
  
  observeEvent(input$acc_close, {
    if (active_tab() == "dashboard")
      safe_acc_close("ov_acc",
                     c("About the WWBI","Contact Information","Citation","Disclaimer"))
  })
  
  # 2. Render the main dynamic UI based on active_tab
  output$main_content <- renderUI({
    tab <- active_tab()
    
    if (tab == "dashboard") {
      tagList(
        # Logos
        fluidRow(
          class = "mb-3 logos-row",
          column(
            4,
            div(class = "logo-wrap",
                tags$img(
                  src = "https://raw.githubusercontent.com/JosefinaSilva96/WWBI/main/www/wbg_institutions_logo.png",
                  class = "wb-logo wb-logo--right", alt = "WBG Institutions"
                ))
          ),
          column(
            4,
            div(class = "logo-wrap",
                tags$img(
                  src = "https://raw.githubusercontent.com/JosefinaSilva96/WWBI/main/www/bl_logo.png",
                  class = "bl-logo", alt = "Bureaucracy Lab"
                ))
          ),
          column(
            4,
            div(class = "logo-wrap",
                tags$img(
                  src = "https://raw.githubusercontent.com/JosefinaSilva96/WWBI/main/www/wbg_dec_logo.png",
                  class = "wb-logo wb-logo--dec", alt = "WBG DEC"
                ))
          )
        ),
        
        h3("Overview"),
        accordion(
          id = "ov_acc",
          multiple = TRUE,
          open = character(0),
          
          accordion_panel(
            "About the WWBI",
            tagList(
              p("The Worldwide Bureaucracy Indicators (WWBI) database is a unique cross-national dataset on public sector employment and wages that aims to fill an information gap, thereby helping researchers, development practitioners, and policymakers gain a better understanding of the personnel dimensions of state capability, the footprint of the public sector within the overall labor market, and the fiscal implications of the public sector wage bill. The dataset is derived from administrative data and household surveys, thereby complementing existing, expert perception-based approaches."),
              p("The dataset provides answers to some of the most important questions on the level and distribution of employment and wages. A few key indicators include:"),
              tags$ul(
                tags$li("Size of the public and private sector workforce"),
                tags$li("Demographics of public and private sector employment: distributions by gender, age, education, and industry"),
                tags$li("Gender equity in public and private sectors"),
                tags$li("Public sector wage premiums"),
                tags$li("Distributions of public sector wage premiums by education, occupation, industry, and gender"),
                tags$li("Gender wage gaps in the public and private sectors"),
                tags$li("Pay compression ratios in public and private sectors (ratio of 90th to 10th percentile earners)"),
                tags$li("The relative size of the public sector wage bill")
              )
            )
          ),
          
          accordion_panel(
            "Contact Information",
            tags$p(
              "Flavia Sacco – ", tags$a(href="mailto:fsaccocapurro@worldbank.org","fsaccocapurro@worldbank.org"), br(),
              "Josefina Silva – ", tags$a(href="mailto:jsilvafuentealba@worldbank.org","jsilvafuentealba@worldbank.org")
            )
          ),
          
          accordion_panel(
            "Citation",
            p("We kindly ask all users of the dashboard to cite it as follows: Source: Worldwide Bureaucracy Indicators (WWBI) Dashboard – World Bank.")
          ),
          
          accordion_panel(
            "Disclaimer",
            p("The findings, interpretations, and conclusions presented in this dashboard are those of the World Bank staff and do not necessarily reflect the views of the World Bank, its affiliated organizations, the Executive Directors of the World Bank, or the governments they represent. The boundaries, colors, denominations, and other information shown on this dashboard do not imply any judgment on the part of the World Bank concerning the legal status of any territory, or the endorsement or acceptance of such boundaries. The terms “country” or “economy,” as used in this dashboard, are used for statistical convenience and do not imply political independence.")
          )
        ),
        
        # Publications
        fluidRow(
          column(
            10,
            h3("📄 Publications"),
            wellPanel(
              style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              h4("Download Team Publications:"),
              tags$ul(
                tags$li(
                  downloadLink("pub1", "Innovating Bureaucracy for a More Capable Government"),
                  br(), tags$small("Report")
                ),
                tags$li(
                  downloadLink("pub2", "Introducing the Worldwide Bureaucracy Indicators: A New Global Dataset on Public Sector Employment and Compensation"),
                  br(), tags$small("Faisal Ali Baig- World Bank Group, Xu Han- University of Maryland, Zahid Hasnain- World Bank Group, Daniel Rogger- World Bank Group ")
                ),
                tags$li(
                  downloadLink("pub3", "Public Sector Employment and Compensation: An Assessment Framework"),
                  br(), tags$small("Report")
                ),
                tags$li(
                  downloadLink("pub4", "Worldwide Bureaucracy Indicators"),
                  br(), tags$small("Report")
                )
              )
            )
          )
        )
      )
      
    } else if (tab == "instructions") {
      tagList(
        h3("📘 Instruction Manual"),
        accordion(
          id = "inst_acc",
          multiple = TRUE,
          open = "About this dashboard",
          
          accordion_panel(
            "About this dashboard",
            p("This Dashboard is a product of the Bureaucracy Lab, a joint initiative between the Governance Global Practice and the Development Impact Evaluation (DIME) Department of the Research Group at the World Bank."),
            p("The dashboard allows users to explore key indicators from the Worldwide Bureaucracy Indicators (WWBI) through a variety of interactive visualizations, which can also be exported into a Word report for further use and analysis."),
            p("Each section of the dashboard presents a set of graphs designed to facilitate benchmarking of state capacity measures across countries, regions, and income groups.")
          ),
          
          accordion_panel(
            "What each section contains",
            tags$ul(
              tags$li(tags$b("Macro-Fundamentals of the Public Sector:"), " This section shows the trends in the size of the public wage bill, expressed as a percentage of both total public expenditure and GDP. It also includes cross-country comparisons of these indicators by income level (measured by GDP per capita)."),
              tags$li(tags$b("Size and Characteristics of the Public Sector:"), " Examine public sector employment within the overall labor market, workforce distribution by industry, and educational attainment vs. private sector employees."),
              tags$li(tags$b("Competitiveness of Public Sector Wages:"), " Public sector wage premium vs. private workers, how it varies by education level, and pay compression in public and private sectors."),
              tags$li(tags$b("Equity in the Public Sector:"), " Female employment and leadership, occupational segregation, and gender wage gaps across public-sector industries."),
              tags$li(tags$b("Download Graph Report:"), " Download a full report with all visualizations or create a custom report by selecting sections/graphs (DOC format with prefilled text).")
            )
          ),
          
          accordion_panel(
            "How to use the dashboard",
            tags$ol(
              tags$li("In each tab, select a country of interest and choose comparator countries, regions, or income groups."),
              tags$li("To check indicator availability, go to “Metadata”. Select an indicator to see which countries have data and their values."),
              tags$li("Dropdowns only list options with available data for the chosen indicator."),
              tags$li("The first selected country appears first in graphs and acts as the benchmark."),
              tags$li("Each tab provides downloadable graphs. To download, select the", icon("camera"), "icon in the top-right corner of each visualization."),
              tags$li("Or set selections across tabs, then use “Download All Graphs” to export a comprehensive, pre-formatted Word report.")
            )
          ),
          
          accordion_panel(
            "Resources & links",
            tags$p(
              "GitHub Repository:The construction files for all indicators included in the dataset can be accessed in the repository:",
              tags$a(href="https://github.com/worldbank/Worldwide-Bureaucracy-Indicators",
                     "https://github.com/worldbank/Worldwide-Bureaucracy-Indicators", target="_blank")
            ),
            tags$p(
              "Data Catalog: The full WWBI dataset, available in multiple file formats, can be accessed at:",
              tags$a(href="https://datacatalog.worldbank.org/int/home",
                     "https://datacatalog.worldbank.org/int/home", target="_blank")
            ),
            accordion_panel(
              "Codebook",
              p("Further details on the dataset, along with definitions of the indicators and their construction, can be found in the codebook below."),
              div(style = "margin-top: 8px;",
                  downloadButton("download_pdf", "📥 Download Codebook", class = "btn btn-primary")
              )
            )
          )
        )
      )
      
    } else if (tab == "metadata") {
      tagList(
        h3("Metadata"),
        fluidRow(
          column(4, div(class = "custom-infobox", infoBox("Indicators", 302, icon = icon("list")))),
          column(4, div(class = "custom-infobox", infoBox("Economies", length(unique(data_wwbi$country_name)), icon = icon("globe")))),
          column(4, div(class = "custom-infobox", infoBox("Temporal Coverage (Annual)", "2000-2022", icon = icon("calendar"))))
        ),
        fluidRow(
          column(4, div(class = "custom-infobox", infoBox("Temporal Coverage (Years)", "22", icon = icon("calendar")))),
          column(4, div(class = "custom-infobox", infoBox("Last Updated", "2025", icon = icon("clock"))))
        ),
        fluidRow(
          div(style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              "This map shows which countries have reported data for the selected indicator.")
        ),
        fluidRow(
          column(6,
                 selectInput("indicatorSelect", "Select Indicator",
                             choices = unique(data_wwbi$indicator_name), selected = NULL)
          )
        ),
        fluidRow(textOutput("countryCount")),
        fluidRow(leafletOutput("worldMap", height = "600px"))
      )
      
    } else if (tab == "wagebill") {
      tagList(
        h3("Wage Bill Graphs"),
        fluidRow(
          div(style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              "This visualization explores the wage bill over time for selected countries.")
        ),
        fluidRow(
          column(
            width = 7,
            selectInput(
              "countries",
              "Select country(ies)/region(s)/income group(s) – Your first selection will be treated as the reference point in both the graph and the output report",
              choices  = sort(unique(data_wwbi_long$country_name)),
              multiple = TRUE,
              width    = "100%"
            )
          ),
          column(
            width = 5,
            radioButtons(
              "graph_choice",
              label = tags$span(class = "rb-title", "Choose wage-bill measure:"),
              choices  = c("Wage Bill as % of Public Expenditure" = "Public",
                           "Wage Bill as % of GDP"                  = "GDP"),
              selected = "Public",
              inline   = FALSE
            )
          )
        ),
        fluidRow(plotlyOutput("plotwagebill", height = "500px")),
        fluidRow(
          div(style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              textOutput("note_wagebill"))
        ),
        fluidRow(
          column(12, downloadButton("downloadWord", "Download Report in Word", class="dl-btn w-100"))
        ),
        fluidRow(
          column(4, downloadButton("dl_csv_wagebill",  "Download data (CSV)",  class = "dl-btn w-100"))
        )
      )
      
    } else if (tab == "wagebill_gdp") {
      tagList(
        h3("Wage Bill & GDP Graphs"),
        fluidRow(
          div(
            style = "background-color: rgba(255,255,255,0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
            "This graph shows the relationship between the size of the wage bill and GDP per capita."
          )
        ),
        fluidRow(
          column(
            width = 7,
            selectInput(
              "countries_gdp",
              "Select country(ies)/region(s)/income group(s) – Your first selection will be treated as the reference point in both the graph and the output report",
              choices  = sort(unique(data_wwbi_long$country_name)),
              multiple = TRUE,
              width    = "100%"
            ),
            br(),
            downloadButton("downloadGDPDoc", "Download GDP Analysis Report",
                           class = "dl-btn w-100")
          ),
          column(
            width = 5,
            tags$label(class = "form-label fw-semibold", "Choose label type"),
            radioButtons("label_type", label = NULL,
                         choices = c("Country", "Region"),
                         selected = "Country")
          )
        ),
        fluidRow(column(12, plotlyOutput("dot_plot_gdp", height = "500px"))),
        fluidRow(
          column(
            12,
            div(
              style = "background-color: rgba(255,255,255,0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              textOutput("note_dotplot_gdp")
            )
          )
        ),
        fluidRow(
          column(12, div(class = "text-end mt-3",
                         downloadButton("dl_csv_gdp", "Download data (CSV)", class = "dl-btn")))
        )
      )
      
    } else if (tab == "public_workforce") {
      tagList(
        h3("Distribution of Public Sector Employment"),
        fluidRow(
          div(
            style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
            "This visualization shows the distribution of the public sector workforce across the three main industries (public administration, health and education)."
          )
        ),
        fluidRow(
          column(
            width = 7,
            selectInput(
              "countries_workforce",
              "Select country(ies)/region(s)/income group(s) – Your first selection will be treated as the reference point in both the graph and the output report",
              choices  = sort(unique(data_wwbi_long$country_name)),
              multiple = TRUE,
              width    = "100%"
            )
          ),
          column(
            width = 5,
            checkboxGroupInput(
              "selected_graphs_public",
              "Select Graphs to Download",
              choices  = c("Multi-Country Graph" = "firstGraph",
                           "Single-Country Graph" = "secondGraph"),
              selected = c("firstGraph", "secondGraph")
            ),
            downloadButton("downloadGraphsemploymentdist",
                           "Download Selected Graphs in Word",
                           class = "dl-btn w-100")
          )
        ),
        fluidRow(plotlyOutput("stackedBarGraph", height = "600px")),
        fluidRow(
          div(
            style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
            textOutput("note_stackedBarGraph")
          )
        ),
        fluidRow(
          column(
            width = 12,
            selectInput(
              "selected_country",
              "Select country/region/income group",
              choices  = sort(unique(data_wwbi_long$country_name)),
              multiple = FALSE,
              width    = "100%"
            )
          )
        ),
        fluidRow(plotlyOutput("horizontalStackedBar", height = "600px")),
        fluidRow(
          div(
            style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
            textOutput("note_horizontalStackedBar")
          )
        ),
        fluidRow(
          column(4, downloadButton("dl_csv_public_workforce",  "Download data (CSV)",  class = "dl-btn w-100"))
        )
      )
      
    } else if (tab == "education") {
      tagList(
        h3("Workers with Tertiary Education"),
        fluidRow(
          div(
            style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
            "This visualization shows the proportion of workers with tertiary education in the public and private sectors."
          )
        ),
        fluidRow(
          column(
            width = 7,
            selectInput(
              "selected_countries",
              "Select country(ies)/region(s)/income group(s) – Your first selection will be treated as the reference point in both the graph and the output report",
              choices  = sort(unique(data_wwbi_long$country_name)),
              multiple = TRUE,
              width    = "100%"
            ),
            br(),
            downloadButton(
              "downloadGraphsWordEducation",
              "Download Tertiary Education Report",
              class = "dl-btn w-100"
            )
          ),
          column(
            width = 5,
            tags$label(class = "form-label fw-semibold", "Choose label type"),
            radioButtons(
              "label_type_edu",
              label   = NULL,
              choices = c("Country", "Region"),
              selected = "Country",
              inline  = TRUE
            )
          )
        ),
        fluidRow(plotlyOutput("barPlot", height = "600px")),
        fluidRow(
          div(
            style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
            textOutput("note_tertiaryEducation")
          )
        ),
        fluidRow(
          column(4, downloadButton("dl_csv_tertiary_edu",  "Download data (CSV)",  class = "dl-btn w-100"))
        )
      )
      
    } else if (tab == "female_leadership") {
      tagList(
        h3("Female Leadership: Occupations and Sector"),
        fluidRow(
          column(
            width = 12,
            div(
              style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              "This visualization shows the share of females in various occupational groups (Managers/Clerks) in the public and private sectors across selected countries."
            )
          )
        ),
        fluidRow(
          column(
            width = 7,
            selectInput(
              "selected_countries",
              "Select country(ies)/region(s)/income group(s) – Your first selection will be treated as the reference point in both the graph and the output report",
              choices  = sort(unique(data_wwbi_long$country_name)),
              multiple = TRUE,
              width    = "100%"
            ),
            br(),
            downloadButton(
              "downloadGraphsWordfemale",
              "Download Female Leadership Occupations Report",
              class = "dl-btn w-100"
            )
          )
        ),
        fluidRow(column(width = 12, plotlyOutput("barPlotwomen", height = "600px"))),
        fluidRow(
          column(
            width = 12,
            div(
              style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              textOutput("note_barPlotwomen")
            )
          )
        ),
        fluidRow(
          column(4, downloadButton("dl_csv_female_leadership",  "Download data (CSV)",  class = "dl-btn w-100"))
        )
      )
      
    } else if (tab == "wagepremium_gender") {
      tagList(
        h3("Public Sector Wage Premium by Gender"),
        fluidRow(
          column(
            width = 12,
            div(
              style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              "This visualization explores the public sector wage premium by gender across selected countrie(s)/region(s)/income groups."
            )
          )
        ),
        fluidRow(
          column(
            width = 7,
            selectInput(
              "countries_first",
              "Select country(ies)/region(s)/income group(s) for the first graph – Your first selection will be treated as the reference point in both the graph and the output report",
              choices  = sort(unique(data_wwbi_long$country_name)),
              multiple = TRUE,
              width    = "100%"
            ),
            br(),
            downloadButton(
              "downloadGraphswagepremiumbygender",
              "Download Public Sector Wage Premium by Gender Report",
              class = "dl-btn w-100"
            )
          )
        ),
        fluidRow(column(12, plotlyOutput("firstGraphGenderWagePremium", height = "600px"))),
        fluidRow(
          column(
            12,
            div(
              style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              textOutput("note_firstGraphGenderWagePremium")
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            div(
              style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              "This visualization explores the public sector wage premium by gender across selected countries and its trend over time."
            )
          )
        ),
        fluidRow(
          column(
            width = 7,
            selectInput(
              "country_second",
              "Select country/region/income group",
              choices  = sort(unique(data_wwbi_long$country_name)),
              multiple = FALSE,
              width    = "100%"
            )
          )
        ),
        fluidRow(column(12, plotlyOutput("secondGraphGenderWagePremium", height = "600px"))),
        fluidRow(
          column(
            12,
            div(
              style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              textOutput("note_secondGraphGenderWagePremium")
            )
          )
        ),
        fluidRow(
          column(4, downloadButton("dl_gender_wageprem_xlsx",  "Download data (CSV)",  class = "dl-btn w-100"))
        )
      )
      
    } else if (tab == "wagepremium") {
      tagList(
        h3("Public Sector Wage Premium"),
        fluidRow(
          column(
            width = 7,
            selectInput(
              "countries_wage_premium",
              "Select country(ies)/region(s)/income group(s) – Your first selection will be treated as the reference point in both the graph and the output report",
              choices  = sort(unique(data_wwbi_long$country_name)),
              multiple = TRUE,
              width    = "100%"
            ),
            br(),
            downloadButton(
              "downloadGraphswagepremium",
              "Download Public Sector Wage Premium Report",
              class = "dl-btn w-100"
            )
          )
        ),
        fluidRow(plotlyOutput("dotPlot", height = "500px")),
        fluidRow(
          div(
            style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
            textOutput("note_wage_premium")
          )
        ), 
        fluidRow(
          column(4, downloadButton("dl_csv_wage_premium",  "Download data (CSV)",  class = "dl-btn w-100"))
        )
      )
      
    } else if (tab == "public_educ") {
      tagList(
        h3("Public Sector Wage Premium by Education Level"),
        fluidRow(
          div(
            style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
            "This visualization explores the public sector wage premium by education level, compared to private formal workers."
          )
        ),
        fluidRow(
          column(
            width = 7,
            selectInput(
              "selected_country",
              "Select country/region/income group – Your selection will be used as the reference in the graph and report",
              choices  = sort(unique(data_wwbi_long$country_name)),
              multiple = FALSE,
              width    = "100%"
            ),
            br(),
            downloadButton(
              "downloadEducationWagePremium",
              "Download Public Sector Wage Premium by Education Level Report",
              class = "dl-btn w-100"
            )
          )
        ),
        fluidRow(plotlyOutput("education_wage_premium_plot", height = "600px")),
        fluidRow(
          div(
            style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
            textOutput("note_education_wage_premium")
          )
        ), 
        fluidRow(
          column(4, downloadButton("dl_csv_wagepremium_educ",  "Download data (CSV)",  class = "dl-btn w-100"))
        )
      )
    } else if (tab == "public_graphs") {
      tagList(
        h3("Public Sector Employment Graphs"),
        # Controls for first graph
        fluidRow(
          column(
            width = 7,
            selectInput(
              "countries_first",
              "Select country(ies)/region(s)/income group(s) – Your first selection will be treated as the reference point in both the graph and the output report",
              choices  = sort(unique(data_wwbi_long$country_name)),
              multiple = TRUE,
              width    = "100%"
            )
          )
        ),
        # First graph
        fluidRow(plotlyOutput("firstGraphpublic", height = "600px")),
        fluidRow(
          div(style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              textOutput("note_firstGraphpublic"))
        ),
        # Single-country selector for second graph
        fluidRow(
          column(
            12,
            selectInput("country_second", "Select country/region/income group",
                        choices = unique(data_wwbi_long$country_name), multiple = FALSE, width = "100%")
          )
        ),
        # Second graph
        fluidRow(plotlyOutput("secondGraphpublic", height = "600px")),
        fluidRow(
          div(style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              textOutput("note_secondGraphpublic"))
        ),
        # Download button
        fluidRow(
          column(12, downloadButton("downloadGraphsWord", "Download Graphs as Word File", class = "dl-btn w-100"))
        ), 
        fluidRow(
          column(
            width = 12,
            div(class = "text-end",
                downloadButton("dl_public_emp_data", "Download data CVS", class = "dl-btn"))
          )
        )
      )
      
    } else if (tab == "gender_workforce") {
      tagList(
        h3("Female share of employment"),
        fluidRow(
          div(
            style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
            "This visualization explores female employment in the public and private sectors across selected countries."
          )
        ),
        fluidRow(
          div(
            style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
            textOutput("note_firstGraphGenderWorkforce")
          )
        ),
        fluidRow(
          column(
            width = 7,
            selectInput(
              "countries_gender",
              "Select country(ies)/region(s)/income group(s) – Your first selection will be treated as the reference point in both the graph and the output report",
              choices  = sort(unique(data_wwbi_long$country_name)),
              multiple = TRUE,
              width    = "100%"
            ),
            br(),
            downloadButton(
              "downloadGraphsWordGender",
              "Download Female Share of Employment Report",
              class = "dl-btn w-100"
            )
          )
        ),
        fluidRow(plotlyOutput("firstGraphGenderWorkforce", height = "600px")),
        fluidRow(
          div(
            style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
            textOutput("note_secondGraphGenderWorkforce")
          )
        ),
        fluidRow(
          column(
            12,
            selectInput(
              "country_gender",
              "Select country/region/income group",
              choices  = sort(unique(data_wwbi_long$country_name)),
              multiple = FALSE,
              width    = "100%"
            )
          )
        ),
        fluidRow(plotlyOutput("secondGraphGenderWorkforce", height = "600px")),
        fluidRow(
          column(
            4,
            downloadButton("dl_gender_workforce_xlsx",
                           "Download data (CSV)",
                           class = "dl-btn w-100")
          )
        )
      )
      
    } else if (tab == "gender_wage_premium") {
      tagList(
        h3("Gender Wage Premium in Public Sector by Industry"),
        fluidRow(
          column(
            width = 12,
            div(
              style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              "This visualization explores the gender wage premium in the public sector by industry across selected countries."
            )
          )
        ),
        fluidRow(
          column(
            width = 7,
            selectInput(
              "selected_countries",
              "Select country(ies)/region(s)/income group(s) – Your first selection will be treated as the reference point in both the graph and the output report",
              choices  = sort(unique(data_wwbi_long$country_name)),
              multiple = TRUE,
              width    = "100%"
            ),
            br(),
            downloadButton(
              "downloadGenderWagePremium",
              "Download Gender Wage Premium in Public Sector by Industry Report",
              class = "dl-btn w-100"
            )
          )
        ),
        fluidRow(column(12, plotOutput("gender_wage_barplot", height = "600px"))),
        fluidRow(
          column(
            width = 12,
            div(
              style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              textOutput("note_gender_wage_barplot")
            )
          )
        ), 
        fluidRow(
          column(
            4,
            downloadButton(
              "dl_csv_gender_wage_industry",
              "Download data (CSV)",
              class = "dl-btn w-100"
            )
          )
        )
      )
    } else if (tab == "pay_compression") {
      tagList(
        h3("Pay Compression Ratios"),
        fluidRow(
          div(
            style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
            "This visualization explores pay compression in the public and private sectors across selected countries."
          )
        ),
        fluidRow(
          column(
            width = 7,
            selectInput(
              inputId = "countries_first",
              label   = "Select country(ies)/region(s)/income group(s) – Your first selection will be treated as the reference point in both the graph and the output report",
              choices = sort(unique(data_wwbi_long$country_name)),
              multiple = TRUE,
              width    = "100%"
            ),
            br(),
            downloadButton(
              "downloadPayCompressionDoc",
              "Download Pay Compression Ratios Report",
              class = "dl-btn w-100"
            )
          )
        ),
        fluidRow(plotlyOutput("paycompression_plot", height = "600px")),
        fluidRow(
          div(
            style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
            textOutput("note_dotplot_pay")
          )
        ),
        fluidRow(
          column(4, downloadButton("dl_csv_pay_compression",  "Download data (CSV)",  class = "dl-btn w-100"))
        )
      )
      
    } else if (tab == "download_all") {
      tagList(
        h3("Download Graph Reports"),
        fluidRow(
          div(style = "background-color: rgba(255, 255, 255, 0.05); border: 1px solid white; border-radius: 10px; padding: 20px;",
              p("You can download a comprehensive report with all graphs or select specific graphs to include in your report.",
                style = "font-size: 16px; color: #333;")
          )
        ),
        br(),
        fluidRow(
          column(6, align = "center",
                 selectInput(
                   inputId = "download_report_countries",
                   label = "Select countries/region/income group(s) for the report/slides:",
                   choices = unique(data_wwbi_long$country_name),
                   selected = c("Chile"),
                   multiple = TRUE,
                   selectize = TRUE
                 )
          )
        ),
        h4("Download a Custom Report"),
        checkboxGroupInput(
          inputId = "selected_graphs",
          label = "Select Graphs to Include:",
          choices = list(
            "Wage Bill" = "wagebill",
            "Wage Bill as % of GDP" = "wagebill_gdp",
            "Tertiary Education" = "tertiaryeducation",
            "Gender Wage Premium" = "genderwagepremium",
            "Public Sector Wage Premium by Education Level" = "wagepremiumeducation",
            "Public Employment" = "public_employment",
            "Public sector Wage Premium by Gender" = "wagepremiumgender",
            "Public Sector Workforce" = "public_workforce",
            "Female share of employment" = "gender_workforce",
            "Female Leadership Occupations" = "female_leadership",
            "Wage Premium" = "wagepremium",
            "Gender Wage premium in Public Sector by Industry" = "gender_wage_premium",
            "Pay Compression Ratios" = "pay_compression"
          ),
          selected = c("wagebill", "public_employment")
        ),
        br(),
        fluidRow(
          column(6, align = "center",
                 downloadButton("downloadAllGraphsDoc", "\U1F4C4 Download Full Word Report",
                                style = "padding: 10px 20px; font-size: 16px; margin-top: 10px;")
          ),
          column(6, align = "center",
                 downloadButton("downloadSelectedGraphsDoc", "\U1F4C4 Download Custom Word Report",
                                style = "padding: 10px 20px; font-size: 16px; margin-top: 10px;")
          ),
          column(6, align = "center",
                 downloadButton("downloadSelectedGraphsPPT", "\U1F4CA Download PowerPoint Slides",
                                style = "padding: 10px 20px; font-size: 16px; margin-top: 10px;")
          )
        )
      )
    }
  })
  
  # ---------------------------
  
  output$download_pdf <- downloadHandler(
    filename = "Codebook and Explanatory Note.pdf",
    content = function(file) {
      file.copy(file.path(data_path, "Files", "WWBI Codebook v3.1.pdf"), file)
    }
  )
  
  #Publications 
  
  output$pub1 <- downloadHandler(
    filename = function() {
      "Innovating-Bureaucracy-for-a-More-Capable-Government.pdf"
    },
    content = function(file) {
      file.copy(
        file.path(data_path, "Files", "Innovating-Bureaucracy-for-a-More-Capable-Government.pdf"),
        file
      )
    }
  )
  output$pub2 <- downloadHandler(
    filename = function() {
      "Public Administration Review - 2021 - Baig - Introducing the Worldwide Bureaucracy Indicators  A New Global Dataset on.pdf"
    },
    content = function(file) {
      file.copy(
        file.path(data_path, "Files", "Public Administration Review - 2021 - Baig - Introducing the Worldwide Bureaucracy Indicators  A New Global Dataset on.pdf"),
        file
      )
    }
  )
  
  output$pub3 <- downloadHandler(
    filename = function() {
      "Public-Sector-Employment-and-Compensation-An-Assessment-Framework.pdf"
    },
    content = function(file) {
      file.copy(
        file.path(data_path, "Files", "Public-Sector-Employment-and-Compensation-An-Assessment-Framework.pdf"),
        file
      )
    }
  )
  
  output$pub4 <- downloadHandler(
    filename = function() {
      "Worldwide-Bureaucracy-Indicators-Methodology-Insights-and-Applications.pdf"
    },
    content = function(file) {
      file.copy(
        file.path(data_path, "Files", "Worldwide-Bureaucracy-Indicators-Methodology-Insights-and-Applications.pdf"),
        file
      )
    }
  )
  
  #Sections 
  
  
  # 3. All your original outputs and downloadHandlers follow.
  # (For brevity, the code below is the same as in your original server code.)
  
  # Reactive expression: select the appropriate dataset based on the radio buttons
  
  selected_data <- reactive({
    req(input$countries)  # Wait until the user selects at least one country
    if (input$graph_choice == "GDP") {
      # Use wage_bill_gdp if the user selects "GDP"
      wage_bill_gdp %>% filter(country_name %in% input$countries)
    } else {
      # Otherwise, use wage_bill_publicexp
      wage_bill_publicexp %>% filter(country_name %in% input$countries)
    }
  })
  
  # Render the Plotly graph for the wage bill
  
  output$plotwagebill <- renderPlotly({
    d <- selected_data()
    
    if (nrow(d) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(
                 title = "No data available",
                 annotations = list(
                   text = "No data available for the selected country/countries.",
                   xref = "paper",
                   yref = "paper",
                   showarrow = FALSE,
                   font = list(size = 16),
                   x = 0.5,
                   y = 0.5
                 )))
    }
    
    # Set the title and y-axis label depending on the selection:
    title_text <- ifelse(input$graph_choice == "GDP",
                         "Wage Bill as % of GDP Over Time",
                         "Wage Bill as % of Public Expenditure Over Time")
    
    y_label <- ifelse(input$graph_choice == "GDP",
                      "Wage Bill (% of GDP)",
                      "Wage Bill (% of Public Expenditure)")
    
    # Create the Plotly graph
    plot_ly(data = d,
            x = ~year,
            y = ~value,
            color = ~country_name,
            type = "scatter",
            mode = "lines+markers",
            marker = list(size = 8)) %>%
      layout(
        title = title_text,
        xaxis = list(title = "Year", dtick = 2),
        yaxis = list(title = y_label),
        legend = list(title = list(text = "Indicator"))
      )
  })
  output$note_wagebill <- renderText({
    if (input$graph_choice == "GDP") {
      "Note: This indicator represents the wage bill as a percentage of GDP, measuring the public sector's wage cost relative to the total economy."
    } else {
      "Note: This indicator represents the wage bill as a percentage of public expenditure, reflecting how much of government spending goes to wages."
    }
  })
  output$downloadWord <- downloadHandler(
    filename = function() {
      paste0("Wage_Bill_Analysis_", Sys.Date(), ".docx")
    },
    content = function(file) {
      # Use the first selected country from the input "countries"
      first_country <- if (!is.null(input$countries) & length(input$countries) > 0) {
        input$countries[1]
      } else {
        "Bangladesh"  # Default fallback
      }
      
      # Get the region using countrycode
      first_region <- countrycode(first_country, origin = "country.name", destination = "region")
      
      report_title <- paste("Wage Bill Analysis Report -", first_country)
      doc <- read_docx()
      
      # Define title style
      title_style <- fp_text(color = "#722F37", font.size = 16, bold = TRUE)
      doc <- doc %>% body_add_fpar(fpar(ftext(report_title, prop = title_style)))
      
      # Add a subtitle
      doc <- doc %>% body_add_par("The macro fundamentals of the public sector", style = "heading 3")
      
      # Create a dynamic introduction text
      intro_text <- paste0(
        "This note presents evidence on public sector employment and compensation practices in ", first_country,
        " using the Worldwide Bureaucracy Indicators (WWBI). The primary data source is the Labor Force Survey (LFS), conducted by the Bureau of Statistics, ",
        "which offers extensive, nationally representative data over multiple years up to 2022. ", 
        "For international comparisons, the analysis includes a set of peer countries for benchmarking, with a particular focus on countries from the ",
        first_region, " region and other aspirational peers."
      )
      
      doc <- doc %>% body_add_par(intro_text, style = "Normal")
      
      # --- Dynamic Analysis Paragraph ---
      selected_countries <- input$countries  # User-selected comparison countries
      
      # Get data for the selected country
      data_country <- wage_bill_publicexp %>% filter(country_name == first_country)
      
      # Extract the wage bill values for 2010 and 2022 (if available)
      value_2010 <- data_country %>% filter(year == 2010) %>% pull(value)
      value_2022 <- data_country %>% filter(year == 2022) %>% pull(value)
      
      # Handle missing data
      if (length(value_2010) == 0) value_2010 <- NA
      if (length(value_2022) == 0) value_2022 <- NA
      
      # Determine whether the wage bill is "low," "moderate," or "high" compared to selected peers
      avg_peer_wage <- wage_bill_publicexp %>%
        filter(country_name %in% selected_countries, year == 2022) %>%
        summarise(mean_wage = mean(value, na.rm = TRUE)) %>%
        pull(mean_wage)
      
      comparison_text <- if (!is.na(value_2022) & !is.na(avg_peer_wage)) {
        if (value_2022 < avg_peer_wage * 0.8) {
          "a relatively low"
        } else if (value_2022 > avg_peer_wage * 1.2) {
          "a relatively high"
        } else {
          "a moderate"
        }
      } else {
        "an uncertain"
      }
      
      # Select top 3 highest wage bill countries among the user's selection
      top_countries <- wage_bill_publicexp %>%
        filter(country_name %in% selected_countries, year == 2022) %>%
        arrange(desc(value)) %>%
        slice(1:3) %>%
        pull(country_name)
      
      top_countries_text <- paste(top_countries, collapse = ", ")
      
      # Determine the wage bill comparison text
      # Ensure we correctly compare first_country's wage bill to the selected countries
      wage_difference_text <- if (!is.na(value_2022) & !is.na(avg_peer_wage)) {
        if (value_2022 > avg_peer_wage) {
          "lower"
        } else if (value_2022 < avg_peer_wage) {
          "higher"
        } else {
          "similar"
        }
      } else {
        "uncertain"
      }
      
      # Get the last available year for the selected country
      last_year <- wage_bill_publicexp %>%
        filter(country_name == first_country) %>%
        summarise(last_year = max(year, na.rm = TRUE)) %>%
        pull(last_year)
      
      # Construct the final analysis text dynamically
      analysis_text <- paste0(
        "The country’s wage bill as a percentage of public expenditures has ",
        "In 2010, the wage bill accounted for around ", 
        ifelse(is.na(value_2010), "N/A", round(value_2010, 1)), 
        " percent of public expenditures, but this gradually changed, reaching ", 
        ifelse(is.na(value_2022), "N/A", round(value_2022, 1)), 
        " percent in ", last_year, ". ",
        "Compared to other countries in the region and global comparators, ", first_country, 
        " allocates ", comparison_text, " proportion of its budget to public sector wages. ",
        "For instance, in ", last_year, ", ", first_country, "’s wage bill stands at ",  
        ifelse(is.na(value_2022), "N/A", round(value_2022, 1)), 
        " percent, whereas countries like ", top_countries_text, 
        " had ", wage_difference_text, " wage bills during the same period. ",
        "This trend reflects ", first_country, "’s approach to public sector wage spending, but it also raises questions about whether this level of spending affects the government's ability to effectively deliver public services."
      )
      
      
      doc <- doc %>% body_add_par(analysis_text, style = "Normal")
      
      # --- Add the Graph Based on User Selection ---
      if (input$graph_choice == "GDP") {
        graph_data <- wage_bill_gdp %>% filter(country_name %in% input$countries)
        
        graph <- ggplot(graph_data, aes(x = year, y = value, color = country_name)) +
          geom_line(size = 1.2) + 
          geom_point(size = 3) +
          labs(title = "Wage Bill as % of GDP Over Time",
               x = "Year",
               y = "Wage Bill (% of GDP)") +
          theme_minimal()
        
        doc <- doc %>% 
          body_add_gg(value = graph, style = "centered") %>%
          body_add_par(ifelse(input$graph_choice == "GDP",
                              "Note: This indicator represents the wage bill as a percentage of GDP, measuring the public sector's wage cost relative to the total economy.",
                              "Note: This indicator represents the wage bill as a percentage of public expenditure, reflecting how much of government spending goes to wages."), 
                       style = "Normal")  %>%
          body_add_par(paste0("This graph shows the wage bill as a percentage of GDP over time for the selected countries. ",
                              "For example, in 2022, ", first_country, " had a wage bill of ", 
                              ifelse(is.na(value_2022), "N/A", round(value_2022, 1)), "% of GDP."), 
                       style = "Normal")
        
      } else {
        graph_data <- wage_bill_publicexp %>% filter(country_name %in% input$countries)
        
        graph <- ggplot(graph_data, aes(x = year, y = value, color = country_name)) +
          geom_line(size = 1.2) + 
          geom_point(size = 3) +
          labs(title = "Wage Bill as % of Public Expenditure Over Time",
               x = "Year",
               y = "Wage Bill (% of Public Expenditure)") +
          theme_minimal()
        
        doc <- doc %>% 
          body_add_gg(value = graph, style = "centered") %>%
          body_add_par(paste0("This graph shows the wage bill as a percentage of public expenditure over time for the selected countries. ",
                              "For instance, in 2022, ", first_country, "’s wage bill accounted for ", 
                              ifelse(is.na(value_2022), "N/A", round(value_2022, 1)), "% of public expenditure."), 
                       style = "Normal")
      }
      
      # Save the document
      print(doc, target = file)
    }
  )
  output$dl_csv_wagebill <- downloadHandler(
    filename = function() {
      type <- if (input$graph_choice == "GDP") "gdp" else "publicexp"
      paste0("wagebill_", type, "_", paste(input$countries, collapse = "-"), ".csv")
    },
    content = function(file) {
      d <- selected_data() %>% arrange(country_name, year)
      validate(need(nrow(d) > 0, "No data to download"))
      readr::write_csv(d, file)
    }
  )
  
  generate_wage_bill_analysis_section <- function(doc, selected_countries) {
    # ✅ Extract first selected country
    first_country <- selected_countries[1] %||% "Unknown Country"
    
    # ✅ Get region using countrycode
    first_region <- countrycode(first_country, origin = "country.name", destination = "region") %||% "its region"
    
    # ✅ Filter data
    data_country <- wage_bill_publicexp %>% filter(country_name == first_country)
    value_2010 <- data_country %>% filter(year == 2010) %>% pull(value) %>% first() %||% NA
    value_2022 <- data_country %>% filter(year == 2022) %>% pull(value) %>% first() %||% NA
    
    last_year <- data_country %>% summarise(last_year = max(year, na.rm = TRUE)) %>% pull(last_year)
    
    # ✅ Filter for all countries selected
    graph_data_gdp <- wage_bill_gdp %>% filter(country_name %in% selected_countries)
    graph_data_exp <- wage_bill_publicexp %>% filter(country_name %in% selected_countries)
    
    # ✅ Create and save graphs
    graph_gdp <- ggplot(graph_data_gdp, aes(x = year, y = value, color = country_name)) +
      geom_line(size = 1.2) + geom_point(size = 3) +
      labs(title = "Wage Bill as % of GDP Over Time", x = "Year", y = "Wage Bill (% of GDP)", color = "Country") +
      theme_minimal()
    
    graph_exp <- ggplot(graph_data_exp, aes(x = year, y = value, color = country_name)) +
      geom_line(size = 1.2) + geom_point(size = 3) +
      labs(title = "Wage Bill as % of Public Expenditure Over Time", x = "Year", y = "Wage Bill (% of Public Expenditure)", color = "Country") +
      theme_minimal()
    
    img_path_gdp <- tempfile(fileext = ".png")
    ggsave(img_path_gdp, plot = graph_gdp, width = 8, height = 6, dpi = 300)
    
    img_path_exp <- tempfile(fileext = ".png")
    ggsave(img_path_exp, plot = graph_exp, width = 8, height = 6, dpi = 300)
    
    # ✅ GDP interpretation
    regional_trend <- wage_bill_gdp %>%
      filter(wb_region == first_region) %>%
      summarise(avg_value = mean(value, na.rm = TRUE)) %>%
      pull(avg_value)
    
    relationship_text <- case_when(
      is.na(regional_trend) ~ "an uncertain relationship",
      regional_trend < -0.2 ~ "a negative relationship",
      regional_trend > 0.2 ~ "a positive relationship",
      TRUE ~ "no strong relationship"
    )
    
    first_country_wage_bill <- wage_bill_gdp %>%
      filter(country_name == first_country) %>%
      summarise(latest = max(value, na.rm = TRUE)) %>%
      pull(latest)
    
    regional_avg_wage_bill <- wage_bill_gdp %>%
      filter(wb_region == first_region) %>%
      summarise(mean_wage = mean(value, na.rm = TRUE)) %>%
      pull(mean_wage)
    
    spending_pattern <- if (!is.na(first_country_wage_bill) && !is.na(regional_avg_wage_bill)) {
      if (first_country_wage_bill > regional_avg_wage_bill * 1.2) {
        "more than expected"
      } else if (first_country_wage_bill < regional_avg_wage_bill * 0.8) {
        "less than expected"
      } else {
        "roughly as expected"
      }
    } else {
      "uncertain compared to regional peers"
    }
    
    first_country_2010 <- wage_bill_gdp %>%
      filter(country_name == first_country, year == 2010) %>%
      summarise(value = first(value)) %>%
      pull(value) %>%
      round(0) %||% NA
    
    first_country_latest <- wage_bill_gdp %>%
      filter(country_name == first_country, year == last_year) %>%
      summarise(value = first(value)) %>%
      pull(value) %>%
      round(0) %||% NA
    
    
    trend_text <- if (!is.na(first_country_2010) && !is.na(first_country_latest)) {
      if (first_country_latest > first_country_2010) {
        paste0("an increase from ", round(first_country_2010, 0), "% in 2010 to ", round(first_country_latest, 0), "% in ", last_year, ".")
      } else if (first_country_latest < first_country_2010) {
        paste0("a decrease from ", round(first_country_2010, 0), "% in 2010 to ", round(first_country_latest, 0), "% in ", last_year, ".")
      } else {
        paste0("no significant change, remaining at ", round(first_country_2010, 0), "% from 2010 to ", last_year, ".")
      }
    } else {
      "insufficient data to determine the trend."
    }
    
    gdp_interpretation <- paste0(
      "Figure 1.1 illustrates the Wage bill as a percentage of GDP for the selected countries, showing ", relationship_text, 
      " between a country’s level of economic development and the size of its public sector in the ", first_region, " region. ",
      first_country, " spends ", spending_pattern, " on its public sector wage bill compared to peers.\n\n",
      "For ", first_country, ", the wage bill as a percentage of GDP shows ", trend_text
    )
    
    doc <- doc %>%
      body_add_par("Wage Bill as % of GDP Over Time", style = "heading 2") %>%
      body_add_img(src = img_path_gdp, width = 6, height = 4) %>%
      body_add_par(gdp_interpretation, style = "Normal")
    
    # ✅ Public Expenditure analysis
    exp_data <- wage_bill_publicexp %>% filter(country_name == first_country)
    
    exp_2010 <- exp_data %>%
      filter(year == 2010) %>%
      summarise(value = max(value, na.rm = TRUE)) %>%
      pull(value) %>%
      round(0) %||% NA
    
    exp_latest <- exp_data %>%
      filter(year == last_year) %>%
      summarise(value = max(value, na.rm = TRUE)) %>%
      pull(value) %>%
      round(0) %||% NA
    
    country_volatility <- sd(exp_data$value, na.rm = TRUE)
    regional_volatility <- wage_bill_publicexp %>%
      filter(wb_region == first_region) %>%
      group_by(country_name) %>%
      summarise(vol = sd(value, na.rm = TRUE)) %>%
      summarise(mean_vol = mean(vol, na.rm = TRUE)) %>%
      pull(mean_vol)
    
    stability_text <- if (!is.na(country_volatility) && !is.na(regional_volatility)) {
      if (country_volatility < regional_volatility * 0.8) {
        "more stable"
      } else if (country_volatility > regional_volatility * 1.2) {
        "more volatile"
      } else {
        "similar in stability"
      }
    } else {
      "uncertain compared to regional peers"
    }
    
    exp_trend_text <- if (!is.na(exp_2010) && !is.na(exp_latest)) {
      paste0(exp_2010, "% in 2010 and has ",
             if (exp_latest > exp_2010) "increased" else if (exp_latest < exp_2010) "decreased" else "remained the same",
             " to ", exp_latest, "% in ", last_year, ".")
    } else {
      "varied over time, as shown in Figure 1.2."
    }
    
    doc <- doc %>%
      body_add_par("Wage Bill as % of Public Expenditure Over Time", style = "heading 2") %>%
      body_add_img(src = img_path_exp, width = 6, height = 4) %>%
      body_add_par(
        paste0(
          "The wage bill as a share of public expenditures in ", first_country, " was ", exp_trend_text,
          " The public sector wage bill in ", first_country, " has exhibited ", stability_text, "."
        ), 
        style = "Normal"
      )
    
    return(doc)
  }
  
  #Slides
  
  generate_wage_bill_analysis_slide <- function(ppt, selected_countries) {
    # Validate selected countries
    if (is.null(selected_countries) || length(selected_countries) == 0) {
      return(ppt)
    }
    
    # Filter data for graphs
    graph_data_gdp <- wage_bill_gdp %>% filter(country_name %in% selected_countries)
    graph_data_exp <- wage_bill_publicexp %>% filter(country_name %in% selected_countries)
    
    if (nrow(graph_data_gdp) == 0 && nrow(graph_data_exp) == 0) {
      return(ppt)
    }
    
    # Graph 1: Wage Bill as % of GDP
    if (nrow(graph_data_gdp) > 0) {
      graph_gdp <- ggplot(graph_data_gdp, aes(x = year, y = value, color = country_name)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        labs(
          title = "Wage Bill as % of GDP Over Time", 
          x = "Year", y = "Wage Bill (% of GDP)", color = "Country"
        ) +
        theme_minimal()
      
      img_path_gdp <- tempfile(fileext = ".png")
      ggsave(img_path_gdp, plot = graph_gdp, width = 8, height = 6, dpi = 300)
      
      ppt <- ppt %>%
        add_slide(layout = "Title and Content", master = "Office Theme") %>%
        ph_with(external_img(img_path_gdp, height = 5, width = 7), location = ph_location_type(type = "body"))
    }
    
    # Graph 2: Wage Bill as % of Public Expenditure
    if (nrow(graph_data_exp) > 0) {
      graph_exp <- ggplot(graph_data_exp, aes(x = year, y = value, color = country_name)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        labs(
          title = "Wage Bill as % of Public Expenditure Over Time", 
          x = "Year", y = "Wage Bill (% of Public Expenditure)", color = "Country"
        ) +
        theme_minimal()
      
      img_path_exp <- tempfile(fileext = ".png")
      ggsave(img_path_exp, plot = graph_exp, width = 8, height = 6, dpi = 300)
      
      ppt <- ppt %>%
        add_slide(layout = "Title and Content", master = "Office Theme") %>%
        ph_with(external_img(img_path_exp, height = 5, width = 7), location = ph_location_type(type = "body"))
    }
    
    return(ppt)
  }
  
  output$dot_plot_gdp <- renderPlotly({
    req(input$countries_gdp)
    
    d <- merged_data %>%
      dplyr::filter(country_name %in% input$countries_gdp)
    
    if (nrow(d) == 0) {
      return(plotly::plotly_empty(type = "scatter", mode = "markers") %>%
               plotly::layout(
                 title = "No data available",
                 annotations = list(
                   text = "No data available for the selected country/countries.",
                   xref = "paper", yref = "paper", showarrow = FALSE,
                   font = list(size = 16), x = 0.5, y = 0.5
                 ),
                 plot_bgcolor = "white", paper_bgcolor = "white"
               ))
    }
    
    first_sel <- input$countries_gdp[1]
    d <- d %>% dplyr::mutate(color = ifelse(country_name == first_sel, "#B3242B", "#003366"))
    
    # Choose label column safely (Region if present & selected, else Country)
    region_col <- intersect(c("region","region_name","Region"), names(d))[1]
    label_vec  <- if (!is.na(region_col) && identical(input$label_type, "Region")) d[[region_col]] else d$country_name
    
    m    <- lm(indicator_value ~ log_gdp, data = d)
    pred <- predict(m, newdata = d)
    
    plotly::plot_ly(d,
                    x = ~log_gdp, y = ~indicator_value,
                    type = "scatter", mode = "markers+text",
                    text = label_vec, textposition = "top center",
                    marker = list(size = 10, color = ~color, opacity = 0.7)) %>%
      plotly::add_trace(x = d$log_gdp, y = pred, inherit = FALSE,
                        type = "scatter", mode = "lines",
                        line = list(color = "gray", dash = "dash"),
                        showlegend = FALSE) %>%
      plotly::layout(
        title = "Wage Bill vs. Log(GDP per Capita)",
        xaxis = list(title = "Log(GDP per Capita, 2015)"),
        yaxis = list(title = "Wage Bill"),
        showlegend = FALSE,
        plot_bgcolor = "white", paper_bgcolor = "white"
      )
  })
  
  output$downloadGDPDoc <- downloadHandler(
    filename = function() paste0("Wage_Bill_vs_GDP_Report_", Sys.Date(), ".docx"),
    content = function(file) {
      req(input$countries_gdp)
      
      filtered_data_df <- merged_data %>%
        dplyr::filter(country_name %in% input$countries_gdp)
      req(nrow(filtered_data_df) > 0)
      
      first_sel    <- input$countries_gdp[1]
      report_title <- paste("Wage Bill vs. GDP Analysis Report –", first_sel)
      
      # choose label (Region if selected & available, else Country)
      region_col <- intersect(c("region","region_name","Region"), names(filtered_data_df))[1]
      label_vec  <- if (!is.na(region_col) && identical(input$label_type, "Region")) {
        filtered_data_df[[region_col]]
      } else {
        filtered_data_df$country_name
      }
      
      # build doc
      doc <- officer::read_docx()
      doc <- doc |>
        officer::body_add_par(report_title, style = "heading 1") |>
        officer::body_add_par("Introduction", style = "heading 2") |>
        officer::body_add_par(
          "This section shows the relationship between the wage bill (expressed as a share of total expenditure) and the income level of countries. It offers a clearer understanding of whether wage bill spending is consistent with countries’ respective income levels.",
          style = "Normal"
        ) |>
        officer::body_add_par("Macro Fundamentals of the Public Sector", style = "heading 2") |>
        officer::body_add_par("", style = "Normal")
      
      # data for plot (first selection red, others blue)
      d_plot <- filtered_data_df |>
        dplyr::mutate(
          highlight = ifelse(country_name == first_sel, "Selected country", "Other countries"),
          label = label_vec
        )
      
      # plot — labels black, NO LEGEND
      p <- ggplot2::ggplot(d_plot, ggplot2::aes(x = log_gdp, y = indicator_value)) +
        ggplot2::geom_point(ggplot2::aes(color = highlight), size = 3, alpha = 0.9, show.legend = FALSE) +
        ggrepel::geom_text_repel(
          ggplot2::aes(label = label),
          color = "black", size = 3, max.overlaps = 30, box.padding = 0.4, point.padding = 0.5,
          show.legend = FALSE
        ) +
        ggplot2::geom_smooth(method = "lm", color = "gray50", linetype = "dashed", se = FALSE) +
        ggplot2::scale_color_manual(
          values = c("Selected country" = "#B3242B", "Other countries" = "#003366"),
          guide = "none"                                  # <- hide legend
        ) +
        ggplot2::labs(
          title = "Wage Bill vs. Log(GDP per Capita)",
          x = "Log(GDP per Capita, 2015)",
          y = "Wage Bill (% of Public Expenditure)"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::coord_cartesian(clip = "off") +
        ggplot2::theme(
          legend.position = "none",                        # <- extra safeguard
          plot.margin = ggplot2::margin(10, 40, 10, 10)
        )
      
      doc <- doc |>
        officer::body_add_gg(value = p, width = 6.5, height = 4.5) |>
        officer::body_add_par(
          "Note: This graph shows the relationship between the wage bill (expressed as a share of total expenditure) and the income level of countries. It offers a clearer understanding of whether wage bill spending is consistent with countries’ respective income levels.The indicator shows the last year available for each selected country/region/income group(s).",
          style = "Normal"
        )
      
      print(doc, target = file)
    }
  )
  
  generate_gdp_analysis_section <- function(doc, selected_countries) {
    filtered_data_df <- merged_data %>% dplyr::filter(country_name %in% selected_countries)
    if (nrow(filtered_data_df) == 0) {
      return(doc %>% officer::body_add_par("No data available for Wage Bill vs. GDP analysis.", style = "Normal"))
    }
    
    first_country <- selected_countries[1]
    
    country_summary <- filtered_data_df %>%
      dplyr::group_by(country_name) %>%
      dplyr::summarise(
        wage_bill = round(mean(indicator_value, na.rm = TRUE), 0),
        gdp_per_capita = round(exp(mean(log_gdp, na.rm = TRUE)), 0),
        .groups = "drop"
      )
    
    first_country_values <- country_summary %>% dplyr::filter(country_name == first_country)
    first_country_wage_bill <- if (nrow(first_country_values) == 0) NA_real_ else first_country_values$wage_bill
    first_country_gdp <- if (nrow(first_country_values) == 0) NA_real_ else first_country_values$gdp_per_capita
    
    regional_avg <- filtered_data_df %>%
      dplyr::summarise(
        avg_wage_bill = round(mean(indicator_value, na.rm = TRUE), 0),
        avg_gdp_per_capita = round(exp(mean(log_gdp, na.rm = TRUE)), 0)
      )
    
    interpretation_text <- paste0(
      "Figure 1.3 illustrates the relationship between the wage bill as a percentage of public expenditure ",
      "and GDP per capita across selected countries. The selected countries have an average wage bill of ",
      regional_avg$avg_wage_bill, "%, with a GDP per capita of $",
      format(regional_avg$avg_gdp_per_capita, big.mark = ","), ".\n\n",
      "For ", first_country, ", the wage bill represents ", first_country_wage_bill,
      "% of public expenditure, with a GDP per capita of $",
      format(first_country_gdp, big.mark = ","), "."
    )
    
    p <- ggplot2::ggplot(filtered_data_df, ggplot2::aes(x = log_gdp, y = indicator_value, color = country_name)) +
      ggplot2::geom_point(size = 3) +
      ggplot2::geom_smooth(method = "lm", color = "gray50", linetype = "dashed", se = FALSE) +
      ggplot2::labs(
        title = "Wage Bill vs. Log(GDP per Capita)",
        x = "Log(GDP per Capita, 2015)",
        y = "Wage Bill (% of Public Expenditure)",
        color = "Country"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::scale_color_manual(values = scales::hue_pal()(length(unique(filtered_data_df$country_name))))
    
    img_path <- tempfile(fileext = ".png")
    ggplot2::ggsave(filename = img_path, plot = p, width = 8, height = 6, dpi = 300)
    
    doc %>%
      officer::body_add_par("Wage bill (% of public expenditure) and GDP per capita in the region", style = "heading 2") %>%
      officer::body_add_par("This note presents evidence on public sector employment and compensation practices in relation to GDP per capita.", style = "Normal") %>%
      officer::body_add_img(src = img_path, width = 6, height = 4) %>%
      officer::body_add_par(
        "Note: This graph shows the relationship between the wage bill (expressed as a share of total expenditure) and the income level of countries. It offers a clearer understanding of whether wage bill spending is consistent with countries’ respective income levels.",
        style = "Normal"
      ) %>%
      officer::body_add_par(interpretation_text, style = "Normal")
  }
  
  #Slides
  
  generate_gdp_analysis_slide <- function(ppt, selected_countries) {
    # Validate input
    if (is.null(selected_countries) || length(na.omit(selected_countries)) == 0) {
      return(ppt)
    }
    
    # Filter data
    filtered_data_df <- merged_data %>% filter(country_name %in% selected_countries)
    
    if (nrow(filtered_data_df) == 0) {
      return(ppt)
    }
    
    # Plot
    plot <- ggplot(filtered_data_df, aes(x = log_gdp, y = indicator_value, color = country_name)) +
      geom_point(size = 3) +
      geom_smooth(method = "lm", color = "gray", linetype = "dashed") +
      labs(
        title = "Wage Bill vs. Log(GDP per Capita)", 
        x = "Log(GDP per Capita, 2015)", 
        y = "Wage Bill (% of Public Expenditure)",
        color = "Country"
      ) +
      theme_minimal() +
      scale_color_manual(values = scales::hue_pal()(length(unique(filtered_data_df$country_name))))
    
    # Save plot to image
    img_path <- tempfile(fileext = ".png")
    ggsave(filename = img_path, plot = plot, width = 8, height = 6, dpi = 300)
    
    # Add slide with image only
    ppt <- ppt %>%
      add_slide(layout = "Title and Content", master = "Office Theme") %>%
      ph_with(external_img(img_path, height = 5, width = 7), location = ph_location_type(type = "body"))
    
    return(ppt)
  }
  dot_data_gdp <- reactive({
    req(input$countries_gdp)
    d <- merged_data %>%
      dplyr::filter(country_name %in% input$countries_gdp)
    
    req(nrow(d) > 0)
    
    # pick a region column if it exists
    region_col <- intersect(c("region","region_name","wb_region","Region"), names(d))[1]
    
    d %>%
      dplyr::mutate(
        label = if (!is.na(region_col) && identical(input$label_type, "Region"))
          .data[[region_col]] else country_name
      ) %>%
      # keep only the columns that exist in your data
      dplyr::select(dplyr::any_of(c("country_name", "label", "year",
                                    "indicator_value", "log_gdp")))
  })
  
  output$dl_csv_gdp <- downloadHandler(
    filename = function() {
      who <- if (length(input$countries_gdp)) paste(input$countries_gdp, collapse = "-") else "all"
      paste0("wagebill_vs_gdp_", who, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      d <- dot_data_gdp()
      req(nrow(d) > 0)
      utils::write.csv(d, file, row.names = FALSE)
    },
    contentType = "text/csv"     # avoids “.htm” save dialog
  )
  
  output$note_dotplot_gdp <- renderText({
    "Note: This graph shows the relationship between the wage bill (expressed as a share of total expenditure) and the income level of countries. It offers a clearer understanding of whether wage bill spending is consistent with countries’ respective income levels. The indicator shows the last year available for each selected country/region/income group(s)."
  })
  
  
  
  #Employment distribution
  
  filtered_workforce_data <- reactive({
    req(input$countries_workforce)
    public_sector_workforce_clean %>% group_by(country_name, indicator_name) %>% slice_max(order_by = year, n = 1) %>% ungroup()
  })
  
  output$stackedBarGraph <- renderPlotly({
    req(input$countries_workforce)
    
    data_to_plot <- filtered_workforce_data() %>% 
      filter(country_name %in% input$countries_workforce)
    
    if (nrow(data_to_plot) == 0) {
      return(plotly_empty(type = "bar") %>%
               layout(
                 title = "No data available",
                 annotations = list(
                   text = "No data available for the selected country/countries.",
                   xref = "paper",
                   yref = "paper",
                   showarrow = FALSE,
                   font = list(size = 16),
                   x = 0.5,
                   y = 0.5
                 ),
                 plot_bgcolor = "white",
                 paper_bgcolor = "white"
               ))
    }
    
    # Define color-blind friendly palette (Okabe-Ito)
    color_blind_palette <- c(
      "Public Administration" = "#E69F00",  # orange
      "Education" = "#56B4E9",              # sky blue
      "Health" = "#009E73",                 # bluish green
      "Other" = "#F0E442"                   # yellow
    )
    
    # Create stacked bar chart
    plot_ly(data = data_to_plot,
            x = ~country_name,
            y = ~value_percentage,
            color = ~indicator_name,
            type = "bar",
            text = ~paste("Country:", country_name,
                          "Indicator:", indicator_name,
                          "Value:", round(value_percentage, 1), "%",
                          "Year:", year),
            textposition = "auto",
            colors = color_blind_palette) %>%
      layout(
        barmode = "stack",
        title = "Public Workforce Distribution by Country",
        xaxis = list(title = "Country"),
        yaxis = list(title = "Workforce Distribution (%)", range = c(0, 100)),
        legend = list(title = list(text = "<b>Indicator</b>"))
      )
  })
  
  output$note_stackedBarGraph <- renderText({
    "Note: This indicator represents the distribution of public sector employment across different industries (Public Administration, Education, Health, and Other) as a percentage of paid public employment. The indicator shows the last year available for each selected country/region/income group(s)."
  })
  output$messageOutput <- renderText({
    filtered_data <- public_sector_workforce %>% filter(country_name == input$selected_country)
    if(nrow(filtered_data) < 2) {
      return(tags$p("Not enough data available for this country to create the graph.", style = "color: red; font-weight: bold;"))
    }
    return(NULL)
  })
  
  output$horizontalStackedBar <- renderPlotly({
    req(input$selected_country)
    filtered_data <- public_sector_workforce %>% filter(country_name == input$selected_country)
    if(nrow(filtered_data) == 0) return(NULL)
    first_year <- min(filtered_data$year, na.rm = TRUE)
    last_year <- max(filtered_data$year, na.rm = TRUE)
    if(is.infinite(first_year) || is.infinite(last_year)) return(NULL)
    data_to_plot <- filtered_data %>% filter(year %in% c(first_year, last_year)) %>% 
      group_by(year, indicator_name) %>% summarise(value_percentage = mean(value_percentage, na.rm = TRUE), .groups = "drop")
    # Color-blind friendly palette (Okabe-Ito)
    color_blind_palette <- c(
      "Public Administration" = "#E69F00",  # orange
      "Education" = "#56B4E9",              # sky blue
      "Health" = "#009E73",                 # bluish green
      "Other" = "#F0E442"                   # yellow
    )
    
    plot_ly(data = data_to_plot,
            x = ~value_percentage,
            y = ~factor(year, levels = c(last_year, first_year)),
            color = ~indicator_name,
            type = "bar",
            orientation = "h",
            text = ~paste0(round(value_percentage, 1), "%"),
            textposition = "inside",
            colors = color_blind_palette) %>%
      layout(barmode = "stack",
             title = paste("Sectoral Distribution of Public Sector Workforce in", input$selected_country, "(", first_year, "&", last_year, ")"),
             xaxis = list(title = "Percentage (%)"),
             yaxis = list(title = "Year"),
             legend = list(title = list(text = "Sector")))
  })
  output$note_horizontalStackedBar <- renderText({
    paste0("Note: This indicator represents the distribution of the public sector workforce across different industries in ", 
           input$selected_country, 
           " for the earliest and latest available years in the dataset. It highlights the changes in sectoral employment over time.")
  })
  
  output$downloadGraphsemploymentdist <- downloadHandler(
    filename = function() paste0("Employment_Distribution_Analysis_", Sys.Date(), ".docx"),
    content  = function(file) {
      
      # ---- Build the dynamic title (first selected country as reference) ----
      ref_country <- if (!is.null(input$countries_workforce) &&
                         length(input$countries_workforce) > 0) {
        input$countries_workforce[[1]]
      } else {
        "Selected Countries"
      }
      report_title <- paste0("Employment Distribution Analysis – ", ref_country)
      
      # ---- Create the Word doc and add title + intro sections ----
      title_style <- fp_text(color = "#722F37", font.size = 16, bold = TRUE)
      doc <- read_docx()
      
      # Styled title line
      doc <- doc %>%
        body_add_fpar(fpar(ftext(report_title, prop = title_style))) %>%
        body_add_par("", style = "Normal") %>%  # spacer
        body_add_par("Introduction", style = "heading 2") %>%
        body_add_par(
          "This section shows the employment distribution of countries.",
          style = "Normal"
        ) %>%
        body_add_par("Size and Characteristics of the public sector", style = "heading 2") %>%
        body_add_par("", style = "Normal")  # spacer before first graph
      
      # =========================
      # First graph: multi-country
      # =========================
      first_graph_data <- filtered_workforce_data() %>%
        dplyr::filter(country_name %in% input$countries_workforce)
      
      if (nrow(first_graph_data) > 0) {
        p1 <- ggplot(first_graph_data,
                     aes(x = country_name, y = value_percentage, fill = indicator_name)) +
          geom_bar(stat = "identity", position = "stack") +
          scale_fill_viridis_d(option = "D") +
          labs(
            title = "Employment distribution by country",
            x = "Country",
            y = "Employment distribution (%)",
            fill = "Sector"
          ) +
          theme_minimal()
        
        img1 <- tempfile("first_graph_", fileext = ".png")
        ggsave(img1, plot = p1, width = 6, height = 4, dpi = 300)
        
        doc <- doc %>%
          body_add_par("First Graph: Public Workforce Distribution by Country", style = "heading 1") %>%
          body_add_img(src = img1, width = 6, height = 4) %>%
          body_add_par(
            "This graph shows the public workforce distribution across multiple countries.",
            style = "Normal"
          )
      } else {
        doc <- doc %>%
          body_add_par("No data available for the selected countries.", style = "Normal")
      }
      
      # =========================
      # Second graph: single country (first vs last year)
      # =========================
      filtered_data <- public_sector_workforce %>%
        dplyr::filter(country_name == input$selected_country)
      
      if (nrow(filtered_data) >= 2) {
        # ensure year is numeric/integer for min/max
        # (skip if already numeric)
        if (!is.numeric(filtered_data$year)) {
          suppressWarnings({
            filtered_data$year <- as.integer(filtered_data$year)
          })
        }
        
        first_year <- suppressWarnings(min(filtered_data$year, na.rm = TRUE))
        last_year  <- suppressWarnings(max(filtered_data$year, na.rm = TRUE))
        
        if (is.finite(first_year) && is.finite(last_year)) {
          
          # Build second_graph_data (only first & last year)
          second_graph_data <- filtered_data %>%
            dplyr::filter(year %in% c(first_year, last_year)) %>%
            dplyr::group_by(year, indicator_name) %>%
            dplyr::summarise(
              value_percentage = sum(value_percentage, na.rm = TRUE),
              .groups = "drop"
            )
          
          if (nrow(second_graph_data) > 0) {
            p2 <- ggplot(second_graph_data,
                         aes(x = value_percentage,
                             y = factor(year, levels = c(last_year, first_year)),
                             fill = indicator_name)) +
              geom_bar(stat = "identity", position = "stack") +
              coord_flip() +
              scale_fill_viridis_d(option = "D") +
              labs(
                title = paste("Sectoral distribution of employment in", input$selected_country),
                x = "Percentage (%)",
                y = "Year",
                fill = "Sector"
              ) +
              theme_minimal()
            
            img2 <- tempfile("second_graph_", fileext = ".png")
            ggsave(img2, plot = p2, width = 6, height = 4, dpi = 300)
            
            doc <- doc %>%
              body_add_par("Second Graph: Employment Distribution", style = "heading 1") %>%
              body_add_img(src = img2, width = 6, height = 4) %>%
              body_add_par(
                "This graph shows the sectoral distribution of public sector workforce for the selected country.",
                style = "Normal"
              )
          } else {
            doc <- doc %>%
              body_add_par("No sectoral data found for the selected years.", style = "Normal")
          }
        } else {
          doc <- doc %>%
            body_add_par("Invalid year data for the selected country.", style = "Normal")
        }
      } else {
        doc <- doc %>%
          body_add_par("Not enough data available for the selected country to create the second graph.", style = "Normal")
      }
      
      # ---- Write the file ----
      print(doc, target = file)
    }
  )
  generate_public_sector_workforce_section <- function(doc, selected_countries) {
    doc <- doc %>% body_add_par("Employment Distribution Analysis", style = "heading 1")
    
    doc <- doc %>% body_add_par(
      "This section presents an analysis of public workforce distribution across different sectors and countries.", 
      style = "Normal"
    )
    
    # ✅ Validate selected countries
    if (is.null(selected_countries) || length(selected_countries) == 0) {
      doc <- doc %>% body_add_par("No countries selected for analysis.", style = "Normal")
      return(doc)
    }
    
    first_country <- selected_countries[1]
    
    # ✅ Filter data
    first_graph_data <- public_sector_workforce %>% 
      filter(country_name %in% selected_countries)
    
    if (nrow(first_graph_data) == 0) {
      doc <- doc %>% body_add_par("No data available for Public Workforce Distribution by Country.", style = "Normal")
      return(doc)
    }
    
    # ✅ Plot graph
    first_graph_ggplot <- ggplot(first_graph_data, aes(x = country_name, y = value_percentage, fill = indicator_name)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_viridis_d(option = "D") +  # Automatically assigns different accessible colors
      labs(
        title = "Public Workforce Distribution by Country", 
        x = "Country", y = "Workforce Distribution (%)", fill = "Sector"
      ) +
      theme_minimal()
    
    img_path1 <- tempfile(fileext = ".png")
    ggsave(img_path1, plot = first_graph_ggplot, width = 8, height = 4)
    
    # ✅ Sector distribution for first country
    sector_distribution <- public_sector_workforce %>%
      filter(country_name == first_country) %>%
      group_by(indicator_name) %>%
      summarise(share = mean(value_percentage, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = indicator_name, values_from = share, values_fill = list(share = 0))
    
    # ✅ Sector averages for other countries
    comparison_distribution <- public_sector_workforce %>%
      filter(country_name %in% selected_countries & country_name != first_country) %>%
      group_by(indicator_name) %>%
      summarise(avg_share = mean(value_percentage, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = indicator_name, values_from = avg_share, values_fill = list(avg_share = 0))
    
    # ✅ Extract values
    `%||%` <- function(x, y) ifelse(is.null(x) || is.na(x), y, x)
    public_admin_share <- sector_distribution$`Public Administration` %||% 0
    education_share <- sector_distribution$Education %||% 0
    health_share <- sector_distribution$Health %||% 0
    
    comparison_education_share <- comparison_distribution$Education %||% 0
    comparison_health_share <- comparison_distribution$Health %||% 0
    
    # ✅ Check for missing values
    if (public_admin_share == 0 && education_share == 0 && health_share == 0) {
      doc <- doc %>% body_add_par(
        paste0("No public sector employment data available for ", first_country, "."),
        style = "Normal"
      )
      return(doc)
    }
    
    # ✅ Largest sector logic
    sector_shares <- c(public_admin_share, education_share, health_share)
    sector_names <- c("Public Administration", "Education", "Health")
    largest_sector <- sector_names[which.max(sector_shares)]
    largest_sector_share <- max(sector_shares)
    
    # ✅ Comparison texts
    education_comparison <- if (education_share > comparison_education_share) {
      paste0("This is higher than the average of ", round(comparison_education_share, 1), "% among the other selected countries.")
    } else {
      paste0("This is lower than the average of ", round(comparison_education_share, 1), "% among the other selected countries.")
    }
    
    health_comparison <- if (health_share > comparison_health_share) {
      paste0("The health sector, while representing a smaller segment at ", 
             round(health_share, 1), "%, still surpasses the average of ", 
             round(comparison_health_share, 1), "% in other selected countries.")
    } else {
      paste0("The health sector accounts for ", round(health_share, 1), "%, aligning closely with the average of ", 
             round(comparison_health_share, 1), "% in other selected countries.")
    }
    
    # ✅ Interpretation
    sector_interpretation_text <- paste0(
      first_country, " has the highest proportion of public sector employees in ", largest_sector,
      ", with ", round(largest_sector_share, 1), "% of paid public sector workers employed in this area. ",
      "The education sector represents ", round(education_share, 1), "% of the workforce. ", education_comparison, " ",
      "The health sector accounts for ", round(health_share, 1), "%, ", health_comparison
    )
    
    # ✅ Add to document
    doc <- doc %>% 
      body_add_par("Public Workforce Distribution by Country", style = "heading 2") %>% 
      body_add_img(src = img_path1, width = 6, height = 4) %>% 
      body_add_par(sector_interpretation_text, style = "Normal")
    
    return(doc)
  }
  
  #Slides
  
  generate_public_sector_workforce_slide <- function(ppt, selected_countries) {
    if (is.null(selected_countries) || length(na.omit(selected_countries)) == 0) {
      return(ppt)
    }
    
    # Filter data
    first_graph_data <- public_sector_workforce %>% 
      filter(country_name %in% selected_countries)
    
    if (nrow(first_graph_data) == 0) {
      return(ppt)
    }
    
    # Create the bar plot
    first_graph_ggplot <- ggplot(first_graph_data, aes(x = country_name, y = value_percentage, fill = indicator_name)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_viridis_d(option = "D") +  # Automatically assigns different accessible colors
      labs(
        title = "Public Workforce Distribution by Country", 
        x = "Country", y = "Workforce Distribution (%)", fill = "Sector"
      ) +
      theme_minimal()
    
    # Save to PNG
    img_path <- tempfile(fileext = ".png")
    ggsave(img_path, plot = first_graph_ggplot, width = 8, height = 4)
    
    # Add slide to ppt
    ppt <- ppt %>%
      add_slide(layout = "Title and Content", master = "Office Theme") %>%
      ph_with(external_img(img_path, height = 5, width = 7), location = ph_location_type(type = "body"))
    
    return(ppt)
  }
  
  
  #Download cvs 
  
  # ---- Download CSV for BOTH public_workforce graphs ----
  output$dl_csv_public_workforce <- downloadHandler(
    filename = function() paste0("public_workforce_data_", Sys.Date(), ".csv"),
    content  = function(file) {
      
      # ---------- Graph 1 data (multi-country, last year available per country/indicator)
      d1 <- tryCatch({
        if (is.null(input$countries_workforce) || !length(input$countries_workforce)) {
          tibble::tibble()
        } else {
          filtered_workforce_data() %>%                      # your reactive
            dplyr::filter(country_name %in% input$countries_workforce) %>%
            dplyr::select(country_name, year, indicator_name, value_percentage) %>%
            dplyr::mutate(graph = "multi_country_last_year")
        }
      }, error = function(e) tibble::tibble())
      
      # ---------- Graph 2 data (single country, first & last year, averaged by indicator)
      d2 <- tryCatch({
        if (is.null(input$selected_country) || !nzchar(input$selected_country)) {
          tibble::tibble()
        } else {
          df2 <- public_sector_workforce %>%
            dplyr::filter(country_name == input$selected_country)
          
          if (nrow(df2) == 0) {
            tibble::tibble()
          } else {
            first_year <- suppressWarnings(min(df2$year, na.rm = TRUE))
            last_year  <- suppressWarnings(max(df2$year, na.rm = TRUE))
            
            if (!is.finite(first_year) || !is.finite(last_year)) {
              tibble::tibble()
            } else {
              df2 %>%
                dplyr::filter(year %in% c(first_year, last_year)) %>%
                dplyr::group_by(year, indicator_name) %>%
                dplyr::summarise(value_percentage = mean(value_percentage, na.rm = TRUE), .groups = "drop") %>%
                dplyr::mutate(
                  country_name = input$selected_country,
                  graph = "single_country_first_last"
                ) %>%
                dplyr::select(country_name, year, indicator_name, value_percentage, graph)
            }
          }
        }
      }, error = function(e) tibble::tibble())
      
      # ---------- Combine & write
      out <- dplyr::bind_rows(d1, d2) %>%
        dplyr::arrange(graph, country_name, indicator_name, year)
      
      # If everything is empty, still write a header-only CSV so the download works
      if (nrow(out) == 0) {
        out <- tibble::tibble(
          country_name = character(),
          year = numeric(),
          indicator_name = character(),
          value_percentage = numeric(),
          graph = character()
        )
      }
      
      utils::write.csv(out, file, row.names = FALSE, na = "")
    }
  )
  
  
  #Tertiary education
  
  output$barPlot <- renderPlotly({
    req(input$selected_countries)
    
    # Filter Data
    filtered_data <- tertiary_education %>% 
      filter(country_name %in% input$selected_countries)
    
    # Check if filtered data is empty
    if (nrow(filtered_data) == 0) {
      return(plotly_empty(type = "bar") %>%
               layout(
                 title = "No data available",
                 annotations = list(
                   text = "No data available for the selected country/countries.",
                   xref = "paper",
                   yref = "paper",
                   showarrow = FALSE,
                   font = list(size = 16),
                   x = 0.5,
                   y = 0.5
                 ),
                 plot_bgcolor = "white",
                 paper_bgcolor = "white"
               ))
    }
    
    # Define Colors
    custom_colors <- c("as a share of private paid employees" = "#0072B2", 
                       "as a share of public paid employees" = "#D55E00")
    
    # Generate Bar Plot
    plot <- filtered_data %>%
      plot_ly(x = ~country_name, y = ~value_percentage, 
              color = ~indicator_name, colors = custom_colors, 
              type = 'bar', barmode = 'group',
              text = ~paste("Country:", country_name,
                            "Indicator:", indicator_name,
                            "Value:", round(value_percentage, 1), "%",
                            "Year:", year),
              textposition = "auto") %>%
      layout(
        title = "Workers with Tertiary Education by Sector and Country",
        xaxis = list(title = "Country"),
        yaxis = list(title = "Tertiary Education (%)"),
        legend = list(title = list(text = "<b>Sector</b>"))
      )
    
    plot
  })
  
  output$note_tertiaryEducation <- renderText({
    "Note: This indicator represents the proportion of individuals with tertiary education in the public and private sectors across selected countries. It highlights differences in educational attainment among paid employees by sector. The indicator shows the last year available for each selected country/region/income group(s)."
  })
  
  output$downloadGraphsWordEducation <- downloadHandler(
    filename = function() paste0("Workers_Tertiary_Education_Report_", Sys.Date(), ".docx"),
    content  = function(file) {
      req(input$selected_countries)
      
      # ---- Filter data ----
      filtered_df <- tertiary_education %>%
        dplyr::filter(country_name %in% input$selected_countries)
      req(nrow(filtered_df) > 0)
      
      # ---- Title text (first selected country) ----
      first_sel    <- input$selected_countries[1]
      report_title <- paste0("Tertiary Education Analysis – ", first_sel)
      
      # ---- Create the Word doc and add title + intro sections ----
      title_style <- fp_text(color = "#722F37", font.size = 16, bold = TRUE)
      doc <- read_docx()
      
      doc <- doc %>%
        # Styled title (Officer fpar/ftext as you requested)
        body_add_fpar(fpar(ftext(report_title, prop = title_style))) %>%
        body_add_par("", style = "Normal") %>%  # spacer
        body_add_par("Introduction", style = "heading 2") %>%
        body_add_par(
          "This section represents the proportion of individuals with tertiary education in the public and private sectors across selected countries.",
          style = "Normal"
        ) %>%
        body_add_par("Size and Characteristics of the public sector", style = "heading 2") %>%
        body_add_par("", style = "Normal")  # spacer before plot
      
      # ---- Choose data label (Region if selected & available, else Country) ----
      region_col <- intersect(c("region", "region_name", "Region"), names(filtered_df))[1]
      label_vec  <- if (!is.na(region_col) && identical(input$label_type_edu, "Region")) {
        filtered_df[[region_col]]
      } else {
        filtered_df$country_name
      }
      
      # ---- Plot (highlight first selection, no legend, black labels) ----
      d_plot <- filtered_df %>%
        dplyr::mutate(
          highlight = ifelse(country_name == first_sel, "Selected country", "Other countries"),
          label     = label_vec,
          country_name = factor(
            country_name,
            levels = c(first_sel, sort(setdiff(unique(country_name), first_sel)))
          )
        )
      
      p <- ggplot(
        d_plot,
        aes(x = country_name, y = value_percentage, fill = indicator_name)
      ) +
        geom_col(                   # same as geom_bar(stat="identity")
          position = position_dodge(width = 0.8),
          width = 0.7
        ) +
        scale_fill_manual(
          values = c(
            "as a share of private paid employees" = "#0072B2",
            "as a share of public paid employees"  = "#D55E00"
          )                                    # <- keep legend visible
        ) +
        labs(
          title = "Tertiary Education by Sector and Country",
          x = "Country",
          y = "Tertiary Education (%)",
          fill = NULL
        ) +
        theme_minimal() +
        theme(
          legend.position = "right",
          plot.margin = margin(10, 40, 10, 10)
        )
      
      # ---- Add plot to Word + note ----
      doc <- doc %>%
        officer::body_add_gg(value = p, width = 6.5, height = 4.5) %>%
        body_add_par(
          "Note: This graph compares the share of workers with tertiary education in the public and private sectors across the selected countries. The first country you selected is highlighted. The indicator shows the last year available for each selected country/region/income group(s).",
          style = "Normal"
        )
      
      # ---- Write the file ----
      print(doc, target = file)
    }
  )
  
  
  generate_tertiary_education_section <- function(doc, selected_countries) {
    # Add Section Title and Introduction
    doc <- doc %>%
      body_add_par("Tertiary Education Analysis", style = "heading 1") %>%
      body_add_par(
        "This section presents an analysis of tertiary education among public and private sector employees across selected countries.", 
        style = "Normal"
      )
    
    # ✅ Ensure valid country selection
    if (is.null(selected_countries) || length(selected_countries) == 0) {
      doc <- doc %>% body_add_par("No countries selected for analysis.", style = "Normal")
      return(doc)
    }
    
    # ✅ Filter relevant data
    filtered_data <- tertiary_education %>%
      filter(country_name %in% selected_countries)
    
    # ✅ Handle empty data
    if (nrow(filtered_data) == 0) {
      doc <- doc %>% body_add_par("No data available for the selected countries.", style = "Normal")
      return(doc)
    }
    
    # ✅ Create ggplot
    ggplot_obj <- ggplot(filtered_data, aes(x = country_name, y = value_percentage, fill = indicator_name)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c(
        "as a share of private paid employees" = "#0072B2", 
        "as a share of public paid employees" = "#D55E00"
      )) +
      labs(
        title = "Workers with Tertiary Education by Sector and Country",
        x = "Country", y = "Tertiary Education (%)", fill = "Sector"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # ✅ Save image
    img_path <- tempfile(fileext = ".png")
    ggsave(img_path, plot = ggplot_obj, width = 8, height = 6)
    
    # ✅ Summary statistics
    avg_public <- round(mean(filtered_data$value_percentage[filtered_data$indicator_name == "as a share of public paid employees"], na.rm = TRUE), 1)
    avg_private <- round(mean(filtered_data$value_percentage[filtered_data$indicator_name == "as a share of private paid employees"], na.rm = TRUE), 1)
    
    highest_public_country <- filtered_data %>%
      filter(indicator_name == "as a share of public paid employees") %>%
      filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>% first()
    
    lowest_public_country <- filtered_data %>%
      filter(indicator_name == "as a share of public paid employees") %>%
      filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>% first()
    
    highest_private_country <- filtered_data %>%
      filter(indicator_name == "as a share of private paid employees") %>%
      filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>% first()
    
    lowest_private_country <- filtered_data %>%
      filter(indicator_name == "as a share of private paid employees") %>%
      filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>% first()
    
    # ✅ Interpretation
    interpretation_text <- paste0(
      "This graph compares tertiary education attainment among employees in the public and private sectors across selected countries. ",
      "On average, ", avg_public, "% of public sector employees have completed tertiary education, while in the private sector, the share is ", avg_private, "%. ",
      "The country with the highest share of tertiary-educated public sector employees is ", highest_public_country, ", whereas ", lowest_public_country, " has the lowest proportion. ",
      "In the private sector, ", highest_private_country, " has the highest tertiary education level among employees, while ", lowest_private_country, " has the lowest."
    )
    
    # ✅ Add plot and interpretation to document
    doc <- doc %>%
      body_add_img(src = img_path, width = 6, height = 4) %>%
      body_add_par("This graph shows the proportion of individuals with tertiary education working in public and private sector employment.", style = "Normal") %>%
      body_add_par(interpretation_text, style = "Normal")
    
    return(doc)
  }
  
  #Slides
  
  generate_tertiary_education_slide <- function(ppt, selected_countries) {
    # ✅ Validate input
    if (is.null(selected_countries) || length(na.omit(selected_countries)) == 0) {
      return(ppt)
    }
    
    # ✅ Filter relevant data
    filtered_data <- tertiary_education %>%
      filter(country_name %in% selected_countries)
    
    if (nrow(filtered_data) == 0) {
      return(ppt)
    }
    
    # ✅ Create ggplot
    plot <- ggplot(filtered_data, aes(x = country_name, y = value_percentage, fill = indicator_name)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c(
        "as a share of private paid employees" = "#0072B2", 
        "as a share of public paid employees" = "#D55E00"
      )) +
      labs(
        title = "Workers with Tertiary Education by Sector and Country",
        x = "Country", y = "Tertiary Education (%)", fill = "Sector"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # ✅ Save plot to image
    img_path <- tempfile(fileext = ".png")
    ggsave(img_path, plot = plot, width = 8, height = 6)
    
    # ✅ Add slide with image only
    ppt <- ppt %>%
      add_slide(layout = "Title and Content", master = "Office Theme") %>%
      ph_with(external_img(img_path, height = 5, width = 7),
              location = ph_location_type(type = "body"))
    
    return(ppt)
  }
  
  #Download cvs
  
  output$dl_csv_tertiary_edu <- downloadHandler(
    filename = function() paste0("tertiary_education_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(input$selected_countries)
      
      out <- tertiary_education %>%
        dplyr::filter(country_name %in% input$selected_countries) %>%
        dplyr::select(country_name, year, indicator_name, value_percentage) %>%
        dplyr::arrange(country_name, indicator_name, year)
      
      # write even if empty so the button still works
      utils::write.csv(out, file, row.names = FALSE, na = "")
    }
  )
  
  
  #Public Sector Wage Premium 
  
  # Render the Dot Plot for Public Sector Wage Premium
  
  output$dotPlot <- renderPlotly({
    req(input$countries_wage_premium)
    
    filtered_data <- public_wage_premium %>%
      dplyr::filter(country_name %in% input$countries_wage_premium) %>%
      dplyr::select(country_name, value_percentage, year) %>%
      tidyr::drop_na(value_percentage)
    
    if (nrow(filtered_data) == 0) {
      return(plotly::plotly_empty(type = "scatter") %>%
               plotly::layout(
                 title = "No data available",
                 annotations = list(
                   text = "No data available for the selected country/countries.",
                   xref = "paper", yref = "paper", showarrow = FALSE,
                   font = list(size = 16), x = 0.5, y = 0.5
                 ),
                 plot_bgcolor = "white", paper_bgcolor = "white"
               ))
    }
    
    filtered_data <- filtered_data %>%
      dplyr::mutate(color = ifelse(country_name == input$countries_wage_premium[1], "#B3242B", "#003366"))
    
    plotly::plot_ly(
      data = filtered_data,
      x = ~country_name, y = ~value_percentage,
      type = "scatter", mode = "markers",
      marker = list(size = 10, opacity = 0.8, color = ~color),
      text = ~paste(
        "Country:", country_name,
        "Value:", round(value_percentage, 1), "%",
        "<br>Year:", year
      )
    ) %>%
      plotly::layout(
        title = "Public Sector Wage Premium (Compared to All Private Employees) by Country",
        xaxis = list(title = "Country"),
        yaxis = list(title = "Public Sector Wage Premium (%)"),
        showlegend = FALSE
      )
  })
  
  output$note_wage_premium <- renderText({
    "Note: This visualization shows the estimated public sector wage premium, compared to private sector counterparts, after controlling for characteristics including gender, education, tenure, and geographic location. The indicator shows the last year available for each selected country/region/income group(s)."
  })
  
  # Download the Report as a Word Document
  output$downloadGraphswagepremium <- downloadHandler(
    filename = function() paste0("Wage_Premium_by_Country_", Sys.Date(), ".docx"),
    content  = function(file) {
      # guard: need at least one country selected
      if (is.null(input$countries_wage_premium) || length(input$countries_wage_premium) == 0) {
        doc <- officer::read_docx() %>%
          body_add_par("Public Sector Wage Premium (Compared to All Private Employees) by Country", style = "heading 1") %>%
          body_add_par("No countries selected.", style = "Normal")
        print(doc, target = file)
        return(invisible())
      }
      
      # data -> same filter as the plotly graph
      sel <- input$countries_wage_premium
      dat <- public_wage_premium %>%
        dplyr::filter(country_name %in% sel) %>%
        dplyr::select(country_name, value_percentage, year) %>%
        tidyr::drop_na(value_percentage)
      
      # if no rows, write a tiny note doc
      if (nrow(dat) == 0) {
        doc <- officer::read_docx() %>%
          body_add_par("Public Sector Wage Premium (Compared to All Private Employees) by Country", style = "heading 1") %>%
          body_add_par("No data available for the selected country/countries.", style = "Normal")
        print(doc, target = file)
        return(invisible())
      }
      
      # highlight first selection like your plotly: red for the first, blue for others
      dat <- dat %>%
        dplyr::mutate(
          highlight = dplyr::if_else(country_name == sel[1], "first", "other")
        )
      
      p <- ggplot(dat, aes(x = country_name, y = value_percentage, color = highlight)) +
        geom_point(size = 3) +
        scale_color_manual(values = c(first = "#B3242B", other = "#003366"), guide = "none") +
        labs(
          title = "Public Sector Wage Premium (Compared to All Private Employees) by Country",
          x = "Country", y = "Public Sector Wage Premium (%)"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
      
      # save plot to a temp image (no hard-coded paths)
      img_path <- tempfile(fileext = ".png")
      ggsave(img_path, plot = p, width = 7, height = 4.5, dpi = 300)
      
      # build the doc
      doc <- officer::read_docx()
      title_style <- officer::fp_text(color = "#222222", font.size = 16, bold = TRUE)
      
      doc <- doc %>%
        body_add_fpar(fpar(ftext("Wage Premium Report", prop = title_style))) %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("Selected countries:", style = "heading 2") %>%
        body_add_par(paste(sel, collapse = ", "), style = "Normal") %>%
        body_add_par("", style = "Normal") %>%
        body_add_par("Chart", style = "heading 2") %>%
        body_add_img(src = img_path, width = 6.5, height = 4) %>%
        body_add_par("Note: The first selected country is highlighted in red.", style = "Normal")
      
      print(doc, target = file)
    }
  )
  
  # Download the Report as a Word Document
  output$downloadGraphswagepremium <- downloadHandler(
    filename = function() paste0("Wage_Premium_Graphs_", Sys.Date(), ".docx"),
    content  = function(file) {
      
      # ---- Title anchor (pick something sensible) ----
      first_sel <- if (!is.null(input$countries_first) && length(input$countries_first) > 0) {
        input$countries_first[1]
      } else if (isTruthy(input$country_second)) {
        input$country_second
      } else if (isTruthy(input$countries_wage_premium)) {
        input$countries_wage_premium[1]
      } else {
        "Selected Country"
      }
      
      report_title <- paste0("Public Sector Wage Premium — ", first_sel)
      
      # ---- Doc scaffolding ----
      title_style <- officer::fp_text(color = "#722F37", font.size = 16, bold = TRUE)
      doc <- officer::read_docx() %>%
        officer::body_add_fpar(officer::fpar(officer::ftext(report_title, prop = title_style))) %>%
        officer::body_add_par("", style = "Normal") %>%
        officer::body_add_par("Introduction", style = "heading 2") %>%
        officer::body_add_par(
          "This section presents the public sector wage premium—the percentage difference in wages between public sector workers and private sector employees—reported separately for men and women in the selected countries and over time for a selected country.",
          style = "Normal"
        ) %>%
        officer::body_add_par("Equity in the Public Sector", style = "heading 2") %>%
        officer::body_add_par("", style = "Normal")
      
      # === GRAPH 0: Cross-section (countries_wage_premium) ===
      if (isTruthy(input$countries_wage_premium)) {
        d0 <- public_wage_premium %>%
          dplyr::filter(country_name %in% input$countries_wage_premium) %>%
          dplyr::select(country_name, value_percentage, year) %>%
          tidyr::drop_na(value_percentage)
        
        if (!is.numeric(d0$value_percentage)) {
          suppressWarnings(d0$value_percentage <- as.numeric(d0$value_percentage))
        }
        
        if (nrow(d0) > 0) {
          d0 <- d0 %>%
            dplyr::mutate(
              highlight = ifelse(country_name == input$countries_wage_premium[1],
                                 "Selected country", "Other countries"),
              country_name = factor(
                country_name,
                levels = c(input$countries_wage_premium[1],
                           sort(setdiff(unique(country_name), input$countries_wage_premium[1])))
              )
            )
          
          p0 <- ggplot2::ggplot(
            d0, ggplot2::aes(x = country_name, y = value_percentage, color = highlight)
          ) +
            ggplot2::geom_point(size = 4.5, alpha = 0.95, show.legend = FALSE) +
            ggplot2::scale_color_manual(values = c(
              "Selected country" = "#B3242B",
              "Other countries"  = "#003366"
            )) +
            ggplot2::labs(
              title = "Public Sector Wage Premium (Compared to All Private Employees) by Country",
              x = "Country", y = "Public Sector Wage Premium (%)"
            ) +
            ggplot2::theme_minimal() +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
          
          doc <- doc %>%
            officer::body_add_par("Public Sector Wage Premium (Cross-Section)", style = "heading 1") %>%
            officer::body_add_gg(value = p0, width = 6.5, height = 4.5) %>%
            officer::body_add_par(
              "Note: This visualization shows the estimated public sector wage premium, compared to private sector counterparts, after controlling for characteristics including gender, education, tenure, and geographic location.",
              style = "Normal"
            ) %>%
            officer::body_add_par("", style = "Normal")
        } else {
          doc <- doc %>%
            officer::body_add_par("Public Sector Wage Premium (Cross-Section)", style = "heading 1") %>%
            officer::body_add_par("No data available for the selected countries.", style = "Normal")
        }
      }
      # ---- Write file ----
      print(doc, target = file)
    } 
  )    
  
  
  generate_wage_premium_report_section <- function(doc, selected_countries) {
    # Add Section Title and Intro
    doc <- doc %>%
      body_add_par("Public Sector Wage Premium Analysis", style = "heading 1") %>%
      body_add_par(
        "This section presents an analysis of public sector wage premiums compared to private sector employees across selected countries.", 
        style = "Normal"
      )
    
    # ✅ Validate country input
    if (is.null(selected_countries) || length(selected_countries) == 0) {
      doc <- doc %>% body_add_par("No countries selected for analysis.", style = "Normal")
      return(doc)
    }
    
    # ✅ Filter data
    filtered_data <- public_wage_premium %>% 
      filter(country_name %in% selected_countries) %>%
      drop_na(value_percentage)
    
    if (nrow(filtered_data) == 0) {
      doc <- doc %>% body_add_par("No data available for the selected countries.", style = "Normal")
      return(doc)
    }
    
    # ✅ Extract first country
    first_country <- selected_countries[1]
    
    # ✅ Summary statistics
    avg_wage_premium <- round(mean(filtered_data$value_percentage, na.rm = TRUE), 1)
    
    highest_country <- filtered_data %>%
      filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>% first()
    
    lowest_country <- filtered_data %>%
      filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>% first()
    
    first_country_premium <- filtered_data %>%
      filter(country_name == first_country) %>%
      pull(value_percentage) %>%
      first() %>% coalesce(0)
    
    comparison_premium <- filtered_data %>%
      filter(country_name != first_country) %>%
      summarise(avg = mean(value_percentage, na.rm = TRUE)) %>%
      pull(avg) %>% coalesce(0)
    
    comparison_statement <- if (!is.na(first_country_premium) && !is.na(comparison_premium)) {
      if (first_country_premium > comparison_premium) {
        paste0("This is higher than the average of ", round(comparison_premium, 1), "% across the other selected countries.")
      } else {
        paste0("This is lower than the average of ", round(comparison_premium, 1), "% across the other selected countries.")
      }
    } else {
      "Comparison data is not available."
    }
    
    # ✅ Plot
    filtered_data <- filtered_data %>%
      mutate(color = ifelse(country_name == first_country, "#0072B2", "#D55E00"))
    
    ggplot_obj <- ggplot(filtered_data, aes(x = country_name, y = value_percentage, color = color)) +
      geom_point(size = 5) +
      scale_color_identity() +
      labs(title = "Public Sector Wage Premium by Country", x = "Country", y = "Wage Premium (%)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    img_path <- tempfile(fileext = ".png")
    ggsave(img_path, plot = ggplot_obj, width = 8, height = 6)
    
    # ✅ Interpretation text
    interpretation_text <- paste0(
      "This graph compares public sector wage premiums across selected countries. ",
      "On average, public sector employees earn ", avg_wage_premium, "% more than private sector employees. ",
      "The country with the highest wage premium is ", highest_country, ", while ", lowest_country, " has the lowest.\n\n",
      "In ", first_country, ", the wage premium is ", round(first_country_premium, 1), "%. ",
      comparison_statement
    )
    
    # ✅ Add to document
    doc <- doc %>% 
      body_add_img(src = img_path, width = 6, height = 4) %>% 
      body_add_par("This graph shows the wage premium in the public sector relative to private sector employees across selected countries.", style = "Normal") %>%
      body_add_par(interpretation_text, style = "Normal")
    
    return(doc)
  }
  
  #Download cvs
  
  output$dl_csv_wage_premium <- downloadHandler(
    filename = function() paste0("public_sector_wage_premium_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(input$countries_wage_premium)
      
      df <- public_wage_premium %>%
        dplyr::filter(country_name %in% input$countries_wage_premium) %>%
        dplyr::select(country_name, year, value_percentage) %>%
        tidyr::drop_na(value_percentage) %>%
        dplyr::mutate(
          highlighted = country_name == input$countries_wage_premium[1],   # matches plot highlight
          color_hex   = ifelse(highlighted, "#B3242B", "#003366")
        ) %>%
        dplyr::arrange(country_name, year)
      
      utils::write.csv(df, file, row.names = FALSE, na = "")
    }
  )
  
  
  #Female share of employment
  
  output$employment_plot <- renderPlotly({
    filtered_data <- gender_workforce %>% 
      filter(country_name %in% input$countries_workforce)
    
    if (nrow(filtered_data) == 0) {
      return(plotly_empty(type = "bar") %>%
               layout(
                 title = "No data available",
                 annotations = list(
                   text = "No data available for the selected country/countries.",
                   xref = "paper",
                   yref = "paper",
                   showarrow = FALSE,
                   font = list(size = 16),
                   x = 0.5,
                   y = 0.5
                 ),
                 plot_bgcolor = "white",
                 paper_bgcolor = "white"
               ))
    }
    
    public_latest <- filtered_data %>% 
      filter(indicator_name == "Females, as a share of public paid employees") %>% 
      group_by(country_name) %>% 
      filter(year == max(year, na.rm = TRUE)) %>% 
      ungroup()
    
    private_latest <- filtered_data %>% 
      filter(indicator_name == "Females, as a share of private paid employees") %>% 
      group_by(country_name) %>% 
      filter(year == max(year, na.rm = TRUE)) %>% 
      ungroup()
    
    if (nrow(public_latest) == 0 || nrow(private_latest) == 0) {
      return(plotly_empty(type = "bar") %>%
               layout(
                 title = "No data available",
                 annotations = list(
                   text = "No data available for one or both sectors in the selected country/countries.",
                   xref = "paper",
                   yref = "paper",
                   showarrow = FALSE,
                   font = list(size = 16),
                   x = 0.5,
                   y = 0.5
                 ),
                 plot_bgcolor = "white",
                 paper_bgcolor = "white"
               ))
    }
    
    plot <- plot_ly(data = public_latest,
                    x = ~country_name,
                    y = ~value_percentage,
                    type = 'bar',
                    color = I("#0072B2"),
                    text = ~paste("Country:", country_name,
                                  "Indicator:", indicator_name,
                                  "Value:", round(value_percentage, 1), "%",
                                  "Year:", year),
                    hoverinfo = "text",
                    name = "Public Sector",
                    showlegend = TRUE) %>%
      add_trace(data = private_latest,
                x = ~country_name,
                y = ~value_percentage,
                type = "scatter",
                mode = "markers",
                marker = list(size = 10, color = "#D55E00"),
                name = "Private Sector",
                text = ~paste("Country: ", country_name, 
                              "<br>Last year available: ", year, 
                              "<br>Employment (%): ", round(value_percentage, 2)),
                hoverinfo = "text",
                showlegend = TRUE) %>%
      layout(barmode = "group",
             title = "Female Employment by Sector (Last Year Available)",
             xaxis = list(title = "Country (Last Year Available)",
                          tickmode = 'array',
                          tickvals = public_latest$country_name,
                          ticktext = paste(public_latest$country_name, "(", public_latest$year, ")")),
             yaxis = list(title = "Employment (%)"),
             legend = list(title = list(text = "Sector")))
    
    plot
  })
  
  output$note_female_employment <- renderText({
    "Note: This indicator represents female employment as a percentage of paid employees in the public and private sectors. Public sector data is displayed as bars, while private sector data is represented as scatter points. The indicator shows the last year available for each selected country/region/income group(s)."
  })
  
  output$note_female_employment_time <- renderText({
    "Note: This visualization explores female employment in the public and private sectors over time for the selected country"
  })
  output$employment_plot_overtime <- renderPlotly({
    filtered_data <- gender_workforce %>% filter(country_name == input$selected_country)
    if(nrow(filtered_data) == 0) return(NULL)
    custom_colors <- c("Females, as a share of private paid employees" = "#0072B2", 
                       "Females, as a share of public paid employees" = "#D55E00")
    plot <- filtered_data %>% plot_ly(x = ~year,
                                      y = ~value_percentage,
                                      color = ~indicator_name,
                                      colors = custom_colors,
                                      type = 'scatter',
                                      mode = 'lines+markers',
                                      hoverinfo = 'text',
                                      text = ~paste("Country:", country_name, "<br>Sector:", indicator_name, "<br>Year:", year, "<br>Female Employment:", value_percentage)) %>%
      layout(title = paste("Female Employment by Sector Over Time in", input$selected_country),
             xaxis = list(title = "Year"),
             yaxis = list(title = "Female Employment (%)"),
             legend = list(title = list(text = "<b>Sector</b>")),
             hovermode = "closest")
    plot <- plot_ly(data = data_to_plot_long,
                    x = ~year,
                    y = ~value_percentage,
                    color = ~indicator_label,
                    colors = c("Male" = "#0072B2", "Female" = "#D55E00"),
                    type = "scatter",
                    mode = "lines+markers") %>%
      add_annotations(x = ~year,
                      y = ~value_percentage,
                      text = ~round(value_percentage, 2),
                      showarrow = FALSE,
                      font = list(size = 12, color = "black"),
                      xanchor = "center",
                      yanchor = "bottom") %>%
      layout(title = "Public Sector Wage Premium Over Time",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Wage Premium (%)"),
             legend = list(title = list(text = "Gender")))
    
    plot
  })
  
  generate_wage_premium_gender_section <- function(doc, selected_countries) {
    # Add section title and intro
    doc <- doc %>%
      body_add_par("Wage Premium Gender Analysis", style = "heading 1") %>%
      body_add_par(
        "This section presents evidence on public sector employment and compensation practices by gender across selected countries.",
        style = "Normal"
      )
    
    if (is.null(selected_countries) || length(selected_countries) == 0) {
      doc <- doc %>% body_add_par("No countries selected for analysis.", style = "Normal")
      return(doc)
    }
    
    first_country <- selected_countries[1]
    
    # ────────────────────────────────────────────────
    # ✅ Graph 1: Multi-Country Wage Premium by Gender
    # ────────────────────────────────────────────────
    data_to_plot <- gender_wage_premium_last %>% filter(country_name %in% selected_countries)
    
    if (nrow(data_to_plot) > 0) {
      # Graph
      multi_country_plot <- ggplot(data_to_plot, aes(x = country_name, y = value_percentage, color = indicator_label)) +
        geom_point(size = 3) +
        scale_color_manual(
          values = c(
            "Male" = "#0072B2",    # Colorblind-friendly blue
            "Female" = "#D55E00"   # Colorblind-friendly orange/red
          )
        ) +
        labs(
          title = "Public Sector Wage Premium by Gender (Latest Year)",
          x = "Country", y = "Wage Premium (%)", color = "Gender"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      
      img_path1 <- tempfile(fileext = ".png")
      ggsave(img_path1, plot = multi_country_plot, width = 8, height = 6)
      
      # Summary statistics
      avg_premium <- round(mean(data_to_plot$value_percentage, na.rm = TRUE), 1)
      
      highest_country <- data_to_plot %>%
        filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
        pull(country_name) %>% first()
      
      lowest_country <- data_to_plot %>%
        filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
        pull(country_name) %>% first()
      
      first_country_premium <- data_to_plot %>%
        filter(country_name == first_country) %>%
        summarise(avg = mean(value_percentage, na.rm = TRUE)) %>%
        pull(avg) %>% round(1)
      
      comparison_premium <- data_to_plot %>%
        filter(country_name != first_country) %>%
        summarise(avg = mean(value_percentage, na.rm = TRUE)) %>%
        pull(avg) %>% round(1)
      
      comparison_statement <- if (first_country_premium > comparison_premium) {
        paste0("This is higher than the average of ", comparison_premium, "% across the other selected countries.")
      } else {
        paste0("This is lower than the average of ", comparison_premium, "% across the other selected countries.")
      }
      
      interpretation1 <- paste0(
        "This graph compares public sector wage premiums by gender across selected countries. ",
        "On average, the gender wage premium is ", avg_premium, "%. ",
        "The highest premium is observed in ", highest_country, ", and the lowest in ", lowest_country, ". ",
        "In ", first_country, ", the premium is ", first_country_premium, "%. ", comparison_statement
      )
      
      # Add to doc
      doc <- doc %>%
        body_add_par("Wage Premium by Gender (Multi-Country)", style = "heading 2") %>%
        body_add_img(src = img_path1, width = 6, height = 4) %>%
        body_add_par(interpretation1, style = "Normal")
    } else {
      doc <- doc %>% body_add_par("No data available for the selected countries (multi-country comparison).", style = "Normal")
    }
    
    # ─────────────────────────────────────────────
    # ✅ Graph 2: Time Series for First Country
    # ─────────────────────────────────────────────
    time_series_data <- gender_wage_premium %>% filter(country_name == first_country)
    
    if (nrow(time_series_data) > 0) {
      time_series_plot <- ggplot(time_series_data, aes(x = year, y = value_percentage, color = indicator_label, group = indicator_label)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        scale_color_manual(
          values = c(
            "Male" = "#0072B2",    # Blue - Colorblind-friendly
            "Female" = "#D55E00"   # Vermillion - Colorblind-friendly
          )
        ) +
        labs(
          title = paste("Wage Premium by Gender Over Time in", first_country),
          x = "Year", y = "Wage Premium (%)", color = "Gender"
        ) +
        theme_minimal()
      
      img_path2 <- tempfile(fileext = ".png")
      ggsave(img_path2, plot = time_series_plot, width = 8, height = 6)
      
      first_year <- min(time_series_data$year, na.rm = TRUE)
      last_year  <- max(time_series_data$year, na.rm = TRUE)
      
      get_value_for_year <- function(df, year, gender) {
        val <- df %>% filter(year == year, indicator_label == gender) %>%
          pull(value_percentage) %>% first()
        ifelse(is.na(val), "Data not available", round(val, 1))
      }
      
      male_first  <- get_value_for_year(time_series_data, first_year, "Male")
      female_first <- get_value_for_year(time_series_data, first_year, "Female")
      male_last   <- get_value_for_year(time_series_data, last_year, "Male")
      female_last  <- get_value_for_year(time_series_data, last_year, "Female")
      
      interpretation2 <- paste0(
        "This graph shows how the public sector wage premium by gender evolved in ", first_country, ". ",
        "In ", first_year, ", the wage premium was ", male_first, "% for men and ", female_first, "% for women. ",
        "By ", last_year, ", it was ", male_last, "% for men and ", female_last, "% for women."
      )
      
      # Add to doc
      doc <- doc %>%
        body_add_par("Wage Premium by Gender (Over Time)", style = "heading 2") %>%
        body_add_img(src = img_path2, width = 6, height = 4) %>%
        body_add_par(interpretation2, style = "Normal")
    } else {
      doc <- doc %>% body_add_par(paste("No time series data available for", first_country), style = "Normal")
    }
    
    return(doc)
  }
  
  #Slides
  
  generate_wage_premium_gender_slide <- function(ppt, selected_countries) {
    if (is.null(selected_countries) || length(na.omit(selected_countries)) == 0) {
      return(ppt)
    }
    
    first_country <- selected_countries[1]
    
    # ────────────────────────────────────────────────
    # Graph 1: Multi-Country Wage Premium by Gender
    # ────────────────────────────────────────────────
    data_to_plot <- gender_wage_premium_last %>% filter(country_name %in% selected_countries)
    
    if (nrow(data_to_plot) > 0) {
      multi_country_plot <- ggplot(data_to_plot, aes(x = country_name, y = value_percentage, color = indicator_label)) +
        geom_point(size = 3) +
        scale_color_manual(
          values = c(
            "Male" = "#0072B2",    # Colorblind-friendly blue
            "Female" = "#D55E00"   # Colorblind-friendly orange/red
          )
        ) +
        labs(
          title = "Public Sector Wage Premium by Gender (Latest Year)",
          x = "Country", y = "Wage Premium (%)", color = "Gender"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      img_path1 <- tempfile(fileext = ".png")
      ggsave(img_path1, plot = multi_country_plot, width = 8, height = 6)
      
      ppt <- ppt %>%
        add_slide(layout = "Title and Content", master = "Office Theme") %>%
        ph_with(external_img(img_path1, height = 5, width = 7), location = ph_location_type(type = "body"))
    }
    
    # ─────────────────────────────────────────────
    # Graph 2: Time Series for First Country
    # ─────────────────────────────────────────────
    time_series_data <- gender_wage_premium %>% filter(country_name == first_country)
    
    if (nrow(time_series_data) > 0) {
      time_series_plot <- ggplot(time_series_data, aes(x = year, y = value_percentage, color = indicator_label, group = indicator_label)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        scale_color_manual(
          values = c(
            "Male" = "#0072B2",    # Blue - Colorblind-friendly
            "Female" = "#D55E00"   # Vermillion - Colorblind-friendly
          )
        ) +
        labs(
          title = paste("Wage Premium by Gender Over Time in", first_country),
          x = "Year", y = "Wage Premium (%)", color = "Gender"
        ) +
        theme_minimal()
      
      img_path2 <- tempfile(fileext = ".png")
      ggsave(img_path2, plot = time_series_plot, width = 8, height = 6)
      
      ppt <- ppt %>%
        add_slide(layout = "Title and Content", master = "Office Theme") %>%
        ph_with(external_img(img_path2, height = 5, width = 7), location = ph_location_type(type = "body"))
    }
    
    return(ppt)
  }
  
  #Download cvs 
  
  # ---- indicator_name values as in your dataset ----
  .public_label  <- "as a share of public paid employees"
  .private_label <- "as a share of private paid employees"
  
  .sector_from <- function(x) dplyr::case_when(
    x == .public_label  ~ "Public",
    x == .private_label ~ "Private",
    TRUE ~ NA_character_
  )
  
  output$dl_csv_gender_emp_both <- downloadHandler(
    filename = function() paste0("female_employment_public_private_", Sys.Date(), ".csv"),
    content  = function(file) {
      
      # accept either set of inputs (workforce tab OR gender tab)
      countries_multi <- if (!is.null(input$countries_workforce) && length(input$countries_workforce)) {
        input$countries_workforce
      } else if (!is.null(input$countries_gender) && length(input$countries_gender)) {
        input$countries_gender
      } else character(0)
      
      country_single <- if (!is.null(input$selected_country) && nzchar(input$selected_country)) {
        input$selected_country
      } else if (!is.null(input$country_gender) && nzchar(input$country_gender)) {
        input$country_gender
      } else NA_character_
      
      empty_schema <- tibble::tibble(
        graph            = character(),
        country_name     = character(),
        sector           = character(),
        year             = numeric(),
        value_percentage = numeric()
      )
      
      # ---------- A) cross-section (latest per country & sector) ----------
      dA <- tryCatch({
        if (!length(countries_multi)) return(empty_schema)
        
        gender_workforce %>%
          dplyr::filter(
            country_name %in% countries_multi,
            indicator_name %in% c(.public_label, .private_label)
          ) %>%
          dplyr::group_by(country_name, indicator_name) %>%
          dplyr::slice_max(order_by = year, n = 1, with_ties = FALSE) %>%
          dplyr::ungroup() %>%
          dplyr::transmute(
            graph  = "cross_section_last_year",
            country_name,
            sector = .sector_from(indicator_name),
            year   = as.numeric(year),
            value_percentage = as.numeric(value_percentage)
          )
      }, error = function(e) empty_schema)
      
      # ---------- B) over-time (single country, both sectors) ----------
      dB <- tryCatch({
        if (is.na(country_single) || !nzchar(country_single)) return(empty_schema)
        
        gender_workforce %>%
          dplyr::filter(
            country_name == country_single,
            indicator_name %in% c(.public_label, .private_label)
          ) %>%
          dplyr::mutate(
            graph  = "single_country_overtime",
            sector = .sector_from(indicator_name)
          ) %>%
          dplyr::select(graph, country_name, sector, year, value_percentage) %>%
          dplyr::arrange(year, sector)
      }, error = function(e) empty_schema)
      
      out <- dplyr::bind_rows(dA, dB)
      if (nrow(out) == 0) out <- empty_schema
      
      utils::write.csv(out, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    }
  )
  
  
  # Wage premium by Education Level 
  
  # Render the Public Sector Wage Premium by Education Level Graph
  
  output$education_wage_premium_plot <- renderPlotly({
    req(input$selected_country)  # Ensure a country is selected
    
    # Filter the data set for the selected country
    filtered_data <- public_wage_premium_educ %>%
      filter(country_name == input$selected_country) %>%
      drop_na(value_percentage)  # Remove NAs
    
    # Check if there's data
    if (nrow(filtered_data) == 0) {
      return(plotly_empty(type = "bar") %>%
               layout(
                 title = "No data available",
                 annotations = list(
                   text = "No data available for the selected country.",
                   xref = "paper",
                   yref = "paper",
                   showarrow = FALSE,
                   font = list(size = 16),
                   x = 0.5,
                   y = 0.5
                 ),
                 plot_bgcolor = "white",
                 paper_bgcolor = "white"
               ))
    }
    
    # Define colorblind-friendly colors for education levels
    education_colors <- c(
      "No Education"        = "#E69F00",  # Orange
      "Primary Education"   = "#56B4E9",  # Sky Blue
      "Secondary Education" = "#009E73",  # Bluish Green
      "Tertiary Education"  = "#D55E00"   # Vermillion
    )
    
    # Create the bar plot
    filtered_data$text <- paste(
      "Country:", filtered_data$country_name,
      "Indicator:", filtered_data$indicator_name,
      "Value:", round(filtered_data$value_percentage, 1), "%",
      "Year:", filtered_data$year
    )
    
    p <- ggplot(filtered_data, aes(x = indicator_name, y = value_percentage, fill = indicator_name)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = education_colors) +
      labs(
        title = "Public Sector Wage Premium by Education Level (Compared to Private Formal Workers)",
        x = "Education Level",
        y = "Wage Premium (%)",
        fill = "Education Level"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")  # Convert ggplot to Plotly for interactivity
  })
  
  output$note_education_wage_premium <- renderText({
    "Note: This indicator represents the public sector wage premium across different education levels, comparing public sector wages to those of private formal workers. The indicator shows the last year available for each selected country/region/income group(s)."
  })
  
  output$downloadEducationWagePremium <- downloadHandler(
    filename = function() paste0("Public_Sector_Wage_Premium_Education_Level_", Sys.Date(), ".docx"),
    content  = function(file) {
      req(input$selected_country)
      
      # ---- Filter and validate data ----
      filtered_df <- public_wage_premium_educ %>%
        dplyr::filter(country_name == input$selected_country) %>%
        tidyr::drop_na(value_percentage)
      
      req(nrow(filtered_df) > 0)
      
      # Ensure numeric (in case it was character)
      if (!is.numeric(filtered_df$value_percentage)) {
        suppressWarnings({
          filtered_df$value_percentage <- as.numeric(filtered_df$value_percentage)
        })
      }
      
      # ---- Title text (selected country) ----
      report_title <- paste0("Public Sector Wage Premium by Education Level – ", input$selected_country)
      
      # ---- Create the Word doc and add title + sections (same style as other reports) ----
      title_style <- fp_text(color = "#722F37", font.size = 16, bold = TRUE)
      doc <- read_docx()
      
      doc <- doc %>%
        body_add_fpar(fpar(ftext(report_title, prop = title_style))) %>%
        body_add_par("", style = "Normal") %>%   # spacer
        body_add_par("Introduction", style = "heading 2") %>%
        body_add_par(
          paste0(
            "This section presents the public sector wage premium—the percentage difference in wages ",
            "between public sector workers and their private formal counterparts—by education level for ",
            input$selected_country, "."
          ),
          style = "Normal"
        ) %>%
        body_add_par("Competitiveness of public sector", style = "heading 2") %>%
        body_add_par("", style = "Normal")       # spacer before plot
      
      # ---- Order levels for a pleasant left-to-right (optional) ----
      edu_order <- c("No Education", "Primary Education", "Secondary Education", "Tertiary Education")
      if ("indicator_name" %in% names(filtered_df)) {
        filtered_df$indicator_name <- factor(filtered_df$indicator_name,
                                             levels = intersect(edu_order, unique(filtered_df$indicator_name)))
      }
      
      # ---- Plot (bar by education level; keep legend visible) ----
      p <- ggplot2::ggplot(
        filtered_df,
        ggplot2::aes(x = indicator_name, y = value_percentage, fill = indicator_name)
      ) +
        ggplot2::geom_col(width = 0.7) +
        ggplot2::scale_fill_manual(
          values = c(
            "No Education"        = "#E69F00",
            "Primary Education"   = "#56B4E9",
            "Secondary Education" = "#009E73",
            "Tertiary Education"  = "#D55E00"
          ),
          name = NULL
        ) +
        ggplot2::labs(
          title = "Public Sector Wage Premium by Education Level",
          x = NULL,                      # remove axis title
          y = "Wage Premium (%)"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::coord_cartesian(clip = "off") +
        ggplot2::theme(
          legend.position = "right",
          plot.margin = ggplot2::margin(10, 40, 10, 10),
          axis.text.x  = ggplot2::element_blank(),   # <-- hide labels
          axis.ticks.x = ggplot2::element_blank()    # <-- hide tick marks
        )
      
      # ---- Add plot + note (same pattern as your other reports) ----
      doc <- doc %>%
        officer::body_add_gg(value = p, width = 6.5, height = 4.5) %>%
        body_add_par(
          "Note: Estimates reflect the public sector wage premium relative to private formal workers, by education level, controlling for observable characteristics where available. The indicator shows the last year available for each selected country/region/income group(s).",
          style = "Normal"
        )
      
      # ---- Write the file ----
      print(doc, target = file)
    }
  )
  
  generate_wage_premium_education_section <- function(doc, selected_countries) {
    # Section Title
    doc <- doc %>% body_add_par("Public Sector Wage Premium by Education Level", style = "heading 1")
    
    # Validate selected countries
    if (is.null(selected_countries) || length(selected_countries) == 0) {
      doc <- doc %>% body_add_par("No countries selected for analysis.", style = "Normal")
      return(doc)
    }
    
    # Extract first country
    first_country <- selected_countries[1]
    
    # Add intro paragraph
    doc <- doc %>% body_add_par(
      paste0("This section presents an analysis of public sector wage premiums based on different education levels for ", 
             first_country, ". The comparison is made against private sector formal workers."), 
      style = "Normal"
    )
    
    # Filter data
    filtered_data <- public_wage_premium_educ %>%
      filter(country_name %in% selected_countries) %>%
      drop_na(value_percentage)
    
    if (nrow(filtered_data) == 0) {
      doc <- doc %>% body_add_par("No data available for the selected countries.", style = "Normal")
      return(doc)
    }
    
    # Plot
    ggplot_obj <- ggplot(filtered_data, aes(x = indicator_name, y = value_percentage, fill = indicator_name)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c(
        "No Education"       = "#E69F00",  # Orange
        "Primary Education"  = "#56B4E9",  # Sky Blue
        "Secondary Education"= "#009E73",  # Bluish Green
        "Tertiary Education" = "#D55E00"   # Vermillion
      )) +
      labs(
        title = "Public Sector Wage Premium by Education Level",
        x = "Education Level",
        y = "Wage Premium (%)",
        fill = "Education Level"
      ) +
      theme_minimal()
    
    img_path <- tempfile(fileext = ".png")
    ggsave(img_path, plot = ggplot_obj, width = 8, height = 6)
    
    # Summary stats
    highest_education <- filtered_data %>%
      filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
      pull(indicator_name) %>%
      first()
    
    lowest_education <- filtered_data %>%
      filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
      pull(indicator_name) %>%
      first()
    
    avg_wage_premium <- round(mean(filtered_data$value_percentage, na.rm = TRUE), 1)
    
    first_country_premium <- filtered_data %>%
      filter(country_name == first_country) %>%
      summarise(avg_premium = mean(value_percentage, na.rm = TRUE)) %>%
      pull(avg_premium) %>%
      coalesce(0)
    
    comparison_premium <- filtered_data %>%
      filter(country_name != first_country) %>%
      summarise(avg_other_countries = mean(value_percentage, na.rm = TRUE)) %>%
      pull(avg_other_countries) %>%
      coalesce(0)
    
    comparison_statement <- if (first_country_premium > comparison_premium) {
      paste0("This is higher than the average of ", round(comparison_premium, 1), "% across the other selected countries.")
    } else {
      paste0("This is lower than the average of ", round(comparison_premium, 1), "% across the other selected countries.")
    }
    
    # Interpretation
    interpretation_text <- paste0(
      "This graph illustrates public sector wage premiums by education level in ", first_country, 
      ", comparing earnings with private sector formal workers. ",
      "On average, the public sector wage premium in ", first_country, " is ", round(first_country_premium, 0), "%. ",
      "The highest wage premium is observed for those with ", highest_education, 
      ", while the lowest is for those with ", lowest_education, ". ", comparison_statement
    )
    
    # Add to doc
    doc <- doc %>% 
      body_add_img(src = img_path, width = 6, height = 4) %>% 
      body_add_par("This graph shows the public sector wage premium by education level, comparing earnings with private sector formal workers.", style = "Normal") %>%
      body_add_par(interpretation_text, style = "Normal")
    
    return(doc)
  }
  
  #Slides
  
  generate_wage_premium_education_slide <- function(ppt, selected_countries) {
    if (is.null(selected_countries) || length(na.omit(selected_countries)) == 0) {
      return(ppt)
    }
    
    # Filter data
    filtered_data <- public_wage_premium_educ %>%
      filter(country_name %in% selected_countries) %>%
      drop_na(value_percentage)
    
    if (nrow(filtered_data) == 0) {
      return(ppt)
    }
    
    # Generate plot
    ggplot_obj <- ggplot(filtered_data, aes(x = indicator_name, y = value_percentage, fill = indicator_name)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c(
        "No Education"       = "#E69F00",  # Orange
        "Primary Education"  = "#56B4E9",  # Sky Blue
        "Secondary Education"= "#009E73",  # Bluish Green
        "Tertiary Education" = "#D55E00"   # Vermillion
      )) +
      labs(
        title = "Public Sector Wage Premium by Education Level",
        x = "Education Level",
        y = "Wage Premium (%)",
        fill = "Education Level"
      ) +
      theme_minimal()
    
    # Save image
    img_path <- tempfile(fileext = ".png")
    ggsave(img_path, plot = ggplot_obj, width = 8, height = 6)
    
    # Add slide
    ppt <- ppt %>%
      add_slide(layout = "Title and Content", master = "Office Theme") %>%
      ph_with(external_img(img_path, height = 5, width = 7), location = ph_location_type(type = "body"))
    
    return(ppt)
  }
  
  #Download cvs 
  
  output$dl_csv_wagepremium_educ <- downloadHandler(
    filename = function() {
      cn <- if (!is.null(input$selected_country) && nzchar(input$selected_country))
        gsub("[^A-Za-z0-9]+", "_", input$selected_country) else "selected_country"
      paste0("public_wage_premium_by_education_", cn, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(input$selected_country)
      
      # data used by the plot
      df <- public_wage_premium_educ %>%
        dplyr::filter(country_name == input$selected_country) %>%
        tidyr::drop_na(value_percentage) %>%
        dplyr::transmute(
          country_name,
          year = as.numeric(year),
          education_level = indicator_name,          # matches your plot's x
          wage_premium_pct = as.numeric(value_percentage)
        )
      
      # latest year per education level (for clean cross-section)
      latest <- df %>%
        dplyr::group_by(education_level) %>%
        dplyr::slice_max(order_by = year, n = 1, with_ties = FALSE) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(view = "latest_per_education")
      
      # full time series for the selected country
      all_years <- df %>%
        dplyr::mutate(view = "all_years")
      
      out <- dplyr::bind_rows(latest, all_years) %>%
        dplyr::arrange(view, education_level, year)
      
      utils::write.csv(out, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    }
  )
  
  
  #Public Sector Graphs 
  
  # First Graph - Multi-Country Dot Plot
  
  output$firstGraphpublic <- renderPlotly({
    filtered_data <- public_sector_emp_temp_last %>% 
      filter(country_name %in% input$countries_first)
    
    # If there's no data, show a fallback message
    if (nrow(filtered_data) == 0) {
      return(plotly_empty(type = "scatter") %>%
               layout(
                 title = "No data available",
                 annotations = list(
                   text = "No data available for the selected country/countries.",
                   xref = "paper",
                   yref = "paper",
                   showarrow = FALSE,
                   font = list(size = 16),
                   x = 0.5,
                   y = 0.5
                 ),
                 plot_bgcolor = "white",
                 paper_bgcolor = "white"
               ))
    }
    
    # Otherwise, generate the plot
    
    filtered_data$text <- paste(
      "Country:", filtered_data$country_name,
      "Indicator:", filtered_data$indicator_label,
      "Value:", round(filtered_data$value_percentage, 1), "%",
      "Year:", filtered_data$year
    )
    
    
    ggplotly(
      ggplot(filtered_data, aes(x = country_name, y = value_percentage, color = indicator_label, text = text)) +
        geom_point(size = 4) +
        scale_color_manual(values = c(
          "as a share of formal employment" = "#E69F00",
          "as a share of paid employment"   = "#56B4E9",
          "as a share of total employment"  = "#009E73"
        )) +
        labs(
          title = "Public Sector Employment (Last Year Available)", 
          x = "Country", 
          y = "Value", 
          color = "Indicator"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      , tooltip = "text") %>%
      layout(legend = list(title = list(text = "Indicator")))
    
  })
  
  
  output$note_firstGraphpublic <- renderText({
    "Note:This visualization shows the relative size of public sector employment in the labor market, measured by its share of formal, paid, and total employment for the latest available year.The indicator shows the last year available for each selected country/region/income group(s). "
  })
  
  # Second Graph - Single-Country Line Plot
  
  output$secondGraphpublic <- renderPlotly({
    filtered_data <- public_sector_emp_temp %>% 
      filter(country_name == input$country_second)
    
    # Fallback if no data available
    if (nrow(filtered_data) == 0) {
      return(plotly_empty(type = "scatter") %>%
               layout(
                 title = "No data available",
                 annotations = list(
                   text = "No data available for the selected country.",
                   xref = "paper",
                   yref = "paper",
                   showarrow = FALSE,
                   font = list(size = 16),
                   x = 0.5,
                   y = 0.5
                 ),
                 plot_bgcolor = "white",
                 paper_bgcolor = "white"
               ))
    }
    
    # Plot if data exists
    ggplotly(
      ggplot(filtered_data, aes(x = year, y = value_percentage, color = indicator_label)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        scale_color_manual(values = c(
          "as a share of formal employment" = "#E69F00",  # Orange
          "as a share of paid employment"   = "#56B4E9",  # Sky Blue
          "as a share of total employment"  = "#009E73"   # Bluish Green
        )) +
        labs(
          title = "Public Sector Employment Over Time", 
          x = "Year", 
          y = "Value", 
          color = "Indicator"
        ) +
        theme_minimal()
    ) %>%
      layout(legend = list(title = list(text = "Indicator")))
  })
  
  
  output$note_secondGraphpublic <- renderText({
    "Note: This visualization shows the evolution of public sector employment in the labor market of the selected country, expressed as its share of formal, paid, and total employment."
  })
  
  
  # Download Handler - Save Graphs to Word Document
  output$downloadGraphsWord <- downloadHandler(
    filename = function() paste0("Public_Sector_Employment_", Sys.Date(), ".docx"),
    content = function(file) {
      
      req(input$countries_first, length(input$countries_first) > 0, input$country_second)
      
      first_sel <- input$countries_first[1]
      
      # --- data (unchanged) ---
      d1 <- public_sector_emp_temp_last %>%
        dplyr::filter(country_name %in% input$countries_first) %>%
        dplyr::mutate(
          indicator_label = factor(indicator_label, levels = c(
            "as a share of formal employment",
            "as a share of paid employment",
            "as a share of total employment"
          )),
          country_name = factor(country_name, levels = input$countries_first)
        )
      req(nrow(d1) > 0, cancelOutput = TRUE)
      
      d2 <- public_sector_emp_temp %>%
        dplyr::filter(country_name == input$country_second) %>%
        dplyr::mutate(
          indicator_label = factor(indicator_label, levels = c(
            "as a share of formal employment",
            "as a share of paid employment",
            "as a share of total employment"
          ))
        )
      req(nrow(d2) > 0, cancelOutput = TRUE)
      
      # --- doc start ---
      doc <- officer::read_docx()
      
      # 1. Title with rule under it
      h1_txt <- paste0("1. Public Sector Employment — ", first_sel)
      h1_fmt <- officer::fp_text(font.size = 20, bold = TRUE)
      rule   <- officer::fp_border(color = "#000000", width = 1)
      p_rule <- officer::fp_par(border.bottom = rule, padding.bottom = 4)
      
      doc <- doc |>
        officer::body_add_fpar(officer::fpar(officer::ftext(h1_txt, h1_fmt))) |>
        officer::body_add_fpar(officer::fpar(officer::ftext(""), fp_p = p_rule))
      
      # 1.1 Introduction  —— your “This report …” text goes here
      h2_fmt <- officer::fp_text(font.size = 14, bold = TRUE)
      doc <- doc |>
        officer::body_add_fpar(officer::fpar(officer::ftext("1.1 Introduction", h2_fmt))) |>
        officer::body_add_par(
          "This report presents the analysis of public sector employment across selected countries and its trend over time.",
          style = "Normal"
        )
      
      # 1.2 Size and Characteristics of the Public Sector (before graphs)
      doc <- doc |>
        officer::body_add_fpar(
          officer::fpar(officer::ftext("1.2 Size and Characteristics of the Public Sector", h2_fmt))
        )
      
      # ---- Graph 1 + note ----
      p1 <- ggplot2::ggplot(d1, ggplot2::aes(x = country_name, y = value_percentage, color = indicator_label)) +
        ggplot2::geom_point(size = 4) +
        ggplot2::scale_color_manual(values = c(
          "as a share of formal employment" = "#E69F00",
          "as a share of paid employment"   = "#56B4E9",
          "as a share of total employment"  = "#009E73"
        ), drop = FALSE, name = "Indicator") +
        ggplot2::labs(title = "Public Sector Employment (Last Year Available)", x = "Country", y = "Value (%)") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
      
      img1 <- tempfile(fileext = ".png"); ggplot2::ggsave(img1, plot = p1, width = 8, height = 6, dpi = 300)
      doc <- doc |>
        officer::body_add_img(src = img1, width = 6.5, height = 4.8) |>
        officer::body_add_par(
          "Note: This visualization shows the relative size of public sector employment in the labor market, measured by its share of formal, paid, and total employment for the latest available year.",
          style = "Normal"
        )
      
      # ---- Graph 2 + note ----
      p2 <- ggplot2::ggplot(d2, ggplot2::aes(x = year, y = value_percentage, color = indicator_label, group = indicator_label)) +
        ggplot2::geom_line(size = 1.1) +
        ggplot2::geom_point(size = 2.8) +
        ggplot2::scale_color_manual(values = c(
          "as a share of formal employment" = "#E69F00",
          "as a share of paid employment"   = "#56B4E9",
          "as a share of total employment"  = "#009E73"
        ), drop = FALSE, name = "Indicator") +
        ggplot2::labs(title = paste0("Public Sector Employment Over Time — ", input$country_second), x = "Year", y = "Value (%)") +
        ggplot2::theme_minimal()
      
      img2 <- tempfile(fileext = ".png"); ggplot2::ggsave(img2, plot = p2, width = 8, height = 6, dpi = 300)
      doc <- doc |>
        officer::body_add_img(src = img2, width = 6.5, height = 4.8) |>
        officer::body_add_par(
          "Note: This visualization shows the evolution of public sector employment in the labor market of the selected country, expressed as its share of formal, paid, and total employment.",
          style = "Normal"
        )
      
      print(doc, target = file)
    }
  )
  generate_public_sector_employment_section <- function(doc, selected_countries) {
    # Add Section Title and Intro
    doc <- doc %>% 
      body_add_par("Public Sector Employment Analysis", style = "heading 1") %>% 
      body_add_par(
        "This section presents the analysis of public sector employment across selected countries and its trend over time.", 
        style = "Normal"
      )
    
    # Validate selected countries
    if (is.null(selected_countries) || length(selected_countries) == 0) {
      doc <- doc %>% body_add_par("No countries selected for analysis.", style = "Normal")
      return(doc)
    }
    
    first_country <- selected_countries[1]
    if (is.na(first_country)) {
      doc <- doc %>% body_add_par("Invalid country selection.", style = "Normal")
      return(doc)
    }
    
    # Filter data for selected countries (last available year)
    filtered_data <- public_sector_emp_temp_last %>% 
      filter(country_name %in% selected_countries)
    
    if (nrow(filtered_data) == 0) {
      doc <- doc %>% body_add_par("No data available for the selected countries.", style = "Normal")
      return(doc)
    }
    
    # First Graph
    first_graph <- ggplot(filtered_data, aes(x = country_name, y = value_percentage, color = indicator_label)) +
      geom_point(size = 4) +
      scale_color_manual(values = c(
        "as a share of formal employment" = "#E69F00",  # Orange
        "as a share of paid employment"   = "#56B4E9",  # Sky Blue
        "as a share of total employment"  = "#009E73"   # Bluish Green
      )) +
      labs(
        title = "Public Sector Employment (Last Year Available)", 
        x = "Country", 
        y = "Employment (%)", 
        color = "Sector"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    img_path1 <- tempfile(fileext = ".png")
    ggsave(img_path1, plot = first_graph, width = 8, height = 6)
    
    # Helper functions
    get_value <- function(df, country, indicator) {
      val <- df %>% filter(country_name == country, indicator_label == indicator) %>% 
        pull(value_percentage) %>% first()
      ifelse(length(val) == 0 || is.na(val), "Data not available", round(val, 0))
    }
    
    get_extreme_country <- function(df, indicator, func) {
      row <- df %>% 
        filter(indicator_label == indicator, !is.na(value_percentage)) %>%
        filter(value_percentage == func(value_percentage, na.rm = TRUE)) %>%
        slice(1)
      
      if (nrow(row) == 0) return(list(country = "N/A", value = "Data not available"))
      return(list(country = row$country_name, value = round(row$value_percentage, 0)))
    }
    
    # Extract values for first country
    formal_first <- get_value(filtered_data, first_country, "as a share of formal employment")
    paid_first   <- get_value(filtered_data, first_country, "as a share of paid employment")
    total_first  <- get_value(filtered_data, first_country, "as a share of total employment")
    
    # Extract max/min across countries
    formal_highest <- get_extreme_country(filtered_data, "as a share of formal employment", max)
    formal_lowest  <- get_extreme_country(filtered_data, "as a share of formal employment", min)
    paid_highest   <- get_extreme_country(filtered_data, "as a share of paid employment", max)
    paid_lowest    <- get_extreme_country(filtered_data, "as a share of paid employment", min)
    total_highest  <- get_extreme_country(filtered_data, "as a share of total employment", max)
    total_lowest   <- get_extreme_country(filtered_data, "as a share of total employment", min)
    
    # Comparison logic
    compare_value <- function(value, high, low) {
      if (value == "Data not available") return("data not available")
      if (value > high) {
        return("This is the highest employment rate among the selected countries.")
      } else if (value < low) {
        return("This is the lowest employment rate among the selected countries.")
      } else {
        return("This falls within the range observed across the selected countries.")
      }
    }
    
    formal_comparison <- compare_value(formal_first, formal_highest$value, formal_lowest$value)
    paid_comparison   <- compare_value(paid_first, paid_highest$value, paid_lowest$value)
    total_comparison  <- compare_value(total_first, total_highest$value, total_lowest$value)
    
    # Interpretation
    interpretation_text1 <- paste0(
      "This graph compares public sector employment across selected countries. ",
      "For employment as a share of formal employment, the highest level is in ", formal_highest$country, 
      " at ", formal_highest$value, "%, while the lowest is in ", formal_lowest$country, 
      " at ", formal_lowest$value, "%.\n\n",
      "For employment as a share of paid employment, the highest level is in ", paid_highest$country, 
      " at ", paid_highest$value, "%, while the lowest is in ", paid_lowest$country, 
      " at ", paid_lowest$value, "%.\n\n",
      "For employment as a share of total employment, the highest level is in ", total_highest$country, 
      " at ", total_highest$value, "%, while the lowest is in ", total_lowest$country, 
      " at ", total_lowest$value, "%.\n\n",
      "In ", first_country, ", public sector employment as a share of formal employment is ", formal_first, "%. ", formal_comparison, "\n",
      "As a share of paid employment, it is ", paid_first, "%. ", paid_comparison, "\n",
      "As a share of total employment, it is ", total_first, "%. ", total_comparison
    )
    
    # Add content to doc
    doc <- doc %>%
      body_add_par("Public Sector Employment - Last Year Available", style = "heading 2") %>%
      body_add_img(src = img_path1, width = 6, height = 4) %>%
      body_add_par(interpretation_text1, style = "Normal")
    
    return(doc)
  }
  
  #Slides 
  
  generate_public_sector_employment_slide <- function(ppt, selected_countries) {
    if (is.null(selected_countries) || length(na.omit(selected_countries)) == 0) {
      return(ppt)
    }
    
    # Filter data for selected countries (last available year)
    filtered_data <- public_sector_emp_temp_last %>% 
      filter(country_name %in% selected_countries)
    
    if (nrow(filtered_data) == 0) {
      return(ppt)  # No data, skip slide
    }
    
    # Plot: Public Sector Employment by Country
    first_graph <- ggplot(filtered_data, aes(x = country_name, y = value_percentage, color = indicator_label)) +
      geom_point(size = 4) +
      scale_color_manual(values = c(
        "as a share of formal employment" = "#E69F00",  # Orange
        "as a share of paid employment" = "#56B4E9",    # Sky Blue
        "as a share of total employment" = "#009E73"    # Bluish Green
      )) +
      labs(
        title = "Public Sector Employment (Last Year Available)", 
        x = "Country", 
        y = "Employment (%)", 
        color = "Sector"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Save image to temp file
    img_path <- tempfile(fileext = ".png")
    ggsave(img_path, plot = first_graph, width = 8, height = 6)
    
    # Add slide with the plot only
    ppt <- ppt %>%
      add_slide(layout = "Title and Content", master = "Office Theme") %>%
      ph_with(external_img(img_path, height = 5, width = 7), location = ph_location_type(type = "body"))
    
    return(ppt)
  }
  
  #Download cvs 
  
  output$dl_public_emp_data <- downloadHandler(
    filename = function() {
      paste0("public_sector_employment_data_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      # ===== Graph 1: Multi-country (last year available) =====
      d1 <- public_sector_emp_temp_last %>%
        dplyr::filter(country_name %in% input$countries_first) %>%
        tidyr::drop_na(value_percentage) %>%
        dplyr::transmute(
          country_name,
          indicator = indicator_label,
          year = as.numeric(year),
          value_percentage = as.numeric(value_percentage)
        ) %>%
        dplyr::arrange(country_name, indicator)
      
      # ===== Graph 2: Single-country over time =====
      d2 <- public_sector_emp_temp %>%
        dplyr::filter(country_name == input$country_second) %>%
        tidyr::drop_na(value_percentage) %>%
        dplyr::transmute(
          country_name,
          indicator = indicator_label,
          year = as.numeric(year),
          value_percentage = as.numeric(value_percentage)
        ) %>%
        dplyr::arrange(year, indicator)
      
      # write two sheets
      writexl::write_xlsx(
        x = list(
          "Graph1_MultiCountry_LastYear" = d1,
          "Graph2_SingleCountry_OverTime" = d2
        ),
        path = file
      )
    }
  )
  
  #Gender Wage premium 
  
  # First Graph - Multi-Country Dot Plot for Wage Premium by Gender
  output$firstGraphGenderWagePremium <- renderPlotly({
    filtered_data <- gender_wage_premium_last %>% 
      filter(country_name %in% input$countries_first)
    
    # Show fallback if no data available
    if (nrow(filtered_data) == 0) {
      return(plotly_empty(type = "scatter") %>%
               layout(
                 title = "No data available",
                 annotations = list(
                   text = "No data available for the selected country/countries.",
                   xref = "paper",
                   yref = "paper",
                   showarrow = FALSE,
                   font = list(size = 16),
                   x = 0.5,
                   y = 0.5
                 ),
                 plot_bgcolor = "white",
                 paper_bgcolor = "white"
               ))
    }
    
    # Create the actual plot
    filtered_data$text <- paste(
      "Country:", filtered_data$country_name,
      "Gender:", filtered_data$indicator_label,
      "Wage Premium:", round(filtered_data$value_percentage, 1), "%",
      "Year:", filtered_data$year
    )
    
    
    ggplotly(
      ggplot(filtered_data, aes(x = country_name, y = value_percentage, color = indicator_label, text = text)) +
        geom_point(size = 4) +
        scale_color_manual(values = c(
          "Male" = "#E69F00",    # Orange
          "Female" = "#56B4E9"   # Sky Blue
        )) +
        labs(
          title = "Public Sector Wage Premium by Gender (Last Year Available)", 
          x = "Country", 
          y = "Wage Premium (%)",
          color = "Indicator"
        ) +
        theme_minimal(),
      tooltip = "text"
    )
  })
  
  output$note_firstGraphGenderWagePremium <- renderText({
    "Note: Each dot shows the public-sector wage premium for the latest available year in each country. The premium is the percentage difference in average wages in the public sector relative to the private sector within the same gender. Positive values mean higher wages in the public sector; negative values mean lower. Country coverage and reference years may differ across countries. The indicator shows the last year available for each selected country/region/income group(s)."
  })
  
  # Second Graph - Single-Country Line Plot for Wage Premium by Gender Over Time
  
  output$secondGraphGenderWagePremium <- renderPlotly({
    filtered_data <- gender_wage_premium %>% 
      filter(country_name == input$country_second)
    
    # Show fallback if no data is available
    if (nrow(filtered_data) == 0) {
      return(plotly_empty(type = "scatter") %>%
               layout(
                 title = "No data available",
                 annotations = list(
                   text = "No data available for the selected country.",
                   xref = "paper",
                   yref = "paper",
                   showarrow = FALSE,
                   font = list(size = 16),
                   x = 0.5,
                   y = 0.5
                 ),
                 plot_bgcolor = "white",
                 paper_bgcolor = "white"
               ))
    }
    
    # Create the ggplot
    min_y <- min(filtered_data$value_percentage, na.rm = TRUE) - 5
    
    ggplotly(
      ggplot(filtered_data, aes(x = year, y = value_percentage, color = indicator_label)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        scale_color_manual(values = c(
          "Male" = "#E69F00",    # Orange
          "Female" = "#56B4E9"   # Sky Blue
        )) +
        labs(
          title = "Public Sector Wage Premium by Gender Over Time", 
          x = "Year", 
          y = "Wage Premium (%)",
          color = "Indicator"
        ) +
        theme_minimal() +
        annotate("text", x = Inf, y = min_y,
                 label = "This indicator represents the gender wage premium across industries in the public sector.",
                 hjust = 1, size = 4, color = "black", fontface = "italic")
    )
  })
  
  output$note_secondGraphGenderWagePremium <- renderText({
    "Note: This indicator represents the public sector wage premium for men and women over time, comparing compensation in the public sector with that in the private sector for each gender."
  })
  
  # Download Handler - Save Gender Wage Premium Graphs to Word Document
  output$downloadGraphswagepremiumbygender <- downloadHandler(
    filename = function() {
      paste0("Public_Sector_Wage_Premium_Gender_", Sys.Date(), ".docx")
    },
    content = function(file) {
      doc <- read_docx()
      
      # Title
      title_style <- fp_text(color = "#722F37", font.size = 16, bold = TRUE)
      doc <- doc %>% body_add_fpar(fpar(ftext("Public Sector Wage Premium by Gender", prop = title_style)))
      
      # Intro Text
      doc <- doc %>% body_add_par(
        "This report presents an analysis of public sector wage premium by gender across selected countries and its trend over time.", 
        style = "Normal"
      )
      
      # First Graph - Save as Image
      first_graph <- ggplot(gender_wage_premium_last %>% filter(country_name %in% input$countries_first), 
                            aes(x = country_name, y = value_percentage, color = indicator_label)) +
        geom_point(size = 4) +
        scale_color_manual(values = c(
          "Male" = "#E69F00",    # Orange
          "Female" = "#56B4E9"   # Sky Blue
        )) +
        labs(
          title = "Public Sector Wage Premium by Gender (Last Year Available)", 
          x = "Country", 
          y = "Wage Premium (%)",
          color = "Gender"
        ) +
        theme_minimal()
      
      img_path1 <- tempfile(fileext = ".png")
      ggsave(img_path1, plot = first_graph, width = 8, height = 6)
      
      doc <- doc %>% body_add_par("Public Sector Wage Premium by Gender - Last Year Available", style = "heading 2")
      doc <- doc %>% body_add_img(src = img_path1, width = 6, height = 4)
      
      # Second Graph - Save as Image
      second_graph <- ggplot(gender_wage_premium %>% filter(country_name == input$country_second), 
                             aes(x = year, y = value_percentage, color = indicator_label)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        scale_color_manual(values = c(
          "Male" = "#E69F00",    # Orange
          "Female" = "#56B4E9"   # Sky Blue
        )) +
        labs(
          title = "Public Sector Wage Premium by Gender Over Time", 
          x = "Year", 
          y = "Wage Premium (%)",
          color = "Gender"
        ) +
        theme_minimal()
      
      
      img_path2 <- tempfile(fileext = ".png")
      ggsave(img_path2, plot = second_graph, width = 8, height = 6)
      
      doc <- doc %>% body_add_par("Public Sector Wage Premium by Gender Over Time", style = "heading 2")
      doc <- doc %>% body_add_img(src = img_path2, width = 6, height = 4) %>% 
        body_add_par("Note: This indicator represents the gender wage premium across industries in the public sector.", 
                     style = "Normal")
      
      # Save the Document
      print(doc, target = file)
    }
  )
  
  generate_wage_premium_gender_report_section <- function(doc, selected_countries) {
    # Section title and intro
    doc <- doc %>%
      body_add_par("Public Sector Wage Premium by Gender", style = "heading 1") %>%
      body_add_par(
        "This section presents an analysis of the public sector wage premium by gender across selected countries.",
        style = "Normal"
      )
    
    # Validate selection
    if (is.null(selected_countries) || length(na.omit(selected_countries)) == 0) {
      doc <- doc %>% body_add_par("No countries selected for analysis.", style = "Normal")
      return(doc)
    }
    
    first_country <- selected_countries[1]
    
    # Filter for last available year data
    filtered_data <- gender_wage_premium_last %>% 
      filter(country_name %in% selected_countries) %>%
      drop_na(value_percentage)
    
    if (nrow(filtered_data) == 0) {
      doc <- doc %>% body_add_par("No data available for the selected countries.", style = "Normal")
      return(doc)
    }
    
    # Safe extraction helpers
    get_rounded_value <- function(df, country, indicator) {
      val <- df %>% filter(country_name == country, indicator_label == indicator) %>%
        pull(value_percentage) %>% first()
      ifelse(is.na(val), "Data not available", round(val, 0))
    }
    
    get_max_country <- function(df, indicator) {
      df_filtered <- df %>% filter(indicator_label == indicator)
      max_val <- max(df_filtered$value_percentage, na.rm = TRUE)
      max_row <- df_filtered %>% filter(value_percentage == max_val)
      if (nrow(max_row) == 0) return(list(country = "N/A", value = "Data not available"))
      return(list(country = max_row$country_name[1], value = round(max_row$value_percentage[1], 0)))
    }
    
    male_first_country <- get_rounded_value(filtered_data, first_country, "Male")
    female_first_country <- get_rounded_value(filtered_data, first_country, "Female")
    
    max_male   <- get_max_country(filtered_data, "Male")
    max_female <- get_max_country(filtered_data, "Female")
    
    # First graph
    first_graph <- ggplot(filtered_data, aes(x = country_name, y = value_percentage, color = indicator_label)) +
      geom_point(size = 4) +
      scale_color_manual(values = c(
        "Male" = "#E69F00",    # Orange
        "Female" = "#56B4E9"   # Sky Blue
      )) +
      labs(
        title = "Public Sector Wage Premium by Gender (Last Year Available)",
        x = "Country", y = "Wage Premium (%)", color = "Gender"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    
    img_path1 <- tempfile(fileext = ".png")
    ggsave(img_path1, plot = first_graph, width = 8, height = 6)
    
    interpretation_text1 <- paste0(
      "This graph displays the public sector wage premium by gender for the last available year across the selected countries. ",
      "In ", first_country, ", the wage premium for male employees is ", male_first_country,
      "%, while for female employees, it is ", female_first_country, "%. ",
      "The country with the highest male wage premium is ", max_male$country, " at ", max_male$value,
      "%, while the highest female wage premium is in ", max_female$country, " at ", max_female$value, "%."
    )
    
    doc <- doc %>%
      body_add_par("Public Sector Wage Premium by Gender (Last Year Available)", style = "heading 2") %>%
      body_add_img(src = img_path1, width = 6, height = 4) %>%
      body_add_par(interpretation_text1, style = "Normal")
    
    # Time series for first selected country
    time_series_data <- gender_wage_premium %>% 
      filter(country_name == first_country)
    
    if (nrow(time_series_data) > 0) {
      second_graph <- ggplot(time_series_data, aes(x = year, y = value_percentage, color = indicator_label, group = indicator_label)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        scale_color_manual(values = c(
          "Male" = "#E69F00",    # Orange
          "Female" = "#56B4E9"   # Sky Blue
        )) +
        labs(
          title = paste("Public Sector Wage Premium by Gender Over Time in", first_country),
          x = "Year", y = "Wage Premium (%)", color = "Gender"
        ) +
        theme_minimal()
      
      
      img_path2 <- tempfile(fileext = ".png")
      ggsave(img_path2, plot = second_graph, width = 8, height = 6)
      
      first_year <- 2010
      last_year  <- max(time_series_data$year, na.rm = TRUE)
      
      get_year_value <- function(df, year, indicator) {
        val <- df %>% filter(year == year, indicator_label == indicator) %>%
          pull(value_percentage) %>% first()
        ifelse(is.na(val), "Data not available", round(val, 0))
      }
      
      male_2010    <- get_year_value(time_series_data, first_year, "Male")
      female_2010  <- get_year_value(time_series_data, first_year, "Female")
      male_last    <- get_year_value(time_series_data, last_year, "Male")
      female_last  <- get_year_value(time_series_data, last_year, "Female")
      
      interpretation_text2 <- paste0(
        "This graph illustrates how the public sector wage premium by gender has evolved over time in ", first_country, ". ",
        "In ", first_year, ", the male wage premium was ", male_2010, "% and the female wage premium was ", female_2010, "%. ",
        "By ", last_year, ", the male wage premium is ", male_last, "% and the female wage premium is ", female_last, "%."
      )
      
      doc <- doc %>%
        body_add_par("Public Sector Wage Premium by Gender Over Time", style = "heading 2") %>%
        body_add_img(src = img_path2, width = 6, height = 4) %>%
        body_add_par(interpretation_text2, style = "Normal")
      
    } else {
      doc <- doc %>% body_add_par("No time series data available for this country.", style = "Normal")
    }
    
    return(doc)
  }
  
  #Slides
  
  generate_wage_premium_gender_report_slide <- function(ppt, selected_countries) {
    if (is.null(selected_countries) || length(na.omit(selected_countries)) == 0) {
      return(ppt)
    }
    
    first_country <- selected_countries[1]
    
    # Filter for last available year (cross-country)
    filtered_data <- gender_wage_premium_last %>% 
      filter(country_name %in% selected_countries) %>%
      drop_na(value_percentage)
    
    if (nrow(filtered_data) > 0) {
      # Graph 1: Public Sector Wage Premium by Gender (Last Year Available)
      first_graph <- ggplot(filtered_data, aes(x = country_name, y = value_percentage, color = indicator_label)) +
        geom_point(size = 4) +
        scale_color_manual(values = c(
          "Male" = "#E69F00",   # Orange
          "Female" = "#56B4E9"  # Sky Blue
        )) +
        labs(
          title = "Public Sector Wage Premium by Gender (Last Year Available)",
          x = "Country", y = "Wage Premium (%)", color = "Gender"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      
      img_path1 <- tempfile(fileext = ".png")
      ggsave(img_path1, plot = first_graph, width = 8, height = 6)
      
      ppt <- ppt %>%
        add_slide(layout = "Title and Content", master = "Office Theme") %>%
        ph_with(external_img(img_path1, height = 5, width = 7), location = ph_location_type(type = "body"))
    }
    
    # Graph 2: Time series for first country
    time_series_data <- gender_wage_premium %>% 
      filter(country_name == first_country)
    
    if (nrow(time_series_data) > 0) {
      second_graph <- ggplot(time_series_data, aes(x = year, y = value_percentage, color = indicator_label, group = indicator_label)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        scale_color_manual(values = c(
          "Male" = "#E69F00",   # Orange
          "Female" = "#56B4E9"  # Sky Blue
        )) +
        labs(
          title = paste("Public Sector Wage Premium by Gender Over Time in", first_country),
          x = "Year", y = "Wage Premium (%)", color = "Gender"
        ) +
        theme_minimal()
      
      img_path2 <- tempfile(fileext = ".png")
      ggsave(img_path2, plot = second_graph, width = 8, height = 6)
      
      ppt <- ppt %>%
        add_slide(layout = "Title and Content", master = "Office Theme") %>%
        ph_with(external_img(img_path2, height = 5, width = 7), location = ph_location_type(type = "body"))
    }
    
    return(ppt)
  }
  
  #Download CVS
  
  # ---- Data behind: 
  # Graph 1 -> gender_wage_premium_last filtered by input$countries_first
  # Graph 2 -> gender_wage_premium filtered by input$country_second
  # -----------------------------------------------
  output$dl_gender_wageprem_xlsx <- downloadHandler(
    filename = function() paste0("wage_premium_by_gender_", Sys.Date(), ".xlsx"),
    content = function(file) {
      
      # Graph 1: multi-country, last year available
      d1 <- gender_wage_premium_last %>%
        dplyr::filter(country_name %in% input$countries_first) %>%
        tidyr::drop_na(value_percentage) %>%
        dplyr::transmute(
          country_name,
          gender = indicator_label,                    # "Male"/"Female"
          year   = as.numeric(year),
          wage_premium_pct = as.numeric(value_percentage)
        ) %>%
        dplyr::arrange(country_name, gender)
      
      # Graph 2: single country over time
      d2 <- gender_wage_premium %>%
        dplyr::filter(country_name == input$country_second) %>%
        tidyr::drop_na(value_percentage) %>%
        dplyr::transmute(
          country_name,
          gender = indicator_label,
          year   = as.numeric(year),
          wage_premium_pct = as.numeric(value_percentage)
        ) %>%
        dplyr::arrange(year, gender)
      
      writexl::write_xlsx(
        x = list(
          "Graph1_MultiCountry_LastYear"  = d1,
          "Graph2_SingleCountry_OverTime" = d2
        ),
        path = file
      )
    }
  )
  
  
  
  # Gender Workforce Graphs
  
  output$firstGraphGenderWorkforce <- renderPlotly({
    req(input$countries_gender)
    
    # 1) keep only selected countries
    d <- gender_workforce %>%
      dplyr::filter(country_name %in% input$countries_gender)
    
    if (nrow(d) == 0) {
      return(plotly::plotly_empty(type = "bar") %>%
               plotly::layout(
                 title = "No data available",
                 annotations = list(
                   text = "No data available for the selected country/countries.",
                   xref = "paper", yref = "paper", showarrow = FALSE,
                   font = list(size = 16), x = 0.5, y = 0.5
                 ),
                 plot_bgcolor = "white", paper_bgcolor = "white"
               ))
    }
    
    # 2) for each country & sector, keep the last available year
    d_last <- d %>%
      dplyr::group_by(country_name, indicator_name) %>%
      dplyr::arrange(year, .by_group = TRUE) %>%
      dplyr::slice_tail(n = 1) %>%           # with_ties = FALSE via arrange+slice_tail
      dplyr::ungroup()
    
    # 3) factor levels for consistent colors
    d_last$indicator_name <- factor(
      d_last$indicator_name,
      levels = c("as a share of private paid employees",
                 "as a share of public paid employees")
    )
    
    # 4) tooltip
    d_last$text <- paste0(
      "Country: ", d_last$country_name,
      "<br>Sector: ", d_last$indicator_name,
      "<br>Employment: ", round(d_last$value_percentage, 1), "%",
      "<br>Year: ", d_last$year
    )
    
    # 5) plot
    plotly::ggplotly(
      ggplot2::ggplot(
        d_last,
        ggplot2::aes(x = country_name, y = value_percentage,
                     fill = indicator_name, text = text)
      ) +
        ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7) +
        ggplot2::scale_fill_manual(values = c(
          "as a share of private paid employees" = "#E69F00",
          "as a share of public paid employees"  = "#56B4E9"
        )) +
        ggplot2::labs(
          title = "Female Employment by Sector (Last Year Available)",
          x = "Country", y = "Employment (%)", fill = "Sector"
        ) +
        ggplot2::theme_minimal(),
      tooltip = "text"
    )
  })
  
  output$note_firstGraphGenderWorkforce <- renderText({
    "Note: This indicator represents the share of females employed in the public and private sectors. It highlights gender differences in workforce participation by sector. The indicator shows the last year available for each selected country/region/income group(s)."
  })
  
  # Second Graph - Single-Country Line Plot
  
  output$secondGraphGenderWorkforce <- renderPlotly({
    filtered_data <- gender_workforce %>% 
      filter(country_name == input$country_gender)
    
    # Show fallback if no data is available
    if (nrow(filtered_data) == 0) {
      return(plotly_empty(type = "scatter") %>%
               layout(
                 title = paste("No data available for", input$country_gender),
                 annotations = list(
                   text = "No data available for the selected country.",
                   xref = "paper",
                   yref = "paper",
                   showarrow = FALSE,
                   font = list(size = 16),
                   x = 0.5,
                   y = 0.5
                 ),
                 plot_bgcolor = "white",
                 paper_bgcolor = "white"
               ))
    }
    
    # Ensure factor levels match color scale
    filtered_data$indicator_name <- factor(filtered_data$indicator_name, 
                                           levels = c("as a share of private paid employees", 
                                                      "as a share of public paid employees"))
    
    # Create the line chart
    ggplotly(
      ggplot(filtered_data, aes(x = year, y = value_percentage, color = indicator_name)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        scale_color_manual(values = c(
          "as a share of private paid employees" = "#E69F00",  # Orange
          "as a share of public paid employees" = "#56B4E9"    # Sky Blue
        )) +
        labs(
          title = paste("Female Employment by Sector Over Time in", input$country_gender), 
          x = "Year", 
          y = "Female Employment (%)", 
          color = "Sector"
        ) +
        theme_minimal()
    )
  })
  
  output$note_secondGraphGenderWorkforce <- renderText({
    "Note: This visualization explores female employment in the public and private sectors over time for the selected country"
  })
  
  # Download Handler - Save Graphs to Word Document
  output$downloadGraphsWordGender <- downloadHandler(
    filename = function() paste0("Female_Share_of_Employment_", Sys.Date(), ".docx"),
    content  = function(file) {
      
      req(input$countries_gender, length(input$countries_gender) > 0, input$country_gender)
      
      # --- Data ---
      d1 <- gender_workforce %>%
        dplyr::filter(country_name %in% input$countries_gender) %>%
        dplyr::group_by(country_name, indicator_name) %>%
        dplyr::arrange(year, .by_group = TRUE) %>%
        dplyr::slice_tail(n = 1) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          indicator_name = factor(
            indicator_name,
            levels = c("as a share of private paid employees",
                       "as a share of public paid employees")
          ),
          country_name = factor(country_name, levels = input$countries_gender)
        )
      req(nrow(d1) > 0, cancelOutput = TRUE)
      
      d2 <- gender_workforce %>%
        dplyr::filter(country_name == input$country_gender) %>%
        dplyr::mutate(
          indicator_name = factor(
            indicator_name,
            levels = c("as a share of private paid employees",
                       "as a share of public paid employees")
          )
        )
      req(nrow(d2) > 0, cancelOutput = TRUE)
      
      # --- Title (fp_text style you requested) ---
      first_sel    <- input$countries_gender[1]
      report_title <- paste0("Female Share of Employment — ", first_sel)
      
      title_style <- officer::fp_text(color = "#722F37", font.size = 16, bold = TRUE)
      doc <- officer::read_docx() %>%
        officer::body_add_fpar(officer::fpar(officer::ftext(report_title, prop = title_style))) %>%
        officer::body_add_par("", style = "Normal")  # spacer
      
      # --- Sections ---
      h2_fmt <- officer::fp_text(font.size = 14, bold = TRUE)
      doc <- doc %>%
        officer::body_add_fpar(officer::fpar(officer::ftext("Introduction", h2_fmt))) %>%
        officer::body_add_par(
          "This report presents the female share of employment in the public and private sectors across selected countries and over time for a selected country.",
          style = "Normal"
        ) %>%
        officer::body_add_fpar(
          officer::fpar(officer::ftext("1.2 Equity in the Public Sector", h2_fmt))
        ) %>%
        officer::body_add_par("", style = "Normal")  # spacer before graphs
      
      # --- Graph 1 (last year available) + note ---
      p1 <- ggplot2::ggplot(
        d1, ggplot2::aes(x = country_name, y = value_percentage, fill = indicator_name)
      ) +
        ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7) +
        ggplot2::scale_fill_manual(values = c(
          "as a share of private paid employees" = "#E69F00",
          "as a share of public paid employees"  = "#56B4E9"
        ), drop = FALSE, name = "Sector") +
        ggplot2::labs(
          title = "Female Employment by Sector (Last Year Available)",
          x = "Country", y = "Employment (%)"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
      
      img1 <- tempfile(fileext = ".png"); ggplot2::ggsave(img1, plot = p1, width = 8, height = 6, dpi = 300)
      doc <- doc %>%
        officer::body_add_img(src = img1, width = 6.5, height = 4.8) %>%
        officer::body_add_par(
          "Note: For each country, bars show the latest available estimate of the female share of employment in the public and private sectors.",
          style = "Normal"
        )
      
      # --- Graph 2 (time series) + note ---
      p2 <- ggplot2::ggplot(
        d2, ggplot2::aes(x = year, y = value_percentage, color = indicator_name, group = indicator_name)
      ) +
        ggplot2::geom_line(size = 1.1) +
        ggplot2::geom_point(size = 2.8) +
        ggplot2::scale_color_manual(values = c(
          "as a share of private paid employees" = "#E69F00",
          "as a share of public paid employees"  = "#56B4E9"
        ), drop = FALSE, name = "Sector") +
        ggplot2::labs(
          title = paste0("Female Employment Over Time — ", input$country_gender),
          x = "Year", y = "Employment (%)"
        ) +
        ggplot2::theme_minimal()
      
      img2 <- tempfile(fileext = ".png"); ggplot2::ggsave(img2, plot = p2, width = 8, height = 6, dpi = 300)
      doc <- doc %>%
        officer::body_add_img(src = img2, width = 6.5, height = 4.8) %>%
        officer::body_add_par(
          "Note: This figure shows the evolution of the female share of employment in the public and private sectors for the selected country.",
          style = "Normal"
        )
      
      # --- Write file ---
      print(doc, target = file)
    }
  )
  
  
  generate_gender_workforce_section <- function(doc, selected_countries) {
    # Add Section Title
    doc <- doc %>% body_add_par("Gender Workforce Analysis", style = "heading 1")
    
    # Add Introduction
    doc <- doc %>% body_add_par(
      "This section presents an analysis of female employment in the public and private sectors across selected countries.", 
      style = "Normal"
    )
    
    # ✅ Ensure at least one country is selected
    if (is.null(selected_countries) || length(selected_countries) == 0) {
      doc <- doc %>% body_add_par("No countries selected for analysis.", style = "Normal")
      return(doc)
    }
    
    # ✅ Extract the first selected country
    first_country <- selected_countries[1]
    
    # ✅ Filter data for selected countries (last available year)
    filtered_data <- gender_workforce %>% 
      filter(country_name %in% selected_countries)
    
    # ✅ Handle empty dataset case
    if (nrow(filtered_data) == 0) {
      doc <- doc %>% body_add_par("No data available for the selected countries.", style = "Normal")
      return(doc)
    }
    
    # ✅ Generate Plot
    first_graph <- ggplot(filtered_data, 
                          aes(x = country_name, y = round(value_percentage, 0), fill = indicator_name)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c(
        "as a share of private paid employees" = "#E69F00",  # orange
        "as a share of public paid employees" = "#56B4E9"    # sky blue
      )) +
      labs(
        title = "Female Employment by Sector (Last Year Available)", 
        x = "Country", y = "Employment (%)", fill = "Sector"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    img_path1 <- tempfile(fileext = ".png")
    ggsave(img_path1, plot = first_graph, width = 8, height = 6)
    
    # ✅ Summary Statistics
    avg_public <- round(mean(filtered_data$value_percentage[filtered_data$indicator_name == "as a share of public paid employees"], na.rm = TRUE), 0)
    avg_private <- round(mean(filtered_data$value_percentage[filtered_data$indicator_name == "as a share of private paid employees"], na.rm = TRUE), 0)
    
    get_country_extreme <- function(data, label, func) {
      data %>%
        filter(indicator_name == label, value_percentage == func(value_percentage, na.rm = TRUE)) %>%
        pull(country_name) %>%
        first()
    }
    
    highest_public_country <- get_country_extreme(filtered_data, "as a share of public paid employees", max)
    lowest_public_country  <- get_country_extreme(filtered_data, "as a share of public paid employees", min)
    highest_private_country <- get_country_extreme(filtered_data, "as a share of private paid employees", max)
    lowest_private_country  <- get_country_extreme(filtered_data, "as a share of private paid employees", min)
    
    # ✅ Extract first country stats
    get_country_value <- function(data, country, label) {
      data %>%
        filter(country_name == country, indicator_name == label) %>%
        pull(value_percentage) %>%
        first() %>%
        coalesce(0) %>%
        round(0)
    }
    
    first_country_public  <- get_country_value(filtered_data, first_country, "as a share of public paid employees")
    first_country_private <- get_country_value(filtered_data, first_country, "as a share of private paid employees")
    
    # ✅ Comparisons
    comparison_public <- if (first_country_public > avg_public) {
      paste0("This is higher than the average of ", avg_public, "% across the other selected countries.")
    } else {
      paste0("This is lower than the average of ", avg_public, "% across the other selected countries.")
    }
    
    comparison_private <- if (first_country_private > avg_private) {
      paste0("This is higher than the average of ", avg_private, "% across the other selected countries.")
    } else {
      paste0("This is lower than the average of ", avg_private, "% across the other selected countries.")
    }
    
    # ✅ Interpretation Text
    interpretation_text1 <- paste0(
      "This graph compares female employment in the public and private sectors across selected countries. ",
      "On average, ", avg_public, "% of public sector employees are female, while in the private sector, the share is ", avg_private, "%. ",
      "The highest female employment in the public sector is in ", highest_public_country, ", while the lowest is in ", lowest_public_country, ". ",
      "In the private sector, ", highest_private_country, " has the highest share of female employees, whereas ", lowest_private_country, " has the lowest.\n\n",
      "In ", first_country, ", female representation in the public sector is ", first_country_public, "%.", 
      " ", comparison_public, "\n",
      "In the private sector, female representation in ", first_country, " is ", first_country_private, "%. ",
      comparison_private
    )
    
    # ✅ Add Content to Word Document
    doc <- doc %>% 
      body_add_par("Female Employment by Sector (Last Year Available)", style = "heading 2") %>% 
      body_add_img(src = img_path1, width = 6, height = 4) %>% 
      body_add_par(interpretation_text1, style = "Normal")
    
    return(doc)
  }
  
  #Slides
  
  generate_gender_workforce_slide <- function(ppt, selected_countries) {
    if (is.null(selected_countries) || length(na.omit(selected_countries)) == 0) {
      return(ppt)
    }
    
    # Filter relevant data
    filtered_data <- gender_workforce %>% 
      filter(country_name %in% selected_countries)
    
    if (nrow(filtered_data) == 0) {
      return(ppt)
    }
    
    # Create ggplot graph
    gender_graph <- ggplot(filtered_data, 
                           aes(x = country_name, y = round(value_percentage, 0), fill = indicator_name)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(
        values = c(
          "as a share of private paid employees" = "#E69F00",  # orange
          "as a share of public paid employees" = "#56B4E9"    # sky blue
        )
      ) +
      labs(
        title = "Female Employment by Sector (Last Year Available)", 
        x = "Country", y = "Employment (%)", fill = "Sector"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Save to PNG
    img_path <- tempfile(fileext = ".png")
    ggsave(img_path, plot = gender_graph, width = 8, height = 6, dpi = 300)
    
    # Add slide with image only
    ppt <- ppt %>%
      add_slide(layout = "Title and Content", master = "Office Theme") %>%
      ph_with(external_img(img_path, height = 5, width = 7), location = ph_location_type(type = "body"))
    
    return(ppt)
  }
  
  #Download CVS
  
  output$dl_gender_workforce_xlsx <- downloadHandler(
    filename = function() paste0("female_employment_by_sector_", Sys.Date(), ".xlsx"),
    content = function(file) {
      # Graph 1: Multi-country (last year per country & sector)
      d1 <- gender_workforce %>%
        dplyr::filter(country_name %in% input$countries_gender) %>%
        dplyr::group_by(country_name, indicator_name) %>%
        dplyr::arrange(year, .by_group = TRUE) %>%
        dplyr::slice_tail(n = 1) %>%
        dplyr::ungroup() %>%
        dplyr::transmute(
          country_name,
          sector = dplyr::recode(
            indicator_name,
            "as a share of private paid employees" = "Private",
            "as a share of public paid employees"  = "Public",
            .default = as.character(indicator_name)
          ),
          year  = as.numeric(year),
          female_share_pct = as.numeric(value_percentage)
        ) %>%
        dplyr::arrange(country_name, sector)
      
      # Graph 2: Single-country over time (both sectors)
      d2 <- gender_workforce %>%
        dplyr::filter(country_name == input$country_gender,
                      indicator_name %in% c("as a share of private paid employees",
                                            "as a share of public paid employees")) %>%
        dplyr::transmute(
          country_name,
          sector = dplyr::recode(
            indicator_name,
            "as a share of private paid employees" = "Private",
            "as a share of public paid employees"  = "Public"
          ),
          year  = as.numeric(year),
          female_share_pct = as.numeric(value_percentage)
        ) %>%
        dplyr::arrange(year, sector)
      
      writexl::write_xlsx(
        x = list(
          "Graph1_MultiCountry_LastYear"  = d1,
          "Graph2_SingleCountry_OverTime" = d2
        ),
        path = file
      )
    }
  )
  
  
  # Female Leadership 
  
  output$barPlotwomen <- renderPlotly({
    if (is.null(input$selected_countries) || length(input$selected_countries) == 0) {
      return(plotly_empty(type = "bar") %>%
               layout(
                 title = "No country selected",
                 annotations = list(
                   text = "Please select at least one country to view the data.",
                   xref = "paper", yref = "paper", showarrow = FALSE,
                   font = list(size = 16), x = 0.5, y = 0.5
                 ),
                 plot_bgcolor = "white", paper_bgcolor = "white"
               ))
    }
    
    filtered_data <- gender_leadership %>%
      dplyr::filter(country_name %in% input$selected_countries) %>%
      # Order groups as requested
      dplyr::mutate(
        indicator_label = factor(
          indicator_label,
          levels = c("Clerks-Private", "Clerks-Public", "Managers-Private", "Managers-Public")
        ),
        # (optional) keep countries in the order the user selected
        country_name = factor(country_name, levels = input$selected_countries)
      )
    
    if (nrow(filtered_data) == 0) {
      return(plotly_empty(type = "bar") %>%
               layout(
                 title = "No data available",
                 annotations = list(
                   text = "No data available for the selected country/countries.",
                   xref = "paper", yref = "paper", showarrow = FALSE,
                   font = list(size = 16), x = 0.5, y = 0.5
                 ),
                 plot_bgcolor = "white", paper_bgcolor = "white"
               ))
    }
    
    # Same hue per occupation; darker tone for Public
    cols <- c(
      "Clerks-Private"   = "#9ECAE1",  # light blue
      "Clerks-Public"    = "#08519C",  # dark blue
      "Managers-Private" = "#FDAE6B",  # light orange
      "Managers-Public"  = "#E6550D"   # dark orange
    )
    
    plot_ly(
      data   = filtered_data,
      x      = ~country_name,
      y      = ~value_percentage,
      color  = ~indicator_label,
      colors = cols,
      type   = "bar",
      barmode = "group",
      text = ~paste0(
        "Country: ", country_name, "<br>",
        "Group–Sector: ", indicator_label, "<br>",
        "Female Share: ", round(value_percentage, 1), "%<br>",
        "Year: ", year
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        title = "Females by Occupational Group and Sector",
        xaxis = list(title = "Country"),
        yaxis = list(title = "Female Share (%)"),
        bargap = 0.2,
        legend = list(traceorder = "normal")  # keep legend in factor order
      )
  })
  
  
  
  output$note_barPlotwomen <- renderText({
    "Note: This indicator represents the share of females in different occupational groups (Managers/Clerks) in both the public and private sectors, highlighting gender representation disparities. The indicator shows the last year available for each selected country/region/income group(s)."
  })
  
  output$downloadGraphsWordfemale <- downloadHandler(
    filename = function() paste0("Females_Occupation_Groups_Analysis_", Sys.Date(), ".docx"),
    content  = function(file) {
      
      req(input$selected_countries, length(input$selected_countries) > 0)
      
      # --- Title (same style as your other report) ---
      first_sel    <- input$selected_countries[1]
      report_title <- paste0("Females by Occupational Group and Sector — ", first_sel)
      
      title_style <- officer::fp_text(color = "#722F37", font.size = 16, bold = TRUE)
      doc <- officer::read_docx() %>%
        officer::body_add_fpar(officer::fpar(officer::ftext(report_title, prop = title_style))) %>%
        officer::body_add_par("", style = "Normal")  # spacer
      
      # --- Section heading style ---
      h2_fmt <- officer::fp_text(font.size = 14, bold = TRUE)
      
      # --- Introduction ---
      doc <- doc %>%
        officer::body_add_fpar(officer::fpar(officer::ftext("Introduction", h2_fmt))) %>%
        officer::body_add_par(
          "This report presents an analysis of female representation in Managers and Clerks across public and private sectors for the selected countries.",
          style = "Normal"
        ) %>%
        officer::body_add_par("", style = "Normal") %>%
        # >>> Your requested subsection <<<
        officer::body_add_fpar(
          officer::fpar(officer::ftext("1.2 Equity in the Public Sector", h2_fmt))
        ) %>%
        officer::body_add_par("", style = "Normal")  # spacer before graphs
      
      # --- Data ---
      filtered_data <- gender_leadership %>%
        dplyr::filter(country_name %in% input$selected_countries) %>%
        dplyr::mutate(
          indicator_label = factor(
            indicator_label,
            levels = c("Clerks-Private", "Clerks-Public", "Managers-Private", "Managers-Public")
          ),
          country_name = factor(country_name, levels = input$selected_countries)
        )
      req(nrow(filtered_data) > 0, cancelOutput = TRUE)
      
      # --- Colors (same hue per occupation; darker for Public) ---
      cols <- c(
        "Clerks-Private"   = "#9ECAE1",  # light blue
        "Clerks-Public"    = "#08519C",  # dark blue
        "Managers-Private" = "#FDAE6B",  # light orange
        "Managers-Public"  = "#E6550D"   # dark orange
      )
      
      # --- Plot (ggplot -> PNG) ---
      p <- ggplot2::ggplot(
        filtered_data,
        ggplot2::aes(x = country_name, y = value_percentage, fill = indicator_label)
      ) +
        ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7) +
        ggplot2::scale_fill_manual(
          values = cols,
          drop   = FALSE,
          name   = "Group–Sector",
          breaks = c("Clerks-Private", "Clerks-Public", "Managers-Private", "Managers-Public"),
          labels = c("Clerks — Private", "Clerks — Public", "Managers — Private", "Managers — Public")
        ) +
        ggplot2::labs(
          title = "Females by Occupational Group and Sector",
          x = "Country", y = "Female Share (%)"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
      
      img_path <- tempfile(fileext = ".png")
      ggplot2::ggsave(img_path, plot = p, width = 8, height = 6, dpi = 300)
      
      # --- Add image + note ---
      doc <- doc %>%
        officer::body_add_img(src = img_path, width = 6.5, height = 4.8) %>%
        officer::body_add_par(
          "Note: Bars are ordered as Clerks–Private, Clerks–Public, Managers–Private, Managers–Public. Blue tones denote Clerks (Public is darker); orange tones denote Managers (Public is darker).",
          style = "Normal"
        )
      
      print(doc, target = file)
    }
  )
  # Color mapping (exactly as requested)
  cols <- c(
    "Clerks-Private"   = "#9ECAE1",  # light blue
    "Clerks-Public"    = "#08519C",  # dark blue
    "Managers-Private" = "#FDAE6B",  # light orange
    "Managers-Public"  = "#E6550D"   # dark orange
  )
  generate_females_occupation_groups_section <- function(doc, selected_countries) {
    # Add Section Title
    doc <- doc %>% body_add_par("Females by Occupational Group and Sector", style = "heading 1")
    
    # Add Introduction
    doc <- doc %>% body_add_par(
      "This section presents an analysis of female representation in different occupational groups across selected countries.", 
      style = "Normal"
    )
    
    # ✅ Ensure at least one country is selected
    if (is.null(selected_countries) || length(selected_countries) == 0) {
      doc <- doc %>% body_add_par("No countries selected for analysis.", style = "Normal")
      return(doc)
    }
    
    # ✅ Extract the first selected country
    first_country <- selected_countries[1]
    if (is.na(first_country)) first_country <- "Unknown Country"
    
    # ✅ Filter data for selected countries
    filtered_data <- gender_leadership %>% filter(country_name %in% selected_countries)
    
    # ✅ Handle empty dataset case
    if (nrow(filtered_data) == 0) {
      doc <- doc %>% body_add_par("No data available for the selected countries.", style = "Normal")
      return(doc)
    }
    
    # ✅ Generate ggplot for Female Occupational Groups
    ggplot_obj <- ggplot(
      filtered_data,
      aes(x = country_name, y = value_percentage, fill = indicator_label)
    ) +
      geom_col(position = position_dodge(width = 0.75), width = 0.7) +
      scale_fill_manual(values = cols, name = "Occupation") +
      labs(
        title = "Females by Occupational Group and Sector",
        x = "Country",
        y = "Female Share (%)"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "top",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()
      )
    
    # ✅ Save the plot as an image
    img_path <- tempfile(fileext = ".png")
    ggsave(img_path, plot = ggplot_obj, width = 8, height = 6)
    
    # ✅ Extract summary statistics
    highest_public_managers <- filtered_data %>%
      filter(indicator_label == "Managers-Public") %>%
      filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>%
      first()
    
    lowest_public_managers <- filtered_data %>%
      filter(indicator_label == "Managers-Public") %>%
      filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>%
      first()
    
    highest_private_managers <- filtered_data %>%
      filter(indicator_label == "Managers-Private") %>%
      filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>%
      first()
    
    lowest_private_managers <- filtered_data %>%
      filter(indicator_label == "Managers-Private") %>%
      filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>%
      first()
    
    highest_public_clerks <- filtered_data %>%
      filter(indicator_label == "Clerks-Public") %>%
      filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>%
      first()
    
    lowest_public_clerks <- filtered_data %>%
      filter(indicator_label == "Clerks-Public") %>%
      filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>%
      first()
    
    highest_private_clerks <- filtered_data %>%
      filter(indicator_label == "Clerks-Private") %>%
      filter(value_percentage == max(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>%
      first()
    
    lowest_private_clerks <- filtered_data %>%
      filter(indicator_label == "Clerks-Private") %>%
      filter(value_percentage == min(value_percentage, na.rm = TRUE)) %>%
      pull(country_name) %>%
      first()
    
    # ✅ Round averages to 1 decimal place
    avg_public_managers <- round(mean(filtered_data$value_percentage[filtered_data$indicator_label == "Managers-Public"], na.rm = TRUE), 1)
    avg_private_managers <- round(mean(filtered_data$value_percentage[filtered_data$indicator_label == "Managers-Private"], na.rm = TRUE), 1)
    avg_public_clerks <- round(mean(filtered_data$value_percentage[filtered_data$indicator_label == "Clerks-Public"], na.rm = TRUE), 1)
    avg_private_clerks <- round(mean(filtered_data$value_percentage[filtered_data$indicator_label == "Clerks-Private"], na.rm = TRUE), 1)
    
    # ✅ Extract employment levels for the first country and comparison
    first_country_public_managers <- filtered_data %>%
      filter(country_name == first_country, indicator_label == "Managers-Public") %>%
      pull(value_percentage) %>%
      first() %>%
      coalesce(0) %>%
      round(1)
    
    first_country_private_managers <- filtered_data %>%
      filter(country_name == first_country, indicator_label == "Managers-Private") %>%
      pull(value_percentage) %>%
      first() %>%
      coalesce(0) %>%
      round(1)
    
    # ✅ Compare first country with others
    comparison_public_managers <- if (first_country_public_managers > avg_public_managers) {
      paste0("This is higher than the average of ", format(avg_public_managers, nsmall = 1), "% across the other selected countries.")
    } else {
      paste0("This is lower than the average of ", format(avg_public_managers, nsmall = 1), "% across the other selected countries.")
    }
    
    comparison_private_managers <- if (first_country_private_managers > avg_private_managers) {
      paste0("This is higher than the average of ", format(avg_private_managers, nsmall = 1), "% across the other selected countries.")
    } else {
      paste0("This is lower than the average of ", format(avg_private_managers, nsmall = 1), "% across the other selected countries.")
    }
    
    interpretation_text <- paste0(
      "This graph compares female representation in different occupational groups across selected countries. ",
      "On average,", avg_public_managers, "% of public sector managers are female, while in the private sector, female managers account for ", avg_private_managers, "%. ",
      "The highest female representation among public sector managers is in ", highest_public_managers, ", whereas the lowest is in  ", lowest_public_managers, ". ",
      "In the private sector, the highest female manager share is in ", highest_private_managers, ", while the lowest is in ", lowest_private_managers, ".\n\n",
      "In ", first_country, ", female managers account for ", first_country_public_managers, "% in the public sector. ", 
      comparison_public_managers, "\n",
      "In the private sector, female managers in ", first_country, " represent ", first_country_private_managers, "%. ",
      comparison_private_managers
    )
    
    # ✅ Add Image and Interpretation to the Document
    doc <- doc %>% 
      body_add_img(src = img_path, width = 6, height = 4) %>% 
      body_add_par("This graph shows the share of females in various occupational groups (Managers/Clerks) in the public and private sectors for the selected countries.", style = "Normal") %>%
      body_add_par(interpretation_text, style = "Normal")
    
    return(doc)
  }
  
  generate_females_occupation_groups_slide <- function(ppt, selected_countries) {
    if (is.null(selected_countries) || length(na.omit(selected_countries)) == 0) {
      return(ppt)
    }
    
    # Filter the data
    filtered_data <- gender_leadership %>% 
      filter(country_name %in% selected_countries)
    
    if (nrow(filtered_data) == 0) {
      return(ppt)  # no data to plot
    }
    
    # Plot
    ggplot_obj <- ggplot(
      filtered_data,
      aes(x = country_name, y = value_percentage, fill = indicator_label)
    ) +
      geom_col(position = position_dodge(width = 0.75), width = 0.7) +
      scale_fill_manual(values = cols, name = "Occupation") +
      labs(
        title = "Females by Occupational Group and Sector",
        x = "Country",
        y = "Female Share (%)"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "top",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()
      )
    
    # Save as image
    img_path <- tempfile(fileext = ".png")
    ggsave(img_path, plot = ggplot_obj, width = 8, height = 6, dpi = 300)
    
    # Add to PowerPoint
    ppt <- ppt %>%
      add_slide(layout = "Title and Content", master = "Office Theme") %>%
      ph_with(external_img(img_path, height = 5, width = 7), location = ph_location_type(type = "body"))
    
    return(ppt)
  }
  
  #Download CVS
  
  output$dl_csv_female_leadership <- downloadHandler(
    filename = function() paste0("female_leadership_occupation_sector_", Sys.Date(), ".csv"),
    content  = function(file) {
      # empty schema to avoid blank file issues
      empty_schema <- tibble::tibble(
        country_name      = character(),
        group_sector      = character(),
        year              = integer(),
        female_share_pct  = numeric()
      )
      
      if (is.null(input$selected_countries) || !length(input$selected_countries)) {
        utils::write.csv(empty_schema, file, row.names = FALSE, na = "")
        return()
      }
      
      d <- gender_leadership %>%
        dplyr::filter(country_name %in% input$selected_countries) %>%
        dplyr::mutate(
          indicator_label = factor(
            indicator_label,
            levels = c("Clerks-Private", "Clerks-Public", "Managers-Private", "Managers-Public")
          ),
          country_name = factor(country_name, levels = input$selected_countries)
        ) %>%
        # Keep the latest year per country & group-sector (matches what users usually expect)
        dplyr::arrange(country_name, indicator_label, dplyr::desc(year)) %>%
        dplyr::group_by(country_name, indicator_label) %>%
        dplyr::slice_head(n = 1) %>%
        dplyr::ungroup() %>%
        dplyr::transmute(
          country_name     = as.character(country_name),
          group_sector     = as.character(indicator_label),
          year             = as.integer(year),
          female_share_pct = as.numeric(value_percentage)
        )
      
      if (nrow(d) == 0) d <- empty_schema
      utils::write.csv(d, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    }
  )
  
  
  #Gender Wage premium in the public sector, by industry 
  output$gender_wage_barplot <- renderPlot({
    
    # Filter data based on user selection
    filtered_data <- gender_wage_premiumpublic %>%
      filter(country_name %in% input$selected_countries, 
             indicator_label %in% c("Public Administration", 
                                    "Education", 
                                    "Health", 
                                    "Other")) 
    
    # Fallback if no data
    if (nrow(filtered_data) == 0) {
      return(
        ggplot() + 
          theme_void() +
          annotate("text", x = 0.5, y = 0.5, label = "No data available for the selected country/countries.",
                   size = 6, color = "grey", hjust = 0.5, vjust = 0.5)
      )
    }
    
    # Create actual ggplot
    ggplot(filtered_data, aes(x = country_name, y = value_percentage, fill = indicator_label)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_viridis_d(name = "Indicator", option = "D") +
      labs(
        title = "Gender Wage Premium in Public Sector by Industry",
        x = "Country", y = "Wage Premium (%)"
      ) +
      theme_minimal() +
      annotate("text", x = Inf, y = min(filtered_data$value_percentage, na.rm = TRUE) - 5, 
               label = "", 
               hjust = 1, size = 4, color = "black", fontface = "italic")
  })
  
  
  output$note_gender_wage_barplot <- renderText({
    "Note: This indicator represents the gender wage premium in the public sector across different industries, comparing wages of female employees to male employees. The indicator shows the last year available for each selected country/region/income group(s)."
  })
  
  
  # Download Handler for Word Document
  
  output$downloadGenderWagePremium <- downloadHandler(
    filename = function() paste0("Gender_Wage_Premium_Report_", Sys.Date(), ".docx"),
    content  = function(file) {
      
      # --- Inputs & title ---
      req(input$selected_countries, length(input$selected_countries) > 0)
      first_sel    <- input$selected_countries[1]
      report_title <- paste0("Gender Wage Premium in Public Sector by Industry — ", first_sel)
      
      # --- Doc + Title (same style) ---
      title_style <- officer::fp_text(color = "#722F37", font.size = 16, bold = TRUE)
      h2_fmt      <- officer::fp_text(font.size = 14, bold = TRUE)
      
      doc <- officer::read_docx() %>%
        officer::body_add_fpar(officer::fpar(officer::ftext(report_title, prop = title_style))) %>%
        officer::body_add_par("", style = "Normal") %>%
        # Sections
        officer::body_add_fpar(officer::fpar(officer::ftext("Introduction", h2_fmt))) %>%
        officer::body_add_par(
          "This report presents an analysis of the gender wage premium in the public sector by industry (Core Public Administration, Education, and Health) across the selected countries.",
          style = "Normal"
        ) %>%
        officer::body_add_par("", style = "Normal") %>%
        officer::body_add_fpar(officer::fpar(officer::ftext("1.2 Equity in the Public Sector", h2_fmt))) %>%
        officer::body_add_par("", style = "Normal")   # spacer before figure
      
      # --- Data ---
      filtered_data <- gender_wage_premiumpublic %>%
        dplyr::filter(
          country_name %in% input$selected_countries,
          indicator_name %in% c(
            "Gender wage premium in the public sector, by industry: Public Administration (compared to male paid employees)",
            "Gender wage premium in the public sector, by industry: Education (compared to male paid employees)",
            "Gender wage premium in the public sector, by industry: Health (compared to male paid employees)",
            ""
          )
        ) %>%
        dplyr::mutate(
          indicator_label = dplyr::case_when(
            indicator_name == "Gender wage premium in the public sector, by industry: Public Administration (compared to male paid employees)" ~ "Core Public Administration",
            indicator_name == "Gender wage premium in the public sector, by industry: Education (compared to male paid employees)"            ~ "Education",
            indicator_name == "Gender wage premium in the public sector, by industry: Health (compared to male paid employees)"               ~ "Health",
            indicator_name == ""                                                                                                         ~ "Other",
            TRUE ~ NA_character_
          ),
          indicator_label = factor(
            indicator_label,
            levels = c("Core Public Administration", "Education", "Health")
          ),
          country_name = factor(country_name, levels = input$selected_countries)
        ) %>%
        tidyr::drop_na(value_percentage, indicator_label)
      
      req(nrow(filtered_data) > 0, cancelOutput = TRUE)
      
      # --- Plot (ggplot -> PNG) ---
      p <- ggplot2::ggplot(
        filtered_data,
        ggplot2::aes(x = country_name, y = value_percentage, fill = indicator_label)
      ) +
        ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7) +
        # keep your palette or swap to a manual one if you prefer
        ggplot2::scale_fill_viridis_d(option = "D", name = "Industry", drop = FALSE) +
        ggplot2::labs(
          title = "Gender Wage Premium in the Public Sector by Industry",
          x = "Country", y = "Wage Premium (%)"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
      
      img_path <- tempfile(fileext = ".png")
      ggplot2::ggsave(img_path, plot = p, width = 8, height = 6, dpi = 300)
      
      # --- Add image + note ---
      doc <- doc %>%
        officer::body_add_img(src = img_path, width = 6.5, height = 4.8) %>%
        officer::body_add_par(
          "Note: Bars show the estimated gender wage premium for women relative to men in the public sector, by industry. Positive values indicate higher wages relative to male paid employees; negative values indicate lower.",
          style = "Normal"
        )
      
      # --- Write file ---
      print(doc, target = file)
    }
  )
  
  generate_gender_wage_premiumbysector_section <- function(doc, selected_countries) {
    # Section Title
    doc <- doc %>% body_add_par("Gender Wage Premium in Public Sector by Industry", style = "heading 1")
    
    # Intro Text
    doc <- doc %>% body_add_par(
      "This section presents an analysis of gender wage premiums in the public sector by industry 
    (Public Administration, Education, and Health) across selected countries.", 
      style = "Normal"
    )
    
    # ✅ Validate selected countries
    if (is.null(selected_countries) || length(na.omit(selected_countries)) == 0) {
      doc <- doc %>% body_add_par("No countries selected for analysis.", style = "Normal")
      return(doc)
    }
    
    first_country <- selected_countries[1]
    if (is.na(first_country)) {
      doc <- doc %>% body_add_par("Invalid country selection.", style = "Normal")
      return(doc)
    }
    
    # ✅ Filter and label the data
    filtered_data <- gender_wage_premiumpublic %>%
      filter(country_name %in% selected_countries,
             indicator_name %in% c(
               "Gender wage premium in the public sector, by industry: Public Administration (compared to male paid employees)", 
               "Gender wage premium in the public sector, by industry: Education (compared to male paid employees)", 
               "Gender wage premium in the public sector, by industry: Health (compared to male paid employees)"
             ))
    
    if (nrow(filtered_data) == 0 || all(is.na(filtered_data$value_percentage))) {
      doc <- doc %>% body_add_par("No data available for the selected countries.", style = "Normal")
      return(doc)
    }
    
    # ✅ Clean labels
    filtered_data$indicator_label <- recode(filtered_data$indicator_name,
                                            "Gender wage premium in the public sector, by industry: Public Administration (compared to male paid employees)" = "Public Administration",
                                            "Gender wage premium in the public sector, by industry: Education (compared to male paid employees)" = "Education",
                                            "Gender wage premium in the public sector, by industry: Health (compared to male paid employees)" = "Health"
    )
    
    # ✅ Plot
    gender_wage_plot <- ggplot(filtered_data, aes(x = country_name, y = value_percentage, fill = indicator_label)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_viridis_d(option = "D") +  # Options: "D", "C", "E", etc.
      labs(
        title = "Gender Wage Premium in Public Sector by Industry",
        x = "Country", y = "Wage Premium (%)", fill = "Industry"
      ) +
      theme_minimal()
    
    # ✅ Save plot
    img_path <- tempfile(fileext = ".png")
    ggsave(img_path, plot = gender_wage_plot, width = 8, height = 6, dpi = 300)
    
    # ✅ Utility functions
    safe_max <- function(x) ifelse(all(is.na(x)), NA, max(x, na.rm = TRUE))
    safe_min <- function(x) ifelse(all(is.na(x)), NA, min(x, na.rm = TRUE))
    safe_mean <- function(x) ifelse(all(is.na(x)), 0, round(mean(x, na.rm = TRUE), 0))
    
    # ✅ Stats by sector
    get_extreme <- function(data, label, func) {
      data %>%
        filter(indicator_label == label, value_percentage == func(value_percentage)) %>%
        pull(country_name) %>%
        first() %>%
        coalesce("N/A")
    }
    
    highest_admin    <- get_extreme(filtered_data, "Public Administration", safe_max)
    lowest_admin     <- get_extreme(filtered_data, "Public Administration", safe_min)
    highest_education <- get_extreme(filtered_data, "Education", safe_max)
    lowest_education  <- get_extreme(filtered_data, "Education", safe_min)
    highest_health    <- get_extreme(filtered_data, "Health", safe_max)
    lowest_health     <- get_extreme(filtered_data, "Health", safe_min)
    
    # ✅ Means
    avg_admin    <- safe_mean(filtered_data$value_percentage[filtered_data$indicator_label == "Public Administration"])
    avg_education <- safe_mean(filtered_data$value_percentage[filtered_data$indicator_label == "Education"])
    avg_health    <- safe_mean(filtered_data$value_percentage[filtered_data$indicator_label == "Health"])
    
    # ✅ First country values
    first_country_admin <- filtered_data %>%
      filter(country_name == first_country, indicator_label == "Public Administration") %>%
      pull(value_percentage) %>%
      first() %>%
      coalesce(0) %>%
      round(0)
    
    first_country_education <- filtered_data %>%
      filter(country_name == first_country, indicator_label == "Education") %>%
      pull(value_percentage) %>%
      first() %>%
      coalesce(0) %>%
      round(0)
    
    comparison_admin <- if (first_country_admin > avg_admin) {
      paste0("This is higher than the average of ", avg_admin, "% across the other selected countries.")
    } else {
      paste0("This is lower than the average of ", avg_admin, "% across the other selected countries.")
    }
    
    comparison_education <- if (first_country_education > avg_education) {
      paste0("This is higher than the average of ", avg_education, "% across the other selected countries.")
    } else {
      paste0("This is lower than the average of ", avg_education, "% across the other selected countries.")
    }
    
    # ✅ Interpretation
    interpretation_text <- paste0(
      "This graph compares the gender wage premium in the public sector across different industries. ",
      "On average, the wage premium in Public Administration is ", avg_admin, "%, in Education it is ", avg_education, "%, ",
      "and in Health it is ", avg_health, "%.\n\n",
      "The highest wage premium in Public Administration is in ", highest_admin, ", while the lowest is in ", lowest_admin, ". ",
      "In Education, the highest wage premium is observed in ", highest_education, ", whereas the lowest is in ", lowest_education, ". ",
      "For Health, the highest gender wage premium is in ", highest_health, ", while the lowest is in ", lowest_health, ".\n\n",
      "In ", first_country, ", the wage premium in Public Administration is ", first_country_admin, "%. ", 
      comparison_admin, "\n",
      "In Education, the wage premium in ", first_country, " is ", first_country_education, "%. ",
      comparison_education
    )
    
    # ✅ Add content to doc
    doc <- doc %>%
      body_add_img(src = img_path, width = 6, height = 4) %>%
      body_add_par("This graph shows the gender wage premium in the public sector across different industries.", style = "Normal") %>%
      body_add_par(interpretation_text, style = "Normal")
    
    return(doc)
  }
  
  
  generate_gender_wage_premiumbysector_slide <- function(ppt, selected_countries) {
    if (is.null(selected_countries) || length(na.omit(selected_countries)) == 0) {
      return(ppt)
    }
    
    # Filter relevant indicators
    filtered_data <- gender_wage_premiumpublic %>%
      filter(
        country_name %in% selected_countries,
        indicator_name %in% c(
          "Gender wage premium in the public sector, by industry: Public Administration (compared to male paid employees)",
          "Gender wage premium in the public sector, by industry: Education (compared to male paid employees)",
          "Gender wage premium in the public sector, by industry: Health (compared to male paid employees)"
        )
      )
    
    if (nrow(filtered_data) == 0 || all(is.na(filtered_data$value_percentage))) {
      return(ppt)  # no data to plot
    }
    
    # Rename for labels
    filtered_data$indicator_label <- recode(
      filtered_data$indicator_name,
      "Gender wage premium in the public sector, by industry: Public Administration (compared to male paid employees)" = "Public Administration",
      "Gender wage premium in the public sector, by industry: Education (compared to male paid employees)" = "Education",
      "Gender wage premium in the public sector, by industry: Health (compared to male paid employees)" = "Health"
    )
    
    # Create ggplot graph
    gender_wage_plot <- ggplot(filtered_data, aes(x = country_name, y = value_percentage, fill = indicator_label)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_viridis_d(option = "D") +  # Discrete viridis scale
      labs(
        title = "Gender Wage Premium in Public Sector by Industry",
        x = "Country", y = "Wage Premium (%)", fill = "Industry"
      ) +
      theme_minimal()
    
    # Save to PNG
    img_path <- tempfile(fileext = ".png")
    ggsave(filename = img_path, plot = gender_wage_plot, width = 8, height = 6, dpi = 300)
    
    # Add slide with image only
    ppt <- ppt %>%
      add_slide(layout = "Title and Content", master = "Office Theme") %>%
      ph_with(external_img(img_path, height = 5, width = 7), location = ph_location_type(type = "body"))
    
    return(ppt)
  }
  
  
  generate_intro_section <- function(doc, selected_countries) {
    # ✅ Use first selected country safely
    first_country <- if (!is.null(selected_countries) && length(selected_countries) > 0 && !is.na(selected_countries[1])) {
      selected_countries[1]
    } else {
      "Unknown Country"
    }
    
    # ✅ Try to detect World Bank region
    first_region <- countrycode(first_country, origin = "country.name", destination = "region")
    if (is.na(first_region)) {
      first_region <- "its respective region"
    }
    
    # ✅ Define styles
    title_style <- fp_text(color = "#722F37", font.size = 20, bold = TRUE)
    subtitle_style <- fp_text(color = "black", font.size = 16, bold = TRUE)
    
    # ✅ Add formatted title
    doc <- doc %>% 
      body_add_fpar(fpar(ftext(first_country, prop = title_style))) %>% 
      body_add_fpar(fpar(ftext("Wage Bill and Public Employment Analysis", prop = subtitle_style)))
    
    # ✅ Construct intro text
    intro_text <- paste0(
      "This note presents evidence on public sector employment and compensation practices in ", first_country,
      " using the Worldwide Bureaucracy Indicators (WWBI). The primary data source is the Labor Force Survey, conducted by the National Statistics Office. ",
      "For international comparisons, peer countries from ", first_region, " are included.",
      "\n\n",
      "The public sector is typically a major source of employment in most countries. ",
      "The provision of basic services such as education, health, citizen security, and justice, among others, ",
      "makes it a central actor in labor markets, with significant impacts on the aggregate results of employment, ",
      "wages, informality, and other economic variables. Moreover, public employment is an indicator of the state’s ",
      "participation in the entire economy, which has implications for macroeconomic balances, allocation efficiency, ",
      "and income distribution. Thus, this analysis comprehensively documents the size of public employment, ",
      "its changes over time, and the characteristics of its workforce.",
      "\n\n",
      "This work documents and analyzes the size, composition, and changes in the levels of employment and wages of ", 
      first_country, "’s public employees compared to the private sector and how these metrics compare to regional peers."
    )
    
    # ✅ Add intro text to the document
    doc <- doc %>% body_add_par(intro_text, style = "Normal")
    
    return(doc)
  }
  
  #Download cvs
  
  output$dl_csv_gender_wage_industry <- downloadHandler(
    filename = function() paste0("gender_wage_premium_public_by_industry_", Sys.Date(), ".csv"),
    content  = function(file) {
      # empty schema so the file isn't structurally blank if no data
      empty_schema <- tibble::tibble(
        country_name        = character(),
        industry            = character(),
        year                = integer(),
        wage_premium_pct    = numeric()
      )
      
      # guard: need selections
      if (is.null(input$selected_countries) || !length(input$selected_countries)) {
        utils::write.csv(empty_schema, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
        return()
      }
      
      # industries shown in the chart
      keep_inds <- c("Public Administration", "Education", "Health", "Other")
      
      d <- gender_wage_premiumpublic %>%
        dplyr::filter(
          country_name %in% input$selected_countries,
          indicator_label %in% keep_inds
        )
      
      if (nrow(d) == 0) {
        utils::write.csv(empty_schema, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
        return()
      }
      
      # match the chart’s “one bar per country & industry” by taking the latest year
      d_out <- d %>%
        dplyr::group_by(country_name, indicator_label) %>%
        dplyr::slice_max(order_by = year, n = 1, with_ties = FALSE) %>%
        dplyr::ungroup() %>%
        dplyr::transmute(
          country_name,
          industry         = indicator_label,
          year             = as.integer(year),
          wage_premium_pct = as.numeric(value_percentage)
        ) %>%
        dplyr::arrange(country_name, industry)
      
      if (nrow(d_out) == 0) d_out <- empty_schema
      utils::write.csv(d_out, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    }
  )
  
  
  
  #Pay compression 
  
  output$paycompression_plot <- renderPlotly({
    req(input$countries_first)  # Ensure at least one country is selected
    
    filtered_data_df <- pay_compression_wide %>%
      filter(country_name %in% input$countries_first)
    
    # Fallback if no data
    if (nrow(filtered_data_df) == 0) {
      return(plotly_empty(type = "scatter") %>%
               layout(
                 title = "No data available",
                 annotations = list(
                   text = "No data available for the selected country/countries.",
                   xref = "paper", yref = "paper",
                   showarrow = FALSE,
                   font = list(size = 16),
                   x = 0.5, y = 0.5
                 ),
                 plot_bgcolor = "white",
                 paper_bgcolor = "white"
               ))
    }
    
    # Fallback if required columns are missing
    if (!all(c("Public_Sector", "Private_Sector") %in% colnames(filtered_data_df))) {
      return(plotly_empty(type = "scatter") %>%
               layout(
                 title = "Data error",
                 annotations = list(
                   text = "Required columns (Public_Sector, Private_Sector) are missing.",
                   xref = "paper", yref = "paper",
                   showarrow = FALSE,
                   font = list(size = 16, color = "red"),
                   x = 0.5, y = 0.5
                 ),
                 plot_bgcolor = "white",
                 paper_bgcolor = "white"
               ))
    }
    
    # Prepare data and trendline
    filtered_data_df <- filtered_data_df %>%
      mutate(color = ifelse(country_name == input$countries_first[1], "#B3242B", "#003366"))
    
    trendline_model <- lm(Public_Sector ~ Private_Sector, data = filtered_data_df)
    trendline_values <- predict(trendline_model, newdata = filtered_data_df)
    
    # Build the plot
    plot_ly() %>%
      add_trace(
        data = filtered_data_df,
        x = ~Private_Sector,
        y = ~Public_Sector,
        type = "scatter",
        mode = "markers+text",
        text = ~country_name,
        textposition = "top center",
        marker = list(size = 10, color = ~color, opacity = 0.7),
        name = "Country"              # <-- this changes "trace 0"
      ) %>%
      add_trace(
        x = filtered_data_df$Private_Sector,
        y = trendline_values,
        type = "scatter",
        mode = "lines",
        line = list(color = "gray", dash = "dash"),
        name = "Trendline"
      ) %>%
      layout(
        title = "Pay Compression: Public vs. Private Sector (Latest Year)",
        xaxis = list(title = "Private Sector Pay Compression"),
        yaxis = list(title = "Public Sector Pay Compression"),
        showlegend = TRUE,
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      )
  })
  
  
  output$note_dotplot_pay <- renderText({
    "Note: This visualization explores pay compression in the public and private sectors across selected countries. Compression ratios are calculated based on the ratio of incomes at the 90th to the 10th percentile. The indicator shows the last year available for each selected country/region/income group(s)."
  })
  
  output$downloadPayCompressionDoc <- downloadHandler(
    filename = function() paste0("Pay_Compression_Ratios_Report_", Sys.Date(), ".docx"),
    content  = function(file) {
      
      req(input$countries_first, length(input$countries_first) > 0)
      
      # ---- Data ----
      d <- pay_compression_wide %>%
        dplyr::filter(country_name %in% input$countries_first)
      
      req(nrow(d) > 0, cancelOutput = TRUE)
      req(all(c("Public_Sector","Private_Sector") %in% names(d)), cancelOutput = TRUE)
      
      # highlight first selection
      first_sel <- input$countries_first[1]
      d <- d %>%
        dplyr::mutate(
          highlight = ifelse(country_name == first_sel, "Selected country", "Other countries"),
          country_name = factor(country_name, levels = input$countries_first)
        )
      
      # ---- Doc start (match style of your other report) ----
      doc <- officer::read_docx()
      
      # H1 with rule underneath
      # ---- Title (styled with fp_text) ----
      first_sel <- input$countries_first[1]
      h1_txt <- paste0("Pay Compression Ratios — ", first_sel)
      
      title_style <- officer::fp_text(color = "#722F37", font.size = 20, bold = TRUE)
      
      doc <- officer::read_docx() %>%
        officer::body_add_fpar(
          officer::fpar(officer::ftext(h1_txt, prop = title_style))
        )
      
      # (optional) thin rule under the title
      rule   <- officer::fp_border(color = "#000000", width = 1)
      p_rule <- officer::fp_par(border.bottom = rule, padding.bottom = 4)
      doc <- doc %>% officer::body_add_fpar(officer::fpar(officer::ftext(""), fp_p = p_rule))
      # 1.1 Introduction
      h2_fmt <- officer::fp_text(font.size = 14, bold = TRUE)
      doc <- doc |>
        officer::body_add_fpar(officer::fpar(officer::ftext("1.1 Introduction", h2_fmt))) |>
        officer::body_add_par(
          "This report presents pay compression ratios in the public and private sectors for the selected countries. Pay compression is the ratio of wages at P90 to P10; higher values indicate a wider dispersion.",
          style = "Normal"
        )
      
      # 1.2 Size and Characteristics of the Public Sector
      doc <- doc |>
        officer::body_add_fpar(
          officer::fpar(officer::ftext(" 1.2 Competitiveness of Public Sector", h2_fmt))
        )
      
      # ---- Graph: Public vs Private + trendline ----
      p <- ggplot2::ggplot(
        d, ggplot2::aes(x = Private_Sector, y = Public_Sector, color = highlight, label = country_name)
      ) +
        ggplot2::geom_point(size = 3.8, alpha = 0.95, show.legend = FALSE) +
        ggrepel::geom_text_repel(size = 3, max.overlaps = 50, color = "black") +
        ggplot2::geom_smooth(method = "lm", se = FALSE, color = "gray50", linetype = "dashed") +
        ggplot2::scale_color_manual(values = c(
          "Selected country" = "#B3242B",  # red
          "Other countries"  = "#003366"   # dark blue
        )) +
        ggplot2::labs(
          title = "Pay Compression: Public vs. Private Sector (Latest Year)",
          x = "Private Sector Pay Compression (P90/P10)",
          y = "Public Sector Pay Compression (P90/P10)"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.margin = ggplot2::margin(10, 40, 10, 10))
      
      img <- tempfile(fileext = ".png")
      ggplot2::ggsave(img, plot = p, width = 8, height = 6, dpi = 300)
      
      doc <- doc |>
        officer::body_add_img(src = img, width = 6.5, height = 4.8) |>
        officer::body_add_par(
          "Note:  This visualization explores pay compression in the public and private sectors across selected countries. Compression ratios are calculated based on the ratio of incomes at the 90th to the 10th percentile.",
          style = "Normal"
        )
      
      # ---- Write file ----
      print(doc, target = file)
    }
  )
  
  
  #Pay compression section  
  
  generate_pay_compression_section <- function(doc, selected_countries) {
    doc <- doc %>% body_add_par("Pay Compression in the Private and Public Sector", style = "heading 1")
    
    doc <- doc %>% body_add_par(
      "This section presents an analysis of the pay compression for the private and public sector 
     across selected countries.", 
      style = "Normal"
    )
    
    if (is.null(selected_countries) || length(na.omit(selected_countries)) == 0) {
      doc <- doc %>% body_add_par("No countries selected for analysis.", style = "Normal")
      return(doc)
    }
    
    first_country <- selected_countries[1]
    
    filtered_data_df <- pay_compression_wide %>%
      filter(country_name %in% selected_countries)
    
    req(nrow(filtered_data_df) > 0)
    
    country_summary <- filtered_data_df %>%
      group_by(country_name) %>%
      summarise(
        public_compression = round(mean(Public_Sector, na.rm = TRUE), 1),  
        private_compression = round(mean(Private_Sector, na.rm = TRUE), 1) 
      )
    
    highest_public <- country_summary %>%
      filter(public_compression == max(public_compression, na.rm = TRUE)) %>% pull(country_name)
    lowest_public <- country_summary %>%
      filter(public_compression == min(public_compression, na.rm = TRUE)) %>% pull(country_name)
    
    highest_private <- country_summary %>%
      filter(private_compression == max(private_compression, na.rm = TRUE)) %>% pull(country_name)
    lowest_private <- country_summary %>%
      filter(private_compression == min(private_compression, na.rm = TRUE)) %>% pull(country_name)
    
    first_country_values <- country_summary %>% filter(country_name == first_country)
    first_public_compression <- first_country_values %>% pull(public_compression) %>% coalesce(NA)
    first_private_compression <- first_country_values %>% pull(private_compression) %>% coalesce(NA)
    
    other_countries_avg <- country_summary %>%
      filter(country_name != first_country) %>%
      summarise(
        avg_public_compression = round(mean(public_compression, na.rm = TRUE), 1),
        avg_private_compression = round(mean(private_compression, na.rm = TRUE), 1)
      )
    
    if (first_country %in% country_summary$country_name) {
      rank_public <- rank(-country_summary$public_compression, ties.method = "min")[country_summary$country_name == first_country]
      rank_private <- rank(-country_summary$private_compression, ties.method = "min")[country_summary$country_name == first_country]
      
      public_position <- dplyr::case_when(
        rank_public == 1 ~ "the highest",
        rank_public == nrow(country_summary) ~ "the lowest",
        TRUE ~ "in the middle range"
      )
      
      private_position <- dplyr::case_when(
        rank_private == 1 ~ "the highest",
        rank_private == nrow(country_summary) ~ "the lowest",
        TRUE ~ "in the middle range"
      )
    } else {
      public_position <- "unranked"
      private_position <- "unranked"
    }
    
    interpretation_text <- paste0(
      "This figure compares pay compression ratios (90th/10th percentile) in the public and private sectors.\n\n",
      "For ", first_country, ", the pay compression ratio is ", first_public_compression, 
      " in the public sector and ", first_private_compression, " in the private sector.\n\n",
      "Among the selected countries, ", highest_public, " has the highest public sector pay compression, while ",
      lowest_public, " has the lowest public sector pay compression.\n\n",
      "In the private sector, ", highest_private, " has the highest pay compression, whereas ",
      lowest_private, " has the lowest.\n\n",
      first_country, " is ranked ", public_position, " in public sector compression and ",
      private_position, " in private sector compression compared to other selected countries.\n\n",
      "A higher compression ratio indicates greater income disparity within the sector. The trendline provides an overall pattern, and the 45-degree reference line represents equality between public and private sector compression."
    )
    
    plot <- ggplot(filtered_data_df, aes(x = Private_Sector, y = Public_Sector, label = country_name)) +  # or group variable
      geom_point(size = 3) +
      geom_text(vjust = -0.5, size = 3) +
      geom_smooth(method = "lm", color = "gray", linetype = "dashed") +
      labs(
        title = "Pay Compression: Public vs. Private Sector",
        x = "Private Sector Pay Compression",
        y = "Public Sector Pay Compression"
      ) +
      scale_color_viridis(discrete = TRUE, option = "D") +
      theme_minimal()
    
    img_path <- tempfile(fileext = ".png")
    ggsave(filename = img_path, plot = plot, width = 8, height = 6, dpi = 300)
    
    doc <- doc %>% 
      body_add_img(src = img_path, width = 6, height = 4) %>%
      body_add_par("Note: The trendline provides a visual reference for overall patterns across countries.", style = "Normal") %>%
      body_add_par(paste(interpretation_text, collapse = ""), style = "Normal")
    
    
    return(doc)
  }
  
  generate_pay_compression_slide <- function(ppt, selected_countries) {
    if (is.null(selected_countries) || length(na.omit(selected_countries)) == 0) {
      return(ppt)
    }
    
    filtered_data_df <- pay_compression_wide %>%
      filter(country_name %in% selected_countries)
    
    req(nrow(filtered_data_df) > 0)
    
    # Create summary for plot
    plot <- ggplot(filtered_data_df, aes(x = Private_Sector, y = Public_Sector, label = country_name)) +  # or group variable
      geom_point(size = 3) +
      geom_text(vjust = -0.5, size = 3) +
      geom_smooth(method = "lm", color = "gray", linetype = "dashed") +
      labs(
        title = "Pay Compression: Public vs. Private Sector",
        x = "Private Sector Pay Compression",
        y = "Public Sector Pay Compression"
      ) +
      scale_color_viridis(discrete = TRUE, option = "D") +
      theme_minimal()
    
    # Save plot as image
    img_path <- tempfile(fileext = ".png")
    ggsave(filename = img_path, plot = plot, width = 6, height = 6, dpi = 300)
    
    # Add slide with only the image
    ppt <- ppt %>%
      add_slide(layout = "Title and Content", master = "Office Theme") %>%
      ph_with(external_img(img_path, height = 4, width = 4),
              location = ph_location_type(type = "body"))
    
    return(ppt)
  }
  
  generate_conclusion_section <- function(doc) {
    # Add Section Title
    doc <- doc %>% body_add_par("Conclusion", style = "heading 1")
    
    # Add Conclusion Text
    doc <- doc %>% body_add_par(
      "This report provides a comprehensive analysis of wage bill trends, gender employment representation, and workforce participation in the public sector. 
      The findings highlight key trends and disparities across different sectors and countries.",
      style = "Normal"
    )
    
    return(doc)
  }
  
  #Download CVS
  output$dl_csv_pay_compression <- downloadHandler(
    filename = function() paste0("pay_compression_public_vs_private_", Sys.Date(), ".csv"),
    content  = function(file) {
      # empty schema so you never get a structurally blank file
      empty_schema <- tibble::tibble(
        country_name    = character(),
        year            = integer(),
        private_sector  = numeric(),
        public_sector   = numeric()
      )
      
      # need selected countries
      if (is.null(input$countries_first) || !length(input$countries_first)) {
        utils::write.csv(empty_schema, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
        return()
      }
      
      # filter source used by the plot
      d <- pay_compression_wide %>%
        dplyr::filter(country_name %in% input$countries_first)
      
      # ensure required columns exist
      if (!all(c("Public_Sector", "Private_Sector") %in% names(d))) {
        utils::write.csv(empty_schema, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
        return()
      }
      
      # include year if present; otherwise fill NA
      out <- d %>%
        dplyr::transmute(
          country_name,
          year           = if ("year" %in% names(d)) as.integer(.data[["year"]]) else NA_integer_,
          private_sector = as.numeric(Private_Sector),
          public_sector  = as.numeric(Public_Sector)
        ) %>%
        dplyr::arrange(country_name)
      
      if (nrow(out) == 0) out <- empty_schema
      utils::write.csv(out, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    }
  )
  
  
  
  add_section_slide <- function(ppt, title) {
    ppt %>%
      add_slide(layout = "Title Slide", master = "Office Theme") %>%
      ph_with(
        value = fpar(
          ftext(title, prop = fp_text(color = "#003366", font.size = 40, bold = TRUE))
        ),
        location = ph_location_type(type = "ctrTitle")
      )
  }
  
  
  #Download selected graphs 
  
  output$downloadSelectedGraphsDoc <- downloadHandler(
    filename = function() { 
      paste0("Wage_bill_and_public_employment_analysis_Selected_Report_", Sys.Date(), ".docx") 
    },
    content = function(file) {
      # Get the selected countries dynamically
      selected_countries <- input$download_report_countries
      
      # Initialize Word document
      doc <- read_docx() 
      
      # Add Report Title
      title_style <- fp_text(color = "#722F37", font.size = 20, bold = TRUE)
      doc <- doc %>% body_add_fpar(fpar(ftext("Wage bill and public employment analysis", prop = title_style)))
      doc <- generate_intro_section(doc, selected_countries)  # Add the Intro First
      
      # Define Section Style 
      section_style <- fp_text(color = "#003366", font.size = 14, bold = TRUE)
      
      # ✅ Dynamically include only selected sections
      selected_sections <- input$selected_graphs
      
      # ✅ Ensure selected_sections is not NULL before checking length
      if (is.null(selected_sections) || length(selected_sections) == 0) {
        doc <- doc %>% body_add_par("No sections selected for download.", style = "Normal")
      } else {
        if ("wagebill" %in% selected_sections) {
          doc <- generate_wage_bill_analysis_section(doc, selected_countries)
        }
        if ("wagebill_gdp" %in% selected_sections) {
          doc <- generate_gdp_analysis_section(doc, selected_countries)
        }
        if ("tertiaryeducation" %in% selected_sections) {
          doc <- generate_tertiary_education_section(doc, selected_countries)
        }
        if ("genderwagepremium" %in% selected_sections) {
          doc <- generate_wage_premium_gender_section(doc, selected_countries)
        }
        if ("wagepremiumeducation" %in% selected_sections) {
          doc <- generate_wage_premium_education_section(doc, selected_countries)
        }
        if ("public_employment" %in% selected_sections) {
          doc <- generate_public_sector_employment_section(doc, selected_countries) 
        }
        if ("wagepremiumgender" %in% selected_sections) {
          doc <- generate_wage_premium_gender_report_section(doc, selected_countries)
        }
        if ("public_workforce" %in% selected_sections) {
          doc <- generate_public_sector_workforce_section(doc, selected_countries)
        }
        if ("gender_workforce" %in% selected_sections) {
          doc <- generate_gender_workforce_section(doc, selected_countries)
        }
        if ("female_leadership" %in% selected_sections) {  # Fixed typo from "femaleocuupation"
          doc <- generate_females_occupation_groups_section(doc, selected_countries)
        }
        if ("wagepremium" %in% selected_sections) {
          doc <- generate_wage_premium_report_section(doc, selected_countries)
        }
        if ("gender_wage_premium" %in% selected_sections) {
          doc <- generate_gender_wage_premiumbysector_section(doc, selected_countries)
        }
        if ("pay_compression" %in% selected_sections) {
          doc <- generate_pay_compression_section(doc, selected_countries = selected_countries)
          
        }
      }
      
      # ✅ Save the customized report
      print(doc, target = file)
    }
  )
  
  
  #Download one single report
  
  output$downloadAllGraphsDoc <- downloadHandler(
    filename = function() { 
      paste0("Wage_bill_and_public_employment_analysis_", Sys.Date(), ".docx") 
    },
    content = function(file) {
      selected_countries <- input$download_report_countries # ✅ Use country selector from the download tab
      
      # Initialize Word document
      doc <- read_docx() 
      
      # Title
      
      title_style <- fp_text(color = "#722F37", font.size = 20, bold = TRUE)
      doc <- doc %>%
        body_add_fpar(fpar(ftext("", prop = title_style))) %>%
        generate_intro_section(selected_countries)
      
      # Section Header Style
      
      section_style <- fp_text(color = "#003366", font.size = 14, bold = TRUE)
      
      # ──────────────────────────────────────
      # 📘 Section 1: Macro-Fundamentals
      # ──────────────────────────────────────
      doc <- doc %>%
        body_add_fpar(fpar(ftext("Macro-Fundamentals of the Public Sector", prop = section_style))) %>%
        generate_wage_bill_analysis_section(selected_countries) %>%
        generate_gdp_analysis_section(selected_countries)
      
      # ──────────────────────────────────────
      # 📘 Section 2: Size and Characteristics
      # ──────────────────────────────────────
      doc <- doc %>%
        body_add_fpar(fpar(ftext("Size and Characteristics of the Public Sector", prop = section_style))) %>%
        generate_public_sector_employment_section(selected_countries) %>%
        generate_tertiary_education_section(selected_countries)
      
      # ──────────────────────────────────────
      # 📘 Section 3: Competitiveness of Public Sector Wages
      # ──────────────────────────────────────
      doc <- doc %>%
        body_add_fpar(fpar(ftext("Competitiveness of Public Sector Wages", prop = section_style))) %>%
        body_add_par(
          "Public sector compensation should theoretically be designed with an awareness of its influence on the broader labor market. According to the theory of “compensating wage differentials,” a job should pay more (or less) depending on its non-wage characteristics that are undesirable (or desirable)...",
          style = "Normal"
        ) %>%
        generate_wage_premium_report_section(selected_countries) %>%
        generate_wage_premium_education_section(selected_countries) %>%
        generate_pay_compression_section(selected_countries = selected_countries)
      
      # ──────────────────────────────────────
      # 📘 Section 4: Equity in the Public Sector
      # ──────────────────────────────────────
      doc <- doc %>%
        body_add_fpar(fpar(ftext("Equity in the Public Sector", prop = section_style))) %>%
        generate_gender_workforce_section(selected_countries) %>%
        generate_females_occupation_groups_section(selected_countries) %>%
        generate_gender_wage_premiumbysector_section(selected_countries) %>%
        generate_wage_premium_gender_report_section(selected_countries)
      
      # ──────────────────────────────────────
      # 📘 Conclusion
      # ──────────────────────────────────────
      doc <- generate_conclusion_section(doc)
      
      # Save final report
      print(doc, target = file)
    }
  )
  
  
  #Power Point Slides 
  
  output$downloadSelectedGraphsPPT <- downloadHandler(
    filename = function() { 
      paste0("Wage_bill_and_public_employment_analysis_Selected_Presentation_", Sys.Date(), ".pptx")
    },
    content = function(file) {
      selected_countries <- input$download_report_countries
      selected_sections <- input$selected_graphs
      
      # Initialize PowerPoint
      ppt <- read_pptx()
      
      # Add a title slide
      ppt <- ppt %>%
        add_slide(layout = "Title Slide", master = "Office Theme") %>%
        ph_with("Wage bill and public employment analysis", location = ph_location_type(type = "ctrTitle")) %>%
        ph_with(paste("Generated on", Sys.Date()), location = ph_location_type(type = "subTitle"))
      
      # Only include selected graphs
      if (!is.null(selected_sections) && length(selected_sections) > 0) {
        
        if ("wagebill" %in% selected_sections || "wagebill_gdp" %in% selected_sections) {
          ppt <- add_section_slide(ppt, "Macro-Fundamentals of the Public Sector")
        }
        if ("wagebill" %in% selected_sections) {
          ppt <- generate_wage_bill_analysis_slide(ppt, selected_countries)
        }
        if ("wagebill_gdp" %in% selected_sections) {
          ppt <- generate_gdp_analysis_slide(ppt, selected_countries)
        }
        if ("public_employment" %in% selected_sections || "public_workforce" %in% selected_sections || "tertiaryeducation" %in% selected_sections) {
          ppt <- add_section_slide(ppt, "Size and Characteristics of the Public Sector")
        }
        if ("public_employment" %in% selected_sections) {
          ppt <- generate_public_sector_employment_slide(ppt, selected_countries)
        }
        if ("public_workforce" %in% selected_sections) {
          ppt <- generate_public_sector_workforce_slide(ppt, selected_countries)
        }
        if ("tertiaryeducation" %in% selected_sections) {
          ppt <- generate_tertiary_education_slide(ppt, selected_countries)
        }
        if ("genderwagepremium" %in% selected_sections || "wagepremiumeducation" %in% selected_sections || "pay_compression" %in% selected_sections) {
          ppt <- add_section_slide(ppt, "Competiiveness of public sector wages")
        }
        
        if ("genderwagepremium" %in% selected_sections) {
          ppt <- generate_wage_premium_gender_slide(ppt, selected_countries)
        }
        if ("wagepremiumeducation" %in% selected_sections) {
          ppt <-generate_wage_premium_education_slide(ppt, selected_countries)
        }
        
        if ("pay_compression" %in% selected_sections) {
          ppt <- generate_pay_compression_slide(ppt, selected_countries)
        }
        
        if ("wagepremiumgender" %in% selected_sections || "gender_workforce" %in% selected_sections || "gender_wage_premium" %in% selected_sections
            || "female_leadership" %in% selected_sections) {
          ppt <- add_section_slide(ppt, "Equity in public sector")
        }
        
        if ("wagepremiumgender" %in% selected_sections) {
          ppt <- generate_wage_premium_gender_report_slide(ppt, selected_countries)
        }
        
        
        if ("gender_workforce" %in% selected_sections) {
          ppt <- generate_gender_workforce_slide(ppt, selected_countries)
        }
        if ("gender_wage_premium" %in% selected_sections) {
          ppt <- generate_gender_wage_premiumbysector_slide(ppt, selected_countries)
        }
        
        if ("female_leadership" %in% selected_sections) {
          ppt <- generate_females_occupation_groups_slide(ppt, selected_countries)
        }
        
        # add more slide generators here as needed
      } else {
        # If no selection: optionally include a placeholder slide
        ppt <- ppt %>%
          add_slide(layout = "Title and Content", master = "Office Theme") %>%
          ph_with_text(type = "title", str = "No graphs selected") %>%
          ph_with_text(type = "body", str = "Please select at least one graph to download.")
      }
      
      # Save the PowerPoint
      print(ppt, target = file)
    }
  )
  
  
  # ---------------------
  # Clean base map with legend
  output$worldMap <- renderLeaflet({
    leaflet(world_spdf) %>%
      addTiles() %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      addLegend(
        position = "bottomright", 
        colors = c("gray", "#6DA96F"),
        labels = c("No Data", "Reported"),
        title = "Indicator Availability",
        opacity = 1
      )
  })
  
  # Create reactive that flags countries that have ANY data for the selected indicator
  filtered_data_for_map <- reactive({
    req(input$indicatorSelect)
    
    data_wwbi %>%
      filter(indicator_name == input$indicatorSelect) %>%
      mutate(
        any_data = apply(select(., starts_with("year_")), 1, function(x) any(!is.na(x)))
      ) %>%
      filter(any_data) %>%
      transmute(country_name, indicator_name, has_data = 1)  # use 'has_data' instead of value_percentage
  })
  
  # Update the map
  observe({
    req(input$indicatorSelect)
    
    reported_countries <- filtered_data_for_map()
    
    if (nrow(reported_countries) == 0) return()
    
    # Match the country names to the shapefile column
    world_data_merged <- world_spdf %>%
      left_join(reported_countries, by = c("name_long" = "country_name"))
    
    # Use a factor color palette based on 0 (missing) and 1 (has data)
    color_pal <- colorFactor(palette = c("gray", "#6DA96F"), domain = c(0, 1))
    
    leafletProxy("worldMap") %>% clearShapes() %>%
      addPolygons(
        data = world_data_merged,
        fillColor = ~color_pal(ifelse(is.na(has_data), 0, has_data)),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        highlightOptions = highlightOptions(color = "#FFD700", weight = 2, fillOpacity = 0.9),
        label = ~paste0("Country: ", name_long, "-", 
                        ifelse(!is.na(has_data), "Reported", "No Data")),
        popup = ~paste(
          "Country:", name_long, "-",
          "Indicator:", ifelse(!is.na(indicator_name), indicator_name, "None"), "-",
          ifelse(!is.na(has_data), "Reported", "No Data Available")
        )
      )
    
    output$countryCount <- renderText({
      paste("Total Countries with Data:", nrow(reported_countries))
    })
  })
  
  
  # Change info Box colors to "purple" to match the quartz theme
  
  output$numberIndicatorsBox <- renderInfoBox({
    infoBox("Indicators", 302, icon = icon("list"), color = "purple")
  })
  
  output$numberCountriesBox <- renderInfoBox({
    infoBox("Economies", length(unique(data_wwbi$country_name)), icon = icon("globe"), color = "purple")
  })
  
  output$temporalCoverageAnnualBox <- renderInfoBox({
    infoBox("Temporal Coverage (Annual)", "2000-2022", icon = icon("calendar"), color = "purple")
  })
  
  output$temporalCoverageYearsBox <- renderInfoBox({
    infoBox("Temporal Coverage (Years)", "22", icon = icon("calendar"), color = "purple")
  })
  
  output$lastUpdatedBox <- renderInfoBox({
    infoBox("Last Updated", "2025", icon = icon("clock"), color = "purple")
  })
  
}

shinyApp(ui = ui, server = server)
