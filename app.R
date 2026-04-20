# Shiny Dashboard for Worldwide Bureaucracy Indicators

library(haven); library(dplyr); library(tidyr); library(stringr)
library(labelled); library(data.table); library(ggplot2); library(shiny)
library(shinythemes); library(DT); library(maps); library(mapdata)
library(leaflet); library(rnaturalearth); library(sf); library(plotly)
library(officer); library(viridis); library(here); library(glue)
library(colourpicker); library(htmlwidgets); library(bs4Dash)
library(countrycode); library(bslib); library(ggthemes); library(shinyBS)
library(tibble); library(ggrepel); library(writexl); library(scales)

### Load data sets ----
data_path <- if (basename(getwd())=="Code") dirname(getwd()) else getwd()
print(paste("Using data path:", data_path))

data_wwbi         <- read_dta(file.path(data_path,"Data/data_wwbi.dta"))
data_gdp          <- read_dta(file.path(data_path,"Data/data_gdp.dta"))
gdp_2015          <- read_dta(file.path(data_path,"Data/gdp_2015.dta"))
world_spdf        <- st_read(file.path(data_path,"Data/world_spatial.gpkg"))
selected_data_long <- read_dta(file.path(data_path,"Data/selected_data_long.dta"))
data_wwbi_long    <- read_dta(file.path(data_path,"Data/data_wwbi_long.dta"))

data_wwbi_long_ts     <- data_wwbi_long %>% filter(is_latest==FALSE | is.na(is_latest))
data_wwbi_long_latest <- data_wwbi_long %>% filter(is_latest==TRUE)

wage_bill_publicexp   <- read_dta(file.path(data_path,"Data/wage_bill_publicexp.dta")) %>%
  filter(is_latest==FALSE | is.na(is_latest))
wage_bill_gdp         <- read_dta(file.path(data_path,"Data/wage_bill_gdp.dta")) %>%
  filter(is_latest==FALSE | is.na(is_latest))

public_sector_emp_temp <- readRDS(file.path(data_path,"Data","public_sector_emp_temp.rds")) %>%
  mutate(across(where(~inherits(.x,"haven_labelled")), as_factor))
public_sector_emp <- readRDS(file.path(data_path,"Data","public_sector_emp.rds"))
public_sector_emp_temp_last <- readRDS(file.path(data_path,"Data","public_sector_emp_temp_last.rds")) %>%
  mutate(across(where(~inherits(.x,"haven_labelled")), as_factor))

public_sector_workforce_clean <- read_dta(file.path(data_path,"Data/public_sector_workforce_clean.dta"))
public_sector_workforce       <- read_dta(file.path(data_path,"Data/public_sector_workforce.dta"))
public_sector_workforce_first_last <- read_dta(file.path(data_path,"Data/public_sector_workforce_first_last.dta"))
gender_workforce      <- read_dta(file.path(data_path,"Data/gender_workforce.dta"))
data_indicator_wb     <- read_dta(file.path(data_path,"Data/data_indicator_wb.dta"))
merged_data           <- read_dta(file.path(data_path,"Data/merged_data.dta"))
tertiary_education    <- read_dta(file.path(data_path,"Data/tertiary_education.dta"))
public_wage_premium   <- read_dta(file.path(data_path,"Data/public_wage_premium.dta"))
public_wage_premium_educ <- read_dta(file.path(data_path,"Data/public_wage_premium_educ.dta"))
gender_wage_premium   <- readRDS(file.path(data_path,"Data","gender_wage_premium.rds"))
gender_wage_premium_last <- readRDS(file.path(data_path,"Data","gender_wage_premium_last.rds"))
gender_leadership     <- readRDS(file.path(data_path,"Data","gender_leadership.rds"))
gender_wage_premiumpublic <- readRDS(file.path(data_path,"Data","gender_wage_premiumpublic.rds"))
pay_compression       <- readRDS(file.path(data_path,"Data","pay_compression.rds"))
pay_compression_wide  <- readRDS(file.path(data_path,"Data","pay_compression_wide.rds"))

# ---------------------------
# HELPERS
# ---------------------------
`%||%` <- function(x, y) if (is.null(x)) y else x
sanitize_vec <- function(x) {
  x <- x %||% character(0); x <- as.character(x)
  unique(x[!is.na(x) & nzchar(x)])
}

# ─── Note style: small italic grey ───────────────────────────────────────────
note_props <- fp_text(font.size=9, italic=TRUE, color="#555555")

add_note <- function(doc, note_text) {
  doc %>% body_add_fpar(
    fpar(ftext(note_text, prop=note_props)),
    style="Normal"
  )
}

# ─── Build multi-country cross-section interpretation ────────────────────────
# Compares reference country vs. max, mid and min (when ≥ 3 countries)
# ─── Cross-section interpretation (matches reference document style) ─────────
# Format: "The highest level is in X at Y%, while the lowest is in Z at W%."
# "In [ref], the value is V%. This is [higher/lower/within range] than the average..."
build_crosssection_interp <- function(df, ref_country, value_col="value_percentage",
                                      country_col="country_name", label="value",
                                      indicator_col=NULL, indicator_vals=NULL) {
  if (!is.null(indicator_col) && !is.null(indicator_vals))
    df <- df %>% filter(.data[[indicator_col]] %in% indicator_vals)
  df <- df %>% filter(!is.na(.data[[value_col]]))
  if (nrow(df)==0) return("")
  
  ref_val  <- df %>% filter(.data[[country_col]]==ref_country) %>% pull(.data[[value_col]]) %>% first()
  max_row  <- df %>% filter(.data[[value_col]]==max(.data[[value_col]],na.rm=TRUE)) %>% slice(1)
  min_row  <- df %>% filter(.data[[value_col]]==min(.data[[value_col]],na.rm=TRUE)) %>% slice(1)
  n_ctries <- nrow(df %>% distinct(.data[[country_col]]))
  avg_all  <- round(mean(df[[value_col]], na.rm=TRUE), 0)
  
  # Mid country (only when >= 3)
  mid_text <- ""
  if (n_ctries >= 3) {
    sorted_df  <- df %>% arrange(.data[[value_col]])
    mid_row    <- sorted_df %>% slice(ceiling(n()/2))
    if (mid_row[[country_col]] != max_row[[country_col]] && mid_row[[country_col]] != min_row[[country_col]]) {
      mid_text <- paste0(
        mid_row[[country_col]], " sits in the middle with ", round(mid_row[[value_col]],0), "%, "
      )
    }
  }
  
  range_text <- paste0(
    "The highest level is in ", max_row[[country_col]], " at ", round(max_row[[value_col]],0), "%, ",
    mid_text,
    "while the lowest is in ", min_row[[country_col]], " at ", round(min_row[[value_col]],0), "%. "
  )
  
  ref_text <- ""
  if (!is.na(ref_val)) {
    others     <- df %>% filter(.data[[country_col]]!=ref_country)
    avg_others <- if (nrow(others)>0) round(mean(others[[value_col]], na.rm=TRUE), 1) else NA
    pos_text   <- if (!is.na(avg_others)) {
      if (ref_val > max(others[[value_col]], na.rm=TRUE))
        "This is the highest value among the selected countries."
      else if (ref_val < min(others[[value_col]], na.rm=TRUE))
        "This is the lowest value among the selected countries."
      else if (ref_val > avg_others * 1.02)
        paste0("This is higher than the average of ", avg_others, "% across the other selected countries.")
      else if (ref_val < avg_others * 0.98)
        paste0("This is lower than the average of ", avg_others, "% across the other selected countries.")
      else
        paste0("This falls within the range observed across the selected countries.")
    } else ""
    ref_text <- paste0(
      "In ", ref_country, ", the value is ", round(ref_val,0), "%. ", pos_text
    )
  }
  paste0(range_text, ref_text)
}

# ─── Time-series interpretation (matches reference document style) ────────────
# Format: "The wage bill as a share of X in [ref] was Y% in [first_year] and has
#          [increased/decreased] to Z% in [last_year]."
# Plus mid-year. Plus comparison with other countries.
build_timeseries_interp <- function(df, ref_country, value_col="value",
                                    year_col="year", country_col="country_name",
                                    indicator_label_col=NULL, indicator_filter=NULL,
                                    unit="%") {
  if (!is.null(indicator_label_col) && !is.null(indicator_filter))
    df <- df %>% filter(.data[[indicator_label_col]] %in% indicator_filter)
  
  ref_data <- df %>% filter(.data[[country_col]]==ref_country) %>%
    arrange(.data[[year_col]]) %>% filter(!is.na(.data[[value_col]]))
  
  if (nrow(ref_data)==0) return(paste0("No time-series data available for ", ref_country, "."))
  
  years_vec <- ref_data[[year_col]]
  vals_vec  <- ref_data[[value_col]]
  n         <- length(years_vec)
  
  y_first <- years_vec[1];              v_first <- round(vals_vec[1], 0)
  y_last  <- years_vec[n];              v_last  <- round(vals_vec[n], 0)
  y_mid   <- years_vec[ceiling(n/2)];   v_mid   <- round(vals_vec[ceiling(n/2)], 0)
  
  direction <- if (v_last > v_first) "increased" else if (v_last < v_first) "decreased" else "remained stable"
  
  ref_text <- paste0(
    "The value in ", ref_country, " was ", v_first, unit, " in ", y_first,
    " and has ", direction, " to ", v_last, unit, " in ", y_last, ". ",
    "By ", y_mid, " it had reached ", v_mid, unit, ". "
  )
  
  # Compare with other countries at their last available year
  others_last <- df %>%
    filter(.data[[country_col]] != ref_country) %>%
    group_by(.data[[country_col]]) %>%
    filter(.data[[year_col]] == max(.data[[year_col]], na.rm=TRUE)) %>%
    slice(1) %>% ungroup() %>% filter(!is.na(.data[[value_col]]))
  
  comp_text <- ""
  if (nrow(others_last) > 0) {
    max_o <- others_last %>% filter(.data[[value_col]]==max(.data[[value_col]],na.rm=TRUE)) %>% slice(1)
    min_o <- others_last %>% filter(.data[[value_col]]==min(.data[[value_col]],na.rm=TRUE)) %>% slice(1)
    n_oth <- nrow(others_last)
    
    mid_text_o <- ""
    if (n_oth >= 3) {
      sorted_o <- others_last %>% arrange(.data[[value_col]])
      mid_o    <- sorted_o %>% slice(ceiling(n()/2))
      if (mid_o[[country_col]] != max_o[[country_col]] && mid_o[[country_col]] != min_o[[country_col]])
        mid_text_o <- paste0(mid_o[[country_col]], " at ", round(mid_o[[value_col]],0), unit, ", ")
    }
    
    comp_text <- paste0(
      "Among the other selected countries in the most recent available year, ",
      max_o[[country_col]], " shows the highest value at ", round(max_o[[value_col]],0), unit, ", ",
      mid_text_o,
      "and ", min_o[[country_col]], " the lowest at ", round(min_o[[value_col]],0), unit, ". "
    )
  }
  paste0(ref_text, comp_text)
}

# ─── "No data" plotly helper ─────────────────────────────────────────────────
# Returns a clean plotly with a centered message when no data is available
no_data_plot <- function(msg = "No data available for the selected country.") {
  plotly::plotly_empty(type="scatter", mode="markers") %>%
    plotly::layout(
      title = list(text=""),
      annotations = list(
        list(x=0.5, y=0.5, text=msg, showarrow=FALSE,
             font=list(size=15, color="#555555"),
             xref="paper", yref="paper",
             xanchor="center", yanchor="middle")
      ),
      plot_bgcolor="white", paper_bgcolor="white"
    )
}

# ─── Helper: which selected countries have NO data in a data frame ────────────
missing_countries <- function(df, selected, country_col="country_name",
                              value_col="value_percentage") {
  present <- df %>%
    filter(.data[[country_col]] %in% selected) %>%
    drop_na(all_of(value_col)) %>%
    pull(.data[[country_col]]) %>% unique()
  setdiff(selected, present)
}

# ─── Helper: add footnote for missing countries in Word report ────────────────
add_missing_footnote <- function(doc, missing_vec) {
  if (length(missing_vec) == 0) return(doc)
  note_props_fn <- fp_text(font.size=9, italic=TRUE, color="#B22222")   # red-ish
  doc %>% body_add_fpar(
    fpar(ftext(
      paste0("* No data available for: ", paste(missing_vec, collapse=", "), "."),
      prop=note_props_fn
    )),
    style="Normal"
  )
}

# ---------------------------
# UI
# ---------------------------
ui <- bootstrapPage(
  theme = bs_theme(version=5, bootswatch="sandstone"),
  tags$head(
    tags$style(HTML("
      :root { --wb-navy:#002244; --wb-blue:#003366; --bg:#F4F6F8; --card:#FFFFFF;
              --soft:#E6F0F7; --text:#1F2A33; --accent:#003366; --border:#E0E6ED; }
      html,body{height:100%;}
      body,.container-fluid,.main-container,.content-wrapper,.flex-grow-1{background-color:var(--bg)!important;color:var(--text)!important;}
      h1,h2,h3,h4,h5,h6{color:var(--wb-navy)!important;font-weight:600;}
      p,li,span,label{color:var(--text)!important;}
      a{color:var(--wb-blue)!important;text-decoration:none;}
      a:hover{color:#0072CE!important;text-decoration:underline;}
      /* ── Sidebar links: always white, never blue ── */
      #sidebar a, #sidebar a:link, #sidebar a:visited, #sidebar a:hover, #sidebar a:active,
      #sidebar .nav-item a, #sidebar .nav-sub-item a,
      #sidebar .nav-item button, #sidebar .nav-sub-item button,
      #sidebar button.action-button, #sidebar a.action-button {
        color: #ffffff !important;
        text-decoration: none !important;
        background: none !important;
        border: none !important;
        padding: 0 !important;
        font: inherit !important;
      }
      #sidebar{height:100vh;width:290px;min-width:290px;background:var(--wb-navy)!important;
        padding:18px 16px;color:#ffffff;overflow-y:auto;border-right:1px solid rgba(0,0,0,0.08);
        box-shadow:none;position:sticky;top:0;}
      #sidebar::-webkit-scrollbar{width:8px;}
      #sidebar::-webkit-scrollbar-thumb{background:rgba(255,255,255,.25);border-radius:8px;}
      .nav-section{display:flex;align-items:center;justify-content:space-between;
        font-size:16px;font-weight:700;padding:10px;margin:12px 6px 4px;
        color:#ffffff;border-radius:8px;transition:background .2s;cursor:pointer;}
      .nav-section:hover{background:rgba(255,255,255,.10);}
      .nav-section::after{content:'▾';font-size:14px;opacity:.8;margin-left:8px;}
      .nav-item{display:flex;align-items:center;gap:10px;margin:6px;padding:10px 12px;
        font-size:16px;font-weight:600;color:#eef5ff;border-radius:10px;transition:transform .08s,background .2s;}
      .nav-item:hover{background:rgba(255,255,255,.10);transform:translateX(2px);}
      .nav-item.active{background:rgba(230,240,247,.18);box-shadow:inset 0 0 0 1px rgba(230,240,247,.35);position:relative;}
      .nav-item.active::before{content:'';position:absolute;left:-6px;top:10px;bottom:10px;width:4px;border-radius:4px;background:var(--soft);}
      #macro_section,#public_sector_section,#public_sector_workforce_section,
      #public_sector_wages_section,#equity_public_sector_section{
        padding:4px 6px 6px 12px;display:none;border-left:1px dashed rgba(255,255,255,.25);margin-left:10px;}
      .nav-sub-item{display:flex;align-items:center;gap:8px;margin:4px 0;padding:8px 10px;
        font-size:15px;color:#eaf3ff;border-radius:8px;transition:background .2s,transform .08s;}
      .nav-sub-item:hover{background:rgba(255,255,255,.10);transform:translateX(2px);}
      .well,.card,.panel,.box,.info-box{background-color:var(--card)!important;color:var(--text)!important;
        border:1px solid var(--border)!important;border-radius:10px;box-shadow:0 2px 6px rgba(0,0,0,0.04);}
      .accordion-item{background-color:var(--card)!important;border:1px solid var(--border)!important;
        border-radius:12px!important;margin-bottom:14px;overflow:hidden;}
      .accordion-button{background-color:var(--card)!important;color:var(--wb-navy)!important;
        box-shadow:none!important;font-size:18px;padding:16px 20px;}
      .accordion-button:not(.collapsed){background-color:var(--soft)!important;}
      .accordion-body{background-color:var(--card)!important;padding:18px 22px;border-top:1px solid var(--border);}
      .logos-row{display:flex;align-items:center;justify-content:space-between;gap:24px;flex-wrap:nowrap;}
      .logo-wrap{flex:1 1 0;height:90px;display:flex;align-items:center;justify-content:center;}
      .logos-row img{height:70px!important;width:auto!important;max-width:100%;object-fit:contain;display:block;}
      .logos-row img.bl-logo{height:130px!important;}
      .btn,.btn-primary,.dl-btn{background-color:var(--wb-blue)!important;border:none!important;color:#fff!important;border-radius:10px;}
      .btn:hover,.dl-btn:hover{background-color:var(--wb-navy)!important;}
      #graph_choice .form-check{margin-bottom:.3rem;}
    ")),
    tags$script(HTML("
      function toggleSection(id){
        var s=document.getElementById(id);
        s.style.display=(s.style.display==='none'||s.style.display==='')?'block':'none';
        var h=document.querySelector('[onclick=\"toggleSection(\\''+id+'\\')\"]');
        if(h){h.classList.toggle('section-open');}
      }
      document.addEventListener('click',function(e){
        if(e.target.closest('.nav-item')){
          document.querySelectorAll('#sidebar .nav-item').forEach(n=>n.classList.remove('active'));
          e.target.closest('.nav-item').classList.add('active');
        }
        if(e.target.closest('.nav-sub-item')){
          document.querySelectorAll('#sidebar .nav-sub-item').forEach(n=>n.classList.remove('active'));
          e.target.closest('.nav-sub-item').classList.add('active');
        }
      },true);
    "))
  ),
  div(class="d-flex",
      div(id="sidebar",
          div(class="nav-item",actionLink("nav_dashboard","Overview")),
          div(class="nav-item",actionLink("nav_instructions","Instructions")),
          div(class="nav-item",actionLink("nav_metadata","Metadata")),
          div(class="nav-section",onclick="toggleSection('macro_section')","Macro Fundamentals of the Public Sector"),
          div(id="macro_section",
              div(class="nav-sub-item",actionLink("nav_wagebill","Wage Bill Graphs")),
              div(class="nav-sub-item",actionLink("nav_wagebill_gdp","Wage Bill & GDP Graphs"))),
          div(class="nav-section",onclick="toggleSection('public_sector_section')","Size and Characteristics of the Public Sector Employment"),
          div(id="public_sector_section",
              div(class="nav-sub-item",actionLink("nav_public_graphs","Public Employment")),
              div(class="nav-sub-item",actionLink("nav_public_workforce","Employment Distribution")),
              div(class="nav-sub-item",actionLink("nav_education","Tertiary Education"))),
          div(class="nav-section",onclick="toggleSection('public_sector_wages_section')","Competitiveness of Public Sector Wages"),
          div(id="public_sector_wages_section",
              div(class="nav-sub-item",actionLink("nav_wagepremium","Wage Premium")),
              div(class="nav-sub-item",actionLink("nav_public_educ","Wage Premium by Education")),
              div(class="nav-sub-item",actionLink("nav_pay_compression","Pay Compression"))),
          div(class="nav-section",onclick="toggleSection('equity_public_sector_section')","Equity in Public Sector Employment"),
          div(id="equity_public_sector_section",
              div(class="nav-sub-item",actionLink("nav_gender_workforce","Female Employment")),
              div(class="nav-sub-item",actionLink("nav_female_leadership","Female Leadership")),
              div(class="nav-sub-item",actionLink("nav_wagepremium_gender","Wage Premium by Gender")),
              div(class="nav-sub-item",actionLink("nav_gender_wage_premium","Gender Wage Premium by Industry"))),
          div(class="nav-item",actionLink("nav_download_all","\U1F4E5 Download All Graphs"))
      ),
      div(class="flex-grow-1 p-4",
          h2("Worldwide Bureaucracy Indicators"),
          uiOutput("main_content")
      )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  
  active_tab <- reactiveVal("dashboard")
  all_country_choices <- sort(unique(data_wwbi_long$country_name))
  
  observeEvent(input$nav_dashboard,          { active_tab("dashboard") })
  observeEvent(input$nav_instructions,       { active_tab("instructions") })
  observeEvent(input$nav_metadata,           { active_tab("metadata") })
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
  
  # ── UI ──────────────────────────────────────────────────────────────────────
  output$main_content <- renderUI({
    tab <- active_tab()
    
    mk_note <- function(txt) div(style="font-size:11px;color:#666;font-style:italic;margin-top:4px;", txt)
    mk_box  <- function(...) div(style="background:rgba(0,0,0,0.03);border:1px solid #E0E6ED;border-radius:8px;padding:16px;", ...)
    
    if (tab=="dashboard") {
      tagList(
        fluidRow(class="mb-3 logos-row",
                 column(4,div(class="logo-wrap",tags$img(src="logos/WBG-Institutions-Horizontal-CMYK-01.jpeg",class="wb-logo wb-logo--right",alt="WBG"))),
                 column(4,div(class="logo-wrap",tags$img(src="logos/bl_logo.png",class="bl-logo",alt="Bureaucracy Lab"))),
                 column(4,div(class="logo-wrap",tags$img(src="logos/WB-DEC-Impact-horizontal-RGB-high.png",class="wb-logo wb-logo--dec",alt="WBG DEC")))
        ),
        h3("Overview"),
        accordion(id="ov_acc",multiple=TRUE,open=character(0),
                  accordion_panel("About the WWBI",tagList(
                    p("The Worldwide Bureaucracy Indicators (WWBI) database is a unique cross-national dataset on public sector employment and wages that aims to fill an information gap, helping researchers, development practitioners, and policymakers gain a better understanding of the personnel dimensions of state capability."),
                    tags$ul(tags$li("Size of the public and private sector workforce"),tags$li("Demographics of public and private sector employment"),
                            tags$li("Gender equity in public and private sectors"),tags$li("Public sector wage premiums"),tags$li("Pay compression ratios")))),
                  accordion_panel("Contact Information",tags$p("Flavia Sacco – ",tags$a(href="mailto:fsaccocapurro@worldbank.org","fsaccocapurro@worldbank.org"),br(),"Josefina Silva – ",tags$a(href="mailto:jsilvafuentealba@worldbank.org","jsilvafuentealba@worldbank.org"))),
                  accordion_panel("Citation",p("Source: Worldwide Bureaucracy Indicators (WWBI) Dashboard – World Bank.")),
                  accordion_panel("Disclaimer",p("The findings presented do not necessarily reflect the views of the World Bank."))
        ),
        fluidRow(column(10,h3("\U1F4C4 Publications"),wellPanel(h4("Download Team Publications:"),
                                                                tags$ul(tags$li(downloadLink("pub1","Innovating Bureaucracy for a More Capable Government"),br(),tags$small("Report")),
                                                                        tags$li(downloadLink("pub2","Introducing the Worldwide Bureaucracy Indicators"),br(),tags$small("Baig et al.")),
                                                                        tags$li(downloadLink("pub3","Public Sector Employment and Compensation: An Assessment Framework"),br(),tags$small("Report")),
                                                                        tags$li(downloadLink("pub4","Worldwide Bureaucracy Indicators"),br(),tags$small("Report"))))))
      )
      
    } else if (tab=="instructions") {
      tagList(h3("\U1F4D8 Instruction Manual"),
              accordion(id="inst_acc",multiple=TRUE,open="About this dashboard",
                        accordion_panel("About this dashboard",p("This Dashboard is a product of the Bureaucracy Lab, a joint initiative between the Governance Global Practice and the Development Impact Evaluation (DIME) Department of the Research Group at the World Bank.")),
                        accordion_panel("How to use the dashboard",tags$ol(
                          tags$li("Select a country of interest and choose comparator countries, regions, or income groups."),
                          tags$li("The first selected country acts as the benchmark."),
                          tags$li("Check indicator availability in the 'Metadata' tab."),
                          tags$li("Download graphs via the camera icon or use 'Download All Graphs' for a Word report."))),
                        accordion_panel("Resources & links",
                                        tags$p("GitHub:",tags$a(href="https://github.com/worldbank/Worldwide-Bureaucracy-Indicators","https://github.com/worldbank/Worldwide-Bureaucracy-Indicators",target="_blank")),
                                        tags$p("Data Catalog:",tags$a(href="https://datacatalog.worldbank.org/int/home","https://datacatalog.worldbank.org/int/home",target="_blank")),
                                        div(style="margin-top:8px;",downloadButton("download_pdf","\U1F4E5 Download Codebook",class="btn btn-primary")))
              ))
      
    } else if (tab=="metadata") {
      tagList(h3("Metadata"),
              fluidRow(column(4,infoBox("Indicators",302,icon=icon("list"))),
                       column(4,infoBox("Economies",length(unique(data_wwbi$country_name)),icon=icon("globe"))),
                       column(4,infoBox("Coverage","2000-2022",icon=icon("calendar")))),
              fluidRow(column(6,selectInput("indicatorSelect","Select Indicator",choices=unique(data_wwbi$indicator_name)))),
              fluidRow(textOutput("countryCount")),
              fluidRow(leafletOutput("worldMap",height="600px")))
      
    } else if (tab=="wagebill") {
      tagList(h3("Wage Bill Graphs"),
              mk_box("This visualization explores the wage bill over time for selected countries."),
              fluidRow(
                column(7,selectInput("countries","Select country(ies)/region(s)/income group(s) – First selection = reference",choices=all_country_choices,multiple=TRUE,width="100%")),
                column(5,radioButtons("graph_choice",label=tags$span(class="rb-title","Choose wage-bill measure:"),
                                      choices=c("Wage Bill as % of Public Expenditure"="Public","Wage Bill as % of GDP"="GDP"),selected="Public",inline=FALSE))),
              fluidRow(plotlyOutput("plotwagebill",height="500px")),
              fluidRow(mk_note(textOutput("note_wagebill"))),
              fluidRow(column(12,downloadButton("downloadWord","Download Report in Word",class="dl-btn w-100"))),
              fluidRow(column(4,downloadButton("dl_csv_wagebill","Download data (CSV)",class="dl-btn w-100"))))
      
    } else if (tab=="wagebill_gdp") {
      tagList(h3("Wage Bill & GDP Graphs"),
              mk_box("This graph shows the relationship between the size of the wage bill and GDP per capita."),
              fluidRow(
                column(7,selectInput("countries_gdp","Select country(ies)/region(s)/income group(s)",choices=all_country_choices,multiple=TRUE,width="100%"),
                       br(),downloadButton("downloadGDPDoc","Download GDP Analysis Report",class="dl-btn w-100")),
                column(5,tags$label(class="form-label fw-semibold","Choose label type"),
                       radioButtons("label_type",label=NULL,choices=c("Country","Region"),selected="Country"))),
              fluidRow(column(12,plotlyOutput("dot_plot_gdp",height="500px"))),
              fluidRow(mk_note(textOutput("note_dotplot_gdp"))),
              fluidRow(column(12,div(class="text-end mt-3",downloadButton("dl_csv_gdp","Download data (CSV)",class="dl-btn")))))
      
    } else if (tab=="public_workforce") {
      tagList(h3("Distribution of Public Sector Employment"),
              mk_box("This visualization shows the distribution of the public sector workforce across the three main industries."),
              fluidRow(
                column(7,selectInput("countries_workforce","Select country(ies)/region(s)/income group(s)",choices=all_country_choices,multiple=TRUE,width="100%")),
                column(5,checkboxGroupInput("selected_graphs_public","Select Graphs to Download",
                                            choices=c("Multi-Country Graph"="firstGraph","Single-Country Graph"="secondGraph"),selected=c("firstGraph","secondGraph")),
                       downloadButton("downloadGraphsemploymentdist","Download Selected Graphs in Word",class="dl-btn w-100"))),
              fluidRow(plotlyOutput("stackedBarGraph",height="600px")),
              fluidRow(mk_note(textOutput("note_stackedBarGraph"))),
              fluidRow(column(12,selectInput("selected_country","Select country/region/income group",choices=all_country_choices,multiple=FALSE,width="100%"))),
              fluidRow(plotlyOutput("horizontalStackedBar",height="600px")),
              fluidRow(mk_note(textOutput("note_horizontalStackedBar"))),
              fluidRow(column(4,downloadButton("dl_csv_public_workforce","Download data (CSV)",class="dl-btn w-100"))))
      
    } else if (tab=="education") {
      tagList(h3("Workers with Tertiary Education"),
              mk_box("This visualization shows the proportion of workers with tertiary education in the public and private sectors."),
              fluidRow(
                column(7,selectInput("selected_countries","Select country(ies)/region(s)/income group(s)",choices=all_country_choices,multiple=TRUE,width="100%"),
                       br(),downloadButton("downloadGraphsWordEducation","Download Tertiary Education Report",class="dl-btn w-100")),
                column(5,tags$label(class="form-label fw-semibold","Choose label type"),
                       radioButtons("label_type_edu",label=NULL,choices=c("Country","Region"),selected="Country",inline=TRUE))),
              fluidRow(plotlyOutput("barPlot",height="600px")),
              fluidRow(mk_note(textOutput("note_tertiaryEducation"))),
              fluidRow(column(4,downloadButton("dl_csv_tertiary_edu","Download data (CSV)",class="dl-btn w-100"))))
      
    } else if (tab=="wagepremium") {
      tagList(h3("Public Sector Wage Premium"),
              fluidRow(column(7,selectInput("countries_wage_premium","Select country(ies)/region(s)/income group(s)",choices=all_country_choices,multiple=TRUE,width="100%"),
                              br(),downloadButton("downloadWagePremiumReport","Download Wage Premium Report",class="dl-btn w-100"))),
              fluidRow(plotlyOutput("dotPlot",height="500px")),
              fluidRow(mk_note(textOutput("note_wage_premium"))),
              fluidRow(column(4,downloadButton("dl_csv_wage_premium","Download data (CSV)",class="dl-btn w-100"))))
      
    } else if (tab=="public_educ") {
      tagList(h3("Public Sector Wage Premium by Education Level"),
              mk_box("This visualization explores the public sector wage premium by education level."),
              fluidRow(column(7,selectInput("selected_country","Select country/region/income group",choices=all_country_choices,multiple=FALSE,width="100%"),
                              br(),downloadButton("downloadEducationWagePremium","Download Education Wage Premium Report",class="dl-btn w-100"))),
              fluidRow(plotlyOutput("education_wage_premium_plot",height="600px")),
              fluidRow(mk_note(textOutput("note_education_wage_premium"))),
              fluidRow(column(4,downloadButton("dl_csv_wagepremium_educ","Download data (CSV)",class="dl-btn w-100"))))
      
    } else if (tab=="public_graphs") {
      tagList(h3("Public Sector Employment Graphs"),
              fluidRow(column(7,selectInput("countries_first","Select country(ies)/region(s)/income group(s)",choices=all_country_choices,multiple=TRUE,width="100%"))),
              fluidRow(plotlyOutput("firstGraphpublic",height="600px")),
              fluidRow(mk_note(textOutput("note_firstGraphpublic"))),
              fluidRow(column(12,selectInput("country_second","Select country/region/income group",choices=all_country_choices,multiple=FALSE,width="100%"))),
              fluidRow(plotlyOutput("secondGraphpublic",height="600px")),
              fluidRow(mk_note(textOutput("note_secondGraphpublic"))),
              fluidRow(column(12,downloadButton("downloadGraphsWord","Download Graphs as Word File",class="dl-btn w-100"))),
              fluidRow(column(12,div(class="text-end",downloadButton("dl_public_emp_data","Download data CSV",class="dl-btn")))))
      
    } else if (tab=="gender_workforce") {
      tagList(h3("Female share of employment"),
              fluidRow(mk_note(textOutput("note_firstGraphGenderWorkforce"))),
              fluidRow(column(7,selectInput("countries_gender","Select country(ies)/region(s)/income group(s)",choices=all_country_choices,multiple=TRUE,width="100%"),
                              br(),downloadButton("downloadGraphsWordGender","Download Female Share of Employment Report",class="dl-btn w-100"))),
              fluidRow(plotlyOutput("firstGraphGenderWorkforce",height="600px")),
              fluidRow(mk_note(textOutput("note_secondGraphGenderWorkforce"))),
              fluidRow(column(12,selectInput("country_gender","Select country/region/income group",choices=all_country_choices,multiple=FALSE,width="100%"))),
              fluidRow(plotlyOutput("secondGraphGenderWorkforce",height="600px")),
              fluidRow(column(4,downloadButton("dl_gender_workforce_xlsx","Download data (CSV)",class="dl-btn w-100"))))
      
    } else if (tab=="female_leadership") {
      tagList(h3("Female Leadership: Occupations and Sector"),
              fluidRow(column(7,selectInput("selected_countries","Select country(ies)/region(s)/income group(s)",choices=all_country_choices,multiple=TRUE,width="100%"),
                              br(),downloadButton("downloadGraphsWordfemale","Download Female Leadership Report",class="dl-btn w-100"))),
              fluidRow(column(12,plotlyOutput("barPlotwomen",height="600px"))),
              fluidRow(mk_note(textOutput("note_barPlotwomen"))),
              fluidRow(column(4,downloadButton("dl_csv_female_leadership","Download data (CSV)",class="dl-btn w-100"))))
      
    } else if (tab=="wagepremium_gender") {
      tagList(h3("Public Sector Wage Premium by Gender"),
              fluidRow(column(7,selectInput("countries_first","Select country(ies)/region(s)/income group(s)",choices=all_country_choices,multiple=TRUE,width="100%"),
                              br(),downloadButton("downloadGraphswagepremiumbygender","Download Wage Premium by Gender Report",class="dl-btn w-100"))),
              fluidRow(column(12,plotlyOutput("firstGraphGenderWagePremium",height="600px"))),
              fluidRow(mk_note(textOutput("note_firstGraphGenderWagePremium"))),
              fluidRow(column(7,selectInput("country_second","Select country/region/income group",choices=all_country_choices,multiple=FALSE,width="100%"))),
              fluidRow(column(12,plotlyOutput("secondGraphGenderWagePremium",height="600px"))),
              fluidRow(mk_note(textOutput("note_secondGraphGenderWagePremium"))),
              fluidRow(column(4,downloadButton("dl_gender_wageprem_xlsx","Download data (CSV)",class="dl-btn w-100"))))
      
    } else if (tab=="gender_wage_premium") {
      tagList(h3("Gender Wage Premium in Public Sector by Industry"),
              fluidRow(column(7,selectInput("selected_countries","Select country(ies)/region(s)/income group(s)",choices=all_country_choices,multiple=TRUE,width="100%"),
                              br(),downloadButton("downloadGenderWagePremium","Download Gender Wage Premium Report",class="dl-btn w-100"))),
              fluidRow(column(12,plotOutput("gender_wage_barplot",height="600px"))),
              fluidRow(mk_note(textOutput("note_gender_wage_barplot"))),
              fluidRow(column(4,downloadButton("dl_csv_gender_wage_industry","Download data (CSV)",class="dl-btn w-100"))))
      
    } else if (tab=="pay_compression") {
      tagList(h3("Pay Compression Ratios"),
              mk_box("This visualization explores pay compression in the public and private sectors across selected countries."),
              fluidRow(column(7,selectInput("countries_first","Select country(ies)/region(s)/income group(s)",choices=all_country_choices,multiple=TRUE,width="100%"),
                              br(),downloadButton("downloadPayCompressionDoc","Download Pay Compression Report",class="dl-btn w-100"))),
              fluidRow(plotlyOutput("paycompression_plot",height="600px")),
              fluidRow(mk_note(textOutput("note_dotplot_pay"))),
              fluidRow(column(4,downloadButton("dl_csv_pay_compression","Download data (CSV)",class="dl-btn w-100"))))
      
    } else if (tab=="download_all") {
      tagList(h3("Download Graph Reports"),
              mk_box(p("Download a comprehensive report with all graphs or select specific graphs.",style="font-size:16px;")),
              br(),
              fluidRow(column(6,align="center",selectInput("download_report_countries","Select countries/regions/income group(s):",
                                                           choices=all_country_choices,selected=c("Chile"),multiple=TRUE,selectize=TRUE))),
              h4("Download a Custom Report"),
              checkboxGroupInput("selected_graphs","Select Graphs to Include:",
                                 choices=list("Wage Bill"="wagebill","Wage Bill as % of GDP"="wagebill_gdp","Tertiary Education"="tertiaryeducation",
                                              "Wage Premium by Education"="wagepremiumeducation","Public Employment"="public_employment",
                                              "Wage Premium by Gender"="wagepremiumgender","Public Sector Workforce"="public_workforce",
                                              "Female Employment"="gender_workforce","Female Leadership"="female_leadership","Wage Premium"="wagepremium",
                                              "Gender Wage Premium by Industry"="gender_wage_premium","Pay Compression"="pay_compression"),
                                 selected=c("wagebill","public_employment")),
              br(),
              fluidRow(
                column(6,align="center",downloadButton("downloadAllGraphsDoc","\U1F4C4 Download Full Word Report",style="padding:10px 20px;font-size:16px;margin-top:10px;")),
                column(6,align="center",downloadButton("downloadSelectedGraphsDoc","\U1F4C4 Download Custom Word Report",style="padding:10px 20px;font-size:16px;margin-top:10px;")),
                column(6,align="center",downloadButton("downloadSelectedGraphsPPT","\U1F4CA Download PowerPoint Slides",style="padding:10px 20px;font-size:16px;margin-top:10px;"))))
    }
  })
  
  # ============================================================
  # PLOTLY OUTPUTS  (unchanged from v1 – kept concise)
  # ============================================================
  selected_data <- reactive({
    req(input$countries)
    if (input$graph_choice=="GDP") wage_bill_gdp %>% filter(country_name %in% input$countries)
    else wage_bill_publicexp %>% filter(country_name %in% input$countries)
  })
  
  output$plotwagebill <- renderPlotly({
    d <- selected_data()
    if (nrow(d)==0) return(no_data_plot(paste0("No data available for the selected countries: ", paste(input$countries, collapse=", "))))
    y_label <- ifelse(input$graph_choice=="GDP","Wage Bill (% of GDP)","Wage Bill (% of Public Expenditure)")
    plot_ly(d,x=~year,y=~value,color=~country_name,type="scatter",mode="lines+markers",marker=list(size=8)) %>%
      layout(title=paste(y_label,"Over Time"),xaxis=list(title="Year",dtick=2),yaxis=list(title=y_label))
  })
  output$note_wagebill <- renderText({
    if (input$graph_choice=="GDP") "Note: Wage bill as a percentage of GDP."
    else "Note: Wage bill as a percentage of public expenditure."
  })
  output$dl_csv_wagebill <- downloadHandler(
    filename=function() paste0("wagebill_",ifelse(input$graph_choice=="GDP","gdp","publicexp"),"_",Sys.Date(),".csv"),
    content=function(file){ d<-selected_data(); readr::write_csv(d,file) })
  
  output$dot_plot_gdp <- renderPlotly({
    req(input$countries_gdp)
    d <- merged_data %>% filter(country_name %in% input$countries_gdp)
    if (nrow(d)==0) return(no_data_plot(paste0("No data available for: ", paste(input$countries_gdp, collapse=", "))))
    first_sel <- input$countries_gdp[1]
    d <- d %>% mutate(color=ifelse(country_name==first_sel,"#B3242B","#003366"))
    region_col <- intersect(c("region","region_name","Region"),names(d))[1]
    label_vec  <- if (!is.na(region_col) && identical(input$label_type,"Region")) d[[region_col]] else d$country_name
    m <- lm(indicator_value~log_gdp,data=d); pred <- predict(m,newdata=d)
    plot_ly(d,x=~log_gdp,y=~indicator_value,type="scatter",mode="markers+text",text=label_vec,textposition="top center",
            marker=list(size=10,color=~color,opacity=0.7)) %>%
      add_trace(x=d$log_gdp,y=pred,inherit=FALSE,type="scatter",mode="lines",line=list(color="gray",dash="dash"),showlegend=FALSE) %>%
      layout(title="Wage Bill vs. Log(GDP per Capita)",xaxis=list(title="Log(GDP per Capita, 2015)"),yaxis=list(title="Wage Bill"),showlegend=FALSE,plot_bgcolor="white",paper_bgcolor="white")
  })
  output$note_dotplot_gdp <- renderText({"Note: Relationship between wage bill and income level. Last year available per country."})
  dot_data_gdp <- reactive({ req(input$countries_gdp); merged_data %>% filter(country_name %in% input$countries_gdp) })
  output$dl_csv_gdp <- downloadHandler(filename=function() paste0("wagebill_vs_gdp_",Sys.Date(),".csv"),
                                       content=function(file){ d<-dot_data_gdp(); utils::write.csv(d,file,row.names=FALSE) })
  
  filtered_workforce_data <- reactive({
    req(input$countries_workforce)
    public_sector_workforce_clean %>% group_by(country_name,indicator_name) %>% slice_max(order_by=year,n=1) %>% ungroup()
  })
  output$stackedBarGraph <- renderPlotly({
    req(input$countries_workforce)
    d <- filtered_workforce_data() %>% filter(country_name %in% input$countries_workforce)
    if (nrow(d)==0) return(no_data_plot(paste0("No data available for: ", paste(input$countries_workforce, collapse=", "))))
    cols <- c("Public Administration"="#E69F00","Education"="#56B4E9","Health"="#009E73","Other"="#F0E442")
    plot_ly(d,x=~country_name,y=~value_percentage,color=~indicator_name,type="bar",text=~paste("Country:",country_name,"Value:",round(value_percentage,1),"%"),colors=cols) %>%
      layout(barmode="stack",title="Public Workforce Distribution by Country",xaxis=list(title="Country"),yaxis=list(title="Workforce Distribution (%)",range=c(0,100)))
  })
  output$note_stackedBarGraph <- renderText({"Note: Distribution of public sector employment across industries as % of paid public employment. Last year available."})
  output$horizontalStackedBar <- renderPlotly({
    req(input$selected_country)
    d <- public_sector_workforce %>% filter(country_name==input$selected_country)
    if (nrow(d)==0) return(no_data_plot(paste0("No data available for: ", input$selected_country)))
    fy <- min(d$year,na.rm=TRUE); ly <- max(d$year,na.rm=TRUE)
    d2 <- d %>% filter(year %in% c(fy,ly)) %>% group_by(year,indicator_name) %>% summarise(value_percentage=mean(value_percentage,na.rm=TRUE),.groups="drop")
    cols <- c("Public Administration"="#E69F00","Education"="#56B4E9","Health"="#009E73","Other"="#F0E442")
    plot_ly(d2,x=~value_percentage,y=~factor(year,levels=c(ly,fy)),color=~indicator_name,type="bar",orientation="h",
            text=~paste0(round(value_percentage,1),"%"),textposition="inside",colors=cols) %>%
      layout(barmode="stack",title=paste("Sectoral Distribution in",input$selected_country,"(",fy,"&",ly,")"),
             xaxis=list(title="Percentage (%)"),yaxis=list(title="Year"))
  })
  output$note_horizontalStackedBar <- renderText({ paste0("Note: Sectoral distribution in ", input$selected_country," for earliest and latest years.")})
  output$dl_csv_public_workforce <- downloadHandler(filename=function() paste0("public_workforce_",Sys.Date(),".csv"),
                                                    content=function(file){ df<-public_sector_workforce %>% filter(country_name %in% input$countries_workforce); utils::write.csv(df,file,row.names=FALSE,na="") })
  outputOptions(output,"dl_csv_public_workforce",suspendWhenHidden=FALSE)
  
  output$barPlot <- renderPlotly({
    req(input$selected_countries)
    d <- tertiary_education %>% filter(country_name %in% input$selected_countries)
    if (nrow(d)==0) return(no_data_plot(paste0("No data available for: ", paste(input$selected_countries, collapse=", "))))
    cols <- c("as a share of private paid employees"="#0072B2","as a share of public paid employees"="#D55E00")
    d %>% plot_ly(x=~country_name,y=~value_percentage,color=~indicator_name,colors=cols,type="bar",barmode="group",
                  text=~paste("Country:",country_name,"Value:",round(value_percentage,1),"%","Year:",year),textposition="auto") %>%
      layout(title="Workers with Tertiary Education",xaxis=list(title="Country"),yaxis=list(title="Tertiary Education (%)"))
  })
  output$note_tertiaryEducation <- renderText({"Note: Proportion of individuals with tertiary education in the public and private sectors. Last year available."})
  output$dl_csv_tertiary_edu <- downloadHandler(filename=function() paste0("tertiary_education_",Sys.Date(),".csv"),
                                                content=function(file){ req(input$selected_countries); out<-tertiary_education %>% filter(country_name %in% input$selected_countries); utils::write.csv(out,file,row.names=FALSE,na="") })
  
  output$dotPlot <- renderPlotly({
    req(input$countries_wage_premium)
    d <- public_wage_premium %>% filter(country_name %in% input$countries_wage_premium) %>% select(country_name,value_percentage,year) %>% drop_na(value_percentage) %>%
      mutate(color=ifelse(country_name==input$countries_wage_premium[1],"#B3242B","#003366"))
    if (nrow(d)==0) return(no_data_plot(paste0("No data available for: ", paste(input$countries_wage_premium, collapse=", "))))
    plot_ly(d,x=~country_name,y=~value_percentage,type="scatter",mode="markers",marker=list(size=10,opacity=0.8,color=~color),
            text=~paste("Country:",country_name,"<br>Value:",round(value_percentage,1),"%","<br>Year:",year)) %>%
      layout(title="Public Sector Wage Premium by Country",xaxis=list(title="Country"),yaxis=list(title="Wage Premium (%)"),showlegend=FALSE)
  })
  output$note_wage_premium <- renderText({"Note: Estimated public sector wage premium controlling for gender, education, tenure and location. Last year available."})
  output$dl_csv_wage_premium <- downloadHandler(filename=function() paste0("wage_premium_",Sys.Date(),".csv"),
                                                content=function(file){ df<-public_wage_premium %>% filter(country_name %in% input$countries_wage_premium) %>% drop_na(value_percentage); utils::write.csv(df,file,row.names=FALSE,na="") })
  
  output$education_wage_premium_plot <- renderPlotly({
    req(input$selected_country)
    d <- public_wage_premium_educ %>% filter(country_name==input$selected_country) %>% drop_na(value_percentage)
    if (nrow(d)==0) return(no_data_plot(paste0("No data available for: ", input$selected_country)))
    edu_cols <- c("No Education"="#E69F00","Primary Education"="#56B4E9","Secondary Education"="#009E73","Tertiary Education"="#D55E00")
    ggplotly(ggplot(d,aes(x=indicator_name,y=value_percentage,fill=indicator_name))+geom_bar(stat="identity")+scale_fill_manual(values=edu_cols)+
               labs(title="Public Sector Wage Premium by Education Level",x="Education Level",y="Wage Premium (%)")+theme_minimal())
  })
  output$note_education_wage_premium <- renderText({"Note: Public sector wage premium across education levels vs. private formal workers. Last year available."})
  output$dl_csv_wagepremium_educ <- downloadHandler(filename=function() paste0("wagepremium_educ_",Sys.Date(),".csv"),
                                                    content=function(file){ df<-public_wage_premium_educ %>% filter(country_name==input$selected_country) %>% drop_na(value_percentage); utils::write.csv(df,file,row.names=FALSE,na="") })
  
  output$firstGraphpublic <- renderPlotly({
    d <- public_sector_emp_temp_last %>% filter(country_name %in% input$countries_first)
    if (nrow(d)==0) return(no_data_plot(paste0("No data available for: ", paste(input$countries_first, collapse=", "))))
    ggplotly(ggplot(d,aes(x=country_name,y=value_percentage,color=indicator_label))+geom_point(size=4)+
               scale_color_manual(values=c("as a share of formal employment"="#E69F00","as a share of paid employment"="#56B4E9","as a share of total employment"="#009E73"))+
               labs(title="Public Sector Employment (Last Year Available)",x="Country",y="Value",color="Indicator")+theme_minimal()+theme(axis.text.x=element_text(angle=45,hjust=1)))
  })
  output$note_firstGraphpublic <- renderText({"Note: Share of public sector employment in formal, paid, and total employment. Last year available."})
  output$secondGraphpublic <- renderPlotly({
    d <- public_sector_emp_temp %>% filter(country_name==input$country_second)
    if (nrow(d)==0) return(no_data_plot(paste0("No data available for: ", input$country_second)))
    ggplotly(ggplot(d,aes(x=year,y=value_percentage,color=indicator_label))+geom_line(size=1.2)+geom_point(size=3)+
               scale_color_manual(values=c("as a share of formal employment"="#E69F00","as a share of paid employment"="#56B4E9","as a share of total employment"="#009E73"))+
               labs(title="Public Sector Employment Over Time",x="Year",y="Value",color="Indicator")+theme_minimal())
  })
  output$note_secondGraphpublic <- renderText({"Note: Evolution of public sector employment over time."})
  output$dl_public_emp_data <- downloadHandler(filename=function() paste0("public_employment_",Sys.Date(),".xlsx"),
                                               content=function(file){
                                                 d1<-public_sector_emp_temp_last %>% filter(country_name %in% input$countries_first) %>% transmute(country_name,indicator=indicator_label,year=as.numeric(year),value_percentage=as.numeric(value_percentage))
                                                 d2<-public_sector_emp_temp %>% filter(country_name==input$country_second) %>% transmute(country_name,indicator=indicator_label,year=as.numeric(year),value_percentage=as.numeric(value_percentage))
                                                 writexl::write_xlsx(list("Graph1_MultiCountry_LastYear"=d1,"Graph2_SingleCountry_OverTime"=d2),path=file)})
  
  output$firstGraphGenderWorkforce <- renderPlotly({
    req(input$countries_gender)
    d <- gender_workforce %>% filter(country_name %in% input$countries_gender) %>%
      group_by(country_name,indicator_name) %>% arrange(year,.by_group=TRUE) %>% slice_tail(n=1) %>% ungroup()
    if (nrow(d)==0) return(no_data_plot(paste0("No data available for: ", paste(input$countries_gender, collapse=", "))))
    d$text <- paste0("Country: ",d$country_name,"<br>Sector: ",d$indicator_name,"<br>Employment: ",round(d$value_percentage,1),"%<br>Year: ",d$year)
    plotly::ggplotly(ggplot(d,aes(x=country_name,y=value_percentage,fill=indicator_name,text=text))+
                       geom_col(position=position_dodge(width=0.8),width=0.7)+
                       scale_fill_manual(values=c("as a share of private paid employees"="#E69F00","as a share of public paid employees"="#56B4E9"))+
                       labs(title="Female Employment by Sector (Last Year Available)",x="Country",y="Employment (%)",fill="Sector")+theme_minimal(),tooltip="text")
  })
  output$note_firstGraphGenderWorkforce <- renderText({"Note: Share of females employed in the public and private sectors. Last year available."})
  output$secondGraphGenderWorkforce <- renderPlotly({
    d <- gender_workforce %>% filter(country_name==input$country_gender)
    if (nrow(d)==0) return(no_data_plot(paste0("No data available for: ", input$country_gender)))
    ggplotly(ggplot(d,aes(x=year,y=value_percentage,color=indicator_name))+geom_line(size=1.2)+geom_point(size=3)+
               scale_color_manual(values=c("as a share of private paid employees"="#E69F00","as a share of public paid employees"="#56B4E9"))+
               labs(title=paste("Female Employment by Sector Over Time in",input$country_gender),x="Year",y="Female Employment (%)",color="Sector")+theme_minimal())
  })
  output$note_secondGraphGenderWorkforce <- renderText({"Note: Female employment in public and private sectors over time."})
  output$dl_gender_workforce_xlsx <- downloadHandler(filename=function() paste0("female_employment_",Sys.Date(),".xlsx"),
                                                     content=function(file){
                                                       d1<-gender_workforce %>% filter(country_name %in% input$countries_gender) %>% group_by(country_name,indicator_name) %>% slice_tail(n=1) %>% ungroup() %>% transmute(country_name,sector=indicator_name,year=as.numeric(year),female_share_pct=as.numeric(value_percentage))
                                                       d2<-gender_workforce %>% filter(country_name==input$country_gender) %>% transmute(country_name,sector=indicator_name,year=as.numeric(year),female_share_pct=as.numeric(value_percentage))
                                                       writexl::write_xlsx(list("Graph1_MultiCountry_LastYear"=d1,"Graph2_SingleCountry_OverTime"=d2),path=file)})
  
  output$firstGraphGenderWagePremium <- renderPlotly({
    d <- gender_wage_premium_last %>% filter(country_name %in% input$countries_first)
    if (nrow(d)==0) return(no_data_plot(paste0("No data available for: ", paste(input$countries_first, collapse=", "))))
    ggplotly(ggplot(d,aes(x=country_name,y=value_percentage,color=indicator_label))+geom_point(size=4)+
               scale_color_manual(values=c("Male"="#E69F00","Female"="#56B4E9"))+
               labs(title="Public Sector Wage Premium by Gender (Last Year Available)",x="Country",y="Wage Premium (%)",color="Gender")+theme_minimal())
  })
  output$note_firstGraphGenderWagePremium <- renderText({"Note: Public-sector wage premium by gender. Positive = higher public sector wages. Last year available."})
  output$secondGraphGenderWagePremium <- renderPlotly({
    d <- gender_wage_premium %>% filter(country_name==input$country_second)
    if (nrow(d)==0) return(no_data_plot(paste0("No data available for: ", input$country_second)))
    ggplotly(ggplot(d,aes(x=year,y=value_percentage,color=indicator_label))+geom_line(size=1.2)+geom_point(size=3)+
               scale_color_manual(values=c("Male"="#E69F00","Female"="#56B4E9"))+
               labs(title="Public Sector Wage Premium by Gender Over Time",x="Year",y="Wage Premium (%)",color="Gender")+theme_minimal())
  })
  output$note_secondGraphGenderWagePremium <- renderText({"Note: Public sector wage premium for men and women over time vs. private sector."})
  output$dl_gender_wageprem_xlsx <- downloadHandler(filename=function() paste0("wage_premium_gender_",Sys.Date(),".xlsx"),
                                                    content=function(file){
                                                      d1<-gender_wage_premium_last %>% filter(country_name %in% input$countries_first) %>% transmute(country_name,gender=indicator_label,year=as.numeric(year),wage_premium_pct=as.numeric(value_percentage))
                                                      d2<-gender_wage_premium %>% filter(country_name==input$country_second) %>% transmute(country_name,gender=indicator_label,year=as.numeric(year),wage_premium_pct=as.numeric(value_percentage))
                                                      writexl::write_xlsx(list("Graph1_MultiCountry_LastYear"=d1,"Graph2_SingleCountry_OverTime"=d2),path=file)})
  
  output$barPlotwomen <- renderPlotly({
    if (is.null(input$selected_countries)||!length(input$selected_countries)) return(no_data_plot("Please select at least one country."))
    d <- gender_leadership %>% filter(country_name %in% input$selected_countries) %>%
      mutate(indicator_label=factor(indicator_label,levels=c("Clerks-Private","Clerks-Public","Managers-Private","Managers-Public")))
    if (nrow(d)==0) return(no_data_plot(paste0("No data available for: ", paste(input$selected_countries, collapse=", "))))
    cols <- c("Clerks-Private"="#9ECAE1","Clerks-Public"="#08519C","Managers-Private"="#FDAE6B","Managers-Public"="#E6550D")
    plot_ly(d,x=~country_name,y=~value_percentage,color=~indicator_label,colors=cols,type="bar",barmode="group",
            text=~paste0("Country: ",country_name,"<br>Group: ",indicator_label,"<br>Female Share: ",round(value_percentage,1),"%"),hoverinfo="text") %>%
      layout(title="Females by Occupational Group and Sector",xaxis=list(title="Country"),yaxis=list(title="Female Share (%)"))
  })
  output$note_barPlotwomen <- renderText({"Note: Share of females in Managers/Clerks positions in public and private sectors. Last year available."})
  output$dl_csv_female_leadership <- downloadHandler(filename=function() paste0("female_leadership_",Sys.Date(),".csv"),
                                                     content=function(file){
                                                       d<-gender_leadership %>% filter(country_name %in% input$selected_countries) %>% group_by(country_name,indicator_label) %>% slice_max(order_by=year,n=1,with_ties=FALSE) %>% ungroup() %>% transmute(country_name=as.character(country_name),group_sector=as.character(indicator_label),year=as.integer(year),female_share_pct=as.numeric(value_percentage))
                                                       utils::write.csv(d,file,row.names=FALSE,na="")})
  
  output$gender_wage_barplot <- renderPlot({
    d <- gender_wage_premiumpublic %>% filter(country_name %in% input$selected_countries, indicator_label %in% c("Public Administration","Education","Health","Other"))
    if (nrow(d)==0) return(ggplot()+theme_void()+annotate("text",x=0.5,y=0.5,label=paste0("No data available for: ",paste(input$selected_countries,collapse=", ")),size=5,color="#555555",hjust=0.5,vjust=0.5))
    ggplot(d,aes(x=country_name,y=value_percentage,fill=indicator_label))+geom_bar(stat="identity",position="dodge")+
      scale_fill_viridis_d(name="Indicator",option="D")+labs(title="Gender Wage Premium in Public Sector by Industry",x="Country",y="Wage Premium (%)")+theme_minimal()
  })
  output$note_gender_wage_barplot <- renderText({"Note: Gender wage premium in the public sector by industry. Last year available."})
  output$dl_csv_gender_wage_industry <- downloadHandler(filename=function() paste0("gender_wage_premium_industry_",Sys.Date(),".csv"),
                                                        content=function(file){
                                                          d<-gender_wage_premiumpublic %>% filter(country_name %in% input$selected_countries,indicator_label %in% c("Public Administration","Education","Health","Other")) %>% group_by(country_name,indicator_label) %>% slice_max(order_by=year,n=1,with_ties=FALSE) %>% ungroup() %>% transmute(country_name,industry=indicator_label,year=as.integer(year),wage_premium_pct=as.numeric(value_percentage))
                                                          utils::write.csv(d,file,row.names=FALSE,na="")})
  
  output$paycompression_plot <- renderPlotly({
    req(input$countries_first)
    d <- pay_compression_wide %>% filter(country_name %in% input$countries_first) %>%
      mutate(Public_Sector=as.numeric(unlist(Public_Sector)),Private_Sector=as.numeric(unlist(Private_Sector))) %>% drop_na(Public_Sector,Private_Sector) %>%
      mutate(color=ifelse(country_name==input$countries_first[1],"#B3242B","#003366"))
    if (nrow(d)==0) return(no_data_plot(paste0("No data available for: ", paste(input$countries_first, collapse=", "))))
    p <- plot_ly() %>% add_trace(data=d,x=~Private_Sector,y=~Public_Sector,type="scatter",mode="markers+text",text=~country_name,textposition="top center",
                                 marker=list(size=10,color=~color,opacity=0.7),name="Country")
    if (nrow(d)>=2 && sd(d$Private_Sector,na.rm=TRUE)>0) {
      m <- lm(Public_Sector~Private_Sector,data=d); x_seq <- seq(min(d$Private_Sector,na.rm=TRUE),max(d$Private_Sector,na.rm=TRUE),length.out=50)
      p <- p %>% add_trace(x=x_seq,y=predict(m,newdata=data.frame(Private_Sector=x_seq)),type="scatter",mode="lines",line=list(color="gray",dash="dash"),name="Trendline")
    }
    p %>% layout(title="Pay Compression: Public vs. Private Sector (Latest Year)",xaxis=list(title="Private Sector Pay Compression"),yaxis=list(title="Public Sector Pay Compression"),plot_bgcolor="white",paper_bgcolor="white")
  })
  output$note_dotplot_pay <- renderText({"Note: Pay compression ratios (P90/P10). Higher values = wider wage dispersion. Last year available."})
  output$dl_csv_pay_compression <- downloadHandler(filename=function() paste0("pay_compression_",Sys.Date(),".csv"),
                                                   content=function(file){
                                                     d<-pay_compression_wide %>% filter(country_name %in% input$countries_first) %>% transmute(country_name,private_sector=as.numeric(Private_Sector),public_sector=as.numeric(Public_Sector))
                                                     utils::write.csv(d,file,row.names=FALSE,na="")})
  
  output$worldMap <- renderLeaflet({
    leaflet(world_spdf) %>% addTiles() %>% setView(lng=0,lat=20,zoom=2) %>%
      addLegend(position="bottomright",colors=c("gray","#6DA96F"),labels=c("No Data","Reported"),title="Indicator Availability",opacity=1)
  })
  filtered_data_for_map <- reactive({
    req(input$indicatorSelect)
    data_wwbi %>% filter(indicator_name==input$indicatorSelect) %>%
      mutate(any_data=apply(select(.,starts_with("year_")),1,function(x) any(!is.na(x)))) %>%
      filter(any_data) %>% transmute(country_name,indicator_name,has_data=1)
  })
  observe({
    req(input$indicatorSelect)
    rc <- filtered_data_for_map(); if (nrow(rc)==0) return()
    wm <- world_spdf %>% left_join(rc,by=c("name_long"="country_name"))
    cp <- colorFactor(palette=c("gray","#6DA96F"),domain=c(0,1))
    leafletProxy("worldMap") %>% clearShapes() %>%
      addPolygons(data=wm,fillColor=~cp(ifelse(is.na(has_data),0,has_data)),fillOpacity=0.7,color="white",weight=1,
                  highlightOptions=highlightOptions(color="#FFD700",weight=2,fillOpacity=0.9),
                  label=~paste0("Country: ",name_long," - ",ifelse(!is.na(has_data),"Reported","No Data")))
    output$countryCount <- renderText({ paste("Total Countries with Data:",nrow(rc)) })
  })
  
  output$download_pdf <- downloadHandler(filename="Codebook and Explanatory Note.pdf",content=function(file) file.copy(file.path(data_path,"Files","WWBI Codebook v3.1.pdf"),file))
  output$pub1 <- downloadHandler(filename=function() "Innovating-Bureaucracy-for-a-More-Capable-Government.pdf",content=function(file) file.copy(file.path(data_path,"Files","Innovating-Bureaucracy-for-a-More-Capable-Government.pdf"),file))
  output$pub2 <- downloadHandler(filename=function() "WWBI-Introduction.pdf",content=function(file) file.copy(file.path(data_path,"Files","Public Administration Review - 2021 - Baig - Introducing the Worldwide Bureaucracy Indicators  A New Global Dataset on.pdf"),file))
  output$pub3 <- downloadHandler(filename=function() "Public-Sector-Employment-and-Compensation-An-Assessment-Framework.pdf",content=function(file) file.copy(file.path(data_path,"Files","Public-Sector-Employment-and-Compensation-An-Assessment-Framework.pdf"),file))
  output$pub4 <- downloadHandler(filename=function() "Worldwide-Bureaucracy-Indicators-Methodology-Insights-and-Applications.pdf",content=function(file) file.copy(file.path(data_path,"Files","Worldwide-Bureaucracy-Indicators-Methodology-Insights-and-Applications.pdf"),file))
  
  # ============================================================
  # WORD DOWNLOAD HANDLERS (individual tabs)
  # ============================================================
  
  # ── Helper: add image + note + interpretation block ─────────────────────────
  add_figure_block <- function(doc, img_path, note_text, interp_text,
                               fig_width=6.5, fig_height=4.5, missing_vec=character(0)) {
    note_props_local <- fp_text(font.size=9, italic=TRUE, color="#555555")
    fn_props_local   <- fp_text(font.size=9, italic=TRUE, color="#B22222")
    doc <- doc %>%
      body_add_img(src=img_path, width=fig_width, height=fig_height) %>%
      body_add_fpar(fpar(ftext(note_text, prop=note_props_local)), style="Normal")
    if (length(missing_vec) > 0) {
      doc <- doc %>% body_add_fpar(
        fpar(ftext(paste0("* No data available for: ", paste(missing_vec, collapse=", "), "."), prop=fn_props_local)),
        style="Normal"
      )
    }
    doc <- doc %>% body_add_par(interp_text, style="Normal")
    return(doc)
  }
  
  output$downloadWord <- downloadHandler(
    filename=function() paste0("Wage_Bill_Analysis_",Sys.Date(),".docx"),
    content=function(file){
      req(input$countries)
      ref <- input$countries[1]
      first_region <- tryCatch(countrycode(ref,origin="country.name",destination="region"),error=function(e) "its region")
      if (is.na(first_region)) first_region <- "its region"
      the_data <- if (input$graph_choice=="GDP") wage_bill_gdp else wage_bill_publicexp
      d <- the_data %>% filter(country_name %in% input$countries)
      
      ts_interp <- build_timeseries_interp(d, ref, value_col="value", year_col="year",
                                           country_col="country_name", unit="%")
      
      title_style <- fp_text(color="#722F37",font.size=16,bold=TRUE)
      doc <- officer::read_docx() %>%
        body_add_fpar(fpar(ftext(paste("Wage Bill Analysis Report -",ref),prop=title_style))) %>%
        body_add_par("The macro fundamentals of the public sector",style="heading 3") %>%
        body_add_par(paste0("This note presents evidence on public sector employment and compensation practices in ",ref,
                            " using the Worldwide Bureaucracy Indicators (WWBI). For international comparisons, peer countries from ",first_region," are included."),style="Normal")
      
      g <- ggplot(d,aes(x=year,y=value,color=country_name))+geom_line(size=1.2)+geom_point(size=3)+
        labs(title=ifelse(input$graph_choice=="GDP","Wage Bill as % of GDP","Wage Bill as % of Public Expenditure"),x="Year",y="Value")+theme_minimal()
      img_path <- tempfile(fileext=".png"); ggsave(img_path,plot=g,width=8,height=5,dpi=300)
      note_txt <- paste0("Note: Wage bill as a percentage of ",ifelse(input$graph_choice=="GDP","GDP","public expenditure")," over time.")
      doc <- add_figure_block(doc, img_path, note_txt, ts_interp)
      print(doc,target=file)
    })
  
  output$downloadGDPDoc <- downloadHandler(
    filename=function() paste0("Wage_Bill_vs_GDP_Report_",Sys.Date(),".docx"),
    content=function(file){
      req(input$countries_gdp)
      d <- merged_data %>% filter(country_name %in% input$countries_gdp); req(nrow(d)>0)
      ref <- input$countries_gdp[1]
      cs_interp <- build_crosssection_interp(d, ref, value_col="indicator_value", label="wage bill (%)")
      
      d_plot <- d %>% mutate(highlight=ifelse(country_name==ref,"Selected country","Other countries"))
      p <- ggplot(d_plot,aes(x=log_gdp,y=indicator_value))+geom_point(aes(color=highlight),size=3,show.legend=FALSE)+
        ggrepel::geom_text_repel(aes(label=country_name),color="black",size=3,max.overlaps=30)+
        geom_smooth(method="lm",se=FALSE,color="gray50",linetype="dashed")+
        scale_color_manual(values=c("Selected country"="#B3242B","Other countries"="#003366"),guide="none")+
        labs(title="Wage Bill vs. Log(GDP per Capita)",x="Log(GDP per Capita, 2015)",y="Wage Bill (% of Public Expenditure)")+theme_minimal()
      img_path <- tempfile(fileext=".png"); ggsave(img_path,plot=p,width=7,height=4.5,dpi=300)
      
      doc <- officer::read_docx() %>% body_add_par(paste("Wage Bill vs. GDP Analysis –",ref),style="heading 1")
      doc <- add_figure_block(doc, img_path, "Note: Relationship between wage bill and income level. Last year available per country.", cs_interp)
      print(doc,target=file)
    })
  
  output$downloadWagePremiumReport <- downloadHandler(
    filename=function() paste0("Public_Sector_Wage_Premium_Report_",Sys.Date(),".docx"),
    content=function(file){
      req(input$countries_wage_premium)
      d <- public_wage_premium %>% filter(country_name %in% input$countries_wage_premium) %>% drop_na(value_percentage)
      ref <- input$countries_wage_premium[1]
      cs_interp <- build_crosssection_interp(d, ref, value_col="value_percentage", label="wage premium")
      
      p <- ggplot(d,aes(x=country_name,y=value_percentage,color=country_name==ref))+geom_point(size=4)+
        scale_color_manual(values=c("TRUE"="#B3242B","FALSE"="#003366"),guide="none")+
        labs(title="Public Sector Wage Premium by Country",x="Country",y="Wage Premium (%)")+
        theme_minimal()+theme(axis.text.x=element_text(angle=45,hjust=1))
      img_path <- tempfile(fileext=".png"); ggsave(img_path,plot=p,width=7,height=4.5,dpi=300)
      doc <- officer::read_docx() %>% body_add_par("Public Sector Wage Premium Analysis",style="heading 1")
      doc <- add_figure_block(doc, img_path, "Note: Estimated wage premium controlling for individual characteristics. Last year available.", cs_interp)
      print(doc,target=file)
    })
  
  output$downloadEducationWagePremium <- downloadHandler(
    filename=function() paste0("Public_Sector_Wage_Premium_Education_Level_",Sys.Date(),".docx"),
    content=function(file){
      req(input$selected_country)
      d <- public_wage_premium_educ %>% filter(country_name==input$selected_country) %>% drop_na(value_percentage)
      req(nrow(d)>0)
      highest <- d %>% filter(value_percentage==max(value_percentage,na.rm=TRUE)) %>% pull(indicator_name) %>% first()
      lowest  <- d %>% filter(value_percentage==min(value_percentage,na.rm=TRUE)) %>% pull(indicator_name) %>% first()
      avg_wp  <- round(mean(d$value_percentage,na.rm=TRUE),1)
      interp  <- paste0("In ",input$selected_country,", the average public sector wage premium is ",avg_wp,"%. ",
                        "Workers with ",highest," enjoy the highest premium, while workers with ",lowest," have the lowest premium compared to private formal workers.")
      
      edu_order <- c("No Education","Primary Education","Secondary Education","Tertiary Education")
      d$indicator_name <- factor(d$indicator_name, levels=intersect(edu_order,unique(d$indicator_name)))
      p <- ggplot(d,aes(x=indicator_name,y=value_percentage,fill=indicator_name))+geom_col(width=0.7)+
        scale_fill_manual(values=c("No Education"="#E69F00","Primary Education"="#56B4E9","Secondary Education"="#009E73","Tertiary Education"="#D55E00"),name=NULL)+
        labs(title="Wage Premium by Education Level",x=NULL,y="Wage Premium (%)")+theme_minimal()+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
      img_path <- tempfile(fileext=".png"); ggsave(img_path,plot=p,width=6.5,height=4.5,dpi=300)
      
      title_style <- fp_text(color="#722F37",font.size=16,bold=TRUE)
      doc <- officer::read_docx() %>% body_add_fpar(fpar(ftext(paste0("Wage Premium by Education – ",input$selected_country),prop=title_style)))
      doc <- add_figure_block(doc, img_path, "Note: Wage premium relative to private formal workers, by education level. Last year available.", interp)
      print(doc,target=file)
    })
  
  output$downloadGraphsWord <- downloadHandler(
    filename=function() paste0("Public_Sector_Employment_",Sys.Date(),".docx"),
    content=function(file){
      req(input$countries_first, input$country_second)
      ref <- input$countries_first[1]
      d1  <- public_sector_emp_temp_last %>% filter(country_name %in% input$countries_first)
      d2  <- public_sector_emp_temp %>% filter(country_name==input$country_second)
      
      cs_interp <- build_crosssection_interp(d1 %>% filter(indicator_label=="as a share of total employment"), ref,
                                             value_col="value_percentage", label="share of total employment")
      ts_interp <- build_timeseries_interp(d2, input$country_second, value_col="value_percentage",
                                           year_col="year", indicator_label_col="indicator_label",
                                           indicator_filter="as a share of total employment")
      
      title_style <- fp_text(color="#722F37",font.size=16,bold=TRUE)
      doc <- officer::read_docx() %>% body_add_fpar(fpar(ftext(paste0("Public Sector Employment – ",ref),prop=title_style))) %>%
        body_add_par("This report presents the analysis of public sector employment across selected countries and its trend over time.",style="Normal")
      
      if (nrow(d1)>0){
        p1 <- ggplot(d1,aes(x=country_name,y=value_percentage,color=indicator_label))+geom_point(size=4)+
          scale_color_manual(values=c("as a share of formal employment"="#E69F00","as a share of paid employment"="#56B4E9","as a share of total employment"="#009E73"))+
          labs(title="Public Sector Employment (Last Year Available)",x="Country",y="Value (%)")+theme_minimal()+theme(axis.text.x=element_text(angle=45,hjust=1))
        img1 <- tempfile(fileext=".png"); ggsave(img1,plot=p1,width=8,height=6,dpi=300)
        doc <- add_figure_block(doc, img1, "Note: Relative size of public sector employment for the latest available year.", cs_interp)
      }
      if (nrow(d2)>0){
        p2 <- ggplot(d2,aes(x=year,y=value_percentage,color=indicator_label,group=indicator_label))+geom_line(size=1.1)+geom_point(size=2.8)+
          scale_color_manual(values=c("as a share of formal employment"="#E69F00","as a share of paid employment"="#56B4E9","as a share of total employment"="#009E73"))+
          labs(title=paste0("Public Sector Employment Over Time — ",input$country_second),x="Year",y="Value (%)")+theme_minimal()
        img2 <- tempfile(fileext=".png"); ggsave(img2,plot=p2,width=8,height=6,dpi=300)
        doc <- add_figure_block(doc, img2, "Note: Evolution of public sector employment over time.", ts_interp)
      }
      print(doc,target=file)
    })
  
  output$downloadGraphsWordEducation <- downloadHandler(
    filename=function() paste0("Workers_Tertiary_Education_Report_",Sys.Date(),".docx"),
    content=function(file){
      req(input$selected_countries)
      d   <- tertiary_education %>% filter(country_name %in% input$selected_countries); req(nrow(d)>0)
      ref <- input$selected_countries[1]
      d_pub <- d %>% filter(indicator_name=="as a share of public paid employees")
      d_pri <- d %>% filter(indicator_name=="as a share of private paid employees")
      cs_pub <- build_crosssection_interp(d_pub, ref, value_col="value_percentage", label="share of public paid employees with tertiary education")
      cs_pri <- build_crosssection_interp(d_pri, ref, value_col="value_percentage", label="share of private paid employees with tertiary education")
      interp <- paste0(cs_pub, " ", cs_pri)
      
      p <- ggplot(d,aes(x=country_name,y=value_percentage,fill=indicator_name))+geom_col(position=position_dodge(0.8),width=0.7)+
        scale_fill_manual(values=c("as a share of private paid employees"="#0072B2","as a share of public paid employees"="#D55E00"))+
        labs(title="Tertiary Education by Sector",x="Country",y="Tertiary Education (%)")+theme_minimal()
      img_path <- tempfile(fileext=".png"); ggsave(img_path,plot=p,width=7,height=4.5,dpi=300)
      
      title_style <- fp_text(color="#722F37",font.size=16,bold=TRUE)
      doc <- officer::read_docx() %>% body_add_fpar(fpar(ftext(paste0("Tertiary Education Analysis – ",ref),prop=title_style)))
      doc <- add_figure_block(doc, img_path, "Note: Proportion with tertiary education in public and private sectors. Last year available.", interp)
      print(doc,target=file)
    })
  
  output$downloadGraphsWordGender <- downloadHandler(
    filename=function() paste0("Female_Share_of_Employment_",Sys.Date(),".docx"),
    content=function(file){
      req(input$countries_gender, input$country_gender)
      ref <- input$countries_gender[1]
      d1  <- gender_workforce %>% filter(country_name %in% input$countries_gender) %>%
        group_by(country_name,indicator_name) %>% arrange(year,.by_group=TRUE) %>% slice_tail(n=1) %>% ungroup()
      d2  <- gender_workforce %>% filter(country_name==input$country_gender)
      
      cs_pub <- build_crosssection_interp(d1 %>% filter(indicator_name=="as a share of public paid employees"), ref,
                                          value_col="value_percentage", label="female share in the public sector")
      ts_pub <- build_timeseries_interp(d2, input$country_gender, value_col="value_percentage",
                                        year_col="year", indicator_label_col="indicator_name",
                                        indicator_filter="as a share of public paid employees")
      
      title_style <- fp_text(color="#722F37",font.size=16,bold=TRUE)
      doc <- officer::read_docx() %>% body_add_fpar(fpar(ftext(paste0("Female Share of Employment — ",ref),prop=title_style)))
      
      if (nrow(d1)>0){
        p1 <- ggplot(d1,aes(x=country_name,y=value_percentage,fill=indicator_name))+geom_col(position=position_dodge(0.8),width=0.7)+
          scale_fill_manual(values=c("as a share of private paid employees"="#E69F00","as a share of public paid employees"="#56B4E9"),name="Sector")+
          labs(title="Female Employment by Sector (Last Year Available)",x="Country",y="Employment (%)")+theme_minimal()+theme(axis.text.x=element_text(angle=45,hjust=1))
        img1 <- tempfile(fileext=".png"); ggsave(img1,plot=p1,width=8,height=6,dpi=300)
        doc <- add_figure_block(doc, img1, "Note: Female share of employment in public and private sectors. Last year available.", cs_pub)
      }
      if (nrow(d2)>0){
        p2 <- ggplot(d2,aes(x=year,y=value_percentage,color=indicator_name,group=indicator_name))+geom_line(size=1.1)+geom_point(size=2.8)+
          scale_color_manual(values=c("as a share of private paid employees"="#E69F00","as a share of public paid employees"="#56B4E9"),name="Sector")+
          labs(title=paste0("Female Employment Over Time — ",input$country_gender),x="Year",y="Employment (%)")+theme_minimal()
        img2 <- tempfile(fileext=".png"); ggsave(img2,plot=p2,width=8,height=6,dpi=300)
        doc <- add_figure_block(doc, img2, "Note: Evolution of female employment in public and private sectors over time.", ts_pub)
      }
      print(doc,target=file)
    })
  
  output$downloadGraphswagepremiumbygender <- downloadHandler(
    filename=function() paste0("Public_Sector_Wage_Premium_Gender_",Sys.Date(),".docx"),
    content=function(file){
      ref <- if (!is.null(input$countries_first)&&length(input$countries_first)>0) input$countries_first[1] else "Unknown"
      d1  <- gender_wage_premium_last %>% filter(country_name %in% input$countries_first)
      title_style <- fp_text(color="#722F37",font.size=16,bold=TRUE)
      doc <- officer::read_docx() %>% body_add_fpar(fpar(ftext(paste0("Public Sector Wage Premium by Gender — ",ref),prop=title_style)))
      
      if (nrow(d1)>0){
        cs_m <- build_crosssection_interp(d1 %>% filter(indicator_label=="Male"),   ref, value_col="value_percentage", label="male wage premium")
        cs_f <- build_crosssection_interp(d1 %>% filter(indicator_label=="Female"), ref, value_col="value_percentage", label="female wage premium")
        interp1 <- paste0("Male employees: ", cs_m, " Female employees: ", cs_f)
        p1 <- ggplot(d1,aes(x=country_name,y=value_percentage,color=indicator_label))+geom_point(size=4)+
          scale_color_manual(values=c("Male"="#E69F00","Female"="#56B4E9"))+
          labs(title="Public Sector Wage Premium by Gender (Last Year Available)",x="Country",y="Wage Premium (%)",color="Gender")+theme_minimal()
        img1 <- tempfile(fileext=".png"); ggsave(img1,plot=p1,width=8,height=6)
        doc <- add_figure_block(doc, img1, "Note: Public sector wage premium by gender. Last year available.", interp1)
      }
      
      if (isTruthy(input$country_second)){
        d2 <- gender_wage_premium %>% filter(country_name==input$country_second)
        if (nrow(d2)>0){
          ts_m <- build_timeseries_interp(d2, input$country_second, value_col="value_percentage",
                                          year_col="year", indicator_label_col="indicator_label", indicator_filter="Male")
          ts_f <- build_timeseries_interp(d2, input$country_second, value_col="value_percentage",
                                          year_col="year", indicator_label_col="indicator_label", indicator_filter="Female")
          interp2 <- paste0("Male wage premium trend: ", ts_m, " Female wage premium trend: ", ts_f)
          p2 <- ggplot(d2,aes(x=year,y=value_percentage,color=indicator_label))+geom_line(size=1.2)+geom_point(size=3)+
            scale_color_manual(values=c("Male"="#E69F00","Female"="#56B4E9"))+
            labs(title="Public Sector Wage Premium by Gender Over Time",x="Year",y="Wage Premium (%)",color="Gender")+theme_minimal()
          img2 <- tempfile(fileext=".png"); ggsave(img2,plot=p2,width=8,height=6)
          doc <- add_figure_block(doc, img2, "Note: Wage premium by gender over time vs. private sector.", interp2)
        }
      }
      print(doc,target=file)
    })
  
  output$downloadGraphsWordfemale <- downloadHandler(
    filename=function() paste0("Females_Occupation_Groups_Analysis_",Sys.Date(),".docx"),
    content=function(file){
      req(input$selected_countries)
      ref <- input$selected_countries[1]
      d   <- gender_leadership %>% filter(country_name %in% input$selected_countries) %>%
        mutate(indicator_label=factor(indicator_label,levels=c("Clerks-Private","Clerks-Public","Managers-Private","Managers-Public")))
      req(nrow(d)>0)
      
      cs_mgr_pub <- build_crosssection_interp(d %>% filter(indicator_label=="Managers-Public"),  ref, value_col="value_percentage", label="female share among public managers")
      cs_mgr_pri <- build_crosssection_interp(d %>% filter(indicator_label=="Managers-Private"), ref, value_col="value_percentage", label="female share among private managers")
      cs_clk_pub <- build_crosssection_interp(d %>% filter(indicator_label=="Clerks-Public"),    ref, value_col="value_percentage", label="female share among public clerks")
      interp <- paste0("Public Managers: ", cs_mgr_pub, "\nPrivate Managers: ", cs_mgr_pri, "\nPublic Clerks: ", cs_clk_pub)
      
      cols <- c("Clerks-Private"="#9ECAE1","Clerks-Public"="#08519C","Managers-Private"="#FDAE6B","Managers-Public"="#E6550D")
      p <- ggplot(d,aes(x=country_name,y=value_percentage,fill=indicator_label))+geom_col(position=position_dodge(0.8),width=0.7)+
        scale_fill_manual(values=cols,drop=FALSE,name="Group–Sector")+
        labs(title="Females by Occupational Group and Sector",x="Country",y="Female Share (%)")+theme_minimal()+theme(axis.text.x=element_text(angle=45,hjust=1))
      img_path <- tempfile(fileext=".png"); ggsave(img_path,plot=p,width=8,height=6,dpi=300)
      
      title_style <- fp_text(color="#722F37",font.size=16,bold=TRUE)
      doc <- officer::read_docx() %>% body_add_fpar(fpar(ftext(paste0("Females by Occupational Group and Sector — ",ref),prop=title_style)))
      doc <- add_figure_block(doc, img_path, "Note: Female share in Managers/Clerks in public and private sectors. Last year available.", interp)
      print(doc,target=file)
    })
  
  output$downloadGenderWagePremium <- downloadHandler(
    filename=function() paste0("Gender_Wage_Premium_Report_",Sys.Date(),".docx"),
    content=function(file){
      req(input$selected_countries)
      ref <- input$selected_countries[1]
      d   <- gender_wage_premiumpublic %>% filter(country_name %in% input$selected_countries, indicator_label %in% c("Public Administration","Education","Health","Other"))
      req(nrow(d)>0)
      
      cs_adm <- build_crosssection_interp(d %>% filter(indicator_label=="Public Administration"), ref, value_col="value_percentage", label="gender wage premium in Public Administration")
      cs_edu <- build_crosssection_interp(d %>% filter(indicator_label=="Education"),             ref, value_col="value_percentage", label="gender wage premium in Education")
      cs_hlth<- build_crosssection_interp(d %>% filter(indicator_label=="Health"),                ref, value_col="value_percentage", label="gender wage premium in Health")
      interp <- paste0("Public Administration: ", cs_adm, " Education: ", cs_edu, " Health: ", cs_hlth)
      
      p <- ggplot(d,aes(x=country_name,y=value_percentage,fill=indicator_label))+geom_col(position=position_dodge(0.8),width=0.7)+
        scale_fill_viridis_d(option="D",name="Industry")+
        labs(title="Gender Wage Premium in the Public Sector by Industry",x="Country",y="Wage Premium (%)")+theme_minimal()+theme(axis.text.x=element_text(angle=45,hjust=1))
      img_path <- tempfile(fileext=".png"); ggsave(img_path,plot=p,width=8,height=6,dpi=300)
      
      title_style <- fp_text(color="#722F37",font.size=16,bold=TRUE)
      doc <- officer::read_docx() %>% body_add_fpar(fpar(ftext(paste0("Gender Wage Premium by Industry — ",ref),prop=title_style)))
      doc <- add_figure_block(doc, img_path, "Note: Gender wage premium in public sector by industry. Last year available.", interp)
      print(doc,target=file)
    })
  
  output$downloadPayCompressionDoc <- downloadHandler(
    filename=function() paste0("Pay_Compression_Ratios_Report_",Sys.Date(),".docx"),
    content=function(file){
      req(input$countries_first)
      d <- pay_compression_wide %>% filter(country_name %in% input$countries_first) %>%
        mutate(Public_Sector=as.numeric(unlist(Public_Sector)),Private_Sector=as.numeric(unlist(Private_Sector))) %>% drop_na(Public_Sector,Private_Sector)
      req(nrow(d)>0)
      ref <- input$countries_first[1]
      cs_pub <- build_crosssection_interp(d, ref, value_col="Public_Sector",  label="public sector pay compression ratio")
      cs_pri <- build_crosssection_interp(d, ref, value_col="Private_Sector", label="private sector pay compression ratio")
      interp <- paste0("Public sector compression: ", cs_pub, " Private sector compression: ", cs_pri)
      
      p <- ggplot(d,aes(x=Private_Sector,y=Public_Sector,label=country_name))+geom_point(size=3)+ggrepel::geom_text_repel(size=3,color="black")+
        geom_smooth(method="lm",se=FALSE,color="gray50",linetype="dashed")+
        labs(title="Pay Compression: Public vs. Private Sector",x="Private Sector (P90/P10)",y="Public Sector (P90/P10)")+theme_minimal()
      img_path <- tempfile(fileext=".png"); ggsave(img_path,plot=p,width=7,height=4.5,dpi=300)
      
      title_style <- fp_text(color="#722F37",font.size=16,bold=TRUE)
      doc <- officer::read_docx() %>% body_add_fpar(fpar(ftext(paste0("Pay Compression Ratios — ",ref),prop=title_style)))
      doc <- add_figure_block(doc, img_path, "Note: Pay compression = P90/P10 income ratio. Higher values indicate wider wage dispersion. Last year available.", interp)
      print(doc,target=file)
    })
  
  output$downloadGraphsemploymentdist <- downloadHandler(
    filename=function() paste0("Employment_Distribution_Analysis_",Sys.Date(),".docx"),
    content=function(file){
      ref <- if (!is.null(input$countries_workforce)&&length(input$countries_workforce)>0) input$countries_workforce[[1]] else "Selected Countries"
      fgd <- filtered_workforce_data() %>% filter(country_name %in% input$countries_workforce)
      cs_interp <- if (nrow(fgd)>0) {
        build_crosssection_interp(fgd %>% filter(indicator_name=="Public Administration"), ref,
                                  value_col="value_percentage", label="share of Public Administration in public employment")
      } else ""
      
      title_style <- fp_text(color="#722F37",font.size=16,bold=TRUE)
      doc <- officer::read_docx() %>% body_add_fpar(fpar(ftext(paste0("Employment Distribution Analysis – ",ref),prop=title_style)))
      if (nrow(fgd)>0){
        p1 <- ggplot(fgd,aes(x=country_name,y=value_percentage,fill=indicator_name))+geom_bar(stat="identity",position="stack")+
          scale_fill_viridis_d(option="D")+labs(title="Employment distribution by country",x="Country",y="Employment distribution (%)")+theme_minimal()
        img1 <- tempfile(fileext=".png"); ggsave(img1,plot=p1,width=6,height=4,dpi=300)
        doc <- add_figure_block(doc, img1, "Note: Distribution of public sector employment across industries. Last year available.", cs_interp)
      }
      print(doc,target=file)
    })
  
  # ============================================================
  # SECTION GENERATORS FOR FULL/CUSTOM REPORT
  # ============================================================
  generate_intro_section <- function(doc, selected_countries) {
    ref <- if (!is.null(selected_countries)&&length(selected_countries)>0&&!is.na(selected_countries[1])) selected_countries[1] else "Unknown Country"
    region <- tryCatch(countrycode(ref,origin="country.name",destination="region"),error=function(e) "its respective region")
    if (is.na(region)) region <- "its respective region"
    title_style    <- fp_text(color="#722F37",font.size=20,bold=TRUE)
    subtitle_style <- fp_text(color="black",  font.size=16,bold=TRUE)
    doc %>%
      body_add_fpar(fpar(ftext(ref,prop=title_style))) %>%
      body_add_fpar(fpar(ftext("Wage Bill and Public Employment Analysis",prop=subtitle_style))) %>%
      body_add_par(paste0(
        "This note presents evidence on public sector employment and compensation practices in ",ref,
        " using the Worldwide Bureaucracy Indicators (WWBI). The primary data source is the Labor Force Survey, conducted by the National Statistics Office. ",
        "For international comparisons, peer countries from ",region," are included."
      ),style="Normal") %>%
      body_add_par(paste0(
        "The public sector is typically a major source of employment in most countries. ",
        "The provision of basic services such as education, health, citizen security, and justice, among others, ",
        "makes it a central actor in labor markets, with significant impacts on the aggregate results of employment, ",
        "wages, informality, and other economic variables. Moreover, public employment is an indicator of the state's ",
        "participation in the entire economy, which has implications for macroeconomic balances, allocation efficiency, ",
        "and income distribution. Thus, this analysis comprehensively documents the size of public employment, ",
        "its changes over time, and the characteristics of its workforce."
      ),style="Normal") %>%
      body_add_par(paste0(
        "This work documents and analyzes the size, composition, and changes in the levels of employment and wages of ",
        ref,"'s public employees compared to the private sector and how these metrics compare to regional peers."
      ),style="Normal")
  }
  
  # ── Helper: add section heading ─────────────────────────────────────────────
  add_section_heading <- function(doc, title) {
    section_style <- fp_text(color="#003366",font.size=14,bold=TRUE)
    doc %>% body_add_fpar(fpar(ftext(title,prop=section_style)))
  }
  
  # ── Individual section generators ───────────────────────────────────────────
  gen_wagebill_section <- function(doc, sel_ctries) {
    ref <- sel_ctries[1]
    first_region <- tryCatch(countrycode(ref,origin="country.name",destination="region"),error=function(e) "the region")
    if (is.na(first_region)) first_region <- "the region"
    doc <- add_section_heading(doc,"Macro-Fundamentals of the Public Sector")
    
    # GDP graph + interpretation
    d_gdp <- wage_bill_gdp %>% filter(country_name %in% sel_ctries)
    if (nrow(d_gdp)>0) {
      ref_gdp <- d_gdp %>% filter(country_name==ref) %>% arrange(year) %>% filter(!is.na(value))
      v_first_gdp <- round(ref_gdp$value[1], 0);            y_first_gdp <- ref_gdp$year[1]
      v_last_gdp  <- round(ref_gdp$value[nrow(ref_gdp)], 0); y_last_gdp  <- ref_gdp$year[nrow(ref_gdp)]
      direction_gdp <- if (v_last_gdp > v_first_gdp) "an increase" else if (v_last_gdp < v_first_gdp) "a decrease" else "no significant change"
      
      # Compare ref spending vs regional average
      reg_avg_gdp   <- round(mean(d_gdp$value[d_gdp$country_name!=ref], na.rm=TRUE), 0)
      ref_last_gdp  <- round(mean(d_gdp$value[d_gdp$country_name==ref], na.rm=TRUE), 0)
      spending_pat  <- if (!is.na(ref_last_gdp) && !is.na(reg_avg_gdp)) {
        if (ref_last_gdp > reg_avg_gdp*1.15) "more than expected" else if (ref_last_gdp < reg_avg_gdp*0.85) "less than expected" else "roughly as expected"
      } else "uncertain"
      
      interp_gdp <- paste0(
        "Figure 1.1 illustrates the Wage bill as a percentage of GDP for the selected countries, showing a relationship between ",
        "a country's level of economic development and the size of its public sector in the ", first_region, " region. ",
        ref, " spends ", spending_pat, " on its public sector wage bill compared to peers.\n",
        "For ", ref, ", the wage bill as a percentage of GDP shows ", direction_gdp, " from ",
        v_first_gdp, "% in ", y_first_gdp, " to ", v_last_gdp, "% in ", y_last_gdp, "."
      )
      g_gdp <- ggplot(d_gdp,aes(x=year,y=value,color=country_name))+geom_line(size=1.2)+geom_point(size=3)+
        labs(title="Wage Bill as % of GDP Over Time",x="Year",y="Wage Bill (% of GDP)",color="Country")+theme_minimal()
      img_gdp <- tempfile(fileext=".png"); ggsave(img_gdp,plot=g_gdp,width=8,height=6,dpi=300)
      miss_gdp <- setdiff(sel_ctries, unique(d_gdp$country_name[!is.na(d_gdp$value)]))
      doc <- doc %>% body_add_par("Wage Bill as % of GDP Over Time",style="heading 2")
      doc <- add_figure_block(doc, img_gdp, "Note: Wage bill as a percentage of GDP over time.", interp_gdp, missing_vec=miss_gdp)
    }
    
    # Public Expenditure graph + interpretation
    d_exp <- wage_bill_publicexp %>% filter(country_name %in% sel_ctries)
    if (nrow(d_exp)>0) {
      ref_exp <- d_exp %>% filter(country_name==ref) %>% arrange(year) %>% filter(!is.na(value))
      v_first_exp <- round(ref_exp$value[1], 0);             y_first_exp <- ref_exp$year[1]
      v_last_exp  <- round(ref_exp$value[nrow(ref_exp)], 0); y_last_exp  <- ref_exp$year[nrow(ref_exp)]
      direction_exp <- if (v_last_exp > v_first_exp) "increased" else if (v_last_exp < v_first_exp) "decreased" else "remained stable"
      
      # Volatility
      country_vol <- sd(ref_exp$value, na.rm=TRUE)
      reg_vol     <- d_exp %>% filter(country_name!=ref) %>% group_by(country_name) %>% summarise(v=sd(value,na.rm=TRUE),.groups="drop") %>% summarise(m=mean(v,na.rm=TRUE)) %>% pull(m)
      stability   <- if (!is.na(country_vol) && !is.na(reg_vol)) {
        if (country_vol < reg_vol*0.8) "more stable" else if (country_vol > reg_vol*1.2) "more volatile" else "similarly stable"
      } else "stable"
      
      # Others comparison at last year
      others_exp_last <- d_exp %>% filter(country_name!=ref) %>% group_by(country_name) %>% slice_max(order_by=year,n=1) %>% ungroup() %>% drop_na(value)
      other_comp <- if (nrow(others_exp_last)>0) {
        max_o <- others_exp_last %>% slice_max(value,n=1); min_o <- others_exp_last %>% slice_min(value,n=1)
        n_o   <- nrow(others_exp_last)
        mid_t <- ""
        if (n_o>=3) {
          mid_o <- others_exp_last %>% arrange(value) %>% slice(ceiling(n_o/2))
          if (mid_o$country_name!=max_o$country_name && mid_o$country_name!=min_o$country_name)
            mid_t <- paste0(mid_o$country_name," at ",round(mid_o$value,0),"%, ")
        }
        paste0("Among the other selected countries, the highest wage bill share is in ",max_o$country_name,
               " at ",round(max_o$value,0),"%, ",mid_t,"and the lowest in ",min_o$country_name," at ",round(min_o$value,0),"%. ")
      } else ""
      
      interp_exp <- paste0(
        "The wage bill as a share of public expenditures in ", ref, " was ", v_first_exp, "% in ", y_first_exp,
        " and has ", direction_exp, " to ", v_last_exp, "% in ", y_last_exp, ". ",
        "The public sector wage bill in ", ref, " has exhibited ", stability, " compared to regional peers. ",
        other_comp
      )
      g_exp <- ggplot(d_exp,aes(x=year,y=value,color=country_name))+geom_line(size=1.2)+geom_point(size=3)+
        labs(title="Wage Bill as % of Public Expenditure Over Time",x="Year",y="Wage Bill (% of Public Expenditure)",color="Country")+theme_minimal()
      img_exp <- tempfile(fileext=".png"); ggsave(img_exp,plot=g_exp,width=8,height=6,dpi=300)
      miss_exp <- setdiff(sel_ctries, unique(d_exp$country_name[!is.na(d_exp$value)]))
      doc <- doc %>% body_add_par("Wage Bill as % of Public Expenditure Over Time",style="heading 2")
      doc <- add_figure_block(doc, img_exp, "Note: Wage bill as a percentage of public expenditure over time.", interp_exp, missing_vec=miss_exp)
    }
    
    # GDP scatter
    d_sc <- merged_data %>% filter(country_name %in% sel_ctries)
    if (nrow(d_sc)>0) {
      avg_wb  <- round(mean(d_sc$indicator_value, na.rm=TRUE), 0)
      avg_gdp <- round(exp(mean(d_sc$log_gdp, na.rm=TRUE)), 0)
      ref_wb  <- d_sc %>% filter(country_name==ref) %>% pull(indicator_value) %>% first() %>% round(0)
      ref_gdp <- d_sc %>% filter(country_name==ref) %>% pull(log_gdp) %>% first()
      ref_gdp_val <- if (!is.na(ref_gdp)) round(exp(ref_gdp), 0) else NA
      
      interp_sc <- paste0(
        "Figure 1.3 illustrates the relationship between the wage bill as a percentage of public expenditure ",
        "and GDP per capita across selected countries. The selected countries have an average wage bill of ",
        avg_wb, "%, with a GDP per capita of $", format(avg_gdp, big.mark=","), ".\n",
        "For ", ref, ", the wage bill represents ", ref_wb, "% of public expenditure, with a GDP per capita of $",
        format(ref_gdp_val, big.mark=","), "."
      )
      p_sc <- ggplot(d_sc,aes(x=log_gdp,y=indicator_value))+
        geom_point(aes(color=country_name==ref),size=3,show.legend=FALSE)+
        ggrepel::geom_text_repel(aes(label=country_name),color="black",size=3,max.overlaps=30)+
        geom_smooth(method="lm",se=FALSE,color="gray50",linetype="dashed")+
        scale_color_manual(values=c("TRUE"="#B3242B","FALSE"="#003366"),guide="none")+
        labs(title="Wage Bill vs. Log(GDP per Capita)",x="Log(GDP per Capita, 2015)",y="Wage Bill (% of Public Expenditure)")+theme_minimal()
      img_sc <- tempfile(fileext=".png"); ggsave(img_sc,plot=p_sc,width=7,height=4.5,dpi=300)
      doc <- doc %>% body_add_par("Wage bill (% of public expenditure) and GDP per capita in the region",style="heading 2") %>%
        body_add_par("This note presents evidence on public sector employment and compensation practices in relation to GDP per capita.",style="Normal")
      doc <- add_figure_block(doc, img_sc,
                              "Note: This graph shows the relationship between the wage bill (expressed as a share of total expenditure) and the income level of countries. It offers a clearer understanding of whether wage bill spending is consistent with countries' respective income levels.",
                              interp_sc)
    }
    return(doc)
  }
  
  gen_public_emp_section <- function(doc, sel_ctries) {
    ref <- sel_ctries[1]
    doc <- add_section_heading(doc,"Size and Characteristics of the Public Sector")
    d   <- public_sector_emp_temp_last %>% filter(country_name %in% sel_ctries)
    if (nrow(d)==0) return(doc)
    
    # Build interpretation exactly like reference document
    get_val <- function(df, country, indicator) {
      v <- df %>% filter(country_name==country, indicator_label==indicator) %>% pull(value_percentage) %>% first()
      ifelse(is.na(v), NA, round(v, 0))
    }
    get_high <- function(df, indicator) df %>% filter(indicator_label==indicator) %>% slice_max(value_percentage,n=1) %>% slice(1)
    get_low  <- function(df, indicator) df %>% filter(indicator_label==indicator) %>% slice_min(value_percentage,n=1) %>% slice(1)
    
    f_val  <- get_val(d, ref, "as a share of formal employment")
    p_val  <- get_val(d, ref, "as a share of paid employment")
    t_val  <- get_val(d, ref, "as a share of total employment")
    fh <- get_high(d,"as a share of formal employment"); fl <- get_low(d,"as a share of formal employment")
    ph <- get_high(d,"as a share of paid employment");   pl <- get_low(d,"as a share of paid employment")
    th <- get_high(d,"as a share of total employment");  tl <- get_low(d,"as a share of total employment")
    avg_others_t <- d %>% filter(country_name!=ref, indicator_label=="as a share of total employment") %>% summarise(a=mean(value_percentage,na.rm=TRUE)) %>% pull(a)
    
    position_text <- function(val, high_val, low_val, avg_o) {
      if (is.na(val)) return("Data not available.")
      if (val >= high_val) return("This is the highest employment rate among the selected countries.")
      if (val <= low_val)  return("This is the lowest employment rate among the selected countries.")
      return("This falls within the range observed across the selected countries.")
    }
    
    interp <- paste0(
      "This graph compares public sector employment across selected countries. ",
      "For employment as a share of formal employment, the highest level is in ",fh$country_name," at ",round(fh$value_percentage,0),
      "%, while the lowest is in ",fl$country_name," at ",round(fl$value_percentage,0),"%. ",
      "For employment as a share of paid employment, the highest level is in ",ph$country_name," at ",round(ph$value_percentage,0),
      "%, while the lowest is in ",pl$country_name," at ",round(pl$value_percentage,0),"%. ",
      "For employment as a share of total employment, the highest level is in ",th$country_name," at ",round(th$value_percentage,0),
      "%, while the lowest is in ",tl$country_name," at ",round(tl$value_percentage,0),"%. \n",
      "In ",ref,", public sector employment as a share of formal employment is ",ifelse(is.na(f_val),"N/A",f_val),"%. ",
      position_text(f_val, fh$value_percentage, fl$value_percentage, NA),"\n",
      "As a share of paid employment, it is ",ifelse(is.na(p_val),"N/A",p_val),"%. ",
      position_text(p_val, ph$value_percentage, pl$value_percentage, NA),"\n",
      "As a share of total employment, it is ",ifelse(is.na(t_val),"N/A",t_val),"%. ",
      position_text(t_val, th$value_percentage, tl$value_percentage, avg_others_t)
    )
    
    p <- ggplot(d,aes(x=country_name,y=value_percentage,color=indicator_label))+geom_point(size=4)+
      scale_color_manual(values=c("as a share of formal employment"="#E69F00","as a share of paid employment"="#56B4E9","as a share of total employment"="#009E73"))+
      labs(title="Public Sector Employment (Last Year Available)",x="Country",y="Employment (%)")+
      theme_minimal()+theme(axis.text.x=element_text(angle=45,hjust=1))
    img <- tempfile(fileext=".png"); ggsave(img,plot=p,width=8,height=6)
    miss_emp <- setdiff(sel_ctries, unique(d$country_name[!is.na(d$value_percentage)]))
    doc <- doc %>% body_add_par("Public Sector Employment Analysis",style="heading 2") %>%
      body_add_par("This section presents the analysis of public sector employment across selected countries and its trend over time.",style="Normal") %>%
      body_add_par("Public Sector Employment - Last Year Available",style="heading 3")
    doc <- add_figure_block(doc, img, "Note: Relative size of public sector employment in the labor market for the latest available year.", interp, missing_vec=miss_emp)
    return(doc)
  }
  
  gen_tertiary_section <- function(doc, sel_ctries) {
    ref <- sel_ctries[1]
    d   <- tertiary_education %>% filter(country_name %in% sel_ctries)
    if (nrow(d)==0) return(doc)
    d_pub <- d %>% filter(indicator_name=="as a share of public paid employees")
    d_pri <- d %>% filter(indicator_name=="as a share of private paid employees")
    avg_pub  <- round(mean(d_pub$value_percentage, na.rm=TRUE), 1)
    avg_pri  <- round(mean(d_pri$value_percentage, na.rm=TRUE), 1)
    high_pub <- d_pub %>% slice_max(value_percentage,n=1) %>% slice(1)
    low_pub  <- d_pub %>% slice_min(value_percentage,n=1) %>% slice(1)
    high_pri <- d_pri %>% slice_max(value_percentage,n=1) %>% slice(1)
    low_pri  <- d_pri %>% slice_min(value_percentage,n=1) %>% slice(1)
    
    interp <- paste0(
      "This graph compares tertiary education attainment among employees in the public and private sectors across selected countries. ",
      "On average, ", avg_pub, "% of public sector employees have completed tertiary education, while in the private sector, the share is ", avg_pri, "%. ",
      "The country with the highest share of tertiary-educated public sector employees is ", high_pub$country_name,
      ", whereas ", low_pub$country_name, " has the lowest proportion. ",
      "In the private sector, ", high_pri$country_name, " has the highest tertiary education level among employees, while ", low_pri$country_name, " has the lowest."
    )
    p <- ggplot(d,aes(x=country_name,y=value_percentage,fill=indicator_name))+geom_bar(stat="identity",position="dodge")+
      scale_fill_manual(values=c("as a share of private paid employees"="#0072B2","as a share of public paid employees"="#D55E00"))+
      labs(title="Workers with Tertiary Education",x="Country",y="Tertiary Education (%)",fill="Sector")+
      theme_minimal()+theme(axis.text.x=element_text(angle=45,hjust=1))
    img <- tempfile(fileext=".png"); ggsave(img,plot=p,width=8,height=6)
    miss_tert <- setdiff(sel_ctries, unique(d$country_name[!is.na(d$value_percentage)]))
    doc <- doc %>% body_add_par("Tertiary Education Analysis",style="heading 2") %>%
      body_add_par("This section presents an analysis of tertiary education among public and private sector employees across selected countries.",style="Normal") %>%
      body_add_par("This graph shows the proportion of individuals with tertiary education working in public and private sector employment.",style="Normal")
    doc <- add_figure_block(doc, img, "Note: Proportion with tertiary education in public and private sectors. Last year available.", interp, missing_vec=miss_tert)
    return(doc)
  }
  
  gen_wagepremium_section <- function(doc, sel_ctries) {
    ref <- sel_ctries[1]
    doc <- add_section_heading(doc,"Competitiveness of Public Sector Wages")
    doc <- doc %>% body_add_par(
      "Public sector compensation should theoretically be designed with an awareness of its influence on the broader labor market. According to the theory of \"compensating wage differentials,\" a job should pay more (or less) depending on its non-wage characteristics that are undesirable (or desirable).",
      style="Normal")
    d <- public_wage_premium %>% filter(country_name %in% sel_ctries) %>% drop_na(value_percentage)
    if (nrow(d)==0) return(doc)
    avg_all    <- round(mean(d$value_percentage, na.rm=TRUE), 1)
    high_c     <- d %>% slice_max(value_percentage,n=1) %>% slice(1)
    low_c      <- d %>% slice_min(value_percentage,n=1) %>% slice(1)
    ref_val    <- d %>% filter(country_name==ref) %>% pull(value_percentage) %>% first() %>% round(1)
    avg_others <- d %>% filter(country_name!=ref) %>% summarise(a=round(mean(value_percentage,na.rm=TRUE),1)) %>% pull(a)
    pos_stmt   <- if (!is.na(ref_val) && !is.na(avg_others)) {
      if (ref_val > avg_others) paste0("This is higher than the average of ",avg_others,"% across the other selected countries.")
      else paste0("This is lower than the average of ",avg_others,"% across the other selected countries.")
    } else ""
    
    interp <- paste0(
      "This graph compares public sector wage premiums across selected countries. ",
      "On average, public sector employees earn ", avg_all, "% more than private sector employees. ",
      "The country with the highest wage premium is ", high_c$country_name, ", while ", low_c$country_name, " has the lowest.\n",
      "In ", ref, ", the wage premium is ", ifelse(is.na(ref_val),"N/A",ref_val), "%. ", pos_stmt
    )
    p <- ggplot(d,aes(x=country_name,y=value_percentage,color=country_name==ref))+geom_point(size=4)+
      scale_color_manual(values=c("TRUE"="#B3242B","FALSE"="#003366"),guide="none")+
      labs(title="Public Sector Wage Premium by Country",x="Country",y="Wage Premium (%)")+
      theme_minimal()+theme(axis.text.x=element_text(angle=45,hjust=1))
    img <- tempfile(fileext=".png"); ggsave(img,plot=p,width=8,height=6)
    miss_wp <- setdiff(sel_ctries, unique(d$country_name[!is.na(d$value_percentage)]))
    doc <- doc %>% body_add_par("Public Sector Wage Premium Analysis",style="heading 2") %>%
      body_add_par("This section presents an analysis of public sector wage premiums compared to private sector employees across selected countries.",style="Normal") %>%
      body_add_par("This graph shows the wage premium in the public sector relative to private sector employees across selected countries.",style="Normal")
    doc <- add_figure_block(doc, img, "Note: Estimated wage premium controlling for gender, education, tenure and location. Last year available.", interp, missing_vec=miss_wp)
    return(doc)
  }
  
  gen_wagepremium_educ_section <- function(doc, sel_ctries) {
    ref <- sel_ctries[1]
    d   <- public_wage_premium_educ %>% filter(country_name %in% sel_ctries) %>% drop_na(value_percentage)
    if (nrow(d)==0) return(doc)
    d_ref       <- d %>% filter(country_name==ref)
    highest_edu <- d_ref %>% slice_max(value_percentage,n=1) %>% pull(indicator_name) %>% first()
    lowest_edu  <- d_ref %>% slice_min(value_percentage,n=1) %>% pull(indicator_name) %>% first()
    ref_avg     <- round(mean(d_ref$value_percentage, na.rm=TRUE), 0)
    avg_others  <- round(mean(d$value_percentage[d$country_name!=ref], na.rm=TRUE), 1)
    pos_stmt    <- if (!is.na(ref_avg) && !is.na(avg_others)) {
      if (ref_avg > avg_others) paste0("This is higher than the average of ",avg_others,"% across the other selected countries.")
      else paste0("This is lower than the average of ",avg_others,"% across the other selected countries.")
    } else ""
    
    interp <- paste0(
      "This graph illustrates public sector wage premiums by education level in ",ref,
      ", comparing earnings with private sector formal workers. ",
      "On average, the public sector wage premium in ",ref," is ",ref_avg,"%. ",
      "The highest wage premium is observed for those with ",highest_edu,
      ", while the lowest is for those with ",lowest_edu,". ",pos_stmt
    )
    edu_order <- c("No Education","Primary Education","Secondary Education","Tertiary Education")
    d$indicator_name <- factor(d$indicator_name,levels=intersect(edu_order,unique(d$indicator_name)))
    p <- ggplot(d,aes(x=indicator_name,y=value_percentage,fill=indicator_name))+geom_bar(stat="identity",position="dodge")+
      scale_fill_manual(values=c("No Education"="#E69F00","Primary Education"="#56B4E9","Secondary Education"="#009E73","Tertiary Education"="#D55E00"))+
      labs(title="Wage Premium by Education Level",x="Education Level",y="Wage Premium (%)")+theme_minimal()
    img <- tempfile(fileext=".png"); ggsave(img,plot=p,width=8,height=6)
    miss_educ <- setdiff(sel_ctries, unique(d$country_name[!is.na(d$value_percentage)]))
    doc <- doc %>% body_add_par("Public Sector Wage Premium by Education Level",style="heading 2") %>%
      body_add_par(paste0("This section presents an analysis of public sector wage premiums based on different education levels for ",ref,". The comparison is made against private sector formal workers."),style="Normal") %>%
      body_add_par("This graph shows the public sector wage premium by education level, comparing earnings with private sector formal workers.",style="Normal")
    doc <- add_figure_block(doc, img, "Note: Wage premium relative to private formal workers by education level. Last year available.", interp, missing_vec=miss_educ)
    return(doc)
  }
  
  gen_paycompression_section <- function(doc, sel_ctries) {
    ref <- sel_ctries[1]
    d   <- tryCatch(
      pay_compression_wide %>% filter(country_name %in% sel_ctries) %>%
        mutate(Public_Sector=suppressWarnings(as.numeric(unlist(Public_Sector))),
               Private_Sector=suppressWarnings(as.numeric(unlist(Private_Sector)))) %>%
        drop_na(Public_Sector,Private_Sector),
      error=function(e) data.frame()
    )
    if (nrow(d)==0) return(doc)
    
    high_pub <- d %>% slice_max(Public_Sector,n=1)  %>% slice(1)
    low_pub  <- d %>% slice_min(Public_Sector,n=1)  %>% slice(1)
    high_pri <- d %>% slice_max(Private_Sector,n=1) %>% slice(1)
    low_pri  <- d %>% slice_min(Private_Sector,n=1) %>% slice(1)
    ref_pub  <- d %>% filter(country_name==ref) %>% pull(Public_Sector)  %>% first() %>% round(1)
    ref_pri  <- d %>% filter(country_name==ref) %>% pull(Private_Sector) %>% first() %>% round(1)
    n_c      <- nrow(d)
    rank_pub <- rank(-d$Public_Sector, ties.method="min")[d$country_name==ref]
    pub_pos  <- if (length(rank_pub)>0) dplyr::case_when(rank_pub==1~"the highest",rank_pub==n_c~"the lowest",TRUE~"in the middle range") else "in the middle range"
    
    mid_pub_text <- ""
    if (n_c >= 3) {
      sorted_d <- d %>% arrange(Public_Sector)
      mid_row  <- sorted_d %>% slice(ceiling(n_c/2))
      if (mid_row$country_name!=high_pub$country_name && mid_row$country_name!=low_pub$country_name)
        mid_pub_text <- paste0(mid_row$country_name," at ",round(mid_row$Public_Sector,1),", ")
    }
    
    interp <- paste0(
      "This figure compares pay compression ratios (90th/10th percentile) in the public and private sectors.\n",
      "For ",ref,", the pay compression ratio is ",ifelse(is.na(ref_pub),"N/A",ref_pub)," in the public sector and ",
      ifelse(is.na(ref_pri),"N/A",ref_pri)," in the private sector.\n",
      "Among the selected countries, ",high_pub$country_name," has the highest public sector pay compression at ",
      round(high_pub$Public_Sector,1),", ",mid_pub_text,
      "while ",low_pub$country_name," has the lowest at ",round(low_pub$Public_Sector,1),". ",
      "In the private sector, ",high_pri$country_name," has the highest pay compression, whereas ",low_pri$country_name," has the lowest.\n",
      ref," is ranked ",pub_pos," in public sector compression compared to other selected countries.\n",
      "A higher compression ratio indicates greater income disparity within the sector."
    )
    plot <- ggplot(d,aes(x=Private_Sector,y=Public_Sector,label=country_name))+geom_point(size=3)+ggrepel::geom_text_repel(size=3)+
      geom_smooth(method="lm",color="gray",linetype="dashed",se=FALSE)+
      labs(title="Pay Compression: Public vs. Private Sector",x="Private Sector Pay Compression",y="Public Sector Pay Compression")+theme_minimal()
    img <- tempfile(fileext=".png"); ggsave(img,plot=plot,width=8,height=6,dpi=300)
    miss_pc <- setdiff(sel_ctries, unique(d$country_name))
    doc <- doc %>% body_add_par("Pay Compression in the Private and Public Sector",style="heading 2") %>%
      body_add_par("This section presents pay compression ratios (90th/10th percentile) for public and private sectors across selected countries.",style="Normal")
    doc <- add_figure_block(doc, img, "Note: P90/P10 income ratio. Higher values = wider wage dispersion. Last year available.", interp, missing_vec=miss_pc)
    return(doc)
  }
  
  gen_female_emp_section <- function(doc, sel_ctries) {
    ref <- sel_ctries[1]
    doc <- add_section_heading(doc,"Equity in the Public Sector")
    d   <- gender_workforce %>% filter(country_name %in% sel_ctries)
    if (nrow(d)==0) return(doc)
    
    d_last <- d %>% group_by(country_name,indicator_name) %>% arrange(year,.by_group=TRUE) %>% slice_tail(n=1) %>% ungroup()
    d_pub  <- d_last %>% filter(indicator_name=="as a share of public paid employees")
    d_pri  <- d_last %>% filter(indicator_name=="as a share of private paid employees")
    avg_pub  <- round(mean(d_pub$value_percentage, na.rm=TRUE), 0)
    avg_pri  <- round(mean(d_pri$value_percentage, na.rm=TRUE), 0)
    high_pub <- d_pub %>% drop_na(value_percentage) %>% slice_max(value_percentage,n=1) %>% slice(1)
    low_pub  <- d_pub %>% drop_na(value_percentage) %>% slice_min(value_percentage,n=1) %>% slice(1)
    high_pri <- d_pri %>% drop_na(value_percentage) %>% slice_max(value_percentage,n=1) %>% slice(1)
    low_pri  <- d_pri %>% drop_na(value_percentage) %>% slice_min(value_percentage,n=1) %>% slice(1)
    ref_pub  <- d_pub %>% filter(country_name==ref) %>% pull(value_percentage) %>% first() %>% round(0)
    ref_pri  <- d_pri %>% filter(country_name==ref) %>% pull(value_percentage) %>% first() %>% round(0)
    avg_o_pub<- d_pub %>% filter(country_name!=ref) %>% summarise(a=round(mean(value_percentage,na.rm=TRUE),0)) %>% pull(a)
    avg_o_pri<- d_pri %>% filter(country_name!=ref) %>% summarise(a=round(mean(value_percentage,na.rm=TRUE),0)) %>% pull(a)
    pos_pub  <- if (!is.na(ref_pub)&&!is.na(avg_o_pub)) {
      if (ref_pub>avg_o_pub) paste0("This is higher than the average of ",avg_o_pub,"% across the other selected countries.")
      else paste0("This is lower than the average of ",avg_o_pub,"% across the other selected countries.")
    } else ""
    pos_pri  <- if (!is.na(ref_pri)&&!is.na(avg_o_pri)) {
      if (ref_pri>avg_o_pri) paste0("This is higher than the average of ",avg_o_pri,"% across the other selected countries.")
      else paste0("This is lower than the average of ",avg_o_pri,"% across the other selected countries.")
    } else ""
    
    interp <- paste0(
      "This graph compares female employment in the public and private sectors across selected countries. ",
      "On average, ",avg_pub,"% of public sector employees are female, while in the private sector, the share is ",avg_pri,"%. ",
      "The highest female employment in the public sector is in ",high_pub$country_name,
      ", while the lowest is in ",low_pub$country_name,". ",
      "In the private sector, ",high_pri$country_name," has the highest share of female employees, whereas ",low_pri$country_name," has the lowest.\n",
      "In ",ref,", female representation in the public sector is ",ifelse(is.na(ref_pub),"N/A",ref_pub),"%. ",pos_pub,"\n",
      "In the private sector, female representation in ",ref," is ",ifelse(is.na(ref_pri),"N/A",ref_pri),"%. ",pos_pri
    )
    g <- ggplot(d_last,aes(x=country_name,y=value_percentage,fill=indicator_name))+geom_bar(stat="identity",position="dodge")+
      scale_fill_manual(values=c("as a share of private paid employees"="#E69F00","as a share of public paid employees"="#56B4E9"))+
      labs(title="Female Employment by Sector (Last Year Available)",x="Country",y="Employment (%)",fill="Sector")+
      theme_minimal()+theme(axis.text.x=element_text(angle=45,hjust=1))
    img <- tempfile(fileext=".png"); ggsave(img,plot=g,width=8,height=6)
    miss_gw <- setdiff(sel_ctries, unique(d_last$country_name[!is.na(d_last$value_percentage)]))
    doc <- doc %>% body_add_par("Gender Workforce Analysis",style="heading 2") %>%
      body_add_par("This section presents an analysis of female employment in the public and private sectors across selected countries.",style="Normal") %>%
      body_add_par("Female Employment by Sector (Last Year Available)",style="heading 3")
    doc <- add_figure_block(doc, img, "Note: Share of females employed in the public and private sectors. Last year available.", interp, missing_vec=miss_gw)
    return(doc)
  }
  
  gen_female_leadership_section <- function(doc, sel_ctries) {
    ref <- sel_ctries[1]
    d   <- gender_leadership %>% filter(country_name %in% sel_ctries)
    if (nrow(d)==0) return(doc)
    avg_pub_mgr  <- round(mean(d$value_percentage[d$indicator_label=="Managers-Public"],  na.rm=TRUE), 1)
    avg_pri_mgr  <- round(mean(d$value_percentage[d$indicator_label=="Managers-Private"], na.rm=TRUE), 1)
    high_pub_mgr <- d %>% filter(indicator_label=="Managers-Public")  %>% drop_na(value_percentage) %>% slice_max(value_percentage,n=1) %>% slice(1)
    low_pub_mgr  <- d %>% filter(indicator_label=="Managers-Public")  %>% drop_na(value_percentage) %>% slice_min(value_percentage,n=1) %>% slice(1)
    high_pri_mgr <- d %>% filter(indicator_label=="Managers-Private") %>% drop_na(value_percentage) %>% slice_max(value_percentage,n=1) %>% slice(1)
    low_pri_mgr  <- d %>% filter(indicator_label=="Managers-Private") %>% drop_na(value_percentage) %>% slice_min(value_percentage,n=1) %>% slice(1)
    ref_pub_mgr  <- d %>% filter(country_name==ref,indicator_label=="Managers-Public")  %>% pull(value_percentage) %>% first() %>% round(1)
    ref_pri_mgr  <- d %>% filter(country_name==ref,indicator_label=="Managers-Private") %>% pull(value_percentage) %>% first() %>% round(1)
    avg_o_pub_mgr <- round(mean(d$value_percentage[d$indicator_label=="Managers-Public"  & d$country_name!=ref], na.rm=TRUE), 1)
    avg_o_pri_mgr <- round(mean(d$value_percentage[d$indicator_label=="Managers-Private" & d$country_name!=ref], na.rm=TRUE), 1)
    pos_pub <- if (!is.na(ref_pub_mgr)&&!is.na(avg_o_pub_mgr)) {
      if (ref_pub_mgr>avg_o_pub_mgr) paste0("This is higher than the average of ",avg_o_pub_mgr,"% across the other selected countries.")
      else paste0("This is lower than the average of ",avg_o_pub_mgr,"% across the other selected countries.")
    } else ""
    pos_pri <- if (!is.na(ref_pri_mgr)&&!is.na(avg_o_pri_mgr)) {
      if (ref_pri_mgr>avg_o_pri_mgr) paste0("This is higher than the average of ",avg_o_pri_mgr,"% across the other selected countries.")
      else paste0("This is lower than the average of ",avg_o_pri_mgr,"% across the other selected countries.")
    } else ""
    
    interp <- paste0(
      "This graph compares female representation in different occupational groups across selected countries. ",
      "On average, ",avg_pub_mgr,"% of public sector managers are female, while in the private sector, female managers account for ",avg_pri_mgr,"%. ",
      "The highest female representation among public sector managers is in ",high_pub_mgr$country_name,
      ", whereas the lowest is in ",low_pub_mgr$country_name,". ",
      "In the private sector, the highest female manager share is in ",high_pri_mgr$country_name,
      ", while the lowest is in ",low_pri_mgr$country_name,".\n",
      "In ",ref,", female managers account for ",ifelse(is.na(ref_pub_mgr),"N/A",ref_pub_mgr),"% in the public sector. ",pos_pub,"\n",
      "In the private sector, female managers in ",ref," represent ",ifelse(is.na(ref_pri_mgr),"N/A",ref_pri_mgr),"%. ",pos_pri
    )
    cols <- c("Clerks-Private"="#9ECAE1","Clerks-Public"="#08519C","Managers-Private"="#FDAE6B","Managers-Public"="#E6550D")
    p <- ggplot(d,aes(x=country_name,y=value_percentage,fill=indicator_label))+geom_col(position=position_dodge(0.75),width=0.7)+
      scale_fill_manual(values=cols,name="Occupation")+
      labs(title="Females by Occupational Group and Sector",x="Country",y="Female Share (%)")+
      theme_minimal()+theme(legend.position="top",axis.text.x=element_text(angle=45,hjust=1))
    img <- tempfile(fileext=".png"); ggsave(img,plot=p,width=8,height=6)
    miss_fl <- setdiff(sel_ctries, unique(d$country_name[!is.na(d$value_percentage)]))
    doc <- doc %>% body_add_par("Females by Occupational Group and Sector",style="heading 2") %>%
      body_add_par("This section presents an analysis of female representation in different occupational groups across selected countries.",style="Normal") %>%
      body_add_par("This graph shows the share of females in various occupational groups (Managers/Clerks) in the public and private sectors for the selected countries.",style="Normal")
    doc <- add_figure_block(doc, img, "Note: Female share in Managers/Clerks positions in public and private sectors. Last year available.", interp, missing_vec=miss_fl)
    return(doc)
  }
  
  gen_gender_wage_industry_section <- function(doc, sel_ctries) {
    ref <- sel_ctries[1]
    d   <- gender_wage_premiumpublic %>% filter(country_name %in% sel_ctries, indicator_label %in% c("Public Administration","Education","Health","Other"))
    if (nrow(d)==0||all(is.na(d$value_percentage))) return(doc)
    avg_adm  <- round(mean(d$value_percentage[d$indicator_label=="Public Administration"], na.rm=TRUE), 0)
    avg_edu  <- round(mean(d$value_percentage[d$indicator_label=="Education"],             na.rm=TRUE), 0)
    avg_hlth <- round(mean(d$value_percentage[d$indicator_label=="Health"],                na.rm=TRUE), 0)
    high_adm <- d %>% filter(indicator_label=="Public Administration") %>% drop_na(value_percentage) %>% slice_max(value_percentage,n=1) %>% slice(1)
    low_adm  <- d %>% filter(indicator_label=="Public Administration") %>% drop_na(value_percentage) %>% slice_min(value_percentage,n=1) %>% slice(1)
    high_edu <- d %>% filter(indicator_label=="Education")             %>% drop_na(value_percentage) %>% slice_max(value_percentage,n=1) %>% slice(1)
    low_edu  <- d %>% filter(indicator_label=="Education")             %>% drop_na(value_percentage) %>% slice_min(value_percentage,n=1) %>% slice(1)
    high_hlth<- d %>% filter(indicator_label=="Health")                %>% drop_na(value_percentage) %>% slice_max(value_percentage,n=1) %>% slice(1)
    low_hlth <- d %>% filter(indicator_label=="Health")                %>% drop_na(value_percentage) %>% slice_min(value_percentage,n=1) %>% slice(1)
    ref_adm  <- d %>% filter(country_name==ref,indicator_label=="Public Administration") %>% pull(value_percentage) %>% first() %>% round(0)
    ref_edu  <- d %>% filter(country_name==ref,indicator_label=="Education")             %>% pull(value_percentage) %>% first() %>% round(0)
    avg_o_adm<- round(mean(d$value_percentage[d$indicator_label=="Public Administration"&d$country_name!=ref],na.rm=TRUE),0)
    avg_o_edu<- round(mean(d$value_percentage[d$indicator_label=="Education"            &d$country_name!=ref],na.rm=TRUE),0)
    pos_adm  <- if (!is.na(ref_adm)&&!is.na(avg_o_adm)) {
      if (ref_adm>avg_o_adm) paste0("This is higher than the average of ",avg_o_adm,"% across the other selected countries.")
      else paste0("This is lower than the average of ",avg_o_adm,"% across the other selected countries.")
    } else ""
    pos_edu  <- if (!is.na(ref_edu)&&!is.na(avg_o_edu)) {
      if (ref_edu>avg_o_edu) paste0("This is higher than the average of ",avg_o_edu,"% across the other selected countries.")
      else paste0("This is lower than the average of ",avg_o_edu,"% across the other selected countries.")
    } else ""
    
    interp <- paste0(
      "This graph compares the gender wage premium in the public sector across different industries. ",
      "On average, the wage premium in Public Administration is ",avg_adm,"%, in Education it is ",avg_edu,"%, and in Health it is ",avg_hlth,"%. \n",
      "The highest wage premium in Public Administration is in ",high_adm$country_name,", while the lowest is in ",low_adm$country_name,". ",
      "In Education, the highest wage premium is observed in ",high_edu$country_name,", whereas the lowest is in ",low_edu$country_name,". ",
      "For Health, the highest gender wage premium is in ",high_hlth$country_name,", while the lowest is in ",low_hlth$country_name,".\n",
      "In ",ref,", the wage premium in Public Administration is ",ifelse(is.na(ref_adm),"N/A",ref_adm),"%. ",pos_adm,"\n",
      "In Education, the wage premium in ",ref," is ",ifelse(is.na(ref_edu),"N/A",ref_edu),"%. ",pos_edu
    )
    gender_wage_plot <- ggplot(d,aes(x=country_name,y=value_percentage,fill=indicator_label))+geom_bar(stat="identity",position="dodge")+
      scale_fill_viridis_d(option="D")+labs(title="Gender Wage Premium in Public Sector by Industry",x="Country",y="Wage Premium (%)",fill="Industry")+theme_minimal()
    img <- tempfile(fileext=".png"); ggsave(img,plot=gender_wage_plot,width=8,height=6,dpi=300)
    miss_gwi <- setdiff(sel_ctries, unique(d$country_name[!is.na(d$value_percentage)]))
    doc <- doc %>% body_add_par("Gender Wage Premium in Public Sector by Industry",style="heading 2") %>%
      body_add_par("This section presents an analysis of gender wage premiums in the public sector by industry (Public Administration, Education, and Health) across selected countries.",style="Normal") %>%
      body_add_par("This graph shows the gender wage premium in the public sector across different industries.",style="Normal")
    doc <- add_figure_block(doc, img, "Note: Gender wage premium in public sector by industry. Last year available.", interp, missing_vec=miss_gwi)
    return(doc)
  }
  
  gen_gender_wagepremium_section <- function(doc, sel_ctries) {
    ref <- sel_ctries[1]
    d1  <- gender_wage_premium_last %>% filter(country_name %in% sel_ctries) %>% drop_na(value_percentage)
    if (nrow(d1)>0) {
      avg_all  <- round(mean(d1$value_percentage, na.rm=TRUE), 1)
      high_c   <- d1 %>% slice_max(value_percentage,n=1) %>% slice(1)
      low_c    <- d1 %>% slice_min(value_percentage,n=1) %>% slice(1)
      ref_val  <- d1 %>% filter(country_name==ref) %>% summarise(a=round(mean(value_percentage,na.rm=TRUE),1)) %>% pull(a)
      avg_o    <- d1 %>% filter(country_name!=ref) %>% summarise(a=round(mean(value_percentage,na.rm=TRUE),1)) %>% pull(a)
      pos_stmt <- if (!is.na(ref_val)&&!is.na(avg_o)) {
        if (ref_val>avg_o) paste0("This is higher than the average of ",avg_o,"% across the other selected countries.")
        else paste0("This is lower than the average of ",avg_o,"% across the other selected countries.")
      } else ""
      interp1 <- paste0(
        "This graph compares public sector wage premiums by gender across selected countries. ",
        "On average, the gender wage premium is ",avg_all,"%. ",
        "The highest premium is observed in ",high_c$country_name,", and the lowest in ",low_c$country_name,". ",
        "In ",ref,", the premium is ",ifelse(is.na(ref_val),"N/A",ref_val),"%. ",pos_stmt
      )
      p1 <- ggplot(d1,aes(x=country_name,y=value_percentage,color=indicator_label))+geom_point(size=4)+
        scale_color_manual(values=c("Male"="#E69F00","Female"="#56B4E9"))+
        labs(title="Wage Premium by Gender (Last Year Available)",x="Country",y="Wage Premium (%)",color="Gender")+theme_minimal()
      img1 <- tempfile(fileext=".png"); ggsave(img1,plot=p1,width=8,height=6)
      miss_gpg <- setdiff(sel_ctries, unique(d1$country_name[!is.na(d1$value_percentage)]))
      doc <- doc %>% body_add_par("Wage Premium Gender Analysis",style="heading 2") %>%
        body_add_par("This section presents evidence on public sector employment and compensation practices by gender across selected countries.",style="Normal") %>%
        body_add_par("Wage Premium by Gender (Multi-Country)",style="heading 3")
      doc <- add_figure_block(doc, img1, "Note: Public sector wage premium by gender for the latest available year.", interp1, missing_vec=miss_gpg)
    }
    # Time series for ref country
    d2 <- gender_wage_premium %>% filter(country_name==ref)
    if (nrow(d2)>0) {
      # Men
      d2m <- d2 %>% filter(indicator_label=="Male") %>% arrange(year) %>% filter(!is.na(value_percentage))
      d2f <- d2 %>% filter(indicator_label=="Female") %>% arrange(year) %>% filter(!is.na(value_percentage))
      y_first <- if (nrow(d2m)>0) d2m$year[1]              else if (nrow(d2f)>0) d2f$year[1]              else NA
      y_last  <- if (nrow(d2m)>0) d2m$year[nrow(d2m)]      else if (nrow(d2f)>0) d2f$year[nrow(d2f)]      else NA
      y_mid   <- if (nrow(d2m)>0) d2m$year[ceiling(nrow(d2m)/2)] else if (nrow(d2f)>0) d2f$year[ceiling(nrow(d2f)/2)] else NA
      vm_first<- if (nrow(d2m)>0) round(d2m$value_percentage[1],1) else NA
      vm_mid  <- if (nrow(d2m)>0) round(d2m$value_percentage[ceiling(nrow(d2m)/2)],1) else NA
      vm_last <- if (nrow(d2m)>0) round(d2m$value_percentage[nrow(d2m)],1) else NA
      vf_first<- if (nrow(d2f)>0) round(d2f$value_percentage[1],1) else NA
      vf_mid  <- if (nrow(d2f)>0) round(d2f$value_percentage[ceiling(nrow(d2f)/2)],1) else NA
      vf_last <- if (nrow(d2f)>0) round(d2f$value_percentage[nrow(d2f)],1) else NA
      
      interp2 <- paste0(
        "This graph shows how the public sector wage premium by gender evolved in ",ref,". ",
        if (!is.na(y_first)) paste0("In ",y_first,", the wage premium was ",ifelse(is.na(vm_first),"N/A",vm_first),"% for men and ",ifelse(is.na(vf_first),"N/A",vf_first),"% for women. ") else "",
        if (!is.na(y_mid) && y_mid!=y_first) paste0("By ",y_mid,", it was ",ifelse(is.na(vm_mid),"N/A",vm_mid),"% for men and ",ifelse(is.na(vf_mid),"N/A",vf_mid),"% for women. ") else "",
        if (!is.na(y_last) && y_last!=y_mid) paste0("By ",y_last,", it was ",ifelse(is.na(vm_last),"N/A",vm_last),"% for men and ",ifelse(is.na(vf_last),"N/A",vf_last),"% for women.") else ""
      )
      p2 <- ggplot(d2,aes(x=year,y=value_percentage,color=indicator_label,group=indicator_label))+geom_line(size=1.2)+geom_point(size=3)+
        scale_color_manual(values=c("Male"="#E69F00","Female"="#56B4E9"))+
        labs(title=paste("Wage Premium Over Time —",ref),x="Year",y="Wage Premium (%)",color="Gender")+theme_minimal()
      img2 <- tempfile(fileext=".png"); ggsave(img2,plot=p2,width=8,height=6)
      doc <- doc %>% body_add_par("Wage Premium by Gender (Over Time)",style="heading 3")
      doc <- add_figure_block(doc, img2, "Note: Evolution of wage premium by gender over time vs. private sector.", interp2)
    }
    return(doc)
  }
  
  gen_workforce_dist_section <- function(doc, sel_ctries) {
    ref <- sel_ctries[1]
    d   <- public_sector_workforce %>% filter(country_name %in% sel_ctries)
    if (nrow(d)==0) return(doc)
    # largest sector avg
    largest_sector <- d %>% group_by(indicator_name) %>% summarise(avg=mean(value_percentage,na.rm=TRUE),.groups="drop") %>% slice_max(avg,n=1)
    ref_adm  <- d %>% filter(country_name==ref,indicator_name=="Public Administration") %>% pull(value_percentage) %>% mean(na.rm=TRUE) %>% round(1)
    ref_edu  <- d %>% filter(country_name==ref,indicator_name=="Education")             %>% pull(value_percentage) %>% mean(na.rm=TRUE) %>% round(1)
    ref_hlth <- d %>% filter(country_name==ref,indicator_name=="Health")                %>% pull(value_percentage) %>% mean(na.rm=TRUE) %>% round(1)
    high_adm <- d %>% filter(indicator_name=="Public Administration") %>% drop_na(value_percentage) %>% slice_max(value_percentage,n=1) %>% slice(1)
    low_adm  <- d %>% filter(indicator_name=="Public Administration") %>% drop_na(value_percentage) %>% slice_min(value_percentage,n=1) %>% slice(1)
    avg_o_adm<- round(mean(d$value_percentage[d$indicator_name=="Public Administration"&d$country_name!=ref],na.rm=TRUE),1)
    pos_adm  <- if (!is.na(ref_adm)&&!is.na(avg_o_adm)) {
      if (ref_adm>avg_o_adm) paste0("This is higher than the average of ",avg_o_adm,"% across the other selected countries.")
      else paste0("This is lower than the average of ",avg_o_adm,"% across the other selected countries.")
    } else ""
    
    interp <- paste0(
      "This graph presents the distribution of the public sector workforce across industries for the selected countries. ",
      "On average, the largest public sector employer is ",largest_sector$indicator_name," with ",round(largest_sector$avg,1),"% of paid public employment. ",
      if (!is.na(ref_adm))  paste0("In ",ref,", Public Administration accounts for ",ref_adm,"% of public employment. ") else "",
      if (!is.na(ref_edu))  paste0("Education represents ",ref_edu,"%. ")   else "",
      if (!is.na(ref_hlth)) paste0("Health accounts for ",ref_hlth,"%. ")   else "",
      "The highest share of Public Administration is in ",high_adm$country_name,
      ", while the lowest is in ",low_adm$country_name,". ",pos_adm,"\n",
      "These figures highlight the structural composition of the public workforce and how it compares across selected countries."
    )
    p1 <- ggplot(d,aes(x=country_name,y=value_percentage,fill=indicator_name))+geom_bar(stat="identity",position="stack")+
      scale_fill_viridis_d(option="D")+labs(title="Public Workforce Distribution",x="Country",y="Workforce Distribution (%)",fill="Sector")+theme_minimal()
    img <- tempfile(fileext=".png"); ggsave(img,plot=p1,width=8,height=4)
    miss_wfd <- setdiff(sel_ctries, unique(d$country_name[!is.na(d$value_percentage)]))
    doc <- doc %>% body_add_par("Public Workforce Distribution",style="heading 2")
    doc <- add_figure_block(doc, img, "Note: Distribution of public sector employment across industries as a percentage of paid public employment. Last year available.", interp, missing_vec=miss_wfd)
    return(doc)
  }
  
  generate_conclusion_section <- function(doc) {
    doc %>% body_add_par("Conclusion",style="heading 1") %>%
      body_add_par(paste0(
        "This report provides a comprehensive analysis of wage bill trends, gender employment representation, and workforce participation in the public sector. \n",
        "The findings highlight key trends and disparities across different sectors and countries."
      ),style="Normal")
  }
  
  # ── Full report ──────────────────────────────────────────────────────────────
  output$downloadAllGraphsDoc <- downloadHandler(
    filename=function() paste0("Wage_bill_and_public_employment_analysis_",Sys.Date(),".docx"),
    content=function(file){
      sel <- sanitize_vec(input$download_report_countries)
      title_style <- fp_text(color="#722F37",font.size=20,bold=TRUE)
      doc <- officer::read_docx() %>% body_add_fpar(fpar(ftext("Worldwide Bureaucracy Indicators Analysis",prop=title_style)))
      doc <- generate_intro_section(doc, sel)
      doc <- gen_wagebill_section(doc, sel)
      doc <- gen_public_emp_section(doc, sel)
      doc <- gen_tertiary_section(doc, sel)
      doc <- gen_wagepremium_section(doc, sel)
      doc <- gen_wagepremium_educ_section(doc, sel)
      doc <- gen_paycompression_section(doc, sel)
      doc <- gen_female_emp_section(doc, sel)
      doc <- gen_female_leadership_section(doc, sel)
      doc <- gen_gender_wage_industry_section(doc, sel)
      doc <- gen_gender_wagepremium_section(doc, sel)
      doc <- gen_workforce_dist_section(doc, sel)
      doc <- generate_conclusion_section(doc)
      print(doc,target=file)
    })
  
  # ── Custom report ────────────────────────────────────────────────────────────
  output$downloadSelectedGraphsDoc <- downloadHandler(
    filename=function() paste0("Wage_bill_and_public_employment_analysis_Selected_",Sys.Date(),".docx"),
    content=function(file){
      sel  <- sanitize_vec(input$download_report_countries)
      secs <- input$selected_graphs
      title_style <- fp_text(color="#722F37",font.size=20,bold=TRUE)
      doc <- officer::read_docx() %>% body_add_fpar(fpar(ftext("Wage Bill and Public Employment Analysis",prop=title_style)))
      doc <- generate_intro_section(doc, sel)
      if (is.null(secs)||!length(secs)) { doc <- doc %>% body_add_par("No sections selected.",style="Normal") }
      else {
        if (any(c("wagebill","wagebill_gdp") %in% secs)) doc <- gen_wagebill_section(doc, sel)
        if ("public_employment"  %in% secs) doc <- gen_public_emp_section(doc, sel)
        if ("tertiaryeducation"  %in% secs) doc <- gen_tertiary_section(doc, sel)
        if ("wagepremium"        %in% secs) doc <- gen_wagepremium_section(doc, sel)
        if ("wagepremiumeducation" %in% secs) doc <- gen_wagepremium_educ_section(doc, sel)
        if ("pay_compression"    %in% secs) doc <- gen_paycompression_section(doc, sel)
        if ("gender_workforce"   %in% secs) doc <- gen_female_emp_section(doc, sel)
        if ("female_leadership"  %in% secs) doc <- gen_female_leadership_section(doc, sel)
        if ("gender_wage_premium" %in% secs) doc <- gen_gender_wage_industry_section(doc, sel)
        if ("wagepremiumgender"  %in% secs) doc <- gen_gender_wagepremium_section(doc, sel)
        if ("public_workforce"   %in% secs) doc <- gen_workforce_dist_section(doc, sel)
      }
      print(doc,target=file)
    })
  
  # ── PowerPoint ───────────────────────────────────────────────────────────────
  add_ppt_section_slide <- function(ppt, title) {
    ppt %>% officer::add_slide(layout="Title Slide",master="Office Theme") %>%
      officer::ph_with(officer::fpar(officer::ftext(title,prop=officer::fp_text(color="#003366",font.size=36,bold=TRUE))),
                       location=officer::ph_location_type(type="ctrTitle"))
  }
  add_ppt_graph <- function(ppt, d, title, x, y, color=NULL, type="point") {
    if (is.null(d)||nrow(d)==0) return(ppt)
    p <- if (type=="line") ggplot(d,aes_string(x=x,y=y,color=color))+geom_line(size=1)+geom_point(size=2)
    else if (type=="bar")  ggplot(d,aes_string(x=x,y=y,fill=color))+geom_col(position=position_dodge(0.8),width=0.7)
    else ggplot(d,aes_string(x=x,y=y,color=color))+geom_point(size=3)
    p <- p+labs(title=title)+theme_minimal()+theme(axis.text.x=element_text(angle=45,hjust=1))
    img <- tempfile(fileext=".png"); ggsave(img,plot=p,width=8,height=5,dpi=300)
    ppt %>% officer::add_slide(layout="Title and Content",master="Office Theme") %>%
      officer::ph_with(officer::external_img(img,height=5,width=7),location=officer::ph_location_type(type="body"))
  }
  
  output$downloadSelectedGraphsPPT <- downloadHandler(
    filename=function() paste0("WWBI_Presentation_",Sys.Date(),".pptx"),
    content=function(file){
      sel  <- sanitize_vec(input$download_report_countries)
      secs <- input$selected_graphs
      ppt  <- officer::read_pptx() %>%
        officer::add_slide(layout="Title Slide",master="Office Theme") %>%
        officer::ph_with("Worldwide Bureaucracy Indicators",location=officer::ph_location_type(type="ctrTitle")) %>%
        officer::ph_with(paste("Generated on",Sys.Date()),location=officer::ph_location_type(type="subTitle"))
      if (!is.null(secs)&&length(secs)>0) {
        if (any(c("wagebill","wagebill_gdp") %in% secs)) ppt <- add_ppt_section_slide(ppt,"Macro-Fundamentals")
        if ("wagebill"          %in% secs) ppt <- add_ppt_graph(ppt,wage_bill_publicexp %>% filter(country_name %in% sel),"Wage Bill as % of Public Expenditure","year","value","country_name","line")
        if ("wagebill_gdp"      %in% secs) ppt <- add_ppt_graph(ppt,merged_data %>% filter(country_name %in% sel),"Wage Bill vs GDP","log_gdp","indicator_value","country_name","point")
        if (any(c("public_employment","tertiaryeducation") %in% secs)) ppt <- add_ppt_section_slide(ppt,"Size and Characteristics")
        if ("public_employment" %in% secs) ppt <- add_ppt_graph(ppt,public_sector_emp_temp_last %>% filter(country_name %in% sel),"Public Sector Employment","country_name","value_percentage","indicator_label","point")
        if ("tertiaryeducation" %in% secs) ppt <- add_ppt_graph(ppt,tertiary_education %>% filter(country_name %in% sel),"Tertiary Education","country_name","value_percentage","indicator_name","bar")
        if (any(c("wagepremium","wagepremiumeducation","pay_compression") %in% secs)) ppt <- add_ppt_section_slide(ppt,"Competitiveness of Public Sector Wages")
        if ("wagepremium"       %in% secs) ppt <- add_ppt_graph(ppt,public_wage_premium %>% filter(country_name %in% sel),"Wage Premium","country_name","value_percentage",NULL,"point")
        if ("wagepremiumeducation" %in% secs) ppt <- add_ppt_graph(ppt,public_wage_premium_educ %>% filter(country_name %in% sel),"Wage Premium by Education","indicator_name","value_percentage","indicator_name","bar")
        if ("pay_compression"   %in% secs) ppt <- add_ppt_graph(ppt,pay_compression_wide %>% filter(country_name %in% sel) %>% mutate(Public_Sector=as.numeric(unlist(Public_Sector)),Private_Sector=as.numeric(unlist(Private_Sector))),"Pay Compression","Private_Sector","Public_Sector","country_name","point")
        if (any(c("wagepremiumgender","gender_workforce","female_leadership","gender_wage_premium") %in% secs)) ppt <- add_ppt_section_slide(ppt,"Equity in the Public Sector")
        if ("wagepremiumgender" %in% secs) ppt <- add_ppt_graph(ppt,gender_wage_premium_last %>% filter(country_name %in% sel),"Wage Premium by Gender","country_name","value_percentage","indicator_label","point")
        if ("gender_workforce"  %in% secs) ppt <- add_ppt_graph(ppt,gender_workforce %>% filter(country_name %in% sel) %>% group_by(country_name,indicator_name) %>% slice_tail(n=1) %>% ungroup(),"Female Employment","country_name","value_percentage","indicator_name","bar")
        if ("female_leadership" %in% secs) ppt <- add_ppt_graph(ppt,gender_leadership %>% filter(country_name %in% sel),"Female Leadership","country_name","value_percentage","indicator_label","bar")
        if ("gender_wage_premium" %in% secs) ppt <- add_ppt_graph(ppt,gender_wage_premiumpublic %>% filter(country_name %in% sel,indicator_label %in% c("Public Administration","Education","Health")),"Gender Wage Premium by Industry","country_name","value_percentage","indicator_label","bar")
        if ("public_workforce"  %in% secs) ppt <- add_ppt_graph(ppt,public_sector_workforce %>% filter(country_name %in% sel),"Workforce Distribution","country_name","value_percentage","indicator_name","bar")
      }
      print(ppt,target=file)
    })
}

shinyApp(ui=ui, server=server)
