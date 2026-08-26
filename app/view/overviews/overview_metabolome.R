box::use(
  shiny[tags, NS, tagList, fluidRow, column, moduleServer],
  shinydashboardPlus[box],
  htmltools[HTML]
)

#' @export
ui <- function(id) {

  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 12, class = "col-lg-12",
        box(
          id = ns("Overview"),
          title = "",
          height = "auto",
          width = 12,
          closable = FALSE,
          solidHeader = FALSE,
          collapsible = FALSE,
          headerBorder = FALSE,
          tags$img(
            src = "static/LindaCrnicInstituteLogo.png",
            width = "auto",
            height = "115px",
            style = "margin: auto;display:block"
          ),
          tags$br(),
          tags$div(
            class = "overviewHeader",
            tags$h2("WHAT IS THE METABOLOME?")
          ),
          tags$div(
            class = "overviewBodyText",
            tags$p(
              tags$a(
                href = "https://en.wikipedia.org/wiki/Metabolomics",
                target = "_blank",
                tags$b("Metabolomics"),
                .noWS = c("outside")
              ),
              HTML(" is the study of all small molecules, or 'metabolites',
                produced by a cell, tissue, or organism while performing typical biological processes,
                such as converting food into energy, or getting rid of waste products from cells.
                Together, these metabolites create a chemical fingerprint (i.e., the 'metabolome')
                that provides valuable information about how that cell, tissue, or organism is functioning.
                Therefore, by measuring all metabolites, we can explore how cells are actively functioning
                in response to both internal gene expression changes and external environmental factors.
                We used metabolomics to compare metabolites between people with and without Down syndrome."
              )
            ),
            tags$br(),
            tags$div(
              class = "overviewHeader",
              tags$h3("OVERVIEW")
            ),
            tags$div(
              class = "overviewBodyText",
              HTML("This dashboard presents metabolomic data generated from
                Plasma samples, as described in "),
              tags$a(
                href = "https://pubmed.ncbi.nlm.nih.gov/31628327/",
                target = "_blank",
                "Powers et al, 2019",
                .noWS = c("outside")
              ),
              HTML(", and "),
              tags$a(
                href = "https://pubmed.ncbi.nlm.nih.gov/37379383/",
                target = "_blank",
                "Galbraith et al, 2023.",
                .noWS = c("outside")
              ),
              tags$br(),
              tags$br(),
              tags$div(
                class = "overviewHeader",
                tags$h3("Effect of trisomy 21")
              ),
              tags$div(
                class = "overviewBodyText",
                tags$p(
                  "Explore the differences in metabolite levels between samples with
                  trisomy 21 and those without (controls)."
                ),
                tags$p("
                  The data are searchable by metabolite name and can be filtered by study,
                  age at time of blood collection, and sex"
                ),
                tags$p("
                  Significant differences between karyotypes (q<0.1, FDR10)
                  are indicated within the selected parameters."
                ),
                tags$br(),
                tags$p("Please click on the information icons for a step-by-step tutorial."),
              )
            ),
            tags$br(),
            tags$br(),
            tags$div(
              class = "overviewHeader",
              tags$h3("DATASETS AND PUBLICATIONS")
            ),
            tags$div(
              class = "overviewBodyText",
              tags$div(
                tags$a(
                  href = "https://www.nature.com/articles/s41467-019-12739-9",
                  target = "_blank",
                  tags$b("Trisomy 21 activates the kynurenine pathway via increased dosage
                  of interferon receptors"),
                  .noWS = c("outside")
                ),
                tags$br(),
                HTML(
                  "Powers RK, Culp-Hill R, Ludwig MP, Smith KP, Waugh KA, Minter R, Tuttle KD, Lewis HC,
                  Rachubinski AL, Granrath RE, Carmona-Iragui M, Wilkerson RB, Kahn DE, Joshi M, Lle&oacute; A,
                  Blesa R, Fortea J, D'Alessandro A, Costello JC, Sullivan KD, Espinosa JM."
                ),
                tags$p(
                  "Nature Communications 2019 Oct 18;10(1):4766. doi: 10.1038/s41467-019-12739-9.PMID: 31628327"
                )
              ),
              tags$br(),
              tags$div(
                tags$a(
                  href = "https://pubmed.ncbi.nlm.nih.gov/37379383/",
                  target = "_blank",
                  tags$b("Multidimensional definition of the interferonopathy of Down syndrome and its
                  response to JAK inhibition")
                ),
                tags$br(),
                HTML(
                  "Galbraith MD, Rachubinski AL, Smith KP, Araya P, Waugh KA, Enriquez-Estrada B, Worek K,
                  Granrath RE, Kinning KT, Paul Eduthan N, Ludwig MP, Hsieh EWY, Sullivan KD, Espinosa JM"
                ),
                tags$p(
                  "Sciences Advances 2023 Jun 28;9(26):eadg6218. doi: 10.1126/sciadv.adg6218.
                  Epub 2023 Jun 28. PMID: 37379383"
                )
              )
            )
          )
        )
      )
    )
  )

}

#' overview page for metabolome application
#' @param id - string - id for this module namespace
#' @export
server <- function(id) {

  moduleServer(id, function(input, output, session) {

  })

}
