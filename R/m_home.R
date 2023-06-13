### CANALE MODULE ##############################################################

# UI ----------------------------------------------------------------------

home_UI <- function(id) {
  susPage(class = "sus-page-home", header = susBanner(), footer = susFooter(),
    susPageSectionFeature(
      susCarousel(
        susCarouselSlide(
          title = tags$em(nowrap(curbcut::cc_t("Towards a")), 
                          nowrap(curbcut::cc_t("sustainable city"))),
          preview = curbcut::cc_t("Welcome"),
          tags$p(curbcut::cc_t("Curbcut is a platform for exploring urban ",
                              "sustainability across ",
                              "multiple spatial and temporal scales. Curbcut ",
                              "offers a justice- and inclusivity-focused ",
                              "approach to sustainability which integrates ",
                              "the widest possible range of data sources to ",
                              "help researchers, policymakers, communities, ",
                              "and individuals.")),
          tags$div(class = "sus-button-group",
                  tags$a(class = "sus-button sus-icon-button sus-button-secondary", 
                         href = "#learn-more", curbcut::cc_t("Learn more"), 
                         curbcut::icon_material("auto_stories")),
                  tags$a(class = "sus-button sus-icon-button sus-button-primary", 
                         href = "#start-exploring", 
                         curbcut::cc_t("Start Exploring Maps"), 
                         curbcut::icon_material("travel_explore"))
          )
        )
      )
    ),
    susPageSection(
      tags$h2(curbcut::cc_t("About Curbcut"), 
              scrollAnchor(id = "learn-more")),
      tags$p(cc_t( 
                  "Curbcut embraces an inclusive vision of urban ",
        "sustainability, allowing users to pose questions ",
        "about environmental issues and contextualize them ",
        "within larger frameworks of equity and ",
        "accessibility. It serves as both a data-exploration ",
        "tool and a knowledge and information-sharing ",
        "resource, designed to encourage greater reflection ",
        "on urban sustainability challenges, and on the ",
        "communities which are most affected by them.")),
      tags$p(cc_t( 
                  "Curbcut is organized into thematic and place-based ",
        "“modules”, each of which takes a narrow slice of ",
        "our data and presents it in a way designed to ",
        "answer existing questions and provoke new ones. ",
        "What is the relationship between heat risk and ",
        "housing tenure? Does my neighbourhood have better ",
        "or worse active transport options than the rest ",
        "of the city? The majority ",
        "of the data is publicly available, and over time ",
        "we will be adding more tools for users to export ",
        "the data and use it themselves.")),
      tags$p(HTML(paste0(
        curbcut::cc_t("See the "),
        "<a style ='cursor:pointer;' onclick = openTab('how_to_use')>",
        curbcut::cc_t("“How to use”"),"</a>",
        curbcut::cc_t("page for more information on ",
        "how Curbcut works. And see the "),
        "<a style ='cursor:pointer;' onclick = openTab('authors')>",
        curbcut::cc_t("“Authors”"), "</a>", 
        curbcut::cc_t(" page to learn more about our team.")))),
      tags$p(class = "text-center", tags$em(
        curbcut::cc_t("An initiative of the "),
        HTML(paste0("<a href = 'https://dci.ischool.utoronto.ca/'>",
                    curbcut::cc_t("Digital Curation Institute,"), 
                    "</a>")),
      )),
      tags$p(class = "text-center", tags$em(
        curbcut::cc_t("In collaboration with the"),
        HTML(paste0("<a href = 'https://www.mcgill.ca/mssi/'>",
                    curbcut::cc_t("McGill Sustainability Systems Initiative"), 
                    "</a>.")),
      ))
    ),
    susPageSection(
      tags$h2(curbcut::cc_t("Maps"), 
              scrollAnchor(id = "start-exploring")),
      tags$div(
        class = "text-width", 
        do.call(linkList, c(
          ready_modules_home(mods_rdy), 
          list(linkListGroup(
            name = curbcut::cc_t("More"), 
            list(name = curbcut::cc_t("Toronto stories"), 
                 onclick = "openTab('stories')"),
            list(name = curbcut::cc_t("Place explorer"), 
                 onclick = "openTab('place_explorer')"))))
      ))
    ), tags$div(style = "width: 250px; height: 50px;", hidden = "", susLegend())
  )
}


# Server ------------------------------------------------------------------

home_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    
    bookmark_server(id = "home", r = r)

  })
}
