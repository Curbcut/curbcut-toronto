### AUTHORS MODULE #####################################################

authors_UI <- function(id) {
  tagList(
    susPage(class = "sus-page-authors", footer = susFooter(), susPageSection(
      h1(curbcut::cc_t("Authors")),
      # susAuthor("Socrates", "Philosopher", "https://cdn.britannica.com/69/75569-050-7AB67C4B/herm-Socrates-half-original-Greek-Capitoline-Museums.jpg",
      #           susAuthorBio("Socrates was a Greek philosopher from Athens who is credited as the founder of Western philosophy and among the first moral philosophers of the ethical tradition of thought. An enigmatic figure, Socrates authored no texts and is known mainly through the posthumous accounts of classical writers, particularly his students Plato and Xenophon."),
      #           susAuthorLink("Wikipedia", href="https://en.wikipedia.org/wiki/Socrates",
      #                         icon=tags$img(src="https://iconarchive.com/download/i54074/danleech/simple/wikipedia.ico")),
      #           susAuthorLink("Encyclopedia Britannica", href="https://www.britannica.com/biography/Socrates",
      #                         icon=tags$img(src="https://cdn.britannica.com/mendel-resources/3-63/images/EBLogo.jpg?v=3.63.12")),
      #           susAuthorLink("Stanford Encyclopedia of Philosophy", href="https://plato.stanford.edu/entries/socrates/")
      #           ),
      h2(curbcut::cc_t("Toronto Team")),
      susAuthor("Christoph Becker", 
                curbcut::cc_t("Toronto Lead"), 
                "team/photos/christoph_becker.jpg",
                susAuthorBio(
                  curbcut::cc_t("Christoph Becker is Professor at the Faculty",
                                "of Information and the School of the Environm",
                                "ent at the University of Toronto and directs ",
                                "the Digital Curation Institute. His research",
                                " focuses on enacting meaningful change in",
                                "computing to meet the urgent need for sustaina",
                                "bility and social justice. He does that by work",
                                "ing with others across fields like human comput",
                                "er interaction, software engineering, science a",
                                "nd technology studies and psychology, to (1) ex",
                                "amine the politics, values, and cognitive proces",
                                "ses of design, (2) develop methods and tools fo",
                                "r just sustainability design, and (3) design pro",
                                "jects to bring forth just sustainabilities in ur",
                                "ban contexts.  His book Insolvent: How to reorien",
                                "t computing for just sustainability appeared at M",
                                "IT Press on June 6, 2023.")
                )),
      susAuthor("Han Qiao", 
                curbcut::cc_t("Designer"), 
                "team/photos/han_qiao.jpg",
                susAuthorBio(
                  curbcut::cc_t("Han Qiao is a PhD student at the Faculty of ",
                                "Information at University of Toronto. Her ",
                                "research interests are a combination of hum",
                                "an-computer interaction, design and urban p",
                                "lanning. She graduated with a Master’s degr",
                                "ee in Urban Planning, where she worked on p",
                                "rojects around understanding and predicting ",
                                "public transit and active transportation us",
                                "age through data and modeling. Han is curre",
                                "ntly exploring the role of urban data in co",
                                "mmunities’ advocacy works and how designing",
                                " data exploration tools through design frame",
                                "works such as  feminist data science and de",
                                "sign justice could empower communities and ",
                                "encourage conversations between different ",
                                "ways of knowing.")
                )),
      susAuthor("Eshta Bhardwaj", 
                curbcut::cc_t("Designer"), 
                "team/photos/eshta_bhardwaj.jpeg",
                susAuthorBio(
                  curbcut::cc_t("Eshta Bhardwaj is a PhD student at the Facul",
                                "ty of Information at University of Toronto. H",
                                "er research interests are in the role of data",
                                " practices in decision-making for climate chan",
                                "ge by studying it as a wicked problem with i",
                                "ssues like incommensurability and psychologic",
                                "al distance. She aims to develop design inter",
                                "ventions that allow experts to make better cl",
                                "imate change-related decisions using data.")
                )),
      h2(curbcut::cc_t("Curbcut Team")),
      susAuthor("David Wachsmuth", 
                curbcut::cc_t("Principal investigator and Lead Designer"), 
                "team/photos/david_wachsmuth.jpeg",
                susAuthorBio(
                  curbcut::cc_t("David Wachsmuth is one of the world’s leading ",
                                "experts on the impacts of short-term rental ",
                                "platforms, such as Airbnb, on cities around ",
                                "the world and consults widely with municipal",
                                "ities a",
                                "nd community organizations on designing appr",
                                "opriate regulations. In addition to his work ",
                                "at Curbcut, David is the Canada Research Chair",
                                "in Urban Governance at McGill University, whe",
                                "re he is also an Associate Professor in the Sc",
                                "hool of Urban Planning.")
                )),
      susAuthor("Kevin Manaugh", 
                curbcut::cc_t("Principal Investigator"), 
                "team/photos/kevin_manaugh.jpg",
                susAuthorBio(
                  curbcut::cc_t("Kevin Manaugh is one of the leading experts on",
                                "the intersection between urban transport syste",
                                "ms and social and environmental justice. In ad",
                                "dition to his work at Curbcut, Kevin is also a",
                                "n associate professor at McGill University joint",
                                "ly appointed in the Department of Geography and",
                                "the Bieler School of Environment.")
                )),
      susAuthor("Maxime Bélanger De Blois", 
                curbcut::cc_t("Senior Designer"), 
                "team/photos/maxime_belanger_de_blois.jpg",
                susAuthorBio(
                  curbcut::cc_t("Maxime is a skilled and resourceful data scien",
                                "tist and dashboard designer with an extensive ",
                                "understanding of data analysis and geovisualiz",
                                "ation. He has a master’s degree in Urban Plann",
                                "ing from McGill University.")
                )),
      susAuthor("Dominique Boulet", 
                curbcut::cc_t("Qualitative research lead"), 
                "team/photos/dominique_boulet.jpg",
                susAuthorBio(
                  curbcut::cc_t("An integral team member since 2022, Dominique ",
                                "is driven to create qualitative work that compl",
                                "ements quantitative information. She has a mast",
                                "er’s degree in Urban Planning from McGill Unive",
                                "rsity and a master’s degree in Anthropology from",
                                "Aarhus University, Copenhagen.")
                ))
      )))
}

authors_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {})
  }
