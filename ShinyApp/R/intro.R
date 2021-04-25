# Layout for Introduction Pane

introUI <- function(id = "intro") {
  ns <- NS(id)
  tagList(fluidRow(
    column(
      width = 4,
      box(
        width = 12,
        solidHeader = TRUE,
        title = "Overview",
        p(
          "Visualising Industry Skill TAlent Shifts (VISTAS) is a R Shiny application that allows individuals and country representatives to view their competitive advantage and understand the evolving labour market around the world. Most visualisations can be done at country-level, industry-level and skill-level."
        ),
        p(
          "There are two main modules in VISTAS - Statistical Analysis and Migration Analysis. The former focuses on statistical tools such as regression, scatter plots and correlation matrix. The latter allows for visualisation of migration trends through various methods. Each module contains various visualisations as shown on the right. Statistical Analysis visualisations are bordered in blue while Migration Analysis visualisations are bordered in red."
        ),
        p(
          "Hover over the various visualisations on the right to see a description of each visualisation."
        )
      )
      ,
      box(
        width = 12,
        solidHeader = TRUE,
        title = "Links",
        div(div("Research Paper"),
            div("Usage Guide"),
            div(
              a(href = "https://va.moomookau.org/posts/2021-02-21-visual-analytics-project-proposal/", "Project Proposal")
            ),
            div(
              a(href = "https://github.com/moomookau/VISTAS", "Project Github")
            ),)
      )
    ),
    column(
      width = 8,
      fluidRow(
        style = "padding-bottom: 20px;",
        flipBox(
          id = "Regression",
          front = div(
            style = "positive: relative;",
            img(
              src = "img/regression.png",
              width = "100%",
              height = "100%"
            ),
            div(class = "intro-centered",
                "Regression")
          ),
          back = div(
            class = "intro-description",
            "The regression plot and histogram visualise the relationship between two variables (among GDP per capita growth, employment growth, industry migration or skill migration) and their marginal distributions."
          ),
          trigger = "hover",
          width = 4
        ),
        flipBox(
          id = "Scatter",
          front = div(
            style = "positive: relative;",
            img(
              src = "img/scatterplot.png",
              width = "100%",
              height = "100%"
            ),
            div(class = "intro-centered",
                "Scatter Plot")
          ),
          back = div(
            class = "intro-description",
            "The interactive scatter plot visualises the values for two variables (among GDP per capita growth, employment growth, industry migration or skill migration) and is coloured by year."
          ),
          trigger = "hover",
          width = 4
        ),
        flipBox(
          id = "Correlation",
          front = div(
            style = "positive: relative;",
            img(
              src = "img/corrmatrix.png",
              width = "100%",
              height = "100%"
            ),
            div(class = "intro-centered",
                "Correlation Matrix")
          ),
          back = div(
            class = "intro-description",
            "The correlation matrix visualises the strength of relationship between pairs of variables. Four variables are used i.e. GDP per capita growth, employment growth, industry migration and skill migration."
          ),
          trigger = "hover",
          width = 4
        )
      ),
      fluidRow(
        style = "padding-bottom: 20px;",
        flipBox(
          id = "Choropleth",
          front = div(
            style = "positive: relative;",
            img(
              src = "img/choropleth.png",
              width = "100%",
              height = "100%"
            ),
            div(class = "intro-centered",
                "Choropleth Map")
          ),
          back = div(
            class = "intro-description",
            "The choropleth visualisation is a thematic map where countries are coloured by country, industry or skill migration values."
          ),
          trigger = "hover",
          width = 4
        ),
        flipBox(
          id = "Chord",
          front = div(
            style = "positive: relative;",
            img(
              src = "img/chorddiagram.png",
              width = "100%",
              height = "100%"
            ),
            div(class = "intro-centered",
                "Chord Diagram")
          ),
          back = div(
            class = "intro-description",
            "The chord diagram is a graphical method of displaying the inter-relationships between data in a matrix. The data are arranged radially around a circle with the relationships between the data points"
          ),
          trigger = "hover",
          width = 4
        ),
        flipBox(
          id = "Slope",
          front = div(
            style = "positive: relative;",
            img(
              src = "img/slopegraph.png",
              width = "100%",
              height = "100%"
            ),
            div(class = "intro-centered",
                "Slope Graph")
          ),
          back = div(
            class = "intro-description",
            "The slope graph allows users to compare changes usually over time for a list of categorical variables. The change in country, industry or skill migration for various countries over the years are shown."
          ),
          trigger = "hover",
          width = 4
        )
      ),
      fluidRow(
        style = "padding-bottom: 20px;",
        flipBox(
          id = "Treemap",
          front = div(
            style = "positive: relative;",
            img(
              src = "img/treemap.png",
              width = "100%",
              height = "100%"
            ),
            div(class = "intro-centered",
                "Treemap")
          ),
          back = div(
            class = "intro-description",
            "The treemap displays hierarchical data as a set of nested rectangles. Each group is represented by a rectangle, which area is proportional to its value. It shows the net country, industry or skill migration by the colour of the rectangles, and either the population or GDP per Capita by the size of the rectangles. The rectangles can be nested by region or income group of the countries, and clicking on a rectangle will show the individual countries within that rectangle."
          ),
          trigger = "hover",
          width = 4
        ),
        flipBox(
          id = "Geofacet",
          front = div(
            style = "positive: relative;",
            img(
              src = "img/geofacet.png",
              width = "100%",
              height = "100%"
            ),
            div(class = "intro-centered",
                "Geofacet")
          ),
          back = div(
            class = "intro-description",
            "The geofacet plot takes data representing different geographic entities and apply a visualisation method to the data for each entity, with the resulting set of visualisations being laid out in a grid that mimics the original geographic topology as closely as possible. It shows the net country, industry or skill migration. If it is based on industry or skill migration and multiple industries or skills are selected, bar charts will be shown for each country."
          ),
          trigger = "hover",
          width = 4
        ),
        box(
          id = "Authors",
          title = "Authors",
          width = 4,
          solidHeader = TRUE,
          div(
            class = "intro-description",
            div(
              span(class = "intro-author-person", "Amos Lau"),
              span(
                a(
                  href = "https://www.linkedin.com/in/amos-lau/",
                  target = "_blank",
                  title = "LinkedIn",
                  icon("linkedin")
                ),
                a(
                  href = "https://github.com/moomookau/",
                  target = "_blank",
                  title = "Github",
                  icon("github")
                ),
                a(
                  href = "https://va.moomookau.org/",
                  target = "_blank",
                  title = "Blog",
                  icon("blog")
                )
              )
            ),
            div(
              span(class = "intro-author-person", "Cheryl Pay"),
              span(
                a(href = "#",
                  #target = "_blank",
                  title = "LinkedIn",
                  icon("linkedin")),
                a(href = "#",
                  #target = "_blank",
                  title = "Github",
                  icon("github")),
                a(href = "#",
                  #target = "_blank",
                  title = "Blog",
                  icon("blog"))
              )
            ),
            div(
              span(class = "intro-author-person", "Louis Chong"),
              span(
                a(href = "#",
                  #target = "_blank",
                  title = "LinkedIn",
                  icon("linkedin")),
                a(href = "#",
                  #target = "_blank",
                  title = "Github",
                  icon("github")),
                a(href = "#",
                  #target = "_blank",
                  title = "Blog",
                  icon("blog"))
              )
            ),
            hr(),
            div("Advisor: Prof Kam Tin Seong"),
          )
        )
      )
    ),
  ))
}