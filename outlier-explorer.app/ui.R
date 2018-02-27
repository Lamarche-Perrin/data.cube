## This file is part of data.cube.
##
## data.cube is an R package for the easy processing of multidimentional
## data. It has been developed by researchers of the Complex Networks team,
## within the Computer Science Laboratory of Paris 6 (LIP6), for the
## ODYCCEUS project, founded by the European Commission FETPROACT 2016-2017
## program under grant 732942.
## 
## Copyright © 2017 Robin Lamarche-Perrin (<Robin.Lamarche-Perrin@lip6.fr>)
## 
## data.cube is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation, either version 3 of the License, or (at your
## option) any later version.
## 
## data.cube is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
## more details.
## 
## You should have received a copy of the GNU General Public License along
## with this program. If not, see <http://www.gnu.org/licenses/>.

library ('shiny')

fluidPage (
    fluidRow (
        column (10,
                titlePanel (div (uiOutput ("app.title"), style="margin: 20px; font-weight: bold;"), windowTitle="Outlier Explorer"),
                fluidRow (
                    column (3,
                            wellPanel (
                                radioButtons ("dataset", label=h4("Select dataset"),
                                              choices=c(
                                                  "Guardian Comments (2016)"="guardian.2016",
                                                  "Twitter Politics (EU)"="twitter.eu",
                                                  "Twitter Politics (FR)"="twitter.fr",
                                                  "Opinion Model"="opinion.traces"#,
                                                  #"IP Traffic MAWI"="mawi.sec"
                                              ), selected=character(0))
                            ),
                            conditionalPanel (condition="$('html').hasClass('shiny-busy')", div (img (src="images/busy2.gif", height=90), style="text-align: center;"))
                            ),
                    column (4,
                            conditionalPanel (
                                condition="(typeof input.dataset !== 'undefined' && input.dataset.length > 0)",
                                wellPanel (
                                    uiOutput ("user.buttons"),
                                    uiOutput ("user.list"),
                                    uiOutput ("user.slider"),
                                    uiOutput ("topic.buttons"),
                                    uiOutput ("topic.list"), 
                                    uiOutput ("topic.slider"),
                                    uiOutput ("time.buttons"),
                                    uiOutput ("time.list"),
                                    uiOutput ("time.slider")
                                )
                            )
                            ),
                    column (3, uiOutput ("input.panel.2")),
                    column (2, uiOutput ("input.panel.3"), uiOutput ("input.panel.4"))
                ),
                hr ()
                ),
        column (2,
                wellPanel (
                    div (img (src='images/odycceus_logo.png', height=80), style="text-align: center; padding: 5px; margin-bottom: 10px;"),
                    tagList ("This application has been developed by the ", a ("Complex Networks", href="http://www.complexnetworks.fr/", target="_blank"), " team at ", a ("LIP6", href="https://www.lip6.fr/", target="_blank"), " / ", a ("UPMC", href="https://www.sorbonne-universite.fr/", target="_blank"), " / ", a ("CNRS", href="http://www.cnrs.fr/", target="_blank"), " for the ", a ("ODYCCEUS", href="https://www.odycceus.eu/", target="_blank"), " project founded by the ", a ("H2020 FETPROACT", href="https://ec.europa.eu/research/participants/portal/desktop/en/opportunities/h2020/calls/h2020-fetproact-2016-2017.html", target="_blank"), " program of the European Commission. Sources are open and available on ", a ("GitHub", href="https://github.com/Lamarche-Perrin/data.cube", target="_blank"), "."),
                    style="padding: 20px; margin: 5px; font-size: 90%; text-align: justify;")
                )
    ),

    conditionalPanel(
        condition="(typeof input.dataset !== 'undefined' && input.dataset.length > 0)",
        mainPanel (
            tabsetPanel (type="tabs",
                         tabPanel ("Data structure", verbatimTextOutput (outputId="data.structure")),
                         tabPanel ("Data plot",
                                   downloadButton (outputId="download.data.plot.pdf", label="Download plot (pdf)"),
                                   downloadButton (outputId="download.data.plot.png", label="Download plot (png)"),
                                   plotOutput (outputId="data.plot")),
                         tabPanel ("Outlier plot",
                                   downloadButton (outputId="download.outlier.plot.pdf", label="Download plot (pdf)"),                                   
                                   downloadButton (outputId="download.outlier.plot.png", label="Download plot (png)"),                                   
                                   fluidRow (
                                       column (9, plotOutput (outputId="outlier.plot")),
                                       column (3, plotOutput (outputId="distribution.plot"))
                                   )
                                   ),
                         tabPanel ("Outlier lists",
                                   fluidRow (
                                       column (6,
                                               downloadButton (outputId="download.positive.outlier.list.csv", label="Download list (csv)"),
                                               downloadButton (outputId="download.positive.outlier.list.json", label="Download list (json)"),
                                               dataTableOutput (outputId="positive.outlier.list")),
                                       column (6,
                                               downloadButton (outputId="download.negative.outlier.list.csv", label="Download list (csv)"),
                                               downloadButton (outputId="download.negative.outlier.list.json", label="Download list (json)"),
                                               dataTableOutput (outputId="negative.outlier.list"))
                                   )
                                   )
                         ), style="width: 100%;"
        )
    )
)
