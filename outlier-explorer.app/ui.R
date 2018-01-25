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
    uiOutput ("app.title"),
    fluidRow (
        column (3,
                radioButtons ("dataset", label=h4("Select dataset"),
                              choices=c(
                                  "Guardian Comments (2016)"="guardian.2016",
                                  "Twitter Politics (EU)"="twitter.eu",
                                  "Twitter Politics (FR)"="twitter.fr",
                                  "Opinion Model"="opinion.traces"
                              ), selected=character(0)),
                conditionalPanel (condition="$('html').hasClass('shiny-busy')", img (src="../images/busy2.gif", height = 90, width = 91))
                ),
        column (3,
                uiOutput ("user.buttons"),
                uiOutput ("user.list"),
                uiOutput ("user.slider"),
                uiOutput ("topic.buttons"),
                uiOutput ("topic.list"), 
                uiOutput ("topic.slider"),
                uiOutput ("time.buttons"),
                uiOutput ("time.list"),
                uiOutput ("time.slider")
                ),
        uiOutput ("column2"),
        uiOutput ("column3")
                
    ),
    hr (),
    conditionalPanel(
        condition = "(typeof input.dataset !== 'undefined' && input.dataset.length > 0)",
        mainPanel (
            tabsetPanel (type="tabs",
                         tabPanel ("Data structure", verbatimTextOutput (outputId="data.structure")),
                         tabPanel ("Data plot", plotOutput (outputId="data.plot")),
                         tabPanel ("Outlier plot",
                                   fluidRow (
                                       column (8, plotOutput (outputId="outlier.plot")),
                                       column (4, plotOutput (outputId="distribution.plot"))
                                   )
                                   ),
                         tabPanel ("Outlier list",
                                   verbatimTextOutput (outputId="positive.outlier.list"),
                                   verbatimTextOutput (outputId="negative.outlier.list")
                                   )
                         ), style="width: 100%;"
        )
    )
)
