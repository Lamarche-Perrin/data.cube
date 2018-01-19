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
    titlePanel ("Outlier Explorer"),
    fluidRow (
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
        column (3,
                h4 ("Normalise data"),
                conditionalPanel (condition="input['user.selection'] != 'none'", checkboxInput ("user.normalisation", label="By users", value=FALSE)),
                conditionalPanel (condition="input['topic.selection'] != 'none'", checkboxInput ("topic.normalisation", label="By topics", value=FALSE)),
                conditionalPanel (condition="input['time.selection'] != 'none'", checkboxInput ("time.normalisation", label="By dates", value=FALSE)),
                radioButtons ("deviation.type", label=h4("Statistical test"), inline=TRUE,
                              choices=c("Poisson test"="poisson", "KL Divergence"="KLdiv")),
                numericInput ("outlier.threshold", label=h4("Outlier threshold"), 3, min=1, step=1),
                checkboxInput ("outlier.labels", label="Display outlier labels", value=FALSE)
                ),
        column (3,
                numericInput ("min.obs", label=h4("Filter data (min value)"), 1, min=0, step=1)
                )
    ),
    hr (),
    mainPanel (
        tabsetPanel (type="tabs",
                     tabPanel ("Data structure", verbatimTextOutput (outputId="data.structure")),
                     tabPanel ("Data plot", plotOutput (outputId="data.plot")),
                     tabPanel ("Outlier plot",
                               fluidRow (
                                   column (8, plotOutput (outputId="outlier.plot", width="100%")),
                                   column (4, plotOutput (outputId="distribution.plot", width="100%"))
                               )
                               ),
                     tabPanel ("Outlier list",
                               verbatimTextOutput (outputId="positive.outlier.list"),
                               verbatimTextOutput (outputId="negative.outlier.list")
                               )
                     )
    )
)
