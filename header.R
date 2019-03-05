library(shiny)
library(ggplot2)
library(FNN)
library(DT)
library(munsell)

source("rock_geom.R")

load("mds_30.RData")
load("mds_360.RData")

load("similarity_30.RData")
load("similarity_360.RData")

scale = .01
size = 700

df = data.frame(x=1:160, y=1:160)

subset = c(6, 14, 34, 39, 59, 69, 73, 95, 102, 119,
           127, 144, 151, 164, 172, 188, 201, 213, 218, 237,
           250, 263, 267, 284, 300, 309, 314, 336, 348, 352)

dimensions = list("MDS Dimension 1" = 1, 
                  "MDS Dimension 2" = 2,
                  "MDS Dimension 3" = 3, 
                  "MDS Dimension 4" = 4,
                  "MDS Dimension 5" = 5,
                  "MDS Dimension 6" = 6,
                  "MDS Dimension 7" = 7,
                  "MDS Dimension 8" = 8,
                  "Lightness of Color" = 14,
                  "Average Grain Size" = 15,
                  "Roughness" = 16,
                  "Shininess" = 17,
                  "Organization" = 18,
                  "Variability of Color" = 19,
                  "Presence of Visible Grain" = 20,
                  "Presence of Fragments" = 21,
                  "Presence of Stripes or Bands" = 22,
                  "Presence of Holes" = 23,
                  "Presence of Physical Layers" = 24,
                  "Presence of Salient Special Features" = 25,
                  "Variability of Grain Size" = 26,
                  "Angular/Rounded Fragments" = 27,
                  "Straight/Curved Stripes" = 28,
                  "Munsell Value" = 29,
                  "Munsell Chroma" = 30,
                  "x'" = 32,
                  "y'" = 33)

cols = as.vector(c(9, 10, 11, unlist(dimensions)))

rocks =    c("Andesite", 
             "Basalt", 
             "Diorite",
             "Gabbro",
             "Granite",
             "Obsidian",
             "Pegmatite",
             "Peridotite",
             "Pumice",
             "Rhyolite",
             "Amphibolite",
             "Anthracite",
             "Gneiss",
             "Hornfels",
             "Marble",
             "Migmatite",
             "Phyllite",
             "Quartzite",
             "Schist",
             "Slate",
             "Bituminous Coal",
             "Breccia",
             "Chert",
             "Conglomerate",
             "Dolomite",
             "Micrite",
             "Rock Gypsum",
             "Rock Salt",
             "Sandstone",
             "Shale")

empty = data.frame(matrix(nrow=1, ncol=32))
colnames(empty) = colnames(mds_30)[c(13, 34, cols)]
empty = t(empty)

mds_label = "mds_plotter_"
ratings_label = "ratings_plotter_"

nearestEven = Vectorize(function(n){
  x = round(n / 2)
  x = max(c(x, 1))
  return(2 * x)
})

hovertext = "var tips = ['',
                        'The image of the rock.', 
                        'The Munsell Chip that most closely matches the rock\\'s averaged color matching data (see paper for details on how the color matching data was analyzed). The label of each chip takes the form \\\"Hue Value / Chroma.\\\"',
                        'The high level category of the rock. A rock may be either igneous, metamorphic, or sedimentary.',
                        'The subtype of the rock. There are 10 subtypes in each category.',
                        'The token number of the rock. There are 12 tokens in each subtype in an arbitrary order.',
                        'The coordinate along the first dimension in the 8-dimensional MDS solution derived from the similarity ratings.',
                        'The coordinate along the second dimension in the 8-dimensional MDS solution derived from the similarity ratings.',
                        'The coordinate along the third dimension in the 8-dimensional MDS solution derived from the similarity ratings.',
                        'The coordinate along the fourth dimension in the 8-dimensional MDS solution derived from the similarity ratings.',
                        'The coordinate along the fifth dimension in the 8-dimensional MDS solution derived from the similarity ratings.',
                        'The coordinate along the sixth dimension in the 8-dimensional MDS solution derived from the similarity ratings.',
                        'The coordinate along the seventh dimension in the 8-dimensional MDS solution derived from the similarity ratings.',
                        'The coordinate along the eighth dimension in the 8-dimensional MDS solution derived from the similarity ratings.',
                        'The average rating of lightness of color, where low values indicate very dark colors, and high colors indicate very light colors.',
                        'The average rating of average grain size, where low values indicate no visible grain, and high values indicate a very coarse grain.',
                        'The average rating of roughness, where low values indicate a very smooth texutre, and high values indicate a very rough texture.',
                        'The average rating of shininess, where low values indicate a dull very dull rock, and high values indicate a very shiny rock.',
                        'The average rating of organization, where low values indicate that the rock is composed of fragments glued together in a haphazard fashion, and high values indicate that the rock is organized into systematic layers, bands, or grains.',
                        'The average rating of variablity of color, where low values indicate no variation, and high values indicate high variation.',
                        'The proportion of participants that judged the rock to have a visible grain.',
                        'The proportion of participants that judged the rock to have fragments.',
                        'The proportion of participants that judged the rock to have stripes or bands.',
                        'The proportion of participants that judged the rock to have holes.',
                        'The proportion of participants that judged the rock to have physical layers.',
                        'The proportion of participants that judged the rock to have a salient special feature.',
                        'The average rating of variablility of grain size conditioned on the presence of a visible grain (see paper for details), where low values indicate low variability, and high values indicate high variability.',
                        'The extent to which the rock has angular or rounded fragments, conditioned on the presence of fragments (see paper for details), where low values indicate angular fragments, and high values indicate rounded fragments.',
                        'The extent to which the rock has straight or curved stripes, conditioned on the presence of stripes (see paper for details), where low values indicate straight stripes, and high values indicate curved stripes.',
                        'The average Munsell Value, derived from the color matching data (see paper for details on how the color matching data was analyzed).',
                        'The average Munsell Chroma, derived from the color matching data (see paper for details on how the color matching data was analyzed).',
                        'The average x value derived from the color matching data. Each x\\',y\\' pair defines a Munsell Hue (see paper for details on how the color matching data was analyzed).',
                        'The average y value derived from the color matching data. Each x\\',y\\' pair defines a Munsell Hue (see paper for details on how the color matching data was analyzed).'
                        ],
                        header = table.columns().header();
                        for (var i = 0; i < tips.length; i++) {
                        $(header[i]).attr('title', tips[i]);
                        }"
                        