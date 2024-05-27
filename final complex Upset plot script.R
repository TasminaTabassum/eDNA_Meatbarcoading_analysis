library(ggplot2)

library(ComplexUpset)


# Read the Excel file
documents_path <- "/Users/jamilshuvo/Desktop/Class_with_others.xlsx"
data_raw <- read_excel(documents_path)

# Convert columns to binary (0 for 0, 1 for any other value)
data_raw <- data_raw %>%
  mutate_at(vars(SDS, LDS, SS, SDBR), list(~ifelse(. == 0, 0, 1)))

#Remove rows with zero values in all columns
data_filtered <- subset(data_raw, !(SDS == 0 & LDS == 0 & SS == 0 & SDBR == 0))


# specify the culumn
S = colnames(data_filtered)[3:6]
S
data_filtered [S] = data_filtered [S] == 1
t(head(data_filtered [S], 1032))

# Specify the colored culumn 
data_filtered[data_filtered $Class == '', 'Class'] = NA
data_filtered  = na.omit(data_filtered )

# set function specification
set_size = function(w, h, factor=1.5) {
  s = 1 * factor
  options(
    repr.plot.width=w * s,
    repr.plot.height=h * s,
    repr.plot.res=100 / factor,
    jupyter.plot_mimetypes='image/png',
    jupyter.plot_scale=1
  )
}

# Command for upset plot
upset(
  data_filtered, S,
  min_size=1,
  width_ratio=0.2,
  set_sizes=(
    upset_set_size(
      geom=geom_bar(
        aes(fill=Class, x=group),
        width=0.2
      ),
      position='right'
    )
  ),
  # moves legends over the set sizes
  guides='over'
)

##############################

set_size = function(w, h, factor=1.5) {
  s = 1 * factor
  options(
    repr.plot.width=w * s,
    repr.plot.height=h * s,
    repr.plot.res=100 / factor,
    jupyter.plot_mimetypes='image/png',
    jupyter.plot_scale=1
  )
}
set_size(8, 3)
upset(
  data_filtered, S,
  base_annotations=list(
    'Species'=intersection_size(
      text=list(
        vjust=0,
        hjust=0,
        angle=30
      ) ,
      mapping=aes(fill=Class)
    ) + scale_fill_manual(values=c(
      'Arachnida'='chocolate', 'Collembola'='coral4',
      'Insecta'='darkorange', 'Others'='lightgoldenrod1'))
  ),
  min_size=1,
  width_ratio=0.33
)


############

ggsave("upset_plot.svg", plot = upset_plot, device = "svg")


