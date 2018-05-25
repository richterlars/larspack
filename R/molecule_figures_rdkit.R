
#' Plot 2D structure based on SMILES string
#'
#' @param smiles
#' @param filenpath path to file
#' @param plot logical, if TRUE the molecule will be directly plot
#' @param size size of the figure, default is 600
#'
#' @return a png figure of the molecule, either saved to disk or directly plot
#' @export
#'
#' @examples
#'
#' #You need to set anaconda environment prior to running RDKit code
#' reticulate::use_condaenv(condaenv = "my-rdkit-env", conda = "/Users/lrichter/miniconda3/bin/conda")
#'
#' #draws molecule directly into plot pane
#' draw_mol("C[C@@]1([C@H]2C[C@H]3[C@@H](C(=O)C(=C([C@]3(C(=O)C2=C(C4=C1C=CC=C4O)O)O)O)C(=O)N)N(C)C)O")
#'
#' in order to crop the white space around pictures ImageMagick can be used.
#' the command in windows is >magick mogrify -trim *.png
draw_mol <- function(smiles, filepath = NA, plot = TRUE, size = 600) {

  if (is.na(filepath)) {
    filepath <- tempfile("plot", fileext = ".png")
  }

  Chem <- reticulate::import("rdkit.Chem")
  Draw <- reticulate::import("rdkit.Chem.Draw")

  mol <- Chem$MolFromSmiles(smiles)
  Draw$MolToImageFile(mol, filepath, size = list(size, size))

  if (plot) {
    img <- png::readPNG(filepath)
    grid::grid.raster(img)
  }
}

#' Plot a 2D grid of figures
#'
#' @param smiles atomic character vector of canonical smiles
#' @param labels atomic character vector for molecule labels
#' @param filepath path to file
#' @param plot logical, if TRUE the molecule will be directly plot
#' @param size size of the figure, default is 600
#'
#' @return a png figure of the molecule, either saved to disk or directly plot
#' @export
#'
#' @examples
#'
makeFigTable <- function(smiles, labels = NA, filepath = NA, plot = TRUE, size = 600) {
  n <- length(smiles)

  if (is.na(filepath)) {
    filepath <- tempfile("plot", fileext = ".png")
  }

  if (is.na(labels)) {
    labels = as.character(1:n)
  }

  Chem <- reticulate::import("rdkit.Chem")
  Draw <- reticulate::import("rdkit.Chem.Draw")

  mols <- purrr::map(.x = smiles, .f = ~ Chem$MolFromSmiles(.x))

  print(mols)

  for (i in 1:n) {
    mols[[i]]$SetProp("_Name",labels[[i]])
  }

  img <- Draw$MolsToGridImage(mols, molsPerRow = min(length(mols)), legends = as.list(labels))
  img$save(filepath)

  if (plot) {
    img <- png::readPNG(filepath)
    grid::grid.raster(img)
  }

}
