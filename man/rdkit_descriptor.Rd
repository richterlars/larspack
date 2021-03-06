% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/descriptor_rdkit.R
\name{rdkit_descriptor}
\alias{rdkit_descriptor}
\title{Title}
\usage{
rdkit_descriptor(smiles, descriptor = "MolWt")
}
\arguments{
\item{smiles}{SMILES string of type character}

\item{descriptor}{c("BertzCT", "MolLogP", "MolMR", "MolWt", "NumHDonors", "NumHAcceptors", "NumRotatableBonds", "TPSA"}
}
\value{
value of the RDKit descriptor
}
\description{
Title
}
\examples{

#You need to set anaconda environment prior to running RDKit code
reticulate::use_condaenv(condaenv = "my-rdkit-env", conda = "/Users/lrichter/miniconda3/bin/conda")
reticulate::use_condaenv(condaenv = "my-rdkit-env", conda = "/Users/lrichter/miniconda3/bin/conda")

rdkit_descriptor("CCO", "MolWt")
}
