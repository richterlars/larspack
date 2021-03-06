% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chemical_structure_search_PDB.R,
%   R/proteinname_from_PDB.R
\name{get_pdbFromMol}
\alias{get_pdbFromMol}
\alias{get_PDB_proteinname}
\title{Chemical Structure search against PDB}
\usage{
get_pdbFromMol(smiles, exactSearch = FALSE, tanimoto_treshold = 0.9)

get_PDB_proteinname(pdb_ids)
}
\arguments{
\item{smiles}{SMILES string of type character}

\item{pdb}{character vector fo 4-letter PDB codes}
}
\value{
character vector of PDB codes

character vector of protein names
}
\description{
Chemical Structure search against PDB

Get protein name from PDB id
}
\section{Functions}{
\itemize{
\item \code{get_pdbFromMol}: PDB Web API to search PDB complexes with share similarity with input ligand
https://www.rcsb.org/pdb/software/rest.do#descEntity

\item \code{get_PDB_proteinname}: PDB Web API to search protein names of PDB
https://www.rcsb.org/pdb/software/rest.do#descEntity
}}

\examples{
get_pdb_id("CC(=O)OC1=CC=CC=C1C(=O)O", 0.9)
"1OXR" "1TGM" "2QQT" "3FCQ" "3GCL" "3IAZ" "4NSB"

get_pdb_proteinname("1TGM")
"Basic phospholipase A2 VRV-PL-VIIIa"

}
