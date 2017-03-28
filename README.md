# MDET - Multi-wavelength source detection software
For information on what the MDET algorithm is, take look at [Optimal Multiwavelength Source Detection: Experience Gained
from the WISE Mission (K. Marsh and T. Jarret)](MarshJarret2012.pdf).

# History
The MDET module was first written in 2008 for as part of the WISE catalog software pipeline at IPAC. Because the WISE computer cluster is occupied with ongoing satellite operations, we need to get the software running on independent systems not on the WISE cluster.
# Building and Compiling
MDET is written in the Fortran programming language, using the extend source (132 column characters) format. MDET's only dependency (besides the standard libraries) is the [FITSIO Subroutine Library](https://heasarc.gsfc.nasa.gov/fitsio/fitsio.html). The FITSIO (Fortran Libraries) are written to always be backwards compatible, so simply download the latest version and install it normally. MDET was originally compiled under the Intel Fortran compiler, but we will be focusing on GNU Fortran since its public and open-source.

The build commands we use for MDET are

`gfortran mdet.f -o mdet -m64 -O3 -ffixed-line-length-132 -L../cfitsio -lcfitsio`

Replace `-L../cfitsio` with the directory where you are keeping the CFITSIO library. Be sure to run this command in the same directory as the MDET source files. `mdet.f` is the main source file for MDET.
# Version Control
The source files were obtained from the latest build on the WISE cluster. To my knowledge, the only documentation and version history of the source is what is provided in its files. Therefore, this repo can only start its version history in the state where the source was obtained.
# Using MDET
[ToDo: Document usage]
