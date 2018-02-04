All fortran unittest programs using fruit should have the rakefile cp
fruit.f90 to the location of the rakefile.  There is no need to copy it
manually from here nor to include it specifically in the repository there.

All fortran programs should have its own local copy of set_precision.f90 copied
from here to the program's source folder.  The precision of the wp
variable can be specified for each program.
