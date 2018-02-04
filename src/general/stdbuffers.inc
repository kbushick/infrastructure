!-------------------------------------------------------------------------------
!   Jared O'Neal - Personal Library of Unnecessary Code 
!-------------------------------------------------------------------------------
! DESCRIPTION:
!> @brief
!> File containing general use pre-processor commands for defining names for
!> standard input, standard output, and standard error.\n
!> To use these macro commands
!> 1) Follow information in TechCode/lib/HOWTO.txt to setup system for finding
!>    the library code within a Makefile or when compiling
!> 2) Top of .f90 files to use macros must include the line
!>         #include "stdbuffers.inc"
!> 3) Compile with preprocessor (-fpp for Intel compiler)
!
!> @author Jared O'Neal
!
! TODO:
! Nothing
!
! SOURCE:
!> http://stackoverflow.com/questions/8508590/standard-input-and-output-units-in-fortran-90
!
!-------------------------------------------------------------------------------

#ifndef stdin
#ifdef f2003
use, intrinsic :: iso_fortran_env, only : stdin=>input_unit
#else
#define stdin 5
#endif
#endif

#ifndef stdout
#ifdef f2003
use, intrinsic :: iso_fortran_env, only : stdout=>output_unit
#else
#define stdout 6
#endif
#endif

#ifndef stderr
#ifdef f2003
use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
#else
#define stderr 0
#endif
#endif

