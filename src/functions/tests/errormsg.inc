!-------------------------------------------------------------------------------
!   Jared O'Neal - Personal Library of Unnecessary Code 
!-------------------------------------------------------------------------------
!
! DESCRIPTION:
!> @brief
!> Upon detecting an error, modules in TechCode/lib will use the ERRORMSG
!> pre-processor macro defined in a project's errormsg.inc file to communicate
!> an error to users and then halt program execution.\N
!> The following must be done to use TechCode/lib modules
!> 1) Copy TechCode/lib/general/errormsg.inc_orig to project folder
!> 2) Rename to errormsg.inc 
!
!> @author Jared O'Neal 
!
! TODO:
! Nothing 
!
! SOURCE:
!> http://stackoverflow.com/questions/8508590/standard-input-and-output-units-in-fortran-90
!> http://stackoverflow.com/questions/7033396/fortan-query-and-print-out-function-or-subroutine-name
!
!-------------------------------------------------------------------------------

#ifndef stderr
#ifdef f2003
use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
#else
#define stderr 0
#endif
#endif

#ifndef stdout
#ifdef f2003
use, intrinsic :: iso_fortran_env, only : stdout=>output_unit
#else
#define stdout 6
#endif
#endif

#define WARNINGMSG(msg) write(stdout, '(A,A)') "WARNING - ", msg
#define ERRORMSG(msg)   write(stderr, '(A,A)') "ERROR - ",   msg

