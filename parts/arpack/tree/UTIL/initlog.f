c-----------------------------------------------------------------------
*  Routine:    initlog
*
*  Purpose:    initialize a file for log output
*
*  Usage:      CALL initlog (LOGFIL, FNAME)
*
*  Arguments
*     LF      - FORTRAN unit number  (Input)
*     FNAME   - file name to open for writing (Output)
*
*-----------------------------------------------------------------------
      subroutine initlog( lf, fname )
      
      integer            lf
      character          fname*(*)
      logical            exist

      open( lf, FILE=fname, STATUS='UNKNOWN' )

      return 
      end
