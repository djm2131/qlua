
c-----------------------------------------------------------------------
*  Routine:    finilog
*
*  Purpose:    initialize a file for log output
*
*  Usage:      CALL finilog (lf)
*
*  Arguments
*
*-----------------------------------------------------------------------
      subroutine finilog( lf )
      integer   lf
      
      close( lf )

      return 
      end
