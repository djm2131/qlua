      SUBROUTINE SECOND( T )
*
      REAL       T
*
*  -- LAPACK auxiliary routine (preliminary version) --
*     patched by SNS, 2013, to compile with gfortran
*
*  Purpose
*  =======
*
*  SECOND returns the user time for a process in seconds.
*  This version gets the time from gfortran CPU_TIME.
*
      REAL               T1

      CALL CPU_TIME( T1 )
      T  = T1

      RETURN

      END
