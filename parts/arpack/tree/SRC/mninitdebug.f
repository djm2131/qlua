c      set COMMON variables for debug from Fortran
c      to avoid cross-compiler alignment issues
      subroutine mninitdebug
     &  ( logfil1, 
     &    mnaupd1, mnaup21, mnaitr1, mneigh1, mnapps1, mngets1, mneupd1)
c      
      integer    mnaupd1, mnaup21, mnaitr1, 
     &    mneigh1, mnapps1, mngets1, mneupd1
c
      include   'debug.h'
c
      logfil = logfil1
      mnaupd = mnaupd1
      mnaup2 = mnaup21
      mnaitr = mnaitr1
      mneigh = mneigh1
      mnapps = mnapps1
      mngets = mngets1
      mneupd = mneupd1

      return 
      end

