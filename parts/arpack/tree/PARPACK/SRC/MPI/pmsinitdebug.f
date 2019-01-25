c      set COMMON variables for debug from Fortran
c      to avoid cross-compiler alignment issues
      subroutine pmsinitdebug
     &  ( logfil1, 
     &    msaupd1, msaup21, msaitr1, mseigh1, msapps1, msgets1, mseupd1)
c      
      integer    msaupd1, msaup21, msaitr1, 
     &    mseigh1, msapps1, msgets1, mseupd1
c
      include   'debug.h'
c
      logfil = logfil1
      msaupd = msaupd1
      msaup2 = msaup21
      msaitr = msaitr1
      mseigh = mseigh1
      msapps = msapps1
      msgets = msgets1
      mseupd = mseupd1

      return 
      end

