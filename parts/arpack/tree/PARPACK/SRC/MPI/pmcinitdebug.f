c      set COMMON variables for debug from Fortran
c      to avoid cross-compiler alignment issues
      subroutine pmcinitdebug
     &  ( logfil1, 
     &    mcaupd1, mcaup21, mcaitr1, mceigh1, mcapps1, mcgets1, mceupd1)
c      
      integer    mcaupd1, mcaup21, mcaitr1, 
     &    mceigh1, mcapps1, mcgets1, mceupd1
c
      include   'debug.h'
c
      logfil = logfil1
      mcaupd = mcaupd1
      mcaup2 = mcaup21
      mcaitr = mcaitr1
      mceigh = mceigh1
      mcapps = mcapps1
      mcgets = mcgets1
      mceupd = mceupd1

      return 
      end
