      subroutine graph ( xpt, ypt, npt, x, y, pts )


****************************************************************************
*
*  P G P L O T    G R A P H I N G   R O U T I N E
*
*  Dr. Andrew J. Pounds  
*  Department of Chemistry 
*  Mercer University 
*
*  pounds_aj@mercer.edu
*
*  This routine takes on input real arrays x and y containing pts
*  points and graphs them on and x-y coordinate system.
*
*  Compilation directions on Cobra:
*
*    gfortran program.f /usr/local/pgplot/libpgplot.a /usr/X11R6/lib/libX11.a
*
****************************************************************************

      integer dev, npt, pts, PGOPEN, grdlin, symbol
      real ratio
      real x(*), y(*)
      real xpt(*), ypt(*)
      real minx, maxx, miny, maxy
      character*60 xlabel, ylabel, title  
      

*     call gminmax (pts, x, y, minx, maxx, miny, maxy )
      call gminmax (npt, xpt, ypt, minx, maxx, miny, maxy )

C Scale plot by 5%

      ymax = ymax * 1.05

      print *, 'Please enter a title for the plot.'
      read '(A)', title
      print *, 'Please enter a x-axis label for the plot.'
      read '(A)', xlabel 
      print *, 'Please enter a y-axis label for the plot.'
      read '(A)', ylabel 
      print *, 'Do you want grid lines included? (1=no, 2=yes)' 
      read *, grdlin
C
C Call PGOPEN to initiate PGPLOT and open the output device; PGOPEN
C will prompt the user to supply the device name and type. Always
C check the return code from PGOPEN.
C
      IF (PGOPEN('?') .LE. 0) STOP
C
C Call the demonstration subroutines (4,5 are put on one page)
C
      call PGEX0

C Call PGENV to specify the range of the axes and to draw a box, and
C PGLAB to label it. The x-axis runs from 0 to 10, and y from 0 to 20.
C
      CALL PGENV(minx,maxx,miny,maxy,0,grdlin)
      CALL PGLAB(xlabel, ylabel, title)
C
      CALL PGLINE(pts,x,y)
      CALL PGPT(npt,xpt,ypt,10)
C-----------------------------------------------------------------------

      CALL PGCLOS

      END
 

      subroutine gminmax ( pts, x, y, minx, maxx, miny, maxy )

      integer pts
      real x(*), y(*)
      real minx, maxx, miny, maxy

      minx = x(1)
      maxx = x(1)
      miny = y(1)
      maxy = y(1)

      do 10 i = 2, pts
         minx = min(minx,x(i))
         maxx = max(maxx,x(i))
         miny = min(miny,y(i))
         maxy = max(maxy,y(i))
10    continue

      end


      SUBROUTINE PGEX0
C-----------------------------------------------------------------------
C This subroutine tests PGQINF and displays the information returned on
C the standard output.
C-----------------------------------------------------------------------
      CHARACTER*64 VALUE
      INTEGER LENGTH
      REAL X, Y, X1, X2, Y1, Y2
C
C Information available from PGQINF:
C
      CALL PGQINF('version',  VALUE, LENGTH)
      WRITE (*,*) 'version=', VALUE(:LENGTH)
      CALL PGQINF('state',    VALUE, LENGTH)
      WRITE (*,*) 'state=',   VALUE(:LENGTH)
      CALL PGQINF('user',     VALUE, LENGTH)
      WRITE (*,*) 'user=',    VALUE(:LENGTH)
      CALL PGQINF('now',      VALUE, LENGTH)
      WRITE (*,*) 'now=',     VALUE(:LENGTH)
      CALL PGQINF('device',   VALUE, LENGTH)
      WRITE (*,*) 'device=',  VALUE(:LENGTH)
      CALL PGQINF('file',     VALUE, LENGTH)
      WRITE (*,*) 'file=',    VALUE(:LENGTH)
      CALL PGQINF('type',     VALUE, LENGTH)
      WRITE (*,*) 'type=',    VALUE(:LENGTH)
      CALL PGQINF('dev/type', VALUE, LENGTH)
      WRITE (*,*) 'dev/type=',VALUE(:LENGTH)
      CALL PGQINF('hardcopy', VALUE, LENGTH)
      WRITE (*,*) 'hardcopy=',VALUE(:LENGTH)
      CALL PGQINF('terminal', VALUE, LENGTH)
      WRITE (*,*) 'terminal=',VALUE(:LENGTH)
      CALL PGQINF('cursor',   VALUE, LENGTH)
      WRITE (*,*) 'cursor=',  VALUE(:LENGTH)
C
C Get view surface dimensions:
C
      CALL PGQVSZ(1, X1, X2, Y1, Y2)
      X = X2-X1
      Y = Y2-Y1
      WRITE (*,100) X, Y, X*25.4, Y*25.4
  100 FORMAT (' Plot dimensions (x,y; inches): ',F9.2,', ',F9.2/
     1        '                          (mm): ',F9.2,', ',F9.2)
C-----------------------------------------------------------------------
      END
