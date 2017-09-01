!******************************************************************************
! generate simple flat plate and cylinder surface triangulated grid
! Written by Chonglin Zhang
! last updated 02/25/2014
!******************************************************************************
MODULE Constants
  IMPLICIT NONE
  SAVE
  INTEGER,PARAMETER :: DOUBLE = SELECTED_REAL_KIND(P=15,R=100)
  REAL*8, PARAMETER :: PI = 3.14159265358979323846264338d0
END MODULE Constants


!******************************************************************************
! Main program
! set the "iGrid_case" to generate flat plate, cylinder, double wedge or tick
! parameters in the program "grid_generation" can be adjusted to appropriate 
! values for different triangle sizes, and different geometry sizes.
!******************************************************************************
PROGRAM grid_generation
  USE Constants
  IMPLICIT NONE

  INTEGER :: iGrid_case

  !-----------------------------------------------------------------------------------------
  ! parameters for the flat plate
  REAL(DOUBLE) :: lx, ly, lz, alpha 
  ! the coordinate of the flat plate's upper left point, acting as reference 
  ! point
  REAL(DOUBLE) :: xa, ya, za
  ! number of rectangle in length and width directions, note each rectangle is
  ! divided into 2 triangles in the final grid
  INTEGER :: iNumX, iNumZ

  !----------------------------------------------------------------------------------------- 
  ! parameter for the cylinder
  REAL(DOUBLE) :: length, radius
  ! the coordinate for the center of the cylinder's base, acting as reference 
  ! point
  REAL(DOUBLE) :: xc, yc, zc  
  ! number of rectangle in the length and circumferencial directions, note each 
  ! rectangle is divided into 2 triangles in the final grid
  INTEGER :: iNumLD, iNumRD  

  !-----------------------------------------------------------------------------------------
  !parameters for the double-wedge
  REAL(DOUBLE) :: theta, Lfor, Hfor, Laft, Haft, b, Ltop, Ltot, Htot !also alpha
  ! coordinates of the double-wedge's front left corner (of the forward wedge face)
  !  REAL(DOUBLE) :: xa, ya, za
  ! number of rectangle in flow-wise and width directions, note each rectangle is
  ! divided into 2 triangles in the final grid
  INTEGER :: iNumfor, iNumaft, iNumtop, iNumback, iNumbottom  !also iNumZ

  !-----------------------------------------------------------------------------------------
  !parameters for the tick configuration
  REAL(DOUBLE) :: beta, len_one, len_two, len_three, len_four, len_five, lb1, lb2
  REAL(DOUBLE) :: lback, hback, hb1, hb2, lt1, lt2, ht1, ht2
  INTEGER :: iNumbfor, iNumbaft !number of rectangles on bottom sections
  REAL(DOUBLE) :: hypotenuse, y, w, v, x, z, phi, pho


  !-----------------------------------------------------------------------------------------
  !parameters for the cylinder-flare configuration
  REAL*8  :: L_cyl, L_cone, Alpha_c, R_cyl, xa_c, Theta_c, l_slant, theta_slant
  INTEGER  :: N1p, N_xslant, N_xcyl, N_xcone, N_theta
  Character(LEN=40) :: string, plane_name
  !-----------------------------------------------------------------------------------------

  ! grid file name
  CHARACTER(LEN=40) :: cFileName_Grid


  ! select the grid you want to generate
  ! 1: flat plate; 2: cylinder; 3: double-wedge; 4: tick; 6: cylinder-flare
  print*, 'Enter grid_case number'
  read*, iGrid_case 
  !iGrid_case = 3

  SELECT CASE(iGrid_case)
  CASE(1)    
     ! set the length, width, and depth(height) of the flat plate (lx, ly, lz)
     ! with sharp leading edge; the leading edge angle is alpha.
     lx = 1.0d0  ! length
     ly = 0.05d0 ! height
     lz = 1.0d0  ! width
     alpha = 30.0d0  ! angle in degree 
     xa = 0.0d0
     ya = 0.0d0
     za = 0.0d0
     iNumX = 50    ! num of rectangles in length direction
     iNumZ = 30    ! num of rectangles in width direction 
     cFileName_Grid = "flat_plate_grid.stl"
     CALL generate_flat_plate(lx,ly,lz,alpha,xa,ya,za,iNumX,iNumZ,cFileName_Grid)
  CASE(2)
     ! set the length and radius of the cylinder
     length = 0.1d0
     radius = 1.0d0
     xc = 0.0d0
     yc = 0.0d0
     zc = 0.0d0
     iNumLD = 1   ! num of rectangles in length direction
     iNumRD = 1000   ! num of rectangles in circumferencial direction
     cFileName_Grid = "cylinder_grid.stl"
     CALL generate_cylinder(length,radius,xc,yc,zc,iNumLD,iNumRD,cFileName_Grid)
  CASE(3)
     ! set the dimensions of the double-wedge model. The forward wedge face is 2 inches 
     !long. The aft face is 1 inch. The forward wedge angle is 30 degrees. The aft wedge 
     !angle is 25 degrees, or 55 (30+25) degrees measured relative to the freestream velocity.
     ! to specify the geometry, we will use the x and y components of the face lengths
     alpha = 30.0d0 !forward wedge angle in degrees
     theta = 55.0d0 !total aft wedge angle in degrees
     Lfor = 0.0440d0 !x-length of forward wedge in meters 
     Hfor = 0.0254d0 !y-length of forward wedge in meters
     Laft = 0.0146d0 !x-length of aft wedge in meters 
     Haft = 0.0208d0 !y-length of aft wedge in meters
     b = 0.1016d0 *0.1d0!width of double-wedge in meters, b = 101.6 mm (4 inches) !shorten for 2D sim 
     Ltop = 0.0108d0 !length of flat top section in meters
     Ltot = 0.0694d0 !total x-length of double-wedge model (flat bottom) in meters
     Htot = 0.0462d0 !total y-length of double-wedge model (flat back) in meters
     xa = 0.0d0
     ya = 0.0d0
     za = 0.0d0
     iNumZ = 20 !number of rectangles in width direction
     iNumfor = 400 !number of rectangles in flow-wise direction on forward face
     iNumaft = 200 ! " " on aft face
     iNumtop = 120 !number of rectangles in x-direction on flat top section
     iNumback = 300
     iNumbottom = 400
     cFileName_Grid = "double_wedge.stl"
     CALL generate_double_wedge(alpha, theta, Lfor, Hfor, Laft, Haft, b,&
          Ltop, Ltot, Htot, xa, ya, za, iNumZ, iNumfor, iNumaft, iNumtop,&
          iNumback, iNumbottom, cFileName_Grid)
  CASE(4)
     !set the dimensions of the tick geometry. everything is in meters 
     alpha = -30.0d0*PI/180.0d0 !angle the top forward face makes with the horizontal
     beta = 24.0d0*PI/180.0d0 !angle the top aft face makes with the horizontal 
     theta = 20.0d0*PI/180.0d0 !angle of the sharp leading edge
     b = 2.04d-1 !z-length of the tick geometry
     len_one = 2.127d-2 !length of the top forward face in meters
     len_two = 5.0d-2 !length of the top aft face in meters
     len_three = 3.2d-2 !length of the bottom forward face in meters
     y = len_one*sin(theta)/sin(106.0d0*PI/180.0d0)
     v = len_one*sin(54.0d0*PI/180.0d0)/sin(106.0d0*PI/180.0d0)
     w = len_three-v
     hypotenuse = sqrt(y*y+w*w-2*y*w*cos(74.0d0*PI/180.0d0))
     pho = asin(y*sin(74.0d0*PI/180.0d0)/hypotenuse)
     phi = 106.0d0*PI/180.0d0 - pho
     x = hypotenuse * cos(phi)
     z = x*tan(phi)
     len_four = len_two + x !length of the bottom aft face 
     len_five = z !length of the back face
     lt1 = len_one*cos(alpha) !x-length of top forward face
     ht1 = len_one*sin(-alpha) !y-length of top forward face
     lt2 = len_two*cos(beta) !x-length of the top aft face
     ht2 = len_two*sin(beta) !y-length of the top aft face
     lb1 = len_three*cos((-alpha+theta)) !x-length of the bottom forward face
     hb1 = len_three*sin((-alpha+theta)) !y-length of the bottom forward face
     lb2 = len_four*cos(beta) !x-length of the bottom aft face
     hb2 = len_four*sin(beta) !y-length of the bottom aft face
     lback = len_five*sin(beta) !x-length of the back face
     hback = len_five*cos(beta) !y-length of the back face
     write(*,*) "l1=", lt1
     write(*,*) "l1+l2=", lt1+lt2
     write(*,*) "l3=", lb1
     write(*,*) "l5=", lback
     write(*,*) "h1=", ht1
     write(*,*) "h2=", ht2
     write(*,*) "h3=", hb1
     write(*,*) "h5=", hback

     inumZ = 20 !number of rectangles in the width (z) direction
     inumfor = 20 !number of rectangles in flow-wise direction on top forward face
     inumaft = 40 !rectangles top aft
     inumback = 20 !number of rectangles on back face
     inumbfor = 20 !number of rectangles in flow-wise direction on bottom forward face
     inumbaft = 45 !rectangles bottom aft
     xa = 0.0d0 !xa, ya, za coordinates of the origin
     ya = 0.0d0
     za = 0.0d0
     cFileName_Grid = "tick.stl"
     CALL generate_tick(alpha, beta, theta, b, lt1, ht1, lt2, ht2, lb1, hb1, lb2, hb2,&
          lback, hback, inumz, inumfor, inumaft, inumback, inumbfor, inumbaft, xa, ya, &
          cFileName_Grid)
  CASE(5)      
     !create triangulated surface grid for flat plate with wholes.
     cFileName_Grid = "flat_plate_with_hole.stl"
     CALL generate_flate_plate_with_holes(cFileName_Grid)
  CASE(6)
     !create triangulated cylinder-flare
     !data format /'(F10.2)'/
     L_cyl = 4.0d0*2.540d-2
     L_cone = 4.0d0*2.540d-2
     write(*, *), 'Please specify angle'
     read(*, *), Theta_c
     !Theta_c = 45.0d0
     Alpha_c = 30.0d0
     xa_c = 0.0002d0-0.02d0
     R_cyl =  1.28d0*2.540d-2
     N1p = 10
     N_xslant = 10
     N_xcyl = 200
     N_xcone = 200
     N_theta = 10.0d0!*(Theta_c/10.0d0)
     l_slant = 2.0d-3
     theta_slant = 30.0d0
     write(string,'(f10.2)' ) Theta_c
     plane_name = "plane_"//trim(adjustl(string))//".stl"
     cFileName_Grid = "cylinder_flare_"//trim(adjustl(string))//".stl"
     CALL generate_cylinder_flare(R_cyl, L_cone, L_cyl, Theta_c, Alpha_c, N1p, N_xslant, &
          N_xcyl, N_xcone, N_theta, xa_c, cFileName_Grid,  & 
          plane_name, l_slant, theta_slant)
  CASE DEFAULT
     WRITE(*,*)"Wrong case selected, specify again!"
  END SELECT

  ! output the surface geometry for visulation with normal vector
  CALL Read_stl_file(cFileName_Grid)


END PROGRAM grid_generation


!*************************************************************************
!This subroutine generate a flat plate surface.
!*************************************************************************
SUBROUTINE generate_flat_plate(lx,ly,lz,alpha,xa,ya,za,iNumX,iNumZ,cFileName_Grid)
  USE Constants
  IMPLICIT NONE
  INTEGER :: iNumX, iNumZ
  REAL(DOUBLE) :: lx, ly, lz, alpha, xa, ya, za
  CHARACTER(LEN=40) :: cFileName_Grid

  INTEGER :: iNumPoint1, iNumPoint2
  REAL(DOUBLE) :: hx, hx1, hx2, hy, hz, lx1,lx2, nx, ny, nz
  REAL(DOUBLE) :: x1, y1, z1, x2, y2, z2
  INTEGER :: i1, j1


  ! cell size of the surface mesh in length and width direction
  iNumPoint1 = 5  ! number of rectangles in the sharp leading edge part 
  iNumPoint2 = iNumX - iNumPoint1
  lx1 = ly/tan(alpha * PI /180.0d0)
  lx2 = lx - lx1
  hx = lx/iNumX
  hx1 = lx1/iNumPoint1
  hx2 = lx2/iNumPoint2
  hy = ly/iNumPoint1
  hz = lz/iNumZ 


  OPEN(1001,FILE=cFileName_Grid)
  WRITE(1001,*)'solid ascii_file_exported_from_Gridgen'
  ! upper flat plate surface
  DO i1 = 1, iNumX
     DO j1 = 1, iNumZ
        x1 = xa + (i1-1) * hx
        y1 = ya
        z1 = za + (j1-1) * hz
        x2 = x1 + hx
        y2 = ya
        z2 = z1 + hz
        nx = 0.0d0
        ny = 1.0d0
        nz = 0.0d0

        WRITE(1001,100)'  facet normal', nx, ny, nz
        WRITE(1001,*)'    outer loop'
        WRITE(1001,100)'      vertex', x1, y1, z1
        WRITE(1001,100)'      vertex', x1, y1, z2
        WRITE(1001,100)'      vertex', x2, y2, z1
        WRITE(1001,*)'    endloop'
        WRITE(1001,*)'  endfacet'

        WRITE(1001,100)'  facet normal', nx, ny, nz
        WRITE(1001,*)'    outer loop'
        WRITE(1001,100)'      vertex', x1, y1, z2
        WRITE(1001,100)'      vertex', x2, y2, z2
        WRITE(1001,100)'      vertex', x2, y2, z1
        WRITE(1001,*)'    endloop'
        WRITE(1001,*)'  endfacet'
     END DO
  END DO
  ! leading edge region
  DO i1 = 1, iNumPoint1
     DO j1 = 1, iNumZ
        x1 = xa + (i1-1) * hx1
        y1 = ya - (i1-1) * hy
        z1 = za + (j1-1) * hz
        x2 = x1 + hx1
        y2 = y1 - hy
        z2 = z1 + hz
        nx = -sin(alpha * PI /180.0d0)
        ny = -cos(alpha * PI /180.0d0)
        nz = 0.0d0

        WRITE(1001,100)'  facet normal', nx, ny, nz
        WRITE(1001,*)'    outer loop'
        WRITE(1001,100)'      vertex', x1, y1, z1
        WRITE(1001,100)'      vertex', x2, y2, z1
        WRITE(1001,100)'      vertex', x1, y1, z2
        WRITE(1001,*)'    endloop'
        WRITE(1001,*)'  endfacet'

        WRITE(1001,100)'  facet normal', nx, ny, nz
        WRITE(1001,*)'    outer loop'
        WRITE(1001,100)'      vertex', x1, y1, z2
        WRITE(1001,100)'      vertex', x2, y2, z1
        WRITE(1001,100)'      vertex', x2, y2, z2
        WRITE(1001,*)'    endloop'
        WRITE(1001,*)'  endfacet'
     END DO
  END DO
  ! lower flat plate surface  
  DO i1 = 1,iNumPoint2
     DO j1 = 1,iNumZ
        x1 = lx1 + (i1-1) * hx2
        y1 = ya - ly
        z1 = za + (j1-1) * hz
        x2 = x1 + hx2
        y2 = ya - ly
        z2 = z1 + hz
        nx = 0.0d0
        ny = -1.0d0
        nz = 0.0d0

        WRITE(1001,100)'  facet normal', nx, ny, nz
        WRITE(1001,*)'    outer loop'
        WRITE(1001,100)'      vertex', x1, y1, z1
        WRITE(1001,100)'      vertex', x2, y2, z1
        WRITE(1001,100)'      vertex', x1, y1, z2
        WRITE(1001,*)'    endloop'
        WRITE(1001,*)'  endfacet'

        WRITE(1001,100)'  facet normal', nx, ny, nz
        WRITE(1001,*)'    outer loop'
        WRITE(1001,100)'      vertex', x1, y1, z2
        WRITE(1001,100)'      vertex', x2, y2, z1
        WRITE(1001,100)'      vertex', x2, y2, z2
        WRITE(1001,*)'    endloop'
        WRITE(1001,*)'  endfacet'
     END DO
  END DO
  ! back region of the flat plate surface
  DO i1 = 1,iNumPoint1
     DO j1 = 1,iNumZ
        x1 = xa + lx
        y1 = ya - (i1-1) * hy
        z1 = za + (j1-1) * hz
        x2 = xa + lx
        y2 = y1 - hy
        z2 = z1 + hz
        nx = 1.0d0
        ny = 0.0d0
        nz = 0.0d0

        WRITE(1001,100)'  facet normal', nx, ny, nz
        WRITE(1001,*)'    outer loop'
        WRITE(1001,100)'      vertex', x1, y1, z1
        WRITE(1001,100)'      vertex', x1, y1, z2
        WRITE(1001,100)'      vertex', x2, y2, z1
        WRITE(1001,*)'    endloop'
        WRITE(1001,*)'  endfacet'

        WRITE(1001,100)'  facet normal', nx, ny, nz
        WRITE(1001,*)'    outer loop'
        WRITE(1001,100)'      vertex', x1, y1, z2
        WRITE(1001,100)'      vertex', x2, y2, z2
        WRITE(1001,100)'      vertex', x2, y2, z1
        WRITE(1001,*)'    endloop'
        WRITE(1001,*)'  endfacet'
     END DO
  END DO
  WRITE(1001,*)'endsolid ascii_file_exported_from_Gridgen'
  CLOSE(1001)

100 format(A,3d30.22) 

END SUBROUTINE generate_flat_plate


!*************************************************************************
!This subroutine generate a cylindrical surface.
!*************************************************************************
SUBROUTINE generate_cylinder(length,radius,xc,yc,zc,iNumLD,iNumRD,cFileName_Grid)
  USE Constants
  IMPLICIT NONE
  INTEGER :: iNumLD, iNumRD
  REAL(DOUBLE) :: length, radius, xc, yc, zc
  CHARACTER(LEN=40) :: cFileName_Grid

  REAL(DOUBLE) :: hz, alpha1, alpha2, nx, ny, nz
  REAL(DOUBLE) :: x1, y1, z1, x2, y2, z2
  INTEGER :: i1, j1


  ! cell size of the surface mesh in axial(length) direction
  hz = length/iNumLD

  OPEN(1001,FILE=cFileName_Grid)
  WRITE(1001,*)'solid ascii_file_exported_from_Gridgen'
  DO i1 = 1,iNumLD
     DO j1 = 1,iNumRD
        alpha1 = (j1-1) * 2.0d0 * PI / iNumRD
        x1 = xc + radius * cos(alpha1)
        y1 = yc + radius * sin(alpha1)
        z1 = zc + (i1-1) * hz
        alpha2 = j1 * 2.0d0 * PI / iNumRD
        x2 = xc + radius * cos(alpha2)
        y2 = yc + radius * sin(alpha2)
        z2 = z1 + hz
        nx = cos(0.5d0 * (alpha1+alpha2))
        ny = sin(0.5d0 * (alpha1+alpha2))
        nz = 0.0d0

        WRITE(1001,*)'facet normal', nx, ny, nz
        WRITE(1001,*)'outer loop'
        WRITE(1001,*)'vertex', x1, y1, z1
        WRITE(1001,*)'vertex', x2, y2, z1
        WRITE(1001,*)'vertex', x1, y1, z2
        WRITE(1001,*)'endloop'
        WRITE(1001,*)'endfacet'

        WRITE(1001,*)'facet normal', nx, ny, nz
        WRITE(1001,*)'outer loop'
        WRITE(1001,*)'vertex', x1, y1, z2
        WRITE(1001,*)'vertex', x2, y2, z1
        WRITE(1001,*)'vertex', x2, y2, z2
        WRITE(1001,*)'endloop'
        WRITE(1001,*)'endfacet'
     END DO
  END DO
  WRITE(1001,*)'endsolid ascii_file_exported_from_Gridgen'
  CLOSE(1001)

  !200 format(A,3d30.22) 

END SUBROUTINE generate_cylinder



!*************************************************************************
!This subroutine generates a double-wedge model.
!*************************************************************************
SUBROUTINE generate_double_wedge(alpha, theta, Lfor, Hfor, Laft, Haft, b,&
     Ltop, Ltot, Htot, xa, ya, za, iNumZ, iNumfor, iNumaft, iNumtop,&
     iNumback, iNumbottom, cFileName_Grid)
  USE Constants
  IMPLICIT NONE
  INTEGER ::  iNumZ, iNumfor, iNumaft, iNumtop, iNumback, iNumbottom
  REAL(DOUBLE) :: alpha, theta, Lfor, Hfor, Laft, Haft, b, Ltop, Ltot, Htot, xa, ya, za
  CHARACTER(LEN=40) :: cFileName_Grid

  REAL(DOUBLE) :: hz, hx1, hx2, hx3, hx4, hy1, hy2, hy3, nx, ny, nz
  REAL(DOUBLE) :: x1, y1, z1, x2, y2, z2
  INTEGER :: i1, j1

  !set cell sizes in x, y, and z directions for each section of the double-wedge
  hz = b/iNumZ !length of individual cell in z-direction, constant for whole geometry
  hx1 = Lfor/iNumfor !length of cell in x-direction for forward wedge face
  hx2 = Laft/iNumaft !" " aft wedge face
  hx3 = Ltop/iNumtop !" " top sectionh_cone, h_cylinder, 
  hx4 = Ltot/iNumbottom !" " bottom section
  hy1 = Hfor/iNumfor !length of cell in y-direction for forward wedge face
  hy2 = Haft/iNumaft !" " aft wedge face
  hy3 = Htot/iNumback !" " back section




  OPEN(1001,FILE=cFileName_Grid)
  WRITE(1001,*)'solid ascii_file_exported_from_Gridgen'

  !forward wedge region
  DO i1 = 1,iNumfor
     DO j1 = 1,iNumZ
        x1 = xa + (i1-1) * hx1
        y1 = ya + (i1-1) * hy1
        z1 = za + (j1-1) * hz
        x2 = x1 + hx1
        y2 = y1 + hy1
        z2 = z1 + hz
        nx = -sin(alpha * PI/180.0d0)
        ny = cos(alpha * PI/180.0d0)
        nz = 0.0d0

        WRITE(1001,200)'  facet normal', nx, ny, nz
        WRITE(1001,*)'    outer loop'
        WRITE(1001,200)'      vertex', x1, y1, z1
        WRITE(1001,200)'      vertex', x1, y1, z2
        WRITE(1001,200)'      vertex', x2, y2, z1
        WRITE(1001,*)'    endloop'
        WRITE(1001,*)'  endfacet'

        WRITE(1001,200)'  facet normal', nx, ny, nz
        WRITE(1001,*)'    outer loop'
        WRITE(1001,200)'      vertex', x1, y1, z2
        WRITE(1001,200)'      vertex', x2, y2, z2
        WRITE(1001,200)'      vertex', x2, y2, z1
        WRITE(1001,*)'    endloop'
        WRITE(1001,*)'  endfacet'
     END DO
  END DO
  !bottom
  DO i1 = 1,iNumbottom
     DO j1 = 1,iNumZ
        x1 = xa + (i1-1) * hx4
        y1 = ya 
        z1 = za + (j1-1) * hz
        x2 = x1 + hx4
        y2 = y1 
        z2 = z1 + hz
        nx = 0.0d0
        ny = -1.0d0
        nz = 0.0d0

        WRITE(1001,200)'  facet normal', nx, ny, nz
        WRITE(1001,*)'    outer loop'
        WRITE(1001,200)'      vertex', x1, y1, z1
        WRITE(1001,200)'      vertex', x2, y2, z1
        WRITE(1001,200)'      vertex', x1, y1, z2
        WRITE(1001,*)'    endloop'
        WRITE(1001,*)'  endfacet'

        WRITE(1001,200)'  facet normal', nx, ny, nz
        WRITE(1001,*)'    outer loop'
        WRITE(1001,200)'      vertex', x1, y1, z2
        WRITE(1001,200)'      vertex', x2, y2, z1
        WRITE(1001,200)'      vertex', x2, y2, z2
        WRITE(1001,*)'    endloop'
        WRITE(1001,*)'  endfacet'
     END DO
  END DO
  !back section
  DO i1 = 1,iNumback
     DO j1 = 1,iNumZ
        x1 = xa + Ltot
        y1 = ya + (i1-1) * hy3
        z1 = za + (j1-1) * hz
        x2 = x1  
        y2 = y1 + hy3
        z2 = z1 + hz
        nx = 1.0d0
        ny = 0.0d0
        nz = 0.0d0

        WRITE(1001,200)'  facet normal', nx, ny, nz
        WRITE(1001,*)'    outer loop'
        WRITE(1001,200)'      vertex', x1, y1, z1
        WRITE(1001,200)'      vertex', x2, y2, z1
        WRITE(1001,200)'      vertex', x1, y1, z2
        WRITE(1001,*)'    endloop'
        WRITE(1001,*)'  endfacet'

        WRITE(1001,200)'  facet normal', nx, ny, nz
        WRITE(1001,*)'    outer loop'
        WRITE(1001,200)'      vertex', x1, y1, z2
        WRITE(1001,200)'      vertex', x2, y2, z1
        WRITE(1001,200)'      vertex', x2, y2, z2
        WRITE(1001,*)'    endloop'
        WRITE(1001,*)'  endfacet'
     END DO
  END DO
  !flat top section
  DO i1 = 1, iNumtop
     DO j1 = 1, iNumZ
        x1 = xa + Lfor + Laft + (i1-1) * hx3
        y1 = ya + Hfor + Haft
        z1 = za + (j1-1) * hz
        x2 = x1 + hx3
        y2 = y1
        z2 = z1 + hz
        nx = 0.0d0
        ny = 1.0d0
        nz = 0.0d0

        WRITE(1001,200)'  facet normal', nx, ny, nz
        WRITE(1001,*)'    outer loop'
        WRITE(1001,200)'      vertex', x1, y1, z1
        WRITE(1001,200)'      vertex', x1, y1, z2
        WRITE(1001,200)'      vertex', x2, y2, z1
        WRITE(1001,*)'    endloop'
        WRITE(1001,*)'  endfacet'

        WRITE(1001,200)'  facet normal', nx, ny, nz
        WRITE(1001,*)'    outer loop'
        WRITE(1001,200)'      vertex', x1, y1, z2
        WRITE(1001,200)'      vertex', x2, y2, z2
        WRITE(1001,200)'      vertex', x2, y2, z1
        WRITE(1001,*)'    endloop'
        WRITE(1001,*)'  endfacet'
     END DO
  END DO
  !aft wedge region
  DO i1 = 1,iNumaft
     DO j1 = 1,iNumZ
        x1 = xa + Lfor + (i1-1) * hx2
        y1 = ya + Hfor + (i1-1) * hy2
        z1 = za + (j1-1) * hz
        x2 = x1 + hx2
        y2 = y1 + hy2
        z2 = z1 + hz
        nx = -sin(theta * PI/180.0d0)
        ny = cos(theta * PI/180.0d0)
        nz = 0.0d0

        WRITE(1001,200)'  facet normal', nx, ny, nz
        WRITE(1001,*)'    outer loop'
        WRITE(1001,200)'      vertex', x1, y1, z1
        WRITE(1001,200)'      vertex', x1, y1, z2
        WRITE(1001,200)'      vertex', x2, y2, z1
        WRITE(1001,*)'    endloop'
        WRITE(1001,*)'  endfacet'

        WRITE(1001,200)'  facet normal', nx, ny, nz
        WRITE(1001,*)'    outer loop'
        WRITE(1001,200)'      vertex', x1, y1, z2
        WRITE(1001,200)'      vertex', x2, y2, z2
        WRITE(1001,200)'      vertex', x2, y2, z1
        WRITE(1001,*)'    endloop'
        WRITE(1001,*)'  endfacet'
     END DO
  END DO
  WRITE(1001,*)'endsolid ascii_file_exported_from_Gridgen'
  CLOSE(1001)

200 format(A,3d30.22)

END SUBROUTINE generate_double_wedge

!****************************************************************************************
!This subroutine generates a tick geometry model
!****************************************************************************************
SUBROUTINE generate_tick(alpha, beta, theta, b, lt1, ht1, lt2, ht2, lb1, hb1, lb2, hb2,&
     lback, hback, inumz, inumfor, inumaft, inumback, inumbfor, inumbaft, xa, ya, &
     cFileName_Grid)

  USE Constants
  IMPLICIT NONE

  INTEGER ::  iNumZ, iNumfor, iNumaft, iNumbaft, iNumback, iNumbfor
  REAL(DOUBLE) :: alpha, beta, theta, b, lt1, ht1, lt2, ht2, lb1, hb1, xa, ya, za
  REAL(DOUBLE) :: lb2, hb2, lback, hback
  CHARACTER(LEN=40) :: cFileName_Grid

  REAL(DOUBLE) :: hz, hx1, hx2, hx3, hx4, hx5, hy1, hy2, hy3, hy4, hy5, nx, ny, nz
  REAL(DOUBLE) :: x1, y1, z1, x2, y2, z2
  INTEGER :: i1, j1

  !1 = top fore, 2 = top aft, 3 = bottom fore, 4 = bottom aft, 5 = back

  !set cell sizes in x, y, and z directions for each section of the double-wedge
  hz = b/iNumZ !length of individual cell in z-direction, constant for whole geometry
  hx1 = lt1/iNumfor !length of cell in x-direction for top forward face
  hx2 = lt2/iNumaft !" " top aft face
  hx3 = lb1/iNumbfor !" " bottom forward face
  hx4 = lb2/iNumbaft !" " bottom aft face
  hx5 = lback/iNumback !" " back face
  hy1 = -ht1/iNumfor !length of cell in y-direction for top forward face
  hy2 = ht2/iNumaft !" " top aft face
  hy3 = -hb1/iNumbfor !" " bottom forward face
  hy4 = hb2/iNumbaft !" " bottom aft face
  hy5 = -hback/iNumback !" " back face

  OPEN(1001,FILE=cFileName_Grid)
  WRITE(1001,*)'solid ascii_file_exported_from_Gridgen'

  !top forward face
  DO i1 = 1,iNumfor
     DO j1 = 1,iNumZ
        x1 = xa + (i1-1) * hx1
        y1 = ya + (i1-1) * hy1
        z1 = za + (j1-1) * hz
        x2 = x1 + hx1
        y2 = y1 + hy1
        z2 = z1 + hz
        nx = -sin(alpha)
        ny = cos(alpha)
        nz = 0.0d0

        WRITE(1001,200)'  facet normal', nx, ny, nz
        WRITE(1001,*)'    outer loop'
        WRITE(1001,200)'      vertex', x1, y1, z1
        WRITE(1001,200)'      vertex', x1, y1, z2
        WRITE(1001,200)'      vertex', x2, y2, z1
        WRITE(1001,*)'    endloop'
        WRITE(1001,*)'  endfacet'

        WRITE(1001,200)'  facet normal', nx, ny, nz
        WRITE(1001,*)'    outer loop'
        WRITE(1001,200)'      vertex', x2, y2, z1
        WRITE(1001,200)'      vertex', x1, y1, z2
        WRITE(1001,200)'      vertex', x2, y2, z2
        WRITE(1001,*)'    endloop'
        WRITE(1001,*)'  endfacet'
     END DO
  END DO

  !top aft face
  DO i1 = 1,iNumaft
     DO j1 = 1,iNumZ
        x1 = xa + lt1 + (i1-1) * hx2
        y1 = ya - ht1 + (i1-1) * hy2
        z1 = za + (j1-1) * hz
        x2 = x1 + hx2
        y2 = y1 + hy2
        z2 = z1 + hz
        nx = -sin(beta)
        ny = cos(beta)
        nz = 0.0d0

        WRITE(1001,200)'  facet normal', nx, ny, nz
        WRITE(1001,*)'    outer loop'
        WRITE(1001,200)'      vertex', x1, y1, z1
        WRITE(1001,200)'      vertex', x1, y1, z2
        WRITE(1001,200)'      vertex', x2, y2, z1
        WRITE(1001,*)'    endloop'
        WRITE(1001,*)'  endfacet'

        WRITE(1001,200)'  facet normal', nx, ny, nz
        WRITE(1001,*)'    outer loop'
        WRITE(1001,200)'      vertex', x1, y1, z2
        WRITE(1001,200)'      vertex', x2, y2, z2
        WRITE(1001,200)'      vertex', x2, y2, z1
        WRITE(1001,*)'    endloop'
        WRITE(1001,*)'  endfacet'
     END DO
  END DO

  !bottom forward face
  DO i1 = 1,iNumbfor
     DO j1 = 1,iNumZ
        x1 = xa + (i1-1) * hx3
        y1 = ya + (i1-1) * hy3
        z1 = za + (j1-1) * hz
        x2 = x1 + hx3
        y2 = y1 + hy3
        z2 = z1 + hz
        nx = -sin((-alpha+theta))
        ny = -cos((-alpha+theta))
        nz = 0.0d0

        WRITE(1001,200)'  facet normal', nx, ny, nz
        WRITE(1001,*)'    outer loop'
        WRITE(1001,200)'      vertex', x1, y1, z1
        WRITE(1001,200)'      vertex', x2, y2, z1
        WRITE(1001,200)'      vertex', x1, y1, z2
        WRITE(1001,*)'    endloop'
        WRITE(1001,*)'  endfacet'

        WRITE(1001,200)'  facet normal', nx, ny, nz
        WRITE(1001,*)'    outer loop'
        WRITE(1001,200)'      vertex', x2, y2, z1
        WRITE(1001,200)'      vertex', x2, y2, z2
        WRITE(1001,200)'      vertex', x1, y1, z2
        WRITE(1001,*)'    endloop'
        WRITE(1001,*)'  endfacet'
     END DO
  END DO

  !bottom aft face
  DO i1 = 1,iNumbaft
     DO j1 = 1,iNumZ
        x1 = xa + lb1 + (i1-1) * hx4
        y1 = ya - hb1 + (i1-1) * hy4
        z1 = za + (j1-1) * hz
        x2 = x1 + hx4
        y2 = y1 + hy4
        z2 = z1 + hz
        nx = sin(beta)
        ny = -cos(beta)
        nz = 0.0d0

        WRITE(1001,200)'  facet normal', nx, ny, nz
        WRITE(1001,*)'    outer loop'
        WRITE(1001,200)'      vertex', x1, y1, z1
        WRITE(1001,200)'      vertex', x2, y2, z1
        WRITE(1001,200)'      vertex', x1, y1, z2
        WRITE(1001,*)'    endloop'
        WRITE(1001,*)'  endfacet'  

        WRITE(1001,200)'  facet normal', nx, ny, nz
        WRITE(1001,*)'    outer loop'
        WRITE(1001,200)'      vertex', x1, y1, z2
        WRITE(1001,200)'      vertex', x2, y2, z1
        WRITE(1001,200)'      vertex', x2, y2, z2
        WRITE(1001,*)'    endloop'
        WRITE(1001,*)'  endfacet'
     END DO
  END DO

  !rear face
  DO i1 = 1,iNumback
     DO j1 = 1,iNumZ
        x1 = xa + lt1+lt2 + (i1-1) * hx5
        y1 = ya - ht1+ht2 + (i1-1) * hy5
        z1 = za + (j1-1) * hz
        x2 = x1 + hx5
        y2 = y1 + hy5
        z2 = z1 + hz
        nx = cos(beta)
        ny = sin(beta)
        nz = 0.0d0

        WRITE(1001,200)'  facet normal', nx, ny, nz
        WRITE(1001,*)'    outer loop'
        WRITE(1001,200)'      vertex', x1, y1, z1
        WRITE(1001,200)'      vertex', x1, y1, z2
        WRITE(1001,200)'      vertex', x2, y2, z1
        WRITE(1001,*)'    endloop'
        WRITE(1001,*)'  endfacet'

        WRITE(1001,200)'  facet normal', nx, ny, nz
        WRITE(1001,*)'    outer loop'
        WRITE(1001,200)'      vertex', x2, y2, z1
        WRITE(1001,200)'      vertex', x1, y1, z2
        WRITE(1001,200)'      vertex', x2, y2, z2 
        WRITE(1001,*)'    endloop'
        WRITE(1001,*)'  endfacet'
     END DO
  END DO

  WRITE(1001,*)'endsolid ascii_file_exported_from_Gridgen'
  CLOSE(1001)
200 format(A,3d30.22) 

END SUBROUTINE generate_tick



!****************************************************************************************
!This subroutine generates a cylinder-flare geometry model
!****************************************************************************************
SUBROUTINE generate_cylinder_flare(R_cyl, L_cone, L_cyl, Theta_c, Alpha_c, N1p, N_xslant, &
     N_xcyl, N_xcone, N_theta, xa_c, cFileName_Grid,  & 
     plane_name, l_slant, theta_slant)

  USE Constants
  IMPLICIT NONE
  CHARACTER(LEN=40) :: cFileName_Grid, plane_name

  INTEGER :: N_x, N_theta, i1, i2, N_xcyl, N_xcone, count, N1p, N_xslant
  REAL*8 :: R_cyl, L_cyl, L_cone, Theta_c, delta_x, delta_theta, xa_c, R_x, x1, x2, Alpha_c
  REAL*8 :: alpha1, alpha2, theta1, y11, y12, y21, y22, z11, z12, z21, z22, L1p
  REAL*8 :: L1, L2, delta_p2, y1,y2, z1, z2, R_x1, R_x2, nx, ny, nz, R_cone, x10
  REAL*8 :: x_cyl, x_tot, y1_cyl, y2_cyl, z1_cyl, z2_cyl,y_p,z_p
  REAL*8 :: theta_slant, delta_y1, delta_y2, delta_z1, delta_z2, l_slant
  Real*8 :: R_cyl1, thickness, alpha_half

  delta_theta = Theta_c/N_theta
  OPEN(1001,FILE=cFileName_Grid)
  OPEN(1002,FILE = plane_name, Status = 'Replace')
  WRITE(1001,*)'solid ascii_file_exported_from_Gridgen'
  WRITE(1002,*)'solid ascii_file_exported_from_Gridgen'
  thickness = l_slant*sin(theta_slant*PI/180.0d0)
  !N_xslant = 10
  !***************************************************
  !outer surface
  !***************************************************
  count = 0

  do i1=1, N_theta
     x1 = xa_c
     alpha1 = (90.0d0 - Theta_c+(i1-1)*delta_theta)*PI/180.0d0
     alpha2 = alpha1 + (delta_theta*PI/180.0d0)
     !write(*,*), (alpha2 - alpha1)
     y1 = R_cyl*sin(alpha1)
     y2 = R_cyl*sin(alpha2)
     z1 = R_cyl*cos(alpha1)
     z2 = R_cyl*cos(alpha2)

     nx = 0.0d0
     ny = sin(0.5d0*(alpha1+alpha2))
     nz = cos(0.5d0*(alpha1+alpha2))	
     delta_x = l_slant*cos(theta_slant*PI/180.0d0)/N_xslant
     !write(*, *), 'delta_x=', delta_x

     do i2=1, N_xslant

        x2 = x1 + delta_x

        WRITE(1001,*)'facet normal', nx, ny, nz
        WRITE(1001,*)'outer loop'
        WRITE(1001,*)'vertex', x1, y1, z1
        WRITE(1001,*)'vertex', x2, y1, z1
        WRITE(1001,*)'vertex', x2, y2, z2
        WRITE(1001,*)'endloop'
        WRITE(1001,*)'endfacet'

        WRITE(1001,*)'facet normal', nx, ny, nz
        WRITE(1001,*)'outer loop'
        WRITE(1001,*)'vertex', x1, y1, z1
        WRITE(1001,*)'vertex', x2, y2, z2
        WRITE(1001,*)'vertex', x1, y2, z2
        WRITE(1001,*)'endloop'
        WRITE(1001,*)'endfacet'

        count = count +2
        x1 = x2
     end do
     write(*, *), 'error=', x1 - (xa_c + l_slant*cos(theta_slant*PI/180.0d0))
     delta_x = (L_cyl-l_slant*cos(theta_slant*PI/1800d0))/N_xcyl
     do i2=1, N_xcyl
        x2 = x1 + delta_x

        WRITE(1001,*)'facet normal', nx, ny, nz
        WRITE(1001,*)'outer loop'
        WRITE(1001,*)'vertex', x1, y1, z1
        WRITE(1001,*)'vertex', x2, y1, z1
        WRITE(1001,*)'vertex', x2, y2, z2
        WRITE(1001,*)'endloop'
        WRITE(1001,*)'endfacet'

        WRITE(1001,*)'facet normal', nx, ny, nz
        WRITE(1001,*)'outer loop'
        WRITE(1001,*)'vertex', x1, y1, z1
        WRITE(1001,*)'vertex', x2, y2, z2
        WRITE(1001,*)'vertex', x1, y2, z2
        WRITE(1001,*)'endloop'
        WRITE(1001,*)'endfacet'

        count = count +2
        x1 = x2
     end do
  end do

  R_x1 = R_cyl
  x1 = xa_c + L_cyl
  !cone1
  delta_x = L_cone/N_xcone
  do i1=1, N_xcone

     x2 = x1 + delta_x
     R_x2 = R_x1 + delta_x*tan(Alpha_c*PI/180)

     do i2 = 1, N_theta

        alpha1 = (90.0d0 - Theta_c+(i2-1)*delta_theta)*PI/180.0d0
        alpha2 = alpha1 + (delta_theta*PI/180.0d0)
        theta1 = 0.5d0*(alpha1+alpha2)
        y11 = R_x1*sin(alpha1)
        y12 = R_x1*sin(alpha2)
        y21 = R_x2*sin(alpha1)
        y22 = R_x2*sin(alpha2)
        z11 = R_x1*cos(alpha1)
        z12 = R_x1*cos(alpha2)
        z21 = R_x2*cos(alpha1)
        z22 = R_x2*cos(alpha2)

        nx = -sin(Alpha_c*PI/180)
        ny = cos(Alpha_c*PI/180)*sin(theta1)
        nz = cos(Alpha_c*PI/180)*cos(theta1)

        WRITE(1001,*)'facet normal', nx, ny, nz
        WRITE(1001,*)'outer loop'
        WRITE(1001,*)'vertex', x1, y11, z11
        WRITE(1001,*)'vertex', x2, y21, z21
        WRITE(1001,*)'vertex', x1, y12, z12
        WRITE(1001,*)'endloop'
        WRITE(1001,*)'endfacet'

        WRITE(1001,*)'facet normal', nx, ny, nz
        WRITE(1001,*)'outer loop'
        WRITE(1001,*)'vertex', x1, y12, z12
        WRITE(1001,*)'vertex', x2, y21, z21
        WRITE(1001,*)'vertex', x2, y22, z22
        WRITE(1001,*)'endloop'
        WRITE(1001,*)'endfacet'      
        count = count +2
     end do
     R_x1 = R_x2
     x1 = x2
  end do
  WRITE(*,*) 'Outer Surface done'
  !***************************************************
  !front
  !***************************************************
  x1 = xa_c   
  delta_x = l_slant*cos(theta_slant*PI/180.0d0)/N_xslant
  y11 = R_cyl
  z11 = 0.0d0
  alpha1 = 0.0d0*PI/180.0d0

  do i1 = 1, N_theta
     x1 = xa_c
     alpha2 = alpha1 +(delta_theta*PI/180.0d0)
     

     y11 = R_cyl*cos(alpha1)
     z11 = R_cyl*sin(alpha1)
     y12 = R_cyl*cos(alpha2)
     z12 = R_cyl*sin(alpha2)

     do i2 = 1, N_xslant


        x2 = x1 + delta_x


        alpha_half = 0.5d0*(alpha1+alpha2)
        delta_y1 = (l_slant/n_xslant)*sin(theta_slant*PI/180.0d0)*cos(alpha1)
        delta_y2 = (l_slant/n_xslant)*sin(theta_slant*PI/180.0d0)*cos(alpha2)
        delta_z1 = (l_slant/n_xslant)*sin(theta_slant*PI/180.0d0)*sin(alpha1)
        delta_z2 = (l_slant/n_xslant)*sin(theta_slant*PI/180.0d0)*sin(alpha2)

        y21 = y11 - delta_y1
        y22 = y12 - delta_y2

        z21 = z11 - delta_z1
        z22 = z12 - delta_z2

        nx = -1.0d0*sin(theta_slant*PI/180.0d0)
        ny = -1.0d0*cos(theta_slant*PI/180.0d0)*cos(alpha_half*PI/180.0d0)
        nz = -1.0d0*cos(theta_slant*PI/180.0d0)*sin(alpha_half*PI/180.0d0)

        WRITE(1001,*)'facet normal', nx, ny, nz
        WRITE(1001,*)'outer loop'
        WRITE(1001,*)'vertex', x1, y11, z11
        WRITE(1001,*)'vertex', x2, y21, z21
        WRITE(1001,*)'vertex', x2, y22, z22
        WRITE(1001,*)'endloop'
        WRITE(1001,*)'endfacet'

        WRITE(1001,*)'facet normal', nx, ny, nz
        WRITE(1001,*)'outer loop'
        WRITE(1001,*)'vertex', x1, y12, z12
        WRITE(1001,*)'vertex', x1, y11, z11
        WRITE(1001,*)'vertex', x2, y22, z22
        WRITE(1001,*)'endloop'
        WRITE(1001,*)'endfacet'  
        count = count +2

        x1 = x2
        y11 = y21
        y12 = y22
        z11 = z21
        z12 = z22

     end do
     alpha1 = alpha2
  end do
  WRITE(*,*) 'Front done'
  !***************************************************
  !plane
  !***************************************************
  L1 = L_cone + L_cyl
  L1p = 1.0d-1
  L2 = 1.0d-1
  y_p = L2*cos(Theta_c*PI/180)
  z_p = L2*sin(Theta_c*PI/180)
  x_cyl = xa_c + L_cyl
  x_tot = xa_c + L1
  delta_p2 = 0.0d0
  nx = 0.0d0
  ny = sin(Theta_c*PI/180)
  nz = -cos(Theta_c*PI/180)
  x10 = xa_c-L1p
  x1= xa_c
  y1 = 0.0d0
  z1 = 0.0d0

  do i2 = 1, N1p
     delta_x=L1p/N1p
     x2 = x10 + delta_x

     y12 = R_cyl*cos(Theta_c*PI/180)


     z12 = R_cyl*sin(Theta_c*PI/180)

     y2 = y1 + L2*cos(Theta_c*PI/180)
     z2 = z1 + L2*sin(Theta_c*PI/180) 


     WRITE(1002,*)'facet normal', nx, ny, nz
     WRITE(1002,*)'outer loop'
     WRITE(1002,*)'vertex', x10, y1, z1
     WRITE(1002,*)'vertex', x10, y12, z12
     WRITE(1002,*)'vertex', x2, y1, z1
     WRITE(1002,*)'endloop'
     WRITE(1002,*)'endfacet'

     WRITE(1002,*)'facet normal', nx, ny, nz
     WRITE(1002,*)'outer loop'
     WRITE(1002,*)'vertex', x2, y1, z1
     WRITE(1002,*)'vertex', x10, y12, z12
     WRITE(1002,*)'vertex', x2, y12, z12
     WRITE(1002,*)'endloop'
     WRITE(1002,*)'endfacet'

     WRITE(1002,*)'facet normal', nx, ny, nz
     WRITE(1002,*)'outer loop'
     WRITE(1002,*)'vertex', x10, y12, z12
     WRITE(1002,*)'vertex', x10, y_p, z_p
     WRITE(1002,*)'vertex', x2, y12, z12
     WRITE(1002,*)'endloop'
     WRITE(1002,*)'endfacet'

     WRITE(1002,*)'facet normal', nx, ny, nz
     WRITE(1002,*)'outer loop'
     WRITE(1002,*)'vertex', x2, y12, z12
     WRITE(1002,*)'vertex', x10, y_p, z_p
     WRITE(1002,*)'vertex', x2, y_p, z_p
     WRITE(1002,*)'endloop'
     WRITE(1002,*)'endfacet'

     count = count +4

     x10 = x2
  end do
  !*******************************
  !region of intersection with the front of the flare
  !*******************************
  delta_x = l_slant*cos(theta_slant*PI/180.0d0)/N_xslant
  y22 = R_cyl*cos(Theta_c*PI/180)

  z22 = R_cyl*sin(Theta_c*PI/180)
 
  y2 = y1 + L2*cos(Theta_c*PI/180.0d0)
  z2 = z1 + L2*sin(Theta_c*PI/180.0d0)

  do i2 = 1, N_xslant

     x2 = x1 + delta_x
    
     y11 = (R_cyl-(x1-xa_c)*tan(theta_slant*PI/180.0d0))* &
          cos(Theta_c*PI/180.0d0)
     y12 = (R_cyl-(x2-xa_c)*tan(theta_slant*PI/180.0d0))* &
          cos(Theta_c*PI/180.0d0)
     if(i2.eq.1) write(*,*), 'y11 - y22 = ', y11 - y22

     z11 = (R_cyl-(x1-xa_c)*tan(theta_slant*PI/180.0d0))* &
          sin(Theta_c*PI/180.0d0)
     z12 = (R_cyl-(x2-xa_c)*tan(theta_slant*PI/180.0d0))* &
          sin(Theta_c*PI/180.0d0)

 

     WRITE(1002,*)'facet normal', nx, ny, nz
     WRITE(1002,*)'outer loop'
     WRITE(1002,*)'vertex', x1, y1, z1
     WRITE(1002,*)'vertex', x1, y11, z11
     WRITE(1002,*)'vertex', x2, y1, z1
     WRITE(1002,*)'endloop'
     WRITE(1002,*)'endfacet'

     WRITE(1002,*)'facet normal', nx, ny, nz
     WRITE(1002,*)'outer loop'
     WRITE(1002,*)'vertex', x2, y1, z1
     WRITE(1002,*)'vertex', x1, y11, z11
     WRITE(1002,*)'vertex', x2, y12, z12
     WRITE(1002,*)'endloop'
     WRITE(1002,*)'endfacet'
     if(i2.ne.1)then
        WRITE(1002,*)'facet normal', nx, ny, nz
	WRITE(1002,*)'outer loop'
	WRITE(1002,*)'vertex', x1, y11, z11
	WRITE(1002,*)'vertex', x1, y22, z22
	WRITE(1002,*)'vertex', x2, y12, z12
	WRITE(1002,*)'endloop'
	WRITE(1002,*)'endfacet'
     endif
     WRITE(1002,*)'facet normal', nx, ny, nz
     WRITE(1002,*)'outer loop'
     WRITE(1002,*)'vertex', x2, y12, z12
     WRITE(1002,*)'vertex', x1, y22, z22
     WRITE(1002,*)'vertex', x2, y22, z22
     WRITE(1002,*)'endloop'
     WRITE(1002,*)'endfacet'

     WRITE(1002,*)'facet normal', nx, ny, nz
     WRITE(1002,*)'outer loop'
     WRITE(1002,*)'vertex', x1, y12, z12
     WRITE(1002,*)'vertex', x1, y_p, z_p
     WRITE(1002,*)'vertex', x2, y12, z12
     WRITE(1002,*)'endloop'
     WRITE(1002,*)'endfacet'

     WRITE(1002,*)'facet normal', nx, ny, nz
     WRITE(1002,*)'outer loop'
     WRITE(1002,*)'vertex', x2, y12, z12
     WRITE(1002,*)'vertex', x1, y_p, z_p
     WRITE(1002,*)'vertex', x2, y_p, z_p
     WRITE(1002,*)'endloop'
     WRITE(1002,*)'endfacet'

     count = count +6
     if(i2.eq.1)then
        count = count - 1
     endif
     x1 = x2
  end do
  !*******************************
  !region of intersection with the rest of the cylinder flare
  !*******************************
  delta_p2=thickness
  do i2 = 1, (N_xcyl+N_xcone)
     if(i2.le.N_xcyl)then
        delta_x=(L_cyl-l_slant*cos(theta_slant*PI/180.0d0))/N_xcyl
     else
        delta_x=L_cone/N_xcone
     endif
     x2 = x1 + delta_x
     y11 = (R_cyl-delta_p2 + min(INT(x1/(L_cyl+xa_c)), 1)*(x1 - xa_c-L_cyl)*tan(Alpha_c*PI/180))* &
          cos(Theta_c*PI/180)
     y12 = y11 + delta_p2*cos(Theta_c*PI/180)
     y21 = (R_cyl-delta_p2 + min(INT(x2/(L_cyl+xa_c)), 1)*(x2 - xa_c-L_cyl)*tan(Alpha_c*PI/180))* &
          cos(Theta_c*PI/180)
     y22 = y21 + delta_p2*cos(Theta_c*PI/180)
     z11 = (R_cyl-delta_p2 + min(INT(x1/(L_cyl+xa_c)), 1)*(x1 - xa_c-L_cyl)*tan(Alpha_c*PI/180))* &
          sin(Theta_c*PI/180)
     z12 = z11 + delta_p2*sin(Theta_c*PI/180)
     z21 = (R_cyl-delta_p2 + min(INT(x2/(L_cyl+xa_c)), 1)*(x2 - xa_c-L_cyl)*tan(Alpha_c*PI/180))* &
          sin(Theta_c*PI/180)
     z22 = z21 + delta_p2*sin(Theta_c*PI/180)
     y2 = y1 + L2*cos(Theta_c*PI/180)
     z2 = z1 + L2*sin(Theta_c*PI/180) 


     WRITE(1002,*)'facet normal', nx, ny, nz
     WRITE(1002,*)'outer loop'
     WRITE(1002,*)'vertex', x1, y11, z11
     WRITE(1002,*)'vertex', x1, y12, z12
     WRITE(1002,*)'vertex', x2, y21, z21
     WRITE(1002,*)'endloop'
     WRITE(1002,*)'endfacet'

     WRITE(1002,*)'facet normal', nx, ny, nz
     WRITE(1002,*)'outer loop'
     WRITE(1002,*)'vertex', x2, y21, z21
     WRITE(1002,*)'vertex', x1, y12, z12
     WRITE(1002,*)'vertex', x2, y22, z22
     WRITE(1002,*)'endloop'
     WRITE(1002,*)'endfacet'

     WRITE(1002,*)'facet normal', nx, ny, nz
     WRITE(1002,*)'outer loop'
     WRITE(1002,*)'vertex', x1, y1, z1
     WRITE(1002,*)'vertex', x1, y11, z11
     WRITE(1002,*)'vertex', x2, y1, z1
     WRITE(1002,*)'endloop'
     WRITE(1002,*)'endfacet'

     WRITE(1002,*)'facet normal', nx, ny, nz
     WRITE(1002,*)'outer loop'
     WRITE(1002,*)'vertex', x2, y1, z1
     WRITE(1002,*)'vertex', x1, y11, z11
     WRITE(1002,*)'vertex', x2, y21, z21
     WRITE(1002,*)'endloop'
     WRITE(1002,*)'endfacet'

     WRITE(1002,*)'facet normal', nx, ny, nz
     WRITE(1002,*)'outer loop'
     WRITE(1002,*)'vertex', x1, y12, z12
     WRITE(1002,*)'vertex', x1, y_p, z_p
     WRITE(1002,*)'vertex', x2, y22, z22
     WRITE(1002,*)'endloop'
     WRITE(1002,*)'endfacet'

     WRITE(1002,*)'facet normal', nx, ny, nz
     WRITE(1002,*)'outer loop'
     WRITE(1002,*)'vertex', x2, y22, z22
     WRITE(1002,*)'vertex', x1, y_p, z_p
     WRITE(1002,*)'vertex', x2, y_p, z_p
     WRITE(1002,*)'endloop'
     WRITE(1002,*)'endfacet'

     count = count + 6

     x1 = x1 + delta_x
  end do

  WRITE(*,*) 'Plane done'
  !***************************************************
  !inner surface
  !***************************************************
  R_cyl1 = R_cyl-thickness


  !cylinder1
  write(*,*), N_xcyl, x1


  do i1=1, N_theta
     x1 = xa_c + l_slant*cos(theta_slant*PI/180.0d0)   
     alpha1 = (90.0d0 - Theta_c+(i1-1)*delta_theta)*PI/180.0d0
     alpha2 = alpha1 + (delta_theta*PI/180.0d0)
     nx = 0.0d0
     ny = -sin(0.5d0*(alpha1+alpha2))
     nz = -cos(0.5d0*(alpha1+alpha2))	
     delta_x = (L_cyl-l_slant*cos(theta_slant*PI/180.d0))/N_xcyl
     do i2=1, N_xcyl
        x2 = x1 + delta_x
        y1 = R_cyl1*sin(alpha1)
        y2 = R_cyl1*sin(alpha2)
        z1 = R_cyl1*cos(alpha1)
        z2 = R_cyl1*cos(alpha2) 
        !WRITE(*,*), x1
        WRITE(1001,*)'facet normal', nx, ny, nz
        WRITE(1001,*)'outer loop'
        WRITE(1001,*)'vertex', x1, y1, z1
        WRITE(1001,*)'vertex', x2, y2, z2
        WRITE(1001,*)'vertex', x2, y1, z1
        WRITE(1001,*)'endloop'
        WRITE(1001,*)'endfacet'

        WRITE(1001,*)'facet normal', nx, ny, nz
        WRITE(1001,*)'outer loop'
        WRITE(1001,*)'vertex', x1, y1, z1
        WRITE(1001,*)'vertex', x1, y2, z2
        WRITE(1001,*)'vertex', x2, y2, z2
        WRITE(1001,*)'endloop'

        WRITE(1001,*)'endfacet'   

        count = count +2
        x1 = x2
     end do
  end do
  x1 = xa_c + L_cyl 
  R_x1 = R_cyl1
  !cone1
  delta_x=L_cone/N_xcone
  do i1=1, N_xcone
     x2 = x1 + delta_x
     R_x2 = R_x1 + delta_x*tan(Alpha_c*PI/180)

     do i2 = 1, N_theta

        alpha1 = (90.0d0 - Theta_c+(i2-1)*delta_theta)*PI/180.0d0
        alpha2 = alpha1 + (delta_theta*PI/180.0d0)
        theta1 = 0.5d0*(alpha1+alpha2)
        y11 = R_x1*sin(alpha1)
        y12 = R_x1*sin(alpha2)
        y21 = R_x2*sin(alpha1)
        y22 = R_x2*sin(alpha2)
        z11 = R_x1*cos(alpha1)
        z12 = R_x1*cos(alpha2)
        z21 = R_x2*cos(alpha1)
        z22 = R_x2*cos(alpha2)

        nx = sin(Alpha_c*PI/180)
        ny = -cos(Alpha_c*PI/180)*sin(theta1)
        nz = -cos(Alpha_c*PI/180)*cos(theta1)

        WRITE(1001,*)'facet normal', nx, ny, nz
        WRITE(1001,*)'outer loop'
        WRITE(1001,*)'vertex', x1, y11, z11
        WRITE(1001,*)'vertex', x1, y12, z12
        WRITE(1001,*)'vertex', x2, y21, z21
        WRITE(1001,*)'endloop'
        WRITE(1001,*)'endfacet'

        WRITE(1001,*)'facet normal', nx, ny, nz
        WRITE(1001,*)'outer loop'
        WRITE(1001,*)'vertex', x1, y12, z12
        WRITE(1001,*)'vertex', x2, y22, z22
        WRITE(1001,*)'vertex', x2, y21, z21
        WRITE(1001,*)'endloop'
        WRITE(1001,*)'endfacet'      
        count = count +2
     end do
     R_x1 = R_x2
     x1 = x2
  end do
  WRITE(*,*) 'Inner Surface done'
  !***************************************************
  !back
  !***************************************************
  x1 = xa_c+L_cyl+L_cone
  R_cone = R_cyl+L_cone*tan(Alpha_c*PI/180)
  WRITE(*,*), R_cyl


  do i1 = 1, N_theta

     alpha1 = (90.0d0 - Theta_c+(i1-1)*delta_theta)*PI/180.0d0
     alpha2 = (90.0d0 - Theta_c+(i1)*delta_theta)*PI/180.0d0 
     y11 = (R_cone-thickness)*sin(alpha1)
     y12 = (R_cone-thickness)*sin(alpha2)
     y21 = R_cone*sin(alpha1)
     y22 = R_cone*sin(alpha2)
     z11 = (R_cone-thickness)*cos(alpha1)
     z12 = (R_cone-thickness)*cos(alpha2)
     z21 = R_cone*cos(alpha1)
     z22 = R_cone*cos(alpha2)

     nx = 1
     ny = 0
     nz = 0

     WRITE(1001,*)'facet normal', nx, ny, nz
     WRITE(1001,*)'outer loop'
     WRITE(1001,*)'vertex', x1, y11, z11
     WRITE(1001,*)'vertex', x1, y21, z21
     WRITE(1001,*)'vertex', x1, y12, z12
     WRITE(1001,*)'endloop'
     WRITE(1001,*)'endfacet'

     WRITE(1001,*)'facet normal', nx, ny, nz
     WRITE(1001,*)'outer loop'
     WRITE(1001,*)'vertex', x1, y21, z21
     WRITE(1001,*)'vertex', x1, y22, z22
     WRITE(1001,*)'vertex', x1, y12, z12
     WRITE(1001,*)'endloop'
     WRITE(1001,*)'endfacet'  

     count = count +2

  end do
  WRITE(1001,*)'endsolid ascii_file_exported_from_Gridgen'
  WRITE(1002,*)'endsolid ascii_file_exported_from_Gridgen'
  WRITE(*,*) 'fin'
  WRITE(*,*) count
  !***************************************************
  close(1001)
  close(1002)
End Subroutine generate_cylinder_flare
!*******************************************************************************
!This subroutine read in the stl file again for output
!*******************************************************************************
SUBROUTINE Read_stl_file(cFileName_Grid)
  USE Constants
  IMPLICIT NONE

  CHARACTER(LEN=40) :: cFileName_Grid

  INTEGER :: icc_NumTriSurf
  REAL(DOUBLE), DIMENSION(:,:,:), ALLOCATABLE :: TriAngSurf_Coord_iCell
  INTEGER :: i1
  CHARACTER (LEN=20) :: stemp
  REAL(DOUBLE), DIMENSION(1:3) :: surfnorm,v1,v2,v3
  CHARACTER(LEN=40) :: cFileName_Surface_output


  CALL  Calculate_iNumTriSurf(icc_NumTriSurf,cFileName_Grid)
  ALLOCATE(TriAngSurf_Coord_iCell(1:icc_NumTriSurf,1:3,1:3))

  OPEN(11,FILE=cFileName_Grid)
  READ(11,*)
  DO i1=1,icc_NumTriSurf
     READ(11,*)stemp,stemp,surfnorm
     READ(11,*)
     READ(11,*)stemp,v1
     TriAngSurf_Coord_iCell(i1,1,1) = v1(1)
     TriAngSurf_Coord_iCell(i1,1,2) = v1(2)
     TriAngSurf_Coord_iCell(i1,1,3) = v1(3)
     READ(11,*)stemp,v2
     TriAngSurf_Coord_iCell(i1,2,1) = v2(1)
     TriAngSurf_Coord_iCell(i1,2,2) = v2(2)
     TriAngSurf_Coord_iCell(i1,2,3) = v2(3)
     READ(11,*)stemp,v3
     TriAngSurf_Coord_iCell(i1,3,1) = v3(1)
     TriAngSurf_Coord_iCell(i1,3,2) = v3(2)
     TriAngSurf_Coord_iCell(i1,3,3) = v3(3)
     READ(11,*)
     READ(11,*)
  END DO
  CLOSE(11)

  cFileName_Surface_output = "surface_geometry.dat"
  CALL OutputSurfGeom(icc_NumTriSurf,TriAngSurf_Coord_iCell,&
       cFileName_Surface_output)                           

  DEALLOCATE(TriAngSurf_Coord_iCell)

END SUBROUTINE read_stl_file



!*******************************************************************************
!This subroutine calculates the number of tri-angular surfaces in the 
!surface geometry file.
!*******************************************************************************
SUBROUTINE Calculate_iNumTriSurf(iNumTriSurf,iFileName)
  INTEGER :: i1,iNumTriSurf,iflag
  CHARACTER(LEN=40) :: iFileName
  CHARACTER (LEN=20) :: stemp


  OPEN(11,FILE=iFileName,STATUS='OLD')
  iflag=0
  READ(11,*)stemp
  READ(11,*)stemp
  DO WHILE(stemp.NE.'endsolid')
     DO i1=1,6
        READ(11,*)
     END DO
     READ(11,*)stemp
     iflag=iflag+1
  END DO
  iNumTriSurf=iflag
  CLOSE(11)
  !  WRITE(*,*)'number of triangle surface=',iNumTriSurf

END SUBROUTINE Calculate_iNumTriSurf


!*******************************************************************************
!This subroutine outputs the solid surface geometry file to visualize in
!Tecplot in FE format
!*******************************************************************************
SUBROUTINE OutputSurfGeom(iNumElement,TriAngSurf_Coord_iCell,cFileName_Surface)
  USE Constants
  IMPLICIT NONE
  CHARACTER(LEN=40) :: cFileName_Surface
  INTEGER :: iNumElement,iNumNode,iV1,iV2,iV3,i1,j1
  REAL(DOUBLE) :: x1,x2,x3,y1,y2,y3,z1,z2,z3
  REAL(DOUBLE), DIMENSION(1:iNumElement,1:3,1:3) :: TriAngSurf_Coord_iCell
  REAL(DOUBLE), DIMENSION(1:3) :: v1,v2,v3,v12,v23,normal

  !The following part outputs the surface mesh into a data file for Tecplot visualization.

  iNumNode=3*iNumElement
  OPEN(21,FILE=cFileName_Surface)
  WRITE(21,*)'title="Surface Geometry File" '
  WRITE(21,*)'Variables = "x","y","z","nx","ny","nz"'
  WRITE(21,*)'Zone n=',iNumNode,',e=',iNumElement,',Datapacking=Block,',&
       'VARLOCATION=([4,5,6]=CELLCENTERED),ZoneType=FETRIANGLE'


  WRITE(21,*)

  DO i1=1,iNumElement
     x1=TriAngSurf_Coord_iCell(i1,1,1)
     x2=TriAngSurf_Coord_iCell(i1,2,1)
     x3=TriAngSurf_Coord_iCell(i1,3,1)
     WRITE(21,*)x1,x2,x3
  END DO

  DO i1=1,iNumElement
     y1=TriAngSurf_Coord_iCell(i1,1,2)
     y2=TriAngSurf_Coord_iCell(i1,2,2)
     y3=TriAngSurf_Coord_iCell(i1,3,2)
     WRITE(21,*)y1,y2,y3
  END DO

  DO i1=1,iNumElement
     z1=TriAngSurf_Coord_iCell(i1,1,3)
     z2=TriAngSurf_Coord_iCell(i1,2,3)
     z3=TriAngSurf_Coord_iCell(i1,3,3)
     WRITE(21,*)z1,z2,z3
  END DO

  DO i1=1,iNumElement
     DO j1 = 1, 3
        v1(j1) = TriAngSurf_Coord_iCell(i1,1,j1)
        v2(j1) = TriAngSurf_Coord_iCell(i1,2,j1)
        v3(j1) = TriAngSurf_Coord_iCell(i1,3,j1)    
     END DO
     v12 = v2 - v1
     v23 = v3 - v2
     CALL Cross_Prod(normal,v12,v23)
     WRITE(21,*)normal(1)
  END DO

  DO i1=1,iNumElement
     DO j1 = 1, 3
        v1(j1) = TriAngSurf_Coord_iCell(i1,1,j1)
        v2(j1) = TriAngSurf_Coord_iCell(i1,2,j1)
        v3(j1) = TriAngSurf_Coord_iCell(i1,3,j1)    
     END DO
     v12 = v2 - v1
     v23 = v3 - v2
     CALL Cross_Prod(normal,v12,v23)
     WRITE(21,*)normal(2)
  END DO

  DO i1=1,iNumElement
     DO j1 = 1, 3
        v1(j1) = TriAngSurf_Coord_iCell(i1,1,j1)
        v2(j1) = TriAngSurf_Coord_iCell(i1,2,j1)
        v3(j1) = TriAngSurf_Coord_iCell(i1,3,j1)    
     END DO
     v12 = v2 - v1
     v23 = v3 - v2
     CALL Cross_Prod(normal,v12,v23)
     WRITE(21,*)normal(3)
  END DO

  WRITE(21,*)
  DO i1=1,iNumElement
     iV1=(i1-1)*3+1
     iV2=(i1-1)*3+2
     iV3=(i1-1)*3+3
     WRITE(21,*)iV1,iV2,iV3
  END DO
  CLOSE(21)

END SUBROUTINE OutputSurfGeom


!***********************************************************************
! ans = v1 * v2, v1,v2 and ans are vectors
!***********************************************************************
SUBROUTINE Cross_Prod(ans,v1,v2)
  USE Constants
  IMPLICIT NONE
  REAL(DOUBLE), DIMENSION(1:3) :: ans,v1,v2

  ans(1)=v1(2)*v2(3)-v1(3)*v2(2)
  ans(2)=v1(3)*v2(1)-v1(1)*v2(3)
  ans(3)=v1(1)*v2(2)-v1(2)*v2(1)  

END SUBROUTINE Cross_Prod



!*******************************************************************************
! generate flat plate grid with holes
!*******************************************************************************
SUBROUTINE generate_flate_plate_with_holes(cFileName_Grid)
  USE Constants
  IMPLICIT NONE

  CHARACTER(LEN=40) :: cFileName_Grid  

  ! number of holes, hole raduis, and hole locations 
  ! later this should be changed to an array to store hole information.
  INTEGER :: iNum_holes                    ! # of holes
  REAL(DOUBLE) :: rd_hole              ! hole radius
  REAL(DOUBLE) :: x_hole, y_hole, z_hole   ! hole location


  ! flat plate dimensions (flat plate is placed in x-y plane) and locations
  REAL(DOUBLE) :: lx, ly, lz  ! lx, ly, lz also serve as the simulation domain size
  REAL(DOUBLE) :: delta_x, delta_y 
  REAL(DOUBLE) :: xa, ya, za, za_upper !lower left-corner of the plate(center of hole)

  ! the number of grid point in the x, y directions for each of the four regions.
  INTEGER :: iNumX1, iNumY1, iNumX2, iNumY2, iNumX3, iNumY3, iNumX4, iNumY4
  ! length in the x, y directions for each of the four regions.
  REAL(DOUBLE) :: lx1, ly1, lx2, ly2, lx3, ly3, lx4, ly4
  REAL(DOUBLE) :: xa1, ya1, xa2, ya2, xa3, ya3, xa4, ya4, za_u, za_d
  ! lx1, ly1 are the half length of rectangular surronding the hole(a quarter hole)

  INTEGER :: iNumX, iNumY

  INTEGER :: i1, iNorm1, iNorm2


  iNum_holes = 1
  !*******************************************************************************
  ! dimension of the flat plate
  lx = 1.0d0 
  ly = 1.0d0
  lz = 5.0d0

  ! location of the lower left corner for the lower flat plate with hole
  xa = 0.0d0
  ya = 0.0d0
  za = 1.0d-2 * lz

  rd_hole = 0.1d0 * lx
  delta_x = 0.1d0 * rd_hole      ! buffer region length arond the hole
  delta_y = 0.1d0 * rd_hole
  !*******************************************************************************


  ! set the parameters for the flat plate with hole  
  ! -------------
  ! |  3  |  4  |
  ! -------------
  ! |  1  |  2  |
  ! -------------
  !*******************************************************************************
  ! number of grid points for the 4 regions in the flat plate with hole
  iNumX1 = 20
  iNumY1 = iNumX1 / 2
  iNumX2 = 50
  iNumY2 = iNumY1
  iNumX3 = iNumY1
  iNumY3 = 50
  iNumX4 = iNumX2
  iNumY4 = iNumY3  
  ! number of grid points for the upper flat plate without hole
  iNumX = iNumX3 + iNumX4
  iNumY = iNumY2 + iNumY4  
  !*******************************************************************************

  !*******************************************************************************
  !  location of the 2 very close x-y planes in z direction  
  za_u = za + 1.0d-5 * lz 
  za_d = za - 1.0d-5 * lz
  ! location of the upper flat plate without hole
  za_upper = za + lz 
  !*******************************************************************************

  ! dimension of the 4 regions for the flat plate with hole
  lx1 = rd_hole + delta_x
  lx2 = lx - lx1
  lx3 = lx1
  lx4 = lx2
  ly1 = rd_hole + delta_y
  ly2 = ly1
  ly3 = ly - ly1
  ly4 = ly3 
  xa1 = xa
  xa2 = xa + lx1
  xa3 = xa
  xa4 = xa2
  ya1 = ya
  ya2 = ya
  ya3 = ya + ly1
  ya4 = ya3  

  iNorm1 = 1
  iNorm2 = -1

  OPEN(1001,FILE=cFileName_Grid)
  WRITE(1001,*)'solid ascii_file_exported_from_Gridgen' 
  CLOSE(1001)
  DO i1 = 1, iNum_holes
     ! loop over all holes. For each hole and its adjacent region, divid it to
     ! 4 parts, and generate grid in each region.
     ! part 1
     CALL get_flat_plate_with_hole_region_1(lx1,ly1,xa1,ya1,za_u,rd_hole,&
          iNumX1,iNumY1,iNorm1,cFileName_Grid)
     CALL get_flat_plate_with_hole_region_1(lx1,ly1,xa1,ya1,za_d,rd_hole,&
          iNumX1,iNumY1,iNorm2,cFileName_Grid)
     ! part 2
     CALL get_flat_plate_with_hole_region_2(lx2,ly2,xa2,ya2,za_u,iNumX2,iNumY2,&
          iNorm1,cFileName_Grid)
     CALL get_flat_plate_with_hole_region_2(lx2,ly2,xa2,ya2,za_d,iNumX2,iNumY2,&
          iNorm2,cFileName_Grid)         
     ! part 3
     CALL get_flat_plate_with_hole_region_2(lx3,ly3,xa3,ya3,za_u,iNumX3,iNumY3,&
          iNorm1,cFileName_Grid)
     CALL get_flat_plate_with_hole_region_2(lx3,ly3,xa3,ya3,za_d,iNumX3,iNumY3,&
          iNorm2,cFileName_Grid)   
     ! part 4
     CALL get_flat_plate_with_hole_region_2(lx4,ly4,xa4,ya4,za_u,iNumX4,iNumY4,&
          iNorm1,cFileName_Grid)
     CALL get_flat_plate_with_hole_region_2(lx4,ly4,xa4,ya4,za_d,iNumX4,iNumY4,&
          iNorm2,cFileName_Grid)   
  END DO

  ! the upper flat plate without holes
  CALL get_flat_plate_with_hole_region_2(lx,ly,xa,ya,lz,iNumX,iNumY,iNorm2,&
       cFileName_Grid)    

  OPEN(1001,FILE=cFileName_Grid,POSITION="APPEND")
  WRITE(1001,*)'endsolid ascii_file_exported_from_Gridgen'
  CLOSE(1001)


END SUBROUTINE generate_flate_plate_with_holes



!*******************************************************************************
! generate grid for the hole regions
!*******************************************************************************
SUBROUTINE get_flat_plate_with_hole_region_1(lx,ly,xa,ya,za,rd,iNumR,iNumL,&
     iNorm,cFileName_Grid)
  USE Constants
  IMPLICIT NONE

  INTEGER :: iNumL, iNumR, iNorm    ! number of grid points along the rectangular
  ! edge, and along the hole edge.
  REAL(DOUBLE) :: lx, ly, xa, ya, za, rd, dx, dy
  CHARACTER(LEN=40) :: cFileName_Grid  

  REAL(DOUBLE) :: hx, hy, nx, ny, nz, d_theta, theta1, theta2
  REAL(DOUBLE) :: x1, y1, x2, y2, x3, y3, x4, y4
  INTEGER :: i1, j1


  ! cell size of the surface mesh in length and width direction
  hx = lx / iNumL
  hy = ly / iNumL
  d_theta = 0.5d0 * PI / iNumR
  OPEN(1001,FILE=cFileName_Grid,POSITION="APPEND")
  DO i1 = 1, iNumR 
     theta1 = (i1-1) * d_theta
     theta2 = i1 * d_theta
     x1 = xa + rd * cos(theta1)
     y1 = ya + rd * sin(theta1)
     x2 = xa + rd * cos(theta2)    
     y2 = ya + rd * sin(theta2)    
     IF(i1.LE.iNumL)THEN
        j1 = i1
        x3 = xa + lx
        y3 = ya + (j1-1) * hy
        x4 = x3
        y4 = y3 + hy      
     ELSE
        j1 = i1 - iNumL
        x3 = xa + lx - (j1-1) * hx
        y3 = ya + ly
        x4 = x3 - hx
        y4 = y3       
     END IF
     nx = 0.0d0
     ny = 0.0d0
     IF(iNorm.EQ.1)THEN       
        nz = 1.0d0
        WRITE(1001,100)'  facet normal', nx, ny, nz
        WRITE(1001,*)'    outer loop'
        WRITE(1001,100)'      vertex', x1, y1, za
        WRITE(1001,100)'      vertex', x3, y3, za
        WRITE(1001,100)'      vertex', x2, y2, za
        WRITE(1001,*)'    endloop'
        WRITE(1001,*)'  endfacet'

        WRITE(1001,100)'  facet normal', nx, ny, nz
        WRITE(1001,*)'    outer loop'
        WRITE(1001,100)'      vertex', x2, y2, za
        WRITE(1001,100)'      vertex', x3, y3, za
        WRITE(1001,100)'      vertex', x4, y4, za
        WRITE(1001,*)'    endloop'
        WRITE(1001,*)'  endfacet'      
     ELSE IF(iNorm.EQ.-1)THEN
        nz = -1.0d0
        WRITE(1001,100)'  facet normal', nx, ny, nz
        WRITE(1001,*)'    outer loop'
        WRITE(1001,100)'      vertex', x1, y1, za
        WRITE(1001,100)'      vertex', x2, y2, za
        WRITE(1001,100)'      vertex', x3, y3, za
        WRITE(1001,*)'    endloop'
        WRITE(1001,*)'  endfacet'

        WRITE(1001,100)'  facet normal', nx, ny, nz
        WRITE(1001,*)'    outer loop'
        WRITE(1001,100)'      vertex', x2, y2, za
        WRITE(1001,100)'      vertex', x4, y4, za
        WRITE(1001,100)'      vertex', x3, y3, za
        WRITE(1001,*)'    endloop'
        WRITE(1001,*)'  endfacet'
     END IF
  END DO

100 format(A,3d30.22)     

END SUBROUTINE get_flat_plate_with_hole_region_1



!*******************************************************************************
! generate grid for the flat plate regions
!*******************************************************************************
SUBROUTINE get_flat_plate_with_hole_region_2(lx,ly,xa,ya,za,iNumX,iNumY,iNorm,&
     cFileName_Grid)
  USE Constants
  IMPLICIT NONE

  INTEGER :: iNumX, iNumY, iNorm
  REAL(DOUBLE) :: lx, ly, xa, ya, za
  CHARACTER(LEN=40) :: cFileName_Grid  

  REAL(DOUBLE) :: hx, hy, nx, ny, nz
  REAL(DOUBLE) :: x1, y1, z1, x2, y2, z2
  INTEGER :: i1, j1


  ! cell size of the surface mesh in length and width direction
  hx = lx / iNumX
  hy = ly / iNumY

  OPEN(1001,FILE=cFileName_Grid,POSITION="APPEND")
  DO i1 = 1, iNumX
     DO j1 = 1, iNumY
        x1 = xa + (i1-1) * hx
        y1 = ya + (j1-1) * hy
        z1 = za 
        x2 = x1 + hx
        y2 = y1 + hy
        z2 = z1
        nx = 0.0d0
        ny = 0.0d0
        IF(iNorm.EQ.1)THEN       
           nz = 1.0d0
           WRITE(1001,100)'  facet normal', nx, ny, nz
           WRITE(1001,*)'    outer loop'
           WRITE(1001,100)'      vertex', x1, y1, z1
           WRITE(1001,100)'      vertex', x2, y1, z1
           WRITE(1001,100)'      vertex', x1, y2, z1
           WRITE(1001,*)'    endloop'
           WRITE(1001,*)'  endfacet'

           WRITE(1001,100)'  facet normal', nx, ny, nz
           WRITE(1001,*)'    outer loop'
           WRITE(1001,100)'      vertex', x1, y2, z1
           WRITE(1001,100)'      vertex', x2, y1, z1
           WRITE(1001,100)'      vertex', x2, y2, z1
           WRITE(1001,*)'    endloop'
           WRITE(1001,*)'  endfacet'

        ELSE IF(iNorm.EQ.-1)THEN
           nz = -1.0d0
           WRITE(1001,100)'  facet normal', nx, ny, nz
           WRITE(1001,*)'    outer loop'
           WRITE(1001,100)'      vertex', x1, y1, z1
           WRITE(1001,100)'      vertex', x1, y2, z1
           WRITE(1001,100)'      vertex', x2, y1, z1
           WRITE(1001,*)'    endloop'
           WRITE(1001,*)'  endfacet'

           WRITE(1001,100)'  facet normal', nx, ny, nz
           WRITE(1001,*)'    outer loop'
           WRITE(1001,100)'      vertex', x1, y2, z1
           WRITE(1001,100)'      vertex', x2, y2, z1
           WRITE(1001,100)'      vertex', x2, y1, z1
           WRITE(1001,*)'    endloop'
           WRITE(1001,*)'  endfacet'      
        END IF
     END DO
  END DO
  CLOSE(1001)

100 format(A,3d30.22) 
END SUBROUTINE get_flat_plate_with_hole_region_2
