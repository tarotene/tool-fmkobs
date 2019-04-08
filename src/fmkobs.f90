program fmkobs
	USE procs

	implicit none

	CHARACTER(len=60) :: suffix
	INTEGER(4) :: ilx, ily
	INTEGER(4) :: nt_wolff0, nt_wolff1, nt_wolff2, nt_SSF0, nt_SSF1, nt_SSF2
	INTEGER(4) :: dur_wolff, dur_SSF
	DOUBLE PRECISION :: dkbt
	INTEGER(4) :: ivel
	CHARACTER(len=4) :: slx, sly
	CHARACTER(len=8) :: slt
	CHARACTER(len=60) :: skbt
	CHARACTER(len=10) :: sxbc, sybc, sinitst, sfield
	CHARACTER(len=60) :: sdirsnap, sdirem
	CHARACTER(len=60), ALLOCATABLE :: sfl_snap_wolff(:), sfl_snap_SSF(:)

	INTEGER(4) :: lbt, rbt

	! INTEGER(4), ALLOCATABLE :: imag(:)
	! DOUBLE PRECISION :: ave_mag, devsq_mag, err_mag, chi

	! INTEGER(4), ALLOCATABLE :: imag2(:)
	! DOUBLE PRECISION :: ave_mag2, devsq_mag2, err_mag2, chi2

	! INTEGER(4), ALLOCATABLE :: imag3(:)
	! DOUBLE PRECISION :: ave_mag3, devsq_mag3, err_mag3, chi3

	INTEGER(4), ALLOCATABLE :: im_total(:)
	DOUBLE PRECISION :: dave_m_total_eq, ddevsq_m_total_eq, derr_m_total_eq, dchi_total_eq
	DOUBLE PRECISION :: dave_m_total_noneq, ddevsq_m_total_noneq, derr_m_total_noneq, dchi_total_noneq

	INTEGER(4), ALLOCATABLE :: im_south(:)
	DOUBLE PRECISION :: dave_m_south_eq, ddevsq_m_south_eq, derr_m_south_eq, dchi_south_eq
	DOUBLE PRECISION :: dave_m_south_noneq, ddevsq_m_south_noneq, derr_m_south_noneq, dchi_south_noneq

	INTEGER(4), ALLOCATABLE :: im_north(:)
	DOUBLE PRECISION :: dave_m_north_eq, ddevsq_m_north_eq, derr_m_north_eq, dchi_north_eq
	DOUBLE PRECISION :: dave_m_north_noneq, ddevsq_m_north_noneq, derr_m_north_noneq, dchi_north_noneq

	INTEGER(4), ALLOCATABLE :: ie_total(:)
	DOUBLE PRECISION :: dave_e_total_eq, ddevsq_e_total_eq, derr_e_total_eq, dc_total_eq
	DOUBLE PRECISION :: dave_e_total_noneq, ddevsq_e_total_noneq, derr_e_total_noneq, dc_total_noneq

	INTEGER(4), ALLOCATABLE :: ie_south(:)
	DOUBLE PRECISION :: dave_e_south_eq, ddevsq_e_south_eq, derr_e_south_eq, dc_south_eq
	DOUBLE PRECISION :: dave_e_south_noneq, ddevsq_e_south_noneq, derr_e_south_noneq, dc_south_noneq

	INTEGER(4), ALLOCATABLE :: ie_north(:)
	DOUBLE PRECISION :: dave_e_north_eq, ddevsq_e_north_eq, derr_e_north_eq, dc_north_eq
	DOUBLE PRECISION :: dave_e_north_noneq, ddevsq_e_north_noneq, derr_e_north_noneq, dc_north_noneq

	INTEGER(4), ALLOCATABLE :: imb(:)
	DOUBLE PRECISION :: dave_mb, ddevsq_mb, derr_mb, dchib

	INTEGER(4), ALLOCATABLE :: imb_south(:)
	DOUBLE PRECISION :: dave_mb_south, ddevsq_mb_south, derr_mb_south, dchib_south

	INTEGER(4), ALLOCATABLE :: imb_north(:)
	DOUBLE PRECISION :: dave_mb_north, ddevsq_mb_north, derr_mb_north, dchib_north

	INTEGER(4), ALLOCATABLE :: ieb(:)
	DOUBLE PRECISION :: dave_eb, ddevsq_eb, derr_eb, dcb

	INTEGER(4), ALLOCATABLE :: ipw(:)
	DOUBLE PRECISION :: dave_pw, ddevsq_pw, derr_pw

	INTEGER(4) :: isizeBin

	INTEGER(4) :: x, y, it, dumt

	
	READ(*,*) ilx, ily, nt_wolff0, nt_wolff1, nt_wolff2, nt_SSF0, nt_SSF1, nt_SSF2, dkbt, ivel, sxbc, sybc, sinitst, sfield, dur_wolff, dur_SSF, isizeBin

	ALLOCATE(sfl_snap_wolff(1:nt_wolff2),sfl_snap_SSF(1:nt_SSF2))

! 	WRITE(slx,'(i0.4)') lx
! 	WRITE(sly,'(i0.4)') ly
! 	WRITE(slt,'(i0.8)') lt
! 	! WRITE(skbt,'(f7.4)') kbt
! 	CALL zeropad(kbt,skbt,"(i0.2)","(f0.4)")
! 	suffix = "_"//slx//"_"//sly//"_"//slt//"_"//TRIM(skbt)//"_"//TRIM(sxbc)//"_"//TRIM(sybc)
! 	dirsnap   = "dat/snap/"//slx//"_"//sly//"_"//slt//"_"//TRIM(skbt)//"_"//TRIM(sxbc)//"_"//TRIM(sybc)
!   sdirem = "dat/stream/"//slx//"_"//sly//"_"//slt//"_"//TRIM(skbt)//"_"//TRIM(sxbc)//"_"//TRIM(sybc)

	CALL convertParamDirname(ilx,ily,dkbt,ivel,sxbc,sybc,sfield,sdirsnap,sdirem)
	! CALL system("source ./sh/_mkdir.sh "//TRIM(sdirsnap))
	! CALL system("source ./sh/_mkdir.sh "//TRIM(sdirem))
	CALL convertTimeFilesnap(nt_wolff2,nt_SSF2,sfl_snap_wolff(1:),sfl_snap_SSF(1:))

	! A-1. calc equilibrium magnetizations
	ALLOCATE(im_total(1:nt_wolff1+nt_wolff2),im_south(1:nt_wolff1+nt_wolff2),im_north(1:nt_wolff1+nt_wolff2))

	OPEN(unit=8,file=TRIM(sdirem)//"/eq_m.dat",status="old",action="read")
	READ(unit=8,fmt='()')
	WolffSweep_Mag: DO it = 1, nt_wolff1+nt_wolff2, 1
		READ(unit=8,fmt=*) dumt, im_total(it), im_south(it), im_north(it)
	END DO WolffSweep_Mag
	CLOSE(unit=8)

	CALL calcAbsAvgs(ilx*ily/1,dur_wolff,isizeBin,im_total(nt_wolff1+nt_wolff2-dur_wolff+1:nt_wolff1+nt_wolff2),dave_m_total_eq,ddevsq_m_total_eq,derr_m_total_eq)
	CALL calcAbsAvgs(ilx*ily/2,dur_wolff,isizeBin,im_south(nt_wolff1+nt_wolff2-dur_wolff+1:nt_wolff1+nt_wolff2),dave_m_south_eq,ddevsq_m_south_eq,derr_m_south_eq)
	CALL calcAbsAvgs(ilx*ily/2,dur_wolff,isizeBin,im_north(nt_wolff1+nt_wolff2-dur_wolff+1:nt_wolff1+nt_wolff2),dave_m_north_eq,ddevsq_m_north_eq,derr_m_north_eq)

	dchi_total_eq = DBLE(ilx*ily/1) * ddevsq_m_total_eq / dkbt
	dchi_south_eq = DBLE(ilx*ily/2) * ddevsq_m_south_eq / dkbt
	dchi_north_eq = DBLE(ilx*ily/2) * ddevsq_m_north_eq / dkbt

	DEALLOCATE(im_total,im_south,im_north)

	! A-2. calc equilibrium energies
	ALLOCATE(ie_total(1:nt_wolff1+nt_wolff2),ie_south(1:nt_wolff1+nt_wolff2),ie_north(1:nt_wolff1+nt_wolff2))

	OPEN(unit=9,file=TRIM(sdirem)//"/eq_e.dat",status="old",action="read")
	READ(unit=9,fmt='()')
	WolffSweep_En: DO it = 1, nt_wolff1+nt_wolff2, 1
		READ(unit=9,fmt=*) dumt, ie_total(it), ie_south(it), ie_north(it)
	END DO WolffSweep_En
	CLOSE(unit=9)

	CALL calcAvgs(ilx*ily/1,dur_wolff,isizeBin,ie_total(nt_wolff1+nt_wolff2-dur_wolff+1:nt_wolff1+nt_wolff2),dave_e_total_eq,ddevsq_e_total_eq,derr_e_total_eq)
	CALL calcAvgs(ilx*ily/2,dur_wolff,isizeBin,ie_south(nt_wolff1+nt_wolff2-dur_wolff+1:nt_wolff1+nt_wolff2),dave_e_south_eq,ddevsq_e_south_eq,derr_e_south_eq)
	CALL calcAvgs(ilx*ily/2,dur_wolff,isizeBin,ie_north(nt_wolff1+nt_wolff2-dur_wolff+1:nt_wolff1+nt_wolff2),dave_e_north_eq,ddevsq_e_north_eq,derr_e_north_eq)

	dc_total_eq = DBLE(ilx*ily/1) * ddevsq_e_total_eq / (dkbt**2)
	dc_south_eq = DBLE(ilx*ily/2) * ddevsq_e_south_eq / (dkbt**2)
	dc_north_eq = DBLE(ilx*ily/2) * ddevsq_e_north_eq / (dkbt**2)
	
	DEALLOCATE(ie_total,ie_south,ie_north)


	! B-0. calc non-equilibrium boundary observables
	ALLOCATE(imb(1:nt_SSF1+nt_SSF2),ieb(1:nt_SSF1+nt_SSF2),ipw(1:nt_SSF1+nt_SSF2))

	OPEN(unit=17,file=TRIM(sdirem)//"/noneq_pw.dat",status="old",action="read")
	READ(unit=17,fmt='()')
	SSF_Sweep_Pow: DO it = 1, nt_SSF1+nt_SSF2, 1
		READ(unit=17,fmt=*) dumt, imb(it), ieb(it), ipw(it)
	END DO SSF_Sweep_Pow
	CLOSE(unit=17)

	CALL calcAbsAvgs(2*ilx,dur_SSF,isizeBin,imb(nt_SSF1+nt_SSF2-dur_SSF+1:nt_SSF1+nt_SSF2),dave_mb,ddevsq_mb,derr_mb)
	CALL calcAvgs(1*ilx,dur_SSF,isizeBin,ieb(nt_SSF1+nt_SSF2-dur_SSF+1:nt_SSF1+nt_SSF2),dave_eb,ddevsq_eb,derr_eb)
	CALL calcAvgs(1*ilx,dur_SSF,isizeBin,ipw(nt_SSF1+nt_SSF2-dur_SSF+1:nt_SSF1+nt_SSF2),dave_pw,ddevsq_pw,derr_pw)

	dchib = DBLE(2*ilx) * ddevsq_mb / dkbt
	dcb = DBLE(1*ilx) * ddevsq_eb / dkbt

	DEALLOCATE(imb,ieb,ipw)

	! B-1. calc non-equilibrium magnetizations
	ALLOCATE(im_total(1:nt_SSF1+nt_SSF2),im_south(1:nt_SSF1+nt_SSF2),im_north(1:nt_SSF1+nt_SSF2))

	OPEN(unit=18,file=TRIM(sdirem)//"/noneq_m.dat",status="old",action="read")
	READ(unit=18,fmt='()')
	SSF_Sweep_Mag: DO it = 1, nt_SSF1+nt_SSF2, 1
		READ(unit=18,fmt=*) dumt, im_total(it), im_south(it), im_north(it)
	END DO SSF_Sweep_Mag
	CLOSE(unit=18)

	CALL calcAbsAvgs(ilx*ily/1,dur_SSF,isizeBin,im_total(nt_SSF1+nt_SSF2-dur_SSF+1:nt_SSF1+nt_SSF2),dave_m_total_noneq,ddevsq_m_total_noneq,derr_m_total_noneq)
	CALL calcAbsAvgs(ilx*ily/2,dur_SSF,isizeBin,im_south(nt_SSF1+nt_SSF2-dur_SSF+1:nt_SSF1+nt_SSF2),dave_m_south_noneq,ddevsq_m_south_noneq,derr_m_south_noneq)
	CALL calcAbsAvgs(ilx*ily/2,dur_SSF,isizeBin,im_north(nt_SSF1+nt_SSF2-dur_SSF+1:nt_SSF1+nt_SSF2),dave_m_north_noneq,ddevsq_m_north_noneq,derr_m_north_noneq)

	dchi_total_noneq = DBLE(ilx*ily/1) * ddevsq_m_total_noneq / dkbt
	dchi_south_noneq = DBLE(ilx*ily/2) * ddevsq_m_south_noneq / dkbt
	dchi_north_noneq = DBLE(ilx*ily/2) * ddevsq_m_north_noneq / dkbt

	DEALLOCATE(im_total,im_south,im_north)


	! B-2. calc non-equilibrium energies
	ALLOCATE(ie_total(1:nt_SSF1+nt_SSF2),ie_south(1:nt_SSF1+nt_SSF2),ie_north(1:nt_SSF1+nt_SSF2))

	OPEN(unit=19,file=TRIM(sdirem)//"/noneq_e.dat",status="old",action="read")
	READ(unit=19,fmt='()')
	SSF_Sweep_En: DO it = 1, nt_SSF1+nt_SSF2, 1
		READ(unit=19,fmt=*) dumt, ie_total(it), ie_south(it), ie_north(it)
	END DO SSF_Sweep_En
	CLOSE(unit=19)

	CALL calcAvgs(ilx*ily/1,dur_SSF,isizeBin,ie_total(nt_SSF1+nt_SSF2-dur_SSF+1:nt_SSF1+nt_SSF2),dave_e_total_noneq,ddevsq_e_total_noneq,derr_e_total_noneq)
	CALL calcAvgs(ilx*ily/2,dur_SSF,isizeBin,ie_south(nt_SSF1+nt_SSF2-dur_SSF+1:nt_SSF1+nt_SSF2),dave_e_south_noneq,ddevsq_e_south_noneq,derr_e_south_noneq)
	CALL calcAvgs(ilx*ily/2,dur_SSF,isizeBin,ie_north(nt_SSF1+nt_SSF2-dur_SSF+1:nt_SSF1+nt_SSF2),dave_e_north_noneq,ddevsq_e_north_noneq,derr_e_north_noneq)

	dc_total_noneq = DBLE(ilx*ily/1) * ddevsq_e_total_noneq / (dkbt**2)
	dc_south_noneq = DBLE(ilx*ily/2) * ddevsq_e_south_noneq / (dkbt**2)
	dc_north_noneq = DBLE(ilx*ily/2) * ddevsq_e_north_noneq / (dkbt**2)

	DEALLOCATE(ie_total,ie_south,ie_north)

	OPEN(unit=30,file="dat/obs/eq_m.dat",status="old",action="write",access="append")
	WRITE(unit=30,fmt='(2(i0.4,", "),f7.4,", ",a,", ",5(f7.4,", "),f7.4)')	ilx, ily, dkbt, TRIM(sfield), dave_m_total_eq, derr_m_total_eq, dave_m_south_eq, derr_m_south_eq, dave_m_north_eq, derr_m_north_eq
	CLOSE(unit=30)
	
	OPEN(unit=30,file="dat/obs/eq_dm.dat",status="old",action="write",access="append")
	WRITE(unit=30,fmt='(2(i0.4,", "),f7.4,", ",a,", ",2(f7.4,", "),f7.4)') ilx, ily, dkbt, TRIM(sfield), dchi_total_eq, dchi_south_eq, dchi_north_eq
	CLOSE(unit=30)

	OPEN(unit=30,file="dat/obs/eq_e.dat",status="old",action="write",access="append")
	WRITE(unit=30,fmt='(2(i0.4,", "),f7.4,", ",a,", ",5(f7.4,", "),f7.4)')	ilx, ily, dkbt, TRIM(sfield), dave_e_total_eq, derr_e_total_eq, dave_e_south_eq, derr_e_south_eq, dave_e_north_eq, derr_e_north_eq
	CLOSE(unit=30)

	OPEN(unit=30,file="dat/obs/eq_de.dat",status="old",action="write",access="append")
	WRITE(unit=30,fmt='(2(i0.4,", "),f7.4,", ",a,", ",2(f7.4,", "),f7.4)') ilx, ily, dkbt, TRIM(sfield), dc_total_eq, dc_south_eq, dc_north_eq
	CLOSE(unit=30)

	OPEN(unit=30,file="dat/obs/noneq_m.dat",status="old",action="write",access="append")
	WRITE(unit=30,fmt='(2(i0.4,", "),f7.4,", ",i0.4,", ",a,", ",5(f7.4,", "),f7.4)') ilx, ily, dkbt, ivel, TRIM(sfield), dave_m_total_noneq, derr_m_total_noneq, dave_m_south_noneq, derr_m_south_noneq, dave_m_north_noneq, derr_m_north_noneq
	CLOSE(unit=30)

	OPEN(unit=30,file="dat/obs/noneq_dm.dat",status="old",action="write",access="append")
	WRITE(unit=30,fmt='(2(i0.4,", "),f7.4,", ",i0.4,", ",a,", ",2(f7.4,", "),f7.4)') ilx, ily, dkbt, ivel, TRIM(sfield), dchi_total_eq, dchi_south_eq, dchi_north_eq
	CLOSE(unit=30)

	OPEN(unit=30,file="dat/obs/noneq_e.dat",status="old",action="write",access="append")
	WRITE(unit=30,fmt='(2(i0.4,", "),f7.4,", ",i0.4,", ",a,", ",5(f7.4,", "),f7.4)') ilx, ily, dkbt, ivel, TRIM(sfield), dave_e_total_noneq, derr_e_total_noneq, dave_e_south_noneq, derr_e_south_noneq, dave_e_north_noneq, derr_e_north_noneq
	CLOSE(unit=30)
	
	OPEN(unit=30,file="dat/obs/noneq_de.dat",status="old",action="write",access="append")
	WRITE(unit=30,fmt='(2(i0.4,", "),f7.4,", ",i0.4,", ",a,", ",2(f7.4,", "),f7.4)') ilx, ily, dkbt, ivel, TRIM(sfield), dc_total_eq, dc_south_eq, dc_north_eq
	CLOSE(unit=30)

	OPEN(unit=30,file="dat/obs/noneq_mb.dat",status="old",action="write",access="append")
	WRITE(unit=30,fmt='(2(i0.4,", "),f7.4,", ",i0.4,", ",a,", ",1(f7.4,", "),f7.4)') ilx, ily, dkbt, ivel, TRIM(sfield), dave_mb, derr_mb
	CLOSE(unit=30)

	OPEN(unit=30,file="dat/obs/noneq_eb.dat",status="old",action="write",access="append")
	WRITE(unit=30,fmt='(2(i0.4,", "),f7.4,", ",i0.4,", ",a,", ",1(f7.4,", "),f7.4)') ilx, ily, dkbt, ivel, TRIM(sfield), dave_eb, derr_eb
	CLOSE(unit=30)

	OPEN(unit=30,file="dat/obs/noneq_pw.dat",status="old",action="write",access="append")
	WRITE(unit=30,fmt='(2(i0.4,", "),f7.4,", ",i0.4,", ",a,", ",1(f7.4,", "),f7.4)') ilx, ily, dkbt, ivel, TRIM(sfield), dave_pw, derr_pw
	CLOSE(unit=30)

end program fmkobs
