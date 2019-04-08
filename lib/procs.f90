  INCLUDE 'mkl_vsl.fi'

  MODULE procs
    USE mkl_vsl
    USE mkl_vsl_type

    IMPLICIT NONE

    TYPE DIRECTION
      INTEGER(4) :: n, s, e, w
    END TYPE DIRECTION    

  CONTAINS
    SUBROUTINE calcAbsAvgs(isizeSys,nsample,isizeBin,iextobs,dave,ddevsq,derr)
	! only valid for enough LARGE number of samples!!
      INTEGER(4), INTENT(in) :: isizeSys, nsample, isizeBin
      INTEGER(4), INTENT(in) :: iextobs(1:nsample)
      DOUBLE PRECISION, INTENT(out) :: dave, ddevsq, derr

      DOUBLE PRECISION :: dintobs(1:nsample)
      DOUBLE PRECISION, ALLOCATABLE :: bin_dintobs(:)

      INTEGER(4) :: nbins
      INTEGER(4) :: ibin


      nbins = nsample / isizeBin
      ALLOCATE(bin_dintobs(1:nbins))

      dintobs(1:nsample) = ABS(DBLE(iextobs(1:nsample)) / DBLE(isizeSys))
      Binning: DO ibin = 1, nbins, 1
        bin_dintobs(ibin) = SUM(dintobs(1+(ibin-1)*isizeBin:ibin*isizeBin)) / DBLE(isizeBin)
      END DO Binning

      dave = SUM(bin_dintobs(1:nbins)) / DBLE(nbins)
      ddevsq = SUM(bin_dintobs(1:nbins)**2) / DBLE(nbins) - (SUM(bin_dintobs(1:nbins)) / DBLE(nbins))**2
      derr = SQRT(ddevsq/DBLE(nbins))
    END SUBROUTINE calcAbsAvgs

    SUBROUTINE calcAvgs(isizeSys,nsample,isizeBin,iextobs,dave,ddevsq,derr)
	! only valid for enough LARGE number of samples!!
      INTEGER(4), INTENT(in) :: isizeSys, nsample, isizeBin
      INTEGER(4), INTENT(in) :: iextobs(1:nsample)
      DOUBLE PRECISION, INTENT(out) :: dave, ddevsq, derr

      DOUBLE PRECISION :: dintobs(1:nsample)
      DOUBLE PRECISION, ALLOCATABLE :: bin_dintobs(:)

      INTEGER(4) :: nbins
      INTEGER(4) :: ibin

      nbins = nsample / isizeBin
      ALLOCATE(bin_dintobs(1:nbins))

      dintobs(1:nsample) = DBLE(iextobs(1:nsample)) / DBLE(isizeSys)
      Binning: DO ibin = 1, nbins, 1
        bin_dintobs(ibin) = SUM(dintobs(1+(ibin-1)*isizeBin:ibin*isizeBin)) / DBLE(isizeBin)
      END DO Binning

      dave = SUM(bin_dintobs(1:nbins)) / DBLE(nbins)
      ddevsq = SUM(bin_dintobs(1:nbins)**2) / DBLE(nbins) - (SUM(bin_dintobs(1:nbins)) / DBLE(nbins))**2
      derr = SQRT(ddevsq/DBLE(nbins))
    END SUBROUTINE calcAvgs
    
    SUBROUTINE zeropad(d,sd,cfmt_int,cfmt_dec)
      DOUBLE PRECISION, INTENT(in) :: d
      CHARACTER(len=30), INTENT(out) :: sd
      CHARACTER(len=*), INTENT(in) :: cfmt_int, cfmt_dec

      INTEGER(4) :: int_d
      DOUBLE PRECISION :: dec_d

      CHARACTER(len=30) :: sint_d
      CHARACTER(len=30) :: sdec_d

      int_d = INT(d)
      dec_d = d - DBLE(INT(d))

      WRITE(sint_d,TRIM(cfmt_int)) int_d
      WRITE(sdec_d,TRIM(cfmt_dec)) dec_d
      sd = TRIM(sint_d)//TRIM(sdec_d)
    END SUBROUTINE zeropad

    SUBROUTINE convertParamDirname(ilx,ily,dkbt,ivel,sxbc,sybc,sfield,sdirsnap,sdirem)
    ! 平衡状態・非平衡定常状態を記述するパラメータを用いる
      INTEGER(4), INTENT(in) :: ilx, ily
      DOUBLE PRECISION, INTENT(in) :: dkbt
      INTEGER(4), INTENT(in) :: ivel
      CHARACTER(len=*), INTENT(in) :: sxbc, sybc, sfield
      CHARACTER(len=*), INTENT(out) :: sdirsnap, sdirem

      CHARACTER(len=30) :: slx, sly, slt0, slt1, skbt, svel

      WRITE(slx,'(i0.4)') ilx
      WRITE(sly,'(i0.4)') ily
      CALL zeropad(dkbt,skbt,"(i0.2)","(f0.4)")
      WRITE(svel,'(i0.4)') ivel

      sdirsnap = "dat/snap/"//TRIM(slx)//"_"//TRIM(sly)//"_"//TRIM(skbt)//"_"//TRIM(svel)//"_"//TRIM(sxbc)//"_"//TRIM(sybc)//"_"//TRIM(sfield)
      sdirem = "dat/stream/"//TRIM(slx)//"_"//TRIM(sly)//"_"//TRIM(skbt)//"_"//TRIM(svel)//"_"//TRIM(sxbc)//"_"//TRIM(sybc)//"_"//TRIM(sfield)
    END SUBROUTINE convertParamDirname

    SUBROUTINE convertTimeFilesnap(ilt_eq,ilt_noneq,sfilesnap_eq,sfilesnap_noneq)
      INTEGER(4), INTENT(in) :: ilt_eq, ilt_noneq
      CHARACTER(len=*), INTENT(out) :: sfilesnap_eq(1:), sfilesnap_noneq(1:)
      
      CHARACTER(len=30) :: st
      INTEGER(4) :: j

      sweeps_eq: DO j = 1, ilt_eq, 1
        WRITE(st,'(i0.9)') j
        sfilesnap_eq(j) = "eq_t"//TRIM(st)
      END DO sweeps_eq

      sweeps_noneq: DO j = 1, ilt_noneq, 1
        WRITE(st,'(i0.9)') j
        sfilesnap_noneq(j) = "noneq_t"//TRIM(st)
      END DO sweeps_noneq
    END SUBROUTINE convertTimeFilesnap
  END MODULE procs
