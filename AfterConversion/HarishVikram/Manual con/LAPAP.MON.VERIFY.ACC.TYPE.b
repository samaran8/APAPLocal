SUBROUTINE LAPAP.MON.VERIFY.ACC.TYPE
*-----------------------------------------------------------------------------

*MODIFICATION HISTORY:

*

* DATE              WHO                REFERENCE                 DESCRIPTION

* 21-APR-2023     Conversion tool    R22 Auto conversion       BP is removed in Insert File

*-----------------------------------------------------------------------------

    $INSERT I_COMMON ;*R22 Auto conversion - START
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT ;*R22 Auto conversion - END

    ACC = COMI
    CALL LAPAP.VERIFY.ACC(ACC,RES)
    COMI = RES

RETURN

END
