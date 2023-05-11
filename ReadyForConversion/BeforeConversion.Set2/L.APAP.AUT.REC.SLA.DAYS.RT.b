*-----------------------------------------------------------------------------
* <Rating>-20</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.AUT.REC.SLA.DAYS.RT
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.FRONT.CLAIMS
    $INSERT I_F.REDO.ISSUE.CLAIMS

    GOSUB INI
    GOSUB PROCESS

INI:
    Y.ISS.CL.OPENING.DATE = R.NEW(ISS.CL.OPENING.DATE)
    Y.ISS.CL.DATE.RESOLUTION = R.NEW(ISS.CL.DATE.RESOLUTION)
    DAYS = "C"
    RETURN
*ToDO; If DATE.RESOLUTION is not null, set up the field CLAIM.SLA with the difference in days between OPENING.DATE & DATE.RESOLUTION
PROCESS:
    IF Y.ISS.CL.DATE.RESOLUTION NE "" THEN
        CALL CDD("",Y.ISS.CL.OPENING.DATE,Y.ISS.CL.DATE.RESOLUTION,DAYS)
        CALL GET.LOC.REF("REDO.ISSUE.CLAIMS", "CLAIM.SLA",LR.POS)
        R.NEW(ISS.CL.LOCAL.REF)<1,LR.POS> = DAYS
    END
    RETURN

END
