$PACKAGE APAP.TAM
SUBROUTINE REDO.UPDATE.CHANGE.INTEREST
*-----------------------------------------------------------------------------
* CODED BY      : JEEVA T
* ODR           : ODR-201003178.RRD152
*-----------------------------------------------------------------------------
*  Description of the routine
*-----------------------------------------------------------------------------
*REDO.UPDATE.CHANGE.INTEREST is used to populate the CONCAT file REDO.CHANGE.INT.ARR file with the
*Arrangement id. This is triggered as a post routine when LENDING-CHANGE-INTEREST
*is triggered

** 18-04-2023 R22 Auto Conversion no changes
** 18-04-2023 Skanda R22 Manual Conversion - No changes
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_AA.LOCAL.COMMON
    $INSERT I_F.AA.INTEREST

    GOSUB GET.LOC.VALUES
    GOSUB OPENFILES
    GOSUB PROCESS


RETURN
*-----------------------------------------------------------------------------
OPENFILES:
*-----------------------------------------------------------------------------

    FN.REDO.CHANGE.INT.ARRANGEMENT='F.REDO.CHANGE.INT.ARRANGEMENT'
    F.REDO.CHANGE.INT.ARRANGEMENT=''
    CALL OPF(FN.REDO.CHANGE.INT.ARRANGEMENT,F.REDO.CHANGE.INT.ARRANGEMENT)
    R.REDO.CHANGE.INT.ARRANGEMENT=''
RETURN

*-----------------------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------------------

    Y.AA.ID=c_aalocArrId
    GOSUB GET.ARR.COND
    Y.ID.NEW=Y.AA.ID:"-":Y.NEXT.REVIEW.DATE
    CALL F.WRITE(FN.REDO.CHANGE.INT.ARRANGEMENT,Y.ID.NEW,R.REDO.CHANGE.INT.ARRANGEMENT)
    CALL JOURNAL.UPDATE(Y.ID.NEW)
RETURN

*-----------------------------------------------------------------------------
ARR.CONDITIONS:
*-----------------------------------------------------------------------------
    ArrangementID = Y.AA.ID ; idProperty = ''; effectiveDate = ''; returnIds = ''; R.CONDITION =''; returnConditions =''; returnError = ''
    CALL AA.GET.ARRANGEMENT.CONDITIONS(ArrangementID, idPropertyClass, idProperty, effectiveDate, returnIds, returnConditions, returnError)
RETURN
*-----------------------------------------------------------------------------
GET.ARR.COND:
*-----------------------------------------------------------------------------
    Y.NEXT.REVIEW.DATE = R.NEW(AA.INT.LOCAL.REF)<1,Y.NEXT.REV.DATE.POS>
RETURN
*-----------------------------------------------------------------------------
GET.LOC.VALUES:
*-----------------------------------------------------------------------------

    LOC.REF.APPL="AA.PRD.DES.INTEREST"
    LOC.REF.FIELDS="L.AA.NXT.REV.DT"
    LOC.REF.POS=""
    CALL MULTI.GET.LOC.REF(LOC.REF.APPL,LOC.REF.FIELDS,LOC.REF.POS)
    Y.NEXT.REV.DATE.POS  =  LOC.REF.POS<1,1>

RETURN
*-----------------------------------------------------------------------------
END
