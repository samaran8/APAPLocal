* @ValidationCode : Mjo1NzcxMjE2MTk6Q3AxMjUyOjE2ODMwOTE0NTQ3NjQ6aGFpOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 03 May 2023 10:54:14
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : hai
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
$PACKAGE APAP.AA ;*MANUAL R22 CODE CONVERSION
SUBROUTINE AA.GET.INTEREST.RATE.MIGRATE


*--------------------------------------------------------------------------------
* Company Name : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Program Name : AA.GET.INTEREST.RATE
*--------------------------------------------------------------------------------
* Description: This is the Post routine for the INTEREST property to update the concat file
* REDO.T.DEP.COLLATERAL during migration.
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
*  DATE             WHO           REFERENCE          DESCRIPTION
* 11 feb 2013    H Ganesh      Migration Issue      Initial Creation.
* 29/03/2023         SURESH        MANUAL R22 CODE CONVERSION    Package Name added APAP.AA
* 29/03/2023     Conversion Tool   AUTO R22 CODE CONVERSION       NO CHANGE
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_AA.LOCAL.COMMON
    $INSERT I_F.AA.INTEREST
    $INSERT I_F.AA.LIMIT



    IF c_aalocActivityStatus EQ 'AUTH' THEN
        GOSUB PROCESS
    END

RETURN

*-----------------------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------------------

    LOC.REF.APPLICATION = "AA.PRD.DES.INTEREST"
    LOC.REF.FIELDS = 'L.AA.REV.RT.TY'
    LOC.REF.POS = ''
    CALL MULTI.GET.LOC.REF(LOC.REF.APPLICATION, LOC.REF.FIELDS, LOC.REF.POS)
    POS.L.AA.REV.RT.TY = LOC.REF.POS<1,1>

    FN.REDO.T.DEP.COLLATERAL = 'F.REDO.T.DEP.COLLATERAL'
    F.REDO.T.DEP.COLLATERAL = ''
    CALL OPF(FN.REDO.T.DEP.COLLATERAL,F.REDO.T.DEP.COLLATERAL)

    R.REDO.T.DEP.COLLATERAL = ''

    GOSUB GET.LIMIT.DETAILS

    Y.INT.PAY.TYPE=R.NEW(AA.INT.LOCAL.REF)<1,POS.L.AA.REV.RT.TY>

    IF Y.INT.PAY.TYPE NE 'BACK.TO.BACK' THEN
        RETURN
    END

    Y.ID = c_aalocArrId
    CALL F.WRITE(FN.REDO.T.DEP.COLLATERAL,Y.ID,R.REDO.T.DEP.COLLATERAL)

RETURN
*-----------------------------------------------------------------------------
GET.LIMIT.DETAILS:
*-----------------------------------------------------------------------------
    ARR.ID      = c_aalocArrId
    EFF.DATE    = c_aalocActivityEffDate
    PROP.CLASS  = 'LIMIT'
    PROPERTY    = ''
    R.CON.LIMIT = ''
    ERR.MSG     = ''
    CALL APAP.AA.redoCrrGetConditions(ARR.ID,EFF.DATE,PROP.CLASS,PROPERTY,R.CON.LIMIT,ERR.MSG)
    Y.LIMIT.REFERENCE = R.CON.LIMIT<AA.LIM.LIMIT.REFERENCE>
    IF Y.LIMIT.REFERENCE EQ '' THEN
        GOSUB END1
    END

RETURN
*-----------------------------------------------------------------------------
END1:
END
