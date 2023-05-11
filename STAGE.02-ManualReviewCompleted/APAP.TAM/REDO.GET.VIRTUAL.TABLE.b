$PACKAGE APAP.TAM
SUBROUTINE REDO.GET.VIRTUAL.TABLE(APPL,LOC.REF.POS,Y.VIRTUAL.TABLE,ERR)

*--------------------------------------------------------------------------------
* Company Name : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Program Name : REDO.GET.VIRTUAL.TABLE
*--------------------------------------------------------------------------------
* Description: This routine is to get the virtual table of local field
********************
* Input Arguments
********************
*  APPL           -> Application
*  LOC.REF.POS    -> Local ref Position in LRT
********************
* Out Arguments
********************
* Y.VIRTUAL.TABLE   -> Virtual Table Name
* ERR             -> Error if any
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
*
*  DATE             WHO         REFERENCE                  DESCRIPTION
* 29-Apr-2011   H GANESH        N.107 CUSTOMER POSITION   INITIAL CREATION
*
** 10-04-2023 R22 Auto Conversion 
** 10-04-2023 Skanda R22 Manual Conversion - No changes
*----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.LOCAL.REF.TABLE
    $INSERT I_F.LOCAL.TABLE

    GOSUB INIT
    GOSUB PROCESS
RETURN
*---------------------------------------------------------------------------------
INIT:
*---------------------------------------------------------------------------------

    VIRTUAL.TABLE=''
    ERR=''

    FN.LOCAL.REF.TABLE='F.LOCAL.REF.TABLE'
    F.LOCAL.REF.TABLE=''
    CALL OPF(FN.LOCAL.REF.TABLE,F.LOCAL.REF.TABLE)

    FN.LOCAL.TABLE='F.LOCAL.TABLE'
    F.LOCAL.TABLE=''
    CALL OPF(FN.LOCAL.TABLE,F.LOCAL.TABLE)

RETURN
*---------------------------------------------------------------------------------
PROCESS:
*---------------------------------------------------------------------------------
* This part gets the virtual table name

    CALL CACHE.READ(FN.LOCAL.REF.TABLE, APPL, R.LRT, LRT.ERR) ;* R22 Auto conversion

    IF LRT.ERR THEN
        ERR='No LRT for this mentioned application'
        RETURN
    END

    Y.LT.ID=R.LRT<EB.LRT.LOCAL.TABLE.NO,LOC.REF.POS>
    CALL CACHE.READ(FN.LOCAL.TABLE, Y.LT.ID, R.LT, LT.ERR) ;* R22 Auto conversion

    IF LT.ERR THEN
        ERR='No LT'
        RETURN
    END

    Y.VIRTUAL.TABLE=R.LT<EB.LTA.VIRTUAL.TABLE>

RETURN
END
