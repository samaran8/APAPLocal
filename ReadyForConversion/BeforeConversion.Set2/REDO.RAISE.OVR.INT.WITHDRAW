*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE REDO.RAISE.OVR.INT.WITHDRAW
*-----------------------------------------------------------

*-----------------------------------------------------------
* Input  Arg: N/A
* Output Arg: N/A
* Deals With: TO Raise Override to have an approval for the withdraw from an internal account using the version TELLER,REDO.EFC.PAG.OTROS
*--------------------------------------------------------------
* Who           Date           Dev Ref           Modification
* APAP          22 May 2017    RTE Fix           Initial Draft
*--------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.VERSION
    $INSERT I_GTS.COMMON
    $INSERT I_F.TELLER

    IF OFS$OPERATION NE 'PROCESS' THEN
        RETURN
    END

    CURR.NO = DCOUNT(R.NEW(TT.TE.OVERRIDE),VM)
    VAR.OVERRIDE.ID = 'REDO.AUTH.REQUIRED'
    TEXT    = VAR.OVERRIDE.ID
    CALL STORE.OVERRIDE(CURR.NO+1)

    RETURN

END
