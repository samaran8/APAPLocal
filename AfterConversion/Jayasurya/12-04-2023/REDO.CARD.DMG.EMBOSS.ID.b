* @ValidationCode : MjoxMDQ3OTUwNDMxOkNwMTI1MjoxNjgxMjA3NzU3NDEzOklUU1NCTkc6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 11 Apr 2023 15:39:17
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.REDORETAIL
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*11-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*11-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*--------------------------------------------------------------------------------------------------------------------
SUBROUTINE REDO.CARD.DMG.EMBOSS.ID
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.CARD.DMG.EMBOSS


    FN.REDO.CARD.GEN = 'F.REDO.CARD.GENERATION'
    F.REDO.CARD.GEN = ''
    CALL OPF(FN.REDO.CARD.GEN,F.REDO.CARD.GEN)

    IF V$FUNCTION EQ 'I' THEN
        CALL F.READ(FN.REDO.CARD.GEN,ID.NEW,R.REDO.CARD.GEN,F.REDO.CARD.GEN,GEN.ERR)
        IF R.REDO.CARD.GEN THEN
            E='ST-DAMAGE.MARK'
            CALL STORE.END.ERROR
        END

        R.NEW(DMG.LST.REG.ID)=''
        R.NEW(DMG.LST.CARD.TYPE)=''
        R.NEW( DMG.LST.SERIES)=''
        R.NEW( DMG.LST.LOST)=''
        R.NEW( DMG.LST.LOST.DESC )=''
        R.NEW( DMG.LST.DAMAGE)=''
        R.NEW( DMG.LST.DAM.DESC)=''
        R.NEW( DMG.LST.MOVE.FROM.INIT)=''
    END


RETURN

END
