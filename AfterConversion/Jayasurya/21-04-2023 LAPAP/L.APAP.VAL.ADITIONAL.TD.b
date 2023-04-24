* @ValidationCode : MjoxNjk0MjQ5ODkzOkNwMTI1MjoxNjgyMDcwMTYzODU1OklUU1NCTkc6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 21 Apr 2023 15:12:43
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.LAPAP
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*21-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 BP REMOVED, END ADDED
*21-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*-------------------------------------------------------------------------------------------------------
SUBROUTINE L.APAP.VAL.ADITIONAL.TD

    $INSERT I_COMMON ;* AUTO R22 CODE CONVERSION START
    $INSERT I_EQUATE
    $INSERT I_F.LATAM.CARD.ORDER ;* AUTO R22 CODE CONVERSION END

*--------------------------------------------------------------------------------------------------------
*--------------------------------------------------------------------------------------------------------
*Description  :
*Linked With  : LATAM.CARD.ORDER,REDO.ADDITIONAL
*In Parameter : N/A
*Out Parameter: N/A
*--------------------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
*    Date            Who                  Reference               Description
*   ------         ------               -------------            -------------
* 22 06 2020    Estalin Valerio           Proyecto Digitalizacion   Initial Creation
*--------------------------------------------------------------------------------------------------------


    GOSUB PROCESS
RETURN

PROCESS:
*--------

    IF R.NEW(CARD.IS.TYPE.OF.CARD) EQ "ADICIONAL" AND LEN(R.NEW(CARD.IS.PROSPECT.ID)) EQ 0 THEN

*AF=CARD.IS.TYPE.OF.CARD
*ETEXT='EB-NO.ADIT.CARD'
*CALL STORE.END.ERROR

        TEXT="L.APAP.VAL.TD.ADI"
        CURR.NO=1
        CALL STORE.OVERRIDE(CURR.NO)
        RETURN

    END
    ELSE ;* AUTO R22 CODE CONVERSION
        IF R.NEW(CARD.IS.TYPE.OF.CARD) EQ "PRINCIPAL" AND LEN(R.NEW(CARD.IS.PROSPECT.ID)) NE 0 THEN
            TEXT="L.APAP.VAL.TD.PRIN"
            CURR.NO=1
            CALL STORE.OVERRIDE(CURR.NO)
            RETURN
        END
    END ;* AUTO R22 CODE CONVERSION

RETURN
END
