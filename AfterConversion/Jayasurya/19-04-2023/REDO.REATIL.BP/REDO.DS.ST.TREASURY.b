* @ValidationCode : MjoxMTM2MTIzMDM5OkNwMTI1MjoxNjgxODgxNDU5MjM0OklUU1NCTkc6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 19 Apr 2023 10:47:39
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
SUBROUTINE REDO.DS.ST.TREASURY(Y.RET)
***********************************************************************
* COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* DEVELOPED BY: Arulprakasam P
* PROGRAM NAME: REDO.DS.ST.SELLERS
* ODR NO      : ODR-2010-07-0082
*----------------------------------------------------------------------
*DESCRIPTION: This routine is attched in DEAL.SLIP.FORMAT 'REDO.BUS.SELL'
* to get the details of the Product selected for LETTER

*IN PARAMETER:  NA
*OUT PARAMETER: NA
*LINKED WITH:
*----------------------------------------------------------------------
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*13-04-2023            CONVERSION TOOL                AUTO R22 CODE CONVERSION           VM TO @VM ,FM TO @FM SM TO @SM and I++ to I=+1
*13-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES

*----------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.SEC.TRADE

    GOSUB PROCESS
RETURN

PROCESS:
********

    ST.OVERRIDE = R.NEW(SC.SBS.OVERRIDE)
    IF ST.OVERRIDE NE '' THEN
        CHANGE @VM TO @FM IN ST.OVERRIDE
        Y.COUNT = DCOUNT(ST.OVERRIDE,@FM)
        INIT = 1
        LOOP
        WHILE INIT LE Y.COUNT
            REMOVE ST.OVERRIDE.1 FROM ST.OVERRIDE SETTING POS
            FINDSTR '*' IN ST.OVERRIDE.1 SETTING POS.1 THEN
                Y.TRA = FIELD(ST.OVERRIDE.1,'*',3,1)
            END
            INIT += 1
        REPEAT
    END
    Y.RET = Y.TRA
RETURN
END
