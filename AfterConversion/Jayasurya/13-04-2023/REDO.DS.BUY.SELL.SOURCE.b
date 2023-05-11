* @ValidationCode : MjotMTc3ODcxNDIwOkNwMTI1MjoxNjgxMzc4MDgzNjE1OklUU1NCTkc6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 13 Apr 2023 14:58:03
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
SUBROUTINE REDO.DS.BUY.SELL.SOURCE(IN.OUT.PARA)
*------------------------------------------------------------------------------------------------------------
* DESCRIPTION : This deal slip routine should be attached to the DEAL.SLIP.FORMAT, REDO.BUY.SELL.DSLIP
*------------------------------------------------------------------------------------------------------------
* * Input / Output
* --------------
* IN Parameter    : NA
* OUT Parameter   : NA
*--------------------------------------------------------------------------------------------------------------
* COMPANY NAME : APAP
* DEVELOPED BY : PRADEEP S
* PROGRAM NAME : REDO.DS.BUY.SELL.SOURCE
*--------------------------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
* Date             Author             Reference                   Description
* 29-APR-2011      Pradeep S         PACS00054288                 Initial Creation
* 27-Jun-2012      Pradeep S         PACS00204543                 Language specific chages
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*13-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*13-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*----------------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.BUY.SELL.SOURCE

    GOSUB INIT
    GOSUB PROCESS

RETURN

*****
INIT:
*****
* Initialisation of variables
*
    Y.ID = IN.OUT.PARA

    FN.REDO.BUY.SELL.SOURCE = 'F.REDO.BUY.SELL.SOURCE'
    F.REDO.BUY.SELL.SOURCE = ''

    CALL OPF(FN.REDO.BUY.SELL.SOURCE,F.REDO.BUY.SELL.SOURCE)

RETURN

********
PROCESS:
********
* Getthe description for source

    R.REC.SOURCE = ''
    CALL F.READ(FN.REDO.BUY.SELL.SOURCE,Y.ID,R.REC.SOURCE,F.REDO.BUY.SELL.SOURCE,ERR.SOURCE)
    IF R.REC.SOURCE THEN
        IN.OUT.PARA = R.REC.SOURCE<BS.SRC.DESCRIPTION,LNGG>
        IF NOT(IN.OUT.PARA) THEN
            IN.OUT.PARA = R.REC.SOURCE<BS.SRC.DESCRIPTION,1>
        END
    END

RETURN

END
