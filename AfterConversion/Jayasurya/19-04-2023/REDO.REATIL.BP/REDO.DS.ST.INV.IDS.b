* @ValidationCode : MjotMTkwMzE5MTUyNzpDcDEyNTI6MTY4MTg4MTMzMzc0NzpJVFNTQk5HOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 19 Apr 2023 10:45:33
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
SUBROUTINE REDO.DS.ST.INV.IDS(Y.RET)
***********************************************************************
* COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* DEVELOPED BY: Arulprakasam P
* PROGRAM NAME: REDO.DS.SECURITY.TERM
* ODR NO      : ODR-2010-07-0082
*----------------------------------------------------------------------
*DESCRIPTION: This routine is attched in DEAL.SLIP.FORMAT 'REDO.BUS.SELL'
* to get the details of the Product selected for LETTER

*IN PARAMETER:  NA
*OUT PARAMETER: NA
*LINKED WITH:
*----------------------------------------------------------------------
* Modification History :
*-----------------------
*DATE           WHO           REFERENCE         DESCRIPTION
*28-Mar-10      Pradeep S     PACS00051213      Mapping changed to Alt Sec ID
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*13-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*13-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*----------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.SEC.TRADE
    $INSERT I_F.SECURITY.MASTER

    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
RETURN

INIT:
*****
    FN.SECURITY.MASTER = 'F.SECURITY.MASTER'
    F.SECURITY.MASTER = ''

RETURN

OPENFILES:
**********
    CALL OPF(FN.SECURITY.MASTER,F.SECURITY.MASTER)
RETURN

PROCESS:
********

    SEC.TRADE.ID = Y.RET

    SEC.CODE.ID = R.NEW(SC.SBS.SECURITY.CODE)

    CALL F.READ(FN.SECURITY.MASTER,SEC.CODE.ID,R.SECURITY.MASTER,F.SECURITY.MASTER,SECURITY.MASTER.ERR)
    Y.ALT.SECURITY.ID = R.SECURITY.MASTER<SC.SCM.ALT.SECURITY.ID>
    Y.ALT.SECURITY.NO = R.SECURITY.MASTER<SC.SCM.ALT.SECURITY.NO>       ;*PACS00051213 - S/E

*PACS00051213 - S
    LOCATE "ALT-NUM-INV" IN Y.ALT.SECURITY.ID<1,1> SETTING POS THEN
* Y.SERIES.NO = Y.ALT.SECURITY.ID<1,POS>
        Y.SERIES.NO = Y.ALT.SECURITY.NO<1,POS>
    END
*PACS00051213 - E

    Y.RET = Y.SERIES.NO
RETURN

END
