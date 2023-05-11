$PACKAGE APAP.TAM
SUBROUTINE REDO.H.BANK.DRAFTS.AUTHORISE
*--------------------------------------------------------------------------------------------------------
*Company   Name    : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
*Developed By      : Temenos Application Management
*Program   Name    : REDO.H.BANK.DRAFTS.AUTHORISE
*--------------------------------------------------------------------------------------------------------
*Description  : This is a authorisation routine to
*Linked With  : REDO.H.BANK.DRAFTS.AUTHORISE
*In Parameter : N/A
*Out Parameter: N/A
*--------------------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
*    Date            Who                      Reference               Description
*   ------         ------                    -------------            -------------
*  10-10-2011       JEEVA T                     B.42
** 11-04-2023 R22 Auto Conversion no changes
** 11-04-2023 Skanda R22 Manual Conversion - No changes
*--------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.H.BANK.DRAFTS
*--------------------------------------------------------------------------------------------------------
**********
MAIN.PARA:
**********
    GOSUB OPEN.FILE
    GOSUB PROCESS.PARA

RETURN
*--------------------------------------------------------------------------------------------------------
OPEN.FILE:
*--------------------------------------------------------------------------------------------------------

    FN.REDO.ITEM.SERIES = 'F.REDO.ITEM.SERIES'
    F.REDO.ITEM.SERIES =''
    CALL OPF(FN.REDO.ITEM.SERIES,F.REDO.ITEM.SERIES)

RETURN
*--------------------------------------------------------------------------------------------------------
PROCESS.PARA:
*--------------------------------------------------------------------------------------------------------

    R.REDO.ITEM.SERIES = ''
    Y.COMPANY = R.NEW(REDO.BANK.BRANCH.DEPT)
    Y.CODE =    R.NEW(REDO.BANK.CODE)
    Y.ITEM =    R.NEW(REDO.BANK.ITEM.CODE)
    Y.FRM =  R.NEW(REDO.BANK.SERIAL.NO)

    Y.STATUS = R.NEW(REDO.BANK.STATUS)
    IF Y.STATUS EQ 'AVAILABLE' THEN
        IF Y.CODE THEN
            Y.ID = Y.COMPANY:'-':Y.CODE:'.':Y.ITEM
        END ELSE
            Y.ID = Y.COMPANY:'.':Y.ITEM
        END
        CALL F.READ(FN.REDO.ITEM.SERIES,Y.ID,R.REDO.ITEM.SERIES,F.REDO.ITEM.SERIES,Y.ERR)
        R.REDO.ITEM.SERIES<-1> = Y.FRM:'*':Y.STATUS:'*':ID.NEW:"*":R.NEW(REDO.BANK.DATE.UPDATED)
        CALL F.WRITE(FN.REDO.ITEM.SERIES,Y.ID,R.REDO.ITEM.SERIES)
    END
RETURN
END
