* @ValidationCode : MjoyMDE2NTc1ODA0OkNwMTI1MjoxNjgxODg0NjUxOTE4OmFqaXRoOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 19 Apr 2023 11:40:51
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ajith
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOAPAP
SUBROUTINE REDO.APAP.VAL.REG.TITLE.DATE
*********************************************************************************************************
*Company   Name    : APAP Bank
*Developed By      : Temenos Application Management
*Program   Name    : REDO.APAP.VAL.REG.TITLE.DATE
*--------------------------------------------------------------------------------------------------------
*Description       : This routiene ia a validation routine. It is used to check if the Security document
*                    is "INVENTORY SHEET', if YES then make the field Title Registration field mandatory
*                    and also check for date not greater than today
*Linked With       : COLLATERAL,DOC.RECEPTION
*In  Parameter     :
*Out Parameter     :
*Files  Used       : COLLATERAL             As          I Mode
*--------------------------------------------------------------------------------------------------------
*Modification Details:
*=====================
*    Date            Who                  Reference               Description
*   ------         ------               -------------            -------------
* 20/05/2010    Shiva Prasad Y     ODR-2009-10-0310 B.180C      Initial Creation
*---------------------------------------------------------------------------------------
*DATE               WHO                       REFERENCE                 DESCRIPTION
*19-04-2023       CONVERSION TOOLS            AUTO R22 CODE CONVERSION  FM to @FM , VM to @VM ,SM to@SM, ++ to +=
*19-04-2023       AJITHKUMAR                  MANUAL R22 CODE CONVERSION NO CHANGE
*----------------------------------------------------------------------------------------
*
*********************************************************************************************************
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.COLLATERAL
    $INSERT I_F.REDO.COLL.TYPE.DET
*--------------------------------------------------------------------------------------------------------
**********
MAIN.PARA:
**********
    GOSUB OPEN.PARA
    GOSUB PROCESS.PARA

RETURN
*--------------------------------------------------------------------------------------------------------
**********
OPEN.PARA:
**********
    FN.REDO.COLL.TYPE.DET = 'F.REDO.COLL.TYPE.DET'
    F.REDO.COLL.TYPE.DET  = ''
    CALL OPF(FN.REDO.COLL.TYPE.DET,F.REDO.COLL.TYPE.DET)

RETURN
*--------------------------------------------------------------------------------------------------------
*************
PROCESS.PARA:
*************
    Y.DOC.TYPE = ''
    GOSUB FIND.MULTI.LOCAL.REF

    REDO.COLL.TYPE.DET.ID = R.NEW(COLL.LOCAL.REF)<1,LOC.L.CO.SEC.DOC>
    CHANGE @SM TO @FM IN REDO.COLL.TYPE.DET.ID
    Y.COUNT = DCOUNT(REDO.COLL.TYPE.DET.ID,@FM)
    Y.CNT = 1
    LOOP
    WHILE Y.CNT LE Y.COUNT
        GOSUB READ.REDO.COLL.TYPE.DET

        IF R.NEW(COLL.LOCAL.REF)<1,LOC.L.CO.SEC.DOC> NE  "" THEN
            IF Y.DOC.TYPE<1,Y.CNT> EQ 'INVENTORY SHEET' OR Y.DOC.TYPE<1,Y.CNT> EQ 'HOJA DE INVENTARIO' THEN
                IF NOT(R.NEW(COLL.LOCAL.REF)<1,LOC.L.CO.REG.DATE>) THEN
                    AF = COLL.LOCAL.REF
                    AV = LOC.L.CO.REG.DATE
                    ETEXT = 'CO-MANDATORY.REG.DATE'
                    CALL STORE.END.ERROR
                    RETURN
                END
            END ELSE
                IF R.NEW(COLL.LOCAL.REF)<1,LOC.L.CO.REG.DATE> THEN
                    AF = COLL.LOCAL.REF
                    AV = LOC.L.CO.REG.DATE
                    ETEXT = 'CO-MANDATORY.REG.DATE'
                    CALL STORE.END.ERROR
                    RETURN
                END
            END
        END
        Y.CNT += 1 ;*R22 AUTO CODE CONVERSION
    REPEAT

    IF R.NEW(COLL.LOCAL.REF)<1,LOC.L.CO.REG.DATE> GT TODAY THEN
        ETEXT = 'CO-TITLE.REG.GT.TODAY'
        CALL STORE.END.ERROR
    END

RETURN
*--------------------------------------------------------------------------------------------------------
************************
READ.REDO.COLL.TYPE.DET:
************************
    R.REDO.COLL.TYPE.DET  = ''
    REDO.COLL.TYPE.DET.ER = ''
    REDO.COLL.TYPE.DET.NEW.ID = REDO.COLL.TYPE.DET.ID<Y.CNT>
    CALL F.READ(FN.REDO.COLL.TYPE.DET,REDO.COLL.TYPE.DET.NEW.ID,R.REDO.COLL.TYPE.DET,F.REDO.COLL.TYPE.DET,REDO.COLL.TYPE.DET.ER)
    IF NOT(Y.DOC.TYPE)  THEN
        Y.DOC.TYPE = R.REDO.COLL.TYPE.DET<DOC.DET.DOCUMENT.TYPE>
    END ELSE
        Y.DOC.TYPE := @VM:R.REDO.COLL.TYPE.DET<DOC.DET.DOCUMENT.TYPE>
    END

RETURN
*--------------------------------------------------------------------------------------------------------
*********************
FIND.MULTI.LOCAL.REF:
*********************
    APPL.ARRAY = 'COLLATERAL'
    FLD.ARRAY  = 'L.CO.SEC.DOC':@VM:'L.CO.REG.DATE'
    FLD.POS    = ''
    CALL MULTI.GET.LOC.REF(APPL.ARRAY,FLD.ARRAY,FLD.POS)
    LOC.L.CO.SEC.DOC       = FLD.POS<1,1>
    LOC.L.CO.REG.DATE      = FLD.POS<1,2>

RETURN
*---------------------------------------------------------------------------------------------------------------------------
END
