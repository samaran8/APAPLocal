* @ValidationCode : MjotMTEzOTI3NDAyMjpDcDEyNTI6MTY4MTI5Nzc0NDc4NjozMzNzdTotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 12 Apr 2023 16:39:04
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : 333su
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
SUBROUTINE REDO.PREVALANCE.STATUS.PROCESS
*********************************************************************************************************
*Company   Name    : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
*Program   Name    : REDO.PREVALANCE.STATUS.PROCESS
*--------------------------------------------------------------------------------------------------------
*Description       : This routine is a authorization routine for attaching the template REDO.PREVALANCE.STATUS.VALIDATE
*In Parameter      :
*Out Parameter     :
*--------------------------------------------------------------------------------------------------------
*Modification Details:
*=====================
*    Date            Who                            Reference                      Description
*   ------         ------                         -------------                    -------------
*  21/09/2011      Riyas                         ODR-2010-08-0490                Initial Creation
*
*Modification History:
*DATE                 WHO                  REFERENCE                     DESCRIPTION
*12/04/2023      CONVERSION TOOL     AUTO R22 CODE CONVERSION        FM TO @FM, VM TO @VM, SM TO @SM, Y.CNT +  TO +=, Y.CNT.SM + 1 TO +=
*12/04/2023         SURESH           MANUAL R22 CODE CONVERSION           NOCHANGE
*********************************************************************************************************

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.REDO.PREVALANCE.STATUS

    GOSUB OPEN.FILES
    GOSUB PROCESS
    GOSUB GOEND
RETURN

************
OPEN.FILES:
************
    FN.REDO.PREVALANCE.STATUS = 'F.REDO.PREVALANCE.STATUS'
    F.REDO.PREVALANCE.STATUS = ''
    CALL OPF(FN.REDO.PREVALANCE.STATUS,F.REDO.PREVALANCE.STATUS)
    Y.FINAL.STATUS = ''; Y.FM.STATUS='';LOOP.SM.CNTR ='' ; LOOP.FM.CNTR = '' ; STAT.FM.CNTR = ''; STAT.SM.CNTR = ''; Y.FLAG = ''; Y.ERROR = ''
RETURN
*--------------------------------------------------------------------------------
PROCESS:
*--------------------------------------------------------------------------------

    PARAM.STATUS = R.NEW(REDO.PRE.STATUS)
    Y.VM.ARRAY = DCOUNT(PARAM.STATUS,@VM)
    Y.SM.ARRAY = DCOUNT(PARAM.STATUS,@SM)

    Y.CNT = 1
    Y.FLAG1 = ''
    LOOP
    WHILE Y.CNT LE Y.VM.ARRAY
        Y.LIST.VAL = ''
        Y.CHECK.VAL = PARAM.STATUS<1,Y.CNT>

        GOSUB INNNER.LOOP
        IF  Y.FLAG AND Y.FLAG1 THEN
            AV =Y.CNT
            AF = REDO.PRE.STATUS
            ETEXT = "EB-AC.STATUS.DUPICATE"
            CALL STORE.END.ERROR
            Y.CNT += Y.VM.ARRAY ;*AUTO R22 CODE CONVERSION
        END
        Y.CNT += 1 ;*AUTO R22 CODE CONVERSION
    REPEAT
RETURN
*-----------
INNNER.LOOP:
*-----------
    Y.IN.CNT = Y.CNT + 1
    LOOP
    WHILE Y.IN.CNT LE Y.VM.ARRAY
        Y.LIST.VAL = PARAM.STATUS<1,Y.IN.CNT>
        Y.MY.CNT = DCOUNT(Y.LIST.VAL,@SM)
        GOSUB CHEC.DUP
        Y.IN.CNT += 1 ;*AUTO R22 CODE CONVERSION
    REPEAT
RETURN
*-----------
CHEC.DUP:
*-----------
    CHANGE @VM TO @FM IN Y.LIST.VAL
    CHANGE @SM TO @FM IN Y.LIST.VAL
    Y.COUNT = DCOUNT(Y.CHECK.VAL,@SM)
    IF Y.COUNT EQ Y.MY.CNT THEN
        Y.FLAG1 = 1
    END ELSE
        Y.FLAG1 = ''
    END
    Y.CNT.SM = 1
    LOOP
    WHILE Y.CNT.SM LE Y.COUNT
        LOCATE Y.CHECK.VAL<1,1,Y.CNT.SM> IN Y.LIST.VAL SETTING POS THEN
            Y.FLAG = '1'
        END ELSE
            Y.FLAG = ''
            Y.CNT.SM += Y.COUNT ;*AUTO R22 CODE CONVERSION
        END
        Y.CNT.SM += 1 ;*AUTO R22 CODE CONVERSION
    REPEAT
    IF  Y.FLAG AND Y.FLAG1 THEN
        AV =Y.CNT
        AF = REDO.PRE.STATUS
        ETEXT = "EB-AC.STATUS.DUPICATE"
        CALL STORE.END.ERROR
        GOSUB GOEND
    END
RETURN
*----------------
GOEND:
*---------------_
END
