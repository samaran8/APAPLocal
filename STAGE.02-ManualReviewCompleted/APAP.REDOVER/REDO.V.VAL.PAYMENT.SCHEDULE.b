* @ValidationCode : MjotMTMzNTUwMTA5NTpDcDEyNTI6MTY4MTczNzQwODQyNDpzYW1hcjotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 17 Apr 2023 18:46:48
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : samar
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOVER
SUBROUTINE REDO.V.VAL.PAYMENT.SCHEDULE
*------------------------------------------------------------------------------------------------------------------
* Developer    : HARISH.Y
* Date         : 20.08.2010
* Description  : REDO.V.VAL.PAYMENT.SCHEDULE
*------------------------------------------------------------------------------------------------------------------
* Input/Output:
* -------------
* In  : --N/A--
* Out : --N/A--
*------------------------------------------------------------------------------------------------------------------
* Dependencies:
* -------------
* Calls     : --N/A--
* Called By : --N/A--
*------------------------------------------------------------------------------------------------------------------
* Revision History:
* -----------------
* Version          Date          Name              Description
* -------          ----          ----              ------------
*  1.0           20.08.2010    HARISH.Y      REDO.V.VAL.PAYMENT.SCHEDULE
*------------------------------------------------------------------------------------------------------------------
*Modification History
*DATE                       WHO                         REFERENCE                                   DESCRIPTION
*17-04-2023            Conversion Tool             R22 Auto Code conversion                      SM TO @SM VM TO @VM,IF CONDITION ADDED
*17-04-2023              Samaran T                R22 Manual Code conversion                         CALL ROUTINE FORMAT MODIFIED
*------------------------------------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AA.PAYMENT.SCHEDULE
    $INSERT I_F.AA.CHARGE
    $INSERT I_AA.LOCAL.COMMON
    $INSERT I_System

    GOSUB INIT
    GOSUB MULTI.GET
    GOSUB PROCESS

RETURN

*-------------------------------------------------------------------------------------------------
*-------------
INIT:
*-------------

    ARR.ID=c_aalocArrId
    EFF.DATE=TODAY
    PROP.CLASS='CHARGE'

    PRODUCT.RECORD=c_aalocProductRecord
    CALL AA.GET.PROPERTY.NAME (PRODUCT.RECORD,PROP.CLASS,Y.PROPERTY)

    PROPERTY=''
    PROPERTY = Y.PROPERTY
    R.Condition=''
    ERR.MSG=''
    CALL APAP.REDOFCFI.REDO.CRR.GET.CONDITIONS(ARR.ID,EFF.DATE,PROP.CLASS,PROPERTY,R.Condition.charge,ERR.MSG)    ;*R22 MANUAL CODE CONVERSION
RETURN


*------------------
MULTI.GET:
*------------------

    Y.APPLICATION = "AA.PRD.DES.CHARGE"
    Y.FIELD.NAME = "CLASS.POLICY":@VM:"INS.POLICY.TYPE"
    Y.FIELD.POS = ""
    CALL MULTI.GET.LOC.REF(Y.APPLICATION,Y.FIELD.NAME,Y.FIELD.POS)

    CLASS.POLICY.POS = Y.FIELD.POS<1,1>
    INS.POLICY.TYPE.POS = Y.FIELD.POS<1,2>





RETURN

*------------------
PROCESS:
*------------------

    START.DATE = R.NEW(AA.PS.START.DATE)
    END.DATE = R.NEW(AA.PS.END.DATE)

    Y.INITIAL=1
    IF NOT(START.DATE) THEN
        AF = AA.PS.START.DATE
        AV = Y.INITIAL
        ETEXT = 'EB-PAY.SCH.START.DATE.MISSING'
        CALL STORE.END.ERROR
    END
    IF NOT(END.DATE) THEN
        AF = AA.PS.END.DATE
        AV = Y.INITIAL
        ETEXT = 'EB-PAY.SCH.END.DATE.MISSING'
        CALL STORE.END.ERROR
    END

    IF R.Condition.charge EQ '' THEN
        VAR.INS.POL.TYPE =System.getVariable('INS.POL.TYPE')
        IF E EQ "EB-UNKNOWN.VARIABLE" THEN     ;*R22 AUTO CODE CONVERSION.START
            VAR.INS.POL.TYPE = ""   ;*R22 AUTO CODE CONVERSION
        END  ;*R22 AUTO CODE CONVERSION.END
        VAR.CLASS.POLICY =System.getVariable('CLASS.POLICY')
        IF E EQ "EB-UNKNOWN.VARIABLE" THEN    ;*R22 AUTO CODE CONVERSION.START
            VAR.CLASS.POLICY = ""    ;*R22 AUTO CODE CONVERSION
        END   ;*R22 AUTO CODE CONVERSION.END
    END ELSE
        VAR.INS.POL.TYPE =R.Condition.charge<AA.CHG.LOCAL.REF><1,INS.POLICY.TYPE.POS>
        VAR.CLASS.POLICY =R.Condition.charge<AA.CHG.LOCAL.REF><1,CLASS.POLICY.POS>
    END

    Y.CNT = 1
    Y.CNT1 = DCOUNT(VAR.INS.POL.TYPE,@SM)
    LOOP
    WHILE Y.CNT LE Y.CNT1
        INS.POL.TYPE = VAR.INS.POL.TYPE<1,1,Y.CNT>
        CLASS.POLICY = VAR.CLASS.POLICY<1,1,Y.CNT>
        GOSUB PROCESS1
        Y.CNT += 1
    REPEAT

RETURN

*    VAR.INS.POL.TYPE = R.Condition.charge<AA.CHG.LOCAL.REF><1,INS.POLICY.TYPE.POS>
*    VAR.CLASS.POLICY = R.Condition.charge<AA.CHG.LOCAL.REF><1,CLASS.POLICY.POS>

*******************
PROCESS1:
*******************


    IF CLASS.POLICY EQ "GROUP" AND INS.POL.TYPE NE "VU" THEN
        IF START.DATE EQ "" THEN
            AF = AA.PS.START.DATE
            ETEXT = "EB-REDO.CHARGE.START.DATE"
            CALL STORE.END.ERROR
        END
    END
    IF CLASS.POLICY EQ "FHA" AND END.DATE EQ '' THEN
        AF = AA.PS.END.DATE
        ETEXT = "EB-ISSUE.DATE.MISSING"
        CALL STORE.END.ERROR
    END
RETURN
END
