* @ValidationCode : MjotMTkxNDA5MTAyMjpDcDEyNTI6MTY4MjA3Mjg4Nzg0MDpzYW1hcjotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 21 Apr 2023 15:58:07
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
$PACKAGE APAP.LAPAP
SUBROUTINE LAPAP.V.VAL.NO.UNICO
******************************************************************************************************************
*Company Name : Asociaciopular de Ahorros y Pramos Bank
*Developed By :APAP
*Date : 14.02.2022
*Program Name : LAPAP.V.VAL.NO.UNICO
*------------------------------------------------------------------------------------------------------------------

*Description : Basada en la logica de la rutina de TEMENOS "REDO.V.VAL.NO.UNICO" This subroutine validates the customer's age and check whether the customer is major or a minor
*Linked With : This routine has to be attached as an input routine to the versions of CUSTOMER,REDO.OPEN.CL.MINOR.TEST
* CUSTOMER,REDO.MOD.CL.MINOR.TEST

*In Parameter : -NA-
*Out Parameter : -NA-
*------------------------------------------------------------------------------------------------------------------
*Modification History
*DATE                WHO                         REFERENCE                DESCRIPTION
*21-04-2023       Conversion Tool        R22 Auto Code conversion          INSERT FILE MODIFIED
*21-04-2023       Samaran T               R22 Manual Code Conversion       CALL ROUTINE FORMAT MODIFIED
*------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON   ;*R22 AUTO CODE CONVERSION.START
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER   ;*R22 AUTO CODE CONVERSION.END
*------------------------------------------------------------------------------------------------------------------

    GOSUB INIT
    GOSUB OPEN.FILES
    GOSUB PROCESS
RETURN
*-------------------------------------------------------------------------------------------------------------------
INIT:
*****
    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
RETURN
*-------------------------------------------------------------------------------------------------------------------
OPEN.FILES:
***********
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)
RETURN
*-------------------------------------------------------------------------------------------------------------------
PROCESS:
********
* L.CU.NOUNICO should accept only 11 characters
* If the length of the variable is not equal to 11 characters,
* then set the ETEXT common variable to the error code, EB-REDO.INVALID.DOC.FORMAT
* Then call LAPAP.S.CALC.DIGIT.PADRON subroutine.If the returned value is FAIL, then set ETEXT as EB-REDO.INCORRECT.CHECKDIGIT

    CALL GET.LOC.REF('CUSTOMER','L.CU.NOUNICO',REF.POS)
    Y.L.CU.NOUNICO = COMI
    Y.LEN.L.CU.NOUNICO = LEN(Y.L.CU.NOUNICO)
    IF Y.L.CU.NOUNICO NE '' THEN
        IF Y.LEN.L.CU.NOUNICO NE 11 THEN
            GOSUB CHECK.COUNT
        END ELSE
            CHECK.NUMERIC = NUM(Y.L.CU.NOUNICO)
            IF CHECK.NUMERIC EQ 1 THEN
                CALL APAP.LAPAP.LAPAP.S.CALC.DIGIT.PADRON(Y.L.CU.NOUNICO)    ;*R22 MANUAL CODE CONVERSION
            END ELSE
                GOSUB CHECK.COUNT
            END
        END
    END
    IF Y.L.CU.NOUNICO EQ 'FAIL' THEN
        AF = EB.CUS.LOCAL.REF
        AV = REF.POS
        ETEXT = 'EB-REDO.INCORRECT.CHECK.DIGIT'
        CALL STORE.END.ERROR
    END
RETURN
*-------------------------------------------------------------------------------------------------------------------
CHECK.COUNT:
*-------------------------------------------------------------------------------------------------------------------
    AF = EB.CUS.LOCAL.REF
    AV = REF.POS
    ETEXT = 'EB-REDO.INVALID.DOC.FORMAT'
    CALL STORE.END.ERROR
RETURN
*---------------------------------------------------------------------------------------------------------------------
END
