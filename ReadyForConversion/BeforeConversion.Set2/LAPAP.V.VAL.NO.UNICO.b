*-----------------------------------------------------------------------------
* <Rating>-44</Rating>
*-----------------------------------------------------------------------------
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
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.CUSTOMER
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
                CALL LAPAP.S.CALC.DIGIT.PADRON(Y.L.CU.NOUNICO)
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
