*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.GET.LOAN.NUMBER
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_System
    $INSERT T24.BP I_F.ALTERNATE.ACCOUNT

    Y.TC.NUMBER = COMI

	*PARA ABRIR EL ACHIVO DE ALTERNATE.ACCOUNT
    FN.AAC = "F.ALTERNATE.ACCOUNT"
    FV.AAC = ""
    RS.AAC = ""
    AAC.ERR = ""

    IF Y.TC.NUMBER = '' THEN
    	RETURN
    END

    FINDSTR "*" IN Y.TC.NUMBER SETTING Ap, Vp THEN
        RETURN
    END ELSE

		*PARA ABRIR EL ACHIVO DE ALTERNATE.ACCOUNT
        FN.AAC = "F.ALTERNATE.ACCOUNT"
        FV.AAC = ""
        RS.AAC = ""
        AAC.ERR = ""

        CALL OPF(FN.AAC, FV.AAC)
        CALL F.READ(FN.AAC, Y.TC.NUMBER, RS.AAC, FV.AAC, AAC.ERR)

        R.NEW(1) = RS.AAC<AAC.GLOBUS.ACCT.NUMBER>

        COMI = RS.AAC<AAC.GLOBUS.ACCT.NUMBER>
        CALL REDO.V.VAL.COLLECT.AA

        COMI = SUBSTRINGS(Y.TC.NUMBER,0,6) : "******" : SUBSTRINGS(Y.TC.NUMBER,13,4)
        R.NEW(1) = RS.AAC<AAC.GLOBUS.ACCT.NUMBER>

        RETURN
    END
END
