* @ValidationCode : MjoyNjkyMDI0NDY6Q3AxMjUyOjE2ODIwNzM2NTI5MTM6SGFyaXNodmlrcmFtQzotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 21 Apr 2023 16:10:52
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.LAPAP
SUBROUTINE L.APAP.CRT.CTA.AHORRO.PJ.RT(Y.FINAL)

*-----------------------------------------------------------------------------
*MODIFICATION HISTORY:
*
* DATE              WHO                REFERENCE                 DESCRIPTION
* 21-APR-2023     Conversion tool    R22 Auto conversion       ++ to +=, BP Removed in insert file
* 21-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*-----------------------------------------------------------------------------
    $INSERT I_COMMON ;*R22 Auto conversion - START
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.CUSTOMER
    $INSERT I_F.ACCOUNT
    $INSERT I_F.CATEGORY ;*R22 Auto conversion- END

**---------------------------------------------------------------------------------------------
**SENTENCIA LOCATE
**---------------------------------------------------------------------------------------------
    LOCATE "ACCOUNT.NUMBER" IN D.FIELDS<1> SETTING LOC.POS.1 THEN
        P.ACC.ID = D.RANGE.AND.VALUE<LOC.POS.1>
    END
    P.COND.FIRMA.1 = ""
    LOCATE "CONDITION.1" IN D.FIELDS<1> SETTING LOC.POS.2 THEN
        P.COND.FIRMA.1 = D.RANGE.AND.VALUE<LOC.POS.2>
    END
    P.FEC.FIRMA.1 = ""
    LOCATE "CONDITION.DATE.1" IN D.FIELDS<1> SETTING LOC.POS.3 THEN
        P.FEC.FIRMA.1 = D.RANGE.AND.VALUE<LOC.POS.3>
    END
    P.COND.FIRMA.2 = ""
    LOCATE "CONDITION.2" IN D.FIELDS<1> SETTING LOC.POS.4 THEN
        P.COND.FIRMA.2 = D.RANGE.AND.VALUE<LOC.POS.4>
    END
    P.FEC.FIRMA.2 = ""
    LOCATE "CONDITION.DATE.2" IN D.FIELDS<1> SETTING LOC.POS.5 THEN
        P.FEC.FIRMA.2 = D.RANGE.AND.VALUE<LOC.POS.5>
    END

    GOSUB INI
    GOSUB GET_ACCOUNT_INF
    GOSUB GET_FIRST_TIT
    GOSUB GET_FIRST_REP
    GOSUB GET_SECOND_REP
    GOSUB FORMAR_Y_FINAL

INI:
**---------------------------------------
**ABRIR LA TABLA CUSTOMER
**---------------------------------------
    FN.CUS = "F.CUSTOMER"
    FV.CUS = ""
    R.CUS = ""
    CUS.ERR = ""
    CALL OPF(FN.CUS,FV.CUS)

**---------------------------------------
**ABRIR LA TABLA FBNK.ACCOUNT
**---------------------------------------
    FN.ACC = "F.ACCOUNT"
    FV.ACC = ""
    R.ACC = ""
    ACC.ERR = ""
    CALL OPF(FN.ACC,FV.ACC)
**---------------------------------------
**ABRIR LA TABLA FBNK.CATEGORY
**---------------------------------------
    FN.CAT = "F.CATEGORY"
    FV.CAT = ""
    R.CAT = ""
    CAT.ERR = ""
    CALL OPF(FN.CAT,FV.CAT)

RETURN


GET_ACCOUNT_INF:
    CALL F.READ(FN.ACC,P.ACC.ID,R.ACC, FV.ACC, ACC.ERR)
    T.AC.CUSTOMER   = R.ACC<AC.CUSTOMER>
    T.AC.CUSTOMER.2  = ""
    T.AC.OPENING.DATE  = R.ACC<AC.OPENING.DATE>
    T.AC.CO.CODE = R.ACC<AC.CO.CODE>
    T.AC.CO.CODE.LAST = T.AC.CO.CODE[7,3]
    T.TIPO.CTA   = "EMPRESARIAL"

    T.CANT.TIT   = 1



RETURN


GET_FIRST_TIT:
    CALL F.READ(FN.CUS,T.AC.CUSTOMER,R.CUS, FV.CUS, CUS.ERR)
    T.TIT.NOMBRE.1 = R.CUS<EB.CUS.NAME.1>
    T.TIT.APELLI.1 = R.CUS<EB.CUS.NAME.2>
    T.TIT.IDENTI.1 = ""
    CALL GET.LOC.REF("CUSTOMER", "L.CU.CIDENT",CUS.POS)
    T.CUS.CIDENT = R.CUS<EB.CUS.LOCAL.REF,CUS.POS>
    CALL GET.LOC.REF("CUSTOMER", "L.CU.RNC",CUS.POS.1)
    T.CUS.RNC = R.CUS<EB.CUS.LOCAL.REF,CUS.POS.1>
    CALL GET.LOC.REF("CUSTOMER", "L.CU.PASS.NAT",CUS.POS.2)
    T.CUS.PASS.NAT = R.CUS<EB.CUS.LOCAL.REF,CUS.POS.2>
    CALL GET.LOC.REF("CUSTOMER", "L.CU.ACTANAC",CUS.POS.3)
    T.CUS.ACT.NAC = R.CUS<EB.CUS.LOCAL.REF,CUS.POS.3>
    CALL GET.LOC.REF("CUSTOMER", "L.CU.NOUNICO",CUS.POS.4)
    T.CUS.NOUNICO = R.CUS<EB.CUS.LOCAL.REF,CUS.POS.4>
    CALL GET.LOC.REF("CUSTOMER", "L.CU.TIPO.CL",CUS.POS.5)
    T.TIPO.CLIENTE = R.CUS<EB.CUS.LOCAL.REF,CUS.POS.5>
    IF T.TIPO.CLIENTE EQ "PERSONA FISICA" THEN
        ENQ.ERROR = "EL TITULAR DE LA CUENTA ESPECIFICADA NO ES PERSONA JURIDICA, FAVOR REVISAR."
        ENQ.ERROR<1,2> = 2
    END
    IF T.CUS.RNC NE "" THEN
        T.TIT.IDENTI.1 = T.CUS.RNC : "-RNC"
    END
    IF T.CUS.PASS.NAT NE "" THEN
        T.TIT.IDENTI.1 = T.CUS.PASS.NAT : "-Pas. No."
    END
    IF T.CUS.ACT.NAC NE "" THEN
        T.TIT.IDENTI.1 = T.CUS.ACT.NAC : "-Act. Nac."
    END
    IF T.CUS.NOUNICO NE "" THEN
        T.TIT.IDENTI.1 = T.CUS.NOUNICO : "-No. Uni."
    END
    IF T.CUS.CIDENT NE "" THEN
        T.TIT.IDENTI.1 = T.CUS.CIDENT : "-Ced."
    END

    T.TIT.DOB.1 = R.CUS<EB.CUS.DATE.OF.BIRTH>
    T.TIT.DOB.FMT.1 = T.TIT.DOB.1[7,2] : "/" : T.TIT.DOB.1[5,2] : "/" : T.TIT.DOB.1[1,4]

*DEBUG
    T.RELATION.CODE  = R.CUS<EB.CUS.RELATION.CODE>
    T.CANT.VM.REL.CO = DCOUNT(T.RELATION.CODE,@VM)
    FOR A = 1 TO T.CANT.VM.REL.CO STEP 1
        RC = R.CUS<EB.CUS.RELATION.CODE, A>
        IF (RC EQ 104) THEN
            IF T.CANT.TIT EQ 1 THEN
                T.AC.CUSTOMER.2 = R.CUS<EB.CUS.REL.CUSTOMER, A>
            END
            IF T.CANT.TIT EQ 2 THEN
                T.AC.CUSTOMER.3 = R.CUS<EB.CUS.REL.CUSTOMER, A>
            END
            T.CANT.TIT += 1 ;*R22 Auto conversion
        END
    NEXT A
*DEBUG
RETURN


GET_FIRST_REP:
    CALL F.READ(FN.CUS,T.AC.CUSTOMER.2,R.CUS, FV.CUS, CUS.ERR)
    T.TIT.NOMBRE.2 = R.CUS<EB.CUS.GIVEN.NAMES>
    T.TIT.APELLI.2 = R.CUS<EB.CUS.FAMILY.NAME>
    T.TIT.IDENTI.2 = ""
    CALL GET.LOC.REF("CUSTOMER", "L.CU.CIDENT",CUS.POS)
    T.CUS.CIDENT = R.CUS<EB.CUS.LOCAL.REF,CUS.POS>
    CALL GET.LOC.REF("CUSTOMER", "L.CU.RNC",CUS.POS.1)
    T.CUS.RNC = R.CUS<EB.CUS.LOCAL.REF,CUS.POS.1>
    CALL GET.LOC.REF("CUSTOMER", "L.CU.PASS.NAT",CUS.POS.2)
    T.CUS.PASS.NAT = R.CUS<EB.CUS.LOCAL.REF,CUS.POS.2>
    IF T.CUS.RNC NE "" THEN
        T.TIT.IDENTI.2 = T.CUS.RNC : "-RNC"
    END
    IF T.CUS.PASS.NAT NE "" THEN
        T.TIT.IDENTI.2 = T.CUS.PASS.NAT : "-Pas. No."
    END
    IF T.CUS.CIDENT NE "" THEN
        T.TIT.IDENTI.2 = T.CUS.CIDENT : "-Ced."
    END
    T.TIT.DOB.2 = R.CUS<EB.CUS.DATE.OF.BIRTH>
    T.TIT.DOB.FMT.2 = T.TIT.DOB.2[7,2] : "/" : T.TIT.DOB.2[5,2] : "/" : T.TIT.DOB.2[1,4]

RETURN

GET_SECOND_REP:
    CALL F.READ(FN.CUS,T.AC.CUSTOMER.3,R.CUS, FV.CUS, CUS.ERR)
    T.TIT.NOMBRE.3 = R.CUS<EB.CUS.GIVEN.NAMES>
    T.TIT.APELLI.3 = R.CUS<EB.CUS.FAMILY.NAME>
    T.TIT.IDENTI.3 = ""
    CALL GET.LOC.REF("CUSTOMER", "L.CU.CIDENT",CUS.POS)
    T.CUS.CIDENT = R.CUS<EB.CUS.LOCAL.REF,CUS.POS>
    CALL GET.LOC.REF("CUSTOMER", "L.CU.RNC",CUS.POS.1)
    T.CUS.RNC = R.CUS<EB.CUS.LOCAL.REF,CUS.POS.1>
    CALL GET.LOC.REF("CUSTOMER", "L.CU.PASS.NAT",CUS.POS.2)
    T.CUS.PASS.NAT = R.CUS<EB.CUS.LOCAL.REF,CUS.POS.2>
    IF T.CUS.RNC NE "" THEN
        T.TIT.IDENTI.3 = T.CUS.RNC : "-RNC"
    END
    IF T.CUS.PASS.NAT NE "" THEN
        T.TIT.IDENTI.3 = T.CUS.PASS.NAT : "-Pas. No."
    END
    IF T.CUS.CIDENT NE "" THEN
        T.TIT.IDENTI.3 = T.CUS.CIDENT : "-Ced."
    END
    T.TIT.DOB.3 = R.CUS<EB.CUS.DATE.OF.BIRTH>
    T.TIT.DOB.FMT.3 = T.TIT.DOB.3[7,2] : "/" : T.TIT.DOB.3[5,2] : "/" : T.TIT.DOB.3[1,4]

RETURN


FORMAR_Y_FINAL:
    IF T.AC.CUSTOMER NE "" THEN
        Y.FIN.1 = P.ACC.ID : "*" : T.AC.OPENING.DATE : "*" : T.TIPO.CTA : "*" : T.CANT.TIT : "*" : T.TIT.NOMBRE.1 : "*"
        Y.FIN.2 = Y.FIN.1 : T.TIT.APELLI.1 : "*" : T.TIT.IDENTI.1 : "*" : T.TIT.DOB.FMT.1 : "*" : T.TIT.NOMBRE.2 : "*" : T.TIT.APELLI.2 : "*"
        Y.FIN.3 = Y.FIN.2 : T.TIT.IDENTI.2 : "*" : T.TIT.DOB.FMT.2 : "*" : T.TIT.NOMBRE.3 : "*" : T.TIT.APELLI.3 : "*" : T.TIT.IDENTI.3 : "*"
        Y.FIN.4 = Y.FIN.3 : T.TIT.DOB.FMT.3 : "*" : T.AC.CO.CODE.LAST : "*" : P.COND.FIRMA.1 : "*" : P.FEC.FIRMA.1 : "*" : P.COND.FIRMA.2 : "*" : P.FEC.FIRMA.2
        Y.FINAL<-1> =  Y.FIN.4
    END ELSE
        ENQ.ERROR = "NO HAY INFORMACION A MOSTRAR."
        ENQ.ERROR<1,2> = 1
        RETURN
    END
RETURN


END
