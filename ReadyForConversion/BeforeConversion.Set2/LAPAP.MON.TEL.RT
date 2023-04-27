*-----------------------------------------------------------------------------
* <Rating>-40</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.MON.TEL.RT

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.CUSTOMER
    $INSERT T24.BP I_F.ACCOUNT


    GOSUB INIT
    GOSUB INITb
    GOSUB PROCESS
    GOSUB END_PROCESS


INIT:
*----

    FN.CUS = "F.CUSTOMER"
    F.CUS = ""

    customer = COMI
    CALL OPF(FN.CUS,F.CUS)

    Y.TEL.CASA = ''
    Y.TEL.OFI = ''
    Y.TEL.CEL = ''

    RETURN

INITb:
*----

    CALL F.READ(FN.CUS,customer,R.CUS,F.CUS,CUS.ERR)

    RETURN


PROCESS:
*-------


    CALL GET.LOC.REF("CUSTOMER","L.CU.TEL.TYPE",POS)
    TEL.TYPE = R.CUS<EB.CUS.LOCAL.REF,POS>

    CALL GET.LOC.REF("CUSTOMER","L.CU.TEL.AREA",POS)
    AREA = R.CUS<EB.CUS.LOCAL.REF,POS>

    CALL GET.LOC.REF("CUSTOMER","L.CU.TEL.NO",POS)
    TEL.NO = R.CUS<EB.CUS.LOCAL.REF,POS>

    Y.QNT.TELS = DCOUNT(TEL.TYPE, @SM)

    RETURN


    END_PROCESS:
*---------------

    FOR A = 1 TO Y.QNT.TELS STEP 1
        IF TEL.TYPE<1,1,A> EQ "1" THEN
            Y.TEL.CASA = AREA<1,1,A> : TEL.NO<1,1,A>
        END
        IF TEL.TYPE<1,1,A> EQ "5" THEN
            Y.TEL.OFI = AREA<1,1,A> : TEL.NO<1,1,A>
        END
        IF TEL.TYPE<1,1,A> EQ "6" THEN
            Y.TEL.CEL = AREA<1,1,A> : TEL.NO<1,1,A>
        END
    NEXT A

    COMI = Y.TEL.CASA : "|" : Y.TEL.OFI : "|" : Y.TEL.CEL

    RETURN


END
