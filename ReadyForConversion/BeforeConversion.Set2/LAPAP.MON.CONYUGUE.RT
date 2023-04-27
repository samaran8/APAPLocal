*-----------------------------------------------------------------------------
* <Rating>10</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.MON.CONYUGUE.RT

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

    Y.NOMBRE.CONYUGUE = ''
    Y.FECHA.NAC.CONYUGUE = ''
    Y.TEL.CONYUGUE = ''

    RETURN

INITb:
*----

    CALL F.READ(FN.CUS,customer,R.CUS,F.CUS,CUS.ERR)

    RETURN


PROCESS:
*-------


    Y.RELATION.CODE = R.CUS<EB.CUS.RELATION.CODE>
    Y.REL.CUSTOMER = R.CUS<EB.CUS.REL.CUSTOMER>
    Y.QNT.REL  = DCOUNT(Y.RELATION.CODE,@VM)

    RETURN


    END_PROCESS:
*---------------
    Y.TEL.CASA = ''
    FOR I = 1 TO Y.QNT.REL STEP 1
        IF Y.RELATION.CODE<I> EQ 7 OR Y.RELATION.CODE<I> EQ 8 THEN
            Y.CONYUGUE = Y.REL.CUSTOMER<I>
            CALL F.READ(FN.CUS,Y.CONYUGUE,R.CUS2,F.CUS,CUS2.ERR)
            Y.NOMBRE.CONYUGUE = R.CUS2<EB.CUS.NAME.1> : " " : R.CUS2<EB.CUS.NAME.2>
            Y.FECHA.NAC.CONYUGUE = R.CUS2<EB.CUS.DATE.OF.BIRTH>

            CALL GET.LOC.REF("CUSTOMER","L.CU.TEL.TYPE",POS)
            TEL.TYPE = R.CUS2<EB.CUS.LOCAL.REF,POS>
            CALL GET.LOC.REF("CUSTOMER","L.CU.TEL.AREA",POS)
            AREA = R.CUS2<EB.CUS.LOCAL.REF,POS>
            CALL GET.LOC.REF("CUSTOMER","L.CU.TEL.NO",POS)
            TEL.NO = R.CUS2<EB.CUS.LOCAL.REF,POS>
            Y.QNT.TELS = DCOUNT(TEL.TYPE, @SM)
            FOR A = 1 TO Y.QNT.TELS STEP 1
                IF TEL.TYPE<1,1,A> EQ "1" THEN
                    Y.TEL.CASA = AREA<1,1,A> : TEL.NO<1,1,A>
                END
            NEXT A
            Y.TEL.CONYUGUE = Y.TEL.CASA
            BREAK
        END
    NEXT I

    COMI = Y.NOMBRE.CONYUGUE : ">" : Y.FECHA.NAC.CONYUGUE : ">" : Y.TEL.CONYUGUE

    RETURN


END
