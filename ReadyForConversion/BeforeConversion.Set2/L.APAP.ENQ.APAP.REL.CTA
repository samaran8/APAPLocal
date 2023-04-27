*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.ENQ.APAP.REL.CTA
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.CUSTOMER
    $INSERT T24.BP I_F.ACCOUNT
    $INSERT T24.BP I_ENQUIRY.COMMON


    Y.ACC.ID = O.DATA
    Y.CUS.ID = ""
    Y.RELACION.CODE = ""
    Y.JOINT.HOLDER = ""
    Y.CADENA = ""

    FN.ACC = "F.ACCOUNT"
    FV.ACC = ""

    FN.CUS = "F.CUSTOMER"
    FV.CUS = ""

    CALL OPF(FN.ACC,FV.ACC)

    CALL F.READ(FN.ACC,Y.ACC.ID,R.ACC,FV.ACC,ACC.ERROR)
    Y.CUS.ID = R.ACC<AC.CUSTOMER>

    Y.CANT.RELACIONES = R.ACC<AC.JOINT.HOLDER>
    Y.CNT = DCOUNT(Y.CANT.RELACIONES,@VM)

    CALL OPF(FN.CUS,FV.CUS)
    CALL F.READ(FN.CUS,Y.CUS.ID,R.CUS,FV.CUS,CUS.ERROR)

    R.NOMBRE.CLIENTE = R.CUS<EB.CUS.NAME.1> : " " : R.CUS<EB.CUS.NAME.2>

    CADENA = Y.CUS.ID : ";" : R.NOMBRE.CLIENTE

    FOR I = 1 TO Y.CNT

        Y.RELACION.CODE = R.ACC<AC.RELATION.CODE , I >

        IF Y.RELACION.CODE = 500 OR Y.RELACION.CODE = 501 OR Y.RELACION.CODE = 510 THEN

            Y.CUS.ID = R.ACC<AC.JOINT.HOLDER, I >
            CALL OPF(FN.CUS,FV.CUS)
            CALL F.READ(FN.CUS,Y.CUS.ID,R.CUS,FV.CUS,CUS.ERROR)

            IF  R.CUS<EB.CUS.NAME.1> NE "" THEN

                CADENA = CADENA :  "*" : Y.CUS.ID : ";" : R.CUS<EB.CUS.NAME.1> : " " : R.CUS<EB.CUS.NAME.2> : ";" : Y.RELACION.CODE

            END
        END

    NEXT I

    O.DATA = CADENA

    RETURN
