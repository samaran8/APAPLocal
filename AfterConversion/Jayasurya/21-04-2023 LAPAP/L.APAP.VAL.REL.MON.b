* @ValidationCode : MjoxNzQ1ODc3MjkwOkNwMTI1MjoxNjgyMDcxNDg4MjkzOklUU1NCTkc6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 21 Apr 2023 15:34:48
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.LAPAP
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*21-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                BP REMOVED , I TO I.VAR
*21-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE L.APAP.VAL.REL.MON
    $INSERT I_COMMON ;* AUTO R22 CODE CONVERSION START
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER
    $INSERT I_F.ACCOUNT
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.RELATION ;* AUTO R22 CODE CONVERSION END


    Y.ACC.ID = COMI
    Y.CUS.ID = ""
    Y.RELACION.CODE = ""
    Y.JOINT.HOLDER = ""
    Y.CADENA = ""

    R.PASAPORTE = ""
    R.CEDULA = ""
    R.RNC = ""
    R.ACTA = ""
    R.NUMERO.UNICO = ""
    R.IDENTIFICACION = ""

    FN.ACC = "F.ACCOUNT"
    FV.ACC = ""

    FN.CUS = "F.CUSTOMER"
    FV.CUS = ""

    FN.REL = "F.RELATION"
    FV.REL = ""

    CALL OPF(FN.ACC,FV.ACC)
    CALL F.READ(FN.ACC,Y.ACC.ID,R.ACC,FV.ACC,ACC.ERROR)



    Y.CANT.RELACIONES = R.ACC<AC.JOINT.HOLDER>
    Y.CNT = DCOUNT(Y.CANT.RELACIONES,@VM)



    FOR I.VAR = 1 TO Y.CNT

        Y.RELACION.CODE = R.ACC<AC.RELATION.CODE , I.VAR >

        Y.CUS.ID = R.ACC<AC.JOINT.HOLDER, I.VAR >

        CALL OPF(FN.CUS,FV.CUS)

        CALL F.READ(FN.CUS,Y.CUS.ID,R.CUS,FV.CUS,CUS.ERROR)

        R.PASAPORTE = R.CUS<EB.CUS.LEGAL.ID>

        CALL GET.LOC.REF("CUSTOMER","L.CU.RNC",Y.POS)
        R.RNC = R.CUS<EB.CUS.LOCAL.REF,Y.POS>

        CALL GET.LOC.REF("CUSTOMER","L.CU.CIDENT",Y.POS)
        R.CEDULA = R.CUS<EB.CUS.LOCAL.REF,Y.POS>

        CALL GET.LOC.REF("CUSTOMER","L.CU.ACTANAC",Y.POS)
        R.ACTA = R.CUS<EB.CUS.LOCAL.REF,Y.POS>

        CALL GET.LOC.REF("CUSTOMER","L.CU.NOUNICO",Y.POS)
        R.NUMERO.UNICO = R.CUS<EB.CUS.LOCAL.REF,Y.POS>

        IF R.CEDULA NE "" THEN

            R.IDENTIFICACION = R.CEDULA

        END

        IF R.RNC NE "" THEN

            R.IDENTIFICACION = R.RNC

        END

        IF R.PASAPORTE NE "" THEN

            R.IDENTIFICACION = R.PASAPORTE

        END

        IF R.ACTA NE ""  THEN

            R.IDENTIFICACION = R.ACTA

        END

        IF R.NUMERO.UNICO NE "" THEN

            R.IDENTIFICACION = R.NUMERO.UNICO

        END

        CALL OPF(FN.REL,FV.REL)
        CALL F.READ(FN.REL,Y.RELACION.CODE,R.REL,FV.REL,REL.ERROR)

        R.DES.REL = R.REL<EB.REL.DESCRIPTION>

        IF  R.CUS<EB.CUS.NAME.1> NE "" THEN

            COMI = CADENA :  "*" : R.IDENTIFICACION : ";" : R.CUS<EB.CUS.NAME.1> : ";" : R.DES.REL
            CADENA = CADENA :  "*" : R.IDENTIFICACION : ";" : R.CUS<EB.CUS.NAME.1> : ";" : R.DES.REL

        END

    NEXT I.VAR

END
