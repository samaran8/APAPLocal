* @ValidationCode : MjoyMDAwODcxODIwOkNwMTI1MjoxNjgyMDc1OTMzODcyOkhhcmlzaHZpa3JhbUM6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 21 Apr 2023 16:48:53
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
SUBROUTINE L.APAP.ENQ.APAP.REL.CTA
*-----------------------------------------------------------------------------
*MODIFICATION HISTORY:
*
* DATE              WHO                REFERENCE                 DESCRIPTION
* 21-APR-2023     Conversion tool    R22 Auto conversion       I to I.VAR, = to EQ, BP Removed in insert file
* 21-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*-----------------------------------------------------------------------------
    $INSERT I_COMMON ;*R22 Auto conversion - START
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER
    $INSERT I_F.ACCOUNT
    $INSERT I_ENQUIRY.COMMON ;*R22 Auto conversion - END


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

    FOR I.VAR = 1 TO Y.CNT

        Y.RELACION.CODE = R.ACC<AC.RELATION.CODE , I.VAR >

        IF Y.RELACION.CODE EQ 500 OR Y.RELACION.CODE EQ 501 OR Y.RELACION.CODE EQ 510 THEN

            Y.CUS.ID = R.ACC<AC.JOINT.HOLDER, I.VAR >
            CALL OPF(FN.CUS,FV.CUS)
            CALL F.READ(FN.CUS,Y.CUS.ID,R.CUS,FV.CUS,CUS.ERROR)

            IF  R.CUS<EB.CUS.NAME.1> NE "" THEN

                CADENA = CADENA :  "*" : Y.CUS.ID : ";" : R.CUS<EB.CUS.NAME.1> : " " : R.CUS<EB.CUS.NAME.2> : ";" : Y.RELACION.CODE

            END
        END

    NEXT I.VAR

    O.DATA = CADENA

RETURN
