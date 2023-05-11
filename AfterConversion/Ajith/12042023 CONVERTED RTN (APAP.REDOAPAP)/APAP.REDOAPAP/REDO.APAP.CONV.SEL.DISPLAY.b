* @ValidationCode : MjotMTUwNjQzNTY5NTpDcDEyNTI6MTY4MTI4MzkwOTEyODphaml0aDotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 12 Apr 2023 12:48:29
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ajith
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOAPAP
SUBROUTINE REDO.APAP.CONV.SEL.DISPLAY
*************************************************************
* COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* DEVELOPED BY: PRADEEP P
* PROGRAM NAME: REDO.ENQ.BUILD.RESTRICT.LIST
* ODR NO      : ODR-2010-08-0470
*----------------------------------------------------------------------
* DESCRIPTION:   This is a conversion routine attached to the Enquiry
*                REDO.ENQ.CUS.BAD.REFERENCE which display the selection fields
*                based on Values inputted by the USER
* IN PARAMETER : O.DATA
* OUT PARAMETER: 0.DATA
* LINKED WITH  :
*----------------------------------------------------------------------
* Modification History :
*-----------------------
* DATE        WHO           REFERENCE         DESCRIPTION
* 31.Aug.2010  PRADEEP P    ODR-2010-08-0470  INITIAL CREATION
*---------------------------------------------------------------------------------------
*DATE               WHO                       REFERENCE                 DESCRIPTION
*12-04-2023       CONVERSION TOOLS            AUTO R22 CODE CONVERSION   F.READ to CACHE.READ
*12-04-2023       AJITHKUMAR                  MANUAL R22 CODE CONVERSION NO CHANGE
*----------------------------------------------------------------------------------------
*----------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.USER

    GOSUB OPENFILES
    GOSUB PROCESS
RETURN
*

OPENFILES:
*----------

    FN.USER = 'F.USER'
    F.USER = ''
    CALL OPF(FN.USER,F.USER)
RETURN

PROCESS:
*-------
    Y.FINAL = ''
    Y.USER = OPERATOR

    Y.DATE = FIELD(O.DATA,'*',1)
    Y.NUMERO.DOCUMENTO = FIELD(O.DATA,'*',2)
    Y.TIPO.DE.DOCUMENTO = FIELD(O.DATA,'*',3)
    Y.LISTA.RESTRICTIVA = FIELD(O.DATA,'*',4)
    Y.TIPO.DE.PERSONA = FIELD(O.DATA,'*',5)

    CALL CACHE.READ(FN.USER, Y.USER, R.USER, F.ERR) ;*R22 AUTO CODE CONVERSION


    IF Y.DATE THEN
        Y.FINAL = "Fecha In Desde-Hasta - ":Y.DATE
    END

    IF Y.NUMERO.DOCUMENTO THEN
        Y.FINAL := " ":"Numero documento - ":Y.NUMERO.DOCUMENTO
    END

    IF Y.TIPO.DE.DOCUMENTO THEN
        Y.FINAL := " ":"Tipo de documento - ":Y.TIPO.DE.DOCUMENTO
    END

    IF Y.LISTA.RESTRICTIVA THEN
        Y.FINAL := " ":"Lista restrictiva - ":Y.LISTA.RESTRICTIVA
    END

    IF Y.TIPO.DE.PERSONA THEN
        Y.FINAL := " ":"Tipo de cliente - ":Y.TIPO.DE.PERSONA
    END

    O.DATA = Y.FINAL

RETURN
END
