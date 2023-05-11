* @ValidationCode : MjoxODY0NzExODIwOlVURi04OjE2ODI1MTQ3ODY2NTk6QWRtaW46LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 26 Apr 2023 18:43:06
* @ValidationInfo : Encoding          : UTF-8
* @ValidationInfo : User Name         : Admin
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOFCFI
SUBROUTINE REDO.FI.FT.PROCESS(Y.REC.ID)


*------------------------------------------------------------------------
* Modification History :
*------------------------------------------------------------------------
*  DATE             WHO                   REFERENCE
* 05-APRIL-2023      Harsha                R22 Auto Conversion  - VM to @VM and FM to @FM
* 05-APRIL-2023      Harsha                R22 Manual Conversion - call routine format modified
*------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_REDO.FI.FT.PROCESS.COMMON
    $INSERT I_F.REDO.TEMP.FI.CONTROL
    $USING APAP.TAM


    CALL F.READ(FN.REDO.TEMP.FI.CONTROL,Y.REC.ID,R.REDO.TEMP.FI.CONTROL,F.REDO.TEMP.FI.CONTROL,ERR)
    OFS.SOURCE.ID ="TAM.OFS.SRC"
    R.PARAM       = R.REDO.TEMP.FI.CONTROL<FI.TEMP.PARAM.VAL>
    CHANGE @VM TO @FM IN R.PARAM

    BEGIN CASE
        CASE R.REDO.TEMP.FI.CONTROL<FI.TEMP.INTER.TYPE> EQ 'ORANGE'
*CALL APAP.TAM.REDO.FI.DEBIT.PROCES.ORANGE(R.PARAM,OUT.REF, OUT.RESP)
            CALL APAP.TAM.redoFiDebitProcesOrange(R.PARAM,OUT.REF, OUT.RESP);*R22 Manual Conversion

        CASE R.REDO.TEMP.FI.CONTROL<FI.TEMP.INTER.TYPE> EQ 'BACEN'
* CALL APAP.TAM.REDO.FI.DEBIT.PROCES.BACEN(R.PARAM, OUT.REF, OUT.RESP)
            CALL APAP.TAM.redoFiDebitProcesBacen(rParams, outResp, outErr) ;*R22 Manual Conversion

        CASE R.REDO.TEMP.FI.CONTROL<FI.TEMP.INTER.TYPE> EQ 'INTNOMINA'
          
            CALL APAP.TAM.redoFiDebitProces(R.PARAM, OUT.REF, OUT.RESP) ;*R22 Manual Conversion
       
        CASE R.REDO.TEMP.FI.CONTROL<FI.TEMP.INTER.TYPE> EQ 'EXTNOMINA'
*CALL APAP.TAM.REDO.FI.EXT.DEBIT.PROCES(R.PARAM,OUT.REF,OUT.RESP)
            CALL APAP.TAM.redoFiExtDebitProces(R.PARAM,OUT.REF,OUT.RESP);*R22 Manual Conversion
    END CASE


    GOSUB UPD.TEMP.FI

RETURN

*=============
UPD.TEMP.FI:
*=============

    Y.ERR.MSG=''
    IF FIELD(OUT.RESP,'/',1)[1,2] NE 'FT' THEN
        Y.ERR.MSG=OUT.RESP
    END

    IF Y.ERR.MSG NE "" THEN
        R.REDO.TEMP.FI.CONTROL<FI.TEMP.STATUS>   ='04'
        R.REDO.TEMP.FI.CONTROL<FI.TEMP.MAIL.MSG> =R.REDO.TEMP.FI.CONTROL<FI.TEMP.MAIL.MSG>:'-':'FAILURE'
    END ELSE
        R.REDO.TEMP.FI.CONTROL<FI.TEMP.STATUS>   ='01'
        R.REDO.TEMP.FI.CONTROL<FI.TEMP.MAIL.MSG> =R.REDO.TEMP.FI.CONTROL<FI.TEMP.MAIL.MSG>:'-':'SUCCESS'
        R.REDO.TEMP.FI.CONTROL<FI.TEMP.FT.REF>   =OUT.REF
    END
    R.REDO.TEMP.FI.CONTROL<FI.TEMP.RESP.MSG>     =OUT.RESP<1>

    CALL F.WRITE(FN.REDO.TEMP.FI.CONTROL,Y.REC.ID,R.REDO.TEMP.FI.CONTROL)
RETURN

END
