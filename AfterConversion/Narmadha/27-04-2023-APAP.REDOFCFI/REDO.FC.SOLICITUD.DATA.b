* @ValidationCode : Mjo4NTE4MjcxNzM6Q3AxMjUyOjE2ODA2MDcxMzA3MTc6SVRTUzotMTotMTozNjM6MTpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 04 Apr 2023 16:48:50
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : 363
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOFCFI
SUBROUTINE REDO.FC.SOLICITUD.DATA(SOL.DATA)
*-----------------------------------------------------------------------------
* DESCRIPTION:
* This is an Nofile Enquiry for the App REDO.FC.SOLICITUD
*
* Input/Output:
* OUT : SOL.DATA (Solicitud Data)
*-----------------------------------------------------------------------------
* Modification History :
*   Date            Who                   Reference               Description
* 09-MAY-2011    LPAZMINO                PACS00051761            FIRST VERSION
* 05-APRIL-2023      Harsha                R22 Auto Conversion  - FM to @FM and I to I.VAR 
* 05-APRIL-2023      Harsha                R22 Manual Conversion - No changes 
*----------------------------------------------------------------------------
*-----------------------------------------------------------------------------

* <region name="INSERTS">
    $INSERT I_COMMON
    $INSERT I_EQUATE

    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.REDO.FC.SOLICITUD
    $INSERT I_F.REDO.FC.CUST.SOLICITUD

* </region>

    GOSUB INITIALISE
    GOSUB PROCESS
RETURN

*-----------------
INITIALISE:
*-----------------
    FN.REDO.FC.SOLICITUD = 'F.REDO.FC.SOLICITUD'
    F.REDO.FC.SOLICITUD  = ''

    FN.REDO.FC.CUST.SOLICITUD = 'F.REDO.FC.CUST.SOLICITUD'
    F.REDO.FC.CUST.SOLICITUD  = ''

    R.CUST.SOLICITUD = ''
    R.SOLICITUD = ''

    Y.CUST.ID = ''
    Y.CUS.SOL = ''
    Y.SOL.ID  = ''
    Y.ERR     = ''

    SOL.DATA  = ''

RETURN

*-----------------
OPEN.FILES:
*-----------------
    CALL OPF(FN.REDO.FC.SOLICITUD,F.REDO.FC.SOLICITUD)
    CALL OPF(FN.REDO.FC.CUST.SOLICITUD,F.REDO.FC.CUST.SOLICITUD)

RETURN

*-----------------
PROCESS:
*-----------------
    GOSUB OPEN.FILES
    LOCATE 'CONTACT.CLIENT' IN D.FIELDS<1> SETTING Y.POS THEN
        Y.CUST.ID = D.RANGE.AND.VALUE<Y.POS>
        GOSUB GET.SOLICITUD.DATA
    END

RETURN

*-----------------
GET.SOLICITUD.DATA:
*-----------------
    CALL F.READ(FN.REDO.FC.CUST.SOLICITUD, Y.CUST.ID, R.CUST.SOLICITUD, F.REDO.FC.CUST.SOLICITUD, Y.ERR)

*Split Solicitudes IDs
    Y.CUS.SOL = R.CUST.SOLICITUD
    Y.CUS.SOL.COUNT = DCOUNT(R.CUST.SOLICITUD,@FM)
    FOR I.VAR = 1 TO Y.CUS.SOL.COUNT
        Y.SOL.ID = FIELD(R.CUST.SOLICITUD,@FM,I.VAR,1)
        GOSUB GET.SOLICITUD.DETAILS
    NEXT I.VAR
RETURN

*--------------------
GET.SOLICITUD.DETAILS:
*--------------------
    CALL F.READ(FN.REDO.FC.SOLICITUD, Y.SOL.ID, R.SOLICITUD, F.REDO.FC.SOLICITUD, Y.ERR)

    Y.ESTATUS = R.SOLICITUD<FC.SL.ESTATUS>
    Y.MONTO = ''
    Y.FECHA = ''

    BEGIN CASE
        CASE Y.ESTATUS EQ 'FORMALIZADA'
            Y.MONTO = R.SOLICITUD<FC.SL.AMT.FRMNEG>
            Y.FECHA = R.SOLICITUD<FC.SL.FEC.FRMNEG>
        CASE Y.ESTATUS EQ 'PREAPROBADA'
            Y.MONTO = R.SOLICITUD<FC.SL.AMT.PREAPROB>
            Y.FECHA = R.SOLICITUD<FC.SL.FEC.PREAPROB>
        CASE Y.ESTATUS EQ 'APROBADA'
            Y.MONTO = R.SOLICITUD<FC.SL.AMT.APROBADO>
            Y.FECHA = R.SOLICITUD<FC.SL.FEC.APROBADO>
        CASE Y.ESTATUS EQ 'REFERIDA'
            Y.MONTO = R.SOLICITUD<FC.SL.AMT.SOLICITA>
            Y.FECHA = R.SOLICITUD<FC.SL.FEC.SOLICITA>
        CASE Y.ESTATUS EQ 'DECLINADA'
            Y.MONTO = R.SOLICITUD<FC.SL.AMT.SOLICITA>
            Y.FECHA = R.SOLICITUD<FC.SL.FEC.SOLICITA>
    END CASE

* Generate output record
    SOL.DATA<-1> = Y.SOL.ID : '*' : Y.MONTO : '*' : Y.FECHA : '*' : Y.ESTATUS

RETURN

END
