* @ValidationCode : Mjo2OTEyNjY0NTY6Q3AxMjUyOjE2ODI1MTM1MDg1NTM6dmlnbmVzaHdhcmk6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 26 Apr 2023 18:21:48
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : vigneshwari
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOENQ
SUBROUTINE NOFILE.REDO.REQUESTS.PROCESS(Y.ARRAY)
***********************************************************************

* COMPANY NAME: ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* DEVELOPED BY: S SUDHARSANAN
* PROGRAM NAME: NOFILE.REDO.REQUESTS.PROCESS
* ODR NO      : ODR-2009-12-0283
*----------------------------------------------------------------------
* Modification History :
*-----------------------
*DATE           WHO           REFERENCE         DESCRIPTION
*16.08.2012  S SUDHARSANAN   ODR-2009-12-0283    INITIAL CREATION
*  DATE             WHO                   REFERENCE
* 06-APRIL-2023      Conversion Tool       R22 Auto Conversion  - FM to @FM and ++ to +=1
* 06-APRIL-2023      Harsha                R22 Manual Conversion - Added APAP.REDOENQ
*----------------------------------------------------------------------------
*----------------------------------------------------------------------


    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.REDO.ISSUE.REQUESTS
    $INSERT I_ENQUIRY.COMMON
*
    GOSUB INIT
    GOSUB GET.CLAIM.IDS
    GOSUB PROCESS
RETURN
*----------------------------------------------------------------------
INIT:
*----------------------------------------------------------------------

    FN.REDO.ISSUE.REQUESTS = 'F.REDO.ISSUE.REQUESTS'
    F.REDO.ISSUE.REQUESTS = ''
    CALL OPF(FN.REDO.ISSUE.REQUESTS,F.REDO.ISSUE.REQUESTS)

RETURN
*----------------------------------------------------------------------
GET.CLAIM.IDS:
*----------------------------------------------------------------------
* All selection fields related to REDO.ISSUE.REQUESTS application are processed here

    FILE.NAME = FN.REDO.ISSUE.REQUESTS
    IN.FIXED = "STATUS EQ OPEN"
    CALL APAP.REDOENQ.RedoEFormSelStmt(FILE.NAME, IN.FIXED, '', SEL.REQ.CMD)   ;*R22 Manual Conversion - Added APAP.REDOENQ
    CALL EB.READLIST(SEL.REQ.CMD,REQ.ID.LST,'',NO.OF.REC.REQ,SEL.ERR)
    Y.COMMON.ARRAY = REQ.ID.LST
RETURN

*-------------------------------------------------------------------------------------------------------
PROCESS:
*------------------------------------------------------------------------------------------------------
    Y.ARRAY.CNT = DCOUNT(Y.COMMON.ARRAY,@FM)
    VAR2=1
    LOOP
    WHILE VAR2 LE Y.ARRAY.CNT

        Y.ISSUE.REQ.ID = Y.COMMON.ARRAY<VAR2>         ;*************3RD FILED
        CALL F.READ(FN.REDO.ISSUE.REQUESTS,Y.ISSUE.REQ.ID,R.REDO.ISSUE.REQ,F.REDO.ISSUE.REQUESTS,REQUESTS.ERR)
        Y.CUSTOMER.CODE = R.REDO.ISSUE.REQ<ISS.REQ.CUSTOMER.CODE>         ;**********1ST FIELD
        Y.STATUS = R.REDO.ISSUE.REQ<ISS.REQ.STATUS>   ;**********4TH FIELD
        Y.CUSTOMER.ID.NO = R.REDO.ISSUE.REQ<ISS.REQ.CUST.ID.NUMBER>       ;**********2ND FIELD
        Y.SUPP.GRP          = R.REDO.ISSUE.REQ<ISS.REQ.SUPPORT.GROUP>  ;  ;**********5TH FIELD
        Y.ARRAY<-1> = Y.CUSTOMER.CODE:'*':Y.CUSTOMER.ID.NO:'*':Y.ISSUE.REQ.ID:'*':Y.STATUS:'*':Y.SUPP.GRP
        VAR2 += 1
    REPEAT
RETURN
*--------------------------------------------------------------------------------------------------------------
END
