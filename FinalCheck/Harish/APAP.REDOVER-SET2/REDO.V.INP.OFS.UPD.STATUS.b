* @ValidationCode : MjoxMjMxMDE5NTM6Q3AxMjUyOjE2ODEyODM5NDIzMTc6c2FtYXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 12 Apr 2023 12:49:02
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : samar
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOVER
SUBROUTINE REDO.V.INP.OFS.UPD.STATUS
*-----------------------------------------------------------------------------------------------------------
*Company   Name    :Asociacion Popular de Ahorros y Prestamos
*Developed By      :PRABHU.N
*Program   Name    :REDO.V.INP.OFS.UPD.STATUS
*-----------------------------------------------------------------------------------------------------------
*DESCRIPTION       :It is the input routine to validate the credit and debit accounts
* ----------------------------------------------------------------------------------------------------------
*
*Modification Details:
*=====================
*   Date               who                     Reference            Description
*===========        ====================       ===============     ==================
* 05-06-2010       GANESH H                    PACS00072713         MODIFICATION
*12-04-2023       Conversion Tool        R22 Auto Code conversion      No Changes
*12-04-2023       Samaran T               R22 Manual Code Conversion    No Changes
 
 
*-----------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_EB.EXTERNAL.COMMON
    $INSERT I_F.REDO.FILE.DATE.PROCESS
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.TSA.SERVICE


    FN.REDO.FILE.DATE.PROCESS = 'F.REDO.FILE.DATE.PROCESS'
    F.REDO.FILE.DATE.PROCESS  = ''
    CALL OPF(FN.REDO.FILE.DATE.PROCESS,F.REDO.FILE.DATE.PROCESS)

    FN.TSA.SERVICE = 'F.TSA.SERVICE'
    F.TSA.SERVICE  = ''
    CALL OPF(FN.TSA.SERVICE,F.TSA.SERVICE)

    GOSUB PROCESS
    GOSUB GOEND
RETURN
*********
PROCESS:
*********

    APPL.ARRAY = "FUNDS.TRANSFER"
    FIELD.ARRAY = "L.COMMENTS"
    FIELD.POS = ''
    CALL MULTI.GET.LOC.REF(APPL.ARRAY,FIELD.ARRAY,FIELD.POS)
    Y.LOC.COMMENTS.POS = FIELD.POS<1,1>
    Y.FILE.ID =  R.NEW(FT.LOCAL.REF)<1,Y.LOC.COMMENTS.POS>
    CALL F.READ(FN.REDO.FILE.DATE.PROCESS,Y.FILE.ID,R.REDO.FILE.DATE.PROCESS,F.REDO.FILE.DATE.PROCESS,REDO.FILE.DATE.PROCESS.ERR)
    IF  V$FUNCTION EQ 'D' THEN
        R.REDO.FILE.DATE.PROCESS<REDO.FILE.PRO.DEB.ACCT.STATUS> = 'RECHAZO POR AUTORIZADOR'
        R.REDO.FILE.DATE.PROCESS<REDO.FILE.PRO.OFS.PROCESS> = 'FAILURE'
    END ELSE
        R.REDO.FILE.DATE.PROCESS<REDO.FILE.PRO.OFS.PROCESS> = 'PROCESS'
    END
    CALL F.WRITE(FN.REDO.FILE.DATE.PROCESS,Y.FILE.ID,R.REDO.FILE.DATE.PROCESS)
RETURN
******
GOEND:
******
END

*---------------------------------------------*END OF SUBROUTINE*-------------------------------------------
