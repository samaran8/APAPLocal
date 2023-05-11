*-----------------------------------------------------------------------------
* <Rating>-40</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE REDO.B.CUSTOMER.RGA.LOAD
*-------------------------------------------------------------------------------
* Company Name      : PAGE SOLUTIONS, INDIA
* Developed By      : Nirmal.P
* Reference         :
*-------------------------------------------------------------------------------
* Subroutine Type   : B
* Attached to       :
* Attached as       : Multi threaded Batch Routine
*-------------------------------------------------------------------------------
* Input / Output :
*----------------
* IN     :
* OUT    :
*-------------------------------------------------------------------------------
* Description: This is a .LOAD Subroutine
*
*-------------------------------------------------------------------------------
*  M O D I F I C A T I O N S
* ***************************
*   Date       Author              Modification Description
*
* 05/02/2015  Ashokkumar.V.P        PACS00368383 - New mapping changes
*--------------------------------------------------------------------------------

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_TSA.COMMON
    $INSERT T24.BP I_BATCH.FILES
    $INSERT LAPAP.BP I_REDO.B.CUSTOMER.RGA.COMMON
    $INSERT TAM.BP I_F.REDO.H.REPORTS.PARAM
    $INCLUDE TAM.BP I_REDO.GENERIC.FIELD.POS.COMMON
*

    GOSUB OPEN.PARA
    GOSUB PROCESS.PARA
*
    RETURN
*-------------------------------------------------------------------------------
OPEN.PARA:
*---------
*
    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

    FN.REDO.H.REPORTS.PARAM = 'F.REDO.H.REPORTS.PARAM'
    F.REDO.H.REPORTS.PARAM = ''
    CALL OPF(FN.REDO.H.REPORTS.PARAM,F.REDO.H.REPORTS.PARAM)

    FN.DR.REG.RIEN15.WORKFILE = 'F.DR.REG.RIEN15.WORKFILE'
    F.DR.REG.RIEN15.WORKFILE = ''
    CALL OPF(FN.DR.REG.RIEN15.WORKFILE,F.DR.REG.RIEN15.WORKFILE)
    Y.FLAG = 1
    RETURN
*--------------------------------------------------------------------------------
PROCESS.PARA:
*------------
    GOSUB GET.PARAM.DETAILS
    GOSUB GET.MULTI.LOCAL.REF
    PROCESS.POST.RTN = ''
    RETURN
*-------------------------------------------------------------------------------
GET.PARAM.DETAILS:
*-----------------
    REDO.H.REPORTS.PARAM.ID = 'REDO.RN15'
*
    R.REDO.H.REPORTS.PARAM = ''; REDO.PARAM.ERR = ''
    CALL CACHE.READ(FN.REDO.H.REPORTS.PARAM,REDO.H.REPORTS.PARAM.ID,R.REDO.H.REPORTS.PARAM,REDO.PARAM.ERR)
    IF REDO.H.REPORTS.PARAM.ID THEN
        FILE.NAME = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.OUT.FILE.NAME>
        TEMP.PATH = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.TEMP.DIR>
        OUT.PATH = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.OUT.DIR>
        FIELD.NAME = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.FIELD.NAME>
        FIELD.VALUE = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.FIELD.VALUE>
        DISPLAY.TEXT = R.REDO.H.REPORTS.PARAM<REDO.REP.PARAM.DISPLAY.TEXT>
    END
    RETURN

GET.MULTI.LOCAL.REF:
*-------------------
*
    Y.POS = ''
    Y.APPLICATION = 'CUSTOMER'
    Y.FIELDS = 'L.CU.CIDENT':VM:'L.CU.RNC':VM:'L.CU.PASS.NAT':VM:'L.CU.TIPO.CL':VM:'L.CU.GRP.RIESGO'
    CALL MULTI.GET.LOC.REF(Y.APPLICATION,Y.FIELDS,Y.POS)
    L.CU.CIDENT.POS = Y.POS<1,1>
    L.CU.RNC.POS = Y.POS<1,2>
    L.CU.FOREIGN.POS = Y.POS<1,3>
    L.CU.TIPO.CL.POS = Y.POS<1,4>
    L.CU.GRP.RIESGO.POS = Y.POS<1,5>
    RETURN
*----------------------------------------------------------------------
END
