* Version 1 13/04/00  GLOBUS Release No. 200508 30/06/05
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
SUBROUTINE REDO.B.BCR.REPORT.GEN.SELECT
*-----------------------------------------------------------------------------
* Select routine to setup the common area for the multi-threaded Close of Business
* job TEMPLATE.EOD.
*-----------------------------------------------------------------------------
* 2011-08-28 : PACS00060197  - C.22 Integration
*              hpasquel@temenos.com
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_BATCH.FILES
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.ENQUIRY
    $INSERT I_F.DATES
*
    $INSERT I_F.REDO.BCR.REPORT.DATA
    $INSERT I_REDO.B.BCR.REPORT.GEN.COMMON
    $INSERT I_F.REDO.INTERFACE.PARAM
    $INSERT I_F.REDO.BCR.REPORT.EXEC
*

    GOSUB PROCESS
RETURN
*-----------------------------------------------------------------------------
PROCESS:
*-----------------------------------------------------------------------------
    LIST.PARAMETERS = '' ; ID.LIST = ''

*SEL.CMD = "SELECT ":FN.AA:" WITH ARR.STATUS EQ 'CURRENT' 'EXPIRED' 'PENDING.CLOSURE' 'CLOSE' AND PRODUCT.LINE EQ 'LENDING' AND PRODUCT.GROUP NE 'LINEAS.DE.CREDITO.TC' "
    SEL.CMD = "SELECT ":FN.AA:" WITH PRODUCT.LINE EQ 'LENDING' "
    CALL EB.READLIST(SEL.CMD,AA.LIST,'',NO.REC,PGM.ERR)

    CALL OCOMO("Total of records to process " : NO.REC)
    CALL BATCH.BUILD.LIST(LIST.PARAMETERS,AA.LIST)
    CALL EB.CLEAR.FILE(FN.DATA,F.DATA)
RETURN
*-----------------------------------------------------------------------------
END
