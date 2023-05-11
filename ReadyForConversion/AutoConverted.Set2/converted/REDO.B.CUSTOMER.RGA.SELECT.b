SUBROUTINE REDO.B.CUSTOMER.RGA.SELECT
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
* Description: This is a .SELECT Subroutine
*
*-------------------------------------------------------------------------------
*  M O D I F I C A T I O N S
* ***************************
*   Date       Author              Modification Description
*
* 05/02/2015  Ashokkumar.V.P        PACS00368383 - New mapping changes
*--------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_REDO.B.CUSTOMER.RGA.COMMON
    $INSERT I_F.REDO.H.REPORTS.PARAM
    $INSERT I_F.DATES

    GOSUB PROCESS.PARA
RETURN
*-------------------------------------------------------------------------------
PROCESS.PARA:

    CALL EB.CLEAR.FILE(FN.DR.REG.RIEN15.WORKFILE,F.DR.REG.RIEN15.WORKFILE)
    SEL.CMD = "SELECT ":FN.CUSTOMER
    LIST.REC = ''
    NO.OF.CUS = ''
    CUS.ERR = ''
    CALL EB.READLIST(SEL.CMD,LIST.REC,'',NO.OF.CUS,CUS.ERR)

    CALL BATCH.BUILD.LIST("",LIST.REC)
RETURN
END
