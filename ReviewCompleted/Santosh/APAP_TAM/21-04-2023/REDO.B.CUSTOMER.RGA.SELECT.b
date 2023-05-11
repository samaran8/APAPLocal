$PACKAGE APAP.TAM
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
** 21-04-2023 R22 Auto Conversion 
** 21-04-2023 Skanda R22 Manual Conversion - No changes
*--------------------------------------------------------------------------------

    $INSERT I_COMMON ;* R22 Auto conversion
    $INSERT I_EQUATE ;* R22 Auto conversion
    $INSERT I_REDO.B.CUSTOMER.RGA.COMMON ;* R22 Auto conversion
    $INSERT I_F.REDO.H.REPORTS.PARAM ;* R22 Auto conversion
    $INSERT I_F.DATES ;* R22 Auto conversion

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
