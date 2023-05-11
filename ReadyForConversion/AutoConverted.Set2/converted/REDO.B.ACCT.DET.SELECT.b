SUBROUTINE REDO.B.ACCT.DET.SELECT
*-------------------------------------------------------------------------------
* Company Name      : PAGE SOLUTIONS, INDIA
* Developed By      : Nirmal.P
* Reference         :
*-------------------------------------------------------------------------------
* Subroutine Type   : B
* Attached to       :
* Attached as       : Multi threaded Batch Routine..
*-------------------------------------------------------------------------------
* Input / Output :
*----------------
* IN     :
* OUT    :
*-------------------------------------------------------------------------------
*  M O D I F I C A T I O N S
* ***************************
*-----------------------------------------------------------------------------------------------------------------
* Defect Reference       Modified By                    Date of Change        Change Details
*(RTC/TUT/PACS)                                        (YYYY-MM-DD)
*-----------------------------------------------------------------------------------------------------------------
* PACS00361294          Ashokkumar.V.P                  14/11/2014           Changes based on mapping.
*-----------------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_REDO.B.ACCT.DET.COMMON
    $INSERT I_F.REDO.H.REPORTS.PARAM


    CALL EB.CLEAR.FILE(FN.DR.REG.RIEN7.WORKFILE,F.DR.REG.RIEN7.WORKFILE)
*
    CA.POS = ''; SEL.LIST = ''; NO.OF.REC1 = ''; ACC.ERR1 = ''
    SEL.CMD1 = "SELECT ":FN.ACCOUNT:" WITH CATEGORY EQ ":Y.CATEG.LIST
    CALL EB.READLIST(SEL.CMD1,SEL.LIST,'',NO.OF.REC1,ACC.ERR1)
    CALL BATCH.BUILD.LIST("",SEL.LIST)
RETURN
END
