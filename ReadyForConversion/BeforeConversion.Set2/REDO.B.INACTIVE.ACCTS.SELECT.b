*-----------------------------------------------------------------------------
* <Rating>-12</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE REDO.B.INACTIVE.ACCTS.SELECT
*-----------------------------------------------------------------------------------------------------------------
*
* Description           : This routine select inactive accounts using fields L.AC.STATUS1 and L.AC.STATUS2
*
*
* Developed By          : Nowful Rahman M
*
* Development Reference : 199_CA02
*
* Attached To           : BATCH>BNK/REDO.B.INACTIVE.ACCTS
*
* Attached As           : Batch Routine
*-----------------------------------------------------------------------------------------------------------------
*------------------------
* Input Parameter:
* ---------------*
* Argument#1 : NA
* Argument#2 : NA
* Argument#3 : NA
*-----------------------------------------------------------------------------------------------------------------
*-----------------*
* Output Parameter:
* ----------------*
* Argument#4 : NA
* Argument#5 : NA
* Argument#6 : NA
*-----------------------------------------------------------------------------------------------------------------
*  M O D I F I C A T I O N S
* ***************************
*-----------------------------------------------------------------------------------------------------------------
* Defect Reference       Modified By                    Date of Change        Change Details
*(RTC/TUT/PACS)                                        (YYYY-MM-DD)
*-----------------------------------------------------------------------------------------------------------------
* PACS00353060          Ashokkumar.V.P                  07/11/2014           Changes based on mapping
*-----------------------------------------------------------------------------------------------------------------
* Include files
*-----------------------------------------------------------------------------------------------------------------
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT LAPAP.BP I_REDO.B.INACTIVE.ACCTS.COMMON

    GOSUB SEL.THE.FILE
    RETURN

SEL.THE.FILE:
*-----------------------------------------------------------------------------------------------------------------
* Select Account with L.AC.STATUS equal to 3YINACTIVE ABANDONED and L.AC.STATUS2 not equal to DECEASED GARNISHMENT
*-----------------------------------------------------------------------------------------------------------------
    CALL EB.CLEAR.FILE(FN.DR.REG.CA02.WORKFILE, F.DR.REG.CA02.WORKFILE)
    SEL.LIST = ''; NO.OF.REC = ''; SEL.CMD = ''
*    SEL.CMD = "SELECT ":FN.ACCOUNT:" WITH CATEGORY EQ ":Y.CAT.VAL.ARR.GRP:" AND L.AC.STATUS1 EQ '3YINACTIVE' 'ABANDONED'"
    SEL.CMD = "SELECT ":FN.ACCOUNT:" WITH ARRANGEMENT.ID EQ '' AND L.AC.STATUS1 EQ '3YINACTIVE' 'ABANDONED'"
    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,RET.CODE)
    CALL BATCH.BUILD.LIST('',SEL.LIST)
    RETURN
*------------------------------------------------------------------------Final End----------------------------------------------------------------------------------------------------
END
