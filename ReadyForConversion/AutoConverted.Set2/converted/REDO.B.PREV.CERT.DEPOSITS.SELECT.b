SUBROUTINE REDO.B.PREV.CERT.DEPOSITS.SELECT
* -------------------------------------------------------------------------------------------------
* Description           : This is the Batch Load Routine used to initalize all the required variables
*
* Developed By          : Amaravathi Krithika B
* Development Reference : CA01
* Attached To           : NA
* Attached As           : NA
*--------------------------------------------------------------------------------------------------
* Input Parameter:
* ---------------*
* Argument#1 : NA
*
*-----------------*
* Output Parameter:
* ----------------*
* Argument#4 : NA

*--------------------------------------------------------------------------------------------------
*  M O D I F I C A T I O N S
* ***************************
*--------------------------------------------------------------------------------------------------
* Defect Reference       Modified By                    Date of Change        Change Details
* (RTC/TUT/PACS)
*--------------------------------------------------------------------------------------------------
* Include files
*--------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AZ.ACCOUNT
    $INSERT I_F.CUSTOMER
    $INSERT I_F.ACCOUNT
    $INSERT I_BATCH.FILES
    $INSERT I_F.DATES
    $INSERT I_REDO.B.PREV.CERT.DEPOSITS.COMMON
    $INSERT I_F.REDO.H.REPORTS.PARAM

    GOSUB SEL.DEPOSITS
RETURN

SEL.DEPOSITS:
*-----------
    IF CONTROL.LIST EQ "" THEN
        CONTROL.LIST = "AZ":@FM:"ACBAL.HIST"
    END
    BEGIN CASE
        CASE CONTROL.LIST<1> EQ "AZ"
            CALL EB.CLEAR.FILE(FN.DR.REG.CA01.WORKFILE,F.DR.REG.CA01.WORKFILE)
            SEL.AZ.ACC = "SELECT ":FN.AZ.ACC:" WITH VALUE.DATE GE ":Y.STAT.MONTH:" AND VALUE.DATE LE ":YTODAY.DAT
            CALL EB.READLIST(SEL.AZ.ACC,SEL.LIST.AZ,'',NO.OF.REC,SEL.ERR)
            CALL BATCH.BUILD.LIST('',SEL.LIST.AZ)
        CASE CONTROL.LIST<1> EQ "ACBAL.HIST"
            SEL.AZ.HIS = "SELECT ":FN.AZ.ACC.BAL.HIST:" WITH DATE GE ":Y.STAT.MONTH:" AND DATE LE ":YTODAY.DAT:" BY-DSND @ID "
            CALL EB.READLIST(SEL.AZ.HIS,SEL.LIST.AZ,'',NO.OF.REC,SEL.ERR.HIS)
            CALL BATCH.BUILD.LIST('',SEL.LIST.AZ)
    END CASE
RETURN
END
