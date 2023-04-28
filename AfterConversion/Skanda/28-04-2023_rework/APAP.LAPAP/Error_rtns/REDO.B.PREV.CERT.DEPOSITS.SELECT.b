$PACKAGE APAP.LAPAP
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
** 24-04-2023 R22 Auto Conversion - FM TO @FM, VM to @VM, SM to @SM
** 24-04-2023 Skanda R22 Manual Conversion - No changes
*--------------------------------------------------------------------------------------------------
* Include files
*--------------------------------------------------------------------------------------------------
    $INSERT I_COMMON ;* R22 Auto conversion
    $INSERT I_EQUATE ;* R22 Auto conversion
    $INSERT I_F.AZ.ACCOUNT ;* R22 Auto conversion
    $INSERT I_F.CUSTOMER ;* R22 Auto conversion
    $INSERT I_F.ACCOUNT ;* R22 Auto conversion
    $INSERT I_BATCH.FILES ;* R22 Auto conversion
    $INSERT I_F.DATES ;* R22 Auto conversion
    $INSERT I_REDO.B.PREV.CERT.DEPOSITS.COMMON ;* R22 Auto conversion
    $INSERT I_F.REDO.H.REPORTS.PARAM ;* R22 Auto conversion

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
