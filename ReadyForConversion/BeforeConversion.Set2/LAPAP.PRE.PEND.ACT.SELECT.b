*-----------------------------------------------------------------------------
* <Rating>-22</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.PRE.PEND.ACT.SELECT
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_BATCH.FILES
    $INSERT T24.BP I_TSA.COMMON
    $INSERT T24.BP I_F.AA.ARRANGEMENT.ACTIVITY
    $INSERT T24.BP I_F.USER
    $INSERT T24.BP I_F.FUNDS.TRANSFER
    $INSERT TAM.BP I_F.REDO.H.REPORTS.PARAM
    $INSERT BP I_F.ST.L.APAP.AAA.PENDIENTENAU
    $INSERT T24.BP I_F.AA.ARRANGEMENT
    $INSERT LAPAP.BP LAPAP.PRE.PEND.ACT.COMMON

    IF NOT(CONTROL.LIST) THEN
        GOSUB BUILD.CONTROL.LIST
    END
    GOSUB SEL.THE.FILE
    RETURN

BUILD.CONTROL.LIST:
*******************
    CALL EB.CLEAR.FILE(FN.ST.L.APAP.AAA.PENDIENTENAU, F.ST.L.APAP.AAA.PENDIENTENAU)
    CONTROL.LIST<-1> = "SELECT.INAU"
    CONTROL.LIST<-1> = "SELECT.LIVE"
    RETURN

*-----------------------------------------------------------------------------------------------------------------
SEL.THE.FILE:
*-----------------------------------------------------------------------------------------------------------------
    LIST.PARAMETER = ""
    BEGIN CASE
    CASE CONTROL.LIST<1,1> EQ "SELECT.INAU"
*SEL.CMD = "SELECT ":FN.ARRANGEMENT.ACTIVITY$NAU: " WITH EFFECTIVE.DATE EQ ":Y.FECHA.ACTUAL
*CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,RET.CODE)
*CALL BATCH.BUILD.LIST('',SEL.LIST)
        LIST.PARAMETER<2> = FN.ARRANGEMENT.ACTIVITY$NAU
        LIST.PARAMETER<3> = "EFFECTIVE.DATE EQ ":Y.FECHA.ACTUAL

    CASE CONTROL.LIST<1,1> EQ "SELECT.LIVE"
        LIST.PARAMETER = ""
        LIST.PARAMETER<2> = FN.ARRANGEMENT.ACTIVITY
        LIST.PARAMETER<3> = "EFFECTIVE.DATE EQ ":Y.FECHA.ACTUAL
*CALL BATCH.BUILD.LIST(LIST.PARAMETER, "")

    END CASE
    CALL BATCH.BUILD.LIST(LIST.PARAMETER, "")
    RETURN
END