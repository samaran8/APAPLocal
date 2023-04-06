* @ValidationCode : MjotMTU4MzAwNzU1NTpDcDEyNTI6MTY4MDc2MDkzODk1NTpJVFNTQk5HOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 06 Apr 2023 11:32:18
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.REDOFCFI
SUBROUTINE REDO.FI.ORANGE.PAYMENTS.SELECT

*-------------------------------------------------------------------------------------------------------------------------------
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*04-04-2023            CONVERSION TOOL                AUTO R22 CODE CONVERSION           VM TO @VM ,FM TO @FM
*04-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES

    $INSERT I_EQUATE
    $INSERT I_COMMON
    $INSERT I_BATCH.FILES
*
    $INSERT I_REDO.FI.ORANGE.PYMT.COMMON
*
    IF PROCESS.GOAHEAD THEN
        GOSUB INITIALISE
        GOSUB OPEN.FILES
        GOSUB CHECK.PRELIM.CONDITIONS
        IF PROCESS.GOAHEAD  THEN
            GOSUB PROCESS
        END ELSE
            CALL TXT(W.ERROR)
        END
    END
*
RETURN
*
* =====
PROCESS:
* =====
*
    CALL BATCH.BUILD.LIST(LIST.PARAM,TR.ID.LIST)
*
RETURN
*
* =========
INITIALISE:

* =========
*
    LOOP.CNT       = 1
    MAX.LOOPS      = 2
    FT.NUM.REC     = 0
    TT.NUM.REC     = 0
*
    CHANGE @FM TO " " IN FT.TRAN.LIST
    CHANGE @FM TO " " IN TT.TRAN.LIST
*
    WSEL.FT   = "TRANSACTION.TYPE EQ " : FT.TRAN.LIST :
    WSEL.FT  := " AND WITH RECORD.STATUS NE REVE"
*
    WSEL.TT   = "TRANSACTION.CODE EQ " : TT.TRAN.LIST :
    WSEL.TT  := " AND WITH RECORD.STATUS NE REVE"
*
    W.ERROR        = ""
    TR.ID.LIST     = ""
    FT.ERR.CODE    = ""
    TT.ERR.CODE    = ""
    LIST.PARAM     = ""
*
RETURN
*
*
* ========
OPEN.FILES:
* ========
*
*
RETURN
*
* ===================
CHECK.PRELIM.CONDITIONS:
* ===================
*
    LOOP
    WHILE LOOP.CNT LE MAX.LOOPS AND PROCESS.GOAHEAD DO
        BEGIN CASE

            CASE LOOP.CNT EQ 1
                IF NOT(CONTROL.LIST) THEN
                    CONTROL.LIST = "FUNDS.TRANSFER":@FM:"TELLER"
                END

            CASE LOOP.CNT EQ 2
                PROCESS.TO.DO = CONTROL.LIST<1,1>
                BEGIN CASE
                    CASE PROCESS.TO.DO EQ "FUNDS.TRANSFER"
                        LIST.PARAM<2> = FN.FUNDS.TRANSFER
                        LIST.PARAM<3> = WSEL.FT

                    CASE PROCESS.TO.DO EQ "TELLER"
                        LIST.PARAM<2> = FN.TELLER
                        LIST.PARAM<3> = WSEL.TT

                    CASE 1
                        W.ERROR = "PROCESS.&.NOT.DEFINED":@FM:PROCESS.TO.DO
                END CASE
        END CASE

        IF W.ERROR THEN
            PROCESS.GOAHEAD = 0
        END

        LOOP.CNT +=1
    REPEAT
*
RETURN
*

END
