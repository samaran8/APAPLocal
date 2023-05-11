* @ValidationCode : Mjo3OTY0NDQ0NjM6Q3AxMjUyOjE2ODExNTE2MjA2MDY6SVRTUzotMTotMTowOjE6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 11 Apr 2023 00:03:40
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
SUBROUTINE REDO.VAU.MTS.REV
*
*   Actualiza el registro de REDO.TRANSACTION.CHAIN si se trata de una
*   transaccion que forma parte de una cadena de transacciones
*-----------------------------------------------------------------------------------------------------
* Modification History:
*
* Date             Who                   Reference      Description
* 11.04.2023       Conversion Tool       R22            Auto Conversion     - FM TO @FM, VM TO @VM
* 11.04.2023       Shanmugapriya M       R22            Manual Conversion   - No changes
*
*------------------------------------------------------------------------------------------------------

*
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.TELLER
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.TELLER.ID
*
    $INSERT I_REDO.NV.COMMON
    $INSERT I_F.REDO.TRANSACTION.CHAIN
*


    GOSUB INITIALISE
    GOSUB OPEN.FILES
    GOSUB CHECK.PRELIM.CONDITIONS
    IF PROCESS.GOAHEAD THEN
        GOSUB PROCESS
    END
*
RETURN
*
PROCESS:
*


*  BEGIN CASE
*  CASE APPLICATION EQ "TELLER"
*      IF R.NEW(TT.TE.LOCAL.REF)<1,WPOSTA> NE "A" THEN
*          R.NEW(TT.TE.LOCAL.REF)<1,WPOSTA> = "A"
*      END ELSE
*          R.NEW(TT.TE.LOCAL.REF)<1,WPOSTA> = "X"
*      END

*  CASE APPLICATION EQ "FUNDS.TRANSFER"
*      IF R.NEW(FT.LOCAL.REF)<1,WPOS.FT.TA> NE "A" THEN
*          R.NEW(FT.LOCAL.REF)<1,WPOS.FT.TA> = "A"
*      END ELSE
*          R.NEW(FT.LOCAL.REF)<1,WPOS.FT.TA> = "X"
*      END
*  END CASE



*
    GOSUB UPDATE.REDO.TRANSACTION.CHAIN
    Y.TLL.ID = R.REDO.TRANSACTION.CHAIN<RTC.TELLER.ID>

    CALL F.READ(FN.REDO.INIT.ID.NV,Y.TLL.ID,R.REDO.INIT.ID.NV,F.REDO.INIT.ID.NV,NV.ERR)
    IF R.REDO.INIT.ID.NV THEN
        CALL F.DELETE(FN.REDO.INIT.ID.NV,Y.TLL.ID)
    END
*
RETURN
*
* ============================
UPDATE.REDO.TRANSACTION.CHAIN:
* ============================
*
    RTR = ""
    CALL F.READU(FN.REDO.TRANSACTION.CHAIN,WINITIAL.ID,R.REDO.TRANSACTION.CHAIN,F.REDO.TRANSACTION.CHAIN,ERR.MSJ,RTR)
    IF NOT(ERR.MSJ) THEN
        GOSUB WRITE.RTC
    END
*
RETURN
*
* ========
WRITE.RTC:
* ========
*
    LOCATE ID.NEW IN R.REDO.TRANSACTION.CHAIN<RTC.TRANS.ID,1> SETTING Y.POS THEN
        R.REDO.TRANSACTION.CHAIN<RTC.TRANS.STATUS,Y.POS> = 'REV'
        R.REDO.TRANSACTION.CHAIN<RTC.TRANS.AUTH>         = "AR"
        IF R.REDO.TRANSACTION.CHAIN<RTC.TRANS.STATUS> EQ 'REV' THEN
            R.REDO.TRANSACTION.CHAIN<RTC.TRANS.AUTH>   = "R"
            NV.AUTOR.PROCESS = ""
            NV.LAST.ID       = ""
        END ELSE
            GOSUB UPDATE.STATUS.RTC
        END
    END
*
    CALL F.WRITE(FN.REDO.TRANSACTION.CHAIN,WINITIAL.ID,R.REDO.TRANSACTION.CHAIN)
*
RETURN
*
* ================
UPDATE.STATUS.RTC:
* ================
*
    WTID.NUMBER = DCOUNT(R.REDO.TRANSACTION.CHAIN<RTC.TRANS.ID>,@VM)
    LOOP.CNT        = 1
    PROCESS.GOAHEAD = 1
*
    LOOP
    WHILE LOOP.CNT LE WTID.NUMBER AND PROCESS.GOAHEAD
        W.STATUS = R.REDO.TRANSACTION.CHAIN<RTC.TRANS.STATUS,LOOP.CNT>
        IF W.STATUS  NE 'REV' THEN
            PROCESS.GOAHEAD = ""
        END
*
        LOOP.CNT += 1
*
    REPEAT
*
    IF PROCESS.GOAHEAD THEN
        R.REDO.TRANSACTION.CHAIN<RTC.TRANS.AUTH>   = "R"
        NV.AUTOR.PROCESS = ""
        NV.LAST.ID       = ""
    END
*
RETURN
*
* =========
INITIALISE:
* =========
*
    PROCESS.GOAHEAD = 1
*
    FN.REDO.TRANSACTION.CHAIN = "F.REDO.TRANSACTION.CHAIN"
    F.REDO.TRANSACTION.CHAIN  = ""

    FN.REDO.INIT.ID.NV = 'F.REDO.INIT.ID.NV'
    F.REDO.INIT.ID.NV = ''
    CALL OPF(FN.REDO.INIT.ID.NV,F.REDO.INIT.ID.NV)
*
*    WCAMPO    = "L.TRAN.AUTH"
*    WCAMPO<2> = "L.INITIAL.ID"
*    WCAMPO    = CHANGE(WCAMPO,FM,VM)
*    CALL MULTI.GET.LOC.REF("TELLER",WCAMPO,YPOS)
*    WPOSTA  = YPOS<1,1>
*    WPOS.LI = YPOS<1,2>
*
*    WCAMPO = "L.TRAN.AUTH"
*    WCAMPO = CHANGE(WCAMPO,FM,VM)
*    CALL MULTI.GET.LOC.REF("FUNDS.TRANSFER",WCAMPO,YPOS)
*    WPOS.FT.TA = YPOS<1,1>
*
    Y.APLS = 'TELLER':@FM:'FUNDS.TRANSFER'
    WCAMPO = 'L.TRAN.AUTH':@VM:'L.INITIAL.ID':@FM:'L.TRAN.AUTH':@VM:'L.INITIAL.ID'
    CALL MULTI.GET.LOC.REF(Y.APLS,WCAMPO,YPOS)

    WPOSTA  = YPOS<1,1>
    WPOS.LI = YPOS<1,2>

    WPOS.FT.TA = YPOS<2,1>
    WPOS.FT.IN = YPOS<2,2>

    IF APPLICATION EQ 'TELLER' THEN
        WINITIAL.ID = R.NEW(TT.TE.LOCAL.REF)<1,WPOS.LI>
    END

    IF APPLICATION EQ 'FUNDS.TRANSFER' THEN
        WINITIAL.ID = R.NEW(FT.LOCAL.REF)<1,WPOS.FT.IN>
    END
*
RETURN
*
OPEN.FILES:
*
*
RETURN
*
CHECK.PRELIM.CONDITIONS:
*
    LOOP.CNT  = 1
    MAX.LOOPS = 1
*
    LOOP
    WHILE LOOP.CNT LE MAX.LOOPS AND PROCESS.GOAHEAD
        BEGIN CASE
*
            CASE LOOP.CNT EQ 1
                IF WINITIAL.ID EQ "" THEN
                    PROCESS.GOAHEAD = ""
                END

        END CASE
*
        LOOP.CNT += 1
*
    REPEAT
*
RETURN
*
END
