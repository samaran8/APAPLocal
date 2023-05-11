* @ValidationCode : MjotNjYzNTYwNzk6Q3AxMjUyOjE2ODE3MjQ5NTkwMzc6SGFyaXNodmlrcmFtQzotMTotMTowOjA6ZmFsc2U6Ti9BOlIyMV9BTVIuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 17 Apr 2023 15:19:19
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOENQ
*-----------------------------------------------------------------------------------------------------
* <Rating>-100</Rating>
*-----------------------------------------------------------------------------------------------------
*Modification Details:
*=====================
* Date          Who                Reference              Description
* ------        -----              -------------          -------------
* 01 Jan 2018   Gopala Krishnan R  PACS00641500           Fixing the Issue
* 17-APR-2023     Conversion tool    R22 Auto conversion       No changes
* 17-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*-----------------------------------------------------------------------------------------------------
SUBROUTINE REDO.E.NOF.RTE.RTN(TXN.ARRAY)

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.TELLER
    $INSERT I_F.T24.FUND.SERVICES
    GOSUB INIT
    GOSUB PROCESS
RETURN
INIT:
******

    FN.FUNDS.TRANSFER='F.FUNDS.TRANSFER'
    F.FUNDS.TRANSFER=''
    CALL OPF(FN.FUNDS.TRANSFER,F.FUNDS.TRANSFER)

    FN.TELLER='F.TELLER'
    F.TELLER=''
    CALL OPF(FN.TELLER,F.TELLER)
    Y.TXN.DATE=''
    FLAG.BRANCH = ''
    FLAG.TELL.ID = ''
    Y.BRANCH = ''
    Y.TELLER.ID = ''

* Modification start - ODR-2009-10-0318 B.126

    FN.T24.FUND.SERVICES='F.T24.FUND.SERVICES'
    F.T24.FUND.SERVICES=''
    CALL OPF(FN.T24.FUND.SERVICES,F.T24.FUND.SERVICES)

    FN.T24.FUND.SERVICES.HIS = 'F.T24.FUND.SERVICES$HIS'
    F.T24.FUND.SERVICES.HIS = ''
    CALL OPF(FN.T24.FUND.SERVICES.HIS,F.T24.FUND.SERVICES.HIS)

    FN.FUNDS.TRANSFER.HIS='F.FUNDS.TRANSFER$HIS'
    F.FUNDS.TRANSFER.HIS=''
    CALL OPF(FN.FUNDS.TRANSFER.HIS,F.FUNDS.TRANSFER.HIS)

    FN.TELLER.HIS='F.TELLER$HIS'
    F.TELLER.HIS=''
    CALL OPF(FN.TELLER.HIS,F.TELLER.HIS)

* Modification end - ODR-2009-10-0318 B.126

RETURN

PROCESS:
*********
    LOCATE "TXN.DATE" IN D.FIELDS<1> SETTING POS THEN
        Y.TXN.DATE=D.RANGE.AND.VALUE<POS>
    END
*PACS00023990 - S
    LOCATE "BRANCH" IN D.FIELDS<1> SETTING POS1 THEN
        FLAG.BRANCH = 1
        Y.BRANCH = D.RANGE.AND.VALUE<POS1>
    END
    LOCATE "TELLER.ID" IN D.FIELDS<1> SETTING POS2 THEN
        FLAG.TELL.ID = 1
        Y.TELLER.ID = D.RANGE.AND.VALUE<POS2>
    END

    LOC.POS = ''; LOC.APP = ''; LOC.FLD = ''
    LOC.APP = "TELLER":
    LOC.FLD = "L.RTE.FORM"
    CALL MULTI.GET.LOC.REF(LOC.APP, LOC.FLD, LOC.POS)
    TT.L.RTE.FORM.POS = LOC.POS<1,1>
  
  
  
    GOSUB FT.PROCESS
    GOSUB TT.PROCESS
    GOSUB TFS.PROCESS
*PACS00023990 -E
RETURN
****************************
FT.PROCESS:
*****************************
*/funds transfer selection
    IF Y.TXN.DATE EQ TODAY THEN
        SEL.FT.CMD = "SSELECT ":FN.FUNDS.TRANSFER:" WITH DEBIT.VALUE.DATE EQ ":Y.TXN.DATE:" AND L.RTE.FORM EQ 'YES'"
    END ELSE
        SEL.FT.CMD ="SSELECT " :FN.FUNDS.TRANSFER.HIS:" WITH DEBIT.VALUE.DATE EQ ":Y.TXN.DATE:" AND L.RTE.FORM EQ 'YES' AND RECORD.STATUS NE 'REVE'"
    END
    IF FLAG.BRANCH THEN
        SEL.FT.CMD:=" AND CO.CODE EQ ":Y.BRANCH
    END
    IF FLAG.TELL.ID ELSE
        CALL EB.READLIST(SEL.FT.CMD,SEL.FT.LIST,'',FT.NOR,FT.ERR)
        LOOP
            REMOVE Y.FT.ID FROM SEL.FT.LIST SETTING FT.POS
        WHILE Y.FT.ID:FT.POS
            R.FUNDS.TRANSFER = ''
            FINDSTR ";" IN  Y.FT.ID  SETTING FT.POS THEN
                CALL F.READ(FN.FUNDS.TRANSFER.HIS,Y.FT.ID,R.FUNDS.TRANSFER,F.FUNDS.TRANSFER.HIS,ERR.FT)
            END ELSE
                CALL F.READ(FN.FUNDS.TRANSFER,Y.FT.ID,R.FUNDS.TRANSFER,F.FUNDS.TRANSFER,ERR.FT)
            END
            Y.DBT.AMT=R.FUNDS.TRANSFER<FT.DEBIT.AMOUNT>
            IF Y.DBT.AMT NE '' THEN
                AMOUNT.EXCEED=Y.DBT.AMT
            END ELSE
                AMOUNT.EXCEED=R.FUNDS.TRANSFER<FT.CREDIT.AMOUNT>
            END
            TRANS.REFERENCE = FIELD(Y.FT.ID,";",1)
            TXN.DATE=R.FUNDS.TRANSFER<FT.DEBIT.VALUE.DATE>
            IF Y.TXN.DATE EQ TODAY THEN
                REC.STATUS='LIVE'
            END ELSE
                REC.STATUS = R.FUNDS.TRANSFER<FT.RECORD.STATUS>
            END
            IF FLAG.BRANCH THEN
                VAR.BRANCH = Y.BRANCH
            END ELSE
                VAR.BRANCH = R.FUNDS.TRANSFER<FT.CO.CODE>
            END
            IF Y.FT.ID NE '' THEN
                TXN.ARRAY<-1>=TRANS.REFERENCE:'*':AMOUNT.EXCEED:'*':TXN.DATE:'*':REC.STATUS:'*':VAR.BRANCH:'*':Y.TELLER.ID
            END
        REPEAT
    END
RETURN
***********************
TT.PROCESS:
*************************
*/teller selection
    IF Y.TXN.DATE EQ TODAY THEN
*SEL.TT.CMD = "SSELECT ":FN.TELLER:" WITH L.RTE.FORM EQ 'YES' AND (VALUE.DATE.1 EQ ":Y.TXN.DATE:" OR VALUE.DATE.2 EQ ":Y.TXN.DATE:")"
*SEL.TT.CMD = "SSELECT ":FN.TELLER:" WITH (VALUE.DATE.1 EQ ":Y.TXN.DATE:" OR VALUE.DATE.2 EQ ":Y.TXN.DATE:")"
        SEL.TT.CMD = "SSELECT ":FN.TELLER
    END ELSE
*SEL.TT.CMD ="SSELECT ":FN.TELLER.HIS:" WITH L.RTE.FORM EQ 'YES' AND RECORD.STATUS NE 'REVE' AND (VALUE.DATE.1 EQ ":Y.TXN.DATE:" OR VALUE.DATE.2 EQ ":Y.TXN.DATE:")"
        SEL.TT.CMD ="SSELECT ":FN.TELLER.HIS:" WITH RECORD.STATUS NE 'REVE' AND (VALUE.DATE.1 EQ ":Y.TXN.DATE:" OR VALUE.DATE.2 EQ ":Y.TXN.DATE:")"
    END
*  IF FLAG.TELL.ID THEN
*    SEL.TT.CMD:=" AND TELLER.ID.1 EQ ":Y.TELLER.ID
*  END
*  IF FLAG.BRANCH THEN
*    SEL.TT.CMD:=" AND CO.CODE EQ ":Y.BRANCH
*  END
    CALL EB.READLIST(SEL.TT.CMD,SEL.TT.LIST,'',TT.NOR,TT.ERR)
    LOOP
        REMOVE Y.TT.ID FROM SEL.TT.LIST SETTING TT.POS
    WHILE Y.TT.ID:TT.POS
        R.TELLER = ''
        FINDSTR ";" IN  Y.TT.ID SETTING FT.POS THEN
            CALL F.READ(FN.TELLER.HIS,Y.TT.ID,R.TELLER,F.TELLER.HIS,ERR.TT)
        END ELSE
            CALL F.READ(FN.TELLER,Y.TT.ID,R.TELLER,F.TELLER,ERR.TELLER)
        END
        OVERRIDE=R.TELLER<TT.TE.OVERRIDE>
        OVERRIDE.FLAG=''
        CHANGE @VM TO @FM IN OVERRIDE
        CHANGE @SM TO @FM IN OVERRIDE
        LOCATE "AML" IN OVERRIDE<1> SETTING POS THEN
            OVERRIDE.FLAG='1'
        END
        RTE.FORM=R.TELLER<TT.TE.LOCAL.REF,TT.L.RTE.FORM.POS>
        IF (RTE.FORM EQ 'YES' OR OVERRIDE.FLAG NE '') AND (R.TELLER<TT.TE.VALUE.DATE.1> EQ Y.TXN.DATE OR R.TELLER<TT.TE.VALUE.DATE.2> EQ Y.TXN.DATE) THEN
            IF (FLAG.TELL.ID AND Y.TELLER.ID EQ R.TELLER<TT.TE.TELLER.ID.1>) OR NOT(FLAG.TELL.ID) THEN
                IF (FLAG.BRANCH AND Y.BRANCH EQ R.TELLER<TT.TE.CO.CODE>) OR NOT(FLAG.BRANCH) THEN
                    TRANS.REFERENCE=FIELD(Y.TT.ID,";",1)
                    AMOUNT.EXCEED=R.TELLER<TT.TE.AMOUNT.LOCAL.1>
                    VALUE.DTE2=R.TELLER<TT.TE.VALUE.DATE.2>
                    VALUE.DTE1=R.TELLER<TT.TE.VALUE.DATE.1>
                    IF VALUE.DTE2 NE '' THEN
                        TXN.DATE=VALUE.DTE2
                    END ELSE
                        TXN.DATE=VALUE.DTE1
                    END
                    IF Y.TXN.DATE EQ TODAY THEN
                        REC.STATUS='LIVE'
                    END ELSE
                        REC.STATUS=R.TELLER<TT.TE.RECORD.STATUS>
                    END
                    IF FLAG.BRANCH THEN
                        VAR.BRANCH = Y.BRANCH
                    END ELSE
                        VAR.BRANCH = R.TELLER<TT.TE.CO.CODE>
                    END
                    IF FLAG.TELL.ID THEN
                        VAR.TELLER.ID = Y.TELLER.ID
                    END ELSE
                        VAR.TELLER.ID = R.TELLER<TT.TE.TELLER.ID.1>
                    END
                    IF Y.TT.ID NE '' THEN
                        TXN.ARRAY<-1>=TRANS.REFERENCE:'*':AMOUNT.EXCEED:'*':TXN.DATE:'*':REC.STATUS:'*':VAR.BRANCH:'*':VAR.TELLER.ID
                    END
                END
            END
        END
    REPEAT
RETURN
**************************
TFS.PROCESS:
***************************
* Modification start - ODR-2009-10-0318 B.126
* As dated on 22 July 2010, for ODR-2009-10-0318 B.126

    IF Y.TXN.DATE EQ TODAY THEN
        SEL.TFS.CMD = "SSELECT ":FN.T24.FUND.SERVICES:" WITH L.RTE.FORM EQ 'YES' AND BOOKING.DATE EQ ":Y.TXN.DATE
    END ELSE
        SEL.TFS.CMD = "SSELECT ":FN.T24.FUND.SERVICES.HIS:" WITH L.RTE.FORM EQ 'YES' AND RECORD.STATUS NE 'REVE' AND BOOKING.DATE EQ ":Y.TXN.DATE
    END
    IF FLAG.BRANCH THEN
        SEL.TFS.CMD:=" AND CO.CODE EQ ":Y.BRANCH
    END
    IF FLAG.TELL.ID ELSE
        CALL EB.READLIST(SEL.TFS.CMD,SEL.TFS.LIST,'',NO.OF.REC,SEL.ERR)
        IF NOT(SEL.TFS.LIST) THEN
            RETURN
        END
        LOOP
            REMOVE T24.FUND.SERVICES.ID FROM SEL.TFS.LIST SETTING Y.TFS.POS
        WHILE T24.FUND.SERVICES.ID : Y.TFS.POS
            FINDSTR ";" IN  T24.FUND.SERVICES.ID SETTING TFS.POS THEN
                R.T24.FUND.SERVICES  = ''
                TFS.ER = ''
                CALL F.READ(FN.T24.FUND.SERVICES.HIS,T24.FUND.SERVICES.ID,R.T24.FUND.SERVICES,F.T24.FUND.SERVICES.HIS,TFS.ERR)
            END ELSE
                R.T24.FUND.SERVICES  = ''
                T24.FUND.SERVICES.ER = ''
                CALL F.READ(FN.T24.FUND.SERVICES,T24.FUND.SERVICES.ID,R.T24.FUND.SERVICES,F.T24.FUND.SERVICES,T24.FUND.SERVICES.ER)
            END

            TRANS.REFERENCE = FIELD(T24.FUND.SERVICES.ID,";",1)
            AMOUNT.EXCEED   = R.T24.FUND.SERVICES<TFS.AMOUNT>
            TXN.DATE        = R.T24.FUND.SERVICES<TFS.BOOKING.DATE>
            IF Y.TXN.DATE EQ TODAY THEN
                REC.STATUS      = 'LIVE'
            END ELSE
                REC.STATUS = R.T24.FUND.SERVICES<TFS.RECORD.STATUS>
            END
            IF FLAG.BRANCH THEN
                VAR.BRANCH = Y.BRANCH
            END ELSE
                VAR.BRANCH = R.T24.FUND.SERVICES<TFS.CO.CODE>
            END
            TXN.ARRAY<-1>=TRANS.REFERENCE:'*':AMOUNT.EXCEED:'*':TXN.DATE:'*':REC.STATUS:'*':VAR.BRANCH:"*":Y.TELLER.ID

        REPEAT
* Modification end - ODR-2009-10-0318 B.126, As dated on 22 July 2010
    END
RETURN
*----------------------------------------------------------------------------------------------------------------------------------------------
END
