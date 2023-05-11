* @ValidationCode : MjotOTk5NDEzOTpDcDEyNTI6MTY4MjQxMjMzODU2MTpIYXJpc2h2aWtyYW1DOi0xOi0xOjA6MTpmYWxzZTpOL0E6REVWXzIwMjEwOC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 25 Apr 2023 14:15:38
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOVER
SUBROUTINE REDO.V.AUTH.ACCOUNT.PROCESS
*-----------------------------------------------------------------------------
*DESCRIPTION:
*------------
*This routine is an authorisation routine attached to below versions,
* ACCOUNT,REDO.TERM.DEPOSIT.LOAN,
* AZ.ACCOUNT,REDO.MAIN
* TELLER,REDO.AA.CP
*
* Input/Output:
*--------------
* IN  : -NA-
* OUT : -NA-
*
* Dependencies:
*---------------
* CALLS : -NA-
* CALLED BY : -NA-
*
* Revision History:
*------------------
*   Date        who               Reference       Description
* 22-07-2011    JEEVA T           PACS00085750    Initial Creation
* 22-08-2011    Bharath G         PACS00100502    Version Inter branch Added
* 09-09-2011    Marimuthu         PACS00121111
* 23-09-2011    Marimuthu         PACS00121130
* 22/02/2013    Vignesh Kumaar R  PACS00251025    Update the Teller Id in the AZ Account
* 28/05/2013    Vignesh Kumaar R  PACS00245167    AUTH.DATE UNKNOWN VARIABLE ERROR
* 01/11/2013    Vignesh Kumaar R  PACS00273063    Chq printing
* 22/04/2014    Vignesh Kumaar R  PACS00273064    USD Cheque
*------------------------------------------------------------------------------------------
*Modification History
*DATE                       WHO                         REFERENCE                                   DESCRIPTION
*10-04-2023            Conversion Tool             R22 Auto Code conversion                FM TO @FM, IF CONDITION ADDED
*10-04-2023              Samaran T                R22 Manual Code conversion                         No Changes
*-----------------------------------------------------------------------------------------------------------------------

    $INSERT I_EQUATE
    $INSERT I_COMMON
    $INSERT I_F.ACCOUNT
    $INSERT I_F.AZ.ACCOUNT
    $INSERT I_System
    $INSERT I_F.TELLER
    $INSERT I_F.FUNDS.TRANSFER

    $INSERT I_F.REDO.MTS.DISBURSE
    $INSERT I_F.REDO.PRINT.CHQ.LIST
    $INSERT I_GTS.COMMON

    GOSUB INIT
    GOSUB PROCESS

* Fix for PACS00273063 [Chq printing]

    IF APPLICATION EQ 'REDO.PRINT.CHQ.LIST' THEN

        GOSUB PRINTER.TYPE
        NEW.CMD = '##UTILITY.ROUTINE##:'    ;* Instruction to run a Utility Routine
        NEW.CMD := 'REDO.UTIL.CHQ.PRINTING:'          ;* Name of the Utility Routine
        NEW.CMD := ID.NEW:'###':Y.SLIP.ID   ;* Get the record id
        CALL EB.SET.NEW.TASK(NEW.CMD)
    END

* End of Fix

RETURN

*************
PRINTER.TYPE:
*************

    Y.PRINTER.TYPE = R.NEW(PRINT.CHQ.LIST.SET.PRINTER)
    Y.CHQ.TYPE = R.NEW(PRINT.CHQ.LIST.CHQ.TYPE)
    Y.US.CHQ.FLAG = ''

* Fix for PACS00273064 [USD Cheque]

    IF ID.NEW[1,2] EQ 'FT' THEN

        FN.FUNDS.TRANSFER = 'F.FUNDS.TRANSFER'
        F.FUNDS.TRANSFER = ''
        CALL OPF(FN.FUNDS.TRANSFER,F.FUNDS.TRANSFER)

        CALL F.READ(FN.FUNDS.TRANSFER,ID.NEW,R.FUNDS.TRANSFER,F.FUNDS.TRANSFER,FUNDS.ERR)
        Y.DB.CCY = R.FUNDS.TRANSFER<FT.DEBIT.CURRENCY>
        Y.CR.CCY = R.FUNDS.TRANSFER<FT.CREDIT.CURRENCY>
        IF Y.DB.CCY EQ 'USD' AND Y.CR.CCY EQ 'USD' THEN
            Y.US.CHQ.FLAG = 1
        END
    END

* End of Fix

    BEGIN CASE
        CASE Y.PRINTER.TYPE EQ 'EPSON'
            IF Y.CHQ.TYPE EQ 'MANAGER' THEN
                Y.SLIP.ID = 'CHQ.PRINT.MGEPS'
            END ELSE
                Y.SLIP.ID = 'CHQ.PRINT.EPS'
            END
            RETURN

        CASE Y.PRINTER.TYPE EQ 'OLIVETTI'
            IF Y.CHQ.TYPE EQ 'MANAGER' THEN
                IF Y.US.CHQ.FLAG THEN
                    Y.SLIP.ID = 'CHQ.PRINT.USOLI'
                END ELSE
                    Y.SLIP.ID = 'CHQ.PRINT.MGOLI'
                END
            END ELSE
                Y.SLIP.ID = 'CHQ.PRINT.OLI'       ;* Fix for PACS00273064
            END
            RETURN

        CASE OTHERWISE
            IF Y.CHQ.TYPE EQ 'MANAGER' THEN
                IF Y.US.CHQ.FLAG THEN
                    Y.SLIP.ID = 'CHQ.PRINT.USRIC'
                END ELSE
                    Y.SLIP.ID = 'CHQ.PRINT.MGRIC'
                END
            END ELSE
                Y.SLIP.ID = 'CHQ.PRINT.RIC'       ;* Fix for PACS00273064
            END
            RETURN

    END CASE

RETURN

******
INIT:
******
*Initialize all the variables

    FN.REDO.MTS.DISBURSE = 'F.REDO.MTS.DISBURSE'
    F.REDO.MTS.DISBURSE = ''
    CALL OPF(FN.REDO.MTS.DISBURSE,F.REDO.MTS.DISBURSE)

* Fix for PACS00251025 [Update the Teller Id in the AZ Account]

    FN.AZ.ACCOUNT = 'F.AZ.ACCOUNT'
    F.AZ.ACCOUNT = ''
    CALL OPF(FN.AZ.ACCOUNT,F.AZ.ACCOUNT)

    LOC.REF.APP = 'ACCOUNT':@FM:'AZ.ACCOUNT'
    LOC.REF.FLDS = 'L.AZ.APP':@FM:'L.AZ.REF.NO'
    LOC.REF.POS = ''
    CALL MULTI.GET.LOC.REF(LOC.REF.APP,LOC.REF.FLDS,LOC.REF.POS)

    LOL.AZ.POS = LOC.REF.POS<1,1>
    L.AZ.REF.NO.POS = LOC.REF.POS<2,1>

* End of Fix

* Fix for PACS00245167 [AUTH.DATE UNKNOWN VARIABLE ERROR]

    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)

* End of Fix

RETURN
*********
PROCESS:
*********
* Updating acocunt id in local template

* Fix for PACS00245167 [AUTH.DATE UNKNOWN VARIABLE ERROR]

    GET.E.VAL = E     ;* Backup for E
    E = ''
    Y.ID = System.getVariable("CURRENT.FT")
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN   ;*R22 AUTO CODE CONVERSION.START
        Y.ID = "" ;*R22 AUTO CODE CONVERSION
    END  ;*R22 AUTO CODE CONVERSION.END

    IF E THEN
        E = ''
    END
    E = GET.E.VAL

    IF Y.ID EQ '' OR Y.ID EQ 'CURRENT.FT' THEN
        IF APPLICATION EQ  'TELLER' AND R.NEW(TT.TE.THEIR.REFERENCE) NE '' THEN
            GET.LOAN.ACCT = R.NEW(TT.TE.THEIR.REFERENCE)
            CALL F.READ(FN.ACCOUNT,GET.LOAN.ACCT,R.ACCOUNT,F.ACCOUNT,ACCOUNT.ERR)
            IF R.ACCOUNT THEN
                GET.LOAN.ID = R.ACCOUNT<AC.ARRANGEMENT.ID>
                SEL.CMD = "SELECT ":FN.REDO.MTS.DISBURSE:" WITH ARRANGEMENT.ID EQ ":GET.LOAN.ID
                CALL EB.READLIST(SEL.CMD,GET.FT.ID,'','','')
            END
        END
        IF GET.FT.ID[1,2] NE 'FT' THEN
            RETURN
        END ELSE
            Y.ID = GET.FT.ID
        END
    END

* End of Fix

    CALL F.READ(FN.REDO.MTS.DISBURSE,Y.ID,R.REDO.MTS.DISBURSE,F.REDO.MTS.DISBURSE,Y.ERR)

    IF APPLICATION EQ 'ACCOUNT' AND PGM.VERSION EQ ",REDO.TERM.DEPOSIT.LOAN" THEN
        CALL System.setVariable("CURRENT.AC.ID",ID.NEW)
    END

    IF APPLICATION EQ 'TELLER' AND (PGM.VERSION EQ ',REDO.AA.CP' OR PGM.VERSION EQ ',REDO.AA.DEPOSIT') THEN
        R.REDO.MTS.DISBURSE<MT.REF.ID> = ID.NEW
        R.REDO.MTS.DISBURSE<MT.AZ.ACCT.STATUS> = 'PROCESSED'
        CALL F.WRITE(FN.REDO.MTS.DISBURSE,Y.ID,R.REDO.MTS.DISBURSE)

* Fix for PACS00251025 [Update the Teller Id in the AZ Account]

        R.AZ.ACCOUNT = ''
        GET.CREDIT.ACCT = R.NEW(TT.TE.ACCOUNT.2)
        CALL F.READ(FN.AZ.ACCOUNT,GET.CREDIT.ACCT,R.AZ.ACCOUNT,F.AZ.ACCOUNT,AZ.ERR)

        IF R.AZ.ACCOUNT THEN
            R.AZ.ACCOUNT<AZ.LOCAL.REF,L.AZ.REF.NO.POS> = ID.NEW
            CALL F.WRITE(FN.AZ.ACCOUNT,GET.CREDIT.ACCT,R.AZ.ACCOUNT)
*      CALL REDO.AZ.WRITE.TRACE("REDO.V.AUTH.ACCOUNT.PROCESS",GET.CREDIT.ACCT)
        END

* End of Fix

    END

    IF APPLICATION EQ 'REDO.PRINT.CHQ.LIST' THEN
        R.REDO.MTS.DISBURSE<MT.AZ.ACCT.STATUS> = 'PROCESSED'
        CALL F.WRITE(FN.REDO.MTS.DISBURSE,Y.ID,R.REDO.MTS.DISBURSE)
    END
*  PACS00100502 - S
    IF APPLICATION EQ 'FUNDS.TRANSFER' AND (PGM.VERSION EQ ',CHQ.OTHERS.LOAN' OR PGM.VERSION EQ ',REDO.AA.INTERBRANCH.ACH.FNL' OR PGM.VERSION EQ ',CHQ.OTHERS.LOAN.DUM') THEN
        R.REDO.MTS.DISBURSE<MT.REF.ID> = ID.NEW
        R.REDO.MTS.DISBURSE<MT.AZ.ACCT.STATUS> = 'PROCESSED'
        CALL F.WRITE(FN.REDO.MTS.DISBURSE,Y.ID,R.REDO.MTS.DISBURSE)
    END
*  PACS00100502 - E

    IF APPLICATION EQ 'AZ.ACCOUNT' AND PGM.VERSION EQ ",REDO.MAIN" THEN
        R.REDO.MTS.DISBURSE<MT.AZ.ACCOUNT>     = ID.NEW
        R.REDO.MTS.DISBURSE<MT.AZ.ACCT.STATUS> = 'PROCESSED'
        CALL F.WRITE(FN.REDO.MTS.DISBURSE,Y.ID,R.REDO.MTS.DISBURSE)
    END

** PACS00121111 -S
    IF APPLICATION EQ 'AZ.ACCOUNT' AND PGM.VERSION EQ ',REDO.MULTI.PROCESS' THEN
        R.REDO.MTS.DISBURSE<MT.AZ.ACCOUNT>     = ID.NEW
        R.REDO.MTS.DISBURSE<MT.AZ.ACCT.STATUS> = 'PROCESSED'
        CALL F.WRITE(FN.REDO.MTS.DISBURSE,Y.ID,R.REDO.MTS.DISBURSE)
    END
** PACS00121111 -E

RETURN
*******************************************************************************************
END
