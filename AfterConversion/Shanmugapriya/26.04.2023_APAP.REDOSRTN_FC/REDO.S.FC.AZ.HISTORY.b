* @ValidationCode : MjoxMzkwMTE5NjkyOkNwMTI1MjoxNjgyNDE1MTQxMTk2OklUU1M6LTE6LTE6MDoxOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 25 Apr 2023 15:02:21
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
$PACKAGE APAP.REDOSRTN
SUBROUTINE REDO.S.FC.AZ.HISTORY(AC.ID, AC.REC)

*
* Subroutine Type : ROUTINE
* Attached to     : ROUTINE REDO.E.NOF.DATCUST
* Attached as     : ROUTINE
* Primary Purpose :
*
* Incoming:
* ---------
*
*
* Outgoing:
* ---------
* AC.REC - data returned to the routine
*
* Error Variables:
*-----------------------------------------------------------------------------------
* Modification History:
*
* Development for : Asociacion Popular de Ahorros y Prestamos
* Development by  : Juan Pablo Armas - TAM Latin America
* Date            :
*Modification history
*Date                Who               Reference                  Description
*06-04-2023      conversion tool     R22 Auto code conversion     VM TO @VM,SM TO @SM,++ TO +=1,K TO VAR,NRO.MONTHS TO 1 STEP -1 TO I.VAR
*06-04-2023      Mohanraj R          R22 Manual code conversion   No changes

*-----------------------------------------------------------------------------------


    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.EB.CONTRACT.BALANCES
    $INSERT I_F.STMT.ACCT.CR
    $INSERT I_F.AZ.ACCT.BAL

    GOSUB INITIALISE
    GOSUB OPEN.FILES

    IF PROCESS.GOAHEAD THEN
        GOSUB PROCESS
    END

RETURN  ;* Program RETURN
*-----------------------------------------------------------------------------------
PROCESS:
*======

    CALL F.READ(FN.EB.CONTRACT.BALANCES,AC.ID,R.EB.CONTRACT.BALANCES,F.EB.CONTRACT.BALANCES,"")

    IF R.EB.CONTRACT.BALANCES THEN
        NRO.MONTHS = DCOUNT(R.EB.CONTRACT.BALANCES<ECB.ACTIVITY.MONTHS>,@VM)
        GOSUB GET.STMT.ACCT.CR
        FOR I.VAR = NRO.MONTHS TO 1 STEP -1 ;*R22 Auto code conversion
*---------------------------------
            Y.CUR.CAMPO = "ACTIVITY.MONTHS"
            Y.ACT.MONTH = R.EB.CONTRACT.BALANCES<ECB.ACTIVITY.MONTHS,I.VAR> ;*R22 Auto code conversion
            Y.VALUE.FLD = Y.CUR.CAMPO:"*":Y.ACT.MONTH
            AC.REC <-1> = Y.VALUE.FLD
*---------------------------------
            Y.CUR.CAMPO = "CR.VAL.BALANCE"
            GOSUB GET.STMT.ACCT.CR.DETAIL
            IF Y.LST.HRD THEN
                ID.STMT.AC = Y.LST.HRD<1>
                CALL F.READ(FN.R.STMT.ACCT.CR,ID.STMT.AC,R.STMT.ACCT.CR,F.STMT.ACCT.CR,"")
                NRO.VAL = DCOUNT(R.STMT.ACCT.CR<IC.STMCR.CR.VAL.BALANCE>,@VM)
                Y.VALUE.FLD = R.STMT.ACCT.CR<IC.STMCR.CR.VAL.BALANCE,NRO.VAL>
            END ELSE
                Y.VALUE.FLD = 'NULO'
            END
            Y.VALUE.FLD = Y.CUR.CAMPO:"*":Y.VALUE.FLD
            AC.REC <-1> = Y.VALUE.FLD
*---------------------------------
            Y.CUR.CAMPO = "CR.INT.AMT"
            IF Y.LST.HRD THEN
                Y.VALUE.FLD = 0
                FOR K.VAR = 1 TO NRO.VAL ;*R22 Auto code conversion
                    Y.VALUE.FLD += R.STMT.ACCT.CR<IC.STMCR.CR.INT.AMT,K.VAR> ;*R22 Auto code conversion
                NEXT
            END ELSE
                Y.VALUE.FLD = 'NULO'
            END
            Y.VALUE.FLD = Y.CUR.CAMPO:"*":Y.VALUE.FLD
            AC.REC <-1> = Y.VALUE.FLD
*---------------------------------
            Y.CUR.CAMPO = "ONLINE.ACTUAL.BAL"
            GOSUB GET.PRINCIPAL
            Y.VALUE.FLD = Y.AZ.ACCT.PRIN
            Y.VALUE.FLD = Y.CUR.CAMPO:"*":Y.VALUE.FLD
            AC.REC <-1> = Y.VALUE.FLD
        NEXT
    END
RETURN

*-----------------
GET.PRINCIPAL:
*-----------------
    Y.AZ.ACCT.PRIN = 0
    CALL F.READ(FN.AZ.ACCT.BAL,AC.ID,R.AZ.ACCT.BAL,F.AZ.ACCT.BAL,"")
    IF R.AZ.ACCT.BAL THEN
        NRO.REGS = DCOUNT(R.AZ.ACCT.BAL<AZ.ACCT.DATE>,@VM)
        FOR Y.J = 1 TO NRO.REGS
            Y.ACCT.DATE = R.AZ.ACCT.BAL<AZ.ACCT.DATE, Y.J>
            IF Y.ACT.MONTH EQ Y.ACCT.DATE[1,6] THEN
                Y.AZ.ACCT.PRIN + = R.AZ.ACCT.BAL<AZ.ACCT.PRINCIPAL, Y.J>
            END
        NEXT

    END
RETURN
*-----------------
GET.STMT.ACCT.CR:
*-----------------
    SEL.CMD = 'SELECT ':FN.STMT.ACCT.CR:' LIKE ':AC.ID:'... BY-DSND @ID'
    LISTA.HDR = ''
    NO.REC.HEADER = ''
    RET.CODE = ''
    CALL EB.READLIST(SEL.CMD, LISTA.HDR, '', NO.REC.HEADER, RET.CODE)

RETURN
*------------------------
GET.STMT.ACCT.CR.DETAIL:
*------------------------
    LISTA.HDR.AUX = LISTA.HDR
    Y.ID.STMT = AC.ID:'-':Y.ACT.MONTH
    Y.LST.HRD = ''
    Y.I = 0
    IF LISTA.HDR.AUX THEN
        LOOP
            REMOVE ID.STMT.CR FROM LISTA.HDR.AUX SETTING POS
        WHILE (ID.STMT.CR AND (Y.I LT 12))
            Y.TAM = LEN(ID.STMT.CR)-2
            ID.STMT.CR.AUX = ID.STMT.CR[1,Y.TAM]
            IF Y.ID.STMT EQ ID.STMT.CR.AUX THEN
                Y.LST.HRD<-1> = ID.STMT.CR
                Y.I += 1
            END

        REPEAT
    END
RETURN

*************************
INITIALISE:
*=========
    PROCESS.GOAHEAD = 1

    FN.EB.CONTRACT.BALANCES  = 'F.EB.CONTRACT.BALANCES'
    F.EB.CONTRACT.BALANCES   = ''
    R.EB.CONTRACT.BALANCES  = ''

    FN.STMT.ACCT.CR = 'F.STMT.ACCT.CR'
    F.STMT.ACCT.CR  = ''
    R.STMT.ACCT.CR  = ''

    FN.ACCT.ACTIVITY = 'F.ACCT.ACTIVITY'
    F.ACCT.ACTIVITY  = ''
    R.ACCT.ACTIVITY  = ''

    FN.AZ.ACCT.BAL = 'F.AZ.ACCT.BAL'
    F.AZ.ACCT.BAL  = ''
    R.AZ.ACCT.BAL  = ''

RETURN

*------------------------
OPEN.FILES:
*=========
    CALL OPF(FN.EB.CONTRACT.BALANCES,F.EB.CONTRACT.BALANCES)
    CALL OPF(FN.STMT.ACCT.CR,F.STMT.ACCT.CR)
    CALL OPF(FN.ACCT.ACTIVITY, F.ACCT.ACTIVITY)
    CALL OPF(FN.AZ.ACCT.BAL,F.AZ.ACCT.BAL)

RETURN
*------------
END
