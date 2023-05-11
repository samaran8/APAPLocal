* @ValidationCode : MjoxODAwMDk3MTU2OkNwMTI1MjoxNjgyNTg5NzY5NDM0OkhhcmlzaHZpa3JhbUM6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 27 Apr 2023 15:32:49
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
$PACKAGE APAP.REDOVER
SUBROUTINE REDO.V.WRAP.SUNNEL(Y.ARRAY)
*--------------------------------------------------------------------------------
*Company   Name    :Asociacion Popular de Ahorros y Prestamos
*Developed By      :PRABHU.N
*Program   Name    :REDO.V.WRAP.SUNNEL
*---------------------------------------------------------------------------------

*DESCRIPTION       :It is a wrapper routine defined as part of sunnel interface
*                   This routine will get the Input from R.NEW and forms the string required
*                   for sunnel based on the value set up in  REDO.SUNNEL.PARAMETER
*                   It also assigns the output from SUNNEL to the T24 field
*LINKED WITH       :
*-----------------------------------------------------------------------------------
*Input parameter :Sunnel Method name(@id of REDO.SUNNEL.PARAMETER)
*output parameter:In case of enquiries it will return the array of values
* ----------------------------------------------------------------------------------
*Modification Details:
*=====================
*   Date               who           Reference            Description
* 01-DEC-2010        Prabhu.N       SUNNEL-CR           Initial Creation
* 13-05-2011         GANESH H       PACS00063129        MODIFICATION
* 07-07-2011         Prabhu N       PACS00079592        Error message for connectivity fail
*-------------------------------------------------------------------------------------
*Modification History
*DATE                       WHO                         REFERENCE                                   DESCRIPTION
*17-04-2023            Conversion Tool             R22 Auto Code conversion                FM TO @FM,VM TO @VM,SM TO @SM,IF CONDITION ADDED,++ TO +=1
*17-04-2023              Samaran T                R22 Manual Code conversion                         CALL ROUTINE FORMAT MODIFIED
*---------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_System
*
    $INSERT I_F.STANDARD.SELECTION
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.STANDING.ORDER
*   $INSERT I_ENQUIRY.COMMON   ;*R22 AUTO CODE CONVERSION
    $INSERT I_F.BENEFICIARY
*
    $INSERT I_F.REDO.SUNNEL.PARAMETER
    $INSERT I_F.REDO.SUNNEL.METHOD
    $USING APAP.REDOCHNLS
*
    $INSERT JBC.h
*
    GOSUB OPF
    GOSUB INIT
    GOSUB PROCESS
*
RETURN
*
*---
OPF:
*---
*
    FN.REDO.SUNNEL.PARAMETER = 'F.REDO.SUNNEL.PARAMETER'
    FN.REDO.SUNNEL.METHOD    = 'F.REDO.SUNNEL.METHOD'

    FN.STANDARD.SELECTION    = 'F.STANDARD.SELECTION'
    F.STANDARD.SELECTION    = ''
    CALL OPF(FN.STANDARD.SELECTION,F.STANDARD.SELECTION)

    FN.CUS.BEN = 'F.CUS.BEN.LIST'
    F.CUS.BEN  = ''
    CALL OPF(FN.CUS.BEN,F.CUS.BEN)

    LOC.FIELD.REF  = ''
    LOC.APP        = ''
    SUNNEL.POS     = ''
    SUNNEL.CARD.NO = ''
*PACS00063129-S
    LOC.FIELD.REF = 'L.FT.CR.CARD.NO':@VM:'L.FT.CR.ACCT.NO':@FM:'L.BEN.CEDULA'
    LOC.APP       = 'FUNDS.TRANSFER':@FM:'BENEFICIARY'
    CALL MULTI.GET.LOC.REF(LOC.APP,LOC.FIELD.REF,SUNNEL.POS)
    CARD.NUM.POS  = SUNNEL.POS<1,1>
    CARD.ACCT.POS = SUNNEL.POS<1,2>
    CEDULA.NO.POS = SUNNEL.POS<2,1>
*PACS00063129-E
*SUNNEL.CUS.CARD.LIST=System.getVariable("CURRENT.CARD.LIST.CUS")
RETURN
*
*---------
INIT:
*---------
*In this part Input strong required for sunnel is formed based on REDO.SUNNEL.PARAMETER and R.NEW
*-------------------------------------------------------------------------------------------------
*PACS00063129-S
    BEGIN CASE
        CASE PGM.VERSION MATCHES ',AI.REDO.CR.CARD.RD.PAY':@VM:',AI.REDO.CR.CARD.US.PAY'
            GOSUB OWN.BEN.GET.METHOD
        CASE PGM.VERSION MATCHES ',AI.REDO.APAP.BEN.CARD.PAY':@VM:',AI.REDO.BEN.CARD.US.PAY'
            GOSUB OTHER.BEN.GET.METHOD
        CASE PGM.VERSION MATCHES ',AI.REDO.CARD.PAYMENT.RD':@VM:',AI.REDO.CARD.PAYMENT.USD'
            GOSUB GET.OWN.STO.METHOD
        CASE PGM.VERSION MATCHES ',AI.REDO.ADD.OWN.BANK.BEN'
            GOSUB VAL.BEN.DETAILS
        CASE OTHERWISE
            Y.METHOD.NAME = Y.ARRAY
    END CASE
*PACS00063129-E
    CALL CACHE.READ(FN.REDO.SUNNEL.PARAMETER,'SYSTEM',R.REDO.SUNNEL.PARAMETER,ERR)
    CALL CACHE.READ(FN.REDO.SUNNEL.METHOD,Y.METHOD.NAME,R.REDO.SUNNEL.METHOD,ERR)

    Y.APPLICATION    = R.REDO.SUNNEL.METHOD<RE.GE.APPLICATION>
    Y.IN.SF.MARKER   = R.REDO.SUNNEL.PARAMETER<SP.IN.SF.MARKER>
    Y.IN.FLD.MAKER   = R.REDO.SUNNEL.PARAMETER<SP.IN.FLD.MARKER>
    Y.OUT.DELIM      = R.REDO.SUNNEL.PARAMETER<SP.OT.FLD.MARKER>
    Y.OT.VAL.MARK    = R.REDO.SUNNEL.PARAMETER<SP.OT.VAL.MARKER>
    Y.OT.SUB.MARK    = R.REDO.SUNNEL.PARAMETER<SP.OT.SUB.MARKER>
    Y.CODE.FIELD     = R.REDO.SUNNEL.METHOD<RE.GE.MSG.CODE.FLD>
    Y.MSG.FIELD      = R.REDO.SUNNEL.METHOD<RE.GE.MSG.DESC.FLD>
    Y.ACTIVATION.KEY = R.REDO.SUNNEL.PARAMETER<SP.ACT.KEY>
    CHANGE @VM TO @FM IN Y.APPLICATION
    LOCATE APPLICATION IN Y.APPLICATION  SETTING Y.APP.POS ELSE
        Y.APP.POS = 1
    END
    Y.FIELD.IN   = R.REDO.SUNNEL.METHOD<RE.GE.T24.IN,Y.APP.POS>
    Y.FIELD.NAME = R.REDO.SUNNEL.METHOD<RE.GE.FLD.NAME,Y.APP.POS>
    Y.FIELD.TYPE = R.REDO.SUNNEL.METHOD<RE.GE.FLD.TYPE,Y.APP.POS>
    Y.FIELD.OUT  = R.REDO.SUNNEL.METHOD<RE.GE.T24.OUT>
    Y.OUT.NAME   = R.REDO.SUNNEL.METHOD<RE.GE.OT.NAME>
    Y.OUT.TYPE   = R.REDO.SUNNEL.METHOD<RE.GE.OT.TYPE>
    Y.RSM.FIELD  = R.REDO.SUNNEL.METHOD<RE.GE.RSM.FIELD.NAME>
    Y.ROUT.FIELD = R.REDO.SUNNEL.METHOD<RE.GE.RSM.ROUTINE.NAME>

    CHANGE @SM TO @FM IN Y.FIELD.IN
    CHANGE @SM TO @FM IN Y.FIELD.NAME
    CHANGE @SM TO @FM IN Y.FIELD.TYPE
    CHANGE @SM TO @FM IN Y.FIELD.OUT
    CHANGE @SM TO @FM IN Y.OUT.NAME
    CHANGE @SM TO @FM IN Y.OUT.TYPE
    CHANGE @SM TO @FM IN Y.RSM.FIELD
    CHANGE @SM TO @FM IN Y.ROUT.FIELD
*
    Y.ARRAY = R.REDO.SUNNEL.METHOD<RE.GE.METHOD.NAME> : R.REDO.SUNNEL.PARAMETER<SP.SEC.MARKER>
*
RETURN
*
* ======
PROCESS:
* ======
*
    IF PGM.VERSION EQ '' THEN
        GOSUB ENQUIRY.PROCESS
        GOSUB CALL.SUNNEL

        GOSUB OUT.ASSIGN
    END
    ELSE
        GOSUB FORM.INPUT
        GOSUB CALL.SUNNEL
        GOSUB PROCESS.OUTPUT
    END
*
RETURN
*
*
*----------------
ENQUIRY.PROCESS:
*----------------
*Forms the Input arguments to sunnel
*-----------------------------------------------------------------------------------------------------------------------------------

    Y.CNT            = 1
    Y.IN.FIELD.DELIM = ''
    Y.ENQ.FIELDS     = D.FIELDS
    Y.IN.FIELD.VAL   = D.RANGE.AND.VALUE
    CHANGE @VM TO @FM IN Y.ENQ.FIELDS
    CHANGE @VM TO @FM IN Y.IN.FIELD.VAL
*
    LOOP
        REMOVE Y.FIELD FROM Y.FIELD.IN SETTING IN.POS
    WHILE Y.FIELD:IN.POS

        LOCATE Y.FIELD IN Y.ENQ.FIELDS SETTING Y.IN.POS THEN

            VAR.FIELD.TYPE = Y.FIELD.TYPE<Y.CNT>
            IF VAR.FIELD.TYPE EQ 'DATE' THEN
                VAR.DATE.VAL             = Y.IN.FIELD.VAL<Y.IN.POS>
                VAR.DATE.VAL             = VAR.DATE.VAL[1,4] : '-':VAR.DATE.VAL[5,2] : '-':VAR.DATE.VAL[7,2]
                Y.IN.FIELD.VAL<Y.IN.POS> = VAR.DATE.VAL
            END

            Y.ARRAY  = Y.ARRAY:Y.IN.FIELD.DELIM:Y.IN.FIELD.VAL<Y.IN.POS>:Y.IN.SF.MARKER:Y.FIELD.NAME<Y.CNT>:Y.IN.SF.MARKER:Y.FIELD.TYPE<Y.CNT>
            Y.IN.FIELD.DELIM = Y.IN.FLD.MAKER
        END
        Y.CNT += 1
    REPEAT
*
RETURN
*
*-----------
FORM.INPUT:
*-----------
    Y.APPLICATION=APPLICATION
    CALL CACHE.READ(FN.STANDARD.SELECTION,Y.APPLICATION,R.STANDARD.SELECTION,ERR)
    TOT.SYS.FLD.NAME = R.STANDARD.SELECTION<SSL.SYS.FIELD.NAME>
    TOT.SYS.FLD.POS  = R.STANDARD.SELECTION<SSL.SYS.FIELD.NO>
    TOT.SYS.FLD.NUM  = R.STANDARD.SELECTION<SSL.SYS.FIELD.NO>
    TOT.USR.FLD.NAME = R.STANDARD.SELECTION<SSL.USR.FIELD.NAME>
    TOT.USR.FLD.POS  = R.STANDARD.SELECTION<SSL.USR.FIELD.NO>
    LOCATE 'LOCAL.REF' IN TOT.SYS.FLD.NAME<1,1> SETTING Y.LOCAL.POS THEN
        Y.LOCAL.POS = TOT.SYS.FLD.POS<1,Y.LOCAL.POS>
    END
    Y.CNT=1
    LOOP
        REMOVE Y.FIELD FROM Y.FIELD.IN SETTING Y.IN.FIELD.POS
        LOCATE Y.FIELD IN TOT.SYS.FLD.NAME<1,1> SETTING Y.FIELD.POS THEN
            Y.SYS.FIELD.POS = TOT.SYS.FLD.POS<1,Y.FIELD.POS>
            Y.FIELD.VAL     = R.NEW(Y.SYS.FIELD.POS)
        END
        ELSE
            LOCATE Y.FIELD IN TOT.USR.FLD.NAME<1,1> SETTING Y.FIELD.POS THEN
                Y.USR.FIELD.POS = TOT.USR.FLD.POS<1,Y.FIELD.POS>
                Y.USR.FIELD.POS = FIELD(Y.USR.FIELD.POS,',',2)
                Y.USR.FIELD.POS = FIELD(Y.USR.FIELD.POS,'>',1)
                Y.FIELD.VAL     = R.NEW(Y.LOCAL.POS)<1,Y.USR.FIELD.POS>
            END
            ELSE
* Code Review - 2012MAY29 - S
                GOSUB GET.FLD.VAL
* Code Review - 2012MAY29 - E
            END
        END


        IF Y.FIELD.VAL EQ '' THEN
            Y.FIELD.VAL = C$SPARE(100)
        END
*PACS00063129-S
        IF PGM.VERSION EQ ',AI.REDO.CARD.PAYMENT.RD' THEN

            Y.FIELD.VAL = C$SPARE(100)
            COMI        = ''

        END
*PACS00063129-E
    WHILE Y.FIELD:Y.OUT.FIELD.POS
        Y.ARRAY          = Y.ARRAY:Y.IN.FIELD.DELIM:Y.FIELD.VAL:Y.IN.SF.MARKER:Y.FIELD.NAME<Y.CNT>:Y.IN.SF.MARKER:Y.FIELD.TYPE<Y.CNT>
        Y.IN.FIELD.DELIM = Y.IN.FLD.MAKER
        Y.CNT += 1
    REPEAT
*
RETURN
*
*------------
CALL.SUNNEL:
*------------
*
*
    Y.ARRAY          = Y.ARRAY:R.REDO.SUNNEL.PARAMETER<SP.SEC.MARKER>
    Y.IN.FIELD.DELIM = ''
    Y.CNT=1
    LOOP
        REMOVE Y.FIELD FROM Y.OUT.NAME SETTING Y.OUT.FIELD.POS
    WHILE Y.FIELD:Y.OUT.FIELD.POS
        Y.ARRAY          = Y.ARRAY : Y.IN.FIELD.DELIM : Y.FIELD : Y.IN.SF.MARKER : Y.OUT.TYPE<Y.CNT>
        Y.IN.FIELD.DELIM = Y.IN.FLD.MAKER
        IF Y.OUT.TYPE<Y.CNT> EQ 'DATE' THEN
            Y.DATE.POS<-1> = Y.CNT + 1
        END
        Y.CNT += 1
    REPEAT
*
    Y.RESPONSE = CALLJEE(Y.ACTIVATION.KEY,Y.ARRAY)
*
    CHANGE Y.OUT.DELIM TO @FM IN Y.ARRAY
    CHANGE Y.OT.VAL.MARK TO @VM IN Y.ARRAY
    CHANGE Y.OT.SUB.MARK TO @SM IN Y.ARRAY

*PACS00063129-S
    IF PGM.VERSION EQ ',AI.REDO.ADD.OWN.BANK.BEN' THEN
        IF Y.ARRAY<12> NE BEN.CEDULA.NO  THEN
            ETEXT = 'EB-CARD.NO.EXIST'
            CALL STORE.END.ERROR
        END
    END
*PACS00063129-E
*
RETURN
*
*--------------
PROCESS.OUTPUT:
*--------------
*In this section output from sunnel is assigned to T24 field based on set up in REDO.SUNNEL.PARAMETER
*------------------------------------------------------------------------------------------------------

    LOCATE Y.CODE.FIELD IN Y.OUT.NAME SETTING Y.CODE.POS THEN
        Y.CODE.POS     = Y.CODE.POS++
        Y.MESSAGE.CODE = Y.ARRAY<Y.CODE.POS>
    END

    IF Y.MESSAGE.CODE NE 0  THEN
        GOSUB ERROR.PROCESS
    END
    ELSE
        IF PGM.VERSION NE '' THEN
            GOSUB OUT.ASSIGN
        END
    END
*
RETURN
*
*----------
OUT.ASSIGN:
*----------
*
    Y.CNT = 2
*
    LOOP
        REMOVE Y.FIELD FROM Y.FIELD.OUT SETTING Y.OUT.FIELD.POS
        LOCATE Y.CNT IN Y.DATE.POS SETTING D.POS THEN
            Y.DATE         = Y.ARRAY<Y.CNT>
            Y.DATE         = Y.DATE[1,4]:Y.DATE[6,2]:Y.DATE[9,2]
            Y.ARRAY<Y.CNT> = Y.DATE
        END
        LOCATE Y.FIELD IN TOT.SYS.FLD.NAME<1,1> SETTING Y.FIELD.POS THEN
            Y.SYS.FIELD.POS        = TOT.SYS.FLD.POS<1,Y.FIELD.POS>
            R.NEW(Y.SYS.FIELD.POS) = Y.ARRAY<Y.CNT>
        END
        ELSE
*
            LOCATE Y.FIELD IN TOT.USR.FLD.NAME<1,1> SETTING Y.FIELD.POS THEN
                Y.USR.FIELD.POS = TOT.USR.FLD.POS<1,Y.FIELD.POS>
                Y.USR.FIELD.POS = FIELD(Y.USR.FIELD.POS,',',2)
                Y.USR.FIELD.POS = FIELD(Y.USR.FIELD.POS,'>',1)
* Code Review - 2012MAY29 - S
                GOSUB GET.RTN.NAME
* Code Review - 2012MAY29 - E
                R.NEW(Y.LOCAL.POS)<1,Y.USR.FIELD.POS> = Y.ARRAY<Y.CNT>
            END
            IF Y.FIELD EQ 'COMI' THEN
                COMI = Y.ARRAY<Y.CNT>
            END
        END
    WHILE Y.FIELD:Y.OUT.FIELD.POS
        Y.CNT += 1
    REPEAT
*
RETURN
*
*-------------
ERROR.PROCESS:
*-------------
*
    BEGIN CASE
        CASE  Y.ARRAY<1> NE 'FAIL'
            LOCATE Y.MSG.FIELD IN Y.FIELD.OUT SETTING Y.MSG.POS THEN
                Y.MESSAGE = Y.ARRAY<Y.MSG.POS>
            END
*Y.ARRAY<1> NE 'FAIL' added for PACS00079592
            IF PGM.VERSION NE '' THEN
                Y.ERROR.CODE = R.REDO.SUNNEL.PARAMETER<SP.MESSAGE.CODE>
                CHANGE @VM TO @FM IN Y.ERROR.CODE
                LOCATE Y.MESSAGE.CODE IN Y.ERROR.CODE SETTING Y.ERROR.POS THEN
                    ETEXT = R.REDO.SUNNEL.PARAMETER<SP.MESSAGE.TYPE,Y.ERROR.POS>
                    CALL STORE.END.ERROR
                END
            END
*
        CASE  1
*PACS00079592-------Start---------------
            ETEXT = "EB-CONNECT.FAIL"
            CALL STORE.END.ERROR
*--------end---------------------------
            BAT.NO     = ''
            BAT.TOT    = ''
            INFO.OR    = ''
            INFO.DE    = ''
            Y.REC.CON    = ''
            EX.USER    = ''
            EX.PC      = ''
            MON.TP     = '03'
            INT.CODE   = 'SUN001'
            INT.TYPE   = 'ONLINE'
            ID.PROC    = ID.NEW
            Y.MESSAGE  = Y.ARRAY<2>
            Y.RESP.ERR = Y.MESSAGE
            DESC       = Y.RESP.ERR
            CALL APAP.REDOCHNLS.redoInterfaceRecAct(INT.CODE,INT.TYPE,BAT.NO,BAT.TOT,INFO.OR,INFO.DE,ID.PROC,MON.TP,DESC,Y.REC.CON,EX.USER,EX.PC)   ;*R22 MANUAL CODE CONVERSION
*
    END CASE
*
*
RETURN
*
*PACS00063129-S
*--------------------*
OWN.BEN.GET.METHOD:
*--------------------*
* Modifed below from FOR...NEXT to LOOP...REPEAT as part of the code review
*
    SUNNEL.CUS.CARD.LIST = System.getVariable("CURRENT.CARD.LIST.CUS")
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN   ;*R22 AUTO CODE CONVERSION.START
        SUNNEL.CUS.CARD.LIST = ""   ;*R22 AUTO CODE CONVERSION
    END   ;*R22 AUTO CODE CONVERSION.END
    Y.METHOD.NAME        = "BUSCAR_TARJETA_CUENTA.FT"
    SUNNEL.CARD.NO       = R.NEW(FT.LOCAL.REF)<1,CARD.NUM.POS>
    TOT.CARD.CUSTOMER    = DCOUNT(SUNNEL.CUS.CARD.LIST,'*')
*
    CNT.CARD = 1
    LOOP
        CARD.CUST.ID = FIELD(SUNNEL.CUS.CARD.LIST,'*',CNT.CARD)
    WHILE (CNT.CARD LE TOT.CARD.CUSTOMER)
        IF SUNNEL.CARD.NO[13,4] EQ CARD.CUST.ID[13,4] THEN
            C$SPARE(100) = CARD.CUST.ID
        END
*
        CNT.CARD += 1
*
    REPEAT
*
RETURN
*
*--------------------*
OTHER.BEN.GET.METHOD:
*--------------------*
* Modifed below from FOR...NEXT to LOOP...REPEAT as part of the code review
*
    CUSTOMER.ID = System.getVariable("EXT.SMS.CUSTOMERS")
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN    ;*R22 AUTO CODE CONVERSION.START
        CUSTOMER.ID = ""    ;*R22 AUTO CODE CONVERSION
    END    ;*R22 AUTO CODE CONVERSION.END
    APAP.BEN.ID = CUSTOMER.ID : "-" : "OWN"
*
    CALL F.READ(FN.CUS.BEN,APAP.BEN.ID,R.BEN.REC,F.CUS.BEN,BEN.ERR)
    IF R.BEN.REC THEN
        TOT.BEN.REC    = DCOUNT(R.BEN.REC,@FM)
        SUNNEL.CARD.NO = R.NEW(FT.LOCAL.REF)<1,CARD.NUM.POS>
*
        CNT.BEN = 1
        LOOP
            Y.METHOD.NAME = 'BUSCAR_TARJETA_CUENTA.FT'
            BEN.CUST.ID   = R.BEN.REC<CNT.BEN>
        WHILE (CNT.BEN LE TOT.BEN.REC)
*
            IF SUNNEL.CARD.NO[13,4] EQ BEN.CUST.ID[13,4] THEN
                C$SPARE(100) = BEN.CUST.ID
            END
*
            CNT.BEN += 1
*
        REPEAT
*
    END
*
RETURN
*
*-----------*
GET.FLD.VAL:
*-----------*
*
    IF Y.FIELD EQ 'COMI' THEN
        Y.FIELD.VAL = COMI
    END
    ELSE
        Y.FIELD.VAL = Y.FIELD
    END
*
RETURN
*
*-----------*
GET.RTN.NAME:
*-----------*
*
    LOCATE Y.FIELD IN Y.RSM.FIELD SETTING ROUT.POS THEN
        ROUT.NAME = Y.ROUT.FIELD<ROUT.POS>
        Y.TEMP=Y.ARRAY<Y.CNT>
        CALL @ROUT.NAME(Y.TEMP,Y.OUT.DATA)
        Y.ARRAY<Y.CNT>=Y.OUT.DATA
    END
*
RETURN
*
*------------------*
GET.OWN.STO.METHOD:
*------------------*
* Modifed below from FOR...NEXT to LOOP...REPEAT as part of the code review
*
    SUNNEL.CUS.CARD.LIST = System.getVariable("CURRENT.CARD.LIST.CUS")
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN    ;*R22 AUTO CODE CONVERSION.START
        SUNNEL.CUS.CARD.LIST = ""    ;*R22 AUTO CODE CONVERSION
    END   ;*R22 AUTO CODE CONVERSION.END
    Y.METHOD.NAME        = "BUSCAR_TARJETA_CUENTA.STO"
    SUNNEL.CARD.NO       = R.NEW(STO.FT.LOC.REF.DATA)<1,1>
    TOT.CARD.CUSTOMER    = DCOUNT(SUNNEL.CUS.CARD.LIST,'*')
*
    CNT.CARD = 1
    LOOP
        CARD.CUST.ID = FIELD(SUNNEL.CUS.CARD.LIST,'*',CNT.CARD)
    WHILE (CNT.CARD LE TOT.CARD.CUSTOMER)
*
        IF SUNNEL.CARD.NO[13,4] EQ CARD.CUST.ID[13,4] THEN
            C$SPARE(100) = CARD.CUST.ID
        END
*
        CNT.CARD += 1
*
    REPEAT
*
RETURN
*
*---------------*
VAL.BEN.DETAILS:
*--------------*
*
    Y.METHOD.NAME  = 'BUSCAR_TARJETA_CUENTA.VAL'
    SUNNEL.CARD.NO = R.NEW(ARC.BEN.BEN.ACCT.NO)
    BEN.CEDULA.NO  = R.NEW(ARC.BEN.LOCAL.REF)<1,CEDULA.NO.POS>
    C$SPARE(100)   = SUNNEL.CARD.NO
*
RETURN
*PACS00063129-E
END
*---------------------------------------------*END OF SUBROUTINE*--------------------------------------
