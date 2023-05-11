$PACKAGE APAP.REDOENQ
SUBROUTINE REDO.E.LIST.CLIE.COLLA(AC.DETAILS)
*------------------------------------------------------------------------
* Modification History :
*------------------------------------------------------------------------
*  DATE             WHO                   REFERENCE                  
* 11-APRIL-2023      Conversion Tool       R22 Auto Conversion  - Added IF E EQ "EB-UNKNOWN.VARIABLE" THEN , VM to @VM , FM to @FM , = to EQ and INCLUDE to INSERT
* 11-APRIL-2023      Harsha                R22 Manual Conversion - No changes                             
*------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.COLLATERAL
    $INSERT I_F.AZ.ACCOUNT
    $INSERT I_System
*============================================
    GOSUB INICIALIZA
    GOSUB OPEN.FILES
    GOSUB PROCESS

RETURN

* ===========
INICIALIZA:
* ===========
    FN.ACCOUNT   = 'F.ACCOUNT'
    F.ACCOUNT    = ''
    R.ACCOUNT    = ''

    FN.AZ      = 'F.AZ.ACCOUNT'
    F.AZ       = ''
    R.AZ       = ''

    Y.STATUS2.FLAG = 0

****LOCAL FIELDS FOR AZ.ACCOUNT
    WCAMPO.AZ<1> = "L.AC.STATUS1"       ;*Estado1 AZ.ACCOUNT
    WCAMPO.AZ<2> = "L.AC.STATUS2"       ;*Estado2 AZ.ACCOUNT
    WCAMPO.AZ = CHANGE(WCAMPO.AZ,@FM,@VM)

****LOCAL FIELDS FOR ACCOUNT
    WCAMPO.AC<1> = "L.AC.STATUS1"       ;*Estado1 ACCOUNT
    WCAMPO.AC<2> = "L.AC.STATUS2"       ;*Estado2 ACCOUNT
    WCAMPO.AC<3> = "L.AC.AV.BAL"
    WCAMPO.AC = CHANGE(WCAMPO.AC,@FM,@VM)

*Get the position for all fields
    LOC.REF.APP = 'AZ.ACCOUNT': @FM :'ACCOUNT'
    WCAMPOS = WCAMPO.AZ: @FM :WCAMPO.AC

    CALL MULTI.GET.LOC.REF(LOC.REF.APP,WCAMPOS,YPOS)

    WPOS.EST1      = YPOS<1,1>
    WPOS.EST2      = YPOS<1,2>
    WPOS.EST1.AC   = YPOS<2,1>
    WPOS.EST2.AC   = YPOS<2,2>
    Y.AC.AV.BAL.POS= YPOS<2,3>

    LOCATE "CUSTOMER.NO" IN D.FIELDS<1> SETTING PRO.POS THEN
        VAR.CUSTOMER = D.RANGE.AND.VALUE<PRO.POS>
    END


RETURN

*=======================
OPEN.FILES:
*=======================
    CALL OPF(FN.ACCOUNT,F.ACCOUNT)
    CALL OPF(FN.AZ,F.AZ)
RETURN

*=======================
PROCESS:
*=======================

    VAR.TIPO = System.getVariable("CURRENT.COL.TP")
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN    ;*R22 Auto Conversion  - Added IF E EQ "EB-UNKNOWN.VARIABLE" THEN
        VAR.TIPO = ""
    END
;*R.NEW(COLL.COLLATERAL.TYPE)
    VAR.CURRENCY = System.getVariable("CURRENT.COL.CUR")
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN     ;*R22 Auto Conversion  - Added IF E EQ "EB-UNKNOWN.VARIABLE" THEN
        VAR.CURRENCY = ""
    END
; *R.NEW(COLL.CURRENCY)

*---------FOR FIX DEPOSITS---------
    IF (VAR.TIPO EQ '152') THEN
*        VAR.CATEGORY = " AND CATEGORY GE '6600' AND CATEGORY LE '6615' AND CATEGORY NE 6602 6605 6607 6609 6611 6613"
*             Para Depósitos: Rango del 6,600 al 6,699 inclusive (estos son aplicables para la Clase de Garantía 152 “Instrumentos de Depósitos a Plazo”). - APAP_Ivelisse Batista 17Mar2017
        VAR.CATEGORY = " AND CATEGORY GE '6600' AND CATEGORY LE '6699'"
        SELECT.STATEMENT = 'SELECT ':FN.AZ:' WITH CUSTOMER EQ ':VAR.CUSTOMER: VAR.CATEGORY
        LOCK.LIST = ''
        LIST.NAME = ''
        SELECTED = ''
        SYSTEM.RETURN.CODE = ''
        Y.ID.AA.PRD = ''
        CALL EB.READLIST(SELECT.STATEMENT,LOCK.LIST,LIST.NAME,SELECTED,SYSTEM.RETURN.CODE)

*Get the data thar view in the list
        LOOP
            REMOVE Y.ID.AA.PRD FROM LOCK.LIST SETTING POS
        WHILE Y.ID.AA.PRD:POS

            CALL F.READ(FN.AZ, Y.ID.AA.PRD, R.AZ,F.AZ, Y.ERR)
            CALL F.READ(FN.ACCOUNT, Y.ID.AA.PRD, R.ACCOUNT,F.ACCOUNT, Y.ERR)

            IF Y.ERR NE '' THEN
                P.MESSAGE = 'ST-REDO.COLLA.ERR.LEE.LOCK'
                RETURN
            END
*GET THE AMONUT AND LOCKED
*            VAR.BLOQUEO = R.ACCOUNT<AC.LOCKED.AMOUNT>
            VAR.MONTO   = R.ACCOUNT<AC.LOCAL.REF,Y.AC.AV.BAL.POS>
            VAR.SALDO   = VAR.MONTO ;* - VAR.BLOQUEO

            Y.AZ.STATUS1 = R.AZ<AZ.LOCAL.REF,WPOS.EST1>
            Y.AZ.STATUS2 = R.AZ<AZ.LOCAL.REF,WPOS.EST2>

            Y.STATUS2.FLAG = 0

            LOCATE "GARNISHMENT" IN Y.AZ.STATUS2<1> SETTING Y.STATUS2.POS THEN
                Y.STATUS2.FLAG=1
            END
            LOCATE "DECEASED" IN Y.AZ.STATUS2<1> SETTING Y.STATUS2.POS THEN
                Y.STATUS2.FLAG=1
            END
            LOCATE "GUARANTEE.STATUS" IN Y.AZ.STATUS2<1> SETTING Y.STATUS2.POS THEN
                Y.STATUS2.FLAG=1
            END


*    IF (VAR.SALDO GT Y.AMT.REQ) AND (Y.STATUS2.FLAG = 0)   THEN
            IF Y.AZ.STATUS1 EQ "ACTIVE" AND Y.STATUS2.FLAG EQ 0 AND VAR.SALDO GT 0 THEN
                AC.DETAILS<-1> =  Y.ID.AA.PRD:"*":VAR.CUSTOMER:"*":R.ACCOUNT<AC.ACCOUNT.TITLE.1>:"*":VAR.SALDO
            END

        REPEAT
    END   ;*152


*---------FOR DEPOSITS---------
    IF (VAR.TIPO EQ '151') OR (VAR.TIPO EQ '153') THEN

***SET FILTER FOR CATEGORYS***
        Y.CATEGORY = ''
        GOSUB GET.CATEGORY

**GET INFORMATION FOR ACCOUNTS**
        SELECT.STATEMENT = 'SELECT ':FN.ACCOUNT:' WITH CUSTOMER EQ ':VAR.CUSTOMER: Y.CATEGORY
        LOCK.LIST = ''
        LIST.NAME = ''
        SELECTED = ''
        SYSTEM.RETURN.CODE = ''
        Y.ID.AA.PRD = ''
        CALL EB.READLIST(SELECT.STATEMENT,LOCK.LIST,LIST.NAME,SELECTED,SYSTEM.RETURN.CODE)

*Get the data thar view in the list
        LOOP
            REMOVE Y.ID.AA.PRD FROM LOCK.LIST SETTING POS
        WHILE Y.ID.AA.PRD:POS



*READ THE DATA ACCOUNT TO RETURNED BY THE ENQUIRY
            CALL F.READ(FN.ACCOUNT, Y.ID.AA.PRD, R.ACCOUNT,F.ACCOUNT, Y.ERR)
            IF Y.ERR NE '' THEN
                P.MESSAGE = 'ST-REDO.COLLA.ERR.LEE.LOCK'
                RETURN
            END
*GET INFORMATION FOR ACCOUND AND LOCKED AMOUNT
*            VAR.BLOQUEO = R.ACCOUNT<AC.LOCKED.AMOUNT>
            VAR.MONTO   = R.ACCOUNT<AC.LOCAL.REF,Y.AC.AV.BAL.POS>
            VAR.SALDO   = VAR.MONTO  ;* - VAR.BLOQUEO

            Y.AZ.STATUS1 =  R.ACCOUNT<AC.LOCAL.REF,WPOS.EST1.AC>
            Y.AZ.STATUS2 =  R.ACCOUNT<AC.LOCAL.REF,WPOS.EST2.AC>

            Y.STATUS2.FLAG = 0

            LOCATE "GARNISHMENT" IN Y.AZ.STATUS2<1> SETTING Y.STATUS2.POS THEN
                Y.STATUS2.FLAG=1
            END
            LOCATE "DECEASED" IN Y.AZ.STATUS2<1> SETTING Y.STATUS2.POS THEN
                Y.STATUS2.FLAG=1
            END
            LOCATE "GUARANTEE.STATUS" IN Y.AZ.STATUS2<1> SETTING Y.STATUS2.POS THEN
                Y.STATUS2.FLAG=1
            END


*    IF (VAR.SALDO GT Y.AMT.REQ) AND (Y.STATUS2.FLAG = 0)   THEN
            IF Y.AZ.STATUS1 EQ "ACTIVE" AND Y.STATUS2.FLAG EQ 0 AND VAR.SALDO GT 0 THEN
                AC.DETAILS<-1> =  Y.ID.AA.PRD:"*":VAR.CUSTOMER:"*":R.ACCOUNT<AC.ACCOUNT.TITLE.1>:"*":VAR.SALDO
            END

        REPEAT
    END   ;*151 AND 153

RETURN

* ===========
GET.CATEGORY:
* ===========

* Para Cuentas de Ahorros: Rango del 6,000 al 6,599 (excluyendo las Categorías del 6011 al 6020 inclusive, que corresponden a Intereses Reinvertidos),
* estas son aplicables para la Clase de garantía 151 - APAP_Ivelisse Batista 17Mar2017

    IF VAR.TIPO EQ 151 THEN
*ACC CATEGORIES 6001 6006 6009 6010 6501 6502 6503 6505 6507 6508  <6002 6008> SON PARA CUENTAS EN USD
*Y.CATEGORY=" AND CATEGORY EQ 6001 6006 6009 6010 6501 6502 6503 6505 6507 6508 6002 6008"
        Y.CATEGORY="  AND (CATEGORY GE '6000' AND CATEGORY LE '6010') AND (CATEGORY GE '6021' AND CATEGORY LE '6599')"
    END

    IF VAR.TIPO EQ 153 THEN
*AND CATEGORY GE 1001 AND CATEGORY LE 1999
        Y.CATEGORY=" AND CATEGORY GE '1001' AND CATEGORY LE '1999'"
    END

    IF VAR.CURRENCY EQ "USD" AND VAR.TIPO  EQ 151 THEN
        Y.CATEGORY=" AND CATEGORY EQ 6002 6008"
    END
RETURN

*--------------------------------------------------------------------------------
END
