* @ValidationCode : Mjo3MzgyOTAzOTY6Q3AxMjUyOjE2ODA2NzE1NjM3NjU6SVRTUzotMTotMToxMTc6MTpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 05 Apr 2023 10:42:43
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : 117
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOFCFI
SUBROUTINE REDO.FC.ENQ.BUILD.INS.TYPE.DI(DATA.OUT)
*
* ====================================================================================
*
*
* ====================================================================================
*
* Subroutine Type :Routine to  Nofile Enquiry
* Attached to     :REDO.FC.INS.TYPE.DE ENQUIRY
* Attached as     :Build routine attach to INSMNT.ISS.ENT.DI field in RCA.PRODUCT.CATALOG-PRODUCTS enquiry
* Primary Purpose :Put de Description of the instrument of REDO.INSTRUMENT.TYPE in O.DATA to enquiry
*
*
* Incoming:
* ---------
*
*
*
* Outgoing:
* ---------
* ---------
*
*
*-----------------------------------------------------------------------------------
* Modification History:
*
* Development for : Asociacion Popular de Ahorros y Prestamos
* Development by  : Bryan Torres (btorresalbornoz@temenos.com) - TAM Latin America
* Date            : Agosto 25 2011

* Revision History:
* -----------------
* Version          Date          Name              Description
* -------          ----          ----              ------------
* 1.1              2012-01-09    Meza William     Code Review
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*04-04-2023            CONVERSION TOOL                AUTO R22 CODE CONVERSION           VM TO @VM ,FM TO @FM SM TO @SM EXTRA TWO IF STATEMENT ADDED
*04-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*=======================================================================

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.REDO.CREATE.ARRANGEMENT

    $INSERT I_F.AZ.ACCOUNT
    $INSERT I_F.ACCOUNT
    $INSERT I_System

*
*************************************************************************

    GOSUB INITIALISE
    GOSUB OPEN.FILES
    GOSUB CHECK.PRELIM.CONDITIONS
    IF PROCESS.GOAHEAD THEN
        GOSUB PROCESS
    END

*
RETURN
*
* ======
PROCESS:
* ======


    Y.COUNT.COLL.TYPE=DCOUNT(Y.COLLATERAL.TYPE.DI,@VM)
    FOR Y.I = 1 TO Y.COUNT.COLL.TYPE

        IF Y.COLLATERAL.TYPE.DI<1,Y.I> EQ 152 THEN          ;* INSTRUMENTO A PLAZO
            GOSUB INSTRUMENTO.PLAZO
        END

        IF Y.COLLATERAL.TYPE.DI<1,Y.I> EQ 151 OR  Y.COLLATERAL.TYPE.DI<1,Y.I> EQ 153 THEN ;* CTA AHORRO
            GOSUB CTA.AHORRO
        END
    NEXT Y.I

RETURN
*
* ======
INSTRUMENTO.PLAZO:
* ======

    Y.CATEGORY=" AND CATEGORY GE '6601' AND CATEGORY LE '6615' AND CATEGORY NE 6602 6605 6607 6609 6611 6613"
    SELECT.STATEMENT = "SELECT ":FN.AZ.ACCOUNT:" WITH CUSTOMER EQ '":Y.ID.TITULA.GARANTIA<1,Y.I>:"' ":Y.CATEGORY:"":Y.STATUS
    Y.REDO.LOAN.LIST = ''
    LIST.NAME = ''
    SELECTED = ''
    SYSTEM.RETURN.CODE = ''
    Y.TYPE.PRODUCT = ''
    CALL EB.READLIST(SELECT.STATEMENT,Y.REDO.LOAN.LIST,LIST.NAME,SELECTED,SYSTEM.RETURN.CODE)
    LOOP
        REMOVE Y.AZ.ID FROM Y.REDO.LOAN.LIST SETTING POS
    WHILE Y.AZ.ID:POS


        CALL CACHE.READ(FN.AZ.ACCOUNT, Y.AZ.ID, R.AZ.ACCOUNT, Y.ERR)
        CALL CACHE.READ(FN.ACCOUNT, Y.AZ.ID, R.ACCOUNT, Y.ERR)
        Y.STATUS2.FLAG=0
        Y.AZ.DESCRIPTION = R.AZ.ACCOUNT<AZ.ALL.IN.ONE.PRODUCT>
        Y.AZ.STATUS1 = R.AZ.ACCOUNT<AZ.LOCAL.REF,Y.AZ.STATUS1.POS>
        Y.AZ.STATUS2 = ''
        Y.AZ.STATUS2 = R.AZ.ACCOUNT<AZ.LOCAL.REF,Y.AZ.STATUS2.POS>
        Y.AZ.MATURITY.DATE = R.AZ.ACCOUNT<AZ.MATURITY.DATE>
        Y.AC.DISPONIBLE = R.ACCOUNT<AC.LOCAL.REF,Y.AC.AV.BAL.POS>
        IF Y.AZ.STATUS1 EQ "ABANDONED" OR Y.AZ.MATURITY.DATE LT TODAY THEN
            Y.AZ.ID=""
            Y.AZ.DESCRIPTION = ""
        END
        IF Y.AZ.STATUS2 THEN
            LOCATE "GARNISHMENT" IN Y.AZ.STATUS2 SETTING Y.STATUS2.POS THEN
                Y.STATUS2.FLAG=1
            END
            LOCATE "DECEASED" IN Y.AZ.STATUS2 SETTING Y.STATUS2.POS THEN
                Y.STATUS2.FLAG=1
            END
        END
        IF Y.AZ.ID NE "" AND Y.AZ.DESCRIPTION NE "" AND Y.STATUS2.FLAG EQ 0 AND Y.AC.DISPONIBLE GT 0 THEN
            DATA.OUT<-1>=Y.AZ.ID:"*":Y.AZ.DESCRIPTION:"*":Y.AC.DISPONIBLE
        END

    REPEAT

RETURN

*
* ======
CTA.AHORRO:
* ======
    Y.CATEGORY = ""
    GOSUB GET.CATEGORY

    SELECT.STATEMENT = "SELECT ":FN.ACCOUNT:" WITH CUSTOMER EQ '":Y.ID.TITULA.GARANTIA<1,Y.I>:"' ": Y.CATEGORY:"":Y.STATUS

    Y.REDO.LOAN.LIST = ''
    LIST.NAME = ''
    SELECTED = ''
    SYSTEM.RETURN.CODE = ''
    Y.TYPE.PRODUCT = ''
    CALL EB.READLIST(SELECT.STATEMENT,Y.REDO.LOAN.LIST,LIST.NAME,SELECTED,SYSTEM.RETURN.CODE)
    LOOP
        REMOVE Y.AC.ID FROM Y.REDO.LOAN.LIST SETTING POS
    WHILE Y.AC.ID:POS


        CALL CACHE.READ(FN.ACCOUNT, Y.AC.ID, R.ACCOUNT, Y.ERR)
        Y.AC.DESCRIPTION = R.ACCOUNT<AC.ACCOUNT.TITLE.1>
        Y.AC.STATUS1 = R.ACCOUNT<AC.LOCAL.REF,Y.AC.STATUS1.POS>
        Y.AC.STATUS2 = R.ACCOUNT<AC.LOCAL.REF,Y.AC.STATUS2.POS>
        Y.AC.DISPONIBLE = R.ACCOUNT<AC.LOCAL.REF,Y.AC.AV.BAL.POS>

*        LOCATE "ABANDONED" IN Y.AC.STATUS1 SETTING Y.AC.POS ELSE
        IF Y.AC.STATUS1 EQ "ABANDONED" THEN
            Y.AC.ID = ""
            Y.AC.DESCRIPTION =""

        END
        LOCATE "DECEASED" IN Y.AC.STATUS2 SETTING Y.STATUS2.POS ELSE
            LOCATE "GARNISHMENT" IN Y.AC.STATUS2 SETTING Y.STATUS2.POS ELSE
                IF Y.AC.ID NE "" AND  Y.AC.DESCRIPTION NE "" AND Y.AC.DISPONIBLE GT 0 THEN
                    DATA.OUT<-1>=Y.AC.ID:"*":Y.AC.DESCRIPTION:"*":Y.AC.DISPONIBLE
                END
            END
*           END
        END

    REPEAT

RETURN
*
* =========
OPEN.FILES:
* =========
*



    CALL OPF(FN.ACCOUNT, F.ACCOUNT)

    CALL OPF(FN.AZ.ACCOUNT, F.AZ.ACCOUNT)
RETURN
*
* =========
INITIALISE:
* =========
*
    LOOP.CNT = 1
    MAX.LOOPS = 1
    PROCESS.GOAHEAD = 1

    FN.REDO.ISSUE.ENTITY="F.REDO.ISSUE.ENTITY"
    F.REDO.ISSUE.ENTITY=""
    FN.REDO.INSTRUMENT.TYPE="F.REDO.INSTRUMENT.TYPE"
    F.REDO.INSTRUMENT.TYPE=""

    Y.ID.TITULA.GARANTIA = System.getVariable("CURRENT.Y.ID.TITULAR.GAR")
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN ;* AUTO R22 CODE CONVERSION
        Y.ID.TITULA.GARANTIA = ""
    END
*  Tus Start
    IF E<1,1> EQ 'EB-UNKNOWN.VARIABLE' THEN
        Y.ID.TITULA.GARANTIA  = ''
    END   ;* Tus End



    FN.AZ.ACCOUNT = 'F.AZ.ACCOUNT'
    F.AZ.ACCOUNT = ''
    FN.ACCOUNT = 'F.ACCOUNT'
    F.ACCOUNT = ''



    LOC.REF.APPLICATION.ACC = 'AZ.ACCOUNT'
    LOC.REF.FIELDS<1> = 'L.AC.STATUS1'
    LOC.REF.FIELDS<2> = 'L.AC.STATUS2'
    LOC.REF.POS = ''
    LOC.REF.FIELDS.ACC = CHANGE(LOC.REF.FIELDS,@FM,@VM)
* CALL MULTI.GET.LOC.REF(LOC.REF.APPLICATION,LOC.REF.FIELDS,LOC.REF.POS)
* Y.AZ.STATUS1.POS = LOC.REF.POS<1,1>
* Y.AZ.STATUS2.POS = LOC.REF.POS<1,2>



    LOC.REF.APPLICATION.TERM = 'AA.PRD.CAT.TERM.AMOUNT'
    LOC.REF.FIELDS.TERM = 'L.AA.RISK.PER'
* LOC.REF.POS.TERM = ''


    Y.VAL.NOMINAL.COLL =''
    Y.VAL.AA.RISK.PER=''
    Y.ARR.AMOUNT=''


    Y.ARR.AMOUNT = System.getVariable("CURRENT.Y.AMOUNT")
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN ;* AUTO R22 CODE CONVERSION
        Y.ARR.AMOUNT = ""
    END
*  Tus Start
    IF E<1,1> EQ 'EB-UNKNOWN.VARIABLE' THEN
        Y.ARR.AMOUNT = ''
    END   ;* Tus End
    Y.CURRENCY   = System.getVariable("CURRENT.Y.CURRENCY")
    IF E EQ "EB-UNKNOWN.VARIABLE" THEN
        Y.CURRENCY = ""
    END
*  Tus Start
    IF E<1,1> EQ 'EB-UNKNOWN.VARIABLE' THEN
        Y.CURRENCY = ''
    END   ;* Tus End



    LOC.REF.APPLICATION.A = 'ACCOUNT'
    LOC.REF.FIELDS.A<1> = 'L.AC.STATUS1'
    LOC.REF.FIELDS.A<2> = 'L.AC.STATUS2'
    LOC.REF.FIELDS.A<3> = 'L.AC.AV.BAL'
    LOC.REF.POS.A = ''
    LOC.REF.FIELDS.A = CHANGE(LOC.REF.FIELDS.A,@FM,@VM)

    LOC.REF.APPLICATION = ''
    LOC.REF.APPLICATION = LOC.REF.APPLICATION.ACC : @FM : LOC.REF.APPLICATION.TERM : @FM : LOC.REF.APPLICATION.A
    LOC.REF.FIELDS = LOC.REF.FIELDS.ACC : @FM : LOC.REF.FIELDS.TERM : @FM : LOC.REF.FIELDS.A
    CALL MULTI.GET.LOC.REF(LOC.REF.APPLICATION, LOC.REF.FIELDS, LOC.REF.POS)
    Y.AZ.STATUS1.POS = LOC.REF.POS<1,1> ;*'L.AC.STATUS1'
    Y.AZ.STATUS2.POS = LOC.REF.POS<1,2> ;*'L.AC.STATUS2'
    TXN.REF.ID.POS = LOC.REF.POS<2,1>   ;*'L.AA.RISK.PER'

* CALL MULTI.GET.LOC.REF(LOC.REF.APPLICATION.A,LOC.REF.FIELDS.A,LOC.REF.POS.A)
    Y.AC.STATUS1.POS = LOC.REF.POS<3,1>
    Y.AC.STATUS2.POS = LOC.REF.POS<3,2>
    Y.AC.AV.BAL.POS  = LOC.REF.POS<3,3>

    DATA.OUT=''

    LOCATE "SEC.CLASSIFY.DI" IN D.FIELDS<1> SETTING PRO.POS THEN
        Y.COLLATERAL.TYPE.DI=D.RANGE.AND.VALUE<PRO.POS>
    END

    Y.CATEGORY=''
    Y.STATUS = ' AND L.AC.STATUS1 EQ ACTIVE AND L.AC.STATUS2 NE DECEASED AND L.AC.STATUS2 NE GARNISHMENT'
RETURN


* ======================
CHECK.PRELIM.CONDITIONS:
* ======================
*

*
RETURN
*

* ===========
GET.CATEGORY:
* ===========

    IF Y.COLLATERAL.TYPE.DI<1,Y.I> EQ 151 THEN
*ACC CATEGORIES 6001 6006 6009 6010 6501 6502 6503 6505 6507 6508  <6002 6008> SON PARA CUENTAS EN USD
        Y.CATEGORY="AND CATEGORY EQ 6001 6006 6009 6010 6501 6502 6503 6505 6507 6508 6002 6008"

    END
    IF Y.COLLATERAL.TYPE.DI<1,Y.I> EQ 153 THEN
*AND CATEGORY GE 1001 AND CATEGORY LE 1999
        Y.CATEGORY="AND CATEGORY GE '1001' AND CATEGORY LE '1999'"
    END
    IF Y.CURRENCY EQ "USD" AND Y.COLLATERAL.TYPE.DI<1,Y.I> EQ 151 THEN
        Y.CATEGORY="AND CATEGORY EQ 6002 6008"
    END

RETURN
*
END
