*-----------------------------------------------------------------------------
* <Rating>18</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.PRE.PAYOFF.CHARGE(CHG.PROP,R.CHG.RECORD,BASE.AMT,CHARGE.AMOUNT)
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.AA.ARRANGEMENT
    $INCLUDE T24.BP I_AA.LOCAL.COMMON
    $INSERT T24.BP I_F.AA.ARRANGEMENT.ACTIVITY
    $INSERT T24.BP I_F.AA.ACTIVITY.CHARGES
    $INSERT T24.BP I_F.ACCT.ACTIVITY
    $INSERT T24.BP I_F.AA.CHARGE
    $INSERT T24.BP I_F.AA.PAYMENT.SCHEDULE
    $INSERT T24.BP I_F.EB.CONTRACT.BALANCES
    $INCLUDE BP I_F.LAPAP.CARGO.EXTRAC
    $INCLUDE BP I_F.LAPAP.CARGO.EXT.SECT
    $INSERT T24.BP I_F.AA.TERM.AMOUNT
    $INSERT T24.BP I_F.CUSTOMER
    $INSERT T24.BP I_F.DATES

    GOSUB MAIN.PROCES
    RETURN
MAIN.PROCES:
    GOSUB LOAD.APLICATIONS
    Y.AA.ID = c_aalocArrId

    Y.TODAY = TODAY
    CHARGE.AMOUNT = 0
    GOSUB GET.TIEMPO.TRANSCURRIDO
    GOSUB GET.CAPITAL.PENDIENTE
    GOSUB READ.AA.ARRANGEMENT
    Y.DIAS = 'C'
    CALL CDD('',Y.START.DATE, Y.TODAY, Y.DIAS)
    GOSUB VALIDATE.SEGMENTO
    GOSUB GET.PAGO.PAYOFF.PARAMETRO

    CHARGE.AMOUNT = ABS(Y.RESUL)

    RETURN
GET.TIEMPO.TRANSCURRIDO:
    PROP.CLASS = 'TERM.AMOUNT';    PROP.NAME  = ''; returnConditions = ''; RET.ERR = ''
    CALL AA.GET.ARRANGEMENT.CONDITIONS(Y.AA.ID,PROP.CLASS,PROP.NAME,'','',returnConditions,ERR.COND)
    R.AA.TERM.AMOUNT = RAISE(returnConditions)
    Y.TIEMPO.PRESTAMO =  R.AA.TERM.AMOUNT<AA.AMT.TERM>
    BEGIN CASE
    CASE (INDEX(Y.TIEMPO.PRESTAMO,'D',1)GT 0 )
        Y.TIEMPO.PRESTAMO = CHANGE(Y.TIEMPO.PRESTAMO,'D',FM)
        Y.TIEMPO.PRESTAMO = Y.TIEMPO.PRESTAMO<1>
    CASE (INDEX(Y.TIEMPO.PRESTAMO,'M',1)GT 0 )
        Y.TIEMPO.PRESTAMO = CHANGE(Y.TIEMPO.PRESTAMO,'M',FM)
        Y.DIAS.DIFF = DIV(365,12)
        Y.TIEMPO.PRESTAMO = Y.TIEMPO.PRESTAMO<1> * Y.DIAS.DIFF
    CASE (INDEX(Y.TIEMPO.PRESTAMO,'Y',1)GT 0 )
        Y.TIEMPO.PRESTAMO = CHANGE(Y.TIEMPO.PRESTAMO,'Y',FM)
        Y.TIEMPO.PRESTAMO = Y.TIEMPO.PRESTAMO<1> * 365
    END CASE
    RETURN
READ.AA.ARRANGEMENT:
    R.AA.ARRANGEMENT = ''; ERR.AA.ARRANGEMENT = ''
    CALL F.READ (FN.AA.ARRANGEMENT,Y.AA.ID,R.AA.ARRANGEMENT,F.AA.ARRANGEMENT,ERR.AA.ARRANGEMENT)
    Y.START.DATE =  R.AA.ARRANGEMENT<AA.ARR.START.DATE>
    Y.PRODUCT = R.AA.ARRANGEMENT<AA.ARR.PRODUCT>
    Y.CUSTOMER.ID = R.AA.ARRANGEMENT<AA.ARR.CUSTOMER>
    RETURN
LOAD.APLICATIONS:
    FN.AA.ARRANGEMENT = 'F.AA.ARRANGEMENT' ; F.AA.ARRANGEMENT = ''
    CALL OPF (FN.AA.ARRANGEMENT,F.AA.ARRANGEMENT)
    FN.LAPAP.CARGO.EXTRAC = 'F.ST.LAPAP.CARGO.EXTRAC' ; F.LAPAP.CARGO.EXTRAC = ''
    CALL OPF (FN.LAPAP.CARGO.EXTRAC,F.LAPAP.CARGO.EXTRAC)

    FN.CUSTOMER = 'F.CUSTOMER'; F.CUSTOMER = ''
    CALL OPF (FN.CUSTOMER,F.CUSTOMER)

*----------------Tabla con parametrizacion por segmentos-------------------------------
    FN.LAPAP.CARGO.EXT.SECT = 'F.ST.LAPAP.CARGO.EXT.SECT' ; F.LAPAP.CARGO.EXT.SECT = ''
    CALL OPF (FN.LAPAP.CARGO.EXT.SECT,F.LAPAP.CARGO.EXT.SECT)
*--------------------------------------------------------------------------------------
    RETURN

GET.CAPITAL.PENDIENTE:
    CALL L.APAP.RETURN.BANLACE.CANCELACION(Y.AA.ID,OUT.RECORD)
    Y.CAPITAL.PENDIENTE = FIELD(OUT.RECORD,"*",2)
    RETURN

VALIDATE.SEGMENTO:

    Y.RESUL = 0
    R.CUSTOMER =''; CUSTOMER.ERR ='';
    CALL F.READ(FN.CUSTOMER,Y.CUSTOMER.ID,R.CUSTOMER,F.CUSTOMER,CUSTOMER.ERR)
    CUS.SEG.POS= '';
    CALL GET.LOC.REF("CUSTOMER","L.CU.SEGMENTO",CUS.SEG.POS)
    Y.SEGMENTO.ID              = R.CUSTOMER<EB.CUS.LOCAL.REF,CUS.SEG.POS>
*----------------Busca si el segmento esta parametrizado-------------------------------
    R.CHARGE.PARAM =''; CHARGE.PARAM.ERR = ''
    CALL F.READ(FN.LAPAP.CARGO.EXT.SECT,Y.SEGMENTO.ID,R.CHARGE.PARAM,F.LAPAP.CARGO.EXT.SECT,CHARGE.PARAM.ERR)
    IF NOT (R.CHARGE.PARAM) THEN
        RETURN
    END

    CALL LAPAP.CHARGE.PAYOFF.SEG.RT(Y.AA.ID,OUT.RECORD1)
    Y.RESUL = OUT.RECORD1
    RETURN

GET.PAGO.PAYOFF.PARAMETRO:

    IF Y.RESUL NE 0 THEN
        RETURN
    END
    R.LAPAP.PAGO.EXTRA = '' ; ERR.PAGO.EXTRA = ''
    CALL F.READ (FN.LAPAP.CARGO.EXTRAC,'SYSTEM',R.LAPAP.CARGO.EXTRAC,F.LAPAP.CARGO.EXTRAC,ERR.PAGO.EXTRA)
    Y.PRODUCTO = R.LAPAP.CARGO.EXTRAC<ST.LAP56.PRODUCTO>
    Y.COMISION = R.LAPAP.CARGO.EXTRAC<ST.LAP56.COMISION>
    Y.BASE.CALCULO = R.LAPAP.CARGO.EXTRAC<ST.LAP56.BASE.CALCULO>
    Y.TIEMPO.TRAN.INI = R.LAPAP.CARGO.EXTRAC<ST.LAP56.TIEMPO.TRAN.INI>
    Y.TIEMPO.TRAN.FIN = R.LAPAP.CARGO.EXTRAC<ST.LAP56.TIEMPO.TRAN.FIN>
    Y.SALDO.BASE = R.LAPAP.CARGO.EXTRAC<ST.LAP56.SALDO.BASE>
    Y.TASA.PORENCIMA = R.LAPAP.CARGO.EXTRAC<ST.LAP56.TASA.PORENCIMA>
    Y.PLAZO.PRESTAMO = R.LAPAP.CARGO.EXTRAC<ST.LAP56.PLAZO.PRESTAMO>
    LOCATE Y.PRODUCT IN Y.PRODUCTO<1,1> SETTING PR.POSN.2 THEN
        Y.PLAZO.PRESTAMO = Y.PLAZO.PRESTAMO<PR.POSN.2,1> * 365
    END
    IF DROUND(Y.TIEMPO.PRESTAMO) GT DROUND(Y.PLAZO.PRESTAMO) THEN
        Y.CNT = DCOUNT(Y.PRODUCTO,VM)
        FOR I = 1 TO Y.CNT
            IF Y.PRODUCT EQ Y.PRODUCTO<1,I> THEN
                IF Y.COMISION<1,I> EQ  'CANCEL.ANTI' THEN
                    Y.FECHA.INC = Y.TIEMPO.TRAN.INI<1,I>
                    Y.FECHA.FIN = Y.TIEMPO.TRAN.FIN<1,I>
                    Y.SALDO.BASE1 =  Y.SALDO.BASE<1,I>
                    Y.TASA.PORENCIMA1 = Y.TASA.PORENCIMA<1,I>
                    GOSUB GET.PROCESS.CAL
                    IF Y.RESUL NE 0 THEN
                        RETURN
                    END
                END
            END
        NEXT I
    END
    RETURN
GET.PROCESS.CAL:

    Y.TASA = 0; Y.FECHA = ''
    CHANGE SM TO FM IN Y.FECHA.INC
    CHANGE VM TO FM IN Y.FECHA.INC
    CHANGE SM TO FM IN Y.FECHA.FIN
    CHANGE SM TO FM IN Y.FECHA.FIN
    CHANGE SM TO FM IN Y.TASA.PORENCIMA1
    CHANGE VM TO FM IN Y.TASA.PORENCIMA1
    CHANGE SM TO FM IN Y.SALDO.BASE1
    CHANGE VM TO FM IN Y.SALDO.BASE1
    Y.CNT1 = DCOUNT(Y.FECHA.INC,FM)
    FOR K = 1 TO Y.CNT1
        Y.BALANCE.IN = Y.FECHA.INC<K> * DIV(365,12)
        Y.BALANCE.FIN = Y.FECHA.FIN<K> * DIV(365,12)
        Y.BALANCE.IN = DROUND(Y.BALANCE.IN)
        Y.BALANCE.FIN = DROUND(Y.BALANCE.FIN)
        IF Y.DIAS GE Y.BALANCE.IN AND Y.DIAS LE Y.BALANCE.FIN THEN
            Y.TASA =  DIV(Y.TASA.PORENCIMA1<K>,100)
            Y.PORCIENTO.CAPITAL = DIV(Y.SALDO.BASE1<K>,100) * Y.CAPITAL.PENDIENTE
            Y.RESUL = Y.PORCIENTO.CAPITAL * Y.TASA
            RETURN
        END
    NEXT K
    RETURN
END
