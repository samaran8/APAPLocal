* Item ID        : BIT007374
*-------------------------------------------------------------------------------------
* Description :
* ------------
* This program allow verify the telephone number taking some condictions
*-------------------------------------------------------------------------------------
* Modification History :
* ----------------------
* Date           Author            Modification Description
* -------------  -----------       ---------------------------
* 2017/12/11     Raquel P.S.         Initial development
*-------------------------------------------------------------------------------------
* Content summary :
* -----------------
* Versions : FUNDS.TRANSFER,L.APAP.PRESTAC.LAB
* EB record      : LAPAP.RTN.ABORT.TXN
* PGM record     : LAPAP.RTN.ABORT.TXN
* Routines       : LAPAP.RTN.ABORT.TXN
*-------------------------------------------------------------------------------------


SUBROUTINE LAPAP.RTN.ABORT.TXN

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.TELLER
    $INSERT I_GTS.COMMON
    $INSERT I_F.FUNDS.TRANSFER

    FN.FUNDS.TRANSFER = "FBNK.FUNDS.TRANSFER"
    F.FUNDS.TRANSFER = ""

    CALL OPF(FN.FUNDS.TRANSFER,F.FUNDS.TRANSFER)
    POS.DEBIT.AMOUNT=COMI


* Conseguir valor local del campo Cuenta de Credito desde la version

    POS.CREDIT.ACT.NO= R.NEW(FT.CREDIT.ACCT.NO)


* Comando de seleccion en base a valores enviados desde el archivo de pago

    SEL.CMD = "SELECT " :FN.FUNDS.TRANSFER: " WITH TRANSACTION.TYPE EQ ACN1 AND DEBIT.AMOUNT EQ " :POS.DEBIT.AMOUNT : " AND CREDIT.ACCT.NO EQ ": POS.CREDIT.ACT.NO
    CALL EB.READLIST(SEL.CMD, Y.FT.ID,"", NO.OF.REC, SEL.ERR)
    Y.VALUE=1


    IF NO.OF.REC GE Y.VALUE
    THEN
        ETEXT='OPERACION ABORTADA; TXN EXISTE CON IGUAL MONTO Y CUENTA.'
        CALL STORE.END.ERROR


***************************************************************************
*      ESCRIBIENDO ARCHIVO DE TEXTO CON LA TRANSACCION FALLIDA      *
***************************************************************************


*        Y.DIR.NAME = "../interface/FLAT.INTERFACE/TRANSPRESTALAB"
*        Y.FILE.NAME = "DETALLETxnABORTADA":DATE():".":OCONV(TIME(),"MTS")
*
*        OPENSEQ Y.DIR.NAME,Y.FILE.NAME TO FV.PTR ELSE
*            CREATE FV.PTR ELSE
*                CRT "CANNOT OPEN DIR ": Y.DIR.NAME
*                STOP
*            END
*        END

*        Y.DETAIL= "DETALLES TRANSACCION EXISTENTE: ":Y.FT.ID: "NO.OF.REC: ":Y.FT.ID: "SEL.CMD: ":SEL.CMD
*        Y.FINAL = "La transaccion de Monto; ":POS.DEBIT.AMOUNT : " y numero de contrato; ":POS.CREDIT.ACT.NO : " presenta una transaccion duplicada tipo ACN1 (" :Y.FT.ID: ") OPERACION ABORTADA!"
*        WRITESEQ Y.FINAL TO FV.PTR
*        ELSE
*            CRT "UNABLE TO WRITE TO FILE"
*        END
*
*        RETURN
    END


END
