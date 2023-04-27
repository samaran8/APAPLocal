$PACKAGE APAP.LAPAP
SUBROUTINE L.APAP.NOFILE.DETA.TRAN(Y.ARREGLO)
*-----------------------------------------------------------------------------
* Bank name: APAP
* Decription: Rutina tipo NOFILES enquiry para buscar todas transanción de la tabla FUNDS.TRANSFER
*             realizada por un usuario
* Developed By: APAP
* Date:  18/06/2021
*-----------------------------------------------------------------------------
*------------------------------------------------------------------------
* Modification History :
*------------------------------------------------------------------------
*  DATE             WHO                   REFERENCE                  
* 21-APRIL-2023      Conversion Tool       R22 Auto Conversion - VM to @VM , FM to @FM and SM to @SM and T24.BP is removed from Insert
* 13-APRIL-2023      Harsha                R22 Manual Conversion - No changes                             
*------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.FT.TXN.TYPE.CONDITION
    $INSERT I_ENQUIRY.COMMON

    GOSUB OPEN.TABLA
    GOSUB PROCESS

RETURN

OPEN.TABLA:
    FN.FT = "F.FUNDS.TRANSFER"
    FV.FT = ""
    CALL OPF (FN.FT,FV.FT)

    FN.TXN.TYPE.CONDITION = "F.FT.TXN.TYPE.CONDITION"
    FV.TXN.TYPE.CONDITION = ""
    CALL OPF (FN.TXN.TYPE.CONDITION,FV.TXN.TYPE.CONDITION)

    Y.USUARIO = ""; Y.SUCURSAL = ""
    LOCATE "USUARIO" IN D.FIELDS<1> SETTING PRO.POS THEN
        Y.USUARIO = D.RANGE.AND.VALUE<PRO.POS>
    END
    LOCATE "CO.CODE" IN D.FIELDS<1> SETTING PRO.POS1 THEN
        Y.SUCURSAL = D.RANGE.AND.VALUE<PRO.POS1>
    END
RETURN

PROCESS:

    SEL.CMD = '' ; SEL.LIST = ''; NO.OF.REC = ''; RET.CODE = ''
    Y.DOBLE.COMA = '"'
    Y.COMA.SIMPLE = "'"
*Y.USUARIO = 'A.12843'
*Y.SUCURSAL = 'DO0010001'
    IF Y.USUARIO NE '' AND Y.SUCURSAL EQ '' THEN
        SEL.CMD = "SELECT ":FN.FT:" WITH INPUTTER LIKE ":Y.DOBLE.COMA:"...":Y.COMA.SIMPLE:Y.USUARIO:Y.COMA.SIMPLE:"...":Y.DOBLE.COMA

    END

    IF Y.SUCURSAL EQ '' AND Y.USUARIO EQ '' THEN
        SEL.CMD = "SELECT ":FN.FT
*:" WITH @ID EQ 'FT211139G18R' "

    END

    IF Y.SUCURSAL NE '' AND Y.USUARIO EQ '' THEN
        SEL.CMD = "SSELECT ":FN.FT:" WITH CO.CODE EQ ":Y.SUCURSAL

    END
    IF Y.SUCURSAL NE '' AND Y.USUARIO NE '' THEN

        SEL.CMD = "SELECT ":FN.FT:" WITH CO.CODE EQ ":Y.SUCURSAL:" AND INPUTTER LIKE ":Y.DOBLE.COMA:"...":Y.COMA.SIMPLE:Y.USUARIO:Y.COMA.SIMPLE:"...":Y.DOBLE.COMA
    END

    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,RET.CODE)
    LOOP
        REMOVE Y.FT.ID FROM SEL.LIST SETTING FT.POST
    WHILE Y.FT.ID:FT.POST  DO
        R.FT = ''; ERROR.FT = ''; Y.INPUTTER  = ''; Y.AUTHORISER = '';
        CALL F.READ (FN.FT,Y.FT.ID,R.FT,FV.FT,ERROR.FT)
        Y.INPUTTER =  R.FT<FT.INPUTTER>
        Y.AUTHORISER =  R.FT<FT.AUTHORISER>
        Y.INPUTTER = FIELD(Y.INPUTTER, "_",2)
        Y.AUTHORISER = FIELD(Y.AUTHORISER, "_",2)
*IF Y.USUARIO EQ Y.INPUTTER OR  Y.USUARIO EQ Y.AUTHORISER THEN
        GOSUB GET.FT.DETALLES
*END
    REPEAT
RETURN

GET.FT.DETALLES:
    R.TXN.TYPE.CONDITION = ''; ERROR.TXN.TYPE.CONDITION = ''; Y.RESULTADO = ''; Y.MONTO = 0;
*CALL F.READ(FN.TXN.TYPE.CONDITION,R.FT<FT.TRANSACTION.TYPE>,R.TXN.TYPE.CONDITION,FV.TXN.TYPE.CONDITION,ERROR.TXN.TYPE.CONDITION)
    CALL CACHE.READ(FN.TXN.TYPE.CONDITION,R.FT<FT.TRANSACTION.TYPE>,R.TXN.TYPE.CONDITION,ERROR.TXN.TYPE.CONDITION)

    Y.DESCRIPCION =  R.TXN.TYPE.CONDITION<FT6.DESCRIPTION>
    Y.DESCRIPCION = CHANGE(Y.DESCRIPCION,@SM,@FM)
    Y.DESCRIPCION = CHANGE(Y.DESCRIPCION,@VM,@FM)
    Y.DESCRIPCION = Y.DESCRIPCION<1>
    Y.MONTO = R.FT<FT.DEBIT.AMOUNT>
    Y.MONEDA = R.FT<FT.DEBIT.CURRENCY>
    IF NOT(Y.MONTO) THEN
        Y.MONTO = R.FT<FT.CREDIT.AMOUNT>
        Y.MONEDA = R.FT<FT.CREDIT.CURRENCY>
    END
    Y.RESULTADO = Y.FT.ID:"*":Y.INPUTTER:"*":Y.AUTHORISER:"*":Y.MONTO:"*":Y.MONEDA:"*":R.FT<FT.TRANSACTION.TYPE>:"*":Y.DESCRIPCION
    Y.RESULTADO :="*":R.FT<FT.DEBIT.ACCT.NO>:"*":R.FT<FT.CREDIT.ACCT.NO>:"*":R.FT<FT.DATE.TIME>
    Y.ARREGLO<-1> = Y.RESULTADO
RETURN



END
