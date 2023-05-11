SUBROUTINE L.APAP.TEL.SIMULACION
*--------------------------------------------------------------------------------------------------
* Description           : Rutina para obtener el telefono del cliente
*                        recibe como parametro el codigo del cliente
* Developed On          : 16-juan-2019
* Developed By          : Bienvenido Romero
* Development Reference : Para el proceso de monitor req: CI009444
*--------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER

    Y.CLIENTE.ID = COMI
    Y.CLIENTE.ID = CHANGE(Y.CLIENTE.ID,"*",@FM)
    Y.CUS.ID = Y.CLIENTE.ID<1>
    Y.TIPO = Y.CLIENTE.ID<2>
    GOSUB INICIO
    GOSUB GET.TELEFONO.CLIENTE
RETURN
INICIO:
*********
    FN.CUS = "F.CUSTOMER"
    F.CUS = ""
    CALL OPF(FN.CUS,F.CUS)
RETURN

GET.TELEFONO.CLIENTE:
*********************
    R.CUSTOMER = ''; ERROR.CUSTOMER = ''
    CALL F.READ(FN.CUS,Y.CUS.ID,R.CUSTOMER,F.CUS,CUS.ERR)
    CALL GET.LOC.REF("CUSTOMER","L.CU.TEL.TYPE",Y.POS)
    Y.TEL.TYPE = R.CUSTOMER<EB.CUS.LOCAL.REF,Y.POS>
    CALL GET.LOC.REF("CUSTOMER","L.CU.TEL.AREA",Y.POS)
    Y.AREA = R.CUSTOMER<EB.CUS.LOCAL.REF,Y.POS>
    CALL GET.LOC.REF("CUSTOMER","L.CU.TEL.NO",Y.POS)
    Y.TEL.NO = R.CUSTOMER<EB.CUS.LOCAL.REF,Y.POS>
    GOSUB CONVERT.FM.CUSTOMER
    LOCATE Y.TIPO IN  Y.TEL.TYPE<1> SETTING CUS.POS THEN
        COMI = Y.AREA<CUS.POS>:Y.TEL.NO<CUS.POS>
    END ELSE
        COMI = ''
    END
RETURN
CONVERT.FM.CUSTOMER:
*******************
    Y.TEL.TYPE = CHANGE(Y.TEL.TYPE,@SM,@FM)
    Y.TEL.TYPE = CHANGE(Y.TEL.TYPE,@VM,@FM)
    Y.TEL.NO = CHANGE(Y.TEL.NO,@SM,@FM)
    Y.TEL.NO = CHANGE(Y.TEL.NO,@VM,@FM)
    Y.AREA = CHANGE(Y.AREA,@SM,@FM)
    Y.AREA = CHANGE(Y.AREA,@VM,@FM)
RETURN

END
