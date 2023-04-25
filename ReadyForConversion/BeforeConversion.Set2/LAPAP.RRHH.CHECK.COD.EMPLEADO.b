*-----------------------------------------------------------------------------
* <Rating>-20</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.RRHH.CHECK.COD.EMPLEADO(ENQ.DATA)
*-------------------------------------------------------------------------
* Developed By  : OLIVER FERMIN
* Date  : 16/12/2019
* Valida si el CUSTOMER.ID posee el codigo de empleado en el campo FAX.1
*-------------------------------------------------------------------------

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_F.CUSTOMER
    $INSERT T24.BP I_F.AA.ARRANGEMENT
    $INSERT T24.BP I_ENQUIRY.COMMON

    GOSUB OPENFILES
    GOSUB PROCESS

    RETURN

*---------
OPENFILES:
*---------

    FN.AA = "F.AA.ARRANGEMENT"
    F.AA = ""
    CALL OPF(FN.AA,F.AA)

    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

    RETURN


PROCESS:
*******

    LOCATE 'ARRANGEMENT.ID' IN ENQ.DATA SETTING CUS.POS THEN
        Y.AA.ID =  ENQ.DATA<4>
    END

    R.AA = '' ; ERROR.AA = '' ;
    CALL F.READ(FN.AA,Y.AA.ID, R.AA, F.AA, ERROR.AA)
    Y.AA.CUSTOMER.ID = R.AA<AA.ARR.CUSTOMER>

    CALL F.READ(FN.CUSTOMER, Y.AA.CUSTOMER.ID, R.CUSTOMER, F.CUSTOMER,ERR)
    Y.CODIGO.EMPLEADO = R.CUSTOMER<EB.CUS.FAX.1>

    IF Y.CODIGO.EMPLEADO EQ '' THEN
        ENQ.ERROR = 'EB-NO.ACCESS.EX.EMPLEADO'
        CALL STORE.END.ERROR
    END

    RETURN

END
