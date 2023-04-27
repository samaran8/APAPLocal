*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE L.APAP.V.FR.CL.ACCT.RT
    $INSERT  I_COMMON
    $INSERT  I_EQUATE
    $INSERT  I_F.REDO.FRONT.CLAIMS
*------------------------------------------------------------------------------------------------------------
    Y.FR.CL.PRODUCT.TYPE = ""
    Y.FR.CL.ACCOUNT.ID = ""
*------------------------------------------------------------------------------------------------------------
*------------------------------------------------------------------------------------------------------------
    Y.FR.CL.PRODUCT.TYPE = R.NEW(FR.CL.PRODUCT.TYPE)
    Y.FR.CL.ACCOUNT.ID = R.NEW(FR.CL.ACCOUNT.ID)
    TEST.VAR = R.OLD(FR.CL.ACCOUNT.ID)
* DEBUG
*------------------------------------------------------------------------------------------------------------
    IF Y.FR.CL.PRODUCT.TYPE EQ "CERTIFICADOS" OR Y.FR.CL.PRODUCT.TYPE EQ "CUENTA.DE.AHORROS" OR Y.FR.CL.PRODUCT.TYPE EQ "PRESTAMOS" THEN
        IF Y.FR.CL.ACCOUNT.ID EQ "" THEN
            MESSAGE = 'ESTE CAMPO ES REQUERIDO CUANDO EL PRODUCTO ES ' : Y.FR.CL.PRODUCT.TYPE
            E = MESSAGE
            ETEXT = E
            CALL ERR
        END
    END
*------------------------------------------------------------------------------------------------------------
    RETURN

END
