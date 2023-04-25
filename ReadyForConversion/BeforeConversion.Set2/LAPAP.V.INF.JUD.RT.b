*-----------------------------------------------------------------------------
* <Rating>-35</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE LAPAP.V.INF.JUD.RT
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT T24.BP I_GTS.COMMON
    $INSERT T24.BP I_System
    $INSERT BP I_F.REDO.ID.CARD.CHECK
    $INSERT T24.BP I_F.COMPANY

*MSG = ''
*MSG<-1> = 'Llamo rutina'
*CALL LAPAP.LOGGER('TESTLOG',ID.NEW,MSG)

    GOSUB DO.INITIALIZE
    IF R.NEW(REDO.CUS.PRF.IDENTITY.TYPE) NE 'RNC' THEN
        IF R.NEW(REDO.CUS.PRF.CUSTOMER.TYPE) EQ 'NO CLIENTE APAP' THEN
            GOSUB GET.CON.JUD
        END
    END
    RETURN
*-----------------------------------------------------------------------------------------------------------------------------------
* Pgm. Name: LAPAP.V.INF.JUD.RT
* Desc.: This subroutine is attached as an embedded validation-input routine for REDO.ID.CARD.CHECK, version.
* By: J.Q.
* On: Sep 23 2022
*-----------------------------------------------------------------------------------------------------------------------------------
*-----------------------------------------------------------------------------------------------------------------------------------
DO.INITIALIZE:
    Y.COMPANY.NAME = R.COMPANY(EB.COM.COMPANY.NAME)
    Y.COMPANY.NAME.FMT = Y.COMPANY.NAME
    CHANGE ' ' TO '.' IN Y.COMPANY.NAME.FMT
    Y.OPERATOR = OPERATOR
    RETURN
*-----------------------------------------------------------------------------------------------------------------------------------
GET.CON.JUD:
    V.EB.API.ID = 'LAPAP.CONJUDICIAL.ITF'
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*C::CEDULA::USUARIO::SUCURSAL::SUCURSAL
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Y.IDENTIFICATION = R.NEW(REDO.CUS.PRF.IDENTITY.NUMBER)
    Y.PARAMETRO.ENVIO = 'C::' : Y.IDENTIFICATION : '::' : Y.OPERATOR : '::' : Y.COMPANY.NAME.FMT : '::' : Y.COMPANY.NAME.FMT
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    CALL EB.CALL.JAVA.API(V.EB.API.ID,Y.PARAMETRO.ENVIO,Y.RESPONSE,Y.CALLJ.ERROR)
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    IF Y.RESPONSE THEN
        GOSUB DO.VALIDATE
    END
    RETURN
*-----------------------------------------------------------------------------------------------------------------------------------
DO.VALIDATE:


*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Y.DECISION = FIELD(Y.RESPONSE,'&',3)

    IF Y.DECISION EQ 'Si' THEN

        IF GETENV("JUDICIAL_INQUIRY_NOTIFY", shouldNotifyAssertion) THEN

            IF shouldNotifyAssertion EQ 'yes' THEN
                CALL LAPAP.PLAF.NOTIFY.RT("JUDICIAL")
            END
        END
        AF = 2
        AV = 1
        AS = 1
        TEXT = "No cumple con politicas internas."
        ETEXT = TEXT
        *E = TEXT
        CALL STORE.END.ERROR
    END

    RETURN
*-----------------------------------------------------------------------------------------------------------------------------------
END
